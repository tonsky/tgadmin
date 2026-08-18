(ns tgadmin.core
  (:require
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure+.core :as clojure+ :refer [if+ when+ cond+]]
   [org.httpkit.client :as http])
  (:import
   [java.io File FileWriter]
   [java.util Timer TimerTask]))

;; utils

(defonce ^Timer timer
  (Timer. true))

(defn- timer-task ^TimerTask [var]
  (proxy [TimerTask] []
    (run []
      (try
        (@var)
        (catch Throwable t
          (.printStackTrace t))))))

(defn swap-dissoc! [*atom key]
  (let [[before after] (swap-vals! *atom dissoc key)]
    (get before key)))

(defn trim [s]
  (if (<= (count s) 80)
    s
    (str (subs s 0 80) "...")))


;; config

(def config
  (edn/read-string (slurp "config.edn")))

(def dev?
  (:dev? config false))

(def token
  (:token config))

(def vote-limit
  (:vote-limit config 3))

(def vote-ttl
  (:vote-ttl config (* 24 60 60 1000)))

(def repeated-messages-limit
  (:repeated-messages-limit config 3))

(def admin-cache-ttl
  (:admin-cache-ttl config (* 60 60 1000)))

;; private group used to probe if messages still exist, bot must be a member
(def dump-group-id
  (:dump-group-id config -5016389519))

;; Time to first clown monitoring
(def reaction-channel-id
  (:reaction-channel-id config
   #_-1002729833355 ;; nikitonsky_pub_test
   -1001339432494))  ;; nikitonsky_pub

(def reaction-group-id
  (:reaction-group-id config
   #_-1002762672757  ;; nikitonsky_chat_test
   -1001436433940))  ;; nikitonsky_chat


;; Telegram API

(defn post!
  ([method]
   (post! method {}))
  ([method opts]
   (try
     (let [opts (cond-> opts
                  ; https://core.telegram.org/bots/api#markdownv2-style
                  (= "MarkdownV2" (:parse_mode opts))
                  (update :text str/replace #"[_*~`>#\+\-|\{\}\.!]" #(str "\\" %)))
           req  {:url     (str "https://api.telegram.org/bot" token method)
                 :method  :post
                 :body    (json/generate-string opts)
                 :headers {"Content-Type" "application/json"}}
           resp @(http/request req)
           body (json/parse-string (:body resp) true)]
       (if (:ok body)
         (:result body)
         (do
           (println "[ ERROR ]" body)
           nil)))
     (catch InterruptedException e
       (throw e))
     (catch Exception e
       (.printStackTrace e)
       nil))))


;; state

;; #{user-id ...}
(def *known-users
  (atom
    (->> (slurp "known_users")
      (re-seq #"(?m)^-?\d+")
      (map parse-long)
      set)))

;; {user-id {:messages     [message ...]
;;           :vote-message message
;;           :approve      #{voter-id ...}
;;           :ban          #{voter-id ...}
;;           :added        <unix time>}}
(def *pending-votes
  (atom {}))

;; {chat-id {:admins #{user-id ...} :fetched timestamp}}
(def *admin-cache
  (atom {}))

(def *reaction-channel-posts
  "{message_id {:date timestamp}}"
  (atom {}))


;; app

(defn check-external [user-id]
  (try
    (let [resp @(http/request
                  {:url             (str "https://lols.bot/?a=" user-id)
                   :method          :get
                   :connect-timeout 5000})]
      (when (= 200 (:status resp))
        (let [body (json/parse-string (:body resp) true)]
          (when (:banned body)
            (str "banned at lols.bot")))))
    (catch Exception e
      (.printStackTrace e))))

(defn message-exists? [message]
  ;; there’s no getMessage in Bot API, so we forward message to a dump group
  ;; and see if it fails. Deleted messages give “message to forward not found”
  ;; in basic groups but “MESSAGE_ID_INVALID” in supergroups
  (try
    (let [resp @(http/request
                  {:url     (str "https://api.telegram.org/bot" token "/forwardMessage")
                   :method  :post
                   :body    (json/generate-string
                              {:chat_id              dump-group-id
                               :from_chat_id         (:id (:chat message))
                               :message_id           (:message_id message)
                               :disable_notification true})
                   :headers {"Content-Type" "application/json"}})
          body (json/parse-string (:body resp) true)]
      (if (:ok body)
        (do
          (post! "/deleteMessage"
            {:chat_id    dump-group-id
             :message_id (-> body :result :message_id)})
          true)
        (let [description (str (:description body))]
          (not
            (or
              (str/includes? description "not found")
              (str/includes? description "MESSAGE_ID_INVALID"))))))
    (catch Exception e
      (.printStackTrace e)
      true)))

(defn message-str [message]
  (str
    (:username (:chat message)) "/" (:message_id message)
    " by " (:id (:from message))
    (when-some [username (:username (:from message))]
      (str " (" username ")"))))

(defn user-str ^String [user]
  (let [{:keys [id username first_name last_name]} user]
    (str id
      (when username (str " @" username))
      (when first_name (str " " first_name))
      (when last_name (str " " last_name)))))

(defn whitelist-user [user]
  (println "[ WHITELIST ]" (user-str user))
  (when-not dev?
    (swap! *known-users conj (:id user))
    (with-open [w (FileWriter. (io/file "known_users") true)]
      (.write w (user-str user))
      (.write w "\n"))))

(defn ban-user [user reason messages]
  (let [chat-id (:id (:chat (first messages)))]
    (doseq [message messages]
      (println "[ DELETING ]" (message-str message) "for" reason)
      (post! "/deleteMessage" {:chat_id chat-id, :message_id (:message_id message)}))
    (println "[ BAN ]" (user-str user) "for" reason)
    (when-not dev?
      (post! "/banChatMember" {:chat_id chat-id, :user_id (:id user)}))))

(defn get-admins [chat-id]
  (let [now    (System/currentTimeMillis)
        cached (@*admin-cache chat-id)]
    (if (and cached (< (- now (:fetched cached)) admin-cache-ttl))
      (:admins cached)
      (let [members (post! "/getChatAdministrators" {:chat_id chat-id})
            admins  (set (map #(-> % :user :id) members))]
        (println "[ ADMINS ] for" chat-id (str/join ", " (map #(-> % :user :username) members)))
        (swap! *admin-cache assoc chat-id {:admins admins :fetched now})
        admins))))

(defn vote-keyboard [user-id approve-count ban-count]
  {:inline_keyboard [[{:text          (str "🤖 Бот (" ban-count ")")
                       :callback_data (str "ban:" user-id)}
                      {:text          (str "🧑 Не бот (" approve-count ")")
                       :callback_data (str "approve:" user-id)}]]})

(defn start-vote [message]
  (let [chat-id    (:id (:chat message))
        message-id (:message_id message)
        user       (:from message)
        user-id    (:id user)
        _          (println "[ VOTE ]" (message-str message))
        mention    (or
                     (when (and (:first_name user) (:last_name user))
                       (str (:first_name user) " " (:last_name user)))
                     (:first_name user)
                     (:last_name user)
                     (:username user)
                     user-id)
        vote-msg   (post! "/sendMessage"
                     {:chat_id          chat-id
                      :reply_parameters {:message_id message-id}
                      :parse_mode       "MarkdownV2"
                      :text             (str "Голосуем! " mention " — бот или нет?")
                      :reply_markup     (vote-keyboard user-id 0 0)})]
    (swap! *pending-votes assoc user-id
      {:messages     [message]
       :vote-message vote-msg
       :approve      #{}
       :ban          #{}
       :added        (System/currentTimeMillis)})))

(defn resolve-vote!
  "Removes user from pending votes and deletes vote message.
   Returns pending map or nil if already resolved by someone else"
  [user-id]
  (when-some [pending (swap-dissoc! *pending-votes user-id)]
    (let [vote-message (:vote-message pending)]
      (post! "/deleteMessage"
        {:chat_id    (-> vote-message :chat :id)
         :message_id (:message_id vote-message)})
      pending)))

(defn handle-callback-query [callback-query]
  (let [callback-id           (:id callback-query)
        voter-id              (:id (:from callback-query))
        chat-id               (-> callback-query :message :chat :id)
        msg-id                (-> callback-query :message :message_id)
        data                  (:data callback-query)
        [_ action target-id]  (re-matches #"(approve|ban):(.*)" data)
        target-id             (some-> target-id parse-long)
        pending               (@*pending-votes target-id)
        {:keys [approve ban messages]} pending
        target-user           (:from (first messages))]

    (cond+
      ;; not in progress
      (not pending)
      (post! "/answerCallbackQuery" {:callback_query_id callback-id})

      ;; repeated vote
      (or
        (and (= "approve" action) (approve voter-id))
        (and (= "ban" action)     (ban voter-id)))
      (post! "/answerCallbackQuery"
        {:callback_query_id callback-id
         :text              "Вы уже проголосовали"})

      :let [_        (post! "/answerCallbackQuery" {:callback_query_id callback-id})
            admins   (get-admins chat-id)
            vote     (fn [pending]
                       (if (= "approve" action)
                         (-> pending
                           (update :approve conj voter-id)
                           (update :ban disj voter-id))
                         (-> pending
                           (update :approve disj voter-id)
                           (update :ban conj voter-id))))
            pending' (-> (swap! *pending-votes update target-id vote)
                       (get target-id))]

      ;; approve
      (or (>= (count (:approve pending')) vote-limit)
          (some admins (:approve pending')))
      (when (resolve-vote! target-id)
        (whitelist-user target-user))

      ;; ban
      (or (>= (count (:ban pending')) vote-limit)
          (some admins (:ban pending')))
      (when-some [pending (resolve-vote! target-id)]
        (ban-user target-user "user vote" (:messages pending)))

      ;; update counts
      (post! "/editMessageReplyMarkup"
        {:chat_id      chat-id
         :message_id   msg-id
         :reply_markup (vote-keyboard target-id (count (:approve pending')) (count (:ban pending')))}))))

(defn handle-message [message]
  (let [user    (:from message)
        user-id (:id user)]
    (cond+
      ;; service messages (joins, etc.)
      (:new_chat_members message)
      :nop

      ;; known
      (and
        (@*known-users user-id)
        (or
          (not dev?)
          (not= "nikitonsky" (:username user))))
      :nop

      ;; pending -- collect messages
      (contains? @*pending-votes user-id)
      (let [pending' (swap! *pending-votes update-in [user-id :messages] conj message)
            messages (get-in pending' [user-id :messages])
            max-freq (->> messages (keep :text) (filter #(> (count %) 10)) frequencies vals (reduce max 0))]
        (when (>= max-freq repeated-messages-limit)
          (when-some [pending (resolve-vote! user-id)]
            (ban-user user "repeated messages" (:messages pending)))))

      ;; unknown -- banned by lols
      :let [reason (check-external user-id)]
      reason
      (ban-user user reason [message])

      ;; first message -- start community vote
      :else
      (start-vote message))))

(defn handle-reaction-post [message]
  (when (= reaction-channel-id (-> message :forward_from_chat :id))
    (let [message-id (:forward_from_message_id message)
          date       (:forward_date message)]
      (swap! *reaction-channel-posts assoc message-id {:date date})
      (println (str "[ TRACKING REACTIONS ] " (-> message :forward_from_chat :title) ", post #" message-id ": “" (trim (:text message)) "”")))))

(defn handle-reaction-count [reaction-count]
  (let [{message-id    :message_id
         reactions     :reactions
         reaction-date :date
         {chat-id      :id
          chat-title   :title} :chat} reaction-count]
    (when+ (and
             (= chat-id reaction-channel-id)
             :let [[reaction & _] (filter #(= "🤡" (-> % :type :emoji)) reactions)]
             reaction
             :let [{post-date :date} (@*reaction-channel-posts message-id)]
             post-date)
      (let [minutes    (-> (- reaction-date post-date) (quot 60))
            declension (cond
                         (#{11 12 13 14} (mod minutes 100)) "минут"
                         (= 1 (mod minutes 10)) "минута"
                         (#{2 3 4} (mod minutes 10)) "минуты"
                         :else "минут")]
        (println (str "[ FIRST REACTION ] Channel " chat-title ", post #" message-id ", reaction " reaction ", delta t " minutes " minutes"))
        (post! "/sendMessage"
          {:chat_id reaction-group-id
           :text    (str "Время до первого 🤡 — " minutes " " declension)})
        (swap! *reaction-channel-posts dissoc message-id)))))

(defn log-update [u]
  (cond-> u
    (-> u :message :reply_to_message :text)
    (update :message update :reply_to_message update :text trim)
    
    true
    prn))

(defn check-pending-votes
  "Re-checks every user with an open vote: bans if lols.bot now reports them,
   retracts the vote if user deleted all their messages"
  []
  (doseq [[user-id pending] @*pending-votes
          :let [user   (-> pending :messages first :from)
                reason (check-external user-id)]]
    (cond+
      ;; reported as spammer while vote was in progress
      reason
      (when-some [pending (resolve-vote! user-id)]
        (ban-user user reason (:messages pending)))

      ;; user deleted all their messages -- retract vote, forget user
      (not-any? message-exists? (:messages pending))
      (when (resolve-vote! user-id)
        (println "[ RETRACTED ]" (user-str user))))))

(defn cleanup-pending-votes []
  (let [now (System/currentTimeMillis)]
    (doseq [[user-id {:keys [added]}] @*pending-votes]
      (when (< added (- now vote-ttl))
        (println "[ CLEANUP ]" user-id)
        (swap! *pending-votes dissoc user-id)))))

(defn -main [& args]
  (println "[ STARTED ]")
  (.scheduleAtFixedRate timer (timer-task #'cleanup-pending-votes) 0 (* 60 60 1000))
  (.scheduleAtFixedRate timer (timer-task #'check-pending-votes) (* 60 1000) (* 60 1000))
  (loop [offset 0]
    (if-some [updates (post! "/getUpdates"
                        {:offset offset
                         :allowed_updates ["message" "callback_query" "message_reaction_count"]})]
      (do
        (doseq [update updates
                :let [_ (log-update update)]]
          (try
            (cond
              (:message update)
              (do
                (handle-reaction-post (:message update))
                (handle-message (:message update)))

              (:callback_query update)
              (handle-callback-query (:callback_query update))

              (:message_reaction_count update)
              (handle-reaction-count (:message_reaction_count update)))
            (catch Exception e
              (.printStackTrace e))))
          
        (if (empty? updates)
          (recur offset)
          (recur (-> updates last :update_id inc long))))
      (recur offset))))

(comment
  (-main)
  
  ;; post in channel
  {:update_id 558985903
   :message
   {:date                    1753738345

    :forward_from_chat
    {:id       -1002729833355
     :title    "Channel Test"
     :username "nikitonsky_pub_test"
     :type     "channel"}

    :chat
    {:id       -1002762672757
     :title    "Channel Test Chat"
     :username "nikitonsky_chat_test"
     :type     "supergroup"}

    :is_automatic_forward    true
    :message_id              15

    :forward_origin
    {:type             "channel"
     :chat
     {:id       -1002729833355
      :title    "Channel Test"
      :username "nikitonsky_pub_test"
      :type     "channel"}
     :message_id       7
     :author_signature "Nikita Prokopov"
     :date             1753738342}

    :from
    {:id         777000
     :is_bot     false
     :first_name "Telegram"}

    :forward_signature       "Nikita Prokopov"
    :forward_from_message_id 7
    :forward_date            1753738342
    :sender_chat
    {:id       -1002729833355
     :title    "Channel Test"
     :username "nikitonsky_pub_test"
     :type     "channel"}
    :text                    "channel test post 5"}}

  ;; reactions
  {:update_id              558985904
   :message_reaction_count
   {:chat
    {:id       -1002729833355
     :title    "Channel Test"
     :username "nikitonsky_pub_test"
     :type     "channel"}
    :message_id 7
    :date       1753738503
    :reactions
    [{:type
      {:type  "emoji"
       :emoji "🤡"}
      :total_count 1}]}}

  (json/parse-string
    (:body @(http/get "https://lols.bot/?a=232806939")) true)
  
  (json/parse-string
    (:body @(http/get "https://lols.bot/?a=2069820207")) true)
  
  (:content-type (:headers @(http/get "https://lols.bot/?a=2069820207")))
  (json/parse-string (:body @(http/get "https://lols.bot/asdas")) true)
  
  (post! "/sendMessage"
    {:chat_id           -1001436433940
     :reply_parameters  {:message_id 95692}
     ; :message_thread_id 95594
     :parse_mode        "MarkdownV2"
     :text              "test"})
    
  
  (post! "/getMe")
  (post! "/getUpdates" {:offset 558841683})
  
  (post! "/getChat" {:chat_id chat-id})
  
  (post! "/getChatMember" {:chat_id chat-id
                           :user_id 232806939})
  
  ;; TEXT
  {:update_id 558841686, :message {:message_id 6, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933169, :text "test"}}

  ;; LINK
  {:update_id 558841688, :message {:message_id 8, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933181, :text "link https://core.telegram.org/bots/api#available-methods", :entities [{:offset 5, :length 52, :type "url"}]}}
  
  ;; MENTION
  {:update_id 558841689, :message {:message_id 9, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933195, :text "mention @nikitonksy", :entities [{:offset 8, :length 11, :type "mention"}]}}

  ;; REPLY
  {:update_id 558841693, :message {:message_id 13, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933272, :message_thread_id 8, :reply_to_message {:message_id 8, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933181, :text "link https://core.telegram.org/bots/api#available-methods", :entities [{:offset 5, :length 52, :type "url"}]}, :text "reply"}}
  
  ;; REPLY WITH LINK
  {:update_id 558841694, :message {:message_id 14, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933343, :message_thread_id 8, :reply_to_message {:message_id 8, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933181, :text "link https://core.telegram.org/bots/api#available-methods", :entities [{:offset 5, :length 52, :type "url"}]}, :text "reply with link https://tonsky.me", :entities [{:offset 16, :length 17, :type "url"}]}}

  ;; IMAGE
  {:update_id 558841695, :message {:message_id 15, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933391, :photo [{:file_id "AgACAgIAAx0Cf56CYQADD2VDqo-bgyhW7BV397vVP8F9VXWKAAKH0jEbY1cgSvnVzKahmq-PAQADAgADcwADMwQ", :file_unique_id "AQADh9IxG2NXIEp4", :file_size 1591, :width 67, :height 90} {:file_id "AgACAgIAAx0Cf56CYQADD2VDqo-bgyhW7BV397vVP8F9VXWKAAKH0jEbY1cgSvnVzKahmq-PAQADAgADbQADMwQ", :file_unique_id "AQADh9IxG2NXIEpy", :file_size 25163, :width 240, :height 320} {:file_id "AgACAgIAAx0Cf56CYQADD2VDqo-bgyhW7BV397vVP8F9VXWKAAKH0jEbY1cgSvnVzKahmq-PAQADAgADeAADMwQ", :file_unique_id "AQADh9IxG2NXIEp9", :file_size 116710, :width 600, :height 800} {:file_id "AgACAgIAAx0Cf56CYQADD2VDqo-bgyhW7BV397vVP8F9VXWKAAKH0jEbY1cgSvnVzKahmq-PAQADAgADeQADMwQ", :file_unique_id "AQADh9IxG2NXIEp-", :file_size 185228, :width 960, :height 1280}], :caption "image"}}
  
  ;; FILE
  {:update_id 558841696, :message {:message_id 16, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933395, :document {:file_name "signal-2023-11-02-091621_002.jpeg", :mime_type "image/jpeg", :thumbnail {:file_id "AAMCAgADHQJ_noJhAAMQZUOqkyF6iAMPsCLxUYJ90jUDB00AAtQ6AAJjVyBKdBF0SoE7F_MBAAdtAAMzBA", :file_unique_id "AQAD1DoAAmNXIEpy", :file_size 22448, :width 240, :height 320}, :thumb {:file_id "AAMCAgADHQJ_noJhAAMQZUOqkyF6iAMPsCLxUYJ90jUDB00AAtQ6AAJjVyBKdBF0SoE7F_MBAAdtAAMzBA", :file_unique_id "AQAD1DoAAmNXIEpy", :file_size 22448, :width 240, :height 320}, :file_id "BQACAgIAAx0Cf56CYQADEGVDqpMheogDD7Ai8VGCfdI1AwdNAALUOgACY1cgSnQRdEqBOxfzMwQ", :file_unique_id "AgAD1DoAAmNXIEo", :file_size 423638}, :caption "files"}}

  ;; VIDEO
  {:update_id 558841697, :message {:message_id 17, :from {:id 232806939, :is_bot false, :first_name "Nikita", :last_name "Prokopov", :username "nikitonsky"}, :chat {:id -1002141094497, :title "Grumpy Queue", :username "grumpy_queue", :type "supergroup"}, :date 1698933447, :video {:thumb {:file_id "AAMCAgADHQJ_noJhAAMRZUOqxwNtG2hUFElRIzYbsbZdMDIAAtw6AAJjVyBKSdcY4T_zjQ4BAAdtAAMzBA", :file_unique_id "AQAD3DoAAmNXIEpy", :file_size 15524, :width 257, :height 320}, :file_name "TBPInvictus-1719397053468492105.mp4", :mime_type "video/mp4", :width 360, :duration 24, :file_size 1114419, :file_unique_id "AgAD3DoAAmNXIEo", :thumbnail {:file_id "AAMCAgADHQJ_noJhAAMRZUOqxwNtG2hUFElRIzYbsbZdMDIAAtw6AAJjVyBKSdcY4T_zjQ4BAAdtAAMzBA", :file_unique_id "AQAD3DoAAmNXIEpy", :file_size 15524, :width 257, :height 320}, :file_id "BAACAgIAAx0Cf56CYQADEWVDqscDbRtoVBRJUSM2G7G2XTAyAALcOgACY1cgSknXGOE_840OMwQ", :height 448}, :caption "video"}}

  ;; EDIT
  {:update_id 558841862, :edited_message {:message_id 85324, :from {:id 1329861181, :is_bot false, :first_name "Алиса", :last_name "Королёва", :username "caralice"}, :chat {:id -1001436433940, :title "Стоящие под стрелой", :username "nikitonsky_chat", :type "supergroup"}, :date 1698937255, :edit_date 1698946666, :message_thread_id 85299, :reply_to_message {:date 1698936933, :forward_from_chat {:id -1001339432494, :title "Стой под стрелой", :username "nikitonsky_pub", :type "channel"}, :edit_date 1698936970, :chat {:id -1001436433940, :title "Стоящие под стрелой", :username "nikitonsky_chat", :type "supergroup"}, :is_automatic_forward true, :message_id 85299, :from {:id 777000, :is_bot false, :first_name "Telegram"}, :forward_signature "Nikita Prokopov", :forward_from_message_id 551, :forward_date 1698936930, :sender_chat {:id -1001339432494, :title "Стой под стрелой", :username "nikitonsky_pub", :type "channel"}, :text "Если говорить об идеях, то одна, которая меня никак не отпускает — это объединить будильник с календарем. Почему это два разных приложения?\n\nСейчас будильник сделан как будто для людей, которые встают каждый день в одно и то же время и у них в жизни ничего не меняется. У меня, к сожалению, жизнь устроена по-другому и поэтому в приложении стопитсот будильников, которые когда-то были актуальны (скорее всего один раз) и с тех пор просто занимают место.\n\nНедавно я сделал себе пару регулярных будильников, чтобы вставать на занятия. Все бы хорошо, но случаются исключения (отпуск, например) и переносы. И приходится опять во всей это толпе будильников ходить и включать-выключать туда-сюда (а потом не забыть включить обратно).\n\nНо самый странный интеракшн — это включить будильник на 10:30 так, чтобы он не прозвенел — оказывается, он когда-то создавался с фильтром «только по четвергам и субботам», но в часах такую несущественную деталь, конечно, не показывают. Получается, ты его вроде включил, а он решил утром не звенеть. Надежно, ничего не скажешь.\n\nВ общем, мой поинт. Это все давно решено в календаре: и повротяющиеся события, и переносы, и исчезания старых неактуальных отметок, и визуализация. Плюс, будильник напрямую завязан на события (кроме случаев, когда ты решил «а чего бы просто по приколу не встать в пять утра», конечно).\n\nНу и нафига тогда отдельное приложение?"}, :text "есть ещё приложение \"напоминания\", которое пытается и в календарь, и в заметки одновременно"}}


  
  [{:update_id 558841683
    :message {:message_id 3
              :from {:id 232806939
                     :is_bot false
                     :first_name "Nikita"
                     :last_name "Prokopov"
                     :username "nikitonsky"}
              :chat {:id -1002141094497
                     :title "Grumpy Queue"
                     :username "grumpy_queue"
                     :type "supergroup"}
              :date 1698930820
              :text "test"}}]
  
  [{:update_id 558841680
    :channel_post {:message_id 63
                   :sender_chat {:id -1001150152488
                                 :title "Grumpy Website Test"
                                 :username "grumpy_test"
                                 :type "channel"}
                   :chat {:id -1001150152488
                          :title "Grumpy Website Test"
                          :username "grumpy_test"
                          :type "channel"}
                   :date 1698930640
                   :text "test"}}]
  [{:update_id 558841679
    :my_chat_member {:chat {:id -1001150152488
                            :title "Grumpy Website Test"
                            :username "grumpy_test"
                            :type "channel"}
                     :from {:id 232806939
                            :is_bot false
                            :first_name "Nikita"
                            :last_name "Prokopov"
                            :username "nikitonsky"}
                     :date 1698930384
                     :old_chat_member {:user {:id 6750399431
                                              :is_bot true
                                              :first_name "nikitonsky_admin"
                                              :username "nikitonsky_admin_bot"}
                                       :status "left"}
                     :new_chat_member {:can_post_messages true
                                       :can_manage_video_chats false
                                       :can_post_stories true
                                       :can_manage_voice_chats false
                                       :can_invite_users false
                                       :can_delete_messages true
                                       :can_be_edited false
                                       :can_edit_messages true
                                       :is_anonymous false
                                       :can_change_info false
                                       :can_restrict_members true
                                       :status "administrator"
                                       :can_edit_stories true
                                       :can_promote_members false
                                       :can_manage_chat true
                                       :user {:id 6750399431
                                              :is_bot true
                                              :first_name "nikitonsky_admin"
                                              :username "nikitonsky_admin_bot"}
                                       :can_delete_stories true}}}])