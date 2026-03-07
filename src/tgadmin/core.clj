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

(defn- timer-task ^TimerTask [f]
  (proxy [TimerTask] []
    (run []
      (try
        (f)
        (catch Throwable t
          (.printStackTrace t))))))

(defn schedule-impl [^long delay f]
  (let [t (timer-task f)]
    (.schedule timer t delay)
    #(.cancel t)))

(defmacro schedule [delay & body]
  `(schedule-impl ~delay
     (fn []
       ~@body)))

(defn swap-dissoc! [*atom key]
  (let [[before after] (swap-vals! *atom dissoc key)]
    (get before key)))

(defn quote-strings [ss]
  (str "'" (str/join "', '" (distinct ss)) "'"))

(defn trim [s]
  (if (<= (count s) 80)
    s
    (str (subs s 0 80) "...")))

;; config

(def config
  (edn/read-string (slurp "config.edn")))

(def token
  (:token config))

(def react-period-ms
  (:react-period-ms config 60000))

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

;; {user-id {:message message
;;           :warning warning}}
(def *pending-warnings
  (atom {}))

;; Time to first clown monitoring
(def reaction-channel-id
  #_-1002729833355 ;; nikitonsky_pub_test
  -1001339432494)  ;; nikitonsky_pub

(def reaction-group-id
  #_-1002762672757 ;; nikitonsky_chat_test
  -1001436433940)  ;; nikitonsky_chat

(def *reaction-channel-posts
  "{message_id {:date timestamp}}"
  (atom {}))

;; app

(defn check-external [message]
  (try
    (let [resp @(http/request
                  {:url             (str "https://lols.bot/?a=" (:id (:from message)))
                   :method          :get
                   :connect-timeout 5000})]
      (when (= 200 (:status resp))
        (let [body (json/parse-string (:body resp) true)]
          (when (:banned body)
            (str "banned at lols.bot")))))
    (catch Exception e
      (.printStackTrace e))))

(defn check-media [message]
  (when-some [types (not-empty
                      (concat
                        (when (:photo message)
                          ["photo"])
                        (cond
                          (:video message)     ["video"]
                          (:animation message) ["animation"]
                          (:document message)  ["document"])
                        (when (some #(= "url" (:type %)) (:entities message))
                          ["url"])
                        (when (some #(= "text_link" (:type %)) (:entities message))
                          ["text_link"])
                        (when (some #(= "mention" (:type %)) (:entities message))
                          ["mention"])))]
    (str "containing: " (str/join ", " types))))

(defn check-mixed-lang [message]
  (when-some [text (:text message)]
    (when-some [words (not-empty
                        (re-seq #"(?uUi)\b\w*(?:\p{IsLatin}\p{IsCyrillic}+\p{IsLatin}+\p{IsCyrillic}|\p{IsCyrillic}\p{IsLatin}+\p{IsCyrillic}+\p{IsLatin})\w*\b" text))]
      (str "mixing cyrillic with latin: " (quote-strings words)))))

(defn check-symbols [message]
  (when-some [text (:text message)]
    (when (or (str/index-of text "⁨"))
      (str "suspicious characters"))))

(defn check-stop-words [message]
  (when-some [s (:text message)]
    (when-some [words (not-empty
                        (concat
                          (re-seq #"(?uUi)\b(?:сотрудничеств|сфер|выплат|направлени|заработ|доход|доллар|средств|деньг|личк|русло|актив|работ|команд|обучени|юмор|улыб|мудак|говн|курс|помощь|напиш|срочн)[а-я]*\b" s)
                          (re-seq #"(?uUi)\b(?:лс)\b" s)
                          (re-seq #"(?uUi)\b[а-я]*(?:менеджмент)[а-я]*\b" s)
                          (re-seq #"(?uUi)\b(?:[0-9\.]+ ?р(?:уб)?\.?)\b" s)
                          (re-seq #"(?uUi)\b(?:usdt|usd|https|http|binance|bitcoin|web|18|p2p|trading)\b" s)
                          (re-seq #"[\p{IsHan}\p{IsHangul}]+" s)
                          (re-seq #"(?uUi)(?:\$|💸|❇️|🚀|❗️)" s)))]
      (str "stop-words: " (quote-strings words)))))

(defn check-message [message]
  ((some-fn check-media check-mixed-lang check-symbols check-stop-words)
   message))

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
  (swap! *known-users conj (:id user))
  (println "[ WHITELIST ]" (user-str user))
  (with-open [w (FileWriter. (io/file "known_users") true)]
    (.write w (user-str user))
    (.write w "\n")))

(defn ban-user [user reason message & messages]
  (let [chat-id (:id (:chat message))
        user    (:from message)
        user-id (:id user)]
    (doseq [message (cons message messages)]
      (println "[ DELETING ]" (message-str message) "for" reason)
      (post! "/deleteMessage" {:chat_id chat-id, :message_id (:message_id message)}))
    (println "[ BAN ]" (user-str user) "for" reason)
    (post! "/banChatMember" {:chat_id chat-id, :user_id user-id})))

(defn warn [message reason]
  (let [chat-id    (:id (:chat message))
        message-id (:message_id message)
        _          (println "[ WARNING ]" (message-str message) "for" reason)
        user       (:from message)
        user-id    (:id user)
        mention    (if (:username user)
                     (str "@" (:username user))
                     (str "[" (or (:first_name user) (:last_name user) "%username%") "](tg://user?id=" (:id user) ")"))
        warning    (post! "/sendMessage"
                     {:chat_id          chat-id
                      :reply_parameters {:message_id message-id}
                      :parse_mode       "MarkdownV2"
                      :text             (str "Привет " mention ", это антиспам. Нажми кнопку, что ты не робот ↓👇")
                      :reply_markup     {:inline_keyboard [[{:text "Я не робот" :callback_data (str "ack:" user-id)}]]}})]
    (swap! *pending-warnings assoc user-id {:message message
                                            :warning warning})
    (schedule react-period-ms
      (when (swap-dissoc! *pending-warnings user-id)
        (ban-user user reason message warning)))))

(defn handle-callback-query [callback-query]
  (let [user-id  (:id (:from callback-query))
        chat-id  (-> callback-query :message :chat :id)
        data     (:data callback-query)]
    (post! "/answerCallbackQuery" {:callback_query_id (:id callback-query)})
    (when (= data (str "ack:" user-id))
      (when-some [{warning :warning} (swap-dissoc! *pending-warnings user-id)]
        (whitelist-user (:from callback-query))
        (post! "/deleteMessage" {:chat_id chat-id, :message_id (:message_id warning)})))))

(defn deny [message]
  (let [user    (:from message)
        user-id (:id user)]
    (when-some [{first-message :message
                 warning       :warning} (swap-dissoc! *pending-warnings user-id)]
      (ban-user user "repeated message" first-message warning message))))

(defn handle-message [message]
  (let [user    (:from message)
        user-id (:id user)
        chat-id (:id (:chat message))]
    (cond+
      ;; known
      (and
        (@*known-users user-id)
        #_(not= "nikitonsky" (:username user)))
      :nop

      ;; pending -- repeated message
      (contains? @*pending-warnings user-id)
      (deny message)
      
      ;; unknown -- banned by lols
      :let [reason (check-external message)]
      reason
      (ban-user user reason message)
      
      ;; unknown -- sus
      :let [reason (check-message message)]
      reason
      (warn message reason)
      
      ;; unknown -- okay
      (:text message)
      (whitelist-user user))))

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

(defn -main [& args]
  (println "[ STARTED ]")
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