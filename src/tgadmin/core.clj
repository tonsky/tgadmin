(ns tgadmin.core
  (:require
    [cheshire.core :as json]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [org.httpkit.client :as http])
  (:import
    [java.io File FileWriter]
    [java.util Timer TimerTask]))

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

(def config
  (edn/read-string (slurp "config.edn")))

(def token
  (:token config))

(def *known-users
  (atom
    (->> (slurp "known_users")
      (re-seq #"(?m)^-?\d+")
      (map parse-long)
      set)))

(defn post!
  ([method]
   (post! method {}))
  ([method opts]
   (try
     (let [req {:url     (str "https://api.telegram.org/bot" token method)
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

(defn append-user [{:keys [id username first_name last_name]}]
  (swap! *known-users conj id)
  (println "[ USER ]" id username first_name last_name)
  (with-open [w (FileWriter. (io/file "known_users") true)]
    (.write w (str id))
    (when username
      (.write w (str " @" username)))
    (when first_name
      (.write w (str " " first_name)))
    (when last_name
      (.write w (str " " last_name)))
    (.write w "\n")))

(defn media? [message]
  (or
    (some #(contains? message %) [:photo :document :video])
    (some #(#{"url" "mention"} (:type %)) (:entities message))))

(defn mixed-lang? [message]
  (or
    (re-find #"\p{IsLatin}\p{IsCyrillic}+\p{IsLatin}+\p{IsCyrillic}" (:text message))
    (re-find #"\p{IsCyrillic}\p{IsLatin}+\p{IsCyrillic}+\p{IsLatin}" (:text message))))

(comment
  (mixed-lang? {:text "Мы ищeм пapтнepoв для тopгoвлu в направленuu арбuтража kрuптовалют. Дaжe бeз oпытa! Гoтoвы yзнaть пoдpoбнoстu? Ждy вac в лuчныx сoобщенuях для обсужденuя."})
  
  (mixed-lang {:text "C++ привет С++ привет"}))

(defn delete [chat-id message-id text]
  (when-some [resp (post! "/deleteMessage" {:chat_id    chat-id
                                            :message_id message-id})]
    (let [reply (post! "/sendMessage" {:chat_id    chat-id
                                       :parse_mode "MarkdownV2"
                                       :text       text})]
      (schedule 60000
        (post! "/deleteMessage" {:chat_id    chat-id
                                 :message_id (:message_id reply)})))))

(defn -main [& args]
  (println "[ STARTED ]")
  (loop [offset 0]
    (if-some [updates (post! "/getUpdates" {:offset offset})]
      (do
        (doseq [update updates
                :let [_       (prn update)
                      message (:message update)]
                :when message
                :let [user    (:from message)
                      user-id (:id user)
                      chat    (:chat message)
                      chat-id (:id chat)
                      mention (if (:username user)
                                (str "@" (:username user))
                                (str "[%username%](tg://user?id=" (:id user) ")"))]
                :when (not (@*known-users user-id))]
          (cond
            ;; unknown user posting links
            (media? message)
            (do
              (println "[ BLOCKED ]" mention "-> @" (:username chat) ":"
                (cond
                  (:photo message)    (str "[photo] " (:caption message))
                  (:video message)    (str "[video] " (:caption message))
                  (:document message) (str "[document] " (:caption message))
                  :else               (:text message)))
              (delete chat-id (:message_id message)
                (str "Хе-хе, сработал антиспам! Напиши обычное сообщение, потом можешь постить ссылки/картинки, " mention)))
            
            ;; unknown user posting mix of cyrillic/latin
            (mixed-lang? message)
            (do
              (println "[ BLOCKED ]" mention "-> @" (:username chat) ":" (:text message))
              (delete chat-id (:message_id message)
                (str "Ты бот штоле? Не надо мешать кириллицу и латиницу, " mention)))
            
            ;; unknown user posting text
            :else
            (when (:text message)
              (append-user user))))
          
        (if (empty? updates)
          (recur offset)
          (recur (-> updates last :update_id inc long))))
      (recur offset))))

(comment
  (-main)
  
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