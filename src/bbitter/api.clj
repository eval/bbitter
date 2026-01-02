(ns bbitter.api
  (:require [bbitter.oauth :as oauth]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.net URLEncoder]))

(defn proxy-video-url
  "Convert a Twitter video URL to a proxy URL."
  [url]
  (when url
    (str "/proxy/video?url=" (URLEncoder/encode url "UTF-8"))))

(defn user-by-screen-name-request [screen-name]
  {:method       :get
   :base-url     "https://api.x.com/graphql/u7wQyGi6oExe8_TRWGMq4Q/UserResultByScreenNameQuery"
   :query-params {:variables {:screen_name screen-name}
                  :features  oauth/default-features}})

(defn user-tweets-request [user-id {:keys [count] :or {count 20}}]
  {:method       :get
   :base-url     "https://api.x.com/graphql/3JNH4e9dq1BifLxAa3UMWg/UserWithProfileTweetsQueryV2"
   :query-params {:variables {:rest_id user-id :count count}
                  :features  oauth/default-features}})

(defn fetch-user-by-screen-name
  "Fetch user info by screen name (handle)."
  [screen-name credentials oauth-session]
  (let [response (oauth/make-request (user-by-screen-name-request screen-name)
                                     credentials
                                     oauth-session)]
    (-> response :body (json/parse-string true))))

(defn fetch-user-tweets
  "Fetch tweets for a user by their ID."
  [user-id credentials oauth-session & [{:keys [count] :or {count 20}}]]
  (let [response (oauth/make-request (user-tweets-request user-id {:count count})
                                     credentials
                                     oauth-session)]
    (-> response :body (json/parse-string true))))

(defn get-user-id
  "Extract user ID from user response."
  [user-response]
  (get-in user-response [:data :user_result :result :rest_id]))

(defn extract-tweets
  "Extract tweet data from the timeline response."
  [tweets-response]
  (let [instructions (get-in tweets-response [:data :user_result :result :timeline_response :timeline :instructions])
        entries (->> instructions
                     (filter #(= (:__typename %) "TimelineAddEntries"))
                     first
                     :entries)]
    (->> entries
         (filter #(str/starts-with? (:entryId %) "tweet-"))
         (map (fn [entry]
                (let [tweet-result (get-in entry [:content :content :tweetResult :result])
                      legacy (get-in tweet-result [:legacy])
                      user-legacy (get-in tweet-result [:core :user_result :result :legacy])
                      ;; Check for retweet - get full text from original tweet
                      retweet-result (get-in legacy [:retweeted_status_result :result])
                      retweet-legacy (get-in retweet-result [:legacy])
                      retweet-author (get-in retweet-result [:core :user_result :result :legacy])
                      is-retweet (some? retweet-legacy)
                      ;; Extract media (images and videos)
                      source-legacy (if is-retweet retweet-legacy legacy)
                      raw-media (get-in source-legacy [:extended_entities :media])
                      media (not-empty
                             (->> raw-media
                                  (map (fn [m]
                                         (if (= (:type m) "video")
                                           ;; Get highest quality MP4
                                           (let [variants (get-in m [:video_info :variants])
                                                 mp4s (->> variants
                                                           (filter #(= (:content_type %) "video/mp4"))
                                                           (sort-by :bitrate >))
                                                 video-url (:url (first mp4s))]
                                             {:type "video"
                                              :url (proxy-video-url video-url)
                                              :poster (:media_url_https m)})
                                           {:type "image"
                                            :url (:media_url_https m)})))
                                  (remove #(nil? (:url %)))))]
                  (when legacy
                    (if is-retweet
                      {:id (:rest_id tweet-result)
                       :text (:full_text retweet-legacy)
                       :created-at (:created_at legacy)
                       :author {:name (:name user-legacy)
                                :screen-name (:screen_name user-legacy)
                                :avatar (:profile_image_url_https user-legacy)}
                       :retweeted-from {:name (:name retweet-author)
                                        :screen-name (:screen_name retweet-author)
                                        :avatar (:profile_image_url_https retweet-author)}
                       :reply-count (:reply_count retweet-legacy)
                       :retweet-count (:retweet_count retweet-legacy)
                       :favorite-count (:favorite_count retweet-legacy)
                       :media media
                       :is-retweet true}
                      {:id (:rest_id tweet-result)
                       :text (:full_text legacy)
                       :created-at (:created_at legacy)
                       :author {:name (:name user-legacy)
                                :screen-name (:screen_name user-legacy)
                                :avatar (:profile_image_url_https user-legacy)}
                       :reply-count (:reply_count legacy)
                       :retweet-count (:retweet_count legacy)
                       :favorite-count (:favorite_count legacy)
                       :media media})))))
         (remove nil?))))

(defn fetch-tweets-by-screen-name
  "Convenience function: fetch tweets by screen name."
  [screen-name credentials oauth-session & [opts]]
  (let [user-response (fetch-user-by-screen-name screen-name credentials oauth-session)
        user-id (get-user-id user-response)]
    (if user-id
      (let [tweets-response (fetch-user-tweets user-id credentials oauth-session opts)]
        {:user (get-in user-response [:data :user_result :result])
         :tweets (extract-tweets tweets-response)
         :raw tweets-response})
      {:error "User not found"
       :raw user-response})))

;; Tweet conversation (detail with replies)

(defn tweet-conversation-request [tweet-id]
  {:method       :get
   :base-url     "https://api.x.com/graphql/Vorskcd2tZ-tc4Gx3zbk4Q/ConversationTimelineV2"
   :query-params {:variables {:focalTweetId            tweet-id
                              :includeHasBirdwatchNotes false
                              :includePromotedContent   false
                              :withBirdwatchNotes       false
                              :withVoice                false
                              :withV2Timeline           true}
                  :features  oauth/default-features}})

(defn parse-tweet-result
  "Parse a single tweet result into our tweet format."
  [tweet-result]
  (let [legacy (get-in tweet-result [:legacy])
        user-legacy (get-in tweet-result [:core :user_result :result :legacy])
        ;; Check for retweet
        retweet-result (get-in legacy [:retweeted_status_result :result])
        retweet-legacy (get-in retweet-result [:legacy])
        retweet-author (get-in retweet-result [:core :user_result :result :legacy])
        is-retweet (some? retweet-legacy)
        ;; Extract media
        source-legacy (if is-retweet retweet-legacy legacy)
        raw-media (get-in source-legacy [:extended_entities :media])
        media (not-empty
               (->> raw-media
                    (map (fn [m]
                           (if (= (:type m) "video")
                             (let [variants (get-in m [:video_info :variants])
                                   mp4s (->> variants
                                             (filter #(= (:content_type %) "video/mp4"))
                                             (sort-by :bitrate >))
                                   video-url (:url (first mp4s))]
                               {:type "video"
                                :url (proxy-video-url video-url)
                                :poster (:media_url_https m)})
                             {:type "image"
                              :url (:media_url_https m)})))
                    (remove #(nil? (:url %)))))]
    (when legacy
      (cond-> {:id (:rest_id tweet-result)
               :text (if is-retweet (:full_text retweet-legacy) (:full_text legacy))
               :created-at (:created_at legacy)
               :in-reply-to (:in_reply_to_status_id_str legacy)
               :conversation-id (:conversation_id_str legacy)
               :author {:name (:name user-legacy)
                        :screen-name (:screen_name user-legacy)
                        :avatar (:profile_image_url_https user-legacy)}
               :retweet-count (if is-retweet (:retweet_count retweet-legacy) (:retweet_count legacy))
               :favorite-count (if is-retweet (:favorite_count retweet-legacy) (:favorite_count legacy))
               :media media}
        is-retweet (assoc :is-retweet true
                          :retweeted-from {:name (:name retweet-author)
                                           :screen-name (:screen_name retweet-author)
                                           :avatar (:profile_image_url_https retweet-author)})))))

(defn extract-conversation
  "Extract the focal tweet and replies from a conversation response."
  [response focal-tweet-id]
  (let [instructions (get-in response [:data :timeline_response :instructions])
        entries (->> instructions
                     (filter #(= (:__typename %) "TimelineAddEntries"))
                     first
                     :entries)
        ;; Find the focal tweet and replies, filtering out promoted content
        all-items (mapcat (fn [entry]
                            (let [entry-id (:entryId entry)
                                  content (:content entry)]
                              (cond
                                ;; Skip promoted tweets
                                (str/includes? entry-id "promoted")
                                nil

                                ;; Single tweet entry
                                (str/starts-with? entry-id "tweet-")
                                (when-let [tweet-result (get-in content [:content :tweetResult :result])]
                                  [(assoc (parse-tweet-result tweet-result)
                                          :entry-type :tweet)])

                                ;; Conversation thread module (replies)
                                (str/starts-with? entry-id "conversationthread-")
                                (->> (get-in content [:items])
                                     (map (fn [item]
                                            (let [item-id (get-in item [:entryId] "")]
                                              ;; Skip promoted items within threads
                                              (when-not (str/includes? item-id "promoted")
                                                (when-let [tweet-result (get-in item [:item :content :tweetResult :result])]
                                                  (assoc (parse-tweet-result tweet-result)
                                                         :entry-type :reply))))))
                                     (remove nil?))

                                :else nil)))
                          entries)
        focal-tweet (first (filter #(= (:id %) focal-tweet-id) all-items))
        ;; Filter replies: must be in the same conversation as the focal tweet
        replies (->> all-items
                     (filter #(= (:entry-type %) :reply))
                     (remove #(= (:id %) focal-tweet-id))
                     ;; Only keep replies that are part of this conversation
                     (filter #(= (:conversation-id %) focal-tweet-id)))]
    {:tweet focal-tweet
     :replies replies}))

(defn fetch-tweet-conversation
  "Fetch a tweet with its replies."
  [tweet-id credentials oauth-session]
  (let [response (oauth/make-request (tweet-conversation-request tweet-id)
                                     credentials
                                     oauth-session)
        parsed (-> response :body (json/parse-string true))]
    (assoc (extract-conversation parsed tweet-id)
           :raw parsed)))
