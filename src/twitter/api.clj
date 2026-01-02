(ns twitter.api
  (:require [twitter.oauth :as oauth]
            [cheshire.core :as json]
            [clojure.string :as str]))

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
                      is-retweet (some? retweet-legacy)]
                  (when legacy
                    (if is-retweet
                      {:id (:rest_id tweet-result)
                       :text (:full_text retweet-legacy)
                       :created-at (:created_at legacy)
                       :author {:name (:name user-legacy)
                                :screen-name (:screen_name user-legacy)}
                       :retweeted-from {:name (:name retweet-author)
                                        :screen-name (:screen_name retweet-author)}
                       :retweet-count (:retweet_count retweet-legacy)
                       :favorite-count (:favorite_count retweet-legacy)
                       :is-retweet true}
                      {:id (:rest_id tweet-result)
                       :text (:full_text legacy)
                       :created-at (:created_at legacy)
                       :author {:name (:name user-legacy)
                                :screen-name (:screen_name user-legacy)}
                       :retweet-count (:retweet_count legacy)
                       :favorite-count (:favorite_count legacy)})))))
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
