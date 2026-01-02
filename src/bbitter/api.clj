(ns bbitter.api
  (:require [babashka.http-client :as http]
            [bbitter.oauth :as oauth]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.net URLEncoder]
           [java.time ZonedDateTime Instant Duration]
           [java.time.format DateTimeFormatter]
           [java.util Locale]))

;; Text processing

(defn strip-leading-mentions
  "Remove leading @mentions from reply text."
  [text]
  (when text
    (str/trim (str/replace text #"^(@\w+\s*)+" ""))))

;; Date formatting

(def twitter-date-formatter
  (DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:ss Z yyyy" Locale/ENGLISH))

(defn parse-twitter-date [date-str]
  (ZonedDateTime/parse date-str twitter-date-formatter))

(defn format-relative-date
  "Format a Twitter date string as a relative timestamp.
   - < 1 min: '45s'
   - < 1 hour: '5m'
   - < 24 hours: '3h'
   - < 6 months: '31 dec'
   - >= 6 months: '6 jul 2025'"
  [date-str]
  (when date-str
    (let [parsed (parse-twitter-date date-str)
          now (ZonedDateTime/now)
          seconds (.getSeconds (Duration/between parsed now))
          minutes (quot seconds 60)
          hours (quot seconds 3600)
          days (quot seconds 86400)]
      (cond
        (< seconds 60)   (str seconds "s")
        (< minutes 60)   (str minutes "m")
        (< hours 24)     (str hours "h")
        (< days 180)     (str (.getDayOfMonth parsed) " "
                              (str/lower-case (.format parsed (DateTimeFormatter/ofPattern "MMM" Locale/ENGLISH))))
        :else            (str (.getDayOfMonth parsed) " "
                              (str/lower-case (.format parsed (DateTimeFormatter/ofPattern "MMM" Locale/ENGLISH)))
                              " " (.getYear parsed))))))

;; Video proxy

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
                                  (remove #(nil? (:url %)))))
                      ;; Extract quote tweet
                      quote-result (get-in tweet-result [:quoted_status_result :result])
                      quote-legacy (:legacy quote-result)
                      quote-user (get-in quote-result [:core :user_result :result :legacy])
                      quote-media-raw (get-in quote-legacy [:extended_entities :media])
                      quote-media (not-empty
                                   (->> quote-media-raw
                                        (map (fn [m]
                                               (if (= (:type m) "video")
                                                 {:type "video"
                                                  :url (proxy-video-url (-> (get-in m [:video_info :variants])
                                                                            (->> (filter #(= (:content_type %) "video/mp4"))
                                                                                 (sort-by :bitrate >)
                                                                                 first :url)))
                                                  :poster (:media_url_https m)}
                                                 {:type "image"
                                                  :url (:media_url_https m)})))
                                        (remove #(nil? (:url %)))))
                      quoted-tweet (when quote-legacy
                                     {:id (:rest_id quote-result)
                                      :text (:full_text quote-legacy)
                                      :author {:name (:name quote-user)
                                               :screen-name (:screen_name quote-user)
                                               :avatar (:profile_image_url_https quote-user)}
                                      :media quote-media})]
                  (when legacy
                    (cond-> (if is-retweet
                              {:id (:rest_id tweet-result)
                               :text (:full_text retweet-legacy)
                               :created-at (:created_at legacy)
                               :time (format-relative-date (:created_at legacy))
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
                               :time (format-relative-date (:created_at legacy))
                               :author {:name (:name user-legacy)
                                        :screen-name (:screen_name user-legacy)
                                        :avatar (:profile_image_url_https user-legacy)}
                               :reply-count (:reply_count legacy)
                               :retweet-count (:retweet_count legacy)
                               :favorite-count (:favorite_count legacy)
                               :media media})
                      quoted-tweet (assoc :quoted-tweet quoted-tweet))))))
         (remove nil?))))

(defn parse-user
  "Parse user data into our format."
  [user-result]
  (let [legacy (:legacy user-result)]
    {:id (:rest_id user-result)
     :name (:name legacy)
     :screen-name (:screen_name legacy)
     :bio (:description legacy)
     :avatar (str/replace (:profile_image_url_https legacy) "_normal" "_400x400")
     :followers-count (:followers_count legacy)
     :following-count (:friends_count legacy)}))

(defn fetch-tweets-by-screen-name
  "Convenience function: fetch tweets by screen name."
  [screen-name credentials oauth-session & [opts]]
  (let [user-response (fetch-user-by-screen-name screen-name credentials oauth-session)
        user-id (get-user-id user-response)]
    (if user-id
      (let [tweets-response (fetch-user-tweets user-id credentials oauth-session opts)
            user-result (get-in user-response [:data :user_result :result])]
        {:user (parse-user user-result)
         :tweets (extract-tweets tweets-response)
         :raw tweets-response})
      {:error "User not found"
       :raw user-response})))

;; Top tweets (highlights) - uses UserTweets endpoint with guest token auth

(def twitter-bearer-token
  "AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs=1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA")

(def user-tweets-features
  "{\"rweb_video_screen_enabled\":false,\"profile_label_improvements_pcf_label_in_post_enabled\":true,\"responsive_web_profile_redirect_enabled\":false,\"rweb_tipjar_consumption_enabled\":true,\"verified_phone_label_enabled\":false,\"creator_subscriptions_tweet_preview_api_enabled\":true,\"responsive_web_graphql_timeline_navigation_enabled\":true,\"responsive_web_graphql_skip_user_profile_image_extensions_enabled\":false,\"premium_content_api_read_enabled\":false,\"communities_web_enable_tweet_community_results_fetch\":true,\"c9s_tweet_anatomy_moderator_badge_enabled\":true,\"responsive_web_grok_analyze_button_fetch_trends_enabled\":false,\"responsive_web_grok_analyze_post_followups_enabled\":false,\"responsive_web_jetfuel_frame\":true,\"responsive_web_grok_share_attachment_enabled\":true,\"responsive_web_grok_annotations_enabled\":false,\"articles_preview_enabled\":true,\"responsive_web_edit_tweet_api_enabled\":true,\"graphql_is_translatable_rweb_tweet_is_translatable_enabled\":true,\"view_counts_everywhere_api_enabled\":true,\"longform_notetweets_consumption_enabled\":true,\"responsive_web_twitter_article_tweet_consumption_enabled\":true,\"tweet_awards_web_tipping_enabled\":false,\"responsive_web_grok_show_grok_translated_post\":false,\"responsive_web_grok_analysis_button_from_backend\":true,\"creator_subscriptions_quote_tweet_preview_enabled\":false,\"freedom_of_speech_not_reach_fetch_enabled\":true,\"standardized_nudges_misinfo\":true,\"tweet_with_visibility_results_prefer_gql_limited_actions_policy_enabled\":true,\"longform_notetweets_rich_text_read_enabled\":true,\"longform_notetweets_inline_media_enabled\":true,\"responsive_web_grok_image_annotation_enabled\":true,\"responsive_web_grok_imagine_annotation_enabled\":true,\"responsive_web_grok_community_note_auto_translation_is_enabled\":false,\"responsive_web_enhance_cards_enabled\":false}")

(defn get-guest-token
  "Get a guest token from Twitter."
  []
  (println "[guest-token] Fetching...")
  (let [response (http/post "https://api.x.com/1.1/guest/activate.json"
                            {:headers {"Authorization" (str "Bearer " twitter-bearer-token)}})
        token (-> response :body (json/parse-string true) :guest_token)]
    (println "[guest-token]" token)
    token))

(defn extract-user-tweets
  "Extract tweets from UserTweets response."
  [response]
  (let [instructions (get-in response [:data :user :result :timeline :timeline :instructions])
        entries (->> instructions
                     (filter #(= (:type %) "TimelineAddEntries"))
                     first
                     :entries)]
    (->> entries
         (filter #(str/starts-with? (get % :entryId "") "tweet-"))
         (map (fn [entry]
                (let [tweet-result (get-in entry [:content :itemContent :tweet_results :result])
                      legacy (:legacy tweet-result)
                      user-result (get-in tweet-result [:core :user_results :result])
                      user-core (:core user-result)
                      user-avatar (get-in user-result [:avatar :image_url])
                      ;; Extract media
                      raw-media (get-in legacy [:extended_entities :media])
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
                                  (remove #(nil? (:url %)))))
                      ;; Extract quote tweet
                      quote-result (get-in tweet-result [:quoted_status_result :result])
                      quote-legacy (:legacy quote-result)
                      quote-user-result (get-in quote-result [:core :user_results :result])
                      quote-user-core (:core quote-user-result)
                      quote-user-avatar (get-in quote-user-result [:avatar :image_url])
                      quote-media-raw (get-in quote-legacy [:extended_entities :media])
                      quote-media (not-empty
                                   (->> quote-media-raw
                                        (map (fn [m]
                                               (if (= (:type m) "video")
                                                 {:type "video"
                                                  :url (proxy-video-url (-> (get-in m [:video_info :variants])
                                                                            (->> (filter #(= (:content_type %) "video/mp4"))
                                                                                 (sort-by :bitrate >)
                                                                                 first :url)))
                                                  :poster (:media_url_https m)}
                                                 {:type "image"
                                                  :url (:media_url_https m)})))
                                        (remove #(nil? (:url %)))))
                      quoted-tweet (when quote-legacy
                                     {:id (:rest_id quote-result)
                                      :text (:full_text quote-legacy)
                                      :author {:name (:name quote-user-core)
                                               :screen-name (:screen_name quote-user-core)
                                               :avatar (when quote-user-avatar
                                                         (str/replace quote-user-avatar "_normal" "_400x400"))}
                                      :media quote-media})]
                  (when legacy
                    (cond-> {:id (:rest_id tweet-result)
                             :text (:full_text legacy)
                             :created-at (:created_at legacy)
                             :time (format-relative-date (:created_at legacy))
                             :author {:name (:name user-core)
                                      :screen-name (:screen_name user-core)
                                      :avatar (when user-avatar (str/replace user-avatar "_normal" "_400x400"))}
                             :reply-count (:reply_count legacy)
                             :retweet-count (:retweet_count legacy)
                             :favorite-count (:favorite_count legacy)
                             :media media}
                      quoted-tweet (assoc :quoted-tweet quoted-tweet))))))
         (remove nil?))))

(defn fetch-user-highlights
  "Fetch highlights/top tweets for a user using guest token auth."
  [user-id]
  (let [guest-token (get-guest-token)
        variables (json/generate-string {:userId user-id
                                         :count 20
                                         :includePromotedContent false
                                         :withQuickPromoteEligibilityTweetFields false
                                         :withVoice false})
        _ (println "[highlights] Fetching for user" user-id)
        response (http/get "https://api.x.com/graphql/Wms1GvIiHXAPBaCr9KblaA/UserTweets"
                           {:headers {"Authorization" (str "Bearer " twitter-bearer-token)
                                      "x-guest-token" guest-token
                                      "Content-Type" "application/json"}
                            :query-params {"variables" variables
                                           "features" user-tweets-features
                                           "fieldToggles" "{\"withArticlePlainText\":false}"}
                            :throw false})
        _ (println "[highlights] Status:" (:status response))]
    (if (= 200 (:status response))
      (extract-user-tweets (json/parse-string (:body response) true))
      (do
        (println "[highlights] Error:" (:body response))
        nil))))

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
                    (remove #(nil? (:url %)))))
        ;; Extract quote tweet
        quote-result (get-in tweet-result [:quoted_status_result :result])
        quote-legacy (:legacy quote-result)
        quote-user (get-in quote-result [:core :user_result :result :legacy])
        quote-media-raw (get-in quote-legacy [:extended_entities :media])
        quote-media (not-empty
                     (->> quote-media-raw
                          (map (fn [m]
                                 (if (= (:type m) "video")
                                   {:type "video"
                                    :url (proxy-video-url (-> (get-in m [:video_info :variants])
                                                              (->> (filter #(= (:content_type %) "video/mp4"))
                                                                   (sort-by :bitrate >)
                                                                   first :url)))
                                    :poster (:media_url_https m)}
                                   {:type "image"
                                    :url (:media_url_https m)})))
                          (remove #(nil? (:url %)))))
        quoted-tweet (when quote-legacy
                       {:id (:rest_id quote-result)
                        :text (:full_text quote-legacy)
                        :author {:name (:name quote-user)
                                 :screen-name (:screen_name quote-user)
                                 :avatar (:profile_image_url_https quote-user)}
                        :media quote-media})
        in-reply-to (:in_reply_to_status_id_str legacy)
        raw-text (if is-retweet (:full_text retweet-legacy) (:full_text legacy))
        text (if in-reply-to (strip-leading-mentions raw-text) raw-text)]
    (when legacy
      (cond-> {:id (:rest_id tweet-result)
               :text text
               :created-at (:created_at legacy)
               :time (format-relative-date (:created_at legacy))
               :in-reply-to in-reply-to
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
                                           :avatar (:profile_image_url_https retweet-author)})
        quoted-tweet (assoc :quoted-tweet quoted-tweet)))))

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
