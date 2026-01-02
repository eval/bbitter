(ns bbitter.oauth
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import (javax.crypto Mac)
           (javax.crypto.spec SecretKeySpec)
           (java.util Base64)))

(def default-headers
  {;; Note: connection and accept-encoding are managed by http-client
   :content-type          "application/json"
   :x-twitter-active-user "yes"
   :authority             "api.x.com"
   :accept-language       "en-US,en;q=0.9"
   :accept                "*/*"
   :dnt                   "1"})

(def default-features
  "{\"creator_subscriptions_quote_tweet_preview_enabled\":false,\"responsive_web_twitter_blue_verified_badge_is_enabled\":true,\"hidden_profile_likes_enabled\":false,\"responsive_web_graphql_exclude_directive_enabled\":true,\"rweb_tipjar_consumption_enabled\":false,\"graphql_is_translatable_rweb_tweet_is_translatable_enabled\":false,\"responsive_web_edit_tweet_api_enabled\":false,\"responsive_web_grok_analysis_button_from_backend\":false,\"tweet_with_visibility_results_prefer_gql_limited_actions_policy_enabled\":false,\"responsive_web_graphql_timeline_navigation_enabled\":false,\"subscriptions_verification_info_reason_enabled\":true,\"android_graphql_skip_api_media_color_palette\":false,\"rweb_video_timestamps_enabled\":false,\"subscriptions_verification_info_verified_since_enabled\":true,\"blue_business_profile_image_shape_enabled\":false,\"vibe_api_enabled\":false,\"responsive_web_grok_analyze_post_followups_enabled\":false,\"unified_cards_ad_metadata_container_dynamic_card_content_query_enabled\":false,\"responsive_web_text_conversations_enabled\":false,\"responsive_web_graphql_skip_user_profile_image_extensions_enabled\":false,\"responsive_web_media_download_video_enabled\":false,\"super_follow_exclusive_tweet_notifications_enabled\":false,\"subscriptions_verification_info_enabled\":true,\"spaces_2022_h2_spaces_communities\":true,\"view_counts_everywhere_api_enabled\":false,\"responsive_web_jetfuel_frame\":false,\"profile_label_improvements_pcf_label_in_post_enabled\":false,\"rweb_lists_timeline_redesign_enabled\":true,\"responsive_web_grok_analyze_button_fetch_trends_enabled\":false,\"verified_phone_label_enabled\":false,\"responsive_web_twitter_article_tweet_consumption_enabled\":false,\"super_follow_badge_privacy_enabled\":false,\"super_follow_user_api_enabled\":false,\"responsive_web_grok_share_attachment_enabled\":false,\"responsive_web_enhance_cards_enabled\":false,\"creator_subscriptions_tweet_preview_api_enabled\":true,\"c9s_tweet_anatomy_moderator_badge_enabled\":false,\"communities_web_enable_tweet_community_results_fetch\":false,\"standardized_nudges_misinfo\":false,\"super_follow_tweet_api_enabled\":false,\"articles_preview_enabled\":false,\"highlights_tweets_tab_ui_enabled\":false,\"creator_subscriptions_subscription_count_enabled\":false,\"freedom_of_speech_not_reach_fetch_enabled\":false,\"longform_notetweets_inline_media_enabled\":false,\"immersive_video_status_linkable_timestamps\":false,\"responsive_web_grok_image_annotation_enabled\":false,\"longform_notetweets_consumption_enabled\":true,\"articles_api_enabled\":false,\"longform_notetweets_richtext_consumption_enabled\":true,\"interactive_text_enabled\":false,\"tweetypie_unmention_optimization_enabled\":false,\"spaces_2022_h2_clipping\":true,\"premium_content_api_read_enabled\":false,\"tweet_awards_web_tipping_enabled\":false,\"longform_notetweets_rich_text_read_enabled\":false}")

(defn hmac-sign
  "Calculate HMAC signature for given data."
  [^String key ^String data ^String hmac-algo]
  (let [signing-key (SecretKeySpec. (.getBytes key) hmac-algo)
        mac (doto (Mac/getInstance hmac-algo) (.init signing-key))]
    (.encodeToString (Base64/getEncoder)
                     (.doFinal mac (.getBytes data)))))

(defn sign [base-string key]
  (hmac-sign key base-string "HmacSHA1"))

(defn url-encode
  "RFC 3986 encoding (OAuth requires this, not application/x-www-form-urlencoded)."
  [s]
  (-> (java.net.URLEncoder/encode (str s) "UTF-8")
      (.replace "+" "%20")
      (.replace "*" "%2A")
      (.replace "%7E" "~")))

(defn- named? [a]
  (instance? clojure.lang.Named a))

(defn as-str [a]
  (if (named? a)
    (name a)
    (str a)))

(defn signing-key [{:keys [consumer-secret]} {:keys [token-secret]}]
  (str (url-encode consumer-secret) "&" (url-encode token-secret)))

(defn parameter-string [params]
  (str/join "&" (sort (map (fn [[k v]]
                             (str (url-encode (as-str k))
                                  "=" (url-encode (as-str v)))) params))))

(defn request->base-string
  "Turns request into string that is used for signing."
  [{:keys [method base-url query-params form-params oauth-params]}]
  (let [oauth-params-snake-cased (update-keys oauth-params #(-> % name (str/replace #"-" "_")))]
    (str/join "&"
              [(-> method name str/upper-case)
               (-> base-url url-encode)
               (url-encode (parameter-string (merge query-params form-params oauth-params-snake-cased)))])))

(defn add-oauth-params [request credentials oauth-session]
  (let [default-oauth-params {:oauth-consumer-key     (:consumer-key credentials)
                              :oauth-nonce            (str (random-uuid))
                              :oauth-signature-method "HMAC-SHA1"
                              :oauth-timestamp        (quot (System/currentTimeMillis) 1000)
                              :oauth-token            (:token oauth-session)
                              :oauth-version          "1.0"}]
    (update request :oauth-params #(merge default-oauth-params %))))

(defn add-oauth-signature [request credentials oauth-session]
  (let [oauth-signature (-> request
                            (request->base-string)
                            (sign (signing-key credentials oauth-session)))]
    (assoc-in request [:oauth-params :oauth-signature] oauth-signature)))

(defn build-authorization-header [{:keys [oauth-params]}]
  (str "OAuth "
       (str/join ", "
                 (map #(str/join "=" %)
                      (update-vals (update-keys oauth-params #(-> % name (str/replace #"-" "_")))
                                   #(-> % as-str url-encode pr-str))))))

(defn make-request
  "Execute an OAuth-signed request."
  [{:keys [method base-url query-params] :as request} credentials oauth-session]
  ;; Encode variables as JSON string BEFORE computing signature
  (let [query-params (cond-> query-params
                       (:variables query-params)
                       (update :variables json/generate-string))
        request (assoc request :query-params query-params)
        signed-request (-> request
                           (add-oauth-params credentials oauth-session)
                           (add-oauth-signature credentials oauth-session))
        auth-header (build-authorization-header signed-request)]
    (http/request {:method  method
                   :uri     base-url
                   :headers (assoc default-headers "Authorization" auth-header)
                   :query-params query-params})))

(defn load-credentials []
  {:consumer-key    (System/getenv "TWITTER_CONSUMER_KEY")
   :consumer-secret (System/getenv "TWITTER_CONSUMER_SECRET")})

(defn load-oauth-session []
  {:token        (System/getenv "TWITTER_OAUTH_TOKEN")
   :token-secret (System/getenv "TWITTER_OAUTH_TOKEN_SECRET")})
