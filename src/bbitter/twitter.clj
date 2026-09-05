(ns bbitter.twitter
  "Fetch and parse user timelines from the X GraphQL API."
  (:require [bbitter.oauth :as oauth]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.time ZonedDateTime Duration]
           [java.time.format DateTimeFormatter]
           [java.util Locale]))

;; Text processing

(defn decode-html-entities
  "Decode common HTML entities in text."
  [text]
  (when text
    (-> text
        (str/replace "&amp;" "&")
        (str/replace "&lt;" "<")
        (str/replace "&gt;" ">")
        (str/replace "&quot;" "\"")
        (str/replace "&#39;" "'")
        (str/replace "&apos;" "'"))))

(defn format-count
  "Format large numbers: 2200 -> '2.2k', 1500000 -> '1.5m'."
  [n]
  (when n
    (cond
      (>= n 1000000) (let [m (/ n 1000000.0)]
                       (if (== (Math/floor m) m)
                         (str (int m) "m")
                         (str (format "%.1f" m) "m")))
      (>= n 1000)    (let [k (/ n 1000.0)]
                       (if (== (Math/floor k) k)
                         (str (int k) "k")
                         (str (format "%.1f" k) "k")))
      :else          (str n))))

(defn truncate-url
  "Truncate a URL for display, max 35 chars."
  [url]
  (when url
    (let [clean (-> url
                    (str/replace #"^https?://" "")
                    (str/replace #"^www\." ""))]
      (if (> (count clean) 35)
        (str (subs clean 0 32) "...")
        clean))))

(defn process-tweet-urls
  "Process URLs in tweet text:
   - Remove media URLs (photo/video)
   - Replace other t.co URLs with clickable links"
  [text url-entities media-entities]
  (if-not text
    text
    (let [media-urls (->> media-entities
                          (map :url)
                          (remove nil?)
                          set)
          url-map (->> url-entities
                       (map (fn [u]
                              (let [display (or (:display_url u)
                                                (truncate-url (:expanded_url u)))
                                    href (:expanded_url u)]
                                [(:url u)
                                 (str "<a href=\"" href "\" title=\"" href "\" target=\"_blank\" rel=\"noopener\" onclick=\"event.stopPropagation();\">" display "</a>")])))
                       (into {}))
          text-no-media (reduce (fn [t url]
                                  (str/replace t (str " " url) ""))
                                text
                                media-urls)
          text-no-media (reduce (fn [t url]
                                  (str/replace t url ""))
                                text-no-media
                                media-urls)
          text-with-urls (reduce (fn [t [tco-url link-html]]
                                   (str/replace t tco-url link-html))
                                 text-no-media
                                 url-map)]
      (str/trim text-with-urls))))

(defn get-full-text
  "Get full tweet text, preferring note_tweet for long tweets.
   Processes URLs to remove media links and expand others."
  [tweet-result legacy]
  (let [note-tweet (get-in tweet-result [:note_tweet :note_tweet_results :result])
        use-note-tweet? (some? (:text note-tweet))
        text (if use-note-tweet? (:text note-tweet) (:full_text legacy))
        url-entities (if use-note-tweet?
                       (get-in note-tweet [:entity_set :urls])
                       (get-in legacy [:entities :urls]))
        media-entities (get-in legacy [:extended_entities :media])]
    (-> text
        (process-tweet-urls url-entities media-entities)
        decode-html-entities)))

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
          days (quot seconds 86400)
          month (str/lower-case (.format parsed (DateTimeFormatter/ofPattern "MMM" Locale/ENGLISH)))]
      (cond
        (< seconds 60) (str seconds "s")
        (< minutes 60) (str minutes "m")
        (< hours 24)   (str hours "h")
        (< days 180)   (str (.getDayOfMonth parsed) " " month)
        :else          (str (.getDayOfMonth parsed) " " month " " (.getYear parsed))))))

;; API requests

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

;; Parsing

(defn- best-mp4-url
  "Pick the highest bitrate MP4 variant of a video media entity."
  [media]
  (->> (get-in media [:video_info :variants])
       (filter #(= (:content_type %) "video/mp4"))
       (sort-by :bitrate >)
       first
       :url))

(defn parse-media
  "Turn raw extended_entities media into a list of {:type :url [:poster]}.
   Videos use the direct mp4 URL; the page loads it with a no-referrer policy."
  [raw-media]
  (not-empty
   (->> raw-media
        (map (fn [m]
               (if (= (:type m) "video")
                 {:type   "video"
                  :url    (best-mp4-url m)
                  :poster (:media_url_https m)}
                 {:type "image"
                  :url  (:media_url_https m)})))
        (remove #(nil? (:url %))))))

(defn- parse-author [user-legacy]
  {:name        (:name user-legacy)
   :screen-name (:screen_name user-legacy)
   :avatar      (:profile_image_url_https user-legacy)})

(defn- parse-quoted-tweet [tweet-result]
  (let [quote-result (get-in tweet-result [:quoted_status_result :result])
        quote-legacy (:legacy quote-result)
        quote-user   (get-in quote-result [:core :user_result :result :legacy])]
    (when quote-legacy
      {:id     (:rest_id quote-result)
       :text   (get-full-text quote-result quote-legacy)
       :author (parse-author quote-user)
       :media  (parse-media (get-in quote-legacy [:extended_entities :media]))})))

(defn- parse-tweet-entry [entry]
  (let [tweet-result   (get-in entry [:content :content :tweetResult :result])
        legacy         (:legacy tweet-result)
        user-legacy    (get-in tweet-result [:core :user_result :result :legacy])
        retweet-result (get-in legacy [:retweeted_status_result :result])
        retweet-legacy (:legacy retweet-result)
        retweet-author (get-in retweet-result [:core :user_result :result :legacy])
        is-retweet     (some? retweet-legacy)
        ;; For a retweet, text, media and counts come from the original tweet.
        source-result  (if is-retweet retweet-result tweet-result)
        source-legacy  (if is-retweet retweet-legacy legacy)
        quoted-tweet   (parse-quoted-tweet tweet-result)]
    (when legacy
      (cond-> {:id             (:rest_id tweet-result)
               :text           (get-full-text source-result source-legacy)
               :created-at     (:created_at legacy)
               :time           (format-relative-date (:created_at legacy))
               :author         (parse-author user-legacy)
               :reply-count    (format-count (:reply_count source-legacy))
               :retweet-count  (format-count (:retweet_count source-legacy))
               :favorite-count (format-count (:favorite_count source-legacy))
               :media          (parse-media (get-in source-legacy [:extended_entities :media]))}
        is-retweet   (assoc :is-retweet true
                            :retweeted-from (parse-author retweet-author))
        quoted-tweet (assoc :quoted-tweet quoted-tweet)))))

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
         (keep parse-tweet-entry))))

(defn parse-user
  "Parse user data into our format."
  [user-result]
  (let [legacy (:legacy user-result)]
    {:id              (:rest_id user-result)
     :name            (:name legacy)
     :screen-name     (:screen_name legacy)
     :bio             (:description legacy)
     :avatar          (str/replace (:profile_image_url_https legacy) "_normal" "_400x400")
     :followers-count (:followers_count legacy)
     :following-count (:friends_count legacy)}))

(defn fetch-tweets-by-screen-name
  "Fetch a user and their latest tweets by screen name.
   Returns {:user ... :tweets ...} or {:error ...}."
  [screen-name credentials oauth-session & [opts]]
  (let [user-response (fetch-user-by-screen-name screen-name credentials oauth-session)
        user-id (get-user-id user-response)]
    (if user-id
      (let [tweets-response (fetch-user-tweets user-id credentials oauth-session opts)
            user-result (get-in user-response [:data :user_result :result])]
        {:user   (parse-user user-result)
         :tweets (vec (extract-tweets tweets-response))})
      {:error "User not found"})))
