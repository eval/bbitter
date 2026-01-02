(ns twitter.cli
  (:require [twitter.api :as api]
            [twitter.oauth :as oauth]
            [cheshire.core :as json]
            [clojure.string :as str]))

(defn parse-args [args]
  (loop [args args
         opts {:count 20}]
    (if (empty? args)
      opts
      (let [[arg & rest-args] args]
        (cond
          (or (= arg "-n") (= arg "--count"))
          (recur (rest rest-args) (assoc opts :count (parse-long (first rest-args))))

          (= arg "--json")
          (recur rest-args (assoc opts :json true))

          (str/starts-with? arg "-")
          (do (println "Unknown option:" arg)
              (recur rest-args opts))

          :else
          (recur rest-args (assoc opts :screen-name arg)))))))

(defn format-tweet [tweet]
  (let [{:keys [text author created-at retweet-count favorite-count]} tweet]
    (str "─────────────────────────────────────────\n"
         "@" (:screen-name author) " · " created-at "\n\n"
         text "\n"
         "♻️ " retweet-count "  ❤️ " favorite-count)))

(defn print-usage []
  (println "Usage: bb tweets <screen-name> [options]")
  (println)
  (println "Options:")
  (println "  -n, --count N   Number of tweets to fetch (default: 20)")
  (println "  --json          Output raw JSON"))

(defn check-env []
  (let [required ["TWITTER_CONSUMER_KEY" "TWITTER_CONSUMER_SECRET"
                  "TWITTER_OAUTH_TOKEN" "TWITTER_OAUTH_TOKEN_SECRET"]
        missing (filter #(nil? (System/getenv %)) required)]
    (when (seq missing)
      (println "Missing environment variables:")
      (doseq [v missing]
        (println "  " v))
      (System/exit 1))))

(defn -main [& args]
  (if (empty? args)
    (print-usage)
    (let [opts (parse-args args)]
      (if (nil? (:screen-name opts))
        (print-usage)
        (do
          (check-env)
          (let [credentials (oauth/load-credentials)
                oauth-session (oauth/load-oauth-session)
                result (api/fetch-tweets-by-screen-name (:screen-name opts)
                                                        credentials
                                                        oauth-session
                                                        {:count (:count opts)})]
            (if (:json opts)
              (println (json/generate-string result {:pretty true}))
              (if (:error result)
                (println "Error:" (:error result))
                (do
                  (println (str "Tweets from @" (:screen-name opts) "\n"))
                  (doseq [tweet (:tweets result)]
                    (println (format-tweet tweet))
                    (println)))))))))))
