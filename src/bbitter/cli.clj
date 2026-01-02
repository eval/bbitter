(ns bbitter.cli
  (:require [bbitter.api :as api]
            [bbitter.oauth :as oauth]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import (java.time ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.util Locale)))

(def twitter-date-formatter
  (DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:ss Z yyyy" Locale/ENGLISH))

(defn parse-twitter-date [date-str]
  (ZonedDateTime/parse date-str twitter-date-formatter))

(defn parse-args [args]
  (loop [args args
         opts {}]
    (if (empty? args)
      opts
      (let [[arg & rest-args] args]
        (cond
          (= arg "--json")
          (recur rest-args (assoc opts :json true))

          (= arg "--all")
          (recur rest-args (assoc opts :all true))

          (str/starts-with? arg "-")
          (do (println "Unknown option:" arg)
              (recur rest-args opts))

          :else
          (recur rest-args (assoc opts :screen-name arg)))))))

(defn format-tweet [tweet]
  (let [{:keys [text author created-at retweet-count favorite-count is-retweet retweeted-from]} tweet]
    (str "─────────────────────────────────────────\n"
         (if is-retweet
           (str "@" (:screen-name author) " retweeted @" (:screen-name retweeted-from) " · " created-at)
           (str "@" (:screen-name author) " · " created-at))
         "\n\n"
         text "\n"
         "♻️ " retweet-count "  ❤️ " favorite-count)))

(defn print-usage []
  (println "Usage: bb tweets <screen-name> [options]")
  (println "       bb tweets --all [options]")
  (println)
  (println "Options:")
  (println "  --all           Fetch from all accounts in accounts.edn")
  (println "  --json          Output raw JSON"))

(defn load-accounts []
  (-> (slurp "accounts.edn")
      (edn/read-string)
      :accounts))

(defn fetch-all-accounts [credentials oauth-session]
  (let [accounts (load-accounts)]
    (println (str "Fetching from " (count accounts) " accounts...\n"))
    (->> accounts
         (mapcat (fn [handle]
                   (println (str "  @" handle))
                   (let [result (api/fetch-tweets-by-screen-name handle credentials oauth-session)]
                     (if (:error result)
                       (do (println (str "    Error: " (:error result)))
                           [])
                       (:tweets result)))))
         (sort-by #(parse-twitter-date (:created-at %)) #(.compareTo %2 %1)))))

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
      (cond
        (:all opts)
        (do
          (check-env)
          (let [credentials (oauth/load-credentials)
                oauth-session (oauth/load-oauth-session)
                tweets (fetch-all-accounts credentials oauth-session)]
            (println)
            (if (:json opts)
              (println (json/generate-string tweets {:pretty true}))
              (doseq [tweet tweets]
                (println (format-tweet tweet))
                (println)))))

        (:screen-name opts)
        (do
          (check-env)
          (let [credentials (oauth/load-credentials)
                oauth-session (oauth/load-oauth-session)
                result (api/fetch-tweets-by-screen-name (:screen-name opts)
                                                        credentials
                                                        oauth-session)]
            (if (:json opts)
              (println (json/generate-string result {:pretty true}))
              (if (:error result)
                (println "Error:" (:error result))
                (do
                  (println (str "Tweets from @" (:screen-name opts) "\n"))
                  (doseq [tweet (:tweets result)]
                    (println (format-tweet tweet))
                    (println)))))))

        :else
        (print-usage)))))
