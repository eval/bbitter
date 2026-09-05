(ns bbitter.fetch
  "bb fetch: read accounts.edn, fetch each account, write tmp/feed.edn."
  (:require [bbitter.oauth :as oauth]
            [bbitter.twitter :as twitter]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def feed-file "tmp/feed.edn")

(defn parse-accounts
  "Split a string of handles on commas or whitespace, dropping any leading @."
  [s]
  (->> (str/split (str/trim s) #"[\s,]+")
       (remove str/blank?)
       (mapv #(str/replace % #"^@" ""))))

(defn load-accounts
  "Read the handle list from the BBITTER_ACCOUNTS env var, or accounts.edn as a
   local fallback. The env var wins so the list can live in a secret."
  []
  (let [env (System/getenv "BBITTER_ACCOUNTS")]
    (cond
      (not (str/blank? env))
      (parse-accounts env)

      (.exists (io/file "accounts.edn"))
      (:accounts (edn/read-string (slurp "accounts.edn")))

      :else
      (do (println "No accounts. Set BBITTER_ACCOUNTS or create accounts.edn.")
          (System/exit 1)))))

(defn check-env
  "Exit with a message when one of the four secrets is missing."
  []
  (let [required ["TWITTER_CONSUMER_KEY" "TWITTER_CONSUMER_SECRET"
                  "TWITTER_OAUTH_TOKEN" "TWITTER_OAUTH_TOKEN_SECRET"]
        missing (filter #(nil? (System/getenv %)) required)]
    (when (seq missing)
      (println "Missing environment variables:")
      (doseq [v missing]
        (println "  " v))
      (System/exit 1))))

(defn fetch-accounts
  "Fetch user and latest tweets for every handle. Failed handles are skipped."
  [handles credentials oauth-session]
  (->> handles
       (keep (fn [handle]
               (println (str "  @" handle))
               (let [result (twitter/fetch-tweets-by-screen-name handle credentials oauth-session)]
                 (if (:error result)
                   (println (str "    Error: " (:error result)))
                   result))))
       (doall)))

(defn -main [& _args]
  (check-env)
  (let [credentials   (oauth/load-credentials)
        oauth-session (oauth/load-oauth-session)
        handles       (load-accounts)
        _             (println (str "Fetching " (count handles) " accounts..."))
        accounts      (fetch-accounts handles credentials oauth-session)
        feed          {:generated-at (str (java.time.Instant/now))
                       :accounts     (vec accounts)}]
    (io/make-parents feed-file)
    (spit feed-file (pr-str feed))
    (println)
    (println (str "Wrote " feed-file " with "
                  (reduce + (map (comp count :tweets) accounts))
                  " tweets from " (count accounts) " accounts"))))
