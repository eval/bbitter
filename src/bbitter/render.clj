(ns bbitter.render
  "bb render: read tmp/feed.edn, write the static site into public/."
  (:require [bbitter.fetch :as fetch]
            [bbitter.twitter :as twitter]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [selmer.parser :as selmer]))

(def templates-dir "templates")
(def public-dir "public")

(defn load-feed []
  (let [file (io/file fetch/feed-file)]
    (when-not (.exists file)
      (println (str "No " fetch/feed-file " found. Run `bb fetch` first."))
      (System/exit 1))
    (edn/read-string (slurp file))))

(defn merge-timeline
  "Merge the tweets of all accounts into one list, newest first."
  [accounts]
  (->> accounts
       (mapcat :tweets)
       (sort-by #(twitter/parse-twitter-date (:created-at %)) #(compare %2 %1))))

(defn status-url [screen-name id]
  (str "https://x.com/" screen-name "/status/" id))

(defn link-tweet
  "Add link fields so the templates need no logic.
   local-names is the set of lower-cased screen names that have a page."
  [local-names tweet]
  (let [local? (fn [author] (contains? local-names (str/lower-case (:screen-name author ""))))
        shown  (if (:is-retweet tweet) (:retweeted-from tweet) (:author tweet))]
    (cond-> (assoc tweet
                   :url (status-url (:screen-name shown) (:id tweet))
                   :author-local (local? (:author tweet)))
      (:retweeted-from tweet)
      (assoc :retweeted-from-local (local? (:retweeted-from tweet)))

      (:quoted-tweet tweet)
      (assoc-in [:quoted-tweet :url]
                (status-url (get-in tweet [:quoted-tweet :author :screen-name])
                            (get-in tweet [:quoted-tweet :id]))))))

(defn write-page [path html]
  (io/make-parents path)
  (spit path html)
  (println (str "Wrote " path)))

(defn render-site [{:keys [generated-at accounts]}]
  (let [users       (map :user accounts)
        local-names (set (map #(str/lower-case (:screen-name %)) users))
        link        (partial link-tweet local-names)
        tweets      (map link (merge-timeline accounts))]
    (selmer/set-resource-path! (.getAbsolutePath (io/file templates-dir)))
    (write-page (str public-dir "/index.html")
                (selmer/render-file "index.html"
                                    {:root          ""
                                     :accounts      users
                                     :account-count (count users)
                                     :tweets        tweets
                                     :tweet-count   (count tweets)
                                     :generated-at  generated-at}))
    (doseq [{:keys [user tweets]} accounts]
      (write-page (str public-dir "/@" (:screen-name user) "/index.html")
                  (selmer/render-file "profile.html"
                                      {:root   "../"
                                       :user   user
                                       :tweets (map link tweets)})))
    (write-page (str public-dir "/style.css") (slurp (io/file templates-dir "style.css")))
    (write-page (str public-dir "/.nojekyll") "")
    (println)
    (println (str "Rendered " (count tweets) " tweets from " (count accounts) " accounts"))))

(defn -main [& _args]
  (render-site (load-feed)))
