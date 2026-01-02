(ns bbitter.render
  (:require [bbitter.api :as api]
            [bbitter.oauth :as oauth]
            [bbitter.cli :as cli]
            [clojure.edn :as edn]
            [selmer.parser :as selmer]))

(defn load-accounts []
  (-> (slurp "accounts.edn")
      (edn/read-string)
      :accounts))

(defn fetch-all-tweets [credentials oauth-session]
  (let [accounts (load-accounts)]
    (->> accounts
         (mapcat (fn [handle]
                   (println (str "Fetching @" handle "..."))
                   (let [result (api/fetch-tweets-by-screen-name handle credentials oauth-session)]
                     (if (:error result)
                       (do (println (str "  Error: " (:error result)))
                           [])
                       (:tweets result)))))
         (sort-by #(cli/parse-twitter-date (:created-at %)) #(.compareTo %2 %1)))))

(defn render-site []
  (cli/check-env)
  (let [credentials (oauth/load-credentials)
        oauth-session (oauth/load-oauth-session)
        accounts (load-accounts)
        tweets (fetch-all-tweets credentials oauth-session)
        template (slurp "templates/index.html")
        html (selmer/render template
                            {:tweets tweets
                             :tweet-count (count tweets)
                             :account-count (count accounts)
                             :generated-at (str (java.time.LocalDateTime/now))})]
    (spit "public/index.html" html)
    (println)
    (println (str "Generated public/index.html with " (count tweets) " tweets"))))

(defn -main [& _args]
  (render-site))
