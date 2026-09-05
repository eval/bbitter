(ns bbitter.render
  (:require [bbitter.oauth :as oauth]
            [bbitter.cli :as cli]
            [clojure.java.io :as io]
            [selmer.parser :as selmer]))

(defn write-page [path html]
  (io/make-parents path)
  (spit path html)
  (println (str "Wrote " path)))

(defn render-index [accounts tweets]
  (selmer/render (slurp "templates/index.html")
                 {:tweets        tweets
                  :tweet-count   (count tweets)
                  :accounts      (map :user accounts)
                  :account-count (count accounts)
                  :generated-at  (str (java.time.LocalDateTime/now))}))

(defn render-profile [{:keys [user tweets]}]
  (selmer/render (slurp "templates/profile.html")
                 {:user   user
                  :tweets tweets
                  :static true}))

(defn render-site []
  (cli/check-env)
  (let [credentials   (oauth/load-credentials)
        oauth-session (oauth/load-oauth-session)
        accounts      (cli/fetch-accounts credentials oauth-session)
        tweets        (cli/merge-timeline accounts)]
    (println)
    (write-page "public/index.html" (render-index accounts tweets))
    (doseq [account accounts]
      (write-page (str "public/@" (get-in account [:user :screen-name]) "/index.html")
                  (render-profile account)))
    (println)
    (println (str "Generated " (count tweets) " tweets from " (count accounts) " accounts"))))

(defn -main [& _args]
  (render-site))
