(ns bbitter.server
  (:require [babashka.http-client :as http]
            [bbitter.api :as api]
            [bbitter.oauth :as oauth]
            [bbitter.cli :as cli]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [selmer.parser :as selmer])
  (:import [java.net ServerSocket URLDecoder]
           [java.io BufferedReader InputStreamReader OutputStream]))

(def mime-types
  {"html" "text/html"
   "css"  "text/css"
   "js"   "application/javascript"
   "png"  "image/png"
   "jpg"  "image/jpeg"
   "jpeg" "image/jpeg"
   "gif"  "image/gif"
   "svg"  "image/svg+xml"
   "mp4"  "video/mp4"})

(defn get-mime-type [path]
  (let [ext (last (str/split path #"\."))]
    (get mime-types ext "application/octet-stream")))

(defn parse-request [reader]
  (let [request-line (.readLine reader)
        [method path _] (when request-line (str/split request-line #" "))]
    {:method method
     :path   (when path (URLDecoder/decode path "UTF-8"))}))

(defn send-response [^OutputStream out status headers body]
  (let [header-str (str "HTTP/1.1 " status "\r\n"
                        (str/join "\r\n" (map (fn [[k v]] (str k ": " v)) headers))
                        "\r\n\r\n")]
    (.write out (.getBytes header-str))
    (when body
      (if (bytes? body)
        (.write out ^bytes body)
        (.write out (.getBytes (str body)))))
    (.flush out)))

(defn serve-static [path public-dir]
  (let [safe-path (-> path
                      (str/replace #"^/" "")
                      (str/replace #"\.\." ""))
        safe-path (if (or (empty? safe-path) (str/ends-with? safe-path "/"))
                    (str safe-path "index.html")
                    safe-path)
        file (io/file public-dir safe-path)]
    (if (.exists file)
      {:status  "200 OK"
       :headers {"Content-Type"   (get-mime-type safe-path)
                 "Content-Length" (str (.length file))}
       :body    (let [ba (byte-array (.length file))]
                  (with-open [is (io/input-stream file)]
                    (.read is ba))
                  ba)}
      {:status  "404 Not Found"
       :headers {"Content-Type" "text/plain"}
       :body    "Not Found"})))

(defn stream-video [url ^OutputStream out]
  (try
    (let [response (http/get url {:headers {"User-Agent" "Mozilla/5.0"}
                                  :as      :stream})
          content-type (get-in response [:headers "content-type"] "video/mp4")
          content-length (get-in response [:headers "content-length"])
          header-str (str "HTTP/1.1 200 OK\r\n"
                          "Content-Type: " content-type "\r\n"
                          (when content-length (str "Content-Length: " content-length "\r\n"))
                          "Cache-Control: max-age=604800\r\n"
                          "\r\n")]
      (.write out (.getBytes header-str))
      (with-open [in (:body response)]
        (let [buf (byte-array 8192)]
          (loop []
            (let [n (.read in buf)]
              (when (pos? n)
                (.write out buf 0 n)
                (recur))))))
      (.flush out)
      :streamed)
    (catch Exception e
      (when-not (str/includes? (str (.getMessage e)) "Broken pipe")
        (println "Video proxy error:" (.getMessage e)))
      :streamed)))

;; Lazy-loaded credentials
(def credentials (delay (oauth/load-credentials)))
(def oauth-session (delay (oauth/load-oauth-session)))

(defn render-tweet-page [tweet-id]
  (try
    (cli/check-env)
    (println (str "Fetching tweet " tweet-id "..."))
    (let [result (api/fetch-tweet-conversation tweet-id @credentials @oauth-session)
          template (slurp "templates/tweet.html")]
      (if (:tweet result)
        {:status  "200 OK"
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body    (selmer/render template
                                 {:tweet       (:tweet result)
                                  :replies     (:replies result)
                                  :reply-count (count (:replies result))})}
        {:status  "404 Not Found"
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body    (selmer/render template {:tweet nil})}))
    (catch Exception e
      (println "Error fetching tweet:" (.getMessage e))
      {:status  "500 Internal Server Error"
       :headers {"Content-Type" "text/plain"}
       :body    (str "Error: " (.getMessage e))})))

(defn render-profile-page [screen-name]
  (try
    (cli/check-env)
    (println (str "Fetching profile @" screen-name "..."))
    (let [result (api/fetch-tweets-by-screen-name screen-name @credentials @oauth-session)
          template (slurp "templates/profile.html")]
      (if (:user result)
        {:status  "200 OK"
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body    (selmer/render template
                                 {:user   (:user result)
                                  :tweets (:tweets result)})}
        {:status  "404 Not Found"
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body    (selmer/render template {:user nil})}))
    (catch Exception e
      (println "Error fetching profile:" (.getMessage e))
      {:status  "500 Internal Server Error"
       :headers {"Content-Type" "text/plain"}
       :body    (str "Error: " (.getMessage e))})))

(defn handle-request [request public-dir ^OutputStream out]
  (let [{:keys [path]} request]
    (cond
      ;; Profile page: /@username
      (re-matches #"/@([a-zA-Z0-9_]+)" path)
      (let [[_ screen-name] (re-matches #"/@([a-zA-Z0-9_]+)" path)]
        (render-profile-page screen-name))

      ;; Tweet detail page: /tweet/123456...
      (re-matches #"/tweet/(\d+)" path)
      (let [[_ tweet-id] (re-matches #"/tweet/(\d+)" path)]
        (render-tweet-page tweet-id))

      ;; Video proxy: /proxy/video?url=...
      (str/starts-with? path "/proxy/video")
      (let [query-start (str/index-of path "?")
            query-str   (when query-start (subs path (inc query-start)))
            params      (when query-str
                          (->> (str/split query-str #"&")
                               (map #(str/split % #"=" 2))
                               (filter #(= 2 (count %)))
                               (into {})))
            video-url   (get params "url")]
        (if (and video-url (str/includes? video-url "twimg.com"))
          (stream-video (URLDecoder/decode video-url "UTF-8") out)
          {:status  "400 Bad Request"
           :headers {"Content-Type" "text/plain"}
           :body    "Invalid video URL"}))

      ;; Static files
      :else
      (serve-static path public-dir))))

(defn start-server [port public-dir]
  (let [server (ServerSocket. port)]
    (println (str "Server running at http://localhost:" port " (with video proxy)"))
    (println (str "Serving files from: " public-dir))
    (while true
      (try
        (let [socket  (.accept server)
              reader  (BufferedReader. (InputStreamReader. (.getInputStream socket)))
              out     (.getOutputStream socket)
              request (parse-request reader)]
          (when (:method request)
            (let [response (handle-request request public-dir out)]
              ;; If response is :streamed, headers were already sent
              (when (map? response)
                (send-response out (:status response) (:headers response) (:body response)))))
          (.close socket))
        (catch Exception e
          (when-not (str/includes? (str (.getMessage e)) "Broken pipe")
            (println "Error handling request:" (.getMessage e))))))))

(defn -main [& args]
  (let [port       (Integer/parseInt (or (first args) "1889"))
        public-dir (or (second args) "public")]
    (start-server port public-dir)))
