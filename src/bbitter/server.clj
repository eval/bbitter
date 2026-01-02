(ns bbitter.server
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str])
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
      (println "Video proxy error:" (.getMessage e))
      {:status  "502 Bad Gateway"
       :headers {"Content-Type" "text/plain"}
       :body    "Failed to fetch video"})))

(defn handle-request [request public-dir ^OutputStream out]
  (let [{:keys [path]} request]
    (cond
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
          (println "Error handling request:" (.getMessage e)))))))

(defn -main [& args]
  (let [port       (Integer/parseInt (or (first args) "1889"))
        public-dir (or (second args) "public")]
    (start-server port public-dir)))
