(ns liberator-tutorial.core
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [compojure.core :refer [defroutes ANY]]
            [liberator.core :refer [resource defresource]]
            [liberator.dev :refer [wrap-trace]]
            [ring.middleware.params :refer [wrap-params]])
  (:import [java.net URI URL]))

;; convert the body to a reader. Useful for testing in the repl
;; where setting the body to a string is much simpler.
(defn body-as-string [ctx]
  (if-let [body (get-in ctx [:request :body])]
    (condp instance? body
      java.lang.String body
      (slurp (io/reader body)))))

;; For PUT and POST parse the body as json and store in the context
;; under the given key.
(defn parse-json [ctx key]
  (when (#{:put :post} (get-in ctx [:request :request-method]))
    (try
      (if-let [body (body-as-string ctx)]
        (let [data (json/read-str body)]
          [false {key data}])
        {:message "No body"})
      (catch Exception e
        (.printStackTrace e)
        {:message (format "IOException: %s" (.getMessage e))}))))

;; For PUT and POST check if the content type is json.
(defn check-content-type [ctx content-types]
  (if (#{:put :post} (get-in ctx [:request :request-method]))
    (or
     (some #{(get-in ctx [:request :headers "content-type"])}
           content-types)
     [false {:message "Unsupported Content-Type"}])
    true))

(defresource parameter [txt]
  :available-media-types ["text/plain"]
  :handle-ok (fn [_] #(str "The text is %s") txt))

(def my-atom (atom nil))

(def dbg-counter (atom 0))

(defresource dbg-resource
  :available-media-types ["text/plain"]
  :allowed-methods [:get :post]
  :handle-ok (fn [_] (format "The counter is %d" @dbg-counter))
  :post! (fn [_] (swap! dbg-counter inc)))

(def posts (ref [])) ;;first used in postbox

(def content (ref ["Replace part or all of this string."])) ;;patchbox

(def entries (ref {}))

;; a helper to create a absolute url for the entry with the given id
(defn build-entry-url [request id]
  (io/URL. (format "%s://%s:%s%s/%s"
                (name (:scheme request))
                (:server-name request)
                (:server-port request)
                (:uri request)
                (str id))))

(comment #(map (fn [id] (str (build-entry-url (get % :request) id)))
                   (keys @entries)))

;; create and list entries
(defresource list-resource
  :available-media-types ["application/json"]
  :allowed-methods [:get :post]
  :known-content-type? #(check-content-type % ["application/json"])
  :malformed? #(parse-json % ::data)
  :post! #(let [id (str (inc (rand-int 100000)))]
            (dosync (alter entries assoc id (::data %)))
            {::id id})
  :post-redirect? true
  :location #(build-entry-url (get % :request) (get % ::id))
  :handle-ok (fn [_]  @entries))

(defresource entry-resource [id]
  :allowed-methods [:get :put :delete]
  :known-content-type? #(check-content-type % ["application/json"])
  :exists? (fn [_]
             (let [e (get @entries id)]
                    (if-not (nil? e)
                      {::entry e})))
  :existed? (fn [_] (nil? (get @entries id ::sentinel)))
  :available-media-types ["application/json"]
  :handle-ok ::entry
  :delete! (fn [_] (dosync (alter entries assoc id nil)))
  :malformed? #(parse-json % ::data)
  :can-put-to-missing? false
  :put! #(dosync (alter entries assoc id (::data %)))
  :new? (fn [_] (nil? (get @entries id ::sentinel))))

(defroutes app
  ;;http://localhost:3000/bar/my%20text
  (ANY "/bar/:txt" [txt] (parameter txt))
  ;;http://localhost:3000/secret?word=tigger
  ;;http://localhost:3000/secret?word=tiger
  (ANY "/secret" []
       (resource :available-media-types ["text/html"]
                 :exists? (fn [ctx]
                            (do
                              (reset! my-atom ctx)

                              (= "tiger" (get-in ctx [:request :query-params "word"]))))
                 :handle-ok (fn [_] (str "You found the secret word! <br><br><b>context:</b> "
                                         @my-atom))
                 :handle-not-found "Uh, that's the wrong word. Guess again!"))
  ;;http://localhost:3000/choice?choice=1
  ;;http://localhost:3000/choice?choice=2
  ;;http://localhost:3000/choice?choice=3
  (ANY "/choice" []
       (resource :available-media-types ["text/html"]
                 :exists? (fn [ctx]
                            (if-let [choice
                                     (get {"1" "stone" "2" "paper" "3" "scissors"}
                                          (get-in ctx [:request :params "choice"]))]
                              {:choice choice}))
                 :handle-ok (fn [ctx]
                              (str "<html>Your choice: "
                                      (get ctx :choice)
                                      " ctx: " ctx "</html>"))
                 :handle-not-found (fn [ctx]
                                     (format "<html>There is no value for the option &quot;%s&quot;"
                                             (get-in ctx [:request :params "choice"] "")))))
  ;;http://localhost:3000/dbg-count
  ;;-> review X-Liberator-Trace headers
  ;;curl -i http://localhost:3000/dbg-count
  ;;curl -i -d '' http://localhost:3000/dbg-count
  (ANY "/dbg-count" [] dbg-resource)

  ;;http://localhost:3000/babel -> text/html
  ;;curl -i http://localhost:3000/babel -> text/plain
  ;;curl -v -H "Accept: application/json" http://localhost:3000/babel
  ;;curl -v -H "Accept: image/png" http://localhost:3000/babel
  (ANY "/babel" []
       (resource :available-media-types ["text/plain" "text/html"
                                         "application/json" "application/clojure;q=0.9"]
                 :handle-ok
                 #(let [media-type
                        (get-in % [:representation :media-type])]
                    (condp = media-type
                      "text/plain" "You requested plain text"
                      "text/html" "<html><h1>You requested HTML</h1></html>"
                      {:message "You requested a media type"
                       :media-type media-type}))
                 :handle-not-acceptable #(let [media-type
                                               (get-in % [:request :headers "accept"])]
                                           (str "Cannot return a representation of this resource for: " media-type))))

  ;;curl -i  http://localhost:3000/changetag
  ;;curl -i -H'If-None-Match: "ijklmnopqr"' http://localhost:3000/changetag
  ;;curl -i -H'If-Match: "ijklmnopqr"' http://localhost:3000/changetag
  (ANY "/changetag" []
       (resource
        :available-media-types ["text/plain"]
        ;; etag changes every 10s
        :etag (let [i (int (mod (/ (System/currentTimeMillis) 10000) 10))]
                (.substring "abcdefhghijklmnopqrst"  i (+ i 10)))
        :handle-ok (format "It's now %s" (java.util.Date.))))

  ;;curl -v -d '' http://localhost:3000/postbox
  ;;curl -v -d 'This is a post' -H "Content-type: text/plain" http://localhost:3000/postbox
  ;;curl -I -X OPTIONS http://localhost:3000/postbox | grep "Allow"
  (ANY "/postbox" []
       (resource
        :allowed-methods [:post :get :head :options]
        :available-media-types ["text/html"]
        :handle-ok (fn [ctx]
                     (format  (str "<html>Post text/plain to this resource.<br>\n"
                                   "There are %d posts at the moment.<br>\n"
                                   ;;"Posts: " (doall (for [post @posts] post))
                                   )
                              (count @posts)))
        :post! (fn [ctx]
                 (dosync
                  (let [body (slurp (get-in ctx [:request :body]))
                        id   (count (alter posts conj body))]
                    {::id id})))
        ;; actually http requires absolute urls for redirect but let's
        ;; keep things simple.
        :post-redirect? (fn [ctx] {:location (format "/postbox/%s" (::id ctx))})))

  ;;curl -i http://localhost:3000/cond-postbox
  ;;-> ETag: "4"
  ;;curl -XPOST -d test -H 'Content-Type: text/plain' -H 'If-Match: "4"' -i http://localhost:3000/cond-postbox
  ;;-> ETag: "5"
  ;;curl -XPOST -d test -H 'Content-Type: text/plain' -H 'If-Match: "4"' -i http://localhost:3000/cond-postbox
  ;;-> 412 Precondition Failed
  ;;-> ETag: "5"
  (ANY "/cond-postbox" []
       (resource
        :allowed-methods [:post :get]
        :available-media-types ["text/html"]
        :handle-ok (fn [ctx]
                     (format  (str "<html>Post text/plain to this resource.<br>\n"
                                   "There are %d posts at the moment.")
                              (count @posts)))
        :post! (fn [ctx]
                 (dosync
                  (let [body (slurp (get-in ctx [:request :body]))
                        id   (count (alter posts conj body))]
                    {::id id})))
        ;; actually http requires absolute urls for redirect but let's
        ;; keep things simple.
        :post-redirect? (fn [ctx] {:location (format "/postbox/%s" (::id ctx))})
        :etag (fn [_] (str (count @posts)))))
  ;;curl -v http://localhost:3000/postbox/2
  (ANY "/postbox/:x" [x]
       (resource
        :allowed-methods [:get :head :options]
        :available-media-types ["text/html"]
        :exists? (fn [ctx] (if-let [d (get @posts (dec (Integer/parseInt x)))] {::data d}))
        :handle-ok ::data))
  ;;
  ;;
  (ANY "/patchbox" []
       (resource
        :allowed-methods [:patch :get :options]
        :available-media-types ["text/html"]
        :handle-ok (fn [ctx]
                     (format  (str "<html><body>\n"
                                   "The current content is:<br/>"
                                   (last @content)
                                   "Patch text/plain to this resource.<br/>"
                                   "Send a string in the format foo|bar.<br/>"
                                   "Any instance of the string to the left<br/>"
                                   "of the '|' will be replaced with the<br/>"
                                   "string to the right.<br/>"
                                   "There have been %d patches issued.<br/>"
                                   "</body></html>")
                              (dec (count @content))))
        :patch-content-types ["text/plain"]
        :patch! (fn [ctx]
                 (dosync
                  (let [body (slurp (get-in ctx [:request :body]))
                        parts (clojure.string/split body #"\|")
                        replaced (clojure.string/replace
                                     (last @content)
                                     (re-pattern (first parts))
                                     (last parts))
                        id   (count (alter content conj replaced))]
                    {::id id})))))
  ;;putting it all together
  (ANY ["/collection/:id", :id #"[0-9]+"] [id] (entry-resource id))
  (ANY "/collection" [] list-resource))

(def handler
  (-> app
      (wrap-trace :header :ui)
      wrap-params))


;; context map ->
;; {:request {:ssl-client-cert nil,
;;            :remote-addr "0:0:0:0:0:0:0:1",
;;            :scheme :http,
;;            :query-params {"word" "tiger"},
;;            :form-params {},
;;            :request-method :get,
;;            :query-string "word=tiger",
;;            :route-params {},
;;            :content-type nil,
;;            :uri "/secret",
;;            :server-name "localhost",
;;            :params {"word" "tiger"},
;;            :headers {"accept-encoding" "gzip, deflate, sdch",
;;                      "cache-control" "max-age=0",
;;                      "connection" "keep-alive",
;;                      "user-agent" "Mozilla/5.0 (Macintosh; Intel...",
;;                      "accept-language" "en-US,en;q=0.8",
;;                      "accept" "text/html,application/xhtml+xml...",
;;                      "host" "localhost:3000",
;;                      "cookie" "ring-session=%2FdNMGg..."
;;                      },
;;            :content-length nil,
;;            :server-port 3000,
;;            :character-encoding nil,
;;            :body # }
;;  :resource {:put-to-different-url? #,
;;             :put! #, :available-charsets #,
;;             :processable? #,
;;             :allowed? #,
;;             :valid-content-header? #,
;;             :available-languages #,
;;             :conflict? #,
;;             :existed? #,
;;             :service-available? #,
;;             :known-methods #,
;;             :delete-enacted? #,
;;             :allowed-methods #,
;;             :exists? #,
;;             :handle-moved-temporarily #,
;;             :handle-moved-permanently #,
;;             :can-post-to-missing? #,
;;             :handle-see-other #,
;;             :known-content-type? #,
;;             :malformed? #,
;;             :moved-permanently? #,
;;             :post! #,
;;             :multiple-representations? #,
;;             :delete! #,
;;             :respond-with-entity? #,
;;             :method-allowed? #,
;;             :uri-too-long? #,
;;             :authorized? #,
;;             :new? #,
;;             :available-media-types #,
;;             :handle-ok #,
;;             :post-redirect? #,
;;             :moved-temporarily? #,
;;             :valid-entity-length? #,
;;             :handle-not-found #,
;;             :known-method? #,
;;             :available-encodings #,
;;             :can-put-to-missing? # }
;;  :representation {:encoding "identity", :media-type "text/html"}}


;; {:representation {:encoding "identity", :media-type "text/html"},
;;  :resource {:put-to-different-url? #,
;;             :put! #,
;;             :available-charsets #,
;;             :processable? #,
;;             :allowed? #,
;;             :valid-content-header? #,
;;             :available-languages #,
;;             :conflict? #,
;;             :existed? #,
;;             :service-available? #,
;;             :known-methods #,
;;             :delete-enacted? #,
;;             :allowed-methods #,
;;             :exists? #,
;;             :handle-moved-temporarily #,
;;             :handle-moved-permanently #,
;;             :can-post-to-missing? #,
;;             :handle-see-other #,
;;             :known-content-type? #,
;;             :malformed? #,
;;             :moved-permanently? #,
;;             :post! #,
;;             :multiple-representations? #,
;;             :delete! #,
;;             :respond-with-entity? #,
;;             :method-allowed? #,
;;             :uri-too-long? #,
;;             :authorized? #,
;;             :new? #,
;;             :available-media-types #,
;;             :handle-ok #,
;;             :post-redirect? #,
;;             :moved-temporarily? #,
;;             :valid-entity-length? #,
;;             :handle-not-found #,
;;             :known-method? #,
;;             :available-encodings #,
;;             :can-put-to-missing? #},
;;  :request {:ssl-client-cert nil,
;;            :remote-addr "0:0:0:0:0:0:0:1",
;;            :scheme :http,
;;            :query-params {"choice" "3"},
;;            :form-params {},
;;            :request-method :get,
;;            :query-string "choice=3",
;;            :route-params {},
;;            :content-type nil,
;;            :uri "/choice",
;;            :server-name "localhost",
;;            :params {"choice" "3"},
;;            :headers {"accept-encoding" "gzip, deflate, sdch",
;;                      "cache-control" "max-age=0",
;;                      "connection" "keep-alive",
;;                      "user-agent" "Mozilla/5.0 (Macintosh; Intel...",
;;                      "accept-language" "en-US,en;q=0.8",
;;                      "accept" "text/html,application/xhtml...",
;;                      "host" "localhost:3000",
;;                      "cookie" "ring-session=%2FdNMGga4EMO..."},
;;            :content-length nil,
;;            :server-port 3000,
;;            :character-encoding nil,
;;            :body #},
;;  :choice "scissors",
;;  :status 200,
;;  :message "OK"}
