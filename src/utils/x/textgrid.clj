(ns utils.textgrid
  (require [clojure.data.json :as json])
  )

(def TG-WS-URL "http://local/py/textgrid.py/tojson?tg_url=")

(defn tg-json-ws [fname]
  (let [url (str TG-WS-URL fname)]
    (slurp url)))

(defn tg->json [fname]
  (json/read-json (tg-json-ws fname)))

(comment
(def tg-cached->json [fname]
  (let [cached-file (str fname ".json")]
    (if (fs/exists? cached-file)
      (json/read cached-file)
      (let [json-txt (tg-json-ws fname)]
        (do
          (u/write-p json-txt (str fname ".json"))
          (json/read json-txt))))))
        )