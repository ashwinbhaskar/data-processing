(ns data-processing.core
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [camel-snake-kebab.core :as csk]
            [data-processing.process :as p]))

(defn- read-json
  [file-path]
  (-> (slurp file-path)
      (json/parse-string csk/->kebab-case-keyword)))


(defn- append-to-file
  [file-name content]
  (spit file-name content :append :true))

(defn- write-to-file-csv
  [content file-name]
  (cond
    (map? content) (as-> (vals content) r
                         (str/join "," r)
                         (str r "\n")
                         (append-to-file file-name r))
    (or (vector? content)
        (seq? content)) (->> content
                             (run! #(write-to-file-csv % file-name)))
    :default (append-to-file file-name (str content "\n"))))

(defn- write-to-file-json
  [map file-name]
  (spit file-name (json/generate-string map)))

(defn -main
  [& _]
  (let [json (read-json "lipstick.json")]
    (-> (get-in json [:mods :list-items])
        p/product-details
        (write-to-file-csv "first.csv"))
    (-> (get-in json [:mods :list-items])
        p/avg-price
        (write-to-file-csv "second.csv"))
    (-> (get-in json [:mods :list-items])
        p/group-by-brand
        (write-to-file-json "third.json"))
    (-> (p/unique-images json)
        (write-to-file-csv "fourth.csv"))))

(comment
  (-> (read-json "/Users/ashwinbhaskar/Downloads/lipstick.json")
      (get-in [:mods :list-items])
      p/product-details
      (write-to-file-csv "first.csv"))
  (-> (read-json "/Users/ashwinbhaskar/Downloads/lipstick.json")
      (get-in [:mods :list-items])
      p/avg-price
      (write-to-file-csv "second.csv"))
  (-> (read-json "/Users/ashwinbhaskar/Downloads/lipstick.json")
      (get-in [:mods :list-items])
      p/group-by-brand
      (write-to-file-json "third.json"))
  (-> (read-json "/Users/ashwinbhaskar/Downloads/lipstick.json")
      p/unique-images
      (write-to-file-csv "fourth.csv")))
