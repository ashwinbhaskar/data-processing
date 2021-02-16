(ns data-processing.process
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn product-details
  [list-items]
  (->> list-items
       (map #(select-keys % [:product-url :price :original-price :skus]))
       (map (fn [{skus :skus :as all}]
              (assoc all :skus (count skus))))))

(defn avg-price
  [list-items]
  (let [prices (->> list-items
                    (filter (fn [{:keys [skus brand-name]}]
                              (and (> (count skus) 2)
                                   (= brand-name "OEM"))))
                    (map :price)
                    (map #(Double/parseDouble %)))]
    (-> (reduce + prices)
        (/ (count prices))
        double)))

(defn group-by-brand
  [list-items]
  (->> list-items
       (group-by :brand-name)
       (map (fn [[k v]]
              {k (count v)}))
       (into {})))

(defn- find-unique-values-at-any-depth
  ([map k]
   (find-unique-values-at-any-depth map k #{}))
  ([data-structure k unique-set]
   (cond
     (vector? data-structure) (->> data-structure
                                   (map #(find-unique-values-at-any-depth % k unique-set))
                                   (reduce set/union))
     (map? data-structure) (let [v           (get data-structure k)
                                 updated-set (conj unique-set v)]
                             (->> (vals data-structure)
                                  (map #(find-unique-values-at-any-depth % k updated-set))
                                  (reduce set/union)))
     :default unique-set)))

(defn unique-images
  [m]
  (->> (find-unique-values-at-any-depth m :image)
       (remove nil?)
       (map #(str/split % #"/"))
       (map last)))