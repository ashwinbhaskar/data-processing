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

(defn maximise-products
  [list-items budget]
  (let [sparse-list (->> list-items
                         (map #(select-keys % [:brand-name :item-id :price]))
                         (map (fn [{price :price :as m}]
                                (assoc m :price (Double/parseDouble price)))))
        cache       (atom {})
        maximise    (fn m [[{price :price brand-name :brand-name :as first} & rest :as items] money product-list]
                      (cond
                        (nil? first) product-list
                        (> price money) (m rest money product-list)
                        :default (or (get @cache [items money])
                                     (let [{existing-price :price :as product-with-brand-name} (->> product-list
                                                                                                    (some (fn [{bn :brand-name :as item}]
                                                                                                            (when (= bn brand-name)
                                                                                                              item))))
                                           with-first    (if product-with-brand-name
                                                           (m rest (+ (- money price) existing-price) (conj (remove #(= product-with-brand-name %) product-list) first))
                                                           (m rest (- money price) (conj product-list first)))
                                           without-first (m rest money product-list)]
                                       (if (> (count with-first) (count without-first))
                                         (do
                                           (swap! cache #(assoc % [items money] with-first))
                                           with-first)
                                         (do
                                           (swap! cache #(assoc % [items money] without-first))
                                           without-first))))))]
    (maximise sparse-list budget [])))


(comment
  (-> (slurp "lipstick.json")
      (cheshire.core/parse-string camel-snake-kebab.core/->kebab-case-keyword)
      (get-in [:mods :list-items])
      (maximise-products 250)))
