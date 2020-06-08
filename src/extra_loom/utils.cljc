(ns extra-loom.utils)


(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))


(def not-in?
  "true if coll does not contains elm"
  (complement in?))


(defn apply-to-if
  [m test f & args]
  "Applies f to m and m if test is true. For conditional steps in threaded -> chains."
  (if test
    (apply f m args)
    m))


(defn join
  "Concat a collection of colls."
  [colls] (reduce concat colls))


(defn update-in-all
  "Like update-in but when special key :all is supplied in the ks vector,
  all map-entries at that level will be accepted."
  [m ks f & args]
  (let [up (fn up [m ks f args]
             (let [[k & ks] ks]
               (if ks
                 (if (= k :all)
                   (reduce (fn [acc [kz v]] (assoc acc kz (up v ks f args))) {} m)
                   (assoc m k (up (get m k) ks f args)))
                 (if (= k :all)
                   (reduce (fn [acc [kz v]] (assoc acc kz (apply f v args))) {} m)
                   (if-let [v (get m k)]
                     (assoc m k (apply f (get m k) args))
                     m)))))]
       (up m ks f args)))


(defn update-in-all-if
  "Like update-in-all but only if test is true."
  [m test ks f & args]
  (if test
    (apply update-in-all m ks f args)
    m))


(defn dissoc-in
  "Dissocs at the ks vector in a nested map.
  Supplying special key :all in the last position in ks will dissoc all entries.
  Supplying :all in another possible will cause all map-entries at that level to be recursed into."
  [m ks]
  (let [up (fn up [m ks]
             (let [[k & ks] ks]
               (if ks
                 (if (= k :all)
                   (reduce (fn [acc [kz v]] (assoc acc kz (up v ks))) {} m)
                   (assoc m k (up (get m k) ks)))
                 (if (= k :all)
                   {}
                   (reduce (fn [acc [kz v]]  (if (= k kz) acc (assoc acc kz v))) {} m)))))]
       (up m ks)))


(defn dissoc-in-when
  "Like dissoc-in, but items are dissoc'd only when (pred value-of-entry) is true."
  [m ks pred]
  (let [up (fn up [m ks]
             (let [[k & ks] ks]
               (if ks
                 (if (= k :all)
                   (reduce (fn [acc [kz v]] (assoc acc kz (up v ks))) {} m)
                   (assoc m k (up (get m k) ks)))
                 (if (= k :all)
                   (reduce (fn [acc [kz v]]
                             (if (pred v) acc (assoc acc kz v))) {} m)
                   (reduce (fn [acc [kz v]]
                             (if (and (= k kz) (pred v)) acc (assoc acc kz v))) {} m)))))]
       (up m ks)))


;; Performance could be improved by building into dissoc-in
(defn dissoc-in-clean
  "Likes dissoc-in but when the result of the dissoc leaves in empty collection in
  the nested map, removes that map-entry that it is in, so cleaning the map from
  empty nested collections like ..:b {:a #{} :b 1}...  [:a #{}] would be removed."
  [m ks]
  (let [k (butlast ks)
        tgt (get-in m k)
        res (dissoc tgt (last ks))]
    (if (empty? res)
      (dissoc-in m k)
      (dissoc-in m ks))))
