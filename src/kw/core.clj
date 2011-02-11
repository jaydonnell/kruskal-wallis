(ns kw.core
  (:require
    [clojure.contrib.io :as io]
    [clojure.contrib.str-utils2 :as su]
    [clojure.contrib.seq-utils :as sequ]
    [incanter.core :as icore]
    [incanter.stats :as istats]))

(defn compact [col]
  (filter identity col))

(defn means [d]
  (map
    (fn [e] [(nth (.column-names d) e) (istats/mean (compact (icore/sel d :cols e)))])
    (range (dec (count (.column-names d))))))

(defn sort-values [d]
  (sort
    (reduce
      (fn [m e] (concat m (compact (icore/sel d :cols e))))
      []
      (range (count (.column-names d))))))

(defn position [v coll]
  (let [ps (sequ/positions #(= v %) coll)]
    (/ (reduce + ps) (count ps))))

(defn rank-map [col sorted-col]
  (map (fn [e] (if (nil? e) nil (+ 1 (position e sorted-col)))) col))

(defn ranked-measures [d]
  (let [sorted-values (sort-values d)
        row-count (count (.rows d))]
    (icore/dataset
      (.column-names d)
      (apply vector (map
        (fn [i] (apply vector (rank-map (icore/sel d :rows i) (sort-values d))))
        (range row-count))))))

(defn group-stats [d]
  (let [cols (.column-names d)
	counts (reduce
		(fn [m i] (assoc m (nth cols i) (count (compact (icore/sel d :cols i)))))
		{}
		(range (count cols)))
	means (reduce
	       (fn [m i] (assoc m (nth cols i) (istats/mean (compact (icore/sel d :cols i)))))
	       {}
	       (range (count cols)))
	average-mean (/ (reduce + (vals means)) (count (vals means)))
	total-count (reduce + (vals counts))]
    
    {:groups cols :counts counts :means means :average-mean average-mean :total-count total-count}))

(defn ssbg [gs]
  (reduce + 0 (map
	       (fn [e] (*
			(e (:counts gs))
			(math/expt
			 (- (e (:means gs)) (:average-mean gs))
			 2)))
	       (:groups gs))))

(defn h [gs]
  (let [ss (ssbg gs)
	n (:total-count gs)]
    (/
     ss
     (/
      (*
       n
       (+ 1 n))
      12))))

(def kw-cdf [df v]
     (idist/cdf (idist/chisq-distribution df) v))