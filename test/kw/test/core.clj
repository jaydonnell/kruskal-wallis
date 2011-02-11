(ns kw.test.core
  (:require
    [incanter.io :as iio]
    [incanter.core :as icore])
  (:use [kw.core] :reload)
  (:use [clojure.test]))

(def cwd (System/getProperty "user.dir"))
(def data (iio/read-dataset (str cwd "/wine.csv") :header true))

(deftest means-test
  (is (= [:A 8.1625] (first (means data)))))
