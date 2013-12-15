(ns stats.core
  (:require [clojure.math.numeric-tower :as math]))

(defn mean [xs]
  (/ (reduce + xs) (count xs)))

(defn median [xs]
  (let [i (dec (/ (count xs) 2))
        sorted (sort xs)]
    (if (even? (count xs))
      (mean [(nth sorted i) (nth sorted (inc i))])
      (nth sorted (math/round i)))))

; --- Quartiles and Outliers -----------

(defn quartile [xs quartile]
  (let [sorted (sort xs)
        result (* quartile
                  (inc (count xs)))]
    (if (integer? result) result
      (let [idx (dec (math/floor result))]
        (mean [(nth sorted idx)
               (nth sorted (inc idx))])))))

(defn first_quartile [xs]
  (quartile xs 1/4))

(defn third_quartile [xs]
  (quartile xs 3/4))

(defn interquartile_range [xs]
  (- (third_quartile xs)
     (first_quartile xs)))

(defn low_outlier? [x q_one iqr]
  (< x (- q_one
          (* 1.5 iqr))))

(defn high_outlier? [x q_three iqr]
  (> x (+ q_three
          (* 1.5 iqr))))

(defn outlier? [x q_one q_three iqr]
  (or (low_outlier? x q_one iqr)
      (high_outlier? x q_three iqr)))

(defn outliers [xs]
  (let [q_one (first_quartile xs)
        q_three (third_quartile xs)
        iqr (interquartile_range xs)]
  (filter #(outlier? % q_one q_three iqr) xs)))

; --- Deviation ---------------------

(defn deviations [xs]
  (let [m (mean xs)]
    (map #(- % m) xs)))

(defn deviation [xs]
  (mean (deviations xs)))

(defn absolute_deviations [xs]
  (let [m (mean xs)]
    (map #(math/abs (- % m)) xs)))

(defn absolute_deviation [xs]
  (mean (absolute_deviations xs)))

(defn sq [n]
  (* n n))

(defn squared_deviations [xs]
  (let [m (mean xs)]
    (map #(sq (- % m)) xs)))

(defn sum_of_squares [xs]
  (reduce + (squared_deviations xs)))

;; For populations

(defn variance_p [xs]
  (mean (squared_deviations xs)))

(defn standard_deviation_p [xs]
  (math/sqrt (variance_p xs)))

;; For samples, using Bessel's Correction

(defn variance_s [xs]
  (/ (reduce + (squared_deviations xs))
     (dec (count xs))))

(defn standard_deviation_s [xs]
  (math/sqrt (variance_s xs)))

; --- Standardized distributions ---------------

(defn zscore [n mu sd]
  (/ (- n mu) sd))

(defn zscores [xs mu sd]
  (map #(zscore % mu sd) xs))

(defn standardize_distribution [xs p_or_s]
  (let [mu (mean xs)
        sd (p_or_s xs)]
    (map #(zscore % mu sd) xs)))

(defn standardize_distribution_p [xs]
  (standardize_distribution xs standard_deviation_p))

(defn standardize_distribution_s [xs]
  (standardize_distribution xs standard_deviation_s))

; --- Section for playing in Light Table

(def data [92
           117
           109
           85
           117
           107
           82])

(def fq (float (first_quartile data)))
(def med (float (median data)))
(def tq (float (third_quartile data)))
(def iqr (float (interquartile_range data)))

(def out (outliers data))

(def med (median data))

(def ss (sum_of_squares data))
(float ss)

(def vp (variance_p data))
(float vp)

(def stdevp (standard_deviation_p data))
(def stdevs (standard_deviation_s data))

(def mn (mean data))
(float mn)

(def plusstdevp (+ mn stdevp))
(def minusstdevp (- mn stdevp))

(def plusstdevs (+ mn stdevs))
(def minusstdevs (- mn stdevs))

(def within_one_stddev_s (filter #(and (< % plusstdevs) (> % minusstdevs)) data))

(def proportion_within_one_stddev_s (/ (count within_one_stddev)
                                     (count data)))

(def sdp (standardize_distribution_p data))

(standard_deviation_p sdp)
