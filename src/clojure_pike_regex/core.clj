(ns clojure-pike-regex.core)

(declare matchhere)  ; forward declaration

(defn tails [coll]
  (take (inc (count coll)) (iterate rest coll)))

(defn matchstar [c regexp text]
  (some
   (partial matchhere regexp)
   (take-while #(or (= c (first %)) (= \. c) (empty? %)) (tails text))))

(defn matchhere [regexp text]
  (cond
   (empty? regexp) true
   (= \* (second regexp)) (matchstar (first regexp) (nthrest regexp 2) text)
   (= '(\$) regexp) (empty? text)
   (empty? text) nil
   (= \. (first regexp)) (recur (rest regexp) (rest text))
   (= (first regexp) (first text)) (recur (rest regexp) (rest text))))

(defn match [regexp text]
  (if (= \^ (first regexp))
    (matchhere (rest regexp) text)
    (some (partial matchhere regexp) (tails text))))


