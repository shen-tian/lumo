(ns lumo.core
  (:require [clj-time.core :as t]
            [clojure.string :as str]
            [com.evocomputing.colors :as c]
            [clj-opc.core :as opc])
  (:gen-class))

(defn color->word
  [color]
  {:r (c/red color) :g (c/green color) :b (c/blue color)})

(defn dist
  [x y]
  (min (Math/abs (double (- x y)))
       (Math/abs (double (- (inc x) y)))
       (Math/abs (double (- (dec x) y)))))

(defn map-to-strip
  [t]
  (let [len 50
        sec (+ (* (t/milli t) 0.001) (t/second t))
        base-color (c/create-color {:h (* sec 6)
                                    :s 50
                                    :l 50})
        pos (/ (mod sec 10.0) 10.0)]
    (map color->word
          (map #(c/darken base-color
                          (* 100 (dist (/ % len) pos)))
               (range len)))))

(defn tick
  [client]
  (let [now (t/now)]
    (opc/show! client (map-to-strip now))
    true))

(defn run
  [client]
  (while true
    (do 
      (tick client)
      (Thread/sleep 33))))

(defn init-client
  []
  (opc/client "127.0.0.1" 7890 1000))

(defn -main
  []
  (let [client (init-client)]
    (println ":: lumo ::")
    (run client)))
