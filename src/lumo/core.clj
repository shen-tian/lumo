(ns lumo.core
  (:require [clj-time.core :as t]
            [clojure.string :as str]
            [com.evocomputing.colors :as c]
            [clj-opc.core :as opc])
  (:gen-class))

(defn get-hue
  [hour]
  (cond
    (< hour 6)  150 ;;"pre-dawn" green
    (< hour 8)  180 ;;"dawn"     teal
    (< hour 17) 60  ;;"day"      yellow
    (< hour 20) 300 ;;"dusk"     pink
    (< hour 24) 270 ;;"evening"  purple
    :else "wot"))

(defn get-lumin
  [hour]
  (cond
    (< hour 6) 10
    (< hour 8) 50
    (< hour 17) 10
    (< hour 20) 50
    (< hour 24) 30
    :else 0))

(defn utc->sast
  [date]
  (t/to-time-zone  date (t/time-zone-for-offset 2)))

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
        base-color (c/create-color {:h (get-hue (t/hour (utc->sast t)))
                                    :s 50
                                    :l (get-lumin (t/hour (utc->sast t)))})
        pos (/ (mod sec 10.0) 10.0)]
    (map color->word
          (map #(c/darken base-color
                          (* 100 (dist (/ % len) pos)))
               (range len)))))

(defn set-strip
  [client time]
  (opc/show! client (map-to-strip time)))

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
    (println ":: starting lumo ::")
    (run client)))
