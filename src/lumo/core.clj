(ns lumo.core
  (:require [clj-time.core :as t]
            [clojure.string :as str]
            [com.evocomputing.colors :as c]
            [clj-opc.core :as opc])
  (:gen-class))

;; Some time of day lookups

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

(defn breath
  "Breathing function. Period 1, Range [0,1]
  http://sean.voisen.org/blog/2011/10/breathing-led-with-arduino/"
  [x]
  (let [e-inv (/ 1 Math/E)]
    (/
     (- (Math/exp (Math/sin (* x 2 Math/PI))) e-inv)
     (- Math/E e-inv))))

(defn bounce
  [x lim]
  (if (< x lim) x (- (* 2 lim) x)))

(defn heart
  "Heartbeat effect. A double bounce, followed by a pause.
  1 beat per second. Range [0,1]"
  [x]
  (let [xx (mod x 1)
        beat 0.8]
    (if (< xx beat)
      (-> (* 3 (/ 1 beat) xx)
          (bounce 1.5)
          (bounce 1))
      0)))

(defn breath-map
  [t]
  (let [len 50
        sec (+ (* (t/milli t) 0.001) (t/second t))
        base-color (c/create-color {:h 180
                                    :s 40
                                    :l (* (breath (/ sec 3)) 100)})
        pos (/ (mod sec 10.0) 10.0)]
    (map color->word
          (map #(c/darken base-color
                          (* 100 (dist (/ % len) pos)))
               (range len)))))

(defn heart-map
  [t]
  (let [len 50
        sec (+ (* (t/milli t) 0.001) (t/second t))
        base-color (c/create-color {:h 0
                                    :s 80
                                    :l (* (heart (* 0.67 sec)) 60)})]
    (map color->word
         (map #(c/darken base-color
                         (* 50 (dist (/ % len) 0)))
              (range len)))))

(defn set-strip
  [client time]
  (opc/show! client (map-to-strip time)))

(defn tick
  [client]
  (let [now (t/now)]
    (opc/show! client (map-to-strip now))
    true))

(defn run-pattern
  [client pattern-map]
  (while true
    (do
      (let [now (t/now)]
        (opc/show! client (pattern-map now)))
      (Thread/sleep 33))))

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
