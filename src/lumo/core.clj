(ns lumo.core
  (:require [clj-time.core :as t]
            [clojure.string :as str]
            [com.evocomputing.colors :as c]
            [clj-opc.core :as opc]
            [environ.core :refer [env]])
  (:gen-class))

;; Some time of day lookups

(defn get-hue
  [hour]
  (cond
    (< hour 6)  30 ;;"pre-dawn" yellow
    (< hour 8)  180 ;;"dawn"     teal
    (< hour 17) 60  ;;"day"      yellow
    (< hour 20) 300 ;;"dusk"     pink
    (< hour 22) 270 ;; purple
    (< hour 24) 30 ;;"evening"  yellow
    :else "wot"))

(defn get-lumin
  [hour]
  (cond
    (< hour 6) 25
    (< hour 8) 50
    (< hour 17) 15
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

(defn breath
  "Breathing function. Period 1, Range [0,1]
  http://sean.voisen.org/blog/2011/10/breathing-led-with-arduino/"
  [x]
  (let [e-inv (/ 1 Math/E)]
    (/
     (- (Math/exp (Math/sin (* x 2 Math/PI))) e-inv)
     (- Math/E e-inv))))

(defn bounce
  "Bounces off of the upper bound"
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

(defn lin-trans
  [f period max-r]
  (fn [x]
    (* (f (/ x period)) max-r)))

(defn breath-map
  [t]
  (let [len 50
        sec (+ (* (t/milli t) 0.001) (t/second t))
        base-color (c/create-color 
                    {:h 180
                     :s 40
                     :l ((lin-trans breath 3 100) sec)})
        pos (/ (mod sec 10.0) 10.0)]
    (map #(c/darken base-color
                    (* 100 (dist (/ % len) pos)))
         (range len))))

(defn heart-map
  [t]
  (let [len 50
        sec (+ (* (t/milli t) 0.001) (t/second t))
        base-color (c/create-color 
                    {:h 0
                     :s 80
                     :l ((lin-trans heart 1 70) sec)})]
    (map #(c/darken base-color
                    (* 50 (dist (/ % len) 0)))
         (range len))))

(defn map-to-strip
  [t]
  (let [len  50
        sec  (+ (* (t/milli t) 0.001) (t/second t))
        hue  (get-hue (t/hour (utc->sast t)))
        lum  (get-lumin (t/hour (utc->sast t)))
        base-color (c/create-color 
                    {:h hue
                     :s 50
                     :l (* ((lin-trans breath 3 1) sec) lum)})
        pos 0.5]
    (map #(c/darken base-color
                    (* lum (dist (/ % len) pos)))
         (range len))))

(defn tick
  ([client pattern-map]
   (tick client pattern-map (t/now)))
  ([client pattern-map time]
   (opc/show! client (map color->word
                          (pattern-map time)))))

(defn run-pattern-at 
  [client pattern-map h m]
  (let [offset (t/in-seconds 
                (t/interval (t/now) 
                            (t/date-time 2020 1 1 h m)))]
    (while true
      (do
        (let [t (t/plus (t/now) (t/seconds offset))]
          (tick client pattern-map t))
        (Thread/sleep 33)))))

(defn run-pattern
  [client pattern-map]
  (while true
    (do
      (let [now (t/now)]
        (tick client pattern-map now))
      (Thread/sleep 33))))

(defn init-client
  []
  (opc/client (if-let [host (env :opc-host)]
                host "127.0.0.1")
              (if-let [port (env :opc-port)]
                port 7890)
              1000))

(defn -main
  []
  (let [client (init-client)]
    (println ":: starting lumo ::")
    (run-pattern client map-to-strip)))
