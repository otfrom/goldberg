;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canon Fodder - Chris Ford (ThoughtWorks)     ;;
;;                                              ;;
;; http://github.com/ctford/goldberg            ;;
;; http://github.com/overtone/overtone          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING - This will cause the 200MB sampled  ;;
;; piano to be downloaded and cached locally.   ;;
(ns goldberg.variations.canone-alla-quarta
  (:use
    [overtone.live :exclude [scale bpm run pitch shift]]
    [overtone.inst.sampled-piano :only [sampled-piano] :rename {sampled-piano piano#}]))

(defn play-on# [instrument# notes]
  (let [play-at# (fn [[ms midi]]
                   (at ms (instrument# midi))
                   (at (+ ms 150) (ctl piano# :gate 0)))]
    (->> notes (map play-at#) dorun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Synth                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst saw# [freq 440 depth 5]
  (let [envelope (env-gen (perc 0.1 0.4) (lf-pulse:kr 2) :action FREE)]
    (*
      envelope
      (saw (+ freq (* depth (lf-saw:kr (lf-pulse:kr 0.1 0.2))))))))

(defn synth# [midi-note] (-> midi-note midi->hz saw#))
;;(def play# (partial play-on# synth#))
;;(def play# (partial play-on# piano#))
(defn play# [inst notes]
  (partial play-on# inst))

(defn even-melody# [pitches inst]
  (let [times (reductions + (cons (now) (repeat 400)))
        notes (map vector times pitches)]
    (play-on# inst notes)))

;(synth# 55)
;(even-melody# (range 60 73) synth#)

;(piano# 55)
;(even-melody# (range 60 67) piano#)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scale                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sum-n [series n] (reduce + (take n series)))

(defn scale [intervals]
  #(if-not (neg? %)
     (sum-n (cycle intervals) %)
     ((comp - (scale (reverse intervals)) -) %)))

(def major (scale [2 2 1 2 2 2 1]))
(def minor (scale [2 1 2 2 1 2 2]))

(defn start-from [base] (partial + base))

;; from middle C (60)
(def C (start-from 60))
(def Db (start-from 61))
(def D (start-from 62))
(def Eb (start-from 63))
(def E (start-from 64))
(def F (start-from 65))
(def Gb (start-from 66))
(def G (start-from 67))
(def Ab (start-from 68))
(def A (start-from 69))
(def Bb (start-from 70))
(def B (start-from 71))

(def G-minor (comp G minor))

;; (even-melody#
;;  (let [_ -100]
;;    (map (comp D major) [0 1 2 0, 0 1 2 0, 2 3 4 _, 2 3 4 _]))
;;  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstractions                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bpm [beats] (fn [beat] (-> beat (/ beats) (* 60) (* 1000))))
;;((bpm 120) 3)

(defn run [[from & tos]]
  (if-let [to (first tos)]
    (let [up-or-down (if (<= from to)
                       (range from to)
                       (reverse (range (inc to) (inc from))))]
      (concat up-or-down (run tos)))
    [from]))

;; (even-melody# (map (comp G major)
;;            (run [0 4 -1 0 1 0])
;;            ))

(defn accumulate [series] (map (partial sum-n series) (range (count series))))
(def repeats (partial mapcat #(apply repeat %)))
(def runs (partial mapcat run))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Melody                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def melody
  (let [call
          [(repeats [[2 1/4] [1 1/2] [14 1/4] [1 3/2]])
          (runs [[0 -1 3 0] [4] [1 8]])]
        response
          [(repeats [[10 1/4] [1 1/2] [2 1/4] [1 9/4]])
          (runs [[7 -1 0] [0 -3]])]
        development
          [(repeats [[1 3/4] [12 1/4] [1 1/2] [1 1] [1 1/2] [12 1/4] [1 3]])
          (runs [[4] [4] [2 -3] [-1 -2] [0] [3 5] [1] [1] [1 2] [-1 1 -1] [5 0]])]
        [durations pitches] (map concat call response development)
        timings (map (partial + 1/2) (accumulate durations))]
    (map vector timings pitches)))

(def bass
  (let [triples (partial mapcat #(repeat 3 %))]
    (map vector
       (accumulate (repeats [[21 1] [13 1/4]]))
       (concat (triples (runs [[-7 -10] [-12 -10]])) (run [5 -7])))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canone alla quarta - Johann Sebastian Bach   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canon [f] (fn [notes] (concat notes (f notes))))

(def timing 0)
(def pitch 1)
(defn skew [k f] (fn [points] (map #(update-in % [k] f) points)))
(defn shift [point] (fn [points] (map #(->> % (map + point) vec) points)))

(defn simple [wait] (shift [wait 0]))
(defn interval [interval] (shift [0 interval]))
(def mirror (skew pitch -))
(def crab (skew timing -))
(def table (comp mirror crab))

(defn canone-alla-quarta [canon-type]
  (canon (comp (interval -3) canon-type (simple 3))))

(defn canon# [start tempo scale inst canonm]
  (let [in-time (comp (shift [start 0]) (skew timing tempo))
        in-key (skew pitch scale)
        play-now# (comp play-on# inst in-key in-time)]

    (-> bass play-now#)
    (-> melody canonm play-now#)))

;; (canon# (now) (bpm 90) (comp Db minor) piano# (canone-alla-quarta crab))
;; (canon# (now) (bpm 90) (comp Db major) (canone-alla-quarta mirror))
;; (canon# (now) (bpm 90) (comp G major) (canone-alla-quarta table))
;; (stop)
