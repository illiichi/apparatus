#! /usr/bin/env clojure

;; 装置 (apparatus)
;; by illiichi

(use 'overtone.live)

(def xs [1 3/2 2/3 2 3/4 1/3 9/4 1/2 3 9/8])

(defn T [n] (.pow (biginteger 2) (- 12 n)))
(defn F [n] (/ 1 (T n)))

(defn fade-in [start-time dur]
  (env-gen (envelope [0 0 1] [start-time dur] -8)))

(defmacro fade [length a d r gate]
  `(env-gen (envelope [0 1 1 0] (map #(~'* ~length %) ~[a d r]) -4) ~gate))

(defn transpose [xss]
  (apply map list xss))

(defn lower-bound [in]
  (case (first in)
    latch:ar (lower-bound (second in))
    (sin-osc sin-osc:kr lf-tri lf-tri:kr
             lf-saw lf-saw:kr white-noise
             lf-noise0 lf-noise1 lf-noise2
             lf-noise0:kr lf-noise1:kr lf-noise2:kr) -1
    0))

(defmacro rg-lin
  ([in] `(lin-lin ~in ~(lower-bound in) 1 0 1))
  ([in lo hi] `(lin-lin ~in ~(lower-bound in) 1 ~lo ~hi)))

(defmacro rg-exp
  ([in] `(lin-exp ~in ~(lower-bound in) 1 0 1))
  ([in lo hi] `(lin-exp ~in ~(lower-bound in) 1 ~lo ~hi)))

(defmacro switch [trig a b]
  `(let [t# (abs ~trig)]
     (~'+
      (~'* t# ~a)
      (~'* (~'- 1 t#) ~b))))

(defmacro switch->
  [in b eff] `(switch ~b (-> ~in ~eff) ~in))

(defmacro reduce-> [initial f & colls]
  (if (= (count colls) 1)
    `(reduce ~f ~initial ~(first colls))
    `(reduce ~f ~initial (transpose ~(vec colls)))))

(defmacro reduce*> [initial n f]
  `(reduce (fn [acc# _#] (~f acc#)) ~initial (range ~n)))


(defn n-range [min max num]
  (range min max (/ (- max min) num)))

(defmacro rotate-> [snd pos]
  `(let [[snd1# snd2#] ~snd] (rotate2 snd1# snd2# ~pos)))

(defmacro play [body]
  `((synth (out 0 ~body))))


(play
 (-> (map #(* (sin-osc (* 500 %1
                          (demand:kr (impulse 1/128 %2) 0 (dseq (reductions * 1 xs) INF))))
              (lf-noise1 1/32))
          xs
          (shuffle (n-range 0 5/4 (count xs))))
     splay
     (rotate-> (sin-osc 0.01))
     (* 8 (fade-in 4 16))
     (tanh)))

(play
 (-> (mix (blip [10 13 8]))
     (rhpf [6000 4800 3200 7200]
           (rg-exp (sin-osc 0.002) 0.001 0.5))
     (* (repeatedly 4 #(lf-pulse (rg-exp (lf-noise0:kr 1) 1/8 1))))
     splay
     (free-verb 1 1)
     (switch-> (rg-lin (sin-osc 0.008)) (* (lf-pulse 16 0 1/8)))
     (reduce*> 3 (fn [acc] (+ acc (* 3/4 (delay-c acc 1/4 1/4)))))
     (* 16 (fade-in 12 64))
     (tanh)))

(play
 (let [bl  (blip (rg-exp (mix (map lf-pulse [1/4 1/5 1/7 1/64])) 1
                         (-> (mix (map #(lf-saw % 1) [1/128 1/76 1/8 1/27]))
                             (rg-lin 5 100)
                             (round 20))))
       w   (rg-lin (lf-tri:kr (F 1)))]
   (-> (map #(-> (+ (local-in 2) bl)
                 (* (lf-pulse %1 0
                              (round (rg-lin (sin-osc:kr 1/16 %2) 1/8 3/4)
                                     1/8))))
            xs
            (n-range 0 1 (count xs)))
       splay
       (rotate-> (sin-osc 0.1))
       (doto local-out)
       (switch-> (lf-noise0 1/5) (free-verb 1 0.1))
       (hpf 80)
       (* 32 (fade-in 24 64))
       (reduce-> (fn [acc [x y]] (free-verb acc x y))
                 (iterate #(* % 1/2) 1/2)
                 (map #(lf-pulse % 0 w) xs))
       tanh)))

(play
 (let [gate  (* (switch (< (white-noise:kr) (rg-lin (sin-osc:kr 0.03) 0.5 1))
                        (impulse 1/8) (dust:kr 16))
                (lf-pulse 1/24 0 1/4))
       speed (rg-exp (env-gen (env-perc 0.05 0.2) gate)
                     0.001 50)
       env (env-gen (env-perc 0.02 0.1) gate)]
   (-> (map #(* (-> (white-noise)
                    (resonz % 0.001)
                    (lpf (* 2 %)))
                (sin-osc speed %2))
            (reductions * 50 (cycle [1.05 1.08 1.48 2.28]))
            (n-range 0 3 14))
       splay
       (* 2048 env (fade-in (T 3) (T 2)))
       (ringz 10 0.001)
       (free-verb 0.7 0.14)
       (rotate-> (sin-osc -0.8))
       tanh
       (* (cubed (lf-noise2 1/3))))))

(play
 (let [decay-time (+ (lf-pulse (rg-lin (lf-noise0:kr 1/16) 1/32 1/4) 0 1/32)
                     1e-4 (rg-exp (lf-noise0 1/4) 1e-4 1.5e-3))
       room       (rg-exp (sin-osc 0.02) 1e-3 1)
       rt         (rg-lin (lf-tri:kr (F 2) 1/2))]
   (-> (map #(pulse (switch-> 4000 rt (* %)) (lf-noise1 8))
            (n-range 11/12 13/12 4))
       splay
       (rotate-> (sin-osc 3.3))
       (reduce-> (fn [acc x]
                   (switch-> acc (env-gen (env-perc 1 15) (impulse (* 1/32 x) 1/2))
                             (ringz (* x 500) decay-time)))
                 (take 5 xs))
       (switch-> (lag-ud (lf-noise0 1/4) 0.2 0.01)
                 (reduce*> 3 #(free-verb % 1 room)))
       (tanh)
       (* (cubed (lf-noise1 1/32)) 4
          (-> (+ 1/4 (lf-tri (F 5))) (* 2) clip:ar)
          (fade-in (T 3) (T 4))))))


(play
 (let [release (rg-exp (mix (map sin-osc:kr [1/12 1/8 1/16 1/3])) 2 15)
       length  (rg-exp (lf-saw:kr (F 0) 1) (T 1) (T 5))
       gate    (impulse (min 1/8 (/ 1 release (rg-lin (sin-osc:kr 0.02) 1 8))))
       freq    (-> 500
                   (switch-> (lf-pulse (F 1) 1/2 1/4)
                             (* (demand:kr gate 0 (dseq (reductions * 1 xs) INF)))))]
   (-> (map #(* (sin-osc (* %1 freq (rg-lin (lf-pulse 8 0 1/4) 1 3/2)))
                (env-gen (env-perc 1 1) %2))
            (iterate #(* % 3/2) 1)
            (map #(impulse % 1/2) xs))
       splay
       (* 12)
       (tanh)
       (rotate-> (sin-osc (switch (< (lf-noise0:kr 1/16) 0.5) 33.0 0.33)))
       (reduce*> 4 #(free-verb % (rg-lin (lf-noise1 8) 0 1)))
       (* (env-gen (env-perc (* 1/4 release) release) gate)
          (fade length 1/16 3/4 3/16
                (impulse (/ 2/3 length) 1/2))))))

(play
 (let [w-min  (rg-lin (lf-tri:kr (F 1) -1) 1e-8 1e-2)
       env    (lf-pulse 1/8 0 (rg-exp (lf-noise0:kr 1) w-min 1/2))
       length (rg-exp (lf-tri:kr (F 0) -1) (T 1) (T 6))
       gate   (impulse (/ 1 length) 3/4)]
   (-> (map #(* (bpf (white-noise) (rg-exp (lf-noise0 8) 200 10000) 0.1)
                (lf-pulse:ar (rg-exp (lf-noise0 1/8) 1/4 4)
                             % (rg-lin (lf-noise0:ar 1/2) 0.1 0.9)))
            (n-range 0 1 8))
       splay
       (rotate-> (sin-osc 33.3))
       (* 8 env (fade length 1/16 1/2 1/2 gate))
       (g-verb 4 0.2)
       (ringz (rg-exp (lf-noise0 8) 1 30) 0.00001)
       (hpf 80)
       tanh
       (* (cubed (lf-noise0 1))))))
