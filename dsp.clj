(ns dsp.core)
 
(defn median [data]
  (let [data-sort (sort data)]
    (cond  (nil?  (first data-sort)) nil
          :else   (nth  data-sort  (+ ( / (count data) 2 )) 1)  )))

(defn average [data]
  (let [data-sort (sort data)]
    (cond  (nil?  (first data-sort)) nil
          :else    (/ (reduce + data) (count data) ))))



 ;データを分割してmedianなどに渡せるようにｎによってリストの長さをｎにする
(defn data-cons-n [data n result]
  (if (< n 0) ( rest result)
    (recur (rest data) (- n 1) (conj result (first data))  )))

(defn median-filter [data n]
  (pmap median (data-cons data n) ))

(defn moveing-average [data n]
  (pmap average (data-cons data n) ))


(defn data-cons [data n ]
  (let  [len (count data) ]
    (letfn[( bin-data [bin result m] ;;;[result 現在の要素数 )]
             (if (< len m) (reverse(rest result))
               (recur (rest bin) (conj result (data-cons-n bin n '() )  )  (+ m 1)) ))]
      (bin-data data nil 0) )))


(def plot-data []
  (let [解析データ]
    (doto 
      (view (xy-plot x (count x)))
      (scatter-plot x y)
       view
      (add-lines x (:fitted lm))；最小二条
      
)))
