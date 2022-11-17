(ns carrillon.app
  (:require
    [clojure.string :as str]
     ;[clojure.pprint] 
   [clojure.pprint :refer [print-table]]
   [carrillon.apto :refer [apartments]]
   [carrillon.pdf :refer [extract-transactions of-io]]
  ))

; #(= apt (:apt %))

;  (fn [x] 
;   (= apt (:apt x)))

(defn print-apartment [transactions apt]
  (let [fields (if (= apt :xxx)
                 [:date :amount :from :sucursal]
                 [:date :amount :from])]
    (println "***** " apt " *******")
    (->> transactions
         (filter (of-io :i))
         (filter #(= apt (:apt %)))
         (print-table fields))))


(defn print-all-apartments [transactions]
  (doall
   (map
    #(print-apartment transactions (first %))
    apartments)))

(defn run [opts]
  (let [transactions (extract-transactions "2022.pdf")]
    (print-all-apartments transactions)))

(comment 
  
  (def transactions 
    (extract-transactions "2022.pdf"))
   

    (->> transactions
       (filter (of-io :u))
       ;count
       (print-table [:date 
                           :apt 
                           :amount 
                         ; :type
                     ]))
  

(print-apartment transactions :6B)
(print-apartment transactions :5B)
(print-apartment transactions :xxx)

(print-all-apartments transactions)
  
  
;
  )