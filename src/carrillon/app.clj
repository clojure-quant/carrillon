(ns carrillon.app
  (:require
   [clojure.string :as str]
     ;[clojure.pprint] 
   [clojure.pprint :refer [print-table]]
   [carrillon.apto :refer [apartments]]
   [carrillon.pdf :refer [extract-transactions of-io]]))

; #(= apt (:apt %))

;  (fn [x] 
;   (= apt (:apt x)))

(defn print-apartment [transactions apt]
  (let [fields (if (= apt :xxx)
                 [:date :amount :from :sucursal :id]
                 [:date :amount :from :sucursal])]
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

(defn hack-6C [{:keys [apt amount] :as m}]
  ; 200.0 is maria jesus.
  (if (and (= apt :xxx)
           (= amount "200.00"))
    (assoc m :apt :6C)
    m))

(defn hack-sing1 [{:keys [apt amount] :as m}]
  ; 1,512.00 is aman singh.
  (if (and (= apt :xxx)
           (= amount "1,512.00"))
    (assoc m :apt :L1)
    m))

(defn hack-sing2 [{:keys [apt amount] :as m}]
  ; 1,800.00 is aman singh.
  (if (and (= apt :xxx)
           (= amount "1,800.00"))
    (assoc m :apt :3B)
    m))

(defn hack-aizaga [{:keys [apt amount] :as m}]
  (if (and (= apt :xxx)
           (= amount "315.00"))
    (assoc m :apt :1C)
    m))

(defn hack-l4 [{:keys [apt amount] :as m}]
  (if (and (= apt :xxx)
           (= amount "126.00"))
    (assoc m :apt :L4)
    m))

(defn hack-alvaro [{:keys [apt amount] :as m}]
  (if (and (= apt :xxx)
           (= amount "300.00"))
    (assoc m :apt :6B)
    m))

(defn hack-id [{:keys [id] :as m}]
  (let [apt (case id
              "217659184" :3C ; 2022 17 whatsapp lizca

              "231624416" :1D ; payment banco general debt 
              "245461181" :1D ;| 04/06/2022
              "263372009" :1D ;| 30/06/2022 |   105.00 |       |         1 |  |
              "263372284" :1D ;| 30/06/2022 |   105.00 |       |         1 |  |
              "263372579" :1D ;| 30/06/2022 |   105.00 |       |         1 |  |
              "249747083" :1D ;| 07/07/2022 |   105.00 |       |        30 |  |
              "257821529" :1D ;| 08/09/2022 |   105.00 |       |        30 |  |
              "261855295" :1D ;| 08/10/2022 |   105.00 |       |        30 |  |
              "265649501" :1D ;| 08/11/2022 |   105.00 |       |        30 |  |  

              "232496850"  :4B ; 330 cheque jose alberto

              "284948830" :2A ; alegria. (nota de voz de grimaldo)  

              "108217302" :6B ; alvaro. (nota de voz de alvaro) 
              "111058789" :6B ; alvaro. (nota de voz de alvaro)

            ; flor
              "126604083" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "148702668" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "138731147" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "135524005" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "113558310" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "126582299" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "102858304" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "105023603" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "97033971" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "89596510" :2C ; flor (llamada con margho, esposo pago antes del muerto)
              "81579441" :2C ; flor (llamada con margho, esposo pago antes del muerto)

            ;
              nil)]
    (if apt
      (assoc m :apt apt)
      m)))

(defn hacks [m]
  (->> m
       hack-6C
       hack-sing1
       hack-sing2
       hack-aizaga ; some could be eida or flor 
       hack-l4
       hack-id
       hack-alvaro))


(defn run [opts]
  (let [{:keys [transactions text]} (extract-transactions "2022.pdf")
        transactions (map hacks transactions)]
    (spit "2022-raw.txt" text)
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

  (run {})


;
  )