(ns carrillon.app
  (:require
   [clojure.string :as str]
  ; [clojure.math :refer [ceil floor round]]
     ;[clojure.pprint] 
   [clojure.pprint :refer [print-table]]
   [carrillon.apto :refer [apartments  apartment-quotas]]
   [carrillon.pdf :refer [extract-transactions of-io]]))

; #(= apt (:apt %))

;  (fn [x] 
;   (= apt (:apt x)))

(defn trans-of-apt [apt transactions]
  (->> transactions
       (filter (of-io :i))
       (filter #(= apt (:apt %)))))

(defn table-str [fields cols]
  (with-out-str
    (print-table fields cols)))

(defn transaction-table-str [transactions]
  (with-out-str
    (print-table
     [:date :amount :from :id :sucursal]
     transactions)))

(defn round [nr]
  (-> nr Math/floor int))

(defn calc-apartment [transactions apt]
  (let [payments  (trans-of-apt apt transactions)
        quotas (apartment-quotas apt)
        balance (concat payments quotas)
        paid (reduce + (map :amount-f payments))
        quotas (reduce + (map :amount-f quotas))]
    {:payments payments
     :quotas quotas
     :balance balance
     :stats {:apt apt
             :paid (round paid)
             :quotas (round quotas)
             :balance (round (+ paid quotas))}}))

(defn print-apartment [transactions apt]
  (let [{:keys [payments balance stats] :as apt-data}  (calc-apartment transactions apt)
        table-payments (transaction-table-str payments)
        table-balance (transaction-table-str balance)]
    (spit (str "in-txt/" (name apt) ".txt")
          (str (name apt) "\r\n" table-payments))
    (spit (str "balance-txt/" (name apt) ".txt")
          (str (name apt) "\r\n" table-balance
               "\r\n"
               "\r\n quotas: " (:quotas stats)
               "\r\n paid: " (:paid stats)
               "\r\n balance: " (:balance stats)
               ))
    apt-data))


(defn print-all-apartments [transactions]
  (let [stats (map
               #(print-apartment transactions (first %))
               apartments)
        ;stats (map #(dissoc % :payments :quotas :balance) stats)
        stats (map #(:stats %) stats)]
    (println "stats: " stats)
    (->> stats
         (table-str [:apt :quotas :paid :balance])
         (spit "summary.txt"))))


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

              ; banco general / montilla
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

              ; rosemary
              "81579210" :4A ; rosemary (whatsapp con yvette)
              "90135366" :4A ; rosemary (whatsapp con yvette)
              "97769041" :4A ; rosemary (whatsapp con yvette)
              "104452340" :4A ; rosemary (whatsapp con yvette)
              "112721490" :4A ; rosemary (whatsapp con yvette)
              "122267762" :4A ; rosemary (whatsapp con yvette)
              "130793685" :4A ; rosemary (whatsapp con yvette)
              "134645655" :4A ; rosemary (whatsapp con yvette)
              "142317086" :4A ; rosemary (whatsapp con yvette)
              "148684351" :4A ; rosemary (whatsapp con yvette)
              "160187688" :4A ; rosemary (whatsapp con yvette)
              "208504118" :4A ; rosemary (whatsapp con yvette)
              "226428240" :4A ; rosemary (whatsapp con yvette)
              "220849650" :4A ; rosemary (whatsapp con yvette)
              "141177110" :4A ; rosemary (whatsapp con yvette)
              "235240157" :4A ; rosemary (whatsapp con yvette)
              "197381349" :4A ; rosemary (whatsapp con yvette)
              "237513903" :4A ; rosemary (whatsapp con yvette)
              "241281237" :4A ; rosemary (whatsapp con yvette)
              "245070233" :4A ; rosemary (whatsapp con yvette)
              "179035353" :4A ; rosemary (whatsapp con yvette)

              ; grimaldo
              "82119575" :1A ; grimaldo (whatsapp con grimaldo)
              "90391729" :1A ; grimaldo (whatsapp con grimaldo)
              "99890015" :1A ; grimaldo (whatsapp con grimaldo)
              "111381732" :1A ; grimaldo (whatsapp con grimaldo)
              "117729619" :1A ; grimaldo (whatsapp con grimaldo)
              "118895855" :1A ; grimaldo (whatsapp con grimaldo)

              ; aizaga
              "84603534" :1C ; aizaga (whatsapp con aizaga)

              ; lizca
              "103132794" :3C ; lizca (whatsapp con lizca)
              "98220251" :3C ; lizca (whatsapp con lizca)
              "120843908" :3C ; lizca (whatsapp con lizca)
              "129182651" :3C ; lizca (whatsapp con lizca)
              "84603971" :3C ; lizca (whatsapp con lizca)
              "186127688" :3C ; lizca (whatsapp con lizca)
              "172037873" :3C ; lizca (whatsapp con lizca)
            
            ;
              nil)]
    (if apt
      (assoc m :apt apt)
      m)))

(defn assoc-parsed-amount [{:keys [amount] :as m}]
  (let [amount-f (if (or (nil? amount) (str/blank? amount))
                   0
                   (Float/parseFloat (str/replace amount #"," "")))]
    (assoc m :amount-f amount-f)))

(defn hacks [m]
  (->> m
       hack-6C
       hack-sing1
       hack-sing2
       hack-aizaga ; some could be eida or flor 
       hack-l4
       hack-id
       hack-alvaro
       assoc-parsed-amount))



(comment
  (Float/parseFloat "150")
  (Float/parseFloat "150.00")
  (Float/parseFloat "150")
  (Float/parseFloat "2400.04")
  (assoc-parsed-amount {:amount "150.00"})
  (assoc-parsed-amount {:amount "2,400.40"})
  (assoc-parsed-amount {:amount ""})
  (assoc-parsed-amount {:amount nil})

  ;
  )

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