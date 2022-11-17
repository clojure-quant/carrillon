(ns carrillon.pdf
  (:require
   [clojure.string :as str]
   [pdfboxing.text :as text] 
   [carrillon.apto :refer [assoc-apartment]])
  (:import 
    [java.util.regex Pattern]
   )
  )

; call print-table "fully qualified" = with namespace
#_(clojure.pprint/print-table [{:name "chris" :loves "read"}
                             {:name "joseph" :loves "punchi"}
                             ])

; call print-table only with function name
#_(print-table [{:name "chris" :loves "read"}
             {:name "joseph" :loves "punchi"}])


(defn match->map
  "convert vector of one transaction to a map"
  [[_ type from date id sucursal balance amount]]
  {:type (str/trim type)
   :from from
   :date date
   :sucursal sucursal
   :amount (str/trim amount)
   :id id})




(def line-format
  ;  type          from  date       id      suc   balance      amount
   #"(?m)^([\w \:\/\.\(\)]+) *\(([\s\w\.\\]*)\)([\d\/]*) (\d*) (\d*) ([\d,]*.\d\d)([\d,]*.\d\d) *$"
   ;(Pattern/compile "^([\\w \:\\/\\.\\(\\)]+) *\(([\\s\\w\\.\\]*)\\)([\\d\\/]*) (\\d*) (\\d*) ([\\d,]*.\\d\\d)([\\d,]*.\\d\\d) *$" Pattern/MULTILINE) 
  )

(def text-demo-in
  (str
   "N/C: ACH(EIDA REBECA ARIA BANCA MOVIL GLOBAL BANK)23/08/2022 1261866106 0 18,941.89315.00" "\n"
   "DEPOSITO COMPLETO ()08/11/2022 265649501 30 24,191.33105.00" "\n"
   "N/C: ACH(LUIS EDUARDO ANG Pago nov 2022)09/11/2022 1329041835 0 24,296.33105.00" "\n"
   "N/C: N/C ACH(LUIS EDUARDO ANG Pago nov 2022)09/11/2022 1329041835 0 24,296.33105.00" "\n"
   "DEPOSITO COMPLETO ()12/11/2022 284948830 25 24,396.33100.00 " "\n"
   "N/C: B.LINEA           N/C ACH BANCA EN LINEA(mantenimiento 3C)23/09/2022 222824955 1 20,885.33105.00" "\n"
   "DEPOSITO COMPLETO ()25/02/2022 232496850 5 32,969.91330.00" "\n"
   "N/C: B.LINEA           N/C ACH BANCA EN LINEA(Carrillon 3C 
mantenimiento)" "\n"

   ))

(def text-demo-out
  (str
   "PAGO DE CHEQUE (No. 1825)15/08/2022 216396550 17 46,717.69207.73" "\n"
  ))

(defn sane-type [type]
  (let [stype (if (or (nil? type)
                      (str/blank? type))
                "" type)
        stype (str/replace stype #"\n" "")
        stype (str/trim stype)]
    stype))

(defn limit-type [stype]
  (subs stype 0 (min 50 (count stype))))


(defn assoc-io
  "associates :io"
  [{:keys [type] :as m}]
  (let [type (sane-type type) 
        stype (limit-type type)
        io (cond 
             ; incoming
             (not (= (:apt m) :xxx)) :i ; matched trans is incoming
             (str/starts-with? type "N/C:") :i
             ;(= "N/C: ACH" type) :i
             ;(= "N/C: N/C ACH" type) :i ; from global bank
             ;(= "N/C: B.LINEA           N/C ACH BANCA EN LINEA" type) :i ; from global bank
             (= "DEPOSITO COMPLETO" type) :i
             (= "CHEQUE PRESENTADO POR COMPENSACION DEVUELTO" type) :i
             (= "DEPOSITO COMPLETO" type) :i
             ; outgoing
             (= "CHEQUE PRESENTADO POR COMPENSACION" type) :o
             (= "PAGO DE CHEQUE" type) :o
             (= "CERTIFICACION DE CHEQUES" type) :o
             (= "ND POR CHQ DEVUELTO" type) :o
             (str/starts-with? type "N/D:") :o
             ; undefined
             :else (do 
                     (println (format "undefined type:[%s]" stype))
                     :u))]
    (assoc m :io io :type stype)))


(comment 
  (->> (re-seq line-format text-demo-in)
          (map match->map)
         ;(map :type)
          ;(map incoming? ) 
          (map assoc-io))
  
  (->> (re-seq line-format text-demo-out)
       (map match->map)
        ;(map outgoing?)
        (map assoc-io))
  ;
 )

(defn of-io [io-target]
  (fn [{:keys [io]}]
    (= io io-target)))


#_ (defn text->transactions2 [text]
  (let [transactions-vecs (re-seq line-format text)
        transactions (map match->map transactions-vecs)
        incoming-transactions (filter incoming? transactions)
        incoming-transactions-with-apt (map assoc-apartment incoming-transactions)
        ]
      incoming-transactions-with-apt))


(defn text->transactions [text]
 (->> (re-seq line-format text)
      (map match->map)
      (map assoc-apartment)
      (map assoc-io)))

(defn extract-transactions [filename]
  (let [text-pdf (text/extract filename)
        transactions (text->transactions text-pdf)]
    {:text text-pdf
     :transactions transactions}))


(comment 
  (re-matches #"(?s)\d{3}.\d{3}" "123\n456")
  (re-find line-format   "DEPOSITO COMPLETO ()08/11/2022 265649501 30 24,191.33105.00")
 (re-find line-format text-demo-in)
 (->> (re-seq line-format text-demo-in)
     (map match->map))
  
  (text->transactions text-demo-in)
  
  (def text-pdf 
      (text/extract "2022.pdf"))
  
  (count text-pdf)
 (println 
  (subs text-pdf 10000)
  )
  
  (def transactions 
     (text->transactions text-pdf))
  
  (count transactions)
 (first transactions)
  
 ;
  )




