(ns carrillon.pdf
  (:require
   ;[clojure.pprint] 
   [clojure.pprint :refer [print-table]]
   [pdfboxing.text :as text]))

; call print-table "fully qualified" = with namespace
#_(clojure.pprint/print-table [{:name "chris" :loves "read"}
                             {:name "joseph" :loves "punchi"}
                             ])

; call print-table only with function name
#_(print-table [{:name "chris" :loves "read"}
             {:name "joseph" :loves "punchi"}])


(defn match->map [[_ type from date id sucursal balance amount]]
  {:type type
   :from from 
   :date date
   :sucursal sucursal
   :amount amount})

(def apartments
  {:L1 [#"AMAN"] ;aman singh. 
   :L2 [#"AMAN"] ;	aman singh
   :L3 [#"CHAN"] ; ricardo how chan. rosana how chan.
   :L4	[#"CAROLINA MARIA" #"MIRIAM JOSEFINA"] ;	luis corrarubia
   :L5	[#"EFIGENIA COGLEY"] ;	leureliz holding (lisbeth)
   :L6	[#"CANO" #"EXPERT TRAVEL"] ;	isabel cano
   :L7	[#"XISPAVA"] ;	xispava sa administrador
   :L8	[#"XISPAVA"] ;	xispava sa administrador
   :L9	[#"XISPAVA"] ;	xispava sa administrador
   :1A	[#"GRIMALDO IBRAHIM" #"ZULEIKA MIREYA" #"GABRIEL GERARDO"] ;	zumarco sa. grimaldo. mama lureica.
   :1B	[#"NIVIA"] ;	nivia vernaza  
   :1C	[#"AIZAGA"] ;luis aizaga
   :1D	[#"BGENERAL"] ;banco general (alexander montilla ortega)
   :2A	[] ; Prof. Guillermo Alegría Zúñiga minlsterio educacion.
   :2B	[#"EMCA"] ;immobiliaria emca (adonai abogado)
   :2C	[#"FLOR CHAVEZ GUTI"] ;	flor chavez contadora.
   :2D	[#"LUIS EDUARDO ANG"] ;	luis anguizola inmobilia came
   :3A	[#"3A"] ;	odette poschl. sonja poschl. 
   :3B	[#"AMAN"] ;	aman singh.
   :3C	[#"DIDIA BARRANCO" #"3C"] ;	lisca linette luna barranco
   :3D	[#"JESSICA LIRIETH"] ;	estellina mendoza
   :4A	[#"IVETTE CRISTINA" #"ROSA ALCEDO"] ;	Rosemary Dickinson
   :4B	[#"JOSE ALBERTO MOL"] ;	evelia gonzales. jose alberto
   :4C	[#"DANIELA VIRGINIA"] ;	corp admin electronica erick y daniela
   :4D	[#"FERNANDEZ"] ;	angel j fernandez bitchito fundacion
   :5A	[#"RODRIGUEZ"] ;	jose luis rodriguez
   :5B	[#"5B" #"ROBERTO PEREZ" #"MARGO MANUELLA"] ;	margho callaghan (2 apartments)
   :5C	[#"5C" #"MARGO MANUELLA" ] ;	margho callaghan
   :5D	[#"FUNDACION SARUD"] ;	fundacion sarud
   :6A	[#"EIDA REBECA ARIA"] ;	chino. sang ho li. eida
   :6B	[#"SER" #"ALVARO SANTIDRIA"] ;	ser holdings corp
   :6C	[#"MARIA DE JESUS"] ; maria jesus
   :6D	[#"PHILIPPE JEAN LO"] ; philppe antoine gelis. 
   :out	[#"No." #"GIRADOS"] ; payments with cheque
   })

(defn match-first [patterns name]
   (some #(re-find % name) patterns))

(comment 
  (match-first (:6B apartments) "asdf")
  (match-first (:6B apartments) "SER")
  (match-first (:6B apartments) "SER HOLDINGS")
  (match-first (:6B apartments) "Alvaro Santi") 
  ;
  )

(defn find-apartment [name]
  (some (fn [[apt patterns]]
                  (when-let [m (match-first patterns name)]
                    apt)) apartments))

(comment 
  (find-apartment "SER")  
  ;
 )

(defn assoc-apartment 
  "associates :apt"
  [{:keys [from] :as m}]
  (let [apt (or (find-apartment from) :xxx)]
    (assoc m :apt apt)))

(def line-format
  #"([\w\s\:\/]+)\((.*)\)([\w\/]*)\s(\d*)\s(\d*)\s([\d,]*.\d\d)([\d,]*.\d\d)\s*")

(def text-demo
  (str
   "N/C: ACH(EIDA REBECA ARIA BANCA MOVIL GLOBAL BANK)23/08/2022 1261866106 0 18,941.89315.00" "\n"
   "DEPOSITO COMPLETO ()08/11/2022 265649501 30 24,191.33105.00" "\n"
   "N/C: ACH(LUIS EDUARDO ANG Pago nov 2022)09/11/2022 1329041835 0 24,296.33105.00" "\n"
   "N/C: N/C ACH(LUIS EDUARDO ANG Pago nov 2022)09/11/2022 1329041835 0 24,296.33105.00" "\n"
   "DEPOSITO COMPLETO ()12/11/2022 284948830 25 24,396.33100.00 " "\n"
   "PAGO DE CHEQUE (No. 1825)15/08/2022 216396550 17 46,717.69207.73"
  ))


(defn incoming? [{:keys [type] :as m}]
  (println "transaction type: ]" type "[")
  (or (= "N/C: ACH" type)
      (= "DEPOSITO COMPLETO " type)
      (= "N/C: N/C ACH" type) ; from global bank
      ))


(defn text->transactions [text]
 (->> (re-seq line-format text)
      (map match->map)
      (filter incoming?)
      (map assoc-apartment)))

(defn print-apartment [transactions apt]
  (let [fields (if (= apt :xxx)
                 [:date :amount :from :sucursal]
                 [:date :amount :from])] 
  (println "***** " apt " *******")  
  (->> transactions
      (filter #(= apt (:apt %)))
       (print-table fields))))


(defn print-all-apartments [transactions]
  (doall 
   (map 
    (partial print-apartment transactions)
    (conj (keys apartments) :xxx))))

(defn run [opts]
  (let [text-pdf (text/extract "2022.pdf")
        transactions (text->transactions text-pdf)]
    (print-all-apartments transactions)  
    ))

(comment 
  (re-matches #"(?s)\d{3}.\d{3}" "123\n456")
  (re-find line-format   "DEPOSITO COMPLETO ()08/11/2022 265649501 30 24,191.33105.00")
 (re-find line-format text-demo)
 (->> (re-seq line-format text-demo)
     (map match->map))
  
  (text->transactions text-demo)
  
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
  
  (print-table [:date :apt :amount] transactions)

(print-apartment transactions :6B)
(print-apartment transactions :xxx)
(print-all-apartments transactions)

 
 ;
  )




