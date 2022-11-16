(ns carillon.pdf
  (:require
   [clojure.pprint :refer [print-table]]
   [pdfboxing.text :as text]))

(defn match->map [[_ type from date id sub balance amount]]
  {:type type
   :from from 
   :date date
   :amount amount})

(def apartments
  {:L1 [#"AMAN"] ;aman singh. 
   :L2 [#"AMAN"] ;	aman singh
   :L3 [#"EXPERT TRAVEL"] ; ricardo how chan. rosana how chan.
   :L4	[#"CAROLINA MARIA"] ;	luis corrarubia
   :L5	[#"EFIGENIA COGLEY"] ;	leureliz holding (lisbeth)
   :L6	[#"CHAN"] ;	isabel cano
   :L7	[#"CHAN"] ;	xispava sa administrador
   :L8	[#"CHAN"] ;	xispava sa administrador
   :L9	[#"CHAN"] ;	xispava sa administrador
   :1A	[#"GRIMALDO IBRAHIM" #"ZULEIKA MIREYA" #"GABRIEL GERARDO"] ;	zumarco sa. grimaldo. mama lureica.
   :1B	[#"CHAN"] ;	nivia vernaza  
   :1C	[#"CHAN"] ;luis aizaga
   :1D	[#"CHAN"] ;banco general (alexander montilla ortega)
   :2A	[] ; Prof. Guillermo Alegría Zúñiga minlsterio educacion.
   :2B	[#"CHAN"] ;immobiliaria emca (adonai abogado)
   :2C	[#"FLOR CHAVEZ GUTI"] ;	flor chavez contadora.
   :2D	[#"LUIS EDUARDO ANG"] ;	luis anguizola inmobilia came
   :3A	[#"CHAN"] ;	odette poschl. sonja poschl. 
   :3B	[#"CHAN"] ;	aman singh.
   :3C	[#"DIDIA BARRANCO"] ;	lisca linette luna barranco
   :3D	[#"CHAN"] ;	estellina mendoza
   :4A	[#"IVETTE CRISTINA" #"ROSA ALCEDO"] ;	Rosemary Dickinson
   :4B	[#"JOSE ALBERTO MOL"] ;	evelia gonzales. jose alberto
   :4C	[#"DANIELA VIRGINIA"] ;	corp admin electronica erick y daniela
   :4D	[#"CHAN"] ;	angel j fernandez bitchito fundacion
   :5A	[#"CHAN"] ;	jose luis rodriguez
   :5B	[#"MARGO MANUELLA"] ;	margho callaghan (2 apartments)
   :5C	[#"MARGO MANUELLA" #"ROBERTO PEREZ"] ;	margho callaghan
   :5D	[#"FUNDACION SARUD"] ;	fundacion sarud
   :6A	[#"EIDA REBECA ARIA"] ;	chino. sang ho li. eida
   :6B	[#"SER" #"ALVARO SANTIDRIA"] ;	ser holdings corp
   :6C	[#"MARIA DE JESUS"] ; maria jesus
   :6D	[#"PHILIPPE JEAN LO"] ; philppe antoine gelis. 
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
   "DEPOSITO COMPLETO ()08/11/2022 265649501 30 24,191.33105.00" "\n"
   "N/C: ACH(LUIS EDUARDO ANG Pago nov 2022)09/11/2022 1329041835 0 24,296.33105.00" "\n"
   "DEPOSITO COMPLETO ()12/11/2022 284948830 25 24,396.33100.00 " "\n"))


(defn text->transactions [text]
 (->> (re-seq line-format text)
      (map match->map)
      (map assoc-apartment)))

(defn print-apartment [transactions apt]
  (let [fields (if (= apt :xxx)
                 [:date :amount :from]
                 [:date :amount])]
  (->> transactions
      (filter #(= apt (:apt %)))
       (print-table fields))))

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
  (subs text-pdf 100)
  )
  
  (def transactions 
     (text->transactions text-pdf))
  
  (count transactions)
 (first transactions)
  
  (print-table [:date :apt :amount] transactions)
  :
  )


(print-apartment transactions :6B )
(print-apartment transactions :xxx)




()         
              