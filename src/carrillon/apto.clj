(ns carrillon.apto
  (:require
    [clojure.string :as str]))


(def apartments
  (sorted-map 
   :L1 [#"AMAN"] ;aman singh. 
   :L2 [#"AMAN"] ;	aman singh
   :L3 [#"CHAN"] ; ricardo how chan. rosana how chan.
   :L4	[#"CAROLINA MARIA" #"MIRIAM JOSEFINA" #"EXPERT TRAVEL"] ;	luis corrarubia
   :L5	[#"EFIGENIA COGLEY"] ;	leureliz holding (lisbeth)
   :L6	[#"CANO" ] ;	isabel cano
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
   :5C	[#"5C" #"MARGO MANUELLA"] ;	margho callaghan
   :5D	[#"FUNDACION SARUD"] ;	fundacion sarud
   :6A	[#"EIDA REBECA ARIA"] ;	chino. sang ho li. eida
   :6B	[#"SER" #"ALVARO SANTIDRIA"] ;	ser holdings corp
   :6C	[#"MARIA DE JESUS"] ; maria jesus
   :6D	[#"PHILIPPE JEAN LO"] ; philppe antoine gelis. 
   ;:out	[#"No." #"GIRADOS"] ; payments with cheque
   :xxx []
   ))

(defn match-first [patterns name]
  (some #(re-find % name) patterns))


(comment
  (match-first (:6B apartments) "asdf")
  (match-first (:6B apartments) "SER")
  (match-first (:6B apartments) "SER HOLDINGS")
  (match-first (:6B apartments) "Alvaro Santi")
  ;
  )

(defn find-apartment
  "links a name to an apartment
   returns keywod"
  [name]
  (some (fn [[apt patterns]]
          (when-let [m (match-first patterns name)]
            apt)) apartments))

(comment
  (find-apartment "SER")
  (find-apartment "christian")
  ;
  )

(defn assoc-apartment
  "associates :apt"
  [{:keys [from] :as m}]
  (let [apt (or (find-apartment from) :xxx)]
    (assoc m :apt apt)))


(defn apartment-category [apt]
  (let [[f l](name apt)
        f (str f)
        l (str l)]
    (cond 
      (= f "L") 
        {:cat :local
         :cost 126.0}
      (or (= l "A") (= l "B"))
        {:cat :big
         :cost 150.0}
      (or (= l "C") (= l "D"))
        {:cat :small
         :cost 105.0}
      :else
        {:cat :xxx
         :cost 0.0})))
  
  (comment
       (apartment-category :L6)
      (apartment-category :6A)
      (apartment-category :6B)
      (apartment-category :6C)
      (apartment-category :6D)
    (->  (apartment-category :6D)
         :cat
         name
     )
     
    
    
    ;
    )
  
  (defn quota [year month apt]
    (let [{:keys [cat cost]} (apartment-category apt)
          date  (str "01/" month "/" year) ; 25/10/2019
          from (str "mant " (name cat) " " year "-" month)
          amount-f (- 0 cost)
          ]
    {:io :i
     :amount (str amount-f)
     :amount-f amount-f
     :date date
     :from from
     }))
  
  
  (defn quota-year [year m-start m-end apt]
    (map   
      #(quota year % apt)
      (range m-start (inc m-end))))
  
  
(defn apartment-quotas [apt]
  (concat 
     (quota-year 2019 06 12 apt)
     (quota-year 2020 01 12 apt)
     (quota-year 2021 01 12 apt)
     (quota-year 2022 01 12 apt)))
   
  
  (comment 
       (quota 2022 11 :6B)
       (quota 2022 11 :6C)
       (quota 2022 11 :L3)
     (-> (quota-year 2022 01 11 :L3)
         count ) 
     (-> (quota-year 2021 01 11 :L3)
         count)
    
    (-> (apartment-quotas :L3)
        count)
    
     
    
    

    
    
    
    ;
    )