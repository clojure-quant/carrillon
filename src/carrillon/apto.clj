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

