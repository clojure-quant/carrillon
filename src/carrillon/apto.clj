(ns carrillon.apto
  (:require
   [clojure.string :as str]))


(def apartments
  (sorted-map
   :L1 {:owner "aman singh"
        :bank [#"AMAN"]}
   :L2 {:owner "aman singh"
        :bank [#"AMAN"]}
   :L3 {:owner "ricardo how chan. rosana how chan"
        :bank [#"CHAN"]}
   :L4 {:owner "luis corrarubia"
        :bank 	[#"CAROLINA MARIA" #"MIRIAM JOSEFINA" #"EXPERT TRAVEL"]}
   :L5 {:owner "leureliz holding (lisbeth)"
        :bank [#"EFIGENIA COGLEY"]}
   :L6 {:owner "isabel cano"
        :bank 	[#"CANO"]}
   :L7	{:owner "xispava sa agenor"
        :bank [#"XISPAVA"]}
   :L8	{:owner "xispava sa agenor"
        :bank [#"XISPAVA"]}
   :L9	{:owner "xispava sa agenor"
        :bank [#"XISPAVA"]}
   :1A	{:owner "zumarco sa. grimaldo. mama lureica."
        :bank [#"GRIMALDO IBRAHIM" #"ZULEIKA MIREYA" #"GABRIEL GERARDO"]}
   :1B	{:owner "nivia vernaza"
        :bank [#"NIVIA"]}
   :1C	{:owner "luis aizaga"
        :bank [#"AIZAGA"]}
   :1D	{:owner "banco general (alexander montilla ortega)"
        :bank [#"BGENERAL"]}
   :2A	{:owner "Prof. Guillermo Alegría Zúñiga minlsterio educacion."
        :bank []}
   :2B	{:owner "immobiliaria emca (adonai abogado)"
        :bank [#"EMCA"]}
   :2C	{:owner "flor chavez contadora."
        :bank [#"FLOR CHAVEZ GUTI"]}
   :2D	{:owner "luis anguizola inmobilia came"
        :bank [#"LUIS EDUARDO ANG"]}
   :3A	{:owner "odette poschl. sonja poschl. "
        :bank [#"3A"]}
   :3B	{:owner "aman singh"
        :bank [#"AMAN"]}
   :3C	{:owner "lisca linette luna barranco"
        :bank [#"DIDIA BARRANCO" #"3C"]}
   :3D	{:owner "estellina mendoza"
        :bank [#"JESSICA LIRIETH"]}
   :4A	{:owner "Rosemary Dickinson"
        :bank [#"IVETTE CRISTINA" #"ROSA ALCEDO"]}
   :4B	{:owner "evelia gonzales. jose alberto"
        :bank [#"JOSE ALBERTO MOL"]}
   :4C	{:owner "corp admin electronica erick y daniela"
        :bank [#"DANIELA VIRGINIA"]}
   :4D	{:owner "angel j fernandez bitchito fundacion"
        :bank [#"FERNANDEZ"]}
   :5A	{:owner "jose luis rodriguez"
        :bank [#"RODRIGUEZ"]}
   :5B	{:owner "margho callaghan (ex roberto)"
        :bank [#"5B" #"ROBERTO PEREZ" #"MARGO MANUELLA"]}
   :5C	{:owner "margho callaghan"
        :bank [#"5C" #"MARGO MANUELLA"]}
   :5D	{:owner "fundacion sarud"
        :bank [#"FUNDACION SARUD"]}
   :6A	{:owner "sang ho li. eida"
        :bank [#"EIDA REBECA ARIA"]}
   :6B	{:owner "ser holdings corp (ex alvaro)"
        :bank [#"SER" #"ALVARO SANTIDRIA"]}
   :6C	{:owner "maria jesus"
        :bank [#"MARIA DE JESUS"]}
   :6D	{:owner "philppe antoine gelis"
        :bank [#"PHILIPPE JEAN LO"]}
   ;:out	[#"No." #"GIRADOS"] ; payments with cheque
   :xxx []))

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
  (some (fn [[apt {:keys [bank]}]]
          (when-let [m (match-first bank name)]
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
  (let [[f l] (name apt)
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
       name)



    ;
  )

(defn quota [year month apt]
  (let [{:keys [cat cost]} (apartment-category apt)
        date  (str "01/" month "/" year) ; 25/10/2019
        from (str "mant " (name cat) " " year "-" month)
        amount-f (- 0 cost)]
    {:io :i
     :amount (str amount-f)
     :amount-f amount-f
     :date date
     :from from}))


(defn quota-year [year m-start m-end apt]
  (if (= :xxx apt) []
    (map
     #(quota year % apt)
     (range m-start (inc m-end)))))


(defn apartment-quotas [apt]
  (concat
   (quota-year 2019 06 12 apt)
   (quota-year 2020 01 12 apt)
   (quota-year 2021 01 12 apt)
   (quota-year 2022 01 11 apt)))


(comment
  (quota 2022 11 :6B)
  (quota 2022 11 :6C)
  (quota 2022 11 :L3)
  (-> (quota-year 2022 01 11 :L3)
      count)
  (-> (quota-year 2021 01 11 :L3)
      count)

  (-> (apartment-quotas :L3)
      count)








    ;
  )