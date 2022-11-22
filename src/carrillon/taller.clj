(ns carrillon.taller)


(defn odd [n] 
  (let [r (unchecked-remainder-int n 2)]
     (if (= r 1) true false
    ) 
 
    ))

(unchecked-remainder-int 8 2)

(odd 487)
(odd 500)

(filter odd [ 1 2 3 4 5 6 7])
(filter odd (range 1000))

(remove odd (range 1000))

(def participants [{:name "joseph" :sex :m}
                   {:name "christian" :sex :m}
                   {:name "isa" :sex :f}])

(filter male participants)

(defn male [p]
  (if (= :m (:sex p))
    true      
    false))

(male {:name "joseph" :sex :m})
(male {:name "isa" :sex :f})

(male {:name "elizabeth" :sex :f})
(male {:name "punchi" :specie "dog" :sex :m})

(filter male participants)
(remove male participants)


 

(first participants)



(:sex (first participants))
(last participants)

