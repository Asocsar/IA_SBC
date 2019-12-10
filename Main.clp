(defclass Libro_Fantasia
	(is-a USER)
	(role concrete)
	(single-slot Title
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Year
		(type INTEGER)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Pages
		(type INTEGER)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot Writer
		(type INSTANCE)
;+		(allowed-classes Autor)
		(cardinality 1 2)
		(create-accessor read-write))
	(single-slot Language
		(type SYMBOL)
		(allowed-values Easy Medium Hard)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Popularity
		(type SYMBOL)
		(allowed-values Popular Critic Best_Seller Normal Non-Popular)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Epic
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Fabula
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Magic
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Adventure
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Cyberpunk
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Superheores
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Sword_and_Sorcery
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Spooky
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Romantic
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Dark
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Science+Fiction
	(is-a Libro_Fantasia)
	(role concrete))

(defclass Persona
	(is-a USER)
	(role concrete)
	(single-slot Surname
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Nationality
		(type SYMBOL)
		(allowed-values Spain Norway EU France UK Japan Greece)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Name
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Autor
	(is-a Persona)
	(role concrete)
	(single-slot Difficulty
		(type SYMBOL)
		(allowed-values Easy Medium Hard)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot LIbro
		(type INSTANCE)
;+		(allowed-classes Libro_Fantasia)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write)))

(defclass Lector
	(is-a Persona)
	(role concrete)
	(single-slot Frequency
		(type SYMBOL)
		(allowed-values Daily Ocasionally Whenever)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot Moment
		(type SYMBOL)
		(allowed-values Morning Night Evening)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(single-slot Taste
		(type SYMBOL)
		(allowed-values Popular Critic Best_Seller National Foreigner)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Age
		(type INTEGER)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot Place
		(type SYMBOL)
		(allowed-values Home Public_Transport Public_Space)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(multislot Friend
		(type INSTANCE)
;+		(allowed-classes Persona)
		(create-accessor read-write)))



;;;****************************
;;;* ESTRUCTURAS *
;;;****************************

(deftemplate stop_gen (multislot st (type STRING)))
(deftemplate stop_aut (multislot st (type STRING)))
(deftemplate stop_pag (multislot st (type STRING)))
(deftemplate autores (multislot name (type STRING)))
(deftemplate generof (multislot titles (type SYMBOL)))
(deftemplate autoresf (multislot name (type STRING)))
(deftemplate solution (multislot titles (type STRING)))
(deftemplate filtro (multislot titles (type STRING)) (multislot number (type FLOAT) (default 0.0)))
(deftemplate lista (multislot name (type STRING)) (multislot number (type INTEGER)))
(deftemplate books (slot name (type STRING)) (slot puntuaje (type INTEGER) (default 0)))
(deftemplate likes (slot id (type SYMBOL)))
(deftemplate age (slot age (type INTEGER )))
(deftemplate nbooks (slot num (type INTEGER)))
(deftemplate nhours (slot num (type INTEGER)))
(deftemplate npages (slot num (type INTEGER)))
(deftemplate setting (slot id (type SYMBOL)))
(deftemplate narrative (slot id (type SYMBOL)))
(deftemplate nationality (slot id (type SYMBOL)))
(deftemplate writers (multislot name (type SYMBOL)))



;;;****************************
;;;* FUNCTIONS *
;;;****************************

(deffunction ask-integer (?question ?init ?ende)
   (printout t ?question crlf)
   (bind ?answer (read))
   (bind ?end FALSE)
   (while (not ?end) do
      (if (eq (type ?answer) INTEGER) then
      (if (and (<= ?answer ?ende) (>= ?answer ?init)) then
          (bind ?end TRUE)
        else
      (printout t "The number has to be between ")
			(printout t ?init)
			(printout t " and ")
			(printout t ?ende)
			(printout t ", you fool " crlf)
      (bind ?answer (read)))
      else
      (printout t "Please write a number " crlf)
      (bind ?answer (read))
      ))
      ?answer)

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question crlf)
   (bind ?answer (read))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question crlf)
      (bind ?answer (read))
  ?answer) ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE
       else FALSE))

(deffunction class_pages (?pages)
	(bind ?r "")
	(if (and (>= ?pages 0) (<= ?pages 150)) then
		(bind ?r Short)
	)
	(if (and (> ?pages 150) (<= ?pages 400)) then
		(bind ?r Average)
	)
	(if (> ?pages 400) then
		(bind ?r Long)
	) ?r)


;;;****************************
;;;* RULES TO DETERMINE IF A BOOK WILL BE OR NOT PART OF THE SOLUTION *
;;;****************************


(defrule puntuaje_difficultad ""
	 (declare (salience 99))
	 ?i <- (object (Title ?title) (Language ?lang))
	 ?j <- (likes (id ?e))
	 (test (eq ?lang ?e))
	 =>
	 (bind ?st (nth$ 1 (find-fact ((?l books)) (eq ?l:name ?title))))
	 (modify ?st (name ?title) (puntuaje (+ (fact-slot-value ?st puntuaje) 1)))
	 (assert (difficultad))
	 )

(defrule puntuaje_genero ""
	 (declare (salience 99))
	 ?i <- (object (Title ?title))
	 ?j <- (likes (id ?e))
	 (test (eq (type ?i) ?e))
	 =>
	 (bind ?st (nth$ 1 (find-fact ((?l books)) (eq ?l:name ?title))))
	 (modify ?st (name ?title) (puntuaje (+ (fact-slot-value ?st puntuaje) 1) ) )
	 (assert (genero))
	 )

(defrule puntuaje_paginas ""
	 (declare (salience 99))
	 ?i <- (object (Title ?title) (Pages ?p))
	 ?j <- (likes (id ?e))
	 (test (eq (class_pages ?p) ?e))
	 =>
	 (bind ?st (nth$ 1 (find-fact ((?l books)) (eq ?l:name ?title))))
	 (modify ?st (name ?title) (puntuaje (+ (fact-slot-value ?st puntuaje) 1)))
	 (assert (paginas))
	 )

(defrule puntuaje_popularidad ""
	 (declare (salience 99))
	 ?i <- (object (Title ?title) (Popularity ?pop))
	 ?j <- (likes (id ?e))
	 (test (eq ?pop ?e))
	 =>
	 (bind ?st (nth$ 1 (find-fact ((?l books)) (eq ?l:name ?title))))
	 (modify ?st (name ?title) (puntuaje (+ (fact-slot-value ?st puntuaje) 1)))
	 (assert (popularidad))
   )

(defrule puntuaje_idioma ""
  (declare (salience 99))
  ?i <- (object (Title ?title) (Writer ?w))
  (object (name ?w) (Nationality ?nat))
  ?j <- (likes (id ?e))
  (test (eq ?e ?nat))
  =>
  (bind ?st (nth$ 1 (find-fact ((?l books)) (eq ?l:name ?title))))
  (modify ?st (name ?title) (puntuaje (+ (fact-slot-value ?st puntuaje) 1)))
  (assert (idioma)))


(defrule ordena-llista
  (declare (salience 10))
	?indice <- (lista (name $?t_ini ?t1 ?t2 $?t_end) (number $?n_ini ?n1 ?n2 $?n_end))
	(test (eq (length$ ?t_ini) (length$ ?n_ini)))
	(test (< ?n1 ?n2))
	=>
	(assert (lista (name ?t_ini ?t2 ?t1 ?t_end) (number ?n_ini ?n2 ?n1 ?n_end)))
	(retract ?indice))

  (defrule ordena-llista_ref
    (final_aut $?)
    (final_gen $?)
		(final_pages $?)
  	?indice <- (filtro (titles $?t_ini ?t1 ?t2 $?t_end) (number $?n_ini ?n1 ?n2 $?n_end))
  	(test (eq (length$ ?t_ini) (length$ ?n_ini)))
  	(test (< ?n1 ?n2))
  	=>
  	(assert (filtro (titles ?t_ini ?t2 ?t1 ?t_end) (number ?n_ini ?n2 ?n1 ?n_end)))
  	(retract ?indice))

(defrule puntuaje_genros_refinamiento
  (declare (salience 10))
  (generof (titles $?selec))
  ?f <- (filtro (titles $?t_ini ?t $?t_end) (number $?n_ini ?n $?n_end))
  ?o <- (object (Title ?tit))
  ?g <- (stop_gen (st $?gen))
  (test (not (member ?t ?gen)))
  (test (eq ?tit ?t))
  (test (or (eq (length$ ?t_ini) (length$ ?n_ini)) (eq (length$ ?t_end) (length$ ?n_end))))
  =>
  (bind ?cond FALSE)
  (if (eq (type ?selec) SYMBOL) then (bind ?cond (eq ?selec (type ?o)))
  else (bind ?cond (member (type ?o) ?selec)))
  (if ?cond then
  (modify ?f (titles ?t_ini ?t ?t_end) (number ?n_ini (+ ?n 1) ?n_end))
  (modify ?g (st ?gen ?t)))
  (assert (final_gen)))


  (defrule puntuaje_autores_refinamiento
    (declare (salience 10))
    (autoresf (name $?selec))
    ?f <- (filtro (titles $?t_ini ?t $?t_end) (number $?n_ini ?n $?n_end))
    ?o <- (object (Title ?tit) (Writer ?w))
    (object (name ?w) (Surname ?s))
    ?g <- (stop_aut (st $?gen))
    (test (not (member ?t ?gen)))
    (test (eq ?tit ?t))
    (test (or (eq (length$ ?t_ini) (length$ ?n_ini)) (eq (length$ ?t_end) (length$ ?n_end))) )
    =>
    (bind ?cond FALSE)
    (if (eq (type ?selec) STRING) then (bind ?cond (eq ?selec ?s))
    else (bind ?cond (member ?s ?selec)))
    (if ?cond then
    (modify ?f (titles ?t_ini ?t ?t_end) (number ?n_ini (+ ?n 1) ?n_end))
    (modify ?g (st ?gen ?t)))
		(assert (final_aut)))


	(defrule puntuaje_paginas_refinamiento
		(declare (salience 10))
		?pag <- (npages (num ?npaginas))
		?ind <- (filtro (titles $?t_ini ?t $?t_fin) (number $?n_ini ?n $?n_end))
		(object (Title ?tit) (Pages ?p))
		?g <- (stop_pag (st $?gen))
		(test (not (member ?t ?gen)))
		(test (eq ?tit ?t))
		=>
		(bind ?diff (- ?p ?npaginas))
		(if (< ?diff 0) then (bind ?diff (* ?diff -1)))
		(modify ?ind (titles ?t_ini ?t ?t_fin) (number ?n_ini (+ ?n (- 1 (/ ?diff 5000))) ?n_end))
		(modify ?g (st ?gen ?t))
		(assert (final_pages))
		)

;book1 5 book2 4 book3 3 book4 3 book5 2

;size ?t + 1 <= 3  --> 0 + 1 --> ?t (book1 )
;size ?t + 1 <= 3  --> 1 + 1 --> ?t (book1 book2)
;size ?t + 1 <= 3  --> 2 + 2 --> ?t (book1 book2 book4)



(defrule filtracion ""
  (declare (salience 5))
  (filter $?)
  ?l <- (lista (name $?titles) (number $?puntuajes))
 =>
  (bind ?i 1)
  (bind ?finish FALSE)
  (bind $?values "")
  (while (not ?finish) do
  (bind $?temp "")
  (bind ?diferent TRUE)
    (while (eq ?diferent TRUE) do
      (if (eq ?temp "") then
        (bind ?temp (nth$ ?i ?titles))
      else
      (bind ?temp ?temp (nth$ ?i ?titles)))
      (bind ?le (<= (+ ?i 1) (length$ ?titles)))
      (bind ?diferent (<= ?i (length$ ?titles)))
      (if ?le then (bind ?diferent (and ?diferent (eq (nth$ (+ ?i 1) ?puntuajes) (nth$ ?i ?puntuajes)))))
      (bind ?i (+ ?i 1))
    )
    (bind ?v1 "")
    (bind ?v2 "")
    (if (eq (type ?values) STRING ) then (bind ?v1 1) else (bind ?v1 (length$ ?values)))
    (if (eq (type ?temp) STRING ) then (bind ?v2 1) else (bind ?v2 (length$ ?temp)))
    (if (> (+ ?v2 ?v1) 3) then (bind ?finish TRUE)
    else
    (if (eq ?values "") then (bind ?values ?temp)
      else
      (bind ?values ?values ?temp))))

  (bind ?end "")
  (if (eq (type ?values) STRING) then (bind ?end 2) else (bind ?end (- 3 (length$ ?values))))

  (if (<> ?end 0) then
		(bind $?numeros 0.0)
		(loop-for-count (?i 2 (length$ ?temp)) do (bind ?numeros ?numeros 0.0))
    (assert (generos_filtro))
    (assert (autores_filtro))
    (assert (filtro (titles ?temp) (number ?numeros))))
  (assert (solution (titles ?values)))
 )

;;;****************************
;;;* INITIAL AND END OF THE PROGRAM *
;;;****************************

(defrule assertion ""
 (initial $?)
 ?i <- (object (Title ?title))
 	=>
	(assert (books (name ?title) ) ) )


(defrule system-banner ""
	(not (initial ?))
  =>
  (load-instances "Instancies.pins")
  (assert (initial))
  (assert (stop_aut (st "none")))
  (assert (stop_gen (st "none")))
	(assert (stop_pag (st "none")))
  (assert (autores (name "")))
	(assert (autoresf (name "")))
	(assert (generof (titles None)))
  (bind $?i 1)
  (printout t crlf crlf)
  (printout t "System of Recomendation for Books")
  (printout t crlf crlf))

(defrule end ""
	(declare (salience -10))
	?l <- (solution (titles $?t))
	?f <- (filtro (titles $?tit) (number $?num))
	(final_aut $?)
	(final_gen $?)
	(final_pages $?)
	=>
	(bind ?i 1)
	(bind ?k 1)
	(printout t "We recomend you the following list of books" crlf)
	(if (not(eq (nth$ 1 ?t) "")) then
	(printout t ?t crlf)
	(bind ?i (+ ?i 1))
	(while (<= ?i 3)
	do
	(printout t (nth$ ?k ?tit) crlf)
	(bind ?i (+ ?i 1))
	(bind ?k (+ ?k 1)))
	else
	(while (<= ?i 3)
	do
	(if (<= ?k (length$ ?t)) then (printout t (nth$ ?k ?t) crlf))
	(if (> ?k (length$ ?t)) then (printout t (nth$ (- ?k (length$ ?t)) ?tit) crlf))
	(bind ?k (+ 1 ?k))
	(bind ?i (+ 1 ?i))
	)))

  (defrule crea_llista ""
    (difficultad $?)
		(genero $?)
		(paginas $?)
    (popularidad $?)
    (idioma $?)
    =>
		(bind ?facts (find-all-facts ((?f books)) TRUE))
		(bind $?val1 (fact-slot-value (nth$ 1 ?facts) name ))
		(bind $?val2 (fact-slot-value (nth$ 1 ?facts) puntuaje ))
		(bind ?i 2)
		(while (<= ?i (length$ ?facts))
		do
		(bind $?val1 ?val1 (fact-slot-value (nth$ ?i ?facts) name ))
		(bind $?val2 ?val2 (fact-slot-value (nth$ ?i ?facts) puntuaje ))
		(bind ?i (+ ?i 1))
		)
		(assert (lista (name ?val1) (number ?val2)))
    (assert (filter)))


  ;;;**********************************
  ;;;* ABSTRACTION OF THE INFORMATION *
  ;;;**********************************

		; 5 <= AGE <= 17 -> YOUNG
		; 18 <= AGE <= 30 -> ADULT
		; 31 <= AGE -> OLD ADULT
		(defrule Age_rule ""
			(initial $?)
			=>
			(bind ?response (ask-integer "How old are you? (INTEGER NUMBER) " 5 120))
			(assert (age (age ?response)))
			(if (and (>= ?response 5) (<= ?response 17)) then
				(assert(likes (id Young)))
			)
			(if (and (>= ?response 18) (<= ?response 30)) then
				(assert(likes (id Adult)))
			)
			(if (> ?response 30) then
				(assert(likes (id Old_adult)))
			))


		; 0 <= Nº HOURS <= 2 -> OCASIONALLY
		; 3 <= Nº HOURS <= 7 -> CASUAL
		; 8 <= Nº HOURS <= 14 -> REGULAR
		; 14 <= Nº HOURS -> DEDICATED
		(defrule Number_hours ""
			(initial $?)
			=>
			(bind ?response (ask-integer "How many hours do you read per week? (INTEGER NUMBER) " 0  168))
			(assert (nhours (num ?response)))
			(if (and (>= ?response 0) (<= ?response 2)) then
				(assert(likes (id Ocasionally)))
			)
			(if (and (>= ?response 3) (<= ?response 7)) then
				(assert(likes (id Casual)))
			)
			(if (and (>= ?response 8) (<= ?response 14)) then
				(assert(likes (id Regular)))
			)
			(if (> ?response 14) then
				(assert(likes (id Dedicated)))
			))

		; 0 <= Nº PAGES <= 150 -> SHORT
		; 151 <= Nº PAGES <= 400 -> AVERAGE
		; 400 > Nº PAGES -> LONG
		(defrule Number_pages ""
				(initial $?)
				=>
				(bind ?response (ask-integer "About how many pages have the books you usually read? (INTEGER NUMBER) " 0 5000))
				(assert (npages (num ?response)))
				(if (and (>= ?response 0) (<= ?response 150)) then
					(assert(likes (id Short)))
				)
				(if (and (> ?response 150) (<= ?response 400)) then
					(assert(likes (id Average)))
				)
				(if (> ?response 400) then
					(assert(likes (id Long)))
				))


    (defrule nationality ""
      (initial $?)
      =>
      (bind ?response (ask-question "What Language do you speak ? (Spanish/Norwegian/English/French/Japanese/Greek)" Spanish Norwegian English French Japanese Greek ))
      (switch ?response
        (case Spanish then (assert (likes (id Spain))))
        (case Norwegian then (assert (likes (id Norway))))
        (case English then (assert (likes (id UK))) (assert (likes (id EU))))
        (case French then (assert (likes (id France))))
        (case Japanese then (assert (likes (id Japan))))
        (case Greek then (assert (likes (id Greece)))))

      (bind ?response (ask-question "Do you speak another Language ? (Spanish/Norwegian/English/French/Japanese/Greek/None)" Spanish Norwegian English French Japanese Greek None))
      (while (not (eq ?response None)) do
      (switch ?response
        (case Spanish then (assert (likes (id Spain))))
        (case Norwegian then (assert (likes (id Norway))))
        (case English then (assert (likes (id UK))) (assert (likes (id EU))))
        (case French then (assert (likes (id France))))
        (case Japanese then (assert (likes (id Japan))))
        (case Greek then (assert (likes (id Greece)))))
      (bind ?response (ask-question "Dou you speak another Language ? (Spanish/Norwegian/English/French/Japanese/Greek/None)" Spanish Norwegian English French Japanese Greek None))
      )
      )


		; NARRATIVE ACTION AND SETTING MEDIAVAL -> ADVENTURE, EPIC, MAGIC, SWORD_AND_SORCERY
		; NARRATIVE DEVELOPMENT AND SETTING MEDIAVAL -> FABULA
		; NARRATIVE ACTION AND SETTING URBAN -> SUPERHEROES, CYBERPUNK, SCI-FI
		; NARRATIVE DEVELOPMENT AND SETTING URBAN -> ROMANTIC, DARK, SPOOKY
		(defrule setting_rule ""
				(initial $?)
				=>
				(bind ?response1 (ask-question "What type of setting do you prefer on books, medieval/fantasy or modern/urban? (Medieval/Urban) " Medieval Urban))
				(bind ?response2 (ask-question "What do you prefer in a story, a lot of action or more character and world development? (Action/Development)" Action Development ))
				(assert (setting (id ?response1)))
				(assert (narrative (id ?response2)))
        (if (eq ?response1 Medieval) then
					(if (eq ?response2 Action) then
						(assert(likes (id Adventure)) (likes (id Epic)) (likes (id Magic)) (likes (id Sword_and_Sorcery)))
					else
						(assert(likes (id Fabula))))
					else
					(if (eq ?response2 Action) then
					 (assert(likes (id Superheores)) (likes (id Cyberpunk)) (likes (id Science+Fiction)))
					 else
					 (assert(likes (id Dark)) (likes (id Romantic)) (likes (id Spooky))))
				))

		; 0 <= Nº OF BOOKS <= 5 -> BEGINNER
		; 6 <= Nº OF BOOKS <= 15 -> AMATEUR
    ;?l <- (filtro (titles $?t_ini ?t $?t_end) (number $?num_ini ?num $?num_end))
    ;(test (eq (length$ ?t_ini) (length$ ?num_ini)))
		; 15 <= Nº OF BOOKS -> ADVANCED
		(defrule Number_books ""
			(initial $?)
			=>
			(bind ?response (ask-integer "How many books have you read? (INTEGER NUMBER) " 0 100))
			(assert (nbooks (num ?response)))
			(if (and (>= ?response 0) (<= ?response 5)) then
				(assert(likes (id Beginner)))
			)
			(if (and (>= ?response 6) (<= ?response 15)) then
				(assert(likes (id Amateur)))
			)
			(if (> ?response 15) then
				(assert(likes (id Advanced)))
			))


    (defrule autors ""
			(declare (salience 10))
      (autores_filtro $?)
			?l <- (filtro (titles $?t) (number $?n))
			?o <- (object (Title ?title) (Writer ?w))
      (test (member ?title ?t))
		  (object (name ?w) (Surname ?sur))
      =>
			(bind ?aut (find-fact ((?a autores)) TRUE))
			(bind $?aut (nth$ 1 ?aut))
			;(printout t ?aut crlf)
			(bind $?names (fact-slot-value ?aut name ))
			;(printout t ?names crlf)
			(if (not (member ?sur ?names)) then
			(bind ?names ?names ?sur)
			(modify ?aut (name ?names)))
      )

		(defrule autors_question ""
			(autores_filtro $?)
			(autores (name $?names))
			=>
			(bind ?names ?names "None")
			(bind $?symbols (sym-cat(nth$ 1 ?names)))
			(printout t "Do you like any of these writers?" crlf)
			(loop-for-count (?i 2 (length$ ?names)) do
				(printout t (nth$ ?i ?names) crlf)
				(bind ?symbols ?symbols (sym-cat(nth$ ?i ?names))))
			(bind ?response (ask-question "" ?symbols))
			(bind ?pos (member ?response ?symbols))
			(bind ?symbols (delete$ ?symbols ?pos ?pos))
			(bind ?names (delete$ ?names ?pos ?pos))
			(if (not (eq ?response None)) then
        (bind ?aux (str-cat ?response))
				(bind $?result ?aux)
      	(while (not (eq ?response None)) do
        	(printout t "Do you like any other writers?" crlf)
					(loop-for-count (?i 1 (length$ ?names)) do
						(printout t (nth$ ?i ?names) crlf))
        	(bind ?response (ask-question "" ?symbols))
					(bind ?pos (member ?response ?symbols))
					(bind ?symbols (delete$ ?symbols ?pos ?pos))
					(bind ?names (delete$ ?names ?pos ?pos))
        	(if (not (eq ?response None)) then
						(bind ?aux (str-cat ?response))
						(bind ?result ?result ?aux)))
      	(assert (autoresf (name ?result))))
			)


    (defrule genres ""
      (generos_filtro $?)
			(not (no_more_genero $?))
      ?l <- (filtro (titles $?t) (number $?num))
      =>
      (bind ?instancias (find-all-instances ((?o Libro_Fantasia)) (member (send ?o get-Title) ?t)))
			(bind $?gen None (type (nth$ 1 ?instancias)))
			(loop-for-count (?i 2 (length$ ?instancias)) do
			  (if (not (member(type (nth$ ?i ?instancias)) ?gen)) then
					(bind ?gen ?gen (type (nth$ ?i ?instancias)))
				)
			)
      (printout t "Do you have any genre preferences?" crlf)
			(loop-for-count (?i 1 (length$ ?gen)) do
				(printout t (nth$ ?i ?gen) crlf))
      (bind ?response (ask-question "" ?gen))
			(bind ?pos (member ?response ?gen))
			(bind ?gen (delete$ ?gen ?pos ?pos))
      (if (not (eq ?response None)) then
				(bind $?result ?response)
      	(while (not (eq ?response None)) do
        	(printout t "Do you have any other genre preferences?" crlf)
					(loop-for-count (?i 1 (length$ ?gen)) do
						(printout t (nth$ ?i ?gen) crlf))
        	(bind ?response (ask-question "" ?gen))
					(bind ?pos (member ?response ?gen))
					(bind ?gen (delete$ ?gen ?pos ?pos))
        	(if (not (eq ?response None)) then (bind ?result ?result ?response)))
      	(assert (generof (titles ?result)))
				(assert (no_more_genero)))
    )

   ;ASSERT LIKES EASY LANGUAGE IF:
   ;IF YOUNG AND OCASIONALLY OR CASUAL
	 ;IF ADULT AND OCASIONALLY
	 (defrule Easy_language ""
	 			(likes (id Beginner))
				(or (and(likes (id Young)) (likes (id Ocasionally|Casual)))
					(and(likes (id Adult)) (likes (id Ocasionally))))
				=>
				(assert (likes (id Easy))))



	 ;ASSERT LIKES MEDIUM LANGUAGE IF:
	 ;IF YOUNG AND AMATEUR
	 ;IF YOUNG AND BEGINNER AND REGULAR OR DEDICATED
	 ;IF YOUNG AND ADVANCED AND OCASIONALLY OR CASUAL OR REGULAR
	 ;IF ADULT AND BEGINNER AND CASUAL OR REGULAR OR DEDICATED
	 ;IF ADULT AND AMATEUR OR ADVANCED AND OCASIONALLY
	 ;IF OLD ADULT AND OCASIONALLY OR CASUAL AND BEGINNER OR AMATEUR
	 (defrule Medium_language ""
	 			(or (and (likes (id Young)) (or (likes (id Amateur)) (and (likes (id Beginner)) (likes (id Regular|Dedicated)))
					(and (likes (id Advanced)) (likes (id Ocasionally|Casual|Regular)))))
				(and (likes (id Adult)) (or (and (likes (id Beginner)) (likes (id Casual|Regular|Dedicated)))
					(and (likes (id Amateur|Advanced)) (likes (id Ocasionally)))))
				(and (likes (id Old_adult)) (likes (id Ocasionally|Casual)) (likes (id Beginner|Amateur))))
				=>
				(assert (likes (id Medium))))



   ;ASSERT LIKES HARD LANGUAGE IF:
	 ;IF YOUNG AND DEDICATED AND ADVANCED
	 ;IF ADULT AND CASUAL OR REGULAR OR DEDICATED AND AMATEUR OR ADVANCED
	 ;IF OLD ADULT AND (REGULAR OR DEDICATED) OR (OCASIONALLY OR CASUAL AND ADVANCED)
	 (defrule Hard_Language ""
	 			(or (and (likes (id Young)) (likes (id Dedicated)) (likes (id Advanced)))
				(and (likes (id Adult)) (likes (id Casual|Regular|Dedicated)) (likes (id Amateur|Advanced)))
				(and (likes (id Old_adult)) (or (likes (id Regular|Dedicated)) (and (likes (id Ocasionally|Casual)) (likes (id Advanced))))))
				=>
				(assert(likes (id Hard))))



   ;ASSERT LIKES SHORT EXTENSION IF:
	 ;IF AMATEUR AND OCASIONALLY
	 ;IF BEGINNER AND OCASIONALLY OR CASUAL OR REGULAR
	 (defrule Short_extension ""
	 			(or (and (likes (id Amateur)) (likes (id Ocasionally)))
				(and (likes (id Beginner)) (likes (id Ocasionally|Casual|Regular))))
				=>
				(assert (likes (id Short))))



   ;ASSERT LIKES AVERAGE EXTENSION IF:
	 ;IF BEGINNER AND DEDICATED
	 ;IF AMATEUR AND CASUAL OR REGULAR
	 ;IF ADVANCED AND OCASIONALLY
	 (defrule Average_extension ""
	 			(or (and (likes (id Beginner)) (likes (id Dedicated)))
				(and (likes (id Amateur)) (likes (id Casual|Regular)))
				(and (likes (id Advanced)) (likes (id Ocasionally))))
				=>
				(assert (likes (id Average))))



   ;ASSERT LIKES LONG EXTENSION IF:
	 ;IF AMATEUR AND DEDICATED
	 ;IF ADVANCED AND CASUAL OR REGULAR OR DEDICATED
	 (defrule Long_extension ""
	 			(or (and (likes (id Amateur)) (likes (id Dedicated)))
				(and (likes (id Advanced)) (likes (id Casual|Regular|Dedicated))))
				=>
				(assert (likes (id Long))))

   ;ASSERT LIKES BEST SELLER IF BEGINNER AND CASUAL OR OCASIONALLY
   (defrule best_seller ""
      (and (likes (id Beginner)) (likes (id Casual|Ocasionally)))
      =>
      (assert (likes (id Best_Seller))))

   ;ASSERT LIKES POPULAR IF:
   ;IF BEGINNER AND REGULAR
   ;IF AMATEUR AND CASUAL OR OCASIONALLY
   (defrule popular ""
      (or (and (likes (id Beginner)) (likes (id Regular)))
      (and (likes (id Amateur)) (likes (id Casual|Ocasionally))))
      =>
      (assert (likes (id Popular)))
      )

   ;ASSERT LIKES CRITIC IF:
   ;IF BEGINNER AND DEDICATED
   ;IF AMATEUR AND REGULAR
   ;IF ADVANCED AND CAUAL OR OCASIONALLY
   (defrule critic ""
      (or (and (likes (id Beginner)) (likes (id Dedicated)))
      (and (likes (id Amateur)) (likes (id Regular)))
      (and (likes (id Advanced)) (likes (id Casual|Ocasionally))))
      =>
      (assert (likes (id Critic))))

   ;ASSERT LIKES NORMAL IF:
   ;IF AMATEUR AND DEDICATED
   ;IF ADVANCED AND REGULAR
   (defrule normal ""
      (or (and (likes (id Amateur)) (likes (id Dedicated)))
      (and (likes (id Advanced)) (likes (id Regular))))
      =>
      (assert (likes (id Normal))))

   ;ASSERT LIKES IF ADVANCED AND DEDICATED
   (defrule non_popular ""
      (and (likes (id Advanced)) (likes (id Dedicated)))
      =>
      (assert (likes (id Non-Popular))))
