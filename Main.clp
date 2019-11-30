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
	(multislot Writer
		(type INSTANCE)
;+		(allowed-classes Autor)
		(cardinality 1 2)
		(create-accessor read-write))
	(single-slot Pages
		(type INTEGER)
;+		(cardinality 1 1)
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
		(allowed-values Spain Norway EU France UK Japan)
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
;;;* STARTUP *
;;;****************************

;(load "clases.clp")
;(load-instances "Instances.pins")

;;;****************************
;;;* ESTRUCTURAS *
;;;****************************

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
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE
       else FALSE))

(deffunction class_pages (?pages)
	(bind ?r "")
	(if (and (>= ?pages 0) (<= ?pages 150)) then
		(bind ?r Short)
	)
	(if (and (> ?pages 150) (<= ?pages 400)) then
		(bind ?r Normal)
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
	 (modify ?st (name ?title) (puntuaje (+ (fact-slot-value ?st puntuaje) 1)))
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

(defrule ordena-llista
	?indice <- (lista (name $?t_ini ?t1 ?t2 $?t_end) (number $?n_ini ?n1 ?n2 $?n_end))
	(test (eq (length$ ?t_ini) (length$ ?n_ini)))
	(test (< ?n1 ?n2))
	=>
	(assert (lista (name ?t_ini ?t2 ?t1 ?t_end) (number ?n_ini ?n2 ?n1 ?n_end)))
	(retract ?indice))

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
  (load-instances "Instances.pins")
  (assert (initial))
  (bind $?i 1)
  (printout t crlf crlf)
  (printout t "System of Recomendation for Books")
  (printout t crlf crlf))

(defrule end ""
	(declare (salience -10))
	?l <- (lista (name $?t) (number $?n))
	=>
	(bind ?i 1)
	(printout t (length$ ?t) crlf)
	(printout t "We recomend you the following list of books" crlf)
	(while (<= ?i 3)
	do
	(printout t (nth$ ?i ?t) crlf)
	(bind ?i (+ ?i 1))
	)
	)

  (defrule crea_llista ""
    (difficultad $?)
		(genero $?)
		(paginas $?)
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
		(assert (lista (name ?val1) (number ?val2))))


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
			(if (and (>= ?response 3) (<= ?response 17)) then
				(assert(likes (id Casual)))
			)
			(if (and (>= ?response 8) (<= ?response 14)) then
				(assert(likes (id Regular)))
			)
			(if (> ?response 14) then
				(assert(likes (id Dedicated)))
			))

		; 0 <= Nº PAGES <= 150 -> SHORT
		; 151 <= Nº PAGES <= 400 -> NORMAL
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
					(assert(likes (id Normal)))
				)
				(if (> ?response 400) then
					(assert(likes (id Long)))
				))


		; NARRATIVE ACTION AND SETTING MEDIAVAL -> ADVENTURE, EPIC, MAGIC, SWORD_AND_SORCERY
		; NARRATIVE DEVELOPMENT AND SETTING MEDIAVAL -> FABULA
		; NARRATIVE ACTION AND SETTING URBAN -> SUPERHEROES, CYBERPUNK, SCI-FI
		; NARRATIVE DEVELOPMENT AND SETTING URBAN -> ROMANTIC, DARK, SPOOKY
		(defrule setting_rule ""
				(initial $?)
				=>
				(bind ?response1 (ask-question "What type of setting do you prefer on books, medieval/fantasy or modern/urban? (Medieval/Urban) " Medieval Urban))
				(bind ?response2 (ask-question "What do you prefer in a story, a lot of action or more character and world development?  (Action/Development) " Action Development ))
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
		; 15 <= Nº OF BOOKS -> ADVANCED
		(defrule Number_books ""
			(initial $?)
			=>
			(bind ?response (ask-integer "How many books have you read? (INTEGER NUMBER) " 0 40))
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

		; IF YOUNG OR ADULT AND BEGGINER -> EASY
		(defrule for_young_B ""
				(or (likes (id Young)) (likes (id Adult)) )
				(likes (id Beginner))
				=>
				(assert (likes (id Easy))))

		; IF YOUNG AND AMATEUR OR ADVANCES -> MEDIUM
		(defrule for_young_else ""
				(likes (id Young))
				(or (likes (id Amateur)) (likes (id Advanced)))
				=>
				(assert (likes (id Medium))))

		; IF ADULT OR OLD_ADULT AND AMATEUR -> MEDIUM
		(defrule for_adults_Am ""
				(or (likes (id Adult)) (likes (id Old_adult)))
				(likes (id Amateur))
				=>
				(assert (likes (id Medium))))

		; IF ADULT OR OLD_ADULT AND ADVANCED -> HARD
		(defrule for_adults_Ad ""
				(or (likes (id Adult)) (likes (id Old_adult)))
				(likes (id Advanced))
				=>
				(assert (likes (id Hard))))

		; IF OLD_ADULT AND BEGINNER -> MEDIUM
		(defrule for_old_adult_B ""
				(likes (id Old_adult))
				(likes (id Beginner))
				=>
				(assert (likes (id Medium))))
