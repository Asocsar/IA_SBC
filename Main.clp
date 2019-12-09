
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
  (load-instances "Instancies.pins")
  (assert (initial))
  (bind $?i 1)
  (printout t crlf crlf)
  (printout t "System of Recomendation for Books")
  (printout t crlf crlf))

(defrule end ""
	(declare (salience -10))
  (final $?)
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
    (assert (final)))


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

      (bind ?response (ask-question "Dou you speak another Language ? (Spanish/Norwegian/English/French/Japanese/Greek/None)" Spanish Norwegian English French Japanese Greek None))
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
