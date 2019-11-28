(defclass Libro_Fantasia
	(is-a USER)
	(role abstract)
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
	(role abstract)
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



(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?allowed-values)
      (printout t ?question)
      (bind ?answer (read))
  ?answer) ?answer)


	(deffunction ask-integer (?question ?init ?ende)
	   (printout t ?question)
	   (bind ?answer (read))
	   (bind ?end FALSE)
	   (while (not ?end) do
	      (if (eq (type ?answer) INTEGER) then
	      (if (and (<= ?answer ?ende) (>= ?answer ?init)) then
	          (bind ?end TRUE)
	        else
	      (printout t "Remember that your age has to be between ")
				(printout t ?init)
				(printout t " and ")
				(printout t ?ende)
				(printout t ", you fool " crlf)
	      (bind ?answer (read)))
	      else
	      (printout t "Remember that your age has to be a number, or didn't you go to the school ? " crlf)
	      (bind ?answer (read))
	      ))
	      ?answer)


(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE
       else FALSE))

(deffunction filter_books ()
  (bind ?f (find-all-facts ((?l likes)) TRUE))
  (bind $?r (fact-slot-value (nth$ 1 ?f) id))
	(bind ?aux "")
  (bind ?j 2)
	(bind $?ret ?aux)
  (while (<= ?j (length$ ?f))
  do
    (bind ?v (fact-slot-value (nth$ ?j ?f) id))
    (bind $?r ?r ?v)
    (bind ?j (+ ?j 1))
  )
	(printout t ?ret)
  (do-for-all-instances ((?inst Libro_Fantasia)) TRUE
     (if (eq ?aux ?ret) then
				(bind $?ret ?inst)
				else
				(bind $?ret ?ret ?inst)))
    ?ret)


;;;****************************
;;;* RULES TO DETERMINE IF A BOOK WILL BE OR NOT PART OF THE SOLUTION *
;;;****************************


;

(deffunction )


;;;****************************
;;;* INITIAL AND END OF THE PROGRAM *
;;;****************************

(defrule system-banner ""
	(not (initial ?))
  =>
  (load-instances "Instances.pins")
  (assert (initial))
  (bind $?i 1)
  (printout t crlf crlf)
  (printout t "System of Recomendation for Books")
  (printout t crlf crlf))


  (defrule exit ""
    (gustados $?)
		(populares $?)
    =>
		(printout t crlf)
    (bind ?result (filter_books))
		(bind ?i 1)
		(while (<= ?i 3);(length$ ?result))
		do
		(bind ?inst (nth$ ?i ?result))
		(printout t (send ?inst get-Title) crlf)
		(bind ?i (+ ?i 1))
		)
    (printout t crlf crlf)
    (printout t "Sortida")
    (printout t crlf crlf))


  ;;;****************************
  ;;;* QUESTIONS *
  ;;;****************************

  (defrule start_question ""
    (initial $?)
    =>
    (bind ?response (ask-question "What difficulty do you prefer? (Easy/Medium/Hard/None) " Easy Medium Hard None))
		(if (eq ?response None) then
			(assert(likes (id None_Language)))
			else
			(assert(likes (id ?response)))
		)
    (while (and (not (eq ?response None)) (yes-or-no-p "Do you have any other preferences? (yes/no)"))
    do
    (bind ?response (ask-question "What difficulty do you prefer? (Easy/Medium/Hard)" Easy Medium Hard ))
		(if (eq ?response None) then
			(assert(likes (id None_popular)))
			else
			(assert(likes (id ?response)))
		)
    )
    (assert (gustados)))



		(defrule Popularity ""
	   (initial $?)
	    =>
	    (bind ?response (ask-question "Do you want a famous book ? (Popular/Critic/Best_Seller/Normal/Non-Popular/None) " Popular Critic Best_Seller Normal Non-Popular None))
			(if (eq ?response None) then
				(assert(likes (id None_popular)))
				else
				(assert(likes (id ?response)))
			)

	    (while (and (not (eq ?response None)) (yes-or-no-p "Do you have any other preferences? (yes/no)"))
	    do
	    (bind ?response (ask-question "Do you want a famous book ? (Popular/Critic/Best_Seller/Normal/Non-Popular) " Popular Critic Best_Seller Normal Non-Popular))
			(if (eq ?response None) then
				(assert(likes (id None_popular)))
				else
				(assert(likes (id ?response)))
			)
	    )
	    (assert (populares)))


		(defrule Age ""
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

		(defrule nbooks ""
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

			(defrule nhours ""
				(initial $?)
				=>
				(bind ?response (ask-integer "How many hours do you read per week? (INTEGER NUMBER) "))
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

				(defrule npages ""
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

				(defrule setting ""
					(initial $?)
					=>
					(bind ?response (ask-question "What type of setting do you prefer on books, medieval/fantasy or modern/urban? (Medieval/Urban) " Medieval Urban))
					(assert (setting (id ?response)))
					(if (eq ?response Medieval) then
						(assert(likes (id Adventure)) (likes (id Epic)) (likes (id Magic)) (likes (id Fabula)) (likes (id Sword_and_Sorcery)))
						else
						(assert(likes (id Cyberpunk)) (likes (id Dark)) (likes (id Romantic)) (likes (id Spooky)) (likes (id Superheores)))
					))

				(defrule narrative ""
					(initial $?)
					=>
					(bind ?response (ask-question "What do you prefer in a story, a lot of action or more character and world development?  (Action/Development) " ))
					(assert (narrative (id ?response)))
					(if (eq ?response Action) then
						(assert(likes (id Adventure)) (likes (id Epic)) (likes (id Magic)) (likes (id Sword_and_Sorcery)) (likes (id Superheores)) (likes (id Cyberpunk)))
						else
						(assert(likes (id Dark)) (likes (id Romantic)) (likes (id Spooky)) (likes (id Fabula)))
					))

					(defrule para_jovenes ""
					((es_joven))
					((es_begginer)))
