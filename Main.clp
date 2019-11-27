

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
		(allowed-values Espa%C3%B1a Noruega EU France UK Jap%C3%B3n)
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


(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE
       else FALSE))

(deffunction filter_books ()
  ;(bind $?instancias  (find-all-instances ((?inst Libro_Fantasia)) ))
  (bind ?f (find-all-facts ((?l likes)) TRUE))
  ;(printout t ?f crlf)
  (bind $?r (fact-slot-value (nth$ 1 ?f) id))
	(bind ?aux "")
  (bind ?j 2)
	(bind $?ret ?aux)
  (while (<= ?j (length$ ?f))
  do
    (bind ?v (fact-slot-value (nth$ ?j ?f) id))
    (bind $?r ?r ?v)
    ;(printout t ?r crlf)
    (bind ?j (+ ?j 1))
  )
  (do-for-all-instances ((?inst Libro_Fantasia)) (or (member ?inst:Language ?r)
    (member ?inst:Popularity ?r) (member (class ?inst) ?r) ;(member ?inst:Year ?r)
    ) (if (eq ?aux ?ret) then
				(bind $?ret ?inst)
				else
				(bind $?ret ?ret ?inst)))
    ?ret)

;;;****************************
;;;* INITIAL RULES *
;;;****************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (load-instances "Instances.pins")
  (assert (initial))
  (bind $?i 1)
  (printout t crlf crlf)
  (printout t "System of Recomendation for Books")
  (printout t crlf crlf))



  ;(deffunction function ($?rules)
  ;() )


  (defrule exit ""
    (final $?)
    ;(eq length$ (likes (id ?k)) 3)
    =>
    (printout t (filter_books))
    (printout t crlf crlf)
    (printout t "Sortida")
    (printout t crlf crlf))


  ;;;****************************
  ;;;* QUESTIONS *
  ;;;****************************

  (defrule start_question ""
    (initial $?)
    =>
    (assert (initial))
    (bind ?response (ask-question "What difficulty do you prefer? (Easy/Medium/Hard) " Easy Medium Hard))
    (assert(likes (id ?response)))
    (while (yes-or-no-p "Do you have any other preferences? (yes/no)")
    do
    (bind ?response (ask-question "What difficulty do you prefer? (Easy/Medium/Hard)" Easy Medium Hard))
    (assert(likes (id ?response)))
    )
    (assert (final)))
