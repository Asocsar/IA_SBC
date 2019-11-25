;;;****************************
;;;* STARTUP *
;;;****************************

;(load "clases.clp")
;(load-instances "Instances.pins")

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

;;;****************************
;;;* INITIAL RULES *
;;;****************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "System of Recomendation for Books")
  (printout t crlf crlf))


  (defrule exit ""
    (declare (salience 20))
    =>
    (printout t crlf crlf)
    (printout t "Sortida")
    (printout t crlf crlf))


  ;;;****************************
  ;;;* QUESTIONS *
  ;;;****************************

(defrule start_question ""
  (not (initial $?))
  =>
  (assert (initial))
  (bind ?response (ask-question "What difficulty do you prefer (Easy/Medium/Hard)? " Easy Medium Hard))
  (bind $?instancias  (find-all-instances ((?inst Libro_Fantasia)) (eq ?inst:Language ?response)))
  (bind ?i 1)
  (while (<= ?i (length$ ?instancias))
  do
  (bind ?elemental (nth$ ?i ?instancias))
  (printout t (send ?elemental get-Title) crlf)
  (bind ?i (+ ?i 1)))
  )

;(assert (duck-sound quack))
