; Sat Nov 30 20:07:01 CET 2019
; 
;+ (version "3.5")
;+ (build "Build 663")


(defclass %3ACLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
	(is-a USER)
	(role abstract)
	(single-slot Year
		(type INTEGER)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Name
		(type STRING)
;+		(cardinality 1 1)
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
	(single-slot Popularity
		(type SYMBOL)
		(allowed-values Popular Critic Best_Seller Normal Non-Popular)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot LIbro
		(type INSTANCE)
;+		(allowed-classes Libro_Fantasia)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(single-slot proyecto_Class7
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Surname
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot proyecto_Class4
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Language
		(type SYMBOL)
		(allowed-values Easy Medium Hard)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Title
		(type STRING)
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
	(multislot Friend
		(type INSTANCE)
;+		(allowed-classes Persona)
		(create-accessor read-write))
	(single-slot Frequency
		(type SYMBOL)
		(allowed-values Daily Ocasionally Whenever)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot libros
		(type SYMBOL)
;+		(allowed-parents Libro_Fantasia)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot proyecto_Class13
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Pages
		(type INTEGER)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Difficulty
		(type SYMBOL)
		(allowed-values Easy Medium Hard)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot Writer
		(type INSTANCE)
;+		(allowed-classes Autor)
		(cardinality 1 2)
		(create-accessor read-write))
	(single-slot Nationality
		(type SYMBOL)
		(allowed-values Spain Norway EU France UK Japan)
;+		(cardinality 0 1)
		(create-accessor read-write)))

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