;;Authors: 	Mahdi Razavi
;;Instructor:	Dr. Ghasem Aghaei
;;Date:		02.10.2017
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;--------------------------------------------------------------------------------
;;TripAdviser.clp		
;;														
;;This program is designed for use by somebody plan for a trip. The user will   
;;be prompted for and input some information about her/him self such as age,    
;;personality, Intereset adn so on. At the end, Expert system suggest some place 
;;for her/him trips.							
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;--------------------------------------------------------------------------------
;;Methods
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defmethod get-integer ((?query STRING))
   (bind ?value FALSE)
   (while (not (integerp ?value))
      (printout t ?query " ")
      (bind ?value (read)))
	?value)

(defmethod get-integer ((?query STRING) (?lower INTEGER) (?upper INTEGER))
   (bind ?value FALSE)
   (while (or (not (integerp ?value)) (< ?value ?lower) (> ?value ?upper))
      (printout t ?query " (" ?lower " - " ?upper ") ")
      (bind ?value (read)))
	?value)

(defmethod get-yes-no ((?query STRING))
   (bind ?value FALSE)
   (while (or (not (stringp ?value)) (neq ?value "yes") (neq ?value "y") (neq ?value "no") (neq ?value "n"))
      (printout t ?query " ")
      (bind ?value (read)))
	?value)
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;--------------------------------------------------------------------------------
;;Facts
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;Only need to assume that input rules have not been displayed.
(deffacts timeAssumptions
	(rules no)
	())

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;--------------------------------------------------------------------------------
;;Fuzzy Set Definition								;;
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(deftemplate ageGroup
	18 100 years
	(
		(young (z 18 30))		; 18<age<30
		(adult (PI 10 40))		; 30<age<50
		(old (s 50 90))			; 50<age<100
	)
)

(deftemplate personalityIntrovertExtrovert
	0 100 point
	(
		(veryIntrovert(z 0 20))
		(Introvert (PI 10 35))
		(Normal (PI 10 50))
		(Extrovert (PI 10 65))
		(VeryExtrovert (s 80 100))
	)
)

(deftemplate personalityAllocentricsPsychocentrics
	0 100 point
	(
		(veryAllocentrics(z 0 20))
		(Allocentrics (PI 10 35))
		(Normal (PI 10 50))
		(Psychocentrics (PI 10 65))
		(VeryPsychocentrics (s 80 100))
	)
)


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;--------------------------------------------------------------------------------
;;Interactive part of Program						
;;	The user will be prompted for some personal information. The input will 
;;consist of yes/no questions (for which there is minimal input validation) and 
;;prompts for range of number. 
;;										
;;Deffrules that need the rules to be specified:				
;;	+getActualFeedTime							;;
;;	+getNapTimeYes								;;
;;	+getDiaperTimeYes							;;
;;	+getCurrTime (note that, by use of salience, there is no LHS condition)	;;
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;Begin with an introduction to the program
(defrule introduction
	?i <- (initial-fact)
	=>
	(printout t "This program is designed to help tourist or travel agency to decide choosing some destination that match their point of view." crlf)
	(printout t "You will be prompted for the passenger's name, age, and gender." crlf crlf)
	(assert (get name))
	(retract ?i))
;;--------------------------------------------------------------------------------
;;Name and Age and Gender
;;--------------------------------------------------------------------------------
;;Move into getting inputs
(defrule getNameAgeGender
	(declare (salience 100))
	?g <- (get name)
	=>
	(printout t "Before we go any farther, what is the passenger's name? ")
	(bind ?response (read))
	(bind ?name ?response)
	(assert (name ?response))
	(printout t crlf)
	
	(printout t "How old is " ?response " (18-100)? ")
	;(bind ?response (read))
	(bind ?response (get-integer "Enter a number: " 18 100))
	(assert (crispAge ?response))
	(printout t crlf)
		
	(printout t "What is " ?name "'s gender? (1:man   2:woman)? ")
	;(bind ?response (read))
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 2)
		then(assert(gender woman))
	else(if(eq ?response 1)
		then(assert(gender man)))
	else (assert (gender man)))
	(printout t crlf)
	
	(printout t "What is " ?name "'s style? (1:Traditionalism 2:Modernism)? ")
	;(bind ?response (read))
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 2)
		then(assert(style modernism))
	else(if(eq ?response 1)
		then(assert(style traditionalism)))
	)
	(assert (get personality1))
	(retract ?g)
)

;;--------------------------------------------------------------------------------
;;Name prefix
;;--------------------------------------------------------------------------------
;;Determine prefix for name based on gender
(defrule createNamePrefix
	(name ?name)
	(gender ?genderType)
	=>
	(if (eq ?genderType man)
		then(assert (namePrefix Mr))
	else (assert (namePrefix Mrs))
	)
)
	
;;--------------------------------------------------------------------------------
;;Personality
;;--------------------------------------------------------------------------------
;;Next, we will ask the personality between introvert and extrovert
(defrule getPersonalityIntrovertExtrovert
	(declare (salience 99))
	?f <- (get personality1)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is " ?prefix " " ?name " introvert or extrovert(1:Introvert  2:extrovert)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 2)
		then(assert(personality1 extrovert))
	else(if(eq ?response 1)
		then(assert(personality1 introvert)))
	)
	(assert (get personality1Measurement))
	(printout t crlf)
	(retract ?f)
)

;;Introvert or Extrovert measurement
(defrule getPersonalityIntrovertExtrovertMeasurement
	(declare (salience 98))
	?f <- (get personality1Measurement)
	(name ?name)
	(namePrefix ?prefix)
	(personality1 ?p1)
	=>
	(printout t crlf "What is " ?prefix " " ?name "'s degree of " ?p1"(0 - 100)? " crlf)
	(bind ?response (get-integer "Enter a number between: " 0 100))
	(assert (crispPersonality1 ?response))
	(assert (get personality2))
	(printout t crlf)
	(retract ?f)
)

;;Next, we will ask the personality between Allocentric and Psychocentric
(defrule getPersonalityAllocentricPsychocentric
	(declare (salience 97))
	?f <- (get personality2)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is " ?prefix " " ?name " Allocentric or Psychocentric(1:Allocentric  2:Psychocentric)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 2)
		then(assert(personality2 psychocentric))
	else(if(eq ?response 1)
		then(assert(personality2 allocentric)))
	)
	(assert (get personality2Measurement))
	(printout t crlf)
	(retract ?f)
)

;;Allocentric or Psychocentric Measurement
(defrule getPersonalityAllocentricPsychocentricMeasurement
	(declare (salience 96))
	?f <- (get personality2Measurement)
	(name ?name)
	(namePrefix ?prefix)
	(personality2 ?p1)
	=>
	(printout t crlf "What is " ?prefix " " ?name "'s degree of " ?p1"(0 - 100)? " crlf)
	(bind ?response (get-integer "Enter a number between: " 0 100))
	(assert (crispPersonality2 ?response))
	(assert (get otherPurpose1))
	(printout t crlf)
	(retract ?f)
)

;;--------------------------------------------------------------------------------
;;Purpose Treatment
;;--------------------------------------------------------------------------------
;;Next, we will ask about Treatment
(defrule getPurposeTreatment
	(declare (salience 95))
	?f <- (get otherPurpose1)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is 'Treatment' one of " ?prefix " " ?name "'s purpose of trip(1:yes  2:no)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 1)
		then
			(assert(purposeTreatment yes))
			(assert(get purposeTreatmentType))
	)
	(assert(get otherPurpose2))
	(printout t crlf)
	(retract ?f)
)

(defrule getPurposeTreatmentType
	(declare (salience 94))
	?f <- (get purposeTreatmentType)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "What type of 'Treatment' " ?prefix " " ?name " needs(1:Beauty 2:Medicinal)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 2)
		then(assert(purpose reatmentType medicinal))
	else(if(eq ?response 1)
		then(assert(purpose reatmentType beauty)))
	)
	(printout t crlf)
	(retract ?f)
)


;;--------------------------------------------------------------------------------
;;Purpose Leisure
;;--------------------------------------------------------------------------------
;;Next, we will ask about Leisure
(defrule getPurposeLeisure
	(declare (salience 93))
	?f <- (get otherPurpose2)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is 'Leisure' one of " ?prefix " " ?name "'s purpose of trip(1:yes  2:no)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 1)
		then
			(assert(purposeLeisure yes))
			(assert(get purposeLeisureType))
	)
	(assert (get otherPurpose3))
	(printout t crlf)
	(retract ?f)
)

(defrule getPurposeLeisureType
	(declare (salience 92))
	?f <- (get purposeLeisureType)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "What type of 'Leisure' " ?prefix " " ?name " likes(1:Relaxation 2:Sport 3:Cinema 4:Theater 5:Music)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 5))
	(if(eq ?response 1)
		then(assert(purpose leisuretType relaxation))
	else(if(eq ?response 2)
		then(assert(purpose leisuretType sport)))
	else(if(eq ?response 3)
		then(assert(purpose leisuretType cinema)))
	else(if(eq ?response 4)
		then(assert(purpose leisuretType theater)))
	else(if(eq ?response 5)
		then(assert(purpose leisuretType music)))
	)
	(printout t crlf)
	(retract ?f)
)


;;--------------------------------------------------------------------------------
;;Purpose OutdoorActivity
;;--------------------------------------------------------------------------------
;;Next, we will ask about OutdoorActivity
(defrule getPurposeOutdoorActivity
	(declare (salience 91))
	?f <- (get otherPurpose3)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is 'OutdoorActivity' one of " ?prefix " " ?name "'s purpose of trip(1:yes  2:no)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 1)
		then
			(assert(purposeOutdoorActivity yes))
			(assert(get purposeOutdoorActivityType))
	)
	(assert (get otherPurpose4))
	(printout t crlf)
	(retract ?f)
)

(defrule getPurposeOutdoorActivityType
	(declare (salience 90))
	?f <- (get purposeOutdoorActivityType)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "What type of 'Outdoor Activity' " ?prefix " " ?name " likes(1:Urban 2:Nature 3:Beach 4:Desert)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 5))
	(if(eq ?response 1)
		then(assert(purpose OutdoorActivityType urban))
	else(if(eq ?response 2)
		then(assert(purpose OutdoorActivityType nature)))
	else(if(eq ?response 3)
		then(assert(purpose OutdoorActivityType beach)))
	else(if(eq ?response 4)
		then(assert(purpose OutdoorActivityType desert)))
	)
	(printout t crlf)
	(retract ?f)
)

;;--------------------------------------------------------------------------------
;;Purpose Museum
;;--------------------------------------------------------------------------------
;;Next, we will ask about Museum
(defrule getPurposeMuseum
	(declare (salience 89))
	?f <- (get otherPurpose4)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is 'Museum' one of " ?prefix " " ?name "'s purpose of trip(1:yes  2:no)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 1)
		then
			(assert(purposeMuseum yes))
			(assert(get purposeMuseumType))
	)
	(assert (get otherPurpose5))
	(printout t crlf)
	(retract ?f)
)

(defrule getPurposeMuseumType
	(declare (salience 88))
	?f <- (get purposeMuseumType)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "What type of 'Museum' " ?prefix " " ?name " likes(1:Art 2:Civilization 3:Science 4:Nature 5:Antient)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 5))
	(if(eq ?response 1)
		then(assert(purpose MuseumType art))
	else(if(eq ?response 2)
		then(assert(purpose MuseumType civilization)))
	else(if(eq ?response 3)
		then(assert(purpose MuseumType science)))
	else(if(eq ?response 4)
		then(assert(purpose MuseumType nature)))
	else(if(eq ?response 5)
		then(assert(purpose MuseumType antient)))
	)
	(printout t crlf)
	(retract ?f)
)


;;--------------------------------------------------------------------------------
;;Purpose Gastronomy (Food)
;;--------------------------------------------------------------------------------
;;Next, we will ask about Culinary
(defrule getPurposeCulinary
	(declare (salience 87))
	?f <- (get otherPurpose5)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is 'Culinary' one of " ?prefix " " ?name "'s purpose of trip(1:yes  2:no)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 1)
		then
			(assert(purposeCulinary yes))
			(assert (get purposeCulinaryType))
	)
	(assert (get otherPurpose5))
	(printout t crlf)
	(retract ?f)
)

(defrule getPurposeCulinaryType
	(declare (salience 86))
	?f <- (get purposeCulinaryType)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "What type of 'Culinary' " ?prefix " " ?name " likes(1:'Cooking with locals' 2:'Cooking workshops' 3:'Eating at locals’ homes' 4:'Eating at local restaurants' 5:'Eating street food')? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 5))
	(if(eq ?response 1)
		then(assert(purpose CulinaryType cookingWithLocals))
	else(if(eq ?response 2)
		then(assert(purpose CulinaryType cookingWorkshops)))
	else(if(eq ?response 3)
		then(assert(purpose CulinaryType eatingAtLocalsHomes)))
	else(if(eq ?response 4)
		then(assert(purpose CulinaryType eatingAtLocalRestaurants)))
	else(if(eq ?response 5)
		then(assert(purpose CulinaryType eatingStreetFood)))
	)
	(printout t crlf)
	(retract ?f)
)


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
;;--------------------------------------------------------------------------------
;;Realize inputs								;;
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;Convert crisp amount of personality to border
(defrule realize-Personality-extrovert
	(declare (salience 20))
	?f <- (personality1 extrovert)
	(personality1 extrovert)
	(crispPersonality1 ?p1)
	=>
	(bind ?p1Real (+(/ ?p1 2) 50))
	(assert (crispPersonality1 ?p1Real))
	(printout t "crispPersonality1=" ?p1Real crlf)
	(retract ?f)
)

(defrule realize-Personality-introvert
	(declare (salience 20))
	?f <- (personality1 introvert)
	(personality1 introvert)
	(crispPersonality1 ?p1)
	=>
	(bind ?p1Real (- 50 (/ ?p1 2)))
	(assert (crispPersonality1 ?p1Real))
	(printout t "crispPersonality1=" ?p1Real crlf)
	(retract ?f)
)

(defrule realize-Personality-allocentric
	(declare (salience 20))
	?f <- (personality2 allocentric)
	(personality2 allocentric)
	(crispPersonality2 ?p2)
	=>
	(bind ?p2Real (- 50 (/ ?p2 2)))
	(assert (crispPersonality2 ?p2Real))
	(printout t "crispPersonality2=" ?p2Real crlf)
	(retract ?f)
)

(defrule realize-Personality-psychocentric
	(declare (salience 20))
	?f <- (personality2 psychocentric)
	(personality2 psychocentric)
	(crispPersonality2 ?p2)
	=>
	(bind ?p2Real (+(/ ?p2 2) 50))
	(assert (crispPersonality2 ?p2Real))
	(printout t "crispPersonality2=" ?p2Real crlf)
	(retract ?f)
)

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
;;--------------------------------------------------------------------------------
;;Fuzzify inputs								;;
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;Fuzzify the age and personality
(defrule fuzzify-Age-Personality
	(declare (salience 15))
	(crispAge ?a)
	(crispPersonality1 ?p1)
	(crispPersonality2 ?p2)
	=>
	(assert (ageGroup (?a 0) (?a 1) (?a 0)))
	(assert (personalityIntrovertExtrovert (?p1 0) (?p1 1) (?p1 0)))
	(assert (personalityAllocentricsPsychocentrics (?p2 0) (?p2 1) (?p2 0)))
)


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
;;--------------------------------------------------------------------------------
;;Output Results							;;
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;

(defrule output-suggest-history
	(declare (salience 10))
	(ageGroup young)
	(personalityIntrovertExtrovert extrovert)
	(personalityAllocentricsPsychocentrics Psychocentrics)
	=>
	(assert (suggest canyon))
	(printout t "you can go canyoning=" crlf)
)



