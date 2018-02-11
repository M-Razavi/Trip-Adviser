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
	(rules no))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;--------------------------------------------------------------------------------
;;Fuzzy Set Definition								;;
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(deftemplate age
	18 100 years
	(
		(young (z 18 30))		; 18<age<30
		(adult (PI 10 40))		; 30<age<50
		(old (s 50 90))			; 50<age<100
	)
)

(deftemplate personalityIntrovert
	0 100 point
	(
		(low (z 0 35))
		(medium (PI 15 50))
		(high (s 80 100))
	)
)

(deftemplate personalityExtrovert
	0 100 point
	(
		(low (z 0 35))
		(medium (PI 15 50))
		(high (s 80 100))
	)
)

(deftemplate personalityAllocentrics
	0 100 point
	(
		(low (z 0 35))
		(medium (PI 15 50))
		(high (s 80 100))
	)
)

(deftemplate personalityPsychocentrics
	0 100 point
	(
		(low (z 0 35))
		(medium (PI 15 50))
		(high (s 80 100))
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
	(declare (salience 1))
	?g <- (get name)
	=>
	(printout t "Before we go any farther, what is the passenger's name? ")
	(bind ?response (read))
	(bind ?name ?response)
	(assert (name ?response))
	(printout t "How old is " ?response " (18-100)? ")
	;(bind ?response (read))
	(bind ?response (get-integer "Enter a number: " 18 100))
	(assert (crispAge ?response))
	(printout t "What is " ?name "'s gender? (1:man   2:woman)? ")
	;(bind ?response (read))
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 2)
		then(assert(gender woman))
	else(if(eq ?response 1)
		then(assert(gender man)))
	else (assert (gender man)))
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
	(declare (salience 2))
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
	(declare (salience 3))
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
	(declare (salience 4))
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
	(declare (salience 5))
	?f <- (get personality2Measurement)
	(name ?name)
	(namePrefix ?prefix)
	(personality2 ?p1)
	=>
	(printout t crlf "What is " ?prefix " " ?name "'s degree of " ?p1"(0 - 100)? " crlf)
	(bind ?response (get-integer "Enter a number between: " 0 100))
	(assert (crispPersonality2 ?response))
	(assert (get otherInterest1))
	(printout t crlf)
	(retract ?f)
)

;;--------------------------------------------------------------------------------
;;Interest
;;--------------------------------------------------------------------------------
;;Next, we will ask the Interest
(defrule getInterestTreatment
	(declare (salience 6))
	?f <- (get otherInterest1)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is 'Treatment' one of " ?prefix " " ?name "'s purpose of trip(1:yes  2:no)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 1)
		then(assert(interestTreatment yes))
	
	)
	(assert (get interestTreatmentType))
	(printout t crlf)
	(retract ?f)
)

(defrule getInterestTreatmentType
	(declare (salience 7))
	?f <- (get interestTreatmentType)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "What type of 'Treatment' " ?prefix " " ?name "needs(1:Beauty 2:Medicinal)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 2)
		then(assert(interest reatmentType medicinal))
	else(if(eq ?response 1)
		then(assert(interest reatmentType beauty)))
	)
	(assert (get otherInterest2))
	(printout t crlf)
	(retract ?f)
)

