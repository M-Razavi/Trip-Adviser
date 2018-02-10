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
	0 5 point
	(
	(low (z 0 3))
	(medium (PI 1 3))
	(high (s 4 5))
	)
)

(deftemplate personalityExtravert
	0 5 point
	(
	(low (z 0 3))
	(medium (PI 1 3))
	(high (s 4 5))
	)
)

(deftemplate personalityAllocentrics
	0 5 point
	(
	(low (z 0 3))
	(medium (PI 1 3))
	(high (s 4 5))
	)
)

(deftemplate personalityPsychocentrics
	0 5 point
	(
	(low (z 0 3))
	(medium (PI 1 3))
	(high (s 4 5))
	)
)

(deftemplate personalityIntrovert
	0 5 point
	(
	(low (z 0 3))
	(medium (PI 1 3))
	(high (s 4 5))
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
	(printout t "This program is designed to help tourist or travel agency to decide chosing some destination that match their point of view." crlf)
	(printout t "You will be prompted for the passenger's name, age, and gender." crlf crlf)
	(assert (get name))
	(retract ?i))
;;--------------------------------------------------------------------------------
;;Name and Age and Gender
;;--------------------------------------------------------------------------------
;;Move into getting inputs
(defrule getNameAgeGender
	?g <- (get name)
	=>
	(printout t "Before we go any farther, what is the passenger's name? ")
	(bind ?response (read))
	(bind ?name ?response)
	(assert (name ?response))
	(printout t "How old is " ?response " (18-100)? ")
	(bind ?response (read))
	(assert (crispAge ?response))
	(printout t "What is " ?name "'s gender? (1:man   2:woman)")
	(bind ?response (read))
	(if(eq ?response 2)
		then(assert(gender woman))
	else(if(eq ?response 1)
		then(assert(gender man)))
	else (assert (gender undefined)))
	(assert (get personality))
	(retract ?g))
	
	