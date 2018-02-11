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
(deffacts initials
	(show fuzzyValue)
	(checkType yes)
	(show endMessage)
)
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;Only need to assume that input rules have not been displayed.

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;--------------------------------------------------------------------------------
;;Fuzzy Set Definition								;;
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(deftemplate AgeGroup
	18 100 years
	(
		(Young (z 18 31))		; 18<age<30
		(Adult (pi 11 40))		; 30<age<50
		(Old (s 50 90))			; 50<age<100
	)
)

(deftemplate PersonalityIntrovertExtrovert
	0 100 point
	(
		(VeryIntrovert(z 0 26))
		(Introvert (pi 11 35))
		(Normal (pi 15 50))
		(Extrovert (pi 11 65))
		(VeryExtrovert (s 75 100))
	)
)

(deftemplate PersonalityAllocentricPsychocentric
	0 100 point
	(
		(VeryPsychocentric(z 0 26))
		(Psychocentric (pi 11 35))
		(Normal (pi 15 50))
		(Allocentric (pi 11 65))
		(VeryAllocentric (s 75 100))
	)
)


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;--------------------------------------------------------------------------------
;;Interactive part of Program						
;;	The user will be prompted for some personal information. The input will 
;;consist of yes/no questions (for which there is minimal input validation) and 
;;prompts for range of number. 
;;										
;;(note that, by use of salience, there is no LHS condition)	;;
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
;;1- Purpose Treatment
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
;;2- Purpose Leisure
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
	(printout t crlf "What type of 'Leisure' " ?prefix " " ?name " likes(1:Relaxation 2:Sport 3:Cinema 4:Theatre 5:Music)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 5))
	(if(eq ?response 1)
		then(assert(purpose leisuretType relaxation))
	else(if(eq ?response 2)
		then(assert(purpose leisuretType sport)))
	else(if(eq ?response 3)
		then(assert(purpose leisuretType cinema)))
	else(if(eq ?response 4)
		then(assert(purpose leisuretType theatre)))
	else(if(eq ?response 5)
		then(assert(purpose leisuretType music)))
	)
	(printout t crlf)
	(retract ?f)
)


;;--------------------------------------------------------------------------------
;;3- Purpose OutdoorActivity
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
	(printout t crlf "What type of 'Outdoor Activity' " ?prefix " " ?name " likes(1:Urban 2:Nature 3:Beach 4:Desert 5:River 6:History)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 6))
	(if(eq ?response 1)
		then(assert(purpose OutdoorActivityType urban))
	else(if(eq ?response 2)
		then(assert(purpose OutdoorActivityType nature)))
	else(if(eq ?response 3)
		then(assert(purpose OutdoorActivityType beach)))
	else(if(eq ?response 4)
		then(assert(purpose OutdoorActivityType desert)))
	else(if(eq ?response 5)
		then(assert(purpose OutdoorActivityType river)))
	else(if(eq ?response 6)
		then(assert(purpose OutdoorActivityType history)))
	)
	(printout t crlf)
	(retract ?f)
)

;;--------------------------------------------------------------------------------
;;4- Purpose Museum
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
	(printout t crlf "What type of 'Museum' " ?prefix " " ?name " likes(1:Art 2:Civilization 3:Science 4:Nature 5:Ancient)? " crlf)
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
		then(assert(purpose MuseumType ancient)))
	)
	(printout t crlf)
	(retract ?f)
)


;;--------------------------------------------------------------------------------
;;5- Purpose Gastronomy (Food)
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
	(assert (get otherPurpose6))
	(printout t crlf)
	(retract ?f)
)

(defrule getPurposeCulinaryType
	(declare (salience 86))
	?f <- (get purposeCulinaryType)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "What type of 'Culinary' " ?prefix " " ?name " likes(1:'Cooking with locals' 2:'Cooking workshops' 3:'Eating at local's homes' 4:'Eating at local restaurants' 5:'Eating street food')? " crlf)
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

;;--------------------------------------------------------------------------------
;;6- Purpose Religious 
;;--------------------------------------------------------------------------------
;;Next, we will ask about Religious
(defrule getPurposeReligious
	(declare (salience 85))
	?f <- (get otherPurpose6)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "Is 'Religious' one of " ?prefix " " ?name "'s purpose of trip(1:yes  2:no)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 2))
	(if(eq ?response 1)
		then
			(assert(purposeReligious yes))
			(assert (get purposeReligiousType))
	)
	(assert (get end))
	(printout t crlf)
	(retract ?f)
)

(defrule getPurposeReligiousType
	(declare (salience 84))
	?f <- (get purposeReligiousType)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "What type of 'Religious' " ?prefix " " ?name " likes(1:Islam 2:Christian 3:Zoroastrianism)? " crlf)
	(bind ?response (get-integer "Enter a number: " 1 3))
	(if(eq ?response 1)
		then(assert(purpose ReligiousType islam))
	else(if(eq ?response 2)
		then(assert(purpose ReligiousType christian)))
	else(if(eq ?response 3)
		then(assert(purpose ReligiousType zoroastrianism)))
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
	(declare (salience 40))
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
	(declare (salience 40))
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
	(declare (salience 40))
	?f <- (personality2 allocentric)
	(personality2 allocentric)
	(crispPersonality2 ?p2)
	=>
	(bind ?p2Real (+ 50 (/ ?p2 2)))
	(assert (crispPersonality2 ?p2Real))
	(printout t "crispPersonality2=" ?p2Real crlf)
	(retract ?f)
)

(defrule realize-Personality-psychocentric
	(declare (salience 40))
	?f <- (personality2 psychocentric)
	(personality2 psychocentric)
	(crispPersonality2 ?p2)
	=>
	(bind ?p2Real (- 50 (/ ?p2 2)))
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
	(declare (salience 40))
	(crispAge ?a)
	(crispPersonality1 ?p1)
	(crispPersonality2 ?p2)
	=>
	(assert (AgeGroup (?a 0) (?a 1) (?a 0)))
	(assert (PersonalityIntrovertExtrovert (?p1 0) (?p1 1) (?p1 0)))
	(assert (PersonalityAllocentricPsychocentric (?p2 0) (?p2 1) (?p2 0)))
)
	
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
;;--------------------------------------------------------------------------------
;;plot-fuzzy-value						;;
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defrule show-fuzzy-value-AgeGroup
	(declare (salience 0))
	(show fuzzyValue)
	=>
	(plot-fuzzy-value t * nil nil 
				(create-fuzzy-value AgeGroup Young)
				(create-fuzzy-value AgeGroup Adult)
				(create-fuzzy-value AgeGroup Old)						
	)
)

(defrule show-fuzzy-value-personality1
	(declare (salience 0))
	(show fuzzyValue)
	=>
	(plot-fuzzy-value t "-+" nil nil 
				(create-fuzzy-value PersonalityIntrovertExtrovert Introvert)
				(create-fuzzy-value PersonalityIntrovertExtrovert Extrovert)
	)
)

(defrule show-fuzzy-value-Personality2
	(declare (salience 0))
	(show fuzzyValue)
	=>
	(plot-fuzzy-value t "-+" nil nil 
				(create-fuzzy-value PersonalityAllocentricPsychocentric Allocentric)
				(create-fuzzy-value PersonalityAllocentricPsychocentric Psychocentric)
	)
)


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
;;--------------------------------------------------------------------------------
;;Output Results	
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defrule check-explorer
	(declare (salience 30))
	?f <- (checkType yes)
	(or (PersonalityIntrovertExtrovert Introvert) (PersonalityIntrovertExtrovert VeryIntrovert))
	(or (PersonalityAllocentricPsychocentric Allocentric) (PersonalityAllocentricPsychocentric VeryAllocentric))
	=>
	(assert (type explorer))
	(retract ?f)
)

(defrule check-adventurer
	(declare (salience 30))
	?f <- (checkType yes)
	(or (PersonalityIntrovertExtrovert Extrovert) (PersonalityIntrovertExtrovert VeryExtrovert))
	(or (PersonalityAllocentricPsychocentric Allocentric) (PersonalityAllocentricPsychocentric VeryAllocentric))
	=>
	(assert (type adventurer))
	(retract ?f)
)

(defrule check-guided
	(declare (salience 30))
	?f <- (checkType yes)
	(or (PersonalityIntrovertExtrovert Introvert) (PersonalityIntrovertExtrovert VeryIntrovert))
	(or (PersonalityAllocentricPsychocentric Psychocentric) (PersonalityAllocentricPsychocentric VeryPsychocentric))
	=>
	(assert (type guided))
	(retract ?f)
)

(defrule check-groupie
	(declare (salience 30))
	?f <- (checkType yes)
	(or (PersonalityIntrovertExtrovert Extrovert) (PersonalityIntrovertExtrovert VeryExtrovert))
	(or (PersonalityAllocentricPsychocentric Psychocentric) (PersonalityAllocentricPsychocentric VeryPsychocentric))
	=>
	(assert (type groupie))
	(retract ?f)
)


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
;;--------------------------------------------------------------------------------
;;Advise destination	
;;--------------------------------------------------------------------------------
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defrule suggest-food1
	(declare (salience 10))
	(purposeCulinary yes)
	(purpose CulinaryType cookingWithLocals)
	=>
	(printout t "For 'cookingWithLocals' you should participate in local celebration" crlf)
)
(defrule suggest-food2
	(declare (salience 10))
	(purposeCulinary yes)
	(purpose CulinaryType cookingWorkshops)
	=>
	(printout t "For 'cookingWorkshops' you should participate in local celebration" crlf)
)
(defrule suggest-food3
	(declare (salience 10))
	(purposeCulinary yes)
	(purpose CulinaryType eatingAtLocalsHomes)
	=>
	(printout t "For 'eatingAtLocalsHomes' you should live in local home-stay" crlf)
)
(defrule suggest-food4
	(declare (salience 10))
	(purposeCulinary yes)
	(purpose CulinaryType eatingAtLocalRestaurants)
	=>
	(printout t "For 'eatingAtLocalRestaurants' you should go 'ÔåÑÒÇÏ', 'ÎÇä ÓÑÇ', 'ÓíÊí ÓäÊÑ' restaurant " crlf)
)
(defrule suggest-food5
	(declare (salience 10))
	(purposeCulinary yes)
	(purpose CulinaryType eatingStreetFood)
	=>
	(printout t "For 'eatingStreetFood' you should go to 'ÔÇåíä ÔåÑ' and enjoy it" crlf)
)
;;-------------------------------------------------
(defrule suggest-religious1
	(declare (salience 10))
	(purposeReligious yes)
	(purpose ReligiousType islam)
	=>
	(printout t "For 'islam' you can visit 'ãÓÌÏ ÌÇãÚ' 'ãÓÌÏÓíÏ' 'ãÓÌÏ ÔíÎ áØÝ Çááå' " crlf)
)
(defrule suggest-religious2
	(declare (salience 10))
	(purposeReligious yes)
	(purpose ReligiousType christian)
	=>
	(printout t "For 'christian' you can visit 'ãÑíã ãÞÏÓ' 'æÇä˜' 'ÆæÑ ãÞÏÓ' churches" crlf)
)
(defrule suggest-religious3
	(declare (salience 10))
	(purposeReligious yes)
	(purpose ReligiousType zoroastrianism)
	=>
	(printout t "For 'zoroastrianism' you can visit 'ÂÊÔÇå' 'ÎÇäå ÒÑÊÔÊíÇä' 'ÏÑ ãåÑ æåÑ ãåÑÈÇä'" crlf)
)
;;-------------------------------------------------
(defrule suggest-museum1
	(declare (salience 10))
	(purposeMuseum yes)
	(purpose MuseumType art)
	=>
	(printout t "For 'art' you can visit 'åäÑåÇí ÒíÈÇ' 'åäÑåÇí ãÚÇÕÑ' 'åá ÓÊæä' museum" crlf)
)
(defrule suggest-museum2
	(declare (salience 10))
	(purposeMuseum yes)
	(purpose MuseumType civilization)
	=>
	(printout t "For 'civilization' you can visit 'ÍãÇã ÚáíÞí ÂÞÇ' 'ÎÇäå ãÔÑæØå' 'æÇä˜' museum" crlf)
)
(defrule suggest-museum3
	(declare (salience 10))
	(purposeMuseum yes)
	(purpose MuseumType science)
	=>
	(printout t "For 'science' you can visit 'ÂãæÒÔ æ ÑæÑÔ' museum" crlf)
)
(defrule suggest-museum4
	(declare (salience 10))
	(purposeMuseum yes)
	(purpose MuseumType nature)
	=>
	(printout t "For 'nature' you can visit 'ÊÇÑíÎ ØÈíÚí' 'äÇŽæÇä' museum" crlf)
)
(defrule suggest-museum5
	(declare (salience 10))
	(purposeMuseum yes)
	(purpose MuseumType ancient)
	=>
	(printout t "For 'ancient' you can visit 'åá ÓÊæä' 'ÇÔÑÇÝ' 'ÚÕÇÑÎÇäå' museum" crlf)
)
;;-------------------------------------------------
(defrule suggest-treatment1
	(declare (salience 10))
	(purposeTreatment yes)
	(purpose reatmentType medicinal)
	=>
	(printout t "For 'medicinal' you can visit 'ãÑÇä' 'ÔÑíÚÊí' 'ÎæÑÔíÏ' hospitals" crlf)
)
(defrule suggest-treatment2
	(declare (salience 10))
	(purposeTreatment yes)
	(purpose reatmentType beauty)
	=>
	(printout t "For 'beauty' you can visit 'íÇÓ' 'ÂÑÇÏ' clinics" crlf)
)
;;-------------------------------------------------
(defrule suggest-leisure1
	(declare (salience 10))
	(purposeLeisure yes)
	(purpose leisuretType relaxation)
	(purpose OutdoorActivityType desert)
	=>
	(printout t "For 'relaxation' you can go 'ãÑäÌÇÈ' 'æÑÒäå' deserts" crlf)
)
(defrule suggest-leisure6
	(declare (salience 10))
	(purposeLeisure yes)
	(purpose leisuretType relaxation)
	(purpose OutdoorActivityType nature)
	=>
	(printout t "For 'relaxation' you can go 'äÇŽæÇä' 'ÇÏÇä' nature" crlf)
)
(defrule suggest-leisure2
	(declare (salience 10))
	(purposeLeisure yes)
	(purpose leisuretType sport)
	=>
	(printout t "For 'sport' you can go 'ãÌãæÚå ÇäÞáÇÈ' 'ÈÇÛ ÌæÇä' " crlf)
)
(defrule suggest-leisure3
	(declare (salience 10))
	(purposeLeisure yes)
	(purpose leisuretType cinema)
	=>
	(printout t "For 'cinema' you can go 'ÑÏíÓ åÇÑÈÇÛ' 'ÞÏÓ' cinemas " crlf)
)
(defrule suggest-leisure4
	(declare (salience 10))
	(purposeLeisure yes)
	(purpose leisuretType theatre)
	=>
	(printout t "For 'theatre' you can go 'ÓæÑå' " crlf)
)
(defrule suggest-leisure5
	(declare (salience 10))
	(purposeLeisure yes)
	(purpose leisuretType music)
	=>
	(printout t "For 'music' you can participate in monthly concerts " crlf)
)
;;-------------------------------------------------
(defrule suggest-outdoor1
	(declare (salience 10))
	(purposeOutdoorActivity yes)
	(purpose OutdoorActivityType urban)
	(style modernism)
	=>
	(printout t "For 'urban' you can visit 'ÓíÊí ÓäÊÑ' 'åÇÑ ÈÇÛ' " crlf)
)
(defrule suggest-outdoor4
	(declare (salience 10))
	(purposeOutdoorActivity yes)
	(purpose OutdoorActivityType urban)
	(style traditionalism)
	=>
	(printout t "For 'urban' you can visit 'ÈÇÒÇÑ ÞÏíã' 'ãíÏÇä äÞÔ ÌåÇä' 'åÇÑ ÈÇÛ' " crlf)
)
(defrule suggest-outdoor2
	(declare (salience 10))
	(purposeOutdoorActivity yes)
	(purpose OutdoorActivityType river)
	=>
	(printout t "For 'river' you can go 'ÓÇãÇä' 'ÈÇÛ ÈåÇÏÑÇä' 'ÇÏÇä' " crlf)
)
(defrule suggest-outdoor3
	(declare (salience 10))
	(purposeOutdoorActivity yes)
	(purpose OutdoorActivityType history)
	=>
	(printout t "For 'history' you can visit 'åá ÓÊæä' 'ãíÏÇä äÞÔ ÌåÇä' 'ãäÇÑ ÌäÈÇä' 'á ÎæÇÌæ' 'Óí æ Óå á' '˜ÇÎ åÔÊ ÈåÔÊ' " crlf)
)
;;-------------------------------------------------
; (defrule output-suggest-history2
	; (declare (salience 10))
	; (AgeGroup Young)
	; =>
	; (assert (suggest canyon))
	; (printout t "Young can go canyoning" crlf)
; )
; (defrule output-suggest-history3
	; (declare (salience 10))
	; (AgeGroup Old)
	; =>
	; (assert (suggest canyon))
	; (printout t "Old can go canyoning" crlf)
; )
; (defrule output-suggest-history4
	; (declare (salience 10))
	; (AgeGroup Adult)
	; =>
	; (assert (suggest canyon))
	; (printout t "Adult can go canyoning" crlf)
; )
;;-------------------------------------------------
(defrule output-wellcome1
	(declare (salience 15))
	(type explorer)
	?f <- (type ?type)
	(AgeGroup ?age)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" crlf)
	(printout t crlf "For " ?prefix " " ?name " as " ?age " " ?type " Isfahan is has big geography that you can enjoy." crlf crlf)
	(retract ?f)
)
(defrule output-wellcome2
	(declare (salience 15))
	(type adventurer)
	?f <- (type ?type)
	(AgeGroup ?age)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" crlf)
	(printout t crlf "For " ?prefix " " ?name " as " ?age " " ?type " Isfahan is has big geography that you can enjoy." crlf crlf)
	(retract ?f)
)
(defrule output-wellcome3
	(declare (salience 15))
	(type guided)
	?f <- (type ?type)
	(AgeGroup ?age)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" crlf)
	(printout t crlf "For " ?prefix " " ?name " as " ?age " " ?type " There are a lot of travel agency with perfect tour in Isfahan." crlf crlf)
	(retract ?f)
)
(defrule output-wellcome4
	(declare (salience 15))
	(type groupie)
	?f <- (type ?type)
	(AgeGroup ?age)
	(name ?name)
	(namePrefix ?prefix)
	=>
	(printout t crlf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" crlf)
	(printout t crlf "For " ?prefix " " ?name " as " ?age " " ?type " You can use Guide book or 4Squar application to enjoy Isfahan." crlf crlf)
	(retract ?f)
)
(defrule output-wellcome5
	(declare (salience 0))
	?f <- (show endMessage)
	(get end)
	=>
	(printout t crlf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" crlf)
	(retract ?f)
	
)




