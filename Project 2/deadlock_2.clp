(import nrc.fuzzy.*)

(import nrc.fuzz.jess.*)

(load-package nrc.fuzzy.jess.FuzzyFunctions)

(deftemplate applicant
    (slot name)
    (slot age (type INTEGER))
    ; Applying for course (Right now limited to CS, ECE, Civil, Mech)
    (slot major)
    ; Intended term of application 
    (slot term (allowed-values fall spring))
    ; GPA in Bachelors out of 4.0
    (slot gpa (type FLOAT))
    ; Any pending backlogs?
    (slot backlog (allowed-values Y N))
    ; Job experience if any in terms of assistantship, internships, volunteer work etc.
    ; Input in the form of Number of years
    (slot workxp (type INTEGER))
    ; To determine if the applicant has research experience and publishings.
    (slot researchxp (type INTEGER))
    ; International or domestic applicant)
    (slot international (allowed-values Y N)) 
    ;Score in GRE
    (slot gre (type INTEGER))
    ; TOEFL score (enter 120 if domestic student)
    (slot toefl (type INTEGER))
    ; Letter of Recommendation
    (slot lor (type INTEGER))
    ; Statement of purpose
    (slot sop (allowed-values Y N))       
    )

(deftemplate university
    (slot name)
    ; Departments in School of Engineering
    (slot department (allowed-values CS ECE Civil Mech))
    ;Total annual fee for the program
    (slot totalfee (type INTEGER))
    ; Number of Recommendation letters required
    (slot reqlor (type INTEGER))
    ; Seat available for the applicant or not
    (slot seatavailable(allowed-values Y N))
    ; Available terms in the university
    (slot term (allowed-values fall spring))
    )

(deftemplate gpa_data
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate workxp_data
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate researchxp_data
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate gre_data
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate toefl_data
    "Auto-generated"
   (declare (ordered TRUE)))


(defglobal ?*gpaVar* = (new FuzzyVariable "gpa" 0.0 4.0))

(defglobal ?*workxpVar* = (new FuzzyVariable "workxp" 0 20))

(defglobal ?*researchxpVar* = (new FuzzyVariable "researchxp" 0 20))

(defglobal ?*greVar* = (new FuzzyVariable "gre" 260 340))

(defglobal ?*toeflVar* = (new FuzzyVariable "toefl" 0 120))

(call nrc.fuzzy.FuzzyValue setMatchThreshold 0.1)

; Rule 0
; Initializing global variables
(defrule MAIN::init-FuzzyVariables
    (declare (salience 100))
    ?applicant <- (applicant (name ?name))
    =>
    (call ?*gpaVar* addTerm "low" (new ZFuzzySet 2.0 3.0))
    (?*gpaVar* addTerm "medium" (new TrapezoidFuzzySet 2.7 3.0 3.2 3.4))
    (?*gpaVar* addTerm "High" (new SFuzzySet 3.2 3.6))
    (?*workxpVar* addTerm "low" (new ZFuzzySet 0 3))
    (?*workxpVar* addTerm "moderate" (new TrapezoidFuzzySet 2 4 5 7))
    (?*workxpVar* addTerm "experienced" (new SFuzzySet 6 9))
    (?*researchxpVar* addTerm "low" (new ZFuzzySet 0 3))
    (?*researchxpVar* addTerm "moderate" (new TrapezoidFuzzySet 2 4 5 7))
    (?*researchxpVar* addTerm "experienced" (new SFuzzySet 6 9))
    (?*greVar* addTerm "low" (new ZFuzzySet 260 295))
    (?*greVar* addTerm "medium" (new TrapezoidFuzzySet 280 300 305 320))
    (?*greVar* addTerm "High" (new SFuzzySet 315 325))
    (?*toeflVar* addTerm "low" (new ZFuzzySet 30 75))
    (?*toeflVar* addTerm "medium" (new TrapezoidFuzzySet 70 85 95 105))
    (?*toeflVar* addTerm "High" (new SFuzzySet 100 110))
    (assert (gpa_data (new FuzzyValue ?*gpaVar* (new SingletonFuzzySet ?applicant.gpa))))
    (assert (workxp_data (new FuzzyValue ?*workxpVar* (new SingletonFuzzySet ?applicant.workxp))))
    (assert (researchxp_data (new FuzzyValue ?*researchxpVar* (new SingletonFuzzySet ?applicant.researchxp))))
    (assert (gre_data (new FuzzyValue ?*greVar* (new SingletonFuzzySet ?applicant.gre))))
    (assert (toefl_data (new FuzzyValue ?*toeflVar* (new SingletonFuzzySet ?applicant.toefl))))        
    )

; Rule 1
; Print initial information
(defrule initial
    (declare (salience 99))
    ?applicant <- (applicant (name ?name))
    =>
    (printout t "********************Welcome to University Application Consultancy Services********************" crlf)
    (printout t "Hey "?applicant.name ". Thank you for reaching out to us" crlf)
    (printout t "Just to be sure, here is the information you provided to us" crlf)
    (printout t "Age: " ?applicant.age crlf)
    (printout t "Intended major: " ?applicant.major crlf)
    (printout t "Intended term: " ?applicant.term crlf)
    (printout t "GPA in undergrad: " ?applicant.gpa crlf)
    (printout t "Backlogs in undergrad: " ?applicant.backlog crlf)
    (printout t "Work Experience: " ?applicant.workxp " years" crlf)
    (printout t "Research Experience: " ?applicant.researchxp crlf)
    (printout t "International or Domestic Student?: " ?applicant.international crlf)
    (printout t "GRE Score: " ?applicant.gre crlf)
    (printout t "TOEFL Score (To be filled as 120 if you are a domestic applicant): " ?applicant.toefl crlf)
    (printout t "Letter of Recommendation: " ?applicant.lor crlf)
    (printout t "Statement of Purpose: " ?applicant.sop crlf)
	)
        
; Rule 2-i
; To check whether the applicant's GPA satisfies the University's criteria  
(defrule GPA_low
    (declare (salience 98))
    (gpa_data ?applicant&:(fuzzy-match ?applicant "low"))
     =>
    (printout t "We regret to inform you that you do not satisfy the academic performance criteria required." crlf)
    )

; Rule 2-ii
(defrule GPA_medium
    (declare (salience 97))
    (gpa_data ?applicant&:(fuzzy-match ?applicant "medium"))
     =>
    (printout t "The admit rate for people with better gpa is higher. We would like you to keep that in mind while applying." crlf)
    )

; Rule 2-iii
(defrule GPA_high
    (declare (salience 96))
    (gpa_data ?applicant&:(fuzzy-match ?applicant "high"))
     =>
    (printout t "Your GPA exceeds the average requirement of the university. While this is not the only criteria your profile is judged on, it is a strong steering factor." crlf)
    )

; Rule 3
; To check whether the the applicant's intended major is offered by the university or not
(defrule checkMajor
    (declare (salience 97))
    ?applicant <- (applicant (name ?name))
    =>
    (if (and (<> ?applicant.major CS) (<> ?applicant.major ECE) (<> ?applicant.major Civil) (<> ?applicant.major Mech)) then
        (printout t "We are sorry but our university does not offer the course you intend to apply for." crlf)
        )
    )  

; Rule 4
; If a student has any pending backlogs, he is rejected by the university
(defrule backlogCheck
    (declare (salience 96))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.backlog Y) then
        (printout t "Sorry but the university finds your academic performance unsatisfactory and cannot offer you admission at this time. (Pending Backlogs)" crlf)
        )
    )       
 
; Rule 5-i
; If the applicant has no job experience then he is notified that getting in might be difficult but he can still apply
(defrule workxp_zero
    (declare (salience 95))
    (workxp_data ?applicant&:(fuzzy-match ?applicant "low"))
     =>
    (printout t "We see that you have low job experience. While this may not necessarily be a red flag on your profile and you can still apply, getting in may be difficult since the application process is highly competitive." crlf)
    )

; Rule 5-ii
(defrule workxp_mod
    (declare (salience 95))
    (workxp_data ?applicant&:(fuzzy-match ?applicant "moderate"))
     =>
    (printout t "We see that you have some job experience. Some experience on this side of life always comes in handy when your profile is being reviewed by the board." crlf)
    )

; Rule 5-iii
(defrule workxp_exp
    (declare (salience 95))
    (workxp_data ?applicant&:(fuzzy-match ?applicant "experienced"))
     =>
    (printout t "We see that you have extensive job experience. This boosts your overall profile and makes it stronger." crlf)
    )

; Rule 6-i
; If the applicant has no research experience then he is notified that getting in might be difficult but he can still apply
(defrule researchxp_low
    (declare (salience 94))
    (researchxp_data ?applicant&:(fuzzy-match ?applicant "low"))
    =>
    (printout t "We see that you have low research experience. While this may not necessarily be a red flag on your profile and you can still apply, getting in may be difficult since the application process is highly competitive." crlf)
    )

;Rule 6-ii
(defrule researchxp_mod
    (declare (salience 94))
    (researchxp_data ?applicant&:(fuzzy-match ?applicant "moderate"))
    =>
    (printout t "We see that you have moderate research experience. This strongly boosts your profile and would outshine any other aspect of your profile." crlf)
    )

; Rule 6-iii
(defrule researchxp_exp
    (declare (salience 94))
    (researchxp_data ?applicant&:(fuzzy-match ?applicant "experienced"))
    =>
    (printout t "We see that you have extensive experience in research. We believe this strongly boosts yours profile and increases you chances of getting an acceptance." crlf)
    )

; Rule 7
; To inform the applicant that if he or she is an international applicant getting in will be tough
(defrule nationalityCheck
    (declare (salience 93))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.international Y) then
        (printout t "The international intake at our university is really low and competition will be really tough." crlf)
        )
    )

; Rule 8-i
; To validate a user's profile through their GRE score
(defrule greScore_low
    (declare (salience 92))
    (gre_data ?applicant&:(fuzzy-match ?applicant "low"))
     =>
    (printout t "You do not meet the university's criteria of minimum GRE score requirement." crlf)
    )

; Rule 8-ii
(defrule greScore_med
    (declare (salience 92))
    (gre_data ?applicant&:(fuzzy-match ?applicant "medium"))
     =>
    (printout t "While your GRE score meets the minimum criteria, generally the university admits students with higher GRE score." crlf)
    )

; Rule 8-iii
(defrule greScore_high
    (declare (salience 92))
    (gre_data ?applicant&:(fuzzy-match ?applicant "high"))
     =>
    (printout t "You exceed the university's criteria of minimum GRE score requirement." crlf)
    )

; Rule 9-i
; To validate the profile on the basis of TOEFL score
(defrule toeflScore_low
    (declare (salience 91))
    (toefl_data ?applicant&:(fuzzy-match ?applicant "low"))
     =>
    (printout t "You do not meet the university's criteria of minimum TOEFL score requirement." crlf)   
    )

; Rule 9-ii
(defrule toeflScore_med
    (declare (salience 91))
    (toefl_data ?applicant&:(fuzzy-match ?applicant "medium"))
     =>
    (printout t "You met the criteria of minimum TOEFL requirement but giving it another shot might be worth it." crlf)   
    )

; Rule 9-iii
(defrule toeflScore_high
    (declare (salience 91))
    (toefl_data ?applicant&:(fuzzy-match ?applicant "high"))
     =>
    (printout t "You exceed the university's criteria of minimum TOEFL score requirement." crlf)   
    )

; Rule 10
; The University requires an applicant to submit 3 or more Letter of Recommendation
(defrule lorCheck
    (declare (salience 90))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.lor 3) then
        (printout t "Your application will be considered incomplete without 3 or more LORs" crlf)
        )
    )

; Rule 11
; The University requires an applicant to submit a Statement of Purpose.
(defrule sopCheck
    (declare (salience 89))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.sop N) then
        (printout t "All applicants are required to submit a Statement of Purpose to complete their application" crlf)
        )
    )

; Rule 12
; If all criteria are met, the portal motivates the applicant to move further with his applicant and submit
(defrule submitApp
    (declare (salience 88))
    ?a <- (applicant (name ?name))
    =>
    (if (or (> ?a.gpa 3.3) (= ?a.backlog N) (> ?a.gre 315) (> ?a.toefl 97) (>= ?a.lor 3) (= ?a.sop Y)) then
        (printout t "You are eligible to apply for the Masters program at our University" crlf)
        )
    )

;Rule 13
(defrule printFacts
    (declare (salience 1))
    =>
    (facts)
    )
(reset)
 
; Rule 14
(defrule init
    (declare (salience 50))
=>       
(assert (applicant (name "John Doe")
            (age 24)
            (major CS)
            (term fall)
            (gpa 3.5) 
            (backlog N)           
            (workxp 2)
            (researchxp 5)
            (international Y)
            (gre 310)
            (toefl 100)
            (lor 3)
        	(sop Y)))
)   
(run)                                 