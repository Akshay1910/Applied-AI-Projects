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
    (slot researchxp (allowed-values Y N))
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

; Rule 1
; Print initial information
(defrule initial
    (declare (salience 100))
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
    (printout t "TOEFL Score (Auto-filled to be 120 if you are a domestic applicant): " ?applicant.toefl crlf)
    (printout t "Letter of Recommendation: " ?applicant.lor crlf)
    (printout t "Statement of Purpose: " ?applicant.sop crlf)
	)
        
; Rule 2
; To check whether the applicant's GPA satisfies the University's criteria  
(defrule checkGPA
    (declare (salience 99))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.gpa 3.3) then
    	(printout t " We regret to inform you that you do not satisfy the academic performance criteria required." crlf)        
       	)
    )

; Rule 3
; To check whether the the applicant's intended major is offered by the university or not
(defrule checkMajor
    (declare (salience 98))
    ?applicant <- (applicant (name ?name))
    =>
    (if (and (<> ?applicant.major CS) (<> ?applicant.major ECE) (<> ?applicant.major Civil) (<> ?applicant.major Mech)) then
        (printout t "We are sorry but our university does not offer the course you intend to apply for." crlf)
        )
    )  

; Rule 4
; If a student has any pending backlogs, he is rejected by the university
(defrule backlogCheck
    (declare (salience 97))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.backlog Y) then
        (printout t "Sorry but the university finds your academic performance unsatisfactory and cannot offer you admission at this time. (Pending Backlogs)" crlf)
        )
    )       
 
; Rule 5
; If the applicant has no job experience then he is notified that getting in might be difficult but he can still apply
(defrule workxpCheck
    (declare (salience 96))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.workxp 0) then
        (printout t "We see that you have no job experience. While this may not necessarily be a red flag on your profile and you can still apply, getting in may be difficult since the application process is highly competitive." crlf)
        )
    )

; Rule 6
; If the applicant has no research experience then he is notified that getting in might be difficult but he can still apply
(defrule researchxpCheck
    (declare (salience 95))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.researchxp N) then
        (printout t "We see that you have no research experience. While this may not necessarily be a red flag on your profile and you can still apply, getting in may be difficult since the application process is highly competitive." crlf)
        )
    )

; Rule 7
; To inform the applicant that if he or she is an international applicant getting in will be tough
(defrule nationalityCheck
    (declare (salience 94))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.international Y) then
        (printout t "The international intake at our university is really low and competition will be really tough." crlf)
        )
    )

; Rule 8
; The University has a minimum GRE score requirement of 315
(defrule greScoreCheck
    (declare (salience 93))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.gre 315) then
        (printout t "You do not meet the university's criteria of minimum GRE score requirement." crlf)
        )
    )

; Rule 9
; The University has a minimum TOEFL requirement of 97
(defrule toeflScoreCheck
    (declare (salience 92))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.toefl 97) then
        (printout t "You do not meet the university's criteria of minimum TOEFL score requirement." crlf)
        )
    )

; Rule 10
; The University requires an applicant to submit 3 or more Letter of Recommendation
(defrule lorCheck
    (declare (salience 91))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.lor 3) then
        (printout t "Your application will be considered incomplete without 3 or more LORs" crlf)
        )
    )

; Rule 11
; The University requires an applicant to submit a Statement of Purpose.
(defrule sopCheck
    (declare (salience 90))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.sop N) then
        (printout t "All applicants are required to submit a Statement of Purpose to complete their application" crlf)
        )
    )

; Rule 12
; If all criteria are met, the portal motivates the applicant to move further with his applicant and submit
(defrule submitApp
    (declare (salience 89))
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
        
(assert (applicant (name "John Doe")
            (age 21)
            (major CS)
            (term fall)
            (gpa 3.5) 
            (backlog N)           
            (workxp 3)
            (researchxp Y)
            (international Y)
            (gre 321)
            (toefl 112)
            (lor 3)
        	(sop Y))) 
(run)                                     