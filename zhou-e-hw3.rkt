;Eric Zhou, ezzhou
;BSL

;(1.)
(define-struct volunteer-org (org-type org-name min-age parental-consent? require-license? training-hours languages-spoken))

(define org1 (make-volunteer-org "animal shelter" "Catdog." 12 #true #false 12 (cons "English" empty)))
(define org2 (make-volunteer-org "soup kitchen" "Mama Mias Delicious Ravioli" 13 #false #false 6 (cons "Italian" (cons "English" (cons "French" (cons "Spanish" empty))))))
(define org3 (make-volunteer-org "nursing home" "Boomer Residencies" 21 #false #true 30 (cons "English" (cons "Chinese" (cons "Hindi" (cons "Spanish" (cons "Arabic" empty)))))))
(define org4 (make-volunteer-org "nursing home" "The Falls at Cordingly Dam" 24 #false #true 120 (cons "English" (cons "Spanish" empty))))

;(2.)
; volunteer-org Template
; fcn-for-volunteer-org volunteer-org -> ???
; (define (fcn-for-volunteer-org a-volunteer-org)
;   (... (volunteer-org-org-type a-volunteer-org)
;        (volunteer-org-org-name a-volunteer-org)
;        (volunteer-org-min-age a-volunteer-org)
;        (volunteer-org-parental-consent? a-volunteer-org)
;        (volunteer-org-require-license? a-volunteer-org)
;        (volunteer-org-training-hours a-volunteer-org)
;        (volunteer-org-languages-spoken a-volunteer-org)))

;(3.)
; a ListOfVolunteerOrg is one of
; empty
; (cons String ListOfVolunteerOrg)
; interp:  ListOfVolunteerOrg represents a list of volunteer organizations
(define L1 (cons org1 empty))
(define L2 (cons org3 (cons org4 empty)))
(define L3 (cons org1 (cons org2 (cons org3 (cons org4 empty)))))

;(4.)
;; lovo-fcn: ListOfVolunteerOrg -> ...
; (define (lovo-fcn a-lovo)
;   (cond [(empty? a-lovo) (...)]
;         [(cons? a-lovo)  (... (first a-lovo)
;                               (lovo-fcn (rest a-lovo)))]))

;(5.)
;Signature: ListOfVolunteerOrg -> Number
;Returns the number of organizations whose minimum age is 13 years or younger
(define (count-hs-eligible a-lovo)
  (cond
    [(empty? a-lovo) 0]
    [(cons? a-lovo) (if (<= (volunteer-org-min-age (first a-lovo)) 13)
                        (+ 1 (count-hs-eligible (rest a-lovo)))
                        (count-hs-eligible (rest a-lovo)))]))

(check-expect (count-hs-eligible L1) 1)
(check-expect (count-hs-eligible L2) 0)
(check-expect (count-hs-eligible L3) 2)

;(6.)
;Signature: ListOfVolunteerOrg Natural -> ListOfVolunteerOrg
;Returns a list of Volunteer Orgs that require volunteers to be licensed but require few than the training hours
(define (list-license-training a-lovo num)
  (cond
    [(empty? a-lovo) null]
    [(cons? a-lovo) (if (and (volunteer-org-require-license? (first a-lovo)) (< (volunteer-org-training-hours (first a-lovo)) num))
                        (cons (first a-lovo) (list-license-training (rest a-lovo) num))
                        (list-license-training (rest a-lovo) num))]))

(check-expect (list-license-training L1 15) empty)
(check-expect (list-license-training L2 50) (cons org3 empty))
(check-expect (list-license-training L3 200) (cons org3 (cons org4 empty)))

;(7.)
;Signature: ListOfVolunteerOrg -> ListOfString
;Returns a list of all the languages spoken by all the organizations
(define (languages-spoken a-lovo)
  (cond
    [(empty? a-lovo) null]
    [(cons? a-lovo) (append (volunteer-org-languages-spoken (first a-lovo)) (languages-spoken (rest a-lovo)))]))

(check-expect (languages-spoken L1) (cons "English" empty))
(check-expect (languages-spoken L3) (cons "English" (cons "Italian" (cons "English" (cons "French" (cons "Spanish" (cons "English" (cons "Chinese" (cons "Hindi" (cons "Spanish" (cons "Arabic" (cons "English" (cons "Spanish" '())))))))))))))
                    
;(8.)
;Signature: ListOfLanguages -> Boolean
;Returns whether or not a organization has clients that speak Spanish
(define (contains-spanish? a-lol)
  (cond
    [(empty? a-lol) #false]
    [(cons? a-lol) (if (string-ci=? "Spanish" (first a-lol))
                       #true
                       (contains-spanish? (rest a-lol)))]))

(check-expect (contains-spanish? (volunteer-org-languages-spoken org1)) #false)
(check-expect (contains-spanish? (volunteer-org-languages-spoken org3)) #true)

;Signature: ListOfVolunteerOrg -> ListOfVolunteerOrg
;Returns a list of volunteer organizations who have Spanish as one of the languages spoken
(define (need-spanish-speakers a-lovo)
  (cond
    [(empty? a-lovo) null]
    [(cons? a-lovo) (if (contains-spanish? (volunteer-org-languages-spoken (first a-lovo)))
                         (cons (first a-lovo) (need-spanish-speakers (rest a-lovo)))
                         (need-spanish-speakers (rest a-lovo)))]))

(check-expect (need-spanish-speakers L1) empty)
(check-expect (need-spanish-speakers L3) (cons org2 (cons org3 (cons org4 empty))))
