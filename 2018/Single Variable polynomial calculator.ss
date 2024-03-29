;; Single Variable polynomial calculator.ss
;;
;; makePoly 
;;  
;;   Given list of intergers, returns a polynomial
;; Takes input in the form (<coefficient> <exponent>...)
;; exponents are always displayed in decending order
;; no coefficient is zero except the zero polynomial
;; zero polynomial may be either (0 0) or ()
;; 
;; writePoly
;;
;;   Takes a polynomial and prints it nicely
;; format as: <c>x^<e> (+|-) ...
;; print exponents in decending order (will handle in makePoly)
;; No coefficient is zero (will handle in makePoly)
;; In case of zero poly print 0
;; If exponent is 1 then print <c>x
;; If exponent is 0 then print <c>
;; x is always lower case
;; must print negatives
;; no space between neg and num if first, else space before and after -
;;
;; addPolys
;;
;;  Takes two polynomials and adds numbers with equal exponents
;;  If 0 then remove from poly
;;
;; subPolys
;;
;;  Takes two polynomials and subtracts numbers 
;;  if 0 then remove element from poly
;;
;; multPolys
;;
;; multiplies two polynomials 
;; multiply conditionals
;; add exponents
;; first times first then first times second and so on
;; then second times first and so on
;;
;; evalPoly
;; 
;; Takes a real number and a poly 
;; returns value of number substituted for x
;;
;; diffPoly
;;
;; differentiates a single poly
;; exponent times variable 
;; exponent minus 1 
;; if exponent equals 0 then remove the value and exponent
;; 
;; integralPoly
;;
;; takes two real numbers and a poly
;; anti differentiates a single poly
;; exponent plus 1
;; variable divides exponent
;; final term doesn't matter
;; then uses evalPoly twice and subtracats the result
;;
;; cleanPoly
;;
;; removes any zero variable and its bonded exponent from the poly
;;
;; I have 18 days to get this program working. 
;;
;; 8 functions
;; additionally requires cleanPoly function 
;; ranked from easiest to hardest:
;;   addPolys  (Oct 29) (harder than i thought) (TESTED)
;;   subPolys  (Oct 29) (a litteral copy of the logic in addPolys) (TESTED)
;;   diffPoly  (Oct 30) (TESTED)
;;   integralPoly (Oct 30) (TESTED)
;;   evalPoly  (Oct 31- Nov 2) (TESTED)
;;   writePoly (Nov 3 - 4) (TESTED)
;;   cleanPoly (Nov 3 - 4) (TESTED)
;;   makePoly (Nov 5 - 7) (READY TO TEST, NEEDS DOCUMENTATION)
;;   multPolys (Nov 8 - 10)
;;   testing (Nov 11 - 16)



;;;;;;;;;;;
;;METHODS;;
;;;;;;;;;;;
;;
;; makePoly(list) --> list
;;
;; writePoly(list) 
;;   continueToWrite(list)
;;   isNextNeg(list)
;;
;; addPolys(list, list) --> list
;;   addPolys2(list, list, list) --> list
;;
;; subPolys(list, list) --> list
;;   subPolys2(list, list, list) --> list
;;
;; multPolys(list, list) --> list
;;
;; evalPoly(list, real) --> real
;;   findTheZeroExponent(list) --> real
;;   calculateTheValue (real, real, real) --> real
;;
;; diffPoly(list) --> list
;;
;; integralPoly(list, real, real) --> list
;;
;; cleanPoly(list) --> list
;;   cleanPoly2(list, list) --> list


;; addPolys(list poly1, list poly2) --> list addedPoly 
;;
;;  Takes two polynomials and adds coefficents with equal exponents
;;  If 0 then remove from poly
;; REQUIRES addPolys2(list, list, list) --> list

(define (addPolys poly1 poly2)
  (cond ;ensures it returns the result of the next function
    (#t (cleanPoly (addPolys2 poly1 poly2 '()))))); clean poly is used to remove any 0 pairs
    ;(#t (addPolys2 poly1 poly2 '()))));addPolys

;; addPolys2(list poly1, list poly2, list result)
;;
;; Parses through the lists present looking for equal exponents to add 
;; the coefficents of
;; returns result

(define (addPolys2 poly1 poly2 result)
  (cond
    ((null? poly1) 
      (append result poly2)) ;if the remainder of poly2 is < poly1
    ((null? poly2) 
      (append result poly1)) ;if the remainder of poly1 is < poly2
;; if exponents equal then add the two variables and give the result to result        
    ((equal? (cadr poly1) (cadr poly2))  
      (addPolys2 (cddr poly1) (cddr poly2) 
        (append result 
          (list 
            (+ (car poly1) (car poly2)) 
            (cadr poly1)))))

    ((> (cadr poly1) (cadr poly2)) 
      (addPoly2 (cddr poly1) poly2 result)) ;if cadr poly1 is greater
    ((< (cadr poly1) (cadr poly2)) 
      (addPoly2 poly1 (cddr poly2) result));if cadr poly2 is greater
    (else (display "uh oh, you shouldn't be here\n") result)));addPolys2


;; subPolys(list poly1, list poly2) --> list subtractedPoly
;;
;;  Takes two polynomials and subtracts coefficents with the same exponent 
;;  if 0 then remove element from poly
;; REQUIRES subpolys2(list, list, list) --> list

(define (subPolys poly1 poly2)
  (cond ;ensures it returns the result of the next function
    (#t (cleanPoly (subPolys2 poly1 poly2 '()))))) ; clean poly is used to remove any 0 pairs
    ;(#t cleanPoly(subPolys2 poly1 poly2 '()))));subPolys

;; subPolys2(list poly1, list poly2, list result)
;;
;; Parses through the lists present looking for equal exponents to subtract
;; the exponents of
;; returns result

(define (subPolys2 poly1 poly2 result)
(cond
  ((null? poly1) (append result poly2)) ;if the remainder of poly2 is < poly1
  ((null? poly2) (append result poly1)) ;if the remainder of poly1 is < poly2
;; if exponents equal then add the two variables and give the result to result        
  ((equal? (cadr poly1) (cadr poly2))  
    (subPolys2 (cddr poly1) (cddr poly2) (append result (list (- (car poly1) (car poly2)) (cadr poly1)))))
  ((> (cadr poly1) (cadr poly2)) (addPoly2 (cddr poly1) poly2 result)) ;if cadr poly1 is greater
  ((< (cadr poly1) (cadr poly2))  (addPoly2 poly1 (cddr poly2) result));if cadr poly2 is greater
  (else (display "uh oh, you shouldn't be here\n") result)));subPolys2


;; diffPoly(list polynomial) --> list
;; 
;; Takes a polynomial and differentiates it 
;; rules: multiply exponent by coefficent
;;        decreace exponent by 1
;;        if exponent = 0 then remove exponent and coefficent
;; REQUIRES diffPoly2(list, list) --> list

(define (diffPoly polynomial)
  (cond
    (#t (diffPoly2 polynomial '()))))
    
;; diffPoly2(list polynomial, list result) --> list
;;
;; Takes a list of numbers and a blank list. Applys basic rules of 
;; differentiation on the numbers. Assumes the equation only deals in 
;; the form ax^b (+/-) ...
;; Returns result 

(define (diffPoly2 polynomial result)
  (cond
    ((null? polynomial) result); if we have nothing left to interpret
    ((equal? (cadr polynomial) 1) 
      (diffPoly2 (cddr polynomial) result)); if the exponent - 1 equals 0 and therefore needs to be removed
    (else (diffPoly2 (cddr polynomial) 
      (append result 
        (list 
          (* (car polynomial) (cadr polynomial)) 
          (- (cadr polynomial) 1)))))))


;; integralPoly(list polynomial, real lowerBound, real upperBound) --> real
;; 
;; Takes a polynomail and two bounds. It then determines its antiderivative between those bounds 
;; REQUIRES evalPoly(list, real) --> list and integralPoly2(list, list) --> list

(define (integralPoly polynomial lowerBound upperBound) 
  (cond
    (#t 
      (- 
        (evalPoly (integralPoly2 polynomial '()) upperBound) 
        (evalPoly (integralPoly2 polynomial '()) lowerBound)))))

;; integralPoly2(list polynomail, list result) --> list
;;
;; Looks for -1 else it applys simple integral rules
(define (integralPoly2 polynomial result)
  (cond
    ((null? polynomial) result); if we have nothing left to interpret
    ((equal? (+ (cadr polynomial) 1) 0) 
      (integralPoly2 (cddr polynomial) 
        (append result
          (list 
            (car polynomial)
            ("ln"))))); if the exponent + 1 equals 0 --> ln(x)
    (else 
      (integralPoly2 
        (cddr polynomial) 
        (append result 
          (list 
            (/ (car polynomial) (+ (cadr polynomial) 1)) 
            (+ (cadr polynomial) 1)))))))


;; evalPoly(list polyToCalculate, real valueOfX) --> real
;;
;; Takes a polynomial and a real value for evaluation 
;; First takes the value of x and puts it to the exponents 
;; Second multiplys value by coefficient 
;; Finally adds value to other value calculated thoughout polynomial
;; REQUIRES findTheZeroExponent(list) --> real and calculateTheValue(real, real, real) --> real

(define (evalPoly polyToCalculate valueOfX)
  (cond
    ((null? polyToCalculate) 0);if the polynomial is just 0
    ((equal? 0 valueOfX) 
      (findTheZeroExponent polyToCalculate));if value entered is zero then only constants are returned
    (else (+ 
      (calculateTheValue (car polyToCalculate) (cadr polyToCalculate) valueOfX) 
      (evalPoly (cddr polyToCalculate) valueOfX)))));evalPoly

;; findTheZeroExponent(list polyToParseThrough) --> real
;;
;; Takes a list and parses through looking for an exponent 
;; equal to zero. It then returns the coefficent attached to it

(define (findTheZeroExponent polyToParseThrough)
  (cond
    ((equal? 0 (cadr polyToParseThrough)) 
      (car polyToParseThrough)); if exponent is 0 then return value of coefficent
    ((null? (cddr polyToParseThrough)) 0);if there is no next 
    (else (findTheZeroExponent (cddr polyToParseThrough)))));findTheZeroExponent

;; calculateTheValue(real coefficent, real exponent, real x) --> real
;;
;; Takes three values and preforms coefficent*x^exponent
;; then returns the value calculated

(define (calculateTheValue coefficent exponent x)
  (cond
    ((equal? 0 coefficent) 0);if coefficent is 0
    ((equal? 0 exponent) coefficent);if exponent is 0 then exp x ^ 0 = exp
    ((equal? "ln" exponent) (* (log x) coefficent))
    (else (* (expt x exponent) coefficent))));calculateTheValue


;; writePoly(list polyToClean)
;;
;; Takes a polynomial and writes it in the form
;; coefficent x ^ exponent + ...
;; if next coefficent is negitive it will put a - instead of +
;; if the first coefficent is negitive it will put -coefficent
;; REQUIRES continueToWrite(list)

(define (writePoly polyToWrite)
  (cond
    ((null? polyToWrite) ;if empty poly
      (display "0\n"))
    ((negative? (car polyToWrite))
      (display "-")
      (continueToWrite polyToWrite))
    (else 
      (continueToWrite polyToWrite))));writePoly

;; continueToWrite(list polyToWrite)
;; 
;; checks if the end of the polynomial is reached
;; if not it recursivly calls itself and anothoer function
;; REQUIRES continueToWrite2(list) and isNextNeg(list)

(define (continueToWrite polyToWrite)
  (cond 
    ((null? (cddr polyToWrite)) 
      (continueToWrite2 polyToWrite)
      (display "\n"))
    (else
      (continueToWrite2 polyToWrite)
      (isNextNeg polyToWrite) 
      (continueToWrite (cddr polyToWrite)))))

;; continueToWrite2(list polyToWrite)
;;
;; Looks through cases of what the exponent could be 
;; and prints an appropriate response

(define (continueToWrite2 polyToWrite)
  (cond 
    ((equal? 0 (cadr polyToWrite)) 
      (display (abs (car polyToWrite))))
    ((equal? 1 (cadr polyToWrite))
      (display (abs (car polyToWrite)))
      (display "x"))
    ((negative? (cadr polyToWrite))
      (display (abs (car polyToWrite)))
      (display "/x^")
      (display (abs (cadr polyToWrite))))
    (else 
      (display (abs (car polyToWrite))) 
      (display "x^") 
      (display (cadr polyToWrite)))))

;; isNextNeg(list polyInQuestion)
;;
;; Takes a list and determines if the next coefficent is negetive

(define (isNextNeg polyInQuestion)
  (cond
    ((negative? (caddr polyInQuestion)) ;if negative
      (display " - ")) ;;returns a positive number
    (else (display " + ")))) ;is NextNeg


;; cleanPoly(list polyToClean) --> list
;;
;; Takes a polynomial and removes any coefficents that are 0.
;; Any exponents that lose a coefficent are removed as well.
;; If empty returns the polynomial recieved 
;; REQUIRES cleanPoly2(list, list) --> list

(define (cleanPoly polyToClean)
  (cond
    ((null? polyToClean) polyToClean)
    (else (cleanPoly2 polyToClean '()))));cleanPoly

;; cleanPoly2(list polyToClean, list cleanPoly) --> list
;;
;; Takes a polynomial to itterate through and a blank list
;; checks for 0 in the coefficent spot
;; removes offending pairs
;; returns cleanPoly

(define (cleanPoly2 polyToClean cleanPoly)
  (cond
    ((null? polyToClean) cleanPoly) ;if done
    ((equal? 0 (car polyToClean));if coefficent == 0
      (cleanPoly2 (cddr polyToClean) cleanPoly)) ;skip it 
    (else (cleanPoly2 
      (cddr polyToClean) 
      (append 
        (append cleanPoly 
          (list (car polyToClean))) 
          (list (cadr polyToClean)))))));cleanPoly2


;; makePoly(list listToPoly) --> list
;;
;; Takes a list of real numbers and formats them into two number 
;; chuncks. These chunks are then placed in decending order from
;; highest exponent to lowest. makePoly uses a quicksort function.
;; The function then returns the sorted and formated list.
;; CRUD... TAKE TO ROSANNA FOR HELP DEBUGGING... YUCK
;; REQUIRES: removeCoefficents(list, list, list) --> list

(define (makePoly listToPoly)
  (cond
    ((null? listToPoly) '())
    (else (removeCoefficents listToPoly listToPoly '()))));;makePoly

;; removeCoefficents(list unsortedList, list unsortedCopy, list unsortedExponents) --> list
;;
;; Takes a list, a copy of it and a blank list. Destroys copy to create a list
;; of exponents for the next function. Should return a sorted list
;; TESTED AND RETUNS WHAT I EXPECT IT TO
;; REQUIRES fixTheCoefficents(list, lsit, list) --> list
;; and qSort(list) --> list

(define (removeCoefficents unsortedList unsortedCopy unsortedExponents)
  (cond 
    ((null? unsortedCopy) 
      (fixTheCoefficents unsortedList (qSort unsortedExponents) '())) 
    (else 
      (removeCoefficents unsortedList (cddr unsortedCopy) 
        (append unsortedExponents (list (cadr unsortedCopy)))))));removeCoefficents


(define (fixTheCoefficents unsortedList sortedExponents sortedList)
  (cond
    ((null? sortedExponents) sortedList)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (else 
      (fixTheCoefficents unsortedList (cdr sortedExponents) 
        (append sortedList 
          (fixTheCoefficents2 unsortedList sortedExponents sortedList))))));fixTheCoefficents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fixTheCoefficents2 FULLY TESTED AND GIVES THE RESULT I WANT 
(define (fixTheCoefficents2 unsortedList sortedExponents sortedList)
  (cond
    ((null? unsortedList) sortedList)
    ((equal? (cadr unsortedList) (car sortedExponents)) 
      (append 
        (append sortedList (list (car unsortedList))) 
        (list (cadr unsortedList))))
    (else (fixTheCoefficents2 (cddr unsortedList) sortedExponents sortedList))));fixTheCoefficents2


;; qSort(list aList) --> list
;;
;; Takes a list of numbers to be sorted lowest to highest
;; returns the sorted list
;; REQUIRES partition(function, list) --> list

(define (qSort aList)
  (cond
    ((null? aList) '())
    (else                     ;;(car aList) is pivot
      (append (append
        (qSort 
          (partition 
            (lambda (x) (< x (car aList))) aList))
        (partition 
          (lambda (x) (= x (car aList))) aList)
        (qSort 
          (partition 
            (lambda (x) (> x (car aList))) aList)))))));qSort

;; partition(funciton compFn, list aList) --> list
;;
;; UMM YEAH, I NO LONGER REMEMEBER WHAT THIS FUNCTION DOES...
;; NOR DO I REMEMBER WHAT compFn's TYPE IS OR WHAT IT DOES...

(define (partition compFn aList)
  (cond
    ((null? aList) '())
    ((compFn (car aList)) 
      (cons (car aList) 
        (partition compFn (cdr aList))))
    (else (partition compFn (cdr aList)))))