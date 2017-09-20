;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname owls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Data definition

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; interp. each number in the list is an owl weight in ounces

(define LON1 empty)
(define LON2 (cons 1 empty))
(define LON3 (cons 3 (cons 2 empty)))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)                   ; Number
              (fn-for-lon (rest lon)))]))   ; ListOfNumber

;; Template rules used:
;; - one of: 2 cases:
;; - Atomic distinct: empty
;; - Compound: (cons Number ListOfNumber)
;; - self-reference: (rest lon) is ListOfNumber


;;=====================================
;; Functions

;; ListOfNumber -> Number
;; Consuming list, produces a sum of Numbers contained in that list
(check-expect (olwery-weight empty) 0)                         ; if list empty
(check-expect (olwery-weight (cons 0 empty)) 0)                ; if value is 0 and list contains 1 item
(check-expect (olwery-weight (cons 1 (cons 2 empty))) 3)       ; if list has 2 items

;(define (olwery-weight lon) 0) ; stub

;; <template from ListOfNumber>

(define (olwery-weight lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon) (olwery-weight (rest lon)))]))

;; ListOfNumber -> Number
;; Consuming list, produces the number of items containging in that list by adding 1 every recursion
(check-expect (olwery-size empty) 0)
(check-expect (olwery-size (cons 0 empty)) 1)
(check-expect (olwery-size (cons 3 (cons 9 empty))) 2)

;(define (olwery-size lon) 0) ; stub

;; <template from ListOfNumber>

(define (olwery-size lon)
  (cond [(empty? lon) 0]
        [else
         (+ 1 (olwery-size (rest lon)))]))




