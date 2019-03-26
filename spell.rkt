#lang racket

; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2019                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains simple dictionary definition
(require "dictionary.rkt")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

(define create-bitvector
  (lambda (hfl dict)
    (if (null? hfl)
       '()
       (cons (map (first hfl) dict)
          (create-bitvector (rest hfl) dict)))))

(define element-in-list?
  (lambda (elem list)
    (if (null? list)
        #f
    (if (= elem (car list))
        #t
        (element-in-list? elem (cdr list))
        ))))

;; Calls element-in-list to check if word is in bitvector
(define word-in-bv? 
  (lambda (wordbv masterbv)
    (if (null? wordbv)
        #t
    (if (element-in-list? (car wordbv) (car masterbv))
        (word-in-bv? (cdr wordbv) (cdr masterbv))
        #f))))

;; For use in key with reduce
(define key-op 
  (lambda (x y)
    (+ (* 29 y) (ctv x))
    ))
    
;; -----------------------------------------------------
;; KEY FUNCTION
(define key
  (lambda (w)
    (reduce key-op w 5413)
    ))

;; HASH FUNCTION GENERATORS
(define gen-hash-division-method
  (lambda (size) ;; value of parameter "size" should be a prime number
    (lambda (w)
      (if (null? w)
          '()
      (let ([k (key w)]) ;; create key
      (modulo k size)))))) ;; returns this function

(define gen-hash-multiplication-method
  (lambda (size) 
    (lambda (w)
      (if (null? w)
          '()
      (let ([k (key w)]) ;; create key for word
        (floor (* size(- (* k A) (floor (* k A)))))))))) ;; returns this function

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

;;(define hash-1 (gen-hash-division-method 70111))
;;(define hash-2 (gen-hash-division-method 89989))
;;(define hash-3 (gen-hash-multiplication-method 700426))
;;(define hash-4 (gen-hash-multiplication-method 952))

;;(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
;;(define hashfl-2 (list hash-1 hash-3))
;;(define hashfl-3 (list hash-2 hash-3))
;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
     (lambda (w)
      (if (null? w)
          '()
      (let ([master-bitvector (create-bitvector hashfunctionlist dict)]) ;; list of lists, list of values for each hash function
      (let ([word-bitvector (flatten(create-bitvector hashfunctionlist (list w)))]) ;; flattened to make checking elements simpler
      (if(word-in-bv? word-bitvector master-bitvector)
         #t
         #f)))))))

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

;;(define checker-1 (gen-checker hashfl-1 dictionary))
;;(define checker-2 (gen-checker hashfl-2 dictionary))
;;(define checker-3 (gen-checker hashfl-3 dictionary))
