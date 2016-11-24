#lang racket

;; Constructor

(define (make-segment vect-1 vect-2)
  (cons vect-1 vect-2))

;; Selectors

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))