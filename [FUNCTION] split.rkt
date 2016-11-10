#lang racket

(define (split f1 f2)
  (if (= n 0)
      painter
      (let ((smaller (split painter (- n 1))))
(f1 painter (f2 smaller smaller)))))

(define right-split (split beside below))

(define up-split (split below beside))
  
  