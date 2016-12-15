#lang racket

(define (rotate-90 painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 0.0))
   painter))

(define (below-rotation painter1 painter2)
  (rotate-90 (beside (rotate-270 painter1) (rotate-270 painter2))))

