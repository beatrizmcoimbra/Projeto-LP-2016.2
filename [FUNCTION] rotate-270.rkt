#lang racket

(define (rotate-270 painter)
   ((transform-painter (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
    painter))