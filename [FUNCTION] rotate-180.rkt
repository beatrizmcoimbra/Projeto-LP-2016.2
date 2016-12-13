#lang racket

(define (rotate-180 painter)
   ((transform-painter (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0))
    painter))
