#lang racket

(define (make-vect x y)
   (cons x y))

(define (xcor-vect vector)
   (car vector))

(define (ycor-vect vector)
   (cdr vector))


(define (add-vect vector1 vector2)
  (make-vect
   (+ (xcor-vect vector1) (xcor-vect vector2))
   (+ (ycor-vect vector1) (ycor-vect vector2))))

(define (sub-vect vector1 vector2)
  (make-vect
   (- (xcor-vect vector1) (xcor-vect vector2))
   (- (ycor-vect vector1) (ycor-vect vector2))))

(define (scale-vect vector number)
  (make-vect
   (* number (xcor-vect vector))
   (* number (ycor-vect vector))))
  
