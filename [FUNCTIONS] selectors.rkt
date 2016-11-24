#lang racket


; Primeira implementação

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

; Seletores

(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (car (cdr frame)))
(define (frame-edge2 frame) (car (cdr (cdr frame))))



;Segunda implementação

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame2-origin frame) (car frame))
(define (frame2-edge1 frame) (car(cdr frame)))
(define (frame2-edge2 frame) (cdr (cdr frame)))
