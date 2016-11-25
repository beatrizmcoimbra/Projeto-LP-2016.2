#lang racket

;; FALTA FAZER A d)

;; a)
(define outline->painter
  (let ((v0 (make-vector 0 0))
        (v1 (make-vector 1 0))
        (v2 (make-vector 0 1))
        (v3 (make-vector 1 1)))
    (segments->painter (list (make-segment v0 v1)
                             (make-segment v0 v2)
                             (make-segment v1 v3)
                             (make-segment v2 v3)))))

;; b)
(define diagonals->painter
  (let ((v0 (make-vector 0 0))
        (v1 (make-vector 1 0))
        (v2 (make-vector 0 1))
        (v3 (make-vector 1 1)))
    (segments->painter (list (make-segment v1 v2)
                             (make-segment v0 v3)))))

;; c)
(define dimond->painter
  (let ((v1 (make-vector 0.5 0))
        (v2 (make-vector 1 0.5))
        (v3 (make-vector 0.5 1))
        (v4 (make-vector 0 0.5)))
    (segments->painter (list (make-segment v1 v2)
                             (make-segment v2 v3)
                             (make-segment v3 v4)
                             (make-segment v4 v5)))))