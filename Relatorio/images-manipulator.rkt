#lang racket
(require sicp-pict)

;                                          Image Package (im)

;; Transformador linear

(define (im:transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))

;; Transformações elementares

(define (im:flip-vert painter)
  (im:transform-painter painter
                        (make-vect 0.0 1.0)  
                        (make-vect 1.0 1.0)  
                        (make-vect 0.0 0.0)))

(define (im:flip-horiz painter)
  (im:transform-painter painter
                        (make-vect 1.0 0.0)
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0)))

(define (im:rotate-90 painter)
  (im:transform-painter painter
                        (make-vect 1.0 0.0)
                        (make-vect 1.0 1.0)
                        (make-vect 0.0 0.0)))

(define (im:rotate-180 painter)
  (im:transform-painter painter
                        (make-vect 1.0 1.0)
                        (make-vect 0.0 1.0)
                        (make-vect 1.0 0.0)))


(define (im:rotate-270 painter)
  (im:transform-painter painter
                        (make-vect 0.0 1.0)
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0)))

(define (im:shrink-tl painter)
  (im:transform-painter painter
                        (make-vect 0.0 0.5)
                        (make-vect 0.5 0.5)
                        (make-vect 0.0 1.0)))

(define (im:shrink-tr painter)
  (im:transform-painter painter
                        (make-vect 0.5 0.5)
                        (make-vect 1.0 0.5)
                        (make-vect 0.5 1.0)))

(define (im:shrink-bl painter)
  (im:transform-painter painter
                        (make-vect 0.0 0.0)
                        (make-vect 0.5 0.0)
                        (make-vect 0.0 0.5)))

(define (im:shrink-br painter)
  (im:transform-painter painter
                        (make-vect 0.5 0.0)
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 0.5)))

;; Combinação de imagens

(define (im:beside painter1 painter2)
  (let ((paint-left
         (im:transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 0.5 0.0)
                               (make-vect 0.0 1.0)))
        (paint-right
         (im:transform-painter painter2
                               (make-vect 0.5 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

(define (im:beside3 painter1 painter2 painter3)
  (let ((paint-left
         (im:transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 0.33 0.0)
                               (make-vect 0.0 1.0)))
        (paint-center
         (im:transform-painter painter2
                               (make-vect 0.33 0.0)
                               (make-vect 0.66 0.0)
                               (make-vect 0.33 1.0)))
        (paint-right
         (im:transform-painter painter3
                               (make-vect 0.66 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.66 1.0))))
    (lambda (frame)
      (paint-left frame)
      (paint-center frame)
      (paint-right frame))))

(define (im:below painter1 painter2)
  (let ((paint-bottom
         (im:transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 0.5)))
        (paint-top
         (im:transform-painter painter2
                               (make-vect 0.0 0.5)
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

(define (im:below3 painter1 painter2 painter3)
  (let ((paint-bottom
         (im:transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 0.33)))
        (paint-middle
         (im:transform-painter painter2
                               (make-vect 0.0 0.33)
                               (make-vect 1.0 0.33)
                               (make-vect 0.0 0.67)))
        (paint-top
         (im:transform-painter painter3
                               (make-vect 0.0 0.67)
                               (make-vect 1.0 0.67)
                               (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-middle frame)
      (paint-top frame))))

;; Padrões

(define (im:xadrez-pattern painter) 
  (let ((pattern
        (lambda (frame)
          ((im:shrink-tl painter) frame)
          ((im:shrink-br painter) frame))))
    (im:below (im:beside pattern pattern)
              (im:beside pattern pattern))))

(define (im:right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (im:right-split painter (- n 1))))
        (im:beside painter (im:below smaller smaller)))))

(define (im:up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (im:up-split painter (- n 1))))
        (im:below painter (im:beside smaller smaller)))))

(define (im:corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (im:up-split painter (- n 1)))
            (right (im:right-split painter (- n 1))))
        (let ((top-left (im:beside up up))
              (bottom-right (im:below right right))
              (corner (im:corner-split painter (- n 1))))
          (im:beside (im:below painter top-left)
                     (im:below bottom-right corner))))))

(define (im:square-of-four tl tr
                           bl br)
  (lambda (painter)
    (let ((top (im:beside (tl painter) (tr painter)))
          (bottom (im:beside (bl painter) (br painter))))
      (im:below bottom top))))

(define (im:flipped-pairs painter)
  ((im:square-of-four identity im:flip-vert
                      identity im:flip-vert) painter))

(define (im:square-limit painter n)
  (let ((corner-base (im:corner-split painter n)))
    ((im:square-of-four im:flip-horiz identity
                        im:rotate-180 im:flip-vert) corner-base)))

(define (im:square-of-nine tl tc tr
                           ml mc mr
                           bl bc br)
  (lambda (painter)
    (let ((top (im:beside3 (tl painter) (tc painter) (tr painter)))
          (middle (im:beside3 (ml painter) (mc painter) (mr painter)))
          (bottom (im:beside3 (bl painter) (bc painter) (br painter))))
      (im:below3 bottom middle top))))
