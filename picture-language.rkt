#lang sicp
(#%require sicp-pict)

;; Funções Primitivas

(define (transform-painter-im painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))

;; Transformações de imagens

(define (flip-vert-im painter)
  (transform-painter-im painter
                        (make-vect 0.0 1.0)  
                        (make-vect 1.0 1.0)  
                        (make-vect 0.0 0.0)))

(define (flip-horiz-im painter)
  (transform-painter-im painter
                         (make-vect 1.0 0.0)
                         (make-vect 0.0 0.0)
                         (make-vect 1.0 1.0)))

(define (rotate-90 painter)
  (transform-painter-im painter
                        (make-vect 1.0 0.0)
                        (make-vect 1.0 1.0)
                        (make-vect 0.0 0.0)))

(define (rotate-180 painter)
  (transform-painter-im painter
                        (make-vect 1.0 1.0)
                        (make-vect 0.0 1.0)
                        (make-vect 1.0 0.0)))


(define (rotate-270 painter)
  (transform-painter-im painter
                        (make-vect 0.0 1.0)
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0)))

(define (squash-inwards painter)
  (transform-painter-im painter
                        (make-vect 0.0 0.0)
                        (make-vect 0.65 0.35)
                        (make-vect 0.35 0.65)))


(define (shrink-to-upper-right painter)
  (transform-painter-im painter
                        (make-vect 0.5 0.5)
                        (make-vect 1.0 0.5)
                        (make-vect 0.5 1.0)))

;; Combinação de imagens

(define (beside-im painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter-im painter1
                                 (make-vect 0.0 0.0)
                                 split-point
                                 (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter-im painter2
                                 split-point
                                 (make-vect 1.0 0.0)
                                 (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (beside3-im painter1 painter2 painter3)
  (let ((split-point1 (make-vect 0.33 0.0))
        (split-point2 (make-vect 0.66 0.0)))
    (let ((paint-left
           (transform-painter-im painter1
                                 (make-vect 0.0 0.0)
                                 split-point1
                                 (make-vect 0.0 1.0)))
          (paint-center
           (transform-painter-im painter2
                                 split-point1
                                 split-point2
                                 (make-vect 0.33 1.0)))
          (paint-right
           (transform-painter-im painter3
                                 split-point2
                                 (make-vect 1.0 0.0)
                                 (make-vect 0.66 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-center frame)
        (paint-right frame)))))

(define (below-im painter1 painter2)
  (rotate-90 (beside-im (rotate-270 painter1) (rotate-270 painter2))))

(define (below3-im painter1 painter2 painter3)
  (rotate-90 (beside3-im (rotate-270 painter1) (rotate-270 painter2) (rotate-270 painter3))))

;; High order

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside-im (tl painter) (tr painter)))
          (bottom (beside-im (bl painter) (br painter))))
      (below-im bottom top))))

(define (square-of-nine tl tc tr
                        ml mc mr
                        bl bc br)
  (lambda (painter)
    (let ((top (beside3-im (tl painter) (tc painter) (tr painter)))
          (middle (beside3-im (ml painter) (mc painter) (mr painter)))
          (bottom (beside3-im (bl painter) (bc painter) (br painter))))
      (below3-im bottom middle top))))

;; Operadores

(define (flipped-pairs painter)
  (let ((painter2 (beside-im painter (flip-vert-im painter))))
    (below-im painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside-im painter (below-im smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below-im painter (beside-im smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside-im up up))
              (bottom-right (below-im right right))
              (corner (corner-split painter (- n 1))))
          (beside-im (below-im painter top-left)
                     (below-im bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside-im (flip-horiz-im quarter) quarter)))
      (below-im (flip-vert-im half) half))))

;; Funções de Transformações Lineares

(define (frame-coord-map-im frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor v)
                               (frame-edge1 frame))
                 (vector-scale (vector-ycor v)
                               (frame-edge2 frame))))))

;; Painters

(define e einstein)
(define g diagonal-shading)