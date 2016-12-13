#lang sicp
(#%require sicp-pict)


(define frame1 (make-frame (make-vect 0 0)
                           (make-vect 0 1)
                           (make-vect 0.5 1)))

(define e einstein)

(define p paint)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Ao tentar aplicar o up-split e right-split
;; usando a high-order split, a função paint
;; retorna a imagem splitada. Dá erro.

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs1 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit1 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n)
              )))

(define (frame-coord-map1 frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor v)
                               (frame-edge1 frame))
                 (vector-scale (vector-ycor v)
                               (frame-edge2 frame))))))

;; Não dá pra implementar o segments->painter, pois
;; o pacote não tem o draw-line. Contudo, o pacote
;; já tem o segments-painter.

(define seg->paint-exemple 
  (segments->painter (list (make-segment (make-vector 0 0)
                                         (make-vector 1 1)))))

(define (transform-painter1 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))

(define (flip-vert1 painter)
  (transform-painter1 painter
                      (make-vect 0.0 1.0)   ; new origin
                      (make-vect 1.0 1.0)   ; new end of edge1
                      (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter1 painter
                      (make-vect 0.5 0.5)
                      (make-vect 1.0 0.5)
                      (make-vect 0.5 1.0)))

(define (rotate-90 painter)
  (transform-painter1 painter
                      (make-vect 1.0 0.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter1 painter
                      (make-vect 0.0 0.0)
                      (make-vect 0.65 0.35) ;; Esse vetor está gerando
                                            ;; um erro quando aplicado
                                            ;; à imagem do einstein.
                      (make-vect 0.35 0.65)))


(define (beside1 painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter1 painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter1 painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
       
(define (flip-horiz painter)
   ((transform-painter (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
    painter))
        
        
