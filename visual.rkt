#lang scheme/base
(require racket/draw)
(require racket/gui)

(require "graph.rkt")

(define FIELD_WIDTH 1000)
(define FIELD_HEIGHT 4000)
(define POINT_RADIUS 4)
(define POINT_DIAM (* 2 POINT_RADIUS))

(provide FIELD_WIDTH)
(provide FIELD_HEIGHT)
(provide draw-graph)

(define (ellipse-point x y)
  (cons (- x POINT_RADIUS) (- y POINT_RADIUS)))

(define (draw-graph dc popul cycles)
  
  (define (draw-vert vert x y)
    ;(printf "Vert ~v: (~v ~v)\n" vert x y) 
    (let* ((draw-place (ellipse-point x y)))
      (if (member vert popul)
          (send dc set-brush "red" 'solid)
          (send dc set-brush "black" 'solid))
      (send dc draw-text (~a vert) x (- y (+ 20 POINT_RADIUS)))  
      (send dc draw-ellipse (car draw-place) (cdr draw-place) POINT_DIAM POINT_DIAM)
      (list vert x y)))

  (define (draw-cycle cycle center diff radius)
    (define (draw-cycle-vert lst angle)
      (if (null? lst)
          '()
          (append (list (list (car lst) (+ (car center) (* (cos angle) radius)) (+ (cdr center) (* radius (sin angle))))) (draw-cycle-vert (cdr lst) (+ angle diff)))))
    (draw-cycle-vert (get-verts cycle) 0))

  (define (eq-vert? v1 v2)
    (equal? v1 (car v2)))

  (define (draw-edge edge verts)
    (let ((vert1 (car (member (car edge) verts eq-vert?)))(vert2 (car (member (cdr edge) verts eq-vert?))))
      (send dc draw-line (cadr vert1) (caddr vert1) (cadr vert2) (caddr vert2)))
    )

  (define (border-offset x)
    (+ x (* 3 POINT_DIAM)))
  
  (define (draw-cycles cycles)
    (define (draw-cycles-step lst rad x_ y_)
      (if (null? lst) '()
      (let* ((cur-cycle (car lst))
             (radius (* 10 (count-verts cur-cycle)))
             (next-new (<= FIELD_WIDTH (border-offset (+ x_ radius radius))))
             (vert-num (count-verts cur-cycle))
             (x (if next-new (border-offset 0) x_))
             (y (if next-new (border-offset (+ y_ (max radius rad) (max radius rad))) y_))
             (vert-places (draw-cycle cur-cycle (cons (+ x radius) (+ y radius)) (/ (* 2 pi) vert-num) (* 10 vert-num))))
        (for-each
         (lambda(edge)(draw-edge edge vert-places))
         (make-edges cur-cycle))
        (append
         vert-places
         (draw-cycles-step (cdr lst) (max rad radius) (border-offset (+ x radius radius)) y)))))
    (draw-cycles-step cycles 0 (border-offset 0) (border-offset 0)))
  (send dc set-smoothing 'aligned)
  (let ((verts (draw-cycles cycles)))
    (for-each (lambda(vert) (draw-vert (car vert) (cadr vert) (caddr vert))) verts))
  )