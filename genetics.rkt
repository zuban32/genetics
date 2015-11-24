#lang scheme/base

(require racket/list)
(require racket/draw)
(require racket/gui)
(require plot)

(require "common.rkt")
(require "graph.rkt")
(require "solve.rkt")
(require "visual.rkt")

(provide genetics-solve)

(define (start-popul k verts popul-len)
  (define (create-popul lst len vert)
    (if (= len 0)
        lst
        (if (member vert lst)
            (create-popul lst len (pick-random verts))
            (create-popul (cons vert lst) (- len 1) (pick-random verts)))
        ))
  (if (= popul-len 0)
      '()
      (cons (create-popul '() (gen-random (+ k 1)) (pick-random verts)) (start-popul k verts (- popul-len 1)))))

(define (gen-out-verts verts cycles)
  (define (norm-step lst res int-lst)
    (define (check-vert arg)
      (foldl (lambda (arg1 result) (or result (and (not (equal? (car arg) (car arg1)))(not (member (car arg1) res)) (full-in? (cdr arg) (cdr arg1))))) #f int-lst))
    (if (null? lst)
        res
        (norm-step (cdr lst)
                   (if (check-vert (car lst))
                       (cons (caar lst) res)
                       res)
                   int-lst)
        
        ))
  (let ((int-lst (map (lambda (arg) (cons arg (filter (lambda (arg1) (member arg arg1)) cycles))) verts)))
    (norm-step int-lst '() int-lst)))

(define (normalize verts cycles)
  (let ((out (gen-out-verts verts cycles))(in-cycles (remove-duplicates (flatten cycles))))
    (filter (lambda(arg)(not (member arg out))) verts)))

(define MAX_ITER 30)

(define (genetics-solve graph k enable-visual)
  (define (start-gen cycles)
    
    (define (top lst num)
      (define (top-step cur len)
        (if (= 0 len)
            '()
            (cons (car cur) (top-step (cdr cur) (- len 1)))))
      (top-step (sort lst > #:key fit) (floor num)))
    
    (define (bottom lst num)
      (define (bottom-step cur len)
        (if (= 0 len)
            '()
            (cons (car cur) (bottom-step (cdr cur) (- len 1)))))
      (bottom-step (sort lst < #:key fit) (floor num)))
    
    
    (define (fit1 lst)
      (define (fit-vert x)
        (filter (lambda (arg) (member x arg)) cycles))
      (if (null? cycles) 0
          (/ (length (remove-duplicates (foldl (lambda (arg res) (append res (fit-vert arg))) '() lst))) (length cycles))))
    
    (define (fit2 lst)
      (- 1 (/ (length (filter (lambda (vert) (ormap (lambda(cycle) (member vert cycle)) cycles)) lst))
              (length lst))))
    
    (define (fit3 lst)
      (let ((out (gen-out-verts lst cycles)))
        (/ (length out) (length cycles))))
    
    (define (fit lst)
      (* (fit1 lst) 100))
    
    (define (step-gen popul cycles iter dcs maxs mins avs)
      
      ; absolutely useless function because of memory amout it needs 
      (define (check-cycles)
        (begin (printf "checking cycles\n")
               (let ((combs (comb cycles (+ k 1))))
                 (ormap (lambda(x) (
                                    (let ((part-combs (comb combs 2)))
                                      (andmap (lambda(arg)(not(intersects? (car arg) (cdr arg)))) part-combs))))
                        combs))))
      
      
      (define (mutate popul verts)
        (define (mutate-person lst)
          (if (member lst (bottom popul (/ (length popul) 5)))
              (remove-duplicates (map (lambda (arg) (if (= 0 (random 10)) (pick-random verts) arg)) lst))
              lst))
        (define (mutate-step lst num)
          (if (< num 1)
              lst
              (cons (mutate-person (car lst)) (mutate-step (cdr lst) (- num 1)))
              ))
        (mutate-step (shuffle popul) (length popul))
        )
      
      (define (cr-ovr x y)
        (let* ((c (gen-random (min (length x) (length y)))))
          (list (remove-duplicates (append (drop-right x c) (take-right y c)))
                (remove-duplicates (append (drop-right y c) (take-right x c))))
          )
        )
      
      (define (reprod popul)
        (let ((popul (shuffle (if (odd? (length popul)) (cons (pick-random popul) popul) popul))))
          (if (null? popul)
              '()
              (append (cr-ovr (car popul) (cadr popul)) (reprod (cddr popul))))))
      
      (if 
       (null? cycles)
       (list (list #f))
       (let ((res (filter (lambda (arg) (= 100 (fit arg))) popul)))
         (if (and (null? res) (< 0 iter))
             (let* (
                    (new-popul (append popul (reprod popul)))
                    (surv (top (mutate (top new-popul (length popul)) (get-verts graph)) (length popul)))
                    (best (fit (car surv)))
                    (worst (fit (list-ref surv (sub1 (length surv)))))
                    (mean (average (map fit surv)))
                    )
               (if (equal? #t enable-visual)
                   (let* ((new-dc (new bitmap-dc% (bitmap (make-bitmap FIELD_WIDTH FIELD_HEIGHT)))) (draw (draw-graph new-dc (argmin length (filter (lambda(arg)(= (fit arg) best)) surv)) cycles)) (iter-num (add1 (- MAX_ITER iter))))
                     (step-gen surv cycles (- iter 1) (cons new-dc dcs) (cons (list iter-num best) maxs) (cons (list iter-num worst) mins) (cons (list iter-num mean) avs)))
                   (step-gen surv cycles (- iter 1) dcs maxs mins avs))
               )
             (if (equal? #t enable-visual)
                 (let* (
                        ;(new-dc (new bitmap-dc% (bitmap (make-bitmap FIELD_WIDTH FIELD_HEIGHT))))
                        ;(draw (draw-graph new-dc (argmin length res) cycles))
                        (res (if (null? res) (list (list #f)) res))
                        (max-p (if (= iter MAX_ITER) (list (list 0 100)) '()))
                        (min-p (if (= iter MAX_ITER) (list (list 0 (fit (list-ref (top popul (length popul)) (sub1 (length popul)))))) '()))
                        (av-p (if (= iter MAX_ITER) (list (list 0 (average (map fit popul)))) '()))
                        (graphic
                         (parameterize
                             (
                              (plot-x-far-ticks no-ticks)
                              (plot-y-far-ticks no-ticks)
                              (plot-x-ticks (linear-ticks #:divisors '(1) #:number (add1 (- MAX_ITER iter))))
                              )
                           (new bitmap-dc% (bitmap
                                            (plot-bitmap (list (axes)
                                                               (lines maxs #:color 2 #:label "Max")
                                                               (lines mins #:color 4 #:label "Min")
                                                               (lines avs #:color 6 #:label "Average")
                                                               (points max-p #:color 2)
                                                               (points min-p #:color 4)
                                                               (points av-p #:color 6)
                                                               )
                                                         #:x-label "Iter num"
                                                         #:y-label "Fit"
                                                         #:x-min 0
                                                         #:y-min 0 #:y-max 100
                                                         #:legend-anchor 'bottom-right
                                                         )
                                            )))))
                   (cons res (cons (cons graphic dcs) (add1 (- MAX_ITER iter)))
                         ))
                 (if (null? res) (list (list #f)) res))
             )
         ))
      )
    (let*
        (
         (popul (start-popul k (get-verts graph) 50))
         (sorted-popul (top popul (length popul)))
         (best (fit (car sorted-popul)))
         (dc0 (if enable-visual (new bitmap-dc% (bitmap (make-bitmap FIELD_WIDTH FIELD_HEIGHT))) '()))
         (draw (if enable-visual (draw-graph dc0 (argmin length (filter (lambda(arg)(= (fit arg) best)) sorted-popul)) cycles) '()))
         (tmp-res
          (step-gen
           (map (lambda (x) (normalize x cycles)) popul)
           cycles
           MAX_ITER
           (list dc0)
           (list (list 0 best))
           (list (list 0 (fit (list-ref sorted-popul (sub1 (length sorted-popul))))))
           (list (list 0 (average (map fit popul)))))))
      tmp-res))
  
  (define target (make-bitmap FIELD_WIDTH FIELD_HEIGHT))
  (define dc (new bitmap-dc% (bitmap (make-bitmap FIELD_WIDTH FIELD_HEIGHT))))
  
  (define frame (new frame%
                     (label "Genetics visualizer")
                     (width (* (/ 11 10) FIELD_WIDTH))
                     (height (/ FIELD_HEIGHT 4))))
  
  (let* (
         (cycles (find-cycles graph))
         (tmp-res (start-gen cycles)))
    (if (and (equal? (caar tmp-res) #f) (null? cycles))
        #f
        (if (equal? #t enable-visual)
            (let* ((res (argmin length (car tmp-res))) (dcs (cadr tmp-res)) (iters (map (lambda(iter)(string-append "Iter " (~a iter))) (range (cddr tmp-res)))))
              (define bmp-canvas 
                (new canvas% (parent frame)
                     (style (list 'vscroll 'no-autoclear))
                     (min-height (* FIELD_WIDTH (/ 3 4)))
                     (paint-callback
                      (lambda(c dc)
                        (if (= 0 (- (cddr tmp-res)(send my-choice get-selection)))
                            (send dc set-scale 2 2)
                            (send dc set-scale 1 1)
                            )
                        (send dc draw-bitmap (send (list-ref dcs (- (cddr tmp-res)(send my-choice get-selection))) get-bitmap) 0 0))
                      )))
              
              
              (new canvas% (parent frame)
                   (min-height 100)
                   (min-width 250)
                   ;(horiz-margin (/ FIELD_WIDTH 4))
                   (paint-callback
                    (lambda (c dc)
                      (send dc set-font (send the-font-list find-or-create-font 20 'default 'normal 'normal))   
                      (send dc draw-text (string-append "Result population:  " (if (equal? #f (car res)) "No answer" (~a res))) (/ FIELD_WIDTH 4) 50))
                    ))
              (define my-choice
                (new choice% (parent frame)
                     (label "Choose view  ")
                     (choices (append iters (list "Graphic")))
                     (selection (length iters))
                     (callback
                      (lambda (tp e)
                        (send (send bmp-canvas get-dc) clear)
                        (if (= 0 (- (cddr tmp-res)(send my-choice get-selection)))
                            (send (send bmp-canvas get-dc) set-scale 2 2)
                            (send (send bmp-canvas get-dc) set-scale 1 1)
                            )
                        (send (send bmp-canvas get-dc) draw-bitmap (send (list-ref dcs (- (cddr tmp-res)(send tp get-selection))) get-bitmap) 0 0)))))
              (send bmp-canvas init-auto-scrollbars #f FIELD_HEIGHT 0 0)
              (send (send bmp-canvas get-dc) draw-bitmap (send (list-ref dcs 0) get-bitmap) 0 0)
              ;(send (send bmp-canvas get-dc) set-bitmap (send (list-ref dcs 0) get-bitmap))
              (send frame show #t)
              (if (equal? #f (car res)) #f
                  (list (not (equal? (car res) #f)) (length res) res)))
            (let ((res (argmin length tmp-res)))
              (list (not (equal? (car res) #f)) (length res) res)))))
  )

(define graph
  '(((27 . 35) (19 . 14) (33 . 4) (25 . 13) (23 . 3) (4 . 8) (22 . 26) (15 . 35) (38 . 12) (14 . 32) (10 . 39) (39 . 7) (18 . 0) (5 . 28) (30 . 25) (39 . 19) (29 . 10) (32 . 33) (0 . 38)
               (7 . 12) (3 . 21) (24 . 12) (19 . 39) (34 . 32) (9 . 35) (27 . 11) (19 . 5) (33 . 3) (1 . 13) (0 . 17) (2 . 21) (39 . 37) (35 . 21) (32 . 19)
               (35 . 5) (34 . 19) (20 . 13) (27 . 24) (26 . 20) (5 . 27) (24 . 26) (11 . 38) (11 . 4) (8 . 36) (14 . 9) (22 . 9) (33 . 21) (9 . 15) (25 . 35) (12 . 24) (16 . 33) (30 . 18) (24 . 32) (24 . 20)
               (7 . 6) (11 . 16) (1 . 0) (17 . 5) (32 . 12) (31 . 6) (22 . 24) (6 . 39) (9 . 38) (7 . 39) (5 . 17) (4 . 6) (11 . 10) (1 . 15)) . 10)
  )


(define (test-gen)
  ;(let* ((test-graph (cons (gen-complete-graph 5) 4))(res (genetics-solve (car test-graph) (cdr test-graph) #t)))
  (let* ((test-graph (gen-graph 30)) (res (genetics-solve (car test-graph) (cdr test-graph) #t)))
    (begin
      (println test-graph)
      (printf "Cycles num: ~v\n" (length (find-cycles (car test-graph))))
      (printf "k = ~v\n" (cdr test-graph))
      (println res)
      )))

(test-gen)