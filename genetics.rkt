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
          (if (member lst (top popul (/ (length popul) 4)))
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
          (list (append (drop-right x c) (take-right y c))
                (append (drop-right y c) (take-right x c)))
          )
        )
      
      (define (reprod popul)
        (let ((popul (shuffle (if (odd? (length popul)) (cons (pick-random popul) popul) popul))))
          (if (null? popul)
              '()
              (append (cr-ovr (car popul) (cadr popul)) (reprod (cddr popul))))))
      
      (if (or
           (= 0 iter)
           (null? cycles)
           )
          (list (list #f))
          (let ((res (filter (lambda (arg) (= 100 (fit arg))) popul)))
            (if (null? res)
                (let* (
                       (new-popul (append popul (reprod popul)))
                       (surv (top (top new-popul (length popul)) (length popul)))
                       (best (fit (car surv)))
                       (worst (fit (list-ref surv (sub1 (length surv)))))
                       (mean (average (map fit surv)))
                       )
                  (if (equal? #t enable-visual)
                      (let* ((new-dc (new bitmap-dc% (bitmap (make-bitmap 1000 4000)))) (draw (draw-graph new-dc (car surv) cycles)) (iter-num (add1 (- MAX_ITER iter))))
                        (step-gen surv cycles (- iter 1) (cons new-dc dcs) (cons (list iter-num best) maxs) (cons (list iter-num worst) mins) (cons (list iter-num mean) avs)))
                      (step-gen surv cycles (- iter 1) dcs maxs mins avs))
                  )
                (if (equal? #t enable-visual)
                    (let* (
                           (new-dc (new bitmap-dc% (bitmap (make-bitmap 1000 4000))))
                           (draw (draw-graph new-dc (argmin length res) cycles))
                           (max-p (if (= iter MAX_ITER) (list (list 0 100)) '()))
                           (min-p (if (= iter MAX_ITER) (list (list 0 (fit (list-ref (top popul (length popul)) (sub1 (length popul)))))) '()))
                           (av-p (if (= iter MAX_ITER) (list (list 0 (average (map fit popul)))) '()))
                           (graphic (new bitmap-dc% (bitmap
                                                     (plot-bitmap (list (axes)
                                                                        (lines maxs #:color 2 #:label "Maxs")
                                                                        (lines mins #:color 4 #:label "Mins")
                                                                        (lines avs #:color 6 #:label "Avs")
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
                                                     ))))
                      (cons res (cons (cons graphic (cons new-dc dcs)) (add1 (- MAX_ITER iter)))
                            ))
                    res)
                )
            ))
      )
    (let*
        (
         (popul (start-popul k (get-verts graph) 50))
         (sorted-popul (top popul (length popul)))
         (tmp-res
          (step-gen
           (map (lambda (x) (normalize x cycles)) popul)
           cycles
           MAX_ITER
           '()
           (list (list 0 (fit (car sorted-popul))))
           (list (list 0 (fit (list-ref sorted-popul (sub1 (length sorted-popul))))))
           (list (list 0 (average (map fit popul)))))))
      tmp-res))
  
  (define target (make-bitmap 1000 4000))
  (define dc (new bitmap-dc% (bitmap (make-bitmap 1000 4000))))
  
  (define frame (new frame%
                     (label "Genetics visualizer")
                     (width 1100)
                     (height 1000)))
  
  (let* (
         (cycles (find-cycles graph))
         (tmp-res (start-gen cycles)))
    (if (equal? (caar tmp-res) #f)
        #f
        (if (equal? #t enable-visual)
            (let* ((res (argmin length (car tmp-res))) (dcs (cadr tmp-res)) (iters (map (lambda(iter)(string-append "Iter " (~a iter))) (range (cddr tmp-res)))))
              (define bmp-canvas 
                (new canvas% (parent frame)
                     (style (list 'vscroll 'no-autoclear))
                     (min-height 750)
                     (paint-callback
                      (lambda(c dc)
                        (if (= 0 (- (cddr tmp-res)(send my-choice get-selection)))
                            (send dc set-scale 2 2)
                            (send dc set-scale 1 1)
                            )
                        (send dc draw-bitmap (send (list-ref dcs (- (cddr tmp-res)(send my-choice get-selection))) get-bitmap) 0 0))
                      )))
              
              (define panel (new horizontal-panel% [parent frame]
                                 [alignment '(center center)]
                                 (min-height 50)))
              (new canvas% (parent panel)
                   (min-height 50)
                   (paint-callback
                    (lambda (c e)
                      (send (send c get-dc) draw-text (string-append "Result population:  " (~a res)) 490 0))
                    ))
              (define my-choice
                (new choice% (parent frame)
                     (label "Choose iter  ")
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
              (send bmp-canvas init-auto-scrollbars #f 4000 0 0)
              (send (send bmp-canvas get-dc) draw-bitmap (send (list-ref dcs 0) get-bitmap) 0 0)
              ;(send (send bmp-canvas get-dc) set-bitmap (send (list-ref dcs 0) get-bitmap))
              (send frame show #t)
              (list (not (equal? (car res) #f)) (length res) res))
            (let ((res (argmin length tmp-res)))
              (list (not (equal? (car res) #f)) (length res) res)))))
  )

(define graph
  ;'(((1 . 5) (17 . 18) (5 . 13) (16 . 1) (7 . 13) (14 . 12) (2 . 15) (14 . 6) (14 . 5) (13 . 7) (6 . 14) (12 . 11) (6 . 7) (15 . 10) (6 . 4) (1 . 0) (7 . 3) (17 . 3) (7 . 6) (6 . 16) (4 . 3) (15 . 12) (4 . 5) (12 . 16) (4 . 15) (10 . 7) (15 . 7)) . 2)
  '(((17 . 10) (3 . 10) (12 . 13) (7 . 2) (16 . 0) (4 . 7) (9 . 11) (1 . 13) (6 . 13) (8 . 0) (17 . 13) (8 . 12) (16 . 17) (7 . 18) (2 . 3) (15 . 19) (3 . 5) (19 . 8) (3 . 2) (5 . 14) (14 . 19) (1 . 11) (5 . 17) (0 . 16) (1 . 10) (10 . 2) (13 . 7) (6 . 16) (14 . 5) (18 . 14) (5 . 7) (18 . 7) (18 . 4) (5 . 6) (18 . 3) (4 . 12)) . 6)
  )


;(let* ((test-graph (cons (gen-complete-graph 6) 6))(res (genetics-solve (car test-graph) (cdr test-graph))))
(let* ((test-graph graph) (res (genetics-solve (car test-graph) (cdr test-graph) #t)))
 (begin
  (println test-graph)
 (printf "Cycles num: ~v\n" (length (find-cycles (car test-graph))))
(printf "k = ~v\n" (cdr test-graph))
(println res)
(println (start-bruteforce (car test-graph) (cdr test-graph)))))