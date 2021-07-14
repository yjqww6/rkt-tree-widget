#lang racket/gui
(require "../main.rkt")
(require racket/math racket/format)

(define (paint-mixin %)
  (class %
    (inherit get-dc get-client-size
             get-view-start refresh
             locate-item
             get-root)

    (define selected (make-weak-box #f))

    (define/public (get-selected-item)
      (define c (weak-box-value selected))
      (if (cursor-valid? (get-root) c)
          c
          #f))
                   
    (define/override (paint-item c v x y)
      (define dc (get-dc))
      (define sel? (weak-box-value selected))
      (when (and sel? (cursor-equal? c sel?))
        (define-values (aw ah) (node-cursor-item-size c))
        (send dc draw-rectangle x y aw ah))
      (send dc draw-text (~a v) x y))
      
    (define/override (compute-item-size v)
      (define dc (get-dc))
      (define-values (w h d e)
        (send dc get-text-extent (~a v)))
      (define cw (exact-ceiling w))
      (values cw (exact-ceiling h) (+ cw 5)))

    (define/override (on-event ev)
      (cond
        [(eq? (send ev get-event-type) 'left-down)
         (define-values (vx vy) (get-view-start))
         (define item (locate-item (+ vx (send ev get-x))
                                   (+ vy (send ev get-y))
                                   #t))
         (when item
           (set! selected (make-weak-box item)))
         (refresh)]
        [else (super on-event ev)]))

    (super-new)))

(module+ main
  (define f (new frame% [label "test"] [width 640] [height 240]))
  
  (define t (new (paint-mixin tree-widget%) [parent f] [style '(vscroll)]))

  (collect-garbage)
  (time
   (for ([i (in-range 10)])
     (send t append-item (send t get-root) i #t)
     (for ([j (in-range 3)])
       (send t append-item (cursor-get-child (send t get-root) i) j #t)
       (for ([k (in-range 2)])
         (send t append-item (send t make-indices-cursor (list i j)) k #t)))))
  
  (send f show #t))
