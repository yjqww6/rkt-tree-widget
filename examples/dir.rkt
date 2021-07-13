#lang racket/gui
(require "../main.rkt")

(struct entry (dir? name))

(define (paint-mixin %)
  (class %
    (inherit get-dc get-client-size
             get-view-start refresh
             locate-item
             reset-items append-item expand-item
             get-root make-indices-cursor)

    (define updating? #f)

    (define/private (draw-triangle dc x y h e?)
      (define h1 (/ h 3))
      (define h2 (* h1 2))
      (define h3 (/ h 2))
        
      (define brush (send dc get-brush))
      (define pen (send dc get-pen))
      (send dc set-brush "Gray" 'solid)
      (send dc set-pen "Gray" 0 'solid)
        
      (cond
        [(not e?)
         (send dc draw-polygon
               (list (cons (+ x h1) (+ y h1))
                     (cons (+ x h1) (+ y h2))
                     (cons (+ x h2) (+ y h3))))]
        [else
         (send dc draw-polygon
               (list (cons (+ x h1) (+ y h1))
                     (cons (+ x h2) (+ y h1))
                     (cons (+ x h3) (+ y h2))))])
      (send dc set-pen pen)
      (send dc set-brush brush))
                   
    (define/override (paint-item c v x y)
      (define dc (get-dc))
      (define-values (w h) (node-cursor-item-size c))
      
      (when (entry-dir? v)
        (draw-triangle dc x y h (node-cursor-expand? c)))
      
      (send dc draw-text (~a (entry-name v))
            (+ x h)
            y))
      
    (define/override (compute-item-size v)
      (define dc (get-dc))
      (define-values (w h d e)
        (send dc get-text-extent (entry-name v)))
      (define ch (exact-ceiling h))
      (define cw (exact-ceiling w))
      (cond
        [(entry-dir? v) (values (+ cw ch)
                                ch
                                ch)]
        [else (values cw ch 0)]))

    (define/override (on-event ev)
      (cond
        [(eq? (send ev get-event-type) 'left-down)
         (define-values (vx vy) (get-view-start))
         (define item (locate-item (+ vx (send ev get-x))
                                   (+ vy (send ev get-y))
                                   #t))
         (when item
           (expand-item item (not (node-cursor-expand? item)))
           (refresh))]
        [else (super on-event ev)]))

    (define/public (set-dir path)
      (dynamic-wind
       (λ () (set! updating? #t))
       (λ ()
         (reset-items)
         (let f ([path path] [l '()])
           (for ([p (in-list (directory-list path))]
                 [i (in-naturals)])
             (define dir? (directory-exists? (build-path path p)))
             (append-item (if (null? l)
                              (get-root)
                              (make-indices-cursor (reverse l)))
                          (entry dir? (path->string p))
                          #t)
             (when dir?
               (f (build-path path p) (cons i l))))))
       (λ () (set! updating? #f)))
      (on-positions-changed))

    ;supress
    (define/override (on-positions-changed)
      (unless updating?
        (super on-positions-changed)))

    (super-new)))

(module+ main
  (define f (new frame% [label "test"] [width 320] [height 640]))
  
  (define t (new (paint-mixin tree-widget%)
                 [parent f] [style '(vscroll)]))

  (define b (new button%
                 [parent f]
                 [label "set dir"]
                 [callback (λ (b e)
                             (cond
                               [(get-directory)
                                =>
                                (λ (d) (send t set-dir d))]
                               [else (void)]))]))

  (time (send t set-dir ".."))
  
  (send f show #t))