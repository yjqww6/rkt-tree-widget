#lang racket/base
(require racket/class)
(provide auto-scroll-mixin)

(define (auto-scroll-mixin %)
  (class %
    (inherit refresh
             init-auto-scrollbars get-view-start get-client-size
             get-dc scroll get-virtual-size
             get-total-size)
    (init [(-ws wheel-step) 3]
          [[-style style] '()])
   
    (super-new [style -style])

    (define ws -ws)
    (define scroll?
      (or (memq 'hscroll -style)
          (memq 'vscroll -style)))

    (define/private (update-scrollbar)
      (define-values (cw ch) (get-client-size))

      (define-values (x y) (get-view-start))
      (define-values (w h) (get-total-size cw ch))

      (define vw (max w (+ x cw)))
      (define vh (max h (+ y ch)))
      
      (init-auto-scrollbars vw vh
                            (/ x (max 1 x (- vw cw)))
                            (/ y (max 1 y (- vh ch)))))

    (define/override (on-positions-changed)
      (update-scrollbar)
      (super on-positions-changed))

    (define/override (on-char event)
      (define (c d s v l cl)
        (and d (> l cl)
             (max 0.0 (min  1.0 (/ (+ (* ws s d) v) (- l cl))))))
      (define (adjust dx dy)
        (define s (send event get-wheel-steps))

        (define-values (vx vy) (get-view-start))
        (define-values (vw vh) (get-virtual-size))
        (define-values (cw ch) (get-client-size))

        (scroll (c dx s vx vw cw) (c dy s vy vh ch)))
      (cond
        [scroll?
         (define code (send event get-key-code))
         (case code
           [(wheel-up) (adjust #f -1)]
           [(wheel-down) (adjust #f 1)]
           [(wheel-left) (adjust -1 #f)]
           [(wheel-right) (adjust 1 #f)]
           [else (super on-char event)])]
        [else
         (super on-char event)]))

    (send this wheel-event-mode 'integer)
    ))