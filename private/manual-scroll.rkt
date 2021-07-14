#lang racket/base
(require "interfaces.rkt"
         racket/class racket/gui/base racket/math)
(provide manual-scroll-mixin)

(define manual-scroll-mixin
  (mixin ((class->interface canvas%) scrollable<%> tree-view<%>) ()
    (inherit refresh get-dc
             init-manual-scrollbars get-client-size
             get-scroll-range get-scroll-pos set-scroll-pos
             get-total-size)
    (init [(-ws wheel-step) 3]
          [[-style style] '()])
   
    (super-new [style -style])

    (define ws -ws)
    (define scroll?
      (or (memq 'hscroll -style)
          (memq 'vscroll -style)))

    (define/override (get-scrollable-size)
      (values (get-scroll-range 'horizontal)
              (get-scroll-range 'vertical)))

    (define/override (get-scrollable-pos)
      (values (get-scroll-pos 'horizontal)
              (get-scroll-pos 'vertical)))

    (define/override (get-scrollable-canvas-start)
      (values (- (get-scroll-pos 'horizontal))
              (- (get-scroll-pos 'vertical))))

    (define/override (on-size cw ch)
      (super on-size cw ch)
      (update-scrollbar))

    (define/private (update-scrollbar)
      (define-values (cw ch) (get-client-size))

      (define-values (x y) (get-scrollable-pos))
      (define-values (w h) (get-total-size cw ch))

      (define vw (max 1 (- w cw)))
      (define vh (max 1 (- h ch)))
      
      (init-manual-scrollbars vw vh 100 100 (min vw x) (min vh y)))

    (define/override (on-positions-changed)
      (update-scrollbar)
      (super on-positions-changed))

    (define/override (on-scroll ev)
      (set-scroll-pos (send ev get-direction)
                      (exact-round (send ev get-position)))
      (refresh))

    (define/override (on-char event)
      (define (adjust dx dy)
        (define s (send event get-wheel-steps))
        (define-values (vx vy) (get-scrollable-pos))
        (define-values (vw vh) (get-scrollable-size))
        (define-values (cw ch) (get-client-size))
        (when dx
          (set-scroll-pos 'horizontal
                          (exact-round (max 0 (min vw (+ vx (* ws s dx)))))))
        (when dy
          (set-scroll-pos 'vertical
                          (exact-round (max 0 (min vh (+ vy (* ws s dy)))))))
        (refresh))
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