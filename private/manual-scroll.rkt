#lang racket/base
(require "interfaces.rkt"
         racket/class racket/gui/base racket/math racket/match)
(provide manual-scroll-mixin)

(define manual-scroll-mixin
  (mixin ((class->interface canvas%) scrollable<%> tree<%>) ()
    (inherit refresh get-dc
             init-manual-scrollbars get-client-size
             get-scroll-range get-scroll-pos set-scroll-pos
             get-scrollable-size refresh-now)

    (init [(-ws wheel-step) 2]
          [[-style style] '()])

    (super-new [style
                (cond
                  [(memq 'no-autoclear -style) -style]
                  [else (cons 'no-autoclear -style)])])

    (define ws (* 2 -ws))
    (define scroll?
      (or (memq 'hscroll -style)
          (memq 'vscroll -style)))

    (define scrolling #f)

    (define/override (get-scrollable-client-size)
      (match scrolling
        [#f
         (get-client-size)]
        [(vector x y w h)
         (values w h)]))

    (define/override (get-scrollable-pos)
      (match scrolling
        [#f
         (values (get-scroll-pos 'horizontal)
                 (get-scroll-pos 'vertical))]
        [(vector x y w h)
         (values x y)]))

    (define/override (get-scrollable-canvas-start)
      (values (- (get-scroll-pos 'horizontal))
              (- (get-scroll-pos 'vertical))))

    (define/override (on-paint)
      (send (get-dc) clear)
      (super on-paint))

    (define/override (on-size cw ch)
      (super on-size cw ch)
      (update-scrollbar))

    (define/private (update-scrollbar)
      (define-values (cw ch) (get-client-size))

      (define-values (x y) (get-scrollable-pos))
      (define-values (w h) (get-scrollable-size))

      (define vw (max 1 (- w cw)))
      (define vh (max 1 (- h ch)))
      
      (init-manual-scrollbars vw vh 100 100 (min vw x) (min vh y)))

    (define/override (on-positions-changed)
      (update-scrollbar)
      (super on-positions-changed))

    (define/private (with-clipping-region dc x y w h proc)
      (define rgn (send dc get-clipping-region))
      (cond
        [(not rgn) (send dc set-clipping-rect x y w h)]
        [else (define r (new region% [dc dc]))
              (send r set-rectangle x y w h)
              (send r intersect rgn)
              (send dc set-clipping-region r)])
      (proc)
      (send dc set-clipping-region rgn))

    (define sy 0)

    (define/private (scroll-me dir pos [set? #t])
      (define x (get-scroll-pos 'horizontal))
      (define y sy)
      (when set?
        (set-scroll-pos dir pos))
      (set! sy (get-scroll-pos 'vertical))

      (define refreshed #f)

      (when (eq? dir 'vertical)
        (define ny sy)

        (define-values (cw ch) (get-client-size))

        (define h (abs (- ny y)))

        (unless (or (>= h ch) (= h 0) (odd? h))
          (refresh-now
           (λ (dc)
             (cond
               [(< y ny)
                (send dc copy 0 (- ny y) cw (- ch h) 0 0)]
               [else
                (send (get-dc) copy 0 0 cw (- ch h) 0 (- y ny))])

             (define-values (cx cy) (get-scrollable-canvas-start))

             (with-clipping-region
                 dc 0 (if (< y ny) (- ch h) 0) cw h
               (λ ()
                 (send dc clear)
                 (set! scrolling
                       (cond
                         [(< y ny)
                          (vector x (+ ny (- ch h)) cw h)]
                         [else
                          (vector x ny cw h)]))
                 (super on-paint)
                 (set! scrolling #f)))))
          (set! refreshed #t)))

      (unless refreshed
        (refresh)))

    (define/override (on-scroll ev)
      (scroll-me (send ev get-direction)
                 (send ev get-position)
                 #f))

    (define/override (on-char event)
      (define (adjust dx dy)
        (define s (send event get-wheel-steps))
        (define-values (vx vy) (get-scrollable-pos))
        (define-values (vw vh) (get-scrollable-size))
        (define-values (cw ch) (get-client-size))
        (when dx
          (scroll-me 'horizontal
                     (exact-round (max 0 (min vw (+ vx (* ws s dx)))))))
        (when dy
          (scroll-me 'vertical
                     (exact-round (max 0 (min vh (+ vy (* ws s dy))))))))
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