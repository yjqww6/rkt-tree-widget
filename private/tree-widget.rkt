#lang racket/base
(require "tree-pos-cache.rkt" "interfaces.rkt"
         "manual-scroll.rkt" "tree-mixin.rkt"
         racket/match racket/class racket/gui/base)

(provide (all-defined-out))

(define tree-op-mixin
  (mixin (tree<%>) ()
    (inherit set-root get-root compute-item-size)

    (define/private (validate-cursor who t)
      (unless (cursor-valid? (get-root) t)
        (error who "invalid cursor")))
    

    (define-syntax-rule (define-op (name t a ... v) op)
      (define/public (name t a ... v [expand? #f])
        (validate-cursor 'name t)
        (define-values (w h ind) (compute-item-size v))
        (set-root (op t a ... (λ () (values v w h ind)) expand? #f))))

    (define-op (append-item t v) cursor-append-item)
    (define-op (prepend-item t v) cursor-prepend-item)
    (define-op (insert-item t i v) cursor-insert-item)

    (define/public (update-item t i v)
      (validate-cursor 'update-item t)
      (define-values (w h ind) (compute-item-size v))
      (set-root (cursor-update-item t i (λ () (values v w h ind)))))

    (define/public (delete-item t i)
      (validate-cursor 'delete-item t)
      (set-root (cursor-delete-item t i)))

    (define/public (expand-item t b?)
      (validate-cursor 'expand-item t)
      (set-root (cursor-expand-item t b?)))

    (define/public (reset-items)
      (set-root empty-tree-pos-cache))

    (define/public (make-indices-cursor indices)
      (tree-pos-cache-make-indices-cursor (get-root) indices))

    (super-new)))

(define scrollable-mixin
  (mixin ((class->interface canvas%) tree<%>) (scrollable<%>)
    (inherit get-view-start get-root)
    
    (define/public (get-scrollable-size)
      (root-cursor-total-size (get-root)))
    
    (define/public (get-scrollable-pos)
      (get-view-start))

    (define/public (get-scrollable-canvas-start)
      (values 0 0))

    (super-new)))

(define tree-canvas-mixin
  (mixin ((class->interface canvas%) tree<%> scrollable<%>) ()
    (inherit refresh get-root
             get-scrollable-pos get-client-size
             get-scrollable-canvas-start)

    (define/override (on-positions-changed)
      (refresh))

    (define/public (paint-item i v x y)
      (void))

    (define/override (on-paint)
      (super on-paint)
      
      (define-values (x y) (get-scrollable-pos))
      (define-values (cw ch) (get-client-size))
      (define-values (cx cy) (get-scrollable-canvas-start))

      (define items (root-cursor-get-visible-items (get-root) y (+ y ch)))
      (for ([item (in-list items)])
        (match item
          [(vector c y v)
           (paint-item c v (+ (cursor-indent c) cx) (+ y cy))])))

    (define/public (locate-item x y)
      (define-values (cx cy) (get-scrollable-canvas-start))
      (root-cursor-locate-item (get-root) (and x (- x cx)) (- y cy)))
    
    (super-new)))

(define tree-widget%
  (manual-scroll-mixin
   (tree-canvas-mixin
    (scrollable-mixin
     (tree-op-mixin
      (tree-mixin
       canvas%))))))

(module+ main
  (define tree (new (tree-mixin object%)))
  (define u (new tree-updater% [tree tree]))
  
  (time
   (send u set-tree
         (let f ([i 5])
           (cond
             [(= i 0) (send u empty-tree)]
             [else
              (for/fold ([t (send u empty-tree)])
                        ([x (in-range 10)])
                (send u append-item t x #t (f (sub1 i))))])))))
