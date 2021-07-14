#lang racket/base
(require "tree-pos-cache.rkt" "interfaces.rkt"
         "auto-scroll.rkt" "manual-scroll.rkt"
         racket/match racket/class racket/gui/base)

(provide (all-defined-out))

(define tree-mixin
  (mixin () (tree-view<%>)
    (define cache empty-tree-pos-cache)

    (define/public (compute-item-size)
      (values 1 1 0))

    (define/public (compute-indent v w h)
      w)

    (define/public (get-total-size cw ch)
      (tree-pos-cache-total-size cache))

    (define/public (locate-item x y [check-x? #f])
      (tree-pos-cache-locate-item cache x y check-x?))

    (define/public (get-visible-items start end)
      (tree-pos-cache-get-visible-items cache start end))

    (define/public (get-root)
      cache)

    (define/private (validate-cursor who t)
      (unless (cursor-valid? cache t)
        (error who "invalid cursor")))

    (define/public (on-positions-changed)
      (void))

    (define-syntax-rule (define-op (name t a ... v) op)
      (define/public (name t a ... v [expand? #f])
        (validate-cursor 'name t)
        (define-values (w h ind) (compute-item-size v))
        (set! cache (op t a ... (λ () (values v w h)) expand? ind))
        (on-positions-changed)
        (void)))

    (define-op (append-item t v) tree-pos-cache-append)
    (define-op (prepend-item t v) tree-pos-cache-prepend)
    (define-op (insert-item t i v) tree-pos-cache-insert)
    (define-op (update-item t i v) tree-pos-cache-update)

    (define/public (delete-item t i)
      (validate-cursor 'delete-item t)
      (set! cache (tree-pos-cache-delete t i))
      (on-positions-changed)
      (void))

    (define/public (expand-item t b?)
      (validate-cursor 'expand-item t)
      (set! cache (tree-pos-cache-expand t b?))
      (on-positions-changed)
      (void))

    (define/public (reset-items)
      (set! cache empty-tree-pos-cache)
      (on-positions-changed)
      (void))

    (define/public (make-indices-cursor indices)
      (tree-pos-cache-make-indices-cursor cache indices))
    
    (super-new)))

(define scrollable-mixin
  (mixin ((class->interface canvas%)) (scrollable<%>)
    (inherit get-view-start get-virtual-size)
    
    (define/public (get-scrollable-size)
      (get-virtual-size))
    
    (define/public (get-scrollable-pos)
      (get-view-start))

    (define/public (get-scrollable-canvas-start)
      (values 0 0))

    (super-new)))

(define tree-canvas-mixin
  (mixin ((class->interface canvas%) tree-view<%> scrollable<%>) ()
    (inherit refresh
             get-scrollable-pos get-client-size
             get-scrollable-canvas-start
             get-visible-items)

    (define/override (on-positions-changed)
      (refresh))

    (define/public (paint-item i v x y)
      (void))

    (define/override (compute-item-size v)
      (values 1 1 0))

    (define/override (on-paint)
      (super on-paint)
      
      (define-values (x y) (get-scrollable-pos))
      (define-values (cw ch) (get-client-size))
      (define-values (cx cy) (get-scrollable-canvas-start))

      (define items (get-visible-items y (+ y ch)))
      (for ([item (in-list items)])
        (match item
          [(vector c y v)
           (paint-item c v (+ (cursor-indent c) cx) (+ y cy))])))

    (define/override (locate-item x y [check-x? #f])
      (define-values (cx cy) (get-scrollable-canvas-start))
      (super locate-item (- x cx) (- y cy) check-x?))
    
    (super-new)))

(define tree-widget%
  (manual-scroll-mixin
   (tree-canvas-mixin
    (scrollable-mixin
     (tree-mixin
      canvas%)))))