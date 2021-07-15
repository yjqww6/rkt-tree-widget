#lang racket/base
(require "tree-pos-cache.rkt" "interfaces.rkt"
         "manual-scroll.rkt"
         racket/match racket/class racket/gui/base)

(provide (all-defined-out))

(define tree-mixin
  (mixin () (tree<%>)
    (define cache empty-tree-pos-cache)

    (define/public (compute-item-size v)
      (values 1 1 0))

    (define/public (compute-indent v w h)
      w)

    (define/public (get-total-size cw ch)
      (tree-pos-cache-total-size cache))

    (define/public (locate-item x y)
      (tree-pos-cache-locate-item cache (or x 0) y (and x #t)))

    (define/public (get-visible-items start end)
      (tree-pos-cache-get-visible-items cache start end))

    (define/public (get-root)
      cache)

    (define/public (set-root t)
      (set! cache t)
      (on-positions-changed))

    (define/public (on-positions-changed)
      (void))
    
    (super-new)))

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

    (define-op (append-item t v) tree-pos-cache-append)
    (define-op (prepend-item t v) tree-pos-cache-prepend)
    (define-op (insert-item t i v) tree-pos-cache-insert)

    (define/public (update-item t i v)
      (validate-cursor 'update-item t)
      (define-values (w h ind) (compute-item-size v))
      (set-root (tree-pos-cache-update t i (λ () (values v w h ind)))))

    (define/public (delete-item t i)
      (validate-cursor 'delete-item t)
      (set-root (tree-pos-cache-delete t i)))

    (define/public (expand-item t b?)
      (validate-cursor 'expand-item t)
      (set-root (tree-pos-cache-expand t b?)))

    (define/public (reset-items)
      (set-root empty-tree-pos-cache))

    (define/public (make-indices-cursor indices)
      (tree-pos-cache-make-indices-cursor (get-root) indices))

    (super-new)))

(define tree-updater%
  (class object%
    (init-field tree)
    (super-new)

    (define-syntax-rule (define-op (name t a ... v) op)
      (define/public (name t a ... v [expand? #f] [children #f])
        (define-values (w h ind) (send tree compute-item-size v))
        (op t a ... (λ () (values v w h ind)) expand? (and children (cursor-node children)))))

    (define-op (append-item t v) tree-pos-cache-append)
    (define-op (prepend-item t v) tree-pos-cache-prepend)
    (define-op (insert-item t i v) tree-pos-cache-insert)

    (define/public (update-item t i v)
      (define-values (w h ind) (send tree compute-item-size v))
      (tree-pos-cache-update t i (λ () (values v w h ind))))

    (define/public (delete-item t i)
      (tree-pos-cache-delete t i))

    (define/public (expand-item t b?)
      (tree-pos-cache-expand t b?))

    (define/public (update-children t u)
      (tree-pos-cache-update-children t u))

    (define/public (set-tree t)
      (send tree set-root t))

    (define/public (empty-tree)
      empty-tree-pos-cache)))

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
  (mixin ((class->interface canvas%) tree<%> scrollable<%>) ()
    (inherit refresh
             get-scrollable-pos get-client-size
             get-scrollable-canvas-start
             get-visible-items)

    (define/override (on-positions-changed)
      (refresh))

    (define/public (paint-item i v x y)
      (void))

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

    (define/override (locate-item x y)
      (define-values (cx cy) (get-scrollable-canvas-start))
      (super locate-item (and x (- x cx)) (- y cy)))
    
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
         (let f ([i 4])
           (cond
             [(= i 0) (send u empty-tree)]
             [else
              (for/fold ([t (send u empty-tree)])
                        ([x (in-range 10)])
                (send u append-item t x #t (f (sub1 i))))])))))