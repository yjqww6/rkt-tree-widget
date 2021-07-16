#lang racket/base
(require "interfaces.rkt" "tree-pos-cache.rkt"
         racket/class)

(provide (all-defined-out))

(define tree-mixin
  (mixin () (tree<%>)
    (define cache empty-tree-pos-cache)

    (define/public (compute-item-size v)
      (values 1 1 0))

    (define/public (compute-indent v w h)
      w)

    (define/public (get-root)
      cache)

    (define/public (set-root t)
      (set! cache t)
      (on-positions-changed))

    (define/public (on-positions-changed)
      (void))
    
    (super-new)))

(define tree-updater%
  (class object%
    (init-field tree)
    (super-new)

    (define-syntax-rule (define-op (name t a ... v) op)
      (define/public (name t a ... v [expand? #f] [children #f])
        (define-values (w h ind) (send tree compute-item-size v))
        (op t a ... (λ () (values v w h ind)) expand? (and children (cursor-node children)))))

    (define-op (append-item t v) cursor-append-item)
    (define-op (prepend-item t v) cursor-prepend-item)
    (define-op (insert-item t i v) cursor-insert-item)

    (define/public (update-item t i v)
      (define-values (w h ind) (send tree compute-item-size v))
      (cursor-update-item t i (λ () (values v w h ind))))

    (define/public (delete-item t i)
      (cursor-delete-item t i))

    (define/public (expand-item t b?)
      (cursor-expand-item t b?))

    (define/public (update-children t u)
      (cursor-update-children t u))

    (define/public (set-tree t)
      (send tree set-root t))

    (define/public (empty-tree)
      empty-tree-pos-cache)))
