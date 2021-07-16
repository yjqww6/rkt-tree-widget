#lang racket/base
(require "private/tree-widget.rkt" "private/interfaces.rkt"
         (rename-in "private/tree-pos-cache.rkt"
                    [tree-pos-cache? root-cursor?])
         racket/class racket/contract/base)

(define (cursor? t)
  (or (root-cursor? t) (node-cursor? t)))

(define (generic-cursor? t)
  (or (cursor? t) (indices-cursor? t)))

(provide
 root-cursor?
 node-cursor?
 indices-cursor?
 (all-defined-out)

 tree<%> 
 
 (contract-out
  [cursor-up (-> cursor? cursor?)]
  [cursor-equal? (-> cursor? cursor? boolean?)]
  [cursor-valid? (-> root-cursor? cursor? boolean?)]
  [cursor-children (-> cursor? (listof node-cursor?))]
  [cursor-children-cursor (-> cursor? root-cursor?)]
  [cursor-children-count (-> cursor? exact-nonnegative-integer?)]
  [cursor-get-child (-> cursor? exact-nonnegative-integer? node-cursor?)]
  [node-cursor-item-size (-> node-cursor? (values exact-positive-integer? exact-positive-integer?))]
  
  [node-cursor-value (-> node-cursor? any/c)]
  [node-cursor-expand? (-> node-cursor? boolean?)]
  [node-cursor-pos (-> node-cursor? exact-nonnegative-integer?)]
  [node-cursor-children-indent (-> node-cursor? exact-nonnegative-integer?)]

  [root-cursor-total-size (-> root-cursor? (values exact-nonnegative-integer? exact-nonnegative-integer?))]
  [root-cursor-locate-item (-> root-cursor? (or/c #f exact-nonnegative-integer?) exact-nonnegative-integer?
                               (or/c #f node-cursor?))]
  [root-cursor-get-visible-items (-> root-cursor? exact-nonnegative-integer? exact-nonnegative-integer?
                                     (listof (vector/c node-cursor? exact-nonnegative-integer? any/c)))]
  [tree-mixin mixin-contract]
  
  [tree-widget%
   (class/c
    (init [wheel-step exact-positive-integer?])
    
    [get-root (->m root-cursor?)]
    [set-root (->m root-cursor? void?)]

    [append-item (->*m (generic-cursor? any/c) (boolean?) void?)]
    [prepend-item (->*m (generic-cursor? any/c) (boolean?) void?)]
    [insert-item (->*m (generic-cursor? exact-nonnegative-integer? any/c) (boolean?) void?)]
    [update-item (->m generic-cursor? exact-nonnegative-integer? any/c void?)]
    [delete-item (->m generic-cursor? exact-nonnegative-integer? void?)]
    [expand-item (->m generic-cursor? boolean? void?)]
    [reset-items (->m void?)]
    [on-positions-changed (->m void?)]

    [paint-item (->m node-cursor? any/c exact-nonnegative-integer? exact-nonnegative-integer? void?)]
    [compute-item-size (->m any/c (values exact-positive-integer? exact-positive-integer? exact-nonnegative-integer?))]
    [locate-item (->m (or/c #f exact-nonnegative-integer?) exact-nonnegative-integer? (or/c #f node-cursor?))]

    [make-indices-cursor (->m (non-empty-listof exact-nonnegative-integer?) indices-cursor?)])]

  [tree-updater%
   (class/c
    (init-field [tree (instanceof/c (implementation?/c tree<%>))])

    [append-item (->*m (generic-cursor? any/c) (boolean? (or/c #f cursor?)) root-cursor?)]
    [prepend-item (->*m (generic-cursor? any/c) (boolean? (or/c #f cursor?)) root-cursor?)]
    [insert-item (->*m (generic-cursor? exact-nonnegative-integer? any/c) (boolean? (or/c #f cursor?)) root-cursor?)]
    [update-item (->m generic-cursor? exact-nonnegative-integer? any/c root-cursor?)]
    [delete-item (->m generic-cursor? exact-nonnegative-integer? root-cursor?)]
    [expand-item (->m generic-cursor? boolean? root-cursor?)]
    [update-children (->m generic-cursor? (-> root-cursor? root-cursor?) root-cursor?)]

    [set-tree (->m root-cursor? void?)]
    [empty-tree (->m root-cursor?)])]))