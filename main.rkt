#lang racket/base
(require "private/tree-widget.rkt" "base.rkt"
         racket/class racket/contract/base)


(provide
 (all-from-out "base.rkt")
 
 (contract-out
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

    [make-indices-cursor (->m (non-empty-listof exact-nonnegative-integer?) indices-cursor?)])]))