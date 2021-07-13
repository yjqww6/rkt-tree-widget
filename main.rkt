#lang racket/base
(require "private/tree-widget.rkt"
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
 (contract-out
  [cursor-up (-> cursor? cursor?)]
  [cursor-equal? (-> cursor? cursor? boolean?)]
  [cursor-valid? (-> root-cursor? cursor? boolean?)]
  [cursor-children (-> cursor? (listof node-cursor?))]
  [cursor-get-child (-> cursor? exact-nonnegative-integer? node-cursor?)]
  [node-cursor-item-size (-> node-cursor? (values exact-nonnegative-integer? exact-nonnegative-integer?))]
  [node-cursor-value (-> node-cursor? any/c)]
  [node-cursor-expand? (-> node-cursor? boolean?)]
  
  [tree-widget%
   (class/c
    [get-root (->m root-cursor?)]

    [append-item (->*m (generic-cursor? any/c) (boolean?) void?)]
    [prepend-item (->*m (generic-cursor? any/c) (boolean?) void?)]
    [insert-item (->*m (generic-cursor? exact-nonnegative-integer? any/c) (boolean?) void?)]
    [update-item (->*m (generic-cursor? exact-nonnegative-integer? any/c) (boolean?) void?)]
    [delete-item (->m generic-cursor? exact-nonnegative-integer? void?)]
    [expand-item (->m generic-cursor? boolean? void?)]
    [reset-items (->m void?)]

    [paint-item (->m node-cursor? any/c exact-nonnegative-integer? exact-nonnegative-integer? void?)]
    [compute-item-size (->m any/c (values exact-positive-integer? exact-positive-integer? exact-nonnegative-integer?))]
    [locate-item (->*m (exact-nonnegative-integer? exact-nonnegative-integer?) (boolean?) (or/c #f node-cursor?))]

    [make-indices-cursor (->m (non-empty-listof exact-nonnegative-integer?) indices-cursor?)])]))