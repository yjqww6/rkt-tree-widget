#lang racket/base
(require racket/class)
(provide (all-defined-out))

(define tree-view<%>
  (interface ()
    compute-item-size
    get-total-size
    locate-item
    get-visible-items
    on-positions-changed))

(define scrollable<%>
  (interface ()
    get-scrollable-size
    get-scrollable-pos
    get-scrollable-canvas-start))
