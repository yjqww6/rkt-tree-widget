#lang racket/base
(require racket/class)
(provide (all-defined-out))

(define tree<%>
  (interface ()
    compute-item-size
    get-total-size
    locate-item
    get-visible-items
    get-root
    set-root
    on-positions-changed))

(define scrollable<%>
  (interface ()
    get-scrollable-size
    get-scrollable-pos
    get-scrollable-canvas-start))
