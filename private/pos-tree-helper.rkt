#lang racket/base
(require "pos-tree.rkt"
         syntax/parse/define racket/match racket/unit
         (for-syntax racket/base racket/unit-exptime))

(provide (all-defined-out))

(define-syntax (define-pos-tree stx)
  (syntax-parse stx
    [(_ N:id (Field:id ...)
        ([Augment:id Init:expr Acc:id] ...))
     #:with (l-fields ...) (generate-temporaries #'(Field ...))
     #:with (r-fields ...) (generate-temporaries #'(Field ...))
     #:with (l-augs ...) (generate-temporaries #'(Augment ...))
     #:with (r-augs ...) (generate-temporaries #'(Augment ...))
     #:do
     [(define ops
        (let-values ([(e ls d s)
                      (signature-members #'pos-tree-op^ stx)])
          ls))]
     #:with (defined-ops ...) ops
     #:with (provided ...) (for/list ([op (in-list ops)])
                             (datum->syntax #'N (syntax-e op)))
     #'(begin
         (struct N Node (Field ... Augment ...)
           #:authentic #:transparent)
         (struct Red N () #:authentic #:transparent)
         (struct Black N () #:authentic #:transparent)

         (define red? Red?)
         (define black? Black?)

         (define-syntax-rule (make C l r Field ...)
           (match* (l r)
             [((N _ _ _ l-fields ... l-augs ...)
               (N _ _ _ r-fields ... r-augs ...))
              (C l r (new-node-size l r) Field ...
                 (Acc Field ... l-augs r-augs)
                 ...)]
             [(#f (N _ _ _ r-fields ... r-augs ...))
              (C l r (new-node-size l r) Field ...
                 (Acc Field ... Init r-augs)
                 ...)]
             [((N _ _ _ l-fields ... l-augs ...) #f)
              (C l r (new-node-size l r) Field ...
                 (Acc Field ... l-augs Init)
                 ...)]
             [(#f #f)
              (C l r (new-node-size l r) Field ...
                 (Acc Field ... Init Init)
                 ...)]))

         (define (make-red l r Field ...)
           (make Red l r Field ...))

         (define (make-black l r Field ...)
           (make Black l r Field ...))

         (define-syntax-rule (define-maker name maker)
           (define (name l r t)
             (match t
               [(N _ _ _ Field ... Augment ...)
                (maker l r Field ...)])))
       
         (define-maker red make-red)
         (define-maker black make-black)

         (define (coloring color t)
           (if color
               (match t
                 [(Black l r k Field ... Augment ...)
                  (Red l r k Field ... Augment ...)]
                 [else t])
               (match t
                 [(Red l r k Field ... Augment ...)
                  (Black l r k Field ... Augment ...)]
                 [else t])))

         (define (update-node node v)
           (let-values ([(Field ...) (v node)])
             (match node
               [(Red l r k l-fields ... l-augs ...)
                (make-red l r Field ...)]
               [(Black l r k l-fields ... l-augs ...)
                (make-black l r Field ...)]
               [_ #f])))

         (define (red-leaf v)
           (let-values ([(Field ...) (v)])
             (make-red #f #f Field ...)))

         (define-values/invoke-unit/infer pos-tree-op@)
         (define provided defined-ops) ...
         )]))
