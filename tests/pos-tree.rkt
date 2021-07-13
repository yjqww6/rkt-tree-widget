#lang racket/base
(require "../private/pos-tree.rkt"
         racket/match racket/unit)

(struct ValueNode Node (value) #:authentic #:transparent)
(struct Red ValueNode () #:authentic #:sealed #:transparent)
(struct Black ValueNode () #:authentic #:sealed #:transparent)

(define red? Red?)
(define black? Black?)

(define (red l r t)
  (Red l r (new-node-size l r) (ValueNode-value t)))

(define (red-leaf v)
  (Red #f #f 1 v))

(define (black l r t)
  (Black l r (new-node-size l r) (ValueNode-value t)))

(define (coloring color t)
  (if color
      (match t
        [(Black l r k v) (Red l r k v)]
        [else t])
      (match t
        [(Red l r k v) (Black l r k v)]
        [else t])))

(define (update-node node v)
  (match node
    [(Red l r k _)
     (Red l r k v)]
    [(Black l r k _)
     (Black l r k v)]
    [_ #f]))
  
(define-values/invoke-unit/infer pos-tree-op@)

(define (tree->list t)
  (let f ([t t] [ls '()])
    (match t
      [#f ls]
      [(ValueNode l r _ v)
       (f l (cons v (f r ls)))])))

(define (height t)
  (let f ([t t])
    (match t
      [#f 0]
      [(ValueNode l r _ _)
       (+ 1 (max (f l) (f r)))])))

(define (verify-red t)
  (let f ([t t] [depth 0])
    (match t
      [#f (void)]
      [(or (Red (Red _ _ _ _) _ _ _) (Red _ (Red _ _ _ _) _ _))
       (error 'verify-red "nested red nodes ~s at depth ~s" t depth)]
      [(Node l r _)
       (f l (add1 depth))
       (f r (add1 depth))])))

(define (verify-black t)
  (let f ([t t])
    (match t
      [#f 0]
      [(Node (app f l) (app f r) _)
       (unless (= l r)
         (error 'verify-black "unbalanced"))
       (if (Black? t)
           (add1 l)
           l)])))

(module+ test
  (require rackunit racket/list)

  (define-syntax-rule (check-tree t)
    (let ([x t])
      (check-not-exn
       (λ ()
         (verify-black t)
         (verify-red t)))))

  (define t
    (for/fold ([t #f])
              ([i (in-range 100)])
      (append-item t i)))

  (check-tree t)
  (check-equal? (tree->list t) (range 100))
  (check-equal? (ValueNode-value (get-node t 10)) 10)
  (check-equal? (map ValueNode-value (get-nodes t 10 20)) (range 10 20))


  (define p
    (for/fold ([t #f])
              ([i (in-range 100)])
      (prepend-item t i)))

  (check-tree p)
  (check-equal? (tree->list p) (range 99 -1 -1))

  (define it
    (for/fold ([t t])
              ([i (in-range 10)])
      (insert-item t 10 i)))

  (check-tree it)
  (check-equal? (tree->list it)
                (append (range 0 10)
                        (range 9 -1 -1)
                        (range 10 100)))

  (define de
    (for/fold ([t t])
              ([_ (in-range 10)])
      (delete-item t 10)))

  (check-tree de)
  (check-equal? (tree->list de)
                (append (range 0 10) (range 20 100)))
  )
