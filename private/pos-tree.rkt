#lang racket/base
(require racket/unit racket/fixnum racket/match syntax/parse/define
         (for-syntax racket/base))
(provide (all-defined-out))

(struct Node (left right size)
  #:authentic #:transparent)

(define (tree-size t)
  (cond
    [(not t) 0]
    [else (Node-size t)]))

(define (new-node-size l r)
  (fx+ 1 (tree-size l) (tree-size r)))


(define-syntax-parser case-size
  [(_ (L:expr [P:id p:id])
      (~or*
       (~seq
        [(~literal <) A:expr ...]
        [(~literal =) B:expr ...])
       [(~literal <=) A:expr ...])
      [(~literal >) C:expr ...])
   #'(let ()
       (define sl (fx- p (tree-size L)))
       (cond
         (~?
          (~@
           [(fx< sl 0) (let ([P p]) A ...)]
           [(fx= sl 0) (let ([P p]) B ...)])
          [(fx<= sl 0) (let ([P p]) A ...)])
         [else (let ([P (fx- sl 1)])
                 C) ...]))])

(define (get-node t p)
  (let f ([t t] [p p])
    (match t
      [#f (error 'get-item "out of range")]
      [(Node l r kx)
       (case-size
        (l [p p])
        [< (f l p)]
        [= t]
        [> (f r p)])])))

(define (get-nodes t [start 0] [end (tree-size t)])
  (unless (fx<= end (tree-size t))
    (error 'get-items "out of range"))
  (unless (fx<= start end)
    (error 'get-items "start greater than end"))
  
  (let f ([t t] [start start] [end end] [ls '()])
    (match t
      [#f ls]
      [(Node l r k)
       (cond
         [(fx>= start k) ls]
         [else
          (define kl (tree-size l))
          (define r-start (fx- start (fx+ 1 kl)))
          (define r-end (fx- end (fx+ 1 kl)))
          
          (define new-r
            (cond
              [(fx> r-end 0)
               (f r (fxmax 0 r-start) r-end ls)]
              [else ls]))

          (define new-vr
            (cond
              [(and (fx<= start kl)
                    (fx< kl end))
               (cons t new-r)]
              [else new-r]))

          (cond
            [(fx< start kl)
             (f l start (fxmin kl end) new-vr)]
            [else new-vr])])])))

(define-signature pos-tree^
  (red? black? red black red-leaf coloring update-node))

(define-signature pos-tree-op^
  (pos-tree? insert-item delete-item
             append-item prepend-item
             update-item))

(define-unit pos-tree-op@
  (import pos-tree^)
  (export pos-tree-op^)

  (define-match-expander Red
    (λ (stx)
      (syntax-case stx ()
        [(_ l r k) #'(and (Node l r k) (? red?))])))

  (define-match-expander Black
    (λ (stx)
      (syntax-case stx ()
        [(_ l r k) #'(and (Node l r k) (? black?))])))

  (define (pos-tree? t)
    (or (not t) (Node? t)))

  (define (node color left right t)
    (if color
        (red left right t)
        (black left right t)))

  (define (as-red t)
    (coloring #t t))

  (define (as-black t)
    (coloring #f t))

  (define (balance-left l r t)
    (match l
      [#f #f]
      [(Node (and vx (Red lx rx _)) ry _)
       (red (black lx rx vx) (black ry r t) l)]
      [(Node ly (and vx (Red lx rx _)) _)
       (red (black ly lx l) (black rx r t) vx)]
      [(Node lx rx _)
       (black (red lx rx l) r t)]))

  (define (balance-right l r t)
    (match r
      [#f #f]
      [(Node (and vx (Red lx rx _)) ry _)
       (red (black l lx t) (black rx ry r) vx)]
      [(Node lx (and vy (Red ly ry _)) _)
       (red (black l lx t) (black ly ry vy) r)]
      [(Node lx rx _)
       (black l (red lx rx r) t)]))

  (define (prepend-item t v)
    (as-black
     (let f ([t t])
       (match t
         [#f (red-leaf v)]

         [(Red l r _)
          (red (f l) r t)]

         [(Node l r _)
          (if (red? l)
              (balance-left (f l) r t)
              (black (f l) r t))]))))

  (define (append-item t v)
    (as-black
     (let f ([t t])
       (match t
         [#f (red-leaf v)]

         [(Red l r _)
          (red l (f r) t)]

         [(Node l r _)
          (if (red? r)
              (balance-right l (f r) t)
              (black l (f r) t))]))))

  (define (update-item t p v)
    (match t
      [#f
       (error 'update-item "out of range")]
    
      [(Node l r kx)
       (define c (red? t))
       (case-size
        [l (p p)]
        [< (node c (update-item l p v) r t)]
        [= (update-node t v)]
        [> (node c l (update-item r p v) t)])]))

  (define (insert-item t p v)
    (unless (fx< p (tree-size t))
      (error 'insert-item "out of range"))
    (as-black
     (let f ([t t] [p p])
       (match t
         [#f
          (red-leaf v)]
    
         [(Red l r kx)
          (case-size
           (l [p p])
           [<= (red (f l p) r t)]
           [> (red l (r f p) t)])]

         [(Node l r kx)
          (case-size
           (l [p p])
           [<= (if (red? l)
                   (balance-left (f l p) r t)
                   (black (f l p) r t))]
           [> (if (red? r)
                  (balance-right l (f r p) t)
                  (black l (f r p) t))])]))))

  ;;; deletion

  (struct Del (tree balanced) #:authentic)

  (define (make-black t)
    (match t
      [(Red l r _) (Del (black l r t) #f)]
      [else (Del t #t)]))

  (define (rebalance-left c l r t)
    (match l
      [(Black _ _ _) (Del (balance-left (as-red l) r t) (not c))]
      [(Red lx rx _) (Del (black lx (balance-left (as-red rx) r t) l) #f)]
      [else (Del (black l r t) #f)]))

  (define (rebalance-right c l r t)
    (match r
      [(Black _ _ _) (Del (balance-right l (as-red r) t) (not c))]
      [(Red lx rx _) (Del (black (balance-right l (as-red lx) t) rx r) #f)]
      [else (Del (black l r t) #f)]))

  (struct Delmin (del min) #:authentic)

  (define (delete-min t)
    (match (del-min t)
      [(Delmin (Del t _) _) (as-black t)]))

  (define (del-min t)
    (match t
      [(Black #f r _)
       (match r
         [#f (Delmin (Del #f #t) t)]
         [_ (Delmin (Del (as-black r) #f) t)])]
      [(Red #f r _)
       (Delmin (Del r #f) t)]
      [(Node l r _)
       (match (del-min l)
         [(Delmin (Del lx #t) tx)
          (Delmin (rebalance-right (red? t) lx r t) tx)]
         [(Delmin (Del lx #f) tx)
          (Delmin (Del (node (red? t) lx r t) #f) tx)])]
      [_ (error 'del-min "unreachable")]))

  (define (del t p)
    (match t
      [#f (Del #f #f)]
      [(Node lx rx _)
       (case-size
        (lx [p p])
        [<
         (match (del lx p)
           [(Del ly #t) (rebalance-right (red? t) ly rx t)]
           [(Del ly #f) (Del (node (red? t) ly rx t) #f)])]
        [=
         (match rx
           [#f
            (if (black? t)
                (make-black lx)
                (Del lx #f))]
           [_ (match (del-min rx)
                [(Delmin (Del ry #t) ty)
                 (rebalance-left (red? t) lx ry ty)]
                [(Delmin (Del ry #f) ty)
                 (Del (node (red? t) lx ry ty) #f)])])]
        [>
         (match (del rx p)
           [(Del ry #t) (rebalance-left (red? t) lx ry t)]
           [(Del ry #f) (Del (node (red? t) lx ry t) #f)])])]))

  (define (delete-item t p)
    (match (del t p)
      [(Del tx _) (as-black tx)]))
  )
