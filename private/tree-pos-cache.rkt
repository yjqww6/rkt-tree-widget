#lang racket/base
(require "pos-tree.rkt"
         racket/match)
(provide (all-defined-out))

(module tree racket/base
  (require "pos-tree-helper.rkt")

  (define (tree-total-height t)
    (if t
        (T-total-height t)
        0))

  (define (tree-max-width t)
    (if t
        (T-max-width t)
        0))

  (define (width-max value width height children expand? indent
                     l-max-width r-max-width)
    (max width l-max-width r-max-width
         (if expand? (+ indent (tree-max-width children)) 0)))

  (define (height-total value width height children expand? indent
                        l-total-height r-total-height)
    (+ height l-total-height r-total-height
       (if expand? (tree-total-height children) 0)))

  (define-pos-tree T (value width height children expand? indent)
    ([max-width 0 width-max]
     [total-height 0 height-total]))
  
  (provide (all-defined-out)))

(require 'tree)

(struct tree-pos-cache (root)
  #:authentic #:sealed)

(define empty-tree-pos-cache (tree-pos-cache #f))

(struct frame (node pos frames)
  #:authentic #:sealed)

(struct node-cursor (root node pos frames)
  #:authentic #:sealed)

(struct indices-cursor (root indices)
  #:authentic #:sealed)

(define (cursor-up t)
  (match t
    [(tree-pos-cache root) t]
    [(node-cursor root node pos #f) (tree-pos-cache root)]
    [(node-cursor root _ _ (frame node pos frames))
     (node-cursor root node pos frames)]))

(define (cursor-depth t)
  (match t
    [(tree-pos-cache _) 0]
    [(node-cursor _ _ _ frames)
     (let loop ([f frames] [l 1])
       (match f
         [#f l]
         [(frame _ _ f)
          (loop f (+ 1 l))]))]))

(define (cursor-indent t)
  (match t
    [(tree-pos-cache _) 0]
    [(node-cursor _ n _ frames)
     (let loop ([f frames] [i 0])
       (match f
         [#f i]
         [(frame n _ frames)
          (loop frames (+ i (T-indent n)))]))]))

(define (cursor-equal? a b)
  (match* (a b)
    [((tree-pos-cache a) (tree-pos-cache b)) (eq? a b)]
    [((node-cursor _ a _ _) (node-cursor _ b _ _)) (eq? a b)]
    [(_ _) #f]))

(define (cursor->indices t)
  (match t
    [(tree-pos-cache root) '()]
    [(node-cursor _ _ pos frames)
     (frames->indices frames pos)]))

(define (node-cursor-item-size t)
  (match t
    [(node-cursor _ n _ _)
     (values (T-width n) (T-height n))]))

(define (cursor-children t)
  (define (f node root frames)
    (let f ([t (T-children node)] [offset 0] [ls '()])
      (match t
        [#f ls]
        [(T l r _ _ _ _ _ _ _ _ _)
         (f l offset
            (cons
             (node-cursor root t (+ offset (tree-size l)) frames)
             (f r (+ offset (tree-size l) 1) ls)))])))
  (match t
    [(tree-pos-cache #f) '()]
    [(tree-pos-cache root) (f root root #f)]
    [(node-cursor root node pos frames)
     (f node root (frame node pos frames))]))

(define (cursor-children-cursor t)
  (match t
    [(tree-pos-cache _) t]
    [(node-cursor _ node _ _) (tree-pos-cache (T-children node))]))

(define (cursor-children-count t)
  (match t
    [(tree-pos-cache root) (tree-size root)]
    [(node-cursor _ node _ _) (tree-size (T-children node))]))

(define (frames->indices f pos)
  (let loop ([f f] [l (list pos)])
    (match f
      [#f l]
      [(frame _ pos f)
       (loop f (cons pos l))])))

(define (cursor-get-child t i)
  (match t
    [(tree-pos-cache root)
     (node-cursor root (get-node root i) i #f)]
    [(node-cursor root node pos frames)
     (node-cursor root (get-node (T-children node) i) i (frame node pos frames))]))

(define ((make-node-field-generator v e? c)) 
  (define-values (value width height ind) (v))
  (values value width height c e? ind))

(define (update-node-children t u)
  (define (f node indices)
    (let f ([node node] [indices indices])
      (match indices
        [(cons pos '())
         (update-item
          node pos
          (λ (t) 
            (match-define (T _ _ _ v w h c e? ind _ _) t)
            (values v w h (u c) e? ind)))]
        [(cons p indices)
         (update-item
          node p
          (λ (t)
            (match-define (T _ _ _ v w h c e? ind _ _) t)
            (values v w h (f c indices) e? ind)))])))
  (match t
    [(tree-pos-cache root)
     (tree-pos-cache (u root))]
    [(node-cursor root n pos frames)
     (tree-pos-cache
      (f root (frames->indices frames pos)))]
    [(indices-cursor root indices)
     (tree-pos-cache
      (f root indices))]))

(define (cursor-append-item t v e? c)
  (define (u t) (append-item t (make-node-field-generator v e? c)))
  (update-node-children t u))

(define (cursor-prepend-item t v e? c)
  (define (u t) (prepend-item t (make-node-field-generator v e? c)))
  (update-node-children t u))

(define (cursor-insert-item t i v e? c)
  (define (u t) (insert-item t i (make-node-field-generator v e? c)))
  (update-node-children t u))

(define (cursor-delete-item t i)
  (define (u t) (delete-item t i))
  (update-node-children t u))

(define (cursor-update-item t i v)
  (define (updater t)
    (define-values (value width height ind) (v))
    (values value width height (T-children t) (T-expand? t) ind))
  (define (u t) (update-item t i updater))
  (update-node-children t u))

(define (cursor-expand-item t b)
  (define (updater t)
    (match t
      [(T _ _ _ v w h c _ ind _ _)
       (values v w h c b ind)]))
  (match t
    [(tree-pos-cache root) t]
    [(or (node-cursor _ _ pos _) (indices-cursor _ (cons pos _)))
     (define c (cursor-up t))
     (define (u t) (update-item t pos updater))
     (update-node-children c u)]))

(define (cursor-update-children t u)
  (define (updater t)
    (match t
      [(T _ _ _ v w h c b ind _ _)
       (values v w h (tree-pos-cache-root (u (tree-pos-cache c))) b ind)]))
  (match t
    [(tree-pos-cache root) (u t)]
    [(node-cursor _ _ pos _)
     (define c (cursor-up t))
     (define (f t) (update-item t pos updater))
     (update-node-children c f)]))

(define (node-total-size node)
  (if node
      (values (T-max-width node)
              (T-total-height node))
      (values 0 0)))

(define (root-cursor-total-size t)
  (node-total-size (tree-pos-cache-root t)))

(define (root-cursor-locate-item t x y)
  (define root (tree-pos-cache-root t))
  (let ([x (or x 0)]
        [check-x? (and x #t)])
    (let f ([t root] [y y] [offset 0] [x x] [frames #f])
      (match t
        [(T l r _ _ w h c e? ind mw th)
         #:when (or (not check-x?)
                    (and (< x mw) (< y th)))
         (define thl (tree-total-height l))
         (define thr (tree-total-height r))
         (define thc (- th h thl thr))
         (cond
           [(< y thl) (f l y offset x frames)]
           [(< y (+ thl h))
            (if (or (not check-x?) (and (>= x 0) (< x w)))
                (node-cursor root t (+ offset (tree-size l)) frames)
                #f)]
           [(< y (+ thl h thc))
            (f c (- y thl h) 0 (- x ind) (frame t (+ offset (tree-size l)) frames))]
           [else (f r (- y (- th thr)) (+ offset (tree-size l) 1) x frames)])]
        [else #f]))))

(define (root-cursor-get-visible-items t start end)
  (define root (tree-pos-cache-root t))
  (let f ([t root] [start start] [end end] [offset 0] [y-offset 0] [frames #f] [ls '()])
    (match t
      [#f ls]
      [(T l r _ v _ h c e? _ _ th)
       (define thl (tree-total-height l))
       (define thr (tree-total-height r))
       (define thc (- th h thl thr))
           
       (define new-r
         (let ([a (+ thl h thc)])
           (cond
             [(and (< start th) (< a end))
              (f r (max 0 (- start a)) (min thr (- end a))
                 (+ offset (tree-size l) 1) (+ y-offset a) frames
                 ls)]
             [else ls])))

       (define new-c
         (let ([a (+ thl h)])
           (cond
             [(and e?
                   (< start (+ a thc))
                   (< a end))
              (f c (max 0 (- start a)) (min thc (- end a))
                 0 (+ y-offset a) (frame t (+ offset (tree-size l)) frames) new-r)]
             [else new-r])))

       (define new-m
         (cond
           [(and (< start (+ thl h))
                 (< thl end))
            (cons (vector (node-cursor root t (+ offset (tree-size l)) frames) (+ y-offset thl) v)
                  new-c)]
           [else new-c]))

       (define new-l
         (cond
           [(and (< start thl) (< 0 end))
            (f l start (min thl end)
               offset y-offset frames new-m)]
           [else new-m]))
       new-l])))

(define (cursor-valid? t c)
  (match* (t c)
    [((tree-pos-cache a) (tree-pos-cache b)) (eq? a b)]
    [((tree-pos-cache a) (node-cursor b _ _ _)) (eq? a b)]
    [((tree-pos-cache a) (indices-cursor b _)) (eq? a b)]
    [(_ _) #f]))

(define (cursor-node t)
  (match t
    [(tree-pos-cache root) root]
    [(node-cursor _ node _ _) node]))

(define (node-cursor-value t)
  (T-value (node-cursor-node t)))

(define (node-cursor-expand? t)
  (T-expand? (node-cursor-node t)))

(define (node-cursor-children-indent t)
  (T-indent (node-cursor-node t)))

(define (tree-pos-cache-make-indices-cursor t l)
  (indices-cursor (tree-pos-cache-root t) l))

(module+ debug
  (define (unwrap-tree-pos-cache t)
    (define node
      (match t
        [(tree-pos-cache root) root]
        [else (node-cursor-node t)]))
    (let f ([t node] [ls '()])
      (match t
        [#f ls]
        [(T l r _ v _ _ c e? _ _ _)
         (cond
           [(and e? c)
            (f l (cons (cons v (cons ': (f c '()))) (f r ls)))]
           [else
            (f l (cons v (f r ls)))])])))
  (provide (all-defined-out)))
