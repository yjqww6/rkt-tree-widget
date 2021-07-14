#lang racket/base

(require "../private/tree-pos-cache.rkt"
         (submod "../private/tree-pos-cache.rkt" tree)
         (submod "../private/tree-pos-cache.rkt" debug)
         racket/match racket/list)

(module+ test
  (require rackunit)

  (define t0
    (for/fold ([t empty-tree-pos-cache])
              ([i (in-range 10)])
      (tree-pos-cache-append t (λ () (values i 1 1 0)) #t #f)))

  (define t1
    (for*/fold ([t t0])
               ([i (in-list '(2 4 6))]
                [p (in-range 10)])
      (tree-pos-cache-append (cursor-get-child t i)
                             (λ () (values p 1 1 0)) #t #f)))

  (define t2
    (for/fold ([t t1])
              ([i (in-list '(2))])
      (tree-pos-cache-expand (cursor-get-child t i) #f)))

  (check-equal? (unwrap-tree-pos-cache t2)
                '(0 1 2 3 (4 : 0 1 2 3 4 5 6 7 8 9) 5 (6 : 0 1 2 3 4 5 6 7 8 9) 7 8 9))

  (let-values ([(w h) (tree-pos-cache-total-size t2)])
    (check-equal? (cons w h) (cons 1 30)))
  
  (check-equal? (node-cursor-value (tree-pos-cache-locate-item t2 0 9))
                4)

  (check-equal?
   (for/list ([p (in-list (tree-pos-cache-get-visible-items t2 0 20))])
     (match-define (vector c y v) p)
     v)
   '(0 1 2 3 4 0 1 2 3 4 5 6 7 8 9 5 6 0 1 2))

  (define t3
    (tree-pos-cache-append (indices-cursor (tree-pos-cache-root t2) '(4 0))
                           #;(cursor-get-child (cursor-get-child t2 4) 0)
                           (λ () (values 100 1 1 0))
                           #t #f))
  (check-equal? (unwrap-tree-pos-cache t3)
                '(0 1 2 3 (4 : (0 : 100) 1 2 3 4 5 6 7 8 9) 5 (6 : 0 1 2 3 4 5 6 7 8 9) 7 8 9))
  
  (check-equal? (map node-cursor-value (cursor-children (cursor-get-child t3 4)))
                (range 10))
  (check-equal? (map node-cursor-value (cursor-children (cursor-get-child (cursor-get-child t3 4) 0)))
                '(100))

  (let ()
    (define t
      (for/fold ([t empty-tree-pos-cache])
                ([i (in-range 3)])
        (define new-t (tree-pos-cache-append t (λ () (values i 1 1 0)) #t #f))
        (tree-pos-cache-update-children
         (cursor-get-child new-t i)
         (λ (t)
           (for/fold ([t t])
                     ([i (in-range 10)])
             (tree-pos-cache-append t (λ () (values i 1 1 0)) #t #f))))))
    (check-equal?
     (unwrap-tree-pos-cache t)
     '((0 : 0 1 2 3 4 5 6 7 8 9) (1 : 0 1 2 3 4 5 6 7 8 9) (2 : 0 1 2 3 4 5 6 7 8 9)))))
