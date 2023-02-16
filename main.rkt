#lang racket/base

(require racket/cmdline
         racket/match
         racket/list
         racket/format
         racket/hash
         racket/string
         net/http-easy
         xml/path
         text-table)

(define (->string v)
  (cond
    [(string? v) v]
    [(number? v) (string (integer->char v))]
    [else (~a v)]))

(define (get-page rev subpage label)
  (define res (get (format "http://drdr.racket-lang.org/~a/~a" rev subpage)))
  (for/hash ([row (se-path*/list '(tbody) (response-xexpr res))])
    (match row
      [`(tr ([class "dir"] ,_ ...)
            (td ,_ (a ,_ ,name))
            (td ([sorttable_customkey ,v]) ,v-readable)
            ...
            ,_)
       (values name (entry 'dir label (map string->number v) (map ->string v-readable)))
       ]
      [`(tr ([class "file"] ,_ ...)
            (td ,_ (a ,_ ,name))
            (td ([sorttable_customkey ,v]) ,v-readable)
            ...
            ,_)
       (values name (entry 'file label (map string->number v) (map ->string v-readable)))])))

(struct diffed (diff a b) #:transparent)
(struct entry (type label vs vs-readable) #:transparent)

(define (entry<? a b)
  (match* (a b)
    [((cons _ (diffed x _ _)) (cons _ (diffed y _ _))) (< x y)]
    [((cons _ (diffed x _ _)) _) #t]
    [(_ (cons _ (diffed x _ _))) #f]
    [((cons ka _) (cons kb _)) (string<? ka kb)]))

(define (print-it h)
  (print-table
   (cons
    (list "Name" "Stat" "Type" "Details")
    (for/list ([row (sort (hash->list h) entry<?)])
      (match row
        [(cons k (diffed diff (entry type _ _ va-readable) (entry _ _ _ vb-readable)))
         (list k
               (format "diff: ~as" (~r (/ diff 1000) #:precision 3))
               type
               (format "~a ~a" va-readable vb-readable))]
        [(cons k (entry type label _ v-readable))
         (list k
               (format "only ~a" label)
               type
               v-readable)])))))

(module+ main
  (define-values (rev-a rev-b)
    (command-line
     #:args (rev-a rev-b)
     (values rev-a rev-b)))

  (let loop ([pages '()])
    (define subpage (string-append* (reverse pages)))
    (printf "subpage: ~a\n" subpage)
    (define ha (get-page rev-a subpage 'old))
    (define hb (get-page rev-b subpage 'new))
    (print-it
     (hash-union ha hb
                 #:combine
                 (λ (va vb)
                   (match-define (entry _ _ (list _ sum-a _ ...) _) va)
                   (match-define (entry _ _ (list _ sum-b _ ...) _) vb)
                   (diffed (- sum-a sum-b) va vb))))
    (printf ">>> ")
    (define in (read-line))
    (match (string-trim in)
      ["!quit" (void)]
      [".." (loop (rest pages))]
      [s (loop (cons s pages))])))
