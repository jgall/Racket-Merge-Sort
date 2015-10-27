#lang racket

;; merge-lists : [List-of Number] -> [List-of Number]
;; merges two sorted lists with each other
(define (merge-lists lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [(< (first lon1) (first lon2)) (cons (first lon1)
                                             (merge-lists (rest lon1) lon2))]
        [else (cons (first lon2) (merge-lists lon1 (rest lon2)))]))

;; list->lol : [List-of X] -> [List-of [List-of X]]
;; converts every item of a list into an individual cons
(define (list->lol alist)
  (cond [(empty? alist) empty]
        [else (cons (cons (first alist) empty) (list->lol (rest alist)))]))

;; lol->list : [List-of [List-of X]] -> [List-of X]
;; converts a list of lists into a single list

;; merge-layer : [List-of [List-of Number]] -> [List-of [List-of Number]]
;; merges half of the lists in a list.
(define (merge-layer alist)
  (cond [(empty? alist) '()]
        [(empty? (rest alist)) alist]
        [else (cons (merge-lists (first alist)
                                 (first (rest alist)))
                    (merge-layer (rest (rest alist))))]))

;; merge-sort : [
;; merge sorts a list of lists
(define (merge-sort alist)
  (cond [(empty? alist) '()]
        [(empty? (rest alist)) (first alist)]
        [else (merge-sort (merge-layer alist))]))

;; sort-numbers : [List-of Number] -> [List-of Number]
;; sorts a list of numbers
(define (sort-numbers alon)
  (merge-sort (list->lol alon)))