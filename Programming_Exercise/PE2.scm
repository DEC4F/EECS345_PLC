#|
Programming Exercise 2 for EECS 345 PLC
Stanley
|#

; 1. insert takes a number and a list of numbers in order and inserts the number in the proper place. 
; (equal? (insert 7 '(1 4 5 6 9 10)) '(1 4 5 6 7 9 10))
(define insert 
  (lambda (n lst)
    (insert-cps n lst (lambda (v) v)) ))

(define insert-cps
  (lambda (n lst return)
    (if (> (car lst) n)
      (return (cons n lst))
      (insert-cps n (cdr lst) (lambda (v) (return (cons (car lst) v)))) )))

; 2. merge takes two lists of numbers that are in order and returns a list that contains the combination of both lists in order. 
; (equal? (merge '(3 5 6 7 9) '(0 1 2 4 6 8 9 10)) '(0 1 2 3 4 5 6 6 7 8 9 9 10))
(define merge
  (lambda (lst1 lst2)
    (merge-cps lst1 lst2 (lambda (v) v)) ))

(define merge-cps
  (lambda (lst1 lst2 return)
    (cond 
      ((null? lst1) (return lst2))
      ((null? lst2) (return lst1))
      ((< (car lst2) (car lst1)) (merge-cps lst1 (cdr lst2) (lambda (v1) (return (cons (car lst2) v1)))))
      (else (merge-cps (cdr lst1) lst2 (lambda (v2) (return (cons (car lst1) v2))))) )))

; 3. removedups takes a list of atoms and removes any atom that is a repeat of the atom that immediately precedes it. 
; (equal? (removedups '(a a b b b c c a b b)) '(a b c a b))
(define removedups
  (lambda (lst)
    (removedups-cps lst (lambda (v) v)) ))

(define removedups-cps
    (lambda (lst return)
    (cond 
      ((null? (cdr lst)) (return lst))
      ((eq? (car lst) (cadr lst)) (removedups-cps (cdr lst) (lambda (v1) (return v1))))
      (else (removedups-cps (cdr lst) (lambda (v2) (return (cons (car lst) v2))))))))

; 4. numparens takes a list and returns the number of pairs of parentheses 
; (equal? (numparens '(1 2 3)) 1)
; (equal? (numparens '(1 () (()) (2 3 (4)))) 6)
(define numparens
  (lambda (lst)
    (numparens-cps lst (lambda (v) v)) ))

(define numparens-cps
  (lambda (lst return)
    (cond 
      ((null? lst) (return 1))
      ((list? (car lst)) (numparens-cps (car lst) (lambda (v1) (numparens-cps (cdr lst) (lambda (v2) (return (+ v1 v2)))))))
      (else (numparens-cps (cdr lst) return)) )))

; 5. dup* takes a list and duplicates all contents, including any sublists 
; (equal? (dup* '(1 2 (3 4) 5)) '(1 1 2 2 (3 3 4 4) (3 3 4 4) 5 5))
(define dup*
  (lambda (lst)
    (dup*-cps lst (lambda (v) v)) ))

(define dup*-cps
  (lambda (lst return)
    (cond 
      ((null? lst) 
        (return '()))
      ((list? (car lst))
        (dup*-cps (car lst) (lambda (v1) (dup*-cps (car lst) (lambda (v2) (dup*-cps (cdr lst) (lambda (v3) (return (cons v1 (cons v2 v3))))))))))
      (else 
        (dup*-cps (cdr lst) (lambda (v) (return (cons (car lst) (cons (car lst) v)))))) )))

; 6. removedups* takes a list, that can contain sublists, and removes any atom that is the repeat of the atom that immediately precedes it in the same sublist. 
; (equal? (removedups* '(a a (b b b (d d) b ((d) d)) f (f f g))) '(a (b (d) b ((d) d)) f (f g)))
(define removedups*
  (lambda (lst) 
    (removedups*-cps lst (lambda (v) v)) ))

(define removedups*-cps
  (lambda (lst return) 
    (cond 
      ((null? lst) (return '()))
      ((list? (car lst)) (removedups*-cps (car lst) (lambda (v1) (removedups*-cps (cdr lst) (lambda (v2) (return (cons v1 v2)))))))
      ((null? (cdr lst)) (return lst))
      ((eq? (car lst) (cadr lst)) (removedups*-cps (cdr lst) return))
      (else (removedups*-cps (cdr lst) (lambda (v) (return (cons (car lst) v))))) )))

; 7. mergesort takes a list of numbers and returns a sorted version. If you recall the merge sort algorithm, you use the CPS version of split from lecture to divide the input list into two lists, you recursively call mergesort on each sublist, and then you call merge on the two lists returned by the recursive calls to mergesort. 
(equal? (mergesort '())'())
(equal? (mergesort '(8 1 3 9 6 5 7 2 4 10)) '(1 2 3 4 5 6 7 8 9 10))

(define mergesort
  (lambda (lst)
    (mergesort-cps lst (lambda (v) v))))

(define mergesort-cps
  (lambda (lst return)
    (cond 
      (predicate1 consequent1)
      (predicate2 consequent2)
    )
  )
)

; 8. replaceatoms takes two lists. The first list can contain sublists, but the second list is a single list of atoms. The output should be the first list, but each atom of the first list, from left to right, is replaced by the corresponding atom of the second list, until the second list runs out of atoms. 
(equal? (replaceatoms '((a ((b) c d) ((((e) f g) (h i)) j (k l))) m n (o p)) '(z y x w v u t s r q p o n m l k j)) '((z ((y) x w) ((((v) u t) (s r)) q (p o))) n m (l k)))
(equal? (replaceatoms '((a ((b) c d) ((((e) f g) (h i)) j (k l))) m n (o p)) '(z y x w v u)) '((z ((y) x w) ((((v) u g) (h i)) j (k l))) m n (o p)))

(define replaceatoms
  (lambda (lst1 lst2)
    (replaceatoms-cps lst1 lst2 (lambda (v) v))
  )
)

(define replaceatoms-cps
  (lambda (lst1 lst2 return)
    (cond 
      (predicate1 consequent1)
      (predicate2 consequent2)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write the following function using call/cc and a single helper function that uses "normal" recursion instead of tail recursion ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 9. suffix takes an atom and a list and returns a list containing all elements that occur after the last occurrence of the atom. 
(equal? (suffix 'x '(a b c))  '(a b c))
(equal? (suffix 'x '(a b x c d x e f)) '(e f))

(define suffix
  (lambda (atm lst)
    body
  )
)

; 10. emptysublists takes an atom and a list containing sublists. The output list should be the same as the input list except that any sublist (including the main list) that contains the given atom should be emptied of all elements. 
(equal? (emptysublists 'x '((a b c) (d e x g) (((h i) x) j k ((l m x o))))) '((a b c) () (() j k (()))) )
(equal? (emptysublists 'x '((a b c) (d e x g) (((h i) x) j k ((l m x o) x)))) '((a b c) () (() j k ())))
(equal? (emptysublists 'x '((a b c) (d e x g) x (((h i) x) j k ((l m x o))))) '())

(define emptysublists
  (lambda (atm lst)
    body
  )
)
