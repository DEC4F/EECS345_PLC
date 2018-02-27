#|
Programming Exercise 2 for EECS 345 PLC
Stanley
|#

; insert takes a number and a list of numbers in order and inserts the number in the proper place. 
(define insert 
	(lambda (n lst)
		(insert-cps n lst (lambda (v) v)) ))

(define insert-cps
	(lambda (n lst return)
		(if (> (car lst) n)
			(return (cons n lst))
			(insert-cps n (cdr lst) (lambda (v) (return (cons (car lst) v)))) )))

; (equal? (insert 7 '(1 4 5 6 9 10)) '(1 4 5 6 7 9 10))

; merge takes two lists of numbers that are in order and returns a list that contains the combination of both lists in order. 
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

(equal? (merge '(3 5 6 7 9) '(0 1 2 4 6 8 9 10)) '(0 1 2 3 4 5 6 6 7 8 9 9 10))

; removedups takes a list of atoms and removes any atom that is a repeat of the atom that immediately precedes it. 
(define removedups
	(lambda (lst)
		(removedups-cps lst (lambda (v) v))
	)
)

(define removedups-cps
	(lambda (lst return)
		(cond 
			((null? (cdr lst)) (return lst))
			((eq? (car lst) (cadr lst)) (removedups-cps (cdr lst) (lambda (v1) (return v1))))
		  (else (removedups-cps (cdr lst) (lambda (v2) (return (cons (car lst) v2)))))
		)
	)
)

; (equal? (removedups '(a a b b b c c a b b)) '(a b c a b))

; numparens takes a list and returns the number of pairs of parentheses 
(define numparens
  (lambda (lst)
    (numparens-cps lst (lambda (v) v)) ))

(define numparens-cps
  (lambda (lst return)
    (cond 
      ((null? lst) (return 1))
      ((list? (car lst)) ())
    )
  )
)

(numparens '(1 2 3))
(numparens '(1 () (()) (2 3 (4)))