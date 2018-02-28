(define insert 
	(lambda (n lst)
		(insert-cps n lst (lambda (v) v)) ))

(define insert-cps
	(lambda (n lst return)
		(if (> (car lst) n)
			(return (cons n lst))
			(insert-cps n (cdr lst) (lambda (v) (return (cons (car lst) v)))) )))

; (insert 7 '(1 4 5 6 9 10))

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

; (merge '(3 5 6 7 9) '(0 1 2 4 6 8 9 10))