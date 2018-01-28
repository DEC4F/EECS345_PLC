#|
Programming Exercise 1 for EECS 345 PLC
Stanley
|#

; 1. insert takes a number and a list of numbers in order and inserts the number in the proper place
(define insert 
	(lambda (x lst)
		(cond 
			((null? lst) (list x)) ; base case
			((<= x (car lst)) (cons x lst)) ; match case
			(else
				(cons 
					(car lst) (insert x (cdr lst)) ))))) ; recursive case


; 2. merge takes two lists of numbers that are in order and returns a list that contains the combination of both lists in order. 
(define merge
	(lambda (lst1 lst2)
		(cond
			((null? lst1) lst2)                                                            ; base case, when lst1 is empty, return lst2
			((null? lst2) lst1)                                                            ; the other base case
			((<= (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) (lst2))))     ; recursive case, choose head of lst1 if its smaller
			((>  (car lst1) (car lst2)) (cons (car lst2) (merge (lst1) (cdr lst2)))) ))) ; the other recursive case


; 3. removedups takes a list of atoms and removes any atom that is a repeat of the atom that immediately precedes it 
(define removedups
	(lambda (lst)
	))