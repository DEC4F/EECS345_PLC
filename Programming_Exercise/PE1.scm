#|
Programming Exercise 1 for EECS 345 PLC
Stanley
|#

#| 1. insert takes a number and a list of numbers in order and inserts the number in the proper place
   >(insert 7 '(1 4 5 6 9 10))
   (1 4 5 6 7 9 10) |#
(define insert 
  (lambda (x lst)
    (cond 
      ((null? lst) (list x))                           ; base case, reached end of list
      ((<= x (car lst)) (cons x lst))                  ; match case, x less than current head
      (else (cons (car lst) (insert x (cdr lst)) ))))) ; recursive case

#| 2. merge takes two lists of numbers that are in order and returns a list that contains the combination of both lists in order.
   >(merge '(3 5 6 7 9) '(0 1 2 4 6 8 9 10))
   (0 1 2 3 4 5 6 6 7 8 9 9 10)  |#
(define merge
  (lambda (lst1 lst2)
    (cond
    ((null? lst1) lst2)                                                          ; base case, when lst1 is empty, return lst2
      ((null? lst2) lst1)                                                        ; the other base case
      ((<= (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) lst2)))     ; recursive case, choose head of lst1 if its smaller
      ((>  (car lst1) (car lst2)) (cons (car lst2) (merge lst1 (cdr lst2)))) ))) ; the other recursive case

#| 3. removedups takes a list of atoms and removes any atom that is a repeat of the atom that immediately precedes it 
   >(removedups '(a a b b b c c a b b))
   (a b c a b) |#
(define removedups
  (lambda (lst)
    (cond 
      ((null? (cdr lst)) lst)                                ; base case, return current list if current list only contains 1 element
      ((equal? (car lst) (cadr lst)) (removedups (cdr lst))) ; recursive case, call removedups on next list if current head = next head
      (else (cons (car lst) (removedups (cdr lst)))) )))     ; recursive case, append head to what's returned by calling removedups on next list

#| 4. split h a list of atoms and returns a list that contains two lists of atoms. The first list should contain the 1st, 3rd, 5th, ... atoms, and the second list should contain the 2nd, 4th, 6th, ... atoms. 
   >(split '())
   (() ())
   > (split '(a b c d e))
   ((a c e) (b d)) |#`
(define split
  (lambda (lst)
    (if (or (null? lst) (null? (cdr lst)))                   ; if either current list is null or only contain 1 element
      (list lst '())                                         ; base case, return a list of current list and a null list
      (list (cons (car lst) (car (split (cddr lst))))        ; recursive step, append the head to the head of next next element returned
            (cons (cadr lst) (cadr (split (cddr lst)))) )))) ; recursive case, append the next head to the next head of element returned

#| 5. deepcons takes an element and a list, that possibly containst sublists and places the element in the front of the first element, as deep in the sublist as needed
   > (deepcons 'a '(b ((c) d (e f))))
   (a b ((c) d (e f)))
   > (deepcons 'a '(() ()))
   ((a) ()) |#
(define deepcons
  (lambda (x lst)
    (cond
      ((null? (car lst)) (cons (cons x (car lst)) (cdr lst)))     ; edge case, append x to the rest if the head of curr list is null,
      ((list? (car lst)) (cons (deepcons x (car lst)) (cdr lst))) ; recursive case, append result of deepcons of x and curr head (sublist) with rest
      (else (cons x lst)) )))                                     ; base case, append x to the list

#| 6. numparens takes a list and returns the number of pairs of parentheses 
   > (numparens '(1 2 3))
   1
   > (numparens '(1 () (()) (2 3 (4))))
   6 |#
(define numparens
  (lambda (lst)
    (cond
      ((null? lst) 1)                                                      ; base case, list end, return 1 as the pren of the input list
      ((list? (car lst)) ( + (numparens (car lst)) (numparens (cdr lst)))) ; recursive case, add output of curr head to output of the rest
      (else (numparens (cdr lst))) )))                                     ; recursive case, check on the rest 

#| 7. dup* takes a list and duplicates all contents, including any sublists
   > (dup* '(1 2 (3 4) 5))
   (1 1 2 2 (3 3 4 4) (3 3 4 4) 5 5) |#
(define dup* 
  (lambda (lst) 
    (cond 
      ((null? lst) '())                                                                     ; base case, reached the list end
      ((list? (car lst)) (cons (list (dup* (car lst)) (dup* (car lst)) ) (dup* (cdr lst)))) ; recursive case, recursively dup the sublist itself
      (else (cons (list (car lst) (car lst)) (dup* (cdr lst)))) )))                         ; recursive case, dup curr head and append to the rest

#| 8. removedups* takes a list, that can contain sublists, and removes any atom that is the repeat of the atom that immediately precedes it in the same sublist
   > (removedups* '(a a (b b b (d d) b ((d) d)) f (f f g)))
   (a (b (d) b ((d) d)) f (f g)) |#
(define removedups* 
  (lambda (lst) 
    (cond 
      ((null? lst) '())                                                          ; base case, reached list end
      ((list? (car lst)) (cons (removedups* (car lst)) (removedups* (cdr lst)))) ; recursive case, list head is a list
      ((null? (cdr lst)) lst)                                                    ; base case, curr list only contains 1 element
      ((eq? (car lst) (cadr lst)) (removedups* (cdr lst)))                       ; recursive case, dup found, remove the rest
      (else (cons (car lst) (removedups* (cdr lst)))) )))                        ; recursive case, dup not found, remove the rest

; 9. split* takes a list, that can contain sublists, and returns a list containing two lists. The first list should contain the 1st, 3rd, 5th, ... elements, and the second list should contain the 2nd, 4th, 6th, ... elements. However, if any of these elements are also lists, these elements should be split as well.
(define split* 
  (lambda (lst) 
    (cond
      ((null? lst) (list '() '()))
      ((null? (cdr lst)) (list lst '()))
      ((and (list? (car lst)) (list? (cadr lst)))                    ; both car and cadr are list
        (list (cons (split* (car lst)) (car (split* (cddr lst)))) 
              (cons (split* (cadr lst)) (cadr (split* (cddr lst))))))
      ((and (list? (car lst)) (not (list? (cadr lst))))              ; car is list, cadr is atom
        (list (cons (split* (car lst)) (car (split* (cddr lst))))
              (cons (cadr lst) (cadr (split* (cddr lst)))) ))
      ((and (not (list? (car lst))) (list? (cadr lst)))              ; car is atom, cadr is list
        (list (cons (car lst) (car (split* (cddr lst)))) 
              (cons (split* (cadr lst)) (cadr (split* (cddr lst))))))
      (else (list (cons (car lst) (car (split* (cddr lst))))         ; both car and cadr are atom
              (cons (cadr lst) (cadr (split* (cddr lst))))))
    )))
(split* '(a b ((c d) e f g) (((h i) j k l (m n o p)))))
;((a ((((c) (d)) f) (e g))) (b ((((((h) (i)) k ((m o) (n p))) (j l))) ())))

#| 10. removedups** takes a list, that can contain sublists, and removes any element that, once repeated elements have been removed from it, is the repeat of any element (also once elements have been removed from it) that immediately precedes it in the same sublist
   > (removedups** '(x x (a a b) (a b b) c c))
   (x (a b) c)
   > (removedups** '((a a (b b b (c))) (a (b b b (c c))) (a (b (c c (c (c)))) (b b b (c))) (a (b (c c)) (b b b (c)))))
   ((a (b (c)))) |#
(define removedups**
  (lambda (lst)
    (removedups**-ihelper 1 (removedups* lst)) ))                                     ; can be improved, output is independent of counter size

(define removedups**-ihelper                                                          ; a iterative helper function
  (lambda (counter lst)                                                               ; counter keep tracks of level of iteration
    (cond
      ((zero? counter) lst)                                                           ; base case, return list when reached iteration end
      ((list? (car lst)) (removedups**-ihelper (- counter 1) (removedups (car lst)))) ; recursive case, call removedups on the head if it's a list
      (else (removedups**-ihelper (- counter 1) (removedups lst))) )))                ; recursive case, call removedups if head is not a list

