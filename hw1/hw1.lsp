;TREE-CONTAINS function takes two arguments N and TREE. N is the number to looked for inside the TREE. TREE is an ordered tree.
;The function returns true if N appears in the ordered tree TREE and false otherwise.
;The solution - First case is to check if the first element of the TREE is a tree, then it recursively calls the function with the tree.
;if the N does not appear in the tree, then the function check the rest of the TREE. Second case is to check if the first element is a number
; and it's equal to N, then we found the element and return true. The third case is to check the rest of TREE in case the two above case falses.

(defun TREE-CONTAINS (N TREE)
  (cond ((null TREE) NIL)
        ((listp (car TREE)) (or (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (cdr TREE))))
        ((and (numberp (car TREE)) (equal N (car TREE))) t)
        ( t (TREE-CONTAINS N (cdr TREE)))
        ))

;TREE-MIN function takes one argument TREE which is an ordered tree. it finds the minimum number in the tree and return the value.
;Since TREE is an ordered tree, we know the smallest value number is in the leftest element of the tree.
;The solution is to look at to recursively look for the leftest element. If the first left element of the TREE is an value, it returns that value.
;else it is a list, so we recursivly search for the left element in that list.
  
(defun TREE-MIN (TREE)
  (cond ((null TREE) NIL)
        ((numberp (car TREE)) (car TREE))
        (t (TREE-MIN (car TREE)))
        ))

;TREE-ORDER function takes one argument TREE which is an ordered tree. It return a pre-ordered list of the numbers appearing in the ordered tree TREE.
;Since a TREE has three element, the function appends the second element which is always a number with the first element and the third element that are.
;are recursivly called with the function. Now the case where it actually appends number is when the tree is a number. if it is, then return a single of
;of the number.
    
(defun TREE-ORDER (TREE)
  (cond ((null TREE) NIL)
        ((numberp TREE) (list TREE))
        (t (append (TREE-ORDER (cadr TREE)) (TREE-ORDER (car TREE)) (TREE-ORDER (cddr TREE))))
        ))

;SUB-LIST function takes 3 arguments: L is a list, START is the start index, LEN is the length of the returned list. The function returns a sub-list L
;starting at position START and having length LEN.
;solution: recursivly iterate the list L and minus START by 1 until START is 0, then start to recursivly construct the list by each element until LEN is 0.
;LEN is being minus by 1 recursivly.
  
(defun SUB-LIST (L START LEN)
  (cond ((null L) NIL)
        ((equal LEN 0) NIL)
        ((and (equal START 0) (> LEN 0)) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
        ((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))
        ))

;SPLIT-LIST function takes one argument list L and return a list compromised of two lists. The two lists are appended together to be equal to L.
;solution: divide the length of the list L by 2. The first sub list is retrieved by Call SUB-LIST function on the list L at position zero and LEN
; is the result of the division. The second sub list is retrived by calling SUB-LIST function on list L at postion of length of first list and LEN
; that is the rest of the length of the first list.

(defun SPLIT-LIST (L)
  (list (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (length (SUB-LIST L 0 (/ (length L) 2))) (- (length L) (length (SUB-LIST L 0 (/ (length L) 2)))))))

;BTREE-HEIGHT functions takes one argument TREE and return the height of the ordered tree TREE.
;solution: First case if TREE is one number, then returns zero. If the both of internal node (TREE) element are atom, it returns 1. If the TREE's element is a
;internal node, it adds return value by 1 and recursively call the function on the node. Whichever left or right child of the node has higher height. it chooses 
;that one to return
 
(defun BTREE-HEIGHT (TREE)
  (cond ((atom TREE) 0)
        ((and (listp TREE) (atom (car TREE)) (atom (cadr TREE))) 1)
         ((and (listp TREE) (atom (car TREE)) (listp (cadr TREE))) (+ 1 (BTREE-HEIGHT (cadr TREE))))
         ((and (listp TREE) (listp (car TREE)) (atom (cadr TREE))) (+ 1 (BTREE-HEIGHT (car TREE))))
         ((and (listp TREE) (listp (car TREE)) (listp (cadr TREE))) (if (> (+ 1 (BTREE-HEIGHT (car TREE))) (BTREE-HEIGHT (cadr TREE)))
                                                                        (+ 1 (BTREE-HEIGHT (car TREE)))
                                                                        (+ 1 (BTREE-HEIGHT (cadr TREE)))))))
;LIST2BTREE function takes one argument LEAVES, a list of leaves node. it return a binary tree of that leaves.
;Solution: The function recursivly spliting the list by using SPLIT-LIST function. If the length of the two resulted lists are more than two, it recursivly calls
;the function to split the list again until the resulted lists length are less than or equal to two. 

(defun LIST2BTREE (LEAVES)
  (cond ((= (length LEAVES) 1) (car LEAVES))
        ((= (length LEAVES) 2) (append (car (SPLIT-LIST LEAVES)) (cadr (SPLIT-LIST LEAVES))))
        ((and (< (length (car (SPLIT-LIST LEAVES))) 3) (< (length (cadr (SPLIT-LIST LEAVES))) 3)) (cons (car (SPLIT-LIST LEAVES)) (cons (LIST2BTREE (cadr (SPLIT-LIST LEAVES))) NIL)))
        ((and (> (length (car (SPLIT-LIST LEAVES))) 2) (> (length (cadr (SPLIT-LIST LEAVES))) 2)) (cons (LIST2BTREE (car (SPLIT-LIST LEAVES))) (cons (LIST2BTREE (cadr (SPLIT-LIST LEAVES))) NIL)))
        ((and (> (length (car (SPLIT-LIST LEAVES))) 2) (< (length (cadr (SPLIT-LIST LEAVES))) 3)) (cons (LIST2BTREE (car (SPLIT-LIST LEAVES))) (cons (LIST2BTREE (cadr (SPLIT-LIST LEAVES))) NIL)))
        ((and (< (length (car (SPLIT-LIST LEAVES))) 3) (> (length (cadr (SPLIT-LIST LEAVES))) 2)) (cons (car (SPLIT-LIST LEAVES)) (cons (LIST2BTREE (cadr (SPLIT-LIST LEAVES))) NIL)))
        ))

;BTREE2LIST function takes one argument TREE, a binary TREE. It returns a functions a list of atoms that are leaves of TREE.
;Solution: The function uses append operator to merge the left child and right child of an internal node. It recursivly calls BTREE2LIST if the child is an internal node.
;If the the element is a number, then it returns a list containing the element.

(defun BTREE2LIST (TREE)
  (cond ((numberp TREE) (list TREE)) 
        ((and (atom (car TREE)) (atom (cadr TREE))) (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))
        ((and (listp (car TREE)) (atom (cadr TREE))) (append (BTREE2LIST (car TREE)) (cdr TREE)))
        ((and (atom (car TREE)) (listp (cadr TREE))) (append (car TREE) (BTREE2LIST (cdr TREE))))
        ((and (listp (car TREE)) (listp (cadr TREE))) (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE))))
        ))

;IS-SAME function takes two expressions E1, E2. It returns True of E1 equals to E2 and false if E1 is not equal to E2.
;Solution: If the first element of E1 and E2 are both number and are equal, then it passes and compares the rest of E1 and E2. If both of them are list, it recursively call on IS-SAME
;to compares if the lists are equal and passes to compare the rest of E1 and E2. If one element of E1 and E2 differs, it returns false.

(defun IS-SAME (E1 E2)
  (cond ((and (null E1) (null E2)) t)
        ((and (numberp (car E1)) (numberp (car E2)) (= (car E1) (car E2))) (IS-SAME (cdr E1) (cdr E2)))
        ((and (listp (car E1)) (listp (car E2))) (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
        ))