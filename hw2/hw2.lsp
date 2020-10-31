;Function BFS takes a single argument tree that is the list representation of the tree, and return
;a single, top-level list of the terminal nodes in the order they would be visited by a left-to-right
;breadth-first search.
;Solution: if the first element is not an atom, append it to back of the tree and recursivly call the
;function again. If the first element is an atom, cons it with the rest of the (BFS tree)
(defun BFS (tree)
  ;if tree is a null, just return null
  (cond ((null tree) NIL)
        ;if the first element is a atom, then cons it to the rest of BFS tree
        ((atom (car tree)) (cons (car tree) (BFS (cdr tree))))
        ;if it's not, then it's a list.So append that element to the back of the tree.
        ;and call BFS on the tree again. since append will resolve the parenthesis of that first element
        ; it works.
        (t (BFS (append (cdr tree) (car tree))))))

;Function DFS takes a single argument tree that is the list representation of the tree, and return a
;single, top-level list of the terminal nodes in they would be visited by right to left dept first search
(defun DFS (tree)
  ;if the tree is null, return nil
  (cond ((null tree) NIL)
        ;if the tree is an atom return list of that tree
        ((atom tree) (list tree))
        ;if the tree has one element, return DFS of that tree
        ((null (cdr tree)) (DFS (car tree)))
        ;if the tree is a list and has more than 1 element, append the recursive call of DFS on the rest
        ; with the DFS of the first element. since the first element is the least priority
        (t (append (DFS (cdr tree)) (DFS (car tree))))))

;Function DFSR2L takes two arguments, tree and l. tree represent the tree to search for. l is the depth of
;the tree to search for. It uses DFS to search the tree from left to right.
;solution: the function keep minus l by 1 till it becomes zero then it return NIL.
(defun DFSR2L (tree l)
  (cond ;if null then return nil
        ((null tree) NIL)
        ;if the tree is an atom, return the list of that atom.
        ((atom tree) (list tree))
        ;if l is zero, then return nil
        ((< l 1) NIL)
        ((> l 0) (append (DFSR2L (car tree) (- l 1)) (DFSR2L (cdr tree) l))))) 

;Function real_DFID takes three arguments. tree is the tree to search for. d is the maximum depth of the tree
;l is the dept to search for. Initially l is 0. It grows up to equal to d.
(defun real_DFID (tree d l)
  (cond ((null tree) NIL)
         ;if d is less than 0, then return NIL since it already processed to maximum dept of the tree
         ((< d 0) NIL)
         ;if d is not less than 0, append the result of DFSR2L with the future DFSR2L with more depth search
         (t (append (DFSR2L tree l) (real_DFID tree (- d 1) (+ l 1)))))) 

(defun DFID (tree d) (real_DFID tree d 0))

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (if (equal s '(3 3 NIL)) t NIL))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (cond ;if it moves more m or c than available return NIL
         ((or (> m (car s)) (> c (cadr s))) NIL)
         ;if the move left more m than c in the transitioned state, return NIL 
         ((and (not (equal (+ m (- 3 (car s))) 0)) (> (+ c (- 3 (cadr s))) (+ m (- 3 (car s))))) NIL)
         ;if the move left more m than c in the original state, return NIL
         ((and (not (equal (- (car s) m) 0)) (> (- (cadr s) c) (- (car s) m))) NIL)
         ;return list of list of transitioned state 
         (t (list (list (+ m (- 3 (car s))) (+ c (- 3 (cadr s))) (not (caddr s)))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s) (append (next-state s 1 1) (next-state s 2 0) (next-state s 0 2) (next-state s 1 0) (next-state s 0 1)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) NIL)
        ((equal (car states) s) t)
        (t (on-path s (cdr states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond
        ((null states) NIL)
        ((final-state (car states)) (append (list (car states)) path))
        ((on-path (car states) path) (mult-dfs (cdr states) path))
        ((not (mult-dfs (succ-fn (car states)) (append (list (car states)) path))) (mult-dfs (cdr states) path))
        (t (mult-dfs (succ-fn (car states)) (append (list (car states)) path)))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) (append (list s) path))
        ((on-path s path) NIL)
        (t (mult-dfs (succ-fn s) (append (list s) path)))))



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))

