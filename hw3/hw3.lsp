;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;This is a Helper function for goal test. It returns true if a row contains a box or a keeper.
(defun check_a_row (row)
  (cond ((null row) nil)
        ((or (isBox (car row)) (isKeeper (car row))) t)
        (t (check_a_row (cdr row)))))


; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((null s) t)
        ((check_a_row (car s)) nil)
        (t (goal-test (cdr s))))
  );end defun

;This is a helper function for get-square. it recursivly goes throught the columns
;till it reach the specificed column. return the content for the column.
(defun get-column (r c)
  (cond ((null r) wall)
        ((equal c 0) (car r))
        (t (get-column (cdr r) (- c 1)))))

;A function called get-square that takes in a State S, a row number r, and a column number c.
;It should return the integer content of state S at square (r,c). If the square is outside the scope of
;the problem, return the value of a wall.
(defun get-square (s r c)
  (cond ((null s) wall)
        ((equal r 0) (get-column (car s) c))
        (t (get-square (cdr s) (- r 1) c))))

;The function takes state s, row numbr r and column number c.
;it returns modified the square contend and return a new state.
(defun set-square (s r c v)
  (cond ((null s) nil)
        ((equal r 0) (append (list (append (butlast (car s) (- (length (car s)) c)) (list v) (nthcdr  (+ c 1) (car s)))) (cdr s))) 
        (t (append (list (car s)) (set-square (cdr s) (- r 1) c v)))))

;lv is the list of values to change
;lr is the list of rows to change. lc is the list of columns to change. lr, lc, lv matched respectively. it modified many squares and
;return a new state
(defun set-square-n (s lr lc lv)
  (cond ((null s) nil)
        ((or (null lr) (null lc) (null lv)) s)
        (t (set-square-n (set-square s (car lr) (car lc) (car lv)) (cdr lr) (cdr lc) (cdr lv))))) 


;Process the move. It takes a current position of keeper, next position moved by D direction relative to the keeper,
;the next next specified position moved by D direction relative to the keeper. It returns a new state by moving the keeper in the D direction.
;If the move is invalid, return NIL. curr-k-post is (c r) 
(defun process-move (s curr-k-pos next-k-pos next-next-k-pos)
  (let ((val_ckp (get-square s (cadr curr-k-pos) (car curr-k-pos))) (val_nkp (get-square s (cadr next-k-pos) (car next-k-pos))) (val_nnkp (get-square s (cadr next-next-k-pos) (car next-next-k-pos))))
    (cond ((isKeeper val_ckp)
           (cond ((isBlank val_nkp) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos)) (list (car curr-k-pos) (car next-k-pos)) (list blank  keeper)))
                 ((isStar val_nkp) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos)) (list (car curr-k-pos) (car next-k-pos)) (list blank keeperstar)))
                 ((and (isBox val_nkp) (isBlank val_nnkp)) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos) (cadr next-next-k-pos)) (list (car curr-k-pos) (car next-k-pos) (car next-next-k-pos)) (list blank keeper box)))
                 ((and (isBox val_nkp) (isStar val_nnkp)) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos) (cadr next-next-k-pos)) (list (car curr-k-pos) (car next-k-pos) (car next-next-k-pos)) (list blank keeper boxstar)))
                 ((and (isBoxStar val_nkp) (isBlank val_nnkp)) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos) (cadr next-next-k-pos)) (list (car curr-k-pos) (car next-k-pos) (car next-next-k-pos)) (list blank keeperstar box)))
                 ((and (isBoxStar val_nkp) (isStar val_nnkp)) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos) (cadr next-next-k-pos)) (list (car curr-k-pos) (car next-k-pos) (car next-next-k-pos)) (list blank keeperstar boxstar)))
                 (t nil) ; cant move
            ))
          ((isKeeperStar val_ckp)
           (cond ((isBlank val_nkp) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos)) (list (car curr-k-pos) (car next-k-pos)) (list star  keeper)))
                 ((isStar val_nkp) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos)) (list (car curr-k-pos) (car next-k-pos)) (list star keeperstar)))
                 ((and (isBox val_nkp) (isBlank val_nnkp)) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos) (cadr next-next-k-pos)) (list (car curr-k-pos) (car next-k-pos) (car next-next-k-pos)) (list star keeper box)))
                 ((and (isBox val_nkp) (isStar val_nnkp)) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos) (cadr next-next-k-pos)) (list (car curr-k-pos) (car next-k-pos) (car next-next-k-pos)) (list star keeper boxstar)))
                 ((and (isBoxStar val_nkp) (isBlank val_nnkp)) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos) (cadr next-next-k-pos)) (list (car curr-k-pos) (car next-k-pos) (car next-next-k-pos)) (list star keeperstar box)))
                 ((and (isBoxStar val_nkp) (isStar val_nnkp)) (set-square-n s (list (cadr curr-k-pos) (cadr next-k-pos) (cadr next-next-k-pos)) (list (car curr-k-pos) (car next-k-pos) (car next-next-k-pos)) (list star keeperstar boxstar)))
                 (t nil) ; cant move
           ))
          (t nil) ; cant move
   )
  ))

;1 is move up, 2 is left, 3 is down, 4 is right. mc stands for move content
;It calls the process move for specified direction. 
;x in form (c r)
(defun try-move (s d) 
  (let ((x (getKeeperPosition s 0)))
    (cond ((= d 1) (process-move s x (list (car x) (- (cadr x) 1)) (list (car x) (- (cadr x) 2))))
          ((= d 3) (process-move s x (list (car x) (+ (cadr x) 1)) (list (car x) (+ (cadr x) 2))))
          ((= d 2) (process-move s x (list (- (car x) 1) (cadr x)) (list (- (car x) 2) (cadr x))))
          ((= d 4) (process-move s x (list (+ (car x) 1) (cadr x)) (list (+ (car x) 2) (cadr x))))                                                                                                       
     );cond end
    ))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
    (cleanUpList (list (try-move s 1) (try-move s 3) (try-move s 2) (try-move s 4)));end
   )

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s) 0
  )


;A helper function for real_hl. It returns a number of box in a row
(defun num_box_in_row (row sum)
  (cond ((null row) sum)
        ((isBox (car row)) (num_box_in_row (cdr row) (+ sum 1)))
        (t (num_box_in_row (cdr row) sum))))

;A helper function for hl. Using a tail recursion improves overall performace. It returns the number of box in a state
(defun remaining_box (s sum)
  (cond ((null s) sum)
        (t (remaining_box (cdr s) (num_box_in_row (car s) sum)))))

; Yes, the heuristic is admissible. The function does not overestimate the cost to reach goal.
;Because the cost of a move is one, and it takes at least one move to move any box to a goal.
; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun h1 (s)
  (remaining_box s 0)
  )


;===============H2=================================
 

; check a row contains box or goal and return pos of that in a list
; return value ex: '(((1 2) (2 3)) ((2 3) (4 5)))
(defun row-box-goal-pos (row r c lbox lgoal)
  (cond ((null row) (list lbox lgoal))
        ((isBox (car row)) (row-box-goal-pos (cdr row) r (+ c 1)  (cons (list r c) lbox) lgoal))
        ((isStar (car row)) (row-box-goal-pos (cdr row) r (+ c 1) lbox (cons (list r c) lgoal)))
        (t (row-box-goal-pos (cdr row) r (+ c 1) lbox lgoal))))
  
;get all position of boxes and goal in a state. Return a list containing a list of box pos and a list of goal pos
;lbox is '(()). initial is nil. return value ex: '(((1 2) (2 3)) ((2 3) (4 5)))
(defun find-pos-boxes-goals (s r lbox lgoal)
  (cond ((null s) (list lbox lgoal))
        (t (let ((temp (row-box-goal-pos (car s) r 0 nil nil))) (find-pos-boxes-goals (cdr s) (+ r 1) (append (car temp) lbox) (append (cadr temp) lgoal))))))
        
;Where box_pos and star_pos is informat (r c). Caculate the manhatan distance from a box to a goal.
(defun manhatan_distance (box_pos star_pos)
  (+ (abs (- (car box_pos) (car star_pos))) (abs (- (cadr box_pos) (cadr star_pos)))))

;return a minimum distance from a box to a goal. when init_flag = 0, intial recursion
(defun min_distance (box_pos lgoal dis init_flag) 
  (cond ((null lgoal) dis)
        ((= init_flag 0) (min_distance box_pos (cdr lgoal) (manhatan_distance box_pos (car lgoal)) 1))
        ((= init_flag 1) (min_distance box_pos (cdr lgoal) (min dis (manhatan_distance box_pos (car lgoal))) 1))))

;Sum all minimum mahanatan distance from boxes to goals.
(defun sum_all_minimum_distance (lbox lgoal sum)
  (cond ((null lbox) sum)
        (t (sum_all_minimum_distance (cdr lbox) lgoal (+ sum (min_distance (car lbox) lgoal 0 0))))))
  
; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h405147924 (s)
  ;return 0 if all box are in goals.
  (cond ((= (remaining_box s 0) 0) 0)
        (t (let ((box_goal_pos (find-pos-boxes-goals s 0 nil nil))) (sum_all_minimum_distance (car box_goal_pos) (cadr box_goal_pos) 0))
        )))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
