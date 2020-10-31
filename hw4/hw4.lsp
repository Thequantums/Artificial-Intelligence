;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


;if a var matched and make the var in clause fails, remove var from the clause and return the clause.
(defun check_cl (var cl_it cl_fix)
  (cond ((null cl_it) cl_fix)
        ((and (= (abs (car cl_it)) (abs var)) (< (* (car cl_it) var) 0)) (remove (car cl_it) cl_fix))
        (t (check_cl var (cdr cl_it) cl_fix))))

;return true if the clause fail by current assignment. If the current assignment are not enough to make the clause fails
;it return true.
(defun clause_fail? (assign clause) (if (null assign) nil (let ((temp (check_cl (car assign) clause clause)))
  (cond ;clause empty meaning fail clause.
        ((not temp) t)
        (t (clause_fail? (cdr assign) temp)))))) 

;A helper function for DFS++. assign has a form of '(1 2 -3 4 ...) meaning 1 is assigned as pos, 3 is assigned as neg
;It returns nil if current assignment to variables leads to failure of contraints satifibility.It returns true otherwise.
(defun backtrack_checking (delta assign)
  (cond ((null delta) t)
        ((clause_fail? assign (car delta)) nil)
        (t (backtrack_checking (cdr delta) assign))))

;return true if a varaible make the clause true
(defun clause_true? (var it_cl)
  (cond ((null it_cl) nil)
        ((and (= (abs (car it_cl)) (abs var)) (> (* (car it_cl) var) 0)) t)
        (t (clause_true? var (cdr it_cl))))) 
        
;Helper function for DFS++. It takes a var - a variable, it_delta - current delta, delta - initally nil. It returns a clauses-reduced delta.
;if the clauses are true by the var. If var is nil, then return back the original delta.
(defun reduce_delta (var it_delta delta)
  (cond ((null var) it_delta)
        ((null it_delta) delta)
        ((clause_true? var (car it_delta)) (reduce_delta var (cdr it_delta) delta))
        (t (reduce_delta var (cdr it_delta) (append (list (car it_delta)) delta)))))

;Helper function for sat?. It takes var - a variable to be assigned, val - a value to assigned for variable, n - number of variables + 1, delta - contraints,
;assign - assignment for variable. It returns a list of assignments to variables
;Solution: it use DFS to search starting from variable 1 to variable n + 1. It first assigned variable to be positive literal. If fails, then backtrack and assign it to negative literal.
;It uses backtrack_checking function to check if the current assignment fails the constraints. if it fails (return nil), then it backtracks to above stack and does the negative lieteral assignment.
;For every new assignment of variable, it also uses reduce_delta to reduce clauses that are true by the current assignment.
(defun DFS++ (var val n delta assign) (let ((bt (backtrack_checking delta assign)))
  (cond ((and (= var n) bt) assign)
        ((equal bt t) (let* ((new_delta (reduce_delta (car assign) delta nil)) (left_dfs (DFS++ (+ var 1) 1 n new_delta (append (list (* var val)) assign))))
                        (if (equal left_dfs nil) (DFS++ (+ var 1) 1 n new_delta (append (list (* var -1)) assign)) left_dfs)))
        (t nil))))

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (DFS++ 1 1 (+ n 1) delta nil))
  


;===================Varifying Solution Code============================================
;It uses the reduce_delta function to check solution correctioness. If solution is correct, then the reduce_delta function
;will eventually return nil. Because for each assigment that make the clause true, it removes the clause. If every clause is true, they are all removed.
(defun solution_helper (delta result) (if (null result) nil (let ((temp (reduce_delta (car result) delta nil)))
  (cond ((null temp) t)
        (t (solution_helper temp (cdr result)))))))

;A solution verifying function. It takes a path to the filename as an argument. it return true if solution to problem is correct, fail otherwise.
(defun solution_verify (filename)
  (let* ((cnf (parse-cnf filename)) (result (sat? (first cnf) (second cnf))))
    (solution_helper (second cnf) result)
    )) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

