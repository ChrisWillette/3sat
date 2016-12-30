;;chris willette
;;c.willette@wsu.edu
;;implements a solution to simple decision making problem
;;in this case, climbing a 'hill'
;;to reach a problem state that is more solved with
;;each step until no improvements can be made.


(defun eval-var (var state)
  (second (find var state :key #'first)))
;(cadr (find var state :key #'car)))

(defvar *state* '((a nil)(b t)(c t)(d nil)))
(defvar *clause* '(a (not b) c))

(defun eval-clause (clause state)
  (or (if (atom (first clause))
          (eval-var (first clause) state)
          (not (eval-var (second (first clause)) state)))
     (if (not (null (rest clause)))
        (eval-clause (rest clause) state))))

(defun get-vars (clause)
  (if (not (null clause))
      (if (atom (first clause))
          (cons (first clause)  (get-vars (rest clause)))
          (cons (second (first clause)) (get-vars (rest clause))))))

(defvar *clauses* '((a (not b) c) (a (not b) (not c)) (a (not b) d)))


(defun get-all-vars (clauses)
  (if(> (list-length clauses) 2)
     (union (get-vars (first clauses)) (get-all-vars(rest clauses)))
     (union (get-vars (first clauses))(get-vars(second clauses)))))


(defun unsat-clauses (clauses state)
  (if (not (null (first clauses)))
      (if (not (eval-clause (first clauses) state))
          (cons (first clauses) (unsat-clauses (rest clauses) state))
	(unsat-clauses (rest clauses) state))))


(defun flip-var (var state)
   (if (equalp (first (first state)) var)
       (cons (cons (first (first state)) (list (not (second (first state))))) (rest state))
     (cons (first state) (flip-var var (rest state)))))




(defun get-better-neighbor (clauses state vars max-num-unsat)
  (if (not (null vars))
      (if (>= (list-length (unsat-clauses clauses (flip-var (first vars) state))) max-num-unsat)
          (get-better-neighbor clauses state (rest vars) max-num-unsat)
	(cons (flip-var (first vars) state) (append (unsat-clauses clauses (flip-var (first vars) state)))))))
	      



(defun simple-hill-climb (clauses state dist unsat)
  (cond ((null unsat) state)
	((< dist 1) nil)
	(t (simple-hill-climb
	    clauses
	    (first (get-better-neighbor clauses state (get-all-vars state) (length unsat)))
	    (- dist 1)
	    (rest (get-better-neighbor clauses state (get-all-vars state) (length unsat)))))))



(defvar *score-clauses*
  '((F (NOT B) E) (C (NOT D) E) ((NOT G) F (NOT B)) (D C (NOT A)) (D F B)
    (D (NOT I) (NOT E)) ((NOT H) E I) ((NOT G) F I) (C F (NOT H)) (J G (NOT A))
    ((NOT H) E (NOT A)) (A (NOT F) E) ((NOT G) (NOT E) B) (E (NOT D) (NOT I))
    ((NOT G) I E) ((NOT G) (NOT B) E) (J D (NOT F)) (F (NOT H) E) (D (NOT H) E)
    ((NOT H) (NOT B) C) ((NOT E) I (NOT D)) (B (NOT F) (NOT A)) (E (NOT I) J)
    (E (NOT A) I) (D (NOT F) (NOT H)) (A H D) ((NOT D) G (NOT J)) (F D (NOT A))
    (C D F) ((NOT A) (NOT H) E)))

(defvar *score-state*
  '((A NIL) (B T) (C NIL) (D NIL) (E NIL) (F T) (G T) (H NIL) (I T) (J NIL)))
