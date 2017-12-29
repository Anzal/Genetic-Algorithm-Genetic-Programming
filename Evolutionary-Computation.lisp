
#|
In this project we will produce three things:

1. A high-level evolutionary computation framework

2. The representation, breeding functions, and evaluations to do a simple
GA for boolean vectors and for floating-point vectors, including a few test
functions on each.

3. The representation, breeding functions, and evaluations to do GP for
two problems:
A. Symbolic Regression
B. Artificial Ant

The high-level EC system will work as follows:

- Simple generational evolution
- GA-style Tournament selection
- A simple breeding function
- some simple statistics functions

|#

(defmacro swap (elt1 elt2)
  "Swaps elt1 and elt2, using SETF.  Returns nil."
  (let ((temp (gensym)))
    `(let ((,temp ,elt1))
       (setf ,elt1 ,elt2)
       (setf ,elt2 ,temp)
       nil)))

(defmacro while (test return-value &body body)
  "Repeatedly executes body as long as test returns true.
Then evaluates and returns return-value"
  `(progn (loop while ,test do (progn ,@body)) ,return-value))

(defun random-elt (sequence)
  "Returns a random element from sequence"
  (elt sequence (random (length sequence))))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the test used for duplicates."
  (let (bag)
    (while (< (length bag) num) bag
      (let ((candidate (funcall function)))
  (unless (and no-duplicates
              (member candidate bag :test #'equalp))
      (push candidate bag))))))

;;; TOURNAMENT SELECTION

(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
  (let* ((n (length population)) (best (random n)) (candidate))
    (dotimes (i (- *tournament-size* 1))
      (setf candidate (random n))
      (if(> (elt fitnesses candidate) (elt fitnesses best))
          (setf best candidate)))
    (elt population best)))
        
(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list"
  (let ((individuals nil)) 
    (dotimes (i num)
      (setf individuals (cons (tournament-select-one population fitnesses) individuals)))
    individuals))

(defparameter *best-ind* nil)           ;best individual found in all generations
(defparameter *best-fitness* -10000)    ;best fitness in all generations   

(defun simple-printer (pop fitnesses)  ;; I'm nice and am providing this for you.  :-)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
    (when (or (not best-ind)
          (< best-fit fit))
        (setq best-ind ind)
          (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
          best-fit best-ind)
    (if(> best-fit *best-fitness*) (progn (setf *best-fitness* best-fit) (setf *best-ind* best-ind)))
    fitnesses))

(defun evolve (generations pop-size
                &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"
 
  (let ((population nil) (fitnesses nil) (p1 nil) (p2 nil))
    (funcall setup)
    (dotimes (a pop-size)
      (setf population (cons (funcall creator) population)))
    (setf fitnesses (mapcar evaluator population))
    (funcall printer population fitnesses)

    (dotimes (i generations)
      (setf p1 (funcall selector (/ (length population) 2) population fitnesses))
      (setf p2 (funcall selector (/ (length population) 2) population fitnesses))
      (setf population (mapcar modifier p1 p2))
      (setf population (append (mapcar #'(lambda(a) (elt a 0)) population) (mapcar #'(lambda(a) (elt a 1)) population)))
      (setf fitnesses (mapcar evaluator population))
      (funcall printer population fitnesses))

    (format t "~%Best Individual in all Generations...~%Individual: ~a~%Fitness:~a~%"
          *best-ind* *best-fitness*)))


;;;;;; BOOLEAN VECTOR GENETIC ALGORTITHM



;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; boolean vectors Problem.  Assume that you are evolving a bit vector
;;; *vector-length* long.  

;;; The default test function is Max-Ones.
;;;; Max-ones is defined in section 11.2.1 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other boolean functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :max-ones
;;; :trap
;;; :leading-ones
;;; :leading-ones-blocks



;; perhaps other problems might include... 

(defparameter *boolean-crossover-probability* 0.2)
(defparameter *boolean-mutation-probability* 0.01)
(defparameter *boolean-vector-length* 100)
(defparameter *boolean-problem* :max-ones)

(defun boolean-vector-creator ()
  "Creates a boolean-vector *boolean-vector-length* in size, filled with
random Ts and NILs, or with random 1s and 0s, your option."
    ;;; IMPLEMENT ME
  (mapcar #'(lambda(a) (if(equalp a t) 1 0)) (generate-list *boolean-vector-length* #'random?)))


(defun boolean-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation simply flips the bit of the allele."

    ;;; IMPLEMENT ME  
  (let ((a (copy-list ind1)) (b (copy-list ind2)))  
  (loop for i from 0 to (- (length ind1) 1)
    do(if(< (random 1.0) *boolean-crossover-probability*) (swap (elt a i) (elt b i)))
    do(if(< (random 1.0) *boolean-mutation-probability*) (setf (elt a i) (if(= 1 (elt a i)) 0 1)))
    do(if(< (random 1.0) *boolean-mutation-probability*) (setf (elt b i) (if(= 1 (elt a i)) 0 1))) ) 
  (list a b)))  


(defun boolean-vector-evaluator (ind1)
  "Evaluates an individual, which must be a boolean-vector, and returns
its fitness."
    ;;; IMPLEMENT ME ;Max-Ones
  (funcall #'leading-ones ind1))

(defun max-ones(ind1)
  (apply #'+ ind1))

(defun leading-ones(ind1)
  (let ((n (length ind1)) (z 0))
    (loop for i from 0 to (- n 1)
      until(= 0 (elt ind1 i))
      do(setf z (+ 1 z)))
    z))

(defun trap(ind1)
  (let ((n (length ind1)))
    (+ (- n (apply #'+ ind1)) (* (+ n 1) (apply #'* ind1)))))


(defun boolean-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this to set up various
(ahem) global variables to define the problem being evaluated, I dunno."
  (setq *boolean-crossover-probability* 0.2)
  (setq *boolean-mutation-probability* 0.01)
  (setq *boolean-vector-length* 100)
  (setq *boolean-problem* :max-ones)
  (setq *best-ind* nil)
  (setq *best-fitness* -100000))


;;; an example way to fire up the GA. 
#|
(evolve 50 100
	:setup #'boolean-vector-sum-setup
	:creator #'boolean-vector-creator
	:selector #'tournament-selector
	:modifier #'boolean-vector-modifier
        :evaluator #'boolean-vector-evaluator
	:printer #'simple-printer)
|#







;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM




;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; GA Max-ONES Problem.  Assume that you are evolving a vector
;;; of floating-point numbers *float-vector-length* long.  


;;; The default test function is Rastrigin.
;;;; Rastrigin is defined in section 11.2.2 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other floating-point functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :rastrigin
;;; :rosenbrock
;;; :griewank
;;; :schwefel

;;; If you were really into this, you might also try testing on
;;; rotated problems -- some of the problems above are linearly
;;; separable and rotation makes them harder and more interesting.
;;; See the end of Section 11.2.2.

;;; I have defined the minimum and maximum gene values for rastrigin
;;; as [-5.12, 5.12].  Other problems have other traditional min/max
;;; values, consult Section 11.2.2.

(defparameter *float-vector-length* 5)
(defparameter *float-problem* :rastrigin)
(defparameter *float-min* -5.12)  ;; these will change based on the problem
(defparameter *float-max* 5.12)   ;; likewise

(defparameter *float-crossover-probability* 0.2)
(defparameter *float-mutation-probability* 0.1)   ;; I just made up this number
(defparameter *float-mutation-variance* 0.01)     ;; I just made up this number


(defun random-from-range()
  (+ *float-min* (random (- *float-max* *float-min*))))

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
random numbers in the range appropriate to the given problem"
    ;;; you might as well use random uniform numbers from *float-vector-min*
    ;;; to *float-vector-max*.  
    (generate-list *float-vector-length* #'random-from-range))

(defun normal-random (mean sd)
  "Box Muller Polar Transform."
  (do* ((u (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (v (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (s (+ (* u u) (* v v))
                (+ (* u u) (* v v))))
    ((not (or (= 0 s) (>= s 1)))
     (+ mean (* sd (* u (sqrt (/ (* -2.0 (log s)) s))))))))

(defun gaussian-convolution (ind1)
  (let* ((p *float-mutation-probability*) (min *float-min*) (max *float-max*) (sd (sqrt *float-mutation-variance*)))    
    (loop for i from 0 to (- (length ind1) 1)
      do(if(>= p (random 1.0))
          (do ((n (normal-random 0 sd) (normal-random 0 sd))) 
            ((and (>= (+ (elt ind1 i) n) min) (<= (+ (elt ind1 i) n) max))
             (setf (elt ind1 i) (+ (elt ind1 i) n))))))
    ind1))

(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."
 (let ((a (copy-list ind1)) (b (copy-list ind2)))
  (loop for i from 0 to (- (length ind1) 1)
    do(if(< (random 1.0) *float-crossover-probability*) (swap (elt a i) (elt b i))))

    (list (gaussian-convolution a) (gaussian-convolution b)))
)

(defun float-vector-sum-evaluator (ind1)
  "Evaluates an individual, which must be a floating point vector, and returns
its fitness."
  (funcall #'rastrigin ind1)
)

(defun rastrigin(ind1)
  (let* ((n (length ind1)) (z (* 10 n)))
    (loop for i from 0 to (- n 1)
      do(setf z (+ z (- (expt (elt ind1 i) 2) (* 10 (cos (* 2 pi (elt ind1 i))))))))
    (* -1 z)))

(defun sphere(ind1)
  (let* ((n (length ind1)) (z 0))
    (loop for i from 0 to (- n 1)
      do(setf z (+ z (expt (elt ind1 i) 2))))
    (* -1 z)))

(defun schwefel(ind1)
  (let* ((n (length ind1)) (z (* 10 n)))
    (loop for i from 0 to (- n 1)
      do(setf z (+ z (* -1 (elt ind1 i) (sin (sqrt (abs (elt ind1 i))))))))
    (* -1 z)))

(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated 
and the floating-point ranges involved, etc.  I dunno."

(setq *float-vector-length* 10)
(setq *float-problem* :rastrigin)
(setq *float-min* -5.12)  ;; these will change based on the problem
(setq *float-max* 5.12)   ;; likewise

(setq *float-crossover-probability* 0.2)
(setq *float-mutation-probability* 0.1)   ;; I just made up this number
(setq *float-mutation-variance* 0.01)     ;; I just made up this number
(setf *best-ind* nil)
(setf *best-fitness* -100000))


;;; an example way to fire up the GA.  

#|
(evolve 50 100
	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'float-vector-sum-evaluator
	:printer #'simple-printer)
|#













;;;; GP TREE CREATION CODE

;; set up in the gp setup function -- for example, see
;; the code for gp-symbolic-regression-setup


(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)

(defun make-queue ()
  "Makes a random-queue"
  (make-array '(0) :adjustable t :fill-pointer t))

(defun enqueue (elt queue)
  "Enqueues an element in the random-queue"
  (progn (vector-push-extend elt queue) queue))

(defun queue-empty-p (queue)
  "Returns t if random-queue is empty"
  (= (length queue) 0))

(defun random-dequeue (queue)
  "Picks a random element in queue and removes and returns it.
Error generated if the queue is empty."
  (let ((index (random (length queue))))
    (swap (elt queue index) (elt queue (1- (length queue))))
    (vector-pop queue)))

(defun make-tree (data)
  "Creates a new node that contains 'data' as its data."
  (cons (cons data nil) nil))

(defun first-child (tree)
  "Returns a reference to the first child of the node passed in,
  or nil if this node does not have children."
  (cdr (car tree)))

(defun add-child (tree child)
  "Takes two nodes created with 'make-tree' and adds the
  second node as a child of the first. Returns the first node,
  which will be modified."
  (nconc (car tree) child)
  tree)

(defun ptc2 (size)
  "If size=1, just returns a random terminal.  Else builds and
returns a tree by repeatedly extending the tree horizon with
nonterminals until the total number of nonterminals in the tree,
plus the number of unfilled slots in the horizon, is >= size.
Then fills the remaining slots in the horizon with terminals.
Terminals like X should be added to the tree
in function form (X) rather than just X."

 (let* ((a) (q (make-queue)) (n (elt *nonterminal-set* (random (length *nonterminal-set*))))
       (count1 1) (argCount 0) (start (make-tree (elt n 0))) (root start) (args) (current) (rand-deq-ele))
  (if (= size 1)
      (list (elt *terminal-set* (random (length *terminal-set*))))
    (progn
      (loop for x from 0 to (- (elt n 1) 1) ;;count args n takes
            do(setf argCount (+ argCount 1)))
      (enqueue (list start argCount) q) ;;enqueue (parent, argNumber) into q
      (setf argCount 0)
      (loop while (not (or (= size (+ count1 (length q))) (< size (+ count1 (length q)))))
            do(setf rand-deq-ele (random-dequeue q)) ;;check parent of removed element & numargs to fill
            (setf start (elt rand-deq-ele 0))
            (setf args (elt rand-deq-ele 1))
            (setf a (elt *nonterminal-set* (random (length *nonterminal-set*))))
            (if (> args 1)
                (enqueue (list start (- args 1)) q)) ;ex. if args is 2, still going to have to fill one after this so re-enqueue w/ args-1
            (setf current (make-tree (elt a 0))) ;make a new parent
            (setf count1 (+ count1 1))
            (add-child start current) ;add new node to parent
            (setf start current) ;set start to new parent
            (loop for x from 0 to (- (elt a 1) 1) ;;x = arg number/position
                  do(setf argCount (+ argCount 1))
                  )
            (enqueue (list start argCount) q) ;;enqueue new node
            (setf argCount 0))
      (loop while (> (length q) 0)
            do(setf rand-deq-ele (random-dequeue q))
            (setf a (elt *terminal-set* (random (length *terminal-set*))))
            (setf start (elt rand-deq-ele 0))
            (setf args (elt rand-deq-ele 1))
            (if (> args 1)
                (enqueue (list start (- args 1)) q))
            (setf current (make-tree a))
            (add-child start current)
            (setf start current))
      (elt root 0))))) 


(defparameter *mutation-size* 10)
(defparameter *size-limit* 20)
(defparameter *c 0)

(defun gp-creator ()
  "Picks a random number from 1 to 20, then uses ptc2 to create
a tree of that size"
  (let ((rand-num))
    (setf rand-num (+ 1 (random *size-limit*)))
    (ptc2 rand-num)))

;;; GP TREE MODIFICATION CODE

(defun num-nodes (tree)
  (cond 
    ((null tree) 0)
    ((atom tree) 1)
    ((listp tree) (+ (num-nodes (car tree)) (num-nodes (cdr tree))))))


(defun pick-node (node a)
  (let ((i 0) (parent) (r) (n-subtree nil))
  (if(not (= -1 (elt node 1))) (setf *c (+ *c 1)))
  (if(>= *c a) (setf n-subtree node))
  (if(not (= -1 (elt node 1))) (setf parent (elt (elt node 0) (+ 1 (elt node 1)))) (setf parent (elt node 0)))
  (if(listp parent) (setf r (cdr parent)) (setf r nil))
  (loop 
    until(or (not r) n-subtree)
    do(setf r (cdr r))
    do(setf n-subtree (pick-node (list parent i) a))
    do(setf i (+ i 1)))
  n-subtree))



(defun nth-subtree-parent (tree n)
  "Given a tree, finds the nth node by depth-first search though
the tree, not including the root node of the tree (0-indexed). If the
nth node is NODE, let the parent node of NODE is PARENT,
and NODE is the ith child of PARENT (starting with 0),
then return a list of the form (PARENT i).  For example, in
the tree (a (b c d) (f (g (h) i) j)), let's say that (g (h) i)
is the chosen node.  Then we return ((f (g (h) i) j) 0).
If n is bigger than the number of nodes in the tree
 (not including the root), then we return n - nodes_in_tree
 (except for root)."

    (let ((total-nodes (num-nodes tree)))
      (setf *c -1)
      (if (>= n (- total-nodes 1)) (- n (- total-nodes 1)) (pick-node (list tree -1) n))))

(defun gp-modifier (ind1 ind2)
  "Flips a coin.  If it's heads, then ind1 and ind2 are
crossed over using subtree crossover.  If it's tails, then
ind1 and ind2 are each mutated using subtree mutation, where
the size of the newly-generated subtrees is pickedc at random
from 1 to 10 inclusive.  Doesn't damage ind1 or ind2.  Returns
the two modified versions as a list."
  (let* ((a (copy-tree ind1)) (b (copy-tree ind2))
         (c (nth-subtree-parent a (random (num-nodes a))))
         (d (nth-subtree-parent b (random (num-nodes b))))
         (a-parent) (a-child) (b-parent) (b-child) (temp))
    
    (if(consp c) 
      (progn
        (setf a-parent (elt c 0))
        (setf a-child (+ 1 (elt c 1)))))

    (if(consp d)
      (progn
        (setf b-parent (elt d 0))
        (setf b-child (+ 1 (elt d 1)))))
   
    (if(random?)       
      (if(and (consp c) (consp d))
        (progn  
          (setf temp (elt a-parent a-child))
          (setf (elt a-parent a-child) (elt b-parent b-child))
          (setf (elt b-parent b-child) temp)))
      (progn
        (if(consp c)
          (setf (elt a-parent a-child) (ptc2 (+ 1 (random *mutation-size*)))))
        (if(consp d)
          (setf (elt b-parent b-child) (ptc2 (+ 1 (random *mutation-size*)))))))   
    (list a b)))







;;; SYMBOLIC REGRESSION
;;; This problem domain is similar, more or less, to the GP example in
;;; the lecture notes.  Your goal is to make a symbolic expression which
;;; represents a mathematical function consisting of SIN COS, EXP,
;;; +, -, *, and % (a version of / which doesn't give an error on
;;; divide-by-zero).  And also the function X, which returns a current
;;; value for the X variable.
;;;
;;; In symbolic regression, we generate 20 (x,y) pairs produced at
;;; random which fit the expression y = x^4 + x^3 + x^2 + x.  You can
;;; make up another function is you like, but that's the one we're going
;;; with here.  Using just these data pairs, the GP system will attempt
;;; to ascertain the function.  It will generate possible expressions
;;; as its individuals, and the fitness of an expression is how closely,
;;; for each X value, it gives the correct corresponding Y value.
;;;
;;; This is called SYMBOLIC regression because we're actually learning
;;; the mathematical expression itself, including transcendental functions
;;; like SIN and COS and E^.  As opposed to statistical linear or
;;; quadratic curve-fitting regressions which just try to learn the
;;; linear or quadratic parameters etc.
;;;
;;; An example 100% optimal solution:
;;;
;;; (+ (* (x) (* (+ (x) (* (x) (x))) (x))) (* (+ (x) (cos (- (x) (x)))) (x)))



;;; GP SYMBOLIC REGRESSION SETUP
;;; (I provide this for you)

(defparameter *num-vals* 20)
(defparameter *vals* nil) ;; gets set in gp-setup

(defun gp-symbolic-regression-setup ()
  "Defines the function sets, and sets up vals"

  (setq *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
  (setq *terminal-set* '(x))

  (setq *vals* nil)
  (dotimes (v *num-vals*)
    (push (1- (random 2.0)) *vals*))
  (setf *best-ind* nil)
  (setf *best-fitness* -100000))

(defun poly-to-learn (x) (+ (* x x x x) (* x x x) (* x x) x))

;; define the function set
(defparameter *x* nil) ;; to be set in gp-evaluator
(defun x () *x*)
(defun % (x y) (if (= y 0) 0 (/ x y)))  ;; "protected division"
;;; the rest of the functions are standard Lisp functions




;;; GP SYMBOLIC REGRESSION EVALUATION

;(cos (+ (x) (- (x) (x)) (sin (x))))


(defun gp-symbolic-regression-evaluator (ind)
  "Evaluates an individual by setting *x* to each of the
elements in *vals* in turn, then running the individual and
get the output minus (poly-to-learn *x*).  Take the
absolute value of the this difference.  The sum of all such
absolute values over all *vals* is the 'raw-fitness' Z.  From
this we compute the individual's fitness as 1 / (1 + z) -- thus
large values of Z are low fitness.  Return the final
individual's fitness.  During evaluation, the expressions
evaluated may overflow or underflow, or produce NaN.  Handle all
such math errors by
returning most-positive-fixnum as the output of that expression."

  (let ((z 0))
   (dolist (a *vals*)
    (setf *x* a)
    (setf z (+ z (abs (- (poly-to-learn *x*) (handler-case (eval ind) (error(condition) (format t "~%Warning, ~a" condition) most-positive-fixnum)))))))
  (/ 1 (+ 1 z))))


;;; Example run
#|
(evolve 50 500
	:setup #'gp-symbolic-regression-setup
	:creator #'gp-creator
	:selector #'tournament-selector
	:modifier #'gp-modifier
        :evaluator #'gp-symbolic-regression-evaluator
	:printer #'simple-printer)
|#





;;; GP ARTIFICIAL ANT CODE

;;; In the artificial ant problem domain, you'll be evolving an s-expression
;;; which moves an ant around a toroidal map shown below.  The ant starts out
;;; at (0,0), facing east (to the right).  The functions in
;;; the expression are:
;;; (if-food-ahead --then-- --else--)   If food is directly ahead of the ant,
;;;                                     then evaluate the THEN expression, else
;;;                                     evaluate the ELSE expression
;;; (progn2 --item1-- --item2--)        Evaluate item1, then evaluate item 2
;;; (progn3 --item1-- --item2-- --item3--)  Evaluate item1, then item2, then item3
;;; (move)                              Move forward one unit
;;;                                     If you pass over a food pellet, it is eaten
;;;                                     and removed from the map.
;;; (left)                              Turn left
;;; (right)                             Turn right
;;;
;;;
;;; the way a tree is evaluated is as follows.  The whole tree is evaluated,
;;; and the functions move the ant about.  If the ant has not made a total of
;;; 600 MOVE, LEFT, and RIGHT operations yet, then the tree is evaluated AGAIN
;;; moving the ant some more from its current position.  This goes on until
;;; 600 operations have been completed, at which time the ant will not move
;;; any more.  If 600 operations are completed in the middle of the evaluation
;;; of a tree, the simplest approach is to have the MOVE command simply
;;; "stop working" so the ant doesn't gobble up any more pellets accidentally.

;;; The fitness of the artificial ant is the number of pellets it ate in the
;;; 600-operation period.  Maps are reset between evaluations of different
;;; individuals of course.

;;; Here's an optimal ant program (there are many such):
;;  (progn3 (if-food-ahead (move) (progn2 (left) (progn2 (left)
;;             (if-food-ahead (move) (right))))) (move) (right)))

;;; This is a hard problem for GP and you may need to run many times before you
;;; get a 100% perfect answer.



;;; our ant's food trail map
(defparameter *map-strs* '(
".###............................"
"...#............................"
"...#.....................###...."
"...#....................#....#.."
"...#....................#....#.."
"...####.#####........##........."
"............#................#.."
"............#.......#..........."
"............#.......#..........."
"............#.......#........#.."
"....................#..........."
"............#..................."
"............#................#.."
"............#.......#..........."
"............#.......#.....###..."
".................#.....#........"
"................................"
"............#..................."
"............#...#.......#......."
"............#...#..........#...."
"............#...#..............."
"............#...#..............."
"............#.............#....."
"............#..........#........"
"...##..#####....#..............."
".#..............#..............."
".#..............#..............."
".#......#######................."
".#.....#........................"
".......#........................"
"..####.........................."
"................................"))

(defparameter *map-height* 32)
(defparameter *map-width* 32)


;;; some useful functions for you


;; The four directions.  For relative direction, you might
;; assume that the ant always PERCEIVES things as if it were
;; facing north.
(defconstant *n* 0)
(defconstant *e* 1)
(defconstant *s* 2)
(defconstant *w* 3)

(defun make-map (lis)
  "Makes a map out of a string-list such as *MAP-STRS*"
  (let ((map (make-array (list (length (first lis)) (length lis)))))
    (dotimes (y (length lis) map)
      (dotimes (x (length (elt lis y)))
	(setf (aref map x y)
	            (cond ((equalp #\# (elt (elt lis y) x)) nil)
			      (t t)))))))

(defun direction-to-arrow (dir)
  "Returns a character which represents a given direction -- might
be useful for showing the movement along a path perhaps..."
  (cond ((= dir *n*) #\^)
	((= dir *s*) #\v)
	((= dir *e*) #\>)
	(t #\<)))

(defun maparray (function array &optional (same-type nil))
  "Maps function over array in its major order.  If SAME-TYPE,
then the new array will have the same element type as the old
array; but if a function returns an invalid element then an error
may occur.  If SAME-TYPE is NIL, then the array will accommodate
any type."
  (let ((new-array (apply #'make-array (array-dimensions array)
			    :element-type (if same-type
					          (array-element-type array)
					      t)
			      :adjustable (adjustable-array-p array)
			        (if (vectorp array)
				          `(:fill-pointer ,(fill-pointer array))
				      nil))))
    (dotimes (x (array-total-size array) new-array)
      (setf (row-major-aref new-array x)
	        (funcall function (row-major-aref array x))))))

(defun print-map (map)
  "Prints a map, which must be a 2D array.  If a value in the map
is T (indicating a space), then a '.' is printed.  If a value in the map
is NIL (indicating a food pellet), then a '#' is printed.  If a value in
the map is anything else, then it is simply PRINTed.  This allows you to
consider spaces to be all sorts of stuff in case you'd like to print a
trail of spaces on the map for example.  Returns NIL."
  (let ((dim (array-dimensions map)))
    (dotimes (y (second dim) nil)
      (format t "~%")
      (dotimes (x (first dim))
	(format t "~a"
		(let ((v (aref map x y)))
		    (cond ((equal v t) #\.)
			  ((null v) #\#)
			  (t v))))))))

(defmacro absolute-direction (relative-dir ant-dir)
  "If the ant is facing ANT-DIR, then converts the perceived
RELATIVE-DIR direction into an absolute ('true') direction
and returns that."
  `(mod (+ ,relative-dir ,ant-dir) 4))

(defmacro x-pos-at (x-pos absolute-dir &optional (steps 1))
  "Returns the new x position if one moved STEPS steps the absolute-dir
direction from the given x position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *n*) ,x-pos)         ;; n or s
	            ((= ,absolute-dir *e*) (+ ,x-pos ,steps))     ;; e
		          (t (+ ,x-pos (- ,steps) *map-width*)))         ;; w
	*map-width*))

(defmacro y-pos-at (y-pos absolute-dir &optional (steps 1))
  "Returns the new y position if onee moved STEPS steps in the absolute-dir
direction from the given y position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *e*) ,y-pos)        ;; e or w
	            ((= ,absolute-dir *s*) (+ ,y-pos ,steps))     ;; s
		          (t (+ ,y-pos (- ,steps) *map-height*)))       ;; n
	*map-height*))


(defparameter *current-move* 0 "The move # that the ant is at right now")
(defparameter *num-moves* 600 "How many moves the ant may make")
(defparameter *current-x-pos* 0 "The current X position of the ant")
(defparameter *current-y-pos* 0 "The current Y position of the ant")
(defparameter *current-ant-dir* *e* "The current drection the ant is facing")
(defparameter *eaten-pellets* 0 "How many pellets the ant has eaten so far")
(defparameter *map* (make-map *map-strs*) "The ant's map")




;;; the function set you have to implement

(defmacro if-food-ahead (then else)
  "If there is food directly ahead of the ant, then THEN is evaluated,
else ELSE is evaluated"
  ;; because this is an if/then statement, it MUST be implemented as a macro.
  `(if(equalp nil (aref *map* (x-pos-at *current-x-pos* *current-ant-dir*) (y-pos-at *current-y-pos* *current-ant-dir*))) ,then ,else))

(defun progn2 (arg1 arg2)
    "Evaluates arg1 and arg2 in succession, then returns the value of arg2"
    (eval arg1)
    (eval arg2)
    arg2)  ;; ...though in artificial ant, the return value isn't used ... 

(defun progn3 (arg1 arg2 arg3)
  "Evaluates arg1, arg2, and arg3 in succession, then returns the value of arg3"
  (eval arg1)
  (eval arg2)
  (eval arg3)
  arg3)  ;; ...though in artificial ant, the return value isn't used ...

(defun move ()
  "If the move count does not exceed *num-moves*, increments the move count
and moves the ant forward, consuming any pellet under the new square where the
ant is now.  Perhaps it might be nice to leave a little trail in the map showing
where the ant had gone."
    
    (if(< *current-move* *num-moves*)
      (progn 
      (incf *current-move*)
      (setf (aref *map* *current-x-pos* *current-y-pos*) (direction-to-arrow *current-ant-dir*))
      (setf *current-x-pos* (x-pos-at *current-x-pos* *current-ant-dir*))
      (setf *current-y-pos* (y-pos-at *current-y-pos* *current-ant-dir*))
      (if(equalp (aref *map* *current-x-pos* *current-y-pos*) NIL) 
        (progn
          (incf *eaten-pellets*)
          (setf (aref *map* *current-x-pos* *current-y-pos*) T))))))


(defun right ()
  "Increments the move count, and turns the ant left"
  (setf *current-ant-dir* (mod (+ 1 *current-ant-dir*) 4))
  (incf *current-move*))

(defun left ()
  "Increments the move count, and turns the ant right"
  (setf *current-ant-dir* (mod (+ 4 (- *current-ant-dir* 1)) 4))
  (incf *current-move*))

(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)

(defun gp-artificial-ant-setup ()
  "Sets up vals"
  (setq *nonterminal-set* '((if-food-ahead 2) (progn2 2) (progn3 3)))
  (setq *terminal-set* '(left right move))
  (setq *map* (make-map *map-strs*))
  (setq *current-move* 0)
  (setq *eaten-pellets* 0)
  (setf *best-ind* nil)
  (setf *best-fitness* -100000))


(defun gp-artificial-ant-evaluator (ind)
  "Evaluates an individual by putting it in a fresh map and letting it run
for *num-moves* moves.  The fitness is the number of pellets eaten -- thus
more pellets, higher (better) fitness."
  (let ()
    (setq *eaten-pellets* 0)
    (setq *current-move* 0)
    (setq *current-x-pos* 0)
    (setq *current-y-pos* 0)
    (setq *current-ant-dir* *e*)
    (setq *map* (make-map *map-strs*))
    (loop 
      until(>= *current-move* *num-moves*)
      do(eval ind))
    *eaten-pellets*))


#|
(evolve 50 500
	:setup #'gp-artificial-ant-setup
	:creator #'gp-creator
	:selector #'tournament-selector
	:modifier #'gp-modifier
        :evaluator #'gp-artificial-ant-evaluator
	:printer #'simple-printer)
|#
