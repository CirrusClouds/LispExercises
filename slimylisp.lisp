;; The syntax for defun is defun function (parameters) (describe)
(defun hello_world () (format t "hello world"))

;;Property lists use symbols to represent values
(defun make-cd (title artist rating blah)
  (list :title title :artist artist :rating rating :blah blah))

;;Defining global variables and using them
(defvar *db* nil)
(defun add-record (cd) (push cd *db*))

;;Aesthetic directives are disgustingly scary
(defun dump-db ()
  (dolist (cd *db*) ;;looping over elements db binding each to variable cd
    (format t "~{~a:~10t~a~%~}~%" cd)))

;;ok let's go over this last line...
;;first things first, ~a is the aesthetic directive- it takes an argument and presents it in a readable form
;;~t is for tabulating, it adds spaces. 10 = 10 spaces
;;~% means emit a new line
;;~{ ---- ~} structure means that the inside will be processed as a list, and that format will go over the list of elements and take as many elements as it needs before moving on... its essentially like do list


;;This function prompts for user input, takes it and prints it
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;;This is 

(defun lettingtest (z)
    (Let ((x 2) (y 2))
      (list x y z)))

;;Let computes all the variables at once, but Let* does them one at a time so by a series of variables you can do series of computations

(defparameter *male-names* '(John Barry Francis))
(defparameter *female-names* '(Barbara Johanna Lea Fran))

(defun give-title (name)
  (cond ((member name *male-names*) (cons 'Mr name))
	((member name *female-names*) (cons 'Ms name))
	(t (cons '(Mr or Ms) name))))

(defun right-side (listy)
  (member 'vs listy))
  
;;assoc function looks up an entry in a table, given its key, and returns the entry

(defparameter *words*
	'((one un)
	   (two deux)
	   (three trois)
	   (four quatre)
	  (five cinq)))
;;syntax: (assoc 'one words)


;;The substiute function (subst x y z) substitutes x for y in z

(defun hitobye () (subst 'bye 'hi '(well hi)))

;;The sublis function does the same but doing multiple substitutions at a time (also changes are in reverse order)
;;SAY SOMETHING I'M GIVING UP ON YOU

(defun rosesareviolets () (sublis '((roses . violets) (red . blue)) '(roses are red)))

;;Lambda and mapcar

(mapcar (lambda (n) (* n n)) '(1 2 3 4 5))

;;Find-if searches a list and returns the first element for which a predicate is true, if no elements meet the condition, nil is returned

(defun findingmorethan3 () (find-if (lambda (x) (> x 3)) '(2 4 6 7 8 9)))

;;Reduce reduced the elements of a list into a single result with a function

(defun additive () (reduce #'+ '(1 2 3)))

;;Often #' may be necessary

;;Every takes a predicate and a list, and returns T if there is no false response and nil if there is


;;Mapcar can act over multiple lists

(defun multicar () (mapcar #'(lambda (x y) (list x 'gets y))
			   '(fred wilma george diane)
			   '(job1 job2 job3 job4)))


;;~S rreplaces ~S with an argument

;;with-open-file reads a stream from a file
;;dotimes takes an index through a body and for each step evaluates its form

(defun printylooloo (n) (dotimes (i n)
			  (format t "~&I is ~S." i)))

;;dolist has the same syntax but steps the index variable through the elements of a lisp
;;Return ends the loop and gives a value

;;do is more complex. It defines values and then actions to be taken on the value on the next update, running through a loop. It can take multiple variables

(defun it-intersection (x y)
  (do ((x1 x (rest x1))
       (result nil (if (member (first x1) y)
		       (cons (first x1) result)
		       result)))
      ((null x1) result)))

;;Structures are objects with an arbitrary number of named components

(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))

;;my-vec creates arrays which do not work like lists. The aref function takes an array and an index and returns the corresponding element. It can also be used to change elements.
(defparameter my-vec '#(1 2 3 4))

;;make-array makes an array of the length of the first argument and then specifies the values of the element

(defun onesarray (n) (make-array n :initial-element 1))

;;initial contents takes a list and updates every element according to the elements of the list

(defun descendinglist (n) (if (equal n 0) (list n)
			      (cons n (descendinglist (- n 1)) )))

(defun descendingarray (n) (make-array n :initial-contents (descendinglist (- n 1))))

(defun ascendinglist (n) (reverse
			  (descendinglist n)))
       
(defun range (a b) (if (equal a b)
			 (list b)
		         (cons a (range (+ 1 a) b))))

;;cons doesn't like it when we put a function before a value, but we can actually make an ascendinglist another way, using label and rec

(defun ascendinglistproper (n)
  (labels ((rec (x)
	     (if (equal x n)
		 (list n)
		 (cons x (rec (+ 1 x))))))
    (rec 0)))

;;wow! it allows you to do stuff with the function you made in the other function!

;;Ok, here come macros

(defmacro simple-incf (var)
  (list 'setq var (list '+ var 1)))

;;ppmx traces macro expansions

;;let's change our macro to have optional arguments to change the increments

(defmacro simple-incf2 (var &optional (amount 1))
  (list 'setq var (list '+ var amount)))


(defparameter *a* 3)
(defparameter *b* 3)

;;Always put* around your variables
;;commas evaluate things within backquoted lists

(defmacro simple-incf3 (var &optional (amount 1))
  `(setq ,var (+ ,var ,amount)))

(defmacro set-mutual (var1 var2)
  `(setq ,var1 ',var2)
  `(setq ,var2 ',var1))


(defparameter *c* 2)
(defparameter *d* 3)

;;It seems like macros take variables' values AND names whereas functions only take names
;;If a list element is preceded by ,@ then it will be spliced in, not just appear as a list

(defmacro mix-and-match (p q)
  (let ((x1 (first p))
	(x2 (second p))
	(y1 (first q))
	(y2 (second q)))
    `(list '(,x1 ,y1)
	   '(,x2 ,y1)
	   '(,x1 ,y2)
	   '(,x2 ,y2))))

;;We can actually destructure p and q from the get-go in our arguments

;; Supposing you have a function contained in a variable and you call it in a function, you must call it with funcall. you can't simply call the variable.

(defun fnk (x) (> x 3))

(defun our-remove-if (fn xs)
  (if (null xs)
      nil
      (if (funcall fn (car xs))
	  (our-remove-if fn (cdr xs))
	  (cons (car xs) (our-remove-if fn (cdr xs))))))

;;If we wanted to write a function wwhich takes a list of numbers and added a set amount to them we could use

(defun list+ (xs n)
  (mapcar #'(lambda (x) (+ x n))
	  xs))

;; Labels allows us to write a recursive function in a function whilst also taking local variables

(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
	     (if (consp lst)
		 (+ (if (eq (car lst) obj) 1 0)
		    (instances-in (cdr lst)))
		 0)))
    (mapcar #'instances-in lsts)))


(defun count-instances2 (obj lsts)
  (labels ((instances-in (lst)
	     (cond ((consp lst)
		 (+ (if (eq (car lst) obj) 1 0)
		    (instances-in (cdr lst))))
		   ((eq lst obj) 1)
		   (t 0))))
    (reduce #'+ (mapcar #'instances-in lsts))
    ))

(defun good-reverse (lst)
  (labels ((rev (lst acc)
	     (if (null lst)
		 acc
		 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

;;You can (reverse (lst)) but it doesn't change lst, it simply creates a new variable. For that you have to set variables.

(defun split-at (lst n)
  (cond ((> n (length lst))
	 (error "too long for list"))
	(T
	 (nthcdr n lst))))

;;butlast returns a copy of a list after the (optional nth) conses have been omitted

(defun afewlist (lst n)
  (butlast lst n))

;;Map functions

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))


;; m+ as opposed to calling the whole + function is a good way to ensure map works. Lambda functions are also a way.

;;Using data structures to create a binary tree to play twenty questions

;;(defstruct node contents yes no)

;;(defparameter *nodes* (make-hash-table))
;;
;;(defun run-node (name)
  ;;(let ((n (gethash name *nodes*)))
    ;;(cond ((node-yes n)
;;	   (format t "~A~%>> " (node-contents n))
;;	   (case (read)
;;	     (yes (run-node (node-yes n)))
;;	     (t
;;	      (run-node (node-no n)))))
;;	  (t (node-contents n)))))

;;(defun defnode (name conts &optional yes no)
 ;; (setf (gethash name *nodes*)
;;	(if yes
;;	    (lambda ()
;;		(format t "~A~%>> " conts)
;;		(case (read)
;;		  (yes (funcall (gethash yes *nodes*)))
;;		  (t
;;		   (funcall (gethash no *nodes*)))))
;;	      (lambda () conts))))



(defvar *nodes* nil)

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
	nil
	(let ((conts (second node))
	      (yes (third node))
	      (no (fourth node)))
	  (if yes
	      (let ((yes-fn (compile-net yes))
		    (no-fn (compile-net no)))
		#'(lambda ()
		    (format t "~A~%>> " conts)

		    (funcall (if (eq (read) 'yes)
				 yes-fn
				 no-fn))))
	      #'(lambda () conts))))))


(defnode 'people "is they a mman" 'male 'female)
(defnode 'male 'lincoln)
(defnode 'female 'lincolnbutgirl)

;;Hint: use compile-net to check the code

(defmacro nil! (var)
  `(setq ,var ,nil))

;;Important to note here that nil is a self-evaluating constant and a symbol, and therefore 'nil is just nil, and `(,nil) is also just nil


;;This macro creates a three-way if that 
(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

;;A macro for testing macroexpansions. U work it oout bitch

(defmacro mace (expr)
  `(pprint (macroexpand-1 ',expr)))

;;Defining a simplified dolist as a macro
(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapcar #'(lambda (,var) ,@body)
	   ,list)
     (let ((,var nil))
       ,result)))


;;This is broken for now
;;(defmacro our-do (((init step) (condition (init step) result)) &body body (init step))
;;   `(if ((lambda (,init ,step) ,@condition))
;;	 (let ((,init nil) (,step nil)) (,result))
;;	 ((lambda (,init ,step) ,@body))
;;	 (our-do ((((+ step init) step) (condition result)) &body body))))

;;Suppose we wanted to create a macro for cond
(defmacro our-cond (condevals)
  (let ((lasty (last condevals)))
    (reduce
     (lambda (conny1 conny2)
       `(if ,(car conny1)
	    ,(cdr conny1)
	    ,conny2
	    ))
     (butlast condevals) :from-end t
     :initial-value `(if ,(car lasty)
			 ,(cdr lasty)))))
		     



;;defining our own do

(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
	      (if (consp b)
		  (list (car b) (cadr b))
		  (list b nil)))
	  bindforms))

(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
	      (if (and (consp b) (third b))
		  (list (car b) (third b))
		  nil))
	  bindforms))

(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
	,label
	(if ,test
	    (return (progn ,@result)))
	,@body
	(psetq ,@(make-stepforms bindforms))
	(go ,label))))
	    
;;The and macro (and a b c) is equivalent to (if a (if b c))
;;As such, we can define the and macro as follows

(defmacro our-and (&rest args)
  (cond ((eq (length args) 0) t)  
	((eq (length args) 1) (car args))
	(t `(if ,(car args)
		(our-and ,@(cdr args))))))

;;Using case we can make this code less repetitive

;;Defining let

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
		    (if (consp x) (car x) x))
		binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
		  (if (consp x) (cadr x) nil))
	      binds)))

;;I thought this was an interesting let structure. Gensyms are useful when we want to create a symbol the user can't input so that they're not confused for one another by the interpreter

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s)
		     `(,s (gensym)))
                 syms)
     ,@body))


;;Remember how do* evaluates binds and rebinds in sequence rather than parallel?
;;We can build a particular macro that does do* but binds multiple variables in one initial clause
;;This could be useful for coordinates or games

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
            ,label
            (if ,(car test)
                (return (progn ,@(cdr test))))
            ,@body
            ,@(mvdo-rebind-gen rebinds)
            (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t
         (cons (list (if (atom (caar rebinds))
                         'setq
                         'multiple-value-setq)
                     (caar rebinds)
                     (third (car rebinds)))
               (mvdo-rebind-gen (cdr rebinds))))))

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

;;Defining reduce
;;This is dumb

(defun wrong-foldr (fn1 listvar)
  (cond ((null listvar) nil)
        ((not (cdr listvar)) (car listvar))
        (t
         (funcall fn1 (car listvar) (wrong-foldr fn1 (cdr listvar))))))

(defun wrong-foldl (fn2 listlol)
  (let ((listy (reverse listlol)))
    (cond ((null listy) nil)
          ((not (cdr listy)) (car listy))
          (t
           (funcall fn2 (wrong-foldl fn2 (reverse (cdr listy))) (car listy))))))
  

(defun wrong-reduce (fn3 listman &key (from-end nil))
  (if from-end
             (wrong-foldr fn3 listman))
             (wrong-foldl fn3 listman))
  

;;This is correct

(defun foldr (fn acc xs)
  (if (null xs) acc
      (funcall fn (car xs) (foldr fn acc (cdr xs)))))  
  
(defun foldl (fn acc xs)
  (if (null xs) acc
      (foldl fn (funcall fn acc (car xs)) (cdr xs))))

(defun our-reduce (fn acc xs &key (from-end nil))
  (if from-end
      (foldr fn acc xs)
      (foldl fn acc xs)))

;;99 Lisp Problems

;;1

(defun our-lasty (list)
  (cond  ((null list) nil)
	 ((not (cdr list)) list)
	 (t
	  (our-lasty (cdr list)))))

;;2
(defun our-butlast (list)
  (cond ((null list) nil)
	((not (cdr list)) nil)
	((not (cdr (cdr list))) (car list))
	(t
	 (our-butlast (cdr list)))))
      
;;3

(defun our-element-at (list n)
  (cond ((null list) nil)
	((> n (length list)) nil)
	((= n 0) (car list)) 
	(t
	 (our-element-at (cdr list) (- n 1)))))

;;4

(defun our-lengt (list)
  (if (null list)
      nil
      (+ 1 (our-lengt (rest list)))))

;;5 You have to put a nil as acc. That's just how the question solved it. I think there's a better version.

(defun our-reverso (list acc)
  (if (null list)
      acc
      (our-reverso (cdr list) (cons (car list) acc))))

;;6

(defun palinquestion (list)
  (equal list (reverse list)))


;;7 my attempt

;;(defun flatten (lst acc)
;;  (if (null lst)
;;      acc
;;      ((if (listp (car lst))
;;	   (flatten (car lst) acc))
;;      (flatten (cdr list) (cons acc (car list))))))

;;7 Answer

(defun our-flatten (lsst)
  (if (null lsst)
      nil
      (let ((elem (car lsst)) (resto (cdr lsst)))
	(if (listp elem)
	    (append (our-flatten elem) (our-flatten resto))
	    (append (cons elem nil) (our-flatten resto))))))


;;8

(defun compress (xs)
  (cond ((null xs) nil)
	((not (cdr xs)) xs)
	((eq (car xs) (car (cdr xs))) (compress (cdr xs)))
	(t
	 (cons (car xs) (compress (cdr xs)))))) 


;;Some utilities

(defun nest-reverse (xs acc)
  (cond ((null xs) acc)
        ((consp (car xs))
         (nest-reverse (cdr xs) (cons (nest-reverse (car xs) nil) acc)))
        (t
         (nest-reverse (cdr xs) (cons (car xs) acc)))))


(defun nest-first (xs)
  (cond ((null xs) nil)
	(t
	 (if (consp (car xs))
	     (nest-first (car xs))
	     (car xs)))))

(defun nest-last (xs)
  (cond ((null xs) xs)
	((not (cdr xs))
	 (if (consp (car xs))
	     (nest-last (car xs))
	     xs))
	(t
	 (nest-last (cdr xs)))))
	       
	
(defun nestmap-reverse (xs)
  (reverse (mapcar (lambda (x)
		     (if (consp x)
			 (nestmap-reverse x)
			 x))
		   xs)))

