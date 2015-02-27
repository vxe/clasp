(cl:in-package #:clasp-cleavir)


(defvar *debug-cleavir* nil)

;;; The first argument to this function is an instruction that has a
;;; single successor.  Whether a GO is required at the end of this
;;; function is determined by the code layout algorithm.  
;;; 
;;; The inputs are forms to be evaluated.  The outputs are symbols
;;; that are names of variables.
(defgeneric translate-simple-instruction (instruction inputs outputs abi))

(defgeneric translate-branch-instruction (instruction inputs outputs successors abi))





(defvar *basic-blocks*)
(defvar *ownerships*)
(defvar *tags*)
(defvar *vars*)

(defvar *debug-basic-blocks*)
(defvar *debug-ownerships*)
(defvar *debug-tags*)
(defvar *debug-vars*)

(defun translate-datum (datum)
  (if (typep datum 'cleavir-ir:constant-input)
      (let* ((value (cleavir-ir:value datum))
	     (ltv-index (cmp:codegen-literal nil value nil))
	     (ltv-ref (cmp:irc-intrinsic "cc_lookupLoadTimeReference" cmp:*load-time-value-holder-global-var* (%size_t ltv-index))))
	ltv-ref)
      (let ((var (gethash datum *vars*)))
	(when (null var)
	  (cond
	    ((typep datum 'cleavir-ir:values-location)
	     (warn "What do we do in translate-datum with cleavir-ir:values-location??????"))
	    ((typep datum 'cleavir-ir:immediate-input)
	     (setf var (cmp:jit-constant-size_t (cleavir-ir:value datum))))
	    ((typep datum 'cc-mir:closure-pointer-dynamic-lexical-location)
	     (setf var (llvm-sys:create-alloca cmp:*irbuilder* cmp:+i8*+ (cmp:jit-constant-i32 1) (string (cleavir-ir:name datum)))))
	    ((typep datum 'cleavir-ir:dynamic-lexical-location)
	     (setf var (llvm-sys:create-alloca cmp:*irbuilder* cmp:+t*+ (cmp:jit-constant-i32 1) (string (cleavir-ir:name datum)))))
	    ((typep datum 'cleavir-ir:static-lexical-location)
	     (setf var (llvm-sys:create-alloca cmp:*irbuilder* cmp:+t*+ (cmp:jit-constant-i32 1) (string (cleavir-ir:name datum)))))
	    #+(or)((typep datum 'cleavir-ir:load-time-value-input)
		   (format t "load-time-value-input - what does the datum look like: ~a~%" datum)
		   (warn "Get the load-time-value-input and setf var"))
	    (t (error "translate datum: ~a~%" datum))))
	(setf (gethash datum *vars*) var)
	var)))

(defun translate-lambda-list-item (item)
  (cond ((symbolp item)
	 item)
	((consp item)
	 (ecase (length item)
	   (2 (list (translate-datum (first item))
		    nil
		    (translate-datum (second item))))
	   (3 (list (list (first item)
			  (translate-datum (second item)))
		    nil
		    (translate-datum (third item))))))
	(t
	 (translate-datum item))))

(defun translate-lambda-list (lambda-list)
  (mapcar #'translate-lambda-list-item lambda-list))

(defun layout-basic-block (basic-block abi)
  (destructuring-bind (first last owner) basic-block
    (declare (ignore owner))
    (append (loop for instruction = first
	       then (first (cleavir-ir:successors instruction))
	       for inputs = (cleavir-ir:inputs instruction)
	       for input-vars = (mapcar #'translate-datum inputs)
	       for outputs = (cleavir-ir:outputs instruction)
	       for output-vars = (mapcar #'translate-datum outputs)
	       until (eq instruction last)
	       collect (translate-simple-instruction
			instruction input-vars output-vars abi))
	    (let* ((inputs (cleavir-ir:inputs last))
		   (input-vars (mapcar #'translate-datum inputs))
		   (outputs (cleavir-ir:outputs last))
		   (output-vars (mapcar #'translate-datum outputs))
		   (successors (cleavir-ir:successors last))
		   (successor-tags (loop for successor in successors
				      collect (gethash successor *tags*))))
	      (if (= (length successors) 1)
		  (list (translate-simple-instruction
			 last input-vars output-vars abi)
			(if (typep (second basic-block) 'cleavir-ir:unwind-instruction)
			    (cmp:irc-unreachable)
			    (cmp:irc-br (gethash (first successors) *tags*))))
		  (list (translate-branch-instruction
			 last input-vars output-vars successor-tags abi)))))))

(defun layout-procedure (initial-instruction abi)
  ;; I think this removes every basic-block that
  ;; isn't owned by this initial-instruction
  (let* ((clasp-cleavir-ast-to-hir:*landing-pad* nil)
	 (basic-blocks (remove initial-instruction
			       *basic-blocks*
			       :test-not #'eq :key #'third))
	 ;; Hypothesis: This finds the first basic block
	 (first (find initial-instruction basic-blocks
		      :test #'eq :key #'first))
	 ;; This gathers the rest of the basic blocks
	 (rest (remove first basic-blocks :test #'eq)))
    ;; HYPOTHESIS: This builds a function with no arguments
    ;; that will enclose and set up other functions with arguments
    (let* ((main-fn-name "REPL")
	   (cmp:*current-function-name* main-fn-name)
	   (cmp:*gv-current-function-name* (cmp:jit-make-global-string-ptr cmp:*current-function-name* "fn-name"))
	   (fn (llvm-sys:function-create
		cmp:+fn-prototype+
		'llvm-sys:internal-linkage
		(cmp:jit-function-name cmp:*current-function-name*)
		cmp:*the-module*))
	   (cmp:*current-function* fn)
	   (block (cmp:irc-basic-block-create "entry" fn))
	   (*current-function-entry-basic-block* block)
	   (*function-current-multiple-value-array-address* nil)
	   (irbuilder (llvm-sys:make-irbuilder cmp:*llvm-context*)))
      (let ((args (llvm-sys:get-argument-list fn)))
	(mapcar #'(lambda (arg argname) (llvm-sys:set-name arg argname))
		(llvm-sys:get-argument-list fn) cmp:+fn-prototype-argument-names+)
	;; Set the first argument attribute to be sret
	(if args
	    (let ((attribute-set (llvm-sys:attribute-set-get cmp:*llvm-context* 1 (list 'llvm-sys:attribute-struct-ret))))
	      (llvm-sys:add-attr (first args) attribute-set))))
      ;; Create a basic-block for every remaining tag
      (loop for block in rest
	 for instruction = (first block)
	 do (progn
	      (format t "Creating basic block for rest instruction: ~a~%" instruction)
	      (setf (gethash instruction *tags*) (cmp:irc-basic-block-create "tag"))))
      (llvm-sys:set-insert-point-basic-block irbuilder block)
      ;; HYPOTHESIS: bind variables for every var owned by this
      ;; initial instruction and that are NOT outputs of the initial
      ;; instruction (passed arguments)  I think I should use passed arguments
      ;; to create allocas for them.
      (cmp:with-irbuilder (irbuilder)
	(cmp:with-dbg-function ("repl-FIX"
				:linkage-name "repl-FIX-LINKAGE-NAME"
				:function fn
				:function-type cmp:+fn-prototype+
				:form *form*)
	  (cmp:with-dbg-lexical-block (*form*)
	    (loop for var being each hash-key of *ownerships*
	       using (hash-value owner)
	       when (and (typep var '(or
				      cleavir-ir:lexical-location
				      cleavir-ir:values-location))
			 (eq owner initial-instruction)
			 #+(or)(not (member var (cleavir-ir:outputs
						 initial-instruction))))
	       collect (translate-datum var))
	    (layout-basic-block first abi)
	    (loop for block in rest
	       for instruction = (first block)
	       do (progn
		    (format t "Laying out basic block: ~a~%" block)
		    (format t "Inserting basic block for instruction: ~a~%" instruction)
		    (cmp:irc-begin-block (gethash instruction *tags*))
		    (layout-basic-block block abi)))
	    )))
      (values fn :function-kind nil main-fn-name))))

(defun translate (initial-instruction abi)
  (let* ((ownerships
	  (cleavir-hir-transformations:compute-ownerships initial-instruction)))
    (let*((*ownerships* ownerships)
	  (*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
	  (*tags* (make-hash-table :test #'eq))
	  (*vars* (make-hash-table :test #'eq)))
      (setf *debug-basic-blocks* *basic-blocks*
	    *debug-ownerships* *ownerships*
	    *debug-tags* *tags*
	    *debug-vars* *vars*)
            (layout-procedure initial-instruction abi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.


(defmethod translate-simple-instruction
    ((instr cc-mir:enter-instruction) inputs outputs (abi abi-x86-64))
  (let* ((fn-args (llvm-sys:get-argument-list cmp:*current-function*))
	 (closed-env-arg (second fn-args))
	 (closed-env-dest (first outputs))
	 (calling-convention (make-instance 'calling-convention
					    :nargs (third fn-args)
					    :register-args (nthcdr 3 fn-args))))
    (llvm-sys:create-store cmp:*irbuilder* closed-env-arg closed-env-dest nil)
    #+(or)(format t " fn-args: ~a~%" fn-args)
    (let* ((lambda-list (cleavir-ir:lambda-list instr))
	   (static-environment-output (first (cleavir-ir:outputs instr)))
	   (args (cdr (cleavir-ir:outputs instr)))
	   (landing-pad (cc-mir:landing-pad instr)))
      (when landing-pad
	(let ((exn.slot (llvm-sys:create-alloca cmp:*irbuilder* cmp:+i8*+ (%i32 1) "exn.slot"))
	      (ehselector.slot (llvm-sys:create-alloca cmp:*irbuilder* cmp:+i32+ (%i32 1) "ehselector.slot")))
	  (setf (clasp-cleavir:basic-block landing-pad) (clasp-cleavir:create-landing-pad exn.slot ehselector.slot landing-pad *tags*))))
      #+(or)(progn
	      (format t "    outputs: ~s~%" args)
	      (format t "translated outputs: ~s~%" (mapcar (lambda (x) (translate-datum x)) args))
	      (format t "lambda-list: ~a~%" lambda-list))
      (compile-lambda-list-code lambda-list args calling-convention))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:instruction) inputs outputs abi)
  (error "Implement instruction: ~a for abi: ~a~%" instruction abi)
  (format t "--------------- translate-simple-instruction ~a~%" instruction)
  (format t "    inputs: ~a~%" inputs)
  (format t "    outputs: ~a~%" outputs))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) inputs outputs abi)
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction assignment-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (let ((load (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "tmp")))
    (llvm-sys:create-store cmp:*irbuilder* load (first outputs) nil)))


(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-symbol-instruction) inputs outputs abi)
  (let ((array (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "array"))
	(idx (second inputs)))
    (let ((result (cmp:irc-intrinsic "cc_precalcSymbol" array idx)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-value-instruction) inputs outputs abi)
  (let ((array (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "array"))
	(idx (second inputs)))
    (let ((result (cmp:irc-intrinsic "cc_precalcValue" array idx)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction) inputs outputs (abi abi-x86-64))
  ;; Write the first return value into the result
  (format t "About to with-return-values~%")
  (with-return-values (return-values abi)
    (format t "About to store length~%")
    (cmp:irc-store (%size_t (length inputs)) (number-of-return-values return-values))
    (format t "About to copy inputs into mv~%")
    (dotimes (i (length inputs))
      (cmp:irc-store (cmp:irc-load (elt inputs i)) (return-value-elt return-values i)))))
      
(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) inputs outputs (abi abi-x86-64))
  ;; Create a basic block for each output
  (with-return-values (return-vals abi)
    (let* ((blocks (let (b) (dotimes (i (1+ (length outputs))) (push (cmp:irc-basic-block-create (format nil "mvn~a-" i) nil) b)) (nreverse b)))
	   (final-block (cmp:irc-basic-block-create "mvn-final" nil))
	   (switch (cmp:irc-switch (cmp:irc-load (number-of-return-values return-vals)) (car (last blocks)) (length blocks))))
      (dotimes (n (length blocks))
	(let ((block (elt blocks n)))
	  (cmp:irc-begin-block block)
	  (llvm-sys:add-case switch (%size_t n) block)
	  (dotimes (i (length outputs))
	    (if (< i n)
		(cmp:irc-store (cmp:irc-load (return-value-elt return-vals i)) (elt outputs i))
		(cmp:irc-store (%nil) (elt outputs i))))
	  (cmp:irc-br final-block)))
      (cmp:irc-begin-block final-block))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs (abi abi-x86-64))
  #+(or)(progn
    (format t "--------------- translate-simple-instruction funcall-instruction~%")
    (format t "    inputs: ~a~%" inputs)
    (format t "    outputs: ~a~%" outputs))
  (apply-closure "cc_call" (first inputs) (cdr inputs) abi))


(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-instruction) inputs outputs (abi abi-x86-64))
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction funcall-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (let* ((lpad (clasp-cleavir:landing-pad instruction)))
    (cmp:with-landing-pad (clasp-cleavir:basic-block lpad)
      (apply-closure "cc_invoke" (first inputs) (cdr inputs) abi))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) inputs outputs abi)
  (llvm-sys:create-int-to-ptr cmp:*irbuilder* (cmp:jit-constant-size_t cmp:+nil-value+) cmp:+t*+ "nil"))



(defmethod translate-simple-instruction
    ((instruction cc-mir:indexed-unwind-instruction) inputs outputs abi)
  (cmp:irc-intrinsic "cc_throwDynamicGo" 
		     (%size_t (cc-mir:landing-pad-id instruction))
		     (%size_t (cc-mir:jump-id instruction))))



(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) inputs outputs abi)
  (let ((result (cmp:irc-intrinsic "cc_makeCell")))
    (cmp:irc-store result (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction) inputs outputs abi)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell"))
	(val (llvm-sys:create-load-value-twine cmp:*irbuilder* (second inputs) "val")))
    (cmp:irc-intrinsic "cc_writeCell" cell val)))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction) inputs outputs abi)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell")))
    (let ((result (cmp:irc-intrinsic "cc_readCell" cell)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction) inputs outputs abi)
  (let ((env (cmp:irc-load (first inputs) "env"))
	(idx (second inputs)))
    (let ((result (cmp:irc-intrinsic "cc_fetch" env idx)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) inputs outputs abi)
  (let ((cell (cmp:irc-load (first inputs) "func-name")))
    (let ((result (cmp:irc-intrinsic "cc_fdefinition" cell)))
      (cmp:irc-store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:symbol-value-instruction) inputs outputs abi)
  (let ((sym (cmp:irc-load (first inputs) "sym-name")))
    (let ((result (cmp:irc-intrinsic "cc_symbolValue" sym)))
      (cmp:irc-store result (first outputs)))))



(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) inputs outputs abi)
  (declare (ignore inputs))
  (let* ((enter-instruction (cleavir-ir:code instruction))
	 (enclosed-function (layout-procedure enter-instruction abi)))
    #+(or)(push enclosed-function *functions-to-finalize*)
    #+(or)(progn
	    (warn "------- Implement enclose-instruction: ~a~%" instruction)
	    (format t "   enter-instruction: ~a~%" enter-instruction)
	    (format t "   enclosed-function: ~a~%" enclosed-function)
	    (format t "    inputs: ~a~%" inputs)
	    (format t "    outputs: ~a~%" outputs))
    (let* ((loaded-inputs (mapcar (lambda (x) (cmp:irc-load x "cell")) inputs))
	   (result (cmp:irc-intrinsic-args "cc_enclose" (list* enclosed-function (%size_t (length inputs)) loaded-inputs))))
      (cmp:irc-store result (first outputs) nil))))



#+(or)(progn

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:multiple-value-call-instruction) inputs outputs)
	  `(setf ,(first outputs)
		 (multiple-value-list
		  (funcall ,(first inputs)
			   (append ,@(rest inputs))))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:tailcall-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(return (funcall ,(first inputs) ,@(rest inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:the-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(unless (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
	     (error 'type-error
		    :expected-type ',(cleavir-ir:value-type instruction)
		    :datum ,(first inputs))))


	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:car-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (car ,(first inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:cdr-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (cdr ,(first inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:rplaca-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(rplaca ,(first inputs) ,(second inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:rplacd-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(rplacd ,(first inputs) ,(second inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:t-aref-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (row-major-aref ,(first inputs) ,(second inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:bit-aref-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (row-major-aref ,(first inputs) ,(second inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:unsigned-byte-8-aref-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (row-major-aref ,(first inputs) ,(second inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:short-float-aref-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (row-major-aref ,(first inputs) ,(second inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:single-float-aref-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (row-major-aref ,(first inputs) ,(second inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:double-float-aref-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (row-major-aref ,(first inputs) ,(second inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:long-float-aref-instruction) inputs outputs)
	  `(setq ,(first outputs)
		 (row-major-aref ,(first inputs) ,(second inputs))))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:t-aset-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(setf (row-major-aref ,(first inputs) ,(second inputs))
		 ,(third inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:bit-aset-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(setf (row-major-aref ,(first inputs) ,(second inputs))
		 ,(third inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:unsigned-byte-8-aset-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(setf (row-major-aref ,(first inputs) ,(second inputs))
		 ,(third inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:short-float-aset-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(setf (row-major-aref ,(first inputs) ,(second inputs))
		 ,(third inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:single-float-aset-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(setf (row-major-aref ,(first inputs) ,(second inputs))
		 ,(third inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:double-float-aset-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(setf (row-major-aref ,(first inputs) ,(second inputs))
		 ,(third inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:long-float-aset-instruction) inputs outputs)
	  (declare (ignore outputs))
	  `(setf (row-major-aref ,(first inputs) ,(second inputs))
		 ,(third inputs)))

	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:multiple-to-fixed-instruction) inputs outputs)
	  (let ((temp (gensym)))
	    `(let ((,temp ,(first inputs)))
	       (declare (ignorable ,temp))
	       ,@(loop for output in outputs
		    collect `(setf ,output (pop ,temp))))))
	
	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:unwind-instruction) inputs outputs)
	  (gensym))
	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) inputs outputs successors abi)
  (let ((ceq (cmp:irc-icmp-eq (cmp:irc-load (first inputs)) (cmp:irc-load (second inputs)))))
    (cmp:irc-cond-br ceq (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) inputs outputs successors abi)
  (declare (ignore successors))
  (cmp:irc-ret-void))

#+(or)(progn
	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:typeq-instruction) inputs outputs successors abi)
	  `(if (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
	       (go ,(second successors))
	       (go ,(first successors))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-add-instruction) inputs outputs successors)
	  (let ((result (gensym)))
	    `(let ((,result (+ ,(first inputs) ,(second inputs))))
	       (cond ((typep result 'fixnum)
		      (setq ,(first outputs) ,result)
		      (go ,(first successors)))
		     ((plusp ,result)
		      (setq ,(first outputs)
			    (+ ,result (* 2 most-negative-fixnum)))
		      (go ,(second successors)))
		     (t
		      (setq ,(first outputs)
			    (- ,result (* 2 most-negative-fixnum)))
		      (go ,(second successors)))))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-sub-instruction) inputs outputs successors)
	  (let ((result (gensym)))
	    `(let ((,result (- ,(first inputs) ,(second inputs))))
	       (cond ((typep result 'fixnum)
		      (setq ,(first outputs) ,result)
		      (go ,(first successors)))
		     ((plusp ,result)
		      (setq ,(first outputs)
			    (+ ,result (* 2 most-negative-fixnum)))
		      (go ,(second successors)))
		     (t
		      (setq ,(first outputs)
			    (- ,result (* 2 most-negative-fixnum)))
		      (go ,(second successors)))))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-less-instruction) inputs outputs successors)
	  (declare (ignore outputs))
	  `(if (< ,(first inputs) ,(second inputs))
	       (go ,(first successors))
	       (go ,(second successors))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-not-greater-instruction) inputs outputs successors)
	  (declare (ignore outputs))
	  `(if (<= ,(first inputs) ,(second inputs))
	       (go ,(first successors))
	       (go ,(second successors))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-equal-instruction) inputs outputs successors)
	  (declare (ignore outputs))
	  `(if (= ,(first inputs) ,(second inputs))
	       (go ,(first successors))
	       (go ,(second successors))))
	)

;;; When the FUNCALL-INSTRUCTION is the last instruction of a basic
;;; block, it is because there is a call to a function that will never
;;; return, such as ERROR, and the instruction then has no successors
;;; (which is why it is at the end of the basic block).
;;;
;;; We therefore must provide a method on TRANSLATE-BRANCH-INSTRUCTION
;;; (in addition to the method on TRANSLATE-SIMPLE-INSTRUCTION)
;;; specialized to FUNCALL-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs successors abi)
  (declare (ignore outputs successors))
  `(funcall ,(first inputs) ,@(rest inputs)))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JIT the module
;;;
#||
(defun jit-module-run-main (module main-name)
  (let ((engine-builder (llvm-sys:make-engine-builder module))
	 ;; After make-engine-builder MODULE becomes invalid!!!!!
	      (target-options (llvm-sys:make-target-options)))
	 (llvm-sys:setf-no-frame-pointer-elim target-options t)
	 (llvm-sys:setf-jitemit-debug-info target-options t)
	 (llvm-sys:setf-jitemit-debug-info-to-disk target-options t)
	 (llvm-sys:set-target-options engine-builder target-options)
;;;	 (llvm-sys:set-use-mcjit engine-builder t)
	 (let*((execution-engine (llvm-sys:create engine-builder))
	       (stem (string-downcase (pathname-name filename)))
               (main-fn-name llvm-sys:+clasp-main-function-name+)
	       (time-jit-start (clock-gettime-nanoseconds)))
	   (llvm-sys:finalize-engine-and-register-with-gc-and-run-function execution-engine main-fn-name (namestring (truename filename)) 0 0 *load-time-value-holder-name* )
	   )))
     t)
 nil)
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.
;;;
;;; This will compile a top-level form (doing all tlf processing)
;;; into the current *module*

;; All enclosed functions need to be finalized
(defvar *functions-to-finalize*)



(defun do-compile (form)
  (let* ((clasp-system (make-instance 'clasp))
	 (ast (cleavir-generate-ast:generate-ast form *clasp-env* clasp-system))
	 (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
	 (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast))
	 )
    (cleavir-hir-transformations:hir-transformations hir clasp-system nil nil)
    (cleavir-ir:hir-to-mir hir clasp-system nil nil)
    (when *debug-cleavir* (draw-mir hir)) ;; comment out
    (clasp-cleavir:convert-funcalls hir)
    (setf *ast* hoisted-ast
	  *hir* hir)
    (let ((*form* form)
	  (abi (make-instance 'abi-x86-64)))
      (translate hir abi))))
  
(defun cleavir-compile-t1expr (name form env pathname)
  (and env (error "I don't support anything but top level environment compiles using cleavir"))
  (multiple-value-bind (fn function-kind wrapped-env lambda-name warnp failp)
      (cmp:with-debug-info-generator (:module cmp:*the-module*
					      :pathname pathname)
	(let* ((clasp-system (make-instance 'clasp))
	       (ast (cleavir-generate-ast:generate-ast form *clasp-env* clasp-system))
	       (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
	       (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast))
	       )
	  (cleavir-hir-transformations:hir-transformations hir clasp-system nil nil)
	  (cleavir-ir:hir-to-mir hir clasp-system nil nil)
	  (when *debug-cleavir* (draw-mir hir)) ;; comment out
	  (clasp-cleavir:convert-funcalls hir)
	  (setf *ast* hoisted-ast
		*hir* hir)
	  (let ((*form* form)
		(abi (make-instance 'abi-x86-64)))
	    (translate hir abi))))
    (cmp:cmp-log "------------  Finished building MCJIT Module - about to finalize-engine  Final module follows...\n")
    (or fn (error "There was no function returned by compile-lambda-function"))
    (cmp:cmp-log "fn --> %s\n" fn)
    (cmp:cmp-log-dump cmp:*the-module*)
    (when cmp:*dump-module-on-completion*
      (llvm-sys:dump cmp:*the-module*))
    (cmp:cmp-log "About to test and maybe set up the *run-time-execution-engine*\n")
    (if (not cmp:*run-time-execution-engine*)
	;; SETUP THE *run-time-execution-engine* here for the first time
	;; using the current module in *the-module*
	;; At this point the *the-module* will become invalid because
	;; the execution-engine will take ownership of it
	(setq cmp:*run-time-execution-engine* (cmp:create-run-time-execution-engine cmp:*the-module*))
	(llvm-sys:add-module cmp:*run-time-execution-engine* cmp:*the-module*))
    ;; At this point the Module in *the-module* is invalid because the
    ;; execution-engine owns it
    (cmp:cmp-log "The execution-engine now owns the module\n")
    (setq cmp:*the-module* nil)
    (cmp:cmp-log "About to finalize-engine with fn %s\n" fn)
    (let* ((fn-name (llvm-sys:get-name fn)) ;; this is the name of the function - a string
	   (setup-function
	    (llvm-sys:finalize-engine-and-register-with-gc-and-get-compiled-function
	     cmp:*run-time-execution-engine*
	     'REPL			; main fn name
	     fn				; llvm-fn
	     nil			; environment
	     cmp:*run-time-literals-external-name*
	     "repl-fn.txt"
	     0
	     0
	     nil)))
      (unless (compiled-function-p setup-function)
	(format t "Whoah cleavir-clasp compiled code eval --> ~s~%" compiled-function)
	(return-from cleavir-compile-t1expr (values nil t)))
      (let ((enclosed-function (funcall setup-function cmp:*run-time-literal-holder*)))
	(cmp:set-associated-funcs enclosed-function cmp:*all-funcs-for-one-compile*)
	(values enclosed-function warnp failp)))))


(defun cleavir-compile (name form &key debug)
  (let ((cmp:*cleavir-compile-hook* #'cleavir-compile-t1expr)
	(cmp:*dump-module-on-completion* t)
	(cleavir-generate-ast:*compiler* 'cl:compile)
	(*debug-cleavir* debug))
    (compile name form)))






(defun cleavir-compile-file-form (form)
  (multiple-value-bind (fn kind #|| more ||#)
      (do-compile form)
    (cmp:with-ltv-function-codegen (result ltv-env)
      (cmp:irc-intrinsic "invokeTopLevelFunction" 
			 result 
			 fn 
			 (cmp:irc-renv ltv-env)
			 (cmp:jit-constant-unique-string-ptr "top-level")
			 cmp:*gv-source-file-info-handle*
			 (cmp:irc-i64-*current-source-pos-info*-filepos)
			 (cmp:irc-i32-*current-source-pos-info*-lineno)
			 (cmp:irc-i32-*current-source-pos-info*-column)
			 cmp:*load-time-value-holder-global-var*
			 ))))


#+(or)  (let* ((clasp-system (make-instance 'clasp))
	 (ast (cleavir-generate-ast:generate-ast form *clasp-env* clasp-system))
	 (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
	 (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast))
	 )
    (cleavir-hir-transformations:hir-transformations hir clasp-system nil nil)
    (cleavir-ir:hir-to-mir hir clasp-system nil nil)
    (let ((*form* form)
	  (abi (make-instance 'abi-x86-64)))
      (translate hir abi)))


(defun cleavir-compile-file (given-input-pathname &rest args)
  (let ((cmp:*cleavir-compile-file-hook* #'cleavir-compile-file-form)
	(cmp:*dump-module-on-completion* t)
	(cleavir-generate-ast:*compiler* 'cl:compile-file))
    (apply #'compile-file given-input-pathname args)))




#||
#+(or)(defun interpret-hir (initial-instruction)
	(funcall (compile nil `(lambda () ,(translate initial-instruction)))))
(core:load-time-values-dump cmp::*run-time-literal-holder*)
(core:load-time-values-dump "globalRunTimeValues")
(print "Hello")

(apropos "load-time-values-dump")
||#
