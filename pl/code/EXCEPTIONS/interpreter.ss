#!r7rs

;;; A continuation-passing interpreter for the EXCEPTIONS language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; This is an interpreter for the EXCEPTIONS programming language,
;;; as described in section 5.4
;;; of _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; The design and implementation extend,
;;; and are derived from, code published in that book.

;;; created March 8, 2009
;;; last revised July 25, 2019

(define-library (EXCEPTIONS interpreter)
  (export run)
  (import (scheme base)
          (utilities eopl)
          (EXCEPTIONS syntax-trees)
          (EXCEPTIONS parser)
          (EXCEPTIONS expvals-and-environments)
          (EXCEPTIONS continuations))
  (begin

    ;; run : String -> FinalAnswer

    (define run
      (lambda (source-text)
        (value-of-program (scan&parse source-text))))

    ;; value-of-program : Program -> FinalAnswer

    (define value-of-program
      (lambda (pgm)
        (cases program pgm
          (a-program (exp)
            (value-of/k exp (init-env) (end-cont))))))

    ;; value-of/k : Exp * Env * Cont -> FinalAnswer

    (define value-of/k
      (lambda (exp env cont)
        (cases expression exp
          (const-exp (datum) (apply-cont cont (num-val datum)))
          (var-exp (id) (apply-cont cont (apply-env env id)))
          (diff-exp (minuend subtrahend)
            (value-of/k minuend env (diff1-cont subtrahend env cont)))
          (zero?-exp (testee)
            (value-of/k testee env (zero1-cont cont)))
          (if-exp (condition consequent alternative)
            (value-of/k condition
                        env
                        (if-test-cont consequent alternative env cont)))
          (let-exp (bound-var bound-value body)
            (value-of/k bound-value env (let-exp-cont bound-var body env cont)))
          (proc-exp (parameter body)
            (apply-cont cont (proc-val (a-proc parameter body env))))
          (call-exp (operator operand)
            (value-of/k operator env (rator-cont operand env cont)))
          (letrec-exp (procedure-name parameter procedure-body letrec-body)
            (value-of/k letrec-body
                        (extend-env-rec procedure-name
                                        parameter
                                        procedure-body
                                        env)
                        cont))
          (try-exp (try-body parameter handler-body)
            (value-of/k try-body
                        env
                        (try-cont parameter handler-body env cont)))
          (raise-exp (body)
            (value-of/k body env (raise-body-cont cont)))
          (cons-exp (fore aft)
            (value-of/k fore env (cons1-cont aft env cont)))
          (car-exp (pair)
            (value-of/k pair env (car1-cont cont)))
          (cdr-exp (pair)
            (value-of/k pair env (cdr1-cont cont)))
          (null?-exp (testee)
            (value-of/k testee env (null1-cont cont)))
          (emptylist-exp ()
            (apply-cont cont (list-val '())))
          (list-exp (elements)
            (if (null? elements)
                (apply-cont cont (list-val '()))
                (value-of/k (car elements)
                            env
                            (list-element-cont
                              '() (cdr elements) env cont)))))))

    ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer

    (define apply-procedure/k
      (lambda (applicand argument cont)
        (cases proc applicand
          (a-proc (parameter body saved-env)
            (value-of/k body
                        (extend-env parameter argument saved-env)
                        cont)))))

    ;; The apply-cont procedure
    ;; receives a given expressed value
    ;; for which a given continuation is waiting,
    ;; and carries out the computation stored in the continuation
    ;; to obtain the final answer to a program.

    ;; apply-cont : Cont * ExpVal -> FinalAnswer

    (define apply-cont
      (lambda (cont val)
        (cases continuation cont
          (end-cont () (eopl:printf "End of computation.~%")
                       val)
          (diff1-cont (subtrahend env saved-cont)
            (value-of/k subtrahend env (diff2-cont val saved-cont)))
          (diff2-cont (minuend-value saved-cont)
            (apply-cont saved-cont (num-val (- (expval->num minuend-value)
                                               (expval->num val)))))
          (zero1-cont (saved-cont)
            (apply-cont saved-cont (bool-val (zero? (expval->num val)))))
          (if-test-cont (consequent alternative env saved-cont)
            (if (expval->bool val)
                (value-of/k consequent env saved-cont)
                (value-of/k alternative env saved-cont)))
          (let-exp-cont (bound-var body env saved-cont)
            (value-of/k body (extend-env bound-var val env) saved-cont))
          (rator-cont (operand env saved-cont)
            (value-of/k operand env (rand-cont val saved-cont)))
          (rand-cont (operator-value saved-cont)
            (apply-procedure/k (expval->proc operator-value)
                               val
                               saved-cont))
          (try-cont (parameter handler-body env saved-cont)
            (apply-cont saved-cont val))
          (raise-body-cont (saved-cont)
            (apply-handler val saved-cont))
          (cons1-cont (aft env saved-cont)
            (value-of/k aft env (cons2-cont val saved-cont)))
          (cons2-cont (fore-value saved-cont)
            (apply-cont saved-cont
                        (list-val (cons fore-value (expval->elements val)))))
          (car1-cont (saved-cont)
            (apply-cont saved-cont (car (expval->elements val))))
          (cdr1-cont (saved-cont)
            (apply-cont saved-cont (list-val (cdr (expval->elements val)))))
          (null1-cont (saved-cont)
            (apply-cont saved-cont (bool-val (null? (expval->elements val)))))
          (list-element-cont (so-far remaining env saved-cont)
            (if (null? remaining)
                (apply-cont saved-cont (list-val (reverse (cons val so-far))))
                (value-of/k (car remaining)
                            env
                            (list-element-cont (cons val so-far)
                                               (cdr remaining)
                                               env
                                               saved-cont)))))))

    ;; The apply-handler procedure searches back through the given continuation,
    ;; looking for a (possibly deeply nested) try-continuation.
    ;; When it finds one,
    ;; it binds a given value to the handler's parameter
    ;; and evaluates the handler's body, as given in that continuation.

    ;; apply-handler : ExpVal * Cont -> FinalAnswer

    (define apply-handler
      (lambda (val cont)
        (cases continuation cont
          (end-cont ()
            (report-uncaught-exception))
          (diff1-cont (subtrahend env saved-cont)
            (apply-handler val saved-cont))
          (diff2-cont (minuend-value saved-cont)
            (apply-handler val saved-cont))
          (zero1-cont (saved-cont)
            (apply-handler val saved-cont))
          (if-test-cont (consequent alternative env saved-cont)
            (apply-handler val saved-cont))
          (let-exp-cont (bound-var body env saved-cont)
            (apply-handler val saved-cont))
          (rator-cont (operand env saved-cont)
            (apply-handler val saved-cont))
          (rand-cont (operator-value saved-cont)
            (apply-handler val saved-cont))
          (try-cont (parameter handler-body saved-env saved-cont)
            (value-of/k handler-body
                        (extend-env parameter val saved-env)
                        saved-cont))
          (raise-body-cont (saved-cont)
            (apply-handler val saved-cont))
          (cons1-cont (aft env saved-cont)
            (apply-handler val saved-cont))
          (cons2-cont (fore-value saved-cont)
            (apply-handler val saved-cont))
          (car1-cont (saved-cont)
            (apply-handler val saved-cont))
          (cdr1-cont (saved-cont)
            (apply-handler val saved-cont))
          (null1-cont (saved-cont)
            (apply-handler val saved-cont))
          (list-element-cont (so-far remaining saved-env saved-cont)
            (apply-handler val saved-cont)))))

    (define report-uncaught-exception
      (lambda ()
        (eopl:error 'apply-handler
                    "An exception was raised with no handler in place.~%")))))

;;; As mentioned above,
;;; the procedure definitions are derived from
;;; the work of Friedman and Wand,
;;; who published them on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; The port to R7RS Scheme is copyright (C) 2009, 2015, 2019 by John David Stone
;;; and is likewise released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.
