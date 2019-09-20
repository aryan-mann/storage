#!r7rs

;;; An interpreter for the CLASSES language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; This is an interpreter for the CLASSES programming language,
;;; as described in section 9.3
;;; of _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; The design and implementation extend,
;;; and are derived from, code published in that book.

;;; created April 26, 2009
;;; last revised August 1, 2019

(define-library (CLASSES interpreter)
  (export run)
  (import (scheme base)
          (utilities eopl)
          (CLASSES parser)
          (CLASSES stores)
          (CLASSES expvals-and-environments))
  (begin

    ;; run : String -> ExpVal

    (define run
      (lambda (source-text)
        (value-of-program (scan&parse source-text))))

    ;; value-of-program : Program -> ExpVal

    (define value-of-program
      (lambda (pgm)
        (initialize-store!)
        (cases program pgm
          (a-program (class-decls body)
            (initialize-class-env! class-decls)
            (value-of body (init-env))))))

    ;; value-of : Exp * Env -> ExpVal

    (define value-of
      (lambda (exp env)
        (cases expression exp
          (const-exp (datum) (num-val datum))
          (var-exp (id) (deref (apply-env env id)))
          (diff-exp (minuend subtrahend)
            (num-val (- (expval->num (value-of minuend env))
                        (expval->num (value-of subtrahend env)))))
          (zero?-exp (testee)
            (bool-val (zero? (expval->num (value-of testee env)))))
          (if-exp (condition consequent alternative)
              (if (expval->bool (value-of condition env))
                  (value-of consequent env)
                  (value-of alternative env)))
          (let-exp (bound-vars bound-values body)
            (value-of body (extend-env bound-vars
                                       (map newref (values-of-exps bound-values env))
                                       env)))
          (proc-exp (parameters body)
            (proc-val (a-proc parameters body env)))
          (call-exp (operator operands)
            (let ((proc (expval->proc (value-of operator env)))
                  (args (values-of-exps operands env)))
              (apply-procedure proc args)))
          (letrec-exp (procedure-names parameter-lists procedure-bodies letrec-body)
            (value-of letrec-body (extend-env-rec procedure-names
                                                  parameter-lists
                                                  procedure-bodies
                                                  env)))
          (assign-exp (target source)
            (setref! (apply-env env target) (value-of source env))
            (num-val 27))
          (begin-exp (starter sequence)
            (let loop ((last-val (value-of starter env))
                       (rest-of-sequence sequence))
              (if (null? rest-of-sequence)
                  last-val
                  (loop (value-of (car rest-of-sequence) env)
                        (cdr rest-of-sequence)))))
          (sum-exp (augend addend)
            (num-val (+ (expval->num (value-of augend env))
                        (expval->num (value-of addend env)))))
          (product-exp (multiplicand multiplier)
            (num-val (* (expval->num (value-of multiplicand env))
                        (expval->num (value-of multiplier env)))))
          (cons-exp (fore aft)
            (list-val (cons (value-of fore env)
                            (expval->elements (value-of aft env)))))
          (car-exp (pair)
            (car (expval->elements (value-of pair env))))
          (cdr-exp (pair)
            (list-val (cdr (expval->elements (value-of pair env)))))
          (null?-exp (testee)
            (bool-val (null? (expval->elements (value-of testee env)))))
          (emptylist-exp ()
            (list-val (list)))
          (list-exp (elements)
            (list-val (values-of-exps elements env)))
          (new-object-exp (class-name operands)
            (let ((obj (new-object class-name))
                  (arguments (values-of-exps operands env)))
              (apply-method (find-method class-name 'initialize) obj arguments)
              (object-val obj)))
          (method-call-exp (recipient method-name operands)
            (let* ((obj (expval->obj (value-of recipient env)))
                   (arguments (values-of-exps operands env)))
              (apply-method (find-method (object->class-name obj) method-name)
                            obj
                            arguments)))
          (super-call-exp (method-name operands)
            (let ((obj (apply-env env '%self))
                  (arguments (values-of-exps operands env)))
              (apply-method (find-method (apply-env env '%super) method-name)
                            obj
                            arguments)))
          (self-exp ()
            (object-val (apply-env env '%self))))))

    ;; The apply-procedure procedure
    ;; applies the procedure represented by a given proc
    ;; to a given list of values
    ;; by evaluating the proc's body in an environment
    ;; obtained by adding the binding for the proc's parameters
    ;; to its stored environment.

    ;; apply-procedure : Proc * ListOf(ExpVal) -> ExpVal

    (define apply-procedure
      (lambda (applicand arguments)
        (cases proc applicand
          (a-proc (parameters body saved-env)
            (value-of body (extend-env parameters (map newref arguments) saved-env))))))

    ;;; The values-of-exps procedure evaluates the expressions
    ;;; in a given list, from left to right, in a given environment.

    ;; values-of-exps : Listof(Exp) * Env -> Listof(ExpVal)

    (define values-of-exps
      (lambda (exps env)
        (let kernel ((rest exps))
          (if (null? rest)
              '()
              (let ((car-value (value-of (car rest) env)))
                (cons car-value (kernel (cdr rest))))))))

    ;; The apply-method procedure implements a method call.

    ;; apply-method : Method * Obj * Listof(ExpVal) -> ExpVal

    (define apply-method
      (lambda (m self arguments)
        (cases method m
          (a-method (parameters body super-name field-names)
            (value-of body
                      (extend-env parameters (map newref arguments)
                        (extend-env-with-self-and-super
                          self
                          super-name
                          (extend-env field-names (object->fields self)
                            (empty-env)))))))))))

;;; The definitions in this library
;;; are derived from the work of Friedman and Wand,
;;; who published them on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; The port to R7RS Scheme is copyright (C) 2009, 2015, 2019 by John David Stone
;;; and is likewise released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.
