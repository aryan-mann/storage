#!r7rs

;;; An interpreter for the MUTABLE-PAIRS language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; This is an interpreter for the MUTABLE-PAIRS programming language,
;;; as described in section 4.4
;;; of _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; The design and implementation extend,
;;; and are derived from, code published in that book.

;;; created February 25, 2009
;;; last revised July 18, 2019

(define-library (MUTABLE-PAIRS interpreter)
  (export run)
  (import (scheme base)
          (utilities eopl)
          (MUTABLE-PAIRS parser)
          (MUTABLE-PAIRS stores)
          (MUTABLE-PAIRS expvals-and-environments))
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
          (a-program (exp)
            (value-of exp (init-env))))))

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
          (let-exp (bound-var bound-value body)
            (value-of body (extend-env bound-var
                                       (newref (value-of bound-value env))
                                       env)))
          (proc-exp (parameter body)
            (proc-val (a-proc parameter body env)))
          (call-exp (operator operand)
            (let ((proc (expval->proc (value-of operator env)))
                  (arg (value-of operand env)))
              (apply-procedure proc arg)))
          (letrec-exp (procedure-names parameters procedure-bodies letrec-body)
            (value-of letrec-body (extend-env-rec procedure-names
                                                  parameters
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
          (newpair-exp (left-init right-init)
            (let* ((left-val (value-of left-init env))
                   (right-val (value-of right-init env)))
              (mutpair-val (make-pair left-val right-val))))
          (left-exp (pair)
            (left (expval->mutpair (value-of pair env))))
          (right-exp (pair)
            (right (expval->mutpair (value-of pair env))))
          (setleft-exp (target source)
            (let* ((target-val (value-of target env))
                   (source-val (value-of source env)))
              (setleft (expval->mutpair target-val) source-val)
              (num-val 82)))
          (setright-exp (target source)
            (let* ((target-val (value-of target env))
                   (source-val (value-of source env)))
              (setright (expval->mutpair target-val) source-val)
              (num-val 83))))))

    ;; apply-procedure : Proc * ExpVal -> ExpVal

    (define apply-procedure
      (lambda (applicand argument)
        (cases proc applicand
          (a-proc (parameter body saved-env)
            (value-of body
                      (extend-env parameter (newref argument) saved-env))))))))

;;; The definitions in this library
;;; are derived from the work of Friedman and Wand,
;;; who published them on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; The port to R7RS Scheme is copyright (C) 2009, 2015, 2019 by John David Stone
;;; and is likewise released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.
