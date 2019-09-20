#!r7rs

;;; An interpreter for the CHECKED language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; This is an interpreter for the CHECKED programming language,
;;; as described in section 7.3
;;; of _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; The design and implementation extend,
;;; and are derived from, code published in that book.

;;; created April 9, 2009
;;; last revised July 26, 2019

(define-library (CHECKED interpreter)
  (export run)
  (import (scheme base)
          (utilities eopl)
          (CHECKED parser)
          (LETREC expvals-and-environments)
          (CHECKED type-checker))
  (begin

    ;; run : String -> ExpVal

    (define run
      (lambda (source-text)
        (let ((parse-tree (scan&parse source-text)))
          (type-of-program parse-tree)
          (value-of-program parse-tree))))

    ;; value-of-program : Program -> ExpVal

    (define value-of-program
      (lambda (pgm)
        (cases program pgm
          (a-program (exp)
            (value-of exp (init-env))))))

    ;; value-of : Exp * Env -> ExpVal

    (define value-of
      (lambda (exp env)
        (cases expression exp
          (const-exp (datum) (num-val datum))
          (var-exp (id) (apply-env env id))
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
                                       (value-of bound-value env)
                                       env)))
          (proc-exp (parameter ty body)
            (proc-val (a-proc parameter body env)))
          (call-exp (operator operand)
            (let ((proc (expval->proc (value-of operator env)))
                  (arg (value-of operand env)))
              (apply-procedure proc arg)))
          (letrec-exp (return-type procedure-name parameter parameter-type
                       procedure-body letrec-body)
            (value-of letrec-body (extend-env-rec procedure-name
                                                  parameter
                                                  procedure-body
                                                  env))))))

    ;; apply-procedure : Proc * ExpVal -> ExpVal

    (define apply-procedure
      (lambda (applicand argument)
        (cases proc applicand
          (a-proc (parameter body saved-env)
            (value-of body (extend-env parameter argument saved-env))))))))

;;; As mentioned above,
;;; the procedure definitions are derived from
;;; the work of Friedman and Wand,
;;; who published them on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; The port to R7RS Scheme is copyright (C) 2009, 2015, 2019 by John David Stone
;;; and is likewise released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.
