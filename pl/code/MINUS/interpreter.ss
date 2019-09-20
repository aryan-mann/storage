#!r7rs

;;; An interpreter for the MINUS language

;;; Daniel P. Friedman (dfried@cs.indiana.edu)
;;; Mitchell Wand (wand@ccs.neu.edu)
;;; From _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4).

;;; Ported to R7RS Scheme:

  ;; John David Stone
  ;; Department of Computer Science
  ;; Grinnell College

  ;; last revised July 10, 2019

(define-library (MINUS interpreter)
  (export run)
  (import (scheme base)
          (utilities eopl)
          (MINUS parser)
          (MINUS expvals)
          (MINUS environments))
  (begin

    ;; run : String -> ExpVal

    (define run
      (lambda (string)
        (value-of-program (scan&parse string))))

    ;; value-of-program : Program -> ExpVal

    (define value-of-program
      (lambda (pgm)
        (cases program pgm
          (a-program (exp1)
            (value-of exp1 (init-env))))))

    ;; value-of : Exp * Env -> ExpVal

    (define value-of
      (lambda (exp env)
        (cases expression exp
          (const-exp (num) (num-val num))
          (var-exp (var) (apply-env env var))
          (diff-exp (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                  (val2 (value-of exp2 env)))
              (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))
                (num-val (- num1 num2)))))
          (zero?-exp (exp1)
            (let ((val1 (value-of exp1 env)))
              (let ((num1 (expval->num val1)))
                (if (zero? num1)
                    (bool-val #t)
                    (bool-val #f)))))
          (if-exp (exp1 exp2 exp3)
            (let ((val1 (value-of exp1 env)))
              (if (expval->bool val1)
                  (value-of exp2 env)
                  (value-of exp3 env))))
          (let-exp (var exp1 body)
            (let ((val1 (value-of exp1 env)))
              (value-of body (extend-env var val1 env))))
          (minus-exp (negand)
            (let ((val1 (value-of negand env)))
              (num-val (- (expval->num val1))))))))))

;;; These procedure definitions are the work of Friedman and Wand,
;;; who published them on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; The port to R7RS Scheme is copyright (C) 2019 by John David Stone
;;; and is likewise released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.
