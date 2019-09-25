#!r7rs

;;; An interpreter for the DPROC language

;;; Daniel P. Friedman (dfried@cs.indiana.edu)
;;; Mitchell Wand (wand@ccs.neu.edu)
;;; From _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4).

;;; Ported to R7RS Scheme:

  ;; John David Stone
  ;; Department of Computer Science
  ;; Grinnell College

  ;; last revised July 14, 2019

(define-library (DPROC interpreter)
  (export run)
  (import (scheme base)
          (scheme write)
          (utilities eopl)
          (DPROC parser)
          (DPROC expvals-and-environments))
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
          (proc-exp (var body)
            (proc-val (a-proc var body)))
          (call-exp (rator rand)
            (let ((proc (expval->proc (value-of rator env)))
                  (arg (value-of rand env)))
              (apply-procedure proc arg env))))))

    ;; apply-procedure : Proc * ExpVal -> ExpVal

    (define apply-procedure
      (lambda (applicand argument env)
        (cases proc applicand
          (a-proc (parameter body)
            (value-of body env)))))
))

;;;;;;;;;;;;;;;
;;; Testing ;;;
;;;;;;;;;;;;;;;
(define test-1
  (list 10 20 "
let x = 10 in
let pp = proc (y) x in
let x = 20 in
(pp 0)
"))

(define test-2
  (list 10 10 "
let x = 10 in
let pp = proc (y) x in
(pp 0)
"))

(define compare-test
  (lambda (test)
    (let ((lex-val (list-ref test 0))
          (dyn-val (list-ref test 1))
          (val (expval->num (run (list-ref test 2)))))
      (cond
        ((equal? val dyn-val) "\tDynamic\n")
        ((equal? val lex-val) "\tLexical\n")
        (else (string-append "\tError::Unexpected Value: " val "\n"))))))

(display "Scoping Results: \n")
(display (compare-test test-1))
(display (compare-test test-1))
          
      
;;; These procedure definitions are the work of Friedman and Wand,
;;; who published them on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; The port to R7RS Scheme is copyright (C) 2019 by John David Stone
;;; and is likewise released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.
