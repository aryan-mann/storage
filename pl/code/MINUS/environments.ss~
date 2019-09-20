#!r7rs

;;; Environments for LET

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 1, 2009
;;; last revised July 14, 2019

;;; This library defines simple environments,
;;; as described in section 2.2 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(define-library (LET environments)
  (export environment? empty-env extend-env apply-env init-env)
  (import (scheme base)
          (utilities eopl)
          (LET expvals))
  (begin

    ;; An environment is either empty
    ;; or extends another environment by adding one new variable,
    ;; to which some denoted value is bound.
    ;; In the LET language
    ;; that Friedman and Wand introduce
    ;; in section 3.2 of _Essentials of programming languages_,
    ;; denoted values and expressed values are the same,
    ;; so we'll use values of the expval data type in this role.

    (define-datatype environment environment?
      (empty-env)
      (extend-env (var identifier?)
                  (val expval?)
                  (saved environment?)))

    ;; The apply-env procedure looks up a given variable
    ;; in a given environment
    ;; and returns the denoted value bound to it.
    ;; It is an error to apply apply-env
    ;; to a variable that is not bound in the given environment.
    
    ;; apply-env : Env * Sym -> ExpVal

    (define apply-env
      (lambda (env sought)
        (let kernel ((remaining env))
          (cases environment remaining
            (empty-env () (report-no-binding-found sought env))
            (extend-env (var val saved)
              (if (eqv? var sought)
                  val
                  (kernel saved)))))))

    (define report-no-binding-found
      (lambda (sought env)
        (eopl:error 'apply-env
                    "No binding for ~a was found in environment ~a.~%"
                    sought
                    env)))

    ;; LET programs are evaluated
    ;; in an initial environment
    ;; containing bindings for a few Roman numerals.
    ;; The init-env procedure constructs and returns this environment.

    ;; This code is taken
    ;; from section 3.2 of _Essentials of programming languages_.

    ;; init-env : () -> Env

    (define init-env
      (lambda ()
        (extend-env 'i (num-val 1)
          (extend-env 'v (num-val 5)
            (extend-env 'x (num-val 10) (empty-env))))))))

;;; The definition of the init-env procedure
;;; is due to Daniel P. Friedman (dfried@cs.indiana.edu)
;;; and Mitchell Wand (wand@ccs.neu.edu),
;;; who made it available as part of the Git repository
;;; at https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license
;;; (http://creativecommons.org/licenses/by-nc/3.0/).

;;; The remaining definitions and the port to R7RS Scheme
;;; are copyright (C) 2009, 2015, 2019 by John David Stone
;;; and are similarly released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.
