#!r7rs

;;; Expressed values and environments for a CALL-like language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 18, 2009
;;; last revised August 5, 2019

;;; This library defines a data type
;;; for expressed values of the CALL-BY-REFERENCE programming language,
;;; as described in section 4.5 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; It also defines simple environments,
;;; as described in section 2.2 of that book.
;;; The two datatypes are presented together
;;; because they are mutually recursive.

;;; It is provided here as a (CALL expvals-and-environments) library
;;; simply so that the (CALL stores) library can be correctly tested.

(define-library (CALL expvals-and-environments)
  (export expval? num-val bool-val proc-val expval->num expval->bool expval->proc
          proc? a-proc environment? empty-env extend-env extend-env-rec apply-env
          init-env)
  (import (scheme base)
          (utilities eopl)
          (CALL syntax-trees)
          (CALL stores))
  (begin

    ;; An expressed value in CALL-BY-REFERENCE
    ;; is either an exact integer, a Boolean,
    ;; or a value of the proc (i.e., closure) data type
    ;; defined below.

    (define-datatype expval expval?
      (num-val (num exact-integer?))
      (bool-val (bool boolean?))
      (proc-val (proc proc?)))

    ;; We supplement the data type interface
    ;; with projection functions that recover the values
    ;; stored in the respective fields of the variants.

    ;; expval->num : ExpVal -> Int

    (define expval->num
      (lambda (val)
        (cases expval val
          (num-val (num) num)
          (else (report-expval-extraction-error 'num val)))))

    ;; report-expval-extraction-error : Symbol * ExpVal -> (aborts the computation)

    (define report-expval-extraction-error
      (lambda (bad-type bad-ev)
        (eopl:error (string->symbol
                      (string-append "expval->"
                                     (symbol->string bad-type)))
                    "undefined for expressed value ~a~%"
                    bad-ev)))

    ;; expval->bool : ExpVal -> Bool

    (define expval->bool
      (lambda (val)
        (cases expval val
          (bool-val (bool) bool)
          (else (report-expval-extraction-error 'bool val)))))

    ;; expval->proc : ExpVal -> Proc

    (define expval->proc
      (lambda (val)
        (cases expval val
          (proc-val (proc) proc)
          (else (report-expval-extraction-error 'proc val)))))

    ;; The identifiers used in this data type definition
    ;; differ slightly from those used in Friedman and Wand's book,
    ;; to avoid conflicts with standard Scheme's built-in procedure? procedure.

    (define-datatype proc proc?
      (a-proc (parameter identifier?)
              (body expression?)
              (saved-env environment?)))

    ;; An environment is either empty or extends another environment
    ;; by adding one new variable,
    ;; to which some newly allocated storage location is bound

    (define-datatype environment environment?
      (empty-env)
      (extend-env
        (var identifier?)
        (loc reference?)
        (saved environment?))
      (extend-env-rec
        (p-names (list-of identifier?))
        (b-vars (list-of identifier?))
        (bodies (list-of expression?))
        (saved environment?)))

    ;; The apply-env procedure looks up a given identifier
    ;; in a given environment
    ;; and returns the storage location bound to it.
    ;; It is an error to apply apply-env
    ;; to an identifier that is not bound in the given environment.

    ;; apply-env : Env * Sym -> ExpVal

    (define apply-env
      (lambda (env sought)
        (let kernel ((remaining env))
          (cases environment remaining
            (empty-env ()
              (report-no-binding-found sought env))
            (extend-env (var loc saved)
              (if (eqv? var sought)
                  loc
                  (kernel saved)))
            (extend-env-rec (p-names b-vars bodies saved)
              (let inner ((rest-of-p-names p-names)
                          (rest-of-b-vars b-vars)
                          (rest-of-bodies bodies))
                (cond ((null? rest-of-p-names) (kernel saved))
                      ((eqv? sought (car rest-of-p-names))
                       (newref (proc-val (a-proc (car rest-of-b-vars)
                                                 (car rest-of-bodies)
                                                 remaining))))
                      (else (inner (cdr rest-of-p-names)
                                   (cdr rest-of-b-vars)
                                   (cdr rest-of-bodies))))))))))

    (define report-no-binding-found
      (lambda (sought env)
        (eopl:error 'apply-env
                    "No binding for ~s was found in environment ~s.~%"
                    sought
                    env)))

    ;; CALL-BY-REFERENCE programs are evaluated
    ;; in an initial environment
    ;; containing bindings for a few Roman numerals.
    ;; The init-env procedure constructs and returns this environment.

    ;; This code is taken
    ;; from section 3.2 of _Essentials of programming languages_.

    ;; init-env : () -> Env

    (define init-env
      (lambda ()
        (extend-env 'i (newref (num-val 1))
          (extend-env 'v (newref (num-val 5))
            (extend-env 'x (newref (num-val 10)) (empty-env))))))))

;;; These definitions are derived from similar work
;;; by Daniel P. Friedman (dfried@cs.indiana.edu)
;;; and Mitchell Wand (wand@ccs.neu.edu),
;;; who made them available as part of the Git repository
;;; at https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license
;;; (http://creativecommons.org/licenses/by-nc/3.0/).

;;; These specific versions
;;; and the port to R7RS Scheme
;;; are copyright (C) 2009, 2015, 2019 by John David Stone
;;; and are similarly licensed
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.
