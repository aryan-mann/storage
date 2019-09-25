#!r7rs

;;; Expressed values and environments for DPROC

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 5, 2009
;;; last revised July 15, 2019

;;; This library defines a data type
;;; for expressed values of the DPROC programming language,
;;; as described in section 3.3 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; It also defines simple environments,
;;; as described in section 2.2 of that book.
;;; The two datatypes are presented together
;;; because they are mutually recursive.

(define-library (DPROC expvals-and-environments)
  (export expval? num-val bool-val proc-val expval->num expval->bool expval->proc
          proc? a-proc environment? empty-env extend-env apply-env init-env)
  (import (scheme base)
          (utilities eopl)
          (DPROC syntax-trees))
  (begin

    ;; An expressed value in DPROC
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
              (body expression?)))

    ;; An environment is either empty or extends another environment
    ;; by adding one new variable,
    ;; to which some denoted value is bound.
    ;; In the DPROC language
    ;; that Friedman and Wand introduce
    ;; in section 3.3 of _Essentials of programming languages_,
    ;; denoted values and expressed values are the same,
    ;; so we'll use values of the expval data type in this role.

    (define-datatype environment environment?
      (empty-env)
      (extend-env
        (var identifier?)
        (val expval?)
        (saved environment?)))

    ;;; The apply-env procedure looks up a given identifier
    ;;; in a given environment
    ;;; and returns the denoted value bound to it.
    ;;; It is an error to apply apply-env
    ;;; to an identifier that is not bound in the given environment.

    ;; apply-env : Env * Sym -> ExpVal

    (define apply-env
      (lambda (env sought)
        (let kernel ((remaining env))
          (cases environment remaining
            (empty-env ()
              (report-no-binding-found sought env))
            (extend-env (var val saved)
              (if (eqv? var sought)
                  val
                  (kernel saved)))))))

    (define report-no-binding-found
      (lambda (sought env)
        (eopl:error 'apply-env
                    "No binding for ~s was found in environment ~s.~%"
                    sought
                    env)))

    ;; DPROC programs are evaluated
    ;; in an initial environment
    ;; containing bindings for a few Roman numerals.
    ;; The init-env procedure constructs and returns this environment.

    ;; This code is taken
    ;; from section 3.2 of _Essentials of programming languages_.

    ;; init-env : () -> Env

    (define init-env
      (lambda ()
	(empty-env)))))
;;; The definition of the init-env procedure
;;; is due to Daniel P. Friedman (dfried@cs.indiana.edu)
;;; and Mitchell Wand (wand@ccs.neu.edu),
;;; who made it available as part of the Git repository
;;; at https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license
;;; (http://creativecommons.org/licenses/by-nc/3.0/).

;;; The remaining definitions
;;; and the port to R7RS Scheme
;;; are copyright (C) 2009, 2015, 2019 by John David Stone
;;; and are similarly licensed
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.
