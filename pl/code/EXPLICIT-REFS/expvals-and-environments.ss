#!r7rs

;;; Expressed values and environments for EXPLICIT-REFS

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 18, 2009
;;; last revised July 16, 2019

;;; This library defines a data type
;;; for expressed values of the EXPLICIT-REFS programming language,
;;; as described in section 4.2 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; It also defines simple environments,
;;; as described in section 2.2 of that book.
;;; The two datatypes are presented together
;;; because they are mutually recursive.

(define-library (EXPLICIT-REFS expvals-and-environments)
  (export expval? num-val bool-val proc-val ref-val expval->num expval->bool
          expval->proc expval->ref proc? a-proc environment? empty-env extend-env
          extend-env-rec apply-env init-env)
  (import (scheme base)
          (utilities eopl)
          (EXPLICIT-REFS syntax-trees)
          (EXPLICIT-REFS stores))
  (begin

    ;; An expressed value in EXPLICIT-REFS
    ;; is either an exact integer, a Boolean,
    ;; a value of the proc (i.e., closure) data type defined below,
    ;; or a reference.

    (define-datatype expval expval?
      (num-val (num exact-integer?))
      (bool-val (bool boolean?))
      (proc-val (proc proc?))
      (ref-val (ref reference?)))

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

    ;; expval->ref : ExpVal -> Ref

    (define expval->ref
      (lambda (val)
        (cases expval val
          (ref-val (ref) ref)
          (else (report-expval-extraction-error 'ref val)))))

    ;; The identifiers used in this data type definition
    ;; differ slightly from those used in Friedman and Wand's book,
    ;; to avoid conflicts with standard Scheme's built-in procedure? procedure.

    (define-datatype proc proc?
      (a-proc (parameter identifier?)
              (body expression?)
              (saved-env environment?)))

    ;; An environment is either empty or extends another environment
    ;; by adding one new variable,
    ;; to which some denoted value is bound.
    ;; In the EXPLICIT-REFS language
    ;; that Friedman and Wand introduce
    ;; in section 3.4 of _Essentials of programming languages_,
    ;; denoted values and expressed values are the same,
    ;; so we'll use values of the expval data type in this role.

    ;; To add an identifier that denotes a recursive procedure,
    ;; we'll need to recover both the parameter of the procedure and its body
    ;; when we look up its name,
    ;; so we need a third variant of the environment data type.
    ;; This version is adapted from section 3.4
    ;; of _Essentials of programming languages_.

    (define-datatype environment environment?
      (empty-env)
      (extend-env
        (var identifier?)
        (val expval?)
        (saved environment?))
      (extend-env-rec
        (p-names (list-of identifier?))
        (b-vars (list-of identifier?))
        (bodies (list-of expression?))
        (saved environment?)))

    ;; The apply-env procedure looks up a given identifier
    ;; in a given environment
    ;; and returns the denoted value bound to it.
    ;; It is an error to apply apply-env
    ;; to an identifier that is not bound in the given environment.

    ;; I am indebted to Tyler Dewey 2016
    ;; for calling my attention
    ;; to an error in a previous version
    ;; of this procedure definition.

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
                  (kernel saved)))
            (extend-env-rec (p-names b-vars bodies saved)
              (let inner ((rest-of-p-names p-names)
                          (rest-of-b-vars b-vars)
                          (rest-of-bodies bodies))
                (cond ((null? rest-of-p-names) (kernel saved))
                      ((eqv? sought (car rest-of-p-names))
                       (proc-val (a-proc (car rest-of-b-vars)
                                         (car rest-of-bodies)
                                         remaining)))
                      (else (inner (cdr rest-of-p-names)
                                   (cdr rest-of-b-vars)
                                   (cdr rest-of-bodies))))))))))

    (define report-no-binding-found
      (lambda (sought env)
        (eopl:error 'apply-env
                    "No binding for ~s was found in environment ~s.~%"
                    sought
                    env)))

    ;; EXPLICIT-REFS programs are evaluated
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

;;; The definition of the environment data type
;;; is adapted from Friedman and Wand's code
;;; under the same license.

;;; The remaining definitions
;;; and the port to R7RS Scheme
;;; are copyright (C) 2009, 2015, 2019 by John David Stone
;;; and are similarly licensed
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.
