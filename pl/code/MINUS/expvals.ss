#!r7rs

;;; Expressed values for MINUS

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 1, 2009
;;; last revised July 9, 2019

;;; This library defines a data type
;;; for expressed values of the MINUS programming language,

(define-library (MINUS expvals)
  (export expval? num-val bool-val expval->num expval->bool)
  (import (scheme base)
          (utilities eopl))
  (begin

    ;; An expressed value in MINUS
    ;; is either an exact integer or a Boolean.

    (define-datatype expval expval?
      (num-val (num exact-integer?))
      (bool-val (bool boolean?)))

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
          (else (report-expval-extraction-error 'bool val)))))))


;;; Daniel P. Friedman and Mitchell Wand
;;; wrote earlier versions of the expval data type definition
;;; and of the definitions of the expval->num and expval->bool procedures.
;;; Their work is available at Wand's Github site
;;; as part of the repository https://github.com/mwand/eopl3
;;; and is licensed
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; These revised versions,
;;; the definition of the report-expval-extraction-error procedure,
;;; and the port to R7RS Scheme
;;; are copyright (C) 2009, 2015, 2019 by John David Stone
;;; and are similarly licensed
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.
