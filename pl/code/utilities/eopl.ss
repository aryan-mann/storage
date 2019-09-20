#!r7rs

;;; Utilities from _Essentials of Programming Languages_

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 3, 2019
;;; last revised August 2, 2019

;;; This library implements some features
;;; of the Scheme dialect used
;;; in _Essentials of Programming Languages_,
;;; by Daniel P. Friedman and Mitchell Wand
;;; (Cambridge, Massachusetts: The MIT Press; third edition, 2008;
;;; ISBN 978-0-262-06279-4),
;;; notably the define-datatype and cases syntaxes.

(define-library (utilities eopl)
  (export eopl:printf eopl:error genus species define-datatype cases
          identifier? list-of maybe)
  (import (scheme base)
          (scheme write)
          (scheme process-context)
          (scheme char)
          (srfi basic-format-strings)
          (srfi list-lib))
  (begin

    ;; The eopl:printf procedure writes a suitably formatted text
    ;; to the standard output port.

    (define eopl:printf
      (lambda (format-string . args)
        (display (apply format format-string args))))

    ;; The eopl:error procedure writes a suitably formatted error message
    ;; to the standard error port
    ;; and exits from the program,
    ;; communicating to the operating system
    ;; that it ended abnormally.

    (define eopl:error
      (lambda (location format-string . args)
        (error (apply format (string-append "In ~a: " format-string)
                             location
                             args))))

    ;; The implementation of the define-datatype and cases syntax
    ;; relies on the following auxiliary record type,
    ;; which carries information from the former to the latter
    ;; through the values returned by the constructors 
    ;; for the several variants of a data type.

    (define-record-type <variant>
      (make-variant genus species field-values)
      variant?
      (genus genus)
      (species species)
      (field-values variant-field-values))

    ;; Every value of a data type defined by define-datatype
    ;; is implemented as a record of the type just defined.
    ;; The record contains a symbol naming the data type,
    ;; another symbol naming the variant,
    ;; and a list of the values of the fields for that variant.

    (define-syntax define-datatype

      ;; The authors formally specify the structure
      ;; of a define-datatype-expression
      ;; on page 47 of _Essentials of Programming Languages_.
      ;; The slightly extended structure here
      ;; permits datatypes with no variants
      ;; (but such datatypes are practically useless
      ;; because they have no constructors).

      (syntax-rules ()
        ((define-datatype type-name type-predicate-name
           (variant-name (field-name predicate) ...) ...)
         (begin

           ;; The type predicate for the data type
           ;; determines whether the value it receives
           ;; is a record of the <variant> type defined above
           ;; and, if so, whether its genus
           ;; matches the specifed type name.

           (define type-predicate-name
             (lambda (something)
               (and (variant? something)
                    (eq? (quote type-name) (genus something)))))

           ;; Each of the constructors for the data type
           ;; first checks to make sure
           ;; that all of the values supplied to the constructor
           ;; satisfy the respective constraints
           ;; specified in the define-datatype expression.
           ;; If so, make-variant is called
           ;; to produce the actual value
           ;; to be returned by the constructor.

           (define variant-name
             (lambda (field-name ...)
               (unless (predicate field-name)
                 (error (string-append
                         "A constraint on the value of a field in a data type "
                         "was not satisfied.")
                        (quote field-name))) ...
               (make-variant (quote type-name)
                             (quote variant-name)
                             (list field-name ...)))) ...))))
                          
    ;; The cases syntax
    ;; expands into a case-expression
    ;; that branches on the symbol found in the species field
    ;; of the record it receives.

    (define-syntax cases
      (syntax-rules (else)

        ;; The authors formally specify the structure of a cases-expresssion
        ;; on page 49 of _Essentials of Programming Languages_.
        ;; I have extended the syntax
        ;; to allow for more than one consequent
        ;; in a single variant clause.

        ((cases type-name expression
           (variant-name (field-name ...) side-effect ... consequent) ...
           (else default))

         ;; The expansion first checks to make sure
         ;; that the value belongs to the correct data type,
         ;; then selects the appropriate variant,
         ;; binds the provided parameters to the field values,
         ;; and evaluates the consequent expression for that variant.

         (let ((val expression))
           (unless (and (variant? val)
                        (eq? (quote type-name) (genus val)))
             (error (string-append
                      "The value in a cases-expression "
                      "did not belong to the specified data type.")
                    val
                    (quote type-name)))
           (case (species val)
             ((variant-name)
              (let-values (((field-name ...)
                            (apply values (variant-field-values val))))
                side-effect ...
                consequent)) ...
             (else default))))

        ;; A cases-expression with no else-clause
        ;; is simply expanded to a cases-expression
        ;; in which the else-clause raises an error.

        ((cases type-name expression
           (variant-name (field-name ...) side-effect ... consequent) ...)
         (cases type-name expression
           (variant-name (field-name ...) side-effect ... consequent) ...
           (else (error (string-append
                          "A variant was missing from a cases-expression "
                          "with no else-clause.")
                        (quote type-name)
                        (species expression)))))))

    ;; The identifier? predicate
    ;; determines whether its argument is an identifier.
    ;; We'll count a symbol as an identifier
    ;; if it begins with a letter
    ;; and consists entirely of letters, digits, hyphens, and question marks.

    (define identifier?
      (lambda (something)
        (and (symbol? something)
             (let ((letter-list (string->list (symbol->string something))))
               (and (pair? letter-list)
                    (char-alphabetic? (car letter-list))
                    (every (lambda (letter)
                             (or (char-alphabetic? letter)
                                 (char-numeric? letter)
                                 (char=? letter #\-)
                                 (char=? letter #\?)))
                           (cdr letter-list)))))))

    ;; The list-of procedure
    ;; takes a unary predicate as its argument
    ;; and returns a unary predicate
    ;; that determines whether its argument is a list
    ;; of which every element satisfies the given unary predicate.

    (define list-of
      (lambda (predicate)
        (lambda (something)
          (let kernel ((rest something))
            (or (null? rest)
                (and (pair? rest)
                     (predicate (car rest))
                     (kernel (cdr rest))))))))

    ;; Given a predicate,
    ;; the maybe procedure returns a predicate
    ;; that is satisfied by everything that satisfies the given predicate
    ;; and by #f.

    ;; maybe : (SchemeVal -> Bool) -> (SchemeVal -> Bool)

    (define maybe
      (lambda (predicate)
        (lambda (something)
          (or (predicate something)
              (not something)))))))

;;; Copyright (C) 2019  John David Stone

;;; This program is free software.
;;; You may redistribute it and/or modify it
;;; under the terms of the GNU General Public License
;;; as published by the Free Software Foundation -- 
;;; either version 3 of the License,
;;; or (at your option) any later version.

;;; This program is distributed
;;; in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY --
;;; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.

;;; You should have received a copy
;;; of the GNU General Public License
;;; along with this program.
;;; If not, it is available on the World Wide Web
;;; at https://www.gnu.org/licenses/gpl.html.
