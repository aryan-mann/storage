#!r7rs

;;; A parser for the LETREC-LCPI language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 8, 2009
;;; last revised July 31, 2019

;;; This library provides a parser for the LETREC-LCPI language
;;; developed by Daniel P. Friedman and Mitchell Wand
;;; in section 3.4 of their book
;;; _Essentials of programming languages_ (third edition).

(define-library (LETREC-LCPI parser)
  (export parse-program parse-expression scan&parse)
  (import (scheme base)
          (utilities eopl)
          (utilities character-sources)
          (LETREC-LCPI scanner)
          (LETREC-LCPI tokens)
          (LETREC-LCPI syntax-trees))
  (begin

    ;; The acquire procedure recovers a token
    ;; from a given source,
    ;; signalling an error if none is available.

    ;; acquire-token : Token-source -> Token

    (define acquire-token
      (lambda (token-source)
        (when (token-source 'at-end?)
          (report-unexpected-end-of-source-error))
        (token-source 'get)))

    (define report-unexpected-end-of-source-error
      (lambda ()
        (eopl:error 'acquire-token
                    "The end of the input was encountered unexpectedly.")))

    ;; The match-and-discard procedure
    ;; gets a token from a given source
    ;; and compares its species with the species of the token
    ;; that the parser expects to find.
    ;; If they don't match, an error is reported.

    ;; match-and-discard : Token-source * Token -> ()

    (define match-and-discard
      (lambda (token-source expected)
        (let ((discard (acquire-token token-source)))
          (unless (eq? expected (species discard))
            (report-unexpected-token-error discard expected)))))

    (define report-unexpected-token-error
      (lambda (found expected)
        (eopl:error 'match-and-discard
                    "The token ~a does not match the expected token (~a).~%"
                    found
                    expected)))

    ;; There is a separate parsing procedure
    ;; for each kind of internal node of the syntax tree.

    ;; parse-program : Token-source -> Program

    (define parse-program
      (lambda (token-source)

        ;; <program> ::= <expression>

        (a-program (parse-expression token-source))))
    
    ;; parse-expression : Token-source -> Expression

    (define parse-expression
      (lambda (token-source)

        ;; Get a token
        ;; and determine which of the analyses of expressions
        ;; should be used.

        (let ((current (acquire-token token-source)))
          (cases token current

            
            ;; <expression> ::= empty-list
            (empty-list-token ()
              (parse-empty-list-exp token-source))

            ;; <expression> ::= <numeral>

            (numeral-token (value)
                           (const-exp value))

            ;; <expression> ::= - ( <expression> , <expression> )

            (minus-sign ()
                        (parse-diff-exp token-source))

            ;; <expression> ::= ( <expression> <expression> )

            (open-parenthesis ()
                              (parse-call-exp token-source))

            (comma ()
                   (report-bad-initial-token-error "A comma"))
            (close-parenthesis ()
                               (report-bad-initial-token-error "A close parenthesis"))

            ;; <expression> ::= zero? ( <expression> )

            (zero?-token ()
                         (parse-zero?-exp token-source))

            ;; <expression> ::=
            ;;          if <expression> then <expression> else <expression>

            (if-token ()
                      (parse-if-exp token-source))

            (then-token ()
                        (report-bad-initial-token-error "The keyword then"))
            (else-token ()
                        (report-bad-initial-token-error "The keyword else"))

            ;; <expression> ::= <identifier>

            (identifier-token (id)
                              (var-exp id))

            ;; <expression> ::= let <identifier> = <expression> in <expression>

            (let-token ()
                       (parse-let-exp token-source))

            (equals-sign ()
                         (report-bad-initial-token-error "An equals sign"))
            (in-token ()
                      (report-bad-initial-token-error "The keyword in"))

            ;; <expression> ::= proc ( <identifier> ) <expression>

            (proc-token ()
                        (parse-proc-exp token-source))

            ;; [ARYAN]

            ;; <expression> ::= cons (<expression>)
            (cons-token ()
              (parse-cons-exp token-source))

            ;; <expression> ::= null? (<expression>)
            (null?-token ()
              (parse-null?-exp token-source))

            ;; <expression> ::= car (<expression>)
            (car-token ()
              (parse-car-exp token-source))

            ;; <expression> ::= cdr (<expression>)
            (cdr-token ()
              (parse-cdr-exp token-source))
          
            ;; [ARYAN]

            ;; <expression> ::= letrec <identifier> ( <identifier> ) = <expression>
            ;;                    in <expression>

            (letrec-token ()
                          (parse-letrec-exp token-source))))))

    ;; report-bad-initial-token-error : String -> ()

    (define report-bad-initial-token-error
      (lambda (bad-token-string)
        (eopl:error 'parse-expression
                    "~s may not occur at the beginning of an expression.~%"
                    bad-token-string)))

    ;; parse-diff-exp : Token-source -> DiffExp

    (define parse-diff-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((minuend (parse-expression token-source)))
          (match-and-discard token-source 'comma)
          (let ((subtrahend (parse-expression token-source)))
            (match-and-discard token-source 'close-parenthesis)
            (diff-exp minuend subtrahend)))))

    ;; parse-call-exp : Token-source -> CallExp

    (define parse-call-exp
      (lambda (token-source)
        (let* ((operator (parse-expression token-source))
               (operand (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (call-exp operator operand))))

    ;; parse-zero?-exp : Token-source -> Zero?Exp

    (define parse-zero?-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((testee (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (zero?-exp testee))))

    ;; parse-if-exp : Token-source -> IfExp

    (define parse-if-exp
      (lambda (token-source)
        (let ((condition (parse-expression token-source)))
          (match-and-discard token-source 'then-token)
          (let ((consequent (parse-expression token-source)))
            (match-and-discard token-source 'else-token)
            (let ((alternative (parse-expression token-source)))
              (if-exp condition consequent alternative))))))

    ;; parse-let-exp : Token-source -> LetExp

    (define parse-let-exp
      (lambda (token-source)
        (let ((bound-var (acquire-identifier token-source)))
          (match-and-discard token-source 'equals-sign)
          (let ((bound-value (parse-expression token-source)))
            (match-and-discard token-source 'in-token)
            (let ((body (parse-expression token-source)))
              (let-exp bound-var bound-value body))))))

    ;; acquire-identifier : Token-source -> Sym

    (define acquire-identifier
      (lambda (token-source)
        (let ((candidate (acquire-token token-source)))
          (cases token candidate
            (numeral-token (num)
                           (report-acquire-identifier-error "A numeral"))
            (minus-sign ()
                        (report-acquire-identifier-error "A minus sign"))
            (open-parenthesis ()
                              (report-acquire-identifier-error "An open parenthesis"))
            (comma ()
                   (report-acquire-identifier-error "A comma"))
            (close-parenthesis ()
                               (report-acquire-identifier-error "A close parenthesis"))
            (zero?-token ()
                         (report-acquire-identifier-error "The keyword zero?"))
            (if-token ()
                      (report-acquire-identifier-error "The keyword if"))
            (then-token ()
                        (report-acquire-identifier-error "The keyword then"))
            (else-token ()
                        (report-acquire-identifier-error "The keyword else"))
            (identifier-token (id) id)
            (let-token ()
                       (report-acquire-identifier-error "The keyword let"))
            (equals-sign ()
                         (report-acquire-identifier-error "An equals sign"))
            (in-token ()
                      (report-acquire-identifier-error "The keyword in"))
            (proc-token ()
                        (report-acquire-identifier-error "The keyword proc"))
            (letrec-token ()
                          (report-acquire-identifier-error "The keyword letrec"))))))

    ;; report-acquire-identifier-error : String -> ()

    (define report-acquire-identifier-error
      (lambda (bad-token-string)
        (eopl:error 'acquire-identifier
                    "~s was found in place of an identifier.~%"
                    bad-token-string)))

    ;; parse-proc-exp : Token-source -> ProcExp

    (define parse-proc-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((parameter (acquire-identifier token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (let ((body (parse-expression token-source)))
            (proc-exp parameter body)))))

    ;; [ARYAN]


    ;; cons (expression, expression)
    (define parse-cons-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((thing (parse-expression token-source)))
          (match-and-discard token-source 'comma)
          (let ((other-thing (parse-expression token-source)))
            (match-and-discard token-source 'close-parenthesis)
            (cons-exp thing other-thing)))))


    ;; null? (expression)
    (define parse-null?-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((to-test (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (null?-exp to-test))))

    ;; car (expression)
    (define parse-car-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((lst (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (car-exp lst))))
          
    ;; cdr (expression)
    (define parse-cdr-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((lst (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (cdr-exp lst))))

    ;; empty-list
    (define parse-empty-list-exp
      (lambda (token-source)
        (empty-list-exp)))


    ;; [ARYAN]

    ;; parse-letrec-exp : Token-source -> LetrecExp

    (define parse-letrec-exp
      (lambda (token-source)
        (let ((procedure-name (acquire-identifier token-source)))
          (match-and-discard token-source 'open-parenthesis)
          (let ((bound-var (acquire-identifier token-source)))
            (match-and-discard token-source 'close-parenthesis)
            (match-and-discard token-source 'equals-sign)
            (let ((procedure-body (parse-expression token-source)))
              (match-and-discard token-source 'in-token)
              (let ((letrec-body (parse-expression token-source)))
                (letrec-exp procedure-name
                            bound-var
                            procedure-body
                            letrec-body)))))))

    ;; The scan&parse procedure
    ;; takes a string or an input port as its argument
    ;; and returns a syntax tree for the program
    ;; that the source provides.

    ;; scan&parse : SchemeVal -> Program
   
    (define scan&parse
      (lambda (given)
        (let* ((token-source (scanner (make-character-source given)))
               (syntax-tree (parse-program token-source)))
          (if (token-source 'at-end?)
              syntax-tree
              (report-leftover-tokens-error)))))

    (define report-leftover-tokens-error
      (lambda ()
        (eopl:error 'scan&parse
                    "There were extra, unusable tokens at the end of the program.")))))


;;; Copyright (C) 2009, 2015, 2019  John David Stone

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