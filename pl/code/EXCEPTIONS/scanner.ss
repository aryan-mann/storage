#!r7rs

;;; A scanner for the EXCEPTIONS language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 8, 2009
;;; last revised July 25, 2019

;;; This file provides a scanner
;;; for the EXCEPTIONS language
;;; developed by Daniel P. Friedman and Mitchell Wand
;;; in section 5.4 of their book
;;; _Essentials of programming languages_ (third edition).

(define-library (EXCEPTIONS scanner)
  (export scanner)
  (import (scheme base)
          (scheme char)
          (utilities eopl)
          (utilities character-sources)
          (EXCEPTIONS tokens))

  (begin

  ;; The scanner is implemented as a procedure
  ;; that takes a character source as its argument
  ;; and returns a "token source" object,
  ;; which responds to the same three messages
  ;; as a character source,
  ;; (namely at-end?, peek, and get)
  ;; but returns a token rather than a character
  ;; in response to peek and get messages.

  ;; scanner : Character-source -> Token-source

  (define scanner
    (lambda (character-source)

      ;; When we first peek at a token,
      ;; we'll have to get all of the characters that make it up
      ;; from the character source.
      ;; Since we can get those characters only once, however,
      ;; we'll have to store the token
      ;; so that we can still return it
      ;; when the next peek or get message is received.
      ;; The local variable named buffer
      ;; holds the next available token,
      ;; if we have peeked at it,
      ;; or the symbol empty if we have not.

      (let ((buffer 'empty))
        (lambda (message)

          ;; When the token source that scanner returns
          ;; receives any message,
          ;; it first discards any whitespace
          ;; and any comments
          ;; that it finds in the given character source.
          ;; This ensures that the character source
          ;; is either "at end"
          ;; or positioned to yield a non-whitespace,
          ;; non-comment character.

          (skip-whitespace character-source)
          (case message

            ;; Then, if the message is at-end?,
            ;; the token source returns #t
            ;; if the buffer is empty
            ;; and the character source can supply
            ;; no more input.
            ;; Otherwise, another token may and should be available,
            ;; so the token source returns #f.

            ((at-end?) (and (eqv? buffer 'empty)
                            (character-source 'at-end?)))

            ;; If the message is peek,
            ;; the token source returns the token from the buffer,
            ;; filling it first if it starts out empty.

            ((peek) (when (eqv? buffer 'empty)
                      (set! buffer (get-token character-source)))
                    buffer)
 
            ;; If the message is get,
            ;; the token source returns the token from the buffer
            ;; if there is one
            ;; (emptying out the buffer along the way)
            ;; or the next token from the character source
            ;; if there is no token in the buffer.

            ((get) (if (eqv? buffer 'empty)
                       (get-token character-source)
                       (let ((result buffer))
                         (set! buffer 'empty)
                         result)))

            (else (report-bad-message-error message)))))))

  ;; report-bad-message-error : Symbol -> (aborts the computation)

  (define report-bad-message-error
    (lambda (non-message)
      (eopl:error 'token-source
                  "The message ~a was not recognized.~%"
                  non-message)))

  ;; Comments in the EXCEPTIONS language
  ;; begin with a percentage sign and
  ;; end at the next following newline.

  ;; comment-starter : Char

  (define comment-starter #\%)

  ;; comment-ender : Char

  (define comment-ender #\newline)

  ;; The discard-comment procedure is invoked
  ;; when the beginning of a comment has been detected (and consumed).
  ;; It consumes characters from the source
  ;; up to and including a newline character
  ;; (unless it reaches the end of the source first,
  ;; in which case it consumes all of the remaining
  ;; characters from the source).

  ;; discard-comment : Character-source -> ()

  (define discard-comment
    (lambda (source)
      (let loop ()
        (unless (or (source 'at-end?)
                    (char=? (source 'get) comment-ender))
          (loop)))))

  ;; The skip-whitespace procedure discards whitespace
  ;; and comments from the character source.
  ;; Its postcondition is that
  ;; either the character source
  ;; has no more available characters,
  ;; or the next available character
  ;; is a non-whitespace character
  ;; that is not part of a comment.

  ;; skip-whitespace : Character-source -> ()

  (define skip-whitespace
    (lambda (source)
      (let loop ()
        (unless (source 'at-end?)
          (let ((next (source 'peek)))
            (cond ((char-whitespace? next)
                   (source 'get)
                   (loop))
                  ((char=? next comment-starter)
                   (source 'get)
                   (discard-comment source)
                   (loop))))))))

  ;; The get-token procedure consumes the text of one token
  ;; from the character source
  ;; and constructs and returns the appropriate token.
  ;; It is a precondition of this procedure
  ;; that the next available character can begin a token.

  ;; get-token : Character-source -> Token

  (define get-token
    (lambda (source)
      (let ((next (source 'peek)))
        (cond ((char-numeric? next)
               (numeral-token (get-numeral-value source)))
              ((char-alphabetic? next)
               (let ((id (get-identifier source)))
                 (case id
                   ((zero?) (zero?-token))
                   ((if) (if-token))
                   ((then) (then-token))
                   ((else) (else-token))
                   ((let) (let-token))
                   ((in) (in-token))
                   ((proc) (proc-token))
                   ((letrec) (letrec-token))
                   ((try) (try-token))
                   ((catch) (catch-token))
                   ((raise) (raise-token))
                   ((cons) (cons-token))
                   ((car) (car-token))
                   ((cdr) (cdr-token))
                   ((null?) (null?-token))
                   ((emptylist) (emptylist-token))
                   ((list) (list-token))
                   (else (identifier-token id)))))
              (else
               (source 'get)
               (case next
                 ((#\-) (scan-after-minus source))
                 ((#\() (open-parenthesis))
                 ((#\,) (comma))
                 ((#\)) (close-parenthesis))
                 ((#\=) (equals-sign))
                 (else
                  (report-mislexical-error next))))))))

  ;; report-mislexical-error : Char -> (aborts the computation)

  (define report-mislexical-error
    (lambda (bad-character)
      (eopl:error 'scanner
                  "A token may not begin with the ~a character.~%"
                  bad-character)))

  ;; As we collect the characters that make up a numeral,
  ;; we'll need to compute its value.
  ;; The digit-value procedure assists this process.
  ;; It returns the natural number denoted by any given digit character
  ;; (in ASCII; to make this work for Unicode would be much trickier).

  ;; digit-value : Char -> Int

  (define digit-value
    (let ((zero-slot (char->integer #\0)))
      (lambda (digit)
        (- (char->integer digit) zero-slot))))
  
  ;; The get-numeral-value procedure
  ;; consumes an uninterrupted sequence of digit characters
  ;; from the character source
  ;; and computes and returns the value of the numeral
  ;; that they compose.
  ;; It is a precondition of this procedure
  ;; that the next available character is a digit character.

  ;; get-numeral-value : Character-source -> Natural-number

  (define get-numeral-value
    (lambda (source)
      (let loop ((value (digit-value (source 'get))))
        (if (or (source 'at-end?)
                (not (char-numeric? (source 'peek))))
            value
            (loop (+ (* value 10)
                     (digit-value (source 'get))))))))

  ;; As we collect the characters that make up a keyword or an identifier,
  ;; we'll need to know where to stop.
  ;; The successor? predicate tests whether a given character
  ;; can appear in a keyword or identifier
  ;; (after the initial position, which must be a letter).
  ;; The characters permitted here
  ;; are letters, digits, hyphens, and question marks.

  ;; successor? : Char -> Bool

  (define successor?
    (lambda (ch)
      (or (char-alphabetic? ch)
          (char-numeric? ch)
          (char=? ch #\-)
          (char=? ch #\?))))

  ;; The get-identifier procedure consumes an uninterrupted sequence
  ;; of letters, digits, and question marks
  ;; from the character source,
  ;; assembles them into a string,
  ;; and constructs and returns the symbol
  ;; named by that string.
  ;; It is a precondition of this procedure
  ;; that the next available character is a letter.

  ;; get-identifier : Character-source -> Sym

  (define get-identifier
    (lambda (source)
      (let loop ((reversed-name (list (source 'get))))
        (if (or (source 'at-end?)
                (not (successor? (source 'peek))))
            (string->symbol (list->string (reverse reversed-name)))
            (loop (cons (source 'get) reversed-name))))))

  ;; The scan-after-minus procedure determines whether
  ;; a minus sign that has just been discovered (and consumed)
  ;; is a subtraction operator
  ;; or the beginning of a numeral,
  ;; then consumes the rest of the token's text (if any)
  ;; from the character source and returns the token.

  ;; scan-after-minus : Character-source -> Token

  (define scan-after-minus
    (lambda (source)
      (if (source 'at-end?)
          (minus-sign)
          (let ((next (source 'peek)))
            (if (char-numeric? next)
                (numeral-token (- (get-numeral-value source)))
                (minus-sign))))))))

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
