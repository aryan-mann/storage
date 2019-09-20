#!r7rs

;;; Character sources

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created January 15, 2003
;;; last revised July 9, 2019

;;; This file contains a procedure, make-character-source,
;;; that enables the programmer
;;; to pull in characters of program text
;;; from a string, an open input port, or standard input,
;;; hiding the differences
;;; between the different input methods required.

(define-library (utilities character-sources)

  (export make-character-source)
  (import (scheme base)
          (scheme file)
          (utilities eopl))

  (begin

    ;; make-character-source : SchemeVal -> Character-source

    ;; A character source is implemented as a unary procedure
    ;; that responds to three messages:
    ;;
    ;;   * at-end?, which causes it to return a Boolean value --
    ;;     #f if there are any more characters available from the source,
    ;;     #t if there are not.
    ;;
    ;;   * peek, which causes it to return the next available character
    ;;     from the source,
    ;;     but does not "consume" that character.
    ;;
    ;;   * get, which causes it to return the next available character
    ;;     from the source
    ;;     and renders that character subsequently unavailable.
    ;;
    ;; It is an error to send a peek or get message to any character source
    ;; that has no more characters available.

    (define make-character-source
      (lambda (basis)
        (cond ((string? basis)
               (let ((position 0)
                     (len (string-length basis)))
                 (lambda (message)
                   (case message
                     ((at-end?) (= position len))
                     ((peek)
                      (if (< position len)
                          (string-ref basis position)
                          (report-source-exhaustion-error 'peek basis)))
                     ((get)
                      (if (< position len)
                          (let ((result (string-ref basis position)))
                            (set! position (+ position 1))
                            result)
                          (report-source-exhaustion-error 'get basis)))
                     (else
                      (report-bad-message-error message))))))
              ((input-port? basis)
               (lambda (message)
                 (case message
                   ((at-end?) (eof-object? (peek-char basis)))
                   ((peek)
                    (let ((next (peek-char basis)))
                      (if (eof-object? next)
                          (report-source-exhaustion-error 'peek basis)
                          next)))
                   ((get)
                    (let ((next (peek-char basis)))
                      (if (eof-object? next)
                          (report-source-exhaustion-error 'get basis)
                          (read-char basis))))
                   (else
                    (report-bad-message-error message)))))
              (else
               (report-bad-source-error basis)))))

    ;; report-source-exhaustion-error :
    ;;                        Symbol * SchemeVal -> (aborts the computation)

    (define report-source-exhaustion-error
      (lambda (operation basis)
        (let ((location (string->symbol
                         (string-append "character-source:"
                                        (symbol->string operation)))))
          (eopl:error location
                      "The source ~a can provide no more characters.~%"
                      basis))))
    
    ;; report-bad-message-error : Symbol -> (aborts the computation)

    (define report-bad-message-error
      (lambda (non-message)
        (eopl:error 'character-source
                    "The message ~a was not recognized.~%"
                    non-message)))

    ;; report-bad-source-error : SchemeVal -> (aborts the computation)

    (define report-bad-source-error
      (lambda (non-source)
        (eopl:error 'make-character-source
                    "~a is not a valid source of characters.~%"
                    non-source)))))


;;; Copyright (C) 2003, 2009, 2015, 2019  John David Stone

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
