#!r7rs

;;; Procedure sectioning for R7RS Scheme
;;;
;;; Al Petrovsky
;;; http://srfi.schemers.org/srfi-26/mail-archive/msg00070.html
;;; http://srfi.schemers.org/srfi-26/mail-archive/msg00072.html
;;; 
;;; Documented, adapted, ported to R7RS, and extended by
;;;   John David Stone
;;;   Department of Computer Science
;;;   Grinnell College
;;;
;;; created August 17, 2011
;;; last revised June 25, 2019

(define-library (utilities section)
  (export section)
  (import (scheme base))
  (begin

    ;; The syntax extension with the keyword section
    ;; provides a concise notation for "procedure sections,"
    ;; that is, specializations of a multi-argument procedure
    ;; formed by substituting fixed expressions
    ;; for some of its parameters
    ;; and removing them from the parameter list.

    ;; A section-expression consists of the keyword section
    ;; and one or more subexpressions,
    ;; each of which can be either an ordinary Scheme expression
    ;; or the literal <> ("slot").
    ;; The value of the section-expression
    ;; is a procedure with a parameter
    ;; for each slot in the section-expression
    ;; and a body consisting of the subexpressions
    ;; of the section-expression,
    ;; but with each slot replaced by the corresponding parameter.

    ;; The last subexpression in a section-expression
    ;; can also be the literal <...> ("rest-slot").
    ;; In this case, the value of the section-expression
    ;; is a variable-arity procedure with a "rest" parameter.
    ;; When and if the procedure is called,
    ;; the values of any residual arguments are bundled as a list,
    ;; which becomes the value of the rest parameter.

    ;; This construction was originally proposed
    ;; (under the name `cut') in Scheme Request for Implementation 26
    ;; (http://srfi.schemers.org/srfi-26/srfi-26.html),
    ;; which was finalized and adopted on June 14, 2002.
    ;; In that document, the designer of the construction,
    ;; Sebastian Egner,
    ;; offered the following examples of its use:
    ;;
    ;;      (section cons (+ a 1) <>) expands to
    ;;          (lambda (x2) (cons (+ a 1) x2))
    ;;
    ;;      (section list 1 <> 3 <> 5) expands to
    ;;          (lambda (x2 x4) (list 1 x2 3 x4 5))
    ;;
    ;;      (section list) expands to
    ;;          (lambda () (list))
    ;;
    ;;      (section list 1 <> 3 <...>) expands to
    ;;          (lambda (x2 . xs) (apply list 1 x2 3 xs))
    ;;
    ;;      (section <> a b) expands to
    ;;          (lambda (f) (f a b))
    ;;
    ;;      Double every element of a given list:
    ;;          (map (section * 2 <>) '(1 2 3 4))
    ;;
    ;;      Destructively zero out selected positions in a vector:
    ;;          (map (section vector-set! x <> 0) indices)
    ;;
    ;;      Write the values of several expressions to an output port:
    ;;          (for-each (section write <> port) exprs)
    ;;
    ;;      List the least and greatest of certain values:
    ;;          (map (section <> x y z) (list min max))
    ;;
    ;;      Invoke a sequence of thunks:
    ;;          (for-each (section <>) thunks)

    ;; The internal-sect macro keeps track of the information
    ;; needed at intermediate stages of expansion of a section-expression.
    ;; The plan is to process the subexpressions of the section-expression
    ;; one by one.
    ;; When a slot is encountered,
    ;; a new parameter is generated
    ;; and added both to the parameter list
    ;; and to the body of the procedure that is being constructed;
    ;; a non-slot subexpression
    ;; is simply transferred to the body unchanged.

    ;; The general form of a use of this macro is
    ;;
    ;;            (internal-sect slot-names combination . cs)
    ;;
    ;; Here slot-names is the list of parameters
    ;; that have been generated so far,
    ;; combination is the part
    ;; of the (single-expression) procedure body
    ;; that has been generated so far,
    ;; and cs comprises the subexpressions
    ;; of the original section-expression
    ;; that have not yet been processed.
    ;; Initially, slot-names and combination are empty lists,
    ;; and all of the subexpressions of the original section-expression
    ;; are stored in cs.
    ;;
    ;; Once all of the subexpressions
    ;; of the original section-expression
    ;; have been processed (except possibly for a trailing <...>),
    ;; a lambda-expression for the procedure is assembled,
    ;; and it becomes the result of the macro expansion.
    ;;
    ;; The identifier x is used
    ;; for each parameter that corresponds to a slot
    ;; as it is encountered,
    ;; but Scheme's hygienic macro system
    ;; ensures that different replacement identifiers
    ;; are generated automatically during expansion.
    ;;
    ;; I have extended Egner's proposal (and Petrovsky's implementation)
    ;; to accommodate examples of the form
    ;;
    ;;                    (section reverse <...>)
    ;;
    ;; where the purpose of the slot is to collect all of the arguments,
    ;; converting the specified procedure
    ;; from one that operates on a single list argument
    ;; into one that takes any number of arguments
    ;; and bundles them into a list
    ;; before performing the procedure.

    (define-syntax internal-sect
      (syntax-rules (<> <...>)

        ;; The first three syntax rules
        ;; construct fixed- and variable-arity procedures
        ;; once all of the subexpressions
        ;; of the original section-expression
        ;; have been processed.

        ;; The unusual-looking form "(begin proc)"
        ;; ensures that a non-expression in operator position,
        ;; such as "(section quote <>)",
        ;; is reported as an error.

        ((internal-sect (slot-name ...) (proc arg ...))
         (lambda (slot-name ...) ((begin proc) arg ...)))
        ((internal-sect () (proc) <...>)
         (lambda rest-slot ((begin proc) rest-slot)))
        ((internal-sect (slot-name ...) (proc arg ...) <...>)
         (lambda (slot-name ... . rest-slot)
           (apply proc arg ... rest-slot)))

        ;; The fourth and fifth syntax rules
        ;; process a single subexpression
        ;; of the original section-expression.
        ;; If the subexpression is a slot,
        ;; a parameter is generated
        ;; and appended to the parameter list
        ;; and to the future procedure body:

        ((internal-sect (slot-name ...) (position ...) <> . cs)
         (internal-sect (slot-name ... x) (position ... x) . cs))

        ;; If it is not a slot,
        ;; it is just transferred to the procedure body.

        ((internal-sect (slot-name ...) (position ...) const . cs)
         (internal-sect (slot-name ...) (position ... const) . cs))))

    ;; A section-expression expands into a use of internal-sect with
    ;; appropriate initial subexpressions.

    (define-syntax section
      (syntax-rules ()
        ((section . consts-or-slots)
         (internal-sect () () . consts-or-slots))))))

;;; This code is in the public domain.
;;; Al Petrovsky's statement of donation
;;; is available on the World Wide Web
;;; at http://srfi.schemers.org/srfi-26/mail-archive/msg00077.html.
