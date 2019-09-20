#!r7rs

;;; A type checker for the TYPED-OO language

;;; Daniel P. Friedman (dfried@cs.indiana.edu)
;;; Mitchell Wand (wand@ccs.neu.edu)

;;; This file provides a type-checker
;;; for the TYPED-OO language
;;; developed by the authors
;;; in section 9.5 of their book
;;; _Essentials of programming languages_ (third edition).

;;; adapted and ported to R7RS Scheme by
;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; last revised August 5, 2019

(define-library (TYPED-OO type-checker)
  (export type-of-program type-to-external-form)
  (import (scheme base)
          (utilities eopl)
          (TYPED-OO syntax-trees))
  (begin

    ;; During its static analysis of a program's text,
    ;; the type checker keeps track of the type
    ;; of every bound variable
    ;; in a "type environment" --
    ;; a mapping from identifiers to their declared or inferred types.

    (define-datatype type-environment type-environment?
      (empty-tenv)
      (extend-tenv (vars (list-of identifier?))
                   (tys (list-of type?))
                   (saved type-environment?))
      (extend-tenv-with-self-and-super (self type?)
                                       (super identifier?)
                                       (saved type-environment?)))

    ;;; The apply-tenv procedure
    ;;; looks up a given symbol in a given type environment.

    ;; apply-tenv : Tenv * Identifier -> Type

    (define apply-tenv
      (lambda (tenv sought)
        (let kernel ((remaining tenv))
          (cases type-environment remaining
            (empty-tenv ()
              (report-untyped-identifier-error sought))
            (extend-tenv (vars tys saved)
              (let inner-loop ((rest-of-vars vars)
                         (rest-of-tys tys))
                (cond ((null? rest-of-vars) (kernel saved))
                      ((eqv? (car rest-of-vars) sought) (car rest-of-tys))
                      (else (inner-loop (cdr rest-of-vars)
                                        (cdr rest-of-tys))))))
            (extend-tenv-with-self-and-super (self super saved)
              (case sought
                ((%self) self)
                ((%super) super)
                (else (kernel saved))))))))

    (define report-untyped-identifier-error
      (lambda (violator)
        (eopl:error 'apply-tenv
                    "No declared or inferred type for ~s.~%"
                    violator)))

    ;; TYPED-OO programs are evaluated in an empty initial environment.
    ;; The init-tenv procedure constructs the corresponding type environment.

    ;; init-tenv : () -> Tenv

    (define init-tenv
      (lambda ()
        (empty-tenv)))

    ;; The type checker also maintains a separate, global environment
    ;; for static information about the classes and interfaces
    ;; that the program declares.
    ;; This static information is assembled into a "static class."

    (define-datatype static-class static-class?
      (a-static-class (super-name (maybe identifier?))
                      (interface-names (list-of identifier?))
                      (field-types (list-of type?))
                      (field-names (list-of identifier?))
                      (method-tenv method-tenv?))
      (a-static-interface (method-tenv method-tenv?)))


    ;; static-class->super-name : StaticClass -> Maybe(Identifier)

    (define static-class->super-name
      (lambda (sc)
        (cases static-class sc
          (a-static-class (super-name interface-names field-types field-names
                           method-tenv)
            super-name)
          (a-static-interface (method-tenv)
            (report-interface-as-class-error 'static-class->super-name
                                             sc
                                             "has no superclass name")))))

    (define report-interface-as-class-error
      (lambda (context violator missing-component)
        (eopl:error context
                    "~s is an interface and ~s.~%"
                    violator
                    missing-component)))

    ;; static-class->interface-names : StaticClass -> Listof(Identifier)

    (define static-class->interface-names
      (lambda (sc)
        (cases static-class sc
          (a-static-class (super-name interface-names field-types field-names
                           method-tenv)
            interface-names)
          (a-static-interface (method-tenv)
            (report-interface-as-class-error 'static-class->interface-names
                                             sc
                                             "implements no interfaces")))))

    ;; static-class->field-types : StaticClass -> Listof(Type)

    (define static-class->field-types
      (lambda (sc)
        (cases static-class sc
          (a-static-class (super-name interface-names field-types field-names
                           method-tenv)
            field-types)
          (a-static-interface (method-tenv)
            (report-interface-as-class-error 'static-class->field-types
                                             sc
                                             "has no fields")))))

    ;; static-class->field-names : StaticClass -> Listof(Identifier)

    (define static-class->field-names
      (lambda (sc)
        (cases static-class sc
          (a-static-class (super-name interface-names field-types field-names
                           method-tenv)
            field-names)
          (a-static-interface (method-tenv)
            (report-interface-as-class-error 'static-class->field-names
                                             sc
                                             "has no fields")))))

    ;; static-class->method-tenv : StaticClass -> MethodTenv

    (define static-class->method-tenv
      (lambda (sc)
        (cases static-class sc
          (a-static-class (super-name interface-names field-types field-names
                           method-tenv)
            method-tenv)
          (a-static-interface (method-tenv)
            method-tenv))))

    ;; The global environment is a list of two-element lists,
    ;; each containing a class name and the class it denotes.

    ;; the-static-class-env : StaticClassEnv

    (define the-static-class-env '())

    ;; The initialize-static-class-env! procedure
    ;; puts a program's class declarations
    ;; into its global class environment.

    ;; initialize-static-class-env! : Listof(ClassDecl) -> Unspecified

    (define initialize-static-class-env!
      (lambda (c-decls)
        (empty-the-static-class-env!)
        (add-static-class-binding!
          'object (a-static-class #f '() '() '() '()))
        (for-each add-class-decl-to-static-class-env! c-decls)))

    ;; empty-the-static-class-env! : () -> Unspecified

    (define empty-the-static-class-env!
      (lambda ()
        (set! the-static-class-env '())))

    ;; add-static-class-binding! : Identifier * StaticClass -> Unspecified

    (define add-static-class-binding!
      (lambda (name sc)
        (set! the-static-class-env
              (cons (list name sc) the-static-class-env))))

    ;; add-class-decl-to-static-class-env! : ClassDecl -> Unspecified

    (define add-class-decl-to-static-class-env!
      (lambda (c-decl)
        (cases class-declaration c-decl
          (an-interface-declaration (interface-name abstract-method-decls)
            (let ((m-tenv
                    (abstract-method-decls->method-tenv
                     abstract-method-decls)))
              (check-no-dups! (map car m-tenv) interface-name)
              (add-static-class-binding! interface-name
                                         (a-static-interface m-tenv))))
          (a-class-declaration (class-name super-name interface-names
                                field-types field-names method-decls)
            (let ((super-class (lookup-static-class super-name)))
              (let ((revised-interface-names
                      (append (static-class->interface-names super-class)
                              interface-names))
                    (revised-field-types
                      (append (static-class->field-types super-class)
                              field-types))
                    (revised-field-names
                      (append-field-names
                        (static-class->field-names super-class)
                        field-names))
                    (method-tenv
                      (let ((local-method-tenv
                              (method-decls->method-tenv method-decls)))
                        (check-no-dups! (map car local-method-tenv) class-name)
                        (merge-method-tenvs
                          (static-class->method-tenv super-class)
                          local-method-tenv))))
                (check-no-dups! revised-interface-names class-name)
                (check-no-dups! revised-field-names class-name)
                (check-for-initialize! method-tenv class-name)
                (add-static-class-binding!
                  class-name
                  (a-static-class super-name
                                  revised-interface-names
                                  revised-field-types
                                  revised-field-names
                                  method-tenv))))))))

    ;; lookup-static-class : Identifier -> StaticClass

    (define lookup-static-class
      (lambda (name)
        (let ((maybe-pair (assq name the-static-class-env)))
          (if maybe-pair
              (cadr maybe-pair)
              (report-static-class-name-error name)))))

    (define report-static-class-name-error
      (lambda (unfound-name)
        (eopl:error 'lookup-static-class
                    "~s is not the name of a class.~%"
                    unfound-name)))

    ;; A method type environment is a list of two-element lists,
    ;; each comprising a method name and its type.

    ;; method-tenv? : SchemeVal -> Bool

    (define method-tenv?
      (list-of (lambda (element)
                (and (pair? element)
                     (identifier? (car element))
                     (pair? (cdr element))
                     (type? (cadr element))
                     (null? (cddr element))))))

    ;; We build a method type environment from the method's declarations.

    ;; method-decls->method-tenv : Listof(MethodDecl) -> MethodTenv

    (define method-decls->method-tenv
      (lambda (m-decls)
        (map (lambda (m-decl)
               (cases method-declaration m-decl
                 (a-method-declaration (result-type method-name parameters
                                        parameter-types body)
                   (list method-name (proc-type parameter-types result-type)))))
             m-decls)))

    ;; The same approach works if our method declarations are abstract.

    ;; abstract-method-decls->method-tenv :  Listof(AbstractMethodDecl) -> MethodTenv

    (define abstract-method-decls->method-tenv
      (lambda (m-decls)
        (map (lambda (m-decl)
               (cases abstract-method-declaration m-decl
                 (an-abstract-method-declaration (result-type method-name
                                                  parameters parameter-types)
                   (list method-name (proc-type parameter-types result-type)))))
             m-decls)))

    ;; The find-method-type performs a lookup in the static class environment
    ;; and then in the class's method type environment.

    ;; find-method-type : Identifier * Identifier -> Type

    (define find-method-type
      (lambda (c-name name)
        (or (maybe-find-method-type c-name name)
            (report-no-such-method-type-error name c-name))))

    (define report-no-such-method-type-error
      (lambda (unfound-method-name c-name)
        (eopl:error 'find-method-type
                    "There is no method named ~s in class ~s.~%"
                    unfound-method-name
                    c-name)))

    ;; maybe-find-method-type : Identifier * Identifier -> (Type or Bool)

    (define maybe-find-method-type
      (lambda (c-name name)
        (let ((m-env (static-class->method-tenv (lookup-static-class c-name))))
          (let ((maybe-pair (assv name m-env)))
            (and (pair? maybe-pair)
                 (cadr maybe-pair))))))

    ;; merge-method-tenvs : MethodTenv * MethodTenv -> MethodTenv

    (define merge-method-tenvs
      (lambda (super-m-tenv new-m-tenv)
        (append new-m-tenv super-m-tenv)))

    ;; check-no-dups! : Listof(Identifier) * Identifier -> Unspecified

    (define check-no-dups!
      (lambda (ls class-name)
        (unless (null? ls)
          (let loop ((first (car ls))
                     (rest (cdr ls)))
            (unless (null? rest)
              (if (memv first rest)
                  (report-duplicate-identifier-error first class-name)
                  (loop (car rest) (cdr rest))))))))

    (define report-duplicate-identifier-error
      (lambda (duplicated-identifier class-name)
        (eopl:error 'check-no-dups!
                    "The identifier ~s was duplicated in the definition of the class ~s.~%")
                    duplicated-identifier
                    class-name))

    ;; check-for-initialize! : MethodTenv * Identifier -> Unspecified

    (define check-for-initialize!
      (lambda (method-tenv class-name)
        (or (assv 'initialize method-tenv)
            (report-initialize-check-error class-name))))

    (define report-initialize-check-error
      (lambda (class-name)
        (eopl:error 'check-for-initialize
                    "Class ~s contains no initialize method.~%"
                    class-name)))

    ;; append-field-names : Listof(Identifier) * Listof(Identifier) -> Listof(Identifier)

    (define append-field-names
      (lambda (super-fields new-fields)
        (if (null? super-fields)
            new-fields
            (cons (if (memv (car super-fields) new-fields)
                      (fresh-identifier (car super-fields))
                      (car super-fields))
                  (append-field-names (cdr super-fields) new-fields)))))

    ;; fresh-identifier : Identifier -> Identifier

    (define fresh-identifier
      (let ((counter 0))
        (lambda (base)
          (set! counter (+ counter 1))
          (string->symbol (string-append (symbol->string base)
                                         "%"
                                         (number->string counter))))))

    ;; The check-is-subtype! procedure
    ;; determines whether its first argument is a subtype of (or equal to)
    ;; its second argument
    ;; and raises an error if it is not.

    ;; check-is-subtype! : Type * Type * Exp -> Unspecified

    (define check-is-subtype!
      (lambda (ty1 ty2 exp)
        (unless (is-subtype? ty1 ty2)
          (report-subtype-failure (type-to-external-form ty1)
                                  (type-to-external-form ty2)
                                  exp))))

    ;; is-subtype? : Type * Type -> Bool

    (define is-subtype?
      (lambda (ty1 ty2)
        (cases type ty1
          (int-type ()
            (cases type ty2
              (int-type () #t)
              (else #f)))
          (bool-type ()
            (cases type ty2
              (bool-type () #t)
              (else #f)))
          (proc-type (arg-types1 result-type1)
            (cases type ty2
              (proc-type (arg-types2 result-type2)
                (and (every2? is-subtype? arg-types2 arg-types1)
                     (is-subtype? result-type1 result-type2)))
              (else #f)))
          (void-type ()
            (cases type ty2
              (void-type () #t)
              (else #f)))
          (class-type (name1)
            (cases type ty2
              (class-type (name2)
                (statically-is-subclass? name1 name2))
              (else #f)))
          (list-type (base-type1)
            (cases type ty2
              (list-type (base-type2)
                (is-subtype? base-type1 base-type2))
              (else #f))))))

    (define every2?
      (lambda (predicate left-ls right-ls)
        (let kernel ((left-rest left-ls)
                     (right-rest right-ls))
          (or (and (null? left-rest) (null? right-rest))
              (and (not (null? left-rest))
                   (not (null? right-rest))
                   (predicate (car left-rest) (car right-rest))
                   (kernel (cdr left-rest) (cdr right-rest)))))))

    ;; statically-is-subclass? : Identifier * Identifier -> Bool

    (define statically-is-subclass?
      (lambda (name1 name2)
        (or (eqv? name1 name2)
            (let ((super-name
                    (static-class->super-name (lookup-static-class name1))))
              (and super-name
                   (statically-is-subclass? super-name name2)))
            (let ((interface-names (static-class->interface-names
                                     (lookup-static-class name1))))
              (if (memv name2 interface-names) #t #f)))))

    ;; The report-subtype-failure procedure signals a type mismatch
    ;; and provides a helpful error message.

    ;; report-subtype-failure : String * String * Exp -> (does not return)

    (define report-subtype-failure
      (lambda (ty1 ty2 exp)
        (eopl:error 'check-is-subtype!
                    "~s is not a subtype of ~s in ~a.~%"
                    ty1
                    ty2
                    exp)))

    ;; The type-to-external-form procedure
    ;; returns a Scheme string
    ;; that mimics the external representation of types in TYPED-OO programs.

    ;; type-to-external-form : Type -> String

    (define type-to-external-form
      (lambda (ty)
        (cases type ty
          (int-type () "int")
          (bool-type () "bool")
          (proc-type (arg-types result-type)
            (if (null? arg-types)
                (string-append "(-> "
                               (type-to-external-form result-type)
                               ")")
                (let loop ((so-far (type-to-external-form (car arg-types)))
                           (rest (cdr arg-types)))
                  (if (null? rest)
                      (string-append "("
                                     so-far
                                     " -> "
                                     (type-to-external-form result-type)
                                     ")")
                      (loop (string-append so-far
                                           " * "
                                           (type-to-external-form (car rest)))
                            (cdr rest))))))
          (void-type () "void")
          (class-type (class-name)
            (symbol->string class-name))
          (list-type (base-type)
            (string-append "listof " (type-to-external-form base-type))))))

    ;; The type-of-program procedure computes and returns
    ;; the type of a TYPED-OO program.

    ;; type-of-program : Program -> Type

    (define type-of-program
      (lambda (pgm)
        (cases program pgm
          (a-program (class-decls body)
            (initialize-static-class-env! class-decls)
            (for-each check-class-decl! class-decls)
            (type-of body (init-tenv))))))

    ;; The check-class-decl! procedure
    ;; determines whether the types specified in a class declaration are consistent,
    ;; signalling an error if any inconsistency is found.

    ;; check-class-decl! : ClassDecl -> Unspecified

    (define check-class-decl!
      (lambda (c-decl)
        (cases class-declaration c-decl
          (an-interface-declaration (interface-name abstract-method-decls)
            #t)
          (a-class-declaration (class-name super-name interface-names
                                field-types field-names method-decls)
            (let ((sc (lookup-static-class class-name)))
              (for-each (lambda (method-decl)
                          (check-method-decl! method-decl
                                              class-name
                                              super-name
                                              (static-class->field-types sc)
                                              (static-class->field-names sc)))
                        method-decls))
            (for-each (lambda (interface-name)
                        (check-if-implements! class-name interface-name))
                      interface-names)))))

    ;; check-method-decl! : MethodDecl * Identifier * Identifier *
    ;;                      Listof(Type) * Listof(Identifier) -> Unspecified

    (define check-method-decl!
      (lambda (m-decl self-name super-name field-types field-names)
        (cases method-declaration m-decl
          (a-method-declaration (result-type method-name parameter-names
                                 parameter-types body)
            (let ((tenv (extend-tenv parameter-names parameter-types
                          (extend-tenv-with-self-and-super
                            (class-type self-name)
                            super-name
                            (extend-tenv field-names field-types
                              (init-tenv))))))
              (let ((body-type (type-of body tenv)))
                (check-is-subtype! body-type result-type m-decl)
                (if (eqv? method-name 'initialize)
                    #t
                    (let ((maybe-super-type
                            (maybe-find-method-type super-name method-name)))
                      (if maybe-super-type
                          (check-is-subtype! (proc-type parameter-types
                                                        result-type)
                                             maybe-super-type
                                             body)
                          #t)))))))))

    ;; check-if-implements! : Identifier * Identifier -> Bool

    (define check-if-implements!
      (lambda (class-name interface-name)
        (cases static-class (lookup-static-class interface-name)
          (a-static-class (super-name interface-names field-types field-names
                           method-tenv)
            (report-implementing-class-error interface-name))
          (a-static-interface (method-tenv)
            (for-each (lambda (method-binding)
                        (let ((method-name (car method-binding))
                              (method-type (cadr method-binding)))
                          (let ((class-method-type
                                  (maybe-find-method-type class-name
                                                          method-name)))
                            (if class-method-type
                                (check-is-subtype! class-method-type
                                                   method-type
                                                   class-name)
                                (report-missing-method-error interface-name
                                                             method-name
                                                             class-name)))))
                      method-tenv)))))

    (define report-implementing-class-error
      (lambda (offender)
        (eopl:error 'check-if-implements!
                    "~s in not an interface and so cannot be implemented.~%"
                    offender)))

    (define report-missing-method-error
      (lambda (interface-name method-name class-name)
        (eopl:error 'check-if-implements!
                    "The interface ~s requires the method ~s, but the class ~s does not supply it.~%"
                    interface-name
                    method-name
                    class-name)))

    ;; The type-of procedure computes and returns
    ;; the type of a TYPED-OO expression.

    ;; type-of : Expression * Tenv -> Type

    (define type-of
      (lambda (exp tenv)
        (cases expression exp
          (const-exp (datum) (int-type))
          (diff-exp (minuend subtrahend)
            (let ((minuend-type (type-of minuend tenv))
                  (subtrahend-type (type-of subtrahend tenv)))
              (check-is-subtype! minuend-type (int-type) minuend)
              (check-is-subtype! subtrahend-type (int-type) subtrahend)
              (int-type)))
          (zero?-exp (testee)
            (let ((testee-type (type-of testee tenv)))
              (check-is-subtype! testee-type (int-type) testee)
              (bool-type)))
          (if-exp (condition consequent alternative)
            (let ((condition-type (type-of condition tenv))
                  (consequent-type (type-of consequent tenv))
                  (alternative-type (type-of alternative tenv)))
              (check-is-subtype! condition-type (bool-type) condition)
              (check-is-subtype! consequent-type alternative-type exp)
              consequent-type))
          (var-exp (id) (apply-tenv tenv id))
          (let-exp (bound-vars bound-values body)
            (let ((bound-value-types (types-of-exps bound-values tenv)))
              (type-of body (extend-tenv bound-vars bound-value-types tenv))))
          (proc-exp (parameters parameter-types body)
            (let ((return-type
                    (type-of body
                             (extend-tenv parameters parameter-types tenv))))
              (proc-type parameter-types return-type)))
          (call-exp (operator operands)
            (let ((operator-type (type-of operator tenv))
                  (operand-types (types-of-exps operands tenv)))
              (cases type operator-type
                (proc-type (arg-types result-type)
                  (for-each check-is-subtype! operand-types arg-types operands)
                  result-type)
                (else
                  (report-wrong-type operator-type operator "a procedure")))))
          (letrec-exp (return-types procedure-names parameter-lists
                       parameter-types procedure-bodies letrec-body)
            (let ((tenv-for-letrec-body
                    (extend-tenv procedure-names
                                 (map proc-type parameter-types return-types)
                                 tenv)))
              (let ((procedure-body-types
                      (map (lambda (procedure-body parameters p-types)
                              (type-of procedure-body
                                       (extend-tenv parameters
                                                    p-types
                                                    tenv-for-letrec-body)))
                           procedure-bodies
                           parameter-lists
                           parameter-types)))
                (for-each check-is-subtype!
                          procedure-body-types
                          return-types
                          procedure-bodies)
                (type-of letrec-body tenv-for-letrec-body))))
          (assign-exp (target source)
            (check-is-subtype! (apply-tenv tenv target)
                               (type-of source tenv)
                               source)
            (void-type))
          (begin-exp (starter sequence)
            (let loop ((last-type (type-of starter tenv))
                       (remaining sequence))
              (if (null? remaining)
                  last-type
                  (loop (type-of (car remaining) tenv) (cdr remaining)))))
          (sum-exp (augend addend)
            (let ((augend-type (type-of augend tenv))
                  (addend-type (type-of addend tenv)))
              (check-is-subtype! augend-type (int-type) augend)
              (check-is-subtype! addend-type (int-type) addend)
              (int-type)))
          (product-exp (multiplicand multiplier)
            (let ((multiplicand-type (type-of multiplicand tenv))
                  (multiplier-type (type-of multiplier tenv)))
              (check-is-subtype! multiplicand-type (int-type) multiplicand)
              (check-is-subtype! multiplier-type (int-type) multiplier)
              (int-type)))
          (cons-exp (leader followers)
            (let ((leader-type (type-of leader tenv))
                  (followers-type (type-of followers tenv)))
              (let ((joint (common-type (list-type leader-type)
                                        followers-type)))
                (or joint
                    (report-inhomogeneous-type-error leader-type followers-type exp)))))
          (car-exp (pair)
            (let ((pair-type (type-of pair tenv)))
              (cases type pair-type
                (int-type ()
                  (report-wrong-type pair-type pair "a list"))
                (bool-type ()
                  (report-wrong-type pair-type pair "a list"))
                (proc-type (arg-types result-type)
                  (report-wrong-type pair-type pair "a list"))
                (void-type ()
                  (report-wrong-type pair-type pair "a list"))
                (class-type (class-name)
                  (report-wrong-type pair-type pair "a list"))
                (list-type (base-type)
                  base-type))))
          (cdr-exp (pair)
            (let ((pair-type (type-of pair tenv)))
              (cases type pair-type
                (int-type ()
                  (report-wrong-type pair-type pair "a list"))
                (bool-type ()
                  (report-wrong-type pair-type pair "a list"))
                (proc-type (arg-types result-type)
                  (report-wrong-type pair-type pair "a list"))
                (void-type ()
                  (report-wrong-type pair-type pair "a list"))
                (class-type (class-name)
                  (report-wrong-type pair-type pair "a list"))
                (list-type (base-type)
                  pair-type))))
          (null?-exp (testee)
            (let ((testee-type (type-of testee tenv)))
              (cases type testee-type
                (int-type ()
                  (report-wrong-type testee-type testee "a list"))
                (bool-type ()
                  (report-wrong-type testee-type testee "a list"))
                (proc-type (arg-types result-type)
                  (report-wrong-type testee-type testee "a list"))
                (void-type ()
                  (report-wrong-type testee-type testee "a list"))
                (class-type (class-name)
                  (report-wrong-type testee-type testee "a list"))
                (list-type (base-type)
                  (bool-type)))))
          (emptylist-exp (base-type)
            (list-type base-type))
          (list-exp (first rest)
            (let loop ((so-far (type-of first tenv))
                       (others rest))
              (if (null? others)
                  (list-type so-far)
                  (let ((car-type (type-of (car others) tenv)))
                    (let ((common (common-type so-far car-type)))
                      (if common
                          (loop common (cdr others))
                          (report-inhomogeneous-type-error so-far car-type exp)))))))
          (new-object-exp (class-name operands)
            (let ((arg-types (types-of-exps operands tenv))
                  (c (lookup-static-class class-name)))
              (cases static-class c
                (a-static-interface (method-tenv)
                  (report-cannot-instantiate-interface-error class-name))
                (a-static-class (super-name interface-names field-types
                                 field-names method-tenv)
                  (type-of-call (find-method-type class-name 'initialize)
                                arg-types
                                operands
                                exp)
                  (class-type class-name)))))
          (method-call-exp (obj-exp method-name operands)
            (let ((arg-types (types-of-exps operands tenv))
                  (obj-type (type-of obj-exp tenv)))
              (type-of-call (find-method-type (type->class-name obj-type)
                                              method-name)
                            arg-types
                            operands
                            exp)))
          (super-call-exp (method-name operands)
            (let ((arg-types (types-of-exps operands tenv)))
              (type-of-call (find-method-type (apply-tenv tenv '%super)
                                              method-name)
                            arg-types
                            operands
                            exp)))
          (self-exp ()
            (apply-tenv tenv '%self))
          (cast-exp (body target-class-name)
            (let ((obj-type (type-of body tenv)))
              (if (class-type? obj-type)
                  (class-type target-class-name)
                  (report-cast-of-non-object-error exp))))
          (instanceof-exp (body test-class-name)
            (bool-type)))))

    (define report-inhomogeneous-type-error
      (lambda (alpha beta exp)
        (eopl:error 'type-of
                    "Lists must be homogeneous: ~s doesn't match ~s in ~s.~%"
                    alpha
                    beta
                    exp)))

    (define report-cannot-instantiate-interface-error
      (lambda (offender)
        (eopl:error 'type-of
                    "~s is an interface and cannot be instantiated.~%"
                    offender)))

    (define report-cast-of-non-object-error
      (lambda (offender)
        (eopl:error 'type-of
                    "~s is not an object and cannot be cast.~%"
                    offender)))

    (define class-type?
      (lambda (ty)
        (cases type ty
          (class-type (class-name) #t)
          (else #f))))

    ; common-type : Type * Type -> Maybe(Type)

    (define common-type
      (lambda (ty1 ty2)
        (cases type ty1
          (int-type ()
            (cases type ty2
              (int-type () ty1)
              (else #f)))
          (bool-type ()
            (cases type ty2
              (bool-type () ty1)
              (else #f)))
          (proc-type (arg-types1 result-type1)
            (cases type ty2
              (proc-type (arg-types2 result-type2)
                (proc-type (map common-type arg-types1 arg-types2)
                           (common-type result-type1 result-type2)))
              (else #f)))
          (void-type ()
            (cases type ty2
              (void-type () ty1)
              (else #f)))
          (class-type (name1)
            (cases type ty2
              (class-type (name2)
                (cond ((statically-is-subclass? name1 name2) ty2)
                      ((statically-is-subclass? name2 name1) ty1)
                      (else
                        (common-type (class-type
                                       (static-class->super-name
                                         (lookup-static-class name1)))
                                     (class-type
                                       (static-class->super-name
                                         (lookup-static-class name2)))))))
              (else #f)))
          (list-type (base-type1)
            (cases type ty2
              (list-type (base-type2)
                (list-type (common-type base-type1 base-type2)))
              (else #f))))))

    ;; The report-wrong-type raises an error
    ;; when an expression of the wrong type has been found
    ;; in some context that requires a compound type
    ;; (a procedure or a list).

    ;; report-wrong-type : Type * Expression * String -> (does not return)

    (define report-wrong-type
      (lambda (bad-type bad-operator needed)
        (eopl:error 'type-of
                    "~a is of type ~s, but the context requires ~s."
                    bad-operator
                    (type-to-external-form bad-type)
                    needed)))

    ;; type-of-call : Type * Listof(Type) * Listof(Exp) * Exp -> Type

    (define type-of-call
      (lambda (operator-type operand-types operands call-expression)
        (cases type operator-type
          (proc-type (arg-types result-type)
            (let ((arg-count (length arg-types))
                  (operand-count (length operand-types)))
              (unless (= arg-count operand-count)
                (report-wrong-number-of-arguments 
                       arg-count
                       operand-count
                       call-expression))
              (for-each check-is-subtype! operand-types arg-types operands)
              result-type))
          (else
            (report-wrong-type "operator" operator-type "a method")))))

    (define report-wrong-number-of-arguments
      (lambda (arg-count operand-count call-expression)
        (eopl:error 'type-of-call
                    "~s argument(s) expected, ~s received in ~s."
                    arg-count
                    operand-count
                    call-expression)))

    ;; type->class-name : Type -> Identifier

    (define type->class-name
      (lambda (ty)
        (cases type ty
          (class-type (class-name)
            class-name)
          (else
            (report-class-type-selector-error ty)))))

    (define report-class-type-selector-error
      (lambda (bad-type)
        (eopl:error 'type->class-name
                    "~s is not a class type.~%"
                    (type-to-external-form bad-type))))

    ;; types-of-exps : Listof(Exp) * Tenv -> Listof(Type)

    (define types-of-exps
      (lambda (exps tenv)
        (map (lambda (exp)
               (type-of exp tenv))
             exps)))))

;;; As mentioned above,
;;; the procedure definitions are derived from the work
;;; of Friedman and Wand,
;;; who made their versions available at
;;; http://www.eopl3.com/code.html
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license
;;; (http://creativecommons.org/licenses/by-nc/3.0/).

;;; The modified code presented here
;;; is copyright (C) 2009, 2015, 2019 by John David Stone
;;; is are similarly released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.
