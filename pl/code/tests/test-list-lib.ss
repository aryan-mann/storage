#!r7rs

;;; A test suite for procedures in the SRFI-1 library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created January 8, 1999
;;; last revised June 25, 2019

(import (scheme base)
        (scheme cxr)
        (srfi list-lib)
        (utilities section)
        (utilities testing))

;;; These definitions make some of the tests more concise.

(define true? (section eq? <> #t))
(define false? not)
(define always (lambda args #t))
(define never (lambda args #f))

;;; Some of the tests require a procedure
;;; that operates on any positive number of lists
;;; but is neither commutative nor associative.
;;; REVAPPALL is the one that I chose for this purpose.

(define revappend
  (lambda (reversend base)
    (do ((rest reversend (cdr rest))
         (result base (cons (car rest) result)))
        ((null? rest) result))))

(define revappall
  (lambda (first . rest)
    (let loop ((first first) (rest rest))
      (if (null? rest)
          first
          (revappend first (loop (car rest) (cdr rest)))))))

;;; Some of the tests require an equivalence relation
;;; that is coarser than usual.
;;; SAME-PARITY? is the one that I chose for this purpose.

(define same-parity?
  (lambda (m n)
    (eq? (even? m) (even? n))))

;;; Each test suite is named after the procedure that it tests.

(suite xcons ((base '(Antlia)))

  (test xcons:null-cdr
    (xcons '() 'Andromeda)
    1 (list?)
    (section equal? <> '(Andromeda)))
  
  (test xcons:pair-cdr
    (xcons base 'Apus)
    1 (list?)
    (lambda (result)
      (and (equal? result '(Apus Antlia))
           (eq? (cdr result) base))))

  (test xcons:datum-cdr
    (xcons 'Aquarius 'Aquila)
    1 (pair?)
    (section equal? <> '(Aquila . Aquarius))))


(suite list-tabulate ()

  (test list-tabulate:zero-length
    (list-tabulate 0 never)
    1 (null?))

  (test list-tabulate:identity
    (list-tabulate 5 (lambda (position) position))
    1 (list?)
    (section equal? <> '(0 1 2 3 4)))

  (test list-tabulate:factorial
    (list-tabulate 7 (lambda (position)
                       (do ((multiplier 1 (+ multiplier 1))
                            (product 1 (* product multiplier)))
                           ((< position multiplier) product))))
    1 (list?)
    (section equal? <> '(1 1 2 6 24 120 720))))


(suite cons* ()

  (test cons*:one-non-list-argument
    (cons* 'alpha)
    1 (symbol?)
    (section eq? <> 'alpha))

  (test cons*:one-null-argument
    (cons* '())
    1 (null?))

  (test cons*:one-list-argument
    (cons* '(beta gamma delta))
    1 (list?)
    (section equal? <> '(beta gamma delta)))

  (test cons*:many-non-list-arguments
    (cons* 'epsilon 'zeta 'eta 'theta)
    1 (pair?)
    (section equal? <> '(epsilon zeta eta . theta)))

  (test cons*:last-argument-null
    (cons* 'iota 'kappa '())
    1 (list?)
    (section equal? <> '(iota kappa)))

  (test cons*:last-argument-list
    (cons* 'lambda 'mu '(nu omicron pi))
    1 (list?)
    (section equal? <> '(lambda mu nu omicron pi))))


(suite proper-list? ()

  (test proper-list?:empty-list
    (proper-list? '())
    1 (true?))

  (test proper-list?:non-pair
    (proper-list? 'asterism)
    1 (false?))

  (test proper-list?:non-empty-list
    (proper-list? '(brace bracket breve))
    1 (true?))

  (test proper-list?:dotted-list
    (proper-list? (cons* 'caret 'cedilla 'circumflex 'colon))
    1 (false?))

  (test proper-list?:one-element-circular-list
    (let ((pr (list 'comma)))
      (set-cdr! pr pr)
      (proper-list? pr))
    1 (false?))

  (test proper-list?:longer-circular-list
    (let* ((end-pair (list 'dagger))
           (rota (append (list 'dash 'diaeresis 'diesis 'dot) end-pair)))
      (set-cdr! end-pair rota)
      (proper-list? rota))
    1 (false?)))      


(suite circular-list? ()

  (test circular-list?:empty-list
    (circular-list? '())
    1 (false?))

  (test circular-list?:non-pair
    (circular-list? 'grave)
    1 (false?))

  (test circular-list?:non-empty-list
    (circular-list? '(hyphen leaders macron))
    1 (false?))

  (test circular-list?:dotted-list
    (circular-list? (cons* 'obelisk 'paragraph 'parallels))
    1 (false?))

  (test circular-list?:one-element-circular-list
    (let ((pr (list 'parenthesis)))
      (set-cdr! pr pr)
      (circular-list? pr))
    1 (true?))

  (test circular-list?:longer-circular-list
    (let* ((end-pair (list 'period))
           (rota (append (list 'prime 'semicolon 'star 'tilde) end-pair)))
      (set-cdr! end-pair rota)
      (circular-list? rota))
    1 (true?)))


(suite dotted-list? ()

  (test dotted-list?:empty-list
    (dotted-list? '())
    1 (false?))

  (test dotted-list?:non-pair
    (dotted-list? 'metalanguage)
    1 (true?))

  (test dotted-list?:non-empty-list
    (dotted-list? '(metaphor metonymy middle))
    1 (false?))

  (test dotted-list?:dotted-list
    (dotted-list? (cons* 'minimal 'minor 'missing 'modal))
    1 (true?))

  (test dotted-list?:one-element-circular-list
    (let ((pr (list 'modality)))
      (set-cdr! pr pr)
      (dotted-list? pr))
    1 (false?))

  (test dotted-list?:longer-circular-list
    (let* ((end-pair (list 'mode))
           (rota (append (list 'modification 'modifier 'money 'monotransitive)
                         end-pair)))
      (set-cdr! end-pair rota)
      (dotted-list? rota))
    1 (false?)))      


(suite not-pair? ()

  (test not-pair?:null
    (not-pair? '())
    1 (true?))

  (test not-pair?:atom
    (not-pair? 'umlaut)
    1 (true?))

  (test not-pair?:non-empty-list
    (not-pair? '(virgule achinese))
    1 (false?))

  (test not-pair?:non-list-pair
    (not-pair? '(afghan . ainu))
    1 (false?)))


(suite null-list? ()

  (test null-list?:empty-list
    (null-list? '())
    1 (true?))

  (test null-list?:non-empty-list
    (null-list? '(aka albanian algonquin amharic anglo-norman anglo-saxon))
    1 (false?)))


(suite list= ()

  (test list=:no-lists
    (list= eq?)
    1 (true?))

  (test list=:single-list
    (list= eq? '(annamese 'arabic))
    1 (true?))

  (test list=:multiple-empty-lists
    (list= eq? '() '() '() '() '() '())
    1 (true?))

  (test list=:multiple-lists-one-empty
    (list= eq? '(araucan armenian assamese) '(araucan armenian assamese) '()
               '(araucan armenian assamese) '(araucan armenian assamese))
    1 (false?))

  (test list=:multiple-equal-lists
    (list= eq? '(austral avestan aymara balinese)
               '(austral avestan aymara balinese)
               '(austral avestan aymara balinese)
               '(austral avestan aymara balinese)
               '(austral avestan aymara balinese)
               '(austral avestan aymara balinese))
    1 (true?))
    
  (test list=:different-lengths
    (list= eq? '(baluchi bashkir) '(baluchi bashkir basque))
    1 (false?))

  (test list=:different-orders
    (list= eq? '(batan battak bengali berber)
               '(batan bengali battak berber))
    1 (false?))

  (test list=:different-elements
    (list= eq? '(bihari bikol blackfoot brahui)
               '(bihari bikol breton brahui))
    1 (false?)))


(suite circular-list ()

  (test circular-list:one-element
    (circular-list 'Orion)
    1 (pair?)
    (lambda (result)
      (and (pair? result)
           (eq? (car result) 'Orion)
           (eq? (cdr result) result))))

  (test circular-list:many-elements
    (circular-list 'Pavo 'Pegasus 'Perseus 'Phoenix 'Pictor)
    1 (pair?)
    (lambda (result)
      (and (pair? result)
           (eq? (car result) 'Pavo)
           (pair? (cdr result))
           (eq? (cadr result) 'Pegasus)
           (pair? (cddr result))
           (eq? (caddr result) 'Perseus)
           (pair? (cdddr result))
           (eq? (cadddr result) 'Phoenix)
           (pair? (cddddr result))
           (eq? (car (cddddr result)) 'Pictor)
           (eq? (cdr (cddddr result)) result)))))


(suite length+ ()

  (test length+:empty-list
    (length+ '())
    1 (integer?)
    zero?)

  (test length+:non-pair
    (length+ 'pi)
    1 (integer?)
    zero?)

  (test length+:longer-list
    (length+ '(rho sigma tau upsilon))
    1 (integer?)
    (section = <> 4))

  (test length+:longer-improper-list
    (length+ '(phi chi psi . omega))
    1 (integer?)
    (section = <> 3))

  (test length+:circular-list
    (let* ((end-pair (cons 'accent 'dummy))
           (rota (cons* 'ampersand 'apostrophe 'asterisk end-pair)))
      (set-cdr! end-pair rota)
      (length+ rota))
    1 (false?)))


(suite iota ()

  (test iota:zero-argument
    (iota 0)
    1 (null?))

  (test iota:single-argument
    (iota 7)
    1 (list?)
    (section equal? <> '(0 1 2 3 4 5 6)))

  (test iota:non-zero-start
    (iota 3 -5)
    1 (list?)
    (section equal? <> '(-5 -4 -3)))

  (test iota:zero-step
    (iota 4 -7 0)
    1 (list?)
    (section equal? <> '(-7 -7 -7 -7)))

  (test iota:negative-step
    (iota 5 2 -4)
    1 (list?)
    (section equal? <> '(2 -2 -6 -10 -14)))

  (test iota:positive-step
    (iota 6 4 9)
    1 (list?)
    (section equal? <> '(4 13 22 31 40 49))))


(suite first ()

  (test first:of-one
    (first '(hafnium))
    1 (always)
    (section eq? <> 'hafnium))

  (test first:of-many
    (first '(hahnium helium holmium hydrogen indium))
    1 (always)
    (section eq? <> 'hahnium)))


(suite second ()

  (test second:of-two
    (second '(iodine iridium))
    1 (always)
    (section eq? <> 'iridium))
    
  (test second:of-many
    (second '(iron krypton lanthanum lawrencium lead lithium))
    1 (always)
    (section eq? <> 'krypton)))


(suite third ()

  (test third:of-three
    (third '(lutetium magnesium manganese))
    1 (always)
    (section eq? <> 'manganese))

  (test third:of-many
    (third '(mendelevium mercury molybdenum neodymium neon neptunium nickel))
    1 (always)
    (section eq? <> 'molybdenum)))


(suite fourth ()

  (test fourth:of-four
    (fourth '(niobium nitrogen nobelium osmium))
    1 (always)
    (section eq? <> 'osmium))

  (test fourth:of-many
    (fourth '(oxygen palladium phosphorus platinum plutonium polonium
              potassium praseodymium))
    1 (always)
    (section eq? <> 'platinum)))


(suite fifth ()

    (test fifth:of-five
      (fifth '(promethium protatctinium radium radon rhenium))
    1 (always)
    (section eq? <> 'rhenium))

  (test fifth:of-many
    (fifth '(rhodium rubidium ruthenium rutherfordium samarium scandium))
    1 (always)
    (section eq? <> 'samarium)))


(suite sixth ()

  (test sixth:of-six
    (sixth '(sodium strontium sulfur tantalum technetium tellurium))
    1 (always)
    (section eq? <> 'tellurium))

  (test sixth:of-many
    (sixth '(terbium thallium thorium thulium tin titanium tungsten uranium
             vanadium xenon))
    1 (always)
    (section eq? <> 'titanium)))


(suite seventh ()

  (test seventh:of-seven
    (seventh '(ytterbium yttrium zinc zirconium acacia abele ailanthus))
    1 (always)
    (section eq? <> 'ailanthus))

  (test seventh:of-many
    (seventh '(alder allspice almond apple apricot ash aspen avocado balsa
               balsam banyan))
    1 (always)
    (section eq? <> 'aspen)))


(suite eighth ()

  (test eighth:of-eight
    (eighth '(basswood bay bayberry beech birch boxwood breadfruit buckeye))
    1 (always)
    (section eq? <> 'buckeye))

  (test eighth:of-many
    (eighth '(butternut buttonwood cacao candleberry cashew cassia catalpa
              cedar cherry chestnut chinaberry chinquapin))
    1 (always)
    (section eq? <> 'cedar)))


(suite ninth ()

  (test ninth:of-nine
    (ninth '(cinnamon citron clove coconut cork cottonwood cypress date
             dogwood))
    1 (always)
    (section eq? <> 'dogwood))

  (test ninth:of-many
    (ninth '(ebony elder elm eucalyptus ficus fig fir frankincense ginkgo
             grapefruit guava gum hawthorn))
    1 (always)
    (section eq? <> 'ginkgo)))


(suite tenth ()

  (test tenth:of-ten
    (tenth '(hazel hemlock henna hickory holly hornbeam ironwood juniper
             kumquat laburnum))
    1 (always)
    (section eq? <> 'laburnum))

  (test tenth:of-many
    (tenth '(lancewood larch laurel lemon lime linden litchi locust logwood
             magnolia mahogany mango mangrove maple))
    1 (always)
    (section eq? <> 'magnolia)))

(suite car+cdr ()

  (test car+cdr:one-element-list
    (car+cdr '(muskogee))
    2 (always null?)
    (lambda (fore aft)
      (eq? fore 'muskogee)))

  (test car+cdr:dotted-pair
    (car+cdr '(naga . newari))
    2 (always always)
    (lambda (fore aft)
      (and (eq? fore 'naga)
           (eq? aft 'newari)))))

(suite take ()

  (test take:all-of-list
    (take '(medlar mimosa mulberry nutmeg oak) 5)
    1 (list?)
    (section equal? <> '(medlar mimosa mulberry nutmeg oak)))

  (test take:front-of-list
    (take '(olive orange osier palm papaw peach pear) 5)
    1 (list?)
    (section equal? <> '(olive orange osier palm papaw)))

  (test take:none-of-list
    (take '(poplar quince redwood) 0)
    1 (null?))

  (test take:empty-list
    (take '() 0)
    1 (null?))

  (test take:non-list
    (take 'emotive 0)
    1 (null?))

  (test take:all-but-ender-of-dotted-list
    (take '(en-dash en-rule enclitic . end-attachment) 3)
    1 (list?)
    (section equal? <> '(en-dash en-rule enclitic)))

  (test take:some-of-dotted-list
    (take '(endearment ending . endocentric) 1)
    1 (list?)
    (section equal? <> '(endearment)))

  (test take:circular-list
    (take (circular-list 'endophora 'endpoint 'english 'entailment) 5)
    1 (list?)
    (section equal? <> '(endophora endpoint english entailment endophora))))
          

(suite drop ()

  (test drop:all-of-list
    (drop '(rosewood sandalwood sassfras satinwood senna) 5)
    1 (null?))

  (test drop:front-of-list
    (drop '(sequoia serviceberry spruce sycamore tamarack tamarind tamarugo) 5)
    1 (list?)
    (section equal? <> '(tamarind tamarugo)))

  (test drop:none-of-list
    (drop '(whitebeam whitethorn wicopy) 0)
    1 (list?)
    (section equal? <> '(whitebeam whitethorn wicopy)))

  (test drop:empty-list
    (drop '() 0)
    1 (null?))

  (test drop:non-list
    (drop 'entreaty 0)
    1 (always)
    (section eq? <> 'entreaty))

  (test drop:all-but-ender-of-dotted-list
    (drop '(epistemic epithet epitomisation equal . equative) 4)
    1 (always)
    (section eq? <> 'equative))

  (test drop:some-of-dotted-list
    (drop '(errors established ethic etymology . evaluative) 2)
    1 (pair?)
    (section equal? <> '(ethic etymology . evaluative)))

  (test drop:circular-list
    (drop (circular-list 'event 'exception 'exclamation) 2)
    1 (pair?)
    (lambda (result)
      (and (eq? (car result) 'exclamation)
           (pair? (cdr result))
           (eq? (cadr result) 'event)
           (pair? (cddr result))
           (eq? (caddr result) 'exception)
           (eq? result (cdddr result))))))

(suite take-right ()

  (test take-right:all-of-list
    (take-right '(british bugi bulgarian buriat) 4)
    1 (list?)
    (section equal? <> '(british bugi bulgarian buriat)))

  (test take-right:rear-of-list
    (take-right '(pecan persimmon pine pistachio plane plum pomegranite) 5)
    1 (list?)
    (section equal? <> '(pine pistachio plane plum pomegranite)))

  (test take-right:none-of-list
    (take-right '(burmese caroline castilian) 0)
    1 (null?))

  (test take-right:non-list
    (take-right 'goal 0)
    1 (always)
    (section eq? <> 'goal))

  (test take-right:dotted-list
    (take-right '(govern grading grammar . grammatical) 1)
    1 (pair?)
    (section equal? <> '(grammar . grammatical))))


(suite drop-right ()

  (test drop-right:all-of-list
    (drop-right '(catalan cham chamorro cheremiss cherokee chibcha) 6)
    1 (null?))

  (test 'drop-right:from-rear-of-list
    (drop-right '(tangerine teak thuja torchwood upas walnut wandoo) 5)
    1 (list?)
    (section equal? <> '(tangerine teak)))

  (test 'drop-right:none-of-list
    (drop-right '(chin chinese) 0)
    1 (list?)
    (section equal? <> '(chin chinese))))

  (test drop-right:non-list
    (drop-right 'grammaticalization 0)
    1 (null?))

  (test drop-right:dotted-list
    (drop-right '(graphology greek head headed . headless) 2)
    1 (list?)
    (section equal? <> '(graphology greek)))


;;; List arguments to linear-update procedures
;;; are constructed with the LIST and CONS procedures
;;; rather than as quoted data,
;;; since in some implementations quoted data are not mutable.

(suite take! ()

  (test take!:all-of-list
    (take! (list 'willow 'woollybutt 'wychelm 'yellowwood 'yew) 5) 
    1 (list?)
    (section equal? <> '(willow woollybutt wychelm yellowwood yew)))

  (test take!:front-of-list
    (take! (list 'ylang-ylang 'zebrawood 'affenpinscher 'afghan 'airedale
                 'alsatian 'barbet)
           5)
    1 (list?)
    (section equal? <> '(ylang-ylang zebrawood affenpinscher afghan airedale)))

  (test take!:none-of-list
    (take! (list 'briard 'bulldog 'chihuahua) 0)
    1 (null?))

  (test take!:empty-list
    (take! '() 0)
    1 (null?))

  (test take!:non-list
    (take! 'emotive 0)
    1 (null?))

  (test take!:all-but-ender-of-dotted-list
    (take! (cons 'exclamative (cons 'exclusion 'exclusive)) 2)
    1 (list?)
    (section equal? <> '(exclamative exclusion)))

  (test take!:some-of-dotted-list
    (take! (cons 'exhaustive (cons 'existential (cons 'exocentric ' expandable)))
           2)
    1 (list?)
    (section equal? <> '(exhaustive existential)))

  (test take!:circular-list
    (take! (circular-list 'experiencer 'expletive 'expository) 2)
    1 (list?)
    (section equal? <> '(experiencer expletive))))


(suite drop-right! ()

  (test drop-right!:all-of-list
    (let ((given (list 'flemish 'formosan 'french)))
      (drop-right! given 3))
    1 (null?))

  (test drop-right!:rear-of-list
    (let ((given (list 'frisian 'gadaba 'gaelic 'galcha 'garo)))
      (drop-right! given 2))
    1 (list?)
    (section equal? <> '(frisian gadaba gaelic)))

  (test drop-right!:none-of-list
    (let ((given (list 'german 'gilbertese 'gold 'gondi)))
      (drop-right! given 0))
    1 (list?)
    (section equal? <> '(german gilbertese gold gondi)))

  (test drop-right!:non-list
    (drop-right! 'express 0)
    1 (null?))

  (test drop-right!:all-but-ender-of-dotted-list
    (drop-right! (cons 'extendable
                       (cons 'extent
                             (cons 'external
                                   (cons 'extranuclear 'extraposed))))
                 4)
    1 (null?))

  (test drop-right!:some-of-dotted-list
    (drop-right! (cons 'extraposition
                       (cons 'factitive
                             (cons 'factive 'failure)))
                 2)
    1 (list?)
    (section equal? <> '(extraposition))))


(suite split-at ()

  (test split-at:beginning
    (split-at '(gothic greek gujarati) 0)
    2 (list? list?)
    (lambda (fore aft)
      (and (null? fore)
           (equal? aft '(gothic greek gujarati)))))

  (test split-at:middle
    (split-at '(hawaiian hebrew hindustani ho ibanag icelandic) 4)
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(hawaiian hebrew hindustani ho))
           (equal? aft '(ibanag icelandic)))))

  (test split-at:end
    (split-at '(igorot ilokano irish) 3)
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(igorot ilokano irish))
           (null? aft))))

  (test split-at:empty-list
    (split-at '() 0)
    2 (null? null?))

  (test split-at:non-list
    (split-at 'festival 0)
    2 (list? always)
    (lambda (fore aft)
      (and (null? fore)
           (eq? aft 'festival))))

  (test split-at:all-but-ender-of-dotted-list
    (split-at '(final finite fish . focus) 3)
    2 (list? always)
    (lambda (fore aft)
      (and (equal? fore '(final finite fish))
           (eq? aft 'focus))))

  (test split-at:some-of-dotted-list
    (split-at '(food foreclipping foregrounded . foreign) 2)
    2 (list? pair?)
    (lambda (fore aft)
      (and (equal? fore '(food foreclipping))
           (equal? aft '(foregrounded . foreign)))))

  (test split-at:circular-list
    (split-at (circular-list 'form 'formal 'formulaic) 5)
    2 (list? pair?)
    (lambda (fore aft)
      (and (equal? fore '(form formal formulaic form formal))
           (eq? (car aft) 'formulaic)
           (pair? (cdr aft))
           (eq? (cadr aft) 'form)
           (pair? (cddr aft))
           (eq? (caddr aft) 'formal)
           (eq? aft (cdddr aft))))))


(suite split-at! ()

  (test split-at!:beginning
    (let ((given (list 'italian 'jagatai 'jakun 'japanese 'javanese)))
      (split-at! given 0))
    2 (list? list?)
    (lambda (fore aft)
      (and (null? fore)
           (equal? aft '(italian jagatai jakun japanese javanese)))))

  (test split-at!:middle
    (let ((given (list 'juang 'kabyle 'kachin)))
      (split-at! given 2))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(juang kabyle))
           (equal? aft '(kachin)))))

  (test split-at!:end
    (let ((given (list 'kafiri 'kalmuck)))
      (split-at! given 2))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(kafiri kalmuck))
           (null? aft))))

  (test split-at!:empty-list
    (split-at! '() 0)
    2 (null? null?))

  (test split-at!:non-list
    (split-at! 'fossilised 0)
    2 (list? always)
    (lambda (fore aft)
      (and (null? fore)
           (eq? aft 'fossilised))))

  (test split-at!:all-but-ender-of-dotted-list
    (split-at! (cons 'fraction (cons 'fragment 'free)) 2)
    2 (list? always)
    (lambda (fore aft)
      (and (equal? fore '(fraction fragment))
           (eq? aft 'free))))

  (test split-at!:some-of-dotted-list
    (split-at! (cons 'french
                     (cons 'frequency
                           (cons 'front
                                 (cons 'fronting 'frozen))))
               2)
    2 (list? pair?)
    (lambda (fore aft)
      (and (equal? fore '(french frequency))
           (equal? aft '(front fronting . frozen)))))

  (test split-at!:circular-list
    (split-at! (circular-list 'full 'function 'fused 'futurate) 3)
    2 (list? pair?)
    (lambda (fore aft)
      (and (equal? fore '(full function fused))
           (equal? aft '(futurate full function fused))))))


(suite last ()

  (test last:of-singleton
    (last '(maltese))
    1 (symbol?)
    (section eq? <> 'maltese))

  (test last:of-longer-list
    (last '(mastiff newfoundland nizinny otterhound papillon))
    1 (symbol?)
    (section eq? <> 'papillon))

  (test last:dotted-pair
    (last '(heavy hebrew hendiadys . heterogeneity))
    1 (always)
    (section eq? <> 'hendiadys)))


(suite last-pair ((pair '(pekingese))
                  (dotted-pair '(manx . siamese)))

  (test last-pair:of-singleton
    (last-pair pair)
    1 (pair?)
    (section eq? <> pair))

  (test last-pair:of-longer-list
    (last-pair (cons* 'pomeranian 'poodle 'pug 'puli pair))
    1 (pair?)
    (section eq? <> pair))

  (test last-pair:of-improper-list
    (last-pair (cons* 'abyssinian 'calico dotted-pair))
    1 (pair?)
    (section eq? <> dotted-pair)))


(suite zip ()

  (test zip:all-lists-empty
    (zip '() '() '() '() '())
    1 (null?))

  (test zip:one-list
    (zip '(Pisces Puppis Reticulum))
    1 (list?)
    (section equal? <> '((Pisces) (Puppis) (Reticulum))))

  (test zip:two-lists
    (zip '(Sagitta Sagittarius Scorpio Scutum Serpens)
         '(Sextans Taurus Telescopium Triangulum Tucana))
    1 (list?)
    (section equal? <> '((Sagitta Sextans)
                         (Sagittarius Taurus)
                         (Scorpio Telescopium)
                         (Scutum Triangulum)
                         (Serpens Tucana))))

  (test zip:short-lists
    (zip '(Vela) '(Virgo) '(Volens) '(Vulpecula))
    1 (list?)
    (section equal? <> '((Vela Virgo Volens Vulpecula))))

  (test zip:several-lists
    (zip '(actinium aluminum americium antimony argon)
         '(arsenic astatine barium berkeleium beryllium)
         '(bismuth boron bromine cadmium calcium)
         '(californium carbon cerium cesium chlorine)
         '(chromium cobalt copper curium dysprosium)
         '(einsteinium erbium europium fermium fluorine)
         '(francium gadolinium gallium germanium gold))
    1 (list?)
    (section equal? <> '((actinium arsenic bismuth californium chromium
                          einsteinium francium)
                         (aluminum astatine boron carbon cobalt
                          erbium gadolinium)
                         (americium barium bromine cerium copper
                          europium gallium)
                         (antimony berkeleium cadmium cesium curium
                          fermium germanium)
                         (argon beryllium calcium chlorine dysprosium
                          fluorine gold))))

  (test zip:some-circular-lists
    (zip (circular-list 'historic 'holistic)
         (circular-list 'hollow 'homonymy 'host)
         (circular-list 'human 'hurting 'hybrid 'hypallage 'hypercorrection)
         '(hyphen hypocoristic hyponymy identifiability idiom illness
           illocutionary))
    1 (list?)
    (section equal? <> '((historic hollow human hyphen)
                         (holistic homonymy hurting hypocoristic)
                         (historic host hybrid hyponymy)
                         (holistic hollow hypallage identifiability)
                         (historic homonymy hypercorrection idiom)
                         (holistic host human illness)
                         (historic hollow hurting illocutionary)))))          


(suite unzip1 ()

  (test unzip1:empty-list-of-lists
    (unzip1 '())
    1 (null?))

  (test unzip1:singleton-list-of-lists
    (unzip1 '((niasese)))
    1 (list?)
    (section equal? <> '(niasese)))

  (test unzip1:longer-list-of-lists
    (unzip1 '((nicobarese) (niue) (nogai) (norwegian)))
    1 (list?)
    (section equal? <> '(nicobarese niue nogai norwegian))))


(suite unzip2 ()

  (test unzip2:empty-list-of-lists
    (unzip2 '())
    2 (null? null?))

  (test unzip2:singleton-list-of-lists
    (unzip2 '((retriever rottweiler)))
    2 (list? list?)
    (lambda (firsts seconds)
      (and (equal? firsts '(retriever))
           (equal? seconds '(rottweiler)))))

  (test unzip2:longer-list-of-lists
    (unzip2 '((saluki samoyed)
              (shipperke schnauzer)
              (setter shepherd)
              (skye spaniel)
              (spitz staghound)))
    2 (list? list?)
    (lambda (firsts seconds)
      (and (equal? firsts '(saluki shipperke setter skye spitz))
           (equal? seconds '(samoyed schnauzer shepherd spaniel staghound)))))

  (test unzip2:lists-with-extra-elements
    (unzip2 '((terrier turnspit vizsla wiemaraner)
              (whippet wolfhound)
              (bells bones bongo carillon celesta)
              (chimes clappers conga)))
    2 (list? list?)
    (lambda (firsts seconds)
      (and (equal? firsts '(terrier whippet bells chimes))
           (equal? seconds '(turnspit wolfhound bones clappers))))))


(suite unzip3 ()

  (test unzip3:empty-list-of-lists
    (unzip3 '())
    3 (null? null? null?))

  (test unzip3:singleton-list-of-lists
    (unzip3 '((cymbals gamelan glockenspiel)))
    3 (list? list? list?)
    (lambda (firsts seconds thirds)
      (and (equal? firsts '(cymbals))
           (equal? seconds '(gamelan))
           (equal? thirds '(glockenspiel)))))

  (test unzip3:longer-list-of-lists
    (unzip3 '((gong handbells kettledrum)
              (lyra maraca marimba)
              (mbira membranophone metallophone)
              (nagara naker rattle)
              (sizzler snappers tabor)))
    3 (list? list? list?)
    (lambda (firsts seconds thirds)
      (and (equal? firsts '(gong lyra mbira nagara sizzler))
           (equal? seconds '(handbells maraca membranophone naker snappers))
         (equal? thirds '(kettledrum marimba metallophone rattle tabor)))))

  (test unzip3:lists-with-extra-elements
    (unzip3 '((tambourine timbrel timpani tintinnabula tonitruone)
              (triangle vibraphone xylophone)
              (baccarat banker bezique bingo bridge canasta)
              (casino craps cribbage euchre)))
    3 (list? list? list?)
    (lambda (firsts seconds thirds)
      (and (equal? firsts '(tambourine triangle baccarat casino))
           (equal? seconds '(timbrel vibraphone banker craps))
           (equal? thirds '(timpani xylophone bezique cribbage))))))


(suite unzip4 ()

  (test unzip4:empty-list-of-lists
    (unzip4 '())
    4 (null? null? null? null?))

  (test unzip4:singleton-list-of-lists
    (unzip4 '((fantan faro gin hazard)))
    4 (list? list? list? list?)
    (lambda (firsts seconds thirds fourths)
      (and (equal? firsts '(fantan))
           (equal? seconds '(faro))
           (equal? thirds '(gin))
           (equal? fourths '(hazard)))))

  (test unzip4:longer-list-of-lists
    (unzip4 '((hearts keno loo lottery)
              (lotto lowball monte numbers)
              (ombre picquet pinball pinochle)
              (poker policy quinze romesteq)
              (roulette rum rummy skat)))
    4 (list? list? list? list?)
    (lambda (firsts seconds thirds fourths)
      (and (equal? firsts '(hearts lotto ombre poker roulette))
           (equal? seconds '(keno lowball picquet policy rum))
           (equal? thirds '(loo monte pinball quinze rummy))
           (equal? fourths '(lottery numbers pinochle romesteq skat)))))

  (test unzip4:lists-with-extra-elements
    (unzip4 '((adamant agate alexandrite amethyst aquamarine beryl)
              (bloodstone brilliant carbuncle carnelian)
              (chalcedony chrysoberyl chrysolite chrysoprase citrine coral
               demantoid)
              (diamond emerald garnet girasol heliotrope)))
    4 (list? list? list? list?)
    (lambda (firsts seconds thirds fourths)
      (and (equal? firsts '(adamant bloodstone chalcedony diamond)) 
           (equal? seconds '(agate brilliant chrysoberyl emerald))
           (equal? thirds '(alexandrite carbuncle chrysolite garnet))
           (equal? fourths '(amethyst carnelian chrysoprase girasol))))))


(suite unzip5 ()

  (test unzip5:empty-list-of-lists
    (unzip5 '())
    5 (null? null? null? null? null?))

  (test unzip5:singleton-list-of-lists
    (unzip5 '((hyacinth jacinth jade jargoon jasper)))
    5 (list? list? list? list? list?)
    (lambda (firsts seconds thirds fourths fifths)
      (and (equal? firsts '(hyacinth))
           (equal? seconds '(jacinth))
           (equal? thirds '(jade))
           (equal? fourths '(jargoon))
           (equal? fifths '(jasper)))))

  (test unzip5:longer-list-of-lists
    (unzip5 '((kunzite moonstone morganite onyx opal)
              (peridot plasma ruby sapphire sard)
              (sardonyx spinel star sunstone topaz)
              (tourmaline turquoise zircon Argus basilisk)
              (Bigfoot Briareus bucentur Cacus Caliban)))
    5 (list? list? list? list? list?)
    (lambda (firsts seconds thirds fourths fifths)
      (and (equal? firsts '(kunzite peridot sardonyx tourmaline Bigfoot)) 
           (equal? seconds '(moonstone plasma spinel turquoise Briareus))
           (equal? thirds '(morganite ruby star zircon bucentur))
           (equal? fourths '(onyx sapphire sunstone Argus Cacus))
           (equal? fifths '(opal sard topaz basilisk Caliban)))))

  (test unzip5:lists-with-extra-elements
    (unzip5 '((centaur Cerberus Ceto Charybdis chimera cockatrice Cyclops)
              (dipsas dragon drake Echidna Geryon)
              (Gigantes Gorgon Grendel griffin Harpy hippocampus hippocentaur
               hippocerf)
              (hirocervus Hydra Kraken Ladon manticore Medusa)))
    5 (list? list? list? list? list?)
    (lambda (firsts seconds thirds fourths fifths)
      (and (equal? firsts '(centaur dipsas Gigantes hirocervus)) 
           (equal? seconds '(Cerberus dragon Gorgon Hydra))
           (equal? thirds '(Ceto drake Grendel Kraken))
           (equal? fourths '(Charybdis Echidna griffin Ladon))
           (equal? fifths '(chimera Geryon Harpy manticore))))))


(suite count ()

  (test count:one-empty-list
    (count true? '())
    1 (integer?)
    zero?)

  (test count:many-empty-lists
    (count always '() '() '() '() '())
    1 (integer?)
    zero?)

  (test count:one-list-no-satisfiers
    (count never '(kamasin kanarese kara-kalpak))
    1 (integer?)
    zero?)

  (test count:one-list-all-satisfiers
    (count always '(karen kashmmiri kasubian kavi))
    1 (integer?)
    (section = <> 4))

  (test count:one-list-some-satisfiers
    (count even? '(21 42 63 84 105 126))
    1 (integer?)
    (section = <> 3))

  (test count:many-lists-no-satisfiers
    (count never '(kharia khasi khmer)
                        '(khond khowar kiranti)
                        '(kirghiz kiriwina kodagu))
    1 (integer?)
    zero?)

  (test count:many-lists-all-satisfiers
    (count always '(kohistani koibal)
                       '(kongoese korwa)
                       '(kuki kumyk)
                       '(kurdish kurukh))
    1 (integer?)
    (section = <> 2))

  (test count:many-lists-some-satisfiers
    (count = '(2 3 5 7 11 13 17) '(1 3 5 7 9 11 13))
    1 (integer?)
    (section = <> 3))

  (test count:some-circular-lists
    (count eq? (circular-list 'immediate 'imperative)
               '(imperfective imperative impersonal implicature immediate
                 imperative))
    1 (integer?)
    (section = <> 3)))

(suite append! ()

  (test append!:no-arguments
    (append!)
    1 (null?))

  (test append!:one-argument
    (append! (list 'mermaid 'merman 'Minotaur))
    1 (list?)
    (section equal? <> '(mermaid merman Minotaur)))

  (test append!:several-arguments
    (append! (list 'nixie 'ogre 'ogress 'opinicus)
             (list 'Orthos)
             (list 'Pegasus 'Python)
             (list 'roc 'Sagittary 'salamander 'Sasquatch 'satyr)
             (list 'Scylla 'simurgh 'siren))
    1 (list?)
    (section equal? <> '(nixie ogre ogress opinicus Orthos Pegasus Python roc
                         Sagittary salamander Sasquatch satyr Scylla simurgh
                         siren)))

  (test append!:some-null-arguments
    (append! (list) (list) (list 'Sphinx 'Talos 'troll) (list) (list 'Typhoeus)
             (list) (list) (list))
    1 (list?)
    (section equal? <> '(Sphinx Talos troll Typhoeus)))

  (test append!:all-null-arguments
    (append! (list) (list) (list) (list) (list))
    1 (null?))

  (test append!:non-list-at-end
    (append! (list 'future 'game) (list 'gap 'gender 'general) 'generic)
    1 (pair?)
    (section equal? <> '(future game gap gender general . generic)))

  (test append!:dotted-pair-at-end
    (append! (list 'genitive 'german 'gerund) (cons 'gerundial 'get))
    1 (pair?)
    (section equal? <> '(genitive german gerund gerundial . get))))

(suite append-reverse ()

  (test append-reverse:first-argument-null
    (append-reverse '() '(Typhon unicorn vampire werewolf))
    1 (list?)
    (section equal? <> '(Typhon unicorn vampire werewolf)))

  (test append-reverse:second-argument-null
    (append-reverse '(windigo wivern xiphopagus yeti zombie) '())
    1 (list)
    (section equal? <> '(zombie yeti xiphopagus wivern windigo)))

  (test append-reverse:both-arguments-null
    (append-reverse '() '())
    1 (null?))

  (test append-reverse:neither-argument-null
    (append-reverse '(Afghanistan Albania Algeria Andorra)
                    '(Angola Argentina Armenia))
    1 (list?)
    (section equal? <> '(Andorra Algeria Albania Afghanistan Angola Argentina
                         Armenia))))


(suite append-reverse! ()

  (test append-reverse!:first-argument-null
    (append-reverse! (list)
                     (list 'Australia 'Austria 'Azerbaijan))
    1 (list?)
    (section equal? <> '(Australia Austria Azerbaijan)))

  (test append-reverse!:second-argument-null
    (append-reverse! (list 'Bahrain 'Bangladesh 'Barbados 'Belarus 'Belgium)
                     (list))
    1 (list?)
    (section equal? <> '(Belgium Belarus Barbados Bangladesh Bahrain)))

  (test append-reverse!:both-arguments-null
    (append-reverse! (list) (list))
    1 (null?))

  (test append-reverse!:neither-argument-null
    (append-reverse! (list 'Belize 'Benin 'Bhutan 'Bolivia)
                     (list 'Bosnia 'Botswana 'Brazil))
    1 (list?)
    (section equal? <> '(Bolivia Bhutan Benin Belize Bosnia Botswana Brazil))))


(suite concatenate ()

  (test concatenate:no-lists
    (concatenate '())
    1 (null?))

  (test concatenate:one-list
    (concatenate '((lahnda lampong lamut lapp)))
    1 (list?)
    (section equal? <> '(lahnda lampong lamut lapp)))

  (test concatenate:several-lists
    (concatenate '((latin lettis limbu)
                   (lithuanian livonian)
                   (madurese magyar makassar malagasy)))
    1 (list?)
    (section equal? <> '(latin lettis limbu lithuanian livonian madurese magyar
                         makassar malagasy)))

  (test concatenate:some-null-lists
    (concatenate '(() () (malay) () (malayalam maltese) () () (malto)))
    1 (list?)
    (section equal? <> '(malay malayalam maltese malto)))

  (test concatenate:all-null-lists
    (concatenate '(() () () () () () () () () () () ()))
    1 (null?)))


(suite concatenate! ()

  (test concatenate!:no-lists
    (concatenate! (list))
    1 (null?))

  (test concatenate!:one-list
    (concatenate! (list (list 'manchu 'mandarin 'mangar 'manobo 'manx)))
    1 (list?)
    (section equal? <> '(manchu mandarin mangar manobo manx)))

  (test concatenate!:several-lists
    (concatenate! (list (list 'maori 'marathi 'marquesan)
                        (list 'marshall)
                        (list 'maya 'meithei)
                        (list 'mishmi 'misima 'mon 'mongolian 'montes)))
    1 (list?)
    (section equal? <> '(maori marathi marquesan marshall maya meithei mishmi
                         misima mon mongolian montes)))

  (test concatenate!:some-null-lists
    (concatenate! (list (list) (list 'mordvinian 'moro) (list) (list) (list)
                        (list) (list 'mru) (list) (list 'muong 'murmi) (list)))
    1 (list?)
    (section equal? <> '(mordvinian moro mru muong murmi)))

  (test concatenate!:all-null-lists
    (concatenate! (list (list) (list) (list)))
    1 (null?)))


(suite unfold ()

  (test unfold:predicate-always-satisfied
    (unfold always (section * 2 <>) (section * 3 <>) 1)
    1 (null?))

  (test unfold:normal-case
    (unfold (section = <> 729)
            (section * 2 <>)
            (section * 3 <>)
            1)
    1 (list?)
    (section equal? <> '(2 6 18 54 162 486)))

  (test unfold:with-tail-gen
    (unfold (section = <> 243)
            (section * 2 <>)
            (section * 3 <>)
            1
            (lambda (final-seed)
              (list (* 5 final-seed))))
    1 (list?)
    (section equal? <> '(2 6 18 54 162 1215))))


(suite fold ()

  (test fold:one-null-list
    (fold (lambda (alpha beta) (* alpha (+ beta 1))) 13 '())
    1 (number?)
    (section = <> 13))

  (test fold:one-singleton-list
    (fold (lambda (alpha beta) (* alpha (+ beta 1))) 13 '(15))
    1 (number?)
    (section = <> 210))

  (test fold:one-longer-list
    (fold (lambda (alpha beta) (* alpha (+ beta 1))) 13 '(15 17 19 21 23))
    1 (number?)
    (section = <> 32927582))

  (test fold:several-null-lists
    (fold vector 'Chad '() '() '() '() '())
    1 (always)
    (section eq? <> 'Chad))

  (test fold:several-singleton-lists
    (fold vector 'Chile '(China) '(Colombia) '(Comoros) '(Congo) '(Croatia))
    1 (vector?)
    (section equal? <> '#(China Colombia Comoros Congo Croatia Chile)))

  (test fold:several-longer-lists
    (fold (lambda (alpha beta gamma delta epsilon zeta)
            (cons (vector alpha beta gamma delta epsilon) zeta))
          '()
          '(Cuba Cyprus Denmark Djibouti Dominica Ecuador Egypt)
          '(Eritrea Estonia Ethiopia Fiji Finland France Gabon)
          '(Gambia Georgia Germany Ghana Greece Grenada Guatemala)
          '(Guinea Guyana Haiti Honduras Hungary Iceland India)
          '(Indonesia Iran Iraq Ireland Israel Italy Jamaica))
    1 (list?)
    (section equal? <> '(#(Egypt Gabon Guatemala India Jamaica)
                         #(Ecuador France Grenada Iceland Italy)
                         #(Dominica Finland Greece Hungary Israel)
                         #(Djibouti Fiji Ghana Honduras Ireland)
                         #(Denmark Ethiopia Germany Haiti Iraq)
                         #(Cyprus Estonia Georgia Guyana Iran)
                         #(Cuba Eritrea Gambia Guinea Indonesia))))

  (test fold:lists-of-different-lengths
    (fold (lambda (alpha beta gamma delta)
            (cons (vector alpha beta gamma) delta))
          '()
          '(Japan Jordan Kazakhstan Kenya)
          '(Kiribati Kuwait)
          '(Kyrgyzstan Laos Latvia))
    1 (list?)
    (section equal? <> '(#(Jordan Kuwait Laos) #(Japan Kiribati Kyrgyzstan))))
 
  (test fold:some-circular-lists
    (fold (lambda (alpha beta gamma delta)
            (cons (vector alpha beta gamma) delta))
          '()
          (circular-list 'inclusion 'incorporation)
          '(indefinite independent indeterminate indexical indexing)
          (circular-list 'indicative 'indicator 'indirect))
    1 (list?)
    (section equal? <> '(#(inclusion indexing indicator)
                         #(incorporation indexical indicative)
                         #(inclusion indeterminate indirect)
                         #(incorporation independent indicator)
                         #(inclusion indefinite indicative)))))
                         

(suite pair-fold ()

  (test pair-fold:one-null-list
    (pair-fold revappend '(Spain Sudan) '())
    1 (list?)
    (section equal? <> '(Spain Sudan)))

  (test pair-fold:one-singleton-list
    (pair-fold revappend '(Suriname Swaziland) '(Sweden))
    1 (list?)
    (section equal? <> '(Sweden Suriname Swaziland)))

  (test pair-fold:one-longer-list
    (pair-fold revappend '(Switzerland Syria)
                         '(Taiwan Tajikistan Tanzania Thailand Togo))
    1 (list?)
    (section equal? <> '(Togo Togo Thailand Togo Thailand Tanzania Togo
                         Thailand Tanzania Tajikistan Togo Thailand Tanzania
                         Tajikistan Taiwan Switzerland Syria)))

  (test pair-fold:several-null-lists
    (pair-fold revappall '(Tonga Tunisia) '() '() '() '() '())
    1 (list?)
    (section equal? <> '(Tonga Tunisia)))

  (test pair-fold:several-singleton-lists
    (pair-fold revappall '(Turkey Turkmenistan)
                         '(Tuvalu)
                         '(Uganda)
                         '(Ukraine)
                         '(Uruguay)
                         '(Uzbekistan))
    1 (list?)
    (section equal? <> '(Tuvalu Uganda Ukraine Uruguay Uzbekistan Turkey 
                         Turkmenistan)))

  (test pair-fold:several-longer-lists
    (pair-fold revappall '(Vanuatu Venezuela)
                         '(Vietnam Yemen Yugoslavia Zaire Zambia Zimbabwe Agnon) 
                         '(Aleixandre Andric Asturias Beckett Bellow Benavente
                           Bergson)
                         '(Bjornson Brodsky Buck Bunin Camus Canetti Carducci)
                         '(Cela Churchill Deledda Echegary Eliot Elytis Eucken)
                         '(Faulkner Galsworthy Gide Gjellerup Golding Gordimer
                           Hamsun))
    1 (list?)
    (section equal? <> '(Agnon Bergson Carducci Eucken Hamsun Agnon Zimbabwe
                         Bergson Benavente Carducci Canetti Eucken Elytis Hamsun
                         Gordimer Agnon Zimbabwe Zambia Bergson Benavente Bellow
                         Carducci Canetti Camus Eucken Elytis Eliot Hamsun
                         Gordimer Golding Agnon Zimbabwe Zambia Zaire Bergson
                         Benavente Bellow Beckett Carducci Canetti Camus Bunin
                         Eucken Elytis Eliot Echegary Hamsun Gordimer Golding
                         Gjellerup Agnon Zimbabwe Zambia Zaire Yugoslavia
                         Bergson Benavente Bellow Beckett Asturias Carducci
                         Canetti Camus Bunin Buck Eucken Elytis Eliot Echegary
                         Deledda Hamsun Gordimer Golding Gjellerup Gide Agnon
                         Zimbabwe Zambia Zaire Yugoslavia Yemen Bergson
                         Benavente Bellow Beckett Asturias Andric Carducci
                         Canetti Camus Bunin Buck Brodsky Eucken Elytis Eliot
                         Echegary Deledda Churchill Hamsun Gordimer Golding
                         Gjellerup Gide Galsworthy Agnon Zimbabwe Zambia Zaire
                         Yugoslavia Yemen Vietnam Bergson Benavente Bellow
                         Beckett Asturias Andric Aleixandre Carducci Canetti
                         Camus Bunin Buck Brodsky Bjornson Eucken Elytis Eliot
                         Echegary Deledda Churchill Cela Hamsun Gordimer Golding
                         Gjellerup Gide Galsworthy Faulkner Vanuatu Venezuela)))

  (test pair-fold:lists-of-different-lengths
    (pair-fold revappall '(Hauptmann Hemingway Hesse)
                         '(Heyse Jensen Jimenez Johnson)
                         '(Karlfeldt Kawabata)
                         '(Kipling Lagerkvist Lagerlof Laxness Lewis))
    1 (list?)
    (section equal? <> '(Johnson Jimenez Jensen Kawabata Lewis Laxness Lagerlof
                         Lagerkvist Johnson Jimenez Jensen Heyse Kawabata
                         Karlfeldt Lewis Laxness Lagerlof Lagerkvist Kipling
                         Hauptmann Hemingway Hesse)))

  (test pair-fold:some-circular-lists
    (pair-fold (lambda (alpha beta gamma delta)
                 (cons (list (car alpha) (car beta) (car gamma)) delta))
               '()
               (circular-list 'logical 'long)
               (circular-list 'lower 'main 'mandative)
               '(mandatory manifestation manner manufacture))
    1 (list?)
    (section equal? <> '((long lower manufacture)
                         (logical mandative manner)
                         (long main manifestation)
                         (logical lower mandatory)))))


(suite reduce ()

  (test reduce:null-list
    (reduce (lambda (alpha beta) (* alpha (+ beta 1))) 0 '())
    1 (number?)
    zero?)

  (test reduce:singleton-list
    (reduce (lambda (alpha beta) (* alpha (+ beta 1))) 0 '(25))
    1 (number?)
    (section = <> 25))

  (test reduce:doubleton-list
    (reduce (lambda (alpha beta) (* alpha (+ beta 1))) 0 '(27 29))
    1 (number?)
    (section = <> 812))

  (test reduce:longer-list
    (reduce (lambda (alpha beta) (* alpha (+ beta 1)))
            0
            '(31 33 35 37 39 41 43))
    1 (number?)
    (section = <> 94118227527)))


(suite unfold-right ()

  (test unfold-right:predicate-always-satisfied
    (unfold-right always
                  (section * 2 <>)
                  (section * 3 <>)
                  1)
    1 (null?))

  (test unfold-right:normal-case
    (unfold-right (section = <> 729)
                  (section * 2 <>)
                  (section * 3 <>)
                  1)
    1 (list?)
    (section equal? <> '(486 162 54 18 6 2)))

  (test unfold-right-with-tail
    (unfold-right (section = <> 243)
                  (section * 2 <>)
                  (section * 3 <>)
                  1
                  '(777))
    1 (list?)
    (section equal? <> '(162 54 18 6 2 777))))


(suite fold-right ()

  (test fold-right:one-null-list
    (fold-right (lambda (alpha beta) (* alpha (+ beta 1))) 13 '())
    1 (number?)
    (section = <> 13))

  (test fold-right:one-singleton-list
    (fold-right (lambda (alpha beta) (* alpha (+ beta 1))) 13 '(15))
    1 (number?)
    (section = <> 210))

  (test fold-right:one-longer-list
    (fold-right (lambda (alpha beta) (* alpha (+ beta 1)))
                13
                '(15 17 19 21 23))
    1 (number?)
    (section = <> 32868750))

  (test fold-right:several-null-lists
    (fold-right vector 'Lebanon '() '() '() '() '())
    1 (always)
    (section eq? <> 'Lebanon))

  (test fold-right:several-singleton-lists
    (fold-right vector 'Lesotho '(Liberia) '(Libya) '(Liechtenstein)
                       '(Lithuania) '(Luxembourg))
    1 (vector?)
    (section equal? <> '#(Liberia Libya Liechtenstein Lithuania Luxembourg
                          Lesotho)))

  (test fold-right:several-longer-lists
    (fold-right (lambda (alpha beta gamma delta epsilon zeta)
                  (cons (vector alpha beta gamma delta epsilon) zeta))
                '()
                '(Macedonia Madagascar Malawi Malaysia Maldives Mali Malta)
                '(Mauritania Mauritius Mexico Micronesia Moldova Monaco Mongolia)
                '(Morocco Mozambique Myanmar Namibia Nauru Nepal Netherlands)
                '(Nicaragua Niger Nigeria Norway Oman Pakistan Palau)
                '(Panama Paraguay Peru Philippines Poland Portugal Qatar))
    1 (list?)
    (section equal? <> '(#(Macedonia Mauritania Morocco Nicaragua Panama)
                         #(Madagascar Mauritius Mozambique Niger Paraguay)
                         #(Malawi Mexico Myanmar Nigeria Peru)
                         #(Malaysia Micronesia Namibia Norway Philippines)
                         #(Maldives Moldova Nauru Oman Poland)
                         #(Mali Monaco Nepal Pakistan Portugal)
                         #(Malta Mongolia Netherlands Palau Qatar))))

  (test fold-right:lists-of-different-lengths
    (fold-right (lambda (alpha beta gamma delta)
                  (cons (vector alpha beta gamma) delta))
                '()
                '(Romania Russia Rwanda Senegal)
                '(Seychelles Singapore)
                '(Slovakia Slovenia Somalia))
    1 (list?)
    (section equal? <> '(#(Romania Seychelles Slovakia)
                         #(Russia Singapore Slovenia))))

  (test fold-right:some-circular-lists
    (fold-right (lambda (alpha beta gamma delta)
                  (cons (vector alpha beta gamma) delta))
                '()
                (circular-list 'inference 'inferiority 'infinitival)
                '(infinitive inflection inflectional influence)
                (circular-list 'informal 'information))
    1 (list?)
    (section equal? <> '(#(inference infinitive informal)
                         #(inferiority inflection information)
                         #(infinitival inflectional informal)
                         #(inference influence information)))))


(suite pair-fold-right ()

  (test pair-fold-right:one-null-list
    (pair-fold-right revappend '(Maeterlinck Mahfouz) '())
    1 (list?)
    (section equal? <> '(Maeterlinck Mahfouz)))

  (test pair-fold-right:one-singleton-list
    (pair-fold-right revappend '(Mann Martinson) '(Mauriac))
    1 (list?)
    (section equal? <> '(Mauriac Mann Martinson))) 

  (test pair-fold-right:one-longer-list
    (pair-fold-right revappend
                     '(Milosz Mistral)
                     '(Mommsen Montale Morrison Neruda Oe))
    1 (list?)
    (section equal? <> '(Oe Neruda Morrison Montale Mommsen Oe Neruda Morrison
                         Montale Oe Neruda Morrison Oe Neruda Oe Milosz Mistral)))

  (test pair-fold-right:several-null-lists
    (pair-fold-right revappall '(Pasternak Paz) '() '() '() '() '())
    1 (list?)
    (section equal? <> '(Pasternak Paz)))

  (test pair-fold-right:several-singleton-lists
    (pair-fold-right revappall '(Perse Pirandello)
                               '(Pontoppidan)
                               '(Quasimodo)
                               '(Reymont)
                               '(Rolland)
                               '(Russell))
    1 (list?)
    (section equal? <> '(Pontoppidan Quasimodo Reymont Rolland Russell Perse
                         Pirandello)))

  (test pair-fold-right:several-longer-lists
    (pair-fold-right revappall 
                     '(Sachs Sartre)
                     '(Seferis Shaw Sholokov Siefert Sienkiewicz Sillanpaa Simon)
                     '(Singer Solzhenitsyn Soyinka Spitteler Steinbeck Tagore
                       Undset)
                     '(Walcott White Yeats Anderson Andrews Angelina Aransas)
                     '(Archer Armstrong Alascosa Austin Bailey Bandera Bastrop)
                     '(Baylor Bee Bell Bexar Blanco Borden Bosque Bowie))
    1 (list?)
    (section equal? <> '(Simon Sillanpaa Sienkiewicz Siefert Sholokov Shaw
                         Seferis Undset Tagore Steinbeck Spitteler Soyinka
                         Solzhenitsyn Singer Aransas Angelina Andrews Anderson
                         Yeats White Walcott Bastrop Bandera Bailey Austin
                         Alascosa Armstrong Archer Bowie Bosque Borden Blanco
                         Bexar Bell Bee Baylor Simon Sillanpaa Sienkiewicz Siefert
                         Sholokov Shaw Undset Tagore Steinbeck Spitteler Soyinka
                         Solzhenitsyn Aransas Angelina Andrews Anderson Yeats
                         White Bastrop Bandera Bailey Austin Alascosa Armstrong
                         Bowie Bosque Borden Blanco Bexar Bell Bee Simon Sillanpaa
                         Sienkiewicz Siefert Sholokov Undset Tagore Steinbeck
                         Spitteler Soyinka Aransas Angelina Andrews Anderson Yeats
                         Bastrop Bandera Bailey Austin Alascosa Bowie Bosque
                         Borden Blanco Bexar Bell Simon Sillanpaa Sienkiewicz
                         Siefert Undset Tagore Steinbeck Spitteler Aransas
                         Angelina Andrews Anderson Bastrop Bandera Bailey Austin
                         Bowie Bosque Borden Blanco Bexar Simon Sillanpaa
                         Sienkiewicz Undset Tagore Steinbeck Aransas Angelina
                         Andrews Bastrop Bandera Bailey Bowie Bosque Borden
                         Blanco Simon Sillanpaa Undset Tagore Aransas Angelina
                         Bastrop Bandera Bowie Bosque Borden Simon Undset Aransas
                         Bastrop Bowie Bosque Sachs Sartre)))

  (test pair-fold-right:lists-of-different-lengths
    (pair-fold-right revappall '(Brazoria Brazos Brewster)
                               '(Briscoe Brooks Brown Burleson)
                               '(Burnet Caldwell)
                               '(Calhoun Callahan Cameron Camp Carson))
    1 (list?)
    (section equal? <> '(Burleson Brown Brooks Briscoe Caldwell Burnet Carson
                         Camp Cameron Callahan Calhoun Burleson Brown Brooks
                         Caldwell Carson Camp Cameron Callahan Brazoria Brazos
                         Brewster)))

  (test pair-fold-right:circular-lists
    (pair-fold-right (lambda (alpha beta gamma delta)
                       (cons (list (car alpha) (car beta) (car gamma))
                             delta))
                     '()
                     '(marker masculine mass matched matrix)
                     (circular-list 'maximal 'meal 'means)
                     (circular-list 'measure 'media 'meiosis 'member))
    1 (list?)
    (section equal? <> '((marker maximal measure)
                         (masculine meal media)
                         (mass means meiosis)
                         (matched maximal member)
                         (matrix meal measure)))))


(suite reduce-right ()

  (test reduce-right:null-list
    (reduce-right (lambda (alpha beta) (* alpha (+ beta 1))) 0 '())
    1 (integer?)
    zero?)

  (test reduce-right:singleton-list
    (reduce-right (lambda (alpha beta) (* alpha (+ beta 1))) 0 '(25))
    1 (integer?)
    (section = <> 25))

  (test reduce-right:doubleton-list
    (reduce-right (lambda (alpha beta) (* alpha (+ beta 1))) 0 '(27 29))
    1 (integer?)
    (section = <> 810))

  (test reduce-right:longer-list
    (reduce-right (lambda (alpha beta) (* alpha (+ beta 1)))
                  0
                  '(31 33 35 37 39 41 43))
    1 (integer?)
    (section = <> 93259601719)))


(suite append-map ()

  (test append-map:one-null-list
    (append-map (lambda (element) (list element element)) '())
    1 (null?))

  (test append-map:one-singleton-list
    (append-map (lambda (element) (list element element)) '(Cass))
    1 (list?)
    (section equal? <> '(Cass Cass)))

  (test append-map:one-longer-list
    (append-map (lambda (element) (list element element))
                '(Castro Chambers Cherokee Childress Clay))
    1 (list?)
    (section equal? <> '(Castro Castro Chambers Chambers Cherokee Cherokee
                         Childress Childress Clay Clay)))

  (test append-map:several-null-lists
    (append-map (section reverse <...>) '() '() '() '() '())
    1 (null?))

  (test append-map:several-singleton-lists
    (append-map (section reverse <...>) '(Cochran) '(Coke) '(Coleman) '(Collin)
                                        '(Collingsworth))
    1 (list?)
    (section equal? <> '(Collingsworth Collin Coleman Coke Cochran)))

  (test append-map:several-longer-lists
    (append-map (section reverse <...>)
                '(Colorado Comal Comanche Concho Cooke Coryell Cottle)
                '(Crane Crockett Crosby Culberson Dallam Dallas Dawson)
                '(Delta Denton Dewitt Dickens Dimmit Donley Duval) 
                '(Eastland Ector Edwards Ellis Erath Falls Fannin)
                '(Fayette Fisher Floyd Foard Franklin Freestone Frio))
    1 (list?)
    (section equal? <> '(Fayette Eastland Delta Crane Colorado Fisher Ector Denton
                         Crockett Comal Floyd Edwards Dewitt Crosby Comanche Foard
                         Ellis Dickens Culberson Concho Franklin Erath Dimmit
                         Dallam Cooke Freestone Falls Donley Dallas Coryell Frio
                         Fannin Duval Dawson Cottle)))

  (test append-map:some-circular-lists
    (append-map (section reverse <...>)
                (circular-list 'inhabitant 'inherent 'initial)
                (circular-list 'initialism 'inquiry)
                '(instruction instrument integrated intensificatory intensifier))
    1 (list?)
    (section equal? <> '(instruction initialism inhabitant instrument inquiry
                         inherent integrated initialism initial intensificatory
                         inquiry inhabitant intensifier initialism inherent))))

(suite append-map! ()

  (test append-map!:one-null-list
    (append-map! (lambda (element) (list element element)) (list))
    1 (null?))

  (test append-map!:one-singleton-list
    (append-map! (lambda (element) (list element element)) (list 'Gaines))
    1 (list?)
    (section equal? <> '(Gaines Gaines)))

  (test append-map!:one-longer-list
    (append-map! (lambda (element) (list element element))
                 (list 'Galveston 'Garza 'Gillespie 'Glasscock 'Goliad))
    1 (list?)
    (section equal? <> '(Galveston Galveston Garza Garza Gillespie Gillespie
                         Glasscock Glasscock Goliad Goliad)))

  (test append-map!:several-null-lists
    (append-map! (section reverse <...>) (list) (list) (list) (list) (list))
    1 (null?))

  (test append-map!:several-singleton-lists
    (append-map! (section reverse <...>) (list 'Gonzales)
                                         (list 'Gray)
                                         (list 'Grayson)
                                         (list 'Gregg)
                                         (list 'Grimes))
    1 (list?)
    (section equal? <> '(Grimes Gregg Grayson Gray Gonzales)))

  (test append-map!:several-longer-lists
    (append-map! (section reverse <...>)
                 (list 'Guadalupe 'Hale 'Hall 'Hamilton 'Hansford 'Hardeman
                       'Hardin)
                 (list 'Harris 'Harrison 'Hartley 'Haskell 'Hays 'Hemphill
                       'Henderson)
                 (list 'Hidalgo 'Hill 'Hockley 'Hood 'Hopkins 'Houston 'Howard)
                 (list 'Hudspeth 'Hunt 'Hutchinson 'Irion 'Jack 'Jackson 'Jasper)
                 (list 'Jefferson 'Johnson 'Jones 'Karnes 'Kaufman 'Kendall
                       'Kenedy))
    1 (list?)
    (section equal? <> '(Jefferson Hudspeth Hidalgo Harris Guadalupe Johnson Hunt
                         Hill Harrison Hale Jones Hutchinson Hockley Hartley Hall
                         Karnes Irion Hood Haskell Hamilton Kaufman Jack Hopkins
                         Hays Hansford Kendall Jackson Houston Hemphill Hardeman
                         Kenedy Jasper Howard Henderson Hardin)))

  (test append-map!:some-circular-lists
    (append-map! (section reverse <...>)
                 (circular-list 'interjection 'internal)
                 (circular-list 'interpolation)
                 (circular-list 'interrogative 'intervening 'intonation)
                 '(intransitive intrusive invention inversion))
    1 (list?)
    (section equal? <> '(intransitive interrogative interpolation interjection
                         intrusive intervening interpolation internal invention
                         intonation interpolation interjection inversion
                         interrogative interpolation internal))))


(suite map! ()

  (test map!:one-null-list
    (map! vector (list))
    1 (null?))

  (test map!:one-singleton-list
    (map! vector (list 'Kent))
    1 (list?)
    (section equal? <> '(#(Kent))))

  (test map!:one-longer-list
    (map vector (list 'Kerr 'Kimble 'King 'Kinney 'Kleberg))
    1 (list?)
    (section equal? <> '(#(Kerr) #(Kimble) #(King) #(Kinney) #(Kleberg))))

  (test map!:several-null-lists
    (map! vector (list) (list) (list) (list) (list))
    1 (null?))

  (test map!:several-singleton-lists
    (map! vector (list 'Knox) (list 'Lamar) (list 'Lamb) (list 'Lampasas)
                 (list 'Lavaca))
    1 (list?)
    (section equal? <> '(#(Knox Lamar Lamb Lampasas Lavaca))))

  (test map!:several-longer-lists
    (map! vector (list 'Lee 'Leon 'Liberty 'Limestone 'Lipscomb 'Llano 'Loving)
                 (list 'Lubbock 'Lynn 'McCulloch 'McLennan 'McMullen 'Madison
                       'Marion)
                 (list 'Martin 'Mason 'Matagorda 'Maverick 'Medina 'Menard
                       'Midland)
                 (list 'Milam 'Mills 'Mitchell 'Montague 'Montgomery 'Moore
                       'Morris)
                 (list 'Motley 'Nacogdoches 'Navarro 'Newton 'Nolan 'Nueces
                       'Ochiltree))
    1 (list?)
    (section equal? <> '(#(Lee Lubbock Martin Milam Motley)
                         #(Leon Lynn Mason Mills Nacogdoches)
                         #(Liberty McCulloch Matagorda Mitchell Navarro)
                         #(Limestone McLennan Maverick Montague Newton)
                         #(Lipscomb McMullen Medina Montgomery Nolan)
                         #(Llano Madison Menard Moore Nueces)
                         #(Loving Marion Midland Morris Ochiltree))))
  (test map!:some-circular-lists
    (map! vector '(italian italics iteration iterative japanese)
                 (circular-list 'inverted 'invitation)
                 (circular-list 'irrealis 'irregular 'islands))
    1 (list?)
    (section equal? <> '(#(italian inverted irrealis)
                         #(italics invitation irregular)
                         #(iteration inverted islands)
                         #(iterative invitation irrealis)
                         #(japanese inverted irregular)))))


(suite pair-for-each ()

  (test pair-for-each:one-null-list
    (let ((base '()))
      (pair-for-each (lambda (tail)
                       (set! base (append tail base)))
                     '())
      base)
    1 (null?))

  (test pair-for-each:one-singleton-list
    (let ((base '()))   
      (pair-for-each (lambda (tail)
                       (set! base (append tail base)))
                     '(Victoria))
      base)
    1 (list?)
    (section equal? <> '(Victoria)))

  (test pair-for-each:one-longer-list
    (let ((base '()))
      (pair-for-each (lambda (tail)
                       (set! base (append tail base)))
                     '(Walker Waller Ward Washington Webb))
      base)
    1 (list?)
    (section equal? <> '(Webb Washington Webb Ward Washington Webb Waller Ward
                         Washington Webb Walker Waller Ward Washington Webb)))

  (test pair-for-each:several-null-lists
    (let ((base '()))
      (pair-for-each (lambda tails
                       (set! base
                             (cons (apply vector tails) base)))
                     '() '() '() '() '())
      base)
    1 (null?))

  (test pair-for-each:several-singleton-lists
    (let ((base '()))
      (pair-for-each (lambda tails
                       (set! base
                             (cons (apply vector tails) base)))
                     '(Wharton)
                     '(Wheeler)
                     '(Wichita)
                     '(Wilbarger)
                     '(Willacy))
      base)
    1 (list?)
    (section equal? <> '(#((Wharton) (Wheeler) (Wichita) (Wilbarger)
                           (Willacy)))))

  (test pair-for-each:several-longer-lists
    (let ((base '()))
      (pair-for-each (lambda tails
                       (set! base
                             (cons (apply vector tails) base)))
                     '(Williamson Wilson Winkler Wise Wood Yoakum Young)
                     '(Zapata Zavala Admiral Advil Ajax Anacin Arrid)
                     '(Arnold Ban Barbie Beech Blockbuster Bounce Breck)
                     '(Budweiser Bufferin BVD Carrier Celeste Charmin Cheer)
                     '(Cheerios Cinemax Clairol Clorets Combat Comet Coppertone))
      base)
    1 (list?)
    (section equal? <> '(#((Young) (Arrid) (Breck) (Cheer) (Coppertone))
                         #((Yoakum Young) (Anacin Arrid) (Bounce Breck)
                           (Charmin Cheer) (Comet Coppertone))
                         #((Wood Yoakum Young)
                           (Ajax Anacin Arrid)
                           (Blockbuster Bounce Breck)
                           (Celeste Charmin Cheer) 
                           (Combat Comet Coppertone))
                         #((Wise Wood Yoakum Young) 
                           (Advil Ajax Anacin Arrid)
                           (Beech Blockbuster Bounce Breck)
                           (Carrier Celeste Charmin Cheer)
                           (Clorets Combat Comet Coppertone))
                         #((Winkler Wise Wood Yoakum Young) 
                           (Admiral Advil Ajax Anacin Arrid)
                           (Barbie Beech Blockbuster Bounce Breck)
                           (BVD Carrier Celeste Charmin Cheer)
                           (Clairol Clorets Combat Comet Coppertone))
                         #((Wilson Winkler Wise Wood Yoakum Young) 
                           (Zavala Admiral Advil Ajax Anacin Arrid)
                           (Ban Barbie Beech Blockbuster Bounce Breck)
                           (Bufferin BVD Carrier Celeste Charmin Cheer)
                           (Cinemax Clairol Clorets Combat Comet Coppertone))
                         #((Williamson Wilson Winkler Wise Wood Yoakum Young) 
                           (Zapata Zavala Admiral Advil Ajax Anacin Arrid)
                           (Arnold Ban Barbie Beech Blockbuster Bounce Breck)
                           (Budweiser Bufferin BVD Carrier Celeste Charmin Cheer)
                           (Cheerios Cinemax Clairol Clorets Combat Comet
                            Coppertone)))))

  (test pair-for-each:some-circular-lists
    (let ((base '()))
      (pair-for-each (lambda pairs
                       (set! base (cons (map car pairs) base)))
                     (circular-list 'liaison 'licensing)
                     (circular-list 'light 'likeness 'liking)
                     '(limited limiting link linking location))
      base)
    1 (list?)
    (section equal? <> '((liaison likeness location)
                         (licensing light linking)
                         (liaison liking link)
                         (licensing likeness limiting)
                         (liaison light limited)))))
  

(suite filter-map ()

  (test filter-map:one-null-list
    (filter-map values '())
    1 (null?))

  (test filter-map:one-singleton-list
    (filter-map values '(Crest))
    1 (list?)
    (section equal? <> '(Crest)))

  (test filter-map:one-list-all-elements-removed
    (filter-map never '(Crisco Degree Doritos Dristan Efferdent))
    1 (null?))

  (test filter-map:one-list-some-elements-removed
    (filter-map (lambda (n) (and (even? n) n)) '(44 45 46 47 48 49 50))
    1 (list?)
    (section equal? <> '(44 46 48 50)))

  (test filter-map:one-list-no-elements-removed
    (filter-map values '(ESPN Everready Excedrin Fab Fantastik))
    1 (list?)
    (section equal? <> '(ESPN Everready Excedrin Fab Fantastik)))

  (test filter-map:several-null-lists
    (filter-map vector '() '() '() '() '())
    1 (null?))

  (test filter-map:several-singleton-lists
    (filter-map vector '(Foamy) '(Gatorade) '(Glad) '(Gleem) '(Halcion))
    1 (list?)
    (section equal? <> '(#(Foamy Gatorade Glad Gleem Halcion))))

  (test filter-map:several-lists-all-elements-removed
    (filter-map never '(Hanes HBO Hostess Huggies Ivory Kent Kinney)
                      '(Kleenex Knorr Lee Lenox Lerner Listerine Marlboro)
                      '(Mazola Michelob Midas Miller NBC Newsweek Noxema)
                      '(NutraSweet Oreo Pampers People Planters Playskool Playtex)
                      '(Prego Prell Prozac Purex Ritz Robitussin Rolaids))
    1 (null?))

  (test filter-map:several-lists-some-elements-removed
    (filter-map (lambda arguments
                  (let ((sum (apply + arguments)))
                    (and (odd? sum) sum)))
                '(51 52 53 54 55 56 57)
                '(58 59 60 61 62 63 64)
                '(65 66 67 68 69 70 71)
                '(72 73 74 75 76 77 78)
                '(79 80 81 82 83 84 85))
    1 (list?)
    (section equal? <> '(325 335 345 355)))

  (test filter-map:several-lists-no-elements-removed
    (filter-map vector
                '(Ronzoni Ruffles Scotch Skippy SnackWell Snapple Spam)
                '(Sprite Swanson Thomas Tide Tonka Trojan Tupperware)
                '(Tylenol Velveeta Vicks Victory Visine Wheaties Wise)
                '(Wonder Ziploc Abbott Abingdon Ackley Ackworth Adair)
                '(Adams Adaville Adaza Adel Adelphi Adena Afton))
    1 (list?)
    (section equal? <> '(#(Ronzoni Sprite Tylenol Wonder Adams)
                         #(Ruffles Swanson Velveeta Ziploc Adaville)
                         #(Scotch Thomas Vicks Abbott Adaza)
                         #(Skippy Tide Victory Abingdon Adel)
                         #(SnackWell Tonka Visine Ackley Adelphi)
                         #(Snapple Trojan Wheaties Ackworth Adena)
                         #(Spam Tupperware Wise Adair Afton))))

  (test filter-map:some-circular-lists
    (filter-map (lambda arguments
                  (let ((sum (apply + arguments)))
                    (and (odd? sum) sum)))
                '(734 735 736 737 738)
                (circular-list 739 740)
                (circular-list 741 742 743))
    1 (list?)
    (section equal? <> '(2217 2219))))


(suite map-in-order ()

  (test map-in-order:one-null-list
    (let ((counter 0))
      (map-in-order (lambda (element)
                      (set! counter (+ counter 1))
                      (cons counter element))
                    '()))
    1 (null?))

  (test map-in-order:one-singleton-list
    (let ((counter 0))
      (map-in-order (lambda (element)
                      (set! counter (+ counter 1))
                      (cons counter element))
                    '(Oldham)))
    1 (list?)
    (section equal? <> '((1 . Oldham))))

  (test map-in-order:one-longer-list
    (let ((counter 0))
      (map-in-order (lambda (element)
                      (set! counter (+ counter 1))
                      (cons counter element))
                    '(Orange Panola Parker Parmer Pecos)))
    1 (list?)
    (section equal? <> '((1 . Orange) (2 . Panola) (3 . Parker) (4 . Parmer)
                         (5 . Pecos))))

  (test map-in-order:several-null-lists
    (let ((counter 0))
      (map-in-order (lambda elements
                      (set! counter (+ counter 1))
                      (apply vector counter elements))
                    '() '() '() '() '()))
    1 (null?))

  (test map-in-order:several-singleton-lists
    (let ((counter 0))
      (map-in-order (lambda elements
                      (set! counter (+ counter 1))
                      (apply vector counter elements))
                    '(Polk)
                    '(Potter)
                    '(Presidio)
                    '(Rains)
                    '(Randall)))
    1 (list?)
    (section equal? <> '(#(1 Polk Potter Presidio Rains Randall))))

  (test map-in-order:several-longer-lists
    (let ((counter 0))
      (map-in-order (lambda elements
                      (set! counter (+ counter 1))
                      (apply vector counter elements))
                    '(Reagan Real Reeves Refugio Roberts Robertson Rockwall)
                    '(Runnels Rusk Sabine Schleicher Scurry Shackelford Shelby)
                    '(Sherman Smith Somervell Starr Stephens Sterling Stonewall)
                    '(Sutton Swisher Tarrant Taylor Terrell Terry Throckmorton)
                    '(Titus Travis Trinity Tyler Upshur Upton Uvalde)))
    1 (list?)
    (section equal? <> '(#(1 Reagan Runnels Sherman Sutton Titus)
                         #(2 Real Rusk Smith Swisher Travis)
                         #(3 Reeves Sabine Somervell Tarrant Trinity)
                         #(4 Refugio Schleicher Starr Taylor Tyler)
                         #(5 Roberts Scurry Stephens Terrell Upshur)
                         #(6 Robertson Shackelford Sterling Terry Upton)
                         #(7 Rockwall Shelby Stonewall Throckmorton Uvalde))))

  (test map-in-order:some-circular-lists
    (let ((counter 0))
      (map-in-order (lambda elements
                      (set! counter (+ counter 1))
                      (apply vector counter elements))
                    (circular-list 'joint 'justification 'juxtaposed)
                    '(kin labile latin layered left)
                    (circular-list 'let 'letter 'lexeme 'lexical)))
    1 (list?)
    (section equal? <> '(#(1 joint kin let)
                         #(2 justification labile letter)
                         #(3 juxtaposed latin lexeme)
                         #(4 joint layered lexical)
                         #(5 justification left let)))))


(suite filter ()

  (test filter:null-list
    (filter always '())
    1 (null?))

  (test filter:singleton-list
    (filter always '(Agency))
    1 (list?)
    (section equal? <> '(Agency)))

  (test filter:all-elements-removed
    (filter never '(Ainsworth Akron Albany Albaton Albia))
    1 (null?))

  (test filter:some-elements-removed
    (filter even? '(86 87 88 89 90))
    1 (list?)
    (section equal? <> '(86 88 90)))

  (test filter:no-elements-removed
    (filter always '(Albion Alburnett Alden Alexander Algona))
    1 (list?)
    (section equal? <> '(Albion Alburnett Alden Alexander Algona))))


(suite partition ()

  (test partition:null-list
    (partition never '())
    2 (null? null?))

  (test partition:singleton-list
    (partition never '(Arispe))
    2 (list? list?)
    (lambda (in out)
      (and (null? in) (equal? out '(Arispe)))))

  (test partition:all-satisfying
    (partition always '(Arlington Armstrong Arnold Artesian Arthur))
    2 (list? list?)
    (lambda (in out)
      (and (equal? in '(Arlington Armstrong Arnold Artesian Arthur))
           (null? out))))

  (test partition:mixed-starting-in
    (partition even? '(106 108 109 111 113 114 115 117 118 120))
    2 (list? list?)
    (lambda (in out)
      (and (equal? in '(106 108 114 118 120))
           (equal? out '(109 111 113 115 117)))))

  (test partition:mixed-starting-out
    (partition even? '(121 122 124 126))
    2 (list? list?)
    (lambda (in out)
      (and (equal? in '(122 124 126))
           (equal? out '(121)))))

  (test partition:none-satisfying
    (partition never '(Asbury Ashawa Ashland Ashton Aspinwall))
    2 (list? list?)
    (lambda (in out)
      (and (null? in)
           (equal? out '(Asbury Ashawa Ashland Ashton Aspinwall))))))


(suite remove ()

  (test remove:null-list
    (remove always '())
    1 (null?))

  (test remove:singleton-list
    (remove never '(Alvord))
    1 (list?)
    (section equal? <> '(Alvord)))

  (test remove:all-elements-removed
    (remove always '(Amana Amber Ames Amish Anamosa))
    1 (null?))

  (test remove:some-elements-removed
    (remove even? '(96 97 98 99 100))
    1 (list?)
    (section equal? <> '(97 99)))

  (test remove:no-elements-removed
    (remove never '(Anderson Andover Andrew Andrews Angus))
    1 (list?)
    (section equal? <> '(Anderson Andover Andrew Andrews Angus))))


(suite filter! ()

  (test filter!:null-list
    (filter! always (list))
    1 (null?))

  (test filter!:singleton-list
    (filter! always (list 'Alice))
    1 (list?)
    (section equal? <> '(Alice)))

  (test filter!:all-elements-removed
    (filter! never (list 'Alleman 'Allendorf 'Allerton 'Allison 'Almont))
    1 (null?))

  (test filter!:some-elements-removed
    (filter! even? (list 91 92 93 94 95))
    1 (list?)
    (section equal? <> '(92 94)))

  (test filter!:no-elements-removed
    (filter! always (list 'Almoral 'Alpha 'Alta 'Alton 'Altoona))
    1 (list?)
    (section equal? <> '(Almoral Alpha Alta Alton Altoona))))


(suite partition! ()

  (test partition!:null-list
    (partition! never (list))
    2 (null? null?))

  (test partition!:singleton-list
    (partition! never (list 'Astor))
    2 (list? list?)
    (lambda (in out)
      (and (null? in) (equal? out '(Astor)))))

  (test partition!:all-satisfying
    (partition! always (list 'Atalissa 'Athelstan 'Atkins 'Atlantic 'Attica))
    2 (list? list?)
    (lambda (in out)
      (and (equal? in '(Atalissa Athelstan Atkins Atlantic Attica))
           (null? out))))

  (test partition!:mixed-starting-in
    (partition! odd? (list 127 129 130 132 134 135 136 138 139 141))
    2 (list? list?)
    (lambda (in out)
      (and (equal? in '(127 129 135 139 141))
           (equal? out '(130 132 134 136 138)))))

  (test partition!:mixed-starting-out
    (partition! odd? (list 142 143 145 147))
    2 (list? list?)
    (lambda (in out)
      (and (equal? in '(143 145 147)) (equal? out '(142)))))

  (test partition!:none-satisfying
    (partition! never (list 'Auburn 'Audubon 'Augusta 'Aurelia 'Aureola))
    2 (list? list?)
    (lambda (in out)
      (and (null? in) (equal? out '(Auburn Audubon Augusta Aurelia Aureola))))))


(suite remove! ()

  (test remove!:null-list
    (remove! always (list))
    1 (null?))

  (test remove!:singleton-list
    (remove! never (list 'Anita))
    1 (list?)
    (section equal? <> '(Anita)))

  (test remove!:all-elements-removed
    (remove! always (list 'Ankeny 'Anthon 'Aplington 'Arcadia 'Archer))
    1 (null?))

  (test remove!:some-elements-removed
    (remove! even? (list 101 102 103 104 105))
    1 (list?)
    (section equal? <> '(101 103 105)))

  (test remove!:no-elements-removed
    (remove! never (list 'Ardon 'Aredale 'Argo 'Argyle 'Arion))
    1 (list?)
    (section equal? <> '(Ardon Aredale Argo Argyle Arion))))


(suite find ()

  (test find:in-null-list
    (find always '())
    1 (false?))

  (test find:in-singleton-list
    (find always '(Aurora))
    1 (always)
    (section eq? <> 'Aurora))

  (test find:not-in-singleton-list
    (find never '(Austinville))
    1 (false?))

  (test find:at-front-of-longer-list
    (find always '(Avery Avoca Avon Ayrshire Badger))
    1 (always)
    (section eq? <> 'Avery))

  (test find:in-middle-of-longer-list
    (find even? '(149 151 153 155 156 157 159))
    1 (number?)
    (section = <> 156))

  (test find:at-end-of-longer-list
    (find even? '(161 163 165 167 168))
    1 (number?)
    (section = <> 168))

  (test find:not-in-longer-list
    (find never '(Bagley Bailey Badwin Balfour Balltown))
    1 (false?))

  (test find:in-circular-list
    (find even? (cons 745 (circular-list 747 749 750 751)))
    1 (number?)
    (section = <> 750)))


(suite find-tail ()

  (test find-tail:in-null-list
    (find-tail always '())
    1 (false?))

  (let ((source '(Ballyclough)))
    (test find-tail:in-singleton-list
      (find-tail always source)
      1 (pair?)
      (section eq? <> source)))

  (test find-tail:not-in-singleton-list
    (find-tail never '(Bancroft))
    1 (false?))

  (let ((source '(Bangor Bankston Barney Barnum Bartlett)))
    (test find-tail:at-front-of-longer-list
      (find-tail always source)
      1 (pair?)
      (section eq? <> source)))

  (let ((source '(169 171 173 175 176 177 179)))
    (test find-tail:in-middle-of-longer-list
      (find-tail even? source)
      1 (pair?)
      (section eq? <> (cddddr source))))

  (let ((source '(181 183 185 187 188)))
    (test find-tail:at-end-of-longer-list
      (find-tail even? source)
      1 (pair?)
      (section eq? <> (cddddr source))))

  (test find-tail:not-in-longer-list
    (find-tail never '(Batavia Bauer Baxter Bayard Beacon))
    1 (false?))

  (let ((source (cons 753 (cons 755 (circular-list 757 759 760 761)))))
    (test find-tail:in-circular-list
      (find-tail even? source)
      1 (pair?)
      (section eq? <> (cddddr source)))))


(suite any ()

  (test any:in-one-null-list
    (any always '())
    1 (false?))

  (test any:in-one-singleton-list
    (any vector '(Beaconsfield))
    1 (always)
    (section equal? <> '#(Beaconsfield)))

  (test any:not-in-one-singleton-list
    (any never '(Beaman))
    1 (false?))

  (test any:at-beginning-of-one-longer-list
    (any vector '(Beaver Beaverdale Beckwith Bedford Beebeetown))
    1 (always)
    (section equal? <> '#(Beaver)))

  (test any:in-middle-of-one-longer-list
    (any (lambda (x) (and (odd? x) (+ x 189))) '(190 192 194 196 197 198 200))
    1 (number?)
    (section = <> 386))

  (test any:at-end-of-one-longer-list
    (any (lambda (x) (and (odd? x) (+ x 201))) '(202 204 206 208 209))
    1 (number?)
    (section = <> 410))

  (test any:not-in-one-longer-list
    (any never '(Beech Belinda Belknap Bellefountain Bellevue))
    1 (false?))

  (test any:in-several-null-lists
    (any vector '() '() '() '() '())
    1 (false?))

  (test any:in-several-singleton-lists
    (any vector '(Belmond) '(Beloit) '(Bennett) '(Benson) '(Bentley))
    1 (always)
    (section equal? <> '#(Belmond Beloit Bennett Benson Bentley)))

  (test any:not-in-several-singleton-lists
    (any never '(Benton) '(Bentonsport) '(Berea) '(Berkley) '(Bernard))
    1 (false?))

  (test any:at-beginning-of-several-longer-lists
    (any vector '(Berne Bertram Berwick Bethesda Bethlehem Bettendorf Beulah)
                '(Bevington Bidwell Bingham Birmingham Bladensburg Blairsburg
                  Blairstown)
                '(Blakesburg Blanchard Blencoe Bliedorn Blockton Bloomfield
                  Bloomington)
                '(Bluffton Bode Bolan Bonair Bonaparte Bondurant Boone)
                '(Booneville Botany Botna Bouton Bowsher Boxholm Boyd))
    1 (always)
    (section equal? <> '#(Berne Bevington Blakesburg Bluffton Booneville)))

  (test any:in-middle-of-several-longer-lists
    (any (lambda arguments
           (let ((sum (apply + arguments)))
             (and (odd? sum) (+ sum 210))))
         '(211 212 213 214 215 216 217)
         '(218 219 220 221 222 223 224)
         '(225 226 227 228 229 230 231)
         '(232 233 234 235 236 237 238)
         '(240 242 244 246 247 248 250))
    1 (number?)
    (section = <> 1359))

  (test any:at-end-of-several-longer-lists
    (any (lambda arguments
           (let ((sum (apply + arguments)))
             (and (even? sum) (+ sum 210))))
         '(252 253 254 255 256 257 258)
         '(259 260 261 262 263 264 265)
         '(266 267 268 269 270 271 272)
         '(273 274 275 276 277 278 279)
         '(281 283 285 287 289 291 292))
    1 (number?)
    (section = <> 1576))

  (test any:not-in-several-longer-lists
    (any never '(Boyden Boyer Braddyville Bradford Bradgate Brainard Brandon)
               '(Brayton Brazil Breda Bridgewater Brighton Bristol Bristow)
               '(Britt Bromley Brompton Bronson Brooklyn Brooks Brookville)
               '(Browns Brownville Brunsville Brushy Bryant Bryantsburg Buchanan)
               '(Buckeye Buckhorn Buckingham Bucknell Budd Buffalo Burchinal))
    1 (false?))

  (test any:not-in-lists-of-unequal-length
    (any never '(Burdette Burlington Burnside Burt)
               '(Bushville Bussey)
               '(Buxton Cairo Calamus)
               '(Caledonia Clahoun Callender Calmar Caloma Calumet))
    1 (false?))
  
  (test any:circular-lists
    (any (lambda arguments
           (let ((sum (apply + arguments)))
             (and (= (remainder sum 5) 3) (+ sum 210))))
         (circular-list 780 781)
         (circular-list 785 790 791)
         (circular-list 795 800 805 810 811))
    1 (number?)
    (section = <> 2593)))

(suite every ()

  (test every:in-one-null-list
    (every values '())
    1 (true?))

  (test every:in-one-singleton-list
    (every vector '(Camanche))
    1 (always)
    (section equal? <> '#(Camanche)))

  (test every:not-in-one-singleton-list
    (every never '(Cambria))
    1 (false?))

  (test every:failing-at-beginning-of-one-longer-list
    (every never '(Cambridge Cameron Canby Canton Cantril))
    1 (false?))

  (test every:failing-in-middle-of-one-longer-list
    (every (lambda (x) (and (even? x) (+ x 293))) '(294 296 298 300 301 302 304))
    1 (false?))

  (test every:failing-at-end-of-one-longer-list
    (every (lambda (x) (and (even? x) (+ x 305))) '(306 308 310 312 313))
    1 (false?))

  (test every:in-one-longer-list
    (every vector '(Carbon Carbondale Carl Carlisle Carmel))
    1 (always)
    (section equal? <> '#(Carmel)))

  (test every:in-several-null-lists
    (every vector '() '() '() '() '())
    1 (true?))

  (test every:in-several-singleton-lists
    (every vector '(Carnarvon) '(Carnes) '(Carney) '(Carnforth) '(Carpenter))
    1 (always)
    (section  equal? <> '#(Carnarvon Carnes Carney Carnforth Carpenter)))

  (test every:not-in-several-singleton-lists
    (every never '(Carroll) '(Carrollton) '(Carrville) '(Carson) '(Cartersville))
    1 (false?))

  (test every:failing-at-beginning-of-several-longer-lists
    (every never '(Cascade Casey Castalia Castana Cattese Cedar Centerdale)
                 '(Centerville Centralia Ceres Chapin Chariton Charleston
                   Charlotte)
                 '(Chatsworth Chautauqua Chelsea Cheney Cherokee Chester Chickasaw)
                 '(Chillicothe Churchtown Churchville Churdan Cincinnati Clare
                   Clarence)
                 '(Clarinda Clarion Clark Clarkdale Clarksville Clayton
                   Clearfield))
    1 (false?))

  (test every:failing-in-middle-of-several-longer-lists
    (every (lambda arguments
             (let ((sum (apply + arguments)))
               (and (odd? sum) (+ sum 314))))
           '(315 316 317 318 319 320 321)
           '(322 323 324 325 326 327 328)
           '(329 330 331 332 333 334 335)
           '(336 337 338 339 340 341 342)
           '(343 345 347 349 350 351 353))
    1 (false?))

  (test every:failing-at-end-of-several-longer-lists
    (every (lambda arguments
             (let ((sum (apply + arguments)))
               (and (odd? sum) (+ sum 354))))
           '(355 356 357 358 359 360 361)
           '(362 363 364 365 366 367 368)
           '(369 370 371 372 373 374 375)
           '(376 377 378 379 380 381 382)
           '(383 385 387 389 391 393 394))
    1 (false?))

  (test every:in-several-longer-lists
    (every vector '(Cleghorn Clemons Clermont Cleves Cliffland Climax Clinton)
           '(Clio Clive Cloverdale Clucas Clutier Clyde Coalville)
           '(Coburg Coggon Coin Colesburg Colfax Collett Collins)
           '(Colo Columbia Colwell Commerce Communia Competine Concord)
           '(Conesville Confidence Cono Conover Conrad Conroy Consol))
    1 (always)
    (section equal? <> '#(Clinton Coalville Collins Concord Consol)))

  (test every:in-lists-of-unequal-length
    (every vector '(Conway Cool Cooper Coppock)
                  '(Coralville Corley)
                  '(Cornelia Cornell Corning)
                  '(Correctionville Corwith Corydon Cosgrove Coster Cotter))
    1 (always)
    (section equal? <> '#(Cool Corley Cornell Corwith)))

  (test every:circular-lists
    (every (lambda arguments
             (let ((sum (apply + arguments)))
               (and (< (remainder sum 5) 3) (+ sum 812))))
           (circular-list 815 816)
           (circular-list 820 825 826)
           (circular-list 830 835 840 845 846))
    1 (false?)))


(suite list-index ()

  (test list-index:in-one-null-list
    (list-index always '())
    1 (false?))

  (test list-index:in-one-singleton-list
    (list-index always '(Cottonville))
    1 (number?)
    zero?)

  (test list-index:not-in-one-singleton-list
    (list-index never '(Coulter))
    1 (false?))

  (test list-index:at-front-of-one-longer-list
    (list-index always '(Covington Craig Cranston Crathorne Crawfordsville))
    1 (number?)
    zero?)

  (test list-index:in-middle-of-one-longer-list
    (list-index even? '(395 397 399 401 402 403 405))
    1 (number?)
    (section = <> 4))

  (test list-index:at-end-of-one-longer-list
    (list-index odd? '(406 408 410 412 414 415))
    1 (number?)
    (section = <> 5))

  (test list-index:not-in-one-longer-list
    (list-index never '(Crescent Cresco Creston Crocker Crombie))
    1 (false?))

  (test list-index:in-several-null-lists
    (list-index always '() '() '() '() '())
    1 (false?))

  (test list-index:in-several-singleton-lists
    (list-index always '(Cromwell) '(Croton) '(Cumberland) '(Cumming) '(Curlew))
    1 (number?)
    zero?)

  (test list-index:not-in-several-singleton-lists
    (list-index never '(Cushing) '(Cylinder) '(Dahlonega) '(Dalby) '(Dale))
    1 (false?))

  (test list-index:at-front-of-several-longer-lists
    (list-index always '(Dallas Dana Danbury Danville Darbyville Davenport Dawson)
                       '(Dayton Daytonville Dean Decorah Dedham Deerfield
                         Defiance)
                       '(Delaware Delhi Delmar Deloit Delphos Delta Denhart)
                       '(Denison Denmark Denova Denver Depew Derby Devon)
                       '(Dewar Dexter Diagonal Dickens Dickieville Dike Dillon))
    1 (number?)
    zero?)

  (test list-index:in-middle-of-several-longer-lists
    (list-index (lambda arguments (odd? (apply + arguments)))
                '(416 417 418 419 420 421 422)
                '(423 424 425 426 427 428 429)
                '(430 431 432 433 434 435 436)
                '(437 438 439 440 441 442 443)
                '(444 446 448 450 451 452 454))
    1 (number?)
    (section = <> 4))

  (test list-index:at-end-of-several-longer-lists
    (list-index (lambda arguments (even? (apply + arguments)))
                '(455 456 457 458 459 460)
                '(461 462 463 464 465 466)
                '(467 468 469 470 471 472)
                '(473 474 475 476 477 478)
                '(479 481 483 485 487 488))
    1 (number?)
    (section = <> 5))

  (test list-index:not-in-several-longer-lists
    (list-index never '(Dinsdale Dixon Dodgeville Dolliver Donahue Donnan
                        Donnelley)
                      '(Donnellson Doon Dorchester Doris Douds Dougherty Douglas)
                      '(Doney Dows Drakesville Dresden Dubuque Dudley Dumfries)
                      '(Dumont Dunbar Duncan Duncombe Dundee Dunkerton Dunlap)
                      '(Durango Durant Durham Dutchtown Dyersville Dysart
                       Earlham))
    1 (false?))

  (test list-index:circular-lists
    (list-index (lambda arguments
                  (= (remainder (apply + arguments) 5) 3))
                (circular-list 850 851)
                (circular-list 855 860 861)
                (circular-list 865 870 875 880 881))
    1 (number?)
    (section = <> 29)))


(suite take-while ((rounder (circular-list 'pahari 'palau 'palaung 'pali)))

  (test take-while:empty-list
    (take-while always '())
    1 (null?))

  (test take-while:all-satisfying
    (take-while always '(oraon oriya osmanli ossetic))
    1 (list?)
    (section equal? <> '(oraon oriya osmanli ossetic)))

  (test take-while:non-satisfying-at-front
    (take-while even? '(595 596 597 598 599))
    1 (null?))

  (test take-while:continue-to-middle
    (take-while even? '(600 602 604 606 607 608 609))
    1 (list?)
    (section equal? <> '(600 602 604 606)))

  (test take-while:some-of-circular-list
    (take-while (lambda (element)
                  (not (eq? element 'palaung)))
                rounder)
    1 (list?)
    (section equal? <> '(pahari palau)))

  (test take-while:none-of-circular-list
    (take-while never rounder)
    1 (null?)))


(suite drop-while ((rounder (circular-list 'permian 'phrygian 'polish)))

  (test drop-while:empty-list
    (drop-while always '())
    1 (null?))

  (test drop-while:none-satisfying
    (drop-while never '(pampango pangasinan panjabi))
    1 (list?)
    (section equal? <> '(pampango pangasinan panjabi)))

  (test drop-while:continue-to-middle
    (drop-while even? '(610 612 613 614 615))
    1 (list?)
    (section equal? <> '(613 614 615)))

  (test drop-while:some-of-circular-list
    (drop-while (lambda (element)
                  (not (eq? element 'phrygian)))
                rounder)
    1 (circular-list?)
    (section eq? <> (cdr rounder)))

  (test drop-while:all-of-circular-list
    (drop-while never rounder)
    1 (circular-list?)
    (section eq? <> rounder)))


(suite take-while! ((rounder (circular-list 'portuguese 'prakrit 'quechuan
                                            'rajasthani 'rejang)))

  (test take-while!:empty-list
    (take-while! always (list))
    1 (null?))

  (test take-while!:all-satisfying
    (take-while! always (list 'romaic 'romany 'ronga 'rumanian 'russian))
    1 (list?)
    (section equal? <> '(romaic romany ronga rumanian russian)))

  (test take-while!:non-satisfying-at-front
    (take-while! even? '(617 618 619))
    1 (null?))

  (test take-while!:continue-to-middle
    (take-while! even? '(620 622 624 625 626 627 628))
    1 (list?)
    (section equal? <> '(620 622 624)))

  (test take-while!:some-of-circular-list
    (take-while! (lambda (element)
                   (not (eq? element 'rejang)))
                 rounder)
    1 (list?)
    (section equal? <> '(portuguese prakrit quechuan rajasthani)))

  (test take-while!:none-of-circular-list
    (take-while! never rounder)
    1 (null?)))


(suite span ((rounder (circular-list 'sakai 'samoan 'sanskrit 'santali)))

  (test span:empty-list
    (span always '())
    2 (null? null?))

  (test span:non-satisfying-at-front
    (span never '(sassak savara scots selung semang))
    2 (list? list?)
    (lambda (fore aft)
      (and (null? fore)
           (equal? aft '(sassak savara scots selung semang)))))

  (test span:continue-to-middle
    (span even? '(630 632 634 636 637 638))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(630 632 634 636))
           (equal? aft '(637 638)))))

  (test span:all-satisfying
    (span always '(shan shilha shina))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(shan shilha shina))
           (null? aft))))

  (test span:some-of-circular-list
    (span (lambda (element)
            (not (eq? element 'sanskrit)))
          rounder)
    2 (list? circular-list?)
    (lambda (fore aft)
      (and (equal? fore '(sakai samoan))
           (eq? aft (cddr rounder)))))

  (test span:none-of-circular-list
    (span never rounder)
    2 (list? circular-list)
    (lambda (fore aft)
      (and (null? fore) (eq? aft rounder)))))


(suite break ((rounder (circular-list 'siamese 'sindhi 'singhalese 'slavic
                                      'slovak)))

  (test break:empty-list
    (break never '())
    2 (null? null?))

  (test break:satisfying-at-front
    (break always '(slovenian sorbian soyot))
    2 (list? list?)
    (lambda (fore aft)
      (and (null? fore)
           (equal? aft '(slovenian sorbian soyot)))))

  (test break:continue-to-middle
    (break even? '(639 641 642 643 644 645))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(639 641))
           (equal? aft '(642 643 644 645)))))

  (test break:none-satisfying
    (break never '(spanish sudanese swahili swedish))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(spanish sudanese swahili swedish))
           (null? aft))))

  (test break:some-of-circular-list
    (break (section eq? <> 'slovak) rounder)
    2 (list? circular-list?)
    (lambda (fore aft)
      (and (equal? fore '(siamese sindhi singhalese slavic))
           (eq? aft (cddddr rounder)))))

  (test break:none-of-circular-list
    (break always rounder)
    2 (list? circular-list)
    (lambda (fore aft)
      (and (null? fore) (eq? aft rounder)))))
    

(suite span! ()

  (test span!:empty-list
    (span! always (list))
    2 (null? null?))

  (test span!:non-satisfying-at-front
    (span! never (list 'syriac 'syryenian 'tagala 'tagalog 'tagula))
    2 (list? list?)
    (lambda (fore aft)
      (and (null? fore)
           (equal? aft '(syriac syryenian tagala tagalog tagula)))))

  (test span!:continue-to-middle
    (span! even? (list 646 648 649 650))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(646 648))
           (equal? aft '(649 650)))))

  (test span!:all-satisfying
    (span! always (list 'tahitian 'tamashek 'tamil 'tartar))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(tahitian tamashek tamil tartar))
           (null? aft)))))


(suite break! ()

  (test break!:empty-list
    (break! never (list))
    2 (null? null?))

  (test break!:satisfying-at-front
    (break! always (list 'tavghi 'teleut 'telugu 'tibetan 'tino))
    2 (list? list?)
    (lambda (fore aft)
      (and (null? fore)
           (equal? aft '(tavghi teleut telugu tibetan tino)))))

  (test break!:continue-to-middle
    (break! even? '(651 653 655 656 657))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(651 653 655))
           (equal? aft '(656 657)))))

  (test break!:none-satisfying
    (break! never (list 'tipura 'toda 'tokelau 'tongan 'tuamotuan 'tulu 'tungus))
    2 (list? list?)
    (lambda (fore aft)
      (and (equal? fore '(tipura toda tokelau tongan tuamotuan tulu tungus))
           (null? aft)))))


(suite delete ()

  (test delete:null-list
    (delete '(Fraser . Frederic) '())
    1 (null?))

  (test delete:in-singleton-list
    (delete '(Fredericksburg . Frederika) '((Fredericksburg . Frederika)))
    1 (null?))

  (test delete:not-in-singleton-list
    (delete '(Fredonia . Fredsville) '((Freeman . Freeport)))
    1 (list?)
    (section equal? <> '((Freeman . Freeport))))

  (test delete:at-beginning-of-longer-list
    (delete '(Fremont . Froelich) '((Fremont . Froelich)
                                    (Fruitland . Fulton)
                                    (Furay . Galbraith)
                                    (Galesburg . Galland)
                                    (Galt . Galva)))
    1 (list?)
    (section equal? <> '((Fruitland . Fulton)
                         (Furay . Galbraith)
                         (Galesburg . Galland)
                         (Galt . Galva))))

  (test delete:in-middle-of-longer-list
    (delete '(Gambrill . Garber) '((Gardiner . Gardner)
                                   (Garfield . Garland)
                                   (Garnavillo . Garner)
                                   (Garrison . Garwin)
                                   (Gambrill . Garber)
                                   (Gaza . Geneva)
                                   (Genoa . George)))
    1 (list?)
    (section equal? <> '((Gardiner . Gardner)
                         (Garfield . Garland)
                         (Garnavillo . Garner)
                         (Garrison . Garwin)
                         (Gaza . Geneva)
                         (Genoa . George))))

  (test delete:at-end-of-longer-list
    (delete '(Georgetown . Gerled) '((Germantown . Germanville)
                                     (Giard . Gibbsville)
                                     (Gibson . Gifford)
                                     (Gilbert . Gilbertville)
                                     (Georgetown . Gerled)))
    1 (list?)
    (section equal? <> '((Germantown . Germanville)
                         (Giard . Gibbsville)
                         (Gibson . Gifford)
                         (Gilbert . Gilbertville))))

  (test delete:not-in-longer-list
    (delete '(Gilliatt . Gilman) '((Givin . Gladbrook)
                                   (Gladstone . Gladwin)
                                   (Glasgow . Glendon)
                                   (Glenwood . Glidden)
                                   (Goddard . Goldfield)))
    1 (list?)
    (section equal? <> '((Givin . Gladbrook)
                         (Gladstone . Gladwin)
                         (Glasgow . Glendon)
                         (Glenwood . Glidden)
                         (Goddard . Goldfield))))

  (test delete:several-matches-in-longer-list
    (delete '(Goodell . Gosport) '((Gowrie . Goddard)
                                   (Grable . Graettinger)
                                   (Goodell . Gosport)
                                   (Graf . Grafton)
                                   (Goodell . Gosport)
                                   (Grandview . Granger)
                                   (Goodell . Gosport)))
    1 (list?)
    (section equal? <> '((Gowrie . Goddard)
                         (Grable . Graettinger)
                         (Graf . Grafton)
                         (Grandview . Granger))))

  (test delete:with-comparison-predicate
    (delete 658 '(659 661 663 665 666 667) same-parity?)
    1 (list?)
    (section equal? <> '(659 661 663 665 667)))

  (test delete:several-matches-with-comparison-predicate
    (delete 668 '(669 670 671 672 673 674 675) same-parity?)
    1 (list?)
    (section equal? <> '(669 671 673 675)))

  (test delete:with-comparison-procedure
    (delete 679 '(680 676 678 681 677) <)
    1 (list?)
    (section equal? <> '(676 678 677))))


(suite delete! ()

  (test delete!:null-list
    (delete! (cons 'Henshaw 'Hentons) (list))
    1 (null?))

  (test delete!:in-singleton-list
    (delete! (cons 'Hepburn 'Herndon)
             (list (cons 'Hepburn 'Herndon)))
    1 (null?))

  (test delete!:not-in-singleton-list
    (delete! (cons 'Hesper 'Hiattsville)
             (list (cons 'Hiawatha 'Hicks)))
    1 (list?)
    (section equal? <> '((Hiawatha . Hicks))))

  (test delete!:at-beginning-of-longer-list
    (delete! (cons 'Highland 'Highlandville)
             (list (cons 'Highland 'Highlandville)
                   (cons 'Highview 'Hills)
                   (cons 'Hillsboro 'Hillsdale)
                   (cons 'Hilltop 'Hinton)
                   (cons 'Hiteman 'Hobarton)))
    1 (list?)
    (section equal? <> '((Highview . Hills)
                         (Hillsboro . Hillsdale)
                         (Hilltop . Hinton)
                         (Hiteman . Hobarton))))

  (test delete!:in-middle-of-longer-list
    (delete! (cons 'Hocking 'Holbrook)
             (list (cons 'Holland 'Holmes)
                   (cons 'Holstein 'Homer)
                   (cons 'Homestead 'Hopeville)
                   (cons 'Hopkinton 'Hornick)
                   (cons 'Hocking 'Holbrook)
                   (cons 'Horton 'Hospers)
                   (cons 'Houghton 'Howardville)))
    1 (list?)
    (section equal? <> '((Holland . Holmes)
                         (Holstein . Homer)
                         (Homestead . Hopeville)
                         (Hopkinton . Hornick)
                         (Horton . Hospers)
                         (Houghton . Howardville))))

  (test delete!:at-end-of-longer-list
    (delete! (cons 'Howe 'Hubbard)
             (list (cons 'Hudson 'Hugo)
                   (cons 'Hull 'Humboldt)
                   (cons 'Humeston 'Huntington)
                   (cons 'Hurley 'Huron)
                   (cons 'Howe 'Hubbard)))
    1 (list?)
    (section equal? <> '((Hudson . Hugo)
                         (Hull . Humboldt)
                         (Humeston . Huntington)
                         (Hurley . Huron))))

  (test delete!:not-in-longer-list
    (delete! (cons 'Hurstville 'Hutchins)
             (list (cons 'Huxley 'Iconium)
                   (cons 'Illyria 'Imogene)
                   (cons 'Independence 'Indianapolis)
                   (cons 'Indianola 'Industry)
                   (cons 'Inwood 'Ion)))
    1 (list?)
    (section equal? <> '((Huxley . Iconium)
                         (Illyria . Imogene)
                         (Independence . Indianapolis)
                         (Indianola . Industry)
                         (Inwood . Ion))))

  (test delete!:several-matches-in-longer-list
    (delete! (cons 'Ionia 'Ira)
             (list (cons 'Ireton 'Ironhills)
                   (cons 'Irving 'Irvington)
                   (cons 'Ionia 'Ira)
                   (cons 'Irwin 'Ivester)
                   (cons 'Ionia 'Ira)
                   (cons 'Iveyville 'Ivy)
                   (cons 'Ionia 'Ira)))
    1 (list?)
    (section equal? <> '((Ireton . Ironhills)
                         (Irving . Irvington)
                         (Irwin . Ivester)
                         (Iveyville . Ivy))))

  (test delete!:with-comparison-predicate
    (delete! 682 (list 683 685 686 687 689) same-parity?)
    1 (list?)
    (section equal? <> '(683 685 687 689)))

  (test delete!:several-matches-with-comparison-predicate
    (delete! 690 (list 691 692 693 694 695 696 697 698) same-parity?)
    1 (list?)
    (section equal? <> '(691 693 695 697)))

  (test delete!:with-comparison-procedure
    (delete! 700 (list 698 701 699 702) <)
    1 (list?)
    (section equal? <> '(698 699))))


(suite alist-cons ()

  (test alist-cons:to-empty-list
    (alist-cons 'turkoman 'uigur '())
    1 (list?)
    (section equal? <> '((turkoman . uigur))))

  (test alist-cons:to-existing-alist
    (alist-cons 'ukrainian 'urdu '((uzbek . veps)
                                   (visayan . vote)
                                   (wa . welsh)))
    1 (list?)
    (section equal? <> '((ukrainian . urdu)
                         (uzbek . veps)
                         (visayan . vote)
                         (wa . welsh)))))


(suite alist-copy ()

  (test alist-copy:null-list
    (alist-copy '())
    1 (null?))

  (let ((original '((Maxon . Maxwell)
                    (Maynard . Maysville)
                    (McCallsburg . McCausland)
                    (McClelland . McGregor)
                    (McIntire . McNally))))
    (test alist-copy:flat-list
      (alist-copy original)
      1 (list?)
      (lambda (result)
        (and (equal? result original)
             (not (eq? result original))
             (not (eq? (car result) (car original)))
             (not (eq? (cdr result) (cdr original)))
             (not (eq? (cadr result) (cadr original)))
             (not (eq? (cddr result) (cddr original)))
             (not (eq? (caddr result) (caddr original)))
             (not (eq? (cdddr result) (cdddr original)))
             (not (eq? (cadddr result) (cadddr original)))
             (not (eq? (cddddr result) (cddddr original)))
             (not (eq? (car (cddddr result))
                       (car (cddddr original))))))))

  (let ((first '(McPaul))
        (second '(McPherson Mechanicsville Mederville
                 (Mediapolis Medora)
                 ((Mekee Melbourne Melcher))))
        (third 'Melrose))
    (let ((original (list (cons 'Meltonville first)
                          (cons 'Melvin second)
                          (cons 'Menlo third))))
      (test alist-copy:bush
        (alist-copy original)
        1 (list?)
        (lambda (result)
          (and (equal? result original)
               (not (eq? result original))
               (not (eq? (car result) (car original)))
               (eq? (cdar result) first)
               (not (eq? (cdr result) (cdr original)))
               (not (eq? (cadr result) (cadr original)))
               (eq? (cdadr result) second)
               (not (eq? (cddr result) (cddr original)))
               (not (eq? (caddr result) (caddr original)))
               (eq? (cdaddr result) third)))))))


(suite delete-duplicates ()

  (test delete-duplicates:null-list
    (delete-duplicates '())
    1 (null?))

  (test delete-duplicates:singleton-list
    (delete-duplicates '((Knierim . Knittel)))
    1 (list?)
    (section equal? <> '((Knierim . Knittel))))

  (test delete-duplicates:in-doubleton-list
    (delete-duplicates '((Knoke . Knowlton) (Knoke . Knowlton)))
    1 (list?)
    (section equal? <> '((Knoke . Knowlton))))

  (test delete-duplicates:none-removed-in-longer-list
    (delete-duplicates '((Knox . Knoxville)
                         (Konigsmark . Kossuth)
                         (Koszta . Lacelle)
                         (Lacey . Lacona)
                         (Ladoga . Ladora)))
    1 (list?)
    (section equal? <> '((Knox . Knoxville)
                         (Konigsmark . Kossuth)
                         (Koszta . Lacelle)
                         (Lacey . Lacona)
                         (Ladoga . Ladora))))

  (test delete-duplicates:some-removed-in-longer-list
    (delete-duplicates '((Lafayette . Lainsville)
                         (Lakeside . Lakewood)
                         (Lakeside . Lakewood)
                         (Lakonta . Lakota)
                         (Lafayette . Lainsville)
                         (Lamoille . Lamoni)
                         (Lakeside . Lakewood)
                         (Lamont . Lancaster)
                         (Lakeside . Lakewood)))
    1 (list?)
    (section equal? <> '((Lafayette . Lainsville)
                         (Lakeside . Lakewood)
                         (Lakonta . Lakota)
                         (Lamoille . Lamoni)
                         (Lamont . Lancaster))))

  (test delete-duplicates:all-but-one-removed-in-longer-list
    (delete-duplicates '((Lanesboro . Langdon)
                         (Lanesboro . Langdon)
                         (Lanesboro . Langdon)
                         (Lanesboro . Langdon)
                         (Lanesboro . Langdon)))
    1 (list?)
    (section equal? <> '((Lanesboro . Langdon))))

  (test delete-duplicates:in-doubleton-list-with-comparison-predicate
    (delete-duplicates '(Jamaica James) always)
    1 (list?)
    (section equal? <> '(Jamaica)))

  (test delete-duplicates:none-removed-in-longer-list-with-comparison-predicate
    (delete-duplicates '(Jamestown Jamison Janesville Jefferson Jerome) never)
    1 (list?)
    (section equal? <> '(Jamestown Jamison Janesville Jefferson Jerome)))

  (test delete-duplicates:some-removed-in-longer-list-with-comparison-predicate
    (delete-duplicates '(501 502 503 504 508 510 511)
                       (lambda (x y) (= (+ x y) 1011)))
    1 (list?)
    (section equal? <> '(501 502 503 504 511)))

  (test delete-duplicates:all-but-one-removed-in-longer-list-with-comparison-predicate
    (delete-duplicates '(Jesup Jewell Johnston Joice Jolley) always)
    1 (list?)
    (section equal? <> '(Jesup))))


(suite delete-duplicates! ()

  (test delete-duplicates!:null-list
    (delete-duplicates! (list) always)
    1 (null?))

  (test delete-duplicates!:singleton-list
    (delete-duplicates! (list (cons 'Lester 'Letts)))
    1 (list?)
    (section equal? <> '((Lester . Letts))))

  (test delete-duplicates!:in-doubleton-list
    (delete-duplicates! (list (cons 'Leverette 'Levey)
                              (cons 'Leverette 'Levey)))
    1 (list?)
    (section equal? <> '((Leverette . Levey))))

  (test delete-duplicates!:none-removed-in-longer-list
    (delete-duplicates! (list (cons 'Lewis 'Lexington)
                              (cons 'Liberty 'Libertyville)
                              (cons 'Lidderdale 'Lima)
                              (cons 'Linby 'Lincoln)
                            (cons 'Linden 'Lineville)))
    1 (list?)
    (section equal? <> '((Lewis . Lexington)
                         (Liberty . Libertyville)
                         (Lidderdale . Lima)
                         (Linby . Lincoln)
                         (Linden . Lineville))))

  (test delete-duplicates!:some-removed-in-longer-list
    (delete-duplicates! (list (cons 'Lisbon 'Liscomb)
                              (cons 'Littleport 'Littleton)
                              (cons 'Littleport 'Littleton)
                              (cons 'Livermore 'Livingston)
                              (cons 'Lisbon 'Liscomb)
                              (cons 'Lockman 'Lockridge)
                              (cons 'Littleport 'Littleton)
                              (cons 'Locust 'Logan)
                              (cons 'Littleport 'Littleton)))
    1 (list?)
    (section equal? <> '((Lisbon . Liscomb)
                         (Littleport . Littleton)
                         (Livermore . Livingston)
                         (Lockman . Lockridge)
                         (Locust . Logan))))

  (test delete-duplicates!:all-but-one-removed-in-longer-list
    (delete-duplicates! (list (cons 'Logansport 'Lohrville)
                              (cons 'Logansport 'Lohrville)
                              (cons 'Logansport 'Lohrville)
                              (cons 'Logansport 'Lohrville)
                              (cons 'Logansport 'Lohrville)))
    1 (list?)
    (section equal? <> '((Logansport . Lohrville))))

  (test delete-duplicates!:in-doubleton-list-with-comparison-predicate
    (delete-duplicates! (list 'Jubilee 'Judd) always)
    1 (list?)
    (section equal? <> '(Jubilee)))

  (test delete-duplicates!:none-removed-in-longer-list-with-comparison-predicate
    (delete-duplicates! (list 'Julien 'Juniata 'Kalo 'Kalona 'Kamrar) never)
    1 (list?)
    (section equal? <> '(Julien Juniata Kalo Kalona Kamrar)))

  (test delete-duplicates!:some-removed-in-longer-list-with-comparison-predicate
    (delete-duplicates! (list 511 512 513 514 518 520 521)
                        (lambda (x y) (= (+ x y) 1031)))
    1 (list?)
    (section equal? <> '(511 512 513 514 521)))

  (test delete-duplicates!:all-but-one-removed-in-longer-list-with-comparison-predicate
    (delete-duplicates! (list 'Kanawha 'Kellerton 'Kelley 'Kellogg 'Kendallville)
                        always)
    1 (list?)
    (section equal? <> '(Kanawha))))


(suite alist-delete ()

  (test alist-delete:null-list
    (alist-delete '(Mercer . Milford) '())
    1 (null?))

  (test alist-delete:singleton-list
    (alist-delete '(Meriden . Millersburg) '(((Meriden . Millersburg) . Merrimac)))
    1 (null?))

  (test alist-delete:all-elements-removed
    (alist-delete '(Meservey . Metz)
                  '(((Meservey . Metz) . Meyer)
                    ((Meservey . Metz) . Middletown)
                    ((Meservey . Metz) . Midway)
                    ((Meservey . Metz) . Milford)
                    ((Meservey . Metz) . Millersburg)))
    1 (null?))

  (test alist-delete:some-elements-removed
    (alist-delete '(yenisei . yiddish)
                  '(((yurak . zenaga) . zulu)
                    ((yenisei . yiddish) . acrilan)
                    ((alpaca . aralac) . arras)
                    ((yenisei . yiddish) . astrakhan)
                    ((yenisei . yiddish) . avisco)
                    ((axminster . baize) . balbriggan)))
    1 (list?)
    (section equal? <> '(((yurak . zenaga) . zulu)
                         ((alpaca . aralac) . arras)
                         ((axminster . baize) . balbriggan))))

  (test alist-delete:no-elements-removed
    (alist-delete '(Millerton . batik)
                  '(((Millman . Millnerville) . batiste)
                    ((Millville . Milo) . broadcloth)
                    ((Milton . Minburn) . broadloom)
                    ((Minden . Mineola) . brocade)
                    ((Minerva . Mingo) . brocatel)))
    1 (list?)
    (section equal? <> '(((Millman . Millnerville) . batiste)
                         ((Millville . Milo) . broadcloth)
                         ((Milton . Minburn) . broadloom)
                         ((Minden . Mineola) . brocade)
                         ((Minerva . Mingo) . brocatel))))

  (test alist-delete:all-elements-removed-with-comparison-predicate
    (alist-delete 'Motor '((Moulton . Moville)
                           (Munterville . Murray)
                           (Muscatine . Mystic)
                           (Napier . Nashua)
                           (Nashville . National))
                  always)
    1 (null?))

  (test alist-delete:some-elements-removed-with-comparison-predicate
    (alist-delete 561 '((562 . 563)
                        (565 . 564)
                        (566 . 567)
                        (569 . 568)
                        (570 . 571))
                  (lambda (x y) (odd? (+ x y))))
    1 (list?)
    (section equal? <> '((565 . 564) (569 . 568))))

  (test alist-delete:no-elements-removed-with-comparison-predicate
    (alist-delete 'Nemaha '((Nemaha . Neptune)
                            (Nemaha . Nevinville)
                            (Nemaha . Newburg)
                            (Nemaha . Newhall)
                            (Nemaha . Newport))
                  never)
    1 (list?)
    (section equal? <> '((Nemaha . Neptune)
                         (Nemaha . Nevinville)
                         (Nemaha . Newburg)
                         (Nemaha . Newhall)
                         (Nemaha . Newport))))

  (test alist-delete:with-comparison-procedure
    (alist-delete 587 '((583 . 584) (589 . 590) (585 . 586) (588 . 591)) <)
    1 (list?)
    (section equal? <> '((583 . 584) (585 . 586)))))


(suite alist-delete! ()

  (test alist-delete!:null-list
    (alist-delete! (cons 'Mitchell 'Noble) (list))
    1 (null?))

  (test alist-delete!:singleton-list
    (alist-delete! (cons 'Merrill 'Miles)
                   (list (cons 'Mitchellville 'Nodaway)))
    1 (list?)
    (section equal? <> '((Mitchellville . Nodaway))))

  (test alist-delete!:all-elements-removed
    (alist-delete! (cons 'Modale 'Moingona)
                   (list (cons (cons 'Modale 'Moingona) 'Moneta)
                         (cons (cons 'Modale 'Moingona) 'Monmouth)
                         (cons (cons 'Modale 'Moingona) 'Monroe)
                         (cons (cons 'Modale 'Moingona) 'Monterey)
                         (cons (cons 'Modale 'Moingona) 'Montgomery)))
    1 (null?))

  (test alist-delete!:some-elements-removed
    (alist-delete! (cons 'Mona 'Mondamin)
                   (list (cons (cons 'Mona 'Mondamin) 'Nodaway)
                         (cons (cons 'Norness 'Northboro) 'Northfield)
                         (cons (cons 'Mona 'Mondamin) 'Norwalk)
                         (cons (cons 'Mona 'Mondamin) 'Norwich)
                         (cons (cons 'Norway 'Norwood) 'Norwoodville)))
    1 (list?)
    (section equal? <> '(((Norness . Northboro) . Northfield)
                         ((Norway . Norwood) . Norwoodville))))

  (test alist-delete!:no-elements-removed
    (alist-delete! (cons 'Moningers 'Monti)
                   (list (cons (cons 'Monticello 'Montour) 'Monmouth)
                         (cons (cons 'Montpelier 'Montrose) 'Monona)
                         (cons (cons 'Mooar 'Moorhead) 'Monteith)
                         (cons (cons 'Moorland 'Moran) 'Montezuma)
                         (cons (cons 'Moravia 'Morley) 'Northwood)))
    1 (list?)
    (section equal? <> '(((Monticello . Montour) . Monmouth)
                         ((Montpelier . Montrose) . Monona)
                         ((Mooar . Moorhead) . Monteith)
                         ((Moorland . Moran) . Montezuma)
                         ((Moravia . Morley) . Northwood))))

  (test alist-delete!:all-elements-removed-with-comparison-predicate
    (alist-delete! 'Olmitz (list (cons 'Oneida 'Onslow)
                                 (cons 'Ontario 'Oralabor)
                                 (cons 'Oran 'Orange)
                                 (cons 'Orchard 'Orient))
                   always)
    1 (null?))

  (test alist-delete!:some-elements-removed-with-comparison-predicate
    (alist-delete! 572 (list (cons 573 574)
                             (cons 576 575)
                             (cons 577 578)
                             (cons 580 579)
                             (cons 581 582))
                   (lambda (x y) (even? (+ x y))))
    1 (list?)
    (section equal? <> '((573 . 574) (577 . 578) (581 . 582))))

  (test alist-delete!:no-elements-removed-with-comparison-predicate
    (alist-delete! 'Numa  (list (cons 'Nyman 'Oakdale)
                                (cons 'Oakley 'Oakville)
                                (cons 'Oakwood 'Oasis)
                                (cons 'Ocheyedan 'Odebolt)
                                (cons 'Oelwein 'Ogden))
                   never)
    1 (list?)
    (section equal? <> '((Nyman . Oakdale)
                         (Oakley . Oakville)
                         (Oakwood . Oasis)
                         (Ocheyedan . Odebolt)
                         (Oelwein . Ogden))))

  (test alist-delete!:with-comparison-procedure
    (alist-delete! 596 (list (cons 592 593)
                             (cons 597 598)
                             (cons 599 600)
                             (cons 594 595))
                   <)
    1 (list?)
    (section equal? <> '((592 . 593) (594 . 595)))))


(suite reverse! ()

  (test reverse!:empty-list
    (reverse! (list))
    1 (null?))

  (test reverse!:singleton-list
    (reverse! (list 'Brunei))
    1 (list?)
    (section equal? <> '(Brunei)))

  (test reverse!:longer-list
    (reverse! (list 'Bulgaria 'Burundi 'Cambodia 'Cameroon 'Canada))
    1 (list?)
    (section equal? <> '(Canada Cameroon Cambodia Burundi Bulgaria))))


(suite lset<= ()

  (test lset<=:no-sets
    (lset<= eq?)
    1 (true?))

  (test lset<=:one-set
    (lset<= eq? '(bunting burlap))
    1 (true?))

  (test lset<=:two-disjoint-sets
    (lset<= eq? '(byssus calico) '(cambric))
    1 (false?))

  (test lset<=:two-overlapping-sets
    (lset<= eq? '(canvas casheen) '(cashmere canvas))
    1 (false?))

  (test lset<=:simple-subset
    (lset<= eq? '(cassmere castor cassmere) '(castor celanese cassmere))
    1 (true?))

  (test lset<=:simple-superset
    (lset<= eq? '(challis cheesecloth chenille) '(cheesecloth))
    1 (false?))

  (test lset<=:equal-as-sets
    (lset<= eq? '(chiffon chinchilla chintz chiffon) '(chintz chiffon chinchilla))
    1 (true?))

  (test lset<=:multiple-subsets
    (lset<= eq? '(corduroy)
                '(cotton corduroy cotton)
                '(cotton crepe crepe corduroy)
                '(cretonne crepe cotton corduroy))
    1 (true?))

  (test lset<=:dropping-one
    (lset<= eq? '(dacron dacron dacron dacron dacron)
                '(dacron damask)
                '(dacron damask denim)
                '(dacron damask damask dimity)
                '(dacron damask denim dimity doeskin))
    1 (false?))

  (test lset<=:empty-sets
    (lset<= eq? '() '() '() '() '() '() '() '() '() '(drill) '(drill))
    1 (true?))

  (test lset<=:unusual-equality
    (lset<= same-parity? '(703 705 707 709) '(711 713 715 715 716 717))
    1 (true?)))


(suite lset= ()

  (test lset=:no-sets
    (lset= eq?)
    1 (true?))
  
  (test lset=:one-set
    (lset= eq? '(drugget duck duffel))
    1 (true?))

  (test lset=:empty-sets
    (lset= eq? '() '() '())
    1 (true?))

  (test lset=:two-disjoint-sets
    (lset= eq? '(dungaree) '(duvetyn dynelo faille))
    1 (false?))

  (test lset=:two-overlapping-sets
    (lset= eq? '(felt flannel) '(flannel fleece foulard))
    1 (false?))

  (test lset=:simple-subset
    (lset= eq? '(frieze frieze fustian frieze) '(frieze fustian gabardine))
    1 (false?))

  (test lset=:simple-superset
    (lset= eq? '(georgette gingham gossamer) '(gossamer georgette))
    1 (false?))

  (test lset=:equal-as-sets
    (lset= eq? '(grenadine grogram gunny grogram)
               '(gunny grogram gunny grenadine grenadine))
    1 (true?))

  (test lset=:multiple-equal-sets
    (lset= eq? '(grosgrain haircloth)
               '(haircloth haircloth grosgrain)
               '(haircloth grosgrain grosgrain haircloth grosgrain)
               '(grosgrain haircloth grosgrain))
    1 (true?))

  (test lset=:dropping-one
    (lset= eq? '(herringbone homespun horsehair)
               '(homespun horsehair herringbone horsehair)
               '(horsehair homespun homespun)
               '(herringbone herringbone horsehair homespun))
    1 (false?))

  (test lset=:unusual-equality
    (lset= same-parity? '(718) '(720 722 724) '(726 728))
    1 (true?)))


(suite lset-adjoin ()

  (test lset-adjoin:no-elements
    (lset-adjoin eq? '(huckaback jean))
    1 (list?)
    (section lset= eq? <> '(huckaback jean)))

  (test lset-adjoin:one-old-element
    (lset-adjoin eq? '(leatherette linen linoleum) 'linoleum)
    1 (list?)
    (section lset= eq? <> '(leatherette linen linoleum)))

  (test lset-adjoin:many-old-elements
    (lset-adjoin eq? '(jersey knitwear lawn) 'knitwear 'jersey 'knitwear)
    1 (list?)
    (section lset= eq? <> '(jersey knitwear lawn)))

  (test lset-adjoin:one-new-element
    (lset-adjoin eq? '(linsey-woolsey lisle longcloth luster) 'madras)
    1 (list?)
    (section lset= eq? <> '(madras linsey-woolsey lisle longcloth luster)))

  (test lset-adjoin:many-new-elements
    (lset-adjoin eq? '(malines manta) 'marquisette 'marseille 'mat 'melton)
    1 (list?)
    (section lset= eq? <> '(melton mat marseille marquisette malines manta)))

  (test lset-adjoin:mixture-of-old-and-new
    (lset-adjoin eq? '(messaline mohair moleskin) 'mohair 'mousseline 'murrey
                                                  'moleskin 'murrey 'mohair)
    1 (list?)
    (section lset= eq? <> '(murrey mousseline messaline mohair moleskin))))


(suite lset-union ()

  (test lset-union:no-sets
    (lset-union eq?)
    1 (null?))

  (test lset-union:empty-set
    (lset-union eq? '())
    1 (null?))

  (test lset-union:one-set
    (lset-union eq? '(muslin nansook))
    1 (list?)
    (section lset= eq? <> '(muslin nansook)))

  (test lset-union:two-disjoint-sets
    (lset-union eq? '(nankeen net nylon) '(oilcloth organdy))
    1 (list?)
    (section lset= eq? <> '(nankeen net nylon oilcloth organdy)))

  (test lset-union:two-overlapping-sets
    (lset-union eq? '(organza orlon paisley) '(orlon percale plaid paisley))
    1 (list?)
    (section lset= eq? <> '(organza orlon percale plaid paisley)))

  (test lset-union:nested-sets
    (lset-union eq? '(pongee poplin quilting rayon rugging) '(rayon poplin))
    1 (list?)
    (section lset= eq? <> '(pongee quilting rugging rayon poplin)))

  (test lset-union:two-identical-sets
    (lset-union eq? '(russet sacking sailcloth)
                    '(sacking russet sailcloth russet))
    1 (list?)
    (section lset= eq? <> '(sacking sailcloth russet)))

  (test lset-union:two-empty-sets
    (lset-union eq? '() '())
    1 (null?))

  (test lset-union:two-sets-one-empty
    (lset-union eq? '(samite sateen satin) '())
    1 (list?)
    (section lset= eq? <> '(samite sateen satin)))

  (test lset-union:many-disjoint-sets
    (lset-union eq? '(scrim seersucker) '(serge shantung sharkskin)
                    '(shoddy silk stamin) '(stammel stockinet suede))
    1 (list?)
    (section lset= eq? <> '(scrim seersucker serge shantung sharkskin shoddy silk
                            stamin stammel stockinet suede)))

  (test lset-union:many-overlapping-sets
    (lset-union eq? '(swansdown tabaret tabby)
                    '(taffeta swansdown tapestry tarpaulin)
                    '(tabby terry swansdown taffeta))
    1 (list?)
    (section lset= eq? <> '(tabaret tapestry tarpaulin tabby terry swansdown
                            taffeta)))

  (test lset-union:some-null-sets
    (lset-union eq? '(tick toweling tricot) '() '(tricot tulle) '() '() '(tussah))
    1 (list?)
    (section lset= eq? <> '(tick toweling tricot tulle tussah))))


(suite lset-intersection ()

  (test lset-intersection:empty-set
    (lset-intersection eq? '())
    1 (null?))

  (test lset-intersection:one-set
    (lset-intersection eq? '(tweed veiling velours))
    1 (list?)
    (section lset= eq? <> '(tweed veiling velours)))

  (test lset-intersection:two-disjoint-sets
    (lset-intersection eq? '(velure velvet) '(voile webbing wool))
    1 (null?))

  (test lset-intersection:two-overlapping-sets
    (lset-intersection eq? '(worsted acetate aldehyde) '(amine aldehyde anhydride))
    1 (list?)
    (section lset= eq? <> '(aldehyde)))

  (test lset-intersection:nested-sets
    (lset-intersection eq? '(arsenate benzoate bicarbonate bichloride)
                           '(benzoate arsenate arsenate bichloride))
    1 (list?)
    (section lset= eq? <> '(arsenate benzoate bichloride)))

  (test lset-intersection:two-identical-sets
    (lset-intersection eq? '(bisulfate borate bromide)
                           '(borate bisulfate bromide borate))
    1 (list?)
    (section lset= eq? <> '(bisulfate borate bromide)))

  (test lset-intersection:two-empty-sets
    (lset-intersection eq? '() '())
    1 (null?))

  (test lset-intersection:two-sets-one-empty
    (lset-intersection eq? '(carbide carbohydrate carbonate) '())
    1 (null?))

  (test lset-intersection:many-disjoint-sets
    (lset-intersection eq? '(chlorate chromate citrate)
                           '(cyanide dichromate)
                           '(dioxide disulfate fluoride)
                           '(fulminate halide))
    1 (null?))

  (test lset-intersection:many-overlapping-sets
    (lset-intersection eq? '(hydrate hydride hydroxide iodide)
                           '(hydride iodide lactate monoxide)
                           '(iodide monoxide nitrate oxalate))
    1 (list?)
    (section lset= eq? <> '(iodide)))

  (test lset-intersection:some-null-sets
    (lset-intersection eq? '(oxide permanganate) '() '(oxide peroxide) '() '())
    1 (null?)))


(suite lset-difference ()

  (test lset-difference:empty-set
    (lset-difference eq? '())
    1 (null?))

  (test lset-difference:one-set
    (lset-difference eq? '(phosphate silicate))
    1 (list?)
    (section lset= eq? <> '(phosphate silicate)))

  (test lset-difference:two-disjoint-sets
    (lset-difference eq? '(sulfate tartrate tetroxide) '(thiosulfate trioxide))
    1 (list?)
    (section lset= eq? <> '(sulfate tartrate tetroxide)))

  (test lset-difference:two-overlapping-sets
    (lset-difference eq? '(alabaster amphibole alabaster apatite)
                         '(aplite alabaster apatite))
    1 (list?)
    (section lset= eq? <> '(amphibole)))

  (test lset-difference:nested-sets
    (lset-difference eq? '(argonite asbestos azurite) '(azurite argonite))
    1 (list?)
    (section lset= eq? <> '(asbestos)))

  (test lset-difference:two-identical-sets
    (lset-difference eq? '(barite bauxite boron)
                         '(barite bauxite boron))
    1 (null?))

  (test lset-difference:two-empty-sets
    (lset-difference eq? '() '())
    1 (null?))

  (test lset-difference:two-sets-first-empty
    (lset-difference eq? '() '(brimstone brookite))
    1 (null?))

  (test lset-difference:two-sets-second-empty
    (lset-difference eq? '(brucite calcite carbon) '())
    1 (list?)
    (section lset= eq? <> '(brucite calcite carbon)))

  (test lset-difference:many-disjoint-sets
    (lset-difference eq? '(celestite chalcedony)
                         '(chlorite chromite clay)
                         '(coal cobalt coke corundum cryolite)
                         '(crystal diatomite))
    1 (list?)
    (section lset= eq? <> '(celestite chalcedony)))

  (test lset-difference:many-overlapping-sets
    (lset-difference eq? '(elaterite emery epidote epsomite)
                         '(elaterite emery feldspar fluorite)
                         '(elaterite epidote feldspar fluorospar))
    1 (list?)
    (section lset= eq? <> '(epsomite)))

  (test lset-difference:some-null-sets
    (lset-difference eq? '(garnet glauconite graphite) '() '(garnet gypsum) '()
                         '() '(hatchettite holosiderite))
    1 (list?)
    (section lset= eq? <> '(glauconite graphite))))


(suite lset-xor ()

  (test lset-xor:no-sets
    (lset-xor eq?)
    1 (null?))

  (test lset-xor:empty-set
    (lset-xor eq? '())
    1 (null?))

  (test lset-xor:one-set
    (lset-xor eq? '(hornblende ilmenite iolite))
    1 (list?)
    (section lset= eq? <> '(hornblende ilmenite iolite)))

  (test lset-xor:two-disjoint-sets
    (lset-xor eq? '(jet kaolinite kyanite) '(lazurite lignite))
    1 (list?)
    (section lset= eq? <> '(jet kaolinite kyanite lazurite lignite)))

  (test lset-xor:two-overlapping-sets
    (lset-xor eq? '(lime magnesite malachite) '(maltha manganese magnesite))
    1 (list?)
    (section lset= eq? <> '(lime malachite maltha manganese)))

  (test lset-xor:nested-sets
    (lset-xor eq? '(marcasite marl meerschaum) '(meerschaum marcasite))
    1 (list?)
    (section lset= eq? <> '(marl)))

  (test lset-xor:two-identical-sets
    (lset-xor eq? '(mica molybdenite) '(molybdenite mica))
    1 (null?))

  (test lset-xor:two-empty-sets
    (lset-xor eq? '() '())
    1 (null?))

  (test lset-xor:two-sets-one-empty
    (lset-xor eq? '(monzite obsidian olivine) '())
    1 (list?)
    (section lset= eq? <> '(monzite obsidian olivine)))

  (test lset-xor:many-disjoint-sets
    (lset-xor eq? '(orthoclase ozocerite peat) '(perlite pitchblende)
                  '(pumice pyrite pyroxene quartz) '(quicklime realgar))
    1 (list?)
    (section lset= eq? <> '(orthoclase ozocerite peat perlite pitchblende pumice
                             pyrite pyroxene quartz quicklime realgar)))

  (test lset-xor:many-overlapping-sets
    (lset-xor eq? '(rhodonite rutile selenite siderite)
                  '(rhodonite rutile silica spinel)
                  '(rhodonite selenite silica spodumene))
    1 (list?)
    (section lset= eq? <> '(rhodonite siderite spinel spodumene)))

  (test lset-xor:some-null-sets
    (lset-xor eq? '(talc tourmaline) '() '() '(tourmaline tripoli) '())
    1 (list?)
    (section lset= eq? <> '(talc tripoli))))


(suite lset-diff+intersection ()

  (test lset-diff+intersection:empty-set
    (lset-diff+intersection eq? '())
    2 (null? null?))

  (test lset-diff+intersection:one-set
    (lset-diff+intersection eq? '(vermiculite wollastonite wulfenite))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(vermiculite wollastonite wulfenite))
           (null? int))))

  (test lset-diff+intersection:two-disjoint-sets
    (lset-diff+intersection eq? '(albose altrose) '(arbinose collobiose))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(albose altrose))
           (null? int))))

  (test lset-diff+intersection:two-overlapping-sets
    (lset-diff+intersection eq? '(cellulose dextrin dextrose)
                                '(erythrose fructose dextrin))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(cellulose dextrose))
           (lset= eq? int '(dextrin)))))

  (test lset-diff+intersection:nested-sets
    (lset-diff+intersection eq? '(galactose glucose gulose idose inulin)
                                '(inulin glucose idose))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(galactose gulose))
           (lset= eq? int '(glucose idose inulin)))))

  (test lset-diff+intersection:two-identical-sets
    (lset-diff+intersection eq? '(lactose lactose levulose)
                                '(levulose lactose levulose))
    2 (list? list?)
    (lambda (diff int)
      (and (null? diff)
           (lset= eq? int '(lactose levulose)))))

  (test lset-diff+intersection:two-empty-sets
    (lset-diff+intersection eq? '() '())
    2 (null? null?))

  (test lset-diff+intersection:two-sets-first-empty
    (lset-diff+intersection eq? '() '(lyzose maltose mannose))
    2 (null? null?))

  (test lset-diff+intersection:two-sets-second-empty
    (lset-diff+intersection eq? '(melibiose pentosan) '())
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(melibiose pentosan))
           (null? int))))

  (test lset-diff+intersection:many-disjoint-sets
    (lset-diff+intersection eq? '(raffinose ribose saccharose)
                                '(sorbose starch sucrose tagarose)
                                '(talose)
                                '(trehalose xylose allspice)
                                '(capsicum))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(raffinose ribose saccharose))
           (null? int))))

  (test lset-diff+intersection:many-overlapping-sets
    (lset-diff+intersection eq? '(cardamon cayenne chili chive)
                                '(cardamon cayenne chutney dill)
                                '(cardamon chili chutney garlic))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(chive))
           (lset= eq? int '(cardamon cayenne chili)))))

  (test lset-diff+intersection:some-null-sets
    (lset-diff+intersection eq? '(ginger horseradish cinnamon clove) '()
                                '(cinnamon horseradish) '() '() '(cinnamon))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(ginger clove))
           (lset= eq? int '(horseradish cinnamon))))))


(suite lset-union! ()

  (test lset-union!:no-sets
    (lset-union! eq?)
    1 (null?))

  (test lset-union!:empty-set
    (lset-union! eq? (list))
    1 (null?))

  (test lset-union!:one-set
    (lset-union! eq? (list 'cubeb 'leek 'mace))
    1 (list?)
    (section lset= eq? <> '(cubeb leek mace)))

  (test lset-union!:two-disjoint-sets
    (lset-union! eq? (list 'marjoram 'mayonnaise) (list 'mint 'mustard 'nutmeg))
    1 (list?)
    (section lset= eq? <> '(marjoram mayonnaise mint mustard nutmeg)))

  (test lset-union!:two-overlapping-sets
    (lset-union! eq? (list 'onion 'paprika 'parsley 'pepper)
                     (list 'paprika 'piccalilli 'onion 'pickle 'pepper))
    1 (list?)
    (section lset= eq? <> '(parsley paprika piccalilli onion pickle pepper)))

  (test lset-union!:nested-sets
    (lset-union! eq? (list 'pimiento 'pimpernel 'potherb 'radish)
                     (list 'pimpernel 'radish 'pimpernel))
    1 (list?)
    (section lset= eq? <> '(pimiento potherb radish pimpernel)))

  (test lset-union!:two-identical-sets
    (lset-union! eq? (list 'relish 'sage 'salt) (list 'relish 'sage 'salt))
    1 (list?)
    (section lset= eq? <> '(relish sage salt)))

  (test lset-union!:two-empty-sets
    (lset-union! eq? (list) (list))
    1 (null?))

  (test lset-union!:two-sets-one-empty
    (lset-union! eq? '(savory shallot) (list))
    1 (list?)
    (section lset= eq? <> '(savory shallot)))

  (test lset-union!:many-disjoint-sets
    (lset-union! eq? (list 'soy 'tabasco)
                     (list 'thyme)
                     (list 'turmeric 'vanilla 'vinegar)
                     (list 'abbreviation 'abstention))
    1 (list?)
    (section lset= eq? <> '(soy tabasco thyme turmeric vanilla vinegar
                             abbreviation abstention)))

  (test lset-union!:many-overlapping-sets
    (lset-union! eq? (list 'accent 'accomplishment 'accusative 'acronym)
                     (list 'accent 'accomplishment 'active 'addition)
                     (list 'accent 'accusative 'active 'addressee))
                 
    1 (list?)
    (section lset= eq? <> '(acronym accomplishment addition accent accusative
                            active addressee)))

  (test lset-union!:some-null-sets
    (lset-union! eq? (list 'adjective 'adjunct 'adposition) (list)
                     (list 'adverb) (list) (list 'adversative) (list) (list))
    1 (list?)
    (section lset= eq? <> '(adjective adjunct adposition adverb adversative))))


(suite lset-intersection! ()

  (test lset-intersection!:empty-set
    (lset-intersection! eq? (list))
    1 (null?))

  (test lset-intersection!:one-set
    (lset-intersection! eq? (list 'advice 'affirmation 'affirmative))
    1 (list?)
    (section lset= eq? <> '(advice affirmation affirmative)))

  (test lset-intersection!:two-disjoint-sets
    (lset-intersection! eq? (list 'affix 'afterthought 'age 'agent)
                            (list 'aggregate 'agrammatical))
    1 (null?))

  (test lset-intersection!:two-overlapping-sets
    (lset-intersection! eq? (list 'agreement 'alternation 'alveolar)
                            (list 'ambiclipping 'alveolar 'ambiguity))
    1 (list?)
    (section lset= eq? <> '(alveolar)))

  (test lset-intersection!:nested-sets
    (lset-intersection! eq? (list 'analysability 'anaphor 'anaphora 'anaphoric)
                            (list 'anaphor 'anaphoric))
    1 (list?)
    (section lset= eq? <> '(anaphor anaphoric)))

  (test lset-intersection!:two-identical-sets
    (lset-intersection! eq? '(anchor animacy animal answer)
                            '(animacy answer anchor animal))
    1 (list?)
    (section lset= eq? <> '(anchor animacy animal answer)))

  (test lset-intersection!:two-empty-sets
    (lset-intersection! eq? (list) (list))
    1 (null?))

  (test lset-intersection!:two-sets-one-empty
    (lset-intersection! eq? (list 'antecedent 'antepenult) (list))
    1 (null?))

  (test lset-intersection!:many-disjoint-sets
    (lset-intersection! eq? (list 'anteriority 'apodosis)
                            (list 'apoloogy 'apostrophe 'appellation)
                            (list 'appendage 'apposition))
    1 (null?))

  (test lset-intersection!:many-overlapping-sets
    (lset-intersection! eq? (list 'appositive 'approximate 'approximation 'arabic)
                            (list 'appositive 'approximate 'argument 'article)
                            (list 'appositive 'approximation 'argument
                                  'ascriptive))
    1 (list?)
    (section lset= eq? <> '(appositive)))

  (test lset-intersection!:some-null-sets
    (lset-intersection! eq? (list 'asking 'aspect 'aspectual) (list)
                            (list 'aspectuality) (list) (list))
    1 (null?)))


(suite lset-difference! ()

  (test lset-difference!:empty-set
    (lset-difference! eq? (list))
    1 (null?))

  (test lset-difference!:one-set
    (lset-difference! eq? (list 'assertion 'assertive 'assimilation))
    1 (list?)
    (section lset= eq? <> '(assertion assertive assimilation)))

  (test lset-difference!:two-disjoint-sets
    (lset-difference! eq? (list 'asterisk 'asymmetric 'asyndetic)
                          (list 'atelic 'attributive))
    1 (list?)
    (section lset= eq? <> '(asterisk asymmetric asyndetic)))

  (test lset-difference!:two-overlapping-sets
    (lset-difference! eq? (list 'augmentative 'authorial 'auxiliary)
                          (list 'avoidance 'baby 'authorial))
    1 (list?)
    (section lset= eq? <> '(augmentative auxiliary)))

  (test lset-difference!:nested-sets
    (lset-difference! eq? (list 'back-clipping 'back-formation 'backgrounded
                                'backshift 'bahuvrihi)
                          (list 'back-formation 'backshift))
    1 (list?)
    (section lset= eq? <> '(back-clipping backgrounded bahuvrihi)))

  (test lset-difference!:two-identical-sets
    (lset-difference! eq? (list 'bare 'base 'be 'benefactive)
                          (list 'benefactive 'be 'base 'bare))
    1 (null?))

  (test lset-difference!:two-empty-sets
    (lset-difference! eq? (list) (list))
    1 (null?))

  (test lset-difference!:two-sets-first-empty
    (lset-difference! eq? (list) (list 'benefit 'binary 'bipartite))
    1 (null?))

  (test lset-difference!:two-sets-second-empty
    (lset-difference! eq? (list 'bivalent 'blend) (list))
    1 (list?)
    (section lset= eq? <> '(bivalent blend)))

  (test lset-difference!:many-disjoint-sets
    (lset-difference! eq? (list 'block-quote 'body-part 'boldface)
                          (list 'borrowing)
                          (list 'bound 'boundary 'bounded 'bounding)
                          (list 'bracket 'buildings))
    1 (list?)
    (section lset= eq? <> '(block-quote body-part boldface)))

  (test lset-difference!:many-overlapping-sets
    (lset-difference! eq? (list 'calendar 'canonical 'capitalisation 'cardinal)
                          (list 'capitalisation 'cardinal 'case 'cataphora)
                          (list 'calendar 'cardinal 'cataphora 'catenative))
    1 (list?)
    (section lset= eq? <> '(canonical)))

  (test lset-difference!:some-null-sets
    (lset-difference! eq? (list 'causation 'cause 'central) (list)
                          (list) (list 'change 'causation) (list) (list))
    1 (list?)
    (section lset= eq? <> '(cause central))))


(suite lset-xor! ()

  (test lset-xor!:no-sets
    (lset-xor! eq?)
    1 (null?))

  (test lset-xor!:empty-set
    (lset-xor! eq? (list))
    1 (null?))

  (test lset-xor!:one-set
    (lset-xor! eq? (list 'character 'citation))
    1 (list?)
    (section lset= eq? <> '(character citation)))

  (test lset-xor!:two-disjoint-sets
    (lset-xor! eq? (list 'clausal 'clause 'cleft 'clipping) (list 'clitic))
    1 (list?)
    (section lset= eq? <> '(clausal clause cleft clipping clitic)))

  (test lset-xor!:two-overlapping-sets
    (lset-xor! eq? (list 'clock 'closed 'code)
                   (list 'clock 'cognate 'cognition 'code))
    1 (list?)
    (section lset= eq? <> '(closed cognate cognition)))

  (test lset-xor!:nested-sets
    (lset-xor! eq? (list 'co-indexing 'collection 'collective 'colloquial)
                   (list 'collective 'co-indexing 'collective))
    1 (list?)
    (section lset= eq? <> '(collection colloquial)))

  (test lset-xor!:two-identical-sets
    (lset-xor! eq? (list 'colon 'colour 'combining)
                   (list 'colon 'colour 'combining))
    1 (null?))

  (test lset-xor!:two-empty-sets
    (lset-xor! eq? (list) (list))
    1 (null?))

  (test lset-xor!:two-sets-one-empty
    (lset-xor! eq? (list 'comitative 'comma 'command 'commentary) (list))
    1 (list?)
    (section lset= eq? <> '(comitative comma command commentary)))

  (test lset-xor!:many-disjoint-sets
    (lset-xor! eq? (list 'commercial 'commitment)
                   (list 'common 'communication 'comparative)
                   (list 'compass 'complement))
    1 (list?)
    (section lset= eq? <> '(commercial commitment common communication
                             comparative compass complement)))

  (test lset-xor!:many-overlapping-sets
    (lset-xor! eq? (list 'complex 'compliance 'composition 'compound)
                   (list 'conative 'concealed 'composition 'compound)
                   (list 'conative 'complex 'concession 'composition))
    1 (list?)
    (section lset= eq? <> '(compliance concealed concession composition)))

  (test lset-xor!:some-null-sets
    (lset-xor! eq? '(concessive concrete condensed) (list) (list)
                   '(condensed condition) (list))
    1 (list?)
    (section lset= eq? <> '(concessive concrete condition))))


(suite lset-diff+intersection! ()

  (test lset-diff+intersection!:empty-set
    (lset-diff+intersection! eq? (list))
    2 (null? null?))

  (test lset-diff+intersection!:one-set
    (lset-diff+intersection! eq? (list 'conditional 'conjunct 'conjunction))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(conditional conjunct conjunction))
           (null? int))))

  (test lset-diff+intersection!:two-disjoint-sets
    (lset-diff+intersection! eq? (list 'connective 'consequence 'consequent)
                                 (list 'consonant 'constituent))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(connective consequence consequent))
           (null? int))))

  (test lset-diff+intersection!:two-overlapping-sets
    (lset-diff+intersection! eq? (list 'construction 'consultation)
                                 (list 'contact 'containment 'construction))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(consultation))
           (lset= eq? int '(construction)))))

  (test lset-diff+intersection!:nested-sets
    (lset-diff+intersection! eq? (list 'contest 'continuative 'contraction
                                       'contradictory)
                                 (list 'continuative 'contradictory))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(contest contraction))
           (lset= eq? int '(continuative contradictory)))))

  (test lset-diff+intersection!:two-identical-sets
    (lset-diff+intersection! eq? (list 'contrary 'contrast 'control 'controller)
                                 (list 'contrary 'control 'contrast 'controller))
    2 (list? list?)
    (lambda (diff int)
      (and (null? diff)
           (lset= eq? int '(contrary contrast control controller)))))

  (test lset-diff+intersection!:two-empty-sets
    (lset-diff+intersection! eq? (list) (list))
    2 (null? null?))

  (test lset-diff+intersection!:two-sets-first-empty
    (lset-diff+intersection! eq? (list) (list 'converse 'conversion))
    2 (null? null?))

  (test lset-diff+intersection!:two-sets-second-empty
    (lset-diff+intersection! eq? (list 'convey 'coordinate) (list))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(convey coordinate))
           (null? int))))

  (test lset-diff+intersection!:many-disjoint-sets
    (lset-diff+intersection! eq? (list 'coordination 'coordinator 'copula 'core)
                                 (list 'coreference 'corpus 'countability)
                                 (list 'counter-expectation 'counterfactuality))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(coordination coordinator copula core))
           (null? int))))

  (test lset-diff+intersection!:many-overlapping-sets
    (lset-diff+intersection! eq? (list 'counterpart 'countries 'covert 'creator
                                       'current 'dance)
                                 (list 'countries 'dangling)
                                 (list 'covert 'danish)
                                 (list 'creator 'dash 'dates)
                                 (list 'current))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(counterpart dance))
           (lset= eq? int '(countries covert creator current)))))

  (test lset-diff+intersection!:some-null-sets
    (lset-diff+intersection! eq? (list 'dative 'de-adjectival 'declarative
                                       'defective)
                                 (list)
                                 (list)
                                 (list 'definiendum 'dative 'definiens)
                                 (list)
                                 (list 'definite 'dative 'defective))
    2 (list? list?)
    (lambda (diff int)
      (and (lset= eq? diff '(de-adjectival declarative))
           (lset= eq? int '(dative defective))))))


;;; Copyright (C) 1999, 2019  John David Stone

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
