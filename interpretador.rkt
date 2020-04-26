#lang eopl

;******************************************************************************************
;;Andres Felipe Peralta Cruz 1910301
;;Juan David Varela Requejo 1910305
;;Jorge Alejandro Rojas Arias 1910354


;;;;; Lenguajes basados: JavaScript, C++, Python


;******************************************************************************************
;<programa> :=  <expresion>
;              un-programa (exp)

;<expresion> := <numero>
;               numero-lit (num)

;            := <octal>
;               octal-list (lista)

;            := <hexadecimal>
;               hexa-list (lista)

;            := <base-32>
;               base-32-list (lista)

;            := "\""<texto> "\""
;               texto-lit (txt)

;            := <identificador>
;               var-exp (id)                                       |JavaScript|

;            := (<expresion> <primitiva-binaria> <expresion>)
;               primapp-bin-exp (exp1 prim-binaria exp2)           |Notación infija C++|

;            := <primitiva-unaria> (<expresion>)
;               primapp-un-exp (prim-unaria exp)                   |Python|

;            := Si <expresion> "{" <expresion> "}"  sino "{" <expresion> "}"
;               condicional-exp (test-exp true-exp false-exp)      |C++|

;            := UnicVal <identificador> = VAL
;               unicVal-exp (id)                                   |JavaScript|

;            := const <identificador> = <expresion>
;               constVal-exp (id exp)                              |JavaScript|

;            := sea (<expresion> ";") { <expresion>; }
;               variableLocal-exp (id exp)                         ||

;            := asignar (<identificador> = <expresion>;)
;               set-exp (id exp)                                   |C++|

;            := proc (<identificador> ',') { <expresion> }
;               procedimiento-exp (ids cuero)                      |JavaScript|

;            := evaluar (<expresion> (<expresion> ","))
;               eval-proc-exp(exp exps)                            |Python|

;            := recursive eval (<expresion> (<expresion> ","))
;               R-eval-proc-exp(exp exps)                          |Python|

;            := lista [<expresion> ","]
;               list-exp (exps)                                    |Python|

;            := vacio
;               lista-vacia-exp

;            := verdadero
;               true-exp

;            := falso
;               false-exp

;            := ++ <identificador>
;               azucarSuma (id)                                     |C++|

;            := -- <identificador>
;               azucarResta (id)                                    |C++|                           |C++|

;<primitiva-binaria> :=  + (primitiva-suma)
;                    :=  ~ (primitiva-resta)
;                    :=  / (primitiva-div)
;                    :=  * (primitiva-multiplicacion)
;                    :=  % (primitiva-cociente)

;                    :=  octal+ (primitiva-suma-octal)
;                    :=  octal~ (primitiva-resta-octal)
;                    :=  octal* (primitiva-multiplicacion-octal)

;                    :=  hexa+ (primitiva-suma-hexa)
;                    :=  hexa~ (primitiva-resta-hexa)
;                    :=  hexa* (primitiva-multiplicacion-hexa)

;                    :=  32+ (primitiva-suma-32)
;                    :=  32~ (primitiva-resta-32)
;                    :=  32* (primitiva-multiplicacion-32)

;                    :=  concat (primitiva-concat)

;                    :=  < (primitiva-menor)
;                    :=  > (primitiva-mayor)
;                    :=  <= (primitiva-menor-igual)
;                    :=  >= (primitiva-mayor-igual)
;                    :=  == (primitiva-igual)
;                    :=  != (primitiva-diferente)
;                    :=  and (primitiva-and)
;                    :=  or (primitiva-or)

;<primitiva-unaria>:=  longitud (primitiva-longitud)

;                  :=  add1 (primitiva-add1)
;                  :=  sub1 (primitiva-sub1)

;                  :=  octal-add1 (primitiva-add1-octal)
;                  :=  octal-sub1 (primitiva-sub1-octal)

;                  :=  hexa-add1 (primitiva-add1-hexa)
;                  :=  hexa-sub1 (primitiva-sub1-hexa)

;                  :=  32-add1 (primitiva-add1-32)
;                  :=  32-sub1 (primitiva-sub1-32)

;                  :=  cab (primitiva-cab)
;                  :=  rest (primitiva-resto)
;                  :=  lista? (primitiva-pred-lista)
;                  :=  vacio? (primitiva-pred-vacio)

;                  :=  ! (primitiva-not)

;******************************************************************************************
;Especificación Léxica

(define lexica
'(
  (espacio
   (whitespace) skip)
  (comentario
   ("//" (arbno (not #\newline))) skip)
  (txt
   (letter (arbno (or letter digit ))) string)
  (id
   ("@" letter (arbno (or letter digit "?"))) symbol)
  (num
   (digit (arbno digit)) number)
  (num
   ("-" digit (arbno digit)) number)
  (num
   (digit (arbno digit) "." digit (arbno digit)) number)
  (num
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  )
  )

;Especificación Sintáctica (gramática)

(define gramatica
  '(
    (programa (expresion) un-programa)
    ;; expresiones*************************************************************************************************
    (expresion (num) numero-lit)
    (expresion ("x8" "["(separated-list num ",")"]") base8-num-exp)
    (expresion ("x16" "["(separated-list num ",")"]") base16-num-exp)
    (expresion ("x32" "["(separated-list num ",")"]") base32-num-exp)
    (expresion ("\"" txt "\"") texto-lit)
    (expresion (id) var-exp)
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("Si" expresion "{" expresion "}" "sino" "{" expresion "}") conditional-exp)
    (expresion ("unicVal" id "=" "VAL") unicVal-exp)   ;;Variable con unica asignación
    (expresion ("const" id "=" expresion) constVal-exp);;Definicion de constante
    (expresion ("sea" "(" (separated-list expresion ";")")""{" expresion ";" "}" ) variableLocal-exp) ;;let
    (expresion ("asignar" id "=" expresion ";")set-expresion) ;; set
    (expresion ("proc" "("(separated-list id ",")")" "{" expresion "}")procedimiento-exp)
    (expresion ("evaluar" "(" expresion "["(separated-list expresion ",")"]" ")")eval-proc-exp)
    (expresion ("recursive eval" "(" expresion  "["(separated-list expresion ",")"]" ")")R-eval-proc-exp) ;; para evaluar funciones recursivas
    ;;Listas
    (expresion ("lista["(separated-list expresion ",")"]")list-exp)
    (expresion ("vacio") lista-vacia-exp) ;; empty
    ;; booleanos
    (expresion ("verdadero") true-exp)
    (expresion ("falso") false-exp)
    ;;expresiones
    (expresion ("++" id) azucarSuma)
    (expresion ("--" id) azucarResta)
    ;; primitivas binarias******************************************************************
    ;; Listas
    (primitiva-binaria ("insert") primitiva-insertar);; cons
    (primitiva-binaria ("union") primitiva-union)    ;; append
    ;;aritmeticas base 10
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multiplicacion)
    (primitiva-binaria ("%") primitiva-cociente)
    ;;aritmeticas base 8
    (primitiva-binaria ("octal+") primitiva-suma-octal)
    (primitiva-binaria ("octal~") primitiva-resta-octal)
    (primitiva-binaria ("octal*") primitiva-multiplicacion-octal)
    ;;aritméticas base 16
    (primitiva-binaria ("hexa+") primitiva-suma-hexa)
    (primitiva-binaria ("hexa~") primitiva-resta-hexa)
    (primitiva-binaria ("hexa*") primitiva-multiplicacion-hexa)
    ;;aritméticas base 32
    (primitiva-binaria ("32+") primitiva-suma-32)
    (primitiva-binaria ("32~") primitiva-resta-32)
    (primitiva-binaria ("32*") primitiva-multiplicacion-32)
    ;;strings
    (primitiva-binaria ("concat") primitiva-concat)
    ;;boleanas
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("==") primitiva-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("and") primitiva-and)
    (primitiva-binaria ("or") primitiva-or)
    ;;primitiva-unaria************************************************************
    ;; listas
    (primitiva-unaria ("cab") primitiva-cab)           ;; car
    (primitiva-unaria ("rest") primitiva-resto)         ;; cdr
    (primitiva-unaria ("lista?") primitiva-pred-lista) ;; list?
    (primitiva-unaria ("vacio?") primitiva-pred-vacio) ;; empty? 
    ;;string
    (primitiva-unaria ("longitud") primitiva-longitud)
    ;;base10
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    ;;base 8
    (primitiva-unaria ("octal-add1") primitiva-add1-octal)
    (primitiva-unaria ("octal-sub1") primitiva-sub1-octal)
    ;;base 16
    (primitiva-unaria ("hexa-add1") primitiva-add1-hexa)
    (primitiva-unaria ("hexa-sub1") primitiva-sub1-hexa)
    ;;base 32
    (primitiva-unaria ("32-add1") primitiva-add1-32)
    (primitiva-unaria ("32-sub1") primitiva-sub1-32)
    ;;booleanas
    (primitiva-unaria ("!")primitiva-not) ;; not
    )
  )




;Construidos automáticamente:

(sllgen:make-define-datatypes lexica gramatica)

(define mostrar-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))


;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-exp body (extend-env ids args env))))))


;El Interpretador (FrontEnd + Evaluación + señal para lectura  )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp) (eval-exp exp (ambiente-inicial)))
      (else (eopl:error 'programa "Error de sintaxis"))
      )
    )
  )

; Ambiente inicial
(define ambiente-inicial
  (lambda ()
    (extend-env
     '(@x @y @z)
     '(4 2 5)
     (empty-env))))



;procedimiento que evalua las expresiones 
(define eval-exp
  (lambda (expre env)
    (cases expresion expre
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (var) (buscar-variable env var))
      (primapp-bin-exp (exp1 bin-prim exp2) (aplicar-bin-exp bin-prim exp1 exp2 env))
      (primapp-un-exp (un-prim exp) (aplicar-un-exp un-prim exp env))
      (conditional-exp (test-exp true-exp false-exp) (eval-conditional-exp test-exp true-exp false-exp env))
      
      (procedimiento-exp (ids cuerpo)(closure ids cuerpo env))
      ;cases que evalua los procedimientos
      ;proc value-exps -> proc es el procedimiento que recibe y value-exps
      ; son las expresiones que se le pasan al procedimiento para evaluar.
      (eval-proc-exp(proc value-exps) "Mucho por hacer, tener encuenta paso por parametro y por referencia");;(evaluar-procedimiento proc value-exps env))
      (R-eval-proc-exp(proc value-exps) "x2");;(evaluar-R-procedimiento proc value-exps env))
      ;;(azucarSuma (id) (aplicar-un-exp (primitiva-add1) (var-exp id) env))
      ;;(azucarResta (id) (aplicar-un-exp (primitiva-sub1) (var-exp id) env))
      (set-expresion (ids rhs-exp) "xd")
      (list-exp (exps) (evaluar-lista exps env))
      (lista-vacia-exp () '())
      (else expre)
      )
    )
  )

;;Funcion que evalua listas
(define evaluar-lista
 (lambda (exps env)
   (if(null? exps) '()
      (evaluar-expresiones exps env))
   )
  )

  ;;Funcion que recibe una lista de expresiones en sintaxis abstracta y la retorna parseada en una lista
(define evaluar-expresiones
  (lambda (exps env)
    (if(not(null? (cdr exps)))
        (append (list(eval-exp (car exps) env)) (append(evaluar-expresiones (cdr exps) env)))
        (list(eval-exp (car exps) env))
      )
    )
  )

;;Funcion que evalua if
(define eval-conditional-exp
 (lambda (test-exp true-exp false-exp env)
   (if (eval-exp test-exp env)
       (eval-exp true-exp env)
       (eval-exp false-exp env)
       )
   )
 )

;;Funcion que aplica la primitiva binaria deseada a las expresiones exp1 exp2 en el ambiente env
(define aplicar-bin-exp
  (lambda (primitiva exp1 exp2 env)
    (cases primitiva-binaria primitiva
      (primitiva-suma ()(+ (eval-exp exp1 env)(eval-exp exp2 env)))
      (primitiva-resta ()(- (eval-exp exp1 env)(eval-exp exp2 env)))
      (primitiva-multiplicacion ()(* (eval-exp exp1 env)(eval-exp exp2 env)))
      (primitiva-div ()(/ (eval-exp exp1 env)(eval-exp exp2 env)))
      (primitiva-cociente () (remainder (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-suma-octal()"xd")
      (primitiva-resta-octal () "xd")
      (primitiva-multiplicacion-octal () "xd")
      (primitiva-suma-hexa ()"xd")
      (primitiva-resta-hexa () "xd")
      (primitiva-multiplicacion-hexa() "xd")
      (primitiva-suma-32 () "xd")
      (primitiva-resta-32 () "xd")
      (primitiva-multiplicacion-32 () "xd")
      (primitiva-concat () (string-append (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-menor () (< (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-mayor () (> (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-menor-igual () (<= (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-mayor-igual () (>= (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-igual () (eqv? (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-diferente () (not (eqv? (eval-exp exp1 env) (eval-exp exp2 env))))
      (primitiva-and () (and (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-or () (or (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-insertar() (cons (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-union() (append (eval-exp exp1 env) (eval-exp exp2 env)))
      )
    )
  )

;;Funcion que aplica la primitiva unaria deseada a la expresion exp en el ambiente env
(define aplicar-un-exp
  (lambda (primitiva exp env)
    (cases primitiva-unaria primitiva
      (primitiva-add1 () (if (are-numbers? exp exp env)
                           (+ 1 (eval-exp exp env))
                           "Esta operación no permite valores no numericos"
                           ))
      (primitiva-sub1 () (if (are-numbers? exp exp env)
                           (- (eval-exp exp env) 1)
                           "Esta operación no permite valores no numericos"
                           ))
      (primitiva-longitud () (string-length (eval-exp exp env))) ;pendiente por espacio
      
      (primitiva-cab () (car (eval-exp exp env)))
      (primitiva-resto() (cdr (eval-exp exp env)))
      (primitiva-pred-lista () (list? (eval-exp exp env)))
      (primitiva-pred-vacio () (if(list? (eval-exp exp env)) (null? (eval-exp exp env)) (eopl:error "El dato ingresado no es una lista")))
      (primitiva-add1-octal()"xd")
      (primitiva-sub1-octal ()"xd")
      (primitiva-add1-hexa ()"xd")
      (primitiva-sub1-hexa() "xd")
      (primitiva-add1-32 ()"xd")
      (primitiva-sub1-32 () "xd")
      (primitiva-not () (not (eval-exp exp env)))
      
      )
    )
  )

;; funcion que valida si dos expresiones son numericas
;; exp1 X exp2 X env -> exp1 es la primera expresion que llega a la funcion , exp2 es la segunda expresion que llega a la funcion  y env es el ambiente de dichas variables
(define are-numbers?
  (lambda (exp1 exp2 env)
    (and (number? (eval-exp exp1 env))(number? (eval-exp exp2 env)))
    )
  )


;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;(define iota
;  (lambda (end)
;    (iota-aux 0 end)))
;
;(define iota-aux
;  (lambda (ini fin)
;    (if (>= ini fin)
;        ()
;        (cons ini (iota-aux (+ 1 ini) fin)))))

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
     ;(apply-env-ref env sym)))
    ;env))
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))






;********************************************************************************************
; Ejemplos
;; Expresiones
;; Números
(scan&parse "5") ; numero-lit
(scan&parse "1.5") ; numero-lit
(scan&parse "-12.5548") ; numero-lit
(scan&parse "-50") ; numero-lit
(scan&parse "x8[1, 2, 7, 4, 5]") ; base8-num-exp
(scan&parse "x16[15, 7, 14, 10]") ; base16-num-exp
(scan&parse "x32[30, 25, 15, 33]") ; base32-num-exp
;; Texto
(scan&parse "\"hola\"") ; texto-lit
;; Variables
(scan&parse "@x") ; var-exp
(scan&parse "@x2?") ; var-exp
;; Booleanas
(scan&parse "falso") ; false-exp
(scan&parse "verdadero") ; true-exp
;; Listas
(scan&parse "vacio") ; lista-vacia-exp
(scan&parse "lista[1, 2, 3, 4]") ; list-exp
;; Azúcar sintáctico
(scan&parse "++@x") ; azucarSuma
(scan&parse "--@x") ; azucarResta

;; Primitivas binarias
(scan&parse "(2 + 3)") ; primitiva-suma
(scan&parse "(2 ~ 3)") ; primitiva-resta
(scan&parse "(2 / 3)") ; primitiva-div
(scan&parse "(2 * 3)") ; primitiva-multiplicacion
(scan&parse "(2 % 3)") ; primitiva-cociente
;; Octal
(scan&parse "(x8[1, 2, 3, 4, 6] octal+ x8[3, 5, 6, 1])") ; primitiva-suma-octal
(scan&parse "(x8[1, 2, 3, 4, 6] octal~ x8[3, 5, 6, 1])") ; primitiva-resta-octal
(scan&parse "(x8[1, 2, 3, 4, 6] octal* x8[3, 5, 6, 1])") ; primitiva-multiplicacion-octal
;; Hexadecimal
(scan&parse "(x16[15, 14, 2, 5, 8] hexa+ x16[8, 9, 3, 7])") ; primitiva-suma-hexa
(scan&parse "(x16[15, 14, 2, 5, 8] hexa~ x16[8, 9, 3, 7])") ; primitiva-resta-hexa
(scan&parse "(x16[15, 14, 2, 5, 8] hexa* x16[8, 9, 3, 7])") ; primitiva-multiplicacion-hexa
;; Base 32
(scan&parse "(x32[30, 31, 0, 1] 32+ x32[3, 5, 30])") ; primitiva-suma-32
(scan&parse "(x32[30, 31, 0, 1] 32~ x32[3, 5, 30])") ; primitiva-resta-32
(scan&parse "(x32[30, 31, 0, 1] 32* x32[3, 5, 30])") ; primitiva-multiplicacion-32
;; Booleanas
(scan&parse "(2 > 3)") ; primitiva-mayor
(scan&parse "(2 < 3)") ; primitiva-menor
(scan&parse "(2 >= 3)") ; primitiva-mayor-igual
(scan&parse "(2 <= 3)") ; primitiva-menor-igual
(scan&parse "(2 == 3)") ; primitiva-igual
(scan&parse "(2 != 3)") ; primitiva-diferente
(scan&parse "((2 > 3) and (5 <= 10))") ; primitiva-and
(scan&parse "((2 > 3) or (5 <= 10))") ; primitiva-or
;; Listas
(scan&parse "(3 insert lista[1, 5, 4])") ; primitiva-insertas
(scan&parse "(lista[3, 5, 7] union lista[1, 5, 4])") ; primitiva-union

;; Primitivas unarias
;; Listas
(scan&parse "cab(lista[3, 4, 6])") ; primitiva-cab
(scan&parse "rest(lista[3, 4, 6])") ; primitiva-resto
(scan&parse "lista?(lista[3, 4, 6])") ; primitiva-pred-lista
(scan&parse "vacio?(lista[3, 4, 6])") ; primitiva-pred-vacio
;; Strings
(scan&parse "longitud(\"hola\")") ; primitiva-longitud
;; Booleana
(scan&parse "!(falso)") ; primitiva-not
;; Números
(scan&parse "add1(1)") ; primitiva-add1
(scan&parse "sub1(3)") ; primitiva-sub1
;; Octal
(scan&parse "octal-add1(x8[7, 3, 4])") ; primitiva-add1-octal
(scan&parse "octal-sub1(x8[7, 3, 4])") ; primitiva-sub1-octal
;; Hexadecimal
(scan&parse "hexa-add1(x16[14, 5, 15])") ; primitiva-add1-hexa
(scan&parse "hexa-sub1(x16[14, 5, 15])") ; primitiva-sub1-hexa
;; Base 32
(scan&parse "32-add1(x32[31, 25, 0, 4])") ; primitiva-add1-32
(scan&parse "32-sub1(x32[31, 25, 0, 4])") ; primitiva-sub1-32

;; Condicional
(scan&parse "Si verdadero { 5 } sino { 3 }") ; conditional-exp
(scan&parse "Si falso { 5 } sino { 3 }") ; conditional-exp
(scan&parse "Si(10 > 5) { 5 } sino { 3 }")

;; Variables
(scan&parse "unicVal @x = VAL") ; unicVal-exp
(scan&parse "const @x = 3") ; constVal-exp

;; Let
(scan&parse "sea(unicVal @x = VAL; const @y = 3; const @z = 5) { (@y + @z); }") ; variableLocal-exp

;; Set
(scan&parse "asignar @p = 8;") ; set-expresion

;; Procedimientos
(scan&parse "proc(@x, @y, @z) { asignar @y = 3; }") ; procedimiento-exp
(scan&parse "evaluar(@x [1, 3, 4, 6])") ; eval-proc-exp
(scan&parse "recursive eval (@y [1, 5, 7])") ; R-eva-proc-exp

;;factorial
(scan&parse "sea(const @factorial = proc(@n) { Si (@n > 0){(@n *recursive eval (@factorial [(@n ~ 1)]))} sino{ 1 }  }){ recursive eval (@factorial [5]) ;}")
;;factorial con unicVal
(scan&parse "sea(const @factorial = proc(@n) { Si (@n > 0){(@n *recursive eval (@factorial [(@n ~ 1)]))} sino{ 1 }  }; unicVal @x = VAL){ recursive eval (@factorial [@x]) ;}")
;;operacion rara
(scan&parse "sea(const @OPERACIONRARA = proc(@x, @y) { ((@x * @y) ~ @x )}; unicVal @x = VAL; const @y = 2) { evaluar(@OPERACIONRARA [@x,@y]); }")
