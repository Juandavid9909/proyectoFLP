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
;               procedimiento-exp (ids cuerpo)                      |JavaScript|

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
    (expresion ("unicVal" id "=" expresion) unicVal-exp)   ;;Variable con unica asignación
    (expresion ("VAL") missing-val-exp);;crear variable sin valor asignado
    (expresion ("const" id "=" expresion) constVal-exp);;Definicion de constante
    (expresion ("sea" "(" (separated-list expresion ";")")" "{" expresion ";" "}" ) variableLocal-exp) ;;let
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
     '(const const const)
     (empty-env))))



;procedimiento que evalua las expresiones 
(define eval-exp
  (lambda (expre env)
    (cases expresion expre
      (numero-lit (num) num)
       (base8-num-exp (num8) num8 )
      (base16-num-exp (num16) num16)
      (base32-num-exp (num32) num32)
      (texto-lit (txt) txt)
      (var-exp (var) (buscar-variable env var))
      (primapp-bin-exp (exp1 bin-prim exp2) (aplicar-bin-exp bin-prim exp1 exp2 env))
      (primapp-un-exp (un-prim exp) (aplicar-un-exp un-prim exp env))
      (conditional-exp (test-exp true-exp false-exp) (eval-conditional-exp test-exp true-exp false-exp env))
      (unicVal-exp (id valor-exp) (assing-dcl id (eval-exp valor-exp env)))
      (missing-val-exp() '???)
      (constVal-exp (id valor-exp) (const-dcl id (eval-exp valor-exp env)))
      (variableLocal-exp (declaraciones instruccion) (apply-variableLocal-exp (evaluar-expresiones declaraciones env) instruccion env))
      (set-expresion (id valor-exp) (apply-set-expresion id (eval-exp valor-exp env) env))
      (procedimiento-exp (ids cuerpo)(closure ids cuerpo env))
      (eval-proc-exp(proc value-exps) (evaluar-procedimiento proc value-exps env))
      (R-eval-proc-exp(proc value-exps) (evaluar-R-procedimiento proc value-exps env))
      (azucarSuma (id) (aplicar-un-exp (primitiva-add1) (var-exp id) env))
      (azucarResta (id) (aplicar-un-exp (primitiva-sub1) (var-exp id) env))
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
      (primitiva-suma-octal() (sumaOctal (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-resta-octal () (restaOctal (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-multiplicacion-octal () (multiOctal (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-suma-hexa() (sumaHexa (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-resta-hexa () (restaHexa (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-multiplicacion-hexa () (multiHexa (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-suma-32() (suma32 (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-resta-32 () (resta32 (eval-exp exp1 env) (eval-exp exp2 env)))
      (primitiva-multiplicacion-32 () (multi32 (eval-exp exp1 env) (eval-exp exp2 env)))
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
      (primitiva-longitud () (string-length (eval-exp exp env)))
      
      (primitiva-cab () (car (eval-exp exp env)))
      (primitiva-resto() (cdr (eval-exp exp env)))
      (primitiva-pred-lista () (list? (eval-exp exp env)))
      (primitiva-pred-vacio () (if(list? (eval-exp exp env)) (null? (eval-exp exp env)) (eopl:error "El dato ingresado no es una lista")))
       (primitiva-add1-octal() (successorOctal (eval-exp exp env)))
      (primitiva-sub1-octal () (predecessorOctal (eval-exp exp env)))
      (primitiva-add1-hexa() (successorHexa (eval-exp exp env)))
      (primitiva-sub1-hexa () (predecessorHexa (eval-exp exp env)))
      (primitiva-add1-32() (successor32 (eval-exp exp env)))
      (primitiva-sub1-32 () (predecessor32 (eval-exp exp env)))
      (primitiva-not () (not (eval-exp exp env)))
      
      )
    )
  )

;;***********************************************************SET-EXP***********************************************************
;; Funcion que aplica un set expresion
(define apply-set-expresion
  (lambda (id valor env)
    (cases reference (apply-env-ref env id)
      (a-ref (pos vals states)
             (cond
               [(eqv? (vector-ref states pos) 'const) (eopl:error 'asignar "No es posible modificar una constante ~s" id) ]
               [(eqv? (vector-ref states pos) 'setted) (eopl:error 'asignar "Esta variable de asignacion unica ya ha sido modificada ~s" id)]
               [(eqv? (vector-ref states pos) 'assing)(begin(setref!(apply-env-ref env id) valor 'setted)(buscar-variable env id))]
               )
             )
      )
    )
  )

;;*********************************************************VARIABLELOCAL-EXP*********************************************************

;;Funcion que ejecuta una instruccion teniendo en cuenta declaraciones enviadas y el ambiente
(define apply-variableLocal-exp
  (lambda (declaraciones instruccion env)
    (eval-exp instruccion (extend-env (dcl-ids declaraciones) (dcl-values declaraciones) (dcl-states declaraciones) env))
    )
  )

;;***************************EVAL-PROC-EXP***************************//***************************R-EVAL-PROC-EXP***************************

;;Funcion que recibe un procedimiento (datatype proVal) y una lista de expresiones y ejecuta el procedimiento
;;evaluando la lista de expresiones para los ids del proc en el ambiente en el que fue creada la cerradura
(define evaluar-procedimiento
 (lambda (proc value-exps env)
   ;proc es un Id que referencia un procedimiento en el ambiente
   (cases procVal (eval-exp proc env)
     (closure (ids exp clousured-env)(apply-clousure-exp ids (evaluar-expresiones value-exps env) exp clousured-env))
     )
   )
 )
;;Funcion que evalua los  procedimientos recursivos aplicando la cerradura en el ambiente actual y no unicamente en el contenido por la cerradura
(define evaluar-R-procedimiento
 (lambda (proc value-exps env)
   (cases procVal (eval-exp proc env)
     (closure (ids exp clousured-env)(apply-clousure-exp ids (evaluar-expresiones value-exps env) exp env))
     )
   )
 )
;; funcion que evalua la expresion enviada en el ambiente env extendido con los ids y las exps (lista de expresiones previamente evaluadas) y lista de estados 
(define apply-clousure-exp
  (lambda (ids exps instruction env)
    (eval-exp instruction (extend-env ids exps (create-states ids) env))
    )
  )

;*******************************************************************Procedimientos*******************************************************************

(define-datatype procVal procVal?
  (closure
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb environment?)
   )
  )
;**********************************************************************Ambientes**********************************************************************
;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (state vector?)
   (env environment?)))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals states env)
    (extended-env-record syms (list->vector vals) (list->vector states) env)))

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (deref (apply-env-ref env sym))
    )
  )

;;Funcion que busca una variable en el ambiente y retorna su referencia (datatype reference)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals states env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals states)
                                 (apply-env-ref env sym)))))))


;**************************************************************************Declaraciones**************************************************************************


(define-datatype declaracion declaracion?
  (const-dcl (id symbol?) (valor valor-racket?))
  (assing-dcl (id symbol?) (valor valor-racket?))
  )

;;Funcion que recibe una lista de declaraciones(datatype) y retorna una lista con los ids de las declaraciones
(define dcl-ids
  (lambda (decl)
    (if (eqv? decl empty)
        empty
        (cases declaracion (car decl)
          (const-dcl (id value) (cons id (dcl-ids (cdr decl))))
          (assing-dcl (id value) (cons id (dcl-ids (cdr decl))))
          )
        )
    )
  )
;;funcion que recibe una lista de declaraciones(datatype) y retorna una lista con los valores de las declaraciones
(define dcl-values
  (lambda (decl)
   (if (eqv? decl empty)
        empty
        (cases declaracion (car decl)
          (const-dcl (id value) (cons value (dcl-values (cdr decl))))
          (assing-dcl (id value) (cons value (dcl-values (cdr decl))))
          )
        )
    )
  )
;;funcion que recibe una lista de declaraciones(datatype) y retorna una lista con los estados de las declaraciones (constante o asignacion)
(define dcl-states
  (lambda (decl)
    (if (eqv? decl empty)
        empty
        (cases declaracion (car decl)
          (const-dcl (id value) (cons 'const (dcl-states (cdr decl))))
          (assing-dcl (id value) (cons 'assing (dcl-states (cdr decl))))
          )
        )
    )
  )

;*****************************************************************Referencias*****************************************************************


(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (state vector?)
         ))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec states)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val state)
    (primitive-setref! ref val state)))

(define primitive-setref!
  (lambda (ref val newState)
    (cases reference ref
      (a-ref (pos vec state)
             (begin(vector-set! vec pos val)
                   (vector-set! state pos newState)
                   )))))


;****************************************************************************************
;Funciones Auxiliares

;;Funcion que retorna una lista de 'const del mismo tamaño que la lista que recibe
(define create-states
  (lambda (list)
    (if (null? list)
        empty
        (cons 'const (create-states (cdr list)))
        )
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
;;Funcion que recibe un valor y retorna verdadero
(define valor-racket?
  (lambda (val)
    #t
    )
  )

;; funcion que valida si dos expresiones son numericas
;; exp1 X exp2 X env -> exp1 es la primera expresion que llega a la funcion , exp2 es la segunda expresion que llega a la funcion  y env es el ambiente de dichas variables
(define are-numbers?
  (lambda (exp1 exp2 env)
    (and (number? (eval-exp exp1 env))(number? (eval-exp exp2 env)))
    )
  )

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


;------------------------------------------------------------------Funciones que operan con los numeros en las diferentes bases y sus condiciones --------------------------------------------------------------

;;Representación valida del cero (0) para la los octales, base 16 y 32

(define zero
  (lambda () '(0))
  )

;;Funcion que valida si una entrada n es 0
;; n -> boolean
(define is-zero?
  (lambda (n)
    (or (null? n) (equal?  n '(0)))
    )
  )

;;Funcion que retorna el siguiente valor de un octal
;;octal -> octal
(define successorOctal
  (lambda (l)
    (cond
      [(is-zero? l)'(1)]
      [(< (car l) 7) (cons (+ 1 (car l)) (cdr l))]
      [(eqv? 7 (car l)) (cons 0 (successorOctal (cdr l)))]
      )
    )
  )
;;Funcion que retorna el valor previo de un octal
;;octal -> octal
(define predecessorOctal
  (lambda (l)
    (cond
      [(and (is-zero? (list (car l)))(not (null?(cdr l)))) (cons 7 (predecessorOctal (cdr l)))]
      [(is-zero? l) (zero)]
      [(and (eqv? (car l) 1) (null? (cdr l))) empty]
      [(> 8 (car l)) (cons (- (car l) 1) (cdr l))]
      )
    )
  )
;;Funcion que suma dos Octales y retorna el resultado en Octales
;;l1,l2 : Octal -> Octal
(define sumaOctal
  (lambda (l1 l2)
    (cond
      [(is-zero? l2)l1]
      [(is-zero? l1)l2]
      [else (sumaOctal (successorOctal l1)(predecessorOctal l2))]
      )
    )
  )
;;Funcion que resta dos Octales y retorna el resultado en Octal
;;l1,l2 : Octal -> Octal
(define restaOctal
  (lambda (l1 l2)
    (cond
      [(and (is-zero? l1)(not(is-zero? l2)))(eopl:error "No se cubren los resultados negativos")]
      [(is-zero? l2)l1]
      [(is-zero? l1)(zero)]
      [else (restaOctal (predecessorOctal l1)(predecessorOctal l2))]
      )
    )
  )
;;Funcion que multiplica dos Octales y retorna el resultado en Octal
;;op1,op2 : Octal -> Octal
(define multiOctal
  (lambda (op1 op2)
    (cond
      [(or(is-zero? op1)(is-zero? op2))(zero)]
      [(is-zero? (predecessorOctal op2))op1]
      [else (sumaOctal op1 (multiOctal op1 (predecessorOctal op2)))]
      )
    )
  )

;;Funcion que retorna el siguiente valor de un Hexadecimal
;;bigNum16 -> bigNum16
(define successorHexa
  (lambda (l)
    (cond
      [(is-zero? l)'(1)]
      [(< (car l) 15) (cons (+ 1 (car l)) (cdr l))]
      [(eqv? 15 (car l)) (cons 0 (successorHexa (cdr l)))]
      )
    )
  )
;;Funcion que retorna el valor previo de un Hexadecimal
;;bigNum16 -> bigNum16
(define predecessorHexa
  (lambda (l)
    (cond
      [(and (is-zero? (list (car l)))(not (null?(cdr l)))) (cons 15 (predecessorHexa (cdr l)))]
      [(is-zero? l) (zero)]
      [(and (eqv? (car l) 1) (null? (cdr l))) empty]
      [(> 16 (car l)) (cons (- (car l) 1) (cdr l))]
      )
    )
  )




;;Funcion que suma dos bigNum16 y retorna el resultado en bigNum16
;;l1,l2 : bigNum16 -> bigNum16
(define sumaHexa
  (lambda (l1 l2)
    (cond
      [(is-zero? l2)l1]
      [(is-zero? l1)l2]
      [else (sumaHexa (successorHexa l1)(predecessorHexa l2))]
      )
    )
  )
;;Funcion que resta dos bigNum16 y retorna el resultado en bigNum16
;;l1,l2 : bigNum16 -> bigNum16
(define restaHexa
  (lambda (l1 l2)
    (cond
      [(and (is-zero? l1)(not(is-zero? l2)))(eopl:error "No se cubren los resultados negativos")]
      [(is-zero? l2)l1]
      [(is-zero? l1)(zero)]
      [else (restaHexa (predecessorHexa l1)(successorHexa l2))]
      )
    )
  )
;;Funcion que multiplica dos bigNum16 y retorna el resultado en bigNum16
;;op1,op2 : bigNum16 -> bigNum16
(define multiHexa
  (lambda (op1 op2)
    (cond
      [(or(is-zero? op1)(is-zero? op2))(zero)]
      [(is-zero? (predecessorHexa op2))op1]
      [else (sumaHexa op1 (multiHexa op1 (predecessorHexa op2)))]
      )
    )
  )

;;Funcion que retorna el siguiente valor de un base 32
;;bigNum32 -> bigNum32
(define successor32
  (lambda (l)
    (cond
      [(is-zero? l)'(1)]
      [(< (car l) 31) (cons (+ 1 (car l)) (cdr l))]
      [(eqv? 31 (car l)) (cons 0 (successor32 (cdr l)))]
      )
    )
  )
;;Funcion que retorna el valor previo de un bigNum32
;;bigNum32 -> bigNum32
(define predecessor32
  (lambda (l)
    (cond
      [(and (is-zero? (list (car l)))(not (null?(cdr l)))) (cons 31 (predecessor32 (cdr l)))]
      [(is-zero? l) (zero)]
      [(and (eqv? (car l) 1) (null? (cdr l))) empty]
      [(> 32 (car l)) (cons (- (car l) 1) (cdr l))]
      )
    )
  )




;;Funcion que suma dos bigNum32 y retorna el resultado en bigNum32
;;l1,l2 : bigNum32 -> bigNum32
(define suma32
  (lambda (l1 l2)
    (cond
      [(is-zero? l2)l1]
      [(is-zero? l1)l2]
      [else (suma32 (successor32 l1)(predecessor32 l2))]
      )
    )
  )
;;Funcion que resta dos bigNum32 y retorna el resultado en bigNum32
;;l1,l2 : bigNum32 -> bigNum32
(define resta32
  (lambda (l1 l2)
    (cond
      [(and (is-zero? l1)(not(is-zero? l2)))(eopl:error "No se cubren los resultados negativos")]
      [(is-zero? l2)l1]
      [(is-zero? l1)(zero)]
      [else (resta32 (predecessor32 l1)(successor32 l2))]
      )
    )
  )
;;Funcion que multiplica dos bigNum32 y retorna el resultado en bigNum32
;;op1,op2 : bigNum32 -> bigNum32
(define multi32
  (lambda (op1 op2)
    (cond
      [(or(is-zero? op1)(is-zero? op2))(zero)]
      [(is-zero? (predecessor32 op2))op1]
      [else (suma32 op1 (multi32 op1 (predecessor32 op2)))]
      )
    )
  )




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
