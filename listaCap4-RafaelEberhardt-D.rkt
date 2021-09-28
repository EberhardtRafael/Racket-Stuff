#lang racket
(require 2htdp/image)
(require posn)
(require test-engine/racket-tests)

;------------------------------------------------------------------------------------
;;selecioina-cor: numero inteiro -> "cor"

;;obj: Dado um número, caso este seja menor que 50, a função retorna "orange"
;caso o número seja maior ou igual a 50, retorna-se "red".

;;Exemplo:
;(seleciona-cor 50) = "red".
;(seleciona-cor 10) = "orange".

(define (seleciona-cor numero)
  (if (> 50 numero)
      "orange" ;caso verdadeiro: numero < 50
      "red" ;caso falso: numero > 50
      )
)

(check-expect (seleciona-cor 50) "red")
(check-expect (seleciona-cor 60) "red")
(check-expect (seleciona-cor 40) "orange")

;------------------------------------------------------------------------------------
;;numero-raizes: numero numero numero booleano -> string

;;obj: Dados três números reais correspondentes aos coeficientes de um polinômio de segundo grau
;além de "true" ou "false", a função calcula o número de raízes reais distintas
;(porque um polinômio de segundo grau SEMPRE TEM DUAS RAÍZES) e retorna, ou uma string em português
;indicando o valor encontrado caso "true" ou uma string em inglês indicando o valor encontrado
;caso "false".

;;Exemplo:

;(numero-raizes 1 -5 4 true) = "O número de raízes reais distintas é dois."
;(numero-raizes 1 -2 4 false) = "The number of real distinct solutions is zero."

(define (numero-raizes a b c lingua)
  (define valor "dois")
(define discriminante (- (* b b) (* 4 (* a c))))
  
(cond
  [lingua   
   (cond
     [(< discriminante 1)
      (if (< discriminante 0) (set! valor "zero") (set! valor "um"))
      ]
    )
   (printf "O número de raízes reais distintas é: ~a.\n" valor)
   ]
  [else   
   (cond
      [(> discriminante 0) (set! valor "two")]
      [else (if (< discriminante 0) (set! valor  "zero") (set! valor "one"))]
    )
   (printf "The number of real distinct solutions is: ~a.\n" valor)
   ]
  )
  
)

(check-expect (numero-raizes 1 -5 4 true) "O número de raízes reais distintas é: dois.\n")
(check-expect (numero-raizes 1 -4 4 true) "O número de raízes reais distintas é: um.\n")
(check-expect (numero-raizes 1 -2 4 false) "The number of real distinct solutions is: zero.\n")

;------------------------------------------------------------------------------------

;;calcula-proporcao: numero numero -> string

;;obj: Dados dois números reais a e b, calcula-se a proporção do primeiro em relação ao segundo
;e escreve-se o resultado arredondado em porcentagem.
;(Caso b = 0, retorna-se -1)


;;Exemplo:
;(calcula-proporcao 21 17)= 124.
;(calcula-proporcao 5 10) = 50.
;(calcula-proporcao 10 5) = 200.

(define (calcula-proporcao a b)
  (if (> b 0)
      (round (* (/ a b) 100))     
      -1
      )
)

(check-expect (calcula-proporcao 5 10) 50) ;Ao menos no meu PC, eu coloco um valor errado aqui e nada é alegado.
(check-expect (calcula-proporcao 21 0) -1)

;------------------------------------------------------------------------------------
;desenha-barra: número -> string + retângulo

;obj: Dado um número n maior que zero, este é transformado em string e printado na tela englobado
;por um retângulo cuja largura apresenta um número de pixels corresponde ao valor entrado.
;Importante: n < 0 gera erro.

;Exemplo:
;(desenha-barra 5) = .
;(desenha-barra 50) = .
;(desenha-barra 0) = 0
;(desenha-barra -1) = "rectangle: expects a non negative real number as first argument, given -1"

(define (desenha-barra n)
  (overlay (text (number->string n) 15 "black")
           (rectangle n 30 "solid" (seleciona-cor n))))


;------------------------------------------------------------------------------------
;;visualiza-resultado: string string número número -> string + retângulo

;;obj: Dados dois grupos com seus respectivos tamanhos, a função cria um gráfico de barra
;horizontal indicando a proporção relativa entre os grupos por meio de cores distintas.

;;Exemplos:

;(visualiza-resultado "grupo1" "grupo2" 60 80) = .
;(visualiza-resultado "grupo1" "grupo2" 60 0) = .
;(visualiza-resultado "grupo1" "grupo2" 801 73) = .
;(visualiza-resultado "grupo1" "grupo2" 0 0) = (erro!) rectangle: expects a non negative real number as first argument, given -1


(define (visualiza-resultado grupo1 grupo2 n1 n2)
  (define n (calcula-proporcao n1 (+ n1 n2)))

  (define lado (- 100 n))
  (define cor-letras1 "black")
  (define cor-letras2 "black")

  (cond
    [(< 83 n)
     (if (< 91 n)
         (cond
           [(> lado 0) (set! lado 9)]
           )         
         (set! lado 17)
         )
     ] ;A proporção entre os retângulos não fica correta, mas fica visualmente mais agradável.
    )

  (cond
    [(= n 0) (set! cor-letras1 (seleciona-cor (- 100 n)))]
    [(= (- 100 n) 0) (set! cor-letras2 "white")]

    )
  
  (overlay/xy
   (text (string-append "O tamanho da amostra é: " (number->string (+ n1 n2))) 12 "black")
   20 -30
   (overlay/xy 
          (overlay
           (text (number->string n) 15 cor-letras1)
           (rectangle n 30 "solid" (seleciona-cor n))
          )
          n 0
          (overlay
           (text (number->string (- 100 n)) 15 cor-letras2)
           (rectangle lado 30 "solid" (seleciona-cor (- 100 n)))
           )
          )
   )
 
)



