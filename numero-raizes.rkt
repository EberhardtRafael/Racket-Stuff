#lang racket
(require test-engine/racket-tests)

;;numero-raizes: numero numero numero booleano -> string

;;obj: Dados três números reais correspondentes aos coeficientes de um polinômio de segundo grau
;além de "true" ou "false", a função calcula o número de raízes reais distintas
;(porque um polinômio de segundo grau SEMPRE TEM DUAS RAÍZES) e retorna, ou uma string em português
;indicando o valor encontrado caso "true" ou uma string em inglês indicando o valor encontrado
;caso "false".

;;Exemplo:

;(numero-raizes 1 -5 4 true) = "O número de raízes reais distintas é duas."
;(numero-raizes 1 -2 4 false) = "The number of real distinct solutions is zero."

(define (numero-raizes a b c lingua)
  (define discriminante (- (* b b) (* 4 (* a c))))
  (if lingua

      (if (> discriminante 0) 
      (printf "O número de raízes reais distintas é dois.\n")
      (if (< discriminante 0)
          (printf "O número de raízes reais distintas é zero.\n")
          (printf "O número de raízes reais distintas é um.\n")       
          )
      )

      (if (> discriminante 0) 
      (printf "The number of real distinct solutions is two.\n")
      (if (< discriminante 0)
          (printf "The number of real distinct solutions is zero.\n")
          (printf "The number of real distinct solutions is one.\n")       
          )
      )
     
  ) 
  
)

(check-expect (numero-raizes 1 -5 4 true) "O número de raízes reais distintas é dois.\n")
(check-expect (numero-raizes 1 -4 4 true) "O número de raízes reais distintas é zero.\n")
(check-expect (numero-raizes 1 -2 4 false) "The number of real distinct solutions is zero.\n")