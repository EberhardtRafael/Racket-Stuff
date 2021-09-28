#lang racket

;(require htdp/draw)
;(require posn)
(require 2htdp/image)
(require test-engine/racket-tests)


;;selecioina-cor: numero inteiro -> "cor"

;;obj: Dado um número, caso este seja menor que 50, a função retorna um retângulo laranja
;caso o número seja maior ou igual a 50, retorna-se um retângulo vermelho.
;(Tenho a impressão de que eu deveria retornar um objeto cor, mas ok)

;;Exemplo: (seleciona-cor 50) = "retângulo vermelho".


(define (seleciona-cor numero)
  (if (> 50 numero)
      (rectangle 20 20 "solid" "orange") ;caso verdadeiro: numero < 50
      (rectangle 20 20 "solid" "red") ;caso falso: numero > 50
      )
)

(check-expect (seleciona-cor 50) (rectangle 20 20 "solid" "red"))
 