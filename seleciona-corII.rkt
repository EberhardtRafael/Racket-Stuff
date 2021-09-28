#lang racket

;(require htdp/draw)
;(require posn)
(require 2htdp/image)
(require test-engine/racket-tests)


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
 