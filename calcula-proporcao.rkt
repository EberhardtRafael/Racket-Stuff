#lang racket
(require test-engine/racket-tests)

;;calcula-proporcao: numero numero -> string

;;obj: Dados dois números reais a e b, calcula-se a proporção do primeiro em relação ao segundo
;e escreve-se o resultado arredondado em porcentagem.
;(Caso b = 0, retorna-se -1)


;;Exemplo:
;(calcula-proporcao 21 17)= 124%
;(calcula-proporcao 5 10) = 50%.
;(calcula-proporcao 10 5) = 200%.

(define (calcula-proporcao a b)
  (if (> b 0)
      (printf "~v%.\n"(round (* (/ a b) 100)))
      -1
      )
  )

(check-expect (calcula-proporcao 5 10) "50%.") ;Ao menos no meu PC, eu coloco um valor errado aqui e nada é alegado.
(check-expect (calcula-proporcao 21 0) -1)