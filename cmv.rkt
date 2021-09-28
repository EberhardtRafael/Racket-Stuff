#lang racket
(require test-engine/racket-tests)

;;cmv: número real, número real, número real, número real -> número real

;;obj: Dados números correspondentes, respectivamente, à posição inicial, à posição
;final, ao instante inicial e ao instante final relativos ao movimento de um veículo
;retorna-se um número correspondente a sua velocidade média ao longo desse período.
;Como o usuário não está acostumado a converter o valor observado no relógio em horas,
;o algoritmo permite que se utilize o formato hh.mm semelhante ao que se observa no relógio.
;IMPORTANTE: o programa somente funciona caso se utilize desse formato de horário!
;(Não se deve utilizar : para separar hora de minutos, mas apenas .)

;;Exemplo:

;;(cmv 0 100 0 1) = 100.
;;(cmv 75 125 3.15 5.45) = 20.

(define (cmv rinicial rfinal tinicial tfinal)
  (define velocidade (/ (- rinicial rfinal) ;variação total na posição do veículo
     (- (converte-hora tinicial 0.0) (converte-hora tfinal 0.0)) ;intervalo de tempo ao longo do qual o movimento acontece
     ))
  (printa velocidade)
)
;------------------------------------------------------------------------------------
;;printa numero -> string

;;obj: Printar uma string contendo "A velocidade média do móvel foi: " seguida da
;velocidade calculada por cmv e da unidade de medida no SI.
;Embora seja uma função definida dentro do programa, não deve ser utilizada pelo usuário!
;É utilizada apenas internamente.

(define (printa veloc)
  (printf "A velocidade média do móvel foi: ~a Km/h. " veloc)
  )

;------------------------------------------------------------------------------------
;;converte-hora número real, número real -> número real

;;obj: Como o usuário não está acostumado a converter a quantidade observada no relógio
;em horas, essa função o faz, permitindo que, dada a entrada de um valor
;de tempo do tipo hh.mm na função cvm, este seja conertido em horas e o cálculo da
;velocidade funcione.
;A função funciona de forma recursiva, portanto, a segunda entrada sempre deve ser 0.0.

;(Não se deve utilizar : para separar hora de minutos, mas apenas
;Exemplo:
;Caso queira-se indicar o hoŕario 15 h e 30 min, deve-se utilizar 15.30.
;)

;;Exemplos:


;(converte-hora 0.30 0.0) = 0.5 
;(converte-hora 17.45 0.0) = 17.75


(define (converte-hora hora relogio)
  ;(printf "~v \n" (- hora relogio)) ;serve para testar a função somente
  (if (< (- hora relogio) 1) ;condição
      (+ relogio (/ (* 10 (- hora relogio)) 6)) ;caso verdadeiro
      (converte-hora hora (+ 1 relogio)) ;caso falso
  )
)

(check-expect (cmv 75 125 3 5) 25)
(check-expect (converte-hora 0.30 0.00) 0.50)
