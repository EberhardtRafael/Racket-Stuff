;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista3_RafaelEberhardt_U) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Nome: Rafael Eberhardt
;; Turma: U

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================

;; movimenta_personagem: symbol -> string
;; obj: Dado um símbolo em {'W, 'w, 'A, 'a, 'S, 's, 'D, 'd}, a função retorna uma string contendo "Andando para frente" ('w ou 'W), "Andando para esquerda" ('a ou 'A), "Andando para tras" ('s ou 'S) ou "Andando para direita" ('d ou 'D). A utilização de qualquer outro símbolo gera a string "Parado".

;(Considerei as possibilidades de letra maiúscula ou minúscula para que, caso o jogador aperte "caps lock", o jogo continue funcionando.)

;; Exemplos:
;; (movimenta_personagem 'w) = "Andando para frente"
;; (movimenta_personagem 'D) = "Andando para direita"
;; (movimenta_personagem 's) = "Andando para tras"
;; (movimenta_personagem 'a) = "Andando para esquerda"
;; (movimenta_personagem 't) = "Parado"

(check-expect (movimenta_personagem 'w) "Andando para frente")
(check-expect (movimenta_personagem 'd) "Andando para direita")
(check-expect (movimenta_personagem 's) "Andando para tras")
(check-expect (movimenta_personagem 'a) "Andando para esquerda")
(check-expect(movimenta_personagem 'o)  "Parado")

(define (movimenta_personagem command)  
  (cond
    ;Se a entrada é 'w ou 'W, imprime "... frente"
    [(or (symbol=? command 'w) (symbol=? command 'W) )  "Andando para frente"]
    ;Se a entrada é 'a ou 'A, imprime "... esquerda
    [(or (symbol=? command 'a) (symbol=? command 'A) )  "Andando para esquerda"]
    ;Se a entrada é 's ou 'S, imprime "... tras"
    [(or (symbol=? command 's) (symbol=? command 'S) )  "Andando para tras"]
    ;Se a entrada é qualquer coisa exceto as anteriores, imprime "... frente"
    [(or (symbol=? command 'd) (symbol=? command 'D) )  "Andando para direita"]
    [else "Parado"]
    )

  )
;Noutra cadeira, eu aprendi que bons hábitos de programação não têm a ver com comentar qualquer coisa. O código acima é extremamente claro e somente fica mais difícil de entender com esses comentários. Mas estou fazendo porque assim indicava a lista. Mas, me sinto mal.

;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
;; ==============================================================

;; esta_dentro: número número número número número -> boolean
;; obj: Dados os valores x e y do centro de uma circunferência, além de seu raio, seguido dos valores x e y e um ponto qualquer (na ordem com que foi escrito), a função retorna "verdadeiro" ou "falso" caso, respectivamente, o ponto esteja ou não dentro(sobre) da(a) circunferência.


;; Exemplos:
;; (esta_dentro 0 0 1 (cos (/ pi 4)) (sin (/ pi 4))) = #true
;; (esta_dentro 5 7 2.3 5 9.31) = #false

(check-expect(esta_dentro 0 0 1 (cos (/ pi 4)) (sin (/ pi 4))) #t)
(check-expect(esta_dentro 0 0 1 1 1) #f)
(check-expect(esta_dentro 0 0 1 0.1 0.1) #t)
(check-expect(esta_dentro 5 7 2.3 5 9.31) #f)
(check-expect(esta_dentro 5 7 2.3 5 9.3) #t)
(check-expect(esta_dentro 5 7 2.3 5 9.29) #t)

;;Deveria usar posn? Mas não vimos ainda...

(define (esta_dentro centrox centroy raio pontox pontoy)
 
    (<=
     (+ (* (- pontox centrox) (- pontox centrox) )
        (* (- pontoy centroy) (- pontoy centroy) ))
     (* raio raio))
  )

;; ==============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; ==============================================================

;; esta_dentro_v2: número número número número número -> boolean
;; obj: Dados os valores x e y do centro de uma circunferência, além de seu raio, seguido dos valores x e y e um ponto qualquer (na ordem com que foi escrito), a função retorna "verdadeiro" ou "falso" caso, respectivamente, o ponto esteja ou não dentro(sobre) da(a) circunferência.
;A diferença em relação à função esta_dentro é que a  esta_dentro_v2 faz os cálculos de forma redundante, já que uma comparação aritimética já retorna um booleano.

;; Exemplos:
;; (esta_dentro 0 0 1 (cos (/ pi 4)) (sin (/ pi 4))) = #true
;; (esta_dentro 5 7 2.3 5 9.31) = #false

(check-expect(esta_dentro 0 0 1 (cos (/ pi 4)) (sin (/ pi 4))) #t)
(check-expect(esta_dentro 0 0 1 1 1) #f)
(check-expect(esta_dentro 0 0 1 0.1 0.1) #t)
(check-expect(esta_dentro 5 7 2.3 5 9.31) #f)
(check-expect(esta_dentro 5 7 2.3 5 9.3) #t)
(check-expect(esta_dentro 5 7 2.3 5 9.29) #t)

(define (esta_dentro_v2 centrox centroy raio pontox pontoy)
  (cond
    ;Caso (x - x0)^2 + (y - y0)^2 <= r^2, retorna verdadeiro.    
    [(<=
     (+ (* (- pontox centrox) (- pontox centrox) )
        (* (- pontoy centroy) (- pontoy centroy) ))
     (* raio raio)) #t]
    ;Caso contrário, falso.
    [else #f]
    )
  )

;; Resposta Escrita:
;; Se eu quisesse que a saída fosse uma string, eu deveria usar uma versão da esta_dentro_v2, porque a função cond permite que se dê a consequência de um teste condicional. Nesse caso, a consequência seria printar a string desejada. Na esta_dentro, não existe controle sobre a saída do teste, o que é intrínseco à linguagem. 

;; ==============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;; ==============================================================

;; gastos_mes: número número número número -> string
;; obj: Dados os valores do salário, aluguel, conta de luz e conta de internet, nessa ordem, a função calculas quantas das contas será possível pagar e retorna uma string da forma "Neste mês, foi possível pagar X conta(s).", em que X = 0, 1, 2, 3.

;; Exemplos:
;; (gastos_mes 1000 500 900 300) = "Neste mês, foi possível pagar 2 conta(s)."
;; (gastos_mes 1000 1000 900 300) = "Neste mês, foi possível pagar 1 conta(s)."
;; (gastos_mes 1000 5000 9000 3000) = "Neste mês, foi possível pagar 0 conta(s)."
;; (gastos_mes 1000 500 100 300) = "Neste mês, foi possível pagar 3 conta(s)."

(check-expect (gastos_mes 1000 500 900 300) "Neste mês, foi possível pagar 2 conta(s).")
(check-expect (gastos_mes 1000 1000 900 300) "Neste mês, foi possível pagar 1 conta(s).")
(check-expect (gastos_mes 1000 5000 9000 3000) "Neste mês, foi possível pagar 0 conta(s).")
(check-expect (gastos_mes 1000 500 100 300) "Neste mês, foi possível pagar 3 conta(s).")

(define (gastos_mes sal aluguel luz net)
  (outputGasto_mes
  (cond
    ;Caso o salário seja maior que a soma das três contas, chama a função (outputGasto_mes 3), que imprime "Neste mês, foi possível pagar 3 conta(s)."
    [(>= sal (+ aluguel luz net) ) 3]
    ;Caso o salário seja maior que a soma de duas contas (testando todos os casos), chama (outputGasto_mes 2).
    [(or
     (>= sal (+ aluguel luz) )
     (>= sal (+ aluguel net) )
     (>= sal (+ net luz) )
     ) 2] ;Como a quantidade de casos é pequena, posso criar casos por exaustão. Eu sei que poderia fazer diferente.
    ;Caso o salário seja maior que somente uma das contas, chama (outputGasto_mes 1).
    [(or
      (>= sal aluguel)
      (>= sal luz)
      (>= sal net)
      ) 1]
    ;Caso o aluguel seja menor que tudo, chama (outputGasto_mes 0). 
    [else 0]
    )
  )
  )  
;Mais uma vez, penso que o código acima somente fica mais difícil de ler do que sem comentários.

;outputGasto_mes: número -> string
;obj: Dado qualquer número, a função imprime a string  "Neste mês, foi possível pagar X conta(s).", em que X é o número dado.

;Exemplos:
;(outputGasto_mes 10) = "Neste mês, foi possível pagar 10 conta(s)."

;Os testes foram realizados acima, pois a função acima depende desta.

(define (outputGasto_mes numero)
  (string-append "Neste mês, foi possível pagar " (number->string numero) " conta(s).")
)


; ==============================================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 55 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
;; ==============================================================

;; max_tres: número número número -> número
;; obj: Dados três números, a função retorna o maior dentre eles.

;; Exemplos:
;; (max_tres 1 2 3) = 3
;; (max_tres 1 1 1) = 1
;; (max_tres 0 0 0) = 0
;; (max_tres -1 -2 -3) = -1

(check-expect (max_tres 1 2 3) 3)
(check-expect (max_tres 1 1 1) 1)
(check-expect (max_tres 0 0 0) 0)
(check-expect (max_tres -1 -2 -3) -1)


(define (max_tres a b c)
  (cond
    [(> a b)
     (cond
       [(> a c) a]
       [else c])]
    [else
     (cond
       [(> b c) b]
       [else c]
       )]
    )
  )

;; ==============================================================
;; EXTRA EXTRA EXTRA EXTRA EXTRA EXTRA EXTRA EXTRA EXTRA EXTRA 
;; ==============================================================


;; opera_na_bolsa: número número númeronúmero -> número
;; obj: Dados o valor inicial de uma ação, o valor atual, a quantidade de ações na carteira e a quantidade de capital disponível, a função retorna a quantidade de ações conveniente a ser comprada ou vendida. Uma saída positiva significa compra, enquanto uma saída negativa significa venda.

;; Ex: 
;; (opera_na_bolsa 10 11 10 1000) = 0
;; (opera_na_bolsa 10 20 10 1000) = 50
;; (opera_na_bolsa 10 8 10 1000) = -2
;; (opera_na_bolsa 10 8.1 10 1000) = 0

(check-expect (opera_na_bolsa 10 11 10 1000) 0)
(check-expect (opera_na_bolsa 10 20 10 1000) 50)
(check-expect (opera_na_bolsa 10 8 10 1000) -2)
(check-expect (opera_na_bolsa 10 8.1 10 1000) 0)
(check-expect (opera_na_bolsa 10 8.0001 10 1000) 0)


(define (opera_na_bolsa P0 P N cap)
  (cond
    ;Caso a variação percentual no valor da ação seja maior que 100%, eu vendo todas as ações que tenho.
    [(> (variacao_percentual P0 P) 1) (- N)]
    [else
     (cond
       ;Caso a variação no valor da ação dada seja menor que 100% e maior que 10%, compro (capital/(valor atual)*(variação percentual)) de ações.
       [(> (variacao_percentual P0 P) 0.1)
        (* (/ cap P) (variacao_percentual P0 P))]
       [else
        (cond
          ;Caso a variação no valor da ação dada esteja entre -20% e 10%, não vendo nem vcompro nada.
          [(> (variacao_percentual P0 P) -0.2) 0]
          ;Caso a variação no valor da ação dada seja superior a -20%, vendo uma quantidade de ações correspondente ao produto da variação pela quantidade de ações que possuo.
          [else (* N (variacao_percentual P0 P))]
          )]
       )]    
    )
 )

;variacao_percentual: número número -> número
;Obj: Calcula, na verdade, a variação proporcional de uma quantidade.

;Como é utilizada inúmeras vezes dentro da função opera_na_bolsa, vale a pena definir uma função somente para isso.

;Ex:
;;(variacao_percentual 100 100) = 0
;;(variacao_percentual 100 101) = 0.01
;;(variacao_percentual 100 10) = -0.9

;Já foi testada acima.
(define (variacao_percentual P0 P)
  (/ (- P P0) P0)
  )








