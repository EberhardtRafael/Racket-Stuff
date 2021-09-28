;------------------------------------------------------------------------------------------------
(define-struct Aluno [
                      nome
                      num_matricula
                      curso
                      instituicao
                      ])

;Um Aluno é uma estrutura:
;(make-Aluno string numero string string)
;Interpretção: (make-Aluno "Joao" 172780 "CiC" "UFRGS" )
;é um aluno com nome de João, cujo número de matrícula é 172780, estuda CiC na UFRGS.

(define alun1 (make-Aluno "Joao" 172780 "CiC" "UFRGS" ))
(define alun2 (make-Aluno "Ricardo" 172781 "Matemática" "UFXY"))
(define alun3 (make-Aluno "Maria" 172782 "Medicina" "UFWZ"))
(define alun4 (make-Aluno "Mariana" 172783 "Música" "UFWZ"))

;------------------------------------------------------------------------------------------------

(define-struct InstEnsino[
                          nome
                          ano_fundacao
                          ])

;Um InstEnsino é uma estrutura:
;(make-InstEnsino "UFXY" 2999)
;é uma instituição de ensino denominada UFXY fundada em 2999.


(define uni1 (make-InstEnsino "UFXY" 2999))
(define uni2 (make-InstEnsino "UFWZ" 1900))
;Agora eu também posso construir um Aluno cuja instituição de ensino é um InstEnsino:
;(define alun2 (make-Aluno "Joao" 172780 "CiC" uni1))
;Assim, posso utilizar o seguinte comando:
;(InstEnsino-ano_fundacao (Aluno-instituicao alun2)) = 2999
;------------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------------
;mesmaInstituicao? : Aluno Aluno InstEnsino -> string
;Dadas duas instâncias da estrutura aluno e uma instância da estrutura InstEnsino
;verifica-se se ambos alunos estão ou não matriculados na mesma instituição de ensino.

;Exemplo:
;(Utilizando as instâncias criadas acima)
;(mesmaInstituicao? alun1 alun2 uni1) = "Apenas Ricardo está matriculado(a) em UFXY."
;(mesmaInstituicao? alun1 alun3 uni1) = "Nenhum dos alunos está matriculado(a) em UFXY."
;(mesmaInstituicao? alun3 alun4 uni2) = "Maria e Mariana estão ambos matriculados(as) em UFWZ."

(check-expect (mesmaInstituicao? alun1 alun2 uni1) "Apenas Ricardo está matriculado(a) em UFXY.")

(define (mesmaInstituicao? alun1 alun2 instituicao)
  (cond
    
    [(string=? (Aluno-instituicao alun1) (InstEnsino-nome instituicao)) ;Caso o primeiro aluno esteja matriculado
     (if (string=? (Aluno-instituicao alun2) (InstEnsino-nome instituicao)) ;Verifica se o segundo aluno também está matriculado.
         (string-append (Aluno-nome alun1) " e "  (Aluno-nome alun2) " estão ambos matriculados(as) em " (InstEnsino-nome instituicao) ".")
         (string-append "Apenas " (Aluno-nome alun1) " está matriculado(a) em " (InstEnsino-nome instituicao) ".")
         )
     ]
    
    [(string=? (Aluno-instituicao alun2) (InstEnsino-nome instituicao))
     (string-append "Apenas " (Aluno-nome alun2) " está matriculado(a) em " (InstEnsino-nome instituicao) ".")
     ];Caso o segundo aluno esteja matriculado e o primeiro não.
    
    [else (string-append "Nenhum dos alunos está matriculado(a) em " (InstEnsino-nome instituicao) ".")]

    );Fim do condicional.
 );Fim da função mesmaInstituicao?.

;------------------------------------------------------------------------------------------------

;idade: Aluno Aluno InstEnsino -> numero
;Dadas duas instâncias da estrutura aluno e uma instância da estrutura InstEnsino
;calcula-se a idade da instituição de ensino caso ambos os alunos estejam nela matriculados.
;O programa retorna -1 caso não estejam ambos matriculados na mesma instituição.

;Exemplo:
;(Utilizando as instâncias criadas acima)
;(idade alun1 alun2 uni1) = -1
;(idade alun3 alun4 uni2) = 120.

(check-expect (idade alun3 alun4 uni2) 120)

(define (idade A B instituicao)
  (if (string=?
       (mesmaInstituicao? A B instituicao)
       (string-append (Aluno-nome A) " e "  (Aluno-nome B) " estão ambos matriculados(as) em " (InstEnsino-nome instituicao) "."))
      (- 2020 (InstEnsino-ano_fundacao instituicao))
      -1
      )

  )

;------------------------------------------------------------------------------------------------
(define-struct carro [
                        ano
                        modelo
                        valor
                        ar_cond
                        direcao_hidr
                        vidro_eletrico
                        ])


;Um carro é uma estrutura:
;(make-carro numero string numero booleano booleano booleano)
;Interpretação: (make-carro 2005 "Gol" 12000 #f #t #t)
;é um carro modelo Gol, fabricado em 2005, sem ar condicinado, com direção hidráulica e vidros
;elétricos com valor de mercado de R$ 12.000,00.

(define car1 (make-carro 2005 "Gol" 12000 #f #t #t))

(define-struct moto [
                        ano
                        modelo
                        valor                       
                        ])
;Um moto é uma estrutura:
;(make-moto numero string numero)
;Interpretação: (make-moto 2010 "Dyna Suaper Glide" 28000)
;é uma moto modelo Dyna Super Glide, fabricada em 2010 cujo valor de mercado corresponde a R$ 28.000,00

(define motinha (make-moto 2010 "Dyna Suaper Glide" 28000))

;cdc : carro -> número
;Dada uma instância da estrutura carro, calcula-se o preço da diária de seu aluguel
;a partir de suas características.

;Exemplo:
;(cdc car1) = 90

(check-expect (cdc car1) 90)

(define (cdc car)
  (cond
    [(carro-ar_cond car)
     (if (carro-direcao_hidr car)
         (if (carro-vidro_eletrico car)
             (+ 90 (* 0.0025 (carro-valor car)))
             (+ 60 (* 0.0025 (carro-valor car)))
             )
         (+ 30 (* 0.0025 (carro-valor car)))
         )
     ]
    
    [(carro-direcao_hidr car)
     (if (carro-vidro_eletrico car)
         (+ 60 (* 0.0025 (carro-valor car)))
         (+ 30 (* 0.0025 (carro-valor car)))
         )
     ]
    
    [(carro-vidro_eletrico car) (+ 30 (* 0.0025 (carro-valor car)))]
    
    [else (* 0.0025 (carro-valor car))]
    )

  )

;cdm : moto -> número
;Dada uma instância da estrutura moto, calcula-se o preço da diária de seu aluguel
;a partir de suas características.

;Exemplo:
;(cdm motinha) = 140

(check-expect (cdm motinha) 140)

(define (cdm bike)
  (if (< (moto-ano bike) 2011)
      (+ 70 (* 0.0025 (moto-valor bike)))
      (+ 90 (* 0.0025 (moto-valor bike)))
      )
  )

;------------------------------------------------------------------------------------------------
(define PI 3.14)

(define ponto1 (make-posn 0 0))
(define ponto2 (make-posn 0 3))


;area: posn posn -> número
;Dados dois pontos no plano cartesiano, sendo o primeiro correspondente ao centro de um círculo
;e o segundo, a um ponto em sua borda, calcula-se a área da figura

(check-expect (area ponto1 ponto2) 28.26)


(define (area ponto1 ponto2)
  (*
   PI
   (+
    (sqr(- (posn-x ponto2) (posn-x ponto1)))
    (sqr(- (posn-y ponto2) (posn-y ponto1)))
      )
   )
  
  )









