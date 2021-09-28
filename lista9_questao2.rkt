;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista9_questao2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct arquivo (nome tamanho)) 
(define-struct diretorio (nome conteudo))

(define ARQ1 (make-arquivo "arq1" 1))
(define ARQ2 (make-arquivo "arq2" 1))
(define ARQ3 (make-arquivo "arq3" 1))
(define ARQ4 (make-arquivo "arq4" 1))
(define ARQ5 (make-arquivo "arq5" 1))
(define ARQ6 (make-arquivo "arq6" 1))

(define CONTEUDO (list ARQ1 ARQ2 ARQ3 ARQ4))

(define SUBDIR1 (make-diretorio "subdir1" (list ARQ1 ARQ2 ARQ1 ARQ3 ARQ6 )))
(define SUBDIR2 (make-diretorio "subdir2" (list SUBDIR1)))
(define SUBDIR3 (make-diretorio "subdir3" (list SUBDIR2 )))
(define SUBDIR4 (make-diretorio "subdir4" (list SUBDIR3)))
(define SUBDIR5 (make-diretorio "subdir5" (list  ARQ3 SUBDIR3 )))
(define SUBDIR6 (make-diretorio "subdir6" (list ARQ4 ARQ2)))
(define SUBDIR7 (make-diretorio "subdir7" (list SUBDIR6)))
(define SUBDIR8 (make-diretorio "subdir8" (list  ARQ5 SUBDIR6 SUBDIR4  SUBDIR5)))

(define DIR1 (make-diretorio "dir1" (list SUBDIR1  ARQ1 ARQ2 ARQ3 ARQ4)))
(define DIR2 (make-diretorio "dir2" '()))
(define DIR3 (make-diretorio "dir3" (list SUBDIR1 SUBDIR2 SUBDIR3 SUBDIR4)))
(define DIR4 (make-diretorio "dir4" (list SUBDIR8)))

(define (mostra-caminho arq dir)
  (cond;Caso o diretorio esteja vazio, diz que o arquivo não foi encontrado.
    [(empty? (diretorio-conteudo dir)) "Arquivo não encontrado"]
    [else
     (cond
       [(arquivo-encontrado? (diretorio-conteudo dir) (arquivo-nome arq))
        (cond
          [(arquivo-encontrado-no-nivel? (diretorio-conteudo dir) (arquivo-nome arq))
           (string-append (diretorio-nome dir) "-> " (arquivo-nome arq))]
          [else
           (cond
             [(diretorio? (first (diretorio-conteudo dir)))
              (cond
                [(arquivo-encontrado?
                  (diretorio-conteudo (first (diretorio-conteudo dir)))
                  (arquivo-nome arq))
                 (string-append (diretorio-nome dir) "-> "
                 (mostra-caminho arq (first (diretorio-conteudo dir))))]
                [else
                 (mostra-caminho arq
                                 (make-diretorio
                                  (diretorio-nome dir)
                                  (rest (diretorio-conteudo dir))))])]
             [(arquivo? (first (diretorio-conteudo dir)))
              (mostra-caminho arq
                              (make-diretorio
                               (diretorio-nome dir)
                               (rest (diretorio-conteudo dir))))])
           ])]
       [else "Arquivo não encontrado"])    

     ]))

(define (arquivo-encontrado? cont nome)
  (cond
    ;Se o conteudo for vazio, retorna falso.
    [(empty? cont) #f]
    [else ;Senão,
     (cond
       ;Verifico se, no nível mais alto, existe um arquivo com aquele nome.
       [(arquivo-encontrado-no-nivel? cont nome) #t] ;Se sim, retorna #t
       [else
        ;Senão, verifico se o primeiro elemento do conteudo é diretorio
        (cond
          [(tem-subnivel-cont? cont)
           (cond
             [(diretorio? (first cont))
              (cond
                [(arquivo-encontrado-subnivel? cont nome) #t]
                [else (arquivo-encontrado? (rest cont) nome)])]
             [(arquivo? (first cont))
              (arquivo-encontrado? (rest cont) nome)])]
          [else #f])])]))

(define (arquivo-encontrado-subnivel? cont nome)
  (cond
    [(empty? cont) #f]
    [else
     (cond
       [(diretorio? (first cont))
        (arquivo-encontrado? (diretorio-conteudo (first cont)) nome)]
       [else (arquivo-encontrado? (rest cont) nome)]) ]))


(define (tem-subnivel-cont? cont)
  (cond
    [(empty? cont) #f]
    [else
     (cond
       [(diretorio? (first cont)) #t]
       [(arquivo? (first cont))
        (tem-subnivel-cont? (rest cont) )])]) )

(define (arquivo-encontrado-no-nivel? cont nome)
  (cond
    ;Se o conteudo for vazio, retorna falso.
    [(empty? cont) #f]
    [else ;Senão,
     (cond ;caso o primeiro elemento do conteudo seja um arquivo,
       [(arquivo? (first cont))
        (cond ;verifica se o nome do arquivo é igual ao nome procurado.
          ;Se sim, retorna #t
          [(string=? (arquivo-nome (first cont)) nome) #t]
          ;Se não, procura no resto do conteudo.
          [else (arquivo-encontrado-no-nivel? (rest cont) nome)])
        ]
       ;Caso o primeiro elemento do conteudo seja um diretorio
       [(diretorio? (first cont))
        ;Procura no resto do conteudo.
        (arquivo-encontrado-no-nivel? (rest cont) nome)] )]) )

