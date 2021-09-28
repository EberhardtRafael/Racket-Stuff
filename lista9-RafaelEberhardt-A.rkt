;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista9-RafaelEberhardt-A) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Nome: Rafael Eberhardt

;; ============================================
;; DEFINIÇÕES DE TIPOS DE DADOS
;; ============================================

;; -----------------
;; TIPO ARQUIVO:
;; -----------------
(define-struct arquivo (nome tamanho)) 
;; Um elemento do conjunto Arquivo tem o formato
;;  (make-arquivo n t), onde
;;     n: String, é o nome do arquivo
;;     t: Número, é o tamanho do arquivo

;; -----------------
;; TIPO CONTEUDO:
;; -----------------
;; Um Conteudo é
;; 1. empty,
;; 2. (cons a lc), onde a: Arquivo e lc: Conteudo
;; 3. (cons d lc), onde d: Diretorio e lc: Conteudo

;; -----------------
;; TIPO DIRETORIO:
;; -----------------
(define-struct diretorio (nome conteudo))
;; Um elemento do conjunto Diretorio tem o formato
;;   (make-diretorio n c), onde
;;     n: String, é o nome do diretório
;;     c: Conteudo, é o conteúdo do diretório

(define ARQ1 (make-arquivo "arq1" 1))
(define ARQ2 (make-arquivo "arq2" 1))
(define ARQ3 (make-arquivo "arq3" 1))
(define ARQ4 (make-arquivo "arq4" 1))
(define ARQ5 (make-arquivo "arq5" 1))
(define ARQ6 (make-arquivo "arq6" 1))

(define CONTEUDO (list ARQ1 ARQ2 ARQ3 ARQ4))

(define SUBDIR1 (make-diretorio "subdir1" (list ARQ5 ARQ6)))
(define SUBDIR2 (make-diretorio "subdir2" '()))
(define SUBDIR3 (make-diretorio "subdir3" (list ARQ1)))
(define SUBDIR4 (make-diretorio "subdir4" (list ARQ2 ARQ3)))
(define SUBDIR5 (make-diretorio "subdir5" (list SUBDIR1 ARQ3 SUBDIR3)))
(define SUBDIR6 (make-diretorio "subdir6" (list SUBDIR5 ARQ4)))
(define SUBDIR7 (make-diretorio "subdir7" (list SUBDIR6)))
(define SUBDIR8 (make-diretorio "subdir8" (list SUBDIR5 ARQ5 SUBDIR6)))

(define DIR1 (make-diretorio "dir1" (list SUBDIR1  ARQ1 ARQ2 ARQ3 ARQ4)))
(define DIR2 (make-diretorio "dir2" '()))
(define DIR3 (make-diretorio "dir3" (list SUBDIR1 SUBDIR2 SUBDIR3 SUBDIR4)))
(define DIR4 (make-diretorio "dir4" (list SUBDIR8)))

;; ==========================================
;; DEFINIÇÕES DE FUNÇÕES
;; ==========================================

;; =========================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; =========================================

;; arquivo-encontrado-no-nivel?: conteudo -> boolean
;Obj.: dados o conteúdo de um diretório e um nome de arquivo, verifica se existe um arquivo com este nome neste conteúdo, sem considerar subdiretórios.

;Exemplos:
;(arquivo-encontrado-no-nivel? (diretorio-conteudo DIR1) "arq1") = #t
;(arquivo-encontrado-no-nivel? (diretorio-conteudo DIR1) "arq5") = #f

(check-expect (arquivo-encontrado-no-nivel? (diretorio-conteudo DIR1) "arq1") #t)
(check-expect (arquivo-encontrado-no-nivel? (diretorio-conteudo DIR1) "arq5") #f)
(check-expect (arquivo-encontrado-no-nivel? (diretorio-conteudo DIR2) "arq1") #f)

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


;; =========================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
;; =========================================

;; arquivo-encontrado? : conteudo -> boolean
;;Obj.: dados o conteúdo de um diretório e um nome de arquivo, verificase existe um arquivo com este nome neste conteúdo, considerando subdiretórios, ou seja, a função retornaverdadeiro se um arquivo com o nome dado existe na lista de conteúdo recebida, ou em qualquer diretório dalista.

;Exemplos:
;(arquivo-encontrado? (diretorio-conteudo DIR1) "arq1") = #t
;(arquivo-encontrado? (diretorio-conteudo DIR1) "arq5") = #f

(check-expect (arquivo-encontrado? (diretorio-conteudo DIR1) "arq1") #t)
(check-expect (arquivo-encontrado? (diretorio-conteudo DIR1) "arq5") #t)
(check-expect (arquivo-encontrado? (diretorio-conteudo DIR2) "arq1") #f)
(check-expect (arquivo-encontrado? (diretorio-conteudo DIR3) "arq3") #t)
(check-expect (arquivo-encontrado? (diretorio-conteudo DIR4) "arq6") #t)
(check-expect (arquivo-encontrado? (diretorio-conteudo DIR2) "arq4") #f)

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

;Computacionalmente, usar esta função é tolo.
(define (tem-subnivel-dir? dir)
  (cond
    [(empty? (diretorio-conteudo dir)) #f]
    [else
     (cond
       [(diretorio? (first (diretorio-conteudo dir))) #t]
       [(arquivo? (first (diretorio-conteudo dir)))
        (tem-subnivel-dir?
         (make-diretorio
          (diretorio-nome dir) (rest (diretorio-conteudo dir))))])]) )

;; =========================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; =========================================
;; insere-arquivo :
;Obj.: dado um diretório corrente e um arquivo, insere este arquivo neste diretório, retornando o diretório atualizado.

(check-expect (insere-arquivo DIR2 ARQ1)
              (make-diretorio "dir2" (make-arquivo "arq1" 1)))

(check-expect
 (insere-arquivo DIR1 ARQ1)
(make-diretorio
 "dir1"
 (cons
  (make-arquivo "arq1" 1)
  (cons
   (make-diretorio
    "subdir1"
    (cons
     (make-arquivo "arq5" 1)
     (cons (make-arquivo "arq6" 1) '())))
   (cons
    (cons
     (make-arquivo "arq2" 1)
     (cons
      (make-arquivo "arq3" 1)
      (cons (make-arquivo "arq4" 1) '())))
    '())))))
;Perceber que a posição de ARQ1 foi modificada dentro do diretório, ou seja, o arquivo foi substituido.

(define (insere-arquivo dir arquivo)
  (cond
    ;Caso o diretorio esteja vazio
    [(empty? (diretorio-conteudo dir))
     ;Cria um diretorio com o arquivo dado
     (make-diretorio (diretorio-nome dir) arquivo)]
    [else ;Senão
     (cond ;Caso haja um arquivo com o mesmo nome no diretorio
       [(arquivo-encontrado-no-nivel? (diretorio-conteudo dir) (arquivo-nome arquivo));Cria um diretorio contendo aquele arquivo e o resultado de (deleta-arquivo) aplicada sobre o diretorio.
        (make-diretorio
         (diretorio-nome dir)
         (cons arquivo
               (deleta-arquivo (diretorio-conteudo dir)
                               arquivo)))]
       [else ;Se o arquivo não pré existir, cria um diretorio contendo o arquivo e o conteudo do diretorio dado.
        (make-diretorio
         (diretorio-nome dir)
         (cons arquivo (diretorio-conteudo dir)))])]) )

;Constante tipo arquivo, para utiliar durante os testes.
(define ARQTESTE (make-arquivo "teste" 20))

;deleta-arquivo: conteudo arquivo -> conteudo
;Deleta um dado arquivo dentro de um dado diretorio. Retorna o diretorio sem o arquivo.
;Assume que o arquivo existe lá dentro.
(define (deleta-arquivo conteudo arquivo)
  (cond ;Caso o coneudo esteja vazio, retorna lista vazia
    [(empty? conteudo) '()]
    [else ;Caso contrário
       (cond
    [(arquivo? (first conteudo)) ;Se o primeiro elemento de conteudo for arquivo
     (cond;Caso o arquivo tenha o mesmo nome que o primeiro elemento do conteudo
       [(string=? (arquivo-nome arquivo) (arquivo-nome (first conteudo)))
        ;Junta o resto do conteudo dado com o vazio. (nem precisava fazer assim)
        (cons (deleta-arquivo (rest conteudo) arquivo)'())]
       [else;Caso contrario
        ;junta o primeir elemento com o resto da aplicação de (deleta-arquivo)
        (cons (first conteudo) (deleta-arquivo (rest conteudo) arquivo))])
     ]
    [else ;Caso o primeiro elemento do conteudo seja um diretorio
     (cons (first conteudo) (deleta-arquivo (rest conteudo) arquivo))] )]) )

;deleta-arquivo: diretorio arquivo -> diretorio
;Obj.: Deleta um arquivo de um diretorio.

(check-expect (deleta-arquivo-nivel DIR1 ARQ1)
(make-diretorio
 "dir1"
 (cons
  (make-diretorio
   "subdir1"
   (cons
    (make-arquivo "arq5" 1)
    (cons (make-arquivo "arq6" 1) '())))
  (cons
   (cons
    (make-arquivo "arq2" 1)
    (cons
     (make-arquivo "arq3" 1)
     (cons (make-arquivo "arq4" 1) '())))
   '()))))

(check-expect (deleta-arquivo-nivel DIR2 ARQ3)
(make-diretorio "dir2" '()))

;verifica se o arquivo existe no diretorio.
;Se sim, cria um diretorio com todas as características do dado, porém, sem o arquivo dado.
;Se o arquivo não estiver lá, somente retorna o diretório dado.
(define (deleta-arquivo-nivel dir arquivo)
  (cond
    ;verifica se o arquivo existe no diretorio.  
    [(arquivo-encontrado-no-nivel?
      (diretorio-conteudo dir)
      (arquivo-nome arquivo))
     (make-diretorio
      (diretorio-nome dir)
      (deleta-arquivo (diretorio-conteudo dir) arquivo))]
    [else dir]))

;; =========================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
;; =========================================

;; calcula-tamanho : diretorio -> número
;Obj.:  dado um diretório, calcula o tamanho necessário em disco paraarmazenar este diretório.


(define (calcula-tamanho dir)
  (cond
    [(empty? (diretorio-conteudo dir)) 10]
    [else 
     (cond
       [(tem-subnivel-cont? (diretorio-conteudo dir))
        (cond
          [(diretorio? (first (diretorio-conteudo dir)))
           (+ 
            (tamanho-nivel (diretorio-conteudo (first (diretorio-conteudo dir))))
            (calcula-tamanho
             (make-diretorio "prov" (rest (diretorio-conteudo dir))))
            )]
          [(arquivo? (first (diretorio-conteudo dir)))
           (+
            (arquivo-tamanho (first (diretorio-conteudo dir)))
            (calcula-tamanho
             (make-diretorio "prov" (rest (diretorio-conteudo dir)))) ;Comendo um monte d ememória aqui!
            )]
          )]
       [else (tamanho-nivel (diretorio-conteudo dir))]
       )
     ])  )


(define (tamanho-nivel conteudo)
  (cond
    [(empty? conteudo) 10]
    [else
     (cond
       [(tem-subnivel-cont? conteudo)
        (cond
          [(diretorio? (first conteudo))
           (+
              (calcula-tamanho (first conteudo))
              (tamanho-nivel (rest conteudo)))]
          [(arquivo? (first conteudo))
           (+
            (arquivo-tamanho (first conteudo))
            (tamanho-nivel (rest conteudo)))
           ])]
       [else
        (+
            (arquivo-tamanho (first conteudo))
            (tamanho-nivel (rest conteudo)))])
 ]) )

;; =========================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
;; =========================================

;; mostra-caminho :
;;Obj.: dado um nome de arquivo e um diretório, mostra o caminho paraencontrar esse arquivo, ou uma mensagem dizendo que o arquivo não esta nesse diretório



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
                               (rest (diretorio-conteudo dir))))]) ])]
       [else "Arquivo não encontrado"])]))


;; =========================================
;; 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
;; =========================================
;; visualiza-diretorio :

