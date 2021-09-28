;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ListaCap14-RafaelEberhardt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;--------------------------------------------------------------------------------------
;Questão 1)

; Um elemento do conjunto Data é um elemento do cjto. NumerosInteiros
;; d1<=d2? : Data Data -> Boolean
;; dadas 2 datas (apenas o ano), verifica se a primeira é menor ou igual a segunda
;; Exemplos:;: (d1<=d2? 2014 2014) = true
;; (d1<=d2? 2014 2013) = false
(define (d1<=d2? d1 d2)
(<= d1 d2)
  )

;--------------------------------------------------------------------------------------
;Questão 2)

(define-struct filho (pai mãe nome data olhos))
; Um elemento nó de um conjunto Nó (de uma árvore genealógica) é:
; empty, representando falta de informação ou
; (make-filho p m n d o), onde:
;     p: filho, representando o pai da instância filho referida
;     m: filho, representando a mãe da instância filho referida
;     n: string, representando o nome de filho
;     d: número, representa a data de nascimento de filho 
;     o: string, representa a cor dos olhos de filho

(define Althea (make-filho empty empty 'Althea 1915 'brown))
(define Jack (make-filho empty empty 'Jack 1948 'brown))
(define Judy (make-filho empty Althea 'Judy 1945 'green))
(define Monica (make-filho Jack Judy 'Monica 1968 'blue))
(define Ross (make-filho Jack Judy 'Ross 1966 'brown))
(define Sandra (make-filho empty empty 'Sandra 1947 'brown))
(define Leonard (make-filho empty empty 'Leonard 1947 'brown))
(define Rachel (make-filho Leonard Sandra 'Rachel 1969 'blue))
(define Nora (make-filho empty empty 'Nora 1948 'blue))
(define Charles (make-filho empty empty 'Charles 1948 'blue))
(define Chandler (make-filho Charles Nora 'Chandler 1966 'blue))
(define GloriaTribbiani (make-filho empty empty 'GloriaTribbiani 1950 'brown))
(define MrTribbiani (make-filho empty empty 'MrTribbiani 1949 'brown))
(define Joey (make-filho MrTribbiani GloriaTribbiani 'Joey 1969 'brown))
(define Frank (make-filho empty empty 'Frank 1940 'brown))
(define LilyBuffay (make-filho empty empty 'LilyBuffay 1940 'blue))
(define Phoebe (make-filho Frank LilyBuffay 'Phoebe 1965 'blue))
(define Carol (make-filho empty empty 'Carol 1965 'blue))
(define Ben (make-filho Ross Carol 'Ben 1994 'blue))
(define Emma (make-filho Ross Rachel 'Emma 2002 'blue))

;--------------------------------------------------------------------------------------
;Questão 3)

(define (tempai? pessoa)
  (cond
    [(empty? pessoa) #false]
    [else (empty? (filho-pai pessoa))])
  )

(define (temmae? pessoa)
  (cond
    [(empty? pessoa) #false]
    [else (empty? (filho-mãe pessoa))])
  )

(define (teste pessoa)
  (cond
    [(empty? pessoa) '()]
    [else
     (cond ;Verifico se a pessoa tem ambos os pais
       [(and
         (tempai? pessoa)
         (temmae? pessoa)
         ) ;Tendo ambos, verifico qual o mais velho aplicando o programa sobre.
        (cond
          [(d1<=d2?
            (teste (filho-pai pessoa))
            (teste (filho-mãe pessoa))
            )
           (teste (filho-mãe pessoa))]
          [else (teste (filho-pai pessoa))]
          )         
        ]
       [else
        (cond
          [(tempai? pessoa) (teste (filho-pai pessoa))]
          [(temmae? pessoa) (teste (filho-mãe pessoa))]
          [else pessoa]
          )
        ]
       )
     ]
    )
)

;(define (maisIdoso pessoa)
;  (cond
;    [(empty? pessoa) '()]
;    [else
;     (cond
;       [(and
;         (empty? (filho-pai pessoa))
;         (empty? (filho-mãe pessoa)))
;        pessoa] ;A pessoa não tem pai ou mãe, então acaba nela.
;       ;A pessoa tem ou pai ou mãe:       
;       [(cond
;          ;Tem mãe? Então, verificar se ela tem pais.
;          [(empty? (filho-pai pessoa))
;           (and
;         (empty? (filho-pai pessoa))
;         (empty? (filho-mãe pessoa)));

;
;           ]
;          ;Tem pai? Então, verificar se ele tem pais.
;          []
;          ;Caso nenhuma acima, verificar quem é mais velho.
;          []
;          )
;        ]
;       
;       [(and
;         (and
;               (empty? (filho-pai (filho-pai pessoa)))
;               (empty? (filho-mãe (filho-pai pessoa))))
;         (and
;               (empty? (filho-pai (filho-mãe pessoa)))
;               (empty? (filho-mãe (filho-mãe pessoa))))
;         )
;        (cond
;          [
;           (d1<=d2? (filho-data ())
;                       (filho-data ())
;                       )
;           
;           ]
;          );;
;
;        ;];;
;


        ;se ambod os pais não tiverem pais, verificar o mais velho]
  ;     [aso nenhuma  dessas valer, então ]
 ;      [else
 ;       (cond        
  ;        [(empty? (filho-pai pessoa)) (maisIdoso (filho-mãe pessoa))]
  ;        [(empty? (filho-mãe pessoa)) (maisIdoso (filho-pai pessoa))]
    ;      [else ;É porque há os dois pais.
   ;        (cond
    ;         [
    ;          (and
    ;           (empty? (filho-pai (filho-pai pessoa)))
    ;           (empty? (filho-mãe (filho-pai pessoa))))
    ;          (maisIdoso (filho-mãe (filho-pai pessoa)))
    ;          ]
    ;         [
    ;          (and
    ;           (empty? (filho-pai (filho-mãe pessoa)))
    ;           (empty? (filho-mãe (filho-mãe pessoa))))
    ;          (maisIdoso (filho-pai (filho-mãe pessoa)))
    ;          ];

             
    ;         [(d1<=d2? (filho-data (maisIdoso (filho-pai pessoa)))
     ;                  (filho-data (maisIdoso (filho-mãe pessoa))))
      ;        (maisIdoso (filho-mãe pessoa))]
       ;      [else (maisIdoso (filho-pai pessoa))]
        ;     )
         ;  ]          
         ; )
        ;]
       ;)
    ; ]     
   ; )
  ;)

  