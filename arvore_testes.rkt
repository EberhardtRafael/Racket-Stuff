;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arvore_testes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct pessoa(olho idade pai mae))

(define Q(make-pessoa "azul" 80 empty empty))
(define P(make-pessoa "verde" 80 empty empty))
(define O(make-pessoa "verde" 80 empty empty))
(define N(make-pessoa "azul" 80 empty empty))
(define M(make-pessoa "verde" 80 empty empty))
(define L(make-pessoa "verde" 80 empty Q))
(define K(make-pessoa "verde" 80 empty P))
(define J(make-pessoa "verde" 80 N O))
(define I(make-pessoa "verde" 80 M L))
(define H(make-pessoa "verde" 80 empty K))
(define G(make-pessoa "verde" 80 empty J))
(define F(make-pessoa "verde" 80 empty I))
(define E(make-pessoa "verde" 80 H empty))
(define D(make-pessoa "verde" 80 F G))
(define C(make-pessoa "verde" 80 E D))
(define B(make-pessoa "verde" 80 empty C))
(define A(make-pessoa "verde" 80 empty B))
(define Claudionei(make-pessoa "verde" 60 empty A))
(define Vera(make-pessoa "verde" 60 empty empty))
(define Isa(make-pessoa "verde" 60 Q empty))
(define Claudio(make-pessoa "verde" 30 empty Isa))
(define Maria(make-pessoa "verde" 30 Claudionei Vera))
(define jr (make-pessoa "verde" 10 Claudio Maria))

(define (olhoazulpai? pessoa)
  (cond
    [(empty? (pessoa-pai pessoa)) #f]
    [else (string=? (pessoa-olho (pessoa-pai pessoa)) "azul")]) )

(define (olhoazulmae? pessoa)
  (cond
    [(empty? (pessoa-mae pessoa)) #f]
    [else (string=? (pessoa-olho (pessoa-mae pessoa)) "azul")]) )

(define (olhoazulpais? pessoa)
  (cond
    [(and (empty? (pessoa-pai pessoa)) (empty? (pessoa-mae pessoa))) #f]
    [else
     (cond
       ;A pessoa não tem pai
       [(empty? (pessoa-pai pessoa)) (olhoazulmae? pessoa)]
       ;A pessoa não tem mae
       [(empty? (pessoa-mae pessoa)) (olhoazulpai? pessoa)]
       ;A pessoa tem ambos
       [else
        (or (olhoazulmae? pessoa) (olhoazulpai? pessoa))])] ) )

(define (olhoazulfamilia? pessoa)
  (cond
    [(olhoazulpais? pessoa) #t]
    [else
     (cond
       [(and (empty? (pessoa-pai pessoa)) (empty? (pessoa-mae pessoa))) #f]
       [else
        (cond
          [(empty? (pessoa-pai pessoa)) (olhoazulfamilia? (pessoa-mae pessoa))]
          [(empty? (pessoa-mae pessoa)) (olhoazulfamilia? (pessoa-pai pessoa))]
          [else
           (or
            (olhoazulfamilia? (pessoa-mae pessoa))
            (olhoazulfamilia? (pessoa-pai pessoa)))])]
       )])


  )

;;1) A pessoa pode não ter ambos os pais, então, a função retorna falso
;;2) A pessoa pode não ter pai mas ter mae, aí, a função verifica se a mae tem ohlo azul


;;3) A pessoa pode ter pai mas não ter mae, aí, a funçãpo erifica se o pai tem olho azul

;;4)A pessoa pode ter ambos os pais, aí, a função deve erificar ambos.










