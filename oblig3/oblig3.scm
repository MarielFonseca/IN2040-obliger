
;; prekode
;;;; Diverse hjelpekode for innlevering 3 i IN2040, 2025,
;;;; tidlige var det for oblig3a (2020, 2022, 2023)
;;;; erikve [at] ifi.uio.no,...  

;; Tabell-abstraksjon fra seksjon 3.3.3 i SICP:

(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (and record (cdr record))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))))


;; mem-testprosedyre 1; fibonacci-tallene
(define (fib n)
  (display "computing fib of ")
  (display n) (newline)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; mem-testprosedyre 2; fakultet-tallene
(define (fac n)
  (display "computing fac of ")
  (display n) (newline)
  (if (<= n 1)
      1
      (* n (fac (- n 1)))))


;; mem-testprosedyre 3; tar virkårlig mange argumenter (null eller flere).
;; (Returnerer summen av argumentenes kvadrerte differanse fra 42.)
(define (test-proc . args)
  (display "computing test-proc of ")
  (display args) (newline)
  (if (null? args)
      0
      (+ (expt (- 42 (car args)) 2)
         (apply test-proc (cdr args)))))


;; Under definerer vi et grensesnitt som lar oss jobbe med strømmer på
;; samme måte som i seksjon 3.4 i SICP.

(define-syntax
  cons-stream
  (syntax-rules ()
    ((cons-stream head tail) (cons head (delay tail)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

;; Merk at `force' (prosedyre) og `delay' (special form) er innebygd i
;; R5RS. Konseptuelt kan vi tenke oss at de er definert som følger;
;;
;; (define (memo proc)
;;   (let ((forced? #f)
;;         (result #f))
;;     (lambda ()
;;       (if (not forced?)
;; 	  (begin (set! result (proc))
;; 		 (set! forced? #t)))
;;       result)))
;;
;; (define-syntax
;;   delay
;;   (syntax-rules ()
;;     ((delay exp) (memo (lambda () exp)))))
;;
;; (define (force promise)
;;   (promise))


;; Under følger en del hjelpeprosedyrer til de ulike deloppgave, og
;; diverse listeoperasjoner tilpasset strømmer;


;; En prosedyre som lar oss titte på de n første elementene i en strøm:
(define (show-stream stream . n)
  (define (ss-rec stream i)
    (cond ((= i 0) (display "...\n"))
          ((stream-null? stream)  (newline))
          (else (display (stream-car stream))
                (display " ")
                (ss-rec (stream-cdr stream) (- i 1)))))
  (ss-rec stream (if (null? n) 15 (car n))))


;; brukes bl.a. i oppgave 2d:

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))



;; brukes i oppgave 2a:

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;; naturlige tall / natural numbers:
(define nats (integers-starting-from 1))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-interval (+ low 1) high))))


;; nyttig prosedyre for å leke seg med eksempler fra foilene:

(define (stream-map proc . argstreams)
  (if (any? stream-null? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;; hjelpeprosedyre for basistilfellet i stream-map:
(define (any? pred list)
  (cond ((null? list) #f)
        ((pred (car list)) #t)    ;; stopp så fort vi får #t
        (else (any? pred (cdr list)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;tabellen som vi bruker til å holde på alle verdiene
(define memoized-procedures (make-table))

(define (mem message proc)
  (define (original-procedure proc)
    (lookup proc memoized-procedures))

  ;;oppgave 1a
  (cond ((equal? message 'memoize)
         (let ((cache (make-table)))
           ;;lager prosedyre med 0+ argumenter
           (let ((memoized-proc
                  (lambda args
                    (let ((record (assoc args (cdr cache))))
                      (if record
                          ;;hvis resultatet allerede ligger i cache:
                          (cdr record)
                          ;;beregner og lagrer resultatet
                          (let ((result (apply proc args)))
                            (insert! args result cache)
                            result))))))
             ;;husker hvilken prosedyre den tilhører
             (insert! memoized-proc proc memoized-procedures)
             memoized-proc)))
        ;;oppgave 1b
        ((equal? message 'unmemoize)
         (let ((original (original-procedure proc)))
           (if original
               original
               proc)))
        (else
         ('Ukjent mem innhentet))))


;; oppgave 1c
;; set! overskriver den globale bindingen fib med den memoiserte prosedyren.
;; i den nye måten å bruke mem på
;; slår rekursjonen i fib opp navnet fib
;; i det globale miljøet hver gang den kaller seg selv.


;; oppgave 2a
(define (list-to-stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (cdr lst))))


(define (stream-to-list stream . arguments)

  (let ((limit
         (if (null? arguments)
             #f
             (car arguments))))

    (cond
      ((stream-null? stream) '())
      ((and limit (zero? limit)) '())
      (else
       (cons (stream-car stream)
             (stream-to-list (stream-cdr stream)
                             (if limit (- limit 1) #f)))))))

;; oppgave 2b
(define (stream-take n stream) ;; returnerer en ny strøm med de n første elementene
  ;;(let ((current) n))
  (if (or (zero? n) (stream-null? stream))
      the-empty-stream
      (cons-stream
       (stream-car stream)
       (stream-take (- n 1)
                    (stream-cdr stream)))))


;; oppgave 2c
;; uhhh


;; oppgave 2d
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream) 
                   (remove-duplicates
                    (stream-filter
                     (lambda (x)
                       (not (= x (stream-car stream))))
                     (stream-cdr stream))))))

;; tester 1a og 1b:
(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)

;; tester 2a:
(list-to-stream '(1 2 3 4 5))
(stream-to-list (stream-interval 10 20))
  

;; tester 2b:
(define foo (stream-take 10 nats))
foo
