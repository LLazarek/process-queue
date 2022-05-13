#lang at-exp racket

(provide (except-out (struct-out process-queue)
                     process-queue-empty?
                     process-queue-enqueue
                     process-queue-wait
                     process-queue-active-count
                     process-queue-waiting-count
                     process-queue-data
                     process-queue-get-data
                     process-queue-set-data)
         (struct-out process-info)
         (contract-out
          [rename short:process-queue-empty? process-queue-empty?
                  empty?/c]
          [rename short:process-queue-enqueue process-queue-enqueue
                  enqueue/c]
          [rename short:process-queue-wait process-queue-wait
                  wait/c]
          [rename short:process-queue-active-count process-queue-active-count
                  active-count/c]
          [rename short:process-queue-waiting-count process-queue-waiting-count
                  waiting-count/c]
          [rename short:process-queue-get-data process-queue-get-data
                  get-data/c]
          [rename short:process-queue-set-data process-queue-set-data
                  set-data/c]

          [process-queue/c (contract? . -> . contract?)]

          [process-will/c contract?]
          [process-info/c contract?]))

(module+ internal
  (provide process-queue-data))

(require syntax/parse/define
         (for-syntax racket/syntax))

(struct process-queue (empty?
                   enqueue
                   wait
                   active-count
                   waiting-count
                   get-data
                   set-data

                   data))

(struct process-info (data ctl will) #:transparent)
(define process-will/c (process-queue? process-info? . -> . process-queue?))
(define process-info/c
  (struct/dc process-info
             [data any/c]
             [ctl ((or/c 'status 'wait 'interrupt 'kill) . -> . any)]
             [will process-will/c]))

(define empty?/c (process-queue? . -> . boolean?))
(define enqueue/c ({process-queue? (-> process-info/c)}
                   {any/c}
                   . ->* .
                   process-queue?))
(define wait/c (process-queue? . -> . (and/c process-queue? process-queue-empty?)))
(define active-count/c (process-queue? . -> . natural?))
(define waiting-count/c (process-queue? . -> . natural?))
(define get-data/c (process-queue? . -> . any/c))
(define set-data/c (process-queue? any/c . -> . process-queue?))

(define (process-queue/c data/c)
  (struct/dc process-queue
             [empty?         empty?/c]
             [enqueue        enqueue/c]
             [wait           wait/c]
             [active-count   active-count/c]
             [waiting-count  waiting-count/c]
             [get-data       get-data/c]
             [set-data       set-data/c]

             [data           (or/c data/c
                                   (box/c data/c))]))

(define-simple-macro (define-method-shorthands prefix:id [field-name:id ...])
  #:with [accessor ...] (map (λ (field-name) (format-id this-syntax
                                                        "process-queue-~a"
                                                        field-name))
                             (syntax-e #'[field-name ...]))
  #:with [shorthand-id ...] (map (λ (accessor) (format-id this-syntax
                                                          "~a~a"
                                                          #'prefix accessor))
                                 (syntax-e #'[accessor ...]))
  (begin
    (define (shorthand-id a-proc-q . other-args)
      (apply (accessor a-proc-q) a-proc-q other-args))
    ...))

(define-method-shorthands short:
  [empty?
   enqueue
   wait
   active-count
   waiting-count
   get-data
   set-data])
