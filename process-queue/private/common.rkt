#lang at-exp racket/base

(provide current-process-queue-polling-period-seconds)

(define current-process-queue-polling-period-seconds (make-parameter 1))
