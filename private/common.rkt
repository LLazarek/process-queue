#lang at-exp racket/base

(provide current-process-Q-polling-period-seconds)

(define current-process-Q-polling-period-seconds (make-parameter 1))
