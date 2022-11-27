#lang info

(define collection "process-queue")
(define build-deps '("racket-doc" "scribble-lib" "at-exp-lib"))
(define deps '("base"
               "pfds"
               "data-lib"
               "ruinit"))
(define scribblings '(("scribblings/process-queue.scrbl")))
