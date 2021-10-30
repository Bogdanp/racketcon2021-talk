#lang racket/base

(require debugging/server
         racket/random)

(define stop (serve))
(thread
 (lambda ()
   (let loop ([bs #""])
     (sleep (random 5))
     (loop (make-bytes (* (random 128) 1024 1024))))))
(with-handlers ([exn:break? (λ (_)
                              (stop))])
  (sync never-evt))
