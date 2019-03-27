#lang racket/base

(provide call-with-output-file*/delete)

(define (call-with-output-file*/delete path
                                       proc
                                       #:mode [mode 'binary]
                                       #:exists [exists 'error])
  (with-handlers ([exn? (λ (e)
                          (when (file-exists? path)
                            (delete-file path))
                          (raise e))])
    (call-with-output-file*
      path
      #:mode mode
      #:exists exists
      proc)))
