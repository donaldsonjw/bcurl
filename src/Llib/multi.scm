(module bcurl/multi
   (import bcurl/curl
           bcurl/easy)
   (export      
      (make-multi)
      (multi-add-handle! multi easy)
      (multi-remove-handle! multi easy)
      (multi-perform! multi)
      (multi-info! multi)
      (multi-cleanup! multi)
      (multi-wakeup! multi)
      (multi-poll! multi timout)))


(define (make-multi)
   (make-curl-multi))

(define (multi-add-handle! multi easy)
   (curl-multi-add-handle! multi easy))

(define (multi-remove-handle! multi easy)
   (curl-multi-remove-handle! multi easy))

(define (multi-perform! multi)
   (curl-multi-perform! multi))

(define (multi-info! multi)
   (curl-multi-info-read! multi))

(define (multi-cleanup! multi)
   (curl-multi-cleanup! multi))

(define (multi-wakeup! multi)
   (curl-multi-wakeup! multi))

(define (multi-poll! multi timout)
   (curl-multi-poll! multi timout))