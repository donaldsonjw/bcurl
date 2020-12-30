(module bcurl/multi
   (import bcurl/curl
           bcurl/easy)
   
   (export
      (final-class %multi
         handle)
      
      (make-multi)
      (multi-add-handle! multi::%multi easy::%easy)
      (multi-remove-handle! multi::%multi easy::%easy)))


(define (make-multi)
   (instantiate::%multi (handle (make-curl-multi))))

(define (multi-add-handle! multi::%multi easy::%easy)
   (curl-multi-add-handle! (-> multi handle) (-> easy handle)))

(define (multi-remove-handle! multi::%multi easy::%easy)
   (curl-multi-remove-handle! (-> multi handle) (-> easy handle)))