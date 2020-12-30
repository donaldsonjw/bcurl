(module bcurl/mime
   (import bcurl/curl
           bcurl/easy)
   (export
      (final-class %mime
         handle)
      (make-mime::%mime easy::%easy)
      (mime-addpart-w/string! mime::%mime name::bstring data::bstring
         #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
      (mime-addpart-w/filedata! mime::%mime name::bstring filename::bstring
         #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
      (mime-addpart-w/input-port! mime::%mime name::bstring port::input-port
         #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
      (mime-addsubparts! mime::%mime name::bstring subparts::%mime
         #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
      (easy-mime-data-set! easy::%easy mime::%mime)))

(define (make-mime::%mime easy::%easy)
   (let ((res (instantiate::%mime (handle (make-curl-mime (-> easy handle))))))
      (curl-cleanup-handler-add! (-> easy handle)
         (lambda () (when res
                  (curl-mime-free! res)
                  (set! res #f))))
      res))

(define (mime-addpart-w/string! mime::%mime name::bstring data::bstring
           #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
   (let ((part (curl-mime-addpart! (-> mime handle))))
      (curl-mime-name! part name)
      (curl-mime-data! part data)
      (when (not (string=? "" type))
         (curl-mime-type! part type))
      (when (not (null? headers))
         (curl-mime-headers! part headers))
      (when (not (string=? "" filename))
         (curl-mime-filename! part filename))
      #unspecified))

(define (mime-addpart-w/filedata! mime::%mime name::bstring filename::bstring
           #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
   (let ((part (curl-mime-addpart! (-> mime handle))) )
      (curl-mime-name! part name)
      (curl-mime-filedata! part filename)
      (when (not (string=? "" type))
         (curl-mime-type! part type))
      (when (not (null? headers))
         (curl-mime-headers! part headers))
      (when (not (string=? "" filename))
         (curl-mime-filename! part filename))
      #unspecified))

(define (mime-addpart-w/input-port! mime::%mime name::bstring port::input-port
           #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
   (let ((part (curl-mime-addpart! (-> mime handle))))
      (curl-mime-name! part name)
      (curl-mime-data-port! part port)
      (when (not (string=? "" type))
         (curl-mime-type! part type))
      (when (not (null? headers))
         (curl-mime-headers! part headers))
      (when (not (string=? "" filename))
         (curl-mime-filename! part filename))
      #unspecified))

(define (mime-addsubparts! mime::%mime name::bstring subparts::%mime
           #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
   (let ((part (curl-mime-addpart! (-> mime handle))))
      (curl-mime-name! part name)
      (curl-mime-subparts! part subparts)
      (when (not (string=? "" type))
         (curl-mime-type! part type))
      (when (not (null? headers))
         (curl-mime-headers! part headers))
      (when (not (string=? "" filename))
         (curl-mime-filename! part filename))
      #unspecified))

(define (easy-mime-data-set! easy::%easy mime::%mime)
   (curl-option-set! (-> easy handle) 'mimepost (-> mime handle)))


