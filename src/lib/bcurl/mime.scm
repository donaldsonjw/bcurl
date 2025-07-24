(module bcurl/mime
   (import bcurl/curl
           bcurl/easy)
   (export
      (make-mime easy)
      (mime-addpart-w/string! mime name::bstring data::bstring
         #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
      (mime-addpart-w/filedata! mime name::bstring filename::bstring
         #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
      (mime-addpart-w/input-port! mime name::bstring port::input-port
         #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
      (mime-addsubparts! mime name::bstring subparts
         #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
      (easy-mime-data-set! easy mime)))

(define (make-mime easy)
   (let ((res (make-curl-mime easy)))
      (curl-cleanup-handler-add! easy
         (lambda () (when res
                  (curl-mime-free! res)
                  (set! res #f))))
      res))

(define (mime-addpart-w/string! mime name::bstring data::bstring
           #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
   (let ((part (curl-mime-addpart! mime)))
      (curl-mime-name! part name)
      (curl-mime-data! part data)
      (when (not (string=? "" type))
         (curl-mime-type! part type))
      (when (not (null? headers))
         (curl-mime-headers! part headers))
      (when (not (string=? "" filename))
         (curl-mime-filename! part filename))
      #unspecified))

(define (mime-addpart-w/filedata! mime name::bstring filename::bstring
           #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
   (let ((part (curl-mime-addpart! mime)) )
      (curl-mime-name! part name)
      (curl-mime-filedata! part filename)
      (when (not (string=? "" type))
         (curl-mime-type! part type))
      (when (not (null? headers))
         (curl-mime-headers! part headers))
      (when (not (string=? "" filename))
         (curl-mime-filename! part filename))
      #unspecified))

(define (mime-addpart-w/input-port! mime name::bstring port::input-port
           #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
   (let ((part (curl-mime-addpart! mime)))
      (curl-mime-name! part name)
      (curl-mime-data-port! part port)
      (when (not (string=? "" type))
         (curl-mime-type! part type))
      (when (not (null? headers))
         (curl-mime-headers! part headers))
      (when (not (string=? "" filename))
         (curl-mime-filename! part filename))
      #unspecified))

(define (mime-addsubparts! mime name::bstring subparts
           #!optional (type::bstring "") (headers::pair-nil '()) (filename::bstring ""))
   (let ((part (curl-mime-addpart! mime)))
      (curl-mime-name! part name)
      (curl-mime-subparts! part subparts)
      (when (not (string=? "" type))
         (curl-mime-type! part type))
      (when (not (null? headers))
         (curl-mime-headers! part headers))
      (when (not (string=? "" filename))
         (curl-mime-filename! part filename))
      #unspecified))

(define (easy-mime-data-set! easy mime)
   (curl-option-set! easy 'mimepost mime))


