(module bcurl/easy
   (import bcurl/curl)
   (export
      (make-easy::%easy #!optional (url::string ""))
      (easy-perform! easy::%easy)
      (easy-shutdown! easy::%easy)
      (easy-url-set!::%easy easy::%easy url::string)
      (easy-method-set!::%easy easy::%easy method::symbol)
      (easy-input-port-set!::%easy easy::%easy input::input-port)
      (easy-output-port-set!::%easy easy::%easy output::output-port)
      (easy-cainfo-set!::%easy easy::%easy ca-file::string)
      (easy-tls-cert-set!::%easy easy::%easy cert::string #!optional (cert-type::symbol 'PEM))
      (easy-tls-key-set!::%easy easy::%easy key::string #!optional (key-type::symbol 'PEM))
      (easy-proxy-cainfo-set!::%easy easy::%easy ca-file::string)
      (easy-proxy-set!::%easy easy::%easy proxy::string)
      (easy-proxy-tls-key-set!::%easy easy::%easy key::string #!optional (key-type::symbol 'PEM))
      (easy-proxy-tls-cert-set!::%easy easy::%easy cert::string #!optional (cert-type::symbol 'PEM))
      (easy-connection-timeout-set!::%easy easy::%easy timeout-in-seconds::long)
      (easy-request-timeout-set!::%easy easy::%easy timeout-in-seconds::long)
      (easy-follow-location-set!::%easy easy::%easy follow-location::bool)
      (easy-max-redirects-set!::%easy easy::%easy max-redirects::long)
      (easy-auth-type-set!::%easy easy::%easy auth-types::pair-nil)
      (easy-name-and-password-set!::%easy easy::%easy username::string password::string)
      (easy-request-headers-set!::%easy easy::%easy headers::pair-nil)
      (easy-onreceive-header-set!::%easy easy::%easy proc::procedure)
      (easy-status easy::%easy)
      (easy-cookies-set!::%easy easy::%easy cookies::pair-nil)
      (easy-cookie-jar-set!::%easy easy::%easy filename::string)
      (easy-cookie-file-set!::%easy easy::%easy filename::string)
      (easy-effective-url easy::%easy)
      (easy-response-code easy::%easy)
      (easy-total-time easy::%easy)
      (easy-ssl-engines easy::%easy)
      (class %easy
         handle
         out-headers
         in-headers
         status)))


(define-syntax if-let*
   (syntax-rules ()
      ((_ ((var binding) ...) then-body  else-body)
       (let* ((var binding) ...)
          (if (and var ...)
              then-body
              else-body)))))


(define (easy-effective-url easy::%easy)
   (curl-get-info (-> easy handle) 'effective-url))

(define (easy-response-code easy::%easy)
   (curl-get-info (-> easy handle) 'response-code))

(define (easy-total-time easy::%easy)
   (curl-get-info (-> easy handle) 'total-time-t))

(define (easy-ssl-engines easy::%easy)
   (curl-get-info (-> easy handle) 'ssl-engines))


(define (make-easy #!optional (url::string ""))
   (let ((easy::%easy (instantiate::%easy (handle (make-curl))
                                          (out-headers (make-null-curl-slist*))
                                          (in-headers '())
                                          (status #unspecified))))
      (curl-cleanup-handler-add! (-> easy handle)
         (lambda ()
            (when (not (curl-slist*-null? (-> easy out-headers)))
               (curl-slist-free (-> easy out-headers))
               (set! (-> easy out-headers) (make-null-curl-slist*)))))
      (when (not (string=? url ""))
         (easy-url-set! easy url))
      easy))

(define (easy-shutdown! easy::%easy)
   (curl-cleanup! (-> easy handle)))

(define (easy-url-set! easy url::string)
   (curl-option-set! (-> easy handle) 'url url)
   easy)

(define (easy-perform! easy::%easy)
   (curl-perform! (-> easy handle)))

(define (easy-status easy::%easy)
   (-> easy status))

(define (easy-method-set! easy::%easy method::symbol)
   (let ((handle (-> easy handle)))
      (case method
         ((head)
          (curl-option-set! handle 'nobody #t))
         ((get)
          (curl-option-set! handle 'easyget #t))
         ((post)
          (curl-option-set! handle 'post #t))
         ((put)
          (curl-option-set! handle 'upload #t))
         (else
          (error "easy-method-set!" "unsupported easy method" method))))
   easy)

(define (easy-input-port-set! easy::%easy input::input-port)
   (curl-option-set! (-> easy handle) 'readdata input)
   easy)

(define (easy-output-port-set! easy::%easy output::output-port)
   (curl-option-set! (-> easy handle) 'writedata output)
   easy)

(define (easy-request-headers-set! easy::%easy headers::pair-nil)
   (curl-slist-free (-> easy out-headers))
   (set! (-> easy out-headers) (header-list->curl-slist headers))
   (curl-option-set! (-> easy handle) 'easyheader
      (-> easy out-headers))
   easy)

(define (easy-cainfo-set! easy::%easy ca-file::string)
   (curl-option-set! (-> easy handle) 'cainfo ca-file)
   easy)

(define (easy-proxy-cainfo-set! easy::%easy ca-file::string)
   (curl-option-set! (-> easy handle) 'proxy-cainfo ca-file)
   easy)

(define (easy-tls-cert-set! easy::%easy cert::string #!optional (cert-type::symbol 'PEM))
   (let ((type::string (cond ((eq? 'PEM cert-type)
                              "PEM")
                             ((eq? 'DER cert-type)
                              "DER")
                             ((eq? 'P12 cert-type)
                              "P12")
                             (else
                              (error "easy-tls-cert-type-set!"
                                 "unsupported Certificate type" cert-type)))))
      (curl-option-set! (-> easy handle) 'sslcerttype type)
      (curl-option-set! (-> easy handle) 'sslcert cert))
   easy)

(define (easy-tls-key-set! easy::%easy key::string #!optional (key-type::symbol 'PEM))
   (let ((type::string (cond ((eq? 'PEM key-type)
                              "PEM")
                             ((eq? 'DER key-type)
                              "DER")
                             ((eq? 'ENG key-type)
                              "ENG")
                             (else
                              (error "easy-tls-key-set!"
                                 "unsupported key type" key-type)))))
      (curl-option-set! (-> easy handle) 'sslkeytype type)
      (curl-option-set! (-> easy handle) 'sslkey key))
   easy)

(define (easy-proxy-set! easy::%easy proxy::string)
   (curl-option-set! (-> easy handle) 'proxy proxy)
   easy)


(define (easy-proxy-tls-cert-set! easy::%easy cert::string #!optional (cert-type::symbol 'PEM))
   (let ((type::string (cond ((eq? 'PEM cert-type)
                              "PEM")
                             ((eq? 'DER cert-type)
                              "DER")
                             ((eq? 'P12 cert-type)
                              "P12")
                             (else
                              (error "easy-proxy-tls-cert-type-set!"
                                 "unsupported Certificate type" cert-type)))))
      (curl-option-set! (-> easy handle) 'proxy-sslcerttype type)
      (curl-option-set! (-> easy handle) 'proxy-sslcert cert))
   easy)

(define (easy-proxy-tls-key-set! easy::%easy key::string #!optional (key-type::symbol 'PEM))
   (let ((type::string (cond ((eq? 'PEM key-type)
                              "PEM")
                             ((eq? 'DER key-type)
                              "DER")
                             ((eq? 'ENG key-type)
                              "ENG")
                             (else
                              (error "easy-proxy-tls-key-set!"
                                 "unsupported key type" key-type)))))
      (curl-option-set! (-> easy handle) 'proxy-sslkeytype type)
      (curl-option-set! (-> easy handle) 'proxy-sslkey key))
   easy)

(define (easy-connection-timeout-set! easy::%easy timeout-in-seconds::long)
   (curl-option-set! (-> easy handle) 'connecttimeout timeout-in-seconds)
   easy)

(define (easy-request-timeout-set! easy::%easy timeout-in-seconds::long)
   (curl-option-set! (-> easy handle) 'timeout timeout-in-seconds)
   easy)

(define (easy-follow-location-set! easy::%easy follow-location::bool)
   (curl-option-set! (-> easy handle) 'followlocation follow-location)
   easy)

(define (easy-max-redirects-set! easy::%easy max-redirects::long)
   (curl-option-set! (-> easy handle) 'maxredirs max-redirects)
   easy)

(define (easy-auth-type-set! easy::%easy auth-types::pair-nil)
   (if (every symbol? auth-types)
       (curl-option-set! (-> easy handle) 'easyauth auth-types)
       (error "easy-auth-type-set!" "auth-types must be a list of auth"
          auth-types))
   easy)

(define (easy-cookies-set! easy::%easy cookies::pair-nil)
   (curl-option-set! easy 'cookie (format "~(; )" cookies))
   easy)

(define (easy-cookie-jar-set! easy::%easy filename::string)
   (curl-option-set! (-> easy handle) 'cookiejar filename)
   easy)

(define (easy-cookie-file-set! easy::%easy filename::string)
   (curl-option-set! (-> easy handle) 'cookiefile filename)
   easy)

(define (easy-name-and-password-set! easy::%easy username::string password::string)
   (curl-option-set! (-> easy handle) 'username username)
   (curl-option-set! (-> easy handle) 'password password)
   easy)

(define +easy-status-regexp+ "EASY/\\d(\.\\d)?\\s([1-5]\\d\\d)\\s(.*)\\r\\n")


(define (parse-header-status header-line)
   (pregexp-match +easy-status-regexp+ header-line))

(define +easy-header-regexp+ "(.+): (.*)\r\n")

(define (parse-header-line header-line)
   (and-let* ((res (pregexp-match +easy-header-regexp+ header-line)))
      (values (list-ref res 1) (list-ref res 2))))

(define (easy-onreceive-header-set! easy::%easy proc::procedure)
   (let ((out (open-output-procedure
                 (lambda (line)
                    (if-let* ((res (parse-header-status line)))
                       (begin
                          (set! (-> easy in-headers) '())
                          (set! (-> easy status)
                             (cons (list-ref res 2) (list-ref res 3))))
                       (call-with-values (lambda () (parse-header-line line))
                                         (lambda (name value)
                                            (set! (-> easy in-headers)
                                               (cons (cons name value)
                                                  (-> easy in-headers)))
                                            (proc name value))))))))
      (curl-option-set! (-> easy handle) 'headerdata out)
      easy))

