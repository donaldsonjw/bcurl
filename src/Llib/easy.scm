(module bcurl/easy
   (import bcurl/curl)
   (export
      (easy-auth-type-set! easy auth-types::pair-nil)
      (easy-cainfo-set! easy ca-file::string)
      (easy-connection-timeout-set! easy timeout-in-seconds::long)
      (easy-cookie-file-set! easy filename::string)
      (easy-cookie-jar-set! easy filename::string)
      (easy-cookies-set! easy cookies::pair-nil)
      (easy-effective-url easy)
      (easy-follow-location-set! easy follow-location::bool)
      (easy-input-port-set! easy input::input-port)
      (easy-max-redirects-set! easy max-redirects::long)
      (easy-method-set! easy method::symbol)
      (easy-name-and-password-set! easy username::string password::string)
      (easy-onreceive-header-set! easy proc::procedure)
      (easy-output-port-set! easy output::output-port)
      (easy-perform! easy)
      (easy-proxy-cainfo-set! easy ca-file::string)
      (easy-proxy-set! easy proxy::string)
      (easy-proxy-tls-cert-set! easy cert::string #!optional (cert-type::symbol 'PEM))
      (easy-proxy-tls-key-set! easy key::string #!optional (key-type::symbol 'PEM))
      (easy-request-headers-set! easy headers::pair-nil)
      (easy-request-timeout-set! easy timeout-in-seconds::long)
      (easy-response-code easy)
      (easy-shutdown! easy)
      (easy-ssl-engines easy)
      (easy-tls-cert-set! easy cert::string #!optional (cert-type::symbol 'PEM))
      (easy-tls-key-set! easy key::string #!optional (key-type::symbol 'PEM))
      (easy-total-time easy)
      (easy-url-set! easy url::string)
      (make-easy #!optional (url::string ""))
      (easy-effective-method easy)
      (easy-http-connectcode easy)
      (easy-http-version easy)
      (easy-filetime easy)
      (easy-namelookup-time easy)
      (easy-connect-time easy)
      (easy-appconnect-time easy)
      (easy-pretransfer-time easy)
      (easy-starttransfer-time easy)
      (easy-redirect-time easy)
      (easy-redirect-count easy)
      (easy-redirect-url easy)
      (easy-size-upload easy)
      (easy-size-download easy)
      (easy-speed-download easy)
      (easy-header-size easy)
      (easy-request-size easy)
      (easy-ssl-verifyresult easy)
      (easy-proxy-error easy)
      (easy-proxy-ssl-verifyresult easy)
      (easy-content-length-download easy)
      (easy-content-length-upload easy)
      (easy-content-type easy)
      (easy-retry-after easy)
      (easy-httpauth-avail easy)
      (easy-proxyauth-avail easy)
      (easy-os-errno easy)
      (easy-num-connects easy)
      (easy-local-ip easy)
      (easy-local-port easy)
      (easy-cookielist easy)
      (easy-ftp-entry-path easy)
      (easy-certinfo easy)))

(define-syntax if-let*
   (syntax-rules ()
      ((_ ((var binding) ...) then-body  else-body)
       (let* ((var binding) ...)
          (if (and var ...)
              then-body
              else-body)))))

(define (easy-effective-url easy)
   (curl-get-info easy 'effective-url))

(define (easy-response-code easy)
   (curl-get-info easy 'response-code))

(define (easy-total-time easy)
   (curl-get-info easy 'total-time-t))

(define (easy-ssl-engines easy)
   (curl-get-info easy 'ssl-engines))

(define (easy-effective-method easy)
   (curl-get-info easy 'effective-method))

(define (easy-http-connectcode easy)
   (curl-get-info easy 'http-connectcode))

(define (easy-http-version easy)
   (curl-get-info easy 'http-version))

(define (easy-filetime easy)
   (curl-get-info easy 'filetime))

(define (easy-namelookup-time easy)
   (curl-get-info easy 'namelookup-time))

(define (easy-connect-time easy)
   (curl-get-info easy 'connect-time))

(define (easy-appconnect-time easy)
   (curl-get-info easy 'appconnect-time))

(define (easy-pretransfer-time easy)
   (curl-get-info easy 'pretransfer-time))

(define (easy-starttransfer-time easy)
   (curl-get-info easy 'starttransfer-time))

(define (easy-redirect-time easy)
   (curl-get-info easy 'redirect-time))

(define (easy-redirect-count easy)
   (curl-get-info easy 'redirect-count))

(define (easy-redirect-url easy)
   (curl-get-info easy 'redirect-url))

(define (easy-size-upload easy)
   (curl-get-info easy 'size-upload))

(define (easy-size-download easy)
   (curl-get-info easy 'size-download))

(define (easy-speed-download easy)
   (curl-get-info easy 'speed-download))

(define (easy-speed-upload easy)
   (curl-get-info easy 'speed-upload))

(define (easy-header-size easy)
   (curl-get-info easy 'header-size))

(define (easy-request-size easy)
   (curl-get-info easy 'request-size))

(define (easy-ssl-verifyresult easy)
   (curl-get-info easy 'ssl-verifyresult))

(define (easy-proxy-error easy)
   (curl-get-info easy 'proxy-error))

(define (easy-proxy-ssl-verifyresult easy)
   (curl-get-info easy 'proxy-ssl-verifyresult))

(define (easy-content-length-download easy)
   (curl-get-info easy 'content-length-download-t))

(define (easy-content-length-upload easy)
   (curl-get-info easy 'content-length-upload-t))

(define (easy-content-type easy)
   (curl-get-info easy 'content-type))

(define (easy-retry-after easy)
   (curl-get-info easy 'retry-after))

(define (easy-httpauth-avail easy)
   (curl-get-info easy 'httpauth-avail))

(define (easy-proxyauth-avail easy)
   (curl-get-info easy 'proxyauth-avail))

(define (easy-os-errno easy)
   (curl-get-info easy 'os-errno))

(define (easy-num-connects easy)
   (curl-get-info easy 'num-connects))

(define (easy-primary-ip easy)
   (curl-get-info easy 'primary-ip))

(define (easy-local-ip easy)
   (curl-get-info easy 'local-ip))

(define (easy-local-port easy)
   (curl-get-info easy 'local-port))

(define (easy-cookielist easy)
   (curl-get-info easy 'cookielist))

(define (easy-ftp-entry-path easy)
   (curl-get-info easy 'ftp-entry-path))

(define (easy-certinfo easy)
   (curl-get-info easy 'certinfo))

(define (make-easy #!optional (url::string ""))
   (let ((easy (make-curl)))
      (when (not (string=? url ""))
         (easy-url-set! easy url))
      easy))

(define (easy-shutdown! easy)
   (curl-cleanup! easy))

(define (easy-url-set! easy url::string)
   (curl-option-set! easy 'url url)
   easy)

(define (easy-perform! easy)
   (curl-perform! easy))

(define (easy-method-set! easy method::symbol)
   (let ((handle easy))
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

(define (easy-input-port-set! easy input::input-port)
   (curl-option-set! easy 'readdata input)
   easy)

(define (easy-output-port-set! easy output::output-port)
   (curl-option-set! easy 'writedata output)
   easy)

(define (easy-request-headers-set! easy headers::pair-nil)
   (let ((slist-headers (header-list->curl-slist headers)))
      (curl-option-set! easy 'header
         slist-headers))
   easy)

(define (easy-cainfo-set! easy ca-file::string)
   (curl-option-set! easy 'cainfo ca-file)
   easy)

(define (easy-proxy-cainfo-set! easy ca-file::string)
   (curl-option-set! easy 'proxy-cainfo ca-file)
   easy)

(define (easy-tls-cert-set! easy cert::string #!optional (cert-type::symbol 'PEM))
   (let ((type::string (cond ((eq? 'PEM cert-type)
                              "PEM")
                             ((eq? 'DER cert-type)
                              "DER")
                             ((eq? 'P12 cert-type)
                              "P12")
                             (else
                              (error "easy-tls-cert-type-set!"
                                 "unsupported Certificate type" cert-type)))))
      (curl-option-set! easy 'sslcerttype type)
      (curl-option-set! easy 'sslcert cert))
   easy)

(define (easy-tls-key-set! easy key::string #!optional (key-type::symbol 'PEM))
   (let ((type::string (cond ((eq? 'PEM key-type)
                              "PEM")
                             ((eq? 'DER key-type)
                              "DER")
                             ((eq? 'ENG key-type)
                              "ENG")
                             (else
                              (error "easy-tls-key-set!"
                                 "unsupported key type" key-type)))))
      (curl-option-set! easy 'sslkeytype type)
      (curl-option-set! easy 'sslkey key))
   easy)

(define (easy-proxy-set! easy proxy::string)
   (curl-option-set! easy 'proxy proxy)
   easy)


(define (easy-proxy-tls-cert-set! easy cert::string #!optional (cert-type::symbol 'PEM))
   (let ((type::string (cond ((eq? 'PEM cert-type)
                              "PEM")
                             ((eq? 'DER cert-type)
                              "DER")
                             ((eq? 'P12 cert-type)
                              "P12")
                             (else
                              (error "easy-proxy-tls-cert-type-set!"
                                 "unsupported Certificate type" cert-type)))))
      (curl-option-set! easy 'proxy-sslcerttype type)
      (curl-option-set! easy 'proxy-sslcert cert))
   easy)

(define (easy-proxy-tls-key-set! easy key::string #!optional (key-type::symbol 'PEM))
   (let ((type::string (cond ((eq? 'PEM key-type)
                              "PEM")
                             ((eq? 'DER key-type)
                              "DER")
                             ((eq? 'ENG key-type)
                              "ENG")
                             (else
                              (error "easy-proxy-tls-key-set!"
                                 "unsupported key type" key-type)))))
      (curl-option-set! easy 'proxy-sslkeytype type)
      (curl-option-set! easy 'proxy-sslkey key))
   easy)

(define (easy-connection-timeout-set! easy timeout-in-seconds::long)
   (curl-option-set! easy 'connecttimeout timeout-in-seconds)
   easy)

(define (easy-request-timeout-set! easy timeout-in-seconds::long)
   (curl-option-set! easy 'timeout timeout-in-seconds)
   easy)

(define (easy-follow-location-set! easy follow-location::bool)
   (curl-option-set! easy 'followlocation follow-location)
   easy)

(define (easy-max-redirects-set! easy max-redirects::long)
   (curl-option-set! easy 'maxredirs max-redirects)
   easy)

(define (easy-auth-type-set! easy auth-types::pair-nil)
   (if (every symbol? auth-types)
       (curl-option-set! easy 'easyauth auth-types)
       (error "easy-auth-type-set!" "auth-types must be a list of auth"
          auth-types))
   easy)

(define (easy-cookies-set! easy cookies::pair-nil)
   (curl-option-set! easy 'cookie (format "~(; )" cookies))
   easy)

(define (easy-cookie-jar-set! easy filename::string)
   (curl-option-set! easy 'cookiejar filename)
   easy)

(define (easy-cookie-file-set! easy filename::string)
   (curl-option-set! easy 'cookiefile filename)
   easy)

(define (easy-name-and-password-set! easy username::string password::string)
   (curl-option-set! easy 'username username)
   (curl-option-set! easy 'password password)
   easy)

(define +easy-status-regexp+ "EASY/\\d(\.\\d)?\\s([1-5]\\d\\d)\\s(.*)\\r\\n")


(define (parse-header-status header-line)
   (pregexp-match +easy-status-regexp+ header-line))

(define +easy-header-regexp+ "(.+): (.*)\r\n")

(define (parse-header-line header-line)
   (and-let* ((res (pregexp-match +easy-header-regexp+ header-line)))
      (values (list-ref res 1) (list-ref res 2))))

(define (easy-onreceive-header-set! easy proc::procedure)
   (let ((out (open-output-procedure
                  (lambda (line)
                     (proc line)))))
      (curl-option-set! easy 'headerdata out)
      easy))

