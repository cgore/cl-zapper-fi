(defpackage #:zapper-fi/rest-api
  (:use :common-lisp)
  (:export :*valid-networks*
           :*network-aliases*
           :canonicalized-network
           :get-gas-price
           :unknown-network-error))

(in-package :zapper-fi/rest-api)

(defvar *api* "http://api.zapper.fi/v1")

(defvar *api-cache-ttl-seconds* 60)

(defvar *api-key* "96e0cc51-a62e-42ca-acee-910ea7d2a241")

(defvar *valid-networks* '("ethereum" "binance-smart-chain" "polygon" "xdai"))

(defvar *network-aliases* (sigma/hash:populate-hash-table "binance" "binance-smart-chain"
                                                          "bsc"     "binance-smart-chain"
                                                          "eth"     "ethereum"
                                                          "ether"   "ethereum"))

(define-condition unknown-network-error (error)
  ((unknown-network :initarg :unknown-network :reader unknown-network))
  (:report (lambda (condition stream)
             (format stream "Unknown metwork ~A." (unknown-network condition)))))

(defun canonicalized-network (network)
  (cond ((null network)
         "ethereum")
        ((symbolp network)
         (string-downcase (canonicalized-network (symbol-name network))))
        ((and (stringp network)
              (member (string-downcase network) *valid-networks* :test 'string=))
         (string-downcase network))
        ((and (stringp network)
              (gethash (string-downcase network) *network-aliases*))
         (gethash (string-downcase network) *network-aliases*))
        (t (error 'unknown-network-error :unknown-network network))))

(defun http-get (url-components &key (query-args '()))
  "Make an HTTP GET call to the Zapper.fi API."
  (dex:get (quri:make-uri :defaults (cond ((stringp url-components)
                                           (concatenate 'string *api* url-components))
                                          ((listp url-components)
                                           (apply 'concatenate 'string *api* url-components)))
                          :query (if query-args
                                     (acons "api_key" *api-key* query-args)
                                     `(("api_key" . ,*api-key*))))))

(defun http-get-json (url-components &key (query-args '()))
  "Make an HTTP GET call to the Zapper.fi API, expecting JSON back, and parse."
  (let ((yason:*parse-json-arrays-as-vectors*   t)
        (yason:*parse-json-booleans-as-symbols* t))
    (yason:parse (http-get url-components :query-args query-args))))

(defun get-gas-price (&optional network)
  "Get the gas price from the API.
You may optionally specify a network, defaulting to ethereum."
  (http-get-json "/gas-price" :query-args (when network '(("network" . (canonicalized-network network))))))

(defun get-prices (&optional network)
  "Retrieve prices for this network.
You may optionally specify a network, defaulting to ethereum."
  (http-get-json "/prices" :query-args (when network '(("network" . (canonicalized-network network))))))

(defun get-health ()
  "The service returns a 200 status code if all is well."
  (multiple-value-bind (_ http-status-code) (http-get "/health")
    (declare (ignore _))
    http-status-code))
