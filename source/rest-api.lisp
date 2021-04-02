(defpackage #:zapper-fi/rest-api
  (:use :common-lisp)
  (:export :*valid-networks*
           :*network-aliases*
           :canonicalized-network
           :get-gas-price
           :unknown-network-error))

(in-package :zapper-fi/rest-api)

(defvar *api* "http://api.zapper.fi/v1")

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

(defun build-url (&rest url-components)
  (apply #'concatenate 'string *api* url-components))

(defun http-get-json (&rest url-components)
  "Make an HTTP GET call to the Zapper.fi API, expecting JSON back, and parse."
  (let ((yason:*parse-json-arrays-as-vectors*   t)
        (yason:*parse-json-booleans-as-symbols* t))
    (yason:parse (dex:get (apply #'build-url url-components)))))

(defun get-gas-price (&optional network)
  "Get the gas price from the API.
You may optionally specify a network, defaulting to ethereum."
  (if network
      (http-get-json "/gas-price?network=" (canonicalized-network network))
      (http-get-json "/gas-price")))

(defun get-prices (&optional network)
  "Retrieve prices for this network.
You may optionally specify a network, defaulting to ethereum."
  (if network
      (http-get-json "/prices?api_key=" *api-key* "&network=" (canonicalized-network network))
      (http-get-json "/prices?api_key=" *api-key*)))
