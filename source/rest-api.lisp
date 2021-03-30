(defpackage #:zapper-fi/rest-api
  (:use :common-lisp)
  (:export :*valid-networks*
           :get-gas-price))

(in-package :zapper-fi/rest-api)

(defvar *api* "http://api.zapper.fi/v1")

(defvar *valid-networks* '("ethereum" "binance-smart-chain" "polygon"))

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
  (cond ((null network)
         (http-get-json "/gas-price"))
        ((member network *valid-networks* :test 'string=)
         (http-get-json "/gas-price?network=" network))))
