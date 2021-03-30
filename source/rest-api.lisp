(defpackage #:zapper-fi/rest-api
  (:use :common-lisp)
  (:export :get-gas-price))
(in-package :zapper-fi/rest-api)

(defvar *api* "http://api.zapper.fi/v1")

(defun build-url (&rest url-components)
  (apply #'concatenate 'string *api* url-components))

(defun http-get-json (&rest url-components)
  "Make an HTTP GET call to the Zapper.fi API, expecting JSON back, and parse."
  (let ((yason:*parse-json-arrays-as-vectors*   t)
        (yason:*parse-json-booleans-as-symbols* t))
    (yason:parse (dex:get (apply #'build-url url-components)))))

(defun get-gas-price ()
  "Get the gas price from the API."
  (http-get-json "/gas-price"))
