(defpackage #:zapper-fi
  (:use :common-lisp))
(in-package #:zapper-fi)

(defun fast-gas-price ()
  "Get the current fast gas price."
  (gethash "fast" (zapper-fi/rest-api:get-gas-price)))

(defun instant-gas-price ()
  "Get the current fast gas price."
  (gethash "instant" (zapper-fi/rest-api:get-gas-price)))

(defun standard-gas-price ()
  "Get the current fast gas price."
  (gethash "standard" (zapper-fi/rest-api:get-gas-price)))
