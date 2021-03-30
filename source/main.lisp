(defpackage #:zapper-fi
  (:use :common-lisp))
(in-package #:zapper-fi)

(defun gas-price (speed &optional network)
  (gethash speed (zapper-fi/rest-api:get-gas-price network)))

(defun fast-gas-price (&optional network)
  "Get the current fast gas price.
You may optionally specify a network, defaulting to ethereum."
  (gas-price "fast" network))

(defun instant-gas-price (&optional network)
  "Get the current fast gas price.
You may optionally specify a network, defaulting to ethereum."
  (gas-price "instant" network))

(defun standard-gas-price (&optional network)
  "Get the current fast gas price.
You may optionally specify a network, defaulting to ethereum."
  (gas-price "standard" network))
