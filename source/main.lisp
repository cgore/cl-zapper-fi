(defpackage #:zapper-fi
  (:use :common-lisp)
  (:export :fast-gas-price
           :fiat-rate
           :gas-price
           :instant-gas-price
           :standard-gas-price
           :usd/btc
           :usd/fiat-rate
           :usd/xag
           :usd/xau
           :xag/btc
           :xau/btc))
(in-package #:zapper-fi)

(defun fiat-rate (fiat-symbol)
  "Lookup the current fiat currency exchange rate for something in US Dollars."
  (gethash (zapper-fi/rest-api:canonicalized-fiat-symbol fiat-symbol)
           (zapper-fi/rest-api:get-fiat-rates)))

(defun usd/fiat-rate (fiat-symbol)
  "Lookup the current fiat currency exchange rate for something in US Dollars, but inverted."
  (/ 1 (fiat-rate fiat-symbol)))

(defun usd/btc ()
  "The value of Bitcoin in US Dollars."
  (usd/fiat-rate :btc))

(defun usd/xag ()
  "The value of one troy ounce of silver in US Dollars."
  (usd/fiat-rate :xag))

(defun usd/xau ()
  "The value of one troy ounce of gold in US Dollars."
  (usd/fiat-rate :xau))

(defun xag/btc ()
  "The value of one troy ounce of silver in Bitcoin, translated via US Dollar."
  (/ (usd/btc)
     (usd/xag)))

(defun xau/btc ()
  "The value of one troy ounce of gold in Bitcoin, translated via US Dollar."
  (/ (usd/btc)
     (usd/xau)))

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
