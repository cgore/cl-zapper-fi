(defpackage #:zapper-fi
  (:use :common-lisp)
  (:export :fast-gas-price
           :fiat-rate
           :gas-price
           :instant-gas-price
           :standard-gas-price
           :btc-usd
           :fiat-rate-inverted
           :xag-usd
           :xau-usd
           :btc-xag
           :btc-xau))
(in-package #:zapper-fi)

(defun fiat-rate (fiat-symbol)
  "Lookup the current fiat currency exchange rate for something in US Dollars."
  (gethash (zapper-fi/rest-api:canonicalized-fiat-symbol fiat-symbol)
           (zapper-fi/rest-api:get-fiat-rates)))

(defun fiat-rate-inverted (fiat-symbol)
  "Lookup the current fiat currency exchange rate for something in US Dollars, but inverted."
  (/ 1 (fiat-rate fiat-symbol)))

(defun btc-usd ()
  "The value of Bitcoin in US Dollars."
  (fiat-rate-inverted :btc))

(defun xag-usd ()
  "The value of one troy ounce of silver in US Dollars."
  (fiat-rate-inverted :xag))

(defun xau-usd ()
  "The value of one troy ounce of gold in US Dollars."
  (fiat-rate-inverted :xau))

(defun btc-xag ()
  "The value of one troy ounce of silver in Bitcoin, translated via US Dollar."
  (/ (btc-usd)
     (xag-usd)))

(defun btc-xau ()
  "The value of one troy ounce of gold in Bitcoin, translated via US Dollar."
  (/ (btc-usd)
     (xau-usd)))

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
