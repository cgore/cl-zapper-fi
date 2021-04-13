(defpackage #:zapper-fi
  (:use :common-lisp)
  (:export :/eth
           :/xag
           :/xau
           :1/fiat-rate
           :btc/eth
           :btc/usd
           :btc/xag
           :btc/xau
           :eth/usd
           :eth/xag
           :eth/xau
           :fast-gas-price
           :fiat-rate
           :gas-price
           :instant-gas-price
           :standard-gas-price
           :xag/usd
           :xau/usd))
(in-package #:zapper-fi)

(defun fiat-rate (fiat-symbol)
  "Lookup the current fiat currency exchange rate for something in US Dollars."
  (gethash (zapper-fi/rest-api:canonicalized-fiat-symbol fiat-symbol)
           (zapper-fi/rest-api:get-fiat-rates)))

(defun 1/fiat-rate (fiat-symbol)
  "Lookup the current fiat currency exchange rate for something in US Dollars, but inverted."
  (/ 1 (fiat-rate fiat-symbol)))

(defun btc/usd ()
  "The value of Bitcoin in US Dollars."
  (1/fiat-rate :btc))

(defun eth/usd ()
  "The value of Ether in US Dollars."
  (1/fiat-rate :eth))

(defun xag/usd ()
  "The value of one troy ounce of silver in US Dollars."
  (1/fiat-rate :xag))

(defun xau/usd ()
  "The value of one troy ounce of gold in US Dollars."
  (1/fiat-rate :xau))

(defun /usd->/xag (/usd)
  "Translate a per-USD price into a per-XAG (silver) toz price."
  (/ /usd (xag/usd)))

(defun /eth (fiat-symbol)
  "The value of some coin expressed in Ether, translated via US Dollar."
  (/ (1/fiat-rate fiat-symbol)
     (eth/usd)))

(defun /xag (fiat-symbol)
  "The value of some coin expressed in troy ounces of silver, translated via US Dollar."
  (/ (1/fiat-rate fiat-symbol)
     (xag/usd)))

(defun /xau (fiat-symbol)
  "The value of some coin expressed in troy ounces of gold, translated via US Dollar."
  (/ (1/fiat-rate fiat-symbol)
     (xau/usd)))

(defun btc/eth ()
  "The value of Bitcoin expressed in Ether, translated via US Dollar."
  (/eth :btc))

(defun btc/xag ()
  "The value of Bitcoin expressed in troy ounces of silver, translated via US Dollar."
  (/xag :btc))

(defun btc/xau ()
  "The value of Bitcoin expressed in troy ounces of gold, translated via US Dollar."
  (/xau :btc))

(defun eth/xag ()
  "The value of Ether expressed in troy ounces of silver, translated via US Dollar."
  (/xag :eth))

(defun eth/xau ()
  "The value of Ether expressed in troy ounces of gold, translated via US Dollar."
  (/xau :eth))

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
