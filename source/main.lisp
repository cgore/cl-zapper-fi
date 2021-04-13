(defpackage #:zapper-fi
  (:use :common-lisp)
  (:export :/usd->/xag
           :/usd->/xau
           :1/fiat-rate
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

(defun /usd->/xau (/usd)
  "Translate a per-USD price into a per-XAU (gold) toz price."
  (/ /usd (xau/usd)))

(defun btc/xag ()
  "The value of Bitcoin expressed in troy ounces of silver, translated via US Dollar."
  (/usd->/xag (btc/usd)))

(defun btc/xau ()
  "The value of Bitcoin expressed in troy ounces of gold, translated via US Dollar."
  (/usd->/xau (btc/usd)))

(defun eth/xag ()
  "The value of Ethereum expressed in troy ounces of silver, translated via US Dollar."
  (/usd->/xag (eth/usd)))

(defun eth/xau ()
  "The value of Ethereum expressed in troy ounces of gold, translated via US Dollar."
  (/usd->/xau (eth/usd)))

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
