(defpackage #:zapper-fi/rest-api
  (:use :common-lisp)
  (:export :*network-aliases*
           :*valid-networks*
           :canonicalized-ethereum-address
           :canonicalized-fiat-symbol
           :canonicalized-gas-price
           :canonicalized-network
           :get-approval-state
           :get-approval-transaction
           :get-fiat-rates
           :get-gas-price
           :get-health
           :get-prices
           :unknown-network-error))

(in-package :zapper-fi/rest-api)

(defvar *api* "http://api.zapper.fi/v1")

(defvar *api-cache-timeout-seconds* 60)

(defvar *api-key* "96e0cc51-a62e-42ca-acee-910ea7d2a241")

(defvar *valid-protocols*
  '("aave" "aave-amm" "aave-v2" "alpha" "b-protocol" "badger" "balancer" "bancor"
    "barnbridge" "bitcoin" "compound" "cover" "cream" "curve" "defisaver"
    "derivadex" "dhedge" "dforce" "dodo" "dsd" "dydx" "esd" "futureswap" "idle"
    "harvest" "hegic" "keeper-dao" "linkswap" "loopring" "maker" "mooniswap"
    "1inch" "pancakeswap" "nft" "other" "pickle" "pooltogether" "quickswap"
    "rari" "realt" "reflexer" "saddle" "sfinance" "shell" "smoothy" "snowswap"
    "sushiswap" "swerve" "synthetix" "tokensets" "tokens" "uniswap" "uniswap-v2"
    "unit" "value" "vesper" "xsigma" "yearn"))

(defvar *valid-farms* '("masterchef" "single-staking" "geyser" "gauge"))

(defvar *valid-pools*
  '("balancer" "bancor" "curve" "loopring" "oneInch" "pancakeswap" "quickswap"
    "sfinance" "snowswap" "sushiswap" "uniswap" "linkswap" "dodo" "saddle" "xsigma"))

(defvar *valid-networks* '("ethereum" "binance-smart-chain" "polygon" "xdai"))

(defvar *network-aliases* (sigma/hash:populate-hash-table "binance" "binance-smart-chain"
                                                          "bsc"     "binance-smart-chain"
                                                          "eth"     "ethereum"
                                                          "ether"   "ethereum"))

(define-condition unknown-network-error (error)
  ((unknown-network :initarg :unknown-network :reader unknown-network))
  (:report (lambda (condition stream)
             (format stream "Unknown metwork ~A." (unknown-network condition)))))

(defun canonicalized-ethereum-address (address)
  (cond ((and (stringp address)
              (= 42 (length address))
              (string= "0x" (subseq address 0 2)))
         (string-downcase address))))

(defun canonicalized-fiat-symbol (fiat-symbol)
  (cond ((stringp fiat-symbol)
         (string-upcase fiat-symbol))
        ((symbolp fiat-symbol)
         (canonicalized-fiat-symbol (symbol-name fiat-symbol)))))

(defun canonicalized-gas-price (gas-price)
  (cond ((stringp gas-price)
         gas-price)
        ((integerp gas-price)
         (format nil "~D" gas-price))
        ((numberp gas-price)
         (format nil "~A" (float gas-price)))))

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

(function-cache:defcached
    (http-get-cached :timeout *api-cache-timeout-seconds*)
    (url-components &key (query-args '()))
  (if query-args
      (http-get url-components :query-args query-args)
      (http-get url-components)))

(defun http-get-json (url-components &key (query-args '()))
  "Make an HTTP GET call to the Zapper.fi API, expecting JSON back, and parse."
  (let ((yason:*parse-json-arrays-as-vectors*   t)
        (yason:*parse-json-booleans-as-symbols* t))
    (yason:parse (http-get url-components :query-args query-args))))

(function-cache:defcached
    (http-get-json-cached :timeout *api-cache-timeout-seconds*)
    (url-components &key (query-args '()) (final-hook nil))
  (let ((result (if query-args
                    (http-get-json url-components :query-args query-args)
                    (http-get-json url-components))))
    (when final-hook (funcall final-hook result))
    result))

;; We're making this function alias so that we're being explicitly clear when we
;; don't want to cache the results of a call for some reason.  If we call
;; HTTP-GET-JSON-DO-NOT-CACHE we specifically mean you are not to cache the
;; result but instead you must request each time.
(sigma/control:function-alias 'http-get-json 'http-get-json-do-not-cache)

(defun get-approval-transaction (token-address spender-address owner-address gas-price)
  "Returns data that can be used to build an ERC20 approval transaction."
  (http-get-json-cached "/approval-transaction"
                        :query-args `(("tokenAddress"   . ,(canonicalized-ethereum-address token-address))
                                      ("spenderAddress" . ,(canonicalized-ethereum-address spender-address))
                                      ("ownerAddress"   . ,(canonicalized-ethereum-address owner-address))
                                      ("gasPrice"       . ,(canonicalized-gas-price        gas-price)))))

(defun get-approval-state (token-address spender-address owner-address)
  "The approved amount for a specific token"
  (http-get-json-do-not-cache "/approval-state"
                              :query-args `(("tokenAddress"   . ,(canonicalized-ethereum-address token-address))
                                            ("spenderAddress" . ,(canonicalized-ethereum-address spender-address))
                                            ("ownerAddress"   . ,(canonicalized-ethereum-address owner-address)))))

(defun get-fiat-rates ()
  "Retrieve a list of fiat currency exchange rates based on USD."
  (http-get-json-cached "/fiat-rates"))

(defun get-gas-price (&optional network)
  "Get the gas price from the API.
You may optionally specify a network, defaulting to ethereum."
  (http-get-json-cached "/gas-price" :query-args (when network '(("network" . (canonicalized-network network))))))

(defvar *prices-by-address*)
(defvar *prices-by-symbol*)

(defun add-price-to-hashmaps (price-entry)
  (let* ((address   (gethash "address"  price-entry))
         (decimals  (gethash "decimals" price-entry))
         (symbol    (gethash "symbol"   price-entry))
         (price     (gethash "price"    price-entry))
         (new-entry (sigma/hash:populate-hash-table :address  address
                                                    :decimals decimals
                                                    :symbol   symbol
                                                    :price    price)))
    (sigma/hash:sethash address *prices-by-address* new-entry)
    (sigma/hash:sethash symbol  *prices-by-symbol*  new-entry)))

(defun rebuild-prices-hashmaps (result)
  (setf *prices-by-address* (make-hash-table :test 'equal))
  (setf *prices-by-symbol*  (make-hash-table :test 'equal))
  (map 'vector 'add-price-to-hashmaps result))

(defun get-prices (&optional network)
  "Retrieve prices for this network.
You may optionally specify a network, defaulting to ethereum."
  (http-get-json-cached "/prices"
                        :query-args (when network '(("network" . (canonicalized-network network))))
                        :final-hook 'rebuild-prices-hashmaps))

(defun get-health ()
  "The service returns a 200 status code if all is well."
  (multiple-value-bind (_ http-status-code) (http-get-cached "/health")
    (declare (ignore _))
    http-status-code))
