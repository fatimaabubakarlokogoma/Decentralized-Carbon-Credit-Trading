;; trading.clar
;; Facilitates buying and selling of carbon credits

(define-data-var admin principal tx-sender)
(define-map listings
  { listing-id: uint }
  {
    seller: principal,
    amount: uint,
    price-per-unit: uint,
    active: bool
  }
)

(define-map trades
  { trade-id: uint }
  {
    listing-id: uint,
    buyer: principal,
    amount: uint,
    total-price: uint,
    timestamp: uint
  }
)

;; Track token balances internally
(define-map token-balances principal uint)

(define-data-var next-listing-id uint u1)
(define-data-var next-trade-id uint u1)
(define-data-var fee-percentage uint u2) ;; 2% fee

;; Error codes
(define-constant ERR_UNAUTHORIZED u1)
(define-constant ERR_LISTING_NOT_FOUND u2)
(define-constant ERR_LISTING_NOT_ACTIVE u3)
(define-constant ERR_INSUFFICIENT_FUNDS u4)
(define-constant ERR_INSUFFICIENT_CREDITS u5)
(define-constant ERR_INVALID_AMOUNT u6)

;; Admin functions
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_UNAUTHORIZED))
    (ok (var-set admin new-admin))
  )
)

(define-public (set-fee-percentage (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_UNAUTHORIZED))
    (asserts! (<= new-fee-percentage u10) (err u7)) ;; Max 10% fee
    (ok (var-set fee-percentage new-fee-percentage))
  )
)

;; Token management functions
(define-public (deposit-credits (amount uint))
  (begin
    ;; In a real implementation, this would transfer tokens from an external contract
    ;; For now, we'll just update the internal balance
    (ok (map-set token-balances
        tx-sender
        (+ (default-to u0 (map-get? token-balances tx-sender)) amount)))
  )
)

;; Trading functions
(define-public (create-listing (amount uint) (price-per-unit uint))
  (let (
    (listing-id (var-get next-listing-id))
    (seller-balance (default-to u0 (map-get? token-balances tx-sender)))
  )
    ;; Check if seller has enough credits
    (asserts! (>= seller-balance amount) (err ERR_INSUFFICIENT_CREDITS))

    ;; Create listing
    (map-set listings
      { listing-id: listing-id }
      {
        seller: tx-sender,
        amount: amount,
        price-per-unit: price-per-unit,
        active: true
      }
    )

    ;; Reduce seller's balance
    (map-set token-balances tx-sender (- seller-balance amount))

    ;; Increment listing ID
    (var-set next-listing-id (+ listing-id u1))

    (ok listing-id)
  )
)

(define-public (cancel-listing (listing-id uint))
  (let ((listing (unwrap! (map-get? listings { listing-id: listing-id }) (err ERR_LISTING_NOT_FOUND))))
    ;; Check if caller is the seller
    (asserts! (is-eq tx-sender (get seller listing)) (err ERR_UNAUTHORIZED))
    ;; Check if listing is active
    (asserts! (get active listing) (err ERR_LISTING_NOT_ACTIVE))

    ;; Update listing
    (map-set listings
      { listing-id: listing-id }
      (merge listing { active: false })
    )

    ;; Return credits to seller
    (map-set token-balances
      (get seller listing)
      (+ (default-to u0 (map-get? token-balances (get seller listing))) (get amount listing))
    )

    (ok true)
  )
)

(define-public (buy-credits (listing-id uint) (amount uint))
  (let (
    (listing (unwrap! (map-get? listings { listing-id: listing-id }) (err ERR_LISTING_NOT_FOUND)))
    (trade-id (var-get next-trade-id))
    (total-price (* amount (get price-per-unit listing)))
    (fee (/ (* total-price (var-get fee-percentage)) u100))
  )
    ;; Check if listing is active
    (asserts! (get active listing) (err ERR_LISTING_NOT_ACTIVE))
    ;; Check if amount is valid
    (asserts! (<= amount (get amount listing)) (err ERR_INVALID_AMOUNT))
    ;; Check if buyer has enough STX
    (asserts! (>= (stx-get-balance tx-sender) total-price) (err ERR_INSUFFICIENT_FUNDS))

    ;; Transfer STX from buyer to seller (minus fee)
    (try! (stx-transfer? (- total-price fee) tx-sender (get seller listing)))
    ;; Transfer fee to admin
    (try! (stx-transfer? fee tx-sender (var-get admin)))

    ;; Transfer credits to buyer
    (map-set token-balances
      tx-sender
      (+ (default-to u0 (map-get? token-balances tx-sender)) amount)
    )

    ;; Record the trade
    (map-set trades
      { trade-id: trade-id }
      {
        listing-id: listing-id,
        buyer: tx-sender,
        amount: amount,
        total-price: total-price,
        timestamp: block-height
      }
    )

    ;; Update listing
    (if (is-eq amount (get amount listing))
      ;; If all credits are bought, deactivate the listing
      (map-set listings
        { listing-id: listing-id }
        (merge listing {
          amount: u0,
          active: false
        })
      )
      ;; Otherwise, reduce the amount
      (map-set listings
        { listing-id: listing-id }
        (merge listing {
          amount: (- (get amount listing) amount)
        })
      )
    )

    ;; Increment trade ID
    (var-set next-trade-id (+ trade-id u1))

    (ok trade-id)
  )
)

;; Read-only functions
(define-read-only (get-listing (listing-id uint))
  (map-get? listings { listing-id: listing-id })
)

(define-read-only (get-trade (trade-id uint))
  (map-get? trades { trade-id: trade-id })
)

(define-read-only (get-fee-percentage)
  (var-get fee-percentage)
)

(define-read-only (get-balance (address principal))
  (default-to u0 (map-get? token-balances address))
)

