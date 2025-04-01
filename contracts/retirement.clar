;; retirement.clar
;; Permanently removes used credits from circulation

(define-map retirements
  { retirement-id: uint }
  {
    owner: principal,
    amount: uint,
    beneficiary: (string-utf8 100),
    reason: (string-utf8 500),
    timestamp: uint
  }
)

;; Track token balances internally
(define-map token-balances principal uint)

(define-data-var next-retirement-id uint u1)
(define-data-var total-retired uint u0)

;; Error codes
(define-constant ERR_INSUFFICIENT_CREDITS u1)
(define-constant ERR_RETIREMENT_NOT_FOUND u2)

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

;; Retirement functions
(define-public (retire-credits
    (amount uint)
    (beneficiary (string-utf8 100))
    (reason (string-utf8 500)))
  (let (
    (retirement-id (var-get next-retirement-id))
    (user-balance (default-to u0 (map-get? token-balances tx-sender)))
  )
    ;; Check if user has enough credits
    (asserts! (>= user-balance amount) (err ERR_INSUFFICIENT_CREDITS))

    ;; Reduce user's balance (burn the credits)
    (map-set token-balances tx-sender (- user-balance amount))

    ;; Update total retired
    (var-set total-retired (+ (var-get total-retired) amount))

    ;; Record the retirement
    (map-set retirements
      { retirement-id: retirement-id }
      {
        owner: tx-sender,
        amount: amount,
        beneficiary: beneficiary,
        reason: reason,
        timestamp: block-height
      }
    )

    ;; Increment retirement ID
    (var-set next-retirement-id (+ retirement-id u1))

    (ok retirement-id)
  )
)

;; Read-only functions
(define-read-only (get-retirement (retirement-id uint))
  (map-get? retirements { retirement-id: retirement-id })
)

(define-read-only (get-total-retired)
  (var-get total-retired)
)

(define-read-only (get-total-retired-by-user (user principal))
  (fold + (map get-retirement-amount (get-user-retirements user)) u0)
)

(define-read-only (get-user-retirements (user principal))
  (filter get-retirement-by-user (get-retirement-ids))
)

(define-read-only (get-retirement-ids)
  (list u1 u2 u3) ;; This is a placeholder. In a real contract, we would need to track all retirement IDs.
)

(define-read-only (get-retirement-by-user (retirement-id uint))
  (let ((retirement (map-get? retirements { retirement-id: retirement-id })))
    (and (is-some retirement) (is-eq (get owner (unwrap-panic retirement)) tx-sender))
  )
)

(define-read-only (get-retirement-amount (retirement-id uint))
  (default-to u0 (get amount (map-get? retirements { retirement-id: retirement-id })))
)

(define-read-only (get-balance (address principal))
  (default-to u0 (map-get? token-balances address))
)

