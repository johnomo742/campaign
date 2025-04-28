;; Campaign: Decentralized Crowdfunding Contract

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_CAMPAIGN_NOT_FOUND (err u102))
(define-constant ERR_DEADLINE_PASSED (err u103))
(define-constant ERR_GOAL_NOT_REACHED (err u104))
(define-constant ERR_ALREADY_CLAIMED (err u105))
(define-constant ERR_ALREADY_REFUNDED (err u106))
(define-constant ERR_DEADLINE_NOT_REACHED (err u107))
(define-constant ERR_INVALID_DEADLINE (err u108))
(define-constant ERR_INVALID_TITLE (err u109))
(define-constant ERR_INVALID_DESCRIPTION (err u110))
(define-constant ERR_CAMPAIGN_ALREADY_EXISTS (err u111))
(define-constant ERR_PLATFORM_FEE_EXCEEDS_LIMIT (err u112))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var platform-fee-percent uint u3) ;; 3% platform fee
(define-data-var platform-treasury uint u0)

;; Data maps
(define-map campaigns 
  { campaign-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    goal-amount: uint,
    deadline: uint,
    current-amount: uint,
    claimed: bool
  }
)

(define-map contributions
  { campaign-id: uint, contributor: principal }
  { 
    amount: uint,
    refunded: bool
  }
)

(define-data-var next-campaign-id uint u1)

;; Private functions
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-percent)) u100))

;; Public functions
(define-public (create-campaign 
    (title (string-utf8 100)) 
    (description (string-utf8 500)) 
    (goal-amount uint) 
    (duration uint)
  )
  (let (
    (campaign-id (var-get next-campaign-id))
    (deadline (+ block-height duration))
  )
    (asserts! (> goal-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration u0) ERR_INVALID_DEADLINE)
    (asserts! (and (> (len title) u0) (<= (len title) u100)) ERR_INVALID_TITLE)
    (asserts! (and (> (len description) u0) (<= (len description) u500)) ERR_INVALID_DESCRIPTION)
    
    ;; Create new campaign
    (map-set campaigns { campaign-id: campaign-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        goal-amount: goal-amount,
        deadline: deadline,
        current-amount: u0,
        claimed: false
      }
    )
    
    ;; Increment campaign ID counter
    (var-set next-campaign-id (+ campaign-id u1))
    
    (print { event: "campaign-created", campaign-id: campaign-id, creator: tx-sender, goal: goal-amount })
    (ok campaign-id)))


;; Admin functions
(define-public (withdraw-platform-fees)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (let ((balance (var-get platform-treasury)))
      (asserts! (> balance u0) ERR_INVALID_AMOUNT)
      (match (as-contract (stx-transfer? balance tx-sender (var-get contract-owner)))
        success (begin
          (var-set platform-treasury u0)
          (print { event: "platform-fees-withdrawn", amount: balance })
          (ok balance))
        error (err error)))))

(define-public (set-platform-fee-percent (percent uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= percent u10) ERR_PLATFORM_FEE_EXCEEDS_LIMIT) ;; Maximum 10% platform fee
    (ok (var-set platform-fee-percent percent))))

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ok (var-set contract-owner new-owner))))

;; Read-only functions
(define-read-only (get-campaign-details (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign (ok (merge campaign {
      is-successful: (and (>= (get current-amount campaign) (get goal-amount campaign))
                         (>= block-height (get deadline campaign))),
      is-active: (< block-height (get deadline campaign)),
      blocks-remaining: (if (< block-height (get deadline campaign))
                           (- (get deadline campaign) block-height)
                           u0)
    }))
    ERR_CAMPAIGN_NOT_FOUND))

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (match (map-get? contributions { campaign-id: campaign-id, contributor: contributor })
    contribution (ok contribution)
    (ok { amount: u0, refunded: false })))

(define-read-only (get-platform-fee-percent)
  (var-get platform-fee-percent))

(define-read-only (get-campaign-count)
  (- (var-get next-campaign-id) u1))

