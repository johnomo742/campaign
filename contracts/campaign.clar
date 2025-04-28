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

(define-public (contribute (campaign-id uint) (amount uint))
  (let (
    (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
    (current-amount (get current-amount campaign))
    (deadline (get deadline campaign))
    (platform-fee (calculate-platform-fee amount))
    (contribution-key { campaign-id: campaign-id, contributor: tx-sender })
    (existing-contribution (default-to { amount: u0, refunded: false } 
                              (map-get? contributions contribution-key)))
    (total-amount (+ amount platform-fee))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (< block-height deadline) ERR_DEADLINE_PASSED)
    
    ;; Transfer STX from sender to contract
    (match (stx-transfer? total-amount tx-sender (as-contract tx-sender))
      success (begin
        ;; Update campaign amount
        (map-set campaigns { campaign-id: campaign-id }
          (merge campaign { current-amount: (+ current-amount amount) })
        )
        
        ;; Update contribution record
        (map-set contributions contribution-key
          { 
            amount: (+ (get amount existing-contribution) amount),
            refunded: false
          }
        )
        
        ;; Add platform fee to treasury
        (var-set platform-treasury (+ (var-get platform-treasury) platform-fee))
        
        (print { 
          event: "contribution-made", 
          campaign-id: campaign-id, 
          contributor: tx-sender, 
          amount: amount,
          platform-fee: platform-fee
        })
        
        (ok true))
      error (err error))))

(define-public (claim-funds (campaign-id uint))
  (let (
    (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
    (creator (get creator campaign))
    (current-amount (get current-amount campaign))
    (goal-amount (get goal-amount campaign))
    (deadline (get deadline campaign))
    (claimed (get claimed campaign))
  )
    (asserts! (is-eq tx-sender creator) ERR_UNAUTHORIZED)
    (asserts! (>= block-height deadline) ERR_DEADLINE_NOT_REACHED)
    (asserts! (>= current-amount goal-amount) ERR_GOAL_NOT_REACHED)
    (asserts! (not claimed) ERR_ALREADY_CLAIMED)
    
    ;; Transfer funds to creator
    (match (as-contract (stx-transfer? current-amount tx-sender creator))
      success (begin
        ;; Mark campaign as claimed
        (map-set campaigns { campaign-id: campaign-id }
          (merge campaign { claimed: true })
        )
        
        (print { 
          event: "funds-claimed", 
          campaign-id: campaign-id, 
          creator: creator, 
          amount: current-amount
        })
        
        (ok current-amount))
      error (err error))))

(define-public (request-refund (campaign-id uint))
  (let (
    (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
    (current-amount (get current-amount campaign))
    (goal-amount (get goal-amount campaign))
    (deadline (get deadline campaign))
    (contribution-key { campaign-id: campaign-id, contributor: tx-sender })
    (contribution (unwrap! (map-get? contributions contribution-key) ERR_UNAUTHORIZED))
    (contribution-amount (get amount contribution))
    (refunded (get refunded contribution))
  )
    (asserts! (>= block-height deadline) ERR_DEADLINE_NOT_REACHED)
    (asserts! (< current-amount goal-amount) ERR_GOAL_NOT_REACHED)
    (asserts! (not refunded) ERR_ALREADY_REFUNDED)
    (asserts! (> contribution-amount u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer refund to contributor
    (match (as-contract (stx-transfer? contribution-amount tx-sender tx-sender))
      success (begin
        ;; Update contribution as refunded
        (map-set contributions contribution-key
          (merge contribution { refunded: true })
        )
        
        ;; Update campaign amount
        (map-set campaigns { campaign-id: campaign-id }
          (merge campaign { current-amount: (- current-amount contribution-amount) })
        )
        
        (print { 
          event: "refund-processed", 
          campaign-id: campaign-id, 
          contributor: tx-sender, 
          amount: contribution-amount
        })
        
        (ok contribution-amount))
      error (err error))))

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

