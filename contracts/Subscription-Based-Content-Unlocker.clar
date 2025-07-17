(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SUBSCRIPTION (err u101))
(define-constant ERR-EXPIRED (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-SUBSCRIBED (err u104))
(define-constant ERR-TIER-NOT-FOUND (err u105))

(define-constant SUBSCRIPTION-DURATION u2160)
(define-constant CONTRACT-OWNER tx-sender)

(define-data-var platform-fee uint u50)
(define-data-var total-subscribers uint u0)
(define-data-var total-revenue uint u0)

(define-map creators 
    principal 
    {
        revenue: uint,
        subscriber-count: uint,
        content-count: uint
    }
)

(define-map subscription-tiers
    uint 
    {
        price: uint,
        duration: uint,
        benefits: (string-ascii 64)
    }
)

(define-map subscriptions
    { subscriber: principal, creator: principal }
    {
        tier: uint,
        start-height: uint,
        end-height: uint,
        active: bool
    }
)

(define-map content-registry
    uint
    {
        creator: principal,
        tier-required: uint,
        metadata: (string-ascii 256)
    }
)

(define-public (register-creator)
    (let
        ((creator-data {
            revenue: u0,
            subscriber-count: u0,
            content-count: u0
        }))
        (ok (map-set creators tx-sender creator-data))
    )
)

(define-public (create-subscription-tier (tier-id uint) (price uint) (duration uint) (benefits (string-ascii 64)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (map-set subscription-tiers tier-id {
            price: price,
            duration: duration,
            benefits: benefits
        }))
    )
)

(define-public (subscribe-to-creator (creator principal) (tier-id uint))
    (let
    ((tier (unwrap! (map-get? subscription-tiers tier-id) ERR-TIER-NOT-FOUND))
    (current-height burn-block-height)
    (subscription-key { subscriber: tx-sender, creator: creator }))
        
        (asserts! (is-some (map-get? creators creator)) ERR-NOT-AUTHORIZED)
        (asserts! (>= (stx-get-balance tx-sender) (get price tier)) ERR-INSUFFICIENT-FUNDS)
        (asserts! (is-none (map-get? subscriptions subscription-key)) ERR-ALREADY-SUBSCRIBED)
        
        (try! (stx-transfer? (get price tier) tx-sender creator))
        
        (map-set subscriptions 
            subscription-key
            {
                tier: tier-id,
                start-height: current-height,
                end-height: (+ current-height SUBSCRIPTION-DURATION),
                active: true
            }
        )
        
        (var-set total-subscribers (+ (var-get total-subscribers) u1))
        (var-set total-revenue (+ (var-get total-revenue) (get price tier)))
        
        (ok true)
    )
)

(define-public (add-content (content-id uint) (tier-required uint) (metadata (string-ascii 256)))
    (let
        ((creator-data (unwrap! (map-get? creators tx-sender) ERR-NOT-AUTHORIZED)))
        
        (map-set content-registry content-id {
            creator: tx-sender,
            tier-required: tier-required,
            metadata: metadata
        })
        
        (map-set creators tx-sender (merge creator-data {
            content-count: (+ (get content-count creator-data) u1)
        }))
        
        (ok true)
    )
)

(define-read-only (can-access-content (user principal) (content-id uint))
    (let
        ((content (unwrap! (map-get? content-registry content-id) ERR-INVALID-SUBSCRIPTION))
         (subscription (map-get? subscriptions { subscriber: user, creator: (get creator content) }))
         (current-height burn-block-height))
        
        (if (and
                (is-some subscription)
                (>= (get end-height (unwrap-panic subscription)) current-height)
                (>= (get tier (unwrap-panic subscription)) (get tier-required content)))
            (ok true)
            ERR-EXPIRED)
    )
)

(define-read-only (get-subscription-details (subscriber principal) (creator principal))
    (map-get? subscriptions { subscriber: subscriber, creator: creator })
)

(define-read-only (get-creator-stats (creator principal))
    (map-get? creators creator)
)

(define-read-only (get-platform-stats)
    {
        total-subscribers: (var-get total-subscribers),
        total-revenue: (var-get total-revenue),
        platform-fee: (var-get platform-fee)
    }
)
