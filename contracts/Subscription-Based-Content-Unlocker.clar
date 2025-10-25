(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SUBSCRIPTION (err u101))
(define-constant ERR-EXPIRED (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-SUBSCRIBED (err u104))
(define-constant ERR-TIER-NOT-FOUND (err u105))
(define-constant ERR-NO-REVENUE (err u106))
(define-constant ERR-WITHDRAWALS-PAUSED (err u107))
(define-constant ERR-RENEWAL-DISABLED (err u108))
(define-constant ERR-INSUFFICIENT-POINTS (err u109))
(define-constant ERR-INVALID-REWARD (err u110))

(define-constant SUBSCRIPTION-DURATION u2160)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant POINTS-PER-RENEWAL u10)
(define-constant BONUS-POINTS-THRESHOLD u5)

(define-data-var platform-fee uint u50)
(define-data-var total-subscribers uint u0)
(define-data-var total-revenue uint u0)
(define-data-var withdrawals-enabled bool true)

(define-map creators 
    principal 
    {
        revenue: uint,
        subscriber-count: uint,
        content-count: uint,
        total-withdrawn: uint,
        last-withdrawal: uint
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
        active: bool,
        auto-renew: bool,
        renewal-count: uint
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

(define-map loyalty-points
    { subscriber: principal, creator: principal }
    {
        points: uint,
        total-earned: uint,
        last-reward-claim: uint
    }
)

(define-map reward-catalog
    { creator: principal, reward-id: uint }
    {
        cost: uint,
        description: (string-ascii 128),
        available: bool
    }
)

(define-public (register-creator)
    (let
        ((creator-data {
            revenue: u0,
            subscriber-count: u0,
            content-count: u0,
            total-withdrawn: u0,
            last-withdrawal: u0
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
        
        (try! (stx-transfer? (get price tier) tx-sender (as-contract tx-sender)))
        
        (let ((creator-data (unwrap-panic (map-get? creators creator))))
            (map-set creators creator (merge creator-data {
                revenue: (+ (get revenue creator-data) (get price tier)),
                subscriber-count: (+ (get subscriber-count creator-data) u1)
            }))
        )
        
        (map-set subscriptions 
            subscription-key
            {
                tier: tier-id,
                start-height: current-height,
                end-height: (+ current-height SUBSCRIPTION-DURATION),
                active: true,
                auto-renew: false,
                renewal-count: u0
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

(define-public (withdraw-revenue (amount uint))
    (let
        ((creator-data (unwrap! (map-get? creators tx-sender) ERR-NOT-AUTHORIZED))
         (available-revenue (get revenue creator-data))
         (platform-fee-amount (/ (* amount (var-get platform-fee)) u10000))
         (withdrawal-amount (- amount platform-fee-amount)))
        
        (asserts! (var-get withdrawals-enabled) ERR-WITHDRAWALS-PAUSED)
        (asserts! (> available-revenue u0) ERR-NO-REVENUE)
        (asserts! (>= available-revenue amount) ERR-INSUFFICIENT-FUNDS)
        
        (try! (stx-transfer? withdrawal-amount (as-contract tx-sender) tx-sender))
        (try! (stx-transfer? platform-fee-amount (as-contract tx-sender) CONTRACT-OWNER))
        
        (map-set creators tx-sender (merge creator-data {
            revenue: (- available-revenue amount),
            total-withdrawn: (+ (get total-withdrawn creator-data) withdrawal-amount),
            last-withdrawal: burn-block-height
        }))
        
        (ok withdrawal-amount)
    )
)

(define-public (withdraw-all-revenue)
    (let
        ((creator-data (unwrap! (map-get? creators tx-sender) ERR-NOT-AUTHORIZED))
         (available-revenue (get revenue creator-data)))
        
        (asserts! (var-get withdrawals-enabled) ERR-WITHDRAWALS-PAUSED)
        (asserts! (> available-revenue u0) ERR-NO-REVENUE)
        
        (withdraw-revenue available-revenue)
    )
)

(define-public (toggle-withdrawals)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (var-set withdrawals-enabled (not (var-get withdrawals-enabled))))
    )
)

(define-read-only (get-withdrawal-status)
    (var-get withdrawals-enabled)
)

(define-read-only (get-available-revenue (creator principal))
    (let
        ((creator-data (map-get? creators creator)))
        (if (is-some creator-data)
            (ok (get revenue (unwrap-panic creator-data)))
            ERR-NOT-AUTHORIZED)
    )
)

(define-public (enable-auto-renewal (creator principal))
    (let
        ((subscription-key { subscriber: tx-sender, creator: creator })
         (subscription (unwrap! (map-get? subscriptions subscription-key) ERR-INVALID-SUBSCRIPTION)))
        
        (asserts! (get active subscription) ERR-INVALID-SUBSCRIPTION)
        
        (map-set subscriptions 
            subscription-key
            (merge subscription { auto-renew: true })
        )
        
        (ok true)
    )
)

(define-public (disable-auto-renewal (creator principal))
    (let
        ((subscription-key { subscriber: tx-sender, creator: creator })
         (subscription (unwrap! (map-get? subscriptions subscription-key) ERR-INVALID-SUBSCRIPTION)))
        
        (asserts! (get active subscription) ERR-INVALID-SUBSCRIPTION)
        
        (map-set subscriptions 
            subscription-key
            (merge subscription { auto-renew: false })
        )
        
        (ok true)
    )
)

(define-public (process-auto-renewal (subscriber principal) (creator principal))
    (let
        ((subscription-key { subscriber: subscriber, creator: creator })
         (subscription (unwrap! (map-get? subscriptions subscription-key) ERR-INVALID-SUBSCRIPTION))
         (tier (unwrap! (map-get? subscription-tiers (get tier subscription)) ERR-TIER-NOT-FOUND))
         (current-height burn-block-height))
        
        (asserts! (get auto-renew subscription) ERR-RENEWAL-DISABLED)
        (asserts! (<= (get end-height subscription) current-height) ERR-INVALID-SUBSCRIPTION)
        (asserts! (>= (stx-get-balance subscriber) (get price tier)) ERR-INSUFFICIENT-FUNDS)
        
        (try! (stx-transfer? (get price tier) subscriber (as-contract tx-sender)))
        
        (let ((creator-data (unwrap-panic (map-get? creators creator))))
            (map-set creators creator (merge creator-data {
                revenue: (+ (get revenue creator-data) (get price tier))
            }))
        )
        
        (map-set subscriptions 
            subscription-key
            (merge subscription {
                start-height: current-height,
                end-height: (+ current-height (get duration tier)),
                renewal-count: (+ (get renewal-count subscription) u1)
            })
        )
        
        (var-set total-revenue (+ (var-get total-revenue) (get price tier)))
        
        (let
            ((loyalty-key { subscriber: subscriber, creator: creator })
             (existing-loyalty (map-get? loyalty-points loyalty-key))
             (new-renewal-count (+ (get renewal-count subscription) u1))
             (points-earned (if (>= new-renewal-count BONUS-POINTS-THRESHOLD)
                               (* POINTS-PER-RENEWAL u2)
                               POINTS-PER-RENEWAL)))
            (if (is-some existing-loyalty)
                (let ((loyalty-data (unwrap-panic existing-loyalty)))
                    (map-set loyalty-points loyalty-key {
                        points: (+ (get points loyalty-data) points-earned),
                        total-earned: (+ (get total-earned loyalty-data) points-earned),
                        last-reward-claim: (get last-reward-claim loyalty-data)
                    }))
                (map-set loyalty-points loyalty-key {
                    points: points-earned,
                    total-earned: points-earned,
                    last-reward-claim: u0
                })))
        
        (ok true)
    )
)

(define-read-only (get-renewal-status (subscriber principal) (creator principal))
    (let
        ((subscription (map-get? subscriptions { subscriber: subscriber, creator: creator })))
        (if (is-some subscription)
            (ok {
                auto-renew: (get auto-renew (unwrap-panic subscription)),
                renewal-count: (get renewal-count (unwrap-panic subscription))
            })
            ERR-INVALID-SUBSCRIPTION)
    )
)

(define-public (create-reward (reward-id uint) (cost uint) (description (string-ascii 128)))
    (let
        ((creator-data (unwrap! (map-get? creators tx-sender) ERR-NOT-AUTHORIZED))
         (reward-key { creator: tx-sender, reward-id: reward-id }))
        
        (ok (map-set reward-catalog reward-key {
            cost: cost,
            description: description,
            available: true
        }))
    )
)

(define-public (redeem-reward (creator principal) (reward-id uint))
    (let
        ((loyalty-key { subscriber: tx-sender, creator: creator })
         (loyalty-data (unwrap! (map-get? loyalty-points loyalty-key) ERR-INVALID-SUBSCRIPTION))
         (reward-key { creator: creator, reward-id: reward-id })
         (reward (unwrap! (map-get? reward-catalog reward-key) ERR-INVALID-REWARD)))
        
        (asserts! (get available reward) ERR-INVALID-REWARD)
        (asserts! (>= (get points loyalty-data) (get cost reward)) ERR-INSUFFICIENT-POINTS)
        
        (map-set loyalty-points loyalty-key {
            points: (- (get points loyalty-data) (get cost reward)),
            total-earned: (get total-earned loyalty-data),
            last-reward-claim: burn-block-height
        })
        
        (ok true)
    )
)

(define-public (toggle-reward-availability (reward-id uint))
    (let
        ((creator-data (unwrap! (map-get? creators tx-sender) ERR-NOT-AUTHORIZED))
         (reward-key { creator: tx-sender, reward-id: reward-id })
         (reward (unwrap! (map-get? reward-catalog reward-key) ERR-INVALID-REWARD)))
        
        (ok (map-set reward-catalog reward-key (merge reward {
            available: (not (get available reward))
        })))
    )
)

(define-read-only (get-loyalty-points (subscriber principal) (creator principal))
    (map-get? loyalty-points { subscriber: subscriber, creator: creator })
)

(define-read-only (get-reward-details (creator principal) (reward-id uint))
    (map-get? reward-catalog { creator: creator, reward-id: reward-id })
)
