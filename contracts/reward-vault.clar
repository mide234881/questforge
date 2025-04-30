;; reward-vault
;; 
;; A smart contract for QuestForge that manages the reward economy, issuing tokens, badges,
;; and special items when users complete quests. This contract tracks achievement milestones
;; and enables special rewards for consistent performance or completing challenging tasks.
;; The reward system provides tangible incentives with various collectible reward types.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-REWARD (err u101))
(define-constant ERR-REWARD-EXISTS (err u102))
(define-constant ERR-REWARD-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-CAMPAIGN-NOT-ACTIVE (err u105))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u106))
(define-constant ERR-ALREADY-CLAIMED (err u107))
(define-constant ERR-CRITERIA-NOT-MET (err u108))
(define-constant ERR-INVALID-PARAMETERS (err u109))

;; Data structures

;; Reward types enumeration
;; 1: Token (standard reward points)
;; 2: Badge (achievement display item)
;; 3: Special Item (rare collectible)
(define-constant REWARD-TYPE-TOKEN u1)
(define-constant REWARD-TYPE-BADGE u2)
(define-constant REWARD-TYPE-SPECIAL-ITEM u3)

;; Reward definition map
;; Maps reward-id to details about the reward
(define-map rewards
    { reward-id: (string-ascii 24) }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        reward-type: uint,
        value: uint,
        is-active: bool
    }
)

;; User reward balances map
;; Tracks tokens and collected special items/badges
(define-map user-rewards
    { user: principal, reward-id: (string-ascii 24) }
    { balance: uint, last-updated: uint }
)

;; User achievement progress
(define-map user-achievements
    { user: principal, achievement-id: (string-ascii 24) }
    { 
        progress: uint,
        target: uint,
        completed: bool,
        claimed: bool,
        completion-date: (optional uint)
    }
)

;; Achievement definitions
(define-map achievements
    { achievement-id: (string-ascii 24) }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        target: uint,
        reward-id: (string-ascii 24),
        reward-amount: uint,
        is-active: bool
    }
)

;; Time-limited reward campaigns
(define-map reward-campaigns
    { campaign-id: (string-ascii 24) }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        reward-id: (string-ascii 24),
        reward-multiplier: uint,
        start-block: uint,
        end-block: uint,
        task-category: (optional (string-ascii 24)),
        is-active: bool
    }
)

;; Admin principal that can manage rewards
(define-data-var contract-owner principal tx-sender)

;; Private functions

;; Check if sender is the contract owner
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner))
)

;; Get user reward balance for a specific reward
(define-private (get-user-reward-balance (user principal) (reward-id (string-ascii 24)))
    (default-to u0 
        (get balance 
            (map-get? user-rewards { user: user, reward-id: reward-id })
        )
    )
)

;; Update user reward balance
(define-private (update-user-reward 
    (user principal) 
    (reward-id (string-ascii 24)) 
    (amount uint) 
    (add bool)
)
    (let (
        (current-balance (get-user-reward-balance user reward-id))
        (new-balance (if add 
                        (+ current-balance amount) 
                        (if (>= current-balance amount) 
                            (- current-balance amount) 
                            u0)))
    )
        (map-set user-rewards
            { user: user, reward-id: reward-id }
            { balance: new-balance, last-updated: block-height }
        )
        new-balance
    )
)

;; Check if a campaign is active at current block
(define-private (is-campaign-active (campaign-id (string-ascii 24)))
    (match (map-get? reward-campaigns { campaign-id: campaign-id })
        campaign (and 
                    (get is-active campaign)
                    (>= block-height (get start-block campaign))
                    (<= block-height (get end-block campaign))
                )
        false
    )
)

;; Get campaign reward multiplier if active, otherwise return 1
(define-private (get-campaign-multiplier (campaign-id (string-ascii 24)) (task-category (optional (string-ascii 24))))
    (match (map-get? reward-campaigns { campaign-id: campaign-id })
        campaign (if (and 
                        (get is-active campaign)
                        (>= block-height (get start-block campaign))
                        (<= block-height (get end-block campaign))
                        ;; Check if task category matches or campaign has no category requirement
                        (or 
                            (is-none (get task-category campaign))
                            (is-eq task-category (get task-category campaign))
                        )
                    )
                    (get reward-multiplier campaign)
                    u1
                )
        u1
    )
)

;; Read-only functions

;; Check if a reward exists and is active
(define-read-only (is-reward-active (reward-id (string-ascii 24)))
    (match (map-get? rewards { reward-id: reward-id })
        reward (get is-active reward)
        false
    )
)

;; Get reward details
(define-read-only (get-reward-info (reward-id (string-ascii 24)))
    (map-get? rewards { reward-id: reward-id })
)

;; Get user's balance for a specific reward
(define-read-only (get-reward-balance (user principal) (reward-id (string-ascii 24)))
    (get-user-reward-balance user reward-id)
)

;; Get all reward balances for a user (to be implemented with list functions in a real deployment)
(define-read-only (get-user-reward-summary (user principal) (reward-id (string-ascii 24)))
    (map-get? user-rewards { user: user, reward-id: reward-id })
)

;; Get achievement details
(define-read-only (get-achievement-info (achievement-id (string-ascii 24)))
    (map-get? achievements { achievement-id: achievement-id })
)

;; Get user's progress for a specific achievement
(define-read-only (get-achievement-progress (user principal) (achievement-id (string-ascii 24)))
    (map-get? user-achievements { user: user, achievement-id: achievement-id })
)

;; Check if campaign is currently active
(define-read-only (check-campaign-status (campaign-id (string-ascii 24)))
    (is-campaign-active campaign-id)
)

;; Get campaign details
(define-read-only (get-campaign-info (campaign-id (string-ascii 24)))
    (map-get? reward-campaigns { campaign-id: campaign-id })
)

;; Public functions

;; Set contract owner
(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))
    )
)

;; Create or update a reward
(define-public (create-reward 
    (reward-id (string-ascii 24)) 
    (name (string-ascii 50)) 
    (description (string-ascii 200)) 
    (reward-type uint) 
    (value uint)
)
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (or (is-eq reward-type REWARD-TYPE-TOKEN) 
                    (is-eq reward-type REWARD-TYPE-BADGE) 
                    (is-eq reward-type REWARD-TYPE-SPECIAL-ITEM)) 
                 ERR-INVALID-REWARD)
        
        (map-set rewards
            { reward-id: reward-id }
            {
                name: name,
                description: description,
                reward-type: reward-type,
                value: value,
                is-active: true
            }
        )
        (ok true)
    )
)

;; Deactivate a reward
(define-public (deactivate-reward (reward-id (string-ascii 24)))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (match (map-get? rewards { reward-id: reward-id })
            reward (begin
                (map-set rewards
                    { reward-id: reward-id }
                    (merge reward { is-active: false })
                )
                (ok true)
            )
            ERR-REWARD-NOT-FOUND
        )
    )
)

;; Add tokens to a user's balance (for completing quests)
;; This would be called by an authorized quest contract
(define-public (issue-reward (user principal) (reward-id (string-ascii 24)) (amount uint) (campaign-id (optional (string-ascii 24))) (task-category (optional (string-ascii 24))))
    (begin
        ;; Only contract owner or authorized contracts can issue rewards
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-reward-active reward-id) ERR-REWARD-NOT-FOUND)
        
        ;; Apply campaign multiplier if available
        (let (
            (final-amount (if (and (is-some campaign-id) (is-campaign-active (unwrap-panic campaign-id)))
                           (* amount (get-campaign-multiplier (unwrap-panic campaign-id) task-category))
                           amount))
        )
            (ok (update-user-reward user reward-id final-amount true))
        )
    )
)

;; Create or update an achievement
(define-public (create-achievement
    (achievement-id (string-ascii 24))
    (name (string-ascii 50))
    (description (string-ascii 200))
    (target uint)
    (reward-id (string-ascii 24))
    (reward-amount uint)
)
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-reward-active reward-id) ERR-REWARD-NOT-FOUND)
        
        (map-set achievements
            { achievement-id: achievement-id }
            {
                name: name,
                description: description,
                target: target,
                reward-id: reward-id,
                reward-amount: reward-amount,
                is-active: true
            }
        )
        (ok true)
    )
)

;; Update user's progress toward an achievement
(define-public (update-achievement-progress (user principal) (achievement-id (string-ascii 24)) (progress-increment uint))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        
        (match (map-get? achievements { achievement-id: achievement-id })
            achievement (let (
                (user-progress (default-to 
                    { progress: u0, target: (get target achievement), completed: false, claimed: false, completion-date: none }
                    (map-get? user-achievements { user: user, achievement-id: achievement-id })
                ))
                (new-progress (+ (get progress user-progress) progress-increment))
                (completed (>= new-progress (get target achievement)))
                (completion-date (if (and completed (not (get completed user-progress)))
                                    (some block-height)
                                    (get completion-date user-progress)))
            )
                (map-set user-achievements
                    { user: user, achievement-id: achievement-id }
                    {
                        progress: new-progress,
                        target: (get target achievement),
                        completed: completed,
                        claimed: (get claimed user-progress),
                        completion-date: completion-date
                    }
                )
                (ok completed)
            )
            ERR-ACHIEVEMENT-NOT-FOUND
        )
    )
)

;; Claim achievement reward
(define-public (claim-achievement-reward (achievement-id (string-ascii 24)))
    (let (
        (user tx-sender)
        (user-achievement (map-get? user-achievements { user: user, achievement-id: achievement-id }))
    )
        (asserts! (is-some user-achievement) ERR-ACHIEVEMENT-NOT-FOUND)
        (let (
            (achievement-data (unwrap-panic user-achievement))
        )
            (asserts! (get completed achievement-data) ERR-CRITERIA-NOT-MET)
            (asserts! (not (get claimed achievement-data)) ERR-ALREADY-CLAIMED)
            
            (match (map-get? achievements { achievement-id: achievement-id })
                achievement (begin
                    ;; Mark as claimed
                    (map-set user-achievements
                        { user: user, achievement-id: achievement-id }
                        (merge achievement-data { claimed: true })
                    )
                    
                    ;; Issue the reward
                    (update-user-reward 
                        user 
                        (get reward-id achievement) 
                        (get reward-amount achievement) 
                        true
                    )
                    
                    (ok true)
                )
                ERR-ACHIEVEMENT-NOT-FOUND
            )
        )
    )
)

;; Create a time-limited reward campaign
(define-public (create-reward-campaign
    (campaign-id (string-ascii 24))
    (name (string-ascii 50))
    (description (string-ascii 200))
    (reward-id (string-ascii 24))
    (reward-multiplier uint)
    (start-block uint)
    (end-block uint)
    (task-category (optional (string-ascii 24)))
)
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-reward-active reward-id) ERR-REWARD-NOT-FOUND)
        (asserts! (< start-block end-block) ERR-INVALID-PARAMETERS)
        (asserts! (>= reward-multiplier u1) ERR-INVALID-PARAMETERS)
        
        (map-set reward-campaigns
            { campaign-id: campaign-id }
            {
                name: name,
                description: description,
                reward-id: reward-id,
                reward-multiplier: reward-multiplier,
                start-block: start-block,
                end-block: end-block,
                task-category: task-category,
                is-active: true
            }
        )
        (ok true)
    )
)

;; Deactivate a campaign
(define-public (deactivate-campaign (campaign-id (string-ascii 24)))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        
        (match (map-get? reward-campaigns { campaign-id: campaign-id })
            campaign (begin
                (map-set reward-campaigns
                    { campaign-id: campaign-id }
                    (merge campaign { is-active: false })
                )
                (ok true)
            )
            ERR-CAMPAIGN-NOT-ACTIVE
        )
    )
)

;; Allow users to spend tokens (for in-app purchases or rewards)
(define-public (spend-tokens (reward-id (string-ascii 24)) (amount uint))
    (let (
        (user tx-sender)
        (current-balance (get-user-reward-balance user reward-id))
    )
        (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
        (asserts! (is-reward-active reward-id) ERR-REWARD-NOT-FOUND)
        
        (ok (update-user-reward user reward-id amount false))
    )
)

;; Transfer tokens between users (if allowed in the app economy)
(define-public (transfer-tokens (recipient principal) (reward-id (string-ascii 24)) (amount uint))
    (let (
        (sender tx-sender)
        (sender-balance (get-user-reward-balance sender reward-id))
    )
        (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
        (asserts! (is-reward-active reward-id) ERR-REWARD-NOT-FOUND)
        
        ;; Deduct from sender
        (update-user-reward sender reward-id amount false)
        ;; Add to recipient
        (update-user-reward recipient reward-id amount true)
        
        (ok true)
    )
)