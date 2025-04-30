;; character-manager
;;
;; This contract handles the creation and management of user character profiles,
;; storing essential attributes like character class, level, experience points, and skill levels.
;; Users can create personalized characters that grow stronger as they complete quests.
;; The system tracks progression metrics persistently on the blockchain, providing
;; users with a sense of achievement and long-term growth as they tackle their real-world tasks
;; reframed as quests.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-CHARACTER-EXISTS (err u1002))
(define-constant ERR-CHARACTER-NOT-FOUND (err u1003))
(define-constant ERR-INVALID-CLASS (err u1004))
(define-constant ERR-INVALID-LEVEL (err u1005))
(define-constant ERR-INVALID-EXPERIENCE (err u1006))
(define-constant ERR-INVALID-SKILL (err u1007))
(define-constant ERR-SKILL-NOT-FOUND (err u1008))
(define-constant ERR-LEVEL-CAP-REACHED (err u1009))

;; Constants
(define-constant MAX-LEVEL u100)
(define-constant LEVEL-UP-FACTOR u10) ;; level * LEVEL-UP-FACTOR = XP needed for next level
(define-constant DEFAULT-SKILL-LEVEL u1)

;; Valid character classes
(define-constant CLASS-WARRIOR "warrior")
(define-constant CLASS-MAGE "mage")
(define-constant CLASS-ROGUE "rogue")
(define-constant CLASS-HEALER "healer")

;; Data maps
;; Stores character profiles for each user
(define-map characters 
  principal 
  {
    name: (string-ascii 50),
    class: (string-ascii 20),
    level: uint,
    experience: uint,
    created-at: uint
  }
)

;; Stores character skill levels
(define-map character-skills
  { owner: principal, skill-name: (string-ascii 30) }
  { level: uint }
)

;; Data variables
(define-data-var total-characters uint u0)

;; Private functions

;; Checks if a character class is valid
(define-private (is-valid-class (class (string-ascii 20)))
  (or
    (is-eq class CLASS-WARRIOR)
    (is-eq class CLASS-MAGE)
    (is-eq class CLASS-ROGUE)
    (is-eq class CLASS-HEALER)
  )
)

;; Calculates the experience required for the next level
(define-private (experience-for-level (level uint))
  (* level LEVEL-UP-FACTOR)
)

;; Checks if a character has enough experience to level up
(define-private (can-level-up (current-level uint) (current-xp uint))
  (and
    (< current-level MAX-LEVEL)
    (>= current-xp (experience-for-level current-level))
  )
)

;; Performs level-up calculations, returns new level and remaining XP
(define-private (calculate-level-up (current-level uint) (current-xp uint))
  (let ((xp-needed (experience-for-level current-level)))
    {
      new-level: (+ current-level u1),
      new-xp: (- current-xp xp-needed)
    }
  )
)

;; Public functions

;; Creates a new character for the user
(define-public (create-character (name (string-ascii 50)) (class (string-ascii 20)))
  (let ((sender tx-sender))
    (asserts! (is-none (map-get? characters sender)) ERR-CHARACTER-EXISTS)
    (asserts! (is-valid-class class) ERR-INVALID-CLASS)
    
    ;; Create new character with level 1 and 0 experience
    (map-set characters sender {
      name: name,
      class: class,
      level: u1,
      experience: u0,
      created-at: block-height
    })
    
    ;; Increment total characters counter
    (var-set total-characters (+ (var-get total-characters) u1))
    
    (ok true)
  )
)

;; Updates a character's name
(define-public (update-character-name (new-name (string-ascii 50)))
  (let ((sender tx-sender)
        (character (unwrap! (map-get? characters sender) ERR-CHARACTER-NOT-FOUND)))
    
    (map-set characters sender (merge character { name: new-name }))
    (ok true)
  )
)

;; Adds experience points to a character and handles level-up
(define-public (add-experience (amount uint))
  (let ((sender tx-sender)
        (character (unwrap! (map-get? characters sender) ERR-CHARACTER-NOT-FOUND)))
    
    (asserts! (> amount u0) ERR-INVALID-EXPERIENCE)
    
    (let ((current-level (get level character))
          (new-xp (+ (get experience character) amount))
          (updated-character character))
      
      ;; Process level ups if character has enough XP
      (let ((level-up-result (process-level-ups current-level new-xp)))
        (map-set characters sender 
          (merge character {
            level: (get new-level level-up-result),
            experience: (get new-xp level-up-result)
          })
        )
        (ok (get new-level level-up-result))
      )
    )
  )
)

;; Helper function to process multiple level-ups in one transaction
(define-private (process-level-ups (current-level uint) (current-xp uint))
  (if (can-level-up current-level current-xp)
    (let ((level-up (calculate-level-up current-level current-xp)))
      ;; Recursively process additional level-ups if needed
      (process-level-ups (get new-level level-up) (get new-xp level-up))
    )
    ;; Return final level and xp values when no more level-ups are possible
    { new-level: current-level, new-xp: current-xp }
  )
)

;; Increments a character's skill level
(define-public (level-up-skill (skill-name (string-ascii 30)))
  (let ((sender tx-sender)
        (skill-key { owner: sender, skill-name: skill-name }))
    
    ;; Verify character exists
    (asserts! (is-some (map-get? characters sender)) ERR-CHARACTER-NOT-FOUND)
    
    ;; Get current skill level or default to 1
    (let ((current-skill-data (default-to { level: u0 } (map-get? character-skills skill-key))))
      (map-set character-skills 
        skill-key 
        { level: (+ (get level current-skill-data) u1) }
      )
      (ok true)
    )
  )
)

;; Sets a character's skill to a specific level (admin only)
(define-public (set-skill-level (owner principal) (skill-name (string-ascii 30)) (level uint))
  (let ((sender tx-sender)
        (contract-owner (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.questforge-admin get-contract-owner)))
    
    ;; Only the contract owner can set skill levels directly
    (asserts! (is-eq sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? characters owner)) ERR-CHARACTER-NOT-FOUND)
    (asserts! (> level u0) ERR-INVALID-SKILL)
    
    (map-set character-skills 
      { owner: owner, skill-name: skill-name }
      { level: level }
    )
    (ok true)
  )
)

;; Read-only functions

;; Gets a character's profile
(define-read-only (get-character (owner principal))
  (map-get? characters owner)
)

;; Gets a character's current level
(define-read-only (get-character-level (owner principal))
  (match (map-get? characters owner)
    character (ok (get level character))
    ERR-CHARACTER-NOT-FOUND
  )
)

;; Gets a character's current experience points
(define-read-only (get-character-experience (owner principal))
  (match (map-get? characters owner)
    character (ok (get experience character))
    ERR-CHARACTER-NOT-FOUND
  )
)

;; Gets the experience needed for the next level
(define-read-only (get-next-level-experience (owner principal))
  (match (map-get? characters owner)
    character (ok (experience-for-level (get level character)))
    ERR-CHARACTER-NOT-FOUND
  )
)

;; Gets a character's skill level
(define-read-only (get-skill-level (owner principal) (skill-name (string-ascii 30)))
  (match (map-get? character-skills { owner: owner, skill-name: skill-name })
    skill-data (ok (get level skill-data))
    (ok DEFAULT-SKILL-LEVEL)  ;; Default to level 1 if skill not found
  )
)

;; Gets the total number of characters created
(define-read-only (get-total-characters)
  (var-get total-characters)
)

;; Checks if a user has a character
(define-read-only (has-character (owner principal))
  (is-some (map-get? characters owner))
)