;; quest-system
;; 
;; This smart contract powers QuestForge's core quest functionality, enabling users to
;; create tasks (quests) with difficulty ratings, deadlines, categories, and reward values.
;; The system maintains quest status (active, completed, failed), validates completion 
;; requirements, and supports rich metadata to transform everyday tasks into an RPG-like
;; experience. Features include recurring quests and quest chains with prerequisites.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-QUEST-NOT-FOUND (err u1001))
(define-constant ERR-INVALID-PARAMETERS (err u1002))
(define-constant ERR-DEADLINE-PASSED (err u1003))
(define-constant ERR-PREREQUISITES-NOT-MET (err u1004))
(define-constant ERR-QUEST-ALREADY-COMPLETED (err u1005))
(define-constant ERR-QUEST-ALREADY-FAILED (err u1006))
(define-constant ERR-INVALID-STATUS-TRANSITION (err u1007))
(define-constant ERR-MAX-QUESTS-REACHED (err u1008))

;; Status enum values (used for quest-status)
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-FAILED u3)

;; Constants for validation
(define-constant MAX-QUESTS-PER-USER u100)
(define-constant MAX-TITLE-LENGTH u50)
(define-constant MAX-DESCRIPTION-LENGTH u500)
(define-constant MAX-DIFFICULTY u5)
(define-constant MAX-REWARD-VALUE u1000)

;; Data structures

;; Main quest storage
(define-map quests
  { quest-id: uint }
  {
    owner: principal,
    title: (string-utf8 50),
    description: (string-utf8 500),
    difficulty: uint,
    deadline: uint,
    category: (string-utf8 20),
    reward-value: uint,
    status: uint,
    created-at: uint,
    completed-at: (optional uint),
    recurring: bool,
    recurrence-interval: (optional uint),
    metadata: (optional (string-utf8 500))
  }
)

;; Track prerequisites for quest chains
(define-map quest-prerequisites
  { quest-id: uint }
  { prerequisite-quest-ids: (list 10 uint) }
)

;; Track quests by user for easier querying
(define-map user-quests
  { user: principal }
  { quest-ids: (list 100 uint) }
)

;; Counter for generating unique quest IDs
(define-data-var next-quest-id uint u1)

;; Private functions

;; Get the current time from the blockchain
(define-private (get-current-time)
  block-height
)

;; Check if the sender is the owner of a quest
(define-private (is-owner (quest-id uint))
  (let ((quest-data (get-quest-data quest-id)))
    (and
      (is-some quest-data)
      (is-eq tx-sender (get owner (unwrap-panic quest-data)))
    )
  )
)

;; Check if all prerequisites for a quest have been completed
(define-private (check-prerequisites (quest-id uint))
  (let (
    (prereqs (map-get? quest-prerequisites { quest-id: quest-id }))
  )
    (if (is-none prereqs)
      true
      (let (
        (prereq-ids (get prerequisite-quest-ids (unwrap-panic prereqs)))
      )
        (fold and true (map check-prereq-completed prereq-ids))
      )
    )
  )
)

;; Helper to check if a single prerequisite quest is completed
(define-private (check-prereq-completed (prereq-id uint))
  (let (
    (quest-data (map-get? quests { quest-id: prereq-id }))
  )
    (and
      (is-some quest-data)
      (is-eq (get status (unwrap-panic quest-data)) STATUS-COMPLETED)
    )
  )
)

;; Helper to add a quest ID to a user's quest list
(define-private (add-quest-to-user (user principal) (quest-id uint))
  (let (
    (current-quests (default-to { quest-ids: (list) } (map-get? user-quests { user: user })))
    (updated-quests (unwrap-panic (as-max-len? (append (get quest-ids current-quests) quest-id) u100)))
  )
    (map-set user-quests { user: user } { quest-ids: updated-quests })
    (ok true)
  )
)

;; Check if deadline has passed
(define-private (is-deadline-passed (deadline uint))
  (> (get-current-time) deadline)
)

;; Read-only functions

;; Get quest details by ID
(define-read-only (get-quest-data (quest-id uint))
  (map-get? quests { quest-id: quest-id })
)

;; Get a list of all quests owned by a user
(define-read-only (get-user-quests (user principal))
  (default-to { quest-ids: (list) } (map-get? user-quests { user: user }))
)

;; Get all prerequisite quests for a given quest
(define-read-only (get-quest-prerequisites (quest-id uint))
  (default-to { prerequisite-quest-ids: (list) } (map-get? quest-prerequisites { quest-id: quest-id }))
)

;; Check if a quest can be completed
(define-read-only (can-complete-quest (quest-id uint))
  (let (
    (quest-data (get-quest-data quest-id))
  )
    (if (is-none quest-data)
      ERR-QUEST-NOT-FOUND
      (let (
        (quest (unwrap-panic quest-data))
        (current-status (get status quest))
        (deadline (get deadline quest))
      )
        (cond
          (not (is-eq tx-sender (get owner quest))) ERR-NOT-AUTHORIZED
          (is-eq current-status STATUS-COMPLETED) ERR-QUEST-ALREADY-COMPLETED
          (is-eq current-status STATUS-FAILED) ERR-QUEST-ALREADY-FAILED
          (is-deadline-passed deadline) ERR-DEADLINE-PASSED
          (not (check-prerequisites quest-id)) ERR-PREREQUISITES-NOT-MET
          true (ok true)
        )
      )
    )
  )
)

;; Public functions

;; Create a new quest
(define-public (create-quest
    (title (string-utf8 50))
    (description (string-utf8 500))
    (difficulty uint)
    (deadline uint)
    (category (string-utf8 20))
    (reward-value uint)
    (recurring bool)
    (recurrence-interval (optional uint))
    (metadata (optional (string-utf8 500)))
    (prerequisite-quest-ids (list 10 uint))
  )
  (let (
    (current-user-quests (get quest-ids (get-user-quests tx-sender)))
    (quest-id (var-get next-quest-id))
    (current-time (get-current-time))
  )
    ;; Perform validations
    (asserts! (<= (len current-user-quests) MAX-QUESTS-PER-USER) ERR-MAX-QUESTS-REACHED)
    (asserts! (<= (len title) MAX-TITLE-LENGTH) ERR-INVALID-PARAMETERS)
    (asserts! (<= (len description) MAX-DESCRIPTION-LENGTH) ERR-INVALID-PARAMETERS)
    (asserts! (<= difficulty MAX-DIFFICULTY) ERR-INVALID-PARAMETERS)
    (asserts! (> deadline current-time) ERR-INVALID-PARAMETERS)
    (asserts! (<= reward-value MAX-REWARD-VALUE) ERR-INVALID-PARAMETERS)
    
    ;; Create the quest
    (map-set quests
      { quest-id: quest-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        difficulty: difficulty,
        deadline: deadline,
        category: category,
        reward-value: reward-value,
        status: STATUS-ACTIVE,
        created-at: current-time,
        completed-at: none,
        recurring: recurring,
        recurrence-interval: recurrence-interval,
        metadata: metadata
      }
    )
    
    ;; Store prerequisites if any
    (if (> (len prerequisite-quest-ids) u0)
      (map-set quest-prerequisites 
        { quest-id: quest-id }
        { prerequisite-quest-ids: prerequisite-quest-ids }
      )
      true
    )
    
    ;; Add to user's quest list
    (add-quest-to-user tx-sender quest-id)
    
    ;; Increment the quest ID counter
    (var-set next-quest-id (+ quest-id u1))
    
    (ok quest-id)
  )
)

;; Complete a quest
(define-public (complete-quest (quest-id uint))
  (let (
    (quest-opt (get-quest-data quest-id))
    (completion-result (can-complete-quest quest-id))
  )
    ;; First check if the quest can be completed
    (asserts! (is-ok completion-result) completion-result)
    
    (let (
      (quest (unwrap-panic quest-opt))
      (current-time (get-current-time))
    )
      ;; Update the quest status
      (map-set quests
        { quest-id: quest-id }
        (merge quest {
          status: STATUS-COMPLETED,
          completed-at: (some current-time)
        })
      )
      
      ;; If the quest is recurring, create a new instance
      (if (get recurring quest)
        (let (
          (recurrence-interval (default-to u0 (get recurrence-interval quest)))
          (new-deadline (+ (get deadline quest) recurrence-interval))
        )
          (if (> recurrence-interval u0)
            (create-quest
              (get title quest)
              (get description quest)
              (get difficulty quest)
              new-deadline
              (get category quest)
              (get reward-value quest)
              (get recurring quest)
              (get recurrence-interval quest)
              (get metadata quest)
              (list)  ;; No prerequisites for recurring quests
            )
            (ok quest-id)  ;; If no valid recurrence interval, just return current quest id
          )
        )
        (ok quest-id)
      )
    )
  )
)

;; Fail a quest (can be called by owner or automatically by a checker)
(define-public (fail-quest (quest-id uint))
  (let (
    (quest-opt (get-quest-data quest-id))
  )
    (asserts! (is-some quest-opt) ERR-QUEST-NOT-FOUND)
    
    (let (
      (quest (unwrap-panic quest-opt))
    )
      ;; Ensure caller is the owner
      (asserts! (is-eq tx-sender (get owner quest)) ERR-NOT-AUTHORIZED)
      
      ;; Check that quest is in a valid state to be failed
      (asserts! (is-eq (get status quest) STATUS-ACTIVE) ERR-INVALID-STATUS-TRANSITION)
      
      ;; Update the quest status
      (map-set quests
        { quest-id: quest-id }
        (merge quest {
          status: STATUS-FAILED
        })
      )
      
      (ok quest-id)
    )
  )
)

;; Update quest details (only specific fields can be updated)
(define-public (update-quest
    (quest-id uint)
    (title (optional (string-utf8 50)))
    (description (optional (string-utf8 500)))
    (deadline (optional uint))
    (category (optional (string-utf8 20)))
    (reward-value (optional uint))
    (metadata (optional (string-utf8 500)))
  )
  (let (
    (quest-opt (get-quest-data quest-id))
  )
    (asserts! (is-some quest-opt) ERR-QUEST-NOT-FOUND)
    
    (let (
      (quest (unwrap-panic quest-opt))
      (current-time (get-current-time))
    )
      ;; Ensure caller is the owner
      (asserts! (is-eq tx-sender (get owner quest)) ERR-NOT-AUTHORIZED)
      
      ;; Ensure quest is still active
      (asserts! (is-eq (get status quest) STATUS-ACTIVE) ERR-INVALID-STATUS-TRANSITION)
      
      ;; Validate new deadline if provided
      (if (is-some deadline)
        (asserts! (> (unwrap-panic deadline) current-time) ERR-INVALID-PARAMETERS)
        true
      )
      
      ;; Validate reward value if provided
      (if (is-some reward-value)
        (asserts! (<= (unwrap-panic reward-value) MAX-REWARD-VALUE) ERR-INVALID-PARAMETERS)
        true
      )
      
      ;; Update the quest with provided fields
      (map-set quests
        { quest-id: quest-id }
        (merge quest {
          title: (default-to (get title quest) title),
          description: (default-to (get description quest) description),
          deadline: (default-to (get deadline quest) deadline),
          category: (default-to (get category quest) category),
          reward-value: (default-to (get reward-value quest) reward-value),
          metadata: (if (is-some metadata) metadata (get metadata quest))
        })
      )
      
      (ok quest-id)
    )
  )
)

;; Update quest prerequisites
(define-public (update-quest-prerequisites
    (quest-id uint)
    (prerequisite-quest-ids (list 10 uint))
  )
  (let (
    (quest-opt (get-quest-data quest-id))
  )
    (asserts! (is-some quest-opt) ERR-QUEST-NOT-FOUND)
    
    (let (
      (quest (unwrap-panic quest-opt))
    )
      ;; Ensure caller is the owner
      (asserts! (is-eq tx-sender (get owner quest)) ERR-NOT-AUTHORIZED)
      
      ;; Ensure quest is still active
      (asserts! (is-eq (get status quest) STATUS-ACTIVE) ERR-INVALID-STATUS-TRANSITION)
      
      ;; Update prerequisites
      (map-set quest-prerequisites
        { quest-id: quest-id }
        { prerequisite-quest-ids: prerequisite-quest-ids }
      )
      
      (ok quest-id)
    )
  )
)