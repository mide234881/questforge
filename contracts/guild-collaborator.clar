;; guild-collaborator
;; This contract enables social features and collaborative questing between users in the QuestForge platform.
;; It allows users to form guilds, share quests, embark on group challenges, and verify each other's task completion.
;; The contract implements accountability features and competition through leaderboards to transform individual 
;; productivity into a social experience that increases motivation through positive peer pressure.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-GUILD-EXISTS (err u101))
(define-constant ERR-GUILD-NOT-FOUND (err u102))
(define-constant ERR-USER-ALREADY-IN-GUILD (err u103))
(define-constant ERR-USER-NOT-IN-GUILD (err u104))
(define-constant ERR-NOT-GUILD-LEADER (err u105))
(define-constant ERR-CHALLENGE-NOT-FOUND (err u106))
(define-constant ERR-ALREADY-VERIFIED (err u107))
(define-constant ERR-CANNOT-VERIFY-OWN-TASK (err u108))
(define-constant ERR-MAX-MEMBERS-REACHED (err u109))
(define-constant ERR-USER-NOT-FOUND (err u110))
(define-constant ERR-QUEST-NOT-FOUND (err u111))
(define-constant ERR-QUEST-ALREADY-SHARED (err u112))
(define-constant ERR-MAX-GUILDS-CREATED (err u113))

;; Data variables and maps

;; Maximum number of members in a guild
(define-constant MAX-GUILD-MEMBERS u50)
;; Maximum number of guilds a user can create
(define-constant MAX-CREATED-GUILDS u3)

;; Guild data structure
(define-map guilds
  {guild-id: uint}
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    leader: principal,
    created-at: uint,
    member-count: uint
  }
)

;; Keep track of guild membership
(define-map guild-members
  {guild-id: uint, member: principal}
  {joined-at: uint}
)

;; Track guild IDs by leader
(define-map guilds-by-leader
  {leader: principal}
  {guild-ids: (list 10 uint)}
)

;; Track guild memberships by user
(define-map user-guild-memberships
  {user: principal}
  {guild-ids: (list 10 uint)}
)

;; Shared quests within guilds
(define-map shared-quests
  {guild-id: uint, quest-id: uint}
  {
    title: (string-ascii 100),
    shared-by: principal,
    shared-at: uint,
    participants: (list 50 principal)
  }
)

;; Guild challenges for collaborative questing
(define-map guild-challenges
  {challenge-id: uint}
  {
    guild-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 200),
    created-by: principal,
    created-at: uint,
    end-time: uint,
    participants: (list 50 principal)
  }
)

;; Task verification tracking
(define-map task-verifications
  {task-id: uint, verified-by: principal}
  {
    guild-id: uint,
    verified-at: uint,
    owner: principal
  }
)

;; Guild activity feed
(define-map guild-activity
  {guild-id: uint, activity-id: uint}
  {
    activity-type: (string-ascii 20),
    user: principal,
    description: (string-ascii 200),
    timestamp: uint
  }
)

;; Guild leaderboard scores
(define-map guild-leaderboard
  {guild-id: uint, member: principal}
  {
    completed-tasks: uint,
    verified-tasks: uint,
    challenges-participated: uint,
    total-points: uint
  }
)

;; Counter for guild IDs
(define-data-var next-guild-id uint u1)
;; Counter for challenge IDs
(define-data-var next-challenge-id uint u1)
;; Counter for activity IDs
(define-data-var next-activity-id-by-guild (map uint uint) (map))

;; Private functions

;; Add an activity to the guild activity feed
(define-private (add-guild-activity (guild-id uint) (activity-type (string-ascii 20)) (user principal) (description (string-ascii 200)))
  (let 
    (
      (activity-id (default-to u1 (map-get? next-activity-id-by-guild guild-id)))
    )
    (map-set guild-activity
      {guild-id: guild-id, activity-id: activity-id}
      {
        activity-type: activity-type,
        user: user,
        description: description,
        timestamp: (unwrap-panic (get-block-info? time (- block-height u1)))
      }
    )
    (map-set next-activity-id-by-guild guild-id (+ activity-id u1))
    activity-id
  )
)

;; Update user's guild membership list
(define-private (add-guild-to-user-memberships (user principal) (guild-id uint))
  (let
    (
      (current-memberships (default-to {guild-ids: (list)} (map-get? user-guild-memberships {user: user})))
      (updated-memberships (unwrap-panic (as-max-len? (append (get guild-ids current-memberships) guild-id) u10)))
    )
    (map-set user-guild-memberships 
      {user: user} 
      {guild-ids: updated-memberships}
    )
  )
)

;; Remove a guild from user's membership list
(define-private (remove-guild-from-user-memberships (user principal) (guild-id uint))
  (let
    (
      (current-memberships (default-to {guild-ids: (list)} (map-get? user-guild-memberships {user: user})))
      (updated-memberships (filter filter-guild-id (get guild-ids current-memberships)))
    )
    (map-set user-guild-memberships 
      {user: user} 
      {guild-ids: updated-memberships}
    )
  )
  (where filter-guild-id (lambda (id uint) (not (is-eq id guild-id))))
)

;; Initialize leaderboard entry for a new member
(define-private (init-leaderboard-entry (guild-id uint) (member principal))
  (map-set guild-leaderboard
    {guild-id: guild-id, member: member}
    {
      completed-tasks: u0,
      verified-tasks: u0,
      challenges-participated: u0,
      total-points: u0
    }
  )
)

;; Update leaderboard score
(define-private (update-leaderboard (guild-id uint) (member principal) (category (string-ascii 20)) (points uint))
  (let
    (
      (current-scores (default-to 
        {completed-tasks: u0, verified-tasks: u0, challenges-participated: u0, total-points: u0} 
        (map-get? guild-leaderboard {guild-id: guild-id, member: member})))
      
      (new-completed-tasks (if (is-eq category "completed-task") 
                             (+ (get completed-tasks current-scores) u1) 
                             (get completed-tasks current-scores)))
      
      (new-verified-tasks (if (is-eq category "verified-task") 
                            (+ (get verified-tasks current-scores) u1) 
                            (get verified-tasks current-scores)))
      
      (new-challenges (if (is-eq category "challenge") 
                        (+ (get challenges-participated current-scores) u1) 
                        (get challenges-participated current-scores)))
      
      (new-total (+ (get total-points current-scores) points))
    )
    (map-set guild-leaderboard
      {guild-id: guild-id, member: member}
      {
        completed-tasks: new-completed-tasks,
        verified-tasks: new-verified-tasks,
        challenges-participated: new-challenges,
        total-points: new-total
      }
    )
  )
)

;; Check if user is in guild
(define-private (is-guild-member (guild-id uint) (user principal))
  (is-some (map-get? guild-members {guild-id: guild-id, member: user}))
)

;; Public functions

;; Create a new guild
(define-public (create-guild (name (string-ascii 50)) (description (string-ascii 200)))
  (let
    (
      (guild-id (var-get next-guild-id))
      (leader tx-sender)
      (created-guilds (default-to {guild-ids: (list)} (map-get? guilds-by-leader {leader: leader})))
      (guild-count (len (get guild-ids created-guilds)))
    )
    ;; Check if leader has reached max guild creation limit
    (asserts! (< guild-count MAX-CREATED-GUILDS) ERR-MAX-GUILDS-CREATED)
    
    ;; Create the guild
    (map-set guilds
      {guild-id: guild-id}
      {
        name: name,
        description: description,
        leader: leader,
        created-at: (unwrap-panic (get-block-info? time (- block-height u1))),
        member-count: u1
      }
    )
    
    ;; Add leader as first member
    (map-set guild-members 
      {guild-id: guild-id, member: leader}
      {joined-at: (unwrap-panic (get-block-info? time (- block-height u1)))}
    )
    
    ;; Initialize leader's leaderboard entry
    (init-leaderboard-entry guild-id leader)
    
    ;; Track guild ID for leader
    (map-set guilds-by-leader
      {leader: leader}
      {guild-ids: (unwrap-panic (as-max-len? (append (get guild-ids created-guilds) guild-id) u10))}
    )
    
    ;; Add guild to leader's memberships
    (add-guild-to-user-memberships leader guild-id)
    
    ;; Log activity
    (add-guild-activity guild-id "guild-created" leader description)
    
    ;; Increment the guild ID counter
    (var-set next-guild-id (+ guild-id u1))
    
    (ok guild-id)
  )
)

;; Join an existing guild
(define-public (join-guild (guild-id uint))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    ;; Check if guild exists
    (asserts! (is-some guild) ERR-GUILD-NOT-FOUND)
    
    ;; Check if user is already a member
    (asserts! (not (is-guild-member guild-id tx-sender)) ERR-USER-ALREADY-IN-GUILD)
    
    ;; Check if guild is full
    (asserts! (< (get member-count (unwrap-panic guild)) MAX-GUILD-MEMBERS) ERR-MAX-MEMBERS-REACHED)
    
    ;; Add user as member
    (map-set guild-members 
      {guild-id: guild-id, member: tx-sender}
      {joined-at: (unwrap-panic (get-block-info? time (- block-height u1)))}
    )
    
    ;; Initialize member's leaderboard entry
    (init-leaderboard-entry guild-id tx-sender)
    
    ;; Add guild to user's memberships
    (add-guild-to-user-memberships tx-sender guild-id)
    
    ;; Update member count
    (map-set guilds
      {guild-id: guild-id}
      (merge (unwrap-panic guild) {member-count: (+ (get member-count (unwrap-panic guild)) u1)})
    )
    
    ;; Log activity
    (add-guild-activity guild-id "member-joined" tx-sender (concat "joined the guild: " (get name (unwrap-panic guild))))
    
    (ok true)
  )
)

;; Leave a guild
(define-public (leave-guild (guild-id uint))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    ;; Check if guild exists
    (asserts! (is-some guild) ERR-GUILD-NOT-FOUND)
    
    ;; Check if user is a member
    (asserts! (is-guild-member guild-id tx-sender) ERR-USER-NOT-IN-GUILD)
    
    ;; Check if user is not the leader
    (asserts! (not (is-eq tx-sender (get leader (unwrap-panic guild)))) ERR-NOT-AUTHORIZED)
    
    ;; Remove user from guild
    (map-delete guild-members {guild-id: guild-id, member: tx-sender})
    
    ;; Remove guild from user's memberships
    (remove-guild-from-user-memberships tx-sender guild-id)
    
    ;; Update member count
    (map-set guilds
      {guild-id: guild-id}
      (merge (unwrap-panic guild) {member-count: (- (get member-count (unwrap-panic guild)) u1)})
    )
    
    ;; Log activity
    (add-guild-activity guild-id "member-left" tx-sender (concat "left the guild: " (get name (unwrap-panic guild))))
    
    (ok true)
  )
)

;; Share a quest with guild members
(define-public (share-quest (guild-id uint) (quest-id uint) (title (string-ascii 100)))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    ;; Check if guild exists
    (asserts! (is-some guild) ERR-GUILD-NOT-FOUND)
    
    ;; Check if user is a member
    (asserts! (is-guild-member guild-id tx-sender) ERR-USER-NOT-IN-GUILD)
    
    ;; Check if quest is already shared
    (asserts! (is-none (map-get? shared-quests {guild-id: guild-id, quest-id: quest-id})) ERR-QUEST-ALREADY-SHARED)
    
    ;; Share the quest
    (map-set shared-quests
      {guild-id: guild-id, quest-id: quest-id}
      {
        title: title,
        shared-by: tx-sender,
        shared-at: (unwrap-panic (get-block-info? time (- block-height u1))),
        participants: (list tx-sender)
      }
    )
    
    ;; Log activity
    (add-guild-activity guild-id "quest-shared" tx-sender (concat "shared a quest: " title))
    
    (ok true)
  )
)

;; Join a shared quest
(define-public (join-shared-quest (guild-id uint) (quest-id uint))
  (let
    (
      (shared-quest (map-get? shared-quests {guild-id: guild-id, quest-id: quest-id}))
    )
    ;; Check if shared quest exists
    (asserts! (is-some shared-quest) ERR-QUEST-NOT-FOUND)
    
    ;; Check if user is a guild member
    (asserts! (is-guild-member guild-id tx-sender) ERR-USER-NOT-IN-GUILD)
    
    ;; Update participants list
    (map-set shared-quests
      {guild-id: guild-id, quest-id: quest-id}
      (merge (unwrap-panic shared-quest) 
             {participants: (unwrap-panic 
                              (as-max-len? 
                                (append (get participants (unwrap-panic shared-quest)) tx-sender) 
                                u50))}))
    
    ;; Log activity
    (add-guild-activity 
      guild-id 
      "quest-joined" 
      tx-sender 
      (concat "joined a shared quest: " (get title (unwrap-panic shared-quest))))
    
    (ok true)
  )
)

;; Create a guild challenge
(define-public (create-guild-challenge 
                (guild-id uint) 
                (title (string-ascii 100)) 
                (description (string-ascii 200)) 
                (duration uint))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
      (challenge-id (var-get next-challenge-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Check if guild exists
    (asserts! (is-some guild) ERR-GUILD-NOT-FOUND)
    
    ;; Check if user is a member
    (asserts! (is-guild-member guild-id tx-sender) ERR-USER-NOT-IN-GUILD)
    
    ;; Create the challenge
    (map-set guild-challenges
      {challenge-id: challenge-id}
      {
        guild-id: guild-id,
        title: title,
        description: description,
        created-by: tx-sender,
        created-at: current-time,
        end-time: (+ current-time duration),
        participants: (list tx-sender)
      }
    )
    
    ;; Update creator's leaderboard
    (update-leaderboard guild-id tx-sender "challenge" u10)
    
    ;; Log activity
    (add-guild-activity guild-id "challenge-created" tx-sender (concat "created a guild challenge: " title))
    
    ;; Increment challenge ID
    (var-set next-challenge-id (+ challenge-id u1))
    
    (ok challenge-id)
  )
)

;; Join a guild challenge
(define-public (join-guild-challenge (challenge-id uint))
  (let
    (
      (challenge (map-get? guild-challenges {challenge-id: challenge-id}))
    )
    ;; Check if challenge exists
    (asserts! (is-some challenge) ERR-CHALLENGE-NOT-FOUND)
    
    (let
      (
        (guild-id (get guild-id (unwrap-panic challenge)))
        (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      )
      ;; Check if user is a guild member
      (asserts! (is-guild-member guild-id tx-sender) ERR-USER-NOT-IN-GUILD)
      
      ;; Check if challenge is still active
      (asserts! (< current-time (get end-time (unwrap-panic challenge))) (err u120))
      
      ;; Update participants list
      (map-set guild-challenges
        {challenge-id: challenge-id}
        (merge (unwrap-panic challenge) 
               {participants: (unwrap-panic 
                                (as-max-len? 
                                  (append (get participants (unwrap-panic challenge)) tx-sender) 
                                  u50))}))
      
      ;; Update participant's leaderboard
      (update-leaderboard guild-id tx-sender "challenge" u5)
      
      ;; Log activity
      (add-guild-activity 
        guild-id 
        "challenge-joined" 
        tx-sender 
        (concat "joined a guild challenge: " (get title (unwrap-panic challenge))))
      
      (ok true)
    )
  )
)

;; Verify a task completion for another guild member
(define-public (verify-task-completion (guild-id uint) (task-id uint) (task-owner principal))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    ;; Check if guild exists
    (asserts! (is-some guild) ERR-GUILD-NOT-FOUND)
    
    ;; Check if verifier is a guild member
    (asserts! (is-guild-member guild-id tx-sender) ERR-USER-NOT-IN-GUILD)
    
    ;; Check if task owner is a guild member
    (asserts! (is-guild-member guild-id task-owner) ERR-USER-NOT-FOUND)
    
    ;; Check that verifier is not the task owner
    (asserts! (not (is-eq tx-sender task-owner)) ERR-CANNOT-VERIFY-OWN-TASK)
    
    ;; Check if task was not already verified by this verifier
    (asserts! (is-none (map-get? task-verifications {task-id: task-id, verified-by: tx-sender})) ERR-ALREADY-VERIFIED)
    
    ;; Record the verification
    (map-set task-verifications
      {task-id: task-id, verified-by: tx-sender}
      {
        guild-id: guild-id,
        verified-at: (unwrap-panic (get-block-info? time (- block-height u1))),
        owner: task-owner
      }
    )
    
    ;; Update leaderboards
    (update-leaderboard guild-id task-owner "completed-task" u10)
    (update-leaderboard guild-id tx-sender "verified-task" u2)
    
    ;; Log activity
    (add-guild-activity guild-id "task-verified" tx-sender "verified a task completion")
    
    (ok true)
  )
)

;; Read-only functions

;; Get guild details
(define-read-only (get-guild-details (guild-id uint))
  (map-get? guilds {guild-id: guild-id})
)

;; Get guild members
(define-read-only (get-guild-members (guild-id uint) (limit uint) (offset uint))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    (if (is-some guild)
      ;; Implementation would be more complex in practice as you'd need to iterate through members
      ;; This is a simplified placeholder
      (ok {
        guild-id: guild-id,
        members: (list)  ;; In a real implementation, you would query and return actual members
      })
      ERR-GUILD-NOT-FOUND
    )
  )
)

;; Check if a user is a member of a guild
(define-read-only (is-member (guild-id uint) (user principal))
  (is-some (map-get? guild-members {guild-id: guild-id, member: user}))
)

;; Get user's guilds
(define-read-only (get-user-guilds (user principal))
  (default-to {guild-ids: (list)} (map-get? user-guild-memberships {user: user}))
)

;; Get shared quests in a guild
(define-read-only (get-guild-shared-quests (guild-id uint))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    (if (is-some guild)
      ;; Implementation would be more complex in practice
      ;; This is a simplified placeholder
      (ok {
        guild-id: guild-id,
        quests: (list)  ;; In a real implementation, you would query and return actual shared quests
      })
      ERR-GUILD-NOT-FOUND
    )
  )
)

;; Get guild challenges
(define-read-only (get-guild-challenges (guild-id uint))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    (if (is-some guild)
      ;; Implementation would be more complex in practice
      ;; This is a simplified placeholder
      (ok {
        guild-id: guild-id,
        challenges: (list)  ;; In a real implementation, you would query and return actual challenges
      })
      ERR-GUILD-NOT-FOUND
    )
  )
)

;; Get guild leaderboard
(define-read-only (get-guild-leaderboard (guild-id uint))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    (if (is-some guild)
      ;; Implementation would be more complex in practice
      ;; This is a simplified placeholder
      (ok {
        guild-id: guild-id,
        leaderboard: (list)  ;; In a real implementation, you would query and return actual leaderboard entries
      })
      ERR-GUILD-NOT-FOUND
    )
  )
)

;; Get guild activity feed
(define-read-only (get-guild-activity-feed (guild-id uint) (limit uint))
  (let
    (
      (guild (map-get? guilds {guild-id: guild-id}))
    )
    (if (is-some guild)
      ;; Implementation would be more complex in practice
      ;; This is a simplified placeholder
      (ok {
        guild-id: guild-id,
        activities: (list)  ;; In a real implementation, you would query and return actual activity entries
      })
      ERR-GUILD-NOT-FOUND
    )
  )
)

;; Get task verifications
(define-read-only (get-task-verifications (task-id uint))
  ;; Implementation would be more complex in practice
  ;; This is a simplified placeholder
  (ok {
    task-id: task-id,
    verifications: (list)  ;; In a real implementation, you would query and return actual verification entries
  })
)