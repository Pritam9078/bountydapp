;; Challenge Reward System Smart Contract

;; Define SIP-010 trait (optional - only needed if you want to implement it)
(define-trait sip-010-trait
  (
    (transfer (uint principal principal) (response bool uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
  )
)

;; Data variables
(define-data-var admin principal tx-sender)
(define-data-var total-supply uint u0)
(define-data-var next-challenge-id uint u1)

;; Data maps
(define-map challenges 
  { id: uint } 
  { 
    question: (string-utf8 100), 
    answer: (string-utf8 50), 
    reward: uint,
    active: bool
  }
)

(define-map submissions 
  { task-id: uint, user: principal } 
  { solved: bool, timestamp: uint }
)

;; Define fungible token
(define-fungible-token reward-token)

;; Error constants
(define-constant ERR_NOT_ADMIN (err u100))
(define-constant ERR_ALREADY_SOLVED (err u101))
(define-constant ERR_WRONG_ANSWER (err u102))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_CHALLENGE_INACTIVE (err u105))
(define-constant ERR_INSUFFICIENT_BALANCE (err u106))

;; Success constant
(define-constant SUCCESS (ok true))

;; Admin functions
(define-public (add-challenge (question (string-utf8 100)) (answer (string-utf8 50)) (reward uint))
  (let ((challenge-id (var-get next-challenge-id)))
    (asserts! (is-eq tx-sender (var-get admin)) ERR_NOT_ADMIN)
    (map-set challenges 
      { id: challenge-id } 
      { 
        question: question, 
        answer: answer, 
        reward: reward,
        active: true
      }
    )
    (var-set next-challenge-id (+ challenge-id u1))
    (ok challenge-id)
  )
)

(define-public (deactivate-challenge (id uint))
  (let ((challenge-opt (map-get? challenges { id: id })))
    (asserts! (is-eq tx-sender (var-get admin)) ERR_NOT_ADMIN)
    (match challenge-opt
      challenge-data
      (begin
        (map-set challenges 
          { id: id }
          (merge challenge-data { active: false })
        )
        SUCCESS
      )
      ERR_NOT_FOUND
    )
  )
)

(define-public (change-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_NOT_ADMIN)
    (var-set admin new-admin)
    SUCCESS
  )
)

;; Mint reward tokens (private function)
(define-private (mint-reward (recipient principal) (amount uint))
  (begin
    (try! (ft-mint? reward-token amount recipient))
    (var-set total-supply (+ (var-get total-supply) amount))
    SUCCESS
  )
)

;; Main user function - submit answer
(define-public (submit-answer (id uint) (user-answer (string-utf8 50)))
  (let (
    (challenge-opt (map-get? challenges { id: id }))
    (submission-key { task-id: id, user: tx-sender })
    (existing-submission (map-get? submissions submission-key))
  )
    (match challenge-opt
      challenge-data
      (begin
        (asserts! (get active challenge-data) ERR_CHALLENGE_INACTIVE)
        (asserts! (is-none existing-submission) ERR_ALREADY_SOLVED)
        (if (is-eq (get answer challenge-data) user-answer)
            (begin
              (map-set submissions 
                submission-key 
                { solved: true, timestamp: stacks-block-height }
              )
              (try! (mint-reward tx-sender (get reward challenge-data)))
              SUCCESS
            )
            (begin
              (map-set submissions 
                submission-key 
                { solved: false, timestamp: stacks-block-height }
              )
              ERR_WRONG_ANSWER
            )
        )
      )
      ERR_NOT_FOUND
    )
  )
)

;; Read-only functions
(define-read-only (get-question (id uint))
  (match (map-get? challenges { id: id })
    challenge-data
    (if (get active challenge-data)
        (ok (get question challenge-data))
        ERR_CHALLENGE_INACTIVE
    )
    ERR_NOT_FOUND
  )
)

(define-read-only (get-challenge-info (id uint))
  (match (map-get? challenges { id: id })
    challenge-data
    (ok {
      question: (get question challenge-data),
      reward: (get reward challenge-data),
      active: (get active challenge-data)
    })
    ERR_NOT_FOUND
  )
)

(define-read-only (get-my-balance)
  (ok (ft-get-balance reward-token tx-sender))
)

(define-read-only (get-balance (user principal))
  (ok (ft-get-balance reward-token user))
)

(define-read-only (get-total-supply)
  (ok (var-get total-supply))
)

(define-read-only (get-admin)
  (ok (var-get admin))
)

(define-read-only (has-solved (id uint) (user principal))
  (match (map-get? submissions { task-id: id, user: user })
    submission-data
    (ok (get solved submission-data))
    (ok false)
  )
)

(define-read-only (get-next-challenge-id)
  (ok (var-get next-challenge-id))
)

;; Transfer function (if implementing SIP-010)
(define-public (transfer (amount uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) (err u403))
    (ft-transfer? reward-token amount sender recipient)
  )
)