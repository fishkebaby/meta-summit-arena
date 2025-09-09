;; Meta Summit Arena - Cross-Chain Competitive Gaming Ecosystem

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-TOURNAMENT (err u101))
(define-constant ERR-TOURNAMENT-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-SKILL-RATING (err u103))
(define-constant ERR-TOURNAMENT-EXPIRED (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-INVALID-AMOUNT (err u106))
(define-constant ERR-ROUND-NOT-READY (err u107))
(define-constant ERR-SKILL-RATING-LOCKED (err u108))
(define-constant ERR-INVALID-PHASE (err u109))
(define-constant ERR-ORACLE-ERROR (err u110))
(define-constant ERR-INSUFFICIENT-FUNDS (err u111))
(define-constant ERR-TOURNAMENT-ALREADY-EXECUTED (err u112))

;; Contract Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant SKILL-RATING-DECAY-RATE u5) ;; 5% per cycle
(define-constant MIN-TOURNAMENT-SKILL-RATING u100)
(define-constant VOTING-PERIOD u1008) ;; ~1 week in blocks
(define-constant PREPARATION-PERIOD u144) ;; ~1 day in blocks
(define-constant QUADRATIC-SCALING u10000)

;; Data Variables
(define-data-var tournament-counter uint u0)
(define-data-var prize-pool-balance uint u0)
(define-data-var skill-rating-decay-cycle uint u0)
(define-data-var oracle-address (optional principal) none)
(define-data-var arena-paused bool false)
(define-data-var min-quorum uint u1000)

;; Tournament Structure
(define-map tournaments uint {
    id: uint,
    organizer: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    requested-amount: uint,
    phase: (string-ascii 20), ;; "preparation", "voting", "execution", "completed", "cancelled"
    created-at: uint,
    voting-ends-at: uint,
    yes-votes: uint,
    no-votes: uint,
    skill-rating-weighted-yes: uint,
    skill-rating-weighted-no: uint,
    rounds-completed: uint,
    total-rounds: uint,
    performance-score: uint,
    game-modes: (list 5 (string-ascii 20)),
    executed: bool,
    prizes-distributed: uint
})

;; Player Skill Rating System
(define-map player-skill-rating principal {
    base-skill-rating: uint,
    decay-adjusted: uint,
    last-activity: uint,
    successful-tournaments: uint,
    failed-tournaments: uint,
    voting-accuracy: uint,
    locked-skill-rating: uint,
    skill-rating-source: (string-ascii 50)
})

;; Voting Records
(define-map votes {tournament-id: uint, voter: principal} {
    vote-weight: uint,
    skill-rating-at-vote: uint,
    vote-direction: bool, ;; true for yes, false for no
    timestamp: uint,
    quadratic-weight: uint
})

;; Tournament Game Mode System
(define-map tournament-game-modes {tournament-id: uint, mode: (string-ascii 20)} {
    confidence-score: uint,
    historical-success-rate: uint,
    similar-tournaments: (list 10 uint),
    difficulty-assessment: uint
})

;; Round Tracking
(define-map tournament-rounds {tournament-id: uint, round-id: uint} {
    description: (string-utf8 200),
    target-date: uint,
    completion-date: (optional uint),
    required-amount: uint,
    verification-method: (string-ascii 30),
    completed: bool,
    oracle-verified: bool
})

;; Prize Pool Management
(define-map prize-allocations uint {
    tournament-id: uint,
    allocated-amount: uint,
    distributed-amount: uint,
    locked-until: uint,
    reallocation-target: (optional uint)
})

;; Skill Rating Appeals
(define-map skill-rating-appeals principal {
    appeal-reason: (string-utf8 300),
    requested-adjustment: int,
    submitted-at: uint,
    status: (string-ascii 20), ;; "pending", "approved", "rejected"
    reviewed-by: (optional principal)
})

;; Oracle Data Integration
(define-map oracle-requests uint {
    request-type: (string-ascii 30),
    tournament-id: uint,
    data-hash: (buff 32),
    timestamp: uint,
    verified: bool,
    result: (optional uint)
})

;; Helper function to calculate skill rating with decay
(define-private (calculate-current-skill-rating (player principal))
    (let (
        (stored-rating (default-to {
            base-skill-rating: u0,
            decay-adjusted: u0,
            last-activity: u0,
            successful-tournaments: u0,
            failed-tournaments: u0,
            voting-accuracy: u100,
            locked-skill-rating: u0,
            skill-rating-source: "none"
        } (map-get? player-skill-rating player)))
        (blocks-since-activity (- block-height (get last-activity stored-rating)))
        (decay-cycles (/ blocks-since-activity u144))
        (total-decay-rate (* decay-cycles SKILL-RATING-DECAY-RATE))
        (decay-multiplier (if (>= total-decay-rate u100) u0 (- u100 total-decay-rate)))
        (current-skill-rating (/ (* (get base-skill-rating stored-rating) decay-multiplier) u100))
    )
        current-skill-rating
    )
)

;; Utility Functions - Fixed to return proper response type
(define-private (update-player-activity (player principal))
    (let (
        (current-rating (default-to {
            base-skill-rating: u0,
            decay-adjusted: u0,
            last-activity: u0,
            successful-tournaments: u0,
            failed-tournaments: u0,
            voting-accuracy: u100,
            locked-skill-rating: u0,
            skill-rating-source: "activity"
        } (map-get? player-skill-rating player)))
    )
        (map-set player-skill-rating player (merge current-rating {
            last-activity: block-height
        }))
        (ok true)
    )
)

;; Administrative Functions
(define-public (initialize-arena (initial-prize-pool uint) (oracle-addr principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set prize-pool-balance initial-prize-pool)
        (var-set oracle-address (some oracle-addr))
        (ok true)
    )
)

(define-public (update-arena-parameters (new-min-quorum uint) (new-min-skill-rating uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> new-min-quorum u0) ERR-INVALID-AMOUNT)
        (asserts! (> new-min-skill-rating u0) ERR-INVALID-AMOUNT)
        (var-set min-quorum new-min-quorum)
        (ok true)
    )
)

(define-public (pause-arena)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set arena-paused true)
        (ok true)
    )
)

;; Tournament Lifecycle Management
(define-public (create-tournament 
    (title (string-utf8 100))
    (description (string-utf8 500))
    (requested-amount uint)
    (rounds uint)
    (game-modes (list 5 (string-ascii 20))))
    (let (
        (current-skill-rating (calculate-current-skill-rating tx-sender))
        (new-tournament-id (+ (var-get tournament-counter) u1))
        (current-block block-height)
    )
        (asserts! (not (var-get arena-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (>= current-skill-rating MIN-TOURNAMENT-SKILL-RATING) ERR-INSUFFICIENT-SKILL-RATING)
        (asserts! (> requested-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> rounds u0) ERR-INVALID-AMOUNT)
        (asserts! (<= requested-amount (var-get prize-pool-balance)) ERR-INSUFFICIENT-FUNDS)
        
        (map-set tournaments new-tournament-id {
            id: new-tournament-id,
            organizer: tx-sender,
            title: title,
            description: description,
            requested-amount: requested-amount,
            phase: "preparation",
            created-at: current-block,
            voting-ends-at: (+ current-block PREPARATION-PERIOD VOTING-PERIOD),
            yes-votes: u0,
            no-votes: u0,
            skill-rating-weighted-yes: u0,
            skill-rating-weighted-no: u0,
            rounds-completed: u0,
            total-rounds: rounds,
            performance-score: u0,
            game-modes: game-modes,
            executed: false,
            prizes-distributed: u0
        })
        
        (var-set tournament-counter new-tournament-id)
        (unwrap! (update-player-activity tx-sender) ERR-NOT-AUTHORIZED)
        (ok new-tournament-id)
    )
)

(define-public (advance-tournament-phase (tournament-id uint))
    (let (
        (tournament (unwrap! (map-get? tournaments tournament-id) ERR-TOURNAMENT-NOT-FOUND))
        (current-phase (get phase tournament))
        (current-block block-height)
    )
        (asserts! (not (var-get arena-paused)) ERR-NOT-AUTHORIZED)
        
        (if (is-eq current-phase "preparation")
            (begin
                (asserts! (> current-block (+ (get created-at tournament) PREPARATION-PERIOD)) ERR-INVALID-PHASE)
                (map-set tournaments tournament-id (merge tournament {phase: "voting"}))
                (ok "moved-to-voting")
            )
            (if (is-eq current-phase "voting")
                (begin
                    (asserts! (> current-block (get voting-ends-at tournament)) ERR-INVALID-PHASE)
                    (let ((tournament-approved (evaluate-tournament-outcome tournament-id)))
                        (if tournament-approved
                            (begin
                                (map-set tournaments tournament-id (merge tournament {phase: "execution"}))
                                (unwrap! (allocate-prizes tournament-id (get requested-amount tournament)) ERR-INSUFFICIENT-FUNDS)
                                (ok "moved-to-execution")
                            )
                            (begin
                                (map-set tournaments tournament-id (merge tournament {phase: "cancelled"}))
                                (ok "tournament-cancelled")
                            )
                        )
                    )
                )
                ERR-INVALID-PHASE
            )
        )
    )
)

;; Skill Rating-Weighted Voting System
(define-public (cast-vote (tournament-id uint) (vote-direction bool))
    (let (
        (tournament (unwrap! (map-get? tournaments tournament-id) ERR-TOURNAMENT-NOT-FOUND))
        (base-weight (calculate-current-skill-rating tx-sender))
        (current-block block-height)
        (vote-key {tournament-id: tournament-id, voter: tx-sender})
        (quadratic-weight (calculate-quadratic-weight base-weight))
    )
        (asserts! (not (var-get arena-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get phase tournament) "voting") ERR-INVALID-PHASE)
        (asserts! (< current-block (get voting-ends-at tournament)) ERR-TOURNAMENT-EXPIRED)
        (asserts! (is-none (map-get? votes vote-key)) ERR-ALREADY-VOTED)
        (asserts! (> base-weight u0) ERR-INSUFFICIENT-SKILL-RATING)
        
        (map-set votes vote-key {
            vote-weight: base-weight,
            skill-rating-at-vote: base-weight,
            vote-direction: vote-direction,
            timestamp: current-block,
            quadratic-weight: quadratic-weight
        })
        
        (if vote-direction
            (map-set tournaments tournament-id (merge tournament {
                yes-votes: (+ (get yes-votes tournament) u1),
                skill-rating-weighted-yes: (+ (get skill-rating-weighted-yes tournament) quadratic-weight)
            }))
            (map-set tournaments tournament-id (merge tournament {
                no-votes: (+ (get no-votes tournament) u1),
                skill-rating-weighted-no: (+ (get skill-rating-weighted-no tournament) quadratic-weight)
            }))
        )
        
        (unwrap! (update-player-activity tx-sender) ERR-NOT-AUTHORIZED)
        (ok true)
    )
)

;; Round and Prize Pool Management
(define-public (complete-round (tournament-id uint) (round-id uint) (verification-data (buff 32)))
    (let (
        (tournament (unwrap! (map-get? tournaments tournament-id) ERR-TOURNAMENT-NOT-FOUND))
        (round-key {tournament-id: tournament-id, round-id: round-id})
        (round (unwrap! (map-get? tournament-rounds round-key) ERR-ROUND-NOT-READY))
        (current-block block-height)
    )
        (asserts! (is-eq (get phase tournament) "execution") ERR-INVALID-PHASE)
        (asserts! (is-eq tx-sender (get organizer tournament)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed round)) ERR-ROUND-NOT-READY)
        
        ;; Submit oracle verification request
        (unwrap! (request-oracle-verification tournament-id round-id verification-data) ERR-ORACLE-ERROR)
        
        (map-set tournament-rounds round-key (merge round {
            completion-date: (some current-block),
            completed: true
        }))
        
        ;; Update tournament round completion count
        (map-set tournaments tournament-id (merge tournament {
            rounds-completed: (+ (get rounds-completed tournament) u1)
        }))
        
        ;; Release prizes if round verified
        (unwrap! (release-round-prizes tournament-id round-id) ERR-INSUFFICIENT-FUNDS)
        
        (ok true)
    )
)

(define-public (create-round 
    (tournament-id uint) 
    (round-id uint)
    (description (string-utf8 200))
    (target-date uint)
    (required-amount uint)
    (verification-method (string-ascii 30)))
    (let (
        (tournament (unwrap! (map-get? tournaments tournament-id) ERR-TOURNAMENT-NOT-FOUND))
        (round-key {tournament-id: tournament-id, round-id: round-id})
    )
        (asserts! (is-eq tx-sender (get organizer tournament)) ERR