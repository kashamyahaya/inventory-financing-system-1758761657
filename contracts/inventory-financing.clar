;; inventory-financing
;; Inventory-backed financing platform using IoT sensors to monitor collateral
;; and automate lending decisions based on real-time inventory data

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_INVENTORY (err u102))
(define-constant ERR_LOAN_NOT_FOUND (err u103))
(define-constant ERR_BORROWER_NOT_FOUND (err u104))
(define-constant ERR_LENDER_NOT_FOUND (err u105))
(define-constant ERR_INVALID_LTV_RATIO (err u106))
(define-constant ERR_SENSOR_DATA_INVALID (err u107))
(define-constant ERR_INVENTORY_NOT_FOUND (err u108))
(define-constant ERR_LOAN_DEFAULTED (err u109))
(define-constant ERR_PAYMENT_OVERDUE (err u110))

;; Financing parameters
(define-constant MIN_LOAN_AMOUNT u50000) ;; $500
(define-constant MAX_LOAN_AMOUNT u100000000) ;; $1M
(define-constant MAX_LTV_RATIO u8000) ;; 80%
(define-constant MIN_INVENTORY_VALUE u10000) ;; $100
(define-constant INTEREST_RATE_BASE u1000) ;; 10% base rate
(define-constant SENSOR_UPDATE_INTERVAL u144) ;; 10 minutes in blocks
(define-constant DEFAULT_THRESHOLD_DAYS u30) ;; 30 days

;; data maps
;; Borrower profiles and business information
(define-map borrowers
  { borrower: principal }
  {
    business-name: (string-ascii 128),
    industry-type: (string-ascii 64),
    business-address: (string-ascii 256),
    annual-revenue: uint,
    years-in-business: uint,
    credit-score: uint,
    total-borrowed: uint,
    payment-history: uint,
    default-count: uint,
    is-active: bool
  }
)

;; Lender information and capital availability
(define-map lenders
  { lender: principal }
  {
    institution-name: (string-ascii 128),
    available-capital: uint,
    total-lent: uint,
    interest-rate: uint,
    min-loan-amount: uint,
    max-loan-amount: uint,
    preferred-industries: (string-ascii 256),
    risk-tolerance: uint,
    is-active: bool
  }
)

;; Inventory items with IoT sensor data
(define-map inventory-items
  { borrower: principal, item-id: uint }
  {
    item-name: (string-ascii 128),
    category: (string-ascii 64),
    quantity: uint,
    unit-value: uint,
    total-value: uint,
    condition-score: uint,
    location: (string-ascii 128),
    sensor-id: (string-ascii 64),
    last-sensor-update: uint,
    storage-temperature: uint,
    storage-humidity: uint,
    is-collateral: bool
  }
)

;; Loans backed by inventory collateral
(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    lender: principal,
    loan-amount: uint,
    interest-rate: uint,
    loan-term: uint,
    collateral-value: uint,
    ltv-ratio: uint,
    monthly-payment: uint,
    total-paid: uint,
    payments-made: uint,
    loan-status: (string-ascii 16),
    origination-date: uint,
    maturity-date: uint,
    last-payment-date: uint,
    next-due-date: uint
  }
)

;; IoT sensor readings and inventory monitoring
(define-map sensor-data
  { sensor-id: (string-ascii 64), timestamp: uint }
  {
    borrower: principal,
    item-id: uint,
    quantity-reading: uint,
    temperature: uint,
    humidity: uint,
    movement-detected: bool,
    quality-score: uint,
    location-verified: bool,
    battery-level: uint,
    data-integrity: bool
  }
)

;; Loan payments and transaction history
(define-map payments
  { loan-id: uint, payment-id: uint }
  {
    payment-amount: uint,
    principal-amount: uint,
    interest-amount: uint,
    payment-date: uint,
    payment-method: (string-ascii 32),
    late-fee: uint,
    payment-status: (string-ascii 16)
  }
)

;; Contract state variables
(define-data-var loan-counter uint u0)
(define-data-var item-counter uint u0)
(define-data-var payment-counter uint u0)
(define-data-var total-loans-issued uint u0)
(define-data-var total-volume uint u0)
(define-data-var platform-fee-rate uint u200) ;; 2%
(define-data-var contract-paused bool false)

;; private functions
;; Calculate loan-to-value ratio based on current inventory value
(define-private (calculate-ltv-ratio (loan-amount uint) (collateral-value uint))
  (if (> collateral-value u0)
    (/ (* loan-amount u10000) collateral-value)
    u10000 ;; 100% if no collateral
  )
)

;; Calculate monthly payment based on loan terms (simplified without pow)
(define-private (calculate-monthly-payment (principal uint) (rate uint) (term uint))
  (let (
    (monthly-rate (/ rate u1200)) ;; Convert annual rate to monthly
    ;; Simplified calculation without pow function
    (base-payment (/ principal term))
    (interest-adjustment (/ (* principal monthly-rate) u10000))
  )
    (+ base-payment interest-adjustment)
  )
)

;; Assess inventory risk based on sensor data and market conditions
(define-private (assess-inventory-risk (borrower principal) (item-id uint))
  (let (
    (item-data (unwrap! (map-get? inventory-items { borrower: borrower, item-id: item-id }) u100))
    (base-risk u500) ;; Base risk score
    (condition-risk (if (< (get condition-score item-data) u70) u100 u0))
    (age-risk (if (> (- (+ stx-liquid-supply u1) (get last-sensor-update item-data)) SENSOR_UPDATE_INTERVAL) u50 u0))
  )
    (+ base-risk condition-risk age-risk)
  )
)

;; Update inventory valuation based on market conditions
(define-private (update-inventory-value (borrower principal) (item-id uint) (new-unit-value uint))
  (let (
    (item-data (unwrap! (map-get? inventory-items { borrower: borrower, item-id: item-id }) false))
    (new-total-value (* (get quantity item-data) new-unit-value))
  )
    (map-set inventory-items
      { borrower: borrower, item-id: item-id }
      (merge item-data {
        unit-value: new-unit-value,
        total-value: new-total-value
      })
    )
    true
  )
)

;; Validate sensor data integrity and freshness
(define-private (validate-sensor-data (sensor-id (string-ascii 64)) (timestamp uint))
  (let (
    (sensor-reading (map-get? sensor-data { sensor-id: sensor-id, timestamp: timestamp }))
    (current-time (+ stx-liquid-supply u1))
  )
    (match sensor-reading
      data-entry (and
                   (get data-integrity data-entry)
                   (< (- current-time timestamp) SENSOR_UPDATE_INTERVAL))
      false
    )
  )
)

;; public functions
;; Register as a borrower seeking inventory-backed financing
(define-public (register-borrower (business-name (string-ascii 128)) (industry-type (string-ascii 64)) (business-address (string-ascii 256)) (annual-revenue uint) (years-in-business uint) (credit-score uint))
  (begin
    (asserts! (not (var-get contract-paused)) (err u999))
    (asserts! (> annual-revenue u0) ERR_INVALID_AMOUNT)
    (asserts! (> years-in-business u0) ERR_INVALID_AMOUNT)
    (asserts! (<= credit-score u850) ERR_INVALID_AMOUNT) ;; Max credit score 850
    (map-set borrowers
      { borrower: tx-sender }
      {
        business-name: business-name,
        industry-type: industry-type,
        business-address: business-address,
        annual-revenue: annual-revenue,
        years-in-business: years-in-business,
        credit-score: credit-score,
        total-borrowed: u0,
        payment-history: u100, ;; Start with perfect payment history
        default-count: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Register as a lender providing inventory-backed financing
(define-public (register-lender (institution-name (string-ascii 128)) (available-capital uint) (interest-rate uint) (min-loan-amount uint) (max-loan-amount uint) (preferred-industries (string-ascii 256)) (risk-tolerance uint))
  (begin
    (asserts! (not (var-get contract-paused)) (err u999))
    (asserts! (> available-capital u0) ERR_INVALID_AMOUNT)
    (asserts! (>= min-loan-amount MIN_LOAN_AMOUNT) ERR_INVALID_AMOUNT)
    (asserts! (<= max-loan-amount MAX_LOAN_AMOUNT) ERR_INVALID_AMOUNT)
    (asserts! (<= risk-tolerance u1000) ERR_INVALID_AMOUNT) ;; Max risk tolerance 100%
    (map-set lenders
      { lender: tx-sender }
      {
        institution-name: institution-name,
        available-capital: available-capital,
        total-lent: u0,
        interest-rate: interest-rate,
        min-loan-amount: min-loan-amount,
        max-loan-amount: max-loan-amount,
        preferred-industries: preferred-industries,
        risk-tolerance: risk-tolerance,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Add inventory item with IoT sensor integration
(define-public (add-inventory-item (item-name (string-ascii 128)) (category (string-ascii 64)) (quantity uint) (unit-value uint) (location (string-ascii 128)) (sensor-id (string-ascii 64)))
  (let (
    (item-id (+ (var-get item-counter) u1))
    (total-value (* quantity unit-value))
    (current-timestamp (+ stx-liquid-supply u1))
  )
    (asserts! (not (var-get contract-paused)) (err u999))
    (asserts! (is-some (map-get? borrowers { borrower: tx-sender })) ERR_BORROWER_NOT_FOUND)
    (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
    (asserts! (> unit-value u0) ERR_INVALID_AMOUNT)
    (asserts! (>= total-value MIN_INVENTORY_VALUE) ERR_INSUFFICIENT_INVENTORY)
    
    ;; Add inventory item
    (map-set inventory-items
      { borrower: tx-sender, item-id: item-id }
      {
        item-name: item-name,
        category: category,
        quantity: quantity,
        unit-value: unit-value,
        total-value: total-value,
        condition-score: u100, ;; Start with perfect condition
        location: location,
        sensor-id: sensor-id,
        last-sensor-update: current-timestamp,
        storage-temperature: u20, ;; Default 20C
        storage-humidity: u50, ;; Default 50% humidity
        is-collateral: false
      }
    )
    
    ;; Update item counter
    (var-set item-counter item-id)
    (ok item-id)
  )
)

;; Apply for inventory-backed loan
(define-public (apply-for-loan (lender principal) (loan-amount uint) (loan-term uint) (collateral-item-ids (list 20 uint)))
  (let (
    (loan-id (+ (var-get loan-counter) u1))
    (borrower-data (unwrap! (map-get? borrowers { borrower: tx-sender }) ERR_BORROWER_NOT_FOUND))
    (lender-data (unwrap! (map-get? lenders { lender: lender }) ERR_LENDER_NOT_FOUND))
    (current-timestamp (+ stx-liquid-supply u1))
    ;; Simplified collateral calculation - in real implementation would sum all items
    (total-collateral-value u1000000) ;; Placeholder - would calculate from collateral-item-ids
    (ltv-ratio (calculate-ltv-ratio loan-amount total-collateral-value))
    (monthly-payment (calculate-monthly-payment loan-amount (get interest-rate lender-data) loan-term))
    (maturity-date (+ current-timestamp (* loan-term u144))) ;; Approximate blocks per month
  )
    (asserts! (not (var-get contract-paused)) (err u999))
    (asserts! (get is-active borrower-data) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active lender-data) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= loan-amount MIN_LOAN_AMOUNT) (<= loan-amount MAX_LOAN_AMOUNT)) ERR_INVALID_AMOUNT)
    (asserts! (<= ltv-ratio MAX_LTV_RATIO) ERR_INVALID_LTV_RATIO)
    (asserts! (>= (get available-capital lender-data) loan-amount) ERR_INSUFFICIENT_INVENTORY)
    (asserts! (and (>= loan-amount (get min-loan-amount lender-data))
                   (<= loan-amount (get max-loan-amount lender-data))) ERR_INVALID_AMOUNT)
    
    ;; Create loan
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        lender: lender,
        loan-amount: loan-amount,
        interest-rate: (get interest-rate lender-data),
        loan-term: loan-term,
        collateral-value: total-collateral-value,
        ltv-ratio: ltv-ratio,
        monthly-payment: monthly-payment,
        total-paid: u0,
        payments-made: u0,
        loan-status: "active",
        origination-date: current-timestamp,
        maturity-date: maturity-date,
        last-payment-date: u0,
        next-due-date: (+ current-timestamp u4320) ;; 30 days
      }
    )
    
    ;; Update lender's available capital
    (map-set lenders
      { lender: lender }
      (merge lender-data {
        available-capital: (- (get available-capital lender-data) loan-amount),
        total-lent: (+ (get total-lent lender-data) loan-amount)
      })
    )
    
    ;; Update borrower's total borrowed
    (map-set borrowers
      { borrower: tx-sender }
      (merge borrower-data {
        total-borrowed: (+ (get total-borrowed borrower-data) loan-amount)
      })
    )
    
    ;; Update counters
    (var-set loan-counter loan-id)
    (var-set total-loans-issued (+ (var-get total-loans-issued) u1))
    (var-set total-volume (+ (var-get total-volume) loan-amount))
    
    (ok loan-id)
  )
)

;; Process IoT sensor data update
(define-public (update-sensor-data (sensor-id (string-ascii 64)) (item-id uint) (quantity-reading uint) (temperature uint) (humidity uint) (movement-detected bool) (quality-score uint) (location-verified bool) (battery-level uint))
  (let (
    (current-timestamp (+ stx-liquid-supply u1))
    (borrower-data (unwrap! (map-get? borrowers { borrower: tx-sender }) ERR_BORROWER_NOT_FOUND))
  )
    (asserts! (not (var-get contract-paused)) (err u999))
    (asserts! (get is-active borrower-data) ERR_NOT_AUTHORIZED)
    (asserts! (> battery-level u0) ERR_SENSOR_DATA_INVALID)
    (asserts! (<= quality-score u100) ERR_SENSOR_DATA_INVALID)
    
    ;; Record sensor data
    (map-set sensor-data
      { sensor-id: sensor-id, timestamp: current-timestamp }
      {
        borrower: tx-sender,
        item-id: item-id,
        quantity-reading: quantity-reading,
        temperature: temperature,
        humidity: humidity,
        movement-detected: movement-detected,
        quality-score: quality-score,
        location-verified: location-verified,
        battery-level: battery-level,
        data-integrity: true
      }
    )
    
    ;; Update inventory item with sensor data
    (let ((item-data (map-get? inventory-items { borrower: tx-sender, item-id: item-id })))
      (match item-data
        existing-item
        (map-set inventory-items
          { borrower: tx-sender, item-id: item-id }
          (merge existing-item {
            quantity: quantity-reading,
            condition-score: quality-score,
            last-sensor-update: current-timestamp,
            storage-temperature: temperature,
            storage-humidity: humidity,
            total-value: (* quantity-reading (get unit-value existing-item))
          })
        )
        false ;; Item not found, ignore update
      )
    )
    
    (ok true)
  )
)

;; Make loan payment
(define-public (make-payment (loan-id uint) (payment-amount uint))
  (let (
    (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
    (payment-id (+ (var-get payment-counter) u1))
    (current-timestamp (+ stx-liquid-supply u1))
    (interest-portion (/ (* (get loan-amount loan-data) (get interest-rate loan-data)) u12000)) ;; Monthly interest
    (principal-portion (if (> payment-amount interest-portion)
                       (- payment-amount interest-portion)
                       u0))
  )
    (asserts! (not (var-get contract-paused)) (err u999))
    (asserts! (is-eq tx-sender (get borrower loan-data)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get loan-status loan-data) "active") ERR_LOAN_DEFAULTED)
    (asserts! (> payment-amount u0) ERR_INVALID_AMOUNT)
    
    ;; Record payment
    (map-set payments
      { loan-id: loan-id, payment-id: payment-id }
      {
        payment-amount: payment-amount,
        principal-amount: principal-portion,
        interest-amount: interest-portion,
        payment-date: current-timestamp,
        payment-method: "blockchain",
        late-fee: u0,
        payment-status: "completed"
      }
    )
    
    ;; Update loan with payment
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data {
        total-paid: (+ (get total-paid loan-data) payment-amount),
        payments-made: (+ (get payments-made loan-data) u1),
        last-payment-date: current-timestamp,
        next-due-date: (+ current-timestamp u4320), ;; Next 30 days
        loan-status: (if (>= (+ (get total-paid loan-data) payment-amount) (get loan-amount loan-data))
                      "paid-off" "active")
      })
    )
    
    ;; Update counters
    (var-set payment-counter payment-id)
    
    (ok true)
  )
)

;; Emergency pause contract (admin only)
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

;; Resume contract operations (admin only)
(define-public (resume-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-borrower-info (borrower principal))
  (map-get? borrowers { borrower: borrower })
)

(define-read-only (get-lender-info (lender principal))
  (map-get? lenders { lender: lender })
)

(define-read-only (get-loan-info (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-inventory-item (borrower principal) (item-id uint))
  (map-get? inventory-items { borrower: borrower, item-id: item-id })
)

(define-read-only (get-sensor-data (sensor-id (string-ascii 64)) (timestamp uint))
  (map-get? sensor-data { sensor-id: sensor-id, timestamp: timestamp })
)

(define-read-only (get-platform-stats)
  {
    total-loans: (var-get loan-counter),
    loans-issued: (var-get total-loans-issued),
    total-volume: (var-get total-volume),
    total-items: (var-get item-counter),
    platform-fee-rate: (var-get platform-fee-rate),
    contract-paused: (var-get contract-paused)
  }
)

