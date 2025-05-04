(use-modules (backend transaction)
	     (backend account)
	     (backend journal)
	     (srfi srfi-19)
	     (ice-9 match))

(define (assert-equal label a b)
  (if (equal? a b)
      ; (format #t "PASS: ~a\n" label)
      (display "")
      (begin
	(format #t "FAIL: ~a\nExpected: ~a\nGot:     ~a\n" label b a)
	(error label))))

(define (make-test-transaction id date desc from-account to-account amount)
  (let ((from-posting (make-posting id date "" desc from-account (- amount) 0))
	(to-posting (make-posting id date "" desc to-account amount 0)))
    (make-transaction id date desc (list from-posting to-posting))))

(define (make-split-test-transaction id date desc from-account to-account1 to-account2 amount)
  (let ((from-posting (make-posting id date "" desc from-account (- amount) 0))
	(to-posting1 (make-posting id date "" desc to-account1 (/ amount 2) 0))
	(to-posting2 (make-posting id date "" desc to-account2 (/ amount 2) 0)))
    (make-transaction id date desc (list from-posting to-posting1 to-posting2))))

(define transactions
  (list
   (make-split-test-transaction
    1
    (make-date 0 0 0 0 1 1 2023 0)
    "Initial Deposit"
    "Equity:OpeningBalances"
    "Assets:Checking"
    "Assets:NavyFed"
    2000.00)

   (make-test-transaction
    2
    (make-date 0 0 0 0 15 1 2023 0)
    "Paycheck"
    "Income:Salary"
    "Assets:Checking"
    2000.00)

   (make-test-transaction
    3
    (make-date 0 0 0 0 20 1 2023 0)
    "Rent Payment"
    "Assets:Checking"
    "Expenses:Housing:Rent"
    1200.00)

   (make-test-transaction
    4
    (make-date 0 0 0 0 22 1 2023 0)
    "Groceries"
    "Assets:Checking"
    "Expenses:Food:Groceries"
    150.00)

   (make-test-transaction
    5
    (make-date 0 0 0 0 25 1 2023 0)
    "Restaturant"
    "Assets:Checking"
    "Expenses:Food:Dining"
    75.00)))

;; Tests

(define journal (build-journal-from-transactions transactions))
(define accounts (journal-accounts journal))

(assert-equal "Assets balance" 2575.0 (get-account-balance journal "Assets"))
(assert-equal "Assets:Checking balance" 1575.0 (get-account-balance journal "Assets:Checking"))
(assert-equal "Assets:NavyFed balance" 1000.0 (get-account-balance journal "Assets:NavyFed"))
(assert-equal "Equity Balance" -2000.0 (get-account-balance journal "Equity"))
(assert-equal "Equity:OpeningBalances balance" -2000.0 (get-account-balance journal "Equity:OpeningBalances"))
(assert-equal "Income balance" -2000.0 (get-account-balance journal "Income"))
(assert-equal "Income:Salary balance" -2000.0 (get-account-balance journal "Income:Salary"))
(assert-equal "Expenses balance" 1425.0 (get-account-balance journal "Expenses"))
(assert-equal "Expenses:Housing balance" 1200.0 (get-account-balance journal "Expenses:Housing"))


