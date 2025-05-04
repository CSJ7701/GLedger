(define-module (backend transaction)
  #:export (;; Posting record and procedures
	    make-posting
	    posting?
	    posting-txnid
	    posting-date
	    posting-code
	    posting-description
	    posting-account
	    posting-amount
	    posting-total
	    posting->string

	    ;; Transaction record and basic accessors
	    make-transaction
	    transaction?
	    transaction-id
	    transaction-date
	    transaction-description
	    transaction-postings
	    transaction->string

	    ;; Transaction computation procedures
	    transaction-amount ;; Takes transaction, returns int
	    transaction-accounts ;; Takes transaction, returns list of strings (full account names)
	    transaction-summary

	    ;; Transaction collection procedures
	    filter-transactions ;; Takes transaction and regex pattern. Returns list of transactions whose descriptions match that pattern 
	    sort-transactions-by-date ;; Takes list of transactions, returns sorted list 
	    group-transactions-by-month ;; Takes list of transactions, returns alist associating strings of "YYYY-MM" with lists of transactions.
	    ))

(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (srfi srfi-19)
	     (ice-9 regex)
	     (utils))

(define-record-type <posting>
  (make-posting txnid date code description account amount total)
  posting?
  (txnid posting-txnid)               ; ID of parent transaction 
  (date posting-date)                 ; Date of posting (SRFI-19 date)
  (code posting-code)                 ; Optional code field 
  (description posting-description)   ; Description of posting
  (account posting-account)           ; Account name 
  (amount posting-amount)             ; Monetary amount 
  (total posting-total))              ; Running total (optional - currently unused)

(define-record-type <transaction>
  (make-transaction id date description postings)
  transaction?
  (id transaction-id)                    ; Unique ID for the transaction 
  (date transaction-date)                ; Transaction date (SRFI-19 date)
  (description transaction-description)  ; Transaction description 
  (postings transaction-postings))       ; List of postings 

(define (posting->string p)
  "Convert a posting to a string representation suitable for display"
  (format #f "  ~a  ~a"
	  (posting-account p)
	  (posting-amount p)))

(define (transaction->string txn)
  "Convert a transaction to a string representation in hledger-like format"
  (string-append
   (format #f "~a ~a ~a\n"
	   (date->string (transaction-date txn) "~Y-~m-~d")
	   (transaction-id txn)
	   (transaction-description txn))
   (string-join
    (map posting->string (transaction-postings txn))
    "\n")))

(define (transaction-accounts txn)
  "Return a list of all accounts referenced in the transaction's postings"
  (map posting-account (transaction-postings txn)))

(define (transaction-amount txn)
  "Calculate the total positive amount of the transaction. 

In double entry accounting, the sum of all postings should be zero,
so we only count positive amounts to determine the transaction size."
  (let ((positive-postings (filter (lambda (p) (> (posting-amount p) 0))
				   (transaction-postings txn))))
    (if (null? positive-postings)
	0			    ; If no positive postings return 0
	(apply + (map posting-amount positive-postings)))))

(define (transaction-summary txn)
  "Generate a human readable summary of a transaction,
including date, description, and a summary of how it affects each account."
  (let* ((postings (transaction-postings txn))
	 (date (transaction-date txn))
	 (description (transaction-description txn))
	 (accounts (delete-duplicates (map posting-account postings) string=?))
	 (summary-parts
	  (map (lambda (account)
		 (let* ((account-postings (filter (lambda (p)
						    (string=? (posting-account p) account))
						  postings))
			(total-amount (apply + (map posting-amount account-postings))))
		   (format #f "~a: ~a" account total-amount)))
	       accounts)))
    (format #f "~a - ~a (~a)"
	    (date->string date "~Y/~m/~d")
	    description
	    (string-join summary-parts ", "))))

(define (filter-transactions transactions pattern)
  "Filter transactions where description matches the regex pattern"
  (let ((rx (make-regexp pattern)))
    (filter (lambda (txn)
	      (regexp-exec rx (transaction-description txn)))
	    transactions)))

(define (sort-transactions-by-date transactions)
  "Sort transactions in descending order by date (newest first)"
  (sort transactions
	(lambda (a b)
	  (date>? (transaction-date a) (transaction-date b)))))

(define (group-transactions-by-month transactions)
  "Group transactions by year-month, returning an association list.

Keys are strings in YYYY-MM format, values are lists of transactions.
Transactions within each month are in their original order."
  (let ((result '()))
    (for-each
     (lambda (txn)
       (let* ((date (transaction-date txn))
	      (year (date-year date))
	      (month (date-month date))
	      (key (format #f "~4d-~2,'0d" year month))
	      (existing (assoc key result)))
	 (if existing
	     (set-cdr! existing (cons txn (cdr existing)))
	     (set! result (acons key (list txn) result)))))
     transactions)
    (map (lambda (month-pair)
	   (cons (car month-pair)
		 (reverse (cdr month-pair))))
	 result)))


