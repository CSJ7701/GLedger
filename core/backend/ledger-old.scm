(define-module (backend ledger-old)
  #:export (make-ledger
	    ledger?
	    ledger-transactions
	    ledger-accounts
	    ledger-account-index

	    ;; Ledger Creation
	    build-ledger-from-transactions

	    ;; Query Functions
	    get-account-transactions
	    get-transaction-accounts
	    filter-ledger-transactions
	    filter-ledger-accounts

	    ;; Account access
	    get-account
	    get-account-balance

	    ;; Account Hierarchy Functions
	    build-account-hierarchy
	    extract-account-names
	    propagate-balances

	    ;; Index and Lookup Functions
	    build-account-index
	    postings-for-account
	    transaction-for-account?
	    ))

(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (srfi srfi-19)
	     (ice-9 regex)
	     (ice-9 hash-table)
	     (backend account)
	     (backend transaction))

(define-record-type <ledger>
  (make-ledger transactions accounts account-index)
  ledger?
  (transactions ledger-transactions)
  (accounts ledger-accounts)
  (account-index ledger-account-index))

(define (build-ledger-from-transactions transactions)
  "Build a complete ledger with accounts and propogated balances from transactions"
  (let* ((direct-accounts (calculate-direct-accounts transactions))
	 (propagated-accounts (propagate-balances direct-accounts))
	 (account-index (make-hash-table)))
    ;; Build account index
    (for-each
     (lambda (acc)
       (hash-set! account-index (account-name acc) acc))
     propagated-accounts)
    (make-ledger transactions propagated-accounts account-index)))

(define (extract-account-names transactions)
  "Extract all unique account names from transactions."
  (delete-duplicates
   (append-map
    (lambda (txn)
      (map posting-account (transaction-postings txn)))
    transactions)
   string=?))

(define (get-all-account-paths account-names)
  "Get all account names including parent paths"
  (delete-duplicates
   (append account-names
	   (append-map
	    (lambda (name)
	      (generate-parent-paths name))
	    account-names))
   string=?))

(define (generate-parent-paths account-name)
  "Generate all parent paths for an account"
  (let loop ((components (string-split account-name #\:))
	     (paths '())
	     (current ""))
    (if (null? components)
	paths
	(let ((new-path (if (string-null? current)
			    (car components)
			    (string-append current ":" (car components)))))
	  (loop (cdr components)
		(cons new-path paths)
		new-path)))))

(define (calculate-direct-accounts transactions)
  "Create accounts with balances directly from transactions,
including parent accounts with zero balances."
  (let* ((direct-account-names (extract-account-names transactions))
	 (all-account-names (get-all-account-paths direct-account-names))
	 (account-map (make-hash-table)))
    ;; Initialize all accounts with 0 balance
    (for-each
     (lambda (name)
       (hash-set! account-map name (make-account name 0)))
     all-account-names)
    ;; Apply transaction amounts directly to accounts
    (for-each
     (lambda (txn)
       (for-each
	(lambda (posting)
	  (let* ((account-name (posting-account posting))
		 (account (hash-ref account-map account-name))
		 (current-balance (account-balance account))
		 (new-balance (+ current-balance (posting-amount posting))))
	    (hash-set! account-map account-name
		       (make-account account-name new-balance))))
	(transaction-postings txn)))
     transactions)
    ;; Return the account list
    (hash-map->list (lambda (k v) v) account-map)))

(define (propagate-balances accounts)
  "Correctly propagate balances from leaf accounts to parent accounts without double-counting"
  ;; Create a map for efficient account lookup
  (let ((account-map (make-hash-table)))
    (for-each
     (lambda (acc)
       (hash-set! account-map (account-name acc) acc))
     accounts)

    ;; Calculate total balance for each account
    ;; Start with direct balances
    (let ((result-accounts (map
			    (lambda (acc)
			      (make-account (account-name acc)
					    (account-balance acc)))
			    accounts)))
      ;; Sort accounts by depth (deepest first)
      (let ((sorted-accounts
	     (sort result-accounts
		   (lambda (a b)
		     (> (account-depth a) (account-depth b))))))
	;; Process each account (bottom up)
	(let loop ((remaining sorted-accounts)
		   (processed '()))
	  (if (null? remaining)
	      processed
	      (let* ((current (car remaining))
		     (current-name (account-name current))
		     (parent-name (account-parent-name current))
		     ;; Find in result set
		     (processed-current
		      (find (lambda (a)
			      (string=? (account-name a) current-name))
			    processed))
		     ;; If not found, use current account
		     (account-to-process (or processed-current current)))
		;; Propagate this account's balance to its parent (if any)
		(if parent-name
		    (let* ((parent
			    (find (lambda (a)
				    (string=? (account-name a) parent-name))
				  processed))
			   ;; If parent is not yet in processed list, create it
			   (parent-account
			    (or parent
				(let ((orig-parent (hash-ref account-map parent-name)))
				  (make-account parent-name
						(account-balance orig-parent)))))
			   ;; Get parent's current balance
			   (parent-balance (account-balance parent-account))
			   ;; Add current account's balance to parent
			   (updated-parent
			    (update-balance parent-account
					    (+ parent-balance
					       (account-balance account-to-process))))
			   ;; Update the processed list
			   (new-processed
			    (if parent
				;; Replace existing parent entry
				(map (lambda (a)
				       (if (string=? (account-name a) parent-name)
					   updated-parent
					   a))
				     processed)
				;; Add new parent entry
				(cons updated-parent processed))))

		      ;; Continue with next account
		      (loop (cdr remaining)
			    (if processed-current
				;; Current already in processed
				new-processed
				;; Add current to processed
				(cons account-to-process new-processed))))
		    ;; No parent, just add to processed if not already there.
		    (loop (cdr remaining)
			  (if processed-current
			      processed
			      (cons account-to-process processed)))))))))))

(define (build-account-hierarchy transactions)
  "Build a complete account hierarchy from transactions."
  (let* ((direct-accounts (calculate-direct-accounts transactions))
	 (accounts-with-propagated-balances (propagate-balances direct-accounts)))
    accounts-with-propagated-balances))
			 
;; Query Functions
(define (get-account-transactions account-name transactions)
  "Get all transactions affecting a specific account"
  (filter
   (lambda (txn)
     (any (lambda (posting)
	    (string=? (posting-account posting) account-name))
	  (transaction-postings txn)))
   transactions))

(define (get-account ledger account-name)
  "Get account by name from the ledger"
  (hash-ref (ledger-account-index ledger) account-name #f))

(define (get-account-balance ledger account-name)
  "Get the balance of an account by name from the ledger"
  (let ((account (get-account ledger account-name)))
    (if account
	(account-balance account)
	#f)))

(define (get-transaction-accounts transaction accounts)
  "Get all accounts affected by a transaction"
  (let ((affected-names (map posting-account (transaction-postings transaction))))
    (filter (lambda (acc)
	      (member (account-name acc) affected-names))
	    accounts)))

(define* (filter-ledger-transactions ledger
				     #:key account-pattern description-pattern
				     from-date to-date min-amount max-amount)
  "Filter transactions by various criteria"
  (let ((txns (ledger-transactions ledger)))
    (let ((filtered txns))
      ;; Filter by account pattern if specified
      (when account-pattern
	(let ((rx (make-regexp account-pattern)))
	  (set! filtered
		(filter
		 (lambda (txn)
		   (any (lambda (posting)
			  (regexp-exec rx (posting-account posting)))
			(transaction-postings txn)))
		 filtered))))
      ;; Filter by description-pattern if specified
      (when description-pattern
	(let ((rx (make-regexp description-pattern)))
	  (set! filtered
		(filter
		 (lambda (txn)
		   (regexp-exec rx (transaction-description txn)))
		 filtered))))
      ;; Filter by date range if specified
      (when (or from-date to-date)
	(set! filtered
	      (filter
	       (lambda (txn)
		 (let ((date (transaction-date txn)))
		   (and (or (not from-date)
			    (date>=? date from-date))
			(or (not to-date)
			    (date<=? date to-date)))))
	       filtered)))
      ;; Filter by amount range if specified
      (when (or min-amount max-amount)
	(set! filtered
	      (filter
	       (lambda (txn)
		 (let ((amount (transaction-amount txn)))
		   (and (or (not min-amount)
			    (>= amount min-amount))
			(or (not max-amount)
			    (<= amount max-amount)))))
	       filtered)))
      filtered)))

(define* (filter-ledger-accounts ledger #:key name-pattern min-balance max-balance)
  "Filter accounts by various criteria"
  (let ((accounts (ledger-accounts ledger)))
    (let ((filtered accounts))
      ;; Filter by name pattern if specified
      (when name-pattern
	(let ((rx (make-regexp name-pattern)))
	  (set! filtered
		(filter
		 (lambda (acc)
		   (regexp-exec rx (account-name acc)))
		 filtered))))
      ;; Filter by balance range if specified
      (when (or min-balance max-balance)
	(set! filtered
	      (filter
	       (lambda (acc)
		 (let ((balance (account-balance acc)))
		   (and (or (not min-balance)
			    (>= balance min-balance))
			(or (not max-balance)
			    (<= balance max-balance)))))
	       filtered))))
    filtered))

(define (build-account-index transactions)
  "Create a hash table mapping account names to lists of transactions"
  (let ((index (make-hash-table)))
    (for-each
     (lambda (txn)
       (for-each
	(lambda (posting)
	  (let* ((acc-name (posting-account posting))
		 (current (hash-ref index acc-name '())))
	    (hash-set! index acc-name (cons txn current))))
	(transaction-postings txn)))
     transactions)
    index))

(define (postings-for-account account-name txn)
  "Get all postings for a specific account in a transaction"
  (filter (lambda (p)
	    (string=? (posting-account p) account-name))
	  (transaction-postings txn)))

(define (transaction-for-account? account-name txn)
  "Check if a transaction affects a specific account."
  (any (lambda (p)
	 (string=? (posting-account p) account-name))
       (transaction-postings txn)))
