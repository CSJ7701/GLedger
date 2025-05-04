(define-module (backend journal)
  #:export (make-journal
	    journal?
	    journal-transactions
	    journal-accounts
	    journal-account-index

	    ;; Journal Creation
	    build-journal-from-transactions

	    ;; Query Function
	    get-account-transactions
	    get-transaction-accounts
	    filter-journal-transactions
	    filter-journal-accounts

	    ;; Account Access
	    get-account
	    get-account-balance

	    ;; Account Hierarchy Functions
	    build-account-hierarchy
	    extract-account-names
	    propagate-balances))

(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (srfi srfi-19)
	     (ice-9 regex)
	     (ice-9 hash-table)
	     (backend account)
	     (backend transaction)
	     (utils))

(define-record-type <journal>
  (make-journal transactions accounts account-index)
  journal?
  (transactions journal-transactions)    ;; List of transaction records
  (accounts journal-accounts)            ;; List of account records, with calculated balances 
  (account-index journal-account-index)) ;; Hash table for quick account lookup (by name)

(define (build-journal-from-transactions transactions)
  "Build a complete journal with accounts and propagated balances from transactions

Arguments:
  transactions: A list of transaction records.

Returns:
  A journal record containing transactions, account hierarchy, and account index."
  (let* ((direct-accounts (calculate-direct-accounts transactions))
	 (propagated-accounts (propagate-balances direct-accounts))
	 (account-index (make-hash-table)))
    ;; Build account index for quick lookups
    (for-each
     (lambda (acc)
       (hash-set! account-index (account-name acc) acc))
     propagated-accounts)
    (make-journal transactions propagated-accounts account-index)))

(define (extract-account-names transactions)
  "Extract all unique account names that appear in transactions.

Arguments:
  transactions: A list of transaction records.

Returns: 
  A list of unique account name strings."
  (delete-duplicates
   (append-map
    (lambda (txn)
      (map posting-account (transaction-postings txn)))
    transactions)
   string=?))

(define (get-all-account-paths account-names)
  "Generate all acocunt paths including parents for the given account names.

Arguments:
  account-names: A list of account name strings.

Returns:
  A list of all account name strings including parent names."
  (delete-duplicates
   (append account-names
	   (append-map
	    (lambda (name)
		    (generate-parent-paths name))
	    account-names))
   string=?))

(define (generate-parent-paths account-name)
  "Generate all parent paths for an account.
For example, 'Assets:Bank:Checking' generates:
'(\"Assets\" \"Assets:Bank\" \"Assets:Bank:Checking\")

Arguments:
  account-name: A string representing the full account path.

Returns:
  A list of all parent paths as strings"
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
including parent accounts with zero balances.

Arguments: 
  transactions: A list of transaction records.

Returns:
  A list of account records with directly calculated balances."
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
    ;; Return the acocunt list
    (hash-map->list (lambda (k v) v) account-map)))

(define (propagate-balances accounts)
  "Correctly propagate balances from leaf accounts to parent accounts.
This ensures parent accounts have the sum of all their children's balances.

Arguments:
  accounts: A list of account records with direct balances.

Returns:
  A list of account records with propagated balances."
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
  "Build a complete account hierarchy from transactions.
This creates the account tree structure with propageted balances.

Arguments:
  transactions: A list of transaction records.

Returns: 
  A list of account records with properly calculated balances."
  (let* ((direct-accouts (calculate-direct-accounts transactions))
	 (accounts-with-propagated-balances (propagate-balances direct-accouts)))
    accounts-with-propagated-balances))

(define (get-account-transactions account-name transactions)
  "Get all transactions affecting a specific account. 

Arguments:
  account-name: A string representing the account name.
  transactions: A list of transaction records.

Returns:
  A list of transactions that affect the specified account"
  (filter
   (lambda (txn)
     (any (lambda (posting)
	    (string=? (posting-account posting) account-name))
	  (transaction-postings txn)))
   transactions))

(define (get-account journal account-name)
  "Get account by name from the journal. 

Arguments: 
  journal: A journal record.
  account-name: A string representing the account name

Returns:
  An account record, or #f if not found."
  (hash-ref (journal-account-index journal) account-name #f))

(define (get-account-balance journal account-name)
  "Get the balance for an account by name.

Arguments:
  journal: A journal record. 
  account-name: A string representing the account name

Returns: 
  The account balance as a number, or #f if account not found."
  (let ((account (get-account journal account-name)))
    (if account
	(account-balance account)
	#f)))

(define (get-transaction-accounts transaction accounts)
  "Get all accounts affected by a transaction.

Arguments:
  transaction: A transaction record 
  accounts: A list of account records.

Returns:
  A list of account records affected by the transaction."
  (let ((affected-names (map posting-account (transaction-postings transaction))))
    (filter (lambda (acc)
	      (member (account-name acc) affected-names))
	    accounts)))

(define* (filter-journal-transactions journal
                                     #:key account-pattern description-pattern
                                     from-date to-date min-amount max-amount)
  "Filter transactions by various criteria.
   
   Arguments:
     journal: A journal record
     account-pattern: Optional regex pattern to match account names
     description-pattern: Optional regex pattern to match transaction descriptions
     from-date: Optional start date for transactions
     to-date: Optional end date for transactions
     min-amount: Optional minimum transaction amount
     max-amount: Optional maximum transaction amount
   
   Returns:
     A list of transaction records matching all specified criteria"
  (let ((txns (journal-transactions journal)))
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

(define* (filter-journal-accounts journal #:key name-pattern min-balance max-balance)
  "Filter accounts by various criteria.
   
   Arguments:
     journal: A journal record
     name-pattern: Optional regex pattern to match account names
     min-balance: Optional minimum account balance
     max-balance: Optional maximum account balance
   
   Returns:
     A list of account records matching all specified criteria"
  (let ((accounts (journal-accounts journal)))
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


