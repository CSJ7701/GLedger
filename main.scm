
(add-to-load-path "modules")

(use-modules (backend hledger)
	     (backend account)
	     (ice-9 format))

(define ledger-file "test.ledger")

(define (display-accounts accounts)
  (for-each (lambda (acc)
	      (format #t "~a\n" (account->string acc)))
	    accounts))

(define (main args)
  (let* ((file (if (> (length args) 1)
		   (list-ref args 1)
		   "test.ledger"))
	 (accounts (get-accounts file))
	 (balances (account-balances file)))

    (format #t "Found ~a account in ~a\n\n" (length accounts) file)

    (format #t "Account balances:\n")
    (display-accounts (sort-accounts-by-name balances))

    ;; Filtering
    (format #t "\nExpense Accounts:\n")
    (display-accounts
     (filter-accounts balances "^expenses"))))

(main (command-line))
