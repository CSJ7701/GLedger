
(add-to-load-path "modules")

(use-modules (backend hledger)
	     (backend account)
	     (backend transaction)
	     (backend journal)
	     (utils)
	     (ice-9 format)
	     (srfi srfi-19))

(define journal-file "test.ledger")

(define (display-accounts accounts)
  (for-each (lambda (acc)
	      (format #t "~a\n" (account->string acc)))
	    accounts))

(define (display-transactions transactions)
  (for-each (lambda (txn)
	      (format #t "~a\n" (transaction->string txn)))
	    transactions))

(define (main args)
  (let* ((file (if (> (length args) 1)
		   (list-ref args 1)
		   "test.ledger"))
	 (journal (get-journal file)))

    (display (get-transaction-accounts (car (get-account-transactions "assets:bank" (journal-transactions journal))) (journal-accounts journal)))
    ))

(main (command-line))
