(define-module (backend account)
  #:export (make-account account? account-name account-balance update-balance
			 account->string account-path-components sort-accounts-by-name
			 filter-accounts))

(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (ice-9 regex))

(define-record-type <account>
  (make-account name balance)
  account?
  (name account-name)
  (balance account-balance))

(define (update-balance acc new-balance)
  (make-account (account-name acc) new-balance))

(define (account->string acc)
  (format #f "~a: ~a" (account-name acc) (account-balance acc)))

;;; Split account into path components -- ("Assets:Checking" -> ("Assets" "Checking))
(define (account-path-components acc)
  (string-split (account-name acc) #\:))

(define (sort-accounts-by-name accounts)
  (sort accounts (lambda (a b)
		   (string<? (account-name a) (account-name b)))))

(define (filter-accounts accounts pattern)
  (let ((rx (make-regexp pattern)))
    (filter (lambda (acc)
	      (regexp-exec rx (account-name acc)))
	    accounts)))
