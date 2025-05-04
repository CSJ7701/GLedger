;; modules/backend/hledger.scm
(define-module (backend hledger)
  #:export (get-accounts
	    account-balances

	    get-transactions

	    get-journal
	    
	    run-hledger-command
	    ))

(use-modules (utils)
	     (backend account)
	     (backend transaction)
	     (backend journal)
	     (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 textual-ports)
	     (ice-9 regex)
	     (srfi srfi-1)
	     (srfi srfi-19))

;; Run Hledger commands safely
(define (run-hledger-command cmd)
  (let* ((port #f)
	 (result '()))
    (catch #t
      (lambda ()
	(set! port (open-input-pipe (string-append "hledger " cmd)))
	(let loop ((lines '()))
	  (let ((line (read-line port 'concat)))
	    (if (eof-object? line)
		(set! result (reverse lines))
		(loop (cons line lines))))))
      (lambda (key . args)
	(format (current-error-port) "Error running hledger command ~a\n" args)))
    (when port
      (close-pipe port))
    result))

(define (create-empty-account account-name)
  (let ((name (string-trim (strip-newline account-name))))
    (make-account name 0)))

(define (parse-account-line line)
  (catch #t
    (lambda ()
      (let* ((fields (string-split line #\,)))
	(if (< (length fields) 2)
	    (begin
	      (format #t "Not enough fields in line: ~s\n" line)
	      (make-account "error" 0))
	    (let* ((raw-account-name (string-trim (list-ref fields 0)))
		   (raw-balance-str (string-trim (strip-newline (list-ref fields 1))))
		   (account-name (strip-quotes raw-account-name))
		   (balance-str (strip-quotes raw-balance-str))
		   (cleaned-balance (regexp-substitute/global
				     #f "[$,]" balance-str 'pre "" 'post))
		   (balance (string->number cleaned-balance)))
	      (make-account account-name (or balance 0))))))
    (lambda (key . args)
      (format (current-error-port) "Error parsing account line: ~a\n" line)
      (make-account "error" 0))))

(define* (get-transactions #:optional (file #f) (account #f) (date-range #f))
  (let* ((file-arg (if file (string-append " -f " file) ""))
	 (account-arg (if account (string-append " acc " account) ""))
	 (date-arg (if date-range (string-append " -p " date-range) ""))
	 (cmd (string-append file-arg account-arg date-arg " reg --output-format=csv"))
	 (lines (run-hledger-command cmd)))
    (if (null? lines)
	'() ; Return empty list if command fails
	(let* ((postings (parse-posting-lines (cdr lines)))
	       (transactions (group-postings-into-transactions postings)))
	  transactions))))

(define (parse-posting-lines lines)
  (map parse-posting-line lines))

(define (parse-posting-line line)
  (catch #t
    (lambda ()
      (let* ((fields (string-split line #\,))
	     (txnid (string->number (strip-quotes (list-ref fields 0))))
	     (date-str (strip-quotes (list-ref fields 1)))
	     (code (strip-quotes (list-ref fields 2)))
	     (description (strip-quotes (list-ref fields 3)))
	     (account (strip-quotes (list-ref fields 4)))
	     (amount-str (strip-quotes (list-ref fields 5)))
	     (total-str (strip-quotes (list-ref fields 6)))
	     (date (parse-date date-str))
	     (amount (parse-amount amount-str))
	     (total (parse-amount total-str)))
	(make-posting txnid date code description account amount total)))
    (lambda (key . args)
      (format (current-error-port) "Error parsing posting line: ~a\n" line)
      (make-posting 0 (current-date) "" "error" "error" 0 0))))

(define (group-postings-into-transactions postings)
  ;; Group postings by transaction index
  (let ((posting-groups '()))
    ;; First group by txnid
    (for-each
     (lambda (posting)
       (let* ((txnid (posting-txnid posting))
	      (existing (assoc txnid posting-groups)))
	 (if existing
	     (set-cdr! existing (cons posting (cdr existing)))
	     (set! posting-groups (acons txnid (list posting) posting-groups)))))
     postings)

    ;; Create transaction object from the groups of postings
    (map (lambda (group)
	   (let* ((txnid (car group))
		  (postings (cdr group))
		  ;; Sort by account and amount to group similar postings together
		  ;; Keep them distinct
		  (sorted-postings (sort postings
					 (lambda (a b)
					   (if (string=? (posting-account a) (posting-account b))
					       ;; If same account, sort by amount
					       (< (posting-amount a) (posting-amount b))
					       ;; Otherwise sort by account name 
					       (string<? (posting-account a)
							 (posting-account b))))))
		  (first-posting (car sorted-postings))
		  (date (posting-date first-posting))
		  (description (posting-description first-posting)))
	     (make-transaction txnid date description sorted-postings)))
	 posting-groups)))

(define (parse-date date-str)
  ;; Parse a date like "2023-04-15" into an SRFI-19 date object
  (let* ((parts (string-split date-str #\-))
	 (year (string->number (list-ref parts 0)))
	 (month (string->number (list-ref parts 1)))
	 (day (string->number (list-ref parts 2))))
    (make-date 0 0 0 0 day month year 0)))

(define (parse-amount amount-str)
  ;; Parse an amount string like '$50.00' into a number
  (if (or (string-null? amount-str)
	  (string=? amount-str ""))
      0
      (let ((cleaned (regexp-substitute/global
		      #f "[$,]" amount-str 'pre "" 'post)))
	(string->number cleaned))))


(define* (get-journal #:optional (file #f))
  "Get a complete journal object from hledger data"
  (let* ((transactions (get-transactions file)))
    (build-journal-from-transactions transactions)))
