;; modules/backend/hledger.scm
(define-module (backend hledger)
  #:export (get-accounts account-balances run-hledger-command))

(use-modules (utils)
	     (backend account)
	     (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 textual-ports)
	     (ice-9 regex)
             (srfi srfi-1))

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

(define* (get-accounts #:optional  (file #f))
  (let* ((file-arg (if file (string-append " -f " file) ""))
	 (cmd (string-append file-arg " accounts"))
	 (lines (run-hledger-command cmd)))
    (if (null? lines)
	'() ; Return empty list if command failed
	(map create-empty-account (cdr lines)))))

(define* (account-balances #:optional (file #f) (existing-accounts #f))
  (let* ((file-arg (if file (string-append " -f " file) ""))
	 (cmd (string-append file-arg " bal --output-format csv"))
	 (lines (run-hledger-command cmd)))
    (let* ((data-lines (if (null? lines) '() (cdr lines))) ; Skip header
	   (new-balances (map parse-account-line data-lines)))
      (if existing-accounts
	  (map (lambda (acc)
		 (let ((match (find (lambda (a)
				      (string=? (account-name acc) (account-name a)))
				    new-balances)))
		   (if match
		       (make-account (account-name acc) (account-balance match))
		       acc)))
	       existing-accounts)
	  ;; No arg, just return parsed balances
	  new-balances)))) 

