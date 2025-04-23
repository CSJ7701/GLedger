;; modules/backend/ledger.scm
(define-module (backend ledger)
  #:export (ledger-balances))

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-13))

(define (ledger-balances file)
  (let* ((cmd (string-append "ledger -f " file " bal --csv"))
         (port (open-input-pipe cmd)))
    (let loop ((lines '()))
      (let ((line (read-line port 'concat)))
        (if (eof-object? line)
            (reverse lines)
            (loop (cons line lines)))))))
