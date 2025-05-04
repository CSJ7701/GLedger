(define-module (utils)
  #:export (strip-newline
	    strip-quotes

	    file-exists?
	    ensure-directory

	    date<=?
	    date>=?
	    date>?
	    date<?))

(use-modules (ice-9 regex)
	     (ice-9 ftw)
	     (srfi srfi-1)
	     (srfi srfi-19))

;; (define (strip-newline s)
;;   (if (and (not (string-null? s))
;; 	   (char=? (string-ref s (- (string-length s) 1)) #\newline))
;;       (substring s 0 (- (string-length s) 1))
;;       s))

(define (strip-newline str)
  (regexp-substitute/global #f "\n" str 'pre "" 'post))

(define (strip-quotes str)
  (let* ((no-double (regexp-substitute/global #f "\"" str 'pre "" 'post))
	 (no-single (regexp-substitute/global #f "'" no-double 'pre "" 'post)))
    no-single))

(define (file-exists? path)
  (access? path F_OK))

(define (ensure-directory dir)
  (if (file-exists? dir)
      #t
      (mkdir dir)))

;; Helper functions for date comparison
(define (date<=? date1 date2)
  (time<=? (date->time-utc date1) (date->time-utc date2)))
(define (date>=? date1 date2)
  (time>=? (date->time-utc date1) (date->time-utc date2)))
(define (date>? date1 date2)
  (time>? (date->time-utc date1) (date->time-utc date2)))
(define (date<? date1 date2)
  (time<? (date->time-utc date1) (date->time-utc date2)))

