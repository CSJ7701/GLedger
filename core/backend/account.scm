(define-module (backend account)
  #:export (;; Account record functions
	    make-account
	    account?
	    account-name
	    account-balance

	    update-balance ;; Returns a new account with updated balance 
	    account->string
	    account-path-components ;; Takes an account object, returns a list of path components (Assets:Bank -> (Assets Bank)
	    sort-accounts-by-name ;; Takes a list of accounts, returns a sorted list
	    filter-accounts ;; Takes list of accounts and regex pattern. Returns list of accounts whose names match pattern 
	    account-depth ;; Returns the depth of an account object (Assets:Bank -> 2)
	    account-root-name ;; Returns the root/top-level path component of an account object
	    account-leaf-name ;; Returns the leaf/bottom-level path component of an account object 
	    account-parent-name ;; Returns the direct parent path component of an account object 
	    account-parent ;; Returns the direct parent object of an account object 
	    account-child-names ;; Takes account object and list of accounts. Returns the names of all direct children of account 
	    account-children ;; Takes account object and list of accounts. Returns all direct child objects 
	    account-descendant-names ;; Takes account object and list of accounts. Returns list of names of all descendants (children, grandchildren, etc)
	    account-descendants ;; Takes account object and list of accounts. Returns all descendant objects. 
	    account-ancestor? ;; Takes 2 account objects, checks if acc1 is an ancestor of acc2.
	    account-siblings? ;; Takes 2 account objects, checks if they are siblings.
	    account-siblings ;; Takes an account objects and list of accounts. Returns list of all siblings objects
	    account-common-ancestor ;; Takes 2 account objects. Returns their common ancestor, or #f.
	    find-account-by-path ;; Takes list of accounts and list of path components (strings). Returns matching account or #f.
	    accounts-at-depth ;; Takes list of accounts and depth (int). Returns list of all accounts at depth.
	    top-level-accounts ;; Takes list of accounts. Returns list of top-level accounts.
	    group-by-parent ;; Takes list of accounts. Returns alist associating parent name with a list of child accounts.
	    ))

(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (ice-9 regex))

(define-record-type <account>
  (make-account name balance)
  account?
  (name account-name)          ;; String: account name including full path 
  (balance account-balance))   ;; Number: account balance

(define (update-balance acc new-balance)
  "Create a new account with the same name as ACC but with NEW-BALANCE"
  (make-account (account-name acc) new-balance))

(define (account->string acc)
  "Return a string representation of account ACC"
  (format #f "~a: ~a" (account-name acc) (account-balance acc)))

(define (account-path-components acc)
  "Split accout ACC's name into path components.
Example: An account name \"Assets:Checking\" returns (\"Assets\" \"Checking\")."
  (string-split (account-name acc) #\:))

(define (sort-accounts-by-name accounts)
  "Return a new list of ACCOUNTS sorted alphabetically by name."
  (sort accounts (lambda (a b)
		   (string<? (account-name a) (account-name b)))))

(define (filter-accounts accounts pattern)
  "Return accounts from ACCOUNTS whose names match the regular expression PATTERN"
  (let ((rx (make-regexp pattern)))
    (filter (lambda (acc)
	      (regexp-exec rx (account-name acc)))
	    accounts)))

(define (account-depth acc)
  "Return the depth of account ACC in the hierarchy.
Example: \"Assets:Bank:Checking\" has a depth of 3."
  (length (account-path-components acc)))

(define (account-root-name acc)
  "Return the root (top-level) component of account ACC.
Example: for \"Assets:Bank:Checking\", returns \"Assets\"."
  (let ((components (account-path-components acc)))
    (if (null? components)
	""
	(car components))))

(define (account-leaf-name acc)
  "Return the leaf (last) component of account ACC's name.
Example: for \"Assets:Bank:Checking\", returns \"Checking\"."
  (let ((components (account-path-components acc)))
    (if (null? components)
	(account-name acc)
	(list-ref components (- (length components) 1)))))

(define (account-parent-name acc)
  "Return the parent account name of ACC, or #f if it's a top-level account. 
Example: for \"Assets:Bank:Checking\", returns \"Assets:Bank\"."
  (let* ((components (account-path-components acc)))
    (if (<= (length components) 1)
	#f			    ; No parent for top-level accounts
	(string-join (drop-right components 1) ":"))))

(define (account-parent acc accounts)
  "Find and return the parent account of ACC from the list of ACCOUNTS.
Returns #f if ACC is a top-level account or the parent isn't found."
  (let ((parent-name (account-parent-name acc)))
    (and parent-name
	 (find (lambda (a)
		 (string=? (account-name a) parent-name))
	       accounts))))

(define (account-child-names acc accounts)
  "Return a list of names of immediate child accounts of ACC from ACCOUNTS."
  (let ((acc-name (account-name acc))
	(acc-depth (account-depth acc)))
    (filter-map
     (lambda (a)
       (let* ((name (account-name a))
	      (components (string-split name #\:))
	      (depth (length components)))
	 ;; A child must be exactly one level deeper and start with the parent's name
	 (and (= depth (+ acc-depth 1))
	      (string-prefix? (string-append acc-name ":") name)
	      name)))
     accounts)))

(define (account-children acc accounts)
  "Return a list of immediate child account objects of ACC from ACCOUNTS."
  (let ((child-names (account-child-names acc accounts)))
    (filter (lambda (a)
	      (member (account-name a) child-names))
	    accounts)))

(define (account-descendant-names acc accounts)
  "Return a list of names of all descendant accounts of ACC from ACCOUNTS.
This includes children, grandchildren, etc."
  (let ((acc-name (account-name acc)))
    (filter-map
     (lambda (a)
       (let ((name (account-name a)))
	 ;; A descendant must start with parent name, followed by ":"
	 (and (not (string=? name acc-name))
	      (string-prefix? (string-append acc-name ":") name)
	      name)))
     accounts)))

(define (account-descendants acc accounts)
  "Return a list of all descendant account objects of ACC from ACCOUNTS."
  (let ((descendant-names (account-descendant-names acc accounts)))
    (filter (lambda (a)
	      (member (account-name a) descendant-names))
	    accounts)))

(define (account-ancestor? potential-ancestor potential-descendant)
  "Return #t if POTENTIAL-ANCESTOR is an ancestor of POTENTIAL-DESCENDANT, #f otherwise"
  (let ((ancestor-name (account-name potential-ancestor))
	(descendant-name (account-name potential-descendant)))
    (and (not (string=? ancestor-name descendant-name))
	 (string-prefix? (string-append ancestor-name ":") descendant-name))))

(define (account-siblings? acc1 acc2)
  "Return #t if ACC1 and ACC2 are siblings (share the same parent), #f otherwise."
  (let ((parent1 (account-parent-name acc1))
	(parent2 (account-parent-name acc2)))
    (and parent1 parent1 (string=? parent1 parent2))))

(define (account-siblings acc accounts)
  "Return a list of all sibling account objects of ACC from ACCOUNTS."
  (let ((parent-name (account-parent-name acc))
	(acc-name (account-name acc)))
    (if (not parent-name)
	;; Top level accounts are siblings of other top level accounts
	(filter (lambda (a)
		  (and (not (string=? (account-name a) acc-name))
		       (not (account-parent-name a))))
		accounts)
	;; Otherwise, siblings share the same parent
	(filter (lambda (a)
		  (and (not (string=? (account-name a) acc-name))
		       (let ((a-parent (account-parent-name a)))
			 (and a-parent (string=? a-parent parent-name)))))
		accounts))))

(define (account-common-ancestor acc1 acc2)
  "Return the name of the common ancestor of ACC1 and ACC2, or #f if none exists."
  (let* ((path1 (account-path-components acc1))
	 (path2 (account-path-components acc2))
	 (common '()))
    (let loop ((p1 path1) (p2 path2))
      (if (or (null? p1) (null? p2) (not (string=? (car p1) (car p2))))
	  (if (null? common)
	      #f			; No common ancestor
	      (string-join common ":"))
	  (begin
	    (set! common (append common (list (car p1))))
	    (loop (cdr p1) (cdr p2)))))))

(define (find-account-by-path accounts path-components)
  "Find and return the account object from ACCOUNTS that matches PATH-COMPONENTS.
PATH-COMPONENTS should be a list of strings that make up an account path."
  (let ((full-path (string-join path-components ":")))
    (find (lambda (acc)
	    (string=? (account-name acc) full-path))
	  accounts)))

(define (accounts-at-depth accounts depth)
  "Return all accounts from ACCOUNTS that are at the specified DEPTH in the hierarchy."
  (filter (lambda (acc)
	    (= (account-depth acc) depth))
	  accounts))

(define (top-level-accounts accounts)
  "Return all top-level accounts from ACCOUNTS (those with a depth of 1)."
  (accounts-at-depth accounts 1))

(define (group-by-parent accounts)
  "Group ACCOUNTS by their immediate parent, returning an association list.
Each entry in the result has the form (parent-name . child-accounts)."
  (let ((result '()))
    (for-each
     (lambda (acc)
       (let* ((parent (account-parent-name acc))
	      (key (or parent ""))
	      (existing (assoc key result)))
	 (if existing
	     (set-cdr! existing (cons acc (cdr existing)))
	     (set! result (acons key (list acc) result)))))
     accounts)
    result))

