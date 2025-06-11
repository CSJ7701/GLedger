
(use-modules (g-golf))

(gi-import "Gtk")

(define (activate app)
  (let ((window (make <gtk-application-window>
		  #:title "Hello"
		  #:application app))
	(button (make <gtk-button>
		  #:label "Hello, World!")))
    (connect button
	     'clicked
	     (lambda (b)
	       (quit app)))
    (set-child window button)
    (show window)))

(let ((app (make <gtk-application>
	     #:application-id "org.example.GtkApplication")))
  (connect app 'activate activate)
  (let ((status (run app 0 '())))
    (format #t "Application exited with status: ~a~n" status)))
	     
