(import (rnrs) (clos user)
	(srfi :19)
	(srfi :26)
	(srfi :64)
	(text sxml ssax) (text sxml xsd))

(define-xml-type <request> ()
  ((name   :element-type :string :init-keyword :n)
   (expiry :element-type :date   :element-name Expiry :init-keyword :expiry
	   :init-form (current-date)))
  :attributes ((request-type :element-type :string
			     :element-name RequestType :init-keyword :rt))
  :element-name Request
  :namespace "http://sagittarius-scheme.com/types")

(define-xml-type <get-info> ()
  ((request  :element-type (<request> 0 unbounded)
	     :element-name GRequest :init-keyword :req))
  :element-name GetInfo
  :namespace "http://sagittarius-scheme.com/services")

(test-begin "XSD")

(let ((req (make <request> :n "Takashi")))
  (test-equal "unmarshall (1)"
	      `(*TOP*
		(http://sagittarius-scheme.com/types:Request 
		 (@)
		 (http://sagittarius-scheme.com/types:name "Takashi")
		 (http://sagittarius-scheme.com/types:Expiry
		  ,(date->string (current-date) "~Y-~m-~d"))))
	      (unmarshall-sxml req))
  (test-equal "unmarshall (2)"
	      `(*TOP*
		(http://sagittarius-scheme.com/services:GetInfo
		 (@)
		 (http://sagittarius-scheme.com/services:GRequest 
		  (@)
		  (http://sagittarius-scheme.com/types:name "Takashi")
		  (http://sagittarius-scheme.com/types:Expiry
		   ,(date->string (current-date) "~Y-~m-~d")))))
	      (unmarshall-sxml (make <get-info> :req (list req)))))

(let ((sreq (call-with-input-file "files/test.xml"
	      (cut ssax:xml->sxml <> '()))))
  (marshall-sxml <request> sreq)
  (test-assert "marshall (1)" (is-a? (marshall-sxml <request> sreq) <request>))
  (let ((sxml `(*TOP*
		(http://sagittarius-scheme.com/services:GetInfo
		 (@)
		 (http://sagittarius-scheme.com/services:GRequest 
		  (@)
		  (http://sagittarius-scheme.com/types:name "Takashi")
		  (http://sagittarius-scheme.com/types:Expiry
		   ,(date->string (current-date) "~Y-~m-~d")))))))
    (test-equal "marshall (2)"
		sxml
		(unmarshall-sxml (marshall-sxml <get-info> sxml)))))

;; min/max occur test
(define-xml-type <get-info1> ()
  ((request  :element-type <request> ;; default min 1 max 1
	     :element-name GRequest :init-keyword :req))
  :element-name GetInfo
  :namespace "http://sagittarius-scheme.com/services")

(define-xml-type <get-info2> ()
  ((request  :element-type (<request> 2 2)
	     :element-name GRequest :init-keyword :req))
  :element-name GetInfo
  :namespace "http://sagittarius-scheme.com/services")

(let* ((req (make <request> :n "Takashi"))
       (info10 (make <get-info1> :req #f)) ;; no request
       (info20 (make <get-info2> :req '()))
       (info22 (make <get-info2> :req (list req req)))
       (info23 (make <get-info2> :req (list req req req))))
  (test-error "info 1 0" condition? (unmarshall-sxml info10))
  (test-error "info 2 0" condition? (unmarshall-sxml info20))
  (test-assert "info 2 2" (unmarshall-sxml info22))
  (test-error "info 2 3" condition? (unmarshall-sxml info23)))

(let* ((req `(http://sagittarius-scheme.com/services:GRequest 
	      (@)
	      (http://sagittarius-scheme.com/types:name "Takashi")
	      (http://sagittarius-scheme.com/types:Expiry
	       ,(date->string (current-date) "~Y-~m-~d"))))
       (info2 `(*TOP*
		(http://sagittarius-scheme.com/services:GetInfo
		 (@)
		 ,req ,req)))
       (info3 `(*TOP*
		(http://sagittarius-scheme.com/services:GetInfo
		 (@)
		 ,req ,req ,req))))
  (test-error "marshall info2" condition? (marshall-sxml <get-info1> info2))
  (test-error "marshall info3" condition? (marshall-sxml <get-info2> info3))
  )

;; misc test
(define-class <dummy-request> () (name expiry))
(define-class <dummy-info> () (request))
(let* ((dr (make <dummy-request>))
       (di (make <dummy-info>))
       (i  (make <get-info> :req dr)))
  (test-error "dummy-request" condition? (unmarshall-sxml dr))
  (test-error "dummy-info"    condition? (unmarshall-sxml di))
  (test-error "invalid info"  condition? (unmarshall-sxml i)))

(test-end)