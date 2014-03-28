;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sxml/xsd/types.scm - XSD marshall/unmarshall library.
;;;  
;;;   Copyright (c) 2013  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

#!read-macro=sagittarius/regex
(library (text sxml xsd types)
    (export define-xml-type
	    marshall-xml marshall-sxml
	    unmarshall-xml unmarshall-sxml

	    ;; for user customisation
	    xml-value->primitive
	    xml-attribute->primitive

	    primitive->xml-value
	    primitive->xml-attribute
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius object)
	    (sagittarius time) ;; for <date>
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (clos user)
	    (clos core)
	    (text sxml serializer)
	    (text sxml tools)
	    (text sxml sxpath)
	    (text sxml ssax))

  ;; marker class :)
  (define-class <xml-element> () ())

  (define-syntax define-xml-type
    (lambda (x)
      ;; we are adding meta information to slot definition
      ;; so that marshall and unmarshall can see what needs
      ;; to be there and done.
      (define (parse-slots k slots attr?)
	(define (remove-name&type slot name type)
	  (filter-map (lambda (m) 
			(and (not (or (eq? m name) (eq? m type)
				      (eq? m :element-name)
				      (eq? m :element-type)))
			     m))
		      slot))
	(define (resolve-type slot type)
	  ;; type will be expanded to :element-type name :min n :max m
	  ;; default value for both min and max are 1 (so required)
	  ;; nillable needs to be explicitly specified
	  ;; NOTE: users can put :min and :max by themselves.
	  ;; and it is an error if the type is list and :min or :max is
	  ;; represented
	  (define (check min max)
	    (or (and (integer? min) (>= min 0)
		     (if (eq? max 'unbounded) #t (<= min max)))
		(syntax-violation 'define-xml-type "invalid range of min-max"
				  `(:min ,min :max ,max)
				  (syntax->datum x))))
	  ;; FIXME this is ugly...
	  (let ((min (syntax->datum (get-keyword :min slot 1)))
		(max (syntax->datum (get-keyword :max slot 1))))
	    (syntax-case type (unbounded)
	      ((t) (check min max) (list #'t :min min :max max))
	      ((t min)
	       (check (syntax->datum #'min) max)
	       (list #'t :min #'min :max max))
	      ((t min unbounded)
	       (check (syntax->datum #'min) 'unbounded)
	       (list #'t :min #'min :max ''unbounded))
	      ((t min max)
	       (check (syntax->datum #'min) (syntax->datum #'max))
	       (list #'t :min #'min :max #'max))
	      (t (and (or (identifier? #'t) (keyword? #'t)) (check min max))
		 (list #'t :min min :max max))
	      (_ (syntax-violation 'define-xml-type
				   "invalid :element-type specifier"
				   (syntax->datum type)
				   (syntax->datum x))))))
	(define (resolve-slot slot)
	  (let ((name (get-keyword :element-name (cdr slot) #f))
		(type (get-keyword :element-type (cdr slot) #f)))
	    (unless type 
	      (syntax-violation 'define-xml-type
				":element-type keyword is mandatory"
				(syntax->datum slot)
				(syntax->datum x)))
	    (let ((slot (remove-name&type slot name type))
		  (type (resolve-type (cdr slot) type)))
	      (values slot name type))))
	(let loop ((slots slots) (r '()))
	  (if (null? slots)
	      (reverse! r)
	      (let*-values (((slot name type) (resolve-slot (car slots))))
		(loop (cdr slots) 
		      (cons #`(#,@slot :element-name '#,(if name name (car slot))
				       :element-type (lambda () #,(car type))
				       #,@(cdr type)
				       :attribute #,attr?)
			    r))))))
      (define (strip<> s)
	(cond ((#/<(.+?)>/ s) => (lambda (m) (m 1))) (else s)))
      (define (gen-meta k name)
	(let ((s (symbol->string (syntax->datum name))))
	  (datum->syntax k (string->symbol (format "<~a-meta>" (strip<> s))))))
      (define (default-name name)
	(let ((s (symbol->string (syntax->datum name))))
	  (string->symbol (strip<> s))))
      (syntax-case x ()
	((k name (parents ...) slots . options)
	 (with-syntax (((slot-defs ...) (parse-slots #'k #'slots #f))
		       ((attr-defs ...)
			(parse-slots #'k 
				     (get-keyword :attributes #'options '())
				     #t))
		       (elm (datum->syntax #'k 
					   (get-keyword :element-name #'options
							(default-name #'name))))
		       (ns (get-keyword :namespace #'options #f))
		       (meta-class (gen-meta #'k #'name)))
	   (when (and #'ns (not (string? (syntax->datum #'ns))))
	     (syntax-violation 'define-xml-type
			       ":namespace must be string" #'ns))
	   #'(begin 
	       (define-class meta-class (<class> <xml-element>)
		 ;; the class is represents the tag so 
		 ;; hold the element name in metaclass.
		 ((element :init-value 'elm)
		  (namespace :init-value ns)))
	       ;; put <xml-element> as the last CPL so that
	       ;; we can check if the class is marshall/unmarshall-able
	       (define-class name (parents ...)
		 ;; for my convenience attribute first then other elements
		 (attr-defs ... slot-defs ...)
		 :metaclass meta-class)))))))

  ;; TODO add more primitives...
  (define-method primitive->xml-value ((type (eql :string)) value)
    (->string value))

  (define-method primitive->xml-value ((type (eql :date)) (date <date>))
    (date->string date "~Y-~m-~d"))
  ;; trust you :)
  (define-method primitive->xml-value ((type (eql :date)) (date <string>)) date)
  (define-method primitive->xml-value ((type (eql :dateTime)) (date <string>))
    date)

  (define-method primitive->xml-attribute ((type <keyword>) v)
    (primitive->xml-value key v))

  (define (convert-name localname class)
    (let ((namespace (~ class 'namespace)))
      (if namespace
	  (string->symbol (format "~a:~a" namespace localname))
	  localname)))

  ;; convert to SXML
  (define (unmarshall-element name element)
    (define (resolve-name s)
      (let ((slot-name (slot-definition-name s)))
	(values slot-name (slot-definition-option s :element-name slot-name))))
    (define (resolve-type s)
      (values ((slot-definition-option s :element-type))
	      (slot-definition-option s :min)
	      (slot-definition-option s :max)))
    (let ((class (class-of element)))
      (unless (is-a? class <xml-element>)
	(error 'unmarshall "given element is not unmarshallable" element))

      (let* ((slots (class-slots class))
	     (attr  (filter-map
		     (lambda (s)
		       (and (slot-definition-option s :attribute #f) s))
		     slots))
	     (elems (filter-map 
		     (lambda (s)
		       (and (not (slot-definition-option s :attribute #f)) s))
		     slots)))
	`(,name
	  ;; attributes
	  (@ ,@(filter-map (lambda (s)
			     (let-values (((slot-name name) (resolve-name s)))
			       (if (slot-bound? element slot-name)
				   `(,name ,(primitive->xml-attribute 
					     (~ element slot-name)))
				   #f))) attr)
	     ;; in case of super class is there then we need to specify the type
	     ,@(let ((supers (filter-map (lambda (c) 
					   (and (is-a? c <xml-element>) c))
					 (class-direct-supers class))))
		 (if (null? supers)
		     '()
		     ;; use full name (i'm sick and tired of this...)
		     (let ((name (convert-name (~ class 'element) class)))
		       `((http://www.w3.org/2001/XMLSchema-instance:type 
			  ,(symbol->string name))))))
	     )
	  ;; elements
	  ,@(fold-right 
	     (lambda (s knil)
	       (define (check-element-count elems min max)
		 (let ((n (length elems)))
		   ;; if length returns negative number then this is not a list
		   (when (negative? n)
		     (error 'unmarshall-element
			    "the valus must be a list for maxOccur > 1" elems))
		   (when (< n min)
		     (error 'unmarshall-element 
			    "the element is less than minOccur" 
			    `(min ,min) elems))
		   (when (and (not (eq? max 'unbounded)) (> n max))
		     (error 'unmarshall-element 
			    "the element is more than maxOccur" 
			    `(man ,man) elems))
		   #t))
	       (let*-values (((slot-name name) (resolve-name s))
			     ((type min max) (resolve-type s)))
		 (if (slot-bound? element slot-name)
		     (let ((value (~ element slot-name)))
		       ;; if max > 1 or unbounded then value must be a list
		       (if (or (eq? max 'unbounded) (> max 1))
			   (and (check-element-count value min max)
				`(,@(map (lambda (v) 
					   (unmarshall-element 
					    (convert-name name class) v))
					 value) ,@knil))
			   (cond ((keyword? type)
				  ;; primitive so resolve it now
				  (cons
				   `(,(convert-name name class) 
				     ,(primitive->xml-value type value))
				   knil))
				 ((is-a? (class-of value) <xml-element>)
				  (cons (unmarshall-element
					 (convert-name name class)
					 value)
					knil))
				 (else
				  (error 'unmarshall-element
					 "unknown element" value)))))
		     (or (and (zero? min) '())
			 (error 'unmarshall-element
				"element must be presented!" name)))))
	     '() elems)))))

  (define (unmarshall-sxml element)
    (let ((class (class-of element)))
      (unless (is-a? class <xml-element>)
	(error 'unmarshall "given element is not unmarshallable" element))
      ;; make sure the top most level has *TOP* for proper SXML.
      (list
       '*TOP*
       (unmarshall-element (convert-name (~ class 'element) class) element))))
  ;; keyword arguments are more for debugging or so...
  (define (unmarshall-xml element :key 
			  (indent #f)
			  (ns-prefix-assig '()))
    (define (collect-all-namespace xml)
      (let ((m (regex-matcher #/xmlns:(\w+)="([^\"]+)"/ xml)))
	(let loop ((r '()))
	  (if (regex-find m)
	      (loop (acons (m 2) (m 1) r))
	      (reverse! r)))))
    ;; this is safer so that it doesn't have any unwanted spaces
    (let* ((xml (srl:sxml->string (unmarshall-sxml element) '() indent 'xml
				  ns-prefix-assig #t 'omit "1.0"))
	   (namespaces (collect-all-namespace xml)))
      ;; fixup some namespace in its attributes...
      (let loop ((namespaces namespaces) (xml xml))
	(if (null? namespaces)
	    xml
	    (loop (cdr namespaces)
		  (let ((ns (car namespaces)))
		    ;; for now very naive one check only type
		    (regex-replace-all 
		     (regex (string-append "type=\"" (car ns) "[^\"]")) xml 
		     (lambda (m) (string-append "type=\"" (cdr ns) ":")))))))
      ))

  (define-method xml-value->primitive ((type (eql :string)) v)
    (car v))
  (define-method xml-value->primitive ((type (eql :date)) v)
    (string->date (car v) "~Y-~m-~d"))
  (define-method xml-value->primitive ((type (eql :dateTime)) v)
    ;; use ~6 extension for Sagittarius. Fuck!!!
    (string->date (car v) "~6"))

  ;; well...
  (define-method xml-attribute->primitive ((type <keyword>) v)
    (xml-value->primitive type (list v)))

  (define-constant +instance:type+ 
    'http://www.w3.org/2001/XMLSchema-instance:type)

  ;; need to take class to know which object we need to construct
  (define (marshall-sxml class sxml)
    (define (check-namespace element ncname namespace)
      (let ((ns (sxml:name->ns-id (sxml:name element))))
	(or (and (string=? ns namespace)
		 (string=? (symbol->string ncname) (sxml:ncname element)))
	    (error 'marshall-sxml 
		   "element does not belong to the proper namespace" 
		   ns ncname namespace
		   element
		   sxml))))

    (define (marshall-rec o class root-element)
      (let ((content (sxml:content root-element))
	    (namespace (~ class 'namespace)))
	(define (get-name s)
	  (slot-definition-option s :element-name))
	(define (get-type s)
	  ((slot-definition-option s :element-type)))
	;; set slots
	;; attributes
	(for-each
	 (lambda (s)
	   (define (get-use s) (slot-definition-option s :use #f))
	   (let* ((slot-name (slot-definition-name s))
		  (type (get-type s))
		  (use  (get-use s))
		  (attr (sxml:attr root-element (get-name s))))
	     (when (and (eq? use :required) (not attr))
	       (error 'marshall-sxml
		      "mandatory attribute is missing" 
		      (get-name s) root-element))
	     (when attr
	       (let ((v (if (keyword? type)
			    (xml-attribute->primitive type attr)
			    ;; must be a class
			    (let ((o (make type))) 
			      (marshall-rec o type attr)))))
		 (set! (~ o slot-name) v)))))
	 (filter-map (lambda (s)
		       (and (slot-definition-option s :attribute #f)
			    s)) (class-slots class)))
	;; elements
	(for-each
	 (lambda (s)
	   (define (get-max s) (slot-definition-option s :max))
	   (define (get-min s) (slot-definition-option s :min))
	   (define (get-real-type class e)
	     (or (and-let* ((xml-type (sxml:attr e +instance:type+))
			    (subclasses (class-direct-subclasses class)))
		   (find (lambda (c)
			   (let ((ns (slot-ref c 'namespace))
				 (e  (slot-ref c 'element)))
			     (string=? xml-type (format "~a:~a" ns e))))
			 subclasses))
		 class))
	   (define (check-element-count elems min max)
	     (let ((n (length elems)))
	       (when (< n min)
		 (error 'marshall-sxml "too less elements"
			`((min ,min) (max ,max)) elems))
	       (when (and (not (eq? max 'unbounded)) (> n max))
		 (error 'marshall-sxml "too many elements"
			`((min ,min) (max ,max)) elems))))
	   (let* ((ncname (get-name s))
		  (slot-name (slot-definition-name s))
		  (full-name (string->symbol 
			      (format "~a:~a" (or namespace "")
				      ncname)))
		  (type (get-type s))
		  (max (get-max s))
		  (min (get-min s))
		  (e ((sxml:filter (lambda (e)
				     (eq? (sxml:name e) full-name)))
		      content)))
	     (let ((v (if (keyword? type)
			  (map (lambda (e)
				 (xml-value->primitive type (sxml:content e)))
			       e)
			  ;; must be a class
			  (map (lambda (e)
				 (unless (is-a? type <xml-element>)
				   (error 'marshall
					  "given class is not marshallable"
					  type))
				 (let* ((type (get-real-type type e))
					(o (make type)))
				   (marshall-rec o type e)))
			       e))))
	       ;; check min max
	       (check-element-count v min max)
	       (if (or (eq? max 'unbounded) (> max 1))
		   (set! (~ o slot-name) v)
		   (set! (~ o slot-name) (car v))))))
	 (filter-map (lambda (s)
		       (and (not (slot-definition-option s
							 :attribute
							 #f))
			    s)) (class-slots class)))
	o))

    (unless (is-a? class <xml-element>)
      (error 'marshall "given class is not marshallable" class))
    (let ((o (make class)))
      ;; assume *TOP* is there!
      (let ((root-element (car (sxml:content sxml)))
	    (element (~ class 'element))
	    (namespace (~ class 'namespace)))
	(check-namespace root-element element namespace)	
	(marshall-rec o class root-element))))

  (define (marshall-xml class xml)
    (marshall-sxml class (%ssax:xml->sxml (open-string-input-port xml) '())))

  ;; internal parser...
  (define (%ssax:xml->sxml port dummy)
    (define (symbol-append a b)
      (string->symbol (format "~a:~a" a b)))
    ;; To make things easier, we don't use user defined namespace-prefix-assig
    (define namespace-prefix-assig '())
    ;; the attribute which value needs to be qualified.
    ;; say 'name', 'id' and so on should not be but 'type'
    ;; TODO, not sure which one should be other than 'type'
    (define qualified '(http://www.w3.org/2001/XMLSchema-instance:type))
    (letrec
	((namespaces
	  (map (lambda (el)
		 (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
	       namespace-prefix-assig))

	 (RES-NAME->SXML
	  (lambda (res-name)
	    (string->symbol
	     (string-append
	      (symbol->string (car res-name))
	      ":"
	      (symbol->string (cdr res-name))))))
	 (convert-namespace (lambda (namespaces value)
			      (define (gen-value v rns)
				(string-append (symbol->string rns)
					       ":"
					       v))
			      (or (receive (ns v) (string-scan value #\: 'both)
				    ;; we don't do that match such as 
				    ;; {namespace}:string stuff.
				    (and-let* (( ns )
					       (ns (string->symbol ns))
					       (slot (assq ns namespaces))
					       (rns (cadr slot)))
				      (gen-value v rns)))
				  ;; thus no ':' but there may be *DEFAULT*
				  ;; in namespaces
				  (cond ((assq '*DEFAULT* namespaces)
					 => (lambda (default)
					      (and-let* ((rns (cadr default)))
						(gen-value value rns))))
					(else #f))
				  value))))
      (let ((result
	     (reverse
	      ((ssax:make-parser
		NEW-LEVEL-SEED 
		(lambda (elem-gi attributes namespaces
				 expected-content seed)
		  '())
		
		FINISH-ELEMENT
		(lambda (elem-gi attributes namespaces parent-seed seed)
		  (let ((seed (ssax:reverse-collect-str-drop-ws seed))
			(attrs
			 (attlist-fold
			  (lambda (attr accum)
			    ;; TODO it's a bit too much assumption
			    ;; but for now only handles instance:type
			    (let* ((raw-attr (if (pair? (car attr))
						 (symbol-append (car (car attr))
								(cdr (car attr)))
						 (car attr)))
				   (value (if (memq raw-attr qualified)
					      (convert-namespace namespaces
								 (cdr attr))
					      (cdr attr))))
			      (cons (list 
				     (if (symbol? raw-attr)
					 raw-attr
					 (RES-NAME->SXML raw-attr))
				     value) accum)))
			  '() attributes)))
		    (cons
		     (cons 
		      (if (symbol? elem-gi) elem-gi
			  (RES-NAME->SXML elem-gi))
		      (if (null? attrs) seed
			  (cons (cons '@ attrs) seed)))
		     parent-seed)))

		CHAR-DATA-HANDLER
		(lambda (string1 string2 seed)
		  (if (string-null? string2) (cons string1 seed)
		      (cons* string2 string1 seed)))

		DOCTYPE
		(lambda (port docname systemid internal-subset? seed)
		  (when internal-subset?
		    (ssax:warn port
			       "Internal DTD subset is not currently handled ")
		    (ssax:skip-internal-dtd port))
		  (ssax:warn port "DOCTYPE DECL " docname " "
			     systemid " found and skipped")
		  (values #f '() namespaces seed))

		UNDECL-ROOT
		(lambda (elem-gi seed)
		  (values #f '() namespaces seed))

		PI
		((*DEFAULT* . (lambda (port pi-tag seed)
				(cons
				 (list '*PI* pi-tag 
				       (ssax:read-pi-body-as-string port))
				 seed))))
		)
	       port '()))))
	(cons '*TOP*
	      (if (null? namespace-prefix-assig)
		  result
		  (cons
		   (list '@ (cons '*NAMESPACES* 
				  (map (lambda (ns) (list (car ns) (cdr ns)))
				       namespace-prefix-assig)))
		   result)))
	)))
)