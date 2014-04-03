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
	    marshal-xml marshal-sxml
	    unmarshal-xml unmarshal-sxml
	    ;; for backward compatibility
	    (rename (marshal-xml    marshall-xml)
		    (marshal-sxml   marshall-sxml)
		    (unmarshal-xml  unmarshall-xml)
		    (unmarshal-sxml unmarshall-sxml))

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
	    (sagittarius control)
	    (sagittarius time) ;; for <date>
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (clos user)
	    (clos core)
	    (text sxml serializer)
	    (text sxml tools)
	    (text sxml sxpath)
	    (text sxml ssax)
	    ;; for debug...
	    (pp))

  ;; marker class :)
  (define-class <xml-element> () ())

  (define-syntax define-xml-type
    (lambda (x)
      ;; we are adding meta information to slot definition
      ;; so that marshal and unmarshal can see what needs
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
	      (values slot name type any))))
	(let loop ((slots slots) (r '()))
	  (if (null? slots)
	      (reverse! r)
	      (let*-values (((slot name type any) (resolve-slot (car slots))))
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
	       ;; we can check if the class is marshal/unmarshal-able
	       (define-class name (parents ...)
		 ;; for my convenience attribute first then other elements
		 (attr-defs ... slot-defs ...)
		 :metaclass meta-class)))))))

  ;; TODO add more primitives...
  (define-method primitive->xml-value ((type (eql :string)) value)
    (values '() (->string value)))

  (define (int32? o) (and (integer? o) (<= #x-80000000 o #x7FFFFFFF)))
  (define-method primitive->xml-value ((type (eql :int)) value)
    (unless (int32? value) 
      (error 'primitive->xml-value "32 bit integer required" value))
    (values '() (->string value)))
  (define-method primitive->xml-value ((type (eql :integer)) value)
    (unless (integer? value)
      (error 'primitive->xml-value "integer required" value))
    (values '()  (->string value)))

  (define-method primitive->xml-value ((type (eql :date)) (date <date>))
    (values '() (date->string date "~Y-~m-~d")))
  ;; trust you :)
  (define-method primitive->xml-value ((type (eql :date)) (date <string>)) 
    (values '() date))
  (define-method primitive->xml-value ((type (eql :dateTime)) (date <string>))
    (values '() date))
  (define-method primitive->xml-value ((type (eql :dateTime)) (date <date>))
    (values '() (date->string date "~6")))

  (define-constant +instance:type+ 
    'http://www.w3.org/2001/XMLSchema-instance:type)
  (define-constant +xs-ns+ "http://www.w3.org/2001/XMLSchema")

  ;; if it's something we know how to do it then do it
  (define-method primitive->xml-value ((type (eql :anyType)) (value <string>))
    (values `((,+instance:type+ ,(string-append +xs-ns+ ":string")))
	    value))
  (define-method primitive->xml-value ((type (eql :anyType)) (value <integer>))
    (values `((,+instance:type+ ,(string-append +xs-ns+ ":integer")))
	    (->string value)))
  (define-method primitive->xml-value ((type (eql :anyType)) (value <date>))
    (let-values (((attr v) (primitive->xml-value :dateTime value)))
      (values `((,+instance:type+ ,(string-append +xs-ns+ ":dateTime")))
	      v)))
  ;; if it's not, try unmarshal the value. this must be something
  ;; unmashallable
  (define-method primitive->xml-value ((type (eql :anyType)) value)
    (let ((class (class-of value))
	  (v (unmarshal-sxml value)))
      (values `((,+instance:type+ 
		 ,(symbol->string (convert-name (~ class 'element) class))))
	      ;; we don't need *TOP* and the top most element
	      (sxml:content (cadr v)))))

  (define-method primitive->xml-attribute ((type <keyword>) v)
    (let-values (((_ v) (primitive->xml-value key v))) v))

  (define (convert-name localname class)
    (let ((namespace (~ class 'namespace)))
      (if namespace
	  (string->symbol (format "~a:~a" namespace localname))
	  localname)))

  ;; convert to SXML
  (define (unmarshal-element name element)
    (define (resolve-name s)
      (let ((slot-name (slot-definition-name s)))
	(values slot-name (slot-definition-option s :element-name slot-name))))
    (define (resolve-type s)
      (values ((slot-definition-option s :element-type))
	      (slot-definition-option s :min)
	      (slot-definition-option s :max)
	      (slot-definition-option s :any-element #f)))
    (let ((class (class-of element)))
      (unless (is-a? class <xml-element>)
	(error 'unmarshal "given element is not unmarshallable" element))

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
		       `((,+instance:type+
			  ,(symbol->string name))))))
	     )
	  ;; elements
	  ,@(fold-right 
	     (lambda (s knil)
	       (define (check-element-count elems min max)
		 (cond ((or (pair? elems) (null? elems))
			(let ((n (length elems)))
			  ;; if length returns negative number then this
			  ;; is not a list
			  (when (negative? n)
			    (error 'unmarshal-element
				   "the valus must be a list for maxOccur > 1"
				   elems))
			  (when (< n min)
			    (error 'unmarshal-element 
				   "the element is less than minOccur" 
				   `(min ,min) elems))
			  (when (and (not (eq? max 'unbounded)) (> n max))
			    (error 'unmarshal-element 
				   "the element is more than maxOccur" 
				   `(max ,max) elems))
			  #t))
		       ((or (eq? max 'unbounded) (> max 1))
			(error 'unmarshal-element
			       "the value must be a list for maxOccur > 1"
			       elems))
		       ((and (>= 1 min) (and (number? max) (= max 1))))
		       (else
			(error 'unmarshal-element
			       "the value is too less"
			       `(min ,min) `(max ,max) elems))))
	       (let*-values (((slot-name name) (resolve-name s))
			     ((type min max any) (resolve-type s)))
		 (if (slot-bound? element slot-name)
		     (let ((value (~ element slot-name)))
		       (check-element-count value min max)
		       (cond (any
			      ;; is this correct?
			      (if (or (pair? value) (null? value))
				  `(,@(map cadr (map unmarshal-sxml value))
				    ,@knil)
				  (cons (cadr (unmarshal-sxml value)) knil)))
			     ;; if max > 1 or unbounded then value must be a list
			     ((or (eq? max 'unbounded) (> max 1))
			      `(,@(map (cut unmarshal-element
					    (convert-name name class) <>)
				       value) ,@knil))
			     ((keyword? type)
			      (let-values (((attr v)
					    (primitive->xml-value type value)))
				(cons `(,(convert-name name class) 
					(@ ,@attr) 
					,@(if (pair? v) v (list v)))
				      knil)))
			     ((is-a? (class-of value) <xml-element>)
			      (cons (unmarshal-element (convert-name name class)
						       value)
				    knil))
			     (else
			      (error 'unmarshal-element
				     "unknown element" value))))
		     (or (and (zero? min) knil)
			 (error 'unmarshal-element
				"element must be presented!" name)))))
	     '() elems)))))

  (define (unmarshal-sxml element)
    (let ((class (class-of element)))
      (unless (is-a? class <xml-element>)
	(error 'unmarshal "given element is not unmarshallable" element))
      ;; make sure the top most level has *TOP* for proper SXML.
      (list '*TOP*
       (unmarshal-element (convert-name (~ class 'element) class) element))))
  ;; keyword arguments are more for debugging or so...
  (define (unmarshal-xml element :key 
			  (indent #f)
			  (ns-prefix-assig srl:conventional-ns-prefixes))
    (define (collect-all-namespace xml)
      (let ((m (regex-matcher #/xmlns:(\w+)="([^\"]+)"/ xml)))
	(let loop ((r '()))
	  (if (regex-find m)
	      (loop (acons (m 2) (m 1) r))
	      (cond ((assoc +xs-ns+ r) (values r #f))
		    ((string-contains xml (string-append +xs-ns+ ":"))
		     (values (acons +xs-ns+ "xsd" r) #t))
		    (else (values r #f)))))))
    ;; this is safer so that it doesn't have any unwanted spaces
    (let1 xml (srl:sxml->string (unmarshal-sxml element) '() indent 'xml
				    ns-prefix-assig #t 'omit "1.0")
      ;; fixup some namespace in its attributes...
      (let-values (((namespaces need-xs?) (collect-all-namespace xml)))
	(let loop ((namespaces namespaces) 
		   (xml (if need-xs?
			    (regex-replace-first 
			     #/>/ xml
			     (string-append " xmlns:xsd=\"" +xs-ns+ "\">"))
			    xml)))
	  (if (null? namespaces)
	      xml
	      (loop (cdr namespaces)
		    (let ((ns (car namespaces)))
		      ;; for now very naive one check only type
		      (regex-replace-all 
		       (regex (string-append "type=\"" (car ns) ":(\\w*)\"")) xml 
		       (lambda (m)
			 (let ((ext (format "xmlns:~a=\"~a\"" (cdr ns) (car ns)))
			       (s (format "type=\"~a:~a\"" (cdr ns) (m 1))))
			   (if (string-contains xml ext)
			       s
			       (string-append s " " ext)))))))
	      )))))

  (define-method xml-value->primitive ((type (eql :string)) v)
    (car v))
  (define-method xml-value->primitive ((type (eql :date)) v)
    (string->date (car v) "~Y-~m-~d"))
  (define-method xml-value->primitive ((type (eql :dateTime)) v)
    (string->date (car v) "~Y-~m-~dT~H:~M:~S~z"))

  (define-method xml-value->primitive ((type (eql :int)) v)
    (or (and-let* ((i (string->number (car v)))
		   ( (int32? i) ))
	  i)
	(error 'xml-value->primitive "given value is not 32 bit integer"
	       (car v))))
  (define-method xml-value->primitive ((type (eql :integer)) v)
    (or (and-let* ((i (string->number (car v)))
		   ( (integer? i) ))
	  i)
	(error 'xml-value->primitive "given value is not integer" (car v))))

  ;; well JAXB is also doing this
  ;; and there is no way to determine other than this.
  (define-method xml-value->primitive ((type (eql :anyType)) v)
    (car v))

  (define-method xml-value->primitive (type v)
    (error 'xml-value->primitive "unsupported type" type v))

  ;; well...
  (define-method xml-attribute->primitive ((type <keyword>) v)
    (xml-value->primitive type (list v)))

  ;; need to take class to know which object we need to construct
  (define (marshal-sxml class sxml . contexts)
    (define (check-namespace element ncname namespace)
      (let ((ns (sxml:name->ns-id (sxml:name element))))
	(or (and (equal? ns namespace) ;; ns and namespace can be #f
		 (string=? (symbol->string ncname) (sxml:ncname element)))
	    (error 'marshal-sxml 
		   "element does not belong to the proper namespace" 
		   (if ns (format "~a:~a" ns ncname) ncname) 
		   namespace
		   element
		   sxml))))

    (define (marshal-rec o class content root-element)
      (let ((namespace (~ class 'namespace)))
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
	       (error 'marshal-sxml
		      "mandatory attribute is missing" 
		      (get-name s) root-element))
	     (when attr
	       (let ((v (if (keyword? type)
			    (xml-attribute->primitive type attr)
			    ;; must be a class
			    (let ((o (make type))) 
			      (marshal-rec o type (sxml:content attr) attr)))))
		 (set! (~ o slot-name) v)))))
	 (filter-map (lambda (s)
		       (and (slot-definition-option s :attribute #f)
			    s)) (class-slots class)))
	;; elements
	(for-each
	 (lambda (s)
	   (define (get-max s) (slot-definition-option s :max))
	   (define (get-min s) (slot-definition-option s :min))
	   (define (get-any s) (slot-definition-option s :any-element #f))
	   (define (get-real-type class e)
	     (or (and-let* ((xml-type (sxml:attr e +instance:type+))
			    (subclasses (class-direct-subclasses class)))
		   (find (lambda (c)
			   (let ((ns (slot-ref c 'namespace))
				 (e  (slot-ref c 'element)))
			     (string=? xml-type (format "~a:~a" ns e))))
			 subclasses))
		 class))
	   (define (extract-type name type)
	     (if name
		 (or (and (string-prefix? +xs-ns+ name)
			  (let* ((len (string-length name))
				 ;; it's exported but i think it shouldn't 
				 (pos (string-index-right name #\:)))
			    (string->keyword (substring name (+ pos 1) len))))
		     ;; find from context
		     (and (not (null? contexts))
			  (find (lambda (class)
				  (eq? (~ class 'element)
				       (string->symbol name)))
				contexts)))
		 type))
	   (define (check-element-count elems min max full-name)
	     (let ((n (length elems)))
	       (when (< n min)
		 (error 'marshal-sxml "too less elements"
			`((min ,min) (max ,max)) elems full-name))
	       (when (and (not (eq? max 'unbounded)) (> n max))
		 (error 'marshal-sxml "too many elements"
			`((min ,min) (max ,max)) elems full-name))))
	   (define (find-class elem contexts)
	     (let loop ((classes contexts))
	       (if (null? classes)
		   (error 'marshal-sxml "don't know how to marshal" elem
			  contexts)
		   (let1 class (car classes)
		     (if (is-a? class <xml-element>)
			 (let* ((name (~ class 'element))
				(ns   (~ class 'namespace))
				(full-name (->full-name ns name)))
			   (if (eq? full-name (car elem))
			       (values class (lset-difference eq? (list class)
							      contexts))
			       (loop (cdr classes))))
			 ;; well ...
			 (loop (cdr classes)))))))
	   (let* ((ncname (get-name s))
		  (slot-name (slot-definition-name s))
		  (full-name (->full-name namespace ncname))
		  (type (get-type s))
		  (max (get-max s))
		  (min (get-min s))
		  (any (get-any s))
		  (e ((sxml:filter (lambda (e)
				     (eq? (sxml:name e) full-name)))
		      content)))
	     ;; filter the content so that it won't be processed
	     ;; more than once
	     ;; this is funny, lset-difference actually cares the order... 
	     (set! content (lset-difference eq? content e))
	     (let ((v (cond (any
			     (map (lambda (c)
				    (let-values (((this rest)
						  (find-class c contexts)))
				      (apply marshal-sxml this 
					     (cons '*TOP* c)
					     rest)))
				   content))
			    ((keyword? type)
			     (map (lambda (e)
				    (let* ((attr (sxml:attr e +instance:type+))
					   (type (extract-type attr type)))
				      (unless type
					(error 'marshal-sxml "unknown type"
					       attr))
				      (if (keyword? type)
					  (xml-value->primitive 
					   type (sxml:content e))
					  (let ((o (make type)))
					    (marshal-rec o type 
							 (sxml:content e) e)))))
				  e))
			    (else
			      ;; must be a class
			      (map (lambda (e)
				     (unless (is-a? type <xml-element>)
				       (error 'marshal
					      "given class is not marshallable"
					      type))
				     (let* ((type (get-real-type type e))
					    (o (make type)))
				       (marshal-rec o type
						    (sxml:content e) e)))
				   e)))))
	       ;; check min max
	       (check-element-count v min max full-name)
	       (cond ((or (eq? max 'unbounded) (> max 1))
		      (set! (~ o slot-name) v))
		     ((not (null? v))
		      (set! (~ o slot-name) (car v)))))))
	 (filter-map (lambda (s)
		       (and (not (slot-definition-option s
							 :attribute
							 #f))
			    s)) (class-slots class)))
	o))
    (unless (is-a? class <xml-element>)
      (error 'marshal "given class is not marshallable" class))
    (let ((o (make class)))
      ;; assume *TOP* is there!
      (let ((root-element (car (sxml:content sxml)))
	    (element (~ class 'element))
	    (namespace (~ class 'namespace)))
	(check-namespace root-element element namespace)	
	(marshal-rec o class (sxml:content root-element) root-element))))

  (define (marshal-xml class xml . contexts)
    (apply marshal-sxml class (%ssax:xml->sxml (open-string-input-port xml) '())
	   contexts))

  ;; internal parser...
  (define (%ssax:xml->sxml port dummy)
    (define (symbol-append a b)
      (string->symbol (format "~a:~a" a b)))
    ;; To make things easier, we don't use user defined namespace-prefix-assig
    (define namespace-prefix-assig '())
    ;; the attribute which value needs to be qualified.
    ;; say 'name', 'id' and so on should not be but 'type'
    ;; TODO, not sure which one should be other than 'type'
    (define qualified `(,+instance:type+))
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

  (define (->full-name ns name)
    (if ns
	(string->symbol (format "~a:~a" ns name))
	name))
)