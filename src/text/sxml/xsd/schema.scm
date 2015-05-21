;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sxml/xsd/schema.scm - XSD Schema library.
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

;; don't say this is a weird name
(library (text sxml xsd schema)
    (export parse-xsd
	    parse-xsd-file
	    parse-xsd-url

	    ;; pre-defined locators
	    file-locator
	    url-locator

	    ;; namespaces
	    +xsd-1999-ns-uri+
	    +xsd-2000-ns-uri+
	    +xsd-2001-ns-uri+
	    
	    ;; classes
	    ;; elements
	    <xml-schema-definition>
	    <xml-schema-element>
	    <xml-schema-element-proxy>
	    ;; xsd types
	    <xsd-simple-type>
	    <xsd-simple-content>
	    <xsd-complex-type>
	    <xsd-complex-content>
	    <xsd-compositor>
	    <xsd-sequence>
	    <xsd-choice>
	    <xsd-all>
	    <xsd-wildcard>
	    <xsd-any>
	    <xsd-any-attribute>
	    <xsd-restriction>
	    <xsd-extension>
	    <xsd-attribute>
	    <xsd-attribute-group>
	    ;; references
	    <xsd-unique>
	    <xsd-key>
	    <xsd-keyref>
	    <xsd-selector>
	    <xsd-field>
	    ;; list
	    <xsd-list>
	    <xsd-union>

	    schema-source
	    ;; accessors
	    schema-target-namespace
	    schema-name
	    schema-type
	    schema-elements
	    schema-children
	    schema-namespace
	    schema-min-occurs
	    schema-max-occurs
	    schema-nillable
	    schema-ref
	    schema-min-occurs-is-local
	    schema-max-occurs-is-local
	    schema-nillable-is-local
	    schema-resolved-status
	    schema-process-contents
	    schema-base
	    schema-scope
	    schema-use
	    schema-default
	    schema-fixed
	    schema-refer
	    schema-xpath
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (clos core)
	    (clos user)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (text sxml ssax)
	    (text sxml tools)
	    (rfc uri)
	    (rfc http)
	    (pp))

  ;; I don't think we need 1999 and 2000/10 but *just* in case.
  (define-constant +xsd-1999-ns-uri+ "http://www.w3.org/1999/XMLSchema")
  (define-constant +xsd-2000-ns-uri+ "http://www.w3.org/2000/10/XMLSchema")
  (define-constant +xsd-2001-ns-uri+ "http://www.w3.org/2001/XMLSchema")

  (define-constant +ns-list+ `(,+xsd-2001-ns-uri+
			       ,+xsd-2000-ns-uri+
			       ,+xsd-1999-ns-uri+))

  ;; parse context
  ;; XSD allows to have cross-referenced import or include and this
  ;; causes infinite loop to resolve. To avoid this we need to manage
  ;; which file is already loaded and if we find the same file then
  ;; we need to skip it.
  (define-class <parse-context> ()
    (;; alist of file and parsed schema for import
     (schemas :init-value '())
     ;; alist of file and parsed schemas for include.
     (included :init-value '())))
  (define (make-parse-context) (make <parse-context>))
  ;; for my laziness
  ;; this holds the current parse context so that handle-schema
  ;; can refer it.
  (define *current-context* (make-parameter #f))

  ;; conditions
  (define-condition-type &xsd-error &error make-xsd-error xsd-error?
    (element xsd-element))
  (define-condition-type &xsd-premature &xsd-error
    make-xsd-premature xsd-premature?)
  (define-condition-type &xsd-unknown-element &xsd-error
    make-xsd-unknown-element xsd-unknown-element?)

  (define (raise-xsd-error xsd who message . irr)
    (raise (apply condition 
		  (filter values
			  (list xsd
				(and who (make-who-condition who))
				(make-message-condition message)
				(make-irritants-condition irr))))))
  (define (raise-warning who message)
    (raise (condition (make-warning) who message)))

  ;; accessor generics
  ;; seems we will step into the bug of Sagittarius...
  (define-generic schema-target-namespace)
  (define-generic schema-name)
  (define-generic schema-type)
  (define-generic schema-elements)
  (define-generic schema-children)
  (define-generic schema-namespace)
  (define-generic schema-min-occurs)
  (define-generic schema-max-occurs)
  (define-generic schema-nillable)
  (define-generic schema-ref)
  (define-generic schema-min-occurs-is-local)
  (define-generic schema-max-occurs-is-local)
  (define-generic schema-nillable-is-local)
  (define-generic schema-resolved-status)
  (define-generic schema-process-contents)
  (define-generic schema-base)
  (define-generic schema-scope)
  (define-generic schema-use)
  (define-generic schema-default)
  (define-generic schema-fixed)
  (define-generic schema-refer)
  (define-generic schema-xpath)

  ;;;;;;
  ;;; Schema types
  ;; all readers and writers are internal use
  (define-class <xml-schema-definition> ()
    ((target-namespace :reader schema-target-namespace
		       :init-keyword :target-namespace
		       :init-value #f)
     (elements :accessor schema-elements
	       :init-keyword :elements
	       :init-value '())
     (source :reader schema-source :init-keyword :source :init-value #f)))
  (define-method write-object ((o <xml-schema-definition>) out)
    (format out "#<xml-schema-definition target ~a, ~d elements>"
	    (schema-target-namespace o)
	    (length (schema-elements o))))
  ;; Mixin for xsd elements with children
  (define-class <children-mixin> ()
    ((children :accessor schema-children
	       :init-keyword :children
	       :init-value '())))
  ;; Mixin for xsd elements with minOccurs and maxOccurs attributes.
  (define-class <multiplicity-mixin> ()
    ((min-occurs :accessor schema-min-occurs
		 :init-keyword :min-occurs :init-value 1)
     (max-occurs :accessor schema-max-occurs
		 :init-keyword :max-occurs :init-value 1)))
  ;; Mixin for xsd elements with nillable attributes.
  (define-class <nillable-multiplicity-mixin> (<multiplicity-mixin>)
    ((nillable :accessor schema-nillable
	       :init-keyword :nillable :init-value #f)))

  ;;; schema elements
  ;; Representation of the ELEMENT element.
  (define-class <xml-schema-element> (<children-mixin>
				      <nillable-multiplicity-mixin>)
    ((name :accessor schema-name :init-keyword :name :init-value #f)
     (namespace :accessor schema-namespace
		:init-keyword :namespace :init-value "")
     (type :accessor schema-type :init-keyword :type :init-value #f)))
  (define-method write-object ((o <xml-schema-element>) out)
    (format out "#<xml-schema-element ~a>"
	    (or (schema-name o)
		"anonymous")))
  ;; Representation of an xml-schema-element that bears a ref attribute for late
  ;; resolving
  (define-class <xml-schema-element-proxy> (<xml-schema-element>)
    ((ref :accessor schema-ref :init-keyword :ref :init-value #f)
     (min-occurs-is-local :accessor schema-min-occurs-is-local :init-value #f)
     (max-occurs-is-local :accessor schema-max-occurs-is-local :init-value #f)
     (nillable-is-local :accessor schema-nillable-is-local :init-value #f)
     (resolved-status :accessor schema-resolved-status :init-value #f)))
  (define-method schema-name ((o <xml-schema-element-proxy>))
    (if (schema-resolved-status o)
	(call-next-method)
	(raise-xsd-error
	 (make-xsd-premature o)
	 'schema-name "Element is not yet resolved to base element." o)))
  (define-method schema-type ((o <xml-schema-element-proxy>))
    (if (schema-resolved-status o)
	(call-next-method)
	(raise-xsd-error
	 (make-xsd-premature o)
	 'schema-name "Element is not yet resolved to base element." o)))
  (define-method schema-min-occurs ((o <xml-schema-element-proxy>))
    (if (or (schema-min-occurs-is-local o)
	    (schema-resolved-status o))
	(call-next-method)
	(raise-xsd-error
	 (make-xsd-premature o)
	 'schema-name "Element is not yet resolved to base element." o)))
  (define-method (setter schema-min-occurs) ((o <xml-schema-element-proxy>)
					     (min <integer>))
    (if (or (schema-min-occurs-is-local o)
	    (schema-resolved-status o))
	(call-next-method)
	(raise-xsd-error
	 (make-xsd-premature o)
	 'schema-name "Element is not yet resolved to base element." o)))

  (define-method schema-max-occurs ((o <xml-schema-element-proxy>))
    (if (or (schema-max-occurs-is-local o)
	    (schema-resolved-status o))
	(call-next-method)
	(raise-xsd-error
	 (make-xsd-premature o)
	 'schema-name "Element is not yet resolved to base element." o)))
  (define-method write-object ((o <xml-schema-element-proxy>) out)
    (format out "#<xml-schema-element-proxy ~a~a>"
	    (or (~ o 'name) "anonymous")
	    (if (schema-resolved-status o)
		" (resolved)"
		"")))

  ;; resolve scheme proxy

  ;; types
  (define-class <xsd-type> (<children-mixin>)
    ((name :accessor schema-name :init-keyword :name :init-value #f)
     (namespace :accessor schema-namespace
		:init-keyword :namespace :init-value "")))
  (define-method write-object ((o <xsd-type>) out)
    (format out "#<~a ~a>"
	    (~ (class-of o) 'name)
	    (or (schema-name o) "anonymous")))
  (define-class <xsd-simple-type> (<xsd-type>) ())
  (define-class <xsd-simple-content> (<children-mixin>) ())
  (define-class <xsd-complex-type> (<xsd-type>) ())
  (define-class <xsd-complex-content> (<children-mixin>) ())
  (define-class <xsd-compositor> (<children-mixin> <multiplicity-mixin>) ())
  (define-class <xsd-sequence> (<xsd-compositor>) ())
  (define-class <xsd-choice> (<xsd-compositor>) ())
  (define-class <xsd-all> (<xsd-compositor>) ())
  (define-class <xsd-wildcard> (<xsd-type>) ())
  ;; class representing the <any> element.
  (define-class <xsd-any> (<xsd-wildcard> <multiplicity-mixin>)
    ((process-contents :accessor schema-process-contents
		       :init-keyword :process-contents :init-value #f)))
  (define-class <xsd-any-attribute> (<xsd-any>) ())

  ;; TODO handle inside of the restriction...
  (define-class <xsd-restriction> ()
    ((base :accessor schema-base :init-keyword :base :init-value #f)))

  (define-class <xsd-extension> (<children-mixin>)
    ((base :accessor schema-base :init-keyword :base :init-value #f)))

  ;; Attribute declaration object
  (define-class <xsd-attribute> (<children-mixin>)
    ((name :accessor schema-name :init-keyword :name :init-value #f)
     (namespace :accessor schema-namespace 
		:init-keyword :namespace :init-value "")
     (type :accessor schema-type :init-keyword :type :init-value #f)
     (scope :accessor schema-scope :init-keyword :scope :init-value #f)
     (use :accessor schema-use :init-keyword :use :init-value #f)
     (default :accessor schema-default :init-keyword :default :init-value #f)
     (fixed :accessor schema-fixed :init-keyword :fixed :init-value #f)))
  (define-method write-object ((o <xsd-attribute>) out)
    (format out "#<xsd-attribute ~a>"
	    (or (schema-name o) "anonymous")))

  (define-class <xsd-attribute-group> (<children-mixin>) 
    ((name :accessor schema-name :init-keyword :name :init-value #f)
     (ref  :accessor schema-ref :init-keyword :ref :init-value #f)))
  (define-method write-object ((o <xsd-attribute-group>) out)
    (format out "#<xsd-attribute-group ~a:~d>"
	    (or (schema-name o) "anonymous")
	    (length (schema-children o))))

  ;;  better reference mechanisms
  (define-class <xsd-unique> (<xsd-type>) ())
  (define-class <xsd-key> (<xsd-type>)())
  (define-class <xsd-keyref> (<xsd-type>)
    ((refer :accessor schema-refer :init-keyword :refer :init-value #f)))

  (define-class <xsd-selector> ()
    ((xpath :accessor schema-xpath :init-keyword :xpath :init-value #f)))

  (define-class <xsd-field> ()
    ((xpath :accessor schema-xpath :init-keyword :xapth :init-value #f)))

  (define-class <xsd-list> (<xsd-type>)
    ((item-type :accessor schema-item-type :init-keyword :item-type 
		:init-value #f)))
  (define-class <xsd-union> (<xsd-type>)
    ((member-types :accessor schema-member-type :init-keyword :member-types
		   :init-value #f)))

  ;;;;; APIs

  ;;;; User level APIs
  #|
  Parse stream IN and return a list of xml-schema-definition objects.
  Keyword arguments:
  :IMPORT can be used to define the handling of <import> elements.
          If NIL (the default) the element is ignored.
          A non-nil argument is assumed to be a function of 1 argument, the
          schemaLocation attribute of the element. If the function returns NIL,
          the element is ignored. Otherwise is must return a string which can
          be parsed to yield a xsd-schema-definition object.
          The functions wsdl-include-file and wsdl-include-url are suitable 
          arguments.
  :INCLUDE behaves like :IMPORT, but for the <include> element. 
  |#
  (define (parse-xsd in . opts)
    (let ((sxml (ssax:xsd->sxml in)))
      (apply sxml->schema-definitions sxml opts)))

  (define (parse-xsd-file file :key (locator file-locator))
    (call-with-input-file file
      (cut parse-xsd <> :locator locator :source file)))

  (define (parse-xsd-url url :key (locator url-locator))
    (parse-xsd (open-string-input-port (url-locator url))
	       :locator locator :source url))

  (define (file-locator schema-location) 
    (call-with-input-file schema-location (cut get-string-all <>)))

  (define (url-locator schema-location) 
    (let*-values (((scheme specific) (uri-scheme&specific schema-location))
		  ((auth path query frag) (uri-decompose-hierarchical specific))
		  ((status header body)
		   (http-get auth (uri-compose :path path
					       :query query
					       :fragment frag))))
      (if (string=? status "200")
	  body
	  (error 'parse-xsd-url "could not retrieve URL" schema-location))))

  ;;;;;;
  ;;; Middle level APIs
  ;;; this assmume all xsd namespace converted xsd: prefix

  (define (sxml->schema-definitions sxml :key (source #f) 
				    (context (make-parse-context))
				    :allow-other-keys opts)
    (let ((content (sxml:content sxml)))
      ;; sanity check
      (when (null? content) 
	(error 'sxml->schema-definitions "given sxml is not XSD" sxml))
      (unless (null? (cdr content))
	(error 'sxml->schema-definitions "given sxml is not a valid XML" sxml))
      (let* ((top (car content))
	     (name (sxml:name top))
	     (ncname (sxml:ncname top))
	     (ns   (sxml:name->ns-id name)))
	(if (member ns +ns-list+)
	    (let* ((target-namespace (sxml:attr top 'targetNamespace))
		   (xsd (make <xml-schema-definition>
			  :target-namespace target-namespace
			  :source source))
		   (elements (sxml:content top)))
	      (define (xsd? o) (and (is-a? o <xml-schema-definition>) o))
	      (define (not-xsd? o) (and (not (xsd? o)) o))
	      (let loop ((elms elements) (imported '()))
		(if (null? elms)
		    (rlet1 r (cons xsd imported)
		      (resolve-schemas r))
		    (let* ((e (car elms))
			   (elm (invoke-handle-schema e target-namespace
						      context opts)))
		      (cond ((pair? elm)
			     (cond ((eq? (car elm) :imported)
				    (loop (cdr elms) 
					  `(,@(cdr elm) . ,imported)))
				   ((eq? (car elm) :included-part)
				    ;; included-part may contains
				    ;; xsds which are *imported*
				    (set! (~ xsd 'elements)
					  (append!
					   (filter-map not-xsd? (cdr elm))
					   (~ xsd 'elements)))
				    (loop (cdr elms) 
					  `(,@(filter-map xsd? (cdr elm)) 
					    . ,imported)))
				   (else
				    (error 'internal "parse error"))))
			    (elm 
			     (set! (~ xsd 'elements) 
				   (cons elm (~ xsd 'elements)))
			     (loop (cdr elms) imported))
			    (else
			     (loop (cdr elms) imported)))))))
	    (raise-xsd-error
	     (make-xsd-error name)
	     'sxml->schema-definitions "unknown namespace" ns)))))

  ;; misc
  (define (invoke-handle-schema e namespace context opts)
    (define (->keyword s) (make-keyword (string->symbol s)))
    (parameterize ((*current-context* context))
      (handle-schema (->keyword (sxml:ncname e)) e namespace opts)))

  (define-method handle-schema (name elem namespace opts)
    (error 'internal "not supported" name elem))

  ;; element handlers
  (define-method handle-schema ((name (eql :element)) elms namespace opts)
    (let* ((attrs (sxml:attr-list-node elms))
	   (name  (sxml:attr-from-list attrs 'name))
	   (type  (sxml:attr-from-list attrs 'type))
	   (ref   (sxml:attr-from-list attrs 'ref))
	   (e  (apply make 
		      (if ref <xml-schema-element-proxy> <xml-schema-element>)
		      :name name
		      :namespace namespace
		      :type type
		      (if ref `(:ref ,ref) '()))))
      (handle-schema-elements e elms namespace opts)))

  ;; complexType
  (define-method handle-schema ((name (eql :complexType)) elms namespace opts)
    (let* ((name (sxml:attr elms 'name))
	   (xsd-type (make <xsd-complex-type> :name name :namespace namespace)))
      (handle-schema-elements xsd-type elms namespace opts)))

  ;; complexContent
  (define-method handle-schema 
    ((name (eql :complexContent)) elms namespace opts)
    (let ((cc (make <xsd-complex-content>)))
      (handle-schema-elements cc elms namespace opts)))

  ;; choice
  (define-method handle-schema ((name (eql :choice)) elms namespace opts)
    (handle-schema-elements (make <xsd-choice>) elms namespace opts))

  ;; attribute
  (define-method handle-schema ((name (eql :attribute)) elms namespace opts)
    (let* ((attrs (sxml:attr-list-node elms))
	   (name (sxml:attr-from-list attrs 'name))
	   (type (sxml:attr-from-list attrs 'type))
	   (use  (sxml:attr-from-list attrs 'use))
	   (default (sxml:attr-from-list attrs 'default))
	   (fixed   (sxml:attr-from-list attrs 'fixed))
	   (attr (make <xsd-attribute>
		   :name name
		   :namespace namespace
		   :type type
		   :use (case (and use (string->symbol use))
			  ((required)   :required)
			  ((prohinited) :prohibited)
			  (else :optional))
		   :default default
		   :fixed fixed)))
      (handle-schema-elements attr elms namespace opts)))

  ;; attributeGroup
  (define-method handle-schema ((name (eql :attributeGroup))
				elms namespace opts)
    (let* ((attrs (sxml:attr-list-node elms))
	   (name (sxml:attr-from-list attrs 'name))
	   (ref (sxml:attr-from-list attrs 'ref))
	   (group (make <xsd-attribute-group> :name name :ref ref)))
      (handle-schema-elements group elms namespace opts)))

  ;; sequence
  (define-method handle-schema ((name (eql :sequence)) elms namespace opts)
    (handle-schema-elements (make <xsd-sequence>) elms namespace opts))

  ;; simpleType
  (define-method handle-schema ((name (eql :simpleType)) elms namespace opts)
    (let ((name (sxml:attr elms 'name)))
      (handle-schema-elements (make <xsd-simple-type> 
				:name name
				:namespace namespace) elms namespace opts)))

  ;; simpleContent
  (define-method handle-schema ((name (eql :simpleContent)) elms namespace opts)
    (handle-schema-elements (make <xsd-simple-content>) elms namespace opts))

  ;; restriction
  (define-method handle-schema ((name (eql :restriction)) elms namespace opts)
    (let ((xsd-type (make <xsd-restriction> :base (sxml:attr elms 'base))))
      (handle-schema-elements xsd-type elms namespace opts)))

  ;; extension
  (define-method handle-schema ((name (eql :extension)) elms namespace opts)
    (let ((xsd-type (make <xsd-extension> :base (sxml:attr elms 'base))))
      (handle-schema-elements xsd-type elms namespace opts)))

  ;; any
  (define-method handle-schema ((name (eql :any)) elms namespace opts)
    (let* ((pc (sxml:attr elms 'processContents))
	   (min (sxml:attr elms 'minOccurs))
	   (max (sxml:attr elms 'maxOccurs))
	   (any (make <xsd-any>
		  :process-contents (case (and pc (string->symbol pc))
				      ((skip) :skip)
				      ((lax)  :lax)
				      (else :strict)))))
      (handle-schema-elements any elms namespace opts)))

  (define-method handle-schema ((name (eql :anyAttribute)) elms namespace opts)
    (let* ((pc (sxml:attr elms 'processContents))
	   (any (make <xsd-any-attribute>
		  :process-contents (case (and pc (string->symbol pc))
				      ((skip) :skip)
				      ((lax)  :lax)
				      (else :strict)))))
      (handle-schema-elements any elms namespace opts)))
  ;; import
  (define-method handle-schema ((name (eql :import)) elms namespace opts)
    (and-let* ((imported (apply sxml->imported-xsd elms namespace opts)))
      (cons :imported imported)))

  ;; include
  (define-method handle-schema ((name (eql :include)) elms namespace opts)
    (and-let* ((included (apply sxml->include-xsd elms namespace opts)))
      (cons :included-part included)))

  ;; refer...
  ;; TODO check attrs, all required...
  (define-method handle-schema ((name (eql :unique)) elms namespace opts)
    (handle-schema-elements (make <xsd-unique> :name (sxml:attr elms 'name))
			    elms namespace opts))
  (define-method handle-schema ((name (eql :key)) elms namespace opts)
    (handle-schema-elements (make <xsd-key> 
			      :name (sxml:attr elms 'name))
			    elms namespace opts))

  (define-method handle-schema ((name (eql :keyref)) elms namespace opts)
    (handle-schema-elements (make <xsd-keyref> 
			      :name (sxml:attr elms 'name)
			      :refer (sxml:attr elms 'refer))
			    elms namespace opts))

  (define-method handle-schema ((name (eql :selector)) elms namespace opts)
    (handle-schema-elements (make <xsd-selector> :xpath (sxml:attr elms 'xpath))
			    elms namespace opts))
  (define-method handle-schema ((name (eql :field)) elms namespace opts)
    (handle-schema-elements (make <xsd-field> :xpath (sxml:attr elms 'xpath))
			    elms namespace opts))

  (define-method handle-schema ((name (eql :list)) elms namespace opts)
    (handle-schema-elements 
     (make <xsd-list> :item-type (sxml:attr elms 'itemType))
     elms namespace opts))

  (define-method handle-schema ((name (eql :union)) elms namespace opts)
    (handle-schema-elements 
     (make <xsd-unique> :member-types (sxml:attr elms 'memberTypes))
     elms namespace opts))

  ;; ignorable elements
  ;; annnoation
  (define-method handle-schema ((name (eql :annotation)) elms namespace opts)
    #f)

  ;; initialsation handlers
  ;; top most doesn nothing just returns the given element
  (define-method handle-schema-elements (e sxml namespace opts) e)

  ;; children
  (define-method handle-schema-elements 
    ((ch <children-mixin>) sxml namespace opts)
    (let ((children (filter-map 
		     (cut invoke-handle-schema <> namespace 
			  (*current-context*) opts)
		     (sxml:content sxml))))
      (set! (~ ch 'children) children)
      (call-next-method)))

  (define-method handle-schema-elements ((p <multiplicity-mixin>)
					 sxml namespace opts)
    (call-next-method)
    (let ((attrs (sxml:attr-list-node sxml)))
      (when attrs
	(set! (~ p 'min-occurs) 
	      (or (let1 m (sxml:attr-from-list attrs 'minOccurs)
		    (and m (string->number m))) 1))
	(set! (~ p 'max-occurs) 
	      (or (and-let* ((m (sxml:attr-from-list attrs 'maxOccurs)))
		    (if (string=? m "unbounded")
			'unbounded
			(string->number m)))
		  1)))
      p))
  (define-method handle-schema-elements ((p <nillable-multiplicity-mixin>)
					 sxml namespace opts)
    (call-next-method)
    (let ((attrs (sxml:attr-list-node sxml)))
      (set! (~ p 'nillable) (sxml:attr-from-list attrs 'nillable))
      p))
  (define-method handle-schema-elements ((p <xml-schema-element>)
					 sxml namespace opts)
    (call-next-method))

  ;; element proxy
  (define-method handle-schema-elements ((p <xml-schema-element-proxy>)
					 sxml namespace opts)
    (call-next-method)
    (let ((attrs (sxml:attr-list-node sxml)))
      (set! (~ p 'nillable-is-local) (sxml:attr-from-list attrs 'nillable))
      (set! (~ p 'min-occurs-is-local) (sxml:attr-from-list attrs 'minOccurs))
      (set! (~ p 'max-occurs-is-local) (sxml:attr-from-list attrs 'maxOccurs))
      p))

  ;; resolve proxy
  (define (resolve-schemas xsds)
    (dolist (xsd xsds)
      (for-each (cut resolve-element <> xsds) (schema-elements xsd))))

  (define (schema-element-named xsd name)
    (define (find-item-named name elements)
      (define (full-name e)
	(let1 ns (schema-namespace e)
	  (if ns
	      (format "~a:~a" ns (schema-name e))
	      (schema-name e))))
      (find (lambda (e) 
	      (and (is-a? e <xml-schema-element>)
		   (equal? name (full-name e)))) elements))
    (find-item-named name (schema-elements xsd)))

  (define-method resolve-element (e xsds) #t) ;; ignore
  (define-method resolve-element ((e <children-mixin>) xsds)
    (for-each (cut resolve-element <> xsds) (schema-children e)))
  (define-method resolve-element ((e <xml-schema-element-proxy>) xsds)
    (unless (schema-resolved-status e)
      (let1 ref (schema-ref e)
	(define (search ref xsds)
	  (let loop ((xsds xsds))
	    (if (null? xsds)
		#f ;; should this be an error?
		(or (schema-element-named (car xsds) ref)
		    (loop (cdr xsds))))))
	(let1 elt (search ref xsds)
	  (schema-resolved-status e elt)
	  (dolist (child (schema-children elt))
	    (when child (resolve-element child xsds)))
	  (schema-name e (schema-name elt))
	  (schema-type e (schema-type elt))
	  (schema-children e (schema-children elt))
	  (unless (schema-min-occurs-is-local e)
	    (schema-min-occurs e (schema-min-occurs elt)))
	  (unless (schema-max-occurs-is-local e)
	    (schema-max-occurs e (schema-max-occurs elt)))
	  (unless (schema-nillable-is-local e)
	    (schema-nillable e (schema-nillable elt)))))))

  ;;;;  Internal
  ;; handling import and include are bit tricky. we basically resolve
  ;; library dependency however if the XSD contains cross-reference
  ;; then we don't import the previous one. For example;
  ;; files/interdependent1.xsd and files/interdependent2.xsd are
  ;; cross-referenced XSDs. If xsd2scm generates a library for 
  ;; files/interdependent1.xsd then the dependency library of 
  ;; files/interdependent2.xsd won't import the first library to
  ;; avoid infinite loop of library resolution.
  ;; I believe this is reasonable decision but not 100% sure yet.

  ;; import
  (define (sxml->imported-xsd sxml namespace :key (locator #f))
    (let* ((attrs (sxml:attr-list-node sxml))
	   (target-namespace (or (sxml:attr-from-list attrs 'namespace)
				 namespace))
	   (schema-location (sxml:attr-from-list attrs 'schemaLocation))
	   ;; Some of bad mannered XSD can have no schemaLocation attribute
	   (imported (and schema-location locator (locator schema-location))))
      (cond ((assoc schema-location (~ (*current-context*) 'schemas)) => cdr)
	    (imported
	     (let ((mark (cons schema-location #f)))
	       (push! (~ (*current-context*) 'schemas) mark)
	       (let ((imported-xsds (parse-xsd (open-string-input-port imported)
					       :locator locator
					       :context (*current-context*)
					       :source schema-location)))
		 (set! (~ (car imported-xsds) 'target-namespace)
		       target-namespace)
		 ;; for now we assume it's only one xsd
		 ;; TODO can we?
		 (set-cdr! mark (car imported-xsds))
		 imported-xsds)))
	    (else #f))))

  ;; include
  (define (sxml->include-xsd sxml namespace :key (locator #f))
    (let* ((attrs (sxml:attr-list-node sxml))
	   (schema-location (sxml:attr-from-list attrs 'schemaLocation))
	   (include (and schema-location locator (locator schema-location))))
      (cond ((assoc schema-location (~ (*current-context*) 'included))
	     => (lambda (slot)
		  (unless (cdr slot)
		    ;; simply we don't know how to handle this...
		    (raise-xsd-error 
		     (make-xsd-error sxml)
		     'xs:include
		     "Cross referenced include is not supported"))
		  (cdr slot)))
	    (include
	     (let ((mark (cons schema-location #f)))
	       (push! (~ (*current-context*) 'included) mark)
	       (let ((schemas (parse-xsd (open-string-input-port include)
					 :locator locator
					 :context (*current-context*)
					 :source schema-location)))
		 ;; only interested in the first XSD (the rest must be as it is)
		 ;; for my convenience though...
		 (rlet1 r (append! (map 
				    (lambda (e)
				      (when (or (is-a? e <xml-schema-element>)
						(is-a? e <xsd-type>))
					(schema-namespace e namespace))
				      e)
				    (schema-elements (car schemas)))
				   (cdr schemas))
		   (set-cdr! mark r)))))
	    (else #f))))

  ;; internal parser
  (define (ssax:xsd->sxml port)
    ;; To make things easier, we don't use user defined namespace-prefix-assig
    (define namespace-prefix-assig '())
    ;; the attribute which value needs to be qualified.
    ;; say 'name', 'id' and so on should not be but 'type'
    ;; TODO, not sure which one should be other than 'type' and 'base'.
    (define qualified '(type base ref itemType memberTypes))
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
			    (let* ((raw-attr (car attr))
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
