# Sagittarius XSD

This library provides some utilities to handle XSD and XML marshall and
unmarshall.

## XSD to Scheme

This library contains XSD to Scheme utility `xsd2scm`. The file is bare
Scheme script depending on the libraries.

## Collaborate with SOAP

You can use 
[Sagittarius SOAP lite](https://github.com/ktakashi/sagittarius-soap)
or manually send it.

Following is an example how to collabolate with SOAP lite library.

```scheme
(import (soap lite) (soap transport http) (rfc http))

;; Simple example, o is the Scheme XML object.
(define (send&receive url o)
  (let*-values (((server path) (url-server&path url)))
      (let ((context (make-soap-context +soap-version-1.1+
                                        :transport
                                        (make-http-transport server path))))
        (soap-send-request context
                           (soap:envelope context
                                          (soap:body (unmarshall-xml o)))))))
```

## TODO

* document. 
* more test.
* cover whole XSD specification.

Since I didn't write API document yet so for users who want't to try this,
please see `test.scm`, you will find how to use.

Your pull request is always welcome :)