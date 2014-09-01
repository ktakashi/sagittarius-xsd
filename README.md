# Sagittarius XSD

This library provides some utilities to handle XSD and XML marshal and
unmarshal.

## XSD to Scheme

This library contains XSD to Scheme utility `xsd2scm`. The file is bare
Scheme script depending on the libraries.

### Rule file

The conversion from XSD to Scheme will try to resolve dependency which
is XSD import. To resolve it properly, users need to specify rule file
for `xsd2scm`. Following is an example for rule file

```scheme
((:file "foo.xsd" :name (foo) :output "foo.scm")
 ("http://namespace1/name")
 ("http://namespace2/name" :name (name2) :output "name2.scm"))
```

The rule file is a list of rule. A rule must be one of following forms;

- (:file _XSD file_ :name _library name_)
- (:file _XSD file_ :name _library name_ :output _output file_)
- (_namespace_)
- (_namespace_ :name _library name_)
- (_namespace_ :name _library name_ :output _output file_)
- (_any of above_ :no-namespace _indicator_)

_XSD file_ must be the parsed XSD file name. _namespace_ must be parsed
XSD file's targetNamespace attribute value. :no-namespace to make code
without namespace. In some case, WSDL 1.1 for example, requires to elimiate
the target namespace for some reason. To do that _indicator_ must be a
true value. By default, it's #f.

If the dependency is not specified or matched, then it will be ignored.
Thus writing proper dependency rule is important to use without modification
of generated files.

## Collaborate with SOAP

You can use 
[Sagittarius SOAP lite](https://github.com/ktakashi/sagittarius-soap)
or manually send it.

Following is an example how to collaborate with SOAP lite library.

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
                                          (soap:body (unmarshal-xml o)))))))
```

## TODO

* document. 
* more test.
* cover whole XSD specification.
* better dependency resolution.

Since I didn't write API document yet so for users who want't to try this,
please see `test.scm`, you will find how to use.

Your pull request is always welcome :)