#!r6rs

(library (ucl ffi functions)
  (export load-library raw-get-function raw-ptr-function make-callback)
  (import (rnrs) (ikarus foreign) (ucl ffi types))

  (define (load-library . paths) (exists dlopen paths))

  (define (raw-ptr-function ptr args ret)
    ((make-c-callout (symbol->type ret) (map symbol->type args)) ptr))

  (define (raw-get-function lib name args ret)
    (raw-ptr-function (dlsym lib name) args ret))

  (define (make-callback args ret fn)
    ((make-c-callback (symbol->type ret) (map symbol->type args)) fn))
)
