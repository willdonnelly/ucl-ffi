#!r6rs

(library (ucl ffi functions)
  (export load-library raw-get-function raw-ptr-function make-callback)
  (import (rnrs) (mosh ffi) (ucl ffi types))

  (define (load-library . paths)
    (define (load-lib path) (guard (e (else #f)) (open-shared-library path)))
    (exists load-lib paths))

  (define (raw-get-function lib name args ret)
    (make-c-function lib
      (symbol->type ret)
      (string->symbol name)
      (map symbol->type args)))

  (define (raw-ptr-function ptr args ret)
    (pointer->c-function ptr
      (symbol->type ret) 'unnamed (map symbol->type args)))

  (define (make-callback args ret fn)
    (make-c-callback (symbol->type ret) (map symbol->type args) fn))
)
