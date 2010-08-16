#!r6rs

(library (ucl ffi functions)
  (export load-library raw-ptr-function raw-get-function make-callback)
  (import (rnrs) (mzlib foreign) (scheme mpair) (ucl ffi types)
          (ucl ffi omghax-mzscheme))
  (unsafe!)

  (define (load-library . paths)
    (define (load-lib path) (guard (e (else #f)) (ffi-lib path)))
    (exists load-lib paths))

  (define (raw-ptr-function ptr args ret)
    (ffi-call ptr (mlist->list (map symbol->type args)) (symbol->type ret)))

  (define (raw-get-function lib name args ret)
    (get-ffi-obj name lib
      (_cprocedure
        (mlist->list (map symbol->type args))
        (symbol->type ret))))

  (define (make-callback args ret fn)
    (ffi-callback fn (mlist->list (map symbol->type args)) (symbol->type ret)))
)
