#!r6rs

(library (ucl ffi functions)
  (export load-library raw-ptr-function raw-get-function make-callback)
  (import (rnrs) (rnrs eval) (ypsilon ffi) (ucl ffi types) (ucl ffi pointer))

  (define (load-library . paths)
    (define (load-lib path)
      (guard (e (else #f))
        (cons (load-shared-object path) path)))
    (exists load-lib paths))

  (define (raw-raw-ptr-function ptr args ret)
    (make-cdecl-callout (symbol->type ret) (map symbol->type args) ptr))

  (define (raw-raw-get-function lib name args ret)
    ;; It's ugly, but it works dammit!
    (eval `(c-function ,(car lib) ,(cdr lib)
                       ,(symbol->type ret)
                       __stdcall
                       ,(string->symbol name)
                       ,(map symbol->type args))
           (environment '(rnrs) '(ypsilon ffi) '(ucl ffi types))))

  (define (function-convert fn args ret)
    (define (arg-convert t v) (case t ((pointer string) (ptr-get v)) (else v)))
    (define (ret-convert t v) (case t ((pointer string) (ptr-new v)) (else v)))
    (lambda vals (ret-convert ret (apply fn (map arg-convert args vals)))))

  (define (raw-get-function lib name args ret)
    (function-convert (raw-raw-get-function lib name args ret) args ret))
  (define (raw-ptr-function ptr args ret)
    (function-convert (raw-raw-ptr-function (ptr-get ptr) args ret) args ret))

  (define (make-callback args ret fn)
    (define (arg-convert t v) (case t ((pointer string) (ptr-new v)) (else v)))
    (define (ret-convert t v) (case t ((pointer string) (ptr-get v)) (else v)))
    (ptr-new (make-cdecl-callback (symbol->type ret) (map symbol->type args)
      (lambda vals (ret-convert ret (apply fn (map arg-convert args vals)))))))
)
