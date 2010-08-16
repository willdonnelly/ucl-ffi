#!r6rs

(library (ucl ffi functions)
  (export load-library raw-get-function raw-ptr-function make-callback)
  (import (rnrs) (ucl ffi types)
          (primitives ffi/dlopen ffi/dlsym foreign-procedure-pointer
                      foreign-wrap-procedure trampoline->pointer void*-rt))

  (define (load-library . paths)
    (exists ffi/dlopen paths))

  (define (raw-ptr-function ptr args ret)
    (foreign-procedure-pointer
      ((record-accessor void*-rt 'ptr) ptr)
      (map symbol->type args)
      (symbol->type ret)))

  (define (raw-get-function lib name args ret)
    (foreign-procedure-pointer
      (ffi/dlsym lib name)
      (map symbol->type args)
      (symbol->type ret)))


  (define (make-callback args ret fn)
    ((record-constructor void*-rt)
     (trampoline->pointer
       ;; There's supposedly some issues here, the trampoline could be GC'd
       ;; because we don't have a reference to it anymore. If I can find a
       ;; test case for the issue, it may be fixable with FFI/GCPROTECT
       (foreign-wrap-procedure fn (map symbol->type args) (symbol->type ret))
       "callback-trampoline")))
)
