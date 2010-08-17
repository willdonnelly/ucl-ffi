#!r6rs

(library (ucl ffi memory)
  (export (rename (raw-malloc malloc)) free
          null null-ptr? pointer-set! pointer-get
          pointer->integer integer->pointer pointer?)
  (import (rnrs) (mzlib foreign) (ucl ffi types))
  (unsafe!)

  (define my-cast
    (let ((mem (malloc 16)))
      (lambda (val from to)
        (pointer-set! mem 0 from val)
        (pointer-get mem 0 to))))

  (define (pointer->integer ptr)
    (my-cast ptr 'pointer 'ulong))
  (define (integer->pointer int)
    (my-cast int 'ulong 'pointer))
  (define pointer? cpointer?)

  (define (raw-malloc size) (malloc size 'raw))
  ;; FREE is re-exported from MzScheme unaltered

  (define null #f)

  (define (null-ptr? ptr)
    (not ptr))

  (define (pointer-set! pointer offset type value)
    (ptr-set! pointer (symbol->type type) 'abs offset value))
  (define (pointer-get pointer offset type)
    (ptr-ref pointer (symbol->type type) 'abs offset))
)
