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
        (pointer-set! from 0 mem val)
        (pointer-get to 0 mem))))

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

  (define (pointer-set! type offset pointer value)
    (ptr-set! pointer (symbol->type type) 'abs offset value))
  (define (pointer-get type offset pointer)
    (ptr-ref pointer (symbol->type type) 'abs offset))
)
