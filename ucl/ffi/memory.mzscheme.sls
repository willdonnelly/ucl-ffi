#!r6rs

(library (ucl ffi memory)
  (export (rename (raw-malloc malloc)) free null pointer-set! pointer-get
          pointer->integer integer->pointer pointer?)
  (import (rnrs) (mzlib foreign) (ucl ffi types))
  (unsafe!)

  (define (my-cast v from to)
    (let ((p (malloc (sizeof from))))
      (ptr-set! p from v)
      (let ((r (ptr-ref p to)))
        (free p)
        r)))

  (define (pointer->integer ptr)
    (my-cast ptr _pointer _ulong))
  (define (integer->pointer int)
    (my-cast int _ulong _pointer))
  (define pointer? cpointer?)

  (define (raw-malloc size) (malloc size 'raw))
  ;; FREE is re-exported from MzScheme unaltered

  (define null #f)

  (define (pointer-set! pointer offset type value)
    (ptr-set! pointer (symbol->type type) 'abs offset value))
  (define (pointer-get pointer offset type)
    (ptr-ref pointer (symbol->type type) 'abs offset))
)
