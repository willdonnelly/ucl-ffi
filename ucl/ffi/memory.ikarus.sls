#!r6rs

(library (ucl ffi memory)
  (export malloc free null null-ptr? pointer-set! pointer-get
          pointer->integer integer->pointer pointer?)
  (import (rnrs) (ikarus foreign))
  ;; MALLOC and FREE come directly from Ikarus

  (define null (integer->pointer 0))

  (define (null-ptr? ptr)
    (zero? (pointer->integer ptr)))

  (define (pointer-set! pointer offset type value)
    (case type
      ((float)         (pointer-set-c-float!   pointer offset value))
      ((double)        (pointer-set-c-double!  pointer offset value))
      ((schar uchar)   (pointer-set-c-char!    pointer offset value))
      ((sshort ushort) (pointer-set-c-short!   pointer offset value))
      ((sint uint)     (pointer-set-c-int!     pointer offset value))
      ((slong ulong)   (pointer-set-c-long!    pointer offset value))
      ((pointer)       (pointer-set-c-pointer! pointer offset value))
      (else            (error 'pointer-set! "unsupported type" type))))

  (define (pointer-get pointer offset type)
    (case type
      ((float)   (pointer-ref-c-float          pointer offset))
      ((double)  (pointer-ref-c-double         pointer offset))
      ((schar)   (pointer-ref-c-signed-char    pointer offset))
      ((uchar)   (pointer-ref-c-unsigned-char  pointer offset))
      ((sshort)  (pointer-ref-c-signed-short   pointer offset))
      ((ushort)  (pointer-ref-c-unsigned-short pointer offset))
      ((sint)    (pointer-ref-c-signed-int     pointer offset))
      ((uint)    (pointer-ref-c-unsigned-int   pointer offset))
      ((slong)   (pointer-ref-c-signed-long    pointer offset))
      ((ulong)   (pointer-ref-c-unsigned-long  pointer offset))
      ((pointer) (pointer-ref-c-pointer        pointer offset))
      (else      (error 'pointer-get "unsupported type" type))))
)
