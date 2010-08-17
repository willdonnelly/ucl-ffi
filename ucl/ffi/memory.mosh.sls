#!r6rs

(library (ucl ffi memory)
  (export malloc free null null-ptr? pointer-set! pointer-get
          pointer->integer integer->pointer pointer?)
  (import (rnrs) (mosh ffi))

  ;; MALLOC and FREE come directly from Mosh

  (define null (integer->pointer 0))

  (define (null-ptr? ptr)
    (zero? (pointer->integer ptr)))

  (define set-signed-long!
    (if (= size-of-long 4) pointer-set-c-int32! pointer-set-c-int64!))
  (define set-unsigned-long!
    (if (= size-of-long 4) pointer-set-c-uint32! pointer-set-c-uint64!))

  (define (pointer-set! type offset pointer value)
    (case type
      ((float)   (pointer-set-c-float!  pointer offset value))
      ((double)  (pointer-set-c-double! pointer offset value))
      ((schar)   (pointer-set-c-int8!   pointer offset value))
      ((uchar)   (pointer-set-c-uint8!  pointer offset value))
      ((sshort)  (pointer-set-c-int16!  pointer offset value))
      ((ushort)  (pointer-set-c-uint16! pointer offset value))
      ((sint)    (pointer-set-c-int32!  pointer offset value))
      ((uint)    (pointer-set-c-uint32! pointer offset value))
      ((slong)   (set-signed-long!      pointer offset value))
      ((ulong)   (set-unsigned-long!    pointer offset value))
      ((pointer) (pointer-set-c-long!   pointer offset (pointer->integer value)))
      (else      (error 'pointer-set! "unsupported type" type))))

  (define (pointer-get type offset pointer)
    (case type
      ((float)   (pointer-ref-c-float         pointer offset))
      ((double)  (pointer-ref-c-double        pointer offset))
      ((schar)   (pointer-ref-c-int8          pointer offset))
      ((uchar)   (pointer-ref-c-uint8         pointer offset))
      ((sshort)  (pointer-ref-c-int16         pointer offset))
      ((ushort)  (pointer-ref-c-uint16        pointer offset))
      ((sint)    (pointer-ref-c-int32         pointer offset))
      ((uint)    (pointer-ref-c-uint32        pointer offset))
      ((slong)   (pointer-ref-c-signed-long   pointer offset))
      ((ulong)   (pointer-ref-c-unsigned-long pointer offset))
      ((pointer) (pointer-ref-c-pointer       pointer offset))
      (else      (error 'pointer-get "unsupported type" type))))
)
