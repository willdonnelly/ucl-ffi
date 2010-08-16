#!r6rs

;; I don't like the way Ypsilon handles memory management. It seems
;;   to be overly afraid of unchecked access to memory, so it wants
;;   all pointers to be converted into magical bytevectors with the
;;   MAKE-BYTEVECTOR-MAPPING function.
;; Since that's different from the way all the other schemes handle
;;   their memory access, I worked around it. I'm using malloc/free
;;   directly from libc, and making bytevector mappings onto little
;;   regions when I need to read or write them.
;; For now this means the POINTER->INTEGER and INTEGER->POINTER are
;;   no-ops, but later on I plan to wrap the pointers in records so
;;   they'll be disjoint from integers.

(library (ucl ffi memory)
  (export malloc free null pointer-set! pointer-get
          pointer->integer integer->pointer pointer?)
  (import (rnrs) (ypsilon ffi) (ucl ffi functions) (ucl ffi types)
          (ucl ffi pointer))

  (define libc        (load-library "libc.so" "libc.so.6"))
  (define libc-malloc (raw-get-function libc "malloc" '(uint) 'ulong))
  (define libc-free   (raw-get-function libc "free"   '(ulong) 'void))

  (define (malloc s) (ptr-new (libc-malloc s)))
  (define (free p) (libc-free (ptr-get p)))
  (define null (ptr-new 0))

  (define pointer->integer ptr-get)
  (define integer->pointer ptr-new)
  (define pointer? ptr?)

  (define (pointer-set! pointer offset type value)
    (let* ((ptr (+ (pointer->integer pointer) offset))
           (win (make-bytevector-mapping ptr (sizeof type))))
      (case type
        ((float)         (bytevector-c-float-set!  win 0 value))
        ((double)        (bytevector-c-double-set! win 0 value))
        ((uchar schar)   (bytevector-c-int8-set!   win 0 value))
        ((ushort sshort) (bytevector-c-short-set!  win 0 value))
        ((uint sint)     (bytevector-c-int-set!    win 0 value))
        ((ulong slong)   (bytevector-c-long-set!   win 0 value))
        ((pointer)       (bytevector-c-long-set!   win 0 (ptr-get value)))
        (else            (error 'pointer-set! "unsupported type" type)))))

  (define (pointer-get pointer offset type)
    (let* ((ptr (+ (pointer->integer pointer) offset))
           (win (make-bytevector-mapping ptr (sizeof type))))
      (case type
        ((float)   (bytevector-c-float-ref           win 0))
        ((double)  (bytevector-c-double-ref          win 0))
        ((uchar)   (bytevector-c-uint8-ref           win 0))
        ((schar)   (bytevector-c-int8-ref            win 0))
        ((ushort)  (bytevector-c-unsigned-short-ref  win 0))
        ((sshort)  (bytevector-c-short-ref           win 0))
        ((uint)    (bytevector-c-unsigned-int-ref    win 0))
        ((sint)    (bytevector-c-int-ref             win 0))
        ((ulong)   (bytevector-c-unsigned-long-ref   win 0))
        ((slong)   (bytevector-c-long-ref            win 0))
        ((pointer) (ptr-new (bytevector-c-unsigned-long-ref win 0)))
        (else      (error 'pointer-get "unsupported type" type)))))
)
