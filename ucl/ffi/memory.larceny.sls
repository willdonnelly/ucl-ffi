#!r6rs

(library (ucl ffi memory)
  (export malloc free null null-ptr? pointer-set! pointer-get
          pointer->integer integer->pointer pointer?)
  (import (rnrs)
          (primitives %poke8 %poke8u %peek8 %peek8u %poke16 %poke16u %peek16
                      %peek16u %poke32 %poke32u %peek32 %peek32u %poke-long
                      %peek-long %poke-ulong %peek-ulong %poke-pointer void*?
                      %peek-pointer void*-float-set! void*-float-ref void*-rt
                      void*-double-set! void*-double-ref ffi/foreign-procedure
                      *ffi-callout-abi*))

  (define (pointer->integer ptr)
    (define acc (record-accessor void*-rt 'ptr))
    (if ptr (acc ptr) 0))
  (define integer->pointer (record-constructor void*-rt))
  (define (pointer? x) (or (void*? x) (equal? null x)))

  (define (malloc size)
    (let ((internal-malloc (ffi/foreign-procedure *ffi-callout-abi* "malloc"
                                                  '(unsigned32) 'unsigned32))
          (wrap (record-constructor void*-rt)))
      (wrap (internal-malloc size))))

  (define (free pointer)
    (let ((internal-free (ffi/foreign-procedure *ffi-callout-abi* "free"
                                                '(unsigned32) 'void))
          (unwrap (record-accessor void*-rt 'ptr)))
      (internal-free (unwrap pointer))))

  (define null #f)

  (define (null-ptr? ptr)
    (not ptr))

  (define (pointer-set! pointer offset type value)
    (let* ((unwrap (record-accessor void*-rt 'ptr))
           (ptr (+ offset (unwrap pointer))))
      (case type
        ((float)   (void*-float-set!  pointer offset value))
        ((double)  (void*-double-set! pointer offset value))
        ((uchar)   (%poke8u           ptr value))
        ((schar)   (%poke8            ptr value))
        ((ushort)  (%poke16u          ptr value))
        ((sshort)  (%poke16           ptr value))
        ((uint)    (%poke32u          ptr value))
        ((sint)    (%poke32           ptr value))
        ((ulong)   (%poke-ulong       ptr value))
        ((slong)   (%poke-long        ptr value))
        ((pointer) (%poke-pointer     ptr (unwrap value)))
        (else      (error 'pointer-set! "unsupported type" type)))))

  (define (pointer-get pointer offset type)
    (let* ((unwrap (record-accessor void*-rt 'ptr))
           (rewrap (record-constructor void*-rt))
           (ptr (+ offset (unwrap pointer))))
      (case type
        ((float)   (void*-float-ref  pointer offset))
        ((double)  (void*-double-ref pointer offset))
        ((uchar)   (%peek8u          ptr))
        ((schar)   (%peek8           ptr))
        ((ushort)  (%peek16u         ptr))
        ((sshort)  (%peek16          ptr))
        ((uint)    (%peek32u         ptr))
        ((sint)    (%peek32          ptr))
        ((ulong)   (%peek-ulong      ptr))
        ((slong)   (%peek-long       ptr))
        ((pointer) (rewrap (%peek-pointer ptr)))
        (else      (error 'pointer-get "unsupported type" type)))))
)
