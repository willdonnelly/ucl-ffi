#!r6rs

(library (ucl ffi types)
  (export symbol->type sizeof)
  (import (rnrs) (ypsilon ffi))

  (define (sizeof type)
    (case type
      ((float)         4)
      ((double)        8)
      ((schar uchar)   1)
      ((sshort ushort) sizeof:short)
      ((sint uint)     sizeof:int)
      ((slong ulong)   sizeof:long)
      ((pointer)       sizeof:void*)
      (else            (error 'sizeof "unsupported type" type))))

  (define (symbol->type sym)
    (case sym
      ((float)   'float)
      ((double)  'double)
      ((schar)   'int8_t)
      ((uchar)   'uint8_t)
      ((sshort)  'short)
      ((ushort)  'unsigned-short)
      ((sint)    'int)
      ((uint)    'unsigned-int)
      ((slong)   'long)
      ((ulong)   'unsigned-long)
      ((void)    'void)
      ((pointer) 'void*)
      ((string)  'void*)
      (else      (error 'symbol->type "unsupported type" sym))))
)
