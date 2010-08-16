#!r6rs

(library (ucl ffi types)
  (export symbol->type sizeof)
  (import (rnrs) (primitives sizeof:long sizeof:pointer))

  (define (sizeof type)
    (case type
      ((float)         4)
      ((double)        8)
      ((schar uchar)   1)
      ((sshort ushort) 2)
      ((sint uint)     4)
      ((slong ulong)   sizeof:long)
      ((pointer)       sizeof:pointer)
      (else            (error 'sizeof "unsupported type" type))))

  (define (symbol->type sym)
    (case sym
      ((float)   'float)
      ((double)  'double)
      ((schar)   'char)
      ((uchar)   'uchar)
      ((sshort)  'short)
      ((ushort)  'ushort)
      ((sint)    'int)
      ((uint)    'uint)
      ((slong)   'long)
      ((ulong)   'ulong)
      ((void)    'void)
      ((pointer) '(maybe void*)) ;; Maybe means that #f maps to NULL
      ((string)  'void*)
      (else      (error 'symbol->type "unsupported type" sym))))
)
