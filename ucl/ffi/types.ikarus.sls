#!r6rs

(library (ucl ffi types)
  (export symbol->type sizeof)
  (import (rnrs) (ikarus foreign))

  (define (sizeof type)
    (case type
      ((float)         4)
      ((double)        8)
      ((schar uchar)   1)
      ((sshort ushort) 2)
      ((sint uint)     4)
      ((slong ulong)   (pointer-size))
      ((pointer)       (pointer-size))
      (else            (error 'sizeof "unsupported type" type))))

  (define (symbol->type sym)
    (case sym
      ((float)   'float)
      ((double)  'double)
      ((schar)   'signed-char)
      ((uchar)   'unsigned-char)
      ((sshort)  'signed-short)
      ((ushort)  'unsigned-short)
      ((sint)    'signed-int)
      ((uint)    'unsigned-int)
      ((slong)   'signed-long)
      ((ulong)   'unsigned-long)
      ((void)    'void)
      ((pointer) 'pointer)
      ((string)  'pointer)
      (else      (error 'symbol->type "unsupported type" sym))))
)
