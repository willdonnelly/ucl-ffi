#!r6rs

(library (ucl ffi types)
  (export symbol->type sizeof)
  (import (rnrs) (mzlib foreign))

  (define (sizeof type)
    (ctype-sizeof (symbol->type type)))

  (define (symbol->type sym)
    (case sym
      ((float)   _float)
      ((double)  _double)
      ((schar)   _sbyte)
      ((uchar)   _ubyte)
      ((sshort)  _sshort)
      ((ushort)  _ushort)
      ((sint)    _sint)
      ((uint)    _uint)
      ((slong)   _slong)
      ((ulong)   _ulong)
      ((void)    _void)
      ((pointer) _pointer)
      ((string)  _pointer)
      (else      (error 'symbol->type "unsupported type" sym))))
)
