#!r6rs

(library (ucl ffi pointer)
  (export ptr-new ptr-get ptr?)
  (import (rnrs))

  (define-record-type (pointer ptr-new ptr?)
    (fields (immutable ptr ptr-get))))
