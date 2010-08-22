#!r6rs

(library (ucl ffi)
  (export load-library get-function ptr-function make-callback
          malloc free null null-ptr? pointer-set! pointer-get
          pointer->integer integer->pointer pointer?
          string-clone
          bytevector-read string-read
          sizeof)
  (import (rnrs)
    (ucl prelude)
    (ucl ffi types)
    (ucl ffi memory)
    (ucl ffi functions))

  (define (string-clone str)
    (let* ((vec (string->bytevector str (native-transcoder)))
           (chs (bytevector->u8-list vec))
           (len (length chs))
           (mem (malloc (+ len 1))))
      (define (set-char ix ch)
        (pointer-set! 'uchar ix mem ch))
      (for-each set-char (range 0 len) chs)
      (pointer-set! 'uchar len mem 0)
      mem))

  (define (bytevector-read ptr len)
    (define (get-char off) (pointer-get 'uchar off ptr))
    (u8-list->bytevector (map get-char (range 0 len))))

  (define (string-read ptr)
    (define (get-char off) (pointer-get 'uchar off ptr))
    (define (strlen off) (if (equal? (get-char off) 0) off (strlen (+ 1 off))))
    (bytevector->string (bytevector-read ptr (strlen 0)) (native-transcoder)))

  ;; FUNCTION-CONVERT - Do some type conversion around the given function
  ;;                    essentially just makes strings into pointers and
  ;;                    vice versa, and then handles freeing them later
  (define (function-convert fn args ret)
    (define (arg-convert t v) (if (equal? t 'string) (string-clone v) v))
    (define (arg-unconvert t v) (if (equal? t 'string) (free v) v))
    (define (ret-convert t v)
      (if (equal? t 'string) (cleanup (free v) (string-read v)) v))
    (lambda vals
      (define cvals (map arg-convert args vals))
      (ret-convert ret
        (cleanup (for-each arg-unconvert args cvals)
          (apply fn cvals)))))

  (define (get-function lib name args ret)
    (function-convert (raw-get-function lib name args ret) args ret))
  (define (ptr-function ptr args ret)
    (function-convert (raw-ptr-function ptr args ret) args ret))
)
