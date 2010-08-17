#!r6rs

(library (ucl ffi)
  (export load-library get-function ptr-function make-callback
          malloc free null null-ptr? pointer-set! pointer-get
          pointer->integer integer->pointer pointer?
          string-clone string-read sizeof)
  (import (rnrs) (ucl ffi types) (ucl ffi memory) (ucl ffi functions))

  ;; list the integers in the half-open range [x, y)
  (define (range x y) (do ((i (- y 1) (- i 1)) (a '() (cons i a))) ((< i x) a)))

  (define (string-clone str)
    (let* ((vec (string->bytevector str (native-transcoder)))
           (chs (bytevector->u8-list vec))
           (len (length chs))
           (mem (malloc (+ len 1))))
      (define (set-char ix ch)
        (pointer-set! mem ix 'uchar ch))
      (for-each set-char (range 0 len) chs)
      (pointer-set! mem len 'uchar 0)
      mem))

  (define (string-read ptr)
    (define (get-char off) (pointer-get ptr off 'uchar))
    (define (strlen off) (if (equal? (get-char off) 0) off (strlen (+ 1 off))))
    (let* ((len (strlen 0))
           (ixs (range 0 len))
           (chs (map get-char ixs))
           (vec (u8-list->bytevector chs)))
      (bytevector->string vec (native-transcoder))))


  ;; CLEANUP - Do cleanup after evaluating an expression
  (define-syntax cleanup (syntax-rules () ((_ c e) (let ((r e)) c r))))

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
