(import (rnrs) (prefix (ucl ffi) ffi-))

(define libSDL (ffi-load-library "libSDL.so"))

(define sdl-init (ffi-get-function libSDL "SDL_Init" '(uint) 'sint))
(define SDL-INIT-EVERYTHING #x0000FFFF)

(define sdl-set-video-mode
  (ffi-get-function libSDL "SDL_SetVideoMode" '(sint sint sint uint) 'pointer))

(define sdl-fillrect
  (ffi-get-function libSDL "SDL_FillRect" '(pointer pointer uint) 'sint))

(define sdl-flip
  (ffi-get-function libSDL "SDL_Flip" '(pointer) 'sint))

(define sdl-delay
  (ffi-get-function libSDL "SDL_Delay" '(uint) 'void))

(define sdl-quit (ffi-get-function libSDL "SDL_Quit" '() 'void))

(sdl-init SDL-INIT-EVERYTHING)
(define surface (sdl-set-video-mode 640 480 32 0))

(define rect (ffi-malloc 8))
(ffi-pointer-set! rect 0 'sshort 10)
(ffi-pointer-set! rect 2 'sshort 10)
(ffi-pointer-set! rect 4 'ushort 100)
(ffi-pointer-set! rect 6 'ushort 100)

(sdl-fillrect surface rect #xFF00FF00)
(ffi-free rect)
(sdl-flip surface)
(sdl-delay 1000)
(sdl-quit)

(define mem (ffi-malloc 16))

(define (roundtrip cmp type value)
  (ffi-pointer-set! mem 0 type value)
  (let ((val (ffi-pointer-get mem 0 type)))
    (if (not (cmp val value))
        (error 'roundtrip "value didn't survive" (list type value val))
        "Success!")))

(define (similar x y) (< (abs (- x y)) 0.01))

(roundtrip similar 'float  3.0)
(roundtrip similar 'double 4.0)

(roundtrip equal? 'schar -100)
(roundtrip equal? 'uchar  200)

(roundtrip equal? 'sshort -30000)
(roundtrip equal? 'ushort  60000)

(roundtrip equal? 'sint -2000000000)
(roundtrip equal? 'uint  4000000000)

(roundtrip equal? 'slong -2000000000)
(roundtrip equal? 'ulong  4000000000)

(display "Success!")
(newline)

(define (show-size type)
  (display "sizeof '")
  (display type)
  (display " = ")
  (display (ffi-sizeof type))
  (newline))

(show-size 'float)
(show-size 'double)
(show-size 'schar)
(show-size 'sshort)
(show-size 'sint)
(show-size 'slong)
(show-size 'pointer)

(define libTest (ffi-load-library "/home/will/libTest.so"))

(define test-cb
  (ffi-get-function libTest "Callback_Int_2_Int_Int" '(pointer sint sint) 'sint))

(define cb (ffi-make-callback '(sint sint) 'sint *))

(display (test-cb cb 3 3))
(newline)

(define ptr (ffi-malloc 8))
(ffi-pointer-set! ptr 0 'double 1.23)
(set! ptr (ffi-integer->pointer (ffi-pointer->integer ptr)))
(display (ffi-pointer-get ptr 0 'double))
(newline)

(display (ffi-pointer? (ffi-malloc 1)))
(newline)
(display (ffi-pointer? ffi-null))
(newline)

(define add-ptr ((ffi-get-function libTest "Callout_Add" '() 'pointer)))
(define ffi-add (ffi-ptr-function add-ptr '(sint sint) 'sint))
(display (ffi-add 2 2))
(newline)

(define str-ptr (ffi-string-clone "X"))
(display (ffi-pointer-get str-ptr 0 'uchar))
(newline)
(display (ffi-pointer-get str-ptr 1 'uchar))
(newline)

(define print-string (ffi-get-function libTest "PrintString" '(string) 'void))
(print-string "foobar\n")
