
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (30 / 6 - 1994) (23 : 7 : 35 67))
(defq *package* ASSEMBLY)

(defun tkoe
 (x)
 (switch x
  (fib 10)
  (print 'toka)
  (print 'kola)))

(defmacro switch
 (x . y)
 (setq y
  (map y
   (function
    (lambda
     (z)
     (list 'setq (gensym) (list 'function z))))))
 (` progn ,@ y
  (AX-reg , x)
  (mach-code 232 ,
   (* 2 (length y))
   0 ,@
   (let
    ((z nil) (v))
    (while y
     (setq v (addr-of (cadr (pop y))))
     (push (low-byte v) z)
     (push (high-byte v) z))
    (reverse z))
   91 1 195 1 195 46 139 7 144)
  (eval (at-addr (AX-reg)))))

(defun scale
 (x o n)
 (AX-reg x)
 (BX-reg n)
 (CX-reg o)
 (assembly (IMUL BX) (IDIV CX))
 (AX-reg))

(defun fib2
 (x)
 (AX-reg x)
 (assembly
  (CALL 272)
  (JMP 320)
  272
  (CMP AX , 2)
  (JC 304)
  (push AX)
  (sub ax , 1)
  (call 272)
  (MOV BX , AX)
  (POP AX)
  (PUSH BX)
  (sub ax , 2)
  (call 272)
  (pop BX)
  (add AX , BX)
  (RET)
  304
  (RET)
  320)
 (AX-reg))

(defmacro assembly x
 (compile-all 'assembly-2)
 (cons 'mach-code (assembly-2 x)))

(defun test
 (x)
 (AX-reg x)
 (assembly
  (MOV CX , 0)
  (MOV BX , 2)
  (JMP 288)
  288
  (ADD CX , BX)
  (DEC AX)
  (JNZ 288))
 (CX-reg))

(defun assembly-2
 (x)
 (hex nil)
 (out (create 'TO.DEB))
 (flet
  ((pr-hex
    (x)
    (flet
     ((pr-byte
       (b)
       (flet
        ((pr-nyb
          (c)
          (printc
           (if (> c 9) (+ 55 c) (+ 48 c)))))
        (pr-nyb (/ b 16))
        (pr-nyb (remainder b 16)))))
     (pr-byte (high-byte x))
     (pr-byte (low-byte x)))))
  (print 'A)
  (cr)
  (mapc x
   (function
    (lambda
     (l)
     (if
      (numberp l)
      (progn (cr) (print 'A) (pr-hex l))
      (mapc l
       (function
        (lambda
         (s)
         (if (numberp s) (pr-hex s) (print s))
         (sp)))))
     (cr))))
  (repeat-times 10 (print 'NOP) (cr))
  (cr)
  (print 'RCX)
  (cr)
  (print 100)
  (cr)
  (print 'NTMP.ASS)
  (cr)
  (print 'w)
  (cr)
  (print 'q)
  (cr))
 (close (out))
 (out 0)
 (dos-eval '(debug < TO.DEB > NUL))
 (let
  ((s) (c) (n 0))
  (in (open 'TMP.ASS))
  (while
   (< n 10)
   (setq c (readc-bin))
   (if
    (= c 144)
    (setq n (1+ n))
    (setq n 0))
   (push c s))
  (close (in))
  (in 0)
  (reverse (nthcdr 9 s))))

(defq ASSEMBLY (tkoe switch scale fib2 assembly test assembly-2 ASSEMBLY))
