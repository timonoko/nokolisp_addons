
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (18 / 11 - 1989) (20 : 25 : 42 10))
(defq *package* ANYDISP)

(defq ANYDISP
 (ANYDISP anydisp:boxi anydisp:clear-area anydisp:clear-mem anydisp:display-mode
  anydisp:ega-map-select anydisp:ega-plane anydisp:ega-write-mode anydisp:for-all-planes
  anydisp:load-screen anydisp:move-mem-down anydisp:move-mem-up anydisp:move-mode
  anydisp:read-mem-block anydisp:save-screen anydisp:scroll-down anydisp:scroll-l/r
  anydisp:scroll-left anydisp:scroll-right anydisp:scroll-u/d anydisp:scroll-up
  anydisp:scroll-window-down anydisp:scroll-window-l/r anydisp:scroll-window-left
  anydisp:scroll-window-right anydisp:scroll-window-u/d anydisp:scroll-window-up
  anydisp:test anydisp:test2 anydisp:test3 anydisp:write-mem-block paska))

(defun anydisp:boxi
 (x y l k)
 (point x y)
 (draw (+ x l) y)
 (draw (+ x l) (+ y k))
 (draw x (+ y k))
 (draw x y))

(defun anydisp:clear-area
 (x y w h col)
 (comment fill area of origin
  (x y)
  and dimensions
  (w h)
  with zeros
  (or bytes of col)
  (or ega-colors))
 (unless col (setq col *BACKGROUND-1*))
 (unless (or *EGA-MODE* (zerop col)) (setq col 255))
 (setq x (/ x 8))
 (setq w (/ w 8))
 (for
  (y2 y (+ y h))
  (case *DISPLAY-MODE*
   (128
    (anydisp:clear-mem
     (+ -20480 (* 1024 (remainder y2 4)))
     (+ (* 90 (/ y2 4)) x)
     w col))
   (6
    (anydisp:clear-mem
     (+ -18432 (* 512 (remainder y2 2)))
     (+ (* 80 (/ y2 2)) x)
     w col))
   ((18 16 88)
    (anydisp:ega-plane col)
    (anydisp:clear-mem -24576 (+ x (* y2 (/ *X-SIZE* 8))) w 255)
    (anydisp:ega-plane (logxor 15 col))
    (anydisp:clear-mem -24576 (+ x (* y2 (/ *X-SIZE* 8))) w 0))))
 (anydisp:ega-plane 15))

(defun anydisp:clear-mem
 (seg adr len val)
 (AX-reg (or val 0))
 (ES-reg seg)
 (DI-reg adr)
 (CX-reg len)
 (mach-code 252 243 170))

(defun anydisp:display-mode
 (x)
 (unless *BACKGROUND-1* (setq *BACKGROUND-1* 0))
 (unless *COLOR-1* (setq *COLOR-1* 1))
 (setq *EGA-MODE* nil)
 (case x
  (6 (setq *X-SIZE* 640) (setq *Y-SIZE* 200))
  (128 (setq *X-SIZE* 720) (setq *Y-SIZE* 400))
  (88 (setq *X-SIZE* 800) (setq *Y-SIZE* 600) (setq *EGA-MODE* t))
  (18 (setq *X-SIZE* 640) (setq *Y-SIZE* 480) (setq *EGA-MODE* t))
  (16 (setq *X-SIZE* 640) (setq *Y-SIZE* 350) (setq *EGA-MODE* t)))
 (setq *DISPLAY-MODE* x)
 (point 0 0)
 (graph-borders *X-SIZE* *Y-SIZE*)
 (display-mode x)
 (unless (zerop *BACKGROUND-1*) (anydisp:clear-area 0 0 *X-SIZE* *Y-SIZE* *BACKGROUND-1*))
 (color *COLOR-1*))

(defun anydisp:ega-map-select (x) (output-byte 974 4) (output-byte 975 x))

(defun anydisp:ega-plane (x) (output-byte 964 2) (output-byte 965 x))

(defun anydisp:ega-write-mode (x) (output-byte 974 5) (output-byte 975 x))

(defmacro anydisp:for-all-planes x
 (let
  ((y (gensym)) (y2 (gensym)))
  (` let
   ((, y 1) (, y2 0))
   (repeat-times 4
    (anydisp:ega-map-select , y2)
    (anydisp:ega-plane , y)
    ,@ x
    (setq , y (* 2 , y))
    (setq , y2 (1+ , y2)))
   (anydisp:ega-plane 15))))

(defun anydisp:load-screen
 (file)
 (case *DISPLAY-MODE*
  (128 (anydisp:read-mem-block file -20480 0 -1))
  (6 (anydisp:read-mem-block file -18432 0 16384))
  ((88 18 16)
   (setq file (open file))
   (anydisp:for-all-planes
    (BX-reg file)
    (DS-reg -24576)
    (DX-reg 0)
    (CX-reg (* (/ *X-SIZE* 8) (1+ *Y-SIZE*)))
    (msdos 63))
   (close file))))

(defun anydisp:move-mem-down
 (seg DI SI len)
 (ES-reg seg)
 (DS-reg seg)
 (DI-reg DI)
 (SI-reg SI)
 (CX-reg len)
 (mach-code 252 243 164))

(defun anydisp:move-mem-up
 (seg DI SI len)
 (ES-reg seg)
 (DS-reg seg)
 (DI-reg (+ DI (1- len)))
 (SI-reg (+ SI (1- len)))
 (CX-reg len)
 (mach-code 253 243 164))

(defun anydisp:move-mode
 (alku)
 (if *EGA-MODE*
  (if alku
   (progn (anydisp:ega-write-mode 1) (anydisp:ega-map-select 255) (anydisp:ega-plane 15))
   (anydisp:ega-write-mode 0))))

(defun anydisp:read-mem-block
 (file seg adr len)
 (setq file (open file))
 (BX-reg file)
 (DS-reg seg)
 (DX-reg adr)
 (CX-reg len)
 (msdos 63)
 (close file))

(defun anydisp:save-screen
 (file)
 (case *DISPLAY-MODE*
  (128 (anydisp:write-mem-block file -20480 0 -1))
  (6 (anydisp:write-mem-block file -18432 0 16384))
  ((88 18 16)
   (setq file (create file))
   (anydisp:for-all-planes
    (BX-reg file)
    (DS-reg -24576)
    (DX-reg 0)
    (CX-reg (* (/ *X-SIZE* 8) (1+ *Y-SIZE*)))
    (msdos 64))
   (close file))))

(defun anydisp:scroll-down
 (lines)
 (comment scroll screen down number of lines)
 (anydisp:scroll-u/d lines nil))

(defun anydisp:scroll-l/r
 (columns left)
 (let
  ((columns (/ columns 8))
   (w (/ *X-SIZE* 8))
   (seg
    (case *DISPLAY-MODE*
     (128 '(-20480 -19456 -18432 -17408))
     (6 '(-18432 -17920))
     ((88 18 16) '(-24576))))
   (l
    (case *DISPLAY-MODE* ((128 6) 99) ((88 18 16) *Y-SIZE*)))
   (z))
  (anydisp:move-mode t)
  (dolist
   (seg seg)
   (for
    (y 0 l)
    (setq z (* y w))
    (if left
     (anydisp:move-mem-down seg z (+ z columns) (- w columns))
     (anydisp:move-mem-up seg (+ z columns) z (- w columns)))))
  (anydisp:move-mode nil))
 (if left
  (anydisp:clear-area (- *X-SIZE* columns) 0 (+ 8 columns) *Y-SIZE*)
  (anydisp:clear-area 0 0 columns *Y-SIZE*)))

(defun anydisp:scroll-left
 (columns)
 (comment scroll screen number of columns left)
 (anydisp:scroll-l/r columns t))

(defun anydisp:scroll-right
 (columns)
 (comment scroll screen number of columns right)
 (anydisp:scroll-l/r columns))

(defun anydisp:scroll-u/d
 (lines up tmp)
 (anydisp:move-mode t)
 (flet
  ((m
    (seg new len)
    (dolist
     (seg seg)
     (if up (anydisp:move-mem-down seg 0 new len) (anydisp:move-mem-up seg new 0 len)))))
  (case *DISPLAY-MODE*
   (128
    (m
     '(-20480 -19456 -18432 -17408)
     (setq tmp (* 90 (/ lines 4)))
     (- 16384 tmp)))
   (6
    (m
     '(-18432 -17920)
     (setq tmp (* 80 (/ lines 2)))
     (- 8192 tmp)))
   ((16 18 88)
    (m
     '(-24576)
     (setq tmp (* (/ *X-SIZE* 8) lines))
     (- (* *Y-SIZE* (/ *X-SIZE* 8)) tmp)))))
 (anydisp:move-mode nil)
 (if up
  (anydisp:clear-area 0 (- *Y-SIZE* lines) *X-SIZE* lines)
  (anydisp:clear-area 0 0 *X-SIZE* lines)))

(defun anydisp:scroll-up
 (lines)
 (comment scroll screen up number of lines)
 (anydisp:scroll-u/d lines t))

(defun anydisp:scroll-window-down
 (x y w h lines)
 (comment scroll window down number of lines)
 (anydisp:scroll-window-u/d x y w h lines))

(defun anydisp:scroll-window-l/r
 (x y w h columns left)
 (anydisp:move-mode t)
 (let
  ((columns (/ columns 8))
   (w (/ w 8))
   (x (/ x 8))
   (y y)
   (h h)
   (bytes (/ *X-SIZE* 8))
   (z))
  (case *DISPLAY-MODE*
   (128
    (setq y (/ y 4))
    (setq h (sub1 (/ h 4))))
   (6
    (setq y (/ y 2))
    (setq h (sub1 (/ h 2)))))
  (dolist
   (seg
    (case *DISPLAY-MODE*
     (128 '(-20480 -19456 -18432 -17408))
     (6 '(-18432 -17920))
     ((16 18 88) '(-24576))))
   (for
    (line y (+ y h))
    (setq z (+ (* line bytes) x))
    (if left
     (anydisp:move-mem-down seg z (+ z columns) (- w columns))
     (anydisp:move-mem-up seg (+ z columns) z (- w columns))))))
 (anydisp:move-mode nil)
 (if left
  (anydisp:clear-area (+ x w (minus columns)) y columns h)
  (anydisp:clear-area x y columns h)))

(defun anydisp:scroll-window-left
 (x y w h columns)
 (comment scroll window number of columns right)
 (anydisp:scroll-window-l/r x y w h columns t))

(defun anydisp:scroll-window-right
 (x y w h columns)
 (comment scroll window number of columns right)
 (anydisp:scroll-window-l/r x y w h columns))

(defun anydisp:scroll-window-u/d
 (x y w h lines up)
 (anydisp:move-mode t)
 (let
  ((diff) (len) (start) (count) (bstep) (lstep))
  (case *DISPLAY-MODE*
   (128 (setq lstep 4) (setq bstep 90))
   (6 (setq lstep 2) (setq bstep 80))
   ((18 16) (setq lstep 1) (setq bstep 80))
   (88 (setq lstep 1) (setq bstep 100)))
  (setq diff (* bstep (/ lines lstep)))
  (setq len (/ w 8))
  (setq start
   (+ (* bstep (/ y lstep)) (/ x 8)))
  (setq count (* bstep (sub1 (/ h lstep))))
  (dolist
   (seg
    (case *DISPLAY-MODE*
     (128 '(-20480 -19456 -18432 -17408))
     (6 '(-18432 -17920))
     ((88 18 16) '(-24576))))
   (if up
    (for
     (offset start (+ start count (minus diff)) bstep)
     (anydisp:move-mem-down seg offset (+ offset diff) len))
    (for
     (offset
      (+ start count (minus diff) (minus bstep))
      start
      (minus bstep))
     (anydisp:move-mem-up seg (+ offset diff) offset len)))))
 (anydisp:move-mode nil)
 (if up
  (anydisp:clear-area x (+ y h (minus lines)) w lines)
  (anydisp:clear-area x y w lines)))

(defun anydisp:scroll-window-up
 (x y w h lines)
 (comment scroll window up number of lines)
 (anydisp:scroll-window-u/d x y w h lines t))

(defun anydisp:test
 (x)
 (anydisp:display-mode x)
 (anydisp:clear-area 0 0 *X-SIZE* *Y-SIZE* 1)
 (for
  (c 0 15)
  (color c)
  (point (* c 10) 0)
  (draw *X-SIZE* *Y-SIZE*)
  (point *X-SIZE* (* c 10))
  (draw 0 *Y-SIZE*))
 (cr)
 (point 0 0)
 (pprint BOOT)
 (anydisp:clear-area 50 50 100 50)
 (anydisp:scroll-up 50)
 (readcc)
 (anydisp:scroll-down 40)
 (readcc)
 (anydisp:scroll-left 80)
 (readcc)
 (anydisp:scroll-right 80)
 (readcc)
 (anydisp:display-mode 2))

(defun anydisp:test2
 (x)
 (anydisp:display-mode x)
 (color 1)
 (point 0 0)
 (draw *X-SIZE* *Y-SIZE*)
 (point *X-SIZE* 0)
 (draw 0 *Y-SIZE*)
 (cr)
 (point 0 0)
 (pprint BOOT)
 (anydisp:clear-area 50 50 100 50)
 (readcc)
 (progn
  (anydisp:boxi 159 15 122 82)
  (repeat-times 5 (anydisp:scroll-window-up 160 16 120 80 4))
  (readcc)
  (anydisp:boxi 39 119 82 82)
  (repeat-times 5 (anydisp:scroll-window-down 40 120 80 80 4))
  (readcc)
  (anydisp:boxi 287 15 122 98)
  (repeat-times 5 (anydisp:scroll-window-right 288 16 120 96 8))
  (readcc)
  (anydisp:boxi 175 119 122 82)
  (repeat-times 5 (anydisp:scroll-window-left 176 120 120 80 8))
  (readcc))
 (progn
  (anydisp:boxi 79 39 402 222)
  (anydisp:scroll-window-left 80 40 400 220 40)
  (readcc)
  (anydisp:scroll-window-up 80 40 400 220 80)
  (readcc)
  (anydisp:scroll-window-right 80 40 400 220 80)
  (readcc)
  (anydisp:scroll-window-down 80 40 400 220 80)
  (readcc))
 (anydisp:display-mode 2))

(defun anydisp:test3
 (x)
 (anydisp:display-mode x)
 (color 15)
 (point 0 0)
 (draw *X-SIZE* *Y-SIZE*)
 (point *X-SIZE* 0)
 (draw 0 *Y-SIZE*)
 (cr)
 (point 0 0)
 (pprint BOOT)
 (anydisp:clear-area 50 50 100 50)
 (repeat-times 10 (anydisp:scroll-window-up 0 0 *X-SIZE* *Y-SIZE* 4))
 (readcc)
 (anydisp:scroll-window-down 0 0 *X-SIZE* *Y-SIZE* 40)
 (readcc)
 (anydisp:scroll-window-left 0 0 *X-SIZE* *Y-SIZE* 80)
 (readcc)
 (anydisp:scroll-window-right 0 0 *X-SIZE* *Y-SIZE* 80)
 (readcc)
 (anydisp:display-mode 2))

(defun anydisp:write-mem-block
 (file seg adr len)
 (setq file (create file))
 (BX-reg file)
 (DS-reg seg)
 (DX-reg adr)
 (CX-reg len)
 (msdos 64)
 (close file))

(defun paska
 (z)
 (anydisp:display-mode (or z 18))
 (anydisp:clear-area 100 100 100 100 5)
 (color 1)
 (anydisp:boxi 1 1 (1- *X-SIZE*) (- *Y-SIZE* 10))
 (for
  (c 1 16)
  (color c)
  (point 0 0)
  (draw (* c 100) 500))
 (repeat-times 10 (anydisp:scroll-up 5))
 (repeat-times 10 (anydisp:scroll-down 5))
 (anydisp:save-screen 'ko.ko)
 (repeat-times 10 (anydisp:scroll-up 5))
 (anydisp:load-screen 'ko.ko)
 (repeat-times 10 (anydisp:scroll-left 10))
 (repeat-times 10 (anydisp:scroll-right 10))
 (repeat-times 3 (anydisp:scroll-window-up 100 100 100 100 10))
 (repeat-times 3 (anydisp:scroll-window-down 100 100 100 100 10)))
