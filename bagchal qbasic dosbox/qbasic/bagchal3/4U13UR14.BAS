DECLARE SUB deltigercursor ()
DECLARE SUB delcursor ()
DECLARE SUB cursor ()
DECLARE SUB speed ()
DECLARE SUB kgd (kx AS INTEGER)
DECLARE SUB ere ()
DECLARE FUNCTION getnam$ (getx AS INTEGER, gety AS INTEGER, blnkcol AS INTEGER)
DECLARE FUNCTION choosers! ()
DECLARE FUNCTION menuss! ()
DECLARE SUB dkillgoats (killg AS INTEGER)
DECLARE SUB storing ()
DECLARE SUB dialoge (pp AS INTEGER)
DECLARE SUB blank ()
DECLARE SUB backbagchal ()
DECLARE SUB puttiger ()
DECLARE SUB gettgs ()
DECLARE SUB defvariable ()
DECLARE SUB loadtemp (qty$)
DECLARE SUB savetemp (trt$)
DECLARE SUB board ()

DIM SHARED a%(2200)
DIM SHARED B%(2200)
DIM SHARED s%(5, 5)
DIM SHARED tm%(5, 5)
DIM SHARED space AS INTEGER
DIM SHARED tiger AS INTEGER
DIM SHARED goat AS INTEGER
DIM SHARED xx AS INTEGER
DIM SHARED yy AS INTEGER
DIM SHARED x AS INTEGER
DIM SHARED y AS INTEGER
DIM SHARED xf AS INTEGER
DIM SHARED yf AS INTEGER
DIM SHARED turn AS INTEGER
DIM SHARED true AS INTEGER
DIM SHARED false AS INTEGER
DIM SHARED goatturn AS INTEGER
DIM SHARED tigerturn AS INTEGER
DIM SHARED enter AS INTEGER
DIM SHARED goats AS INTEGER
DIM SHARED getx AS INTEGER
DIM SHARED gety AS INTEGER
DIM SHARED getxx AS INTEGER
DIM SHARED getyy AS INTEGER
DIM SHARED goatskill AS INTEGER
DIM SHARED totalgoats AS INTEGER
DIM SHARED cannotmove AS INTEGER
DIM SHARED move AS INTEGER
DIM SHARED tigernotmove AS INTEGER
DIM SHARED computer AS INTEGER
DIM SHARED gotomenu AS INTEGER
DIM SHARED goatname1$
DIM SHARED goatname2$
DIM xmmn$
DIM i AS INTEGER
DIM j  AS INTEGER
DIM undono AS INTEGER
DIM totalundo  AS INTEGER
CLS
ON ERROR GOTO fileerr
true = 1: false = 0
undono = 0

SCREEN 12
gettgs
gotomenu = 0
start:
SCREEN 0
GOSUB menuchoose
'==================define variables============================================
defvariable
'===============baghchal board
board
'============put 4 tiger on the baghchal board
puttiger
'==========define value of xx,yy and enter======================================
x = 210'cursor
y = 143'cursor
xx = 3
yy = 3
undono = 0
GOSUB undoo
backtobagchal:
'===========put cursor in the center or the baghchal board
cursor
'==============game loop======================================================
DO
xmmn1$ = INKEY$
IF xmmn1$ = "u" OR xmmn1$ = "U" THEN GOSUB undoopen: GOTO backtobagchal
IF xmmn1$ = "r" OR xmmn1$ = "R" THEN GOSUB redoopen: GOTO backtobagchal

IF xmmn1$ = CHR$(27) THEN gotomenu = 1: GOTO start
'enter for goat and tiger
IF computer = true AND enter = 4 THEN tigerturn = ture: GOSUB tiger2
IF computer = true AND enter = 3 THEN tigerturn = ture: GOSUB tiger1
IF xmmn1$ = CHR$(13) THEN
 enter = enter + 1
 IF enter = 1 THEN goatturn = true: GOSUB goat1
 IF enter = 2 THEN goatturn = true: GOSUB goat2:  GOSUB undoo
 IF enter = 3 THEN SLEEP 1: tigerturn = true: GOSUB tiger1
 IF enter = 4 THEN tigerturn = ture: GOSUB tiger2:  GOSUB undoo
END IF
'------------------------------moving cursor---------------------
'up move
IF (xmmn1$ = "2" OR xmmn1$ = CHR$(0) + CHR$(80)) AND y < 283 AND yy < 5 THEN
 cursor
 y = y + 70
 yy = yy + 1
 cursor
END IF
'down move
IF (xmmn1$ = "8" OR xmmn1$ = CHR$(0) + CHR$(72)) AND y > 3 AND yy > 1 THEN
 cursor
 y = y - 70
 yy = yy - 1
 cursor
END IF
'right move
IF (xmmn1$ = "6" OR xmmn1$ = CHR$(0) + CHR$(77)) AND x < 370 AND xx < 5 THEN
 cursor
 x = x + 80
 xx = xx + 1
 cursor
END IF
'left move
IF (xmmn1$ = "4" OR xmmn1$ = CHR$(0) + CHR$(75)) AND x > 50 AND xx > 1 THEN
 cursor
 x = x - 80
 xx = xx - 1
 cursor
END IF
'------------------------------moving cursor---------------------
IF computer = 0 THEN LOCATE 24, 10: PRINT "GOAT ["; goatname1$; "] VS TIGER ["; goatname2$ + "]"
IF computer = 1 THEN LOCATE 24, 10: PRINT "GOAT ["; goatname1$; "] VS TIGER [Computer]"

IF enter = 0 THEN LOCATE 25, 10: PRINT "goat ("; goatname1$; " -> turn !)                        "
IF enter = 1 THEN LOCATE 25, 10: PRINT "tiger ("; goatname2$; " -> turn !)                       "
'LOCATE 26, 1: PRINT "x= "; x; "y= "; y; "xx= "; xx; "yy= "; yy; "s%(xx, yy)= "; s%(xx, yy); "enter= "; enter; "goats= ", goats; "goatskill= "; goatskill ' "blocktiger= "; blocktiger; "tigernotmove= "; tigernotmove
LOCATE 26, 10:  PRINT "goatskill ="; goatskill; " [total goats=20]"; " [remaing goats"; 21 - goats; "]"
LOCATE 27, 5: PRINT "(if 6 goats kill then TIGER win) | (if 4 tigers can't move then GOAT win)"
dkillgoats (goatskill)
IF goatskill = 6 THEN
  LOCATE 23, 20: PRINT "TIGER WIN THE GAME [PRESS SPACE BAR TO RETURN MENU]":
  gotomenu = 1
  DO
   xmmn0$ = INKEY$
   IF xmmn0$ = CHR$(32) THEN GOTO start
  LOOP
END IF
GOSUB checkconditiontowingoat
LOOP
'''------------------------------goat 1-----------------------------------------
goat1:
'condition checking for goat
IF enter = 1 AND goatturn = true THEN
 IF goats < 21 THEN
   IF s%(xx, yy) = space THEN
   PUT (x, y), a%(1)'cursor
   ELSE
   enter = 0
   END IF
 ELSE GOSUB goatmove1
 END IF
getx = x
gety = y
getxx = xx
getyy = yy
goatturn = false
END IF
RETURN
'------------------------------goat 2-----------------------------------------
goat2: 'condition checking for goat
IF enter = 2 AND goatturn = true THEN
 IF goats < 21 THEN
  IF getxx = xx AND getyy = yy THEN
   PUT (x, y), a%(1)'cursor
   PUT (x, y), B%(1000)  'goat
   s%(xx, yy) = goat
   goats = goats + 1
   getx = x
   gety = y
   getxx = xx
   getyy = yy
   IF computer = true THEN
    enter = 3
   END IF
  ELSE
   PUT (getx, gety), a%(1)'cursor
   goatturn = false
   enter = 0
  END IF
 ELSE GOSUB goatmove2
 RETURN
 END IF
 goatturn = false
END IF
RETURN
 '------------------------------tiger 1-----------------------------------------
tiger1:
'USER PLAYER
'condition checking for tiger
IF computer = false AND enter = 3 AND tigerturn = true THEN
 IF s%(xx, yy) = tiger THEN
 PUT (x, y), a%(1)'cursor
 getx = x
 gety = y
 getxx = xx
 getyy = yy
 tigerturn = false
 ELSE
 tigerturn = false
 enter = 2
 END IF
END IF
'========================FOR COMPUTER PLAYER===================================
'computer killing and moving condition
'killing condition only
IF computer = true AND enter = 3 AND tigerturn = true THEN
'for computer
'whole condition checking
'left to right killing goat
'==============================================================================
xf = 130'goat location x
yf = 3'goat location y
xxf = 2
FOR yyf = 1 TO 5
 FOR z = 0 TO 2
 IF (s%(1 + z, yyf) = tiger AND s%(2 + z, yyf) = goat AND s%(3 + z, yyf) = space) THEN
  cursor
  getx = xf - 80
  gety = yf
  getxx = xxf - 1
  getyy = yyf
  xxf = xxf + 1
  xf = xf + 80
  enter = 4
  deltigercursor
  x = xf
  y = yf
  xx = xxf
  yy = yyf
  GOTO computerplaynext
 END IF
  xf = xf + 80
  xxf = xxf + 1
 NEXT z
 xf = 130
 yf = yf + 70
 xxf = 2
NEXT yyf
'right to left
'==============================================================================
xf = 290'goat location x
yf = 3'goat location y
xxf = 4
FOR yyf = 1 TO 5
 FOR z = 0 TO 2
 IF (s%(5 - z, yyf) = tiger AND s%(4 - z, yyf) = goat AND s%(3 - z, yyf) = space) THEN
  cursor
  getx = xf + 80'tiger
  gety = yf'tiger
  getxx = xxf + 1
  getyy = yyf
  xxf = xxf - 1
  xf = xf - 80
  enter = 4
  deltigercursor
  x = xf
  y = yf
  xx = xxf
  yy = yyf
  GOTO computerplaynext
 END IF
  xf = xf - 80
  xxf = xxf - 1
 NEXT z
 xf = 290
 yf = yf + 70
 xxf = 4
NEXT yyf
'up to down killing goat
'==============================================================================
xf = 50'goat location x
yf = 73'goat location y
yyf = 2
FOR xxf = 1 TO 5
 FOR z = 0 TO 2
 IF (s%(xxf, 1 + z) = tiger AND s%(xxf, 2 + z) = goat AND s%(xxf, 3 + z) = space) THEN
  cursor
  getx = xf
  gety = yf - 70
  getxx = xxf
  getyy = yyf - 1
  yyf = yyf + 1
  yf = yf + 70
  enter = 4
  deltigercursor
  x = xf
  y = yf
  xx = xxf
  yy = yyf
  GOTO computerplaynext
 END IF
  yf = yf + 70
  yyf = yyf + 1
 NEXT z
 yf = 73
 xf = xf + 80
 yyf = 2
NEXT xxf
'down to up killing goat
'==============================================================================
xf = 50'goat location x
yf = 213'goat location y
yyf = 4
FOR xxf = 1 TO 5
 FOR z = 0 TO 2
 IF (s%(xxf, 5 - z) = tiger AND s%(xxf, 4 - z) = goat AND s%(xxf, 3 - z) = space) THEN
  cursor
  getx = xf
  gety = yf + 70
  getxx = xxf
  getyy = yyf + 1
  yyf = yyf - 1
  yf = yf - 70
  enter = 4
  PUT (xf, yf), a%(1)'delete tiger cursor
  PUT (xf - 2, yf - 2), a%(1000)'cursor
  x = xf
  y = yf
  xx = xxf
  yy = yyf
  GOTO computerplaynext
 END IF
  yf = yf - 70
  yyf = yyf - 1
 NEXT z
 yf = 213
 xf = xf + 80
 yyf = 4
NEXT xxf
'right and down killing goat -2
'==============================================================================
ty = 3
gy = 73
sy = 143
tx = 50
gx = 130
sx = 210
 FOR i = 0 TO 2
 IF s%(1 + i, 1 + i) = tiger AND s%(2 + i, 2 + i) = goat AND s%(3 + i, 3 + i) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 1 + i'tiger location
    getyy = 1 + i'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx + 2
    yy = getyy + 2
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   gx = gx + 80
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
 NEXT i
'right and down killing goat -1
tx = 50
gx = 130
sx = 210
ty = 143
gy = 213
sy = 283
 IF s%(1, 3) = tiger AND s%(2, 4) = goat AND s%(3, 5) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 1 'tiger location
    getyy = 3 'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx + 2
    yy = getyy + 2
    GOTO computerplaynext
   END IF
'right and down killing goat -3
tx = 210
gx = 290
sx = 370
ty = 3
gy = 73
sy = 143
 IF s%(3, 1) = tiger AND s%(4, 2) = goat AND s%(5, 3) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 3 'tiger location
    getyy = 1 'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx + 2
    yy = getyy + 2
    GOTO computerplaynext
   END IF
'left and up killing goat -2
tx = 210
gx = 130
sx = 50
ty = 143
gy = 73
sy = 3
 FOR i = 0 TO 2
 IF s%(3 + i, 3 + i) = tiger AND s%(2 + i, 2 + i) = goat AND s%(1 + i, 1 + i) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 3 + i'tiger location
    getyy = 3 + i'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx - 2
    yy = getyy - 2
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   gx = gx + 80
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
 NEXT i
'left and up killing goat -1
tx = 210
gx = 130
sx = 50
ty = 283
gy = 213
sy = 143
 IF s%(3, 5) = tiger AND s%(2, 4) = goat AND s%(1, 3) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 3 'tiger location
    getyy = 5 'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx - 2
    yy = getyy - 2
    GOTO computerplaynext
   END IF
'left and up killing goat -3
tx = 370
gx = 290
sx = 210
ty = 143
gy = 73
sy = 3
 IF s%(5, 3) = tiger AND s%(4, 2) = goat AND s%(3, 1) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 5 'tiger location
    getyy = 3 'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx - 2
    yy = getyy - 2
    GOTO computerplaynext
   END IF
'left and down killing goat -2
tx = 370
gx = 290
sx = 210
ty = 3
gy = 73
sy = 143
FOR i = 0 TO 2
 IF s%(5 - i, 1 + i) = tiger AND s%(4 - i, 2 + i) = goat AND s%(3 - i, 3 + i) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 5 - i'tiger location
    getyy = 1 + i'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx - 2
    yy = getyy + 2
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   gx = gx - 80
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
 NEXT i
'right and up killing goat -2
tx = 210
gx = 290
sx = 370
ty = 143
gy = 73
sy = 3
 FOR i = 0 TO 2
 IF s%(3 - i, 3 + i) = tiger AND s%(4 - i, 2 + i) = goat AND s%(5 - i, 1 + i) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 3 - i'tiger location
    getyy = 3 + i'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx + 2
    yy = getyy - 2
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   gx = gx - 80
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
 NEXT i
'left and down killing goat -1
tx = 210
gx = 130
sx = 50
ty = 3
gy = 73
sy = 143
 IF s%(3, 1) = tiger AND s%(2, 2) = goat AND s%(1, 3) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 3 'tiger location
    getyy = 1 'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx - 2
    yy = getyy + 2
    GOTO computerplaynext
   END IF
'right and up killing goat -1
tx = 50
gx = 130
sx = 210
ty = 143
gy = 73
sy = 3
 IF s%(1, 3) = tiger AND s%(2, 2) = goat AND s%(3, 1) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 1 'tiger location
    getyy = 3 'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx + 2
    yy = getyy - 2
    GOTO computerplaynext
   END IF
'left and down killing goat -3
tx = 370
gx = 290
sx = 210
ty = 143
gy = 213
sy = 283
 IF s%(5, 3) = tiger AND s%(4, 4) = goat AND s%(3, 5) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 5 'tiger location
    getyy = 3 'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx - 2
    yy = getyy + 2
    GOTO computerplaynext
   END IF
'right and up killing goat -3
tx = 210
gx = 290
sx = 370
ty = 283
gy = 213
sy = 143
 IF s%(3, 5) = tiger AND s%(4, 4) = goat AND s%(5, 3) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty'tiger location
    getxx = 3 'tiger location
    getyy = 5 'tiger location
    x = sx 'goat location
    y = sy'goat location
    enter = 4
    cursor
    xx = getxx + 2
    yy = getyy - 2
    GOTO computerplaynext
   END IF
'whole condition cheking for moving condition
'moving condition only
'left to right moving t+s+g+s
ty = 3
sy = 3
gy = 3
FOR i = 1 TO 5
tx = 50
sx = 130
gx = 210
  FOR j = 0 TO 1
   IF s%(1 + j, i) = tiger AND s%(2 + j, i) = space AND s%(3 + j, i) = goat AND s%(4 + j, i) = space THEN
    delcursor
    getx = tx
    gety = ty
    getxx = 1 + j
    getyy = i
    xx = 2 + j
    yy = i
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   gx = gx + 80
  NEXT j
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
 NEXT i
'right to left moving s+g+s+t
ty = 3
sy = 3
gy = 3
FOR i = 1 TO 5
   tx = 290
   sx = 210
   gx = 130
   FOR j = 0 TO 1
   IF s%(4 + j, i) = tiger AND s%(3 + j, i) = space AND s%(2 + j, i) = goat AND s%(1 + j, i) = space THEN
    delcursor
    getx = tx 'tiger location
    gety = ty '"
    getxx = 4 + j
    getyy = i
    xx = 3 + j
    yy = i
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   gx = gx + 80
  NEXT j
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
 NEXT i
'up to down moving t+s+g+s
tx = 50
sx = 50
gx = 50
FOR i = 1 TO 5
ty = 3
sy = 73
gy = 143
 FOR j = 0 TO 1
   IF s%(i, 1 + j) = tiger AND s%(i, 2 + j) = space AND s%(i, 3 + j) = goat AND s%(i, 4 + j) = space THEN
    delcursor
    getx = tx
    gety = ty
    getxx = i
    getyy = 1 + j
    xx = i
    yy = 2 + j
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
 NEXT j
   tx = tx + 80
   sx = sx + 80
   gx = gx + 80
NEXT i
'down to up moving s+g+s+t
tx = 50
sx = 50
gx = 50
FOR i = 1 TO 5
ty = 213
sy = 143
gy = 73
 FOR j = 0 TO 1
   IF s%(i, 4 + j) = tiger AND s%(i, 3 + j) = space AND s%(i, 2 + j) = goat AND s%(i, 1 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = i'tiger location
    getyy = 4 + j
    xx = i
    yy = 3 + j
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
   NEXT j
   tx = tx + 80
   sx = sx + 80
   gx = gx + 80
   NEXT i
'right and up moving s+g+s+t
tx = 50
sx = 130
gx = 210
ty = 283
sy = 213
gy = 143
  FOR j = 0 TO 1
   IF s%(1 + j, 5 - j) = tiger AND s%(2 + j, 4 - j) = space AND s%(3 + j, 3 - j) = goat AND s%(4 + j, 2 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + j'tiger location
    getyy = 5 - j
    xx = getxx + 1'i
    yy = getyy - 1'3 + j
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   gx = gx + 80
   ty = ty - 70
   sy = sy - 70
   gy = gy - 70
NEXT j
'left and down moving s+g+s+t
tx = 370
sx = 290
gx = 210
ty = 3
sy = 73
gy = 143
  FOR j = 0 TO 1
   IF s%(5 - j, 1 + j) = tiger AND s%(4 - j, 2 + j) = space AND s%(3 - j, 3 + j) = goat AND s%(2 - j, 4 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 5 - j'tiger location
    getyy = 1 + j
    xx = getxx - 1'i
    yy = getyy + 1'3 + j
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   gx = gx - 80
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
   NEXT j
'left and up moving s+g+s+t
 tx = 370
 sx = 290
 gx = 210
 ty = 283
 sy = 213
 gy = 143
  FOR j = 0 TO 1
   IF s%(5 - j, 5 - j) = tiger AND s%(4 - j, 4 - j) = space AND s%(3 - j, 3 - j) = goat AND s%(2 - j, 2 - j) = space THEN
    PUT (getx, gety), a%(1)'delete goat cursor
    PUT (getx - 2, gety - 2), a%(1000)'cursor
    getx = tx'tiger location
    gety = ty
    getxx = 5 - j'tiger location
    getyy = 5 - j
    xx = getxx - 1'i
    yy = getyy - 1'3 + j
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   gx = gx - 80
   ty = ty - 70
   sy = sy - 70
   gy = gy - 70
NEXT j
'right and down moving s+g+s+t
tx = 50
sx = 130
gx = 210
ty = 3
sy = 73
gy = 143
  FOR j = 0 TO 1
   IF s%(1 + j, 1 + j) = tiger AND s%(2 + j, 2 + j) = space AND s%(3 + j, 3 + j) = goat AND s%(4 + j, 4 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + j'tiger location
    getyy = 1 + j
    xx = getxx + 1'i
    yy = getyy + 1'3 + j
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   gx = gx + 80
   ty = ty + 70
   sy = sy + 70
   gy = gy + 70
   NEXT j
'find space to move the tiger
'left to right moving
ty = 3
sy = 3
FOR i = 0 TO 4
tx = 50
sx = 130
  FOR j = 0 TO 3
   IF s%(1 + j, 1 + i) = tiger AND s%(2 + j, 1 + i) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + j'tiger location
    getyy = 1 + i
    xx = getxx + 1'to place the tiger
    yy = getyy
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   NEXT j
   ty = ty + 70
   sy = sy + 70
NEXT i
'right to left moving
'==============================================================================
ty = 3
sy = 3
FOR i = 0 TO 4
tx = 370
sx = 290
  FOR j = 0 TO 3
   IF s%(5 - j, 1 + i) = tiger AND s%(4 - j, 1 + i) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 5 - j'tiger location
    getyy = 1 + i
    xx = getxx - 1'to place the tiger
    yy = getyy
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   NEXT j
   ty = ty + 70
   sy = sy + 70
NEXT i
'up to down moving
tx = 50
sx = 50
FOR i = 0 TO 4
ty = 3
sy = 73
  FOR j = 0 TO 3
   IF s%(1 + i, 1 + j) = tiger AND s%(1 + i, 2 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + i'tiger location
    getyy = 1 + j
    xx = getxx 'to place the tiger
    yy = getyy + 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   ty = ty + 70
   sy = sy + 70
   NEXT j
   tx = tx + 80
   sx = sx + 80
NEXT i
'down to up moving
tx = 50
sx = 50
FOR i = 0 TO 4
ty = 283
sy = 213
 FOR j = 0 TO 3
   IF s%(1 + i, 5 - j) = tiger AND s%(1 + i, 4 - j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + i'tiger location
    getyy = 5 - j
    xx = getxx 'to place the tiger
    yy = getyy - 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   ty = ty - 70
   sy = sy - 70
   NEXT j
   tx = tx + 80
   sx = sx + 80
NEXT i
'down and right moving
tx = 50
sx = 130
ty = 3
sy = 73
  FOR j = 0 TO 3
   IF s%(1 + j, 1 + j) = tiger AND s%(2 + j, 2 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + j'tiger location
    getyy = 1 + j
    xx = getxx + 1'to place the tiger
    yy = getyy + 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   ty = ty + 70
   sy = sy + 70
   NEXT j
'up and left moving
tx = 370
sx = 290
ty = 283
sy = 213
  FOR j = 0 TO 3
   IF s%(5 - j, 5 - j) = tiger AND s%(4 - j, 4 - j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 5 - j'tiger location
    getyy = 5 - j
    xx = getxx - 1'to place the tiger
    yy = getyy - 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   ty = ty - 70
   sy = sy - 70
   NEXT j
'down and right moving 1
tx = 50
sx = 130
ty = 143
sy = 213
  FOR j = 0 TO 1
  IF s%(1 + j, 3 + j) = tiger AND s%(2 + j, 4 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + j'tiger location
    getyy = 3 + j
    xx = getxx + 1'to place the tiger
    yy = getyy + 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   ty = ty + 70
   sy = sy + 70
   NEXT j
'up and left moving 1
tx = 210
sx = 130
ty = 283
sy = 213
  FOR j = 0 TO 1
  IF s%(3 - j, 5 - j) = tiger AND s%(2 - j, 4 - j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 3 - j'tiger location
    getyy = 5 - j
    xx = getxx - 1'to place the tiger
    yy = getyy - 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   ty = ty - 70
   sy = sy - 70
  NEXT j
'down and right moving 3
tx = 210
sx = 290
ty = 3
sy = 73
  FOR j = 0 TO 1
  IF s%(3 + j, 1 + j) = tiger AND s%(4 + j, 2 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 3 + j'tiger location
    getyy = 1 + j
    xx = getxx + 1'to place the tiger
    yy = getyy + 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   ty = ty + 70
   sy = sy + 70
   NEXT j
'up and left moving 3
'==============================================================================
tx = 370
sx = 290
ty = 143
sy = 73
  FOR j = 0 TO 1
  IF s%(5 - j, 3 - j) = tiger AND s%(4 - j, 2 - j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 5 - j'tiger location
    getyy = 3 - j
    xx = getxx - 1'to place the tiger
    yy = getyy - 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   ty = ty - 70
   sy = sy - 70
  NEXT j
'up and right moving
tx = 50
sx = 130
ty = 283
sy = 213
FOR j = 0 TO 3
   IF s%(1 + j, 5 - j) = tiger AND s%(2 + j, 4 - j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + j'tiger location
    getyy = 5 - j
    xx = getxx + 1'to place the tiger
    yy = getyy - 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   ty = ty - 70
   sy = sy - 70
NEXT j
'down and left moving
tx = 370
sx = 290
ty = 3
sy = 73
 FOR j = 0 TO 3
   IF s%(5 - j, 1 + j) = tiger AND s%(4 - j, 2 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 5 - j'tiger location
    getyy = 1 + j
    xx = getxx - 1'to place the tiger
    yy = getyy + 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   ty = ty + 70
   sy = sy + 70
NEXT j
'up and right moving 1
tx = 50
sx = 130
ty = 143
sy = 73
  FOR j = 0 TO 1
   IF s%(1 + j, 3 - j) = tiger AND s%(2 + j, 2 - j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 1 + j'tiger location
    getyy = 3 - j
    xx = getxx + 1'to place the tiger
    yy = getyy - 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   ty = ty - 70
   sy = sy - 70
  NEXT j
'down and left moving 1
tx = 210
sx = 130
ty = 3
sy = 73
  FOR j = 0 TO 1
  IF s%(3 - j, 1 + j) = tiger AND s%(2 - j, 2 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 3 - j'tiger location
    getyy = 1 + j
    xx = getxx - 1'to place the tiger
    yy = getyy + 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   ty = ty + 70
   sy = sy + 70
  NEXT j
'down and left moving 3
'==============================================================================
 tx = 370
 sx = 290
 ty = 143
 sy = 213
  FOR j = 0 TO 1
  IF s%(5 - j, 3 + j) = tiger AND s%(4 - j, 4 + j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 5 - j'tiger location
    getyy = 3 + j
    xx = getxx - 1'to place the tiger
    yy = getyy + 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx - 80
   sx = sx - 80
   ty = ty + 70
   sy = sy + 70
  NEXT j
'up and right moving 3
tx = 210
 sx = 290
 ty = 283
 sy = 213
  FOR j = 0 TO 1
  IF s%(3 + j, 5 - j) = tiger AND s%(4 + j, 4 - j) = space THEN
    delcursor
    getx = tx'tiger location
    gety = ty
    getxx = 3 + j'tiger location
    getyy = 5 - j
    xx = getxx + 1'to place the tiger
    yy = getyy - 1
    x = sx
    y = sy
    enter = 4
    cursor
    GOTO computerplaynext
   END IF
   tx = tx + 80
   sx = sx + 80
   ty = ty - 70
   sy = sy - 70
  NEXT j
END IF
computerplaynext:
RETURN
'------------------------------tiger 2-----------------------------------------
tiger2:
IF enter = 4 AND tigerturn = ture THEN
'-------------moving right and kill right
 't=s=g for moving tiger
IF getyy = yy AND xx > getxx THEN '---------------------------------------
  IF s%(getxx + 1, yy) = space AND getxx + 1 = xx THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    'END
    GOTO tiger2next
  END IF
'eeeeeeeeeeeeeeerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
  'goats killing
  't=g=s for killing goat
  IF s%(getxx, getyy) = tiger AND s%(getxx + 1, getyy) = goat AND xx = getxx + 2 AND s%(xx, yy) = space THEN
   IF computer = false THEN PUT (getx, gety), a%(1)'cursor
   PUT (getx, gety), B%(1)  'tiger
   PUT (getx + 80, gety), B%(1000)'goat
   PUT (x, y), B%(1)  'tiger
   s%(getxx, getyy) = space
   s%(getxx + 1, getyy) = space
   s%(xx, yy) = tiger
   tigerturn = false
   enter = 0
   goatskill = goatskill + 1
   GOTO tiger2next
  END IF
END IF'---------------------------------------
'-------------moving left and kill right
 IF getyy = yy AND xx < getxx THEN '---------------------------------------
  IF s%(getxx - 1, yy) = space AND getxx - 1 = xx THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
  END IF
'goats killing
  IF s%(getxx, getyy) = tiger AND s%(getxx - 1, getyy) = goat AND xx = getxx - 2 AND s%(xx, yy) = space THEN
   IF computer = false THEN PUT (getx, gety), a%(1)'cursor
   PUT (getx, gety), B%(1)  'tiger
   PUT (getx - 80, gety), B%(1000)'goat
   PUT (x, y), B%(1)  'tiger
   s%(getxx, getyy) = space
   s%(getxx - 1, getyy) = space
   s%(xx, yy) = tiger
   tigerturn = false
   enter = 0
   goatskill = goatskill + 1
   GOTO tiger2next
  END IF
END IF'---------------------------------------
'-----------------------------down----------------------------------
 IF getxx = xx AND yy > getyy THEN '---------------------------------------
  IF s%(xx, getyy + 1) = space AND getyy + 1 = yy THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
  END IF
'goats killing
  IF s%(getxx, getyy) = tiger AND s%(getxx, getyy + 1) = goat AND yy = getyy + 2 AND s%(xx, yy) = space THEN
   IF computer = false THEN PUT (getx, gety), a%(1)'cursor
   PUT (getx, gety), B%(1)  'tiger
   PUT (getx, gety + 70), B%(1000)'goat
   PUT (x, y), B%(1)  'tiger
   s%(getxx, getyy) = space
   s%(getxx, getyy + 1) = space
   s%(xx, yy) = tiger
   tigerturn = false
   enter = 0
   goatskill = goatskill + 1
   GOTO tiger2next
  END IF
END IF'---------------------------------------
'-----------------------------up----------------------------------
 IF getxx = xx AND yy < getyy THEN '---------------------------------------
  IF s%(xx, getyy - 1) = space AND getyy - 1 = yy THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
  END IF
'goats killing
   IF s%(getxx, getyy) = tiger AND s%(getxx, getyy - 1) = goat AND yy = getyy - 2 AND s%(xx, yy) = space THEN
   IF computer = false THEN PUT (getx, gety), a%(1)'cursor
   PUT (getx, gety), B%(1)  'tiger
   PUT (getx, gety - 70), B%(1000)'goat
   PUT (x, y), B%(1)  'tiger
   s%(getxx, getyy) = space
   s%(getxx, getyy - 1) = space
   s%(xx, yy) = tiger
   tigerturn = false
   enter = 0
   goatskill = goatskill + 1
   GOTO tiger2next
  END IF
END IF'---------------------------------------
'=========================right-down and left-up================================================
'---------------------------cross right-down------------------------------------
 'cross right down
 IF (xx > getxx AND yy > getxx) AND ((getxx = 1 AND getyy = 3) OR (getxx = 2 AND getyy = 4) OR (getxx = 3 AND getyy = 5)) THEN
   IF getxx = xx - 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'cross right down kill
   'goats killing
   IF getxx = xx - 2 AND getyy = yy - 2 AND s%(getxx, getyy) = tiger AND s%(xx - 1, yy - 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx + 80, gety + 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx + 1, getyy + 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
END IF'---------------------------------------
'move left up
 IF (xx > getxx AND yy > getyy) AND ((getxx = 3 AND getyy = 1) OR (getxx = 4 AND getyy = 2) OR (getxx = 5 AND getyy = 3)) THEN
   IF getxx = xx - 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats killing
   IF getxx = xx - 2 AND getyy = yy - 2 AND s%(getxx, getyy) = tiger AND s%(xx - 1, yy - 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx + 80, gety + 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx + 1, getyy + 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
END IF'---------------------------------------
IF (xx > getxx AND yy > getyy) AND ((getxx = 1 AND getyy = 1) OR (getxx = 2 AND getyy = 2) OR (getxx = 3 AND getyy = 3) OR (getxx = 4 AND getyy = 4) OR (getxx = 5 AND getyy = 5)) THEN
   IF getxx = xx - 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats killing
   IF getxx = xx - 2 AND getyy = yy - 2 AND s%(getxx, getyy) = tiger AND s%(xx - 1, yy - 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx + 80, gety + 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx + 1, getyy + 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
 END IF
'---------------------------cross left-up------------------------------------
IF (xx < getxx AND yy < getyy) AND ((getxx = 1 AND getyy = 3) OR (getxx = 2 AND getyy = 4) OR (getxx = 3 AND getyy = 5)) THEN '---------------------------------------
   IF getxx = xx + 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats killing
   IF getxx = xx + 2 AND getyy = yy + 2 AND s%(getxx, getyy) = tiger AND s%(xx + 1, yy + 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx - 80, gety - 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx - 1, getyy - 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
END IF'---------------------------------------
IF (xx < getxx AND yy < getxx) AND ((getxx = 3 AND getyy = 1) OR (getxx = 4 AND getyy = 2) OR (getxx = 5 AND getyy = 3)) THEN
   IF getxx = xx + 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats killing
   IF getxx = xx + 2 AND getyy = yy + 2 AND s%(getxx, getyy) = tiger AND s%(xx + 1, yy + 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx - 80, gety - 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx - 1, getyy - 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
END IF'---------------------------------------
IF (xx < getxx AND yy < getyy) AND ((getxx = 1 AND getyy = 1) OR (getxx = 2 AND getyy = 2) OR (getxx = 3 AND getyy = 3) OR (getxx = 4 AND getyy = 4) OR (getxx = 5 AND getyy = 5)) THEN
   IF getxx = xx + 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats killing
   IF getxx = xx + 2 AND getyy = yy + 2 AND s%(getxx, getyy) = tiger AND s%(xx + 1, yy + 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx - 80, gety - 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx - 1, getyy - 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
 END IF '---------------------------------------
 '=========================right-down and left-up================================================
'=========================right-up and left-down================================================
'---------------------------cross right-down------------------------------------
 'cross right up
 IF (xx > getxx AND yy < getyy) AND ((getxx = 1 AND getyy = 3) OR (getxx = 2 AND getyy = 2) OR (getxx = 3 AND getyy = 1)) THEN
   IF getxx = xx - 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'cross right up kill goats killing
   IF getxx = xx - 2 AND getyy = yy + 2 AND s%(getxx, getyy) = tiger AND s%(xx - 1, yy + 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx + 80, gety - 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx + 1, getyy - 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
END IF'---------------------------------------
'cross right down ''''''''''''''''
 IF (xx > getxx AND yy < getyy) AND ((getxx = 3 AND getyy = 5) OR (getxx = 4 AND getyy = 4) OR (getxx = 5 AND getyy = 3)) THEN
   IF getxx = xx - 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats killing
   IF getxx = xx - 2 AND getyy = yy + 2 AND s%(getxx, getyy) = tiger AND s%(xx - 1, yy + 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx + 80, gety - 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx + 1, getyy - 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
END IF'---------------------------------------
IF (xx > getxx AND yy < getyy) AND ((getxx = 1 AND getyy = 5) OR (getxx = 2 AND getyy = 4) OR (getxx = 3 AND getyy = 3) OR (getxx = 4 AND getyy = 2) OR (getxx = 5 AND getyy = 1)) THEN
   IF getxx = xx - 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats killing
   IF getxx = xx - 2 AND getyy = yy + 2 AND s%(getxx, getyy) = tiger AND s%(xx - 1, yy + 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx + 80, gety - 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx + 1, getyy - 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
 END IF
'---------------------------cross left-down------------------------------------
 'cross right down
 IF (xx < getxx AND yy > getyy) AND ((getxx = 1 AND getyy = 3) OR (getxx = 2 AND getyy = 2) OR (getxx = 3 AND getyy = 1)) THEN
   IF getxx = xx + 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'cross left down kill
   'goats killing
   IF getxx = xx + 2 AND getyy = yy - 2 AND s%(getxx, getyy) = tiger AND s%(xx + 1, yy - 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx - 80, gety + 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx - 1, getyy + 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
END IF'---------------------------------------
'cross left down kill''''''''''''''''
 IF (xx < getxx AND yy > getyy) AND ((getxx = 3 AND getyy = 5) OR (getxx = 4 AND getyy = 4) OR (getxx = 5 AND getyy = 3)) THEN
   IF getxx = xx + 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats killing
   IF getxx = xx + 2 AND getyy = yy - 2 AND s%(getxx, getyy) = tiger AND s%(xx + 1, yy - 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx - 80, gety + 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx - 1, getyy + 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
END IF'---------------------------------------
IF (xx < getxx AND yy > getyy) AND ((getxx = 1 AND getyy = 5) OR (getxx = 2 AND getyy = 4) OR (getxx = 3 AND getyy = 3) OR (getxx = 4 AND getyy = 2) OR (getxx = 5 AND getyy = 1)) THEN
   IF getxx = xx + 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1)  'delete tiger
    PUT (x, y), B%(1)  'tiger
    s%(xx, yy) = tiger
    s%(getxx, getyy) = space
    tigerturn = false
    enter = 0
    GOTO tiger2next
   END IF
'goats kiling
   IF getxx = xx + 2 AND getyy = yy - 2 AND s%(getxx, getyy) = tiger AND s%(xx + 1, yy - 1) = goat AND s%(xx, yy) = space THEN
    IF computer = false THEN PUT (getx, gety), a%(1)'cursor
    PUT (getx, gety), B%(1)  'tiger
    PUT (getx - 80, gety + 70), B%(1000)'goat
    PUT (x, y), B%(1)  'tiger
    s%(getxx, getyy) = space
    s%(getxx - 1, getyy + 1) = space
    s%(xx, yy) = tiger
    tigerturn = false
    enter = 0
    goatskill = goatskill + 1
    GOTO tiger2next
  END IF
 END IF
'=========================right-down and left-up================================================
'for error
PUT (getx, gety), a%(1)'cursor
  tigerturn = false
  enter = 2
 END IF
tiger2next:
RETURN
'==============================================================================
goatmove0:
 IF goats = 2 AND s%(xx, yy) = goat THEN
  PUT (x, y), a%(1)'cursor
  getx = x
  gety = y
  getxx = xx
  getyy = yy
  ELSE
  enter = 0
 END IF
RETURN
'==============================================================================
goatmove:
 IF goats = 2 THEN
  IF getyy = yy AND xx > getxx THEN '---------------------------------------
   IF s%(getxx + 1, yy) = space AND getxx + 1 = xx THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)  'delete tiger
    PUT (x, y), B%(1000)  'tiger
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    enter = 3
   END IF
  END IF
  END IF
RETURN
'==============================================================================
goatmove1:
IF s%(xx, yy) = goat THEN
 PUT (x, y), a%(1)'cursor
 getx = x
 gety = y
 getxx = xx
 getyy = yy
 goatturn = false
ELSE
 goatturn = false
 enter = 0
END IF
RETURN
'==============================================================================
goatmove2:
'left
 IF getyy = yy AND xx > getxx THEN '---------------------------------------
  IF s%(getxx + 1, yy) = space AND getxx + 1 = xx THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    GOTO goatnext
 END IF
 END IF
'right
 IF getyy = yy AND xx < getxx THEN '---------------------------------------
  IF s%(getxx - 1, yy) = space AND getxx - 1 = xx THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    GOTO goatnext
  END IF
 END IF
'down
 IF getxx = xx AND yy > getyy THEN '---------------------------------------
  IF s%(xx, getyy + 1) = space AND getyy + 1 = yy THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    gety = y
    GOTO goatnext
  END IF
 END IF
'up
 IF getxx = xx AND yy < getyy THEN '---------------------------------------
  IF s%(xx, getyy - 1) = space AND getyy - 1 = yy THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    gety = y
    GOTO goatnext
  END IF
 END IF
'cross right down  1
 IF (xx > getxx AND yy > getxx) AND ((getxx = 1 AND getyy = 3) OR (getxx = 2 AND getyy = 4) OR (getxx = 3 AND getyy = 5)) THEN
   IF getxx = xx - 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
    GOTO goatnext
   END IF
 END IF
'move left up-1
 IF (xx > getxx AND yy > getyy) AND ((getxx = 3 AND getyy = 1) OR (getxx = 4 AND getyy = 2) OR (getxx = 5 AND getyy = 3)) THEN
   IF getxx = xx - 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    getx = x
    gety = y
    goatturn = false
    GOTO goatnext
   END IF
 END IF
'move right and down-1
 IF (xx > getxx AND yy > getyy) AND ((getxx = 1 AND getyy = 1) OR (getxx = 2 AND getyy = 2) OR (getxx = 3 AND getyy = 3) OR (getxx = 4 AND getyy = 4) OR (getxx = 5 AND getyy = 5)) THEN
   IF getxx = xx - 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
'---------------------------cross left-up-2------------------------------------
 IF (xx < getxx AND yy < getyy) AND ((getxx = 1 AND getyy = 3) OR (getxx = 2 AND getyy = 4) OR (getxx = 3 AND getyy = 5)) THEN '---------------------------------------
   IF getxx = xx + 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete gaot
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
IF (xx < getxx AND yy < getxx) AND ((getxx = 3 AND getyy = 1) OR (getxx = 4 AND getyy = 2) OR (getxx = 5 AND getyy = 3)) THEN
   IF getxx = xx + 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
IF (xx < getxx AND yy < getyy) AND ((getxx = 1 AND getyy = 1) OR (getxx = 2 AND getyy = 2) OR (getxx = 3 AND getyy = 3) OR (getxx = 4 AND getyy = 4) OR (getxx = 5 AND getyy = 5)) THEN
   IF getxx = xx + 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
'cross right up
 IF (xx > getxx AND yy < getyy) AND ((getxx = 1 AND getyy = 3) OR (getxx = 2 AND getyy = 2) OR (getxx = 3 AND getyy = 1)) THEN
   IF getxx = xx - 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete gpat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
   GOTO goatnext
   END IF
 END IF
'cross right up ''''''''''''''''
 IF (xx > getxx AND yy < getyy) AND ((getxx = 3 AND getyy = 5) OR (getxx = 4 AND getyy = 4) OR (getxx = 5 AND getyy = 3)) THEN
   IF getxx = xx - 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
IF (xx > getxx AND yy < getyy) AND ((getxx = 1 AND getyy = 5) OR (getxx = 2 AND getyy = 4) OR (getxx = 3 AND getyy = 3) OR (getxx = 4 AND getyy = 2) OR (getxx = 5 AND getyy = 1)) THEN
   IF getxx = xx - 1 AND getyy = yy + 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
'cross left down
 IF (xx < getxx AND yy > getyy) AND ((getxx = 1 AND getyy = 3) OR (getxx = 2 AND getyy = 2) OR (getxx = 3 AND getyy = 1)) THEN
   IF getxx = xx + 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
'cross left down kill''''''''''''''''
 IF (xx < getxx AND yy > getyy) AND ((getxx = 3 AND getyy = 5) OR (getxx = 4 AND getyy = 4) OR (getxx = 5 AND getyy = 3)) THEN
   IF getxx = xx + 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
IF (xx < getxx AND yy > getyy) AND ((getxx = 1 AND getyy = 5) OR (getxx = 2 AND getyy = 4) OR (getxx = 3 AND getyy = 3) OR (getxx = 4 AND getyy = 2) OR (getxx = 5 AND getyy = 1)) THEN
   IF getxx = xx + 1 AND getyy = yy - 1 AND s%(xx, yy) = space THEN
    PUT (getx, gety), a%(1)'delete cursor
    PUT (getx, gety), B%(1000)'delete goat
    PUT (x, y), B%(1000)  'goat
    s%(xx, yy) = goat
    s%(getxx, getyy) = space
    goatturn = false
    getx = x
    gety = y
GOTO goatnext
   END IF
 END IF
'for error
  PUT (getx, gety), a%(1)'cursor
  tigerturn = false
  enter = 0
  GOTO e
goatnext:
  tigerturn = true
  enter = 3
e:
RETURN
checkconditiontowingoat:
'======================1,1=5,1============================
's%(1,1)
IF s%(1, 1) = tiger AND (s%(2, 1) = goat OR s%(2, 1) = tiger) AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(1, 2) = goat OR s%(1, 2) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(3, 3) =  _
goat OR s%(3, 3) = tiger) THEN
 tm%(1, 1) = cannotmove
ELSE tm%(1, 1) = move
END IF
's%(2,1)
IF s%(2, 1) = tiger AND (s%(1, 1) = goat OR s%(1, 1) = tiger) AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(4, 1) = goat OR s%(4, 1) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(2, 3) = goat OR s%(2, 3) = tiger) THEN
 tm%(2, 1) = cannotmove
ELSE tm%(2, 1) = move
END IF
's%(3,1)
IF s%(3, 1) = tiger AND (s%(2, 1) = goat OR s%(2, 1) = tiger) AND (s%(1, 1) = goat OR s%(1, 1) = tiger) AND (s%(4, 1) = goat OR s%(4, 1) = tiger) AND (s%(5, 1) = goat OR s%(5, 1) = tiger) AND (s%(3, 2) = goat OR s%(3, 2) = tiger) AND (s%(3, 3) =  _
goat OR s%(3, 3) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) AND (s%(5, 3) = goat OR s%(5, 3) = tiger) THEN
 tm%(3, 1) = cannotmove
ELSE tm%(3, 1) = move
END IF
's%(4,1)
IF s%(4, 1) = tiger AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(2, 1) = goat OR s%(2, 1) = tiger) AND (s%(5, 1) = goat OR s%(5, 1) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) AND (s%(4, 3) = goat OR s%(4, 3) = tiger) THEN
 tm%(4, 1) = cannotmove
ELSE tm%(4, 1) = move
END IF
's%(5,1)
IF s%(5, 1) = tiger AND (s%(4, 1) = goat OR s%(4, 1) = tiger) AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(5, 2) = goat OR s%(5, 2) = tiger) AND (s%(5, 3) = goat OR s%(5, 3) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) AND (s%(3, 3) =  _
goat OR s%(3, 3) = tiger) THEN
 tm%(5, 1) = cannotmove
ELSE tm%(5, 1) = move
END IF
'=====================1,2=5,2==================================
's%(1,2)
IF s%(1, 2) = tiger AND (s%(1, 1) = goat OR s%(1, 1) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(3, 2) = goat OR s%(3, 2) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(1, 4) = goat OR s%(1, 4) = tiger) THEN
 tm%(1, 2) = cannotmove
ELSE tm%(1, 2) = move
END IF
's%(2,2)
IF s%(2, 2) = tiger AND (s%(1, 2) = goat OR s%(1, 2) = tiger) AND (s%(2, 1) = goat OR s%(2, 1) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(1, 1) = goat OR s%(1, 1) = tiger) AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(3, 2) =  _
goat OR s%(3, 2) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) AND (s%(2, 3) = goat OR s%(2, 3) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) THEN
 tm%(2, 2) = cannotmove
ELSE tm%(2, 2) = move
END IF
's%(3,2)
IF s%(3, 2) = tiger AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(1, 2) = goat OR s%(1, 2) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) AND (s%(5, 2) = goat OR s%(5, 2) = tiger) AND (s%(3, 3) =  _
goat OR s%(3, 3) = tiger) AND (s%(3, 4) = goat OR s%(3, 4) = tiger) THEN
 tm%(3, 2) = cannotmove
ELSE tm%(3, 2) = move
END IF
's%(4,2)
IF s%(4, 2) = tiger AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(4, 1) = goat OR s%(4, 1) = tiger) AND (s%(5, 1) = goat OR s%(5, 1) = tiger) AND (s%(5, 2) = goat OR s%(5, 2) = tiger) AND (s%(5, 3) = goat OR s%(5, 3) = tiger) AND (s%(4, 3) =  _
goat OR s%(4, 3) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(3, 2) = goat OR s%(3, 2) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) THEN
 tm%(4, 2) = cannotmove
ELSE tm%(4, 2) = move
END IF
's%(5,2)
IF s%(5, 2) = tiger AND (s%(5, 1) = goat OR s%(5, 1) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) AND (s%(3, 2) = goat OR s%(3, 2) = tiger) AND (s%(5, 3) = goat OR s%(5, 3) = tiger) AND (s%(5, 4) = goat OR s%(5, 4) = tiger) THEN
 tm%(5, 2) = cannotmove
ELSE tm%(5, 2) = move
END IF
'=======================1,3=5,3==========================
's%(1,3)
IF s%(1, 3) = tiger AND (s%(1, 2) = goat OR s%(1, 2) = tiger) AND (s%(1, 1) = goat OR s%(1, 1) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(2, 3) = goat OR s%(2, 3) = tiger) AND (s%(3, 3) =  _
goat OR s%(3, 3) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(3, 5) = goat OR s%(3, 5) = tiger) AND (s%(1, 4) = goat OR s%(1, 4) = tiger) AND (s%(1, 5) = goat OR s%(1, 5) = tiger) THEN
 tm%(1, 3) = cannotmove
ELSE tm%(1, 3) = move
END IF
's%(2,3)
IF s%(2, 3) = tiger AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(2, 1) = goat OR s%(2, 1) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(4, 3) = goat OR s%(4, 3) = tiger) AND (s%(2, 4) =  _
goat OR s%(2, 4) = tiger) AND (s%(2, 5) = goat OR s%(2, 5) = tiger) THEN
 tm%(2, 3) = cannotmove
ELSE tm%(2, 3) = move
END IF
's%(3,3)
IF s%(3, 3) = tiger AND (s%(2, 3) = goat OR s%(2, 3) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(1, 1) = goat OR s%(1, 1) = tiger) AND (s%(3, 2) = goat OR s%(3, 2) = tiger) AND (s%(3, 1) =  _
goat OR s%(3, 1) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) THEN
 IF (s%(5, 1) = goat OR s%(5, 1) = tiger) AND (s%(4, 3) = goat OR s%(4, 3) = tiger) AND (s%(5, 3) = goat OR s%(5, 3) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) AND (s%(5, 5) = goat OR s%(5, 5) = tiger) AND (s%(3, 4) = goat OR s%(3, 4) =  _
tiger) AND (s%(3, 5) = goat OR s%(3, 5) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) THEN
  IF (s%(1, 5) = goat OR s%(1, 5) = tiger) THEN
   tm%(3, 3) = cannotmove
  ELSE tm%(3, 3) = move
  END IF
 END IF
END IF
's%(4,3)
IF s%(4, 3) = tiger AND (s%(5, 3) = goat OR s%(5, 3) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) AND (s%(4, 5) = goat OR s%(4, 5) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(2, 3) = goat OR s%(2, 3) = tiger) AND (s%(4, 2) =  _
goat OR s%(4, 2) = tiger) AND (s%(4, 1) = goat OR s%(4, 1) = tiger) THEN
 tm%(4, 3) = cannotmove
ELSE tm%(4, 3) = move
END IF
's%(5,3)
IF s%(5, 3) = tiger AND (s%(4, 3) = goat OR s%(4, 3) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) AND (s%(3, 1) = goat OR s%(3, 1) = tiger) AND (s%(5, 2) = goat OR s%(5, 2) = tiger) AND (s%(5, 1) =  _
goat OR s%(5, 1) = tiger) AND (s%(5, 4) = goat OR s%(5, 4) = tiger) AND (s%(5, 5) = goat OR s%(5, 5) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) AND (s%(3, 5) = goat OR s%(3, 5) = tiger) THEN
 tm%(5, 3) = cannotmove
ELSE tm%(5, 3) = move
END IF
'=======================1,4=5,4===========================
's%(1,4)
IF s%(1, 4) = tiger AND (s%(1, 5) = goat OR s%(1, 5) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(1, 2) = goat OR s%(1, 2) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(3, 4) = goat OR s%(3, 4) = tiger) THEN
 tm%(1, 4) = cannotmove
ELSE tm%(1, 4) = move
END IF
's%(2,4)
IF s%(2, 4) = tiger AND (s%(3, 5) = goat OR s%(3, 5) = tiger) AND (s%(2, 5) = goat OR s%(2, 5) = tiger) AND (s%(1, 5) = goat OR s%(1, 5) = tiger) AND (s%(1, 4) = goat OR s%(1, 4) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(2, 3) =  _
goat OR s%(2, 3) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) AND (s%(3, 4) = goat OR s%(3, 4) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) THEN
 tm%(2, 4) = cannotmove
ELSE tm%(2, 4) = move
END IF
's%(3,4)
IF s%(3, 4) = tiger AND (s%(3, 5) = goat OR s%(3, 5) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(1, 4) = goat OR s%(1, 4) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(3, 2) = goat OR s%(3, 2) = tiger) AND (s%(4, 4) =  _
goat OR s%(4, 4) = tiger) AND (s%(5, 4) = goat OR s%(5, 4) = tiger) THEN
 tm%(3, 4) = cannotmove
ELSE tm%(3, 4) = move
END IF
's%(4,4)
IF s%(4, 4) = tiger AND (s%(5, 3) = goat OR s%(5, 3) = tiger) AND (s%(5, 4) = goat OR s%(5, 4) = tiger) AND (s%(5, 5) = goat OR s%(5, 5) = tiger) AND (s%(4, 5) = goat OR s%(4, 5) = tiger) AND (s%(3, 5) = goat OR s%(3, 5) = tiger) AND (s%(3, 4) =  _
goat OR s%(3, 4) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(2, 2) = goat OR s%(2, 2) = tiger) AND (s%(4, 3) = goat OR s%(4, 3) = tiger) AND (s%(4, 2) = goat OR s%(4, 2) = tiger) THEN
 tm%(4, 4) = cannotmove
ELSE tm%(4, 4) = move
END IF
's%(5,4)
IF s%(5, 4) = tiger AND (s%(5, 5) = goat OR s%(5, 5) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) AND (s%(3, 4) = goat OR s%(3, 4) = tiger) AND (s%(5, 3) = goat OR s%(5, 3) = tiger) AND (s%(5, 2) = goat OR s%(5, 2) = tiger) THEN
 tm%(5, 4) = cannotmove
ELSE tm%(5, 4) = move
END IF
'===================1,5=5,5==============================================
's%(1,5)
IF s%(1, 5) = tiger AND (s%(1, 4) = goat OR s%(1, 4) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(2, 5) = goat OR s%(2, 5) = tiger) AND (s%(3, 5) =  _
goat OR s%(3, 5) = tiger) THEN
 tm%(1, 5) = cannotmove
ELSE tm%(1, 5) = move
END IF
's%(2,5)
IF s%(2, 5) = tiger AND (s%(1, 5) = goat OR s%(1, 5) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(2, 3) = goat OR s%(2, 3) = tiger) AND (s%(3, 5) = goat OR s%(3, 5) = tiger) AND (s%(4, 5) = goat OR s%(4, 5) = tiger) THEN
 tm%(2, 5) = cannotmove
ELSE tm%(2, 5) = move
END IF
's%(3,5)
IF s%(3, 5) = tiger AND (s%(2, 5) = goat OR s%(2, 5) = tiger) AND (s%(1, 5) = goat OR s%(1, 5) = tiger) AND (s%(2, 4) = goat OR s%(2, 4) = tiger) AND (s%(1, 3) = goat OR s%(1, 3) = tiger) AND (s%(3, 4) = goat OR s%(3, 4) = tiger) AND (s%(3, 3) =  _
goat OR s%(3, 3) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) AND (s%(5, 3) = goat OR s%(5, 3) = tiger) AND (s%(4, 5) = goat OR s%(4, 5) = tiger) AND (s%(5, 5) = goat OR s%(5, 5) = tiger) THEN
 tm%(3, 5) = cannotmove
ELSE tm%(3, 5) = move
END IF
's%(4,5)
IF s%(4, 5) = tiger AND (s%(5, 5) = goat OR s%(5, 5) = tiger) AND (s%(3, 5) = goat OR s%(3, 5) = tiger) AND (s%(2, 5) = goat OR s%(2, 5) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) AND (s%(4, 3) = goat OR s%(4, 3) = tiger) THEN
 tm%(4, 5) = cannotmove
ELSE tm%(4, 5) = move
END IF
's%(5,5)
IF s%(5, 5) = tiger AND (s%(4, 5) = goat OR s%(4, 5) = tiger) AND (s%(3, 5) = goat OR s%(3, 5) = tiger) AND (s%(4, 4) = goat OR s%(4, 4) = tiger) AND (s%(3, 3) = goat OR s%(3, 3) = tiger) AND (s%(5, 4) = goat OR s%(5, 4) = tiger) AND (s%(5, 3) =  _
goat OR s%(5, 3) = tiger) THEN
 tm%(5, 5) = cannotmove
ELSE tm%(5, 5) = move
END IF
FOR i = 1 TO 5
 FOR j = 1 TO 5
  IF tm%(j, i) = cannotmove THEN tigernotmove = tigernotmove + 1
 NEXT j
NEXT i
IF tigernotmove = 4 THEN
 LOCATE 23, 20: PRINT "GOAT WIN THE GAME [PRESS SPACE BAR TO RETURN MENU]"
 gotomenu = 1
  DO
  xmmn0$ = INKEY$
  IF xmmn0$ = CHR$(32) THEN GOTO start
  LOOP
END IF
tigernotmove = 0
RETURN
menuchoose:
'==============main menu
valu = menuss!
IF valu = 1 THEN 'new game
 valu1 = choosers!
 IF valu1 = 1 THEN 'FOR GOAT AND TIGER [USER]
  computer = false: enter = 0
  dialoge (1)
    goatname1$ = getnam$(-6, 0, 30)
    IF goatname1$ = "0" THEN GOTO menuchoose
  goatname2$ = getnam$(-6, 1, 30)
    IF goatname2$ = "0" THEN GOTO menuchoose
 END IF
IF valu1 = 2 THEN
  computer = true: enter = 0
  dialoge (2)
  goatname1$ = getnam$(-6, 0, 30)
  IF goatname1$ = "0" THEN GOTO menuchoose
  goatname2$ = "Computer"
 END IF
  IF valu1 = 0 THEN valu = 0: GOTO menuchoose
END IF
IF valu = 2 THEN 'load game
 'load file dialoge
  dialoge (3)
   loadfilee$ = getnam$(-8, 0, 30)
   IF loadfilee$ = "0" THEN GOTO menuchoose
   loadtemp (loadfilee$)
   GOSUB deleteundof
   GOSUB undoo
   GOTO backtobagchal
END IF

IF valu = 3 AND gotomenu = 0 THEN 'save game
GOTO menuchoose
END IF
IF valu = 3 AND gotomenu = 1 THEN 'save game
 'save file dialog
   dialoge (4)
  savefilee$ = getnam$(-8, 0, 30)
   IF savefilee$ = "0" THEN GOTO menuchoose
   savetemp (savefilee$)
   GOTO menuchoose
END IF
IF valu = 4 AND gotomenu = 0 THEN 'back to bagchal
 GOTO menuchoose
END IF
IF valu = 4 AND gotomenu = 1 THEN 'back to bagchal
 backbagchal
 IF enter = 2 THEN LOCATE 25, 10: PRINT "tiger ("; goatname2$; " -> turn !)"
 GOTO backtobagchal
ad:
END IF

'clear
IF valu = 5 AND gotomenu = 1 THEN
GOSUB clearundo
tigernotmove = 0
goatskill = 0
enter = 0
RETURN
END IF

IF valu = 5 AND gotomenu = 0 THEN
 GOTO menuchoose
END IF

'swap
IF valu = 6 AND gotomenu = 0 AND computer = false THEN
'error message
GOTO menuchoose
END IF
IF valu = 6 AND gotomenu = 0 AND computer = true THEN
'error message
GOTO menuchoose
END IF
IF valu = 6 AND gotomenu = 1 AND computer = true THEN
'error message
GOTO menuchoose
END IF
IF valu = 6 AND gotomenu = 1 AND computer = false AND (goatskill = 6 OR tigernotmove = 4) THEN
'error message
GOTO menuchoose
END IF
IF valu = 6 AND gotomenu = 1 AND computer = false THEN
'swap player name
 sg$ = goatname1$
 goatname1$ = goatname2$
 goatname2$ = sg$
 backbagchal
 GOTO backtobagchal
END IF
IF valu = 7 THEN
IF undono - 1 = -1 THEN END
GOSUB deleteundof
END
END IF
RETURN
'error-trapping
fileerr:
dialoge (5)
RESUME start

undoo:
savetemp LTRIM$("undo" + (LTRIM$(STR$(undono))))
undono = undono + 1
totalundo = undono - 1
RETURN

undoopen:
undono = undono - 1
IF undono = 0 THEN
 loadtemp (LTRIM$("undo" + (LTRIM$(STR$(undono)))))
 undono = 1
 RETURN
END IF
IF enter = 0 AND computer = 1 THEN undono = undono - 1
loadtemp (LTRIM$("undo" + (LTRIM$(STR$(undono - 1)))))
RETURN

redoopen:
undono = undono + 1
IF undono = totalundo + 2 THEN
 loadtemp (LTRIM$("undo" + (LTRIM$(STR$(totalundo)))))
 undono = totalundo + 1
 RETURN
END IF
IF enter = 0 AND computer = 1 THEN undono = undono + 1
loadtemp (LTRIM$("undo" + (LTRIM$(STR$(undono - 1)))))
RETURN

deleteundof:
FOR delle = 0 TO totalundo
SHELL "del " + LTRIM$("undo" + LTRIM$(STR$(delle)) + ".sav")
NEXT delle
undono = 0
RETURN

clearundo:
FOR delle = 0 TO totalundo + 1
SHELL "del " + LTRIM$("undo" + LTRIM$(STR$(delle)) + ".sav")
NEXT delle
RETURN

SUB backbagchal
DIM i AS INTEGER
DIM j AS INTEGER
DIM xxa AS INTEGER
DIM yya AS INTEGER
board
FOR i = 1 TO 5
 FOR j = 1 TO 5
 IF s%(j, i) = tiger THEN
 xxa = j
 yya = i
'============put 4 tiger on the baghchal board
 IF xxa = 1 THEN xa = 50: GOTO tx
 IF xxa = 2 THEN xa = 130: GOTO tx
 IF xxa = 3 THEN xa = 210: GOTO tx
 IF xxa = 4 THEN xa = 290: GOTO tx
 IF xxa = 5 THEN xa = 370
tx:
 IF yya = 1 THEN ya = 3: GOTO ty
 IF yya = 2 THEN ya = 73: GOTO ty
 IF yya = 3 THEN ya = 143: GOTO ty
 IF yya = 4 THEN ya = 213: GOTO ty
 IF yya = 5 THEN ya = 283
ty:
 PUT (xa, ya), B%(1)  'tiger
END IF
IF s%(j, i) = goat THEN
 xxa = j
 yya = i
'============put 4 tiger on the baghchal board
 IF xxa = 1 THEN xa = 50: GOTO ttx
 IF xxa = 2 THEN xa = 130: GOTO ttx
 IF xxa = 3 THEN xa = 210: GOTO ttx
 IF xxa = 4 THEN xa = 290: GOTO ttx
 IF xxa = 5 THEN xa = 370
ttx:
 IF yya = 1 THEN ya = 3: GOTO tty
 IF yya = 2 THEN ya = 73: GOTO tty
 IF yya = 3 THEN ya = 143: GOTO tty
 IF yya = 4 THEN ya = 213: GOTO tty
 IF yya = 5 THEN ya = 283
tty:
PUT (xa, ya), B%(1000)  'goat
END IF
NEXT j
NEXT i

END SUB

SUB blank
'DIM i AS INTEGER
'FOR i = 0 TO 24
'LOCATE 1 + i, 1: COLOR 0, 0: PRINT "                                                                                                                                                                     "
'NEXT i
COLOR 0, 0: PRINT CHR$(12)
END SUB

SUB board
DIM fcol AS INTEGER
DIM nnn AS INTEGER
DIM i AS INTEGER
SCREEN 12
fcol = 12
nnn = 15
FOR i = 1 TO 5'x
 LINE (80 * i, 50 - nnn)-(80 * i, 50 - nnn + (70 * 4)), fcol
NEXT i
FOR i = 0 TO 4'y
 LINE (80, 50 - nnn + (70 * i))-(400, 50 - nnn + (70 * i)), fcol
NEXT i
LINE (80, 50 - nnn)-(400, 330 - nnn), fcol
LINE (80, 190 - nnn)-(240, 330 - nnn), fcol
LINE (240, 50 - nnn)-(400, 190 - nnn), fcol
LINE (80, 190 - nnn)-(240, 50 - nnn), fcol
LINE (80, 330 - nnn)-(400, 50 - nnn), fcol
LINE (240, 330 - nnn)-(400, 190 - nnn), fcol
LOCATE 28, 20: PRINT "This programe is design by MAHESH MAN NAKARMI"
END SUB

FUNCTION choosers!
DIM cxmmn$
DIM player AS INTEGER
blank
COLOR 15, 10
LOCATE 5 + 6, 30: PRINT "ษออออออออออออออออออออออออออออป"
LOCATE 5 + 7, 30: PRINT "บ       Choose to play       บ"
LOCATE 5 + 8, 30: PRINT "บ                            บ"
LOCATE 5 + 9, 30: PRINT "บ                            บ"
LOCATE 5 + 10, 30: PRINT "ศออออออออออออออออออออออออออออผ"
LOCATE 5 + 8, 30 + 2: PRINT "1 - Human and Human Player"
LOCATE 5 + 9, 30 + 2: PRINT "2 - Human and Computer    "
COLOR 15, 1: LOCATE 5 + 8, 30 + 2: PRINT "1 - Human and Human Player"
player = 1
DO
cxmmn$ = INKEY$
IF cxmmn$ = CHR$(27) THEN choosers! = 0: EXIT FUNCTION
IF cxmmn$ = CHR$(13) THEN GOTO runn

IF cxmmn$ = "1" OR cxmmn$ = "8" OR cxmmn$ = CHR$(0) + CHR$(72) THEN
 player = 1
 COLOR 15, 1: LOCATE 13, 30 + 2:  PRINT "1 - Human and Human Player"
 COLOR 15, 10: LOCATE 14, 30 + 2: PRINT "2 - Human and Computer    "
END IF
IF cxmmn$ = "2" OR cxmmn$ = CHR$(0) + CHR$(80) THEN
 player = 2
 COLOR 15, 10: LOCATE 13, 30 + 2: PRINT "1 - Human and Human Player"
 COLOR 15, 1: LOCATE 14, 30 + 2: PRINT "2 - Human and Computer    "
END IF
LOOP
runn:
IF player = 1 THEN choosers! = 1: EXIT FUNCTION
IF player = 2 THEN choosers! = 2: EXIT FUNCTION
END FUNCTION

SUB cursor
 PUT (x, y), a%(1)
 PUT (x - 2, y - 2), a%(1000)'cursor end
END SUB

SUB defvariable
move = 0
cannotmove = 1
goatturn = 0
tigerturn = 0
goatskill = 0
space = 0
tiger = 1
goat = 2
goats = 1
totalgoats = 20
storing
s%(1, 1) = tiger
s%(5, 5) = tiger
s%(5, 1) = tiger
s%(1, 5) = tiger
END SUB

SUB delcursor
PUT (getx, gety), a%(1)'delete goat cursor
PUT (getx - 2, gety - 2), a%(1000)'cursor
END SUB

SUB deltigercursor
PUT (xf, yf), a%(1)'delete tiger cursor
PUT (xf - 2, yf - 2), a%(1000)'cursor
END SUB

SUB dialoge (pp AS INTEGER)
DIM zq AS INTEGER
DIM plc AS INTEGER
SCREEN 0
blank
zq = 24'x
plc = 10'y
COLOR 15, 10
IF pp = 1 THEN GOTO gtg
IF pp = 2 THEN GOTO gcg
IF pp = 3 THEN GOTO lodf
IF pp = 4 THEN GOTO savf
IF pp = 5 THEN GOTO errd

gtg:
ere
LOCATE plc + 2, zq: PRINT "บ         Enter your name        บ"
LOCATE plc + 3, zq: PRINT "บ Goat   :                       บ"
LOCATE plc + 4, zq: PRINT "บ Tigher :                       บ"
ere
GOTO ene
gcg:
ere
LOCATE plc + 2, zq: PRINT "บ         Enter your name        บ"
LOCATE plc + 3, zq: PRINT "บ Goat   :                       บ"
LOCATE plc + 4, zq: PRINT "บ Tigher : Computer              บ"
ere
GOTO ene

lodf:
ere
LOCATE plc + 2, zq: PRINT "บ         Load your file         บ"
LOCATE plc + 3, zq: PRINT "บ FILE :                         บ"
LOCATE plc + 4, zq: PRINT "บ Without .sav                   บ"
ere
GOTO ene

savf:
ere
LOCATE plc + 2, zq: PRINT "บ         Save your file         บ"
LOCATE plc + 3, zq: PRINT "บ FILE :                         บ"
LOCATE plc + 4, zq: PRINT "บ Without .sav                   บ"
ere
GOTO ene

errd:
ere
LOCATE plc + 2, zq: PRINT "บ           file error           บ"
LOCATE plc + 3, zq: PRINT "บ         Bad File Name          บ"
LOCATE plc + 4, zq: PRINT "บ Without .sav / check your file บ"
ere
GOTO ene

ene:
END SUB

SUB dkillgoats (killg AS INTEGER)
SELECT CASE killg
 CASE 1
  kgd 1
 CASE 2
  kgd 1: kgd 2
 CASE 3
  kgd 1: kgd 2: kgd 3
CASE 4
  kgd 1: kgd 2: kgd 3: kgd 4
CASE 5
  kgd 1: kgd 2: kgd 3: kgd 4: kgd 5
 CASE 6
  kgd 1: kgd 2: kgd 3: kgd 4: kgd 5
END SELECT
END SUB

SUB ere
LOCATE 11, 24: PRINT "ษออออออออออออออออออออออออออออออออป"
LOCATE 15, 24: PRINT "ศออออออออออออออออออออออออออออออออผ"
END SUB

FUNCTION getnam$ (getx AS INTEGER, gety AS INTEGER, blnkcol AS INTEGER)
DIM gname$(20)
DIM z AS INTEGER
DIM gxx$
DIM i AS INTEGER
DIM C$

LOCATE 13 + gety, 41 + getx: COLOR blnkcol, 10: PRINT "_"
z = 0
DO
gg:
gxx$ = INPUT$(1)
IF gxx$ = "0" OR gxx$ = "1" OR gxx$ = "2" OR gxx$ = "3" OR gxx$ = "4" OR gxx$ = "5" OR gxx$ = "6" OR gxx$ = "7" OR gxx$ = "8" OR gxx$ = "9" THEN GOTO gg
IF gxx$ = CHR$(27) THEN getnam$ = "0": GOTO esc
IF gxx$ = CHR$(13) THEN GOTO gxxnex
IF gxx$ = CHR$(8) THEN 'back space
IF z = 0 THEN z = 0: GOTO gg
 z = z - 1
 gname$(z + 1) = " "
 LOCATE 13 + gety, 41 + z + getx: COLOR blnkcol, 10: PRINT "_"
 LOCATE 13 + gety, 41 + z - 1 + getx: COLOR 15, 10: PRINT ""
 LOCATE 13 + gety, 41 + z + 1 + getx: COLOR blnkcol, 10: PRINT " "
 GOTO gg
END IF
z = z + 1
IF z > 20 THEN z = 20: GOTO gg
gname$(z) = gxx$
LOCATE 13 + gety, 41 + z + getx: COLOR blnkcol, 10: PRINT "_"
LOCATE 13 + gety, 41 + z - 1 + getx: COLOR 15, 10: PRINT gxx$
LOOP
gxxnex:
LOCATE 13 + gety, 41 + z + getx: COLOR blnkcol, 10: PRINT " "
COLOR 7, 0
FOR i = 1 TO 20
C$ = C$ + gname$(i)
NEXT i
getnam$ = LTRIM$(C$)
IF LTRIM$(C$) = "" THEN LOCATE 13 + gety, 41 + z + getx: COLOR blnkcol, 10: PRINT "_": GOTO gg
esc:
END FUNCTION

SUB gettgs
'=======================cursor4, tiger and goat design==========================
LINE (50, 50 - 30)-(110, 50 + 30), 9, B
GET (50, 50 - 30)-(110, 50 + 30), a%(1)'small cursor
CLS
LINE (48, 18)-(112, 82), 10, B
GET (48, 18)-(112, 82), a%(1000)'big cursor
CLS

'tiger
LINE (33, 40)-(33, 25), 6
CIRCLE (50, 50), 20, 6, 5.5, .5
CIRCLE (50, 50), 20, 6, 2.6, 4
LINE (62, 69)-(64, 65), 6
LINE (64 - 28, 65)-(62 - 24, 69), 6
CIRCLE (50, 50), 23, 6, 4.2, 5.2
LINE (33, 25)-(33 + 17, 40), 6
LINE (33 + 17, 40)-(67, 25), 6
LINE (67, 40)-(67, 25), 6
LINE (35, 33)-(35 + 1, 39), 8
LINE (38, 36)-(38 + 1, 33), 8
LINE (33 + 21, 40)-(60, 35), 8
LINE (39 + 21, 39)-(62, 34), 8
LINE (42 + 21, 39)-(62, 34), 8
CIRCLE (42, 50), 2, 0
CIRCLE (59, 50), 2, 0
CIRCLE (42, 50), 1, 15
CIRCLE (42, 50), 0, 15
CIRCLE (59, 50), 1, 15
CIRCLE (59, 50), 0, 15
CIRCLE (42, 50), 4, 0, 3.2, .011
CIRCLE (59, 50), 4, 0, 3.2, .011
PSET (47, 58), 4
PSET (53, 58), 4
PSET (50, 60), 4
LINE (5 + 39, 10 + 53)-(17 + 39, 13 + 53), 4, BF
LINE (45 - 1, 63)-(46 - 1, 64), 15
LINE (46 - 1, 64)-(45 - 1, 66), 15
LINE (57 - 1, 63)-(56 - 1, 64), 15
LINE (56 - 1, 65)-(57 - 1, 66), 15
PSET (54 - 1, 64), 15
PSET (52 - 1, 64), 15
PSET (50 - 1, 64), 15
PSET (48 - 1, 64), 15
PSET (54 - 1, 65), 15
PSET (52 - 1, 65), 15
PSET (50 - 1, 65), 15
PSET (48 - 1, 65), 15
LINE (47 - 1, 58)-(30, 57), 14
LINE (47 - 1, 58)-(23, 62), 14
LINE (47 - 1, 58)-(27, 65), 14
LINE (53 - 1, 58)-(53 + 17, 57), 14
LINE (53 - 1, 58)-(53 + 24, 62), 14
LINE (53 - 1, 58)-(53 + 20, 65), 14

GET (50 - 29, 50 - 29)-(50 + 29, 50 + 29), B%(1)'tiger
CLS

'goat
LINE (33, 40)-(33, 25), 7
COLOR 7
PSET (33, 40)
DRAW "l3g6d8g2d5f2r5h2u4e3"
COLOR 7
PSET (67, 40)
DRAW "r3f6d8f2d5g2l5e2u4h3"
LINE (33, 25)-(33 + 10, 40), 7
LINE (33 + 10, 40)-(33 + 24, 40), 7
LINE (33 + 24, 40)-(67, 25), 7
LINE (67, 40)-(67, 25), 7
CIRCLE (50, 50), 20, 7, 5.5, .5
CIRCLE (50, 50), 20, 7, 2.6, 4
LINE (62, 69)-(64, 65), 7
LINE (64 - 28, 65)-(62 - 24, 69), 7
CIRCLE (42, 53), 4, 7
CIRCLE (59, 53), 4, 7
CIRCLE (42, 54), 1, 15
CIRCLE (59, 54), 1, 15
LINE (39, 69)-(42, 75), 7
LINE (42, 75)-(45, 77), 7
LINE (45, 77)-(55, 77), 7
LINE (47, 72)-(53, 72), 7
LINE (44, 67)-(47, 67), 7
LINE (55, 67)-(52, 67), 7
LINE (55, 67)-(55, 69), 7
CIRCLE (51, 46), 6, 7, , , 6
LINE (44, 67)-(44, 69), 7
LINE (61, 69)-(58, 75), 7
LINE (58, 75)-(55, 77), 7

GET (50 - 29, 50 - 29)-(50 + 29, 50 + 29), B%(1000)'goat
CLS
END SUB

SUB kgd (kx AS INTEGER)
IF kx = 1 THEN PUT (480, 50), B%(1000), OR   'goat1
IF kx = 2 THEN PUT (550, 50), B%(1000), OR 'goat2
IF kx = 3 THEN PUT (480, 120), B%(1000), OR'goat3
IF kx = 4 THEN PUT (550, 120), B%(1000), OR 'goat4
IF kx = 5 THEN PUT (480, 190), B%(1000), OR 'goat5
IF kx = 6 THEN PUT (550, 190), B%(1000), OR 'goat6
END SUB

SUB loadtemp (qty$)
DIM bget$(100)
DIM i AS INTEGER
DIM aaaa$
DIM xxa AS INTEGER
DIM yya AS INTEGER

CLS
storing
'storing space on the board
board

OPEN "I", #1, qty$ + ".sav"'"c:\bagchal.sav"
i = 1

DO WHILE NOT EOF(1)
INPUT #1, bget$(i)
i = i + 1
LOOP
CLOSE #1

aaaa$ = bget$(1)
goatname1$ = bget$(2)
goatname2$ = bget$(3)
computer = VAL(bget$(4))
move = VAL(bget$(5))
cannotmove = VAL(bget$(6))
goatturn = VAL(bget$(7))
tigerturn = VAL(bget$(8))
goatskill = VAL(bget$(9))
space = VAL(bget$(10))
tiger = VAL(bget$(11))
goat = VAL(bget$(12))
goats = VAL(bget$(13))
totalgoats = VAL(bget$(14))
x = VAL(bget$(15))
y = VAL(bget$(16))
xx = VAL(bget$(17))
yy = VAL(bget$(18))
enter = VAL(bget$(19))
IF enter = 2 THEN LOCATE 25, 10: PRINT "tiger ("; goatname2$; " -> turn !)"

FOR i = 1 TO 85'80
IF RIGHT$(bget$(i), 5) = "space" THEN
 'PRINT bget$(i)
 xxa = VAL(LEFT$(bget$(i), 1))
 yya = VAL(MID$(bget$(i), 3, 2))
 s%(xxa, yya) = 0'space
END IF
IF RIGHT$(bget$(i), 5) = "tiger" THEN
 'PRINT bget$(i)
 xxa = VAL(LEFT$(bget$(i), 1))
 yya = VAL(MID$(bget$(i), 5, 1))
 s%(xxa, yya) = 1'tiger
'============put 4 tiger on the baghchal board
 IF xxa = 1 THEN xa = 50: GOTO tnextx
 IF xxa = 2 THEN xa = 130: GOTO tnextx
 IF xxa = 3 THEN xa = 210: GOTO tnextx
 IF xxa = 4 THEN xa = 290: GOTO tnextx
 IF xxa = 5 THEN xa = 370
tnextx:
 IF yya = 1 THEN ya = 3: GOTO tnexty
 IF yya = 2 THEN ya = 73: GOTO tnexty
 IF yya = 3 THEN ya = 143: GOTO tnexty
 IF yya = 4 THEN ya = 213: GOTO tnexty
 IF yya = 5 THEN ya = 283
tnexty:
 PUT (xa, ya), B%(1)  'tiger
END IF

IF RIGHT$(bget$(i), 4) = "goat" THEN
 'PRINT bget$(i)
 xxa = VAL(LEFT$(bget$(i), 1))
 yya = VAL(MID$(bget$(i), 5, 1))
 s%(xxa, yya) = 2'tiger
 '============put 4 tiger on the baghchal board
 IF xxa = 1 THEN xa = 50: GOTO gnextx
 IF xxa = 2 THEN xa = 130: GOTO gnextx
 IF xxa = 3 THEN xa = 210: GOTO gnextx
 IF xxa = 4 THEN xa = 290: GOTO gnextx
 IF xxa = 5 THEN xa = 370
gnextx:
 IF yya = 1 THEN ya = 3: GOTO gnexty
 IF yya = 2 THEN ya = 73: GOTO gnexty
 IF yya = 3 THEN ya = 143: GOTO gnexty
 IF yya = 4 THEN ya = 213: GOTO gnexty
 IF yya = 5 THEN ya = 283
gnexty:
   PUT (xa, ya), B%(1000)  'goat
END IF

NEXT i

'tigernotmove
FOR i = 1 TO 85
IF RIGHT$(bget$(i), 4) = "move" THEN
  'PRINT bget$(i)
  xxa = VAL(LEFT$(bget$(i), 1))
  yya = VAL(MID$(bget$(i), 5, 1))
  tm%(xxa, yya) = move
 END IF
IF RIGHT$(bget$(i), 10) = "cannotmove" THEN
  'PRINT bget$(i)
  xxa = VAL(LEFT$(bget$(i), 1))
  yya = VAL(MID$(bget$(i), 3, 2))
  tm%(xxa, yya) = cannotmove
END IF
NEXT i
END SUB

FUNCTION menuss!
DIM menu(8) AS STRING
DIM mes1(3) AS STRING
DIM mes2(10) AS STRING
DIM pp AS INTEGER
DIM i AS INTEGER
DIM mm AS INTEGER
DIM lenn AS INTEGER
DIM xmmn$
mes1(1) = "MAHESH MAN NAKARMI, Brahma Tole, Kathmandu, Nepal       "
mes1(2) = "MAHESH MAN NAKARMI, Brahma Tole, Kathmandu, Nepal "

mes2(1) = "          M A H E S H  M A N  N A K A R M I           "
mes2(2) = "               -: P R E S E N T S :-                  "
mes2(3) = "ÛÛÛÞÞÛÜ  ÜÛÛÛÛÛÜ  ÜÛÛÛÛÛ  ÜÛÛÛÛ ÛÛ  ÛÛ ÜÛÛÛÛÛÜ ÛÛ     "
mes2(4) = "ÛÛ    ÛÛ ÛÛ   ÛÛ ÛÛ   ÛÛ ÛÛ     ÛÛ  ÛÛ ÛÛ   ÛÛ ÛÛ     "
mes2(5) = "ÛÛ    ÛÛ ÛÛ   ÛÛ ÛÛ  ฿฿  ÛÛ     ÛÛ  ÛÛ ÛÛ   ÛÛ ÛÛ     "
mes2(6) = "ÛÛÞÞÞÛÛ  ÛÛÛÛÛÛÛ ÛÛ      ÛÛ     ÛÛÛÛÛÛ ÛÛÛÛÛÛÛ ÛÛ     "
mes2(7) = "ÛÛ    ÛÛ ÛÛ   ÛÛ ÛÛ      ÛÛ     ÛÛ  ÛÛ ÛÛ   ÛÛ ÛÛ     "
mes2(8) = "ÛÛ    ÛÛ ÛÛ   ÛÛ ÛÛ ฿฿ÛÛ ÛÛ     ÛÛ  ÛÛ ÛÛ   ÛÛ ÛÛ     "
mes2(9) = "ÛÛÛÞÞÛ฿  ÛÛ   ÛÛ  ฿ÛÛÛÛÛ  ฿ÛÛÛÛ ÛÛ  ÛÛ ÛÛ   ÛÛ ÛÛÛÛÛÛ "

top:
COLOR 0, 0
LOCATE 1, 1: COLOR 7, 0: PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
LOCATE 2, 1:  PRINT "บGood Evening  Welcome To The Most Popular National Game Of Nepal Date:05-02-16บ"
LOCATE 3, 1:  PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"

LOCATE 5, 1:  PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออป ษออออออออออออออออออออป"
LOCATE 6, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 7, 1:  PRINT "บ                                                       บ บ    MAIN  MENU    บ"
LOCATE 8, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 9, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 10, 1: PRINT "บ                                                       บ บ                    บ"
LOCATE 11, 1: PRINT "บ                                                       บ บ                    บ"
LOCATE 12, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 13, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 14, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 15, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 16, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 17, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 18, 1:  PRINT "บ                                                       บ บ                    บ"
LOCATE 19, 1:  PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออผ ศออออออออออออออออออออผ"

LOCATE 21, 1: PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
LOCATE 22, 1: PRINT "บ     Programmed By:-                                                          บ"
LOCATE 23, 1: PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"

menu(1) = "N - New Game       "
menu(2) = "L - Load Old Game  "
menu(3) = "S - Save This Game "
menu(4) = "B - Back To Bagchal"
menu(5) = "C - Clear Board    "
menu(6) = "W - Swap Players   "
menu(7) = "X - Exit To Dos    "
FOR i = 1 TO 7
LOCATE 8 + i, 61: COLOR 7, 0: PRINT menu(i)
NEXT i
LOCATE 9, 61: COLOR 7, 1: PRINT menu(1)

mm = 1
lenn = LEN(mes1(1))
ii = 1
pp = 1
DO

xmmn$ = INKEY$
ii = ii + 1
IF xmmn$ = CHR$(13) THEN GOTO choose
IF ii = LEN(mes1(1)) THEN
 ii = 1
 pp = pp + 1
 IF pp = 3 THEN pp = 1
END IF
IF pp = 1 THEN
 LOCATE 22, 80 - ii: COLOR 4, 0
 speed
 PRINT LEFT$(mes1(1), ii)
 LOCATE 7, 2: COLOR 4, 0: PRINT MID$(mes2(1), ii, LEN(mes2(1)))
 LOCATE 9, 2: COLOR 4, 0: PRINT MID$(mes2(2), ii, LEN(mes2(2)))
 LOCATE 11, 2: COLOR 4, 0: PRINT MID$(mes2(3), ii, LEN(mes2(3)))
 LOCATE 12, 2: COLOR 4, 0: PRINT MID$(mes2(4), ii, LEN(mes2(4)))
 LOCATE 13, 2: COLOR 4, 0: PRINT MID$(mes2(5), ii, LEN(mes2(5)))
 LOCATE 14, 2: COLOR 4, 0: PRINT MID$(mes2(6), ii, LEN(mes2(6)))
 LOCATE 15, 2: COLOR 4, 0: PRINT MID$(mes2(7), ii, LEN(mes2(7)))
 LOCATE 16, 2: COLOR 4, 0: PRINT MID$(mes2(8), ii, LEN(mes2(8)))
 LOCATE 17, 2: COLOR 4, 0: PRINT MID$(mes2(9), ii, LEN(mes2(9)))
END IF
IF pp = 2 THEN
 LOCATE 22, 24: COLOR 4, 0:
 speed
 PRINT MID$(mes1(2), ii, LEN(mes1(2)));
 LOCATE 7, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(1), ii);
 LOCATE 9, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(2), ii);
 LOCATE 11, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(3), ii);
 LOCATE 12, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(4), ii);
 LOCATE 13, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(5), ii);
 LOCATE 14, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(6), ii);
 LOCATE 15, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(7), ii);
 LOCATE 16, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(8), ii);
 LOCATE 17, 57 - ii: COLOR 4, 0: PRINT LEFT$(mes2(9), ii);
END IF


IF xmmn$ = "n" OR xmmn$ = "N" THEN
 LOCATE 8 + mm, 61: COLOR 7, 0: PRINT menu(mm)
 LOCATE 8 + 1, 61: COLOR 7, 1: PRINT menu(1)
 mm = 1
END IF
IF xmmn$ = "l" OR xmmn$ = "L" THEN
 LOCATE 8 + mm, 61: COLOR 7, 0: PRINT menu(mm)
 LOCATE 8 + 2, 61: COLOR 7, 1: PRINT menu(2)
 mm = 2
END IF
IF xmmn$ = "s" OR xmmn$ = "S" THEN
 LOCATE 8 + mm, 61: COLOR 7, 0: PRINT menu(mm)
 LOCATE 8 + 3, 61: COLOR 7, 1: PRINT menu(3)
 mm = 3
END IF
IF xmmn$ = "b" OR xmmn$ = "B" THEN
 LOCATE 8 + mm, 61: COLOR 7, 0: PRINT menu(mm)
 LOCATE 8 + 4, 61: COLOR 7, 1: PRINT menu(4)
 mm = 4
END IF
IF xmmn$ = "c" OR xmmn$ = "C" THEN
 LOCATE 8 + mm, 61: COLOR 7, 0: PRINT menu(mm)
 LOCATE 8 + 5, 61: COLOR 7, 1: PRINT menu(5)
 mm = 5
END IF
IF xmmn$ = "w" OR xmmn$ = "W" THEN
 LOCATE 8 + mm, 61: COLOR 7, 0: PRINT menu(mm)
 LOCATE 8 + 6, 61: COLOR 7, 1: PRINT menu(6)
 mm = 6
END IF
IF xmmn$ = "x" OR xmmn$ = "X" THEN
 LOCATE 8 + mm, 61: COLOR 7, 0: PRINT menu(mm)
 LOCATE 8 + 7, 61: COLOR 7, 1: PRINT menu(7)
 mm = 7
END IF

IF xmmn$ = "2" OR xmmn$ = CHR$(0) + CHR$(80) THEN
 IF mm = 7 THEN LOCATE 8 + 7, 61: COLOR 7, 0: PRINT menu(mm): mm = 0
IF mm < 7 THEN
  mm = mm + 1
  LOCATE 8 + mm - 1, 61: COLOR 7, 0: PRINT menu(mm - 1)
  LOCATE 8 + mm, 61: COLOR 7, 1: PRINT menu(mm)
 END IF
END IF
IF xmmn$ = "8" OR xmmn$ = CHR$(0) + CHR$(72) THEN
 IF mm = 1 THEN
  LOCATE 9, 61: COLOR 7, 0: PRINT menu(mm)
  mm = 8
 END IF
 IF mm > 1 THEN
  mm = mm - 1
  LOCATE 8 + mm + 1, 61: COLOR 7, 0: PRINT menu(mm + 1)
  LOCATE 8 + mm, 61: COLOR 7, 1: PRINT menu(mm)
 END IF
END IF
LOOP
choose:
IF mm = 1 OR xmmn$ = "n" OR xmmn$ = "N" THEN menuss! = 1: EXIT FUNCTION
IF mm = 2 OR xmmn$ = "l" OR xmmn$ = "L" THEN menuss! = 2: EXIT FUNCTION
IF mm = 3 OR xmmn$ = "s" OR xmmn$ = "S" THEN menuss! = 3: EXIT FUNCTION
IF mm = 4 OR xmmn$ = "b" OR xmmn$ = "B" THEN menuss! = 4: EXIT FUNCTION
IF mm = 5 OR xmmn$ = "c" OR xmmn$ = "C" THEN menuss! = 5: EXIT FUNCTION
IF mm = 6 OR xmmn$ = "w" OR xmmn$ = "W" THEN menuss! = 6: EXIT FUNCTION
IF mm = 7 OR xmmn$ = "x" OR xmmn$ = "X" THEN
 menuss! = 7
 EXIT FUNCTION
END IF
END FUNCTION

SUB puttiger
PUT (50, 3), B%(1)   'tiger
PUT (50 + 80 * 4, 3), B%(1)  'tiger
PUT (50 + 80 * 4, 3 + 70 * 4), B%(1) 'tiger
PUT (50, 3 + 70 * 4), B%(1)   'tiger
END SUB

SUB savetemp (trt$)
DIM i AS INTEGER
DIM j AS INTEGER
DIM uuo$

OPEN "O", #1, trt$ + ".sav"
PRINT #1, "Do not edit | Creted by Bagchal | Date : "; DATE$; "| time : "; TIME$'1
PRINT #1, goatname1$
PRINT #1, goatname2$
PRINT #1, computer
PRINT #1, move
PRINT #1, cannotmove
PRINT #1, goatturn
PRINT #1, tigerturn
PRINT #1, goatskill
PRINT #1, space
PRINT #1, tiger
PRINT #1, goat
PRINT #1, goats
PRINT #1, totalgoats
PRINT #1, x
PRINT #1, y
PRINT #1, xx
PRINT #1, yy
PRINT #1, enter
's%(j, i) = space
FOR i = 1 TO 5
 FOR j = 1 TO 5
  IF s%(j, i) = 0 THEN
   uuo$ = STR$(j) + " ;" + STR$(i) + " = space"
   PRINT #1, LTRIM$(uuo$)
  END IF
 
  IF s%(j, i) = 1 THEN
   uuo$ = STR$(j) + " ;" + STR$(i) + " = tiger"
   PRINT #1, LTRIM$(uuo$)
  END IF
 
  IF s%(j, i) = 2 THEN
    uuo$ = STR$(j) + " ;" + STR$(i) + " = goat"
   PRINT #1, LTRIM$(uuo$)
  END IF

 NEXT j
NEXT i

'tm%(j, i) = move'44+1 - 44+1+25=70
FOR i = 1 TO 5
 FOR j = 1 TO 5
  IF tm%(j, i) = 0 THEN
   uuo$ = STR$(j) + " ;" + STR$(i) + " = move"
   PRINT #1, LTRIM$(uuo$)
  END IF
  IF tm%(j, i) = 1 THEN
   uuo$ = STR$(j) + " ;" + STR$(i) + " = cannotmove"
   PRINT #1, LTRIM$(uuo$)
  END IF
 NEXT j
NEXT i
CLOSE
END SUB

SUB speed
DIM et AS LONG
FOR et = 1 TO 100000: NEXT et
END SUB

SUB storing
DIM k AS INTEGER
DIM j AS INTEGER
FOR k = 1 TO 5
 FOR j = 1 TO 5
 s%(j, k) = space
 tm%(j, k) = move
 NEXT j
NEXT k
END SUB

