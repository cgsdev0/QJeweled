DECLARE SUB HyperHandle2 (x1%, y1%, mycol%)
DECLARE SUB Explosion (x%, y%)
DECLARE SUB HyperHandle (x1%, y1%, x2%, y2%)
DECLARE SUB DrawMult ()
DECLARE SUB DrawPaused ()
DECLARE SUB nou ()
DECLARE FUNCTION YouLose! ()
DECLARE SUB DrawClock ()
DECLARE SUB DrawScore ()
DECLARE SUB BenchmarkTest ()
DECLARE SUB pause ()
DECLARE SUB DropBlocks ()
DECLARE SUB SwapHandle ()
DECLARE SUB RemoveBreaks ()
DECLARE SUB ClearBreaks ()
DECLARE FUNCTION GetBreaks! ()
DECLARE FUNCTION CheckMoves! ()
DECLARE SUB SwapBlocks (x1%, y1%, x2%, y2%)
DECLARE FUNCTION CheckHor! (x%, y%)
DECLARE FUNCTION CheckVer! (x%, y%)
DECLARE SUB StartingCheck ()
DECLARE SUB RandomizeBoard ()
DECLARE SUB DrawCursor ()
DECLARE SUB EraseCursor ()
DECLARE SUB DrawGame ()
DECLARE SUB DrawBlock (x%, y%, col%)
DECLARE SUB StartScreen ()
DECLARE SUB MIDPRINT (dastring$)


REM -------------------------------NOTES----------------------------------------
REM Shane Schulte
REM Project Started: 4/27/10
REM Finished: ------
REM QJeweled
REM NOTE: I don't mind if you want to play around with
REM different constants and variables, but please do
REM not submit those cheated scores to the leaderboards.
REM This is pretty much on the honor system, but if you do,
REM You may ruin the fun for everyone else. Feel free to
REM Modify things just for fun, though.
REM ---------------------------NOTES--------------------------------------------




CLS

DIM SHARED DropSpeed
BenchmarkTest

GameStartPoint:
CLS

StartScreen

CLS

RANDOMIZE TIMER
SCREEN 9
DIM SHARED streak%
DIM SHARED lastbreak!
lastbreak! = TIMER
DIM SHARED bonus%
DIM SHARED multiplier%
DIM SHARED needhyper%
DIM SHARED needbomb%
needhyper% = 0
needbomb% = 0
multiplier% = 1
streak% = 0
bonus% = 0
CONST QUITKEY = "Q"
CONST PAUSEKEY = "P"
CONST LEFTARROW = 75
CONST RIGHTARROW = 77
CONST UPARROW = 72
CONST DOWNARROW = 80
CONST SPACEBAR = 32
CONST BlockSize = 20
CONST BlockSep = 6
CONST BlockOutline = 3
CONST BlocksWide = 8
CONST BlocksTall = 10
CONST PLAYING = 0
CONST GAMEOVER = 1
CONST QUITTING = 2
CONST PAUSED = 3
DIM SHARED InnerColorList(5)
DIM SHARED Score#
DIM SHARED GameStatus
GameStatus = PLAYING
Score# = 0
DIM SHARED OuterColorList(5)
DIM SHARED BlockList%(BlocksWide, BlocksTall)
DIM SHARED BreakList%(BlocksWide, BlocksTall)
CONST FALSE = 0
CONST TRUE = 1
DIM SHARED CursorX%
DIM SHARED CursorY%
DIM SHARED Frozen%
DIM SHARED Clock$
DIM SHARED PauseClock!
PauseClock! = 0
DIM SHARED secs!
DIM SHARED GameTime%
GameTime% = 60
DIM SHARED StartClock!
DIM SHARED l#
Frozen% = FALSE
CALL nou


CursorX% = 0
CursorY% = 0
DIM SHARED sel
sel = FALSE

CONST FieldPosX = 350
CONST FieldPosY = 50

'Define the random colors of the playing field
CALL RandomizeBoard
CALL ClearBreaks

'0 = RED Block
InnerColorList(0) = 12
OuterColorList(0) = 4

'1 = BLUE Block
InnerColorList(1) = 9
OuterColorList(1) = 1

'2 = GREEN
InnerColorList(2) = 10
OuterColorList(2) = 2

'3 = PURPLE
InnerColorList(3) = 13
OuterColorList(3) = 5

'4 = YELLOW
InnerColorList(4) = 14
OuterColorList(4) = 6

StartClock! = TIMER + GameTime%
DrawGame
DrawCursor
DrawScore
DrawClock
DO
        IF GameStatus = PLAYING THEN
                DrawClock
        END IF
        
        IF TIMER > StartClock! + PauseClock! AND GameStatus = PLAYING THEN
                GameStatus = GAMEOVER
                GOTO EndOfLoop
        END IF
        ans$ = UCASE$(INKEY$)
        IF ans$ = PAUSEKEY AND GameStatus = PLAYING THEN GameStatus = PAUSED: p! = TIMER: Frozen% = TRUE: DrawPaused: EraseCursor: GOTO EndOfLoop
        IF ans$ = PAUSEKEY AND GameStatus = PAUSED THEN GameStatus = PLAYING: PauseClock! = PauseClock! + TIMER - p!: Frozen% = FALSE: DrawPaused: DrawGame: DrawCursor: CursorY% = CursorY% - 1
        IF ans$ = QUITKEY THEN GameStatus = QUITTING: GOTO EndOfLoop
        ans = ASC(RIGHT$(CHR$(0) + ans$, 1))
        IF Frozen% = FALSE THEN
        IF sel = FALSE THEN
                SELECT CASE ans
                        CASE LEFTARROW
                                IF CursorX% > 0 THEN
                                        EraseCursor
                                        CursorX% = CursorX% - 1
                                        DrawCursor
                                END IF
                        CASE RIGHTARROW
                               IF CursorX% < BlocksWide - 1 THEN
                                        EraseCursor
                                        CursorX% = CursorX% + 1
                                        DrawCursor
                                END IF
                        CASE UPARROW
                                IF CursorY% > 0 THEN
                                        EraseCursor
                                        CursorY% = CursorY% - 1
                                        DrawCursor
                                END IF
                        CASE DOWNARROW
                                IF CursorY% < BlocksTall - 1 THEN
                                        EraseCursor
                                        CursorY% = CursorY% + 1
                                        DrawCursor
                                END IF
                        CASE SPACEBAR
                                sel = TRUE
                                't = TIMER
                                'CALL RandomizeBoard
                                'DrawGame
                                DrawCursor
                                'PRINT TIMER - t
                END SELECT
        ELSEIF sel = TRUE THEN
                SELECT CASE ans
                        CASE LEFTARROW
                                IF CursorX% > 0 THEN
                                        SwapBlocks CursorX%, CursorY%, CursorX% - 1, CursorY%
                                        IF BlockList%(CursorX%, CursorY%) = 6 THEN
                                                HyperHandle CursorX%, CursorY%, CursorX% - 1, CursorY%
                                        ELSEIF BlockList%(CursorX% - 1, CursorY%) = 6 THEN
                                                HyperHandle CursorX% - 1, CursorY%, CursorX%, CursorY%
                                        ELSEIF GetBreaks = TRUE THEN
                                                SwapHandle
                                        ELSE
                                                SwapBlocks CursorX%, CursorY%, CursorX% - 1, CursorY%
                                        END IF
                                END IF
                        CASE RIGHTARROW
                               IF CursorX% < BlocksWide - 1 THEN
                                        SwapBlocks CursorX%, CursorY%, CursorX% + 1, CursorY%
                                        IF BlockList%(CursorX%, CursorY%) = 6 THEN
                                                HyperHandle CursorX%, CursorY%, CursorX% + 1, CursorY%
                                        ELSEIF BlockList%(CursorX% + 1, CursorY%) = 6 THEN
                                                HyperHandle CursorX% + 1, CursorY%, CursorX%, CursorY%
                                        ELSEIF GetBreaks = TRUE THEN
                                                SwapHandle
                                        ELSE
                                                SwapBlocks CursorX%, CursorY%, CursorX% + 1, CursorY%
                                        END IF
                                END IF
                        CASE UPARROW
                                IF CursorY% > 0 THEN
                                        SwapBlocks CursorX%, CursorY%, CursorX%, CursorY% - 1
                                        IF BlockList%(CursorX%, CursorY%) = 6 THEN
                                                HyperHandle CursorX%, CursorY%, CursorX%, CursorY% - 1
                                        ELSEIF BlockList%(CursorX%, CursorY% - 1) = 6 THEN
                                                HyperHandle CursorX%, CursorY% - 1, CursorX%, CursorY%
                                        ELSEIF GetBreaks = TRUE THEN
                                                SwapHandle
                                        ELSE
                                                SwapBlocks CursorX%, CursorY%, CursorX%, CursorY% - 1
                                        END IF
                                END IF
                        CASE DOWNARROW
                                IF CursorY% < BlocksTall - 1 THEN
                                        SwapBlocks CursorX%, CursorY%, CursorX%, CursorY% + 1
                                        IF BlockList%(CursorX%, CursorY%) = 6 THEN
                                                HyperHandle CursorX%, CursorY%, CursorX%, CursorY% + 1
                                        ELSEIF BlockList%(CursorX%, CursorY% + 1) = 6 THEN
                                                HyperHandle CursorX%, CursorY% + 1, CursorX%, CursorY%
                                        ELSEIF GetBreaks = TRUE THEN
                                                SwapHandle
                                        ELSE
                                                SwapBlocks CursorX%, CursorY%, CursorX%, CursorY% + 1
                                        END IF
                                END IF
                        CASE SPACEBAR
                                sel = FALSE
                                DrawCursor
                END SELECT
        END IF
        END IF
EndOfLoop:
LOOP UNTIL GameStatus = QUITTING OR GameStatus = GAMEOVER

SELECT CASE GameStatus
        CASE QUITTING
                'I guess you could do something here... idk what though
        CASE GAMEOVER
                SCREEN 0
                IF YouLose = TRUE THEN GOTO GameStartPoint
END SELECT

END

SUB BenchmarkTest
DO
t = TIMER
FOR delay& = 0 TO 400000: NEXT delay&
t = TIMER - t
'PRINT "Your benchmarked time was"; t
LOOP UNTIL t > 0
t = INT(t * 10000)
t = INT(10000000000# * (547 / t) / 10000)
DropSpeed = t
'PRINT "Your delay is"; t
'DO
'        ckey$ = INKEY$
'LOOP UNTIL ckey$ <> ""
END SUB

FUNCTION CheckHor (x%, y%)
IF x% >= 1 AND x% <= BlocksWide - 2 AND y% >= 0 AND y% <= BlocksTall - 1 THEN
        curcol% = BlockList%(x%, y%) MOD 7
        IF BlockList%(x% - 1, y%) MOD 7 = curcol% AND BlockList%(x% + 1, y%) MOD 7 = curcol% THEN
                CheckHor = 1
        ELSE
                CheckHor = 0
        END IF
ELSE
        CheckHor = 0
END IF
END FUNCTION

FUNCTION CheckMoves
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                curcol% = BlockList%(i%, j%)
        NEXT j%
NEXT i%
END FUNCTION

FUNCTION CheckVer (x%, y%)
IF x% >= 0 AND x% <= BlocksWide - 1 AND y% >= 1 AND y% <= BlocksTall - 2 THEN
        curcol% = BlockList%(x%, y%) MOD 7
        IF BlockList%(x%, y% - 1) MOD 7 = curcol% AND BlockList%(x%, y% + 1) MOD 7 = curcol% THEN
                CheckVer = 1
        ELSE
                CheckVer = 0
        END IF
ELSE
        CheckVer = 0
END IF
END FUNCTION

SUB ClearBreaks
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                BreakList%(i%, j%) = 5
        NEXT j%
NEXT i%
END SUB

SUB DrawBlock (x%, y%, col%)
IF col% < 6 THEN
        LINE (x%, y%)-(x% + BlockSize, y% + BlockSize), OuterColorList(col%), BF
        LINE (x% + BlockOutline, y% + BlockOutline)-(x% + BlockSize - BlockOutline, y% + BlockSize - BlockOutline), InnerColorList(col%), BF
ELSEIF col% >= 7 THEN
        LINE (x%, y%)-(x% + BlockSize, y% + BlockSize), 0, BF
        LINE (x% + BlockOutline, y% + BlockOutline)-(x% + BlockSize - BlockOutline, y% + BlockSize - BlockOutline), 0, BF
        CIRCLE (x% + INT(BlockSize / 2), y% + INT(BlockSize / 2)), INT(BlockSize / 2), InnerColorList(col% MOD 7)
        PAINT (x% + INT(BlockSize / 2), y% + INT(BlockSize / 2)), InnerColorList(col% MOD 7), InnerColorList(col% MOD 7)
        CIRCLE (x% + INT(BlockSize / 2), y% + INT(BlockSize / 2)), INT(BlockSize / 2), OuterColorList(col% MOD 7)
        CIRCLE (x% + INT(BlockSize / 2), y% + INT(BlockSize / 2)), INT(BlockSize / 2) - 1, OuterColorList(col% MOD 7)
ELSE
        LINE (x%, y%)-(x% + BlockSize, y% + BlockSize), 0, BF
        LINE (x% + BlockOutline, y% + BlockOutline)-(x% + BlockSize - BlockOutline, y% + BlockSize - BlockOutline), 0, BF
        CIRCLE (x% + INT(BlockSize / 2), y% + INT(BlockSize / 2)), INT(BlockSize / 2), 15
        PAINT (x% + INT(BlockSize / 2), y% + INT(BlockSize / 2)), 15, 15
END IF
END SUB

SUB DrawClock
COLOR 15
LOCATE 11, 15
secs! = StartClock! - TIMER + PauseClock!
min$ = STR$(secs! \ 60)
sec$ = MID$(STR$(secs! MOD 60 \ 1), 2)
IF LEN(sec$) = 1 THEN sec$ = "0" + sec$
Clock$ = min$ + ":" + sec$
PRINT "Time Remaining:"; Clock$
LOCATE 12, 15
'PRINT PauseClock!
END SUB

SUB DrawCursor

col% = ((sel * 15) + 7)
x% = CursorX%
y% = CursorY%
LINE (FieldPosX + (x% * (BlockSize + BlockSep)) - 3, FieldPosY + (y% * (BlockSize + BlockSep)) - 3)-(FieldPosX + (x% * (BlockSize + BlockSep)) + BlockSize + 3, FieldPosY + (y% * (BlockSize + BlockSep)) + BlockSize + 3), col%, B
LINE (FieldPosX + (x% * (BlockSize + BlockSep)) - 2, FieldPosY + (y% * (BlockSize + BlockSep)) - 2)-(FieldPosX + (x% * (BlockSize + BlockSep)) + BlockSize + 2, FieldPosY + (y% * (BlockSize + BlockSep)) + BlockSize + 2), col%, B
END SUB

SUB DrawGame
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                DrawBlock FieldPosX + (i% * (BlockSize + BlockSep)), FieldPosY + (j% * (BlockSize + BlockSep)), BlockList%(i%, j%)
        NEXT j%
NEXT i%
END SUB

SUB DrawMult
LOCATE 3, 15
PRINT multiplier%; "x Multiplier"
END SUB

SUB DrawPaused
IF GameStatus = PAUSED THEN
LINE (FieldPosX, FieldPosY)-(FieldPosX + BlocksWide * (BlockSep + BlockSize), FieldPosY + BlocksTall * (BlockSep + BlockSize)), 0, BF
COLOR 4, 0
LOCATE 13, 15
PRINT "GAME PAUSED"
COLOR 7, 0
ELSE
COLOR 0, 0
LOCATE 13, 15
PRINT "GAME PAUSED"
COLOR 7, 0
END IF
END SUB

SUB DrawScore
LOCATE 9, 15
PRINT "Score: "; Score#
END SUB

SUB DropBlocks
EscapeVar = FALSE
DO
DO UNTIL EscapeVar = TRUE
        pause
        EscapeVar = TRUE
        Testvar = FALSE
        FOR j% = BlocksTall - 1 TO 0 STEP -1
                FOR i% = 0 TO BlocksWide - 1
                        IF j% = 0 AND BlockList%(i%, j%) = 5 THEN
                                IF needhyper% = TRUE THEN
                                        BlockList%(i%, j%) = 6
                                        needhyper% = FALSE
                                ELSE
                                        BlockList%(i%, j%) = INT(RND * 5)
                                        IF needbomb% = TRUE THEN
                                                BlockList%(i%, j%) = BlockList%(i%, j%) + 7
                                                needbomb% = FALSE
                                        END IF
                                END IF
                        ELSEIF BlockList%(i%, j%) = 5 THEN
                                EscapeVar = FALSE
                                IF BlockList%(i%, j% - 1) <> 5 THEN
                                        SwapBlocks i%, j%, i%, j% - 1
                                        'EscapeVar = FALSE
                                        Testvar = TRUE
                                END IF
                        END IF
                NEXT i%
        NEXT j%
        IF Testvar = TRUE THEN
                DrawGame
                pause
                Testvar = FALSE
        END IF
LOOP
        g = GetBreaks
        IF g = TRUE THEN EscapeVar = FALSE
        RemoveBreaks
        ClearBreaks
        DrawGame
LOOP UNTIL EscapeVar = TRUE
END SUB

SUB EraseCursor
x% = CursorX%
y% = CursorY%
LINE (FieldPosX + (x% * (BlockSize + BlockSep)) - 3, FieldPosY + (y% * (BlockSize + BlockSep)) - 3)-(FieldPosX + (x% * (BlockSize + BlockSep)) + BlockSize + 3, FieldPosY + (y% * (BlockSize + BlockSep)) + BlockSize + 3), 0, B
LINE (FieldPosX + (x% * (BlockSize + BlockSep)) - 2, FieldPosY + (y% * (BlockSize + BlockSep)) - 2)-(FieldPosX + (x% * (BlockSize + BlockSep)) + BlockSize + 2, FieldPosY + (y% * (BlockSize + BlockSep)) + BlockSize + 2), 0, B
END SUB

SUB Explosion (x%, y%)
DrawGame
pause
dacol% = BlockList%(x%, y%) MOD 7
zecol% = InnerColorList(dacol%)
zacol% = OuterColorList(dacol%)
xx% = FieldPosX% + x% * (BlockSize + BlockSep)
yy% = FieldPosY% + y% * (BlockSize + BlockSep)
FOR i% = 1 TO INT((BlockSize + BlockSep) * 1.5)
        CIRCLE (xx% + INT(BlockSize / 2), yy% + INT(BlockSize / 2)), i%, zecol%
        PAINT (xx% + INT(BlockSize / 2), yy% + INT(BlockSize / 2)), zecol%, zecol%
        CIRCLE (xx% + INT(BlockSize / 2), yy% + INT(BlockSize / 2)), i%, zacol%
        CIRCLE (xx% + INT(BlockSize / 2), yy% + INT(BlockSize / 2)), i%, zacol%
        FOR delay& = 0 TO INT(DropSpeed / 10): NEXT delay&
NEXT i%
pause
pause
pause
LINE (FieldPosX - (BlockSize * 2) - BlockSep, FieldPosY - (BlockSize * 2) - BlockSep)-(FieldPosX + (BlocksWide + 2) * (BlockSep + BlockSize), FieldPosY + (BlocksTall + 2) * (BlockSep + BlockSize)), 0, BF
DrawGame
mycol% = BlockList%(x%, y%)
IF x% > 0 THEN
        IF BlockList%(x% - 1, y%) = 6 THEN
                HyperHandle2 x% - 1, y%, mycol%
                BlockList%(x%, y%) = mycol%
        'ELSEIF BlockList%(x% - 1, y%) >= 7 THEN
        '        Explosion x% - 1, y%
        '        BlockList%(x%, y%) = mycol%
        END IF
        BlockList%(x% - 1, y%) = 5
END IF
IF x% > 0 AND y% > 0 THEN
        IF BlockList%(x% - 1, y% - 1) = 6 THEN
                HyperHandle2 x% - 1, y% - 1, mycol%
                BlockList%(x%, y%) = mycol%
        'ELSEIF BlockList%(x% - 1, y% - 1) >= 7 THEN
        '        Explosion x% - 1, y% - 1
        '        BlockList%(x%, y%) = mycol%
        END IF
        BlockList%(x% - 1, y% - 1) = 5
END IF
IF x% > 0 AND y% < BlocksTall - 1 THEN
        IF BlockList%(x% - 1, y% + 1) = 6 THEN
                HyperHandle2 x% - 1, y% + 1, mycol%
                BlockList%(x%, y%) = mycol%
        'ELSEIF BlockList%(x% - 1, y% + 1) >= 7 THEN
        '        Explosion x% - 1, y% + 1
        '        BlockList%(x%, y%) = mycol%
        END IF
        BlockList%(x% - 1, y% + 1) = 5
END IF
IF y% < BlocksTall - 1 THEN
        BlockList%(x%, y% + 1) = 5
END IF
IF y% > 0 THEN
        BlockList%(x%, y% - 1) = 5
END IF
IF x% < BlocksWide - 1 THEN
        BlockList%(x% + 1, y%) = 5
END IF
IF x% < BlocksWide - 1 AND y% > 0 THEN
        BlockList%(x% + 1, y% - 1) = 5
END IF
IF x% < BlocksWide - 1 AND y% < BlocksTall - 1 THEN
        BlockList%(x% + 1, y% + 1) = 5
END IF

BlockList%(x%, y%) = 5
Score# = Score# + (75 * multiplier%)

END SUB

FUNCTION GetBreaks
GetBreaks = FALSE
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                IF CheckHor(i%, j%) = TRUE OR CheckVer(i%, j%) = TRUE THEN
                        GetBreaks = TRUE
                        BreakList%(i%, j%) = BlockList%(i%, j%)
                END IF
        NEXT j%
NEXT i%

END FUNCTION

SUB HyperHandle (x1%, y1%, x2%, y2%)
wasscore# = Score#
p! = TIMER
Frozen% = TRUE
DrawGame
pause
dacol% = BlockList%(x2%, y2%) MOD 7
zecol% = InnerColorList(dacol%)
xx% = FieldPosX% + x1% * (BlockSep + BlockSize)
yy% = FieldPosY% + y1% * (BlockSep + BlockSize)
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                IF BlockList%(i%, j%) MOD 7 = dacol% THEN
                        LINE (xx%, yy%)-(FieldPosX% + i% * (BlockSep + BlockSize), FieldPosY% + j% * (BlockSep + BlockSize)), zecol%
                END IF
        NEXT j%
NEXT i%
pause
pause
pause
LINE (FieldPosX, FieldPosY)-(FieldPosX + BlocksWide * (BlockSep + BlockSize), FieldPosY + BlocksTall * (BlockSep + BlockSize)), 0, BF
DrawGame
BlockList%(x1%, y1%) = 5
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                IF BlockList%(i%, j%) MOD 7 = dacol% THEN
                        BlockList%(i%, j%) = 5
                        Score# = Score# + (5 * multiplier%)
                END IF
        NEXT j%
NEXT i%
DrawScore
DrawGame
sel = FALSE
DrawCursor
pause
DropBlocks
Frozen% = FALSE
PauseClock! = PauseClock! + TIMER - p!
totalearned# = Score# - wasscore#
IF totalearned# > 900 * multiplier% THEN
multiplier% = multiplier% + 1
DrawMult
END IF
END SUB

SUB HyperHandle2 (x1%, y1%, mycol%)
DrawGame
pause
dacol% = mycol% MOD 7
zecol% = InnerColorList(dacol%)
xx% = FieldPosX% + x1% * (BlockSep + BlockSize)
yy% = FieldPosY% + y1% * (BlockSep + BlockSize)
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                IF BlockList%(i%, j%) MOD 7 = dacol% THEN
                        LINE (xx%, yy%)-(FieldPosX% + i% * (BlockSep + BlockSize), FieldPosY% + j% * (BlockSep + BlockSize)), zecol%
                END IF
        NEXT j%
NEXT i%
pause
pause
pause
LINE (FieldPosX, FieldPosY)-(FieldPosX + BlocksWide * (BlockSep + BlockSize), FieldPosY + BlocksTall * (BlockSep + BlockSize)), 0, BF
DrawGame
BlockList%(x1%, y1%) = 5
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                IF BlockList%(i%, j%) MOD 7 = dacol% THEN
                        BlockList%(i%, j%) = 5
                        Score# = Score# + (5 * multiplier%)
                END IF
        NEXT j%
NEXT i%
DrawScore
DrawGame
pause
END SUB

SUB MIDPRINT (dastring$)
LOCATE CSRLIN, 40 - (LEN(dastring$) / 2)
PRINT dastring$
END SUB

SUB nou
l# = Score#
END SUB

SUB pause
FOR delay& = 0 TO DropSpeed: NEXT delay&
END SUB

SUB RandomizeBoard
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                BlockList%(i%, j%) = INT(RND * 5)
        NEXT j%
NEXT i%
CALL StartingCheck
END SUB

SUB RemoveBreaks
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                countbreak% = 0
                IF BreakList%(i%, j%) <> 5 THEN
                        IF CheckHor(i%, j%) = TRUE THEN
                                IF TIMER - lastbreak! <= 1 THEN
                                     streak% = streak% + 1
                                        IF streak% > 15 THEN streak% = 15
                                        IF streak% MOD 2 = 1 THEN
                                                bonus% = bonus% + 50
                                        END IF
                                ELSE
                                        streak% = 0
                                        bonus% = 0
                                END IF
                                lastbreak! = TIMER
                                countbreak% = 3
                                IF BlockList%(i%, j%) > 6 THEN Explosion i%, j%
                                BlockList%(i%, j%) = 5
                                IF BlockList%(i% + 1, j%) > 6 THEN Explosion i% + 1, j%
                                BlockList%(i% + 1, j%) = 5
                                IF BlockList%(i% - 1, j%) > 6 THEN Explosion i% - 1, j%
                                BlockList%(i% - 1, j%) = 5
                                Score# = Score# + (30 * multiplier%) + bonus%
                                x% = 2
                                BreakVar = FALSE
                                DO UNTIL i% + x% = BlocksWide OR BreakVar = TRUE
                                        IF BlockList%(i% + x%, j%) MOD 7 = BreakList%(i%, j%) MOD 7 THEN
                                                IF BlockList%(i% + x%, j%) > 6 THEN
                                                        Explosion i% + x%, j%
                                                END IF
                                                countbreak% = countbreak% + 1: BlockList%(i% + x%, j%) = 5: Score# = Score# + (15 * multiplier%)
                                        ELSE
                                                BreakVar = TRUE
                                        END IF
                                        x% = x% + 1
                                LOOP
                                x% = 2
                                BreakVar = FALSE
                                DO UNTIL i% - x% <= 0 OR BreakVar = TRUE
                                        IF BlockList%(i% - x%, j%) MOD 7 = BreakList%(i%, j%) MOD 7 THEN
                                                IF BlockList%(i% - x%, j%) > 6 THEN
                                                        Explosion i% - x%, j%
                                                END IF
                                                countbreak% = countbreak% + 1: BlockList%(i% - x%, j%) = 5: Score# = Score# + (15 * multiplier%)
                                        ELSE
                                                BreakVar = TRUE
                                        END IF
                                        x% = x% + 1
                                LOOP
                                IF countbreak% >= 5 THEN
                                        needhyper% = TRUE
                                ELSEIF countbreak% = 4 THEN
                                        needbomb% = TRUE
                                END IF
                        END IF
                        IF CheckVer(i%, j%) = TRUE THEN
                                IF TIMER - lastbreak! <= 1 THEN
                                        streak% = streak% + 1
                                        IF streak% > 15 THEN streak% = 15
                                        IF streak% MOD 2 = 1 THEN
                                                bonus% = bonus% + 50
                                        END IF
                                ELSE
                                        streak% = 0
                                        bonus% = 0
                                END IF
                                lastbreak! = TIMER
                                countbreak% = 3
                                IF BlockList%(i%, j%) > 6 THEN Explosion i%, j%
                                BlockList%(i%, j%) = 5
                                IF BlockList%(i%, j% + 1) > 6 THEN Explosion i%, j% + 1
                                BlockList%(i%, j% + 1) = 5
                                IF BlockList%(i%, j% - 1) > 6 THEN Explosion i%, j% - 1
                                BlockList%(i%, j% - 1) = 5
                                Score# = Score# + (30 * multiplier%) + bonus%
                                x% = 2
                                BreakVar = FALSE
                                DO UNTIL j% + x% = BlocksTall OR BreakVar = TRUE
                                        IF BlockList%(i%, j% + x%) MOD 7 = BreakList%(i%, j%) MOD 7 THEN
                                                IF BlockList%(i%, j% + x%) > 6 THEN
                                                        Explosion i%, j% + x%
                                                END IF
                                                countbreak% = countbreak% + 1: BlockList%(i%, j% + x%) = 5: Score# = Score# + (15 * multiplier%)
                                        ELSE
                                                BreakVar = TRUE
                                        END IF
                                        x% = x% + 1
                                LOOP
                                x% = 2
                                BreakVar = FALSE
                                DO UNTIL j% - x% <= 0 OR BreakVar = TRUE
                                        IF BlockList%(i%, j% - x%) MOD 7 = BreakList%(i%, j%) MOD 7 THEN
                                                IF BlockList%(i%, j% - x%) > 6 THEN
                                                        Explosion i%, j% - x%
                                                END IF
                                                countbreak% = countbreak% + 1: BlockList%(i%, j% - x%) = 5: Score# = Score# + (15 * multiplier%)
                                        ELSE
                                                BreakVar = TRUE
                                        END IF
                                        x% = x% + 1
                                LOOP
                                IF countbreak% >= 5 THEN
                                        needhyper% = TRUE
                                ELSEIF countbreak% = 4 THEN
                                        needbomb% = TRUE
                                END IF
                        END IF
                END IF
        NEXT j%
NEXT i%
DrawScore
END SUB

SUB StartingCheck
DO
RepeatVar = FALSE
FOR i% = 0 TO BlocksWide - 1
        FOR j% = 0 TO BlocksTall - 1
                IF CheckHor(i%, j%) = TRUE OR CheckVer(i%, j%) = TRUE THEN
                        BlockList%(i%, j%) = INT(RND * 5)
                        RepeatVar = TRUE
                END IF
        NEXT j%
NEXT i%
LOOP UNTIL RepeatVar = FALSE
END SUB

SUB StartScreen
COLOR 10, 0
LOCATE 2, 40
MIDPRINT "Shane Schulte's"
LOCATE 4, 40
MIDPRINT "Q-Jeweled!"
COLOR 7, 0
MIDPRINT STRING$(24, "-")
COLOR 7, 0
LOCATE 7, 40
COLOR 31
MIDPRINT "PLEASE GO INTO FULLSCREEN NOW (ALT + ENTER)"
COLOR 15
LOCATE 9, 40
MIDPRINT "The object of the game is to match 3 or more of the same colored block"
MIDPRINT "in a row. When you do so, they will break and new blocks will fall. Try"
MIDPRINT "to break as many matches as you can within the 60 seconds of gameplay."
MIDPRINT "Move the cursor with the arrow keys, and hit space + a direction to swap"
MIDPRINT "pieces. They will only swap if they cause a break."
PRINT
MIDPRINT "The game will pause while things are breaking. To pause the game, press P."
MIDPRINT "To quit the game, press Q. After your minute, you will be able to submit"
MIDPRINT "Your highscore to the leaderboards."
PRINT
MIDPRINT "You recieve 30 points for every broken 3 match, and +15 for every other block"
MIDPRINT "in that row. Also, getting multiple breaks less than a second apart results in"
MIDPRINT "speed bonus points. Matching five in a row creates a hyper block."
PRINT
MIDPRINT "Have fun!"
LOCATE 25, 5
MIDPRINT "Press any key to start the game"
COLOR 7, 0
DO
        ckey$ = INKEY$
LOOP UNTIL ckey$ <> ""
COLOR 7, 0

END SUB

SUB SwapBlocks (x1%, y1%, x2%, y2%)
temp% = BlockList%(x1%, y1%)
BlockList%(x1%, y1%) = BlockList%(x2%, y2%)
BlockList%(x2%, y2%) = temp%
END SUB

SUB SwapHandle
wasscore# = Score#
p! = TIMER
Frozen% = TRUE
DrawGame
pause
RemoveBreaks
ClearBreaks
DrawGame
sel = FALSE
DrawCursor
pause
DropBlocks
Frozen% = FALSE
PauseClock! = PauseClock! + TIMER - p!
totalearned# = Score# - wasscore#
IF totalearned# > 900 * multiplier% THEN
multiplier% = multiplier% + 1
DrawMult
END IF
END SUB

FUNCTION YouLose

CLS

YouLose = FALSE
LOCATE 2, 40
MIDPRINT "Your final score was" + STR$(Score#)
SLEEP 3
MIDPRINT "Would you like to submit this score online (Y/N)?"
A$ = INKEY$
DO
theirans$ = UCASE$(INKEY$)
LOOP UNTIL theirans$ = "Y" OR theirans$ = "N"
COLOR 0
LOCATE 3, 40
MIDPRINT "Would you like to submit this score online (Y/N)?"
COLOR 7
IF theirans$ = "Y" THEN
LOCATE 5, 10
A$ = INKEY$
INPUT "Please enter your YKUG2BW or QJeweled username:  ", username$
DO UNTIL LEN(username$) > 4
        username$ = username$ + "0"
LOOP
userhex$ = ""
FOR i% = 1 TO 4
        userhex$ = userhex$ + HEX$(ASC(MID$(username$, i%, 1)))
NEXT i%

userhex2$ = ""
FOR i% = 1 TO 5
        userhex2$ = userhex2$ + HEX$(ASC(MID$(username$, i%, 1)) + 32)
NEXT i%


LOCATE 7, 40
MIDPRINT "To submit your score online, press alt + enter,"
LOCATE 8, 40
MIDPRINT "go to http://scorchingstrings.com/qjeweled, log in,"
LOCATE 9, 40
MIDPRINT "and then enter these 3 codes:"
LOCATE 11, 20
PRINT "Code 1: " + HEX$(Score# - l#) + userhex$
LOCATE 12, 20
PRINT "Code 2: " + HEX$(GameTime%) + userhex2$
LOCATE 13, 20
PRINT "Code 3: " + HEX$((Score# * GameTime%) MOD 256) + userhex$
LOCATE 15, 20
MIDPRINT "TIP: To copy and paste, right click the icon in the upper left,"
MIDPRINT "And go to Edit --> Mark. Select the code, press enter, and then"
MIDPRINT "You can paste it into the box on the website."
SLEEP 3
A$ = INKEY$
DO UNTIL INKEY$ <> ""
LOOP
END IF

CLS

LOCATE 11, 40
MIDPRINT "Would you like to play again (Y/N)?"
A$ = INKEY$
DO
theirans$ = UCASE$(INKEY$)
LOOP UNTIL theirans$ = "Y" OR theirans$ = "N"

IF theirans$ = "Y" THEN
        YouLose = TRUE
END IF


END FUNCTION

