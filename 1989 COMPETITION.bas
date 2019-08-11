MAIN: CLS
COLOR 15

DIM SHARED PTVALUE(26)
DIM SHARED ALPHA$(26)

DATABASEA:
DATA A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z:

DATABASEPT:
DATA 9,14,1,16,20,5,10,2,21,17,6,25,12,3,22,18,24,7,13,26,15,11,19,4,23,8:

DATABASESTR1:
DATA "BIBLE","IDYLL","NOISE","GULLY","OBESE":

DATABASESTR2:
DATA "OBESE","TITHE","INLET","IGLOO","TOWER":


DATABASECALENDAR:
DATA JANUARY,FEBRUARY,MARCH,APRIL,MAY,JUNE,JULY,AUGUST,SEPTEMBER,OCTOBER,NOVEMBER,DECEMBER:

PRINT "MAIN MENU"
PRINT
PRINT "1) ONE POINTERS"
PRINT "2) TWO POINTERS"
PRINT "3) THREE POINTERS"
PRINT "4) EXIT"
PRINT
INPUT "CHOOSE A LIST: ", SLCT

SELECT CASE SLCT
    CASE 1
        CALL ONEPOINTERS
    CASE 2
        CALL TWOPOINTERS
    CASE 3
        CALL THREEPOINTERS
    CASE 4
        END
    CASE ELSE
END SELECT

GOTO MAIN

SUB ANYKEY
WHILE INKEY$ = ""
    LOCATE 23, 45
    PRINT "PRESS ANYKEY TO CONTINUE"
WEND
END SUB

SUB ONEPOINTERS
WHILE SLCT <> 11
    CLS
    PRINT "ONEPOINTERS"
    PRINT
    COLOR 4
    PRINT "1) 1.1"
    PRINT "2) 1.2"
    PRINT "3) 1.3"
    PRINT "4) 1.4"
    PRINT "5) 1.5"
    PRINT "6) 1.6"
    PRINT "7) 1.7"
    PRINT "8) 1.8"
    PRINT "9) 1.9"
    PRINT "10) 1.10"
    COLOR 15
    PRINT "11) EXIT"
    PRINT
    INPUT "SELECT A PROGRAM: ", SLCT

    SELECT CASE SLCT
        CASE 1
            CALL ONEONE
        CASE 2
            CALL ONETWO
        CASE 3
            CALL ONETHREE
        CASE 4
            CALL ONEFOUR
        CASE 5
            CALL ONEFIVE
        CASE 6
            CALL ONESIX
        CASE 7
            CALL ONESEVEN
        CASE 8
            CALL ONEEIGHT
        CASE 9
            CALL ONENINE
        CASE 10
            CALL ONETEN
        CASE 11
        CASE ELSE
    END SELECT
WEND
END SUB

SUB TWOPOINTERS
WHILE SLCT <> 11
    CLS
    PRINT "TWOPOINTERS"
    PRINT
    COLOR 4
    PRINT "1) 2.1"
    PRINT "2) 2.2"
    PRINT "3) 2.3"
    PRINT "4) 2.4"
    PRINT "5) 2.5"
    PRINT "6) 2.6"
    PRINT "7) 2.7"
    PRINT "8) 2.8"
    PRINT "9) 2.9"
    PRINT "10) 2.10"
    COLOR 15
    PRINT "11) EXIT"
    PRINT
    INPUT "SELECT A PROGRAM: ", SLCT

    SELECT CASE SLCT
        CASE 1
            CALL TWOONE
        CASE 2
            CALL TWOTWO
        CASE 3
            CALL TWOTHREE
        CASE 4
            CALL TWOFOUR
        CASE 5
            CALL TWOFIVE
        CASE 6
            CALL TWOSIX
        CASE 7
            CALL TWOSEVEN
        CASE 8
            CALL TWOEIGHT
        CASE 9
            CALL TWONINE
        CASE 10
            CALL TWOTEN
        CASE 11
        CASE ELSE
    END SELECT
WEND
END SUB

SUB THREEPOINTERS
WHILE SLCT <> 11
    CLS
    PRINT "THREE POINTERS"
    PRINT
    COLOR 4
    PRINT "1) 3.1"
    COLOR 15
    PRINT "2) 3.2"
    PRINT "3) 3.3"
    PRINT "4) 3.4"
    PRINT "5) 3.5"
    PRINT "6) 3.6"
    COLOR 4
    PRINT "7) 3.7"
    COLOR 15
    PRINT "8) 3.8"
    PRINT "9) 3.9"
    PRINT "10) 3.10"
    PRINT "11) EXIT"
    PRINT
    INPUT "SELECT A PROGRAM: ", SLCT

    SELECT CASE SLCT
        CASE 1
            CALL THREEONE
        CASE 2
            CALL THREETWO
        CASE 3
            CALL THREETHREE
        CASE 4
            CALL THREEFOUR
        CASE 5
            CALL THREEFIVE
        CASE 6
            CALL THREESIX
        CASE 7
            CALL THREESEVEN
        CASE 8
            CALL THREEEIGHT
        CASE 9
            CALL THREENINE
        CASE 10
            CALL THREETEN
        CASE 11
        CASE ELSE
            CALL ANYKEY
    END SELECT
WEND
END SUB

SUB ONEONE
CLS
PRINT "1.1"
PRINT
FOR X = 1 TO 19
    PRINT SPACE$(X) + "1989 COMPUTER COMPETITION"
NEXT X
CALL ANYKEY
END SUB

SUB ONETWO
CLS
PRINT "1.2"
INPUT "ENTER NUMBER OF GIGABYTES: ", GIGS
IF GIGS >= 30 THEN
    PRINT "IT CANNOT EXCEED 30"
    CALL ANYKEY
    CALL ONETWO
END IF
PRINT (GIGS * 1024); " MEGABYTES"
CALL ANYKEY
END SUB

SUB ONETHREE
CLS
PRINT "1.3"
INPUT "ENTER A WORD: ", WRD$
WRD = LEN(WRD$)
FOR X = 1 TO (WRD - 1)
    PRINT SPACE$(WRD - 1) + MID$(WRD$, X, 1)
NEXT X
PRINT WRD$
CALL ANYKEY
END SUB

SUB ONEFOUR
CLS
PRINT "1.4"
PRINT
INPUT "ENTER N: ", N
IF N > 9 THEN
    PRINT "NUMBER CANNOT EXCEED 9"
    CALL ANYKEY
    CALL ONEFOUR
END IF
PRINT SPACE$(N + 2) + "1"
FOR X = 2 TO N
    X$ = STR$(X)
    PRINT SPACE$(N - X) + X$ + SPACE$(X * 2) + X$
NEXT X
CALL ANYKEY
END SUB

SUB ONEFIVE
CLS
PRINT "1.5"
PRINT
INPUT "ENTER A DATE: ", DATE
INPUT "ENTER A.D. OR B.C.: ", DATE2$
IF DATE <= 4 AND DATE > 4 = 0 AND DATE2$ = "B.C." THEN
    PRINT (4 - DATE); " A.D."
    GOTO FINISH
END IF
IF DATE2$ = "A.D." THEN
    PRINT DATE + 4; DATE2$
ELSEIF DATE2$ = "B.C." THEN
    PRINT (DATE - 4); DATE2$
END IF
FINISH: CALL ANYKEY
END SUB

SUB ONESIX
CLS
PRINT "1.6"
PRINT
PASS$ = "ITSME"
FOR X = 1 TO 4
    INPUT "ENTER PASSWORD: ", GUESS$
    IF GUESS$ = PASS$ THEN
        PRINT "YOU HAVE ACCESS"
        X = 4
    ELSEIF X = 3 THEN
        PRINT "YOU ARE TRESPASSING"
        X = 4
    ELSE
        PRINT "INVALID PASSWORD"
    END IF
NEXT X
CALL ANYKEY
END SUB

SUB ONESEVEN
CLS
PRINT "1.7"
INPUT "ENTER N: ", N
CHECK = 0
CHECK$ = "NULL"
FOR X = 1 TO N
    INPUT "ENTER DBMS NAME: ", DBMS$
    INPUT "ENTER CONVENIENCE, EFFICIENCY: ", CON, EFF
    IF CON + EFF > CHECK THEN
        CHECK = CON + EFF
        CHECK$ = DBMS$
    END IF
NEXT X
PRINT
PRINT CHECK$ + " IS BEST"
CALL ANYKEY
END SUB

SUB ONEEIGHT
DIM NUM(3000)
DIM Y(3000)
CLS
PRINT "1.8"
PRINT
I = 1
INPUT "ENTER #: ", NUM(I)
WHILE NUM(I) <> -999
    I = I + 1
    INPUT "ENTER #: ", NUM(I)
WEND
FOR Z = 1 TO (I - 1)
    FOR K = 1 TO (Z - 1)
        IF NUM(Z) = NUM(K) THEN GOTO SKIP
    NEXT K
    LET J = J + 1
    LET Y(J) = NUM(Z)
    SKIP:
NEXT Z
FOR T = 1 TO J
    PRINT Y(T);
NEXT T
CALL ANYKEY
END SUB

SUB ONENINE
CLS
PRINT "1.9"
PRINT
INPUT "ENTER A PROBABILITY: ", N
U = 262134 ^ (1 / 2)
Z = U * 63360
Y = Z / 1.5
A = Y ^ 2
B = N / A
C = B * (3 / 32)
D = CINT(C / 12)
PRINT D; "FEET DEEP"
CALL ANYKEY
END SUB

SUB ONETEN
CLS
PRINT "1.10"
PRINT
SEGM = 0
DO UNTIL SEGM > 4
    INPUT "ENTER SEG#, ADRESS: ", SEGM, ADDR
    IF SEGM > 4 THEN
        GOTO FINISH
    END IF
    SELECT CASE SEGM
        CASE 0
            BASES = 219
            LENT = 600
        CASE 1
            BASES = 2300
            LENT = 14
        CASE 2
            BASES = 90
            LENT = 100
        CASE 3
            BASES = 1327
            LENT = 580
        CASE 4
            BASES = 1952
            LENT = 96
    END SELECT
    IF ADDR <= LENT THEN
        PRINT ADDR + BASES
    ELSEIF ADDR > LENT THEN
        PRINT "ADDRESSING ERROR"
    END IF
LOOP
FINISH:
CALL ANYKEY
END SUB

SUB TWOONE
DIM F(20)
CLS
PRINT "2.1"
PRINT
INPUT "Enter your x: ", X
F(1) = 1
F(2) = 1
F(3) = 1
I = 3
WHILE I < X
    F(I + 1) = (F(I) * F(I - 1) + 2) / F(I - 2)
    I = I + 1
WEND
PRINT "F("; X; ") = "; F(X)
CALL ANYKEY
END SUB

SUB TWOTWO
CLS
PRINT 2.2
PRINT
INPUT "ENTER A NUMBER: ", N
Y = N
BEG: WHILE N <> 1
    IF N MOD 5 = 0 THEN
        N = N / 5
        '  PRINT " 5 X";
        SUM$ = SUM$ + " 5 X"
        GOTO BEG
    END IF
    IF N MOD 3 = 0 THEN
        N = N / 3
        '  PRINT " 3 X";
        SUM$ = SUM$ + " 3 X"
        GOTO BEG
    END IF
    IF N MOD 2 = 0 THEN
        N = N / 2
        ' PRINT " 2 X";
        SUM$ = SUM$ + " 2 X"
        GOTO BEG
    END IF
    IF N MOD 5 <> 0 AND N MOD 3 <> 0 AND N MOD 2 <> 0 AND Y MOD 5 <> 0 AND Y MOD 3 <> 0 AND Y MOD 5 <> 0 THEN
        PRINT N
        GOTO FINISH
    ELSEIF N MOD 5 <> 0 AND N MOD 3 <> 0 AND N MOD 2 <> 0 THEN
        FOR Y = 1 TO LEN(SUM$)
            PLACE$ = MID$(SUM$, Y, 1)
            PLACE2$ = MID$(SUM$, (Y + 1), 1)
            IF PLACE$ = "X" AND PLACE2$ = "" THEN
                SUM$ = LEFT$(SUM$, (Y - 2))
            END IF
        NEXT Y
        FOR Z = LEN(SUM$) TO 1 STEP -1
            RevSUM$ = RevSUM$ + MID$(SUM$, Z, 1)
        NEXT Z
        PRINT RevSUM$; "X"; N
        GOTO FINISH
    END IF
WEND
FOR Y = 1 TO LEN(SUM$)
    PLACE$ = MID$(SUM$, Y, 1)
    PLACE2$ = MID$(SUM$, (Y + 1), 1)
    IF PLACE$ = "X" AND PLACE2$ = "" THEN
        SUM$ = LEFT$(SUM$, (Y - 2))
    END IF
NEXT Y
FOR Z = LEN(SUM$) TO 1 STEP -1
    RevSUM$ = RevSUM$ + MID$(SUM$, Z, 1)
NEXT Z

PRINT RevSUM$

FINISH:
CALL ANYKEY
END SUB

SUB TWOTHREE
CLS
PRINT "2.3"
PRINT
INPUT "ENTER A WORD: ", WRD$
CNT = LEN(WRD$)
FOR X = 1 TO CNT
    PLACE$ = MID$(WRD$, X, 1)
    PLACE$ = UCASE$(PLACE$)
    IF PLACE$ = "A" OR PLACE$ = "E" OR PLACE$ = "I" OR PLACE$ = "O" OR PLACE$ = "U" THEN
    ELSE
        SUM$ = SUM$ + PLACE$
    END IF
NEXT X
PRINT SUM$
CALL ANYKEY
END SUB

SUB TWOFOUR
DIM NAME$(6)
CLS
PRINT 2.4
PRINT
FOR I = 1 TO 6
    INPUT "ENTER A NAME"; NAME$(I)
NEXT I
FOR Z = 1 TO 6
    K = 1
    S$ = MID$(NAME$(Z), 1, 1)
    FOR J = 1 TO 6
        WHILE Z <> J AND S$ = MID$(NAME$(J), 1, K) AND K < LEN(NAME$(Z))
            K = K + 1
            S$ = S$ + MID$(NAME$(Z), K, 1)
        WEND
    NEXT J
    PRINT S$
NEXT Z
CALL ANYKEY
END SUB

SUB TWOFIVE
DIM STORE$(300)
DIM NUMSTORE(300)
DIM FACT(300)
PROD = 1
CLS
PRINT "2.5"
PRINT
INPUT "Enter a word: ", WORD$
WORD$ = UCASE$(WORD$)
LWORD = LEN(WORD$)
FACTWORD = Factorial(LWORD)
FOR X = 1 TO LWORD
    PLACE$ = MID$(WORD$, X, 1)
    STORE$(X) = PLACE$
NEXT X
FOR X = 1 TO LWORD
    NUMSTORE(X) = 1
    FACT(X) = 1
NEXT X
FOR X = 1 TO LWORD
    PLACE2$ = MID$(WORD$, X, 1)
    FOR Y = 1 TO X - 1
        PLACE3$ = MID$(WORD$, Y, 1)
        IF PLACE2$ = PLACE3$ THEN
            NUMSTORE(X) = NUMSTORE(X) + 1
            NUMSTORE(Y) = 1
            FACT(X) = Factorial(NUMSTORE(X))
            FACT(Y) = 1
        END IF
    NEXT Y
NEXT X
FOR I = 1 TO LWORD
    PROD = PROD * FACT(I)
NEXT I
PRINT FACTWORD / PROD

CALL ANYKEY
END SUB

SUB TWOSIX
CLS
PRINT "2.6"
PRINT
INPUT "ENTER A SENTENCE: ", SENT$
CLS
PRINT "2.6"
PRINT
PRINT SENT$
PRINT
LSENT = LEN(SENT$)

FOR C = 1 TO LSENT
    PLACE2$ = MID$(SENT$, C, 1)
    IF PLACE2$ = "*" THEN
        SUM$ = SUM$ + " "
    ELSE
        SUM$ = SUM$ + PLACE2$
    END IF
NEXT C
PRINT SUM$

X = 1
FOR I = 1 TO LSENT
    PLACE$ = MID$(SENT$, I, 1)
    IF X = 1 THEN
        PRINT " ";
    ELSE
        PRINT "-";
    END IF
    IF PLACE$ = "*" THEN
        X = X * -1
    ELSE
    END IF
NEXT I

CALL ANYKEY
END SUB

SUB TWOSEVEN
CLS
PRINT "2.7"
PRINT
INPUT "ENTER YOUR OPERATION: ", OP$
OPLEN = LEN(OP$)
FOR X = 1 TO OPLEN
    PLACE$ = MID$(OP$, X, 1)
    SELECT CASE PLACE$
        CASE "+"
            FRST$ = LEFT$(OP$, (X - 1))
            FT = VAL(FRST$)
            SECOND$ = MID$(OP$, (X + 1), (OPLEN - X))
            SD = VAL(SECOND$)
            PRINT FT + SD
        CASE "-"
            FRST$ = LEFT$(OP$, (X - 1))
            FT = VAL(FRST$)
            SECOND$ = MID$(OP$, (X + 1), (OPLEN - X))
            SD = VAL(SECOND$)
            PRINT FT - SD
        CASE "*"
            FRST$ = LEFT$(OP$, (X - 1))
            FT = VAL(FRST$)
            PRINT "FT = "; FT
            SECOND$ = MID$(OP$, (X + 1), (OPLEN - X))
            SD = VAL(SECOND$)
            PRINT "SD = "; SD
            PRINT FT * SD
        CASE "/"
            FRST$ = LEFT$(OP$, (X - 1))
            FT = VAL(FRST$)
            SECOND$ = MID$(OP$, (X + 1), (OPLEN - X))
            SD = VAL(SECOND$)
            PRINT FT / SD
        CASE ELSE
    END SELECT
NEXT X
CALL ANYKEY
END SUB

SUB TWOEIGHT
CLS
DIM E(30, 30)
DIM A(30, 30)
PRINT "2.8"
PRINT
INPUT "ENTER # ROWS, # COLS: ", ROWS, COLS
PRINT
FOR X = 1 TO ROWS
    FOR Y = 1 TO COLS
        PRINT "ENTER ROW"; X; " COL"; Y;
        INPUT ": ", E(X, Y)
    NEXT Y
NEXT X
FOR X = 1 TO ROWS
    FOR Y = 1 TO COLS
        SMALL$ = "TRUE"
        FOR K = 1 TO COLS
            IF (K <> Y) AND (E(X, Y) >= E(X, K)) THEN
                SMALL$ = "FALSE"
            END IF
        NEXT K
        IF SMALL$ = "TRUE" THEN
            LARGE$ = "TRUE"
            FOR K = 1 TO ROWS
                IF (K <> X) AND (E(X, Y) <= E(K, Y)) THEN
                    LARGE$ = "FALSE"
                END IF
            NEXT K
            IF LARGE$ = "TRUE" THEN
                PRINT
                PRINT "SADDLE POINT = "; E(X, Y); "AT ROW"; X; "COL"; Y
            END IF
        END IF
    NEXT Y
NEXT X

CALL ANYKEY
END SUB

SUB TWONINE
CLS
PRINT "2.9"
PRINT
DIM CALENDAR$(12)
DIM SORT(3000)
DIM DATE(3000)
DIM YEAR(3000)

RESTORE DATABASECALENDAR
FOR I = 1 TO 12
    READ CALENDAR$(I)
NEXT I

INPUT "ENTER # OF DATES: ", N
FOR I = 1 TO N
    INPUT "ENTER MONTH: ", MONTH$
    INPUT "ENTER DAY:  ", DATE(I)
    INPUT "ENTER YEAR: ", YEAR(I)
    PRINT
    X = 1
    WHILE X <= 12 AND MONTH$ <> CALENDAR$(X)
        X = X + 1
    WEND
    SORT(I) = X
NEXT I
FOR X = 1 TO N
    FOR K = X + 1 TO N
        IF YEAR(X) > YEAR(K) THEN
            SWAP SORT(X), SORT(K)
            SWAP DATE(X), DATE(K)
            SWAP YEAR(X), YEAR(K)
        END IF
    NEXT K
NEXT X
FOR I = 1 TO N - 1
    FOR J = I + 1 TO N
        IF SORT(I) > SORT(J) AND YEAR(I) = YEAR(J) THEN
            SWAP SORT(I), SORT(J)
            SWAP DATE(I), DATE(J)
            SWAP YEAR(I), YEAR(J)
        END IF
    NEXT J
NEXT I
FOR X = 1 TO N
    FOR K = X + 1 TO N
        IF DATE(X) > DATE(K) AND SORT(X) = SORT(K) AND YEAR(X) = YEAR(K) THEN
            SWAP SORT(X), SORT(K)
            SWAP DATE(X), DATE(K)
            SWAP YEAR(X), YEAR(K)
        END IF
    NEXT K
NEXT X
FOR I = 1 TO N
    PRINT CALENDAR$(SORT(I)), DATE(I), YEAR(I)
NEXT I
CALL ANYKEY
END SUB

SUB TWOTEN
CLS
HOME:
IF Z4 = 1 THEN
    WIDTH 80, 25
    CLS
    GOTO HEY
END IF
WIDTH 90, 20
PRINT "2.6"
PRINT "  NAME", " Q1", " Q2", " Q3", " Q4"
PRINT
PRINT "D. WOOLY", 100, 92, 90, 90
PRINT "M. SMITH", 55, 75, 70, 65
PRINT "C. BROWN ", 94, 70, 62, 70
PRINT "R. GREEN", 90, 74, 80, 85
PRINT "T. STONE", 85, 98, 100, 70
INPUT "ENTER 5 GRADES FOR QUIZ 4: ", D4, M4, C4, R4, T4

CLS
PRINT , , "MS. HEINDEL'S MUSIC CLASS"
PRINT , , "FINAL GRADES"
PRINT , , "SPRING 1989"
PRINT
PRINT
PRINT "  NAME", " Q1", " Q2", " Q3", " Q4", "AVERAGE"
PRINT
PRINT "D. WOOLY", 100, 92, 90, D4,
PRINT USING "###.##"; (100 + 92 + 90 + D4) / 4
PRINT "M. SMITH", 55, 75, 70, M4,
PRINT USING "###.##"; (55 + 75 + 70 + M4) / 4
PRINT "C. BROWN ", 94, 70, 62, C4,
PRINT USING "###.##"; (94 + 70 + 62 + C4) / 4
PRINT "R. GREEN", 90, 74, 80, R4,
PRINT USING "###.##"; (90 + 74 + 80 + R4) / 4
PRINT "T. STONE", 85, 98, 100, T4,
PRINT USING "###.##"; (85 + 98 + 100 + T4) / 4
PRINT
PRINT USING "\      \    ###.##         ###.##         ###.##       ###.##      ###.##"; "AVERAGE:", (100 + 55 + 94 + 90 + 85) / 5, (92 + 75 + 70 + 74 + 98) / 5, (90 + 70 + 62 + 80 + 100) / 5, (D4 + M4 + C4 + R4 + T4) / 5
PRINT
PRINT USING "\             \ ##.##"; "CLASS AVERAGE: "; (((100 + 55 + 94 + 90 + 85) / 5) + ((92 + 75 + 70 + 74 + 98) / 5) + ((90 + 70 + 62 + 80 + 100) / 5) + ((D4 + M4 + C4 + R4 + T4) / 5)) / 4
Z4 = Z4 + 1
SLEEP
GOTO HOME
HEY:
CALL ANYKEY
END SUB

SUB THREEONE
DIM W$(3000)
CLS
PRINT "3.1"
PRINT
INPUT "ENTER A WORD: ", WORD$
LWORD = LEN(WORD$)
PRINT
FOR X = 1 TO LWORD
    W$(X) = MID$(WORD$, X, 1)
NEXT X
FOR I = 1 TO LWORD
    IF W$(I) = "I" OR W$(I) = "A" THEN
        IF MID$(WORD$, I, 3) = "ING" THEN
            FOR X = LWORD TO I + 3
                IF W$(X) = "E" THEN
                    Z$ = "MISSPELLED"
                    I = LWORD
                ELSE
                END IF
            NEXT X
        ELSEIF MID$(WORD$, I, 4) = "ABLE" OR MID$(WORD$, I, 4) = "IBLE" THEN
            FOR X = LWORD TO I + 4
                IF W$(X) = "E" THEN
                    Z$ = "MISSPELLED"
                    I = LWORD
                ELSE
                END IF
            NEXT X
        END IF
    END IF
NEXT I

IF Z$ = "MISSPELLED" THEN
    GOTO FINISH
ELSE
END IF

FOR X = 1 TO LWORD
    IF W$(X) = "C" AND W$(X + 1) = "I" AND W$(X + 2) = "E" THEN
        Z$ = "MISSPELLED"
    ELSE
    END IF
NEXT X

IF Z$ = "MISSPELLED" THEN
    GOTO FINISH
ELSE
END IF
FOR X = 1 TO LWORD
    IF W$(X) = W$(X + 1) AND W$(X) = W$(X + 2) THEN
        Z$ = "MISSPELLED"
    ELSE
    END IF
NEXT X
IF Z$ <> "MISSPELLED" THEN
    Z$ = "CORRECT"
ELSE
END IF
FINISH:
PRINT Z$
CALL ANYKEY
END SUB

SUB THREETWO
CLS
CALL ANYKEY
END SUB

SUB THREETHREE
CLS
CALL ANYKEY
END SUB

SUB THREEFOUR
CLS
CALL ANYKEY
END SUB

SUB THREEFIVE
CLS
CALL ANYKEY
END SUB

SUB THREESIX
CLS
CALL ANYKEY
END SUB

SUB THREESEVEN
CLS
INPUT "ENTER COST, AMOUNT: ", COST, PAID
INPUT "ENTER MISSING COIN: ", MIS$
MIS$ = UCASE$(MIS$)

SELECT CASE MIS$
    CASE "QUARTER"
        H = 1
    CASE "DIME"
        H = 2
    CASE "NICKEL"
        H = 3
    CASE "PENNY"
    CASE ""
        H = 5
END SELECT

CHNG2 = PAID - COST
CHNG2 = CHNG2 * 100
CHNG = CHNG2

'QUARTER
IF H <> 1 THEN
    QNUM$ = STR$(CHNG / 25)
    CHNG = CHNG MOD 25
    CHECK = 1
    FOR X = 1 TO LEN(QNUM$)
        QPLACE$ = MID$(QNUM$, X, 1)
        IF QPLACE$ = "." THEN
            CHECK = -1
        ELSEIF CHECK = 1 THEN
            QSUM$ = QSUM$ + QPLACE$
        END IF
    NEXT X
    QNUM = VAL(QSUM$)
END IF


'DIME
IF H <> 2 THEN
    DNUM$ = STR$(CHNG / 10)
    CHNG = CHNG MOD 10
    CHECK = 1
    FOR X = 1 TO LEN(DNUM$)
        DPLACE$ = MID$(DNUM$, X, 1)
        IF DPLACE$ = "." THEN
            CHECK = -1
        ELSEIF CHECK = 1 THEN
            DSUM$ = DSUM$ + DPLACE$
        END IF
    NEXT X
    DNUM = VAL(DSUM$)
END IF

'NICKEL
IF H <> 3 THEN
    NNUM$ = STR$(CHNG / 5)
    CHNG = CHNG MOD 5
    CHECK = 1
    FOR X = 1 TO LEN(NNUM$)
        NPLACE$ = MID$(NNUM$, X, 1)
        IF NPLACE$ = "." THEN
            CHECK = -1
        ELSEIF CHECK = 1 THEN
            NSUM$ = NSUM$ + NPLACE$
        END IF
    NEXT X
    NNUM = VAL(NSUM$)
END IF

IF CHNG MOD 5 < 5 AND QNUM > 0 AND H = 3 THEN
    QNUM = QNUM - 1
    DNUM = DNUM + 3
    CHNG = CHNG - 5
END IF


PRINT CHNG; "PENNIES"
IF H <> 3 AND NNUM > 1 THEN
    PRINT NNUM; "NICKELS"
ELSEIF H <> 3 AND N = 1 THEN
    PRINT NNUM; "NICKEL"
END IF
IF H <> 2 AND DNUM > 1 THEN
    PRINT DNUM; "DIMES"
ELSEIF H <> 2 AND DNUM = 1 THEN
    PRINT DNUM; "DIME"
END IF
IF H <> 1 AND QNUM > 1 THEN
    PRINT QNUM; "QUARTERS"
ELSEIF H <> 1 AND QNUM = 1 THEN
    PRINT QNUM; "QUARTER"
END IF

CHNG2$ = STR$(CHNG2)
CHECK = 1
FOR X = 1 TO LEN(CHNG2$)
    CPLACE$ = MID$(CHNG2$, X, 1)
    IF CPLACE$ = "." THEN
        CHECK = -1
    ELSEIF CHECK = 1 THEN
        CSUM$ = CSUM$ + CPLACE$
    END IF
NEXT X
CNUM = VAL(CSUM$)

PRINT "TOTAL CHANGE RETURNED = "; CNUM; "CENTS"

'PENNY IS ANYTHING LEFT OF CHNG
CALL ANYKEY
END SUB

SUB THREEEIGHT
CLS
CALL ANYKEY
END SUB

SUB THREENINE
CLS
DIM OSTR1$(5)
DIM OSTR2$(5)

RESTORE DATABASEA
FOR I = 1 TO 26
    READ ALPHA$(I)
NEXT I

RESTORE DATABASEPT
FOR I = 1 TO 26
    READ PTVALUE(I)
NEXT I

RESTORE DATABASESTR1
FOR I = 1 TO 5
    READ OSTR1$(I)
NEXT I

RESTORE DATABASESTR2
FOR I = 1 TO 5
    READ OSTR2$(I)
NEXT I

FOR I = 1 TO 5
    PRINT USING "\   \ ###      \   \ ###"; OSTR1$(I); CALCVAL(OSTR1$(I)), OSTR2$(I); CALCVAL(OSTR2$(I))
    SUMOSTR1 = SUMOSTR1 + CALCVAL(OSTR1$(I))
    SUMOSTR2 = SUMOSTR2 + CALCVAL(OSTR2$(I))
NEXT I

PRINT SPACE$(5); SUMOSTR1; SPACE$(10); SUMOSTR2

IF SUMOSTR1 > SUMOSTR2 THEN
    PRINT SPACE$(6) + "***"
ELSEIF SUMOSTR1 < SUMOSTR2 THEN
    PRINT SPACE$(21) + "***"
END IF

NEWWORDS$ = "NULL"
WHILE NEWWORDS$ <> ""
    INPUT "Enter word: ", INWORD$
    NEWWORDS$ = UCASE$(INWORD$)
    IF NEWWORDS$ <> "" THEN
        WHILE (LEN(NEWWORDS$) <> 5)
            'PRINT NEWWORDS$
            INPUT "Enter word: ", NEWWORDS$
        WEND
    END IF
    FOR I = 1 TO 5
        IF (MID$(OSTR1$(I), 1, 1) = MID$(NEWWORDS$, 1, 1)) THEN
            IF (CALCVAL(NEWWORDS$) > CALCVAL(OSTR1$(I))) THEN
                OSTR1$(I) = NEWWORDS$
            END IF
        END IF
    NEXT I
    FOR I = 1 TO 5
        IF (MID$(OSTR2$(I), 2, 1) = MID$(NEWWORDS$, 2, 1)) THEN
            IF (CALCVAL(NEWWORDS$) > CALCVAL(OSTR2$(I))) THEN
                OSTR2$(I) = NEWWORDS$
            END IF
        END IF
    NEXT I
WEND

SUMOSTR1 = 0
SUMOSTR2 = 0

PRINT

FOR I = 1 TO 5
    PRINT USING "\   \ ###      \   \ ###"; OSTR1$(I); CALCVAL(OSTR1$(I)), OSTR2$(I); CALCVAL(OSTR2$(I))
    SUMOSTR1 = SUMOSTR1 + CALCVAL(OSTR1$(I))
    SUMOSTR2 = SUMOSTR2 + CALCVAL(OSTR2$(I))
NEXT I

PRINT SPACE$(5); SUMOSTR1; SPACE$(10); SUMOSTR2

IF SUMOSTR1 > SUMOSTR2 THEN
    PRINT SPACE$(6) + "***"
ELSEIF SUMOSTR1 < SUMOSTR2 THEN
    PRINT SPACE$(21) + "***"
END IF

NEWWORDS$ = "NULL"
WHILE NEWWORDS$ <> "QUIT"
    INPUT "Enter word: ", INWORD$
    NEWWORDS$ = UCASE$(INWORD$)
    IF NEWWORDS$ <> "QUIT" THEN
        WHILE (LEN(NEWWORDS$) <> 5)
            'PRINT NEWWORDS$
            INPUT "Enter word: ", NEWWORDS$
        WEND
    END IF
    FOR I = 1 TO 5
        IF (MID$(OSTR1$(I), 1, 1) = MID$(NEWWORDS$, 1, 1)) THEN
            IF (CALCVAL(NEWWORDS$) > CALCVAL(OSTR1$(I))) THEN
                OSTR1$(I) = NEWWORDS$
            END IF
        END IF
    NEXT I
    FOR I = 1 TO 5
        IF (MID$(OSTR2$(I), 2, 1) = MID$(NEWWORDS$, 2, 1)) THEN
            IF (CALCVAL(NEWWORDS$) > CALCVAL(OSTR2$(I))) THEN
                OSTR2$(I) = NEWWORDS$
            END IF
        END IF
    NEXT I
WEND

WHILE NEWWORDS$ <> "QUIT"
    FOR I = 1 TO 5
        PRINT USING "\   \ ###      \   \ ###"; OSTR1$(I); CALCVAL(OSTR1$(I)), OSTR2$(I); CALCVAL(OSTR2$(I))
        SUMOSTR1 = SUMOSTR1 + CALCVAL(OSTR1$(I))
        SUMOSTR2 = SUMOSTR2 + CALCVAL(OSTR2$(I))
    NEXT I

    PRINT SPACE$(5); SUMOSTR1; SPACE$(10); SUMOSTR2

    IF SUMOSTR1 > SUMOSTR2 THEN
        PRINT SPACE$(6) + "***"
    ELSEIF SUMOSTR1 < SUMOSTR2 THEN
        PRINT SPACE$(21) + "***"
    END IF


    SUMOSTR1 = 0
    SUMOSTR2 = 0

WEND
CALL ANYKEY
END SUB

SUB THREETEN
CLS
CALL ANYKEY
END SUB



FUNCTION Factorial (x)
IF x = 0 THEN
    Factorial = 1
ELSE
    Factorial = x * Factorial(x - 1)
END IF
END FUNCTION

FUNCTION CALCVAL (X$)
FOR I = 1 TO LEN(X$)
    'PRINT MID$(X$, I, 1)
    PLACE$ = MID$(X$, I, 1)
    NUM = 1
    'PRINT ALPHA$(NUM)
    WHILE PLACE$ <> ALPHA$(NUM)
        NUM = NUM + 1
    WEND
    PT = PT + PTVALUE(NUM)
NEXT I
CALCVAL = PT
'PRINT PT
END FUNCTION

