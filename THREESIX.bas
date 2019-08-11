DIM NUM$(2)
DIM BOX(2, 100)
DIM CALC(100)
PRINT 3.8
PRINT

INPUT "ENTER BASE: ", BAS
INPUT "ENTER FIRST INTEGER: ", NUM$(1)
INPUT "ENTER SECOND INTEGER: ", NUM$(2)

LEN1 = LEN(NUM$(1))
LEN2 = LEN(NUM$(2))

IF LEN2 > LEN1 THEN
    T = LEN2
ELSEIF LEN1 > LEN2 THEN
    T = LEN1
ELSE
    T = LEN1
END IF


FOR I = 1 TO 2
    PLACE$ = MID$(NUM$(I), 1, 1)
    IF PLACE$ = "-" THEN
    ELSEIF PLACE$ <> "-" THEN
        NUM$(I) = " " + NUM$(I)
    END IF
NEXT I

FOR I = 1 TO 2
    FOR J = 2 TO LEN(NUM$(I))
        BOX(I, J) = VAL(MID$(NUM$(I), J, 1))
        IF MID$(NUM$(I), J - 1, 1) = "-" THEN
            BOX(I, J) = BOX(I, J) * -1
            BOX(I, J - 1) = 0
        END IF
    NEXT J
NEXT I

V = 1

FOR I = (LEN2 + 1) TO 2 STEP -1
    PROD = 0
    PLC = 0
    PLC$ = ""
    CARRY = 0
    FOR J = (LEN1 + 1) TO 2 STEP -1
        PROD = (BOX(2, I) * BOX(1, J)) + CARRY
        CARRY = INT(PROD / BAS)
        IF J > 2 THEN
            PLC = PROD MOD BAS
        ELSEIF J = 2 THEN
            PLC = PROD
        END IF
        PLC$ = LTRIM$(STR$(PLC)) + PLC$
    NEXT J
    CALC(V) = VAL(LTRIM$(PLC$))
    IF SGN(CALC(V)) = -1 THEN
        CALC(V) = CALC(V) / 10
    ELSE
    END IF
    'PRINT CALC(V)
    V = V + 1
NEXT I

FOR I = 1 TO T
    SUM = SUM + (CALC(I) * (10 ^ (I - 1)))
NEXT

PRINT SUM

'FOR I = 1 TO 2
'FOR J = 1 TO LEN(NUM$(I))
'PRINT BOX(I, J);
'NEXT J
'PRINT
'NEXT I





