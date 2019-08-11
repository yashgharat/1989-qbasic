DIM BOX$(6)
DIM CORD(100)
CLS
PRINT 3.8
PRINT
FOR I = 1 TO 6
    STRI$ = ""
    INPUT "ENTER NUMBER: ", d
    WHILE d <> 0
        X = d MOD 2
        X$ = STR$(X)
        STRI$ = X$ + STRI$
        STRI$ = LTRIM$(STRI$)
        d = d \ 2
        '    PRINT "D = "; D; "S$ = "; S$
    WEND
    WHILE LEN(STRI$) <> 7
        STRI$ = "0" + STRI$
        STRI$ = LTRIM$(STRI$)
    WEND
    BOX$(I) = STRI$
NEXT I


CLS
PRINT 3.8
PRINT
FOR I = 1 TO 6: PRINT BOX$(I): NEXT I
PRINT

FOR I = 1 TO 6
    FOR J = 1 TO 6
        PLACE$ = MID$(BOX$(J), I, 1)
        PLACE2$ = MID$(BOX$(J), I + 1, 1)
        CHECK = 1
        'IF I = 1 THEN PRINT PLACE$, PLACE2$
        IF PLACE$ = "0" OR PLACE2$ = "0" THEN
            CHECK = 0
            SUM2 = SUM2 + 1
        ELSEIF PLACE$ = "1" AND PLACE2$ = "1" AND CHECK <> 0 THEN
            IF SUM = 0 THEN
                HI = J
            ELSEIF SUM > 0 THEN
                HI2 = J
            END IF
            SUM = SUM + 1
            SUM2 = 0
        END IF
        IF CHECK = 0 AND SUM2 < 2 AND SUM > 3 THEN
            PRINT "("; HI; ","; I; ") ("; HI2; ","; I + 1; ")"
        END IF
    NEXT J
    SUM = 0
    SUM2 = 0
NEXT I

FOR I = 1 TO 5
    FOR J = 1 TO 7
        PLACE$ = MID$(BOX$(I), J, 1)
        PLACE2$ = MID$(BOX$(I + 1), J, 1)
        'IF I = 5 AND J > 2 THEN PRINT PLACE$; PLACE2$; SUM
        CHECK = 1
        IF PLACE$ = "0" OR PLACE2$ = "0" THEN
            CHECK = 0
            SUM2 = SUM2 + 1
        ELSEIF PLACE$ = "1" AND PLACE2$ = "1" AND CHECK <> 0 THEN
            IF SUM = 0 THEN
                HI = J
            ELSEIF SUM > 0 THEN
                HI2 = J
            END IF
            SUM = SUM + 1
            SUM2 = 0
        END IF
        IF CHECK = 0 AND SUM2 <= 1 AND SUM >= 2 THEN
            PRINT "("; I; ","; HI; ") ("; I + 1; ","; HI2; ")"
        END IF
    NEXT J
    SUM = 0
    SUM2 = 0
NEXT I

