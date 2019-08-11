DIM SOL$(15)

PRINT "ROWS = "; 1; 2; 3; 4; 5
PRINT "-----------------------"
PRINT "COLUMNS"


SUM = 1
FOR X = 1 TO 5
    FOR Y = 1 TO 5
        FOR K = 1 TO 5
            FOR I = 1 TO 5
                FOR J = 1 TO 5
                    X$ = LTRIM$(STR$(X)): Y$ = LTRIM$(STR$(Y)): K$ = LTRIM$(STR$(K)): I$ = LTRIM$(STR$(I)): J$ = LTRIM$(STR$(J))
                    CHECK$ = X$ + Y$ + K$ + I$ + J$
                    FOR Z = 1 TO LEN(CHECK$)
                        PLACE$ = MID$(CHECK$, Z, 1)
                        FOR C = Z + 1 TO LEN(CHECK$)
                            PLACE2$ = MID$(CHECK$, C, 1)
                            IF PLACE$ = PLACE2$ THEN
                                PAUSE = 1
                            ELSE
                            END IF
                        NEXT C
                    NEXT Z
                    IF PAUSE = 0 THEN
                        FOR Z = 1 TO LEN(CHECK$) - 1
                            PLACE$ = MID$(CHECK$, Z, 1)
                            PLACE2$ = MID$(CHECK$, Z + 1, 1)
                            P = VAL(PLACE$)
                            P2 = VAL(PLACE2$)
                            IF P2 - P >= 2 OR P - P2 >= 2 THEN
                            ELSE
                                PAUSE = 1
                            END IF
                        NEXT Z
                    END IF
                    IF PAUSE = 0 THEN
                        SOL$(SUM) = CHECK$
                        SUM = SUM + 1
                    ELSE
                    END IF
                    PAUSE = 0
                    CHECK$ = ""
                NEXT J
            NEXT I
        NEXT K
    NEXT Y
NEXT X
FOR I = 1 TO SUM - 1
    PRINT SPACE$(8);
    FOR J = 1 TO 5
        PLACE$ = MID$(SOL$(I), J, 1)
        PRINT PLACE$; "  ";
    NEXT J
    PRINT
NEXT I
