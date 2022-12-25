         TITLE 'ADVENTCODE PUZZLE 2A'
*
         COPY  ASMMSP              * Enable HLASM struct.prog.macro's
*
*---------------------------------------------------------------------*
* PUZZLE2A mainline                                                   *
*---------------------------------------------------------------------*
PUZZLE2A CEEENTRY AUTO=WORKDSA_SIZ,MAIN=YES,BASE=(R10,R11)
*
         USING WORKDSA,R13         * Assign R13 to WORKDSA
*
*                                  * Allocate memory for working stg
         CALL  CEEGTST,(=A(0),=A(WORKAREA_SIZ),WA_PTR,FC),VL,          X
               MF=(E,PAR7)
*
*                                  * Was memory successfully allocated?
         IF (CLC,FC(8),NE,=XL8'0000000000000000') THEN
            L     R2,=A(16)        * Nope, just return 16
            B     PUZZLE2A_RET_IMMED * End program
         ENDIF
*
         L     R9,WA_PTR           * Put ptr to working storage in R9
         USING WORKAREA,R9         * Assign R9 to WORKAREA
*
         BAL   R8,INIT             * Perform initializations
*
         CLC   RETCODE,=A(0)       * All good so far?
         BNE   PUZZLE2A_RET        * If not, get out
*
READNEXT EQU   *
*
         BAL   R8,READLINE         * Read an INPUT record
*
         IF (CLI,INP_EOF,EQ,C'Y') THEN * Have we reached EOF yet?
            B     NO_MORE_INPUT
         ENDIF
*
         BAL   R8,PROCESS          * Process an INPUT record
*
         B     READNEXT            * Loop around for next record
*
NO_MORE_INPUT EQU   *              * No more INPUT records
*
         BAL   R8,RESULT           * Write result to sysout
*
PUZZLE2A_RET EQU   *               * Branch here for normal end
*
         BAL   R8,WRAPUP           * Perform wrap things up
*
         L     R2,RETCODE          * Put return code in R2
*
*                                  * Free memory for working stg
         CALL  CEEFRST,(WA_PTR,FC),VL,MF=(E,PAR7)
*
PUZZLE2A_RET_IMMED EQU   *         * Branch here when alloc mem failed
*
         CEETERM RC=(R2),MF=(E,PAR7) * End program
*
*---------------------------------------------------------------------*
* Initializations                                                     *
*---------------------------------------------------------------------*
INIT     DS    0H
*
         MVC   RETCODE,=A(0)       * Initialize retcode
*
         MVI   INP_OPEN,C'N'       * Initialize INPUT is not open
*
         MVC   TOTALSCORE,=A(0)    * Initialize total score
*
         MVC   PATHLEN,=A(27)
         MVC   PATHNAME(27),=C'/u/yin/ybtks/day2_input.txt'
opts     USING O_FLAGS,OOPTIONS
         MVI   opts.O_FLAGS4,O_RDONLY * Open file read-only
         DROP  opts
mod      USING S_MODE,OMODE
         MVI   mod.S_TYPE,FT_REGFILE * It's a regular file
         MVI   mod.S_MODE2,S_IRUSR * U=R--
         MVI   mod.S_MODE3,S_IRGRP+S_IROTH * G=R--,O=R--
         DROP  mod
*
         CALL  BPX1OPN,(PATHLEN,PATHNAME,OOPTIONS,OMODE,OPENRETV,      X
               BPXRETC,BPXRSNC),VL,MF=(E,PAR7)
*
         IF (CLC,OPENRETV,NE,=F'-1') THEN * If open was successful
            MVI   INP_OPEN,C'Y'    * Set INPUT is open
            MVI   INP_EOF,C'N'     * Set INPUT is not EOF
         ELSE
            CALL  PUZ#ERR,(OPENERR),MF=(E,PAR7)
         ENDIF
*
INIT_RET EQU   *                   * Done with initializations
*
         BR    R8                  * Return to caller
*
*---------------------------------------------------------------------*
* Wrap things up                                                      *
*---------------------------------------------------------------------*
WRAPUP   DS    0H
*
         IF (CLI,INP_OPEN,EQ,C'Y') THEN * If file was opened earlier
            CALL  BPX1CLO,(OPENRETV,CLSERETV,BPXRETC,BPXRSNC),VL,      X
               MF=(E,PAR7)
*
            IF (CLC,OPENRETV,EQ,=F'-1') THEN * If close unsuccessful
               CALL  PUZ#ERR,(CLOSEERR),MF=(E,PAR7)
            ENDIF
         ENDIF
*
WRAPUP_RET EQU   *
         BR    R8                  * Return to caller
*
*---------------------------------------------------------------------*
* Read an INPUT record                                                *
*---------------------------------------------------------------------*
READLINE DS    0H
*
         LA    R7,INPREC           * Start at the beginning of buffer
         LA    R6,L'INPREC         * All of buffer is left to fill
*
         DO UNTIL=(CLC,RETCODE,NE,=A(0))
*
            ST    R7,INPRECA       * Save R7 as buffer pointer
*
            CALL  BPX1RED,(OPENRETV,INPRECA,=A(0),=A(1),READRETV,      X
               BPXRETC,BPXRSNC),VL,MF=(E,PAR7)
*
            IF (CLC,READRETV,EQ,=F'1') THEN * If we read a character
               IF (CLI,0(R7),EQ,X'15') THEN * If new line, quit loop
                  ASMLEAVE
               ENDIF
*
            ELSEIF (CLC,READRETV,EQ,=A(0)) THEN * If no character read
               MVI   INP_EOF,C'Y'  * Then we're at EOF, quit loop
*
               ASMLEAVE
*
*           errno -1, so error and quit
            ELSE
               CALL  PUZ#ERR,(READERR),MF=(E,PAR7)
*
               B     READLINE_RET
            ENDIF
*
            S     R6,=A(1)         * Decrease buf space left by 1
            IF (Z) THEN            * No more buf?
               MVC   RETCODE,=A(12) * Error
*
               CALL  CEEMOUT,(BUFFERERR,=A(2),FC),VL,MF=(E,PAR7)
*
               B     READLINE_RET  * And get out
            ENDIF
*
            LA    R7,1(,R7)        * Advance 1 position in buffer
*
         ENDDO
*
         LA    R5,INPREC           * Calculate
         SR    R7,R5               *   length of line just read
         ST    R7,INPRECLEN        *      and store it
*
READLINE_RET EQU   *
         BR    R8                  * Return to caller
*
*---------------------------------------------------------------------*
* Process an input record                                             *
*---------------------------------------------------------------------*
PROCESS  DS    0H
*
         IF (CLC,INPRECLEN,EQ,=A(3)) THEN * Just to be sure skip empty
            LA    R7,INPREC        * Point R7 to input rec
            SELECT CLI,2(R7),EQ    * Determine my choice first
            WHEN C'X'              * I chose X = Rock
               LA    R6,1          * My score should go up with 1
               SELECT CLI,0(R7),EQ * Determine opponent's choice
               WHEN C'A'           * They chose A = Rock
                  LA    R6,3(,R6)  * Which is a draw, score up with 3
*              WHEN C'B'           * They chose B = Paper
*                 NOP              * Which is a loss, no added score
               WHEN C'C'           * They chose C = Scissors
                  LA    R6,6(,R6)  * Which is a win, score up with 6
               ENDSEL
            WHEN C'Y'              * I chose Y = Paper
               LA    R6,2          * My score should go up with 2
               SELECT CLI,0(R7),EQ * Determine opponent's choice
               WHEN C'A'           * They chose A = Rock
                  LA    R6,6(,R6)  * Which is a win, score up with 6
               WHEN C'B'           * They chose B = Paper
                  LA    R6,3(,R6)  * Which is a draw, score up with 3
*              WHEN C'C'           * They chose C = Scissors
*                 NOP              * Which is a loss, no added score
               ENDSEL
            WHEN C'Z'              * I chose Z = Scissors
               LA    R6,3          * My score should go up with 3
               SELECT CLI,0(R7),EQ * Determine opponent's choice
*              WHEN C'A'           * They chose A = Rock
*                 NOP              * Which is a loss, no added score
               WHEN C'B'           * They chose B = Paper
                  LA    R6,6(,R6)  * Which is a win, score up with 6
               WHEN C'C'           * They chose C = Scissors
                  LA    R6,3(,R6)  * Which is a draw, score up with 3
               ENDSEL
            ENDSEL
*
            A     R6,TOTALSCORE    * Add this round's score to total
            ST    R6,TOTALSCORE    * and put it back in the var
         ENDIF
*
PROCESS_RET EQU   *
         BR    R8                  * Return to caller
*
*---------------------------------------------------------------------*
* Write out result                                                    *
*---------------------------------------------------------------------*
RESULT   DS    0H
*
*
         XR    R0,R0               * Check for zero terminator
         LA    R6,MSG+2            * Point to start of message
         LA    R7,RESULTMSG        * Text to copy (parm in R1)
         MVST  R6,R7               * Copy zero term string
         BC    1,*-4               * If interrupted, continue
*
         L     R15,TOTALSCORE      * Put total score in R15
         CVD   R15,DEC8            * Convert to decimal
         UNPK  ZONED8,DEC8         * Convert to packed
         OI    ZONED8+7,X'F0'      * Get rid of sign at the end
         MVI   ZONED8+8,X'00'      * Add zero terminator
         LARL  R14,TRT_SKIP_ZEROS  * Point R14 to TRT table
         TRT   ZONED8(7),0(R14)    * Trim leading zeros
         BC    7,*+8               * CC=0 is all zeros
         LA    R1,ZONED8+7         * if so, the point to last digit
         LR    R7,R1               * In any case point R7 to number
         MVST  R6,R7               * Append number
         BC    1,*-4               * If interrupted, continue
*
         LA    R5,MSG+2            * Get start of error message
         SR    R6,R5               * Calculate length
         STH   R6,MSG              * Store as length prefix
*
         CALL  CEEMOUT,(MSG,=A(2),FC),VL,MF=(E,PAR7)
*
RESULT_RET EQU   *
         BR    R8                  * Return to caller
*
         LTORG
*
*---------------------------------------------------------------------*
* Constants                                                           *
*---------------------------------------------------------------------*
*
*                                  * Error messages
                             DS    0F
RESULTMSG                    DC    C'TOTAL SCORE WAS ',X'00'
                             DS    0F
OPENERR                      DC    C'OPEN INPUT FAILED ',X'00'
                             DS    0F
CLOSEERR                     DC    C'CLOSE INPUT FAILED ',X'00'
                             DS    0F
READERR                      DC    C'READ INPUT FAILED ',X'00'
*
BUFFERERR                    DS    0F
                             DC    AL2(L'BUFFERERRTXT)
BUFFERERRTXT                 DC    C'RAN OUT OF READ BUFFER'
*
* LE Program Prolog Area
PPA                          CEEPPA
*
*---------------------------------------------------------------------*
* DSECTs                                                              *
*---------------------------------------------------------------------*
*
WORKDSA                      DSECT
*
                             ORG   *+CEEDSASZ
*
WA_PTR                       DS    A   * Pointer to my working storage
PAR7                         DS    7A  * Room for passing max 7 parms
FC                           DS    3A  * CEE feedback code
*
WORKDSA_SIZ                  EQU   *-WORKDSA
*
WORKAREA                     DSECT
*
RETCODE                      DS    A     * Return code
*
PATHLEN                      DS    A     * Path length
PATHNAME                     DS    CL40  * Path
OOPTIONS                     DS    F     * Open options
OMODE                        DS    F     * Open mode
OPENRETV                     DS    F     * Open return value
CLSERETV                     DS    F     * Close return value
READRETV                     DS    F     * Read return value
BPXRETC                      DS    F     * Return code
BPXRSNC                      DS    F     * Reason code
*
INP_EOF                      DS    C     * EOF char for DCBINP
INP_OPEN                     DS    C     * Flag for DCBINP is open
*
INPRECA                      DS    A
INPREC                       DS    CL80  * INPUT record
INPRECLEN                    DS    F
*
TOTALSCORE                   DS    F     * My total score
*
                             DS    0A
MSG                          DS    AL2,CL80  * Error message
*
                             DS    0AD
DEC8                         DS    PL8   * Working fields
ZONED8                       DS    CL8   * for numeric type conversion
ZONED8_EXTRA                 DS    CL4
*
WORKAREA_SIZ                 EQU   *-WORKAREA
*
         BPXYOPNF
         BPXYMODE
         BPXYFTYP
*
         CEECAA
*
         CEEDSA
*
PUZZLE2A CSECT
*
         DROP
*
* Construct error message with errno and errno2
*
PUZ#ERR  CEEENTRY AUTO=WORKDSA#ERR_SIZ,MAIN=NO,BASE=(R10)
*
         USING WORKDSA#ERR,R13     * Assign R13 to WORKDSA#ERR
*
         USING WORKAREA,R9         * Assign R9 to WORKAREA
*
         XR    R0,R0               * Check for zero terminator
         LA    R6,MSG+2            * Point to start of error message
         L     R7,0(,R1)           * Text to copy (parm in R1)
         MVST  R6,R7               * Copy zero term string
         BC    1,*-4               * If interrupted, continue
*
         L     R15,BPXRETC         * Put errno in R15
         CVD   R15,DEC8            * Convert to decimal
         UNPK  ZONED8,DEC8         * Convert to packed
         OI    ZONED8+7,X'F0'      * Get rid of sign at the end
         MVI   ZONED8+8,X'00'      * Add zero terminator
         TRT   ZONED8(7),TRT_SKIP_ZEROS * Trim leading zeros
         BC    7,*+8               * CC=0 is all zeros
         LA    R1,ZONED8+7         * if so, the point to last digit
         LR    R7,R1               * In any case point R7 to number
         MVST  R6,R7               * Append number
         BC    1,*-4               * If interrupted, continue
*
         MVC   0(2,R6),=X'4000'    * Append space and zero term
         LA    R6,1(,R6)           * Advance 1 position
*
         UNPK  ZONED8(5),BPXRSNC+2(3) * Convert errno2 to hex
         TR    ZONED8(4),HEXTAB    * Turn into true hex
         MVI   ZONED8+4,X'00'      * Add zero term
         LA    R7,ZONED8           * Point R7 to hex string to append
         MVST  R6,R7               * Append hex string
         BC    1,*-4               * If interrupted, continue
*
         LA    R5,MSG+2            * Get start of error message
         SR    R6,R5               * Calculate length
         STH   R6,MSG              * Store as length prefix
*
         CALL  CEEMOUT,(MSG,=A(2),FC#ERR),VL,MF=(E,PAR3#ERR)
*
         MVC   RETCODE,=A(12)      * Give back error return code
*
PUZ#ERR_RET EQU   *
         CEETERM
*
         LTORG
*
*---------------------------------------------------------------------*
* Constants                                                           *
*---------------------------------------------------------------------*
*
                             DS    0F
HEXTAB                       EQU   *-C'0'
                             DC    C'0123456789ABCDEF'
*
TRT_SKIP_ZEROS               DS    0F
                             DC    (C'0')X'FF'
                             DC    X'00'
                             DC    (255-C'0'-1)X'FF'
*
*---------------------------------------------------------------------*
* DSECTs                                                              *
*---------------------------------------------------------------------*
*
WORKDSA#ERR                  DSECT
*
                             ORG   *+CEEDSASZ
*
PAR3#ERR                     DS    3A    * Room for passing max 3 parms
FC#ERR                       DS    3A    * CEE feedback code
*
WORKDSA#ERR_SIZ              EQU   *-WORKDSA#ERR
*
*---------------------------------------------------------------------*
* Register equates                                                    *
*---------------------------------------------------------------------*
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         END   PUZZLE2A
