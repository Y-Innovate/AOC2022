         TITLE 'ADVENTCODE PUZZLE 6A'
*
         COPY  ASMMSP              * Enable HLASM struct.prog.macro's
*
*---------------------------------------------------------------------*
* PUZZLE6A mainline                                                   *
*---------------------------------------------------------------------*
PUZZLE6A CEEENTRY AUTO=WORKDSA_SIZ,MAIN=YES,BASE=(R10,R11)
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
            B     PUZZLE6A_RET_IMMED * End program
         ENDIF
*
         L     R9,WA_PTR           * Put ptr to working storage in R9
         USING WORKAREA,R9         * Assign R9 to WORKAREA
*
         BAL   R8,INIT             * Perform initializations
*
         CLC   RETCODE,=A(0)       * All good so far?
         BNE   PUZZLE6A_RET        * If not, get out
*
READNEXT EQU   *
*
         BAL   R8,READCHAR         * Read an INPUT record
*
         IF (CLI,INP_EOF,EQ,C'Y') THEN * Have we reach EOF yet?
            B     NO_MORE_INPUT
         ENDIF
*
         BAL   R8,PROCESS          * Process an INPUT record
*
         IF (CLI,FOUND_IT,EQ,C'Y') THEN * Found the sequence yet?
            B     NO_MORE_INPUT
         ENDIF
*
         B     READNEXT            * Loop around for next record
*
NO_MORE_INPUT EQU   *              * No more INPUT records
*
         BAL   R8,RESULT           * Write result to sysout
*
PUZZLE6A_RET EQU   *               * Branch here for normal end
*
         BAL   R8,WRAPUP           * Perform wrap things up
*
         L     R2,RETCODE          * Put return code in R2
*
*                                  * Free memory for working stg
         CALL  CEEFRST,(WA_PTR,FC),VL,MF=(E,PAR7)
*
PUZZLE6A_RET_IMMED EQU   *         * Branch here when alloc mem failed
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
         MVC   CHARCOUNT,=A(0)     * Initialize character counter
*
         MVI   FOUND_IT,C'N'       * Initialize to not found it
*
         MVC   PATHLEN,=A(27)
         MVC   PATHNAME(27),=C'/u/yin/ybtks/day6_input.txt'
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
* Read an INPUT character                                             *
*---------------------------------------------------------------------*
READCHAR DS    0H
*
         MVC   INPREC(3),INPREC+1  * Shift last 3 read chars left 1 pos
*
         LA    R7,INPREC+3         * Point R7 to pos 4
         ST    R7,INPRECA          * Save R7 as buffer pointer
*
         CALL  BPX1RED,(OPENRETV,INPRECA,=A(0),=A(1),READRETV,         X
               BPXRETC,BPXRSNC),VL,MF=(E,PAR7)
*
         IF (CLC,READRETV,EQ,=F'1') THEN * If we read a character
            IF (CLI,0(R7),EQ,X'15') THEN * If new line
               MVI   INP_EOF,C'Y'  * Then we're at EOF, quit loop
*
               B     READCHAR_RET
            ENDIF
*
            ASI   CHARCOUNT,1
*
         ELSEIF (CLC,READRETV,EQ,=A(0)) THEN * If no character read
            MVI   INP_EOF,C'Y'  * Then we're at EOF, quit loop
*
            B     READCHAR_RET
*
*        errno -1, so error and quit
         ELSE
            CALL  PUZ#ERR,(READERR),MF=(E,PAR7)
*
            B     READCHAR_RET
         ENDIF
*
READCHAR_RET EQU   *
         BR    R8                  * Return to caller
*
*---------------------------------------------------------------------*
* Process an input record                                             *
*---------------------------------------------------------------------*
PROCESS  DS    0H
*
         IF (CLC,CHARCOUNT,LT,=A(4)) THEN * Skip until we have at
            B      PROCESS_RET     * least 4 chars
         ENDIF
*
         L     R2,INPREC           * Get 4 chars in R2
         XR    R3,R3               * Clear out R3
         ICM   R3,4,INPREC         * Repeat first char 3 times at 0111
         ICM   R3,2,INPREC
         ICM   R3,1,INPREC
         XR    R3,R2               * Exclusive Or with 4 chars
         ST    R3,INPREC+4         * Put in memory for testing
         TM    INPREC+5,X'FF'      * Was char 2 equal?
         IF (NZ) THEN              * If not
            TM    INPREC+6,X'FF'   * Was char 3 equal?
            IF (NZ) THEN           * If not
               TM    INPREC+7,X'FF' * Was char 4 equal?
            ENDIF
         ENDIF
*
         BZ    PROCESS_RET         * If any were equal, get out
*
         XR    R3,R3               * Clear out R3
         ICM   R3,2,INPREC+1       * Repeat second char 2 times at 0011
         ICM   R3,1,INPREC+1
         XR    R3,R2               * Exclusive Or with 4 chars
         ST    R3,INPREC+4         * Put in memory for testing
         TM    INPREC+6,X'FF'      * Was char 3 equal?
         IF (NZ) THEN              * If not
            TM    INPREC+7,X'FF'   * Was char 4 equal?
         ENDIF
*
         BZ    PROCESS_RET         * If any were equal, get out
*
         XR    R3,R3               * Clear out R3
         IC    R3,INPREC+2         * Repeat third char at 0001
         XR    R3,R2               * Exclusive Or with 4 chars
         ST    R3,INPREC+4         * Put in memory for testing
         TM    INPREC+7,X'FF'      * Was char 4 equal?
*
         BZ    PROCESS_RET         * If so, get out
*
         B     PROCESS_RET_FOUND   * If we end up here, all were
*                                  * different, so jump to found it!
*
PROCESS_RET EQU   *
         BR    R8                  * Return to caller
*
PROCESS_RET_FOUND EQU   *
         MVI   FOUND_IT,C'Y'
*
         BR    R8                  * Return to caller
*
*---------------------------------------------------------------------*
* Write out result                                                    *
*---------------------------------------------------------------------*
RESULT   DS    0H
*
         XR    R0,R0               * Check for zero terminator
         LA    R6,MSG+2            * Point to start of message
         LA    R7,RESULTMSG        * Text to copy (parm in R1)
         MVST  R6,R7               * Copy zero term string
         BC    1,*-4               * If interrupted, continue
*
         L     R15,CHARCOUNT       * Put character count in R15
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
RESULTMSG                    DC    C'CHARS PROCESSED ',X'00'
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
FOUND_IT                     DS    C     * Flag to indicate seq found
*
INPRECA                      DS    A
INPREC                       DS    CL80  * INPUT record
INPRECLEN                    DS    F
*
CHARCOUNT                    DS    F
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
PUZZLE6A CSECT
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
         END   PUZZLE6A
