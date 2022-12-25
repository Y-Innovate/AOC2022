         TITLE 'ADVENTCODE PUZZLE 3B'
*
         COPY  ASMMSP              * Enable HLASM struct.prog.macro's
*
*---------------------------------------------------------------------*
* PUZZLE3B mainline                                                   *
*---------------------------------------------------------------------*
PUZZLE3B CEEENTRY AUTO=WORKDSA_SIZ,MAIN=YES,BASE=(R10,R11)
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
            B     PUZZLE3B_RET_IMMED * End program
         ENDIF
*
         L     R9,WA_PTR           * Put ptr to working storage in R9
         USING WORKAREA,R9         * Assign R9 to WORKAREA
*
         BAL   R8,INIT             * Perform initializations
*
         CLC   RETCODE,=A(0)       * All good so far?
         BNE   PUZZLE3B_RET        * If not, get out
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
PUZZLE3B_RET EQU   *               * Branch here for normal end
*
         BAL   R8,WRAPUP           * Perform wrap things up
*
         L     R2,RETCODE          * Put return code in R2
*
*                                  * Free memory for working stg
         CALL  CEEFRST,(WA_PTR,FC),VL,MF=(E,PAR7)
*
PUZZLE3B_RET_IMMED EQU   *         * Branch here when alloc mem failed
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
         MVI   INP_RECS,X'00'      * Initialize records read
*
         MVC   TOTALSCORE,=A(0)    * Initialize total score
*
         MVC   PATHLEN,=A(27)
         MVC   PATHNAME(27),=C'/u/yin/ybtks/day3_input.txt'
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
         LT    R7,INPRECLEN
*
         IF (NZ) THEN              * Just to be sure skip empty
            IF (CLI,INP_RECS,EQ,X'00') THEN
               MVC   INPREC2,INPREC
               MVC   INPREC2LEN,INPRECLEN
               MVI   INP_RECS,X'01'
               B     PROCESS_RET
            ELSEIF (CLI,INP_RECS,EQ,X'01')
               MVC   INPREC3,INPREC
               MVC   INPREC3LEN,INPRECLEN
               MVI   INP_RECS,X'02'
               B     PROCESS_RET
            ENDIF
*
            LA    R2,TR_WORK       * Initialize
            LA    R3,256*2         *    work TR tables
            XR    R0,R0            *       with
            XR    R1,R1            *          all
            MVCL  R2,R0            *             zeros
*
            L     R6,INPRECLEN     * Get last rec length
            XR    R5,R5            * Clear out R5 for character
            XR    R4,R4            * Clear out R4 for offset
            LA    R3,INPREC        * Point R3 to input line
FILL_ENTRY  LA    R2,TR_WORK       * Point R2 to work TR table 1
            IC    R5,0(R4,R3)      * Get current char
            AR    R2,R5            * Advance R2 to this character
            STC   R5,0(,R2)        * Put char in TR table
            TR    0(1,R2),TR_PRIO  * TRanslate char to prio
            LA    R4,1(,R4)        * Increase offset
            BCT   R6,FILL_ENTRY    * Loop around for next entry
*
            L     R6,INPREC2LEN    * Get first buffered rec length
            XR    R5,R5            * Clear out R5 for character
            XR    R4,R4            * Clear out R4 for offset
            LA    R3,INPREC2       * Point R3 to first buffered rec
FILL_ENTRY2 LA    R2,TR_WORK       * Point R2 to work TR table 1
            IC    R5,0(R4,R3)      * Get current char
            AR    R2,R5            * Advance R2 to this character
            IF (CLI,0(R2),NE,X'00') THEN * If char also in rec 1
               LA    R1,TR_WORK2   * Point R2 to work TR table 2
               AR    R1,R5         * Advance R2 to this character
               MVC   0(1,R1),0(R2) * Copy prio
            ENDIF
            LA    R4,1(,R4)        * Increase offset
            BCT   R6,FILL_ENTRY2   * Loop around for next entry
*
            L     R6,INPREC3LEN    * Get first buffered rec length
            LA    R3,INPREC3       * Point R3 to first buffered rec
            XR    R2,R2            * Prepare to receive TR result
            BCTR  R6,R0            * Prepare for EXecute
            B     *+10             * Skip past TR instruction to EX
            TRT   0(1,R3),TR_WORK2 * Test for previously seen char
            EX    R6,*-6           * EXecute TRT instruction
*
            A     R2,TOTALSCORE    * Prio should be in R2, so add
            ST    R2,TOTALSCORE    *    to total score
         ENDIF
*
         MVI   INP_RECS,X'00'      * Start new triplet
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
         DS    0F
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F
TR_PRIO  DC    X'00000000000000000000000000000000'  0
         DC    X'00000000000000000000000000000000'  1
         DC    X'00000000000000000000000000000000'  2
         DC    X'00000000000000000000000000000000'  3
         DC    X'00000000000000000000000000000000'  4
         DC    X'00000000000000000000000000000000'  5
         DC    X'00000000000000000000000000000000'  6
         DC    X'00000000000000000000000000000000'  7
         DC    X'00010203040506070809000000000000'  8 a-i
         DC    X'000A0B0C0D0E0F101112000000000000'  9 j-r
         DC    X'0000131415161718191A000000000000'  A s-z
         DC    X'00000000000000000000000000000000'  B
         DC    X'001B1C1D1E1F20212223000000000000'  C A-I
         DC    X'002425262728292A2B2C000000000000'  D J-R
         DC    X'00002D2E2F3031323334000000000000'  E
         DC    X'00000000000000000000000000000000'  F
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
INP_RECS                     DS    C     * Counter 1,2,3 recs read
*
INPRECA                      DS    A
INPREC                       DS    CL80  * INPUT record
INPRECLEN                    DS    F
INPREC2                      DS    CL80  * INPUT record 2
INPREC2LEN                   DS    F
INPREC3                      DS    CL80  * INPUT record 3
INPREC3LEN                   DS    F
*
MATCH1PRIO                   DS    C     * Matching prio in recs 1-2
MATCH2PRIO                   DS    C     * Matching prio in recs 2-3
MATCH1PTR                    DS    A     * Ptr to matching char 1-2
MATCH2PTR                    DS    A     * Ptr to matching char 2-3
*
TOTALSCORE                   DS    F     * My total score
*
TR_WORK                      DS    CL256 * Working copy of TR table
TR_WORK2                     DS    CL256 * Working copy of TR table 2
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
PUZZLE3B CSECT
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
         END   PUZZLE3B
