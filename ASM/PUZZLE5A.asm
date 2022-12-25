         TITLE 'ADVENTCODE PUZZLE 5A'
*
         COPY  ASMMSP              * Enable HLASM struct.prog.macro's
*
*---------------------------------------------------------------------*
* PUZZLE5A mainline                                                   *
*---------------------------------------------------------------------*
PUZZLE5A CEEENTRY AUTO=WORKDSA_SIZ,MAIN=YES,BASE=(R10,R11)
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
            B     PUZZLE5A_RET_IMMED * End program
         ENDIF
*
         L     R9,WA_PTR           * Put ptr to working storage in R9
         USING WORKAREA,R9         * Assign R9 to WORKAREA
*
         BAL   R8,INIT             * Perform initializations
*
         CLC   RETCODE,=A(0)       * All good so far?
         BNE   PUZZLE5A_RET        * If not, get out
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
PUZZLE5A_RET EQU   *               * Branch here for normal end
*
         BAL   R8,WRAPUP           * Perform wrap things up
*
         L     R2,RETCODE          * Put return code in R2
*
*                                  * Free memory for working stg
         CALL  CEEFRST,(WA_PTR,FC),VL,MF=(E,PAR7)
*
PUZZLE5A_RET_IMMED EQU   *         * Branch here when alloc mem failed
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
         MVC   STACK1+2(8),=C'BVSNTCHQ' * Initial stack values
         MVC   STACK2+2(4),=C'WDBG'     *    are simply
         MVC   STACK3+2(7),=C'FWRTSQB'  *       hardcoded here
         MVC   STACK4+2(8),=C'LGWSZJDN'
         MVC   STACK5+2(5),=C'MPDVF'
         MVC   STACK6+2(3),=C'FWJ'
         MVC   STACK7+2(6),=C'LNQBJV'
         MVC   STACK8+2(8),=C'GTRCJQSN'
         MVC   STACK9+2(7),=C'JSQCWDM'
*
         MVC   STACK1(2),=H'8'     * Initial stack lengths
         MVC   STACK2(2),=H'4'     *    are simply
         MVC   STACK3(2),=H'7'     *       hardcoded here
         MVC   STACK4(2),=H'8'
         MVC   STACK5(2),=H'5'
         MVC   STACK6(2),=H'3'
         MVC   STACK7(2),=H'6'
         MVC   STACK8(2),=H'8'
         MVC   STACK9(2),=H'7'
*
         MVC   PATHLEN,=A(27)
         MVC   PATHNAME(27),=C'/u/yin/ybtks/day5_input.txt'
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
         L     R7,INPRECLEN
*
         IF (C,R7,GE,4),AND,(CLC,INPREC(5),EQ,=C'move ') THEN
*
            LA    R6,INPREC+5      * Start at
            LR    R4,R6            * Save start addr in R4
            XR    R5,R5            * Prepare R5 for char
            LARL  R1,TRT_SKIP_NUMBERS * TR table for skipping nrs
            TRTE  R6,R5,0          * Look for first non-number
*
            LR    R3,R6            * Copy found address
            SR    R3,R4            * Calculate number length
            MVI   ZONED8,X'00'     * Initialize ZONED8
            MVC   ZONED8+1(7),ZONED8
            LA    R2,ZONED8+8      * Point past ZONED8
            SR    R2,R3            * Point to where number starts
            BCTR  R3,R0            * Prepare for EXecute
            B     *+10             * Skip move instruction
            MVC   0(1,R2),0(R4)    * Move instruction to EXecute
            EX    R3,*-6           * EXecute move instruction
            PACK  DEC8,ZONED8      * Convert zoned to decimal
            CVB   R5,DEC8          * Convert decimal to binary
            ST    R5,HOWMANY       * Save how many boxes to move
*
            XR    R5,R5            *   the end
            LARL  R1,TRT_SKIP_ANY_BUT_NUMBERS
            TRTE  R6,R5,0          * Skip non-numbers
*
            LR    R4,R6            * Save start addr in R4
            XR    R5,R5            * Prepare R5 for char
            LARL  R1,TRT_SKIP_NUMBERS * TR table for skipping nrs
            TRTE  R6,R5,0          * Look for first non-number
*
            LR    R3,R6            * Copy found address
            SR    R3,R4            * Calculate number length
            MVI   ZONED8,X'00'     * Initialize ZONED8
            MVC   ZONED8+1(7),ZONED8
            LA    R2,ZONED8+8      * Point past ZONED8
            SR    R2,R3            * Point to where number starts
            BCTR  R3,R0            * Prepare for EXecute
            B     *+10             * Skip move instruction
            MVC   0(1,R2),0(R4)    * Move instruction to EXecute
            EX    R3,*-6           * EXecute move instruction
            PACK  DEC8,ZONED8      * Convert zoned to decimal
            CVB   R5,DEC8          * Convert decimal to binary
            ST    R5,FROMSTACK     * Save source stack
*
            XR    R5,R5            *   the end
            LARL  R1,TRT_SKIP_ANY_BUT_NUMBERS
            TRTE  R6,R5,0          * Skip non-numbers
*
            LR    R4,R6            * Save start addr in R4
            XR    R5,R5            * Prepare R5 for char
            LARL  R1,TRT_SKIP_NUMBERS * TR table for skipping nrs
            TRTE  R6,R5,0          * Look for first non-number
*
            LR    R3,R6            * Copy found address
            SR    R3,R4            * Calculate number length
            MVI   ZONED8,X'00'     * Initialize ZONED8
            MVC   ZONED8+1(7),ZONED8
            LA    R2,ZONED8+8      * Point past ZONED8
            SR    R2,R3            * Point to where number starts
            BCTR  R3,R0            * Prepare for EXecute
            B     *+10             * Skip move instruction
            MVC   0(1,R2),0(R4)    * Move instruction to EXecute
            EX    R3,*-6           * EXecute move instruction
            PACK  DEC8,ZONED8      * Convert zoned to decimal
            CVB   R5,DEC8          * Convert decimal to binary
            ST    R5,TOSTACK       * Save target stack
*
            L     R15,FROMSTACK    * Multiply FROMSTACK - 1
            BCTR  R15,R0           *    by STACKSIZ
            XR    R14,R14          *       to get
            M     R14,=A(STACKSIZ) *          ptr to
            LA    R3,STACK1        *             the
            AR    R3,R15           *                'from' stack
            LR    R5,R3            * Save that ptr in R5 for later
            LH    R14,0(,R3)       * Get stack length
            LA    R3,1(R14,R3)     * Point R3 to last crate in stack
*
            L     R15,TOSTACK      * Multiply TOSTACK - 1
            BCTR  R15,R0           *    by STACKSIZ
            XR    R14,R14          *       to get
            M     R14,=A(STACKSIZ) *          ptr to
            LA    R2,STACK1        *             the
            AR    R2,R15           *                'to' stack
            LR    R4,R2            * Save that ptr in R4 for later
            LH    R14,0(,R2)       * Get stack length
            LA    R2,2(R14,R2)     * Point R2 past last crate in stack
*
            L     R1,HOWMANY       * Put move length in R1
            BCTR  R1,R0            * Prepare for EXecute
            B     *+10             * Skip past MVCIN instruction
            MVCIN 0(1,R2),0(R3)    * MVCIN instruction to EXecute
            EX    R1,*-6           * Move inverse string
*
            SR    R3,R1            * Point R3 to the first crate moved
            MVI   0(R3),X'00'      * Clear it out
            IF (C,R1,GT,=A(0)) THEN * If more than 1 crate was moved
               BCTR  R1,R0         * Prepare for another EXecute
               B     *+10          * Skip mast MVC instruction
               MVC   1(1,R3),0(R3) * MVC instruction to EXecute
               EX    R1,*-6        * Clear out from 2nd crate to last
            ENDIF
*
            LH    R14,0(,R5)       * Get 'from' stack length
            L     R15,HOWMANY      * Get how many crates were moved
            SR    R14,R15          * Subtract amount moved from length
            STH   R14,0(,R5)       * And put it back
*
            LH    R14,0(,R4)       * Get 'to' stack length
            AR    R14,R15          * Add amount moved to length
            STH   R14,0(,R4)       * And put it back
*
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
         LA    R2,TOPCRATES        * Point R2 to TOPCRATES
*
*        Loop 9 times, once for each stack
         DO FROM=(R15,9)
            LA    R3,STACK1        * Multiply current stack nr - 1
            LA    R7,9             *    by STACKSIZ
            SR    R7,R15           *       to get
            XR    R6,R6            *          ptr to
            M     R6,=A(STACKSIZ)  *             the
            AR    R3,R7            *                current stack
            LH    R4,0(,R3)        * Get stack length
            LA    R3,1(R4,R3)      * Point R3 to last crate in stack
            MVC   0(1,R2),0(R3)    * Put last crate in TOPCRATES
            LA    R2,1(,R2)        * Advance 1 position in TOPCRATES
         ENDDO
*
         MVI   TOPCRATES+9,X'00'   * Add zero terminator
*
         XR    R0,R0               * Check for zero terminator
         LA    R6,MSG+2            * Point to start of message
         LA    R7,RESULTMSG        * Text to copy (parm in R1)
         MVST  R6,R7               * Copy zero term string
         BC    1,*-4               * If interrupted, continue
*
         LA    R7,TOPCRATES        * Text to copy
         MVST  R6,R7               * Copy zero term string
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
RESULTMSG                    DC    C'TOP CRATES ARE ',X'00'
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
STACK1                       DS    CL(STACKSIZ) * Stack 1
STACK2                       DS    CL(STACKSIZ) * Stack 2
STACK3                       DS    CL(STACKSIZ) * Stack 3
STACK4                       DS    CL(STACKSIZ) * Stack 4
STACK5                       DS    CL(STACKSIZ) * Stack 5
STACK6                       DS    CL(STACKSIZ) * Stack 6
STACK7                       DS    CL(STACKSIZ) * Stack 7
STACK8                       DS    CL(STACKSIZ) * Stack 8
STACK9                       DS    CL(STACKSIZ) * Stack 9
*
HOWMANY                      DS    F     * How many boxes to move
FROMSTACK                    DS    F     * Source stack
TOSTACK                      DS    F     * Target stack
*
TOPCRATES                    DS    CL10
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
STACK                        DSECT
STACKHEIGHT                  DS    H
STACKTEXT                    DS    CL100
STACKSIZ                     EQU   *-STACK
*
         BPXYOPNF
         BPXYMODE
         BPXYFTYP
*
         CEECAA
*
         CEEDSA
*
PUZZLE5A CSECT
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
TRT_SKIP_NUMBERS             DS    0F
                             DC    (C'0')X'FF'
                             DC    (10)X'00'
                             DC    (255-C'9'-1)X'FF'
*
TRT_SKIP_ANY_BUT_NUMBERS     DS    0F
                             DC    (C'0')X'00'
                             DC    (10)X'FF'
                             DC    (255-C'9'-1)X'00'
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
         END   PUZZLE5A
