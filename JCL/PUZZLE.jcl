//PUZZLE6B JOB 'PUZZLE6B',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID
//*
//      EXPORT SYMLIST=(HLQ,SRC)
//         SET HLQ=YBTKS
//         SET SRC=PUZZLE6B
//*
//ASM     EXEC PGM=ASMA90,
//             PARM='ADATA,DBCS,LIST'
//SYSLIB    DD DISP=SHR,DSN=SYS1.MACLIB
//          DD DISP=SHR,DSN=SYS1.MODGEN
//          DD DISP=SHR,DSN=HLA.SASMMAC2
//          DD DISP=SHR,DSN=CEE.SCEEMAC
//          DD DISP=SHR,DSN=&HLQ..ADVENT.Y2022.ASM
//SYSIN     DD DISP=SHR,DSN=&HLQ..ADVENT.Y2022.ASM(&SRC)
//SYSLIN    DD DISP=SHR,DSN=&HLQ..OBJECT(&SRC)
//SYSADATA  DD DISP=SHR,DSN=&HLQ..SYSADATA(&SRC)
//SYSUT1    DD SPACE=(4096,(120,120),,,ROUND),UNIT=SYSDA
//SYSPRINT  DD DISP=SHR,DSN=&HLQ..ASM.LISTING(&SRC)
//*
//COPYASM EXEC PGM=IEBGENER
//SYSUT1    DD DISP=SHR,DSN=&HLQ..ASM.LISTING(&SRC)
//SYSIN     DD DUMMY
//SYSUT2    DD SYSOUT=*
//SYSPRINT  DD SYSOUT=*
//*
//XTRACT  EXEC PGM=EQALANGX,REGION=32M,COND=(4,LT),
//             PARM='(ASM ERROR'
//STEPLIB   DD DISP=SHR,DSN=ISM330.SIPVMODA
//SYSADATA  DD DISP=SHR,DSN=&HLQ..SYSADATA(&SRC)
//IDILANGX  DD DISP=OLD,DSN=&HLQ..EQALANGX
//SYSPRINT  DD SYSOUT=*
//*
//LKED    EXEC PGM=IEWL,COND=(0,NE),REGION=2M,
//             PARM='LIST,XREF,RENT,REUS'
//SYSLIB    DD DISP=SHR,DSN=CEE.SCEELKED
//          DD DISP=SHR,DSN=SYS1.CSSLIB
//          DD DISP=SHR,DSN=&HLQ..OBJECT
//SYSLIN    DD *,SYMBOLS=EXECSYS
 INCLUDE SYSLIB(&SRC)
 ENTRY &SRC
 NAME &SRC(R)
//SYSLMOD   DD DISP=SHR,DSN=&HLQ..LOAD
//SYSUT1    DD UNIT=SYSDA,DCB=BLKSIZE=1024,
//             SPACE=(1024,(200,20))
//SYSPRINT  DD SYSOUT=*
//*
//GO      EXEC PGM=EQANMDBG,PARMDD=PARAMS,COND=(4,LT),REGION=0M,
//             MEMLIMIT=1024M
//PARAMS    DD *,SYMBOLS=EXECSYS
&SRC,TEST(ALL,,,VTAM%YBTKS:*)/
/*
//STEPLIB   DD DISP=SHR,DSN=&HLQ..LOAD
//          DD DISP=SHR,DSN=CEE.SCEERUN
//          DD DISP=SHR,DSN=CEE.SCEERUN2
//          DD DISP=SHR,DSN=ISM330.SEQAMOD
//EQADEBUG  DD DISP=SHR,DSN=&HLQ..EQALANGX
//INSPLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=72,BLKSIZE=0)
//SYSOUT    DD SYSOUT=*
//STDOUT    DD SYSOUT=*
//STDERR    DD SYSOUT=*
//
//GO      EXEC PGM=&SRC,COND=(0,NE)
//STEPLIB   DD DISP=SHR,DSN=&HLQ..LOAD
//SYSOUT    DD SYSOUT=*