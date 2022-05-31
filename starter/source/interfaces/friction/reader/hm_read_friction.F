Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  HM_READ_FRICTION              source/interfaces/friction/reader/hm_read_friction.F
Chd|-- called by -----------
Chd|        HM_READ_FRICTION_MODELS       source/interfaces/friction/reader/hm_read_friction_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FRICTION(
     1        NIF     ,NOM_OPT   ,TITR       ,UNITAB    ,IGRPART       ,
     2        IPART   ,NSET      ,TAGPRT_FRIC,TABCOUPLEPARTS_FRIC_TMP  ,
     .                                                TABCOEF_FRIC_TMP ,
     3        MFROT   ,IFQ       ,XFILTR     ,FRICFORM       ,
     4        IFLAG   ,ORTHFRIC  ,IFRICORTH_TMP,NGRPF        ,
     4        LENGRPF  ,LENG     ,NOINTF      ,LSUBMODEL     )

C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER NIF ,IFLAG ,MFROT ,IFQ ,FRICFORM ,NSET ,ORTHFRIC , NGRPF,LENG,NOINTF
      INTEGER IPART(LIPART1,*) ,TAGPRT_FRIC(*),
     .   TABCOUPLEPARTS_FRIC_TMP(NINTERFRIC,*),IFRICORTH_TMP(NINTERFRIC,*),
     .   LENGRPF(*)
      my_real
     .   XFILTR
      my_real TABCOEF_FRIC_TMP(NINTERFRIC,*)
      CHARACTER TITR*nchartitle
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
      TYPE (SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr12_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,L ,IP ,IP1 ,IP2 ,N ,N1 ,N2 ,KK ,NL ,
     .    GRPART1 ,GRPART2 ,IPART1 ,IPART2 ,FLAGP1 ,FLAGP2,FLAGGRP1, 
     .    FLAGGRP2  ,IDTGRS1 ,IGRPART1 ,IDTGRS2 ,IGRPART2 ,NCOUPLE ,
     .    IPP ,IPP1 ,IPP2  ,IDIR ,NTAB ,LENF ,GRPN ,GRPN1 ,GRPN2 ,
     .    NP0 ,NGR0 ,K ,NGR ,J1 ,J2 ,STAT ,WORK(70000),NINPUT
       INTEGER, DIMENSION(:),   ALLOCATABLE :: 
     .    TRIGRPT ,INDEX ,NEWGRP ,TAGG1 ,TAGG2    
      my_real
     .   C1 ,C2 ,C3 ,C4 ,C5 ,C6 ,ALPHA ,C11 ,C22 ,C33 ,C44 ,C55 ,C66 ,
     .   FRIC  ,VISCF ,FRIC2  ,VISCF2 
      LOGICAL IS_AVAILABLE

C=======================================================================
C     READING FRICTION Model /FRICTION
C=======================================================================

       IS_AVAILABLE = .FALSE.

       ALLOCATE (TRIGRPT(LENG),STAT=stat) 
       ALLOCATE (INDEX(2*LENG),STAT=stat) 
       ALLOCATE (NEWGRP(LENG+1),STAT=stat) 
       ALLOCATE (TAGG1(LENG),STAT=stat) 
       ALLOCATE (TAGG2(LENG),STAT=stat) 

       CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NIF),LTITR) 
C
       NOM_OPT(1,NIF)=NOINTF
C
       NSET = 0
       NCOUPLE = 0
C
       IF(IFLAG==0 ) THEN 
          ORTHFRIC = 0
       ELSE
          IF(ORTHFRIC ==0) THEN
            LENF= 1
          ELSE
            LENF = 2
          ENDIF
       ENDIF

C--------------------------------------------------
C DEFAULT VALUES
C--------------------------------------------------

C EXTRACT DATAS (INTEGER VALUES)
       CALL HM_GET_INTV('ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
       CALL HM_GET_INTV('ifiltr',IFQ,IS_AVAILABLE,LSUBMODEL)
       CALL HM_GET_INTV('iform',FRICFORM,IS_AVAILABLE,LSUBMODEL)
C
C EXTRACT DATAS (REAL VALUES)
       CALL HM_GET_FLOATV('xfreq',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
       CALL HM_GET_FLOATV('c1',C1,IS_AVAILABLE,LSUBMODEL,UNITAB)
       CALL HM_GET_FLOATV('c2',C2,IS_AVAILABLE,LSUBMODEL,UNITAB)
       CALL HM_GET_FLOATV('c3',C3,IS_AVAILABLE,LSUBMODEL,UNITAB)
       CALL HM_GET_FLOATV('c4',C4,IS_AVAILABLE,LSUBMODEL,UNITAB)

       CALL HM_GET_FLOATV('c5',C5,IS_AVAILABLE,LSUBMODEL,UNITAB)
       CALL HM_GET_FLOATV('c6',C6,IS_AVAILABLE,LSUBMODEL,UNITAB)
       CALL HM_GET_FLOATV('fric',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
       CALL HM_GET_FLOATV('vis_f',VISCF,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C CHECKS
C
       IF (ALPHA.EQ.0.) IFQ = 0
       ALPHA = ALPHA 

       IF (FRICFORM.EQ.0) FRICFORM = 1
       IF (FRICFORM.EQ.2.AND.IFQ.LT.10) IFQ = IFQ + 10
C
       IF (IFQ.GT.0) THEN
           IF (IFQ.EQ.10) XFILTR = ONE
           IF (MOD(IFQ,10).EQ.1) XFILTR = ALPHA
           IF (MOD(IFQ,10).EQ.2) XFILTR=FOUR*ATAN2(ONE,ZERO) / ALPHA
           IF (MOD(IFQ,10).EQ.3) XFILTR=FOUR*ATAN2(ONE,ZERO) * ALPHA
           IF (XFILTR.LT.ZERO) THEN
              CALL ANCMSG(MSGID=1591,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NOINTF,
     .                    C1=TITR,
     .                    R1=ALPHA)
           ELSEIF (XFILTR.GT.1.AND.MOD(IFQ,10).LE.2) THEN
                CALL ANCMSG(MSGID=1591,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=NOINTF,
     .                      C1=TITR,
     .                      R1=ALPHA)
            ENDIF
       ELSE
           XFILTR = ZERO
       ENDIF

C
C STORAGE IN TEMPARAY INTBUF_FRIC_TAB
C
       IF(IFLAG == 1 ) THEN        

          IF((FRIC.NE.ZERO.OR.MFROT.NE.0).AND.VISCF.EQ.ZERO)VISCF=ONE

          IF (FRICFORM.EQ.2)VISCF=ZERO

          TABCOEF_FRIC_TMP(NIF,1) = FRIC
          TABCOEF_FRIC_TMP(NIF,2) = VISCF
          IF( MFROT > 0) THEN
             TABCOEF_FRIC_TMP(NIF,3) = C1
             TABCOEF_FRIC_TMP(NIF,4) = C2
             TABCOEF_FRIC_TMP(NIF,5) = C3
             TABCOEF_FRIC_TMP(NIF,6) = C4
             TABCOEF_FRIC_TMP(NIF,7) = C5
             TABCOEF_FRIC_TMP(NIF,8) = C6
         ENDIF
       ENDIF
C
C- OUTPUT DESCRIPTION OF THE MODEL 
C          1st defaults

       IF(IFLAG==1) THEN
          WRITE(IOUT,1500) NOINTF, TRIM(TITR)
          IF(FRICFORM ==2) THEN
            WRITE(IOUT,1508)
          ELSE
            WRITE(IOUT,1509) 
          ENDIF
          IF(MFROT.EQ.0)THEN
             WRITE(IOUT,1503)  
          ELSEIF(MFROT.EQ.1)THEN
            WRITE(IOUT,3505) 
          ELSEIF(MFROT.EQ.2)THEN
            WRITE(IOUT,3506) 
          ELSEIF(MFROT.EQ.3)THEN
            WRITE(IOUT,3507)  
          ENDIF
          WRITE(IOUT,1502)MOD(IFQ,10), XFILTR
          WRITE(IOUT,1501)

          IF(MFROT.EQ.0)THEN
             WRITE(IOUT,3503)  FRIC
             IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
          ELSEIF(MFROT.EQ.1)THEN
            WRITE(IOUT,1504)  FRIC,C1,C2,C3,C4,C5,C6
          ELSEIF(MFROT.EQ.2)THEN
            WRITE(IOUT,1505)  FRIC,C1,C2,C3,C4,C5,C6
          ELSEIF(MFROT.EQ.3)THEN
            WRITE(IOUT,1506)  C1,C2,C3,C4,C5,C6
          ENDIF
       ENDIF
C

C--------------------------------------------------
C FRICION COEFFICIENTS TABLE FOR CONNECTED PARTS
C--------------------------------------------------


C- OUTPUT DESCRIPTION OF THE MODEL
C          Coefficients table

       IF(IFLAG==1)  WRITE(IOUT,1507)

C EXTRACT DATAS (INTEGER VALUES) : Number of connected parts as defined by user 
       CALL HM_GET_INTV('N',NINPUT,IS_AVAILABLE,LSUBMODEL)

       DO NL=1,NINPUT

C EXTRACT DATAS (INTEGER VALUES) 

          CALL HM_GET_INT_ARRAY_INDEX('grpart_ID1',GRPART1,NL,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('grpart_ID2',GRPART2,NL,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('part_ID1',IPART1,NL,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('part_ID2',IPART2,NL,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('Idir',IDIR,NL,IS_AVAILABLE,LSUBMODEL)

C EXTRACT DATAS (REAL VALUES) 

          CALL HM_GET_FLOAT_ARRAY_INDEX('c1_part',C1,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('c2_part',C2,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('c3_part',C3,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('c4_part',C4,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('c5_part',C5,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('c6_part',C6,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('fric_part',FRIC,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('vis_f_part',VISCF,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)

          IF(IDIR ==1) THEN
            ORTHFRIC =1
            CALL HM_GET_FLOAT_ARRAY_INDEX('c1_2',C11,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('c2_2',C22,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('c3_2',C33,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('c4_2',C44,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('c5_2',C55,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('c6_2',C66,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('fric_2',FRIC2,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('vis_f_2',VISCF2,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)

          ENDIF

C
C----CHECKS PARTS 
C 
          FLAGP1 = 0
          FLAGP2 = 0  
          FLAGGRP1 = 0  
          FLAGGRP2 = 0  

          IF(IPART1/=0)THEN
            DO N=1,NPART
               IF(IPART1 == IPART(4,N))THEN
                  FLAGP1 = 1 
                  N1 = N
                  EXIT
               ENDIF
            ENDDO

            IF(FLAGP1 == 0)THEN
              CALL ANCMSG(MSGID=1590,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NOINTF,
     .                    C1=TITR,
     .                    I2=IPART1)
            ENDIF
          ENDIF
C
          IF(IPART2/=0)THEN
             DO N=1,NPART
                IF(IPART2 == IPART(4,N))THEN
                  FLAGP2 = 1 
                  N2 = N
                  EXIT
                 ENDIF
             ENDDO

             IF(FLAGP2 == 0)THEN
                 CALL ANCMSG(MSGID=1590,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=NOINTF,
     .                      C1=TITR,
     .                      I2=IPART2)
             ENDIF
          ENDIF
C
C----CHECK PARTS group
C
          IF(GRPART1/=0)THEN
            FLAGGRP1 = 0
            KK=NGRNOD+
     +          NGRBRIC+NGRQUAD+NGRSHEL+NGRSH3N+NGRTRUS+NGRBEAM+NGRSPRI
            DO N=1,NGRPART
              IF (IGRPART(N)%ID == GRPART1) THEN
                IDTGRS1=N
                FLAGGRP1 = 1
                EXIT
              END IF
            END DO
            IF(FLAGGRP1 ==  0) THEN
                CALL ANCMSG(MSGID=1590,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=NOINTF,
     .                      C1=TITR,
     .                      I2=IGRPART1)
            ENDIF
          ENDIF
C
          IF(GRPART2/=0)THEN
            FLAGGRP2 = 0
            KK=NGRNOD+
     +          NGRBRIC+NGRQUAD+NGRSHEL+NGRSH3N+NGRTRUS+NGRBEAM+NGRSPRI
            DO N=1,NGRPART
              IF (IGRPART(N)%ID == GRPART2) THEN
                IDTGRS2=N
                FLAGGRP2 = 1
                EXIT
              END IF
             END DO
             IF(FLAGGRP2 ==  0) THEN
                 CALL ANCMSG(MSGID=1590,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=NOINTF,
     .                      C1=TITR,
     .                      I2=IGRPART2)
             ENDIF
          ENDIF

C
C----CHECK coefficient values
C
          IF(IFLAG == 1 ) THEN
             IF((FRIC.NE.ZERO.OR.MFROT.NE.0).AND.VISCF.EQ.ZERO)VISCF=ONE

             IF (FRICFORM.EQ.2)VISCF=ZERO

             IF(IDIR > 0) THEN

                IF((FRIC2.NE.ZERO.OR.MFROT.NE.0).AND.VISCF2.EQ.ZERO)VISCF2=ONE

                IF (FRICFORM.EQ.2)VISCF2=ZERO

                IF((FRIC2.NE.ZERO.OR.MFROT.NE.0).AND.VISCF2.EQ.ZERO)VISCF2=ONE

                IF (FRICFORM.EQ.2)VISCF2=ZERO

              ENDIF
          ENDIF

C
C COUNTING AND STORAGE IN TEMPARAY INTBUF_FRIC_TAB
C
          IF(FLAGP1 /= 0.AND.FLAGP2 /= 0)THEN
C
             IF(IFLAG ==0 ) THEN
               IF(TAGPRT_FRIC(N1) ==0 ) THEN
                  NGRPF = NGRPF + 1
                  TAGPRT_FRIC(N1)=NGRPF ! tag parts
                  LENGRPF(NGRPF) = 1
               ELSE 
! If part is already read : look to group of parts belonging and it and split it to ensure group of parts are not lapped
                 GRPN = TAGPRT_FRIC(N1)
                 IF(LENGRPF(GRPN)/=1) THEN
                    NGRPF = NGRPF + 1
                    TAGPRT_FRIC(N1)=NGRPF ! tag parts
                    LENGRPF(NGRPF) = 1
                    LENGRPF(GRPN) =LENGRPF(GRPN) - 1
                 ENDIF
               ENDIF
               IF(TAGPRT_FRIC(N2) ==0 ) THEN
                  NGRPF = NGRPF + 1
                  TAGPRT_FRIC(N2)=NGRPF ! tag parts
                  LENGRPF(NGRPF) = 1
               ELSE 
! If part is already read : look to group of parts belonging and it and split it to ensure group of parts are not lapped
                  GRPN = TAGPRT_FRIC(N2)
                  IF(LENGRPF(GRPN)/=1) THEN
                    NGRPF = NGRPF + 1
                    TAGPRT_FRIC(N2)=NGRPF ! tag parts     
                    LENGRPF(NGRPF) = 1
                    LENGRPF(GRPN) =LENGRPF(GRPN) - 1
                  ENDIF
                ENDIF
             ENDIF
C
             IF(IFLAG == 1 ) THEN   
C
                GRPN1 = TAGPRT_FRIC(N1)
                GRPN2 = TAGPRT_FRIC(N2)

                IF(GRPN1 > GRPN2 ) THEN
                  N = GRPN1
                  GRPN1 = GRPN2
                  GRPN2 = N
                ENDIF
                NSET = NSET + 1
                NCOUPLE = NCOUPLE + 1   
                TABCOUPLEPARTS_FRIC_TMP(NIF,NCOUPLE) = GRPN1 
                NCOUPLE = NCOUPLE + 1   
                TABCOUPLEPARTS_FRIC_TMP(NIF,NCOUPLE) = GRPN2

                NTAB = LENF*8*(NSET-1)+8                                                         
                TABCOEF_FRIC_TMP(NIF,NTAB+1) = FRIC
                TABCOEF_FRIC_TMP(NIF,NTAB+2) = VISCF
                IF(MFROT > 0) THEN
                  TABCOEF_FRIC_TMP(NIF,NTAB+3) = C1
                  TABCOEF_FRIC_TMP(NIF,NTAB+4) = C2
                  TABCOEF_FRIC_TMP(NIF,NTAB+5) = C3
                  TABCOEF_FRIC_TMP(NIF,NTAB+6) = C4
                  TABCOEF_FRIC_TMP(NIF,NTAB+7) = C5
                  TABCOEF_FRIC_TMP(NIF,NTAB+8) = C6 
                ENDIF
                IFRICORTH_TMP(NIF,NSET) = IDIR
                IF(IDIR > 0) THEN
                  NTAB = 16*NSET
                  TABCOEF_FRIC_TMP(NIF,NTAB+1) = FRIC2
                  TABCOEF_FRIC_TMP(NIF,NTAB+2) = VISCF2
                  IF(MFROT > 0) THEN
                    TABCOEF_FRIC_TMP(NIF,NTAB+3) = C11
                    TABCOEF_FRIC_TMP(NIF,NTAB+4) = C22
                    TABCOEF_FRIC_TMP(NIF,NTAB+5) = C33
                    TABCOEF_FRIC_TMP(NIF,NTAB+6) = C44
                    TABCOEF_FRIC_TMP(NIF,NTAB+7) = C55
                    TABCOEF_FRIC_TMP(NIF,NTAB+8) = C66 
                  ENDIF
                ENDIF

C--Output--
C
                WRITE (IOUT,2001) IPART(4,N1),IPART(4,N2)
                IF(IDIR==0) THEN
                  WRITE(IOUT,1510)
                  IF(MFROT.EQ.0)THEN
                    WRITE(IOUT,3503) FRIC
                    IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
                  ELSEIF(MFROT.EQ.1)THEN
                    WRITE(IOUT,1504) FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.2)THEN
                    WRITE(IOUT,1505)FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.3)THEN
                    WRITE(IOUT,1506)C1,C2,C3,C4,C5,C6
                  ENDIF
                ELSE
                  WRITE(IOUT,1511)
                  WRITE(IOUT,1512)
                  IF(MFROT.EQ.0)THEN
                    WRITE(IOUT,3503) FRIC
                    IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
                  ELSEIF(MFROT.EQ.1)THEN
                    WRITE(IOUT,1504) FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.2)THEN
                    WRITE(IOUT,1505)FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.3)THEN
                    WRITE(IOUT,1506)C1,C2,C3,C4,C5,C6
                  ENDIF
                  WRITE(IOUT,1513)
                  IF(MFROT.EQ.0)THEN
                    WRITE(IOUT,3503) FRIC2
                    IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF2
                  ELSEIF(MFROT.EQ.1)THEN
                    WRITE(IOUT,1504) FRIC2,C11,C22,C33,C44,C55,C66
                  ELSEIF(MFROT.EQ.2)THEN
                    WRITE(IOUT,1505)FRIC2,C11,C22,C33,C44,C55,C66
                  ELSEIF(MFROT.EQ.3)THEN
                    WRITE(IOUT,1506)C11,C22,C33,C44,C55,C66
                  ENDIF
                ENDIF
C
             ENDIF
C
          ENDIF 
C
          IF(FLAGP1 /= 0.AND.FLAGGRP2 /= 0)THEN
C
            IF(IFLAG ==0 ) THEN
              IF(TAGPRT_FRIC(N1) ==0 ) THEN
                 NGRPF = NGRPF + 1
                 TAGPRT_FRIC(N1)=NGRPF ! tag parts
                 LENGRPF(NGRPF) = 1
              ELSE
                 GRPN = TAGPRT_FRIC(N1)
                 IF(LENGRPF(GRPN)/=1) THEN
                   NGRPF = NGRPF + 1
                   LENGRPF(NGRPF) = 1
                   TAGPRT_FRIC(N1)=NGRPF ! tag parts
                   LENGRPF(GRPN) =LENGRPF(GRPN) - 1
                 ENDIF
              ENDIF            

              NP0 = 0
              DO I=1,IGRPART(IDTGRS2)%NENTITY
                 IP=IGRPART(IDTGRS2)%ENTITY(I)
                 IF(TAGPRT_FRIC(IP) ==0 ) THEN
                   NP0 = NP0 +1
                 ENDIF
              ENDDO
              IF(NP0 == IGRPART(IDTGRS2)%NENTITY) THEN
                 NGRPF = NGRPF + 1
                 LENGRPF(NGRPF) = NP0
                 DO I=1,IGRPART(IDTGRS2)%NENTITY
                   IP=IGRPART(IDTGRS2)%ENTITY(I)
                   TAGPRT_FRIC(IP)=NGRPF ! tag parts
                 ENDDO

                 J2 = 1
                 TAGG2(1) = NGRPF             
              ELSE
                 IF(NP0 >0 ) THEN
                    NGRPF = NGRPF + 1
                    LENGRPF(NGRPF) = NP0
                    DO I=1,IGRPART(IDTGRS2)%NENTITY
                       IP=IGRPART(IDTGRS2)%ENTITY(I)
                       IF(TAGPRT_FRIC(IP) ==0 ) TAGPRT_FRIC(IP) = NGRPF 
                    ENDDO
                 ENDIF
                 INDEX(1:2*LENG) = 0
                 TRIGRPT(1:LENG) = 0
                 DO I=1,IGRPART(IDTGRS2)%NENTITY
                    IP=IGRPART(IDTGRS2)%ENTITY(I)
                    IF(TAGPRT_FRIC(IP)==0) TAGPRT_FRIC(IP)=NGRPF ! tag parts
                    TRIGRPT(I) = TAGPRT_FRIC(IP) 
                    INDEX(I) = I
                 ENDDO
                 CALL MY_ORDERS( 0, WORK, TRIGRPT, INDEX, IGRPART(IDTGRS2)%NENTITY , 1)

                 NGR0 = TRIGRPT(INDEX(1))
                 J= 1
                 NEWGRP(1:LENG+1) = 0 
                 TAGG2(1:LENG) = 0

                 DO I=2,IGRPART(IDTGRS2)%NENTITY
                   NGR = TRIGRPT(INDEX(I))  
                   IF(NGR/=NGR0) THEN
                     IF(LENGRPF(NGR0) /= I-NEWGRP(J)-1) THEN
                        TAGG2(J) =1                   
                     ENDIF
                     J = J +1
                     NGR0  =  NGR  
                     NEWGRP( J) = I-1
                   ENDIF                                   
                 ENDDO   
                 IF(LENGRPF(NGR0) /= IGRPART(IDTGRS2)%NENTITY-NEWGRP(J)) THEN
                   TAGG2(J) =1                   
                 ENDIF
                 NEWGRP( J+1) = I-1
                 DO K=1,J
                    IF(TAGG2(K)==1) THEN
                      NGRPF = NGRPF + 1
                      LENGRPF(NGRPF) = NEWGRP( K+1) - NEWGRP( K)
                      IP=IGRPART(IDTGRS2)%ENTITY(INDEX(NEWGRP( K)+1))
                      NGR0 = TAGPRT_FRIC(IP) 
                      LENGRPF(NGR0) =LENGRPF(NGR0) -LENGRPF(NGRPF)
                      DO I =NEWGRP( K)+1,NEWGRP( K+1)      
                        IP=IGRPART(IDTGRS2)%ENTITY(INDEX(I))
                        TAGPRT_FRIC(IP) =NGRPF 
                      ENDDO 
                    ENDIF
                 ENDDO   

              ENDIF
            ENDIF


            IF(IFLAG == 1 ) THEN             
C
               GRPN1 = TAGPRT_FRIC(N1)
C
! If part or group of parts is already read : look to group of parts belonging and it and split it to ensure group of parts are not lapped
               INDEX(1:2*LENG) = 0
               TRIGRPT(1:LENG) = 0
               TAGG2(1:LENG) = 0
               DO I=1,IGRPART(IDTGRS2)%NENTITY
                  IP=IGRPART(IDTGRS2)%ENTITY(I)
                  TRIGRPT(I) = TAGPRT_FRIC(IP) 
                  INDEX(I) = I
               ENDDO
               CALL MY_ORDERS( 0, WORK, TRIGRPT, INDEX, IGRPART(IDTGRS2)%NENTITY , 1)

               NGR0 = TRIGRPT(INDEX(1))
               J= 1
               TAGG2(1) = NGR0
               DO I=2,IGRPART(IDTGRS2)%NENTITY
                  NGR = TRIGRPT(INDEX(I)) 
                  IF(NGR/=NGR0) THEN
                     J = J +1
                     NGR0  =  NGR 
                     TAGG2(J) = NGR0 
                   ENDIF                                   
               ENDDO 
               J2 = J     
C
               DO K=1,J2
                  GRPN2 = TAGG2(K)

                  IF(N1 > GRPN2 ) THEN
                     N = GRPN1
                     IPP = GRPN2
                     IP = GRPN1
                  ELSE
                     IPP = GRPN1
                  ENDIF

                      
                  NCOUPLE = NCOUPLE + 1          
                  TABCOUPLEPARTS_FRIC_TMP(NIF,NCOUPLE) = IPP  
                  NCOUPLE = NCOUPLE + 1          
                  TABCOUPLEPARTS_FRIC_TMP(NIF,NCOUPLE) = IP
C
                  NTAB = LENF*8*(NSET-1)+8
                  TABCOEF_FRIC_TMP(NIF,NTAB+1) = FRIC
                  TABCOEF_FRIC_TMP(NIF,NTAB+2) = VISCF
                  IF(MFROT > 0) THEN
                     TABCOEF_FRIC_TMP(NIF,NTAB+3) = C1
                     TABCOEF_FRIC_TMP(NIF,NTAB+4) = C2
                     TABCOEF_FRIC_TMP(NIF,NTAB+5) = C3
                     TABCOEF_FRIC_TMP(NIF,NTAB+6) = C4
                     TABCOEF_FRIC_TMP(NIF,NTAB+7) = C5
                     TABCOEF_FRIC_TMP(NIF,NTAB+8) = C6
                  ENDIF
                  IFRICORTH_TMP(NIF,NSET) = IDIR

c
                  IF(IDIR==1) THEN

                     NTAB = 16*NSET
                     TABCOEF_FRIC_TMP(NIF,NTAB+1) = FRIC2
                     TABCOEF_FRIC_TMP(NIF,NTAB+2) = VISCF2
                     IF(MFROT > 0) THEN
                        TABCOEF_FRIC_TMP(NIF,NTAB+3) = C11
                        TABCOEF_FRIC_TMP(NIF,NTAB+4) = C22
                        TABCOEF_FRIC_TMP(NIF,NTAB+5) = C33
                        TABCOEF_FRIC_TMP(NIF,NTAB+6) = C44
                        TABCOEF_FRIC_TMP(NIF,NTAB+7) = C55
                        TABCOEF_FRIC_TMP(NIF,NTAB+8) = C66 
                     ENDIF

                  ENDIF
               ENDDO
             ENDIF

c

C--Output--
             IF(IFLAG == 1 ) THEN             
                WRITE (IOUT,2003)
     .             IPART(4,N1),GRPART2
                IF(IDIR==0) THEN
                   WRITE(IOUT,1510)
                   IF(MFROT.EQ.0)THEN
                     WRITE(IOUT,3503) FRIC
                     IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
                   ELSEIF(MFROT.EQ.1)THEN
                      WRITE(IOUT,1504) FRIC,C1,C2,C3,C4,C5,C6
                   ELSEIF(MFROT.EQ.2)THEN
                      WRITE(IOUT,1505)FRIC,C1,C2,C3,C4,C5,C6
                   ELSEIF(MFROT.EQ.3)THEN
                      WRITE(IOUT,1506)C1,C2,C3,C4,C5,C6
                   ENDIF
                ELSE
                   WRITE(IOUT,1511)
                   WRITE(IOUT,1512)
                   IF(MFROT.EQ.0)THEN
                      WRITE(IOUT,3503) FRIC
                      IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
                   ELSEIF(MFROT.EQ.1)THEN
                      WRITE(IOUT,1504) FRIC,C1,C2,C3,C4,C5,C6
                   ELSEIF(MFROT.EQ.2)THEN
                      WRITE(IOUT,1505)FRIC,C1,C2,C3,C4,C5,C6
                   ELSEIF(MFROT.EQ.3)THEN
                      WRITE(IOUT,1506)C1,C2,C3,C4,C5,C6
                   ENDIF
                   WRITE(IOUT,1513)
                   IF(MFROT.EQ.0)THEN
                      WRITE(IOUT,3503) FRIC2
                      IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF2
                   ELSEIF(MFROT.EQ.1)THEN
                      WRITE(IOUT,1504) FRIC2,C11,C22,C33,C44,C55,C66
                   ELSEIF(MFROT.EQ.2)THEN
                      WRITE(IOUT,1505)FRIC2,C11,C22,C33,C44,C55,C66
                   ELSEIF(MFROT.EQ.3)THEN
                      WRITE(IOUT,1506)C11,C22,C33,C44,C55,C66
                   ENDIF
                ENDIF
             ENDIF

          ENDIF
C
          IF(FLAGP2 /= 0.AND.FLAGGRP1 /= 0)THEN

            IF(IFLAG==0) THEN
               IF(TAGPRT_FRIC(N2) ==0 ) THEN
                  NGRPF = NGRPF + 1
                  TAGPRT_FRIC(N2)=NGRPF ! tag parts
                  LENGRPF(NGRPF) = 1
               ELSE
                  GRPN = TAGPRT_FRIC(N2)
                  IF(LENGRPF(GRPN)/=1) THEN
                     NGRPF = NGRPF + 1
                     LENGRPF(NGRPF) = 1
                     TAGPRT_FRIC(N2)=NGRPF ! tag parts
                     LENGRPF(GRPN) =LENGRPF(GRPN) - 1
                  ENDIF
               ENDIF

               NP0 = 0
               DO I=1,IGRPART(IDTGRS1)%NENTITY
                 IP=IGRPART(IDTGRS1)%ENTITY(I)
                IF(TAGPRT_FRIC(IP) ==0 ) THEN
                    NP0 = NP0 +1
                 ENDIF
               ENDDO



               IF(NP0 == IGRPART(IDTGRS1)%NENTITY) THEN
                 NGRPF = NGRPF + 1
                 LENGRPF(NGRPF) = NP0
                 DO I=1,IGRPART(IDTGRS1)%NENTITY
                    IP=IGRPART(IDTGRS1)%ENTITY(I)
                    TAGPRT_FRIC(IP)=NGRPF ! tag parts
                 ENDDO    

               ELSE
                 IF(NP0 >0 ) THEN
                    NGRPF = NGRPF + 1
                    LENGRPF(NGRPF) = NP0
                    DO I=1,IGRPART(IDTGRS1)%NENTITY
                       IP=IGRPART(IDTGRS1)%ENTITY(I)
                       IF(TAGPRT_FRIC(IP) ==0 ) TAGPRT_FRIC(IP) = NGRPF 
                    ENDDO
                 ENDIF
                 INDEX(1:2*LENG) = 0
                 TRIGRPT(1:LENG) = 0
                 DO I=1,IGRPART(IDTGRS1)%NENTITY
                    IP=IGRPART(IDTGRS1)%ENTITY(I)
                    IF(TAGPRT_FRIC(IP)==0) TAGPRT_FRIC(IP)=NGRPF ! tag parts
                    TRIGRPT(I) = TAGPRT_FRIC(IP) 
                    INDEX(I) = I
                 ENDDO
                 CALL MY_ORDERS( 0, WORK, TRIGRPT, INDEX, IGRPART(IDTGRS1)%NENTITY , 1)

                 NGR0 = TRIGRPT(INDEX(1))
                 J= 1
                 NEWGRP(1:LENG+1) = 0 
                 TAGG1(1:LENG) = 0
                 DO I=2,IGRPART(IDTGRS1)%NENTITY
                    NGR = TRIGRPT(INDEX(I))  
                    IF(NGR/=NGR0) THEN
                      IF(LENGRPF(NGR0) /= I-NEWGRP(J)-1) THEN
                         TAGG1(J) =1                   
                      ENDIF
                      J = J +1
                      NGR0  =  NGR  
                      NEWGRP( J) = I-1
                    ENDIF                                   
                 ENDDO   

                 IF(LENGRPF(NGR0) /= IGRPART(IDTGRS1)%NENTITY-NEWGRP(J)) THEN
                    TAGG1(J) =1                   
                 ENDIF
                 NEWGRP( J+1) = I-1


                 DO K=1,J
                    IF(TAGG1(K)==1) THEN
                       NGRPF = NGRPF + 1
                       LENGRPF(NGRPF) = NEWGRP( K+1) - NEWGRP( K)
                       IP=IGRPART(IDTGRS1)%ENTITY(INDEX(NEWGRP( K)+1))
                       NGR0 = TAGPRT_FRIC(IP) 
                       LENGRPF(NGR0) =LENGRPF(NGR0) -LENGRPF(NGRPF)
                       DO I =NEWGRP( K)+1,NEWGRP( K+1)      
                         IP=IGRPART(IDTGRS1)%ENTITY(INDEX(I))
                         TAGPRT_FRIC(IP) =NGRPF 
                       ENDDO
                    ENDIF
                 ENDDO 
               ENDIF
            ENDIF 
C
            IF(IFLAG == 1 ) THEN             
C
              GRPN2 = TAGPRT_FRIC(N2)
C
              INDEX(1:2*LENG) = 0
              TRIGRPT(1:LENG) = 0
              TAGG1(1:LENG) = 0
              DO I=1,IGRPART(IDTGRS1)%NENTITY
                 IP=IGRPART(IDTGRS1)%ENTITY(I)
                 TRIGRPT(I) = TAGPRT_FRIC(IP) 
                 INDEX(I) = I
              ENDDO
              CALL MY_ORDERS( 0, WORK, TRIGRPT, INDEX, IGRPART(IDTGRS1)%NENTITY , 1)

              NGR0 = TRIGRPT(INDEX(1))
              J= 1
              TAGG1(1) = NGR0
              DO I=2,IGRPART(IDTGRS1)%NENTITY
                 NGR = TRIGRPT(INDEX(I))  
                 IF(NGR/=NGR0) THEN
                    J = J +1
                    NGR0  =  NGR 
                    TAGG1(J) = NGR0 
                 ENDIF                                   
              ENDDO   
              J1 = J  
C
              DO K=1,J1
                 GRPN1 = TAGG1(K)
                 IF(GRPN1 > N2 ) THEN
                    N = N2
                    IPP = GRPN1
                    IP = N2
                 ELSE
                    IPP = GRPN1
                 ENDIF


                 NCOUPLE = NCOUPLE + 1          
                 TABCOUPLEPARTS_FRIC_TMP(NIF,NCOUPLE) = IPP  
                 NCOUPLE = NCOUPLE + 1          
                 TABCOUPLEPARTS_FRIC_TMP(NIF,NCOUPLE) = IP
C
                 NTAB = LENF*8*(NSET-1)+8
                 TABCOEF_FRIC_TMP(NIF,NTAB+1) = FRIC
                 TABCOEF_FRIC_TMP(NIF,NTAB+2) = VISCF
                 IF(MFROT > 0 ) THEN
                   TABCOEF_FRIC_TMP(NIF,NTAB+3) = C1
                   TABCOEF_FRIC_TMP(NIF,NTAB+4) = C2
                   TABCOEF_FRIC_TMP(NIF,NTAB+5) = C3
                   TABCOEF_FRIC_TMP(NIF,NTAB+6) = C4
                   TABCOEF_FRIC_TMP(NIF,NTAB+7) = C5
                   TABCOEF_FRIC_TMP(NIF,NTAB+8) = C6
                 ENDIF
                 IFRICORTH_TMP(NIF,NSET) = IDIR
c
                 IF(IDIR==1) THEN

                    NTAB = 2*8*NSET
                    TABCOEF_FRIC_TMP(NIF,NTAB+1) = FRIC2
                    TABCOEF_FRIC_TMP(NIF,NTAB+2) = VISCF2
                    TABCOEF_FRIC_TMP(NIF,NTAB+3) = C11
                    TABCOEF_FRIC_TMP(NIF,NTAB+4) = C22
                    TABCOEF_FRIC_TMP(NIF,NTAB+5) = C33
                    TABCOEF_FRIC_TMP(NIF,NTAB+6) = C44
                    TABCOEF_FRIC_TMP(NIF,NTAB+7) = C55
                    TABCOEF_FRIC_TMP(NIF,NTAB+8) = C66 

                 ENDIF
c
              ENDDO

            ENDIF

C--Output--
            IF(IFLAG == 1 ) THEN             
               WRITE (IOUT,2002)
     .             GRPART1,IPART(4,N2)
               IF(IDIR==0) THEN
                  WRITE(IOUT,1510)
                  IF(MFROT.EQ.0)THEN
                     WRITE(IOUT,3503) FRIC
                     IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
                  ELSEIF(MFROT.EQ.1)THEN
                     WRITE(IOUT,1504) FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.2)THEN
                     WRITE(IOUT,1505)FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.3)THEN
                     WRITE(IOUT,1506)C1,C2,C3,C4,C5,C6
                  ENDIF
               ELSE
                  WRITE(IOUT,1511)
                  WRITE(IOUT,1512)
                  IF(MFROT.EQ.0)THEN
                     WRITE(IOUT,3503) FRIC
                     IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
                  ELSEIF(MFROT.EQ.1)THEN
                      WRITE(IOUT,1504) FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.2)THEN
                     WRITE(IOUT,1505)FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.3)THEN
                     WRITE(IOUT,1506)C1,C2,C3,C4,C5,C6
                  ENDIF
                  WRITE(IOUT,1513)
                  IF(MFROT.EQ.0)THEN
                     WRITE(IOUT,3503) FRIC2
                     IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF2
                  ELSEIF(MFROT.EQ.1)THEN
                     WRITE(IOUT,1504) FRIC2,C11,C22,C33,C44,C55,C66
                  ELSEIF(MFROT.EQ.2)THEN
                     WRITE(IOUT,1505)FRIC2,C11,C22,C33,C44,C55,C66
                  ELSEIF(MFROT.EQ.3)THEN
                     WRITE(IOUT,1506)C11,C22,C33,C44,C55,C66
                  ENDIF
               ENDIF
            ENDIF

          ENDIF
C
          IF(FLAGGRP1 /= 0.AND.FLAGGRP2 /=0)THEN

            IF(IFLAG==0) THEN
              NP0 = 0
              DO I=1,IGRPART(IDTGRS1)%NENTITY
                IP=IGRPART(IDTGRS1)%ENTITY(I)
                IF(TAGPRT_FRIC(IP) ==0 ) THEN
                   NP0 = NP0 +1
                ENDIF
              ENDDO



              IF(NP0 == IGRPART(IDTGRS1)%NENTITY) THEN
                 NGRPF = NGRPF + 1
                 LENGRPF(NGRPF) = NP0
                 DO I=1,IGRPART(IDTGRS1)%NENTITY
                    IP=IGRPART(IDTGRS1)%ENTITY(I)
                    TAGPRT_FRIC(IP)=NGRPF ! tag parts
                 ENDDO    

              ELSE
                 IF(NP0 >0 ) THEN
                    NGRPF = NGRPF + 1
                    LENGRPF(NGRPF) = NP0
                    DO I=1,IGRPART(IDTGRS1)%NENTITY
                       IP=IGRPART(IDTGRS1)%ENTITY(I)
                       IF(TAGPRT_FRIC(IP) ==0 ) TAGPRT_FRIC(IP) = NGRPF 
                    ENDDO
                 ENDIF
                 INDEX(1:2*LENG) = 0
                 TRIGRPT(1:LENG) = 0
                 DO I=1,IGRPART(IDTGRS1)%NENTITY
                    IP=IGRPART(IDTGRS1)%ENTITY(I)
                    IF(TAGPRT_FRIC(IP)==0) TAGPRT_FRIC(IP)=NGRPF ! tag parts
                    TRIGRPT(I) = TAGPRT_FRIC(IP) 
                    INDEX(I) = I
                 ENDDO
                 CALL MY_ORDERS( 0, WORK, TRIGRPT, INDEX, IGRPART(IDTGRS1)%NENTITY , 1)

                 NGR0 = TRIGRPT(INDEX(1))
                 J= 1
                 NEWGRP(1:LENG+1) = 0 
                 TAGG1(1:LENG) = 0
                 DO I=2,IGRPART(IDTGRS1)%NENTITY
                     NGR = TRIGRPT(INDEX(I))  
                    IF(NGR/=NGR0) THEN
                      IF(LENGRPF(NGR0) /= I-NEWGRP(J)-1) THEN
                         TAGG1(J) =1                   
                      ENDIF
                      J = J +1
                      NGR0  =  NGR  
                      NEWGRP( J) = I-1
                    ENDIF                                   
                 ENDDO   

                 IF(LENGRPF(NGR0) /= IGRPART(IDTGRS1)%NENTITY-NEWGRP(J)) THEN
                    TAGG1(J) =1                   
                 ENDIF
                 NEWGRP( J+1) = I-1


                 DO K=1,J
                    IF(TAGG1(K)==1) THEN
                       NGRPF = NGRPF + 1
                       LENGRPF(NGRPF) = NEWGRP( K+1) - NEWGRP( K)
                       IP=IGRPART(IDTGRS1)%ENTITY(INDEX(NEWGRP( K)+1))
                       NGR0 = TAGPRT_FRIC(IP) 
                       LENGRPF(NGR0) =LENGRPF(NGR0) -LENGRPF(NGRPF)
                       DO I =NEWGRP( K)+1,NEWGRP( K+1)      
                         IP=IGRPART(IDTGRS1)%ENTITY(INDEX(I))
                         TAGPRT_FRIC(IP) =NGRPF 
                      ENDDO
                    ENDIF
                 ENDDO 
              ENDIF

              NP0 = 0
              DO I=1,IGRPART(IDTGRS2)%NENTITY
                IP=IGRPART(IDTGRS2)%ENTITY(I)
                IF(TAGPRT_FRIC(IP) ==0 ) THEN
                    NP0 = NP0 +1
                 ENDIF
              ENDDO
              IF(NP0 == IGRPART(IDTGRS2)%NENTITY) THEN
                 NGRPF = NGRPF + 1
                 LENGRPF(NGRPF) = NP0
                 DO I=1,IGRPART(IDTGRS2)%NENTITY
                    IP=IGRPART(IDTGRS2)%ENTITY(I)
                    TAGPRT_FRIC(IP)=NGRPF ! tag parts
                 ENDDO


                 J2 = 1
                 TAGG2(1) = NGRPF             
              ELSE
                 IF(NP0 >0 ) THEN
                    NGRPF = NGRPF + 1
                    LENGRPF(NGRPF) = NP0
                    DO I=1,IGRPART(IDTGRS2)%NENTITY
                       IP=IGRPART(IDTGRS2)%ENTITY(I)
                       IF(TAGPRT_FRIC(IP) ==0 ) TAGPRT_FRIC(IP) = NGRPF 
                    ENDDO
                 ENDIF
                 INDEX(1:2*LENG) = 0
                 TRIGRPT(1:LENG) = 0
                 DO I=1,IGRPART(IDTGRS2)%NENTITY
                    IP=IGRPART(IDTGRS2)%ENTITY(I)
                    IF(TAGPRT_FRIC(IP)==0) TAGPRT_FRIC(IP)=NGRPF ! tag parts
                    TRIGRPT(I) = TAGPRT_FRIC(IP) 
                    INDEX(I) = I
                 ENDDO
                 CALL MY_ORDERS( 0, WORK, TRIGRPT, INDEX, IGRPART(IDTGRS2)%NENTITY , 1)

                 NGR0 = TRIGRPT(INDEX(1))
                 J= 1
                 NEWGRP(1:LENG+1) = 0 
                 TAGG2(1:LENG) = 0

                 DO I=2,IGRPART(IDTGRS2)%NENTITY
                    NGR = TRIGRPT(INDEX(I))  
                    IF(NGR/=NGR0) THEN
                      IF(LENGRPF(NGR0) /= I-NEWGRP(J)-1) THEN
                         TAGG2(J) =1                   
                      ENDIF
                      J = J +1
                      NGR0  =  NGR  
                      NEWGRP( J) = I-1
                    ENDIF                                   
                 ENDDO   
                 IF(LENGRPF(NGR0) /= IGRPART(IDTGRS2)%NENTITY-NEWGRP(J)) THEN
                    TAGG2(J) =1                   
                 ENDIF
                 NEWGRP( J+1) = I-1
                 DO K=1,J
                    IF(TAGG2(K)==1) THEN
                       NGRPF = NGRPF + 1
                       LENGRPF(NGRPF) = NEWGRP( K+1) - NEWGRP( K)
                       IP=IGRPART(IDTGRS2)%ENTITY(INDEX(NEWGRP( K)+1))
                       NGR0 = TAGPRT_FRIC(IP) 
                       LENGRPF(NGR0) =LENGRPF(NGR0) -LENGRPF(NGRPF)
                       DO I =NEWGRP( K)+1,NEWGRP( K+1)      
                         IP=IGRPART(IDTGRS2)%ENTITY(INDEX(I))
                         TAGPRT_FRIC(IP) =NGRPF 
                       ENDDO 
                    ENDIF
                 ENDDO   

              ENDIF
            ENDIF

            IF(IFLAG == 1 ) THEN

C
               INDEX(1:2*LENG) = 0
               TRIGRPT(1:LENG) = 0
               TAGG1(1:LENG) = 0
               DO I=1,IGRPART(IDTGRS1)%NENTITY
                  IP=IGRPART(IDTGRS1)%ENTITY(I)
                  TRIGRPT(I) = TAGPRT_FRIC(IP) 
                  INDEX(I) = I
               ENDDO
               CALL MY_ORDERS( 0, WORK, TRIGRPT, INDEX, IGRPART(IDTGRS1)%NENTITY , 1)

               NGR0 = TRIGRPT(INDEX(1))
               J= 1
               TAGG1(1) = NGR0
               DO I=2,IGRPART(IDTGRS1)%NENTITY
                  NGR = TRIGRPT(INDEX(I))  
                  IF(NGR/=NGR0) THEN
                     J = J +1
                     NGR0  =  NGR 
                     TAGG1(J) = NGR0 
                  ENDIF                                   
               ENDDO   
               J1 = J
C
               INDEX(1:2*LENG) = 0
               TRIGRPT(1:LENG) = 0
               TAGG2(1:LENG) = 0
               DO I=1,IGRPART(IDTGRS2)%NENTITY
                  IP=IGRPART(IDTGRS2)%ENTITY(I)
                  TRIGRPT(I) = TAGPRT_FRIC(IP) 
                  INDEX(I) = I
               ENDDO
               CALL MY_ORDERS( 0, WORK, TRIGRPT, INDEX, IGRPART(IDTGRS2)%NENTITY , 1)

               NGR0 = TRIGRPT(INDEX(1))
               J= 1
               TAGG2(1) = NGR0
               DO I=2,IGRPART(IDTGRS2)%NENTITY
                  NGR = TRIGRPT(INDEX(I)) 
                  IF(NGR/=NGR0) THEN
                     J = J +1
                     NGR0  =  NGR 
                     TAGG2(J) = NGR0 
                  ENDIF                                   
               ENDDO 
               J2 = J      
C
               DO K=1,J1
                  GRPN1 = TAGG1(K)
                  DO J=1,J2
                     GRPN2 = TAGG2(J)
                    IF(GRPN1 > GRPN2 ) THEN
                       N = GRPN2
                       IPP2 = GRPN1
                       IPP1 = N
                    ELSE
                       IPP1 = GRPN1
                       IPP2 = GRPN2                        
                    ENDIF
                    NSET = NSET + 1

                    NCOUPLE = NCOUPLE + 1          
                    TABCOUPLEPARTS_FRIC_TMP(NIF,NCOUPLE) = IPP1  
                    NCOUPLE = NCOUPLE + 1          
                    TABCOUPLEPARTS_FRIC_TMP(NIF,NCOUPLE) = IPP2
C
                    NTAB = LENF*8*(NSET-1)+8
                    TABCOEF_FRIC_TMP(NIF,NTAB+1) = FRIC
                    TABCOEF_FRIC_TMP(NIF,NTAB+2) = VISCF
                    IF(MFROT >0) THEN
                       TABCOEF_FRIC_TMP(NIF,NTAB+3) = C1
                       TABCOEF_FRIC_TMP(NIF,NTAB+4) = C2
                       TABCOEF_FRIC_TMP(NIF,NTAB+5) = C3
                       TABCOEF_FRIC_TMP(NIF,NTAB+6) = C4
                       TABCOEF_FRIC_TMP(NIF,NTAB+7) = C5
                       TABCOEF_FRIC_TMP(NIF,NTAB+8) = C6
                    ENDIF
                    IFRICORTH_TMP(NIF,NSET) = IDIR
c
                    IF(IDIR==1) THEN

                       NTAB = 16*NSET
                       TABCOEF_FRIC_TMP(NIF,NTAB+1) = FRIC2
                       TABCOEF_FRIC_TMP(NIF,NTAB+2) = VISCF2
                       IF(MFROT >0) THEN
                          TABCOEF_FRIC_TMP(NIF,NTAB+3) = C11
                          TABCOEF_FRIC_TMP(NIF,NTAB+4) = C22
                          TABCOEF_FRIC_TMP(NIF,NTAB+5) = C33
                          TABCOEF_FRIC_TMP(NIF,NTAB+6) = C44
                          TABCOEF_FRIC_TMP(NIF,NTAB+7) = C55
                          TABCOEF_FRIC_TMP(NIF,NTAB+8) = C66 
                       ENDIF
                    ENDIF
c
                  ENDDO
c
               END DO 

            ENDIF

C--Output--
            IF(IFLAG == 1 ) THEN             
               WRITE (IOUT,2004)
     .             GRPART1,GRPART2
               IF(IDIR==0) THEN
                  WRITE(IOUT,1510)
                  IF(MFROT.EQ.0)THEN
                     WRITE(IOUT,3503) FRIC
                     IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
                  ELSEIF(MFROT.EQ.1)THEN
                     WRITE(IOUT,1504) FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.2)THEN
                     WRITE(IOUT,1505)FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.3)THEN
                     WRITE(IOUT,1506)C1,C2,C3,C4,C5,C6
                  ENDIF
               ELSE
                  WRITE(IOUT,1511)
                  WRITE(IOUT,1512)
                  IF(MFROT.EQ.0)THEN
                     WRITE(IOUT,3503) FRIC
                     IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF
                  ELSEIF(MFROT.EQ.1)THEN
                     WRITE(IOUT,1504) FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.2)THEN
                     WRITE(IOUT,1505)FRIC,C1,C2,C3,C4,C5,C6
                  ELSEIF(MFROT.EQ.3)THEN
                     WRITE(IOUT,1506)C1,C2,C3,C4,C5,C6
                  ENDIF
                  WRITE(IOUT,1513)
                  IF(MFROT.EQ.0)THEN
                     WRITE(IOUT,3503) FRIC2
                     IF(FRICFORM /= 2) WRITE(IOUT,3504) VISCF2
                  ELSEIF(MFROT.EQ.1)THEN
                     WRITE(IOUT,1504) FRIC2,C11,C22,C33,C44,C55,C66
                  ELSEIF(MFROT.EQ.2)THEN
                     WRITE(IOUT,1505)FRIC2,C11,C22,C33,C44,C55,C66
                  ELSEIF(MFROT.EQ.3)THEN
                     WRITE(IOUT,1506)C11,C22,C33,C44,C55,C66
                  ENDIF
               ENDIF
            ENDIF

c               ENDDO
c
          ENDIF
C

       ENDDO ! N=1,NLINE

       DEALLOCATE (TRIGRPT,INDEX,NEWGRP,TAGG1,TAGG2) 

C
       RETURN



 1500 FORMAT(/1X,'     FRICTION INTERFACE MODEL NUMBER :',I10,1X,A/
     .            1X,'     -------------------------------             '/)
 1501 FORMAT(    /1X,'       DEFAULT VALUES               ' /
     .            1X,'       --------------             ' )

 1502 FORMAT(
     .    '        FRICTION FILTERING FLAG. . . . . . . . . ',I10/,
     .    '        FILTERING FACTOR . . . . . . . . . . . . ',1PG20.13/)
 1503 FORMAT(/
     .    '        FRICTION MODEL 0 (Coulomb Law) ')
 3503 FORMAT(/
     .    '          FRICTION COEFFICIENT . . . . . . . . . . ',1PG20.13/)
 3504 FORMAT(
     .    '          FRICTION CRITICAL DAMPING FACTOR. . . . .',1PG20.13/)
 3505 FORMAT(//
     .    '        FRICTION MODEL 1 (Viscous Polynomial)'/
     .    '          MU = MUo + C1 p + C2 v + C3 pv + C4 p^2 + C5 v^2'/)
 1504 FORMAT(//
     .    '          Muo. . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C1 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C2 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C3 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C4 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C5 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          TANGENTIAL PRESSURE LIMIT. . .. . . . . .',1PG20.13/)
 3506 FORMAT(/
     .    '        FRICTION MODEL 2 (Darmstad Law) :'/
     .    '          MU = MUo+c1*exp(c2*v)*p^2+c3*exp(c4*v)*p+c5*exp(c6*v)')
 1505 FORMAT(/
     .    '          Muo. . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C1 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C2 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C3 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C4 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C5 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C6 . . . . . . . . . . . . . . . . . . . ',1PG20.13/)
 3507 FORMAT(/
     .    '        FRICTION MODEL 3 (Renard law) ')
 1506 FORMAT(/
     .    '          C1 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C2 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C3 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C4 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C5 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '          C6 . . . . . . . . . . . . . . . . . . . ',1PG20.13/)
c 2503 FORMAT(/
c     .    '      FRICTION COEFFICIENT . . . . . . . . .  . . ',1PG20.13/
c     .    '      FRICTION CRITICAL DAMPING FACTOR. . . . . . ',1PG20.13)

 2001 FORMAT(/
     .    '          PART 1 . . . . . . . . . . . . . . . . . ',I10/,
     .    '          PART 2 . . . . . . . . . . . . . . . . . ',I10)
 2002 FORMAT(/
     .    '          GR_PART 1 . . . . . . . . . . . . . . . .',I10/,
     .    '          PART 2 . . . . . . . . . . . . . . . . . ',I10)
 2003 FORMAT(/
     .    '          PART 1 . . . . . . . . . . . . . . . . . ',I10/,
     .    '          GR_PART 2 . . . . . . . . . . . . . . . .',I10)
 2004 FORMAT(/
     .    '          GR_PART 1 . . . . . . . . . . . . . . . . ',I10/,
     .    '          GR_PART 2 . . . . . . . . . . . . . . . . ',I10)


 1507 FORMAT(    /1X,'       FRICTION COEFFICIENTS TABLE            ' /
     .            1X,'       ---------------------------             '/)

 1508 FORMAT( '        FRICTION FORMULATION: INCREMENTAL (STIFFNESS) ',
     .             'FORMULATION')
 1509 FORMAT( '        FRICTION FORMULATION: TOTAL (VISCOUS) ',
     .             'FORMULATION')
 1510 FORMAT(/
     .    '          ISOTROPIC FRICTION  ')
 1511 FORMAT(/
     .    '          ORTHOTROPIC FRICTION  ')
 1512 FORMAT(/
     .    '          FRICTION DIRECTION 1 : ')
 1513 FORMAT(/
     .    '          FRICTION DIRECTION 2 : ')

      END SUBROUTINE HM_READ_FRICTION

