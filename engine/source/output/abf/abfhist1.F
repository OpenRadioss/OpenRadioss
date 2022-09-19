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
Chd|  ABFHIST1                      source/output/abf/abfhist1.F  
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        EOR_C                         source/output/tools/sortie_c.c
Chd|        FLU_FIL_C                     source/output/tools/sortie_c.c
Chd|        FRETITL2                      source/input/freform.F        
Chd|        MY_CTIME                      source/system/timer_c.c       
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
             SUBROUTINE ABFHIST1(FILNAM,IFIL ,NTHGRP2,LONG ,
     2                 IWA  ,PM     ,GEO     ,IPART,
     3                 SUBSET,ITHGRP,ITHBUF,IGEO ,
     4                 IPM  ,IPARTH ,NPARTH ,NVPARTH ,
     5                 NVSUBTH ,ITHVAR,ITHFLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "impl1_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr13_c.inc"
#include      "scrfs_c.inc"
#include      "chara_c.inc"
#include      "rnur_c.inc"
#include      "titr_c.inc"
#include      "scr17_c.inc"
#include      "scr07_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   IWA(*),
     .   IPART(LIPART1,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .   ITHGRP(NITHGR,*),ITHBUF(*), IFIL,
     .   NTHGRP2, LONG,
     .   NPARTH,IPARTH(NPARTH,*),NVPARTH,
     .   NVSUBTH,ITHVAR(*),ITHFLAG
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*)
      CHARACTER FILNAM*100
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      REAL R4
      INTEGER ITITLE(100), IFILNAM(2148), ICODE, I, NJOINV, NRBAGV,
     .   NG, II, N, IH, ITY, NEL, NFT, K, MTN, NACCELV,
     .   IRUNR,NVAR,MID,PID,IAD1,IAD2,J,IAD,NGLOB,LTITL,NRECORD,
     .   IVAR(SITHVAR),WORK(70000),MODE,CPT,SAME_TITLE(NTHGRP2),
     .   INDEX(2*NTHGRP2),ITITLE_ALL(100,NTHGRP2)
      my_real
     .   TITLE(20),TIT40(10),TIT80(20),TIT100(25)
      CHARACTER EOR*8, CH8*8, CARD*80, BLA*7, CH8M*8, CH8L*8, CH8T*8
      CHARACTER CH80*80,TITL*100,VAR*10,TITL0*10

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      CHARACTER STRR*8, STRI*8
C-----------------------------------------------
      EXTERNAL STRR,STRI
      DATA BLA/'       '/
      DATA EOR/'ZZZZZEOR'/
C
      WRITE(CARD,'(20A4)')TEXT
      READ(CARD,'(20A4)')TITLE
        ICODE=3050
        LTITL = 100
        LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+LONG)
        DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(IFIL)
       IF(MCHECK==0)THEN
         CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
       ELSE
         CALL OPEN_C(IFILNAM,LEN_TMP_NAME,8)
       RETURN
       ENDIF
C-------TITRE------------
       DO I=1,80
         ITITLE(I)=ICHAR(CARD(I:I))
       ENDDO
       CALL EOR_C(84)
       CALL WRITE_I_C(ICODE,1)
       CALL WRITE_C_C(ITITLE,80)
       CALL EOR_C(84)
C-------ivers date------------
      CALL MY_CTIME(ITITLE)
      DO I=1,24
         CH80(I:I)=CHAR(ITITLE(I))
      ENDDO
      CH80(25:33) =' RADIOSS '
      CH80(34:59) =VERSIO(2)(9:34)
      CH80(60:80) =CPUNAM
      DO I=25,80
         ITITLE(I)=ICHAR(CH80(I:I))
      ENDDO
       CALL EOR_C(80)
       CALL WRITE_C_C(ITITLE,80)
       CALL EOR_C(80)
C
C-------ADDITIONAL RECORDS------------
C
C       NOMBRE DE RECORDS ADDITIONNELS
        NRECORD=2
          CALL EOR_C(4)
          CALL WRITE_I_C(NRECORD,1)
          CALL EOR_C(4)
C
C       1ER RECORD : LONGUEUR DES TITRES
          CALL EOR_C(4)
          CALL WRITE_I_C(LTITL,1)
          CALL EOR_C(4)
C
C       2EME RECORD : FAC_MASS,FAC_LENGTH,FAC_TIME 
          CALL EOR_C(12)
          R4=FAC_MASS
          CALL WRITE_R_C(R4,1)
          R4=FAC_LENGTH
          CALL WRITE_R_C(R4,1)
          R4=FAC_TIME
          CALL WRITE_R_C(R4,1)
          CALL EOR_C(12)
C-------TSTOP------------
          CALL EOR_C(4)
          R4=TSTOP
          CALL WRITE_R_C(R4,1)
          CALL EOR_C(4)
C-------IMPLICIT------------
          CALL EOR_C(4)
          CALL WRITE_I_C(IMPL_S,1)
          CALL EOR_C(4)
C-------HIERARCHIE INFO------------
      IWA(1)=NPART+NTHPART
      IWA(2)=NUMMAT
      IWA(3)=NUMGEO
      IWA(4)=NSUBS
      IWA(5)=NTHGRP2
        IF(NSECT==0.AND.NSFLSW/=0) IWA(5)=NTHGRP2+1
        NGLOB=15
       IF (IFIL/=11) NGLOB=0
      IWA(6)=NGLOB
      CALL WRTDES(IWA,IWA,6,3,0)

      DO I=1,IWA(6)
          IWA(I)=I
      ENDDO
      CALL WRTDES(IWA,IWA,NGLOB,3,0)
C-------PART DESCRIPTION------------
      DO N=1,NPART+NTHPART
        NVAR=IPARTH(NVPARTH,N)
        IAD =IPARTH(NVPARTH+1,N)
        CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,N),40)
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
         CALL EOR_C(20+LTITL)
         CALL WRITE_I_C(IPART(4,N),1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL WRITE_I_C(IPART(7,N),1)
         CALL WRITE_I_C(IPART(1,N),1)
         CALL WRITE_I_C(IPART(2,N),1)
         CALL WRITE_I_C(NVAR,1)
         CALL EOR_C(20+LTITL)
        II=0
        DO I=IAD,IAD+NVAR-1
          II=II+1
          IWA(II)=ITHBUF(I)
        ENDDO
        IF(NVAR/=0)CALL WRTDES(IWA,IWA,NVAR,3,0)
      ENDDO
C-------MATER DESCRIPTION------------
      DO N=1,NUMMAT
        MID = IPM(1,N)
        CALL FRETITL2(TITL,IPM(NPROPMI-LTITR+1,N),40)
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
         CALL EOR_C(4+LTITL)
         CALL WRITE_I_C(MID,1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(4+LTITL)
      ENDDO
C-------GEO DESCRIPTION------------
      DO N=1,NUMGEO
        PID = IGEO(1,N)
        CALL FRETITL2(TITL,IGEO(NPROPGI-LTITR+1,N),40)
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
         CALL EOR_C(4+LTITL)
         CALL WRITE_I_C(PID,1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(4+LTITL)
      ENDDO
C-------HIERARCHIE DESCRIPTION------------
      DO N=1,NSUBS
!!        NVAR=ISUBTH(NVSUBTH,N)
!!        IAD =ISUBTH(NVSUBTH+1,N)
        NVAR=SUBSET(N)%NVARTH(ITHFLAG)
        IAD =SUBSET(N)%THIAD
!!        CALL FRETITL2(TITL,ISUBS(LISUB1-LTITR+1,N),40)
        TITL = SUBSET(N)%TITLE
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
         CALL EOR_C(20+LTITL)
!!         CALL WRITE_I_C(ISUBS(1,N),1)
         CALL WRITE_I_C(SUBSET(N)%ID,1)
!!         CALL WRITE_I_C(ISUBS(10,N),1)
         CALL WRITE_I_C(SUBSET(N)%PARENT,1)
!!         CALL WRITE_I_C(ISUBS(2,N),1)
         CALL WRITE_I_C(SUBSET(N)%NCHILD,1)
!!         CALL WRITE_I_C(ISUBS(4,N),1)
         CALL WRITE_I_C(SUBSET(N)%NPART,1)
         CALL WRITE_I_C(NVAR,1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(20+LTITL)
!!        IF(ISUBS(2,N)/=0)CALL WRTDES(IBUFSSG(ISUBS(3,N)),
!!     .                      IBUFSSG(ISUBS(3,N)),ISUBS(2,N),3,0)
        IF(SUBSET(N)%NCHILD/=0)CALL WRTDES(SUBSET(N)%CHILD,
     .                      SUBSET(N)%CHILD,SUBSET(N)%NCHILD,3,0)
!!        IF(ISUBS(4,N)/=0)CALL WRTDES(IBUFSSG(ISUBS(5,N)),
!!     .                      IBUFSSG(ISUBS(5,N)),ISUBS(4,N),3,0)
        IF(SUBSET(N)%NPART/=0)CALL WRTDES(SUBSET(N)%PART,
     .                      SUBSET(N)%PART,SUBSET(N)%NPART,3,0)
        II=0
        DO I=IAD,IAD+NVAR-1
          II=II+1
          IWA(II)=ITHBUF(I)
        ENDDO
        IF(NVAR/=0)CALL WRTDES(IWA,IWA,NVAR,3,0)

      ENDDO
C-------TH GROUP name sorting ( check /TH with same title )
      DO N=1,NTHGRP2
        INDEX(N) = N
        SAME_TITLE(N) = 0
        DO I=1,100
          ITITLE_ALL(I,N) = 0
        ENDDO
      ENDDO

      DO N=1,NTHGRP2
        CALL FRETITL2(TITL,ITHGRP(NITHGR-LTITR+1,N),LTITL)
        DO I=1,LTITL
          ITITLE_ALL(I,N)=ICHAR(TITL(I:I))
        ENDDO
      ENDDO

      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITITLE_ALL, INDEX, NTHGRP2 , LTITL)

      DO N=1,NTHGRP2-1
        CPT = 0
        DO I=1,LTITL
          IF(ITITLE_ALL(I,INDEX(N+1)) == ITITLE_ALL(I,INDEX(N)) ) CPT = CPT + 1
        ENDDO
        IF (CPT == LTITL) THEN
          SAME_TITLE(INDEX(N)) = 1
          SAME_TITLE(INDEX(N+1)) = 1
        ENDIF
      ENDDO
c
C-------TH GROUP------------
      DO N=1,NTHGRP2
        NVAR=ITHGRP(6,N)
        CALL FRETITL2(TITL,ITHGRP(NITHGR-LTITR+1,N),40)
c
        IF(SAME_TITLE(N) == 1) THEN
          WRITE(TITL0,'(I10)')ITHGRP(1,N)
          DO I=1,10
            ITITLE(I)=ICHAR(TITL0(I:I))
          ENDDO
          DO I=11,LTITL
            ITITLE(I)=ICHAR(TITL(I-10:I-10))
          ENDDO
        ELSE
          DO I=1,LTITL
            ITITLE(I)=ICHAR(TITL(I:I))
          ENDDO
        ENDIF
c
        ITY=ITHGRP(2,N)
        IF (ITY==100) ITY=6
         CALL EOR_C(20+LTITL)
         CALL WRITE_I_C(ITHGRP(1,N),1)
         CALL WRITE_I_C(ITY,1)
         CALL WRITE_I_C(ITHGRP(3,N),1)
         CALL WRITE_I_C(ITHGRP(4,N),1)
         CALL WRITE_I_C(ITHGRP(6,N),1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(20+LTITL)
        IAD1=ITHGRP(5,N)+2*ITHGRP(4,N)
        IAD2=ITHGRP(8,N)
        DO J=1,ITHGRP(4,N)
        CALL FRETITL2(TITL,ITHBUF(IAD2),40)
         DO I=1,LTITL
           ITITLE(I)=ICHAR(TITL(I:I))
         ENDDO
         CALL EOR_C(4+LTITL)
           CALL WRITE_I_C(ITHBUF(IAD1),1)
           CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(4+LTITL)
          IAD1=IAD1+1
          IAD2=IAD2+40
        ENDDO
        IF(NVAR/=0) THEN
          DO I=1,NVAR
            DO J=1,10
              VAR(J:J)=CHAR(ITHVAR((ITHGRP(9,N)-1+I-1)*10+J))
              IVAR(J)=ITHVAR((ITHGRP(9,N)-1+I-1)*10+J)
            ENDDO
            CALL EOR_C(10)
              CALL WRITE_C_C(IVAR,10)
            CALL EOR_C(10)
          ENDDO
c          CALL WRTDES(ITHBUF(ITHGRP(7,N)),
c     .                           ITHBUF(ITHGRP(7,N)),NVAR,3,0)
        ENDIF
      ENDDO
C-------TH GROUP + 1 section fluide------------
      IF(NSECT==0.AND.NSFLSW/=0) THEN
        NVAR=6
        TITL='FLUID SECTION'
         DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
         ENDDO
         CALL EOR_C(20+LTITL)
         CALL WRITE_I_C(104,1)
         CALL WRITE_I_C(104,1)
         CALL WRITE_I_C(1,1)
         CALL WRITE_I_C(NSFLSW,1)
         CALL WRITE_I_C(NVAR,1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(20+LTITL)
        DO J=1,NSFLSW
           CALL EOR_C(4+LTITL)
           CALL WRITE_I_C(J,1)
           CALL WRITE_C_C(ITITLE,LTITL)
           CALL EOR_C(4+LTITL)
        ENDDO
        DO I=1,6
            IWA(I)=I
        ENDDO
        CALL WRTDES(IWA,IWA,6,3,0)
      ENDIF
      CALL FLU_FIL_C()
C
      RETURN
      END
