Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HIST13                        source/output/th/hist13.F     
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        EOR_C                         source/output/tools/sortie_c.c
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        STRI                          source/tools/univ/stri.F      
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE HIST13(IPARG ,IXS  ,IXQ  ,IXC    ,IXT    ,
     2                  IXP   ,IXR  ,IWA  ,ITAB   ,PM     ,
     3                  NPBY  ,IXTG ,IRFE ,IXUR   ,LACCELM,
     4                  IPARI ,IPART,ITHGRP ,ITHBUF,CHRUN_OLD)
C=======================================================================
C   OLD TH V3 RADIOSS INPUT V3 or V4
C=======================================================================
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
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr13_c.inc"
#include      "scr17_c.inc"
#include      "scrfs_c.inc"
#include      "chara_c.inc"
#include      "rnur_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRFE
      INTEGER IPARG(NPARG,*), IXS(NIXS,*), IXQ(NIXQ,*),
     .   IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*),
     .   IXUR(NIXUR,*), IXTG(NIXTG,*), IWA(*), ITAB(*),
     .   IPARI(NPARI,*),LACCELM(3,*),IPART(LIPART1,*), NPBY(NNPBY,*),
     .   ITHGRP(NITHGR,*), ITHBUF(*)
C     REAL
      my_real
     .   PM(NPROPM,*)
      CHARACTER CHRUN_OLD*2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITITLE(80), IFILNAM(2148), ICODE, I, NJOINV, NRBAGV,
     .   NG, II, N, IH, ITY, NEL, NFT, K, MTN, NACCELV,NINTERS,
     .   IRUNR,NN,IAD,J,ITYP
C     REAL
      my_real
     .   TITLE(20)
      CHARACTER EOR*8, CH8*8, CARD*80, FILNAM*100, BLA*7

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      CHARACTER STRI*8
C-----------------------------------------------
      EXTERNAL STRI
      DATA BLA/'       '/
      DATA EOR/'ZZZZZEOR'/
C
C
      NINTERS = 0
      DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IF(ITYP==101)NINTERS = NINTERS + NN
      ENDDO
C
      FILNAM=ROOTNAM(1:ROOTLEN)//'T'//CHRUN_OLD
C
      WRITE(CARD,'(20A4)')TEXT
      READ(CARD,'(20A4)')TITLE
C      ICODE=3017
C      ICODE=3023
      ICODE=3030
C      ICODE=3040
C
      LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN+3
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+3) 

      IF(ITFORM==0)THEN
       OPEN(UNIT=IUHIS,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='UNFORMATTED',STATUS='UNKNOWN')
      ELSEIF(ITFORM==1.OR.ITFORM==2)THEN
       OPEN(UNIT=IUHIS,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')
      ELSEIF(ITFORM==3)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(1)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
      ELSEIF(ITFORM==4)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(1)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,3)
       ITFORM=3
      ELSEIF(ITFORM==5)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(1)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,6)
       ITFORM=3
      ENDIF
C
      IF(ITFORM==0)THEN
        WRITE(IUHIS)ICODE,TITLE
      ELSEIF(ITFORM==1)THEN
        CH8=STRI(ICODE)
        WRITE(IUHIS,'(A)')FILNAM(1:ROOTLEN+3)
        WRITE(IUHIS,'(2A)')CH8,CARD(1:72)
      ELSEIF(ITFORM==2)THEN
        WRITE(IUHIS,'(2A)')FILNAM(1:ROOTLEN+3),' FORMAT'
        WRITE(IUHIS,'(A,I5,A,I5,A)')EOR,1,'I',72,'C'
        WRITE(IUHIS,'(I5,A)')ICODE,CARD(1:72)
      ELSEIF(ITFORM==3)THEN
       DO 5 I=1,80
 5     ITITLE(I)=ICHAR(CARD(I:I))
       CALL EOR_C(84)
       CALL WRITE_I_C(ICODE,1)
       CALL WRITE_C_C(ITITLE,80)
       CALL EOR_C(84)
      ENDIF
C
      IF(NSMAT/=0.AND.INVSTR<40) THEN
C  009        DO N=1,NPART
        DO N=1,NUMMAT-1
           IWA(N)=0
        ENDDO
        DO N=1,NPART
          IF(IPART(8,N)>=1) IWA(IPART(1,N))=1
        ENDDO
        NSMAT=0
        DO N=1,NUMMAT-1
          NSMAT=NSMAT+IWA(N)
        ENDDO
      ENDIF
C
      NGLV=12
      NMTV=6
      NINV=6
      NRWV=6
      NRBV=9
      NNODV=9
      NSCV=9
      NJOINV=6
      NRBAGV=9
C      NELV=22
      NACCELV=3
      NELSV=19
      NELTV=6
      NELPV=9
      NELRV=14
      NELCV=22
      NELQV =NELSV
      NELTGV=NELCV
      NELURV=12
C
C
      IWA(1) =NGLV
      IWA(2) =NSMAT
      IWA(3) =NMTV
      IWA(4) =NSNOD
      IWA(5) =NNODV
      IWA(6) =NSELQ
      IWA(7) =NELQV
      IWA(8) =NSELS
      IWA(9) =NELSV
      IWA(10)=NSELC
      IWA(11)=NELCV
      IWA(12)=NSELT
      IWA(13)=NELTV
      IWA(14)=NSELP
      IWA(15)=NELPV
      IWA(16)=NSELR
      IWA(17)=NELRV
      IWA(18)=NINTERS
      IWA(19)=NINV
      IWA(20)=NRWALL
      IWA(21)=NRWV
      IWA(22)=NSRBY
      IWA(23)=NRBV
      IWA(24)=NSECT
      IF (NSECT ==0 ) IWA(24)=NSFLSW
      IWA(25)=NSCV
      IWA(26)=NJOINT
      IWA(27)=NJOINV
      IWA(28)=NRBAG+NVOLU
      IWA(29)=NRBAGV
      IWA(30)=NSELTG
      IWA(31)=NELTGV
C      IWA(32)=NSELUR
      IWA(32)=NSELU
      IWA(33)=NELURV
      IWA(34)=NACCELM
      IWA(35)=NACCELV
      IUNIT=IUHIS
      CALL WRTDES(IWA,IWA,35,ITFORM,0)
C
      IF(NSMAT/=0) THEN
        IF(INVSTR<40) THEN
C  009          DO N=1,NPART
          DO N=1,NUMMAT-1
           IWA(N)=0
          ENDDO
          II=0
          DO N=1,NPART
           IF(IPART(8,N)>=1)THEN
             II=IPART(1,N)
             IWA(II)=IPART(5,N)
           ENDIF
          ENDDO
          NSMAT=0
C  009          DO N=1,NPART
          DO N=1,NUMMAT-1
           IF(IWA(N)/=0)THEN
             NSMAT=NSMAT+1
             IWA(NSMAT)=IWA(N)
           ENDIF
          ENDDO
        ELSE
          DO N=1,NPART
             IWA(N)=IPART(4,N)
          ENDDO
        ENDIF
      ENDIF
C
      IF(NSMAT/=0) THEN
        CALL WRTDES(IWA,IWA,NSMAT,ITFORM,0)
      ENDIF
C--------------------------------
       IF(NINTERS/=0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==101)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              II=II+1
              IWA(II)=IPARI(15,I)
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,NINTERS,ITFORM,0)
       ENDIF
C
       IF(NRWALL /= 0) THEN
        II=0
        DO I=1,NRWALL
            II=II+1
            IWA(II)=II
        ENDDO
        CALL WRTDES(IWA,IWA,NRWALL,ITFORM,0)
       ENDIF
C
C--------------------------------
      IF(NSRBY/=0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==103)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              II=II+1
              IWA(II)=ITAB(NPBY(1,I))
c              IWA(II)=ITHBUF(J)
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,NSRBY,ITFORM,0)
      ENDIF
C--------------------------------
C
       IF(NSECT/=0) THEN
        II=0
        DO I=1,NSECT
            II=II+1
            IWA(II)=II
        ENDDO
        CALL WRTDES(IWA,IWA,NSECT,ITFORM,0)
       ELSEIF(NSFLSW/=0) THEN
        II=0
        DO I=1,NSFLSW
            II=II+1
            IWA(II)=II
        ENDDO
        CALL WRTDES(IWA,IWA,NSFLSW,ITFORM,0)
       ENDIF
C
       IF(NJOINT/=0) THEN
        II=0
        DO I=1,NJOINT
            II=II+1
            IWA(II)=II
        ENDDO
        CALL WRTDES(IWA,IWA,NJOINT,ITFORM,0)
       ENDIF
C
       IF(NRBAG+NVOLU/=0) THEN
        II=0
        DO I=1,NRBAG+NVOLU
            II=II+1
            IWA(II)=II
        ENDDO
        CALL WRTDES(IWA,IWA,NRBAG+NVOLU,ITFORM,0)
       ENDIF
C
C--------------------------------
      IF(NACCELM/=0) THEN
       DO N=1,NACCELM
         IWA(N)=LACCELM(2,N)
       ENDDO
        CALL WRTDES(IWA,IWA,NACCELM,ITFORM,0)
      ENDIF
C
      IF(NSNOD/=0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==0)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              II=II+1
              IWA(II)=ITAB(I)
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
      IF (NSELS>0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==1)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              MTN=NINT(PM(19,IXS(1,I)))
              II=II+1
              IWA(II)=IXS(NIXS,I)
              II=II+1
              IWA(II)=MTN
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
      IF (NSELQ>0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==2)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              MTN=NINT(PM(19,IXQ(1,I)))
              II=II+1
              IWA(II)=IXQ(NIXQ,I)
              II=II+1
              IWA(II)=MTN
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
      IF (NSELC>0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==3)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              MTN=NINT(PM(19,IXC(1,I)))
              II=II+1
              IWA(II)=IXC(NIXC,I)
              II=II+1
              IWA(II)=MTN
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
      IF (NSELTG>0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==7)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              MTN=NINT(PM(19,IXTG(1,I)))
              II=II+1
              IWA(II)=IXTG(NIXTG,I)
              II=II+1
              IWA(II)=MTN
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
      IF (NSELT>0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==4)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              MTN=NINT(PM(19,IXT(1,I)))
              II=II+1
              IWA(II)=IXT(NIXT,I)
              II=II+1
              IWA(II)=MTN
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
      IF (NSELP>0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==5)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              MTN=NINT(PM(19,IXP(1,I)))
              II=II+1
              IWA(II)=IXP(NIXP,I)
              II=II+1
              IWA(II)=MTN
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
      IF (NSELR>0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==6)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              II=II+1
              IWA(II)=IXR(NIXR,I)
              II=II+1
              IWA(II)=0
            ENDDO
          ELSEIF(ITYP==100) THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              II=II+1
              IWA(II)=ITHBUF(J+2*NN)
              II=II+1
              IWA(II)=0
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
C      IF (NSELUR>0) THEN
      IF (NSELU>0) THEN
        II=0
        DO N=1,NTHGRP
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          IF(ITYP==50)THEN
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              II=II+1
              IWA(II)=IXUR(NIXUR,I)
              II=II+1
              IWA(II)=0
            ENDDO
          ENDIF
        ENDDO
        CALL WRTDES(IWA,IWA,II,ITFORM,0)
      ENDIF
C
      RETURN
      END
