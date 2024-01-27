Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HIST1                         source/output/th/hist1.F      
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        CUR_FIL_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        EOR_C                         ../common_source/tools/input_output/write_routtines.c
Chd|        FLU_FIL_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        FRETITL2                      source/input/freform.F        
Chd|        MY_CTIME                      source/system/timer_c.c       
Chd|        OPEN_C                        ../common_source/tools/input_output/write_routtines.c
Chd|        WRITE_C_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        STRI                          source/tools/univ/stri.F      
Chd|        STRR                          source/tools/univ/strr.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        TH_MOD                        share/modules/th_mod.F        
Chd|====================================================================
             SUBROUTINE HIST1(FILNAM,IFIL ,NTHGRP2,LONG ,
     2                 IWA  ,PM     ,GEO  ,IPART,
     3                 SUBSET,ITHGRP,ITHBUF,IGEO ,
     4                 IPM  ,IPARTH ,NPARTH ,NVPARTH ,
     5                 NVSUBTH ,ITTYP,ITHFLAG,ITHVAR,IFILTITL,
     6                 SITHBUF,NAMES_AND_TITLES)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE INOUTFILE_MOD
      USE TH_MOD
      USE NAMES_AND_TITLES_MOD, only:NAMES_AND_TITLES_,LTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr13_c.inc"
#include      "scrfs_c.inc"
#include      "chara_c.inc"
#include      "titr_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "sysunit.inc"
#include      "rad2r_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)                     :: SITHBUF  ! Size of ithbuf
      INTEGER,INTENT(IN), DIMENSION(SITHBUF) :: ITHBUF   ! Time history buffer
      INTEGER 
     .   IWA(*),
     .   IPART(LIPART1,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .   ITHGRP(NITHGR,*), IFIL,
     .   NTHGRP2, LONG,
     .   NPARTH,IPARTH(NPARTH,*),NVPARTH,NVSUBTH,
     .   ITTYP,ITHFLAG,ITHVAR(*),IFILTITL
C     REAL
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*)
      CHARACTER FILNAM*100
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE(NAMES_AND_TITLES_),INTENT(IN) :: NAMES_AND_TITLES  !< NAMES_AND_TITLES host the input deck names and titles for outputs
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      REAL R4
      INTEGER ITITLE(100), IFILNAM(100), ICODE, I, NJOINV, NRBAGV,
     .   NG, II, N, IH, ITY, NEL, NFT, K, MTN, NACCELV,
     .   IRUNR,NVAR,MID,PID,IAD1,IAD2,J,IAD,LTITL,NRECORD,
     .   SEEK_LOC,IPART1,IPART2    
C     REAL
      my_real
     .   TIT40(10),TIT80(20),TIT100(25)
      my_real,DIMENSION(20) :: TITLE
      CHARACTER EOR*8, CH8*8,BLA*7, CH8M*8, CH8L*8, CH8T*8 
      CHARACTER (LEN=LTITLE) :: CARD
      CHARACTER CH80*80,TITL*100,VAR*10
      INTEGER :: LEN_TMP_NAME, TITLSUM
      INTEGER, DIMENSION(20) :: TEXT
      CHARACTER(len=2148) :: TMP_NAME
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      CHARACTER STRR*8, STRI*8
C-----------------------------------------------
      EXTERNAL STRR,STRI
      DATA BLA/'       '/
      DATA EOR/'ZZZZZEOR'/
C=======================================================================
C
      CARD(1:LTITLE)=NAMES_AND_TITLES%TITLE(1:LTITLE)
C      ICODE=3017
C      ICODE=3023
C      ICODE=3030
      IF(TH_VERS>=2021)THEN
        ICODE=4021
        LTITL = 100
      ELSEIF(TH_VERS>=50)THEN
        ICODE=3050
        LTITL = 100
      ELSEIF(TH_VERS>=47)THEN
        ICODE=3041
        LTITL = 80
      ELSE
        ICODE=3040
        LTITL = 40
      ENDIF
C
      LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN+LONG
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))  
C
      IF(TH_TITLES == 1)
     .     OPEN(UNIT=IFILTITL,FILE=TMP_NAME(1:LEN_TMP_NAME)//'_TITLES',
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')
C     
      IF(ITTYP==0)THEN
        OPEN(UNIT=IUNIT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='UNFORMATTED',STATUS='UNKNOWN')
      ELSEIF(ITTYP==1.OR.ITTYP==2)THEN
       OPEN(UNIT=IUNIT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')
      ELSEIF(ITTYP==3)THEN
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
      ELSEIF(ITTYP==4)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(IFIL)
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,3)  
       ITTYP=3
      ELSEIF(ITTYP==5)THEN
       DO I=1,LEN_TMP_NAME
         IFILNAM(I)=ICHAR(TMP_NAME(I:I))
       ENDDO
       CALL CUR_FIL_C(IFIL)       
       CALL OPEN_C(IFILNAM,LEN_TMP_NAME,6)
       ITTYP=3
      ENDIF
C-------TITRE------------
      IF(ITTYP==0)THEN
        READ(CARD,'(20A4)')TITLE
        WRITE(IUNIT)ICODE,TITLE
      ELSEIF(ITTYP==1)THEN
        CH8=STRI(ICODE)
        WRITE(IUNIT,'(A)')FILNAM(1:ROOTLEN+LONG)         
        WRITE(IUNIT,'(2A)')CH8,CARD(1:72)
      ELSEIF(ITTYP==2)THEN
        WRITE(IUNIT,'(2A)')FILNAM(1:ROOTLEN+LONG),' FORMAT'
        WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',72,'C'
        WRITE(IUNIT,'(I5,A)')ICODE,CARD(1:72)
      ELSEIF(ITTYP==3)THEN
       DO I=1,80
         ITITLE(I)=ICHAR(CARD(I:I))
       ENDDO
       CALL EOR_C(84)
       CALL WRITE_I_C(ICODE,1)
       CALL WRITE_C_C(ITITLE,80)
       CALL EOR_C(84)
      ENDIF
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
      IF(ITTYP==0)THEN
        READ(CH80,'(20A4)')TITLE  
        WRITE(IUNIT)TITLE  
      ELSEIF(ITTYP==1)THEN
        WRITE(IUNIT,'(A)')CH80
      ELSEIF(ITTYP==2)THEN
        WRITE(IUNIT,'(2A)')FILNAM(1:ROOTLEN+LONG),' FORMAT'
        WRITE(IUNIT,'(A,I5,A)')EOR,80,'C'
        WRITE(IUNIT,'(A)')CH80
      ELSEIF(ITTYP==3)THEN
       CALL EOR_C(80)
       CALL WRITE_C_C(ITITLE,80)
       CALL EOR_C(80)
      ENDIF
C
C-------ADDITIONAL RECORDS------------
      IF(TH_VERS>=50)THEN
C
C       NOMBRE DE RECORDS ADDITIONNELS
        NRECORD=2
        IF(ITTYP==0)THEN
          WRITE(IUNIT)NRECORD
        ELSEIF(ITTYP==1)THEN
          CH8=STRI(NRECORD)
          WRITE(IUNIT,'(2A)')CH8
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A)')EOR,1,'I'
          WRITE(IUNIT,'(I5)')NRECORD
        ELSEIF(ITTYP==3)THEN
          CALL EOR_C(4)
          CALL WRITE_I_C(NRECORD,1)
          CALL EOR_C(4)
        ENDIF
C
C       1ER RECORD : LONGUEUR DES TITRES
        IF(ITTYP==0)THEN
          WRITE(IUNIT)LTITL
        ELSEIF(ITTYP==1)THEN
          CH8=STRI(LTITL)
          WRITE(IUNIT,'(2A)')CH8
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A)')EOR,1,'I'
          WRITE(IUNIT,'(I5)')LTITL
        ELSEIF(ITTYP==3)THEN
          CALL EOR_C(4)
          CALL WRITE_I_C(LTITL,1)
          CALL EOR_C(4)
        ENDIF
C
C       2EME RECORD : FAC_MASS,FAC_LENGTH,FAC_TIME        
        IF(ITTYP==0)THEN
          WRITE(IUNIT) FAC_MASS,FAC_LENGTH,FAC_TIME 
        ELSEIF(ITTYP==1)THEN
          CH8M=STRR(FAC_MASS)
          CH8L=STRR(FAC_LENGTH)
          CH8T=STRR(FAC_TIME)
          WRITE(IUNIT,'(3A8)')CH8M,CH8L,CH8T
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A)')EOR,3,'R'
          WRITE(IUNIT,'((5(1X,1PE15.8)))')FAC_MASS,FAC_LENGTH,FAC_TIME 
        ELSEIF(ITTYP==3)THEN
          CALL EOR_C(12)
          R4=FAC_MASS
          CALL WRITE_R_C(R4,1)
          R4=FAC_LENGTH
          CALL WRITE_R_C(R4,1)
          R4=FAC_TIME
          CALL WRITE_R_C(R4,1)
          CALL EOR_C(12)
        ENDIF
      END IF      
C-------HIERARCHY INFO------------
      IWA(1)=NPART+NTHPART
      IWA(2)=NUMMAT
      IWA(3)=NUMGEO
      IWA(4)=NSUBS
      IWA(5)=NTHGRP2 
      IF(NSECT==0.AND.NSFLSW/=0) IWA(5)=NTHGRP2+1
      IF(TH_VERS >= 2021)THEN
        NGLOBTH=15   
      ELSE
        NGLOBTH=12
      END IF
c
      IF (IUNIT /= IUHIS) THEN
        IWA(6)= 0
      ELSE
        IWA(6)= NGLOBTH
      ENDIF
c
      CALL WRTDES(IWA,IWA,6,ITTYP,0)
       
      DO I=1,IWA(6)
          IWA(I)=I
      ENDDO
      IF(IUNIT == IUHIS) CALL WRTDES(IWA,IWA,NGLOBTH,ITTYP,0)     
C-------PART DESCRIPTION------------
      DO N=1,NPART+NTHPART
        NVAR=IPARTH(NVPARTH,N)
        IAD =IPARTH(NVPARTH+1,N)
        CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,N),40)
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
        IF (N > NPART)THEN
          IPART1 = 0
          IPART2 = 0
        ELSE
          IPART1 = IPART(1,N)
          IPART2 = IPART(2,N)
        ENDIF
        IF(ITTYP==0)THEN
          IF(LTITL==40)THEN
            READ(TITL,'(10A4)')TIT40
            WRITE(IUNIT)IPART(4,N),TIT40,IPART(7,N),
     .                  IPART1,IPART2,NVAR
          ELSE IF(LTITL==80)THEN
            READ(TITL,'(20A4)')TIT80
            WRITE(IUNIT)IPART(4,N),TIT80,IPART(7,N),
     .                  IPART1,IPART2,NVAR
          ELSE
            READ(TITL,'(25A4)')TIT100
            WRITE(IUNIT)IPART(4,N),TIT100,IPART(7,N),
     .                  IPART1,IPART2,NVAR
          ENDIF
        ELSEIF(ITTYP==1)THEN
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A,I5,A,I5,A)')EOR,1,'I',40,'C',4,'I'
          WRITE(IUNIT,'(I10,A,4I5)')IPART(4,N),TITL(1:LTITL),
     .                IPART(7,N),IPART1,IPART2,NVAR
        ELSEIF(ITTYP==3)THEN
         CALL EOR_C(20+LTITL)
         CALL WRITE_I_C(IPART(4,N),1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL WRITE_I_C(IPART(7,N),1)
         CALL WRITE_I_C(IPART1,1)
         CALL WRITE_I_C(IPART2,1)
         CALL WRITE_I_C(NVAR,1)
         CALL EOR_C(20+LTITL)
        ENDIF
        II=0
        DO I=IAD,IAD+NVAR-1
          II=II+1
          IF(I <= SITHBUF) THEN
            IWA(II)=ITHBUF(I)
          ELSE
            IWA(II) = 0 
          ENDIF
        ENDDO
        IF(NVAR/=0)CALL WRTDES(IWA,IWA,NVAR,ITTYP,0)       
      ENDDO
C-------MATER DESCRIPTION------------
      DO N=1,NUMMAT
        MID = IPM(1,N)
        CALL FRETITL2(TITL,IPM(NPROPMI-LTITR+1,N),40)
        TITLSUM=SUM(IPM(NPROPMI-LTITR+1:NPROPMI-LTITR+40,N))
        IF(TITLSUM == 0)THEN
          TITL(1:LTITL)=' '
          TITL(1:8)='no_title'
        ENDIF
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
        IF(ITTYP==0)THEN
          IF(LTITL==40)THEN
            READ(TITL,'(10A4)')TIT40
            WRITE(IUNIT)MID,TIT40
          ELSE IF(LTITL==80)THEN
            READ(TITL,'(20A4)')TIT80
            WRITE(IUNIT)MID,TIT80
          ELSE
            READ(TITL,'(25A4)')TIT100
            WRITE(IUNIT)MID,TIT100
          ENDIF
        ELSEIF(ITTYP==1)THEN
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',LTITL,'C'
          WRITE(IUNIT,'(I10,A)')MID,TITL(1:LTITL)
        ELSEIF(ITTYP==3)THEN
         CALL EOR_C(4+LTITL)
         CALL WRITE_I_C(MID,1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(4+LTITL)
        ENDIF
      ENDDO
C-------GEO DESCRIPTION------------
      DO N=1,NUMGEO
        PID = IGEO(1,N)
        CALL FRETITL2(TITL,IGEO(NPROPGI-LTITR+1,N),40)
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
        IF(ITTYP==0)THEN
          IF(LTITL==40)THEN
            READ(TITL,'(10A4)')TIT40
            WRITE(IUNIT)PID,TIT40
          ELSE IF(LTITL==80)THEN
            READ(TITL,'(20A4)')TIT80
            WRITE(IUNIT)PID,TIT80
          ELSE
            READ(TITL,'(25A4)')TIT100
            WRITE(IUNIT)PID,TIT100
          ENDIF
        ELSEIF(ITTYP==1)THEN
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',LTITL,'C'
          WRITE(IUNIT,'(I10,A)')PID,TITL(1:LTITL)
          
        ELSEIF(ITTYP==3)THEN
         CALL EOR_C(4+LTITL)
         CALL WRITE_I_C(PID,1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(4+LTITL)
        ENDIF
      ENDDO
C-------HIERARCHY DESCRIPTION------------
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
        IF(ITTYP==0)THEN
          IF(LTITL==40)THEN
            READ(TITL,'(10A4)')TIT40
!!            WRITE(IUNIT)ISUBS(1,N),ISUBS(10,N),
!!     .               ISUBS(2,N),ISUBS(4,N),NVAR,TIT40
            WRITE(IUNIT)SUBSET(N)%ID,SUBSET(N)%PARENT,
     .               SUBSET(N)%NCHILD,SUBSET(N)%NPART,NVAR,TIT40
          ELSE IF(LTITL==00)THEN  
            READ(TITL,'(20A4)')TIT80
!!            WRITE(IUNIT)ISUBS(1,N),ISUBS(10,N),
!!     .               ISUBS(2,N),ISUBS(4,N),NVAR,TIT80
            WRITE(IUNIT)SUBSET(N)%ID,SUBSET(N)%PARENT,
     .               SUBSET(N)%NCHILD,SUBSET(N)%NPART,NVAR,TIT80
          ELSE
            READ(TITL,'(25A4)')TIT100
!!            WRITE(IUNIT)ISUBS(1,N),ISUBS(10,N),
!!     .               ISUBS(2,N),ISUBS(4,N),NVAR,TIT100
            WRITE(IUNIT)SUBSET(N)%ID,SUBSET(N)%PARENT,
     .               SUBSET(N)%NCHILD,SUBSET(N)%NPART,NVAR,TIT100
          ENDIF
        ELSEIF(ITTYP==1)THEN
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,5,'I',LTITL,'C'
!!          WRITE(IUNIT,'(5I10,A)')ISUBS(1,N),ISUBS(10,N),
!!     .                ISUBS(2,N),ISUBS(4,N),NVAR,TITL(1:LTITL)
          WRITE(IUNIT,'(5I10,A)')SUBSET(N)%ID,SUBSET(N)%PARENT,
     .               SUBSET(N)%NCHILD,SUBSET(N)%NPART,NVAR,TITL(1:LTITL)
        ELSEIF(ITTYP==3)THEN
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
        ENDIF
!!        IF(ISUBS(2,N)/=0)CALL WRTDES(IBUFSSG(ISUBS(3,N)),
!!     .                      IBUFSSG(ISUBS(3,N)),ISUBS(2,N),ITTYP,0)
        IF(SUBSET(N)%NCHILD/=0)CALL WRTDES(SUBSET(N)%CHILD,
     .                      SUBSET(N)%CHILD,SUBSET(N)%NCHILD,ITTYP,0)
!!        IF(ISUBS(4,N)/=0)CALL WRTDES(IBUFSSG(ISUBS(5,N)),
!!     .                      IBUFSSG(ISUBS(5,N)),ISUBS(4,N),ITTYP,0)
        IF(SUBSET(N)%NPART/=0)CALL WRTDES(SUBSET(N)%PART,
     .                      SUBSET(N)%PART,SUBSET(N)%NPART,ITTYP,0)
        II=0
        DO I=IAD,IAD+NVAR-1
          II=II+1
          IWA(II)=ITHBUF(I)
        ENDDO
        IF(NVAR/=0)CALL WRTDES(IWA,IWA,NVAR,ITTYP,0)
        
      ENDDO
C-------TH GROUP------------
        DO N=1,NTHGRP2
        NVAR=ITHGRP(6,N)
        CALL FRETITL2(TITL,ITHGRP(NITHGR-LTITR+1,N),40)
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
C (nstrands elements are treated as a spring group)
        ITY=ITHGRP(2,N)
        IF (ITY==100) ITY=6
        IF(ITTYP==0)THEN
          IF(LTITL==40)THEN
            READ(TITL,'(10A4)')TIT40
            WRITE(IUNIT)ITHGRP(1,N),ITY,
     .               ITHGRP(3,N),ITHGRP(4,N),ITHGRP(6,N),TIT40
          ELSE IF(LTITL==80)THEN
            READ(TITL,'(20A4)')TIT80
            WRITE(IUNIT)ITHGRP(1,N),ITY,
     .               ITHGRP(3,N),ITHGRP(4,N),ITHGRP(6,N),TIT80
          ELSE
            READ(TITL,'(25A4)')TIT100
            WRITE(IUNIT)ITHGRP(1,N),ITY,
     .               ITHGRP(3,N),ITHGRP(4,N),ITHGRP(6,N),TIT100
          ENDIF
        ELSEIF(ITTYP==1)THEN
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,5,'I',LTITL,'C'
          WRITE(IUNIT,'(5I10,A)')ITHGRP(1,N),ITY,
     .               ITHGRP(3,N),ITHGRP(4,N),ITHGRP(6,N),TITL(1:LTITL)
        ELSEIF(ITTYP==3)THEN
         CALL EOR_C(20+LTITL)
         CALL WRITE_I_C(ITHGRP(1,N),1)
         CALL WRITE_I_C(ITY,1)
         CALL WRITE_I_C(ITHGRP(3,N),1)
         CALL WRITE_I_C(ITHGRP(4,N),1)
         CALL WRITE_I_C(ITHGRP(6,N),1)
         CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(20+LTITL)
        ENDIF
        IAD1=ITHGRP(5,N)+2*ITHGRP(4,N)
        IAD2=ITHGRP(8,N)
        DO J=1,ITHGRP(4,N)
        CALL FRETITL2(TITL,ITHBUF(IAD2),40)
        DO I=1,LTITL
          ITITLE(I)=ICHAR(TITL(I:I))
        ENDDO
          IF(ITTYP==0)THEN
            IF(LTITL==40)THEN
              READ(TITL,'(10A4)')TIT40
             WRITE(IUNIT)ITHBUF(IAD1),TIT40
            ELSE IF(LTITL==80)THEN
              READ(TITL,'(20A4)')TIT80
              WRITE(IUNIT)ITHBUF(IAD1),TIT80
            ELSE
              READ(TITL,'(25A4)')TIT100
              WRITE(IUNIT)ITHBUF(IAD1),TIT100
            ENDIF
          ELSEIF(ITTYP==1)THEN
          ELSEIF(ITTYP==2)THEN
            WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',LTITL,'C'
            WRITE(IUNIT,'(I10,A)')ITHBUF(IAD1),TITL(1:LTITL)
          ELSEIF(ITTYP==3)THEN
         CALL EOR_C(4+LTITL)
           CALL WRITE_I_C(ITHBUF(IAD1),1)
           CALL WRITE_C_C(ITITLE,LTITL)
         CALL EOR_C(4+LTITL)
          ENDIF
          IAD1=IAD1+1
          IAD2=IAD2+40
        ENDDO
        IF(NVAR/=0)THEN
          CALL WRTDES(ITHBUF(ITHGRP(7,N)),
     .                           ITHBUF(ITHGRP(7,N)),NVAR,ITTYP,0)
          IF(TH_TITLES == 1)THEN
            DO I=1,ITHGRP(4,N)
              DO J=1,NVAR
                DO K=1,10
                  VAR(K:K)=CHAR(ITHVAR((ITHGRP(9,N)-1+J-1)*10+K))
                ENDDO 
                WRITE(IFILTITL,'(I10)')ITHGRP(2,N)
                WRITE(IFILTITL,'(A)')VAR(1:10)
              ENDDO 
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C-------TH GROUP + 1 section fluide------------
      IF(NSECT==0.AND.NSFLSW/=0) THEN
        NVAR=6
        TITL='FLUID SECTION'
        IF(ITTYP==0)THEN
            IF(LTITL==40)THEN
              READ(TITL,'(10A4)')TIT40
             WRITE(IUNIT)104,104,
     .               1,NSFLSW,NVAR,TIT40
            ELSE IF(LTITL==80)THEN
              READ(TITL,'(20A4)')TIT80
              WRITE(IUNIT)104,104,
     .               1,NSFLSW,NVAR,TIT80
            ELSE
              READ(TITL,'(25A4)')TIT100
              WRITE(IUNIT)104,104,
     .               1,NSFLSW,NVAR,TIT100
            ENDIF
        ELSEIF(ITTYP==1)THEN
        ELSEIF(ITTYP==2)THEN
          WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,5,'I',LTITL,'C'
          WRITE(IUNIT,'(5I10,A)')104,104,
     .               1,NSFLSW,ITHGRP(6,N),TITL(1:LTITL)
        ELSEIF(ITTYP==3)THEN
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
        ENDIF
        DO J=1,NSFLSW
          IF(ITTYP==0)THEN
            IF(LTITL==40)THEN
              WRITE(IUNIT)J,TIT40
            ELSE IF(LTITL==80)THEN
              WRITE(IUNIT)J,TIT80
            ELSE
              WRITE(IUNIT)J,TIT100
            ENDIF
          ELSEIF(ITTYP==1)THEN
          ELSEIF(ITTYP==2)THEN
            WRITE(IUNIT,'(A,I5,A,I5,A)')EOR,1,'I',LTITL,'C'
            WRITE(IUNIT,'(I10,A)')J,TITL(1:LTITL)
          ELSEIF(ITTYP==3)THEN
           CALL EOR_C(4+LTITL)
           CALL WRITE_I_C(J,1)
           CALL WRITE_C_C(ITITLE,LTITL)
           CALL EOR_C(4+LTITL)
          ENDIF
        ENDDO
        DO I=1,6
            IWA(I)=I
        ENDDO
        CALL WRTDES(IWA,IWA,6,ITTYP,0)      
      ENDIF
C
      IF ((IRAD2R==1).AND.(R2R_SIU==1)) THEN      
        CALL FLU_FIL_C()
        IF (IDDOM==0) THEN
          SEEK_LOC = IUNIT-29
          IF (IUNIT == 3) SEEK_LOC = 1
          SEEK_FLAG(SEEK_LOC) = 1
        ENDIF  
      ENDIF  
C
      IF(TH_TITLES == 1) CLOSE(IFILTITL)
C
      RETURN
      END
