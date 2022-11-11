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
Cbm|====================================================================
Cbm|  CHK_DT_ENGINE                       src/rad2r/routines_r2r.F      
Cbm|-- called by -----------
Cbm|        R2R_GROUP                       src/rad2r/r2r_group.F          
Cbm|-- calls ---------------
Cbm|====================================================================
Chd|====================================================================
Chd|  R2R_SPEEDUP                   source/coupling/rad2rad/r2r_speedup.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FIND_DT_ENGINE                source/coupling/rad2rad/r2r_speedup.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE R2R_SPEEDUP(DTELEM,DTNODA,DT_R2R,COST_R2R,ISOLOFF,
     .                       ISHEOFF,ITRUOFF ,IPOUOFF ,IRESOFF ,ITRIOFF,
     .                       IQUAOFF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "sphcom.inc"
#include      "warn_c.inc"
#include      "rnur_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISOLOFF(*),ISHEOFF(*),ITRUOFF(*),IPOUOFF(*),IRESOFF(*),ITRIOFF(*),
     .   IQUAOFF(*)
      my_real
     .   DTELEM(*),DTNODA,DT_R2R(4,*),COST_R2R
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,FLG_CHK,FLG_CTL,DOMLEN,NUM,FLG_CHK_SUB,FLG_CTL_SUB
      my_real
     .   DT,DT_SUB,SPEEDUP,COST_SUB
      CHARACTER NAM*150
C-----------------------------------------------
C      
      CALL FIND_DT_ENGINE(DTELEM,DTNODA,DT,FLG_CHK,FLG_CTL,
     .                   ISOLOFF,ISHEOFF,ITRUOFF ,IPOUOFF ,IRESOFF ,
     .                   ITRIOFF,IQUAOFF)
C
      DO I=1,NUMSPH-NSPHRES
        COST_R2R = COST_R2R + 12.0
      END DO
C
      IF (IDDOM/=0) THEN
C
        DT_R2R(1,1) = DT
        DT_R2R(2,1) = COST_R2R
        IF (FLG_CHK>0) DT_R2R(3,1) = ONE
        IF (FLG_CTL>0) DT_R2R(4,1) = ONE
C
      ELSE
C
        WRITE(IOUT,1000)
        WRITE(IOUT,1001)
C
        IF (FLG_CHK==0) THEN 
          CALL ANCMSG(MSGID=1095,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ROOTNAM(1:ROOTLEN)//'_0001.rad')
        ENDIF
C
        DT_SUB = DT_R2R(1,1)
        COST_SUB = DT_R2R(2,1)
C
        IF (FLG_SWALE==1) THEN
          DT_SUB = DT
          DT = DT_R2R(1,1)
          COST_SUB = COST_R2R
          COST_R2R = DT_R2R(2,1)
        ENDIF
C
        FLG_CHK_SUB = 0
        FLG_CTL_SUB = 0
        IF (DT_R2R(3,1)>EM20) FLG_CHK_SUB = 1
        IF (DT_R2R(4,1)>EM20) FLG_CTL_SUB = 1
        NAM=DOM_NAME(ISUBDOM(8,1):ISUBDOM(8,1)+
     .               ISUBDOM(7,1)-1)
        DOMLEN = ISUBDOM(7,1)
C
        IF (FLG_CHK_SUB==0) THEN 
          CALL ANCMSG(MSGID=1095,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=NAM(1:DOMLEN)//'_0001.rad')
        ENDIF
C
        SPEEDUP = 0.9*((COST_R2R+COST_SUB)/((COST_R2R*DT_SUB/DT)+COST_SUB))
C
        WRITE(IOUT,2000) DT
        WRITE(IOUT,2001) NAM(1:DOMLEN),DT_SUB
        WRITE(IOUT,2002) SPEEDUP
C
      ENDIF

C--------------------------------------------------------
 1000 FORMAT(//,'         MULTIDOMAINS SPEEDUP ESTIMATION')
 1001 FORMAT(  '         ------------------------')
 2000 FORMAT(/,'     ESTIMATED TIME STEP FOR FULLDOMAIN',3X,1PG20.13)
 2001 FORMAT('     ESTIMATED TIME STEP FOR SUBDOMAIN',1X,A,3X,1PG20.13)
 2002 FORMAT(/,'     ESTIMATED THEORETICAL SPEEDUP',2X,F10.1,//)
C--------------------------------------------------------
 
      RETURN
      END

Cbm|====================================================================
Cbm|  CHK_DT_ENGINE                       src/rad2r/routines_r2r.F      
Cbm|-- called by -----------
Cbm|        R2R_GROUP                       src/rad2r/r2r_group.F          
Cbm|-- calls ---------------
Cbm|====================================================================
Chd|====================================================================
Chd|  FIND_DT_ENGINE                source/coupling/rad2rad/r2r_speedup.F
Chd|-- called by -----------
Chd|        R2R_SPEEDUP                   source/coupling/rad2rad/r2r_speedup.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE FIND_DT_ENGINE(DTELEM,DTNODA,DT,FLG_CHK,FLG_CTL,
     .                         ISOLOFF,ISHEOFF,ITRUOFF ,IPOUOFF ,IRESOFF ,
     .                         ITRIOFF,IQUAOFF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE INOUTFILE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "sphcom.inc"
#include      "rnur_c.inc"
#include      "r2r_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FLG_CHK,FLG_CTL,ISOLOFF(*),ISHEOFF(*),ITRUOFF(*),
     .   IPOUOFF(*),IRESOFF(*),ITRIOFF(*),IQUAOFF(*)
      my_real
     .   DT,DTELEM(*),DTNODA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IO_ERR1,I,DT_INDEX(11),FLAG_TRI(11),NUME(11),NUMEL
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80
      my_real
     .   DTSCA,DTMINI,DTINI,DTMAX,DT_MIN_ELEM(11),DTFAC1(11)
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
      INTEGER :: IERROR
      INTEGER, DIMENSION(:), ALLOCATABLE :: PERM
C-----------------------------------------------
C
      FLG_CHK = 0
      FLG_CTL = 0
      DTMAX = EP30
      DT_INDEX(:) = 0
      DT_MIN_ELEM(:) = EP30 
      FLAG_TRI(:) = 0
      DTFAC1(:) = DTFAC
      NUMEL= NUMELC+NUMELS+NUMELT+NUMELQ+NUMELP+NUMELR+NUMELUR+NUMELTG
     .      +NUMELX+NUMSPH+NUMELIG3D
      MY_ALLOCATE( PERM,NUMEL )
  
C-----------------------------------------------
C     Computation of index in dtelem for each type of elements
C-----------------------------------------------
      NUME(1) = NUMELS
      NUME(2) = NUMELQ
      NUME(3) = NUMELC
      NUME(4) = NUMELT
      NUME(5) = NUMELP
      NUME(6) = NUMELR
      NUME(7) = NUMELTG
      NUME(8) = NUMELUR
      NUME(9) = NUMELX
      NUME(10) = NUMSPH
      NUME(11) = NUMELIG3D           
      DT_INDEX(1)  = 0
      DO I=2,11
        DT_INDEX(I)  = DT_INDEX(I-1)  + NUME(I-1)
      END DO

C-----------------------------------------------
C     Off elements must be taken into account and sorting if necessart
C-----------------------------------------------  
      DO I = 1, NUMELS
        IF (ISOLOFF(I)/=0) THEN
          DTELEM(I+DT_INDEX(1)) = EP30
          FLAG_TRI(1) = 1
        ENDIF
      END DO
C
      DO I = 1, NUMELQ
        IF (IQUAOFF(I)/=0) THEN
          DTELEM(I+DT_INDEX(2)) = EP30
          FLAG_TRI(2) = 1
        ENDIF
      END DO
C
      DO I = 1, NUMELC
        IF (ISHEOFF(I)/=0) THEN
          DTELEM(I+DT_INDEX(3)) = EP30
          FLAG_TRI(3) = 1
        END IF
      END DO
C
      DO I = 1, NUMELT
        IF (ITRUOFF(I)/=0) THEN
          DTELEM(I+DT_INDEX(4)) = EP30
          FLAG_TRI(4) = 1
        END IF
      END DO
C
      DO I = 1, NUMELP
        IF (IPOUOFF(I)/=0) THEN
          DTELEM(I+DT_INDEX(5)) = EP30
          FLAG_TRI(5) = 1
        END IF
      END DO
C
      DO I = 1, NUMELR
        IF (IRESOFF(I)/=0) THEN
          DTELEM(I+DT_INDEX(6)) = EP30
          FLAG_TRI(6) = 1
        END IF
      END DO
C
      DO I = 1, NUMELTG
        IF (ITRIOFF(I)/=0) THEN
          DTELEM(I+DT_INDEX(7)) = EP30
          FLAG_TRI(7) = 1
        END IF
      END DO
C
      DO I=1,11
        IF (FLAG_TRI(I)==1) THEN
            CALL MYQSORT(NUME(I),DTELEM(1+DT_INDEX(I)),PERM(1+DT_INDEX(I)),IERROR)
            DTELEM(NUMEL+1+DT_INDEX(I):NUMEL+NUME(I)+1+DT_INDEX(I)) = PERM(1+DT_INDEX(I):NUME(I)+1+DT_INDEX(I))
        ENDIF
      END DO

C-----------------------------------------------
C     Computation of min time step for each type of element 
C----------------------------------------------- 
      DO I=1,11
        DT_MIN_ELEM(I) = DTELEM(1+DT_INDEX(I))
      END DO

C-----------------------------------------------
C     Prereading of data in engine input file
C-----------------------------------------------   
      FILNAM=ROOTNAM(1:ROOTLEN)//'_0001.rad'
      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))    
      LEN_TMP_NAME = INFILE_NAME_LEN+LEN_TRIM(FILNAM)
      OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
C
      IF (IO_ERR1/=0) THEN
         FILNAM=ROOTNAM(1:ROOTLEN)//'D01'
         TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))    
         LEN_TMP_NAME = INFILE_NAME_LEN+LEN_TRIM(FILNAM)
         OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
      ENDIF      

      IF (IO_ERR1==0) THEN
C
        FLG_CHK = 1
 10     READ(71,'(A)',END=20) KEYA
C--
        IF (KEYA(1:3)=='/DT') THEN
          READ(71,'(A)',END=20) KEYA2 
          IF(KEYA2(1:1)/='$'.AND.KEYA2(1:1)/='#') THEN
            BACKSPACE(71)
          ENDIF
        ENDIF
C
C--     nodal time step--
        IF(KEYA(1:8)=='/DT/NODA') THEN
          READ(71,*,END=20) DTSCA,DTMINI 
          DTNODA = DTSCA*DTNODA
          IF(KEYA(1:12)=='/DT/NODA/CST') THEN
            FLG_CTL = 1 
            DTNODA = MAX(DTNODA,DTMINI)
          ENDIF
C--     solid element time step --
        ELSEIF(KEYA(1:9)=='/DT/BRICK') THEN
          READ(71,*,END=20) DTSCA,DTMINI
          DTFAC1(1) = DTSCA 
          IF (KEYA(10:13)=='/DEL') THEN
            DO I = 1,NUMELS
              IF (DTSCA*DTELEM(DT_INDEX(1)+I)>DTMINI) THEN
                DT_MIN_ELEM(1) = DTELEM(DT_INDEX(1)+I)
                EXIT
              ENDIF
            END DO
          ENDIF
C--     quad element time step --
        ELSEIF(KEYA(1:9)=='/DT/QUAD') THEN
          READ(71,*,END=20) DTSCA,DTMINI
          DTFAC1(2) = DTSCA 
          IF (KEYA(10:13)=='/DEL') THEN
            DO I = 1,NUMELQ
              IF (DTSCA*DTELEM(DT_INDEX(2)+I)>DTMINI) THEN
                DT_MIN_ELEM(2) = DTELEM(DT_INDEX(2)+I)
                EXIT
              ENDIF
            END DO
          ENDIF
C--     shell element time step --
        ELSEIF(KEYA(1:9)=='/DT/SHELL') THEN
          READ(71,*,END=20) DTSCA,DTMINI
          DTFAC1(3) = DTSCA
          IF ((KEYA(10:14)/='/STOP').AND.(KEYA(10:13)/='/CST')) THEN
            DO I = 1,NUMELC
              IF (DTSCA*DTELEM(DT_INDEX(3)+I)>DTMINI) THEN
                DT_MIN_ELEM(3) = DTELEM(DT_INDEX(3)+I)
                EXIT
              ENDIF
            END DO
          ENDIF
C--     beam element time step --
        ELSEIF (KEYA(1:8)=='/DT/BEAM') THEN
          READ(71,*,END=20) DTSCA,DTMINI
          DTFAC1(4) = DTSCA
          IF (KEYA(9:12)=='/DEL') THEN
            DO I = 1,NUMELT
              IF (DTSCA*DTELEM(DT_INDEX(4)+I)>DTMINI) THEN
                DT_MIN_ELEM(4) = DTELEM(DT_INDEX(4)+I)
                EXIT
              ENDIF
            END DO
          ENDIF
C--     truss element time step --
        ELSEIF (KEYA(1:9)=='/DT/TRUSS') THEN
          READ(71,*,END=20) DTSCA,DTMINI
          DTFAC1(5) = DTSCA 
          IF (KEYA(10:13)=='/DEL') THEN
            DO I = 1,NUMELP
              IF (DTSCA*DTELEM(DT_INDEX(5)+I)>DTMINI) THEN
                DT_MIN_ELEM(5) = DTELEM(DT_INDEX(5)+I)
                EXIT
              ENDIF
            END DO
          ENDIF 
C--     spring element time step --
        ELSEIF (KEYA(1:10)=='/DT/SPRING') THEN
          READ(71,*,END=20) DTSCA,DTMINI
          DTFAC1(6) = DTSCA 
          IF (KEYA(11:14)=='/DEL') THEN
            DO I = 1,NUMELR
              IF (DTSCA*DTELEM(DT_INDEX(6)+I)>DTMINI) THEN
                DT_MIN_ELEM(6) = DTELEM(DT_INDEX(6)+I)
                EXIT
              ENDIF
            END DO
          ENDIF
C--     sh3n element time step --
        ELSEIF(KEYA(1:9)=='/DT/SH_3N') THEN
          READ(71,*,END=20) DTSCA,DTMINI
          DTFAC1(7) = DTSCA
          IF ((KEYA(10:14)/='/STOP').AND.(KEYA(10:13)/='/CST')) THEN
            DO I = 1,NUMELTG
              IF (DTSCA*DTELEM(DT_INDEX(7)+I)>DTMINI) THEN
                DT_MIN_ELEM(7) = DTELEM(DT_INDEX(7)+I)
                EXIT
              ENDIF
            END DO
          ENDIF
C--     sph particle time step --
        ELSEIF (KEYA(1:10)=='/DT/SPHCEL') THEN
          READ(71,*,END=20) DTSCA,DTMINI
          DTFAC1(10) = DTSCA 
          IF (KEYA(11:14)=='/DEL') THEN
            DO I = 1,NUMSPH
              IF (DTSCA*DTELEM(DT_INDEX(10)+I)>DTMINI) THEN
                DT_MIN_ELEM(10) = DTELEM(DT_INDEX(10)+I)
                EXIT
              ENDIF
            END DO
          ENDIF       
C--     default time step --
        ELSEIF(KEYA(1:4)=='/DT ') THEN
          FLG_CTL = 1
          READ(71,*,END=20) DTSCA,DTMINI
          DO I=1,11
            DTFAC1(I) = DTSCA
          END DO
C--     max time step--
        ELSEIF(KEYA(1:5)=='/DTIX') THEN
          FLG_CTL = 1
          READ(71,*,END=20) DTINI,DTMAX      
        ENDIF
C
        GOTO 10
C
 20     CONTINUE
        CLOSE(71)
C
      ENDIF

C-----------------------------------------------
C     Prediction of final time step
C----------------------------------------------- 
      DT = EP30
      DO I=1,11
        DT = MIN(DT,DTFAC1(I)*DT_MIN_ELEM(I))
      END DO
C
      IF (DTNODA>ZERO) DT = DTNODA
C
      DT = MIN(DT,DTMAX)

      DEALLOCATE( PERM )
                   
C------------------------------------------- 
      RETURN
      END
