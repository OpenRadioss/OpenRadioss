Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    volprep            ../engine/source/airbag/volpresp.F
!||--- called by ------------------------------------------------------
!||    monvol0            ../engine/source/airbag/monvol0.F
!||--- uses       -----------------------------------------------------
!||    finter_mixed_mod   ../engine/source/tools/finter_mixed.F90
!||    h3d_mod            ../engine/share/modules/h3d_mod.F
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||    sensor_mod         ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE VOLPREP(IVOLU  ,RVOLU   ,NJET    ,IBAGJET ,RBAGJET   ,
     2                   NSENSOR,SENSOR_TAB,X       ,V       ,A         ,
     3                   NORMAL ,NPC     ,TF      ,NN      ,SURF_NODES,
     4                   IADMV  ,FSKY    ,FSKYV   ,FEXT    ,H3D_DATA  ,
     5                   SURF_ELTYP,SURF_ELEM,NODE_NUMBER,TOTAL_CONTRIBUTION_NUMBER, 
     6                   CONTRIBUTION_INDEX,CONTRIBUTION_NUMBER,
     7                   NODE_ID,CONTRIBUTION,WFEXT,PYTHON)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE python_funct_mod
      USE finter_mixed_mod
      USE H3D_MOD 
      USE SENSOR_MOD
C-----------------------------------------------
C     AIRBAGS INPUT FORMAT 4.4
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C  o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR !< number of sensor
      INTEGER ,INTENT(IN) :: NODE_NUMBER !< number of node of the airbag
      INTEGER ,INTENT(IN) :: TOTAL_CONTRIBUTION_NUMBER !< total number of contribution
      INTEGER IVOLU(*),NJET,IBAGJET(NIBJET,*),
     .        NPC(*),NN,IADMV(4,*),SURF_NODES(NN,4),SURF_ELTYP(NN),
     .        SURF_ELEM(NN)
      INTEGER, DIMENSION(NN,4), INTENT(in) :: CONTRIBUTION_INDEX !< index to the array CONTRIBUTION
      INTEGER, DIMENSION(NODE_NUMBER+1), INTENT(in) :: CONTRIBUTION_NUMBER !< number of contribution per node
      INTEGER, DIMENSION(NODE_NUMBER), INTENT(in) :: NODE_ID !< node id
      my_real, DIMENSION(TOTAL_CONTRIBUTION_NUMBER,3), INTENT(inout) :: CONTRIBUTION !< contribution array, the force are saved here
      my_real RVOLU(*),RBAGJET(NRBJET,*),X(3,*), V(3,*), A(3,*), NORMAL(3,NN),TF(*),FSKY(8,LSKY),FSKYV(LSKY,8),FEXT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
      TYPE(PYTHON_), intent(inout) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,NJ1,NJ2,NJ3,IPT,IPA,IPZ,N1,N2,N3,N4,IJ,ISENS,K,ITYP
      INTEGER :: ADDRESS,NOD,LOCAL_CONTRIBUTION_NUMBER
      my_real :: PRES,FX,FY,FZ,P,PEXT,DP,Q,PJ,PJ0,XX,YY,ZZ,
     .           THETA,NX1,NX2,NY1,NY2,NZ1,NZ2,AN1,AN,
     .           XM,YM,ZM,NX3,NY3,NZ3,
     .           PS1,PS2,PS3,AN2,AN3,DET,MU,FACR,
     .           API,DIST,TSTART,TS,TMPO,FPT,FPA,FPZ,SCALT,SCALA,SCALD
      my_real, DIMENSION(3) :: TMP
C-----------------------------------------------
      API=HUNDRED80/PI
C-----------------------------------------------
!$OMP PARALLEL PRIVATE(N1,N2,N3,N4,P,II,FX,FY,FZ,ADDRESS,NOD,LOCAL_CONTRIBUTION_NUMBER,TMP)
      PRES   =RVOLU(12)
      Q      =RVOLU(23)
      PEXT   =RVOLU(3)
      DP     =Q+PRES-PEXT
      SCALT  =RVOLU(26)   
      SCALA  =RVOLU(29)   
      SCALD  =RVOLU(30)   
!$OMP DO SCHEDULE(guided)
      DO I=1,TOTAL_CONTRIBUTION_NUMBER
        CONTRIBUTION(I,1) = ZERO
        CONTRIBUTION(I,2) = ZERO
        CONTRIBUTION(I,3) = ZERO
      ENDDO
!$OMP END DO

!$OMP DO SCHEDULE(guided)
      DO I=1,NN
C
        N1 = SURF_NODES(I,1)
        N2 = SURF_NODES(I,2)
        N3 = SURF_NODES(I,3)
        IF(SURF_ELTYP(I) /= 7) N4 = SURF_NODES(I,4)
C
        IF(SURF_ELTYP(I) == 3)THEN
          P=DP*FOURTH
          II=SURF_ELEM(I)
C
          FX=P*NORMAL(1,I)
          FY=P*NORMAL(2,I)
          FZ=P*NORMAL(3,I)
C
          FSKY(1,IADMV(1,I))=FX
          FSKY(2,IADMV(1,I))=FY
          FSKY(3,IADMV(1,I))=FZ
C
          FSKY(1,IADMV(2,I))=FX
          FSKY(2,IADMV(2,I))=FY
          FSKY(3,IADMV(2,I))=FZ
C
          FSKY(1,IADMV(3,I))=FX
          FSKY(2,IADMV(3,I))=FY
          FSKY(3,IADMV(3,I))=FZ
C
          FSKY(1,IADMV(4,I))=FX
          FSKY(2,IADMV(4,I))=FY
          FSKY(3,IADMV(4,I))=FZ
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            ADDRESS = CONTRIBUTION_INDEX(I,1)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
            ADDRESS = CONTRIBUTION_INDEX(I,2)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
            ADDRESS = CONTRIBUTION_INDEX(I,3)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
            ADDRESS = CONTRIBUTION_INDEX(I,4)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
          ENDIF
        ELSEIF(SURF_ELTYP(I)==7)THEN
          P=DP*THIRD
          II=SURF_ELEM(I) + NUMELC
C
          FX=P*NORMAL(1,I)
          FY=P*NORMAL(2,I)
          FZ=P*NORMAL(3,I)
C
          FSKY(1,IADMV(1,I))=FX
          FSKY(2,IADMV(1,I))=FY
          FSKY(3,IADMV(1,I))=FZ
C
          FSKY(1,IADMV(2,I))=FX
          FSKY(2,IADMV(2,I))=FY
          FSKY(3,IADMV(2,I))=FZ
C
          FSKY(1,IADMV(3,I))=FX
          FSKY(2,IADMV(3,I))=FY
          FSKY(3,IADMV(3,I))=FZ
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            ADDRESS = CONTRIBUTION_INDEX(I,1)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
            ADDRESS = CONTRIBUTION_INDEX(I,2)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
            ADDRESS = CONTRIBUTION_INDEX(I,3)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
          ENDIF
        ELSE
          P=DP*FOURTH
          II=I+NUMELC+NUMELTG
C
          FX=P*NORMAL(1,I)
          FY=P*NORMAL(2,I)
          FZ=P*NORMAL(3,I)
C
          FSKY(1,IADMV(1,I))=FX
          FSKY(2,IADMV(1,I))=FY
          FSKY(3,IADMV(1,I))=FZ
C
          FSKY(1,IADMV(2,I))=FX
          FSKY(2,IADMV(2,I))=FY
          FSKY(3,IADMV(2,I))=FZ
C
          FSKY(1,IADMV(3,I))=FX
          FSKY(2,IADMV(3,I))=FY
          FSKY(3,IADMV(3,I))=FZ
C
          FSKY(1,IADMV(4,I))=FX
          FSKY(2,IADMV(4,I))=FY
          FSKY(3,IADMV(4,I))=FZ
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            ADDRESS = CONTRIBUTION_INDEX(I,1)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
            ADDRESS = CONTRIBUTION_INDEX(I,2)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
            ADDRESS = CONTRIBUTION_INDEX(I,3)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
            ADDRESS = CONTRIBUTION_INDEX(I,4)
            CONTRIBUTION(ADDRESS,1) = FX
            CONTRIBUTION(ADDRESS,2) = FY
            CONTRIBUTION(ADDRESS,3) = FZ
          ENDIF
        ENDIF
      ENDDO

!$OMP END DO

      IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
!$OMP DO SCHEDULE(guided)
        DO I=1,NODE_NUMBER
          NOD = NODE_ID(I)
          ADDRESS = CONTRIBUTION_NUMBER(I)
          LOCAL_CONTRIBUTION_NUMBER = CONTRIBUTION_NUMBER(I+1) - CONTRIBUTION_NUMBER(I)
          TMP(1) = ZERO
          TMP(2) = ZERO
          TMP(3) = ZERO
          DO J=1,LOCAL_CONTRIBUTION_NUMBER
            TMP(1) = TMP(1) + CONTRIBUTION(ADDRESS+J,1)
            TMP(2) = TMP(2) + CONTRIBUTION(ADDRESS+J,2)
            TMP(3) = TMP(3) + CONTRIBUTION(ADDRESS+J,3)
          ENDDO
          FEXT(1,NOD) = FEXT(1,NOD)+ TMP(1)
          FEXT(2,NOD) = FEXT(2,NOD)+ TMP(2)
          FEXT(3,NOD) = FEXT(3,NOD)+ TMP(3)
        ENDDO
!$OMP END DO
      ENDIF

!$OMP END PARALLEL
C
      ITYP=IVOLU(2)
      IF(ITYP/=4.AND.ITYP/=5.AND.ITYP/=7.AND.ITYP/=9)THEN
        RETURN
      ENDIF
C-------------------------------------------
C     INJECTEURS
C-------------------------------------------
      DO IJ =1,NJET
        NJ1  = IBAGJET( 5,IJ)
        NJ2  = IBAGJET( 6,IJ)
        NJ3  = IBAGJET( 7,IJ)
        IPT  = IBAGJET( 8,IJ)
        IPA  = IBAGJET( 9,IJ)
        IPZ  = IBAGJET(10,IJ)
        FPT  = RBAGJET(12,IJ)
        FPA  = RBAGJET(13,IJ)
        FPZ  = RBAGJET(14,IJ)
 
        ISENS=IBAGJET(4,IJ)
        IF(ISENS==0)THEN
          TSTART=ZERO
        ELSE
          TSTART=SENSOR_TAB(ISENS)%TSTART
        ENDIF

        IF(NJ1/=0.AND.TT>=TSTART)THEN
          TS=TT-TSTART
          PJ0=FPT*FINTER_MIXED(PYTHON,NFUNCT,IPT,TS*SCALT,NPC,TF)
!$OMP DO SCHEDULE(guided)
          DO I=1,TOTAL_CONTRIBUTION_NUMBER
            CONTRIBUTION(I,1) = ZERO
            CONTRIBUTION(I,2) = ZERO
            CONTRIBUTION(I,3) = ZERO
          ENDDO
!$OMP END DO

!$OMP PARALLEL PRIVATE(N1,N2,N3,N4,PJ,II,XX,YY,ZZ,XM,YM,ZM,NX1,NY1,NZ1,NX2,NY2,NZ2,NX3,NY3,NZ3,
!$OMP+ PS1,PS2,PS3,AN2,AN3,DET,MU,FACR,AN1,AN,TMPO,THETA,DIST,FX,FY,FZ,ADDRESS,
!$OMP+ NOD,LOCAL_CONTRIBUTION_NUMBER,TMP)

!$OMP DO SCHEDULE(guided) REDUCTION(+:WFEXT)
          DO I=1,NN

            IF(SURF_ELTYP(I)==3)THEN
              PJ=FOURTH*PJ0

              N1 = SURF_NODES(I,1)
              N2 = SURF_NODES(I,2)
              N3 = SURF_NODES(I,3)
              N4 = SURF_NODES(I,4)

              II=SURF_ELEM(I)

              XX = FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
              YY = FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
              ZZ = FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))

              XM=HALF*(X(1,NJ1)+X(1,NJ3))
              YM=HALF*(X(2,NJ1)+X(2,NJ3))
              ZM=HALF*(X(3,NJ1)+X(3,NJ3))

              NX1 = XX-XM
              NY1 = YY-YM
              NZ1 = ZZ-ZM

              ! decomposition de (M,P) sur (M,N2) et (M,N3)
              NX2 = X(1,NJ2)-XM
              NY2 = X(2,NJ2)-YM
              NZ2 = X(3,NJ2)-ZM

              NX3 = X(1,NJ3)-XM
              NY3 = X(2,NJ3)-YM
              NZ3 = X(3,NJ3)-ZM

              PS1 = NX1*NX2+NY1*NY2+NZ1*NZ2
              PS2 = NX2*NX3+NY2*NY3+NZ2*NZ3
              PS3 = NX1*NX3+NY1*NY3+NZ1*NZ3
              AN2 = NX2*NX2+NY2*NY2+NZ2*NZ2
              AN3 = NX3*NX3+NY3*NY3+NZ3*NZ3
              DET = PS2*PS2-AN2*AN3

              MU     = (PS2*PS1-PS3*AN2)/SIGN(MAX(EM30,ABS(DET)),DET)

              FACR =MIN(ONE,MAX(-ONE,MU))
              NX1=NX1-FACR*NX3
              NY1=NY1-FACR*NY3
              NZ1=NZ1-FACR*NZ3

              AN1 = (NX1**2+NY1**2+NZ1**2)
              AN = MAX(EM30,SQRT((NORMAL(1,I)**2+NORMAL(2,I)**2+NORMAL(3,I)**2)*AN1))
              PJ=PJ*MAX(ZERO,(NX1*NORMAL(1,I)+NY1*NORMAL(2,I)+NZ1*NORMAL(3,I))/AN)
              AN = MAX(EM30,SQRT((NX2**2+NY2**2+NZ2**2)*AN1))
              TMPO = (NX1*NX2+NY1*NY2+NZ1*NZ2)/AN
              TMPO = SIGN(MIN(ONE,ABS(TMPO)),TMPO)
              THETA=API*ACOS(TMPO)
              PJ=PJ*FPA*FINTER_MIXED(PYTHON,NFUNCT,IPA,THETA*SCALA,NPC,TF)
              DIST = SQRT(AN1)
              IF(IPZ/=0)PJ=PJ*FPZ*FINTER_MIXED(PYTHON,NFUNCT,IPZ,DIST*SCALD,NPC,TF)

              FX=PJ*NORMAL(1,I)
              FY=PJ*NORMAL(2,I)
              FZ=PJ*NORMAL(3,I)

              FSKY(1,IADMV(1,I))=FSKY(1,IADMV(1,I))+FX
              FSKY(2,IADMV(1,I))=FSKY(2,IADMV(1,I))+FY
              FSKY(3,IADMV(1,I))=FSKY(3,IADMV(1,I))+FZ

              FSKY(1,IADMV(2,I))=FSKY(1,IADMV(2,I))+FX
              FSKY(2,IADMV(2,I))=FSKY(2,IADMV(2,I))+FY
              FSKY(3,IADMV(2,I))=FSKY(3,IADMV(2,I))+FZ

              FSKY(1,IADMV(3,I))=FSKY(1,IADMV(3,I))+FX
              FSKY(2,IADMV(3,I))=FSKY(2,IADMV(3,I))+FY
              FSKY(3,IADMV(3,I))=FSKY(3,IADMV(3,I))+FZ

              FSKY(1,IADMV(4,I))=FSKY(1,IADMV(4,I))+FX
              FSKY(2,IADMV(4,I))=FSKY(2,IADMV(4,I))+FY
              FSKY(3,IADMV(4,I))=FSKY(3,IADMV(4,I))+FZ

              IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .           ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
                ADDRESS = CONTRIBUTION_INDEX(I,1)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
                ADDRESS = CONTRIBUTION_INDEX(I,2)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
                ADDRESS = CONTRIBUTION_INDEX(I,3)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
                ADDRESS = CONTRIBUTION_INDEX(I,4)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
              ENDIF

              WFEXT=WFEXT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                  +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                  +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
            ELSEIF(SURF_ELTYP(I)==7)THEN
              PJ=PJ0*THIRD

              N1 = SURF_NODES(I,1)
              N2 = SURF_NODES(I,2)
              N3 = SURF_NODES(I,3)

              II=SURF_ELEM(I) + NUMELC

              XX = (X(1,N1)+X(1,N2)+X(1,N3))*THIRD
              YY = (X(2,N1)+X(2,N2)+X(2,N3))*THIRD
              ZZ = (X(3,N1)+X(3,N2)+X(3,N3))*THIRD

              XM=HALF*(X(1,NJ1)+X(1,NJ3))
              YM=HALF*(X(2,NJ1)+X(2,NJ3))
              ZM=HALF*(X(3,NJ1)+X(3,NJ3))

              NX1 = XX-XM
              NY1 = YY-YM
              NZ1 = ZZ-ZM

              ! decomposition de (M,P) sur (M,N2) et (M,N3)
              NX2 = X(1,NJ2)-XM
              NY2 = X(2,NJ2)-YM
              NZ2 = X(3,NJ2)-ZM

              NX3 = X(1,NJ3)-XM
              NY3 = X(2,NJ3)-YM
              NZ3 = X(3,NJ3)-ZM

              PS1 = NX1*NX2+NY1*NY2+NZ1*NZ2
              PS2 = NX2*NX3+NY2*NY3+NZ2*NZ3
              PS3 = NX1*NX3+NY1*NY3+NZ1*NZ3
              AN2 = NX2*NX2+NY2*NY2+NZ2*NZ2
              AN3 = NX3*NX3+NY3*NY3+NZ3*NZ3
              DET = PS2*PS2-AN2*AN3

              MU     = (PS2*PS1-PS3*AN2)/SIGN(MAX(EM30,ABS(DET)),DET)

              FACR =MIN(ONE,MAX(-ONE,MU))
              NX1=NX1-FACR*NX3
              NY1=NY1-FACR*NY3
              NZ1=NZ1-FACR*NZ3

              AN1 = (NX1**2+NY1**2+NZ1**2)
              AN = MAX(EM30,SQRT((NORMAL(1,I)**2+NORMAL(2,I)**2+NORMAL(3,I)**2)*AN1))
              PJ=PJ*MAX(ZERO,(NX1*NORMAL(1,I)+NY1*NORMAL(2,I)+NZ1*NORMAL(3,I))/AN)
              AN = MAX(EM30,SQRT((NX2**2+NY2**2+NZ2**2)*AN1))
              TMPO = (NX1*NX2+NY1*NY2+NZ1*NZ2)/AN
              TMPO = SIGN(MIN(ONE,ABS(TMPO)),TMPO)
              THETA=API*ACOS(TMPO)
              PJ=PJ*FPA*FINTER_MIXED(PYTHON,NFUNCT,IPA,THETA*SCALA,NPC,TF)
              DIST = SQRT(AN1)
              IF(IPZ/=0)PJ=PJ*FPZ*FINTER_MIXED(PYTHON,NFUNCT,IPZ,DIST*SCALD,NPC,TF)

              FX=PJ*NORMAL(1,I)
              FY=PJ*NORMAL(2,I)
              FZ=PJ*NORMAL(3,I)

              FSKY(1,IADMV(1,I))=FSKY(1,IADMV(1,I))+FX
              FSKY(2,IADMV(1,I))=FSKY(2,IADMV(1,I))+FY
              FSKY(3,IADMV(1,I))=FSKY(3,IADMV(1,I))+FZ

              FSKY(1,IADMV(2,I))=FSKY(1,IADMV(2,I))+FX
              FSKY(2,IADMV(2,I))=FSKY(2,IADMV(2,I))+FY
              FSKY(3,IADMV(2,I))=FSKY(3,IADMV(2,I))+FZ

              FSKY(1,IADMV(3,I))=FSKY(1,IADMV(3,I))+FX
              FSKY(2,IADMV(3,I))=FSKY(2,IADMV(3,I))+FY
              FSKY(3,IADMV(3,I))=FSKY(3,IADMV(3,I))+FZ

              IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .           ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
                ADDRESS = CONTRIBUTION_INDEX(I,1)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
                ADDRESS = CONTRIBUTION_INDEX(I,2)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
                ADDRESS = CONTRIBUTION_INDEX(I,3)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
              ENDIF

              WFEXT=WFEXT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3))
     +                  +FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                  +FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
            ELSE
              PJ=FOURTH*PJ0

              N1 = SURF_NODES(I,1)
              N2 = SURF_NODES(I,2)
              N3 = SURF_NODES(I,3)
              N4 = SURF_NODES(I,4)

              II=I+NUMELC+NUMELTG

              XX = FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
              YY = FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
              ZZ = FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))

              XM=HALF*(X(1,NJ1)+X(1,NJ3))
              YM=HALF*(X(2,NJ1)+X(2,NJ3))
              ZM=HALF*(X(3,NJ1)+X(3,NJ3))

              NX1 = XX-XM
              NY1 = YY-YM
              NZ1 = ZZ-ZM

              ! decomposition de (M,P) sur (M,N2) et (M,N3)
              NX2 = X(1,NJ2)-XM
              NY2 = X(2,NJ2)-YM
              NZ2 = X(3,NJ2)-ZM

              NX3 = X(1,NJ3)-XM
              NY3 = X(2,NJ3)-YM
              NZ3 = X(3,NJ3)-ZM

              PS1 = NX1*NX2+NY1*NY2+NZ1*NZ2
              PS2 = NX2*NX3+NY2*NY3+NZ2*NZ3
              PS3 = NX1*NX3+NY1*NY3+NZ1*NZ3
              AN2 = NX2*NX2+NY2*NY2+NZ2*NZ2
              AN3 = NX3*NX3+NY3*NY3+NZ3*NZ3
              DET = PS2*PS2-AN2*AN3

              MU     = (PS2*PS1-PS3*AN2)/SIGN(MAX(EM30,ABS(DET)),DET)

              FACR =MIN(ONE,MAX(-ONE,MU))
              NX1=NX1-FACR*NX3
              NY1=NY1-FACR*NY3
              NZ1=NZ1-FACR*NZ3

              AN1 = (NX1**2+NY1**2+NZ1**2)
              AN = MAX(EM30,SQRT((NORMAL(1,I)**2+NORMAL(2,I)**2+NORMAL(3,I)**2)*AN1))
              PJ=PJ*MAX(ZERO,(NX1*NORMAL(1,I)+NY1*NORMAL(2,I)+NZ1*NORMAL(3,I))/AN)
              AN = MAX(EM30,SQRT((NX2**2+NY2**2+NZ2**2)*AN1))
              TMPO = (NX1*NX2+NY1*NY2+NZ1*NZ2)/AN
              TMPO = SIGN(MIN(ONE,ABS(TMPO)),TMPO)
              THETA=API*ACOS(TMPO)
              PJ=PJ*FPA*FINTER_MIXED(PYTHON,NFUNCT,IPA,THETA*SCALA,NPC,TF)
              DIST = SQRT(AN1)
              IF(IPZ/=0)PJ=PJ*FPZ*FINTER_MIXED(PYTHON,NFUNCT,IPZ,DIST*SCALD,NPC,TF)

              FX=PJ*NORMAL(1,I)
              FY=PJ*NORMAL(2,I)
              FZ=PJ*NORMAL(3,I)

              FSKY(1,IADMV(1,I))=FSKY(1,IADMV(1,I))+FX
              FSKY(2,IADMV(1,I))=FSKY(2,IADMV(1,I))+FY
              FSKY(3,IADMV(1,I))=FSKY(3,IADMV(1,I))+FZ

              FSKY(1,IADMV(2,I))=FSKY(1,IADMV(2,I))+FX
              FSKY(2,IADMV(2,I))=FSKY(2,IADMV(2,I))+FY
              FSKY(3,IADMV(2,I))=FSKY(3,IADMV(2,I))+FZ

              FSKY(1,IADMV(3,I))=FSKY(1,IADMV(3,I))+FX
              FSKY(2,IADMV(3,I))=FSKY(2,IADMV(3,I))+FY
              FSKY(3,IADMV(3,I))=FSKY(3,IADMV(3,I))+FZ

              FSKY(1,IADMV(4,I))=FSKY(1,IADMV(4,I))+FX
              FSKY(2,IADMV(4,I))=FSKY(2,IADMV(4,I))+FY
              FSKY(3,IADMV(4,I))=FSKY(3,IADMV(4,I))+FZ

              IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .           ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
                ADDRESS = CONTRIBUTION_INDEX(I,1)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
                ADDRESS = CONTRIBUTION_INDEX(I,2)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
                ADDRESS = CONTRIBUTION_INDEX(I,3)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
                ADDRESS = CONTRIBUTION_INDEX(I,4)
                CONTRIBUTION(ADDRESS,1) = FX
                CONTRIBUTION(ADDRESS,2) = FY
                CONTRIBUTION(ADDRESS,3) = FZ
              ENDIF

              WFEXT=WFEXT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                  +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                  +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
            ENDIF
          ENDDO
!$OMP END DO

          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
!$OMP DO SCHEDULE(guided)
            DO I=1,NODE_NUMBER
              NOD = NODE_ID(I)
              ADDRESS = CONTRIBUTION_NUMBER(I)
              LOCAL_CONTRIBUTION_NUMBER = CONTRIBUTION_NUMBER(I+1) - CONTRIBUTION_NUMBER(I)
              TMP(1) = ZERO
              TMP(2) = ZERO
              TMP(3) = ZERO
              DO J=1,LOCAL_CONTRIBUTION_NUMBER
                TMP(1) = TMP(1) + CONTRIBUTION(ADDRESS+J,1)
                TMP(2) = TMP(2) + CONTRIBUTION(ADDRESS+J,2)
                TMP(3) = TMP(3) + CONTRIBUTION(ADDRESS+J,3)
              ENDDO
              FEXT(1,NOD) = FEXT(1,NOD)+ TMP(1)
              FEXT(2,NOD) = FEXT(2,NOD)+ TMP(2)
              FEXT(3,NOD) = FEXT(3,NOD)+ TMP(3)
            ENDDO
!$OMP END DO
          ENDIF

!$OMP END PARALLEL
        ENDIF
      ENDDO


      RETURN
      END
