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
Chd|  SIGEPS34T                     source/materials/mat/mat034/sigeps34t.F
Chd|-- called by -----------
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS34T(NEL     ,NGL     ,MAT     ,PID     ,UPARAM  ,
     .                     IPM     ,GEO     ,OFF     ,FOR     ,STI     ,
     .                     EINT    ,AREA    ,AL0     ,AL      ,EPSP    ,
     .                     NUVAR   ,UVAR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)  :: NEL,NUVAR
      INTEGER ,DIMENSION(NEL) ,INTENT(IN)  :: MAT,PID,NGL
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      my_real ,DIMENSION(NPROPG ,NUMGEO) ,INTENT(IN) :: GEO
      my_real ,DIMENSION(*) ,INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL) :: OFF,FOR,EINT,AREA,AL0,AL,STI,EPSP
      my_real ,DIMENSION(NEL,NUVAR) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IADBUF,NINDX
      INTEGER ,DIMENSION(NEL) :: INDX,ICC,ISRATE,VFLAG
      my_real :: K3,NU2,DSIG,H01,H02,H1,H2,SV,DDEXX,DEPSDXX,
     .  DEXX,DEPSVXX,DP
      my_real, DIMENSION(NEL) :: BULK,G_INS,G_INF,GE,GE2,GV,GV2,BETA,
     .   DEPS,GAP,EPS,C1,C2
c=======================================================================     
      DO I=1,NEL
        IADBUF   = IPM(7,MAT(I))-1
        BULK(I)  = UPARAM(IADBUF+1)
        G_INS(I) = UPARAM(IADBUF+2)
        G_INF(I) = UPARAM(IADBUF+3)
        BETA(I)  = UPARAM(IADBUF+4)
        GAP(I)   = GEO(2,PID(I))
      ENDDO
c        
      GE(1:NEL)  = G_INF(1:NEL)                ! elastic part of shear modulus
      GV(1:NEL)  = G_INS(1:NEL)-G_INF(1:NEL)   ! viscous part of shear modulus
      GE2(1:NEL) = GE(1:NEL) * DEUX
      GV2(1:NEL) = GV(1:NEL) * DEUX 
      C1(1:NEL)  = UN - EXP(-BETA(1:NEL)*DT1)
      C2(1:NEL)  =-C1(1:NEL) / BETA(1:NEL)
c      
      DEPS(1:NEL) = EPSP(1:NEL) * DT1   ! strain increment
      EPS(1:NEL) = UVAR(1:NEL,2) + DEPS(1:NEL) ! total normal strain
      UVAR(1:NEL,2) = EPS(1:NEL) 
c
      DO I=1,NEL
        IF (GAP(I) > ZERO .AND. AL(I) <= (AL0(I)-GAP(I))) OFF(I)=UN
      ENDDO
c
      DO I=1,NEL
        K3  = BULK(I)*TROIS
        NU2 = (K3 - GE2(I)) / (K3 + GE(I))
        NU2 = MAX(NU2, UN)
        EINT(I) = EINT(I) + FOR(I)*DEPS(I)*AL(I)*UNDEMI
        AREA(I) = AREA(I)*(UN - NU2*DEPS(I)*OFF(I))
        STI(I)  = K3
      ENDDO
c
      DO I=1,NEL
        DDEXX = DEPS(I)*DTIERS   ! strain increment deviator    
        DEPSDXX = DTIERS*EPSP(I)
        DEXX = DTIERS*EPS(I)
        !
        DEPSVXX = C1(I)*(DEXX - UVAR(I,1))     + C2(I)*DEPSDXX
        DP  = BULK(I)*DEPS(I)
        !!        
        DSIG = GE2(I)*DDEXX - GV2(I)*DEPSVXX + DP
        FOR(I) = FOR(I) + DSIG * AREA(I)
        STI(I) = MAX(STI(I), ABS(DSIG / MAX(DDEXX,EM20)))
        STI(I) = STI(I)*OFF(I)
        FOR(I) = FOR(I)*OFF(I)  
        UVAR(I,1) =  UVAR(I,1) + DEPSVXX + DDEXX
      END DO
c
      DO I=1,NEL
        EINT(I) = EINT(I) + FOR(I)*DEPS(I)*AL(I)*UNDEMI
      ENDDO
c-----------
      RETURN
      END
