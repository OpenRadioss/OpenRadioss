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
Chd|  PORFOR5                       source/airbag/porfor5.F       
Chd|-- called by -----------
Chd|        AIRBAGB1                      source/airbag/airbagb1.F      
Chd|-- calls ---------------
Chd|        ROTO                          source/airbag/roto.F          
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PORFOR5(SVTFAC,IM,IPM,PM,ELBUF_STR,P,PEXT,IEL,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*),IM,IEL,NEL
      my_real
     .   SVTFAC,PM(NPROPM,*),P,PEXT
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C      INTEGER I,J,MTN,NEL,NFT,IAD,NPT,ISTRA,JHBE,IEXPAN,IPT,MIDPT(5)
C      DATA MIDPT/1,1,2,2,3/
      INTEGER I,J,MTN
      my_real
     .   LR1,FTHK,C1,C2,C3,LBD1,LBD2,EPSXX,EPSYY,DELTAP,COS_PHI,TAN_PHI,
     .   APOR0,APOR1,RS,DELTAA,EPS(5,1),DIR(1,2)
      my_real,
     .  DIMENSION(:), POINTER :: UVAR
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C COMPUTE EFFECTIVE LEAKAGE AREA ACCORDING TO AUTOLIV FORMULATION
C----------------------------------------------------------------
      SVTFAC= ZERO
      TAN_PHI=ZERO   !PHI=SHEAR ANGLE - FIBER ANGLE=PI/2-PHI
      DO I=1,5
        EPS(I,1) = ZERO
      ENDDO
      MTN=IPM(2,IM)
C
      IF(MTN==19) THEN
        J = (IEL-1)*8
        DO I=1,5
          EPS(I,1) = ELBUF_STR%GBUF%STRA(J+I)
        ENDDO
        DIR(1,1) = ELBUF_STR%BUFLY(1)%DIRA(IEL)
        DIR(1,2) = ELBUF_STR%BUFLY(1)%DIRA(IEL+NEL)
        CALL ROTO(1,1,EPS,DIR,1)
      ELSEIF (MTN == 58) THEN
c        IPT= MIDPT(NPT)
c        J = (IPT-1)*NEL*NUVAR+K-NFT-1
c        EPS(1,1) = ELBUF(J+3*NEL)
c        EPS(2,1) = ELBUF(J+4*NEL)
c        TAN_PHI  = ELBUF(J+5*NEL)
c        J = (IPT-1)*NEL*NUVAR+K-NFT-1
        UVAR => ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR
        EPS(1,1) = UVAR(3*NEL+IEL)  ! uvar(iel,4)
        EPS(2,1) = UVAR(4*NEL+IEL)  ! uvar(iel,5)
        TAN_PHI  = UVAR(5*NEL+IEL)  ! uvar(iel,6)
      ENDIF
C
      LBD1  = ONE+EPS(1,1)
      LBD2  = ONE+EPS(2,1)
      RS    = LBD1*LBD2
      IF(RS > ONE) THEN
        LR1   = PM(164,IM)
        FTHK  = PM(166,IM)
        C1    = PM(167,IM)
        C2    = PM(168,IM)
        C3    = PM(169,IM)
        DELTAP= MAX(P/PEXT-ONE,ZERO)
        APOR0 = (LR1-FTHK)*(LR1-FTHK)
        APOR1 = (LR1*LBD1-FTHK/SQRT(LBD2))*(LR1*LBD2-FTHK/SQRT(LBD1))
        DELTAA= MAX(APOR1-APOR0,ZERO)
        COS_PHI = ONE / SQRT(ONE + TAN_PHI*TAN_PHI)
        SVTFAC= C1*APOR0*EXP(C2*LOG(DELTAP)) + C3*DELTAA
        SVTFAC= SVTFAC*COS_PHI/(RS*LR1*LR1)
      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  PORFORM5                      source/airbag/porfor5.F       
Chd|-- called by -----------
Chd|        FVVENT0                       source/airbag/fvvent0.F       
Chd|-- calls ---------------
Chd|        ROTO                          source/airbag/roto.F          
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PORFORM5(SVTFAC,IM,IPM,PM,ELBUF_STR,P,PEXT,IEL,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*),IM,IEL,NEL
      my_real
     .   SVTFAC,PM(NPROPM,*),P,PEXT
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,MTN
      my_real
     .   LR1,FTHK,C1,C2,C3,LBD1,LBD2,EPSXX,EPSYY,DELTAP,COS_PHI,TAN_PHI,
     .   APOR0,APOR1,RS,DELTAA,EPS(5,1),DIR(1,2)
      my_real,
     .  DIMENSION(:), POINTER :: UVAR
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C COMPUTE EFFECTIVE LEAKAGE AREA ACCORDING TO AUTOLIV FORMULATION
C----------------------------------------------------------------
      SVTFAC= ZERO
      TAN_PHI=ZERO   !PHI=SHEAR ANGLE - FIBER ANGLE=PI/2-PHI
      DO I=1,5
        EPS(I,1) = ZERO
      ENDDO
      MTN=IPM(2,IM)
C
      IF(MTN==19) THEN
        J = (IEL-1)*8
        DO I=1,5
          EPS(I,1) = ELBUF_STR%GBUF%STRA(J+I)
        ENDDO
        DIR(1,1) = ELBUF_STR%BUFLY(1)%DIRA(IEL)
        DIR(1,2) = ELBUF_STR%BUFLY(1)%DIRA(IEL+NEL)
        CALL ROTO(1,1,EPS,DIR,1)
      ELSEIF (MTN == 58) THEN
        UVAR => ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR
        EPS(1,1) = UVAR(3*NEL+IEL)  ! uvar(iel,4)
        EPS(2,1) = UVAR(4*NEL+IEL)  ! uvar(iel,5)
        TAN_PHI  = UVAR(5*NEL+IEL)  ! uvar(iel,6)
      ENDIF
C
      LBD1  = ONE+EPS(1,1)
      LBD2  = ONE+EPS(2,1)
      RS    = LBD1*LBD2
      IF(RS > ONE) THEN
        LR1   = PM(164,IM)
        FTHK  = PM(166,IM)
        C1    = PM(167,IM)
        C2    = PM(168,IM)
        C3    = PM(169,IM)
        DELTAP= MAX(P/PEXT-ONE,ZERO)
        APOR0 = (LR1-FTHK)*(LR1-FTHK)
        APOR1 = (LR1*LBD1-FTHK/SQRT(LBD2))*(LR1*LBD2-FTHK/SQRT(LBD1))
        DELTAA= MAX(APOR1-APOR0,ZERO)
        COS_PHI = ONE / SQRT(ONE + TAN_PHI*TAN_PHI)
        SVTFAC= C1*APOR0*EXP(C2*LOG(DELTAP)) + C3*DELTAA
        SVTFAC= SVTFAC*COS_PHI/(LR1*LR1)
      ENDIF
C
      RETURN
      END
