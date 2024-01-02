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
Chd|  AFIMP2                        source/ale/ale2d/afimp2.F     
Chd|-- called by -----------
Chd|        ATHERM                        source/ale/atherm.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AFIMP2(
     1                  PM  ,X           ,IXQ  ,T  ,GRAD  ,
     2                  COEF,ALE_CONNECT ,FV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
! SPMD CASE : SIXQ >= NIXQ*NUMELQ    (SIXQ = NIXQ*NUMELQ_L+NIXQ*NQVOIS_L)
! IXQ(1:NIXQ, 1:NUMELQ) local elems
!    (1:NIXQ, NUMELQ+1:) additional elems (also on adjacent domains but connected to the boundary of the current domain)
!
! SPMD CASE : SX >= 3*NUMNOD    (SX = 3*(NUMNOD_L+NRCVVOIS_L))
! X(1:3,1:NUMNOD) : local nodes
!  (1:3, NUMNOD+1:) additional nodes (also on adjacent domains but connected to the boundary of the current domain)
!
      INTEGER IXQ(NIXQ,SIXQ/NIXQ)
      my_real PM(NPROPM,NUMMAT), X(3,SX/3), T(*), GRAD(4,*), COEF(*), FV(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JFACE(MVSIZ), JVOIS(MVSIZ), NC1(MVSIZ), NC2(MVSIZ), IPERM(2,4),
     .        IFIMP, I,II, MAT, IFQ, J, IAD2, LGTH
      my_real Y1(MVSIZ)  , Y2(MVSIZ), Z1(MVSIZ) , Z2(MVSIZ) ,
     .        TFLU(MVSIZ), XF(MVSIZ), N1Y(MVSIZ), N1Z(MVSIZ),
     .        AREA
C-----------------------------------------------
      DATA IPERM / 1,2,
     .             2,3,
     .             3,4,
     .             4,1/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C---------------------------------------------------------------------
C     CALCULATION OF IMPOSED FLUXES
C---------------------------------------------------------------------
      IFIMP=0
      DO I=LFT,LLT
        II =NFT+I
        MAT=IXQ(1,II)
        IFQ=NINT(PM(44,MAT))
        IF(IFQ /= 0)THEN
          TFLU(I)=PM(60,MAT)*FV(IFQ)
          XF(I)=ONE
          IFIMP=1
        ELSE
          TFLU(I)=ZERO
          XF(I)=ZERO
        ENDIF
      ENDDO
      
      IF(IFIMP == 0)RETURN
C---------------------------------------------------------------------
C     FINDING RELATED FACE
C---------------------------------------------------------------------
      DO I=LFT,LLT
        II =NFT+I
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
        LGTH = ALE_CONNECT%ee_connect%iad_connect(II+1) - IAD2
        DO J=1,LGTH
          JFACE(I)=J
          JVOIS(I)=ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
          IF(JVOIS(I) <= 0)CYCLE
          MAT=IXQ(1,JVOIS(I))
          MTN=NINT(PM(19,MAT))
          IF(MTN /= 11)EXIT
        ENDDO!next J
      ENDDO!next I
      
C-----------------------------------------------
C     SURFACE CALCULATION
C-----------------------------------------------
      DO I=LFT,LLT
        II =NFT+I
        NC1(I) = IXQ(1+IPERM(1,JFACE(I)),II)
        NC2(I) = IXQ(1+IPERM(2,JFACE(I)),II)

        Y1(I)  = X(2,NC1(I))
        Z1(I)  = X(3,NC1(I))

        Y2(I)  = X(2,NC2(I))
        Z2(I)  = X(3,NC2(I))

        N1Y(I) = (Z2(I)-Z1(I))
        N1Z(I) = -(Y2(I)-Y1(I))
      ENDDO

      IF(N2D == 1)THEN
        DO I=LFT,LLT
          N1Y(I) = N1Y(I)*(Y1(I)+Y2(I))*HALF
          N1Z(I) = N1Z(I)*(Y1(I)+Y2(I))*HALF
        ENDDO
      ENDIF
      
C------------------------------------------
C     NORMAL VECTOR CALCULATION
C------------------------------------------
      DO I=LFT,LLT
        II    = NFT+I
        AREA  = SQRT(N1Y(I)**2+N1Z(I)**2)
        T(II) = (ONE-XF(I))*T(II) + XF(I)*T(JVOIS(I)) - AREA*TFLU(I)*HALF*(COEF(II)+COEF(JVOIS(I))) /
     .   MAX(EM20,COEF(II)*COEF(JVOIS(I))*GRAD(JFACE(I),I))
      ENDDO

      RETURN
      END
