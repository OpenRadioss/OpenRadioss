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
Chd|  AFIMP3                        source/ale/ale3d/afimp3.F     
Chd|-- called by -----------
Chd|        ATHERM                        source/ale/atherm.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AFIMP3(PM ,X, IXS, T,GRAD ,COEF ,ALE_CONNECT ,FV)
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
! SPMD CASE : SIXS >= NIXS*NUMELS    (SIXS = NIXS*NUMELS_L+NIXS*NSVOIS_L)
! IXQ(1:NIXS, 1:NUMELS) local elems
!    (1:NIXS, NUMELS+1:) additional elems (also on adjacent domains but connected to the boundary of the current domain)
!
! SPMD CASE : SX >= 3*NUMNOD    (SX = 3*(NUMNOD_L+NRCVVOIS_L))
! X(1:3,1:NUMNOD) : local nodes
!  (1:3, NUMNOD+1:) additional nodes (also on adjacent domains but connected to the boundary of the current domain)
!      
      INTEGER IXS(NIXS,SIXS/NIXS)
      my_real PM(NPROPM,NUMMAT), X(3,SX/3), T(*), GRAD(6,*), COEF(*), FV(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JFACE(MVSIZ), JVOIS(MVSIZ), NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ),
     .        IPERM(4,6), IFIMP, I, II, MAT, IFQ, J, IAD2, LGTH
      my_real X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .        Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), TFLU(MVSIZ), XF(MVSIZ),
     .        N1X, N1Y, N1Z, AREA
C-----------------------------------------------
      DATA IPERM / 1,2,3,4,
     .             4,3,7,8,
     .             8,7,6,5,
     .             5,6,2,1,
     .             2,6,7,3,
     .             1,4,8,5/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C---------------------------------------------------------------------
C     CALCULATION OF IMPOSED FLUXES
C---------------------------------------------------------------------
      IFIMP=0
      DO I=LFT,LLT
        II =NFT+I
        MAT=IXS(1,II)
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
        LGTH = ALE_CONNECT%ee_connect%iad_connect(II + 1) - IAD2
        DO J=1,LGTH
         JFACE(I)=J
         JVOIS(I) = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
         IF(JVOIS(I) <= 0)CYCLE!next J
         MAT=IXS(1,JVOIS(I))
         MTN=NINT(PM(19,MAT))
         IF(MTN /= 11)EXIT!next I
        ENDDO!next J
      ENDDO!next I
C-----------------------------------------------
C     SURFACE CALCULATION
C-----------------------------------------------
      DO I=LFT,LLT
        II =NFT+I
        NC1(I)=IXS(1+IPERM(1,JFACE(I)),II)
        NC2(I)=IXS(1+IPERM(2,JFACE(I)),II)
        NC3(I)=IXS(1+IPERM(3,JFACE(I)),II)
        NC4(I)=IXS(1+IPERM(4,JFACE(I)),II)

        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))

        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))

        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))

        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
      ENDDO
C------------------------------------------
C     NORMAL VECTOR CALCULATION
C------------------------------------------
      DO I=LFT,LLT
        II =NFT+I
        N1X=(Y3(I)-Y1(I))*(Z2(I)-Z4(I)) - (Z3(I)-Z1(I))*(Y2(I)-Y4(I))
        N1Y=(Z3(I)-Z1(I))*(X2(I)-X4(I)) - (X3(I)-X1(I))*(Z2(I)-Z4(I))
        N1Z=(X3(I)-X1(I))*(Y2(I)-Y4(I)) - (Y3(I)-Y1(I))*(X2(I)-X4(I))
        AREA  = HALF * SQRT(N1X**2+N1Y**2+N1Z**2)
        T(II) = (ONE-XF(I))*T(II) + XF(I)*T(JVOIS(I))
     1  - AREA*TFLU(I)*HALF*(COEF(II)+COEF(JVOIS(I))) /
     2   MAX(EM20,COEF(II)*COEF(JVOIS(I))*GRAD(JFACE(I),I))
      ENDDO

      RETURN
      END
