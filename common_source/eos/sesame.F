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
!||    sesame_mod   ../common_source/eos/sesame.F
!||--- called by ------------------------------------------------------
!||    eosmain      ../common_source/eos/eosmain.F
!||    eosmain51    ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
       MODULE SESAME_MOD
       IMPLICIT NONE
       CONTAINS
!||====================================================================
!||    sesame          ../common_source/eos/sesame.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- calls      -----------------------------------------------------
!||    mintp1_rt       ../common_source/eos/mintp1_rt.F
!||    mintp_re        ../common_source/eos/mintp_re.F
!||    mintp_rt        ../common_source/eos/mintp_rt.F
!||--- uses       -----------------------------------------------------
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||====================================================================
      SUBROUTINE SESAME(IFLAG , NEL,      PMIN  ,OFF  ,EINT  ,RHO   ,RHO0  ,
     2                  ESPE  , DVOL,     PNEW  ,DPDM ,DPDE  ,THETA ,
     3                  BUFMAT, EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of SESAME EOS
!----------------------------------------------------------------------------
!! \details STAGGERED SCHEME IS EXECUTED IN TWO PASSES IN EOSMAIN : IFLG=0 THEN IFLG=1
!! \details COLLOCATED SCHEME IS DOING A SINGLE PASS : IFLG=2
!! \details
!! \details  STAGGERED SCHEME
!! \details     EOSMAIN / IFLG = 0 : DERIVATIVE CALCULATION FOR SOUND SPEED ESTIMATION c[n+1] REQUIRED FOR PSEUDO-VISCOSITY (DPDE:partial derivative, DPDM:total derivative)
!! \details     MQVISCB            : PSEUDO-VISCOSITY Q[n+1]
!! \details     MEINT              : INTERNAL ENERGY INTEGRATION FOR E[n+1] : FIRST PART USING P[n], Q[n], and Q[n+1] CONTRIBUTIONS
!! \details     EOSMAIN / IFLG = 1 : UPDATE P[n+1], T[N+1]
!! \details                          INTERNAL ENERGY INTEGRATION FOR E[n+1] : LAST PART USING P[n+1] CONTRIBUTION
!! \details                            (second order integration dE = -P.dV where P = 0.5(P[n+1] + P[n]) )
!! \details  COLLOCATED SCHEME
!! \details     EOSMAIN / IFLG = 2 : SINGLE PASS FOR P[n+1] AND DERIVATIVES
!----------------------------------------------------------------------------C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
!#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER :: IFLAG, NEL
        my_real :: BUFMAT(SBUFMAT),
     .             OFF(NEL) , EINT(NEL), RHO(NEL) , RHO0(NEL),
     .             ESPE(NEL), DVOL(NEL), PNEW(NEL),
     .             DPDM(NEL), DPDE(NEL), THETA(NEL)
        TYPE(EOS_PARAM_),INTENT(IN) :: EOS_STRUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I, NR, NT, IDR, IDT, IDP, IDE
        my_real :: ESPEM, PRES, DTDE, DPDR, DPDT
        my_real :: PMIN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        NR  = EOS_STRUCT%IPARAM(1)
        NT  = EOS_STRUCT%IPARAM(2)
        IDR = EOS_STRUCT%IPARAM(3)

        IF(IFLAG == 0) THEN
          DO I=1,NEL
            IDT = IDR + NR
            IDP = IDT + NT
            IDE = IDP + NR * NT
            ESPEM = ESPE(I)/RHO0(I)
            DTDE = ZERO
            CALL MINTP_RE(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDE),RHO(I),THETA(I),ESPEM,DTDE)
            CALL MINTP1_RT(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDP),RHO(I),THETA(I),PRES,DPDR,DPDT)
            DPDM(I) = RHO0(I)*DPDR
            DPDE(I) = DPDT*DTDE/RHO0(I)
            PNEW(I) = MAX(PRES,PMIN)*OFF(I)! P(mu[n+1],E[n])
          ENDDO

        ELSEIF(IFLAG == 1) THEN
          DO I=1,NEL
            IDT = IDR + NR
            IDP = IDT + NT
            IDE = IDP + NR * NT
            ESPEM=ESPE(I)/RHO0(I)
            CALL MINTP_RE(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDE),RHO(I),THETA(I),ESPEM,DTDE)
            CALL MINTP_RT(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDP),RHO(I),THETA(I),PNEW(I),DPDR)
            PNEW(I)= MAX(PNEW(I),PMIN)*OFF(I)! P(mu[n+1],E[n+1])
            EINT(I)= EINT(I)-HALF*DVOL(I)*PNEW(I)
          ENDDO

        ELSEIF(IFLAG == 2) THEN
          DO I=1, NEL
            IDT = IDR + NR
            IDP = IDT + NT
            IDE = IDP + NR * NT
            ESPEM = ESPE(I)/RHO0(I)
            CALL MINTP_RE(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDE),RHO(I),THETA(I),ESPEM,DTDE)
            CALL MINTP1_RT(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDP),RHO(I),THETA(I),PRES,DPDR,DPDT)
            DPDM(I) = RHO0(I)*DPDR
            DPDE(I) = DPDT*DTDE/RHO0(I)
            PNEW(I) = MAX(PRES,PMIN)*OFF(I)
          ENDDO
        ENDIF
C-----------------------------------------------
        RETURN
      END SUBROUTINE SESAME
      END MODULE SESAME_MOD
