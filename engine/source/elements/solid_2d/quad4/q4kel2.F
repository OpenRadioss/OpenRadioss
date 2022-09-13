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
Chd|  Q4KEL2                        source/elements/solid_2d/quad4/q4kel2.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- calls ---------------
Chd|        Q4KELIJ2                      source/elements/solid_2d/quad4/q4kel2.F
Chd|        Q4KELIJS2                     source/elements/solid_2d/quad4/q4kel2.F
Chd|====================================================================
      SUBROUTINE Q4KEL2 (
     1                PY1,PY2,PY3,PY4,
     2                PZ1,PZ2,PZ3,PZ4,
     3                PYC1,PYC2,PZC1,PZC2,AY,R22,R23,
     4                K11,K12,K13,K14,K22,
     5                K23,K24,K33,K34,K44,
     4                K11U,K12U,K13U,K14U,K22U,
     5                K23U,K24U,K33U,K34U,K44U,
     4                K11L,K12L,K13L,K14L,K22L,
     5                K23L,K24L,K33L,K34L,K44L,
     6                DD,GG,DG,G33,IKSUP,ICP,OFF)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP, IKSUP
      my_real
     .    PY1(*), PY2(*), PY3(*), PY4(*), 
     .    PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .    PYC1(*), PYC2(*), PZC1(*), PZC2(*), AY(*), R22(*), R23(*),
     .    K11(2,2,*), K12(2,2,*), K13(2,2,*), K14(2,2,*), K22(2,2,*), 
     .    K23(2,2,*), K24(2,2,*), K33(2,2,*), K34(2,2,*), K44(2,2,*), 
     .    K11U(2,2,*),K12U(2,2,*),K13U(2,2,*),K14U(2,2,*),K22U(2,2,*), 
     .    K23U(2,2,*),K24U(2,2,*),K33U(2,2,*),K34U(2,2,*),K44U(2,2,*), 
     .    K11L(2,2,*),K12L(2,2,*),K13L(2,2,*),K14L(2,2,*),K22L(2,2,*), 
     .    K23L(2,2,*),K24L(2,2,*),K33L(2,2,*),K34L(2,2,*),K44L(2,2,*), 
     .    DD(3,3,*), GG(*), DG(3,3,*), G33(3,3,*), OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IS
      my_real
     .    PYC3(MVSIZ), PYC4(MVSIZ), PZC3(MVSIZ), PZC4(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        PYC3(I) = -PYC1(I)
        PYC4(I) = -PYC2(I)
        PZC3(I) = -PZC1(I)
        PZC4(I) = -PZC2(I)
      ENDDO
C
      IF (IKSUP==0) THEN
        IS = 1
        CALL Q4KELIJS2(PY1, PZ1, PY1, PZ1,
     .                PYC1, PZC1, PYC1, PZC1, AY, R22, R23,
     .                DD, GG, K11, K11U, K11L, IS)
        CALL Q4KELIJS2(PY2, PZ2, PY2, PZ2,
     .                PYC2, PZC2, PYC2, PZC2, AY, R22, R23,
     .                DD, GG, K22, K22U, K22L, IS)
        CALL Q4KELIJS2(PY3, PZ3, PY3, PZ3,
     .                PYC3, PZC3, PYC3, PZC3, AY, R22, R23,
     .                DD, GG, K33, K33U, K33L, IS)
        CALL Q4KELIJS2(PY4, PZ4, PY4, PZ4,
     .                PYC4, PZC4, PYC4, PZC4, AY, R22, R23,
     .                DD, GG, K44, K44U, K44L, IS)
        IS = 0
        CALL Q4KELIJS2(PY1, PZ1, PY2, PZ2,
     .                PYC1, PZC1, PYC2, PZC2, AY, R22, R23,
     .                DD, GG, K12, K12U, K12L, IS)
        CALL Q4KELIJS2(PY1, PZ1, PY3, PZ3,
     .                PYC1, PZC1, PYC3, PZC3, AY, R22, R23,
     .                DD, GG, K13, K13U, K13L, IS)
        CALL Q4KELIJS2(PY1, PZ1, PY4, PZ4,
     .                PYC1, PZC1, PYC4, PZC4, AY, R22, R23,
     .                DD, GG, K14, K14U, K14L, IS)
        CALL Q4KELIJS2(PY2, PZ2, PY3, PZ3,
     .                PYC2, PZC2, PYC3, PZC3, AY, R22, R23,
     .                DD, GG, K23, K23U, K23L, IS)
        CALL Q4KELIJS2(PY2, PZ2, PY4, PZ4,
     .                PYC2, PZC2, PYC4, PZC4, AY, R22, R23,
     .                DD, GG, K24, K24U, K24L, IS)
        CALL Q4KELIJS2(PY3, PZ3, PY4, PZ4,
     .                PYC3, PZC3, PYC4, PZC4, AY, R22, R23,
     .                DD, GG, K34, K34U, K34L, IS)
      ELSE
        IS = 1
        CALL Q4KELIJ2(PY1, PZ1, PY1, PZ1,
     .                PYC1, PZC1, PYC1, PZC1, AY, R22, R23,
     .                DD, DG, G33, K11, IS)
        CALL Q4KELIJ2(PY2, PZ2, PY2, PZ2,
     .                PYC2, PZC2, PYC2, PZC2, AY, R22, R23,
     .                DD, DG, G33, K22, IS)
        CALL Q4KELIJ2(PY3, PZ3, PY3, PZ3,
     .                PYC3, PZC3, PYC3, PZC3, AY, R22, R23,
     .                DD, DG, G33, K33, IS)
        CALL Q4KELIJ2(PY4, PZ4, PY4, PZ4,
     .                PYC4, PZC4, PYC4, PZC4, AY, R22, R23,
     .                DD, DG, G33, K44, IS)
        IS = 0
        CALL Q4KELIJ2(PY1, PZ1, PY2, PZ2,
     .                PYC1, PZC1, PYC2, PZC2, AY, R22, R23,
     .                DD, DG, G33, K12, IS)
        CALL Q4KELIJ2(PY1, PZ1, PY3, PZ3,
     .                PYC1, PZC1, PYC3, PZC3, AY, R22, R23,
     .                DD, DG, G33, K13, IS)
        CALL Q4KELIJ2(PY1, PZ1, PY4, PZ4,
     .                PYC1, PZC1, PYC4, PZC4, AY, R22, R23,
     .                DD, DG, G33, K14, IS)
        CALL Q4KELIJ2(PY2, PZ2, PY3, PZ3,
     .                PYC2, PZC2, PYC3, PZC3, AY, R22, R23,
     .                DD, DG, G33, K23, IS)
        CALL Q4KELIJ2(PY2, PZ2, PY4, PZ4,
     .                PYC2, PZC2, PYC4, PZC4, AY, R22, R23,
     .                DD, DG, G33, K24, IS)
        CALL Q4KELIJ2(PY3, PZ3, PY4, PZ4,
     .                PYC3, PZC3, PYC4, PZC4, AY, R22, R23,
     .                DD, DG, G33, K34, IS)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  Q4KELIJ2                      source/elements/solid_2d/quad4/q4kel2.F
Chd|-- called by -----------
Chd|        Q4KEL2                        source/elements/solid_2d/quad4/q4kel2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4KELIJ2 (PYI, PZI, PYJ, PZJ,
     .                     PYCI, PZCI, PYCJ, PZCJ, AY, R22, R23,
     .                     DD, DG, G33, KIJ, IS)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IS
      my_real
     .   PYI(*), PZI(*), PYJ(*), PZJ(*),
     .   PYCI(*), PZCI(*), PYCJ(*), PZCJ(*), AY(*), R22(*), R23(*),
     .   DD(3,3,*), DG(3,3,*), G33(3,3,*), KIJ(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EP,IASY
      my_real
     .    AYS(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO EP=LFT,LLT
        KIJ(1,1,EP) = KIJ(1,1,EP) + 
     .                DD(1,1,EP)*PYI(EP)*PYJ(EP) + 
     .                DG(1,1,EP)*(PYI(EP)*PZCJ(EP)+PZCI(EP)*PYJ(EP)) + 
     .                G33(1,1,EP)*(PZCI(EP)*PZCJ(EP))
        KIJ(1,2,EP) = KIJ(1,2,EP) + 
     .                DD(1,2,EP)*PYI(EP)*PZJ(EP) + 
     .                DG(1,1,EP)*PYI(EP)*PYCJ(EP) + 
     .                DG(2,1,EP)*PZCI(EP)*PZJ(EP) + 
     .                G33(1,1,EP)*PZCI(EP)*PYCJ(EP)
        KIJ(2,1,EP) = KIJ(2,1,EP) + 
     .                DD(1,2,EP)*PZI(EP)*PYJ(EP) + 
     .                DG(1,1,EP)*PYCI(EP)*PYJ(EP) + 
     .                DG(2,1,EP)*PZI(EP)*PZCJ(EP) + 
     .                G33(1,1,EP)*PYCI(EP)*PZCJ(EP)
        KIJ(2,2,EP) = KIJ(2,2,EP) + 
     .                DD(2,2,EP)*PZI(EP)*PZJ(EP) + 
     .                DG(2,1,EP)*(PYCI(EP)*PZJ(EP)+PZI(EP)*PYCJ(EP)) + 
     .                G33(1,1,EP)*PYCI(EP)*PYCJ(EP)
      ENDDO
C
      IF (N2D==1) THEN
        DO EP=LFT,LLT
          AYS(EP) = AY(EP)*AY(EP)
        ENDDO
        IF (JCVT==0) THEN
          DO EP=LFT,LLT
            KIJ(1,1,EP) = KIJ(1,1,EP) +
     .                  DD(1,3,EP)*AY(EP)*(PYI(EP)+PYJ(EP)) +
     .                  DD(3,3,EP)*AYS(EP) +
     .                  DG(3,1,EP)*AY(EP)*(PYCI(EP)+PZCJ(EP))
            KIJ(1,2,EP) = KIJ(1,2,EP) +
     .                  DD(2,3,EP)*AY(EP)*PZJ(EP) +
     .                  DG(3,1,EP)*AY(EP)*PYCJ(EP)
            KIJ(2,1,EP) = KIJ(2,1,EP) +
     .                  DD(2,3,EP)*PZI(EP)*AY(EP) +
     .                  DG(3,1,EP)*PYCI(EP)*AY(EP)
          ENDDO
C         asymmetric part
C          IASY = 0
C          IF (IASY/=0) THEN
C          ENDIF
        ELSE
          DO EP=LFT,LLT
            KIJ(1,1,EP) = KIJ(1,1,EP) + 
     .                  DD(1,3,EP)*R22(EP)*AY(EP)*(PYI(EP)+PYJ(EP)) +
     .                  DD(3,3,EP)*R22(EP)*R22(EP)*AYS(EP) +
     .                  DG(3,1,EP)*AY(EP)*R22(EP)*(PYCI(EP)+PZCJ(EP))
            KIJ(1,2,EP) = KIJ(1,2,EP) + 
     .                  DD(1,3,EP)*R23(EP)*AY(EP)*PYI(EP) +
     .                  DD(2,3,EP)*R22(EP)*AY(EP)*PZJ(EP) +
     .                  DD(3,3,EP)*R22(EP)*R23(EP)*AYS(EP) +
     .          DG(3,1,EP)*AY(EP)*(R22(EP)*PYCJ(EP)+R23(EP)*PZCI(EP))
            KIJ(2,1,EP) = KIJ(2,1,EP) + 
     .                  DD(1,3,EP)*R23(EP)*AY(EP)*PYJ(EP) +
     .                  DD(2,3,EP)*R22(EP)*AY(EP)*PZI(EP) +
     .                  DD(3,3,EP)*R22(EP)*R23(EP)*AYS(EP) +
     .          DG(3,1,EP)*AY(EP)*(R23(EP)*PZCJ(EP)+R22(EP)*PYCI(EP))
            KIJ(2,2,EP) = KIJ(2,2,EP) + 
     .                  DD(2,3,EP)*R23(EP)*AY(EP)*(PZI(EP)+PZJ(EP)) +
     .                  DD(3,3,EP)*R23(EP)*R23(EP)*AYS(EP) +
     .                  DG(3,1,EP)*AY(EP)*R23(EP)*(PYCI(EP)+PZCJ(EP))
          ENDDO
C         asymmetric part
C          IASY = 0
C          IF (IASY/=0) THEN
C          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  Q4KELIJS2                     source/elements/solid_2d/quad4/q4kel2.F
Chd|-- called by -----------
Chd|        Q4KEL2                        source/elements/solid_2d/quad4/q4kel2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4KELIJS2 (PYI, PZI, PYJ, PZJ,
     .                      PYCI, PZCI, PYCJ, PZCJ, AY, R22, R23,
     .                      DD, GG, KIJ, KIJU, KIJL, IS)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IS
      my_real
     .   PYI(*), PZI(*), PYJ(*), PZJ(*),
     .   PYCI(*), PZCI(*), PYCJ(*), PZCJ(*), AY(*), R22(*), R23(*),
     .   DD(3,3,*), GG(*), KIJ(2,2,*), KIJU(2,2,*), KIJL(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EP,IASY
      my_real
     .    AYS(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO EP=LFT,LLT
        KIJ(1,1,EP) = KIJ(1,1,EP) + 
     .                DD(1,1,EP)*PYI(EP)*PYJ(EP) + 
     .                GG(EP)*PZCI(EP)*PZCJ(EP)
        KIJ(1,2,EP) = KIJ(1,2,EP) + 
     .                DD(1,2,EP)*PYI(EP)*PZJ(EP) + 
     .                GG(EP)*PZCI(EP)*PYCJ(EP)
        KIJ(2,1,EP) = KIJ(2,1,EP) + 
     .                DD(1,2,EP)*PZI(EP)*PYJ(EP) + 
     .                GG(EP)*PYCI(EP)*PZCJ(EP)
        KIJ(2,2,EP) = KIJ(2,2,EP) + 
     .                DD(2,2,EP)*PZI(EP)*PZJ(EP) + 
     .                GG(EP)*PYCI(EP)*PYCJ(EP)
      ENDDO
C
      IF (N2D==1) THEN
        DO EP=LFT,LLT
          AYS(EP) = AY(EP)*AY(EP)
        ENDDO
        IF (JCVT==0) THEN
          DO EP=LFT,LLT
            KIJ(1,1,EP) = KIJ(1,1,EP) + 
     .                  DD(1,3,EP)*AY(EP)*(PYI(EP)+PYJ(EP)) + 
     .                  DD(3,3,EP)*AYS(EP)
            KIJ(1,2,EP) = KIJ(1,2,EP) + 
     .                  DD(2,3,EP)*AY(EP)*PZJ(EP) 
            KIJ(2,1,EP) = KIJ(2,1,EP) + 
     .                  DD(2,3,EP)*PZI(EP)*AY(EP) 
          ENDDO
C         asymmetric part
C          IASY = 0
C          IF (IASY/=0) THEN
C          DO EP=LFT,LLT
C            KIJU(1,1,EP) = KIJU(1,1,EP) - 
C     .                  AY(EP)*(DD(1,1,EP)*PYJ(EP)+DD(1,3,EP)*AY(EP))
C            KIJU(1,2,EP) = KIJU(1,2,EP) - AY(EP)*DD(1,2,EP)*PZJ(EP)
C            KIJU(2,1,EP) = KIJU(2,1,EP) - AY(EP)*GG(EP)*PZCJ(EP)
C            KIJU(2,2,EP) = KIJU(2,2,EP) - AY(EP)*GG(EP)*PYCJ(EP)
C            KIJL(1,1,EP) = KIJL(1,1,EP) - 
C     .                  AY(EP)*(DD(1,1,EP)*PYI(EP)+DD(1,3,EP)*AY(EP))
C            KIJL(1,2,EP) = KIJL(1,2,EP) - AY(EP)*GG(EP)*PZCI(EP)
C            KIJL(2,1,EP) = KIJL(2,1,EP) - AY(EP)*DD(1,2,EP)*PZI(EP)
C            KIJL(2,2,EP) = KIJL(2,2,EP) - AY(EP)*GG(EP)*PYCI(EP)
C          ENDDO
C          ENDIF
        ELSE
          DO EP=LFT,LLT
            KIJ(1,1,EP) = KIJ(1,1,EP) + 
     .                  DD(1,3,EP)*R22(EP)*AY(EP)*(PYI(EP)+PYJ(EP)) +
     .                  DD(3,3,EP)*R22(EP)*R22(EP)*AYS(EP)
            KIJ(1,2,EP) = KIJ(1,2,EP) + 
     .                  DD(1,3,EP)*R23(EP)*AY(EP)*PYI(EP) +
     .                  DD(2,3,EP)*R22(EP)*AY(EP)*PZJ(EP) +
     .                  DD(3,3,EP)*R22(EP)*R23(EP)*AYS(EP)
            KIJ(2,1,EP) = KIJ(2,1,EP) + 
     .                  DD(1,3,EP)*R23(EP)*AY(EP)*PYJ(EP) +
     .                  DD(2,3,EP)*R22(EP)*AY(EP)*PZI(EP) +
     .                  DD(3,3,EP)*R22(EP)*R23(EP)*AYS(EP)
            KIJ(2,2,EP) = KIJ(2,2,EP) + 
     .                  DD(2,3,EP)*R23(EP)*AY(EP)*(PZI(EP)+PZJ(EP)) +
     .                  DD(3,3,EP)*R23(EP)*R23(EP)*AYS(EP)
          ENDDO
C         asymmetric part
C          IASY = 0
C          IF (IASY/=0) THEN
C          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
