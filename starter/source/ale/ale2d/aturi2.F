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
C
Chd|====================================================================
Chd|  ATURI2                        source/ale/ale2d/aturi2.F     
Chd|-- called by -----------
Chd|        BINIT2                        source/ale/bimat/binit2.F     
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ATURI2(IPARG,RHO,PM,IX,X,RK,RE,AIRE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is initializing state for
C  turbulency model K-EPS in ALE (2D)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP), IX(7,*)
      my_real, INTENT(INOUT) :: X(3,NUMNOD), RK(MVSIZ), RE(MVSIZ)
      my_real, INTENT(IN) :: RHO(MVSIZ),PM(NPROPM,NUMMAT), AIRE(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, MAT, NG, MTN2, LLT2, NFT2, LFT2, JJ, J
      my_real XL, XK, C1, XE, D2, Y0, Z0, Y1, Z1
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(MTN == 17)THEN
        DO I=LFT,LLT
          II=I+NFT
          MAT=IABS(IX(1,II))
          RK(I)=RHO(I)*PM(87,MAT)
          XL=SQRT(AIRE(I))
          XK= MAX(RK(I),EM15)/RHO(I)
          C1=PM(81,MAT)
          XE=SQRT(XK*SQRT(C1))**3/(ZEP4187*XL)
          RE(I)=RHO(I)*XE
        END DO
      ELSE
       DO I=LFT,LLT
         II=I+NFT
         MAT=IABS(IX(1,II))
         RK(I)=RHO(I)*PM(87,MAT)
         XL=SQRT(AIRE(I))
         XK= MAX(RK(I),EM15)/RHO(I)
         D2=EP30
         Y0=X(2,IX(2,II))+X(2,IX(3,II))+X(2,IX(4,II))+X(2,IX(5,II))
         Z0=X(3,IX(2,II))+X(3,IX(3,II))+X(3,IX(4,II))+X(3,IX(5,II))
         DO NG=1,NGROUP
           MTN2=IPARG(1,NG)
           IF(MTN2 == 17)THEN
             LLT2=IPARG(2,NG)
             NFT2=IPARG(3,NG)
             LFT2=1
             DO JJ=LFT2,LLT2
               J=JJ+NFT2
               Y1=X(2,IX(2,J))+X(2,IX(3,J))+X(2,IX(4,J))+X(2,IX(5,J))
               Z1=X(3,IX(2,J))+X(3,IX(3,J))+X(3,IX(4,J))+X(3,IX(5,J))
               D2= MIN(D2,(Y1-Y0)**2+(Z1-Z0)**2)
             END DO !JJ=LFT2,LLT2
           ENDIF
         END DO !NG=1,NGROUP
         XL=XL+ FOURTH*SQRT(D2)
         XE=ZEP33*SQRT(XK)**3/XL
         RE(I)=RHO(I)*XE
       END DO !I=LFT,LLT
      ENDIF
C-----------------------------------------------
      RETURN
      END
