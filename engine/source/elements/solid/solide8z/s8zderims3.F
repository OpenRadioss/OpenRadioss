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
Chd|  S8ZDERIMS3                    source/elements/solid/solide8z/s8zderims3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDERIMS3(VOLDP,
     .    KSI,ETA,ZETA,WI,
     .    HX,    HY,    HZ,   
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC1,JAC2,JAC3,
     .    JAC4,JAC5,JAC6,
     .    JAC7,JAC8,JAC9,
     .    JACI1,JACI2,JACI3,
     .    JACI4,JACI5,JACI6,
     .    JACI7,JACI8,JACI9,NNEGA,INDEX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNEGA,INDEX(MVSIZ)
C     REAL
      my_real
     .   KSI,ETA,ZETA,WI,
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),
     .   JACI1(*),JACI2(*),JACI3(*),
     .   JACI4(*),JACI5(*),JACI6(*),
     .   JACI7(*),JACI8(*),JACI9(*)
      DOUBLE PRECISION 
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ICOR,ep
C     REAL
C                                                                     12
      my_real
     .   DET ,DETT ,  
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   JAC_38_29, JAC_19_37, JAC_27_18,
     .   JAC_26_35, JAC_34_16, JAC_15_24
      DOUBLE PRECISION 
     .   DETDP
C-----------------------------------------------
#include "vectorize.inc"
      DO J=1,NNEGA
       I = INDEX(J)
       JAC1(I)=CJ1(I)+HX(I,3)*ETA+(HX(I,2)+HX(I,4)*ETA)*ZETA
       JAC2(I)=CJ2(I)+HY(I,3)*ETA+(HY(I,2)+HY(I,4)*ETA)*ZETA
       JAC3(I)=CJ3(I)+HZ(I,3)*ETA+(HZ(I,2)+HZ(I,4)*ETA)*ZETA
C
       JAC4(I)=CJ4(I)+HX(I,1)*ZETA+(HX(I,3)+HX(I,4)*ZETA)*KSI
       JAC5(I)=CJ5(I)+HY(I,1)*ZETA+(HY(I,3)+HY(I,4)*ZETA)*KSI
       JAC6(I)=CJ6(I)+HZ(I,1)*ZETA+(HZ(I,3)+HZ(I,4)*ZETA)*KSI
C
       JAC7(I)=CJ7(I)+HX(I,2)*KSI+(HX(I,1)+HX(I,4)*KSI)*ETA
       JAC8(I)=CJ8(I)+HY(I,2)*KSI+(HY(I,1)+HY(I,4)*KSI)*ETA
       JAC9(I)=CJ9(I)+HZ(I,2)*KSI+(HZ(I,1)+HZ(I,4)*KSI)*ETA
C       
C     JACOBIAN
C
       JAC_59_68=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
       JAC_67_49=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
       JAC_38_29=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
       JAC_19_37=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
       JAC_27_18=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
       JAC_26_35=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
       JAC_34_16=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
       JAC_15_24=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
       JAC_48_57=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
       DETDP=ONE_OVER_512*(JAC1(I)*JAC_59_68+JAC2(I)*JAC_67_49+JAC3(I)*JAC_48_57)
       DET = DETDP
       VOLDP(I)= WI*DETDP
       DETT=ONE_OVER_512/DET
       JACI1(I)=DETT*JAC_59_68
       JACI4(I)=DETT*JAC_67_49
       JACI7(I)=DETT*JAC_48_57
       JACI2(I)=DETT*JAC_38_29
       JACI5(I)=DETT*JAC_19_37
       JACI8(I)=DETT*JAC_27_18
       JACI3(I)=DETT*JAC_26_35
       JACI6(I)=DETT*JAC_34_16
       JACI9(I)=DETT*JAC_15_24
      ENDDO
C
      RETURN
      END
