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
Chd|  CZBE3                         source/elements/shell/coquez/czbe3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZBE3(JFT ,JLT  ,AREA ,A_I  ,X13  ,
     2                  X24  ,Y13  ,Y24  ,MX13 ,MX23 ,
     3                  MX34 ,MY13 ,MY23 ,MY34 ,Z1   ,
     4                  PX1  ,PX2  ,PY1  ,PY2  ,RX   ,
     5                  RY   ,SX   ,SY   ,RX2  ,RY2  ,
     6                  SX2  ,SY2  ,RHX  ,RHY  ,SHX  ,
     7                  SHY  ,PH1  ,PH2  ,HXX  ,HYY  ,
     8                  HXY  ,NPLAT,IPLAT)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT,IPLAT(*)
      my_real 
     .   X13(*),X24(*),Y13(*),Y24(*),AREA(*),Z1(*),A_I(*),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*)
      my_real 
     .   PX1(*),PX2(*),PY1(*),PY2(*),PH1(*),PH2(*),
     .   RX(*),RY(*), SX(*),SY(*),RX2(*),RY2(*), SX2(*),SY2(*),
     .   RHX(MVSIZ,4),RHY(MVSIZ,4), SHX(MVSIZ,4),SHY(MVSIZ,4),HXX(*),HYY(*),HXY(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,EP
C-----------gama(I)=hI/4-PH(I), PH:anti-sym comme bxI------
#include "vectorize.inc"
       DO I=JFT,JLT
        EP=IPLAT(I)
        PX1(I) = Y24(EP)*A_I(EP)
        PX2(I) = -Y13(EP)*A_I(EP)
        PY1(I) = -X24(EP)*A_I(EP)
        PY2(I) =  X13(EP)*A_I(EP)
        PH1(I) =  MX13(EP)*PX1(I)+ MY13(EP)*PY1(I)
        PH2(I) =  MX13(EP)*PX2(I)+ MY13(EP)*PY2(I)
       ENDDO 
C-----------Bc = Bcr0(1+r*rI)+Bcs0(1+s*sI)------
C-----------rhx(I)=rkxk+hkxk*SI---shx(I)=skxk+hkxk*RI-
C----------shear----R : -1 1 1 -1; S: -1 -1 1 1--
#include "vectorize.inc"
       DO I=JFT,JLT
        EP=IPLAT(I)
        SX(I) = MX34(EP)*A_I(EP) 
        SY(I) = MY34(EP)*A_I(EP) 
        RX(I) = MX23(EP)*A_I(EP) 
        RY(I) = MY23(EP)*A_I(EP) 
        RHX(I,1) = MX23(EP)-MX13(EP) 
        RHX(I,3) = MX23(EP)+MX13(EP) 
        RHY(I,1) = MY23(EP)-MY13(EP) 
        RHY(I,3) = MY23(EP)+MY13(EP) 
        SHX(I,1) = MX34(EP)-MX13(EP)
        SHX(I,2) = MX34(EP)+MX13(EP)
        SHY(I,1) = MY34(EP)-MY13(EP)
        SHY(I,2) = MY34(EP)+MY13(EP)
       ENDDO
C
       DO I=JFT,JLT
        SX2(I) = SX(I)*SX(I) 
        SY2(I) = SY(I)*SY(I) 
        RX2(I) = RX(I)*RX(I) 
        RY2(I) = RY(I)*RY(I)
        RHX(I,2) = RHX(I,1) 
        RHX(I,4) = RHX(I,3) 
        RHY(I,2) = RHY(I,1) 
        RHY(I,4) = RHY(I,3) 
        SHX(I,3) = SHX(I,2)
        SHX(I,4) = SHX(I,1)
        SHY(I,3) = SHY(I,2)
        SHY(I,4) = SHY(I,1)
        HXX(I)=FIVEP333*(SY2(I)+RY2(I))
        HYY(I)=FIVEP333*(SX2(I)+RX2(I))
        HXY(I)=-FIVEP333*(SX(I)*SY(I)+RX(I)*RY(I))
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CZBER3                        source/elements/shell/coquez/czbe3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|        CZDERIRZ                      source/elements/shell/coquez/czdef.F
Chd|====================================================================
        SUBROUTINE CZBER3(JFT ,JLT  ,AREA ,A_I  ,X13  ,
     1                   X24  ,Y13  ,Y24  ,MX13 ,MX23 ,
     2                   MX34 ,MY13 ,MY23 ,MY34 ,Z1   ,
     3                   RX   ,RY   ,SX   ,SY   ,PRX  ,
     4                   PRY  ,PRXY ,PRZ  ,PHKRX,PHKRY,
     5                   PHKRXY,PHERX,PHERY,PHERXY,
     6                   PHKRZ,PHERZ ,PHKX ,PHKY ,PHEX ,
     7                   PHEY ,IPLAT)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IPLAT(*)
      my_real 
     .   X13(*),X24(*),Y13(*),Y24(*),AREA(*),Z1(*),A_I(*),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*),
     .   RX(*),RY(*), SX(*),SY(*)
      my_real 
     .   PRX(4,*),PRY(4,*),PRXY(4,*),PHKRX(4,*),PHKRY(4,*),
     .   PHKRXY(4,*),PHERX(4,*),PHERY(4,*),PHERXY(4,*),PRZ(4,*),
     .   PHKRZ(4,*),PHERZ(4,*),PHKX(*) ,PHKY(*) ,PHEX(*) ,PHEY(*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real 
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4)
C---------------
        CALL CZDERIRZ(JFT ,JLT  ,AREA ,X13 ,X24   ,
     2               Y13  ,Y24  ,MX13 ,MX23,MX34  ,
     3               MY13 ,MY23 ,MY34 ,BM0RZ,BMKRZ,
     4               BMERZ )
#include "vectorize.inc"
       DO I=JFT,JLT
        EP=IPLAT(I)
        DO J=1,4
         PRX(J,I)= BM0RZ(EP,1,J)*A_I(EP)
         PRY(J,I)= BM0RZ(EP,2,J)*A_I(EP)
         PRXY(J,I)=BM0RZ(EP,3,J)*A_I(EP)
C         
         PHKRX(J,I)= BMKRZ(EP,1,J)*A_I(EP)
         PHKRY(J,I)= BMKRZ(EP,2,J)*A_I(EP)
         PHKRXY(J,I)=BMKRZ(EP,3,J)*A_I(EP)
C         
         PHERX(J,I)= BMERZ(EP,1,J)*A_I(EP)
         PHERY(J,I)= BMERZ(EP,2,J)*A_I(EP)
         PHERXY(J,I)=BMERZ(EP,3,J)*A_I(EP)
C         
         PRZ(J,I)=BM0RZ(EP,4,J)*A_I(EP)
        ENDDO 
       ENDDO
C       
       DO J=1,4
       DO I=JFT,JLT
        EP=IPLAT(I)
        PHKRZ(J,I)=BMKRZ(EP,4,J)*A_I(EP)
        PHERZ(J,I)=BMERZ(EP,4,J)*A_I(EP)
       ENDDO 
       ENDDO 
C       
       DO I=JFT,JLT
        PHEX(I)= FOUR*SY(I)
        PHEY(I) =-FOUR*SX(I)
        PHKX(I)=-FOUR*RY(I)  
        PHKY(I)= FOUR*RX(I)
       ENDDO 
C
      RETURN
      END
