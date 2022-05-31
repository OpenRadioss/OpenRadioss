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
Chd|  I12VIT3                       source/interfaces/interf/i12vit3.F
Chd|-- called by -----------
Chd|        INTTI12V                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I12VIT3(NSN   , NMN,
     2 A     ,IRECT ,
     3 CRST  ,MSR   , NSV   , IRTL,
     4 V     , MS   ,WEIGHT , MMASS ,TAGKINE,
     5 SKEW  , WA    ,TETS   ,TETM,ILEV,IREF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*),TAGKINE(*),
     .   ILEV,IREF
      my_real
     .   A(*), CRST(2,*), V(*),MS(*), MMASS(*),WA(6,*),
     .   SKEW(LSKEW,*),TETS(*),TETM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, ISK, ICOD, II, L, JJ,
     .   NN,JL
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,SP,SM,TP,TM,MAS,
     .   P(9),AXR,AYR,AZR,CST,SST
C-----------------------------------------------
      NIR=2
      IF(N2D.EQ.0)NIR=4
      IF(ILEV.EQ.1)THEN
        DO I=1,9
          P(I)=SKEW(I,IREF+1)
        ENDDO
        DO II=1,NMN
          J=MSR(II)
          J3=3*J
          J2=J3-1
          J1=J2-1
          CST=COS(TETM(II))
          SST=SIN(TETM(II))

          AMX=A(J1)
          AMY=A(J2)
          AMZ=A(J3)
          AXR=AMX*P(1)+AMY*P(2)+AMZ*P(3)
          AYR=AMX*P(4)+AMY*P(5)+AMZ*P(6)
          AZR=AMX*P(7)+AMY*P(8)+AMZ*P(9)
          WA(1,II)=AXR
          WA(2,II)=    AYR*CST+AZR*SST
          WA(3,II)=   -AYR*SST+AZR*CST

          AMX=V(J1)
          AMY=V(J2)
          AMZ=V(J3)
          AXR=AMX*P(1)+AMY*P(2)+AMZ*P(3)
          AYR=AMX*P(4)+AMY*P(5)+AMZ*P(6)
          AZR=AMX*P(7)+AMY*P(8)+AMZ*P(9)
          WA(4,II)=AXR
          WA(5,II)=    AYR*CST+AZR*SST
          WA(6,II)=   -AYR*SST+AZR*CST
        ENDDO
      ENDIF

      DO II=1,NSN
        IF(TAGKINE(II).LT.0)CYCLE
        I=NSV(II)
        L=IRTL(II)
        SS=CRST(1,II)
        TT=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + TT)
        TM=FOURTH*(ONE - TT)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
        I3=3*I
        I2=I3-1
        I1=I2-1
        AMX=ZERO
        AMY=ZERO
        AMZ=ZERO
        VMX=ZERO
        VMY=ZERO
        VMZ=ZERO
        IF(ILEV.EQ.1)THEN
          IF(TETS(II).LT.10000.)THEN
            DO JJ=1,NIR
              J=IRECT(JJ,L)
              AMX=AMX+WA(1,J)*H(JJ)
              AMY=AMY+WA(2,J)*H(JJ)
              AMZ=AMZ+WA(3,J)*H(JJ)
              VMX=VMX+WA(4,J)*H(JJ)
              VMY=VMY+WA(5,J)*H(JJ)
              VMZ=VMZ+WA(6,J)*H(JJ)
            ENDDO
            CST=COS(TETS(II))
            SST=SIN(TETS(II))
            AXR=AMX
            AYR=    AMY*CST-AMZ*SST
            AZR=    AMY*SST+AMZ*CST
            AMX=AXR*P(1)+AYR*P(4)+AZR*P(7)
            AMY=AXR*P(2)+AYR*P(5)+AZR*P(8)
            AMZ=AXR*P(3)+AYR*P(6)+AZR*P(9)
            AXR=VMX
            AYR=    VMY*CST-VMZ*SST
            AZR=    VMY*SST+VMZ*CST
            VMX=AXR*P(1)+AYR*P(4)+AZR*P(7)
            VMY=AXR*P(2)+AYR*P(5)+AZR*P(8)
            VMZ=AXR*P(3)+AYR*P(6)+AZR*P(9)
          ELSE
            DO JJ=1,NIR
              J=MSR(IRECT(JJ,L))
              J3=3*J
              J2=J3-1
              J1=J2-1
              AMX=AMX+A(J1)*H(JJ)
              AMY=AMY+A(J2)*H(JJ)
              AMZ=AMZ+A(J3)*H(JJ)
              VMX=VMX+V(J1)*H(JJ)
              VMY=VMY+V(J2)*H(JJ)
              VMZ=VMZ+V(J3)*H(JJ)
            ENDDO
          ENDIF !(TETS(II).LT.10000.)
        ELSE
          DO JJ=1,NIR
            J=MSR(IRECT(JJ,L))
            J3=3*J
            J2=J3-1
            J1=J2-1
            AMX=AMX+A(J1)*H(JJ)
            AMY=AMY+A(J2)*H(JJ)
            AMZ=AMZ+A(J3)*H(JJ)
            VMX=VMX+V(J1)*H(JJ)
            VMY=VMY+V(J2)*H(JJ)
            VMZ=VMZ+V(J3)*H(JJ)
          ENDDO
        ENDIF
        A(I1)=AMX
        A(I2)=AMY
        A(I3)=AMZ
        V(I1)=VMX
        V(I2)=VMY
        V(I3)=VMZ
      END DO !II=1,NSN

      DO II=1,NMN
        J=MSR(II)
        MS(J)=MMASS(II)
      ENDDO

      RETURN
      END

