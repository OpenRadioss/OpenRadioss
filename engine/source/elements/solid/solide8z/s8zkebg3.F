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
Chd|  S8ZKEBG3                      source/elements/solid/solide8z/s8zlke3.F
Chd|-- called by -----------
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEBG3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, 
     .   BXYI, BYXI ,BXZI,BZXI ,BYZI,BZYI,
     .   BXYJ, BYXJ ,BXZJ,BZXJ ,BYZJ,BZYJ, 
     .   PXYI, PYXI ,PXZI,PZXI ,PYZI,PZYI,
     .   PXYJ, PYXJ ,PXZJ,PZXJ ,PYZJ,PZYJ, DG, KIJ,IS,ICP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,IS,ICP
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),  
     .   BXYI(*), BYXI(*) ,BXZI(*),BZXI(*) ,BYZI(*),BZYI(*),
     .   BXYJ(*), BYXJ(*) ,BXZJ(*),BZXJ(*) ,BYZJ(*),BZYJ(*), 
     .   PXYI(*), PYXI(*) ,PXZI(*),PZXI(*) ,PYZI(*),PZYI(*),
     .   PXYJ(*), PYXJ(*) ,PXZJ(*),PZXJ(*) ,PYZJ(*),PZYJ(*), 
     .   DG(3,3,*), KIJ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP
C     REAL
      my_real
     .   BI(3,3,MVSIZ),BJ(3,3,MVSIZ),CI(3,3,MVSIZ),CJ(3,3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IS==1) THEN
       IF (ICP==1) THEN
        DO I=LFT,LLT
         BI(1,1,I)=PXI(I)
         BI(2,2,I)=PYI(I)
         BI(3,3,I)=PZI(I)
         BI(1,2,I)=ZERO
         BI(2,1,I)=ZERO
         BI(1,3,I)=ZERO
         BI(3,1,I)=ZERO
         BI(2,3,I)=ZERO
         BI(3,2,I)=ZERO
        ENDDO
       ELSE
        DO I=LFT,LLT
         BI(1,1,I)=PXI(I)
         BI(2,2,I)=PYI(I)
         BI(3,3,I)=PZI(I)
         BI(1,2,I)=BYXI(I)
         BI(2,1,I)=BXYI(I)
         BI(1,3,I)=BZXI(I)
         BI(3,1,I)=BXZI(I)
         BI(2,3,I)=BZYI(I)
         BI(3,2,I)=BYZI(I)
        ENDDO
       ENDIF!.. icp =1
       DO I=LFT,LLT
        CI(1,1,I)=PXYI(I)
        CI(1,2,I)=PYXI(I)
        CI(1,3,I)=ZERO
        CI(2,1,I)=ZERO
        CI(2,2,I)=PYZI(I)
        CI(2,3,I)=PZYI(I)
        CI(3,1,I)=PXZI(I)
        CI(3,2,I)=ZERO
        CI(3,3,I)=PZXI(I)
       ENDDO
C------use tempo CJ----
       DO I=1,3 
       DO J=1,3 
        DO EP=LFT,LLT
          CJ(I,J,EP)=ZERO
        ENDDO
       ENDDO
       ENDDO
C       
       DO I=1,3 
       DO J=1,3 
        DO EP=LFT,LLT
          CJ(I,J,EP)=CJ(I,J,EP)+BI(1,I,EP)*(DG(1,1,EP)*CI(1,J,EP)+
     1                  DG(1,2,EP)*CI(2,J,EP)+DG(1,3,EP)*CI(3,J,EP))+
     2                BI(2,I,EP)*(DG(2,1,EP)*CI(1,J,EP)+
     3                  DG(2,2,EP)*CI(2,J,EP)+DG(2,3,EP)*CI(3,J,EP))+
     4                BI(3,I,EP)*(DG(3,1,EP)*CI(1,J,EP)+
     5                  DG(3,2,EP)*CI(2,J,EP)+DG(3,3,EP)*CI(3,J,EP))
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3 
       DO J=I,3 
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+CJ(I,J,EP)+CJ(J,I,EP)
        ENDDO
       ENDDO
       ENDDO
      ELSE ! IS = 0 -- non-symmetry
       IF (ICP==1) THEN
        DO I=LFT,LLT
         BI(1,1,I)=PXI(I)
         BI(2,2,I)=PYI(I)
         BI(3,3,I)=PZI(I)
         BI(1,2,I)=ZERO
         BI(2,1,I)=ZERO
         BI(1,3,I)=ZERO
         BI(3,1,I)=ZERO
         BI(2,3,I)=ZERO
         BI(3,2,I)=ZERO
        ENDDO
        DO I=LFT,LLT
         BJ(1,1,I)=PXJ(I)
         BJ(2,2,I)=PYJ(I)
         BJ(3,3,I)=PZJ(I)
         BJ(1,2,I)=ZERO
         BJ(2,1,I)=ZERO
         BJ(1,3,I)=ZERO
         BJ(3,1,I)=ZERO
         BJ(2,3,I)=ZERO
         BJ(3,2,I)=ZERO
        ENDDO
       ELSE ! .. icp = 0
        DO I=LFT,LLT
         BI(1,1,I)=PXI(I)
         BI(2,2,I)=PYI(I)
         BI(3,3,I)=PZI(I)
         BI(1,2,I)=BYXI(I)
         BI(2,1,I)=BXYI(I)
         BI(1,3,I)=BZXI(I)
         BI(3,1,I)=BXZI(I)
         BI(2,3,I)=BZYI(I)
         BI(3,2,I)=BYZI(I)
        ENDDO
        DO I=LFT,LLT
         BJ(1,1,I)=PXJ(I)
         BJ(2,2,I)=PYJ(I)
         BJ(3,3,I)=PZJ(I)
         BJ(1,2,I)=BYXJ(I)
         BJ(2,1,I)=BXYJ(I)
         BJ(1,3,I)=BZXJ(I)
         BJ(3,1,I)=BXZJ(I)
         BJ(2,3,I)=BZYJ(I)
         BJ(3,2,I)=BYZJ(I)
        ENDDO
       ENDIF
       DO I=LFT,LLT
        CI(1,1,I)=PXYI(I)
        CI(1,2,I)=PYXI(I)
        CI(1,3,I)=ZERO
        CI(2,1,I)=ZERO
        CI(2,2,I)=PYZI(I)
        CI(2,3,I)=PZYI(I)
        CI(3,1,I)=PXZI(I)
        CI(3,2,I)=ZERO
        CI(3,3,I)=PZXI(I)
       ENDDO
       DO I=LFT,LLT
        CJ(1,1,I)=PXYJ(I)
        CJ(1,2,I)=PYXJ(I)
        CJ(1,3,I)=ZERO
        CJ(2,1,I)=ZERO
        CJ(2,2,I)=PYZJ(I)
        CJ(2,3,I)=PZYJ(I)
        CJ(3,1,I)=PXZJ(I)
        CJ(3,2,I)=ZERO
        CJ(3,3,I)=PZXJ(I)
       ENDDO
       DO I=1,3 
       DO J=1,3 
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+BI(1,I,EP)*(DG(1,1,EP)*CJ(1,J,EP)+
     1                  DG(1,2,EP)*CJ(2,J,EP)+DG(1,3,EP)*CJ(3,J,EP))+
     2                BI(2,I,EP)*(DG(2,1,EP)*CJ(1,J,EP)+
     3                  DG(2,2,EP)*CJ(2,J,EP)+DG(2,3,EP)*CJ(3,J,EP))+
     4                BI(3,I,EP)*(DG(3,1,EP)*CJ(1,J,EP)+
     5                  DG(3,2,EP)*CJ(2,J,EP)+DG(3,3,EP)*CJ(3,J,EP))+
candr.. transpose of DG must be taken here
c$$$     6                CI(1,I,EP)*(DG(1,1,EP)*BJ(1,J,EP)+
c$$$     7                  DG(1,2,EP)*BJ(2,J,EP)+DG(1,3,EP)*BJ(3,J,EP))+
c$$$     8                CI(2,I,EP)*(DG(2,1,EP)*BJ(1,J,EP)+
c$$$     9                  DG(2,2,EP)*BJ(2,J,EP)+DG(2,3,EP)*BJ(3,J,EP))+
c$$$     A                CI(3,I,EP)*(DG(3,1,EP)*BJ(1,J,EP)+
c$$$     B                  DG(3,2,EP)*BJ(2,J,EP)+DG(3,3,EP)*BJ(3,J,EP))
     6                CI(1,I,EP)*(DG(1,1,EP)*BJ(1,J,EP)+
     7                  DG(2,1,EP)*BJ(2,J,EP)+DG(3,1,EP)*BJ(3,J,EP))+
     8                CI(2,I,EP)*(DG(1,2,EP)*BJ(1,J,EP)+
     9                  DG(2,2,EP)*BJ(2,J,EP)+DG(3,2,EP)*BJ(3,J,EP))+
     A                CI(3,I,EP)*(DG(1,3,EP)*BJ(1,J,EP)+
     B                  DG(2,3,EP)*BJ(2,J,EP)+DG(3,3,EP)*BJ(3,J,EP))

        ENDDO
       ENDDO
       ENDDO
      ENDIF
C 
      RETURN
      END
