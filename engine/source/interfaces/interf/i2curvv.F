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
Chd|  I2CURVV                       source/interfaces/interf/i2curvv.F
Chd|-- called by -----------
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- calls ---------------
Chd|        I2CURV_REP                    source/interfaces/interf/i2curv_rep.F
Chd|        I2_FFORM                      source/interfaces/interf/i2_fform.F
Chd|        INV3                          source/elements/joint/rskew33.F
Chd|====================================================================
      SUBROUTINE I2CURVV(
     .           NSN     ,NMN     ,MS      ,V       ,A       ,
     .           AR      ,VR      ,X       ,IRECT   ,NSV     ,
     .           MSR     ,IRTL    ,CRST    ,WEIGHT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .   A(3,*),V(3,*),AR(3,*),VR(3,*),X(3,*),MS(*),CRST(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR,I,J,L,II,JJ,IS
      INTEGER INOD(4)
C     REAL
      my_real
     .   LS1,LS2,LT1,LT2,LS,LT,S,T
      my_real
     .   VS(6),AS(6),VM(6,4),AM(6,4),ROT(9),ROTI(9),
     .   H(4),HH(4),HRS(4),HRT(4),HPS(4),HPT(4),HPRS(4),HPRT(4),
     .   HXS(4),HXT(4)
C=======================================================================
      DO II=1,NSN
        IS=NSV(II)
        IF (IS > 0) THEN
          L=IRTL(II)
          NIR = 4
          DO JJ=1,NIR
            INOD(JJ) = IRECT(JJ,L)
          ENDDO
C---
          CALL I2CURV_REP(INOD    , X      ,V       ,LS1     ,LS2     ,
     .                    LT1     ,LT2     ,ROT(1)  ,ROT(4)  ,ROT(7)  ,
     .                    ROT(2)  ,ROT(5)  ,ROT(8)  ,ROT(3)  ,ROT(6)  ,
     .                    ROT(9)  )                                    
          CALL INV3(ROT,ROTI)
C---
          S=CRST(1,II)
          T=CRST(2,II)
          CALL I2_FFORM(
     .         NIR,S,T,H,HH,HRS,HRT,HPS,HPT,HPRS,HPRT,
     .         HXS,HXT,LS1,LS2,LT1,LT2,LS,LT)
C---
          DO JJ=1,NIR
            J=INOD(JJ)
            VM(1,JJ) = ROT(1)*V(1,J) + ROT(4)*V(2,J) + ROT(7)*V(3,J)
            VM(2,JJ) = ROT(2)*V(1,J) + ROT(5)*V(2,J) + ROT(8)*V(3,J)
            VM(3,JJ) = ROT(3)*V(1,J) + ROT(6)*V(2,J) + ROT(9)*V(3,J)
            AM(1,JJ) = ROT(1)*A(1,J) + ROT(4)*A(2,J) + ROT(7)*A(3,J)
            AM(2,JJ) = ROT(2)*A(1,J) + ROT(5)*A(2,J) + ROT(8)*A(3,J)
            AM(3,JJ) = ROT(3)*A(1,J) + ROT(6)*A(2,J) + ROT(9)*A(3,J)
            VM(4,JJ) = ROT(1)*VR(1,J)+ROT(4)*VR(2,J)+ROT(7)*VR(3,J)  
            VM(5,JJ) = ROT(2)*VR(1,J)+ROT(5)*VR(2,J)+ROT(8)*VR(3,J)  
            VM(6,JJ) = ROT(3)*VR(1,J)+ROT(6)*VR(2,J)+ROT(9)*VR(3,J)  
            AM(4,JJ) = ROT(1)*AR(1,J)+ROT(4)*AR(2,J)+ROT(7)*AR(3,J)  
            AM(5,JJ) = ROT(2)*AR(1,J)+ROT(5)*AR(2,J)+ROT(8)*AR(3,J)  
            AM(6,JJ) = ROT(3)*AR(1,J)+ROT(6)*AR(2,J)+ROT(9)*AR(3,J)  
          ENDDO
C---
          VS = ZERO                  
          AS = ZERO                  
          DO JJ=1,NIR                                    
            VS(1) = VS(1) + H(JJ) * VM(1,JJ)                 
            VS(2) = VS(2) + H(JJ) * VM(2,JJ)                 
            VS(3) = VS(3) + HH(JJ) *VM(3,JJ)
     .                    + HRS(JJ)*VM(4,JJ) + HRT(JJ) *VM(5,JJ)
            VS(4) = VS(4) + HPS(JJ)*VM(3,JJ) + HPRS(JJ)*VM(4,JJ)
     .                                       + HXS(JJ) *VM(5,JJ)     
            VS(5) = VS(5) + HPT(JJ)*VM(3,JJ) + HPRT(JJ)*VM(5,JJ)     
     .                                       + HXT(JJ) *VM(4,JJ)     
            VS(6) = VS(6) + H(JJ) * VM(6,JJ)                      
C
            AS(1) = AS(1) + H(JJ) * AM(1,JJ)                 
            AS(2) = AS(2) + H(JJ) * AM(2,JJ)                 
            AS(3) = AS(3) + HH(JJ) *AM(3,JJ)
     .                    + HRS(JJ)*AM(4,JJ)               
     .                    + HRT(JJ)*AM(5,JJ)
            AS(4) = AS(4) + HPS(JJ)*AM(3,JJ) + HPRS(JJ)*AM(4,JJ)     
     .                                       + HXS(JJ) *AM(5,JJ)     
            AS(5) = AS(5) + HPT(JJ)*AM(3,JJ) + HPRT(JJ)*AM(5,JJ)     
     .                                       + HXT(JJ) *AM(4,JJ)     
            AS(6) = AS(6) + H(JJ) * AM(6,JJ)                      
          ENDDO    
C---
          A(1,IS) = ROTI(1)*AS(1)+ROTI(4)*AS(2)+ROTI(7)*AS(3) 
          A(2,IS) = ROTI(2)*AS(1)+ROTI(5)*AS(2)+ROTI(8)*AS(3)  
          A(3,IS) = ROTI(3)*AS(1)+ROTI(6)*AS(2)+ROTI(9)*AS(3) 
          V(1,IS) = ROTI(1)*VS(1)+ROTI(4)*VS(2)+ROTI(7)*VS(3)
          V(2,IS) = ROTI(2)*VS(1)+ROTI(5)*VS(2)+ROTI(8)*VS(3)
          V(3,IS) = ROTI(3)*VS(1)+ROTI(6)*VS(2)+ROTI(9)*VS(3)
          AR(1,IS)=ROTI(1)*AS(4)+ROTI(4)*AS(5)+ROTI(7)*AS(6)   
          AR(2,IS)=ROTI(2)*AS(4)+ROTI(5)*AS(5)+ROTI(8)*AS(6)   
          AR(3,IS)=ROTI(3)*AS(4)+ROTI(6)*AS(5)+ROTI(9)*AS(6)   
          VR(1,IS)=ROTI(1)*VS(4)+ROTI(4)*VS(5)+ROTI(7)*VS(6)   
          VR(2,IS)=ROTI(2)*VS(4)+ROTI(5)*VS(5)+ROTI(8)*VS(6)   
          VR(3,IS)=ROTI(3)*VS(4)+ROTI(6)*VS(5)+ROTI(9)*VS(6)   
C
        ENDIF
      ENDDO
C-----------
      RETURN
      END
