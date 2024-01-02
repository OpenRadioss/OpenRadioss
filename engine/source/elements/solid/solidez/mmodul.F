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
Chd|  MMODUL                        source/elements/solid/solidez/mmodul.F
Chd|-- called by -----------
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3_or.F
Chd|-- calls ---------------
Chd|        C33STIF2EL                    source/elements/solid/solidez/c33stif2el.F
Chd|        GETTRANSV                     source/elements/solid/solidez/gettransv.F
Chd|        MSTIFORTHV                    source/elements/solid/solidez/mstiforthv.F
Chd|====================================================================
      SUBROUTINE MMODUL(JFT     ,JLT     ,PM    ,MAT    ,MTN    ,
     .                  GAMA    ,UPARAM  ,CC    ,CG     ,G33    )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  ,MTN
      INTEGER MAT(*)
C     REAL
      my_real
     .   PM(NPROPM,*),CC(MVSIZ,3,3),UPARAM(*),
     .   GAMA(MVSIZ,6),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IAD,J,K,ipr,NBDAMA
C     REAL
      my_real
     .   NU,LAMDA,GG,C1,QC(MVSIZ,9),QCG(MVSIZ,9),QG(MVSIZ,9),
     .   QGC(MVSIZ,9),G3(MVSIZ,3),TT,TV,CA,CB,CN,
     .   S1,NU12,NU21,EFAC,ARMA,ARM1,ARM2,ARM3,ET24
      my_real
     .   CC24(MVSIZ,3,3),C3(MVSIZ,3)
C-----------------------------------------------
       CALL GETTRANSV(JFT,JLT,GAMA,QC,QCG,QGC,QG)
       IF (MTN==14.OR.MTN==12) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(I,1,1) =PM(40,MX)
         CC(I,2,2) =PM(43,MX)
         CC(I,3,3) =PM(45,MX)
         CC(I,1,2) =PM(41,MX)
         CC(I,2,3) =PM(44,MX)
         CC(I,1,3) =PM(42,MX)
         G3(I,1) =PM(46,MX)
         G3(I,2) =PM(47,MX)
         G3(I,3) =PM(48,MX)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN==24) THEN
        MX  =MAT(1)
C-------ET24 for the moment is elastic, could be set more precisely     
        ET24 = PM(50,MX)
        ARM1 = PM(53,MX) 
        ARM2 = PM(54,MX) 
        ARM3 = PM(55,MX) 
        ARMA = ARM1 + ARM2 + ARM3
        GG  = PM(22,MX)
        TT  = PM(24,MX)
        TV  = PM(25,MX)
C-------damages are negliged for increment (consisting to isotropic case )
C-----will apply the dommage to generalized stress        
        DO I=JFT,JLT
         CC(I,1,1) = TT
         CC(I,2,2) = TT
         CC(I,3,3) = TT
         CC(I,1,2) = TV
         CC(I,2,3) = TV
         CC(I,1,3) = TV
         CC(I,2,1) = TV
         CC(I,3,2) = TV
         CC(I,3,1) = TV
c
         G3 (I,1:3)= GG
        END DO 
        IF (ARMA>ZERO) THEN
C--------assemble the two parts in reinforced system   
         DO I=JFT,JLT
          C3(I,1) =(ONE-ARM1)*CC(I,1,1)+ET24*ARM1
          C3(I,2) =(ONE-ARM2)*CC(I,2,2)+ET24*ARM2
          C3(I,3) =(ONE-ARM3)*CC(I,3,3)+ET24*ARM3
         ENDDO
C--------translate C3 to elem sys--(CC24,G33   ,CG)
        CALL C33STIF2EL(JLT   ,QC   ,QCG   ,QGC    ,QG     ,
     .                  C3    ,CC24 ,G33   ,CG     ) 
C--------final assemblage ---- 
         DO J = 1,3
          DO I=JFT,JLT
           CC(I,J,J) =CC24(I,J,J)
           G33(I,J,J)=G33(I,J,J)+G3(I,J)
          ENDDO
         ENDDO
          DO I=JFT,JLT
           CC(I,1,2) = CC(I,1,2) + CC24(I,1,2)
           CC(I,1,3) = CC(I,1,3) + CC24(I,1,3)
           CC(I,2,3) = CC(I,2,3) + CC24(I,2,3)
           CC(I,2,1) = CC(I,1,2) 
           CC(I,3,1) = CC(I,1,3) 
           CC(I,3,2) = CC(I,2,3) 
          ENDDO
        ELSE
C----- w/o armature :--> isotropic      
         DO I=JFT,JLT
          G33(I,1,1) = GG
          G33(I,2,2) = G33(I,1,1)
          G33(I,3,3) = G33(I,1,1)
          G33(I,1,2) = ZERO
          G33(I,2,3) = ZERO
          G33(I,1,3) = ZERO
          G33(I,2,1) = ZERO
          G33(I,3,2) = ZERO
          G33(I,3,1) = ZERO
          CG(I,1:3,1:3)=ZERO
         ENDDO
        END IF
       ELSEIF (MTN==25) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         NU12 =PM(35,MX)
         NU21 =PM(36,MX)
         S1 = ONE-NU12*NU21
         CC(I,1,1) =PM(33,MX)/MAX(EM20,S1)
         CC(I,2,2) =PM(34,MX)/MAX(EM20,S1)
         CC(I,3,3) =PM(186,MX)
         CC(I,1,2) =HALF*(NU21*CC(I,1,1)+NU12*CC(I,2,2))
         CC(I,2,3) =ZERO
         CC(I,1,3) =ZERO
         G3(I,1) =PM(37,MX)
         G3(I,2) =PM(38,MX)
         G3(I,3) =PM(39,MX)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN==28.OR.MTN==50.OR.MTN==68) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(I,1,1) = UPARAM(1)
         CC(I,2,2) = UPARAM(2)
         CC(I,3,3) = UPARAM(3)
         CC(I,1,2) = ZERO
         CC(I,2,3) = ZERO
         CC(I,1,3) = ZERO
         G3(I,1) = UPARAM(4)
         G3(I,2) = UPARAM(5)
         G3(I,3) = UPARAM(6)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN==53) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(I,1,1) = UPARAM(1)
         CC(I,2,2) = UPARAM(2)
         CC(I,3,3) = CC(I,2,2)
         CC(I,1,2) = ZERO
         CC(I,2,3) = ZERO
         CC(I,1,3) = ZERO
         G3(I,1) = UPARAM(3)
         G3(I,2) = UPARAM(4)
         G3(I,3) = G3(I,1)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN == 93) THEN     
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(I,1,1) = UPARAM(4)
         CC(I,2,2) = UPARAM(7)
         CC(I,3,3) = UPARAM(9)
         CC(I,1,2) = UPARAM(5)
         CC(I,2,3) = UPARAM(8)
         CC(I,1,3) = UPARAM(6)
         G3(I,1)   = UPARAM(10)
         G3(I,2)   = UPARAM(11)
         G3(I,3)   = UPARAM(12)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     )        
       ELSEIF (MTN==107) THEN
        DO I=JFT,JLT
          MX = MAT(I)
          NU12 = UPARAM(4)
          NU21 = UPARAM(5)
          S1 = ONE-NU12*NU21
          CC(I,1,1) =UPARAM(1)/MAX(EM20,S1)
          CC(I,2,2) =UPARAM(2)/MAX(EM20,S1)
          CC(I,3,3) =UPARAM(3)
          CC(I,1,2) =HALF*(NU21*CC(I,1,1)+NU12*CC(I,2,2))
          CC(I,2,3) =ZERO
          CC(I,1,3) =ZERO
          G3(I,1)   = UPARAM(10)
          G3(I,2)   = UPARAM(11)
          G3(I,3)   = UPARAM(12) 
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     )
       ELSEIF (MTN==112) THEN
        DO I=JFT,JLT
          MX = MAT(I)
          NU12 = UPARAM(4)
          NU21 = UPARAM(5)
          S1 = ONE-NU12*NU21
          CC(I,1,1) =UPARAM(1)/MAX(EM20,S1)
          CC(I,2,2) =UPARAM(2)/MAX(EM20,S1)
          CC(I,3,3) =UPARAM(3)
          CC(I,1,2) =HALF*(NU21*CC(I,1,1)+NU12*CC(I,2,2))
          CC(I,2,3) =ZERO
          CC(I,1,3) =ZERO
          G3(I,1)   = UPARAM(10)
          G3(I,2)   = UPARAM(11)
          G3(I,3)   = UPARAM(12) 
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN == 122) THEN     
        DO I=JFT,JLT
          CC(I,1,1) = UPARAM(58)
          CC(I,2,2) = UPARAM(61)
          CC(I,3,3) = UPARAM(63)
          CC(I,1,2) = UPARAM(59)
          CC(I,2,3) = UPARAM(62)
          CC(I,1,3) = UPARAM(60)
          G3(I,1)   = UPARAM(10)
          G3(I,2)   = UPARAM(11)
          G3(I,3)   = UPARAM(12)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     )   
       ELSE
C----------warring out isotrope effective       
        DO I=JFT,JLT
         MX  =MAT(I)
         NU =PM(21,MX)
         C1 =THREE*PM(32,MX)/(ONE+NU)
         LAMDA=C1*NU
C-------GG:2G---HH(2,I):G-------
         GG =C1*(ONE-TWO*NU)
         CC(I,1,1) = LAMDA+GG
         CC(I,2,2) = CC(I,1,1)
         CC(I,3,3) = CC(I,1,1)
         CC(I,1,2) = LAMDA
         CC(I,2,3) = LAMDA
         CC(I,1,3) = LAMDA
         CC(I,2,1) = LAMDA
         CC(I,3,2) = LAMDA
         CC(I,3,1) = LAMDA
c
         G33(I,1,1) = GG*HALF
         G33(I,2,2) = G33(I,1,1)
         G33(I,3,3) = G33(I,1,1)
         G33(I,1,2) = ZERO
         G33(I,2,3) = ZERO
         G33(I,1,3) = ZERO
         G33(I,2,1) = ZERO
         G33(I,3,2) = ZERO
         G33(I,3,1) = ZERO
C        
         CG(I,1:3,1:3)=ZERO
        ENDDO
       ENDIF
 
C
      RETURN
      END
