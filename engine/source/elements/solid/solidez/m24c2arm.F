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
Chd|  M24C2ARM                      source/elements/solid/solidez/m24c2arm.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GETTRANSV                     source/elements/solid/solidez/gettransv.F
Chd|        MSTIFORTHV                    source/elements/solid/solidez/mstiforthv.F
Chd|====================================================================
      SUBROUTINE M24C2ARM(NEL    ,GAMA  ,ANGDAM ,CC     ,G3     ,
     .                    C3    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real, DIMENSION(MVSIZ,6) ,INTENT(IN):: GAMA
      my_real, DIMENSION(MVSIZ,6) ,INTENT(IN):: ANGDAM
      my_real, DIMENSION(MVSIZ,3,3) ,INTENT(IN):: CC
      my_real, DIMENSION(MVSIZ,3) ,INTENT(IN):: G3
      my_real, DIMENSION(MVSIZ,3) ,INTENT(INOUT):: C3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   QC(MVSIZ,9),QCG(MVSIZ,9),QG(MVSIZ,9),
     .   QGC(MVSIZ,9),GG33(MVSIZ,3,3),QCA(MVSIZ,6),
     .   CCA(MVSIZ,3,3),CG(MVSIZ,3,3)
      my_real
     .   G11,G21,G31,G12,G22,G32,G13,G23,G33,
     .   R11,R21,R31,R12,R22,R32,R13,R23,R33
C=======================================================================
        DO I = 1,NEL
          G11=GAMA(I,1)
          G21=GAMA(I,2)
          G31=GAMA(I,3)
          G12=GAMA(I,4)
          G22=GAMA(I,5)
          G32=GAMA(I,6)
          G13=G21*G32-G31*G22
          G23=G31*G12-G11*G32
          G33=G11*G22-G21*G12
          R11=ANGDAM(I,1)
          R21=ANGDAM(I,2)
          R31=ANGDAM(I,3)
          R12=ANGDAM(I,4)
          R22=ANGDAM(I,5)
          R32=ANGDAM(I,6)
          R13=R21*R32-R31*R22
          R23=R31*R12-R11*R32
          R33=R11*R22-R21*R12
          QCA(I,1)=R11*G11+R21*G21+R31*G31
          QCA(I,2)=R11*G12+R21*G22+R31*G32
          QCA(I,3)=R11*G13+R21*G23+R31*G33
          QCA(I,4)=R12*G11+R22*G21+R32*G31
          QCA(I,5)=R12*G12+R22*G22+R32*G32
          QCA(I,6)=R12*G13+R22*G23+R32*G33
        END DO 
       CALL GETTRANSV(1,NEL,QCA,QC,QCG,QGC,QG)
       DO I = 1,NEL
        CCA(I,1:3,1:3)= CC(I,1:3,1:3)
       END DO 
       CALL MSTIFORTHV(1  ,NEL   ,QC  ,QCG  ,QGC    ,
     .                 QG ,CCA   ,G3  ,GG33  ,CG     ) 
       DO J = 1,3
       DO I = 1,NEL
        C3(I,J)= CC(I,J,J)
       END DO 
       END DO 
c-----------
      RETURN
      END
