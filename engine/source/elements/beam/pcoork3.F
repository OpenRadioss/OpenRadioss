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
Chd|  PCOORK3                       source/elements/beam/pcoork3.F
Chd|-- called by -----------
Chd|        PKE3                          source/elements/beam/pke3.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PCOORK3(JFT    ,JLT    ,X      ,NCC   ,OFFG  ,
     2                   PID    ,MAT    ,NGL    ,AL    ,ALI   , 
     3                   ALS    ,ISMSTR ,R11    ,R12   ,R13   ,
     4                   R21    ,R22    ,R23    ,R31   ,R32   ,
     5                   R33    ,RLOC   ,OFF    )
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
      INTEGER JFT, JLT ,ISMSTR
      INTEGER NCC(6,*),PID(*),MAT(*),NGL(*)
      my_real
     .   X(3,*),OFFG(*),AL(*),ALI(*),ALS(*),
     .   R11(*),R12(*),R13(*),
     .   R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*),RLOC(3,*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ)
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),NORM
C-----------------------------------------------
C
      DO I=JFT, JLT
       MAT(I)=NCC(1,I)
       PID(I)=NCC(5,I)
       NGL(I)=NCC(6,I)
       NC1(I)=NCC(2,I)
       NC2(I)=NCC(3,I)
       NC3(I)=NCC(4,I)
      ENDDO 
C----------------------------
C     COORDONNEES
C----------------------------
      DO I=JFT, JLT
       X1(I)=X(1,NC1(I))
       Y1(I)=X(2,NC1(I))
       Z1(I)=X(3,NC1(I))
       X2(I)=X(1,NC2(I))
       Y2(I)=X(2,NC2(I))
       Z2(I)=X(3,NC2(I))
       X3(I)=X(1,NC3(I))
       Y3(I)=X(2,NC3(I))
       Z3(I)=X(3,NC3(I))
      ENDDO 
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      DO  I=JFT, JLT
       R12(I)=RLOC(1,I)
       R22(I)=RLOC(2,I)
       R32(I)=RLOC(3,I)
       R11(I)=X2(I)-X1(I)
       R21(I)=Y2(I)-Y1(I)
       R31(I)=Z2(I)-Z1(I)
      ENDDO 
C
      IF (ISMSTR/=0) THEN
       DO  I=JFT, JLT
        AL(I)=ALS(I)
       ENDDO
      ELSE 
       DO  I=JFT, JLT
        AL(I)=SQRT(R11(I)*R11(I)+R21(I)*R21(I)+R31(I)*R31(I))
       ENDDO
      ENDIF 
      DO  I=JFT, JLT
       IF (AL(I)<EM20) OFFG(I)=ZERO
       ALI(I)=ONE/MAX(EM20,AL(I))
      ENDDO 
C
      DO I=JFT, JLT
       R11(I)=R11(I)*ALI(I)
       R21(I)=R21(I)*ALI(I)
       R31(I)=R31(I)*ALI(I)
      ENDDO 
      DO I=JFT, JLT
       R13(I)=R21(I)*R32(I)-R31(I)*R22(I)
       R23(I)=R31(I)*R12(I)-R11(I)*R32(I)
       R33(I)=R11(I)*R22(I)-R21(I)*R12(I)
      ENDDO 
C
      DO I=JFT, JLT
       NORM=SQRT(R13(I)*R13(I)+R23(I)*R23(I)+R33(I)*R33(I))
       R13(I)=R13(I)/NORM
       R23(I)=R23(I)/NORM
       R33(I)=R33(I)/NORM
      ENDDO 
      DO I=JFT, JLT
       R12(I)=R23(I)*R31(I)-R33(I)*R21(I)
       R22(I)=R33(I)*R11(I)-R13(I)*R31(I)
       R32(I)=R13(I)*R21(I)-R23(I)*R11(I)
      ENDDO 
C
      DO I=JFT,JLT
       OFF(I)=OFFG(I)
      ENDDO
C
      RETURN
      END
