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
Chd|  R3COORK3                      source/elements/spring/r3coork3.F
Chd|-- called by -----------
Chd|        R12KE3                        source/elements/spring/r12ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R3COORK3(JFT    ,JLT    ,X      ,NCC   ,PID    ,
     1                    NGL    ,R11    ,R21   ,R31    ,R12    ,
     2                    R22    ,R32   ,BETA   ,OFFG   ,OFF    )
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
#include      "impl1_c.inc"
#include      "comlock.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT 
      INTEGER NCC(NIXR,*),PID(*),NGL(*)
C     REAL
      my_real
     .   X(3,*),BETA(*),OFFG(*),OFF(*),
     .   R11(*),R21(*),R31(*),R12(*),R22(*),R32(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ),X3(MVSIZ),Y1(MVSIZ), Y2(MVSIZ),  
     .   Y3(MVSIZ),Z1(MVSIZ), Z2(MVSIZ),Z3(MVSIZ), 
     .   AL1(MVSIZ),AL2(MVSIZ),NORM
C-----------------------------------------------
C
      DO I=JFT, JLT
       PID(I)=NCC(1,I)
       NGL(I)=NCC(5,I)
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
       R11(I)=ONE
       R21(I)=ZERO
       R31(I)=ZERO
       R12(I)=ONE
       R22(I)=ZERO
       R32(I)=ZERO
      ENDDO
C 
      DO  I=JFT, JLT
       R11(I)=X2(I)-X1(I)
       R21(I)=Y2(I)-Y1(I)
       R31(I)=Z2(I)-Z1(I)
       AL1(I)=SQRT(R11(I)*R11(I)+R21(I)*R21(I)+R31(I)*R31(I))
       AL1(I)=MAX(EM15,AL1(I))
      ENDDO 
C
      DO I=JFT, JLT
       IF (AL1(I)>EM15) THEN
        NORM=ONE/AL1(I)
        R11(I)=R11(I)*NORM
        R21(I)=R21(I)*NORM
        R31(I)=R31(I)*NORM
       ENDIF 
      ENDDO 
C
      DO  I=JFT, JLT
       R12(I)=X2(I)-X3(I)
       R22(I)=Y2(I)-Y3(I)
       R32(I)=Z2(I)-Z3(I)
       AL2(I)=SQRT(R12(I)*R12(I)+R22(I)*R22(I)+R32(I)*R32(I))
       AL2(I)=MAX(EM15,AL2(I))
      ENDDO 
C
      DO I=JFT, JLT
       IF (AL2(I)>EM15) THEN
        NORM=ONE/AL2(I)
        R12(I)=R12(I)*NORM
        R22(I)=R22(I)*NORM
        R32(I)=R32(I)*NORM
       ENDIF 
      ENDDO 
      IF (IMP_CHK > 0) THEN
       DO I=JFT,JLT
        IF(OFFG(I)/=ZERO)THEN
         IF(AL1(I)<=EM15)THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IW = IMP_IW + 1
         ENDIF 
         IF(AL2(I)<=EM15)THEN
#include "lockon.inc"
            WRITE(IOUT ,2002) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IW = IMP_IW + 1
         ENDIF 
        ENDIF 
       ENDDO
      ENDIF 
C
      DO I=JFT, JLT
       BETA(I) = PI - ACOS(R11(I)*R12(I)+R21(I)*R22(I)+R31(I)*R32(I))
      ENDDO 
C
      DO I=JFT,JLT
       OFF(I)=OFFG(I)
      ENDDO
C
      RETURN
 2001 FORMAT(/'***WARNING :SPRING TYPE12 ZERO-LENGTH N1N2: ELE. NB:',
     .          I8/)
 2002 FORMAT(/'***WARNING :SPRING TYPE12 ZERO-LENGTH N2N3: ELE. NB:',
     .          I8/)
      END
