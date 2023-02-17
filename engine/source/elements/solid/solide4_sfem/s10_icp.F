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
Chd|  S10_ICP                       source/elements/solid/solide4_sfem/s10_icp.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|        JACOB_J33                     source/elements/solid/solide8e/s8edefo3.F
Chd|====================================================================
      SUBROUTINE S10_ICP(OFFG,JACG,FACP,NU,
     .                   DXX, DXY, DXZ,
     .                   DYX, DYY, DYZ,
     .                   DZX, DZY, DZZ,
     .                   VOL0,VOLN,VOL0DP,VOLDP,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: OFFG,VOL0
      DOUBLE PRECISION , DIMENSION(NEL) ,INTENT(IN) :: VOL0DP
      my_real ,DIMENSION(MVSIZ) ,INTENT(IN) :: JACG,FACP,NU
      my_real ,DIMENSION(MVSIZ) ,INTENT(INOUT) :: VOLN,
     .   DXX, DXY, DXZ,
     .   DYX, DYY, DYZ,
     .   DZX, DZY, DZZ
      DOUBLE PRECISION , DIMENSION(NEL) ,INTENT(INOUT) :: VOLDP
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K
      my_real
     .   JAC(MVSIZ),FAC,BASE,JFAC

C----------------------------
      IF(ISMSTR==1.OR.ISMSTR>=10)THEN
        IF (ISMSTR==10) THEN
          CALL JACOB_J33(JAC,
     .                DXX, DXY, DXZ,
     .                DYX, DYY, DYZ,
     .                DZX, DZY, DZZ)
          DO I=1,NEL
           IF(OFFG(I)==ZERO.OR.ABS(OFFG(I))>ONE) CYCLE
           FAC=TWO_THIRD*NU(I)
           BASE = JACG(I)/MAX(EM20,JAC(I))
           JFAC =EXP(FAC*LOG(MAX(EM20,BASE)))
           DXX(I) = JFAC*DXX(I)+JFAC-ONE
           DYY(I) = JFAC*DYY(I)+JFAC-ONE
           DZZ(I) = JFAC*DZZ(I)+JFAC-ONE
           DXY(I) = JFAC*DXY(I)
           DYX(I) = JFAC*DYX(I)
           DZX(I) = JFAC*DZX(I)
           DXZ(I) = JFAC*DXZ(I)
           DYZ(I) = JFAC*DYZ(I)
           DZY(I) = JFAC*DZY(I)
           VOLN(I) = JFAC*VOLN(I)             
          ENDDO
        END IF
      ELSE
c-------------------------------------------------------------------------
       IF(IRESP==1)THEN
        DO I=1,NEL
         IF(OFFG(I)==ZERO.OR.ABS(OFFG(I))>ONE) CYCLE
         VOLDP(I) = (ONE-FACP(I))*VOLDP(I)+FACP(I)*JACG(I)*VOL0DP(I)             
         VOLN(I) = VOLDP(I) 
        ENDDO
       ELSE
        DO I=1,NEL
         IF(OFFG(I)==ZERO.OR.ABS(OFFG(I))>ONE) CYCLE
         VOLN(I) = (ONE-FACP(I))*VOLN(I)+FACP(I)*JACG(I)*VOL0(I) 
        ENDDO
       ENDIF
      END IF !(ISMSTR==1.OR.ISMSTR>=10)THEN
C       
      RETURN
      END SUBROUTINE S10_ICP
