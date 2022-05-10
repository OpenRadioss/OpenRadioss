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
Chd|  INGRBRIC                      source/interfaces/interf1/ingrbric.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INGRBRIC(MSN,NOINT,BRICS,ITAB,MSV, IXS, NBRIC, PM,S_MSV)
C-----------------------------------------------------------------------
C This subroutine lists all brick in the /GRBRICK 
C (for example it is used with /inter/type22 which recquires a grbrick_id)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN)    :: NOINT, NBRIC,S_MSV
      INTEGER , INTENT(IN)    :: BRICS(NBRIC), ITAB(NUMNOD), IXS(NIXS,NUMELS)
      INTEGER , INTENT(INOUT) :: MSN, MSV(S_MSV)
      my_real , INTENT(IN)    :: PM(NPROPM,NUMMAT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                 :: I, IMAT, IALE_, IF_ALE, IF_EUL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      MSN       = NBRIC
      I22_ALEUL = 0
      IF_ALE    = 0
      IF_EUL    = 0
      DO I=1,NBRIC	  
        MSV(I) = BRICS(I)
      END DO
      
      IF(INT22.NE.0)THEN
        DO I=1,NBRIC
          IMAT  = IXS(1,MSV(I))
          IALE_ = PM(72,IMAT)
          IF(IALE_.EQ.1)IF_ALE=1
          IF(IALE_.EQ.2)IF_EUL=1
        ENDDO
        IF(IF_ALE.EQ.1)THEN
          I22_ALEUL = 1
        ELSEIF(IF_EUL.EQ.1)THEN
          I22_ALEUL = 2
        ELSE
          I22_ALEUL = 0
        ENDIF
      ENDIF
      
      IF(IPRI.GE.1) THEN
        WRITE(IOUT,'(/,A,/)')   ' BRICKS USED FOR FLUID DOMAIN DEFINITION'
        IF (NBRIC.GT.0)THEN
          WRITE(IOUT,FMT=FMW_10I)(IXS(11,MSV(I)),I=1,MSN)
        ELSE
          WRITE(IOUT,FMT='(A)') ' empty !'
        ENDIF
      ENDIF
C------------------------------------------------------------
      RETURN
      END
