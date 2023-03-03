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
C
Chd|====================================================================
Chd|  ALESO2                        source/ale/ale2d/aleso2.F     
Chd|-- called by -----------
Chd|        ALELEC                        source/ale/alelec.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALESO2(ALE_CONNECTIVITY,PM,IXQ,IGEO)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is displaying in Starter listing file
C  element/element connectivities (ALE)
C  specific case of quads (4-node-solid-elems in 2D) : buffer is IXQ contrary to IXTG for triangles
C Precondition : IPRI >= 3 (/IOFLAG Starter option)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IXQ(7,NUMELQ),IGEO(NPROPGI,NUMGEO)
      my_real,INTENT(IN) :: PM(NPROPM,NUMMAT)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IVPR(4), M, IAL, I, IAD1, LGTH
      INTEGER JALE_FROM_MAT, JALE_FROM_PROP, JTHE_FROM_MAT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(IPRI <= 3)THEN
        WRITE(IOUT,1200)
        DO M=1,NUMELQ
          JTHE_FROM_MAT = NINT(PM(71,IABS(IXQ(1,M)))) !/HEAT/MAT
          JALE_FROM_MAT = NINT(PM(72,IABS(IXQ(1,M)))) !/EULER/MAT or /ALE/MAT
          JALE_FROM_PROP = IGEO(62,IABS(IXQ(6,M)))    !/PROP/TYPE14 (IALE_FLAG)
          IAL = JTHE_FROM_MAT + JALE_FROM_MAT + JALE_FROM_PROP
          IF(IAL == 0)CYCLE
          IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(M)
          LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(M+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(M)        
          DO I=1,LGTH
            IVPR(I)=ALE_CONNECTIVITY%ee_connect%connected(IAD1 + I - 1)
            IF(IVPR(I) /= 0)IVPR(I)=IXQ(7,IVPR(I))
          ENDDO
          WRITE(IOUT,'(5I10)')IXQ(7,M),(IVPR(I),I=1,4)
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
C-----------------------------------------------    
 1200 FORMAT(//
     & 6X,'A.L.E.  ELEMENT CONNECTIVITIES                ',/
     & 6X,'------------------------------                ',/
     & 6X,'ELEM      ELE1      ELE2      ELE3      ELE4  ')
      END
