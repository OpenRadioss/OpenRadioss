Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    aleso2t                ../starter/source/ale/ale2d/aleso2t.F
!||--- called by ------------------------------------------------------
!||    alelec                 ../starter/source/ale/alelec.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE ALESO2T(ALE_CONNECTIVITY, PM, IXTG, IGEO)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is displaying in Starter listing file
C  element/element connectivities (ALE)
C  specific case of triangles (3-node-solid-elems in 2D) : buffer is IXTG instead of IXQ (quads)
C Precondition : IPRI >= 3 (/IOFLAG Starter option)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
CC-----------------------------------------------
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
      INTEGER, INTENT(IN) :: IXTG(NIXTG, NUMELTG),IGEO(NPROPGI,NUMGEO)
      my_real, INTENT(IN) :: PM(NPROPM, NUMMAT)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IVPR(4), M, IAL, I, IAD1, LGTH
      INTEGER :: JTHE_FROM_MAT, JALE_FROM_MAT, JALE_FROM_PROP
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      
      IF(IPRI >= 3)THEN
        WRITE(IOUT,1200)
        DO M=1,NUMELTG
          JTHE_FROM_MAT = NINT(PM(71,IABS(IXTG(1,M)))) !/HEAT/MAT
          JALE_FROM_MAT = NINT(PM(72,IABS(IXTG(1,M)))) !/EULER/MAT or /ALE/MAT
          JALE_FROM_PROP = IGEO(62,IABS(IXTG(5,M)))    !/PROP/TYPE14 (IALE_FLAG)
          IAL = JTHE_FROM_MAT + JALE_FROM_MAT + JALE_FROM_PROP 
          IF(IAL == 0)CYCLE
          IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(M)
          LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(M+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(M)
          DO I=1,3
            IVPR(I)=ALE_CONNECTIVITY%ee_connect%connected(IAD1 + I - 1)
            IF(IVPR(I) /= 0)IVPR(I)=IXTG(NIXTG,IVPR(I))
          ENDDO
          WRITE(IOUT,'(5I10)')IXTG(NIXTG,M),(IVPR(I),I=1,4)
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
