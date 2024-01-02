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
Chd|  SURFTAGE                      source/groups/surftage.F      
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SURFTAGE(NUMEL,IX,NIX,NIX1,NIX2,IELTYP,
     .                    TAGBUF,ISU,NSEG,FLAG,
     .                    INDX,NINDX,NSEG_TOT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,IX(NIX,*),NIX1,NIX2,NUMEL,IELTYP,
     .      TAGBUF(*),NSEG,FLAG,INDX(*),NINDX,NSEG_TOT
!
      TYPE (SURF_) :: ISU
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,K,II,INV4(4),INV3(3)
      DATA INV4/4,3,2,1/
      DATA INV3/3,2,1/
C=======================================================================
       NSEG=NSEG+NINDX
!
       IF (FLAG == 1) THEN
         DO II = 1, NINDX
            JJ=INDX(II)
            NSEG_TOT = NSEG_TOT + 1
            IF(TAGBUF(JJ) == 1)THEN
              DO K=NIX1,NIX2
                ISU%NODES(NSEG_TOT,K-1) = IX(K,JJ)
              ENDDO
            ELSEIF(TAGBUF(JJ) == -1)THEN
              IF (IELTYP == 3) THEN
                DO K=NIX2,NIX1,-1
                  ISU%NODES(NSEG_TOT,INV4(K-1)) = IX(K,JJ)
                ENDDO
               ELSEIF (IELTYP == 7) THEN
                DO K=NIX2,NIX1,-1
                  ISU%NODES(NSEG_TOT,INV3(K-1)) = IX(K,JJ)
                ENDDO
               ENDIF ! IF (IELTYP == 3)
            ENDIF
            IF(NIX2-NIX1 == 2)THEN
              ISU%NODES(NSEG_TOT,4) = ISU%NODES(NSEG_TOT,3)
            ENDIF
            ISU%ELTYP(NSEG_TOT) = IELTYP
            ISU%ELEM(NSEG_TOT)  = JJ
         ENDDO
       ENDIF
C-----------
      RETURN
      END
