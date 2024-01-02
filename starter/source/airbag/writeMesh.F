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
Chd|  WRITEMESH                     source/airbag/writeMesh.F     
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE WRITEMESH(VOLID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVMBAG_MESHCONTROL_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: VOLID
C-----------------------------------------------
C     Local variable
C-----------------------------------------------
      CHARACTER(LEN=1024) :: FILENAME
      CHARACTER(LEN = 100) :: RAD_INTER, RAD_HEADER, RAD_BEGIN, RAD_END
      INTEGER :: II, N1, N2, N3, N4

      INTEGER LEN_FILNAM
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME


      RAD_HEADER = "#RADIOSS STARTER"
      RAD_INTER = "#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|"
      RAD_BEGIN = "/BEGIN"
      RAD_END = "/END"

      WRITE(FILENAME, "(A6,I4.4,A4)") "MONVOL",VOLID,".inc"
      
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILENAME(1:len_trim(FILENAME))    
      LEN_TMP_NAME = OUTFILE_NAME_LEN+len_trim(FILENAME)
      OPEN(UNIT=10,FILE=TMP_NAME(1:LEN_TMP_NAME),FORM='formatted')

      WRITE(10, "(A100)") RAD_INTER
      WRITE(10, "(A5)") "/NODE"
      WRITE(10, "(A100)") RAD_INTER
      DO II = NB_TOTAL_NODE_OLD + 1, NB_TOTAL_NODE
         WRITE(10, "(I10, 1PG20.13, 1PG20.13, 1PG20.13)") ITAB_TMP(II), 
     .        NODE_COORD(1, II), NODE_COORD(2, II), NODE_COORD(3, II)
      ENDDO
C     VOID MATERIAL
      WRITE(10, "(A100)") RAD_INTER
      WRITE(10, "(A11)") "/MAT/VOID/1"
      WRITE(10, "(A5)") "tetra"
      WRITE(10, "(A12)") "/PROP/VOID/1"
      WRITE(10, "(A5)") "tetra"
      WRITE(10, "(A7)") "/PART/1"
      WRITE(10, "(A5)") "tetra"
      WRITE(10, "(I10, I10)") 1, 1
      WRITE(10, "(A14)") "/GRBRIC/PART/1"
      WRITE(10, "(A5)") "tetra"
      WRITE(10, "(I10)") 1
      WRITE(10, "(A100)") RAD_INTER
C     WRITE TETRAS
      WRITE(10, "(A100)") RAD_INTER
      WRITE(10, "(A9)") "/TETRA4/1"
      WRITE(10, "(A100)") RAD_INTER
      DO II = NB_TOTAL_SOLIDS_OLD + 1, NB_TOTAL_SOLIDS
         N1 = IXS_TEMP(NIXS * (II - 1) + 2)
         N2 = IXS_TEMP(NIXS * (II - 1) + 4)
         N3 = IXS_TEMP(NIXS * (II - 1) + 7)
         N4 = IXS_TEMP(NIXS * (II - 1) + 9)
         WRITE(10, "(5I10)") II, ITAB_TMP(N1), ITAB_TMP(N2), ITAB_TMP(N3), ITAB_TMP(N4)
      ENDDO
C     END OF FILE
      WRITE(10, "(A8)") "#enddata"
      CLOSE(10)
      END SUBROUTINE WRITEMESH
