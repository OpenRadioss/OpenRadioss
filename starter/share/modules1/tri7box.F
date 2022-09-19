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
Chd|  TRI7BOX                       share/modules1/tri7box.F      
Chd|-- called by -----------
Chd|        I11BUC_VOX1                   source/interfaces/inter3d1/i11buc1.F
Chd|        I11RESETVOXEL1                source/interfaces/inter3d1/i11trivox1.F
Chd|        I11TRIVOX1                    source/interfaces/inter3d1/i11trivox1.F
Chd|        I25BUCE_EDG                   source/interfaces/inter3d1/i25buce_edg.F
Chd|        I25BUC_VOX1                   source/interfaces/inter3d1/i25buc_vox1.F
Chd|        I25COR3E                      source/interfaces/inter3d1/i25cor3e.F
Chd|        I25COR3T                      source/interfaces/inter3d1/i25cor3t.F
Chd|        I25COR3_E2S                   source/interfaces/inter3d1/i25cor3_e2s.F
Chd|        I25PEN3_E2S                   source/interfaces/inter3d1/i25pen3_e2s.F
Chd|        I25PEN3_EDG                   source/interfaces/inter3d1/i25pen3_edg.F
Chd|        I25STO                        source/interfaces/inter3d1/i25sto.F
Chd|        I25STO_E2S                    source/interfaces/inter3d1/i25sto_e2s.F
Chd|        I25STO_EDG                    source/interfaces/inter3d1/i25sto_edg.F
Chd|        I25TRIVOX1                    source/interfaces/inter3d1/i25trivox1.F
Chd|        I25TRIVOX_EDG                 source/interfaces/inter3d1/i25trivox_edg.F
Chd|        I7BUC_VOX1                    source/interfaces/inter3d1/i7buc_vox1.F
Chd|        I7TRIVOX1                     source/interfaces/inter3d1/i7trivox1.F
Chd|        SPBUC31                       source/elements/sph/spbuc31.F 
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE TRI7BOX
C Temporary array for VOXEL SORTING
      INTEGER , DIMENSION(:), ALLOCATABLE :: NEXT_NOD
      INTEGER , DIMENSION(:), ALLOCATABLE :: CHAIN_ELEM,CHAIN_NEXT
      INTEGER , DIMENSION(:), ALLOCATABLE :: CHAIN_LAST
      INTEGER LVOXEL
      PARAMETER(LVOXEL = 8000000)
C      PARAMETER(LVOXEL = 4000000)
      INTEGER INIVOXEL, VOXEL1(LVOXEL)
      PARAMETER(LRVOXEL = 31)
      INTEGER CRVOXEL(0:LRVOXEL,0:LRVOXEL)
      DATA INIVOXEL /1/
C
      END MODULE TRI7BOX
