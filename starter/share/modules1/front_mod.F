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
Chd|  FRONT_MOD                     share/modules1/front_mod.F    
Chd|-- called by -----------
Chd|        C_FRONT                       source/restart/ddsplit/c_front.F
Chd|        C_IDDCONNECT                  source/spmd/node/ddtools.F    
Chd|        C_IFRONT                      source/spmd/node/ddtools.F    
Chd|        C_VOIS                        source/restart/ddsplit/c_vois.F
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        FILL_INTERCEP                 source/spmd/node/ddtools.F    
Chd|        I11BUC_VOX1                   source/interfaces/inter3d1/i11buc1.F
Chd|        I24SETNODES                   source/interfaces/inter3d1/i24setnodes.F
Chd|        I7BUC_VOX1                    source/interfaces/inter3d1/i7buc_vox1.F
Chd|        IDDCONNECTPLUS                source/spmd/node/frontplus.F  
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        INIEND                        source/interfaces/inter3d1/iniend.F
Chd|        INIEND2D                      source/interfaces/inter3d1/iniend.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|        INI_IDDCONNECT                source/spmd/node/ddtools.F    
Chd|        INI_IFRONT                    source/spmd/node/ddtools.F    
Chd|        IPARI_L_INI                   source/restart/ddsplit/ipari_l_ini.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        LINE_DECOMP                   source/model/group/line_decomp.F
Chd|        PLIST_BFS                     source/spmd/node/ddtools.F    
Chd|        PLIST_IDDCONNECT              source/spmd/node/ddtools.F    
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        PREPARE_INT25                 source/model/mesh/build_cnel.F
Chd|        PREPARE_SPLIT_I11             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I17             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I2              source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I20             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I21             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I24             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I25             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I25E2E          source/spmd/prepare_split_i25e2e.F
Chd|        PREPARE_SPLIT_I7              source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I8              source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I9              source/restart/ddsplit/inter_tools.F
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|        REALLOC_IDDCONNECT            source/spmd/node/ddtools.F    
Chd|        REALLOC_IFRONT                source/spmd/node/ddtools.F    
Chd|        SET_FRONT8                    source/spmd/node/ddtools.F    
Chd|        SET_INTERCEP                  source/spmd/node/ddtools.F    
Chd|        SPLIT_ADSKYN_25               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I11                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I24                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|        SPLIT_REMNODE_I24             source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I25             source/restart/ddsplit/inter_tools.F
Chd|        W_FI                          source/restart/ddsplit/w_fi.F 
Chd|        W_FRONT                       source/restart/ddsplit/w_front.F
Chd|        W_TYPE8                       source/restart/ddsplit/split_interfaces.F
Chd|        INTERSURFL                    source/spmd/node/ddtools.F    
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        SECND_SURFACE_ON_DOMAIN       source/interfaces/inter3d1/i24setnodes.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE FRONT_MOD
#include "my_real.inc"

      TYPE my_front
        INTEGER, DIMENSION(:,:), POINTER :: P
	INTEGER, DIMENSION(:)  , ALLOCATABLE :: IENTRY
      END TYPE my_front

c IFRONT chained-list
      TYPE(my_front) :: IFRONT

      TYPE my_connectdom
        INTEGER, DIMENSION(:,:), POINTER :: PDOM
	INTEGER, DIMENSION(:,:)  , ALLOCATABLE :: IENTRYDOM
      END TYPE my_connectdom

c IFRONT64 chained-list
      TYPE(my_connectdom) :: IDDCONNECT
      INTEGER SIDDCONNECT, IDDCONNECT_END

      INTEGER, DIMENSION(:),ALLOCATABLE :: IENTRY2
      INTEGER, DIMENSION(:),ALLOCATABLE :: FLAGKIN

      INTEGER SIFRONT, IFRONT_END

      TYPE INTERSURFP
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE INTERSURFP

      END MODULE FRONT_MOD
C=======================================================================
