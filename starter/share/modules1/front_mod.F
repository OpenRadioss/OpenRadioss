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
      !||    front_mod                 ../starter/share/modules1/front_mod.F
      !||--- called by ------------------------------------------------------
      !||    c_front                   ../starter/source/restart/ddsplit/c_front.F
      !||    c_iddconnect              ../starter/source/spmd/node/ddtools.F
      !||    c_ifront                  ../starter/source/spmd/node/ddtools.F
      !||    c_vois                    ../starter/source/restart/ddsplit/c_vois.F
      !||    ddsplit                   ../starter/source/restart/ddsplit/ddsplit.F
      !||    domdec2                   ../starter/source/spmd/domdec2.F
      !||    dometis                   ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    fill_intercep             ../starter/source/spmd/node/ddtools.F
      !||    i11buc_vox1               ../starter/source/interfaces/inter3d1/i11buc1.F
      !||    i24setnodes               ../starter/source/interfaces/inter3d1/i24setnodes.F
      !||    i7buc_vox1                ../starter/source/interfaces/inter3d1/i7buc_vox1.F
      !||    iddconnectplus            ../starter/source/spmd/node/frontplus.F
      !||    ifrontplus                ../starter/source/spmd/node/frontplus.F
      !||    ini_iddconnect            ../starter/source/spmd/node/ddtools.F
      !||    ini_ifront                ../starter/source/spmd/node/ddtools.F
      !||    iniend                    ../starter/source/interfaces/inter3d1/iniend.F
      !||    iniend2d                  ../starter/source/interfaces/inter3d1/iniend.F
      !||    inint3                    ../starter/source/interfaces/inter3d1/inint3.F
      !||    inintr                    ../starter/source/interfaces/interf1/inintr.F
      !||    intersurfl                ../starter/source/spmd/node/ddtools.F
      !||    ipari_l_ini               ../starter/source/restart/ddsplit/ipari_l_ini.F
      !||    lectur                    ../starter/source/starter/lectur.F
      !||    line_decomp               ../starter/source/model/group/line_decomp.F
      !||    nlocal                    ../starter/source/spmd/node/ddtools.F
      !||    plist_bfs                 ../starter/source/spmd/node/ddtools.F
      !||    plist_iddconnect          ../starter/source/spmd/node/ddtools.F
      !||    plist_ifront              ../starter/source/spmd/node/ddtools.F
      !||    prepare_int25             ../starter/source/model/mesh/build_cnel.F
      !||    prepare_split_i11         ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i17         ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i2          ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i20         ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i21         ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i24         ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i25         ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i25e2e      ../starter/source/spmd/prepare_split_i25e2e.F
      !||    prepare_split_i7          ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i8          ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i9          ../starter/source/restart/ddsplit/inter_tools.F
      !||    r2r_split                 ../starter/source/coupling/rad2rad/r2r_split.F
      !||    read_ebcs                 ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    realloc_iddconnect        ../starter/source/spmd/node/ddtools.F
      !||    realloc_ifront            ../starter/source/spmd/node/ddtools.F
      !||    secnd_surface_on_domain   ../starter/source/interfaces/inter3d1/i24setnodes.F
      !||    set_front8                ../starter/source/spmd/node/ddtools.F
      !||    set_intercep              ../starter/source/spmd/node/ddtools.F
      !||    split_adskyn_25           ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i11            ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i24            ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_ebcs                ../starter/source/boundary_conditions/ebcs/split_ebcs.F
      !||    split_interfaces          ../starter/source/restart/ddsplit/split_interfaces.F
      !||    split_remnode_i24         ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_remnode_i25         ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_ielem           ../starter/source/restart/ddsplit/split_seg_ielem.F
      !||    w_fi                      ../starter/source/restart/ddsplit/w_fi.F
      !||    w_front                   ../starter/source/restart/ddsplit/w_front.F
      !||    w_type8                   ../starter/source/restart/ddsplit/split_interfaces.F
      !||====================================================================
      MODULE FRONT_MOD
#include "my_real.inc"

      TYPE my_front
          INTEGER, DIMENSION(:,:), POINTER :: P
          INTEGER, DIMENSION(:)  , ALLOCATABLE :: IENTRY
      END TYPE my_front     
      
c IFRONT chained-list      
      TYPE(my_front) :: IFRONT

      TYPE my_connectdom
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: PDOM
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
