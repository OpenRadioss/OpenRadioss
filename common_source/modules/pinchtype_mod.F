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
      !||====================================================================
      !||    pinchtype_mod         ../common_source/modules/pinchtype_mod.F
      !||--- called by ------------------------------------------------------
      !||    accelepinch           ../engine/source/assembly/accelepinch.F
      !||    asspar                ../engine/source/assembly/asspar.F
      !||    cbapinchproj          ../engine/source/elements/shell/coqueba/cbapinchproj.F
      !||    cbapinchthk           ../engine/source/elements/shell/coqueba/cbapinchthk.F
      !||    cbastra3pinch         ../engine/source/elements/shell/coqueba/cbastra3pinch.F
      !||    cmain3pinch           ../engine/source/elements/shell/coqueba/cmain3pinch.F
      !||    contrl                ../starter/source/starter/contrl.F
      !||    ddsplit               ../starter/source/restart/ddsplit/ddsplit.F
      !||    deplapinch            ../engine/source/assembly/deplapinch.F
      !||    force                 ../engine/source/loads/general/force.F90
      !||    force_imp             ../engine/source/loads/general/force_imp.F
      !||    forcepinch            ../engine/source/loads/general/forcepinch.F
      !||    forintc               ../engine/source/elements/forintc.F
      !||    free_pinch            ../common_source/modules/pinchtype_mod.F
      !||    h3d_pre_skin_ixskin   ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_pre_skin_scalar   ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_pre_dim      ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_pre_map      ../engine/source/output/h3d/h3d_results/h3d_skin_pre_map.F
      !||    h3d_skin_vector       ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
      !||    hist2                 ../engine/source/output/th/hist2.F
      !||    hm_read_pload         ../starter/source/loads/general/pload/hm_read_pload.F
      !||    hm_read_prop01        ../starter/source/properties/shell/hm_read_prop01.F
      !||    i25ass0               ../engine/source/interfaces/int25/i25ass3.F
      !||    i25ass3               ../engine/source/interfaces/int25/i25ass3.F
      !||    i25for3               ../engine/source/interfaces/int25/i25for3.F
      !||    iniresa               ../engine/source/output/restart/rdresa.F
      !||    lectur                ../engine/source/input/lectur.F
      !||    radioss2              ../engine/source/engine/radioss2.F
      !||    rdcomi                ../engine/source/output/restart/rdcomm.F
      !||    rdresa                ../engine/source/output/restart/rdresa.F
      !||    rdresb                ../engine/source/output/restart/rdresb.F
      !||    resol                 ../engine/source/engine/resol.F
      !||    resol_head            ../engine/source/engine/resol_head.F
      !||    resol_init            ../engine/source/engine/resol_init.F
      !||    restalloc             ../engine/source/output/restart/arralloc.F
      !||    sortie_main           ../engine/source/output/sortie_main.F
      !||    thcoq                 ../engine/source/output/th/thcoq.F
      !||    thnod                 ../engine/source/output/th/thnod.F
      !||    vitessepinch          ../engine/source/assembly/vitessepinch.F
      !||    wrcomi                ../engine/source/output/restart/wrcomm.F
      !||    wrcomip               ../starter/source/restart/ddsplit/wrcommp.F
      !||    wrrestp               ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE PINCHTYPE_MOD

#include "my_real.inc"

      INTEGER  :: NPINCH
      INTEGER  :: NPLOADPINCH 

      TYPE PINCH 
        INTEGER :: SAPINCH  ! since acceleration size is 3*NUMNOD*NTHREAD                              
c       INTEGER ,DIMENSION(:), ALLOCATABLE :: INDEXPINCH  
C       [PM] future : mapping of NUMNOD to NUMPINCH
        my_real ,DIMENSION(:,:), ALLOCATABLE :: XPINCH   
        my_real ,DIMENSION(:,:), ALLOCATABLE :: DPINCH    
        my_real ,DIMENSION(:,:), ALLOCATABLE :: VPINCH    
        my_real ,DIMENSION(:,:), ALLOCATABLE :: APINCH  
        my_real ,DIMENSION(:)  , ALLOCATABLE :: STIFPINCH  
        my_real ,DIMENSION(:)  , ALLOCATABLE :: MSPINCH      
      END TYPE PINCH  

      
      END MODULE PINCHTYPE_MOD

      !||====================================================================
      !||    free_pinch      ../common_source/modules/pinchtype_mod.F
      !||--- called by ------------------------------------------------------
      !||    resol           ../engine/source/engine/resol.F
      !||--- uses       -----------------------------------------------------
      !||    pinchtype_mod   ../common_source/modules/pinchtype_mod.F
      !||====================================================================
      SUBROUTINE FREE_PINCH(PINCH_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE PINCHTYPE_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
        TYPE(PINCH) :: PINCH_DATA

         IF(ALLOCATED(PINCH_DATA%XPINCH    )) DEALLOCATE(PINCH_DATA%XPINCH   ) 
         IF(ALLOCATED(PINCH_DATA%DPINCH    )) DEALLOCATE(PINCH_DATA%DPINCH   )  
         IF(ALLOCATED(PINCH_DATA%VPINCH    )) DEALLOCATE(PINCH_DATA%VPINCH   )   
         IF(ALLOCATED(PINCH_DATA%APINCH    )) DEALLOCATE(PINCH_DATA%APINCH   )  
         IF(ALLOCATED(PINCH_DATA%STIFPINCH )) DEALLOCATE(PINCH_DATA%STIFPINCH)   
         IF(ALLOCATED(PINCH_DATA%MSPINCH   )) DEALLOCATE(PINCH_DATA%MSPINCH  )  
      END SUBROUTINE FREE_PINCH

