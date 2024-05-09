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
Chd|  PINCHTYPE_MOD                 modules/pinchtype_mod.F       
Chd|-- called by -----------
Chd|        FREE_PINCH                    common_source/modules/pinchtype_mod.F
Chd|        CONTRL                        starter/source/starter/contrl.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        HM_READ_PLOAD                 starter/source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PROP01                starter/source/properties/shell/hm_read_prop01.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        ACCELEPINCH                   engine/source/assembly/accelepinch.F
Chd|        ASSPAR                        engine/source/assembly/asspar.F
Chd|        CBAPINCHPROJ                  engine/source/elements/shell/coqueba/cbapinchproj.F
Chd|        CBAPINCHTHK                   engine/source/elements/shell/coqueba/cbapinchthk.F
Chd|        CBASTRA3PINCH                 engine/source/elements/shell/coqueba/cbastra3pinch.F
Chd|        CMAIN3PINCH                   engine/source/elements/shell/coqueba/cmain3pinch.F
Chd|        DEPLAPINCH                    engine/source/assembly/deplapinch.F
Chd|        FORCEPINCH                    engine/source/loads/general/forcepinch.F
Chd|        FORCE_IMP                     engine/source/loads/general/force_imp.F
Chd|        FORINTC                       engine/source/elements/forintc.F
Chd|        H3D_PRE_SKIN_IXSKIN           engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_PRE_SKIN_SCALAR           engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_PRE_DIM              engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_PRE_MAP              engine/source/output/h3d/h3d_results/h3d_skin_pre_map.F
Chd|        H3D_SKIN_VECTOR               engine/source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        HIST2                         engine/source/output/th/hist2.F
Chd|        I25ASS0                       engine/source/interfaces/int25/i25ass3.F
Chd|        I25ASS3                       engine/source/interfaces/int25/i25ass3.F
Chd|        I25FOR3                       engine/source/interfaces/int25/i25for3.F
Chd|        INIRESA                       engine/source/output/restart/rdresa.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDRESA                        engine/source/output/restart/rdresa.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        THCOQ                         engine/source/output/th/thcoq.F
Chd|        THNOD                         engine/source/output/th/thnod.F
Chd|        VITESSEPINCH                  engine/source/assembly/vitessepinch.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
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

Chd|====================================================================
Chd|  FREE_PINCH                    modules/pinchtype_mod.F       
Chd|-- called by -----------
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 modules/pinchtype_mod.F       
Chd|====================================================================
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

