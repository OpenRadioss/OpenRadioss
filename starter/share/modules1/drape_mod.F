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
Chd|====================================================================
Chd|  DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|-- called by -----------
Chd|        C3GRHEAD                      source/elements/sh3n/coque3n/c3grhead.F
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CGRHEAD                       source/elements/shell/coque/cgrhead.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CINMAS                        source/elements/shell/coque/cinmas.F
Chd|        CMAINI3                       source/elements/sh3n/coquedk/cmaini3.F
Chd|        CORTHINI                      source/elements/shell/coque/corthini.F
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        LAYINI1                       source/elements/shell/coqueba/layini1.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        PRE_STACKGROUP                source/stack/pres_stackgroup.F
Chd|        SHELLTHK_UPD                  source/properties/composite_options/drape/shellthk_upd.F
Chd|        STACKGROUP_DRAPE              source/stack/stackgroup_drape.F
Chd|        ST_QAPRINT_COMPOSITE_OPTIONS  source/output/qaprint/st_qaprint_composite_options.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        T3GRHEAD                      source/elements/solid_2d/tria/t3grhead.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        W_DRAPE                       source/restart/ddsplit/w_drape.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE DRAPE_MOD
c=======================================================================      
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
c=======================================================================      
c
      TYPE DRAPE_
        INTEGER                               :: NPT 
        INTEGER , DIMENSION(:)  , ALLOCATABLE ::     IDRAPE
        my_real,  DIMENSION(:,:), ALLOCATABLE ::     RDRAPE 
      END TYPE DRAPE_  
C=======================================================================
      END MODULE DRAPE_MOD
