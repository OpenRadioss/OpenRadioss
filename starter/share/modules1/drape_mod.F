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
      !||    drape_mod                      ../starter/share/modules1/drape_mod.F
      !||--- called by ------------------------------------------------------
      !||    c3grhead                       ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails                      ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c3init3                        ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    c3inmas                        ../starter/source/elements/sh3n/coque3n/c3inmas.F
      !||    cbainit3                       ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cdkinit3                       ../starter/source/elements/sh3n/coquedk/cdkinit3.F
      !||    cgrhead                        ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails                       ../starter/source/elements/shell/coque/cgrtails.F
      !||    cinit3                         ../starter/source/elements/shell/coque/cinit3.F
      !||    cinmas                         ../starter/source/elements/shell/coque/cinmas.F
      !||    cmaini3                        ../starter/source/elements/sh3n/coquedk/cmaini3.F
      !||    corthini                       ../starter/source/elements/shell/coque/corthini.F
      !||    ddsplit                        ../starter/source/restart/ddsplit/ddsplit.F
      !||    hm_read_drape                  ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    initia                         ../starter/source/elements/initia/initia.F
      !||    layini1                        ../starter/source/elements/shell/coqueba/layini1.F
      !||    lectur                         ../starter/source/starter/lectur.F
      !||    pre_stackgroup                 ../starter/source/stack/pres_stackgroup.F
      !||    shellthk_upd                   ../starter/source/properties/composite_options/drape/shellthk_upd.F
      !||    st_qaprint_composite_options   ../starter/source/output/qaprint/st_qaprint_composite_options.F
      !||    st_qaprint_driver              ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    stackgroup_drape               ../starter/source/stack/stackgroup_drape.F
      !||    t3grhead                       ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails                      ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    w_drape                        ../starter/source/restart/ddsplit/w_drape.F
      !||====================================================================
        MODULE DRAPE_MOD
c=======================================================================      
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
c=======================================================================      
c      
      TYPE DRAPE_PLY_
        INTEGER                           ::  NSLICE   ! number of slice
        INTEGER                           ::  IPID     ! ID PID
c-------  layer variables     
        my_real, DIMENSION(:,:) , ALLOCATABLE ::   RDRAPE
        INTEGER, DIMENSION(:,:) , ALLOCATABLE ::   IDRAPE 
c-------       
      END TYPE DRAPE_PLY_      
      !!
      TYPE DRAPE_
        INTEGER                                        ::  NPLY_DRAPE
        INTEGER                                        ::  NPLY
        INTEGER,            DIMENSION(:)   , ALLOCATABLE   ::  INDX_PLY
        TYPE (DRAPE_PLY_) , DIMENSION(:)   , ALLOCATABLE   ::  DRAPE_PLY
      END TYPE DRAPE_  
      !!
      TYPE DRAPEG_
        INTEGER                              ::  NUMSH4
        INTEGER                              ::  NUMSH3
        INTEGER, DIMENSION(:) , ALLOCATABLE  ::  INDX
      END TYPE DRAPEG_  
      
      TYPE DRAPE_WORK_
        INTEGER                              ::  IDSTACK  ! idstack
        INTEGER, DIMENSION(:)  , ALLOCATABLE ::  PLYID    ! Id ply
        INTEGER, DIMENSION(:)  , ALLOCATABLE ::  NPT_PLY  ! Number of int point
        INTEGER, DIMENSION(:)  , ALLOCATABLE ::  PLYNUM   ! Number of ply 
      END TYPE DRAPE_WORK_
      
      INTEGER :: NUMELC_DRAPE    ,NUMELTG_DRAPE , SCDRAPE, STDRAPE
C=======================================================================
      END MODULE DRAPE_MOD
