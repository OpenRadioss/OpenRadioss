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
      !||    outmax_mod           ../common_source/modules/outmax_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit              ../starter/source/restart/ddsplit/ddsplit.F
      !||    genh3d               ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    h3d_nodal_tensor     ../engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
      !||    h3d_nodal_vector     ../engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
      !||    i2for27_pen          ../engine/source/interfaces/interf/i2for27_pen.F
      !||    i2for27p_pen         ../engine/source/interfaces/interf/i2for27p_pen.F
      !||    i2mom27_cin          ../engine/source/interfaces/interf/i2mom27_cin.F
      !||    i2mom27p_cin         ../engine/source/interfaces/interf/i2mom27p_cin.F
      !||    ini_h3dtmax_engine   ../starter/source/starter/contrl.F
      !||    ini_outmax_auto      ../starter/source/elements/elbuf_init/ini_outmax_auto.F
      !||    ini_tmax             ../engine/source/output/ini_outmax.F
      !||    rdcomi               ../engine/source/output/restart/rdcomm.F
      !||    rdresb               ../engine/source/output/restart/rdresb.F
      !||    resol                ../engine/source/engine/resol.F
      !||    resol_init           ../engine/source/engine/resol_init.F
      !||    restalloc            ../engine/source/output/restart/arralloc.F
      !||    sortie_main          ../engine/source/output/sortie_main.F
      !||    tensgps3             ../engine/source/output/anim/generate/tensor6.F
      !||    tensgpstrain         ../engine/source/output/anim/generate/tensgpstrain.F
      !||    tmax_ipart           ../engine/source/output/tmax_ipart.F
      !||    upd_tmax             ../engine/source/output/upd_outmax.F
      !||    w_outmaxn            ../starter/source/restart/ddsplit/w_outmaxn.F
      !||    wrcomi               ../engine/source/output/restart/wrcomm.F
      !||    wrcomip              ../starter/source/restart/ddsplit/wrcommp.F
      !||    wrrestp              ../engine/source/output/restart/wrrestp.F
      !||====================================================================
        MODULE OUTMAX_MOD

#include "my_real.inc"
!--------------------------------------------------------------------
!       OUTMAX_MOD : elementary MOD is directly in GBUF :elbuf_mod
!--------------------------------------------------------------------
        integer  NMAX_N     ! nodal Tmax number, work var
        integer  NMAX_E     ! elementary Tmax number, work var
        integer  LMAX_DIS
        integer  LMAX_VEL
        integer  LMAX_NSIG  ! noda p1&p3 for 3D
        integer  LMAX_NSTRA ! noda p1&p3 for 3D
C------ used to initialize LMAX_* (for elementary only)
        integer :: NKEYMAX = 6  
        integer :: NCY_GPS, NCY_GPSTR 
C---------Output Max contact force and pressure 
        integer :: SFCONT_MAX, SPCONT_MAX 
        integer :: IFCONTMAX,IFCONTPMAX, IFCONT2MAX,IFCONTP2MAX,IFCONT2MIN,IFCONTP2MIN
C        /VONM;/SIGEQ; /ENER ; /DAMA, /TENS/STRESS ;/TENS/STRAIN
        integer, DIMENSION(:) ,  ALLOCATABLE :: IKEYMAX,IGPSTAG,IGPSTRATAG
        integer, DIMENSION(:,:) ,ALLOCATABLE :: IPART_OK 
        my_real, DIMENSION(:) , ALLOCATABLE ::  TM_DIS,TM_VEL  ! 3*NUMNOD
        my_real, DIMENSION(:) , ALLOCATABLE ::   TM_NSIG1 ,TM_NSIG3 ! 6*NUMNOD
        my_real, DIMENSION(:) , ALLOCATABLE ::   TM_NSTRA1,TM_NSTRA3
C------ work arrays        
        my_real, DIMENSION(:) , ALLOCATABLE ::   TM_PDIS,TM_PVEL
        my_real, DIMENSION(:) , ALLOCATABLE ::   TM_PNSIG,TM_PNSTRA
        my_real, DIMENSION(:) , ALLOCATABLE ::   GPSTMP,P2TMP
C---------Output Max contact force and pressure 
        my_real, DIMENSION(:) , ALLOCATABLE ::   FNCONT_MAX,FTCONT_MAX
        my_real, DIMENSION(:) , ALLOCATABLE ::   FCONT2_MAX,FNCONT2_MAX,FTCONT2_MAX
        my_real, DIMENSION(:) , ALLOCATABLE ::   FCONT2_MIN,FNCONT2_MIN,FTCONT2_MIN
        my_real, DIMENSION(:) , ALLOCATABLE ::   NPCONT2_MIN,NPCONT2_MAX
C---------Output of moment for int type2
        my_real, DIMENSION(:,:) , ALLOCATABLE ::  MCONT2       

        END MODULE OUTMAX_MOD
