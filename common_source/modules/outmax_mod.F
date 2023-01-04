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
Chd|  OUTMAX_MOD                    modules/outmax_mod.F          
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        INI_H3DTMAX_ENGINE            starter/source/starter/contrl.F
Chd|        INI_OUTMAX_AUTO               starter/source/elements/elbuf_init/ini_outmax_auto.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        W_OUTMAXN                     starter/source/restart/ddsplit/w_outmaxn.F
Chd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
Chd|        H3D_NODAL_TENSOR              engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|        H3D_NODAL_VECTOR              engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
Chd|        INI_TMAX                      engine/source/output/ini_outmax.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        UPD_TMAX                      engine/source/output/upd_outmax.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
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
C---------Output Max contact force and pressure 
        integer :: SFCONT_MAX, SPCONT_MAX 
        integer :: IFCONTMAX,IFCONTPMAX, IFCONT2MAX,IFCONTP2MAX,IFCONT2MIN,IFCONTP2MIN
C        /VONM;/SIGEQ; /ENER ; /DAMA, /TENS/STRESS ;/TENS/STRAIN
        integer, DIMENSION(:) ,  ALLOCATABLE :: IKEYMAX 
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

        END MODULE OUTMAX_MOD
