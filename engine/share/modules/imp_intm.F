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
      !||    imp_intm        ../engine/share/modules/imp_intm.F
      !||--- called by ------------------------------------------------------
      !||    ass_spmd        ../engine/source/implicit/assem_int.F
      !||    ass_spmd11      ../engine/source/implicit/assem_int.F
      !||    assem_ksl       ../engine/source/mpi/implicit/imp_fri.F
      !||    cp_iadd         ../engine/source/mpi/implicit/imp_fri.F
      !||    cp_slnr         ../engine/source/mpi/implicit/imp_fri.F
      !||    diag_int        ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_frkm        ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_frkm1       ../engine/source/mpi/implicit/imp_fri.F
      !||    doub_nrs        ../engine/source/mpi/implicit/imp_fri.F
      !||    ffizero         ../engine/source/interfaces/int07/i7ke3.F
      !||    fr_a2b          ../engine/source/mpi/implicit/imp_fri.F
      !||    fr_a2bd         ../engine/source/mpi/implicit/imp_fri.F
      !||    fr_u2d          ../engine/source/mpi/implicit/imp_fri.F
      !||    fr_u2dd         ../engine/source/mpi/implicit/imp_fri.F
      !||    i10corp3        ../engine/source/interfaces/int10/i10corp3.F
      !||    i10frf3         ../engine/source/interfaces/int10/i10keg3.F
      !||    i10keg3         ../engine/source/interfaces/int10/i10keg3.F
      !||    i10kfor3        ../engine/source/interfaces/int10/i10keg3.F
      !||    i11corkp3       ../engine/source/interfaces/int11/i11corp3.F
      !||    i11corp3        ../engine/source/interfaces/int11/i11corp3.F
      !||    i11frf3         ../engine/source/interfaces/int11/i11keg3.F
      !||    i11keg3         ../engine/source/interfaces/int11/i11keg3.F
      !||    i11kfor3        ../engine/source/interfaces/int11/i11keg3.F
      !||    i24corp3        ../engine/source/interfaces/int24/i24cork3.F
      !||    i24keg3         ../engine/source/interfaces/int24/i24ke3.F
      !||    i7corkp3        ../engine/source/interfaces/int07/i7cork3.F
      !||    i7corp3         ../engine/source/interfaces/int07/i7cork3.F
      !||    i7frf3          ../engine/source/interfaces/int07/i7keg3.F
      !||    i7keg3          ../engine/source/interfaces/int07/i7keg3.F
      !||    i7kfor3         ../engine/source/interfaces/int07/i7keg3.F
      !||    iddl_int        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_diags       ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_diagsn      ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_fr7i        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frii        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frkm        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frks        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frsl        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frsn        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_fvkm        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_fvksl       ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_fvkss       ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_init        ../engine/source/implicit/imp_init.F
      !||    imp_mumps1      ../engine/source/implicit/imp_mumps.F
      !||    imp_pcgh        ../engine/source/implicit/imp_pcg.F
      !||    ind_fvn         ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_kinefr      ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_kinfrk      ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_sld         ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_dd0         ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_ddfv        ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_frfd        ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_frkc        ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_frud        ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_intm        ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_kic         ../engine/source/implicit/imp_solv.F
      !||    ini_ksi         ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_slnr        ../engine/source/mpi/implicit/imp_fri.F
      !||    int_matvp       ../engine/source/implicit/imp_int_k.F
      !||    jdifrtok        ../engine/source/mpi/implicit/imp_fri.F
      !||    mav_ltfr        ../engine/source/mpi/implicit/imp_fri.F
      !||    mav_ltfr_gpu    ../engine/source/mpi/implicit/imp_fri.F
      !||    mumps_set       ../engine/source/implicit/imp_mumps.F
      !||    mumps_set2      ../engine/source/implicit/imp_mumps.F
      !||    ndofi_nsl       ../engine/source/mpi/implicit/imp_fri.F
      !||    pr_deb          ../engine/source/implicit/imp_solv.F
      !||    pr_solnfo       ../engine/source/implicit/imp_solv.F
      !||    rowfr_dim       ../engine/source/mpi/implicit/imp_fri.F
      !||    rowfr_dim11     ../engine/source/mpi/implicit/imp_fri.F
      !||    rowfr_dim24     ../engine/source/mpi/implicit/imp_fri.F
      !||    rowfr_ind       ../engine/source/mpi/implicit/imp_fri.F
      !||    rowfr_ind11     ../engine/source/mpi/implicit/imp_fri.F
      !||    rowfr_ind24     ../engine/source/mpi/implicit/imp_fri.F
      !||    scom_frk        ../engine/source/mpi/implicit/imp_fri.F
      !||    scom_frk1       ../engine/source/mpi/implicit/imp_fri.F
      !||    scom_frud       ../engine/source/mpi/implicit/imp_fri.F
      !||    set_ind_fr      ../engine/source/mpi/implicit/imp_fri.F
      !||    spmd_ifc1       ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_ifcd       ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_ifcf       ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_ifrf       ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_ifrf_gpu   ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_ifri       ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_ifru       ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_ifru_gpu   ../engine/source/mpi/implicit/imp_spmd.F
      !||    tag_intml       ../engine/source/mpi/implicit/imp_fri.F
      !||    tag_ints        ../engine/source/mpi/implicit/imp_fri.F
      !||    tra_frkm        ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_fr          ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_kml         ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_ksl         ../engine/source/mpi/implicit/imp_fri.F
      !||====================================================================
      MODULE IMP_INTM
#include "my_real.inc"
      TYPE int_pointer2
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE int_pointer2
cow51d1      TYPE int_pointer
cow51d1        INTEGER, DIMENSION(:), POINTER :: P
cow51d1      END TYPE int_pointer
C
      INTEGER NI2_FR,NRB_FR,NI2_FRS,NRB_FRS,NML,NBC_FR,NFX_FR,
     .        NRBE3_FR,NRBE3_FRS,NSPC_FR,NRBE2_FR,NRBE2_FRS
C tableau de int de taille NSREM :noeuds secnds remotes dans l'order proc croissant
C indiquant liste noeuds remote a recevoir du "proc main"
      INTEGER, DIMENSION(:), ALLOCATABLE :: FR_SREM
C tableau de pointeurs IND_INT de taille NINTER :index de structure interface a FR_SREM
      TYPE(int_pointer2), DIMENSION(:), ALLOCATABLE :: IND_INT
C  51d1      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: IND_INT
C tableau de int SHF_INT de taille NINTER :nb de SREM des precedent interfaces
      INTEGER, DIMENSION(:), ALLOCATABLE :: SHF_INT
C tableau de int de taille NSL :noeuds secnds locales  (apres upd)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISL
C =ISL sauf mis a zero des noeuds doubles
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISLM
C tableau de int de taille NML :noeuds mains locales (apres upd)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IML
C tableau de int de taille (NBINTC,NSPMD) :nb noeuds secnds locales (tempo)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: INBSL
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_SREM,IAD_SL
      my_real, DIMENSION(:),ALLOCATABLE :: STIFS
C tableau de divers reels de taille (,NSREM),H_E(4,):fonction de form, 
C---N_E(3,) :direction de impact, DFI(3,) :deplacement,FFI(3,) forces.
      my_real, DIMENSION(:,:),ALLOCATABLE :: H_E,N_E,DFI,FFI
C tableau de K_DIAG reels de taille (3,NSL) 
      my_real, DIMENSION(:,:),ALLOCATABLE :: DIAG_S
C tableau de int de taille NI2_FR,NRB_FR :noeuds front avec kine------
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IFRSR,IFRS2,IFRS4
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFRS3
C tableau de int IDDL de taille 3*(NSL,NML )6*(NRB_FR ,4*NI2_FR)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IDDSL,IDDML,IDDMR
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: IDDMI2,IDDMI3
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IDDMR2
C tableau de int de taille NI2_FRS,NRB_FRS :la part de NSL------
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFRSR_S,IFRS2_S,IFRS3_S
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFRS4_S
C tableau de K_DIAG reels de taille (6,IFRSRS) 
      my_real, DIMENSION(:,:),ALLOCATABLE :: DIAG_MR,DIAG_MR2
C tableau de K_DIAG reels de taille (6,4,NI2_FRS) 
      my_real, DIMENSION(:,:,:),ALLOCATABLE :: DIAG_M2,DIAG_M3
C de taille (3,NBC_FR),(2,NFX_FR):imposed velocity
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IBC_FR,IFX_FR
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISPC_FR
C int de taille (NRW_FR)
      INTEGER NRW_FR
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRW_FR
C quand INTP_D>0 la matrice de rigidite M.C.R.S.-----
      INTEGER INTP_D,NDDL_SI,NDDL_SL,NZ_SI,NZ_SL
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_SI,JDI_SI,IKC_SI
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_SS,JDI_SL,IKC_SL
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_SINR,JDI_SINR
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_SLNR,JDI_SLNR
      INTEGER, DIMENSION(:), ALLOCATABLE :: IDDL_SI,IDDL_SL
      my_real, DIMENSION(:), ALLOCATABLE :: DIAG_SL,LT_SI,LT_SL
      my_real, DIMENSION(:), ALLOCATABLE :: USI,FSI
      my_real, DIMENSION(:), ALLOCATABLE :: FRCDI,MRCDI
C      
      END MODULE IMP_INTM
C
      !||====================================================================
      !||    imp_i7cp     ../engine/share/modules/imp_intm.F
      !||--- called by ------------------------------------------------------
      !||    cp_ifront    ../engine/source/implicit/produt_v.F
      !||    cp_impbuf    ../engine/source/implicit/produt_v.F
      !||    imp_stif24   ../engine/source/implicit/imp_solv.F
      !||====================================================================
      MODULE IMP_I7CP
      INTEGER LII7CP,LRI7CP,LENSCP(20)
#include "my_real.inc"
      INTEGER, DIMENSION(:), ALLOCATABLE :: II7CP,NEWFRCP
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_STIFOLD
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IPARICP
      my_real, DIMENSION(:), ALLOCATABLE :: RI7CP
C      
      END MODULE IMP_I7CP
C

