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
Chd|  IMP_INTM                      share/modules/imp_intm.F      
Chd|-- called by -----------
Chd|        ASSEM_KSL                     source/mpi/implicit/imp_fri.F 
Chd|        ASS_SPMD                      source/implicit/assem_int.F   
Chd|        ASS_SPMD11                    source/implicit/assem_int.F   
Chd|        CP_IADD                       source/mpi/implicit/imp_fri.F 
Chd|        CP_SLNR                       source/mpi/implicit/imp_fri.F 
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_FRKM1                     source/mpi/implicit/imp_fri.F 
Chd|        DOUB_NRS                      source/mpi/implicit/imp_fri.F 
Chd|        FFIZERO                       source/interfaces/int07/i7ke3.F
Chd|        FR_A2B                        source/mpi/implicit/imp_fri.F 
Chd|        FR_A2BD                       source/mpi/implicit/imp_fri.F 
Chd|        FR_U2D                        source/mpi/implicit/imp_fri.F 
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        I10CORP3                      source/interfaces/int10/i10corp3.F
Chd|        I10FRF3                       source/interfaces/int10/i10keg3.F
Chd|        I10KEG3                       source/interfaces/int10/i10keg3.F
Chd|        I10KFOR3                      source/interfaces/int10/i10keg3.F
Chd|        I11CORKP3                     source/interfaces/int11/i11corp3.F
Chd|        I11CORP3                      source/interfaces/int11/i11corp3.F
Chd|        I11FRF3                       source/interfaces/int11/i11keg3.F
Chd|        I11KEG3                       source/interfaces/int11/i11keg3.F
Chd|        I11KFOR3                      source/interfaces/int11/i11keg3.F
Chd|        I24CORP3                      source/interfaces/int24/i24cork3.F
Chd|        I24KEG3                       source/interfaces/int24/i24ke3.F
Chd|        I7CORKP3                      source/interfaces/int07/i7cork3.F
Chd|        I7CORP3                       source/interfaces/int07/i7cork3.F
Chd|        I7FRF3                        source/interfaces/int07/i7keg3.F
Chd|        I7KEG3                        source/interfaces/int07/i7keg3.F
Chd|        I7KFOR3                       source/interfaces/int07/i7keg3.F
Chd|        IDDL_INT                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_DIAGS                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_DIAGSN                    source/mpi/implicit/imp_fri.F 
Chd|        IMP_FR7I                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRII                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKS                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRSL                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRSN                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKM                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKSL                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKSS                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_INIT                      source/implicit/imp_init.F    
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IND_FVN                       source/mpi/implicit/imp_fri.F 
Chd|        IND_KINEFR                    source/mpi/implicit/imp_fri.F 
Chd|        IND_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|        IND_SLD                       source/mpi/implicit/imp_fri.F 
Chd|        INI_DD0                       source/mpi/implicit/imp_fri.F 
Chd|        INI_DDFV                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FRFD                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FRKC                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FRUD                      source/mpi/implicit/imp_fri.F 
Chd|        INI_INTM                      source/mpi/implicit/imp_fri.F 
Chd|        INI_KIC                       source/implicit/imp_solv.F    
Chd|        INI_KSI                       source/mpi/implicit/imp_fri.F 
Chd|        INI_SLNR                      source/mpi/implicit/imp_fri.F 
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|        JDIFRTOK                      source/mpi/implicit/imp_fri.F 
Chd|        MAV_LTFR                      source/mpi/implicit/imp_fri.F 
Chd|        MAV_LTFR_GPU                  source/mpi/implicit/imp_fri.F 
Chd|        MUMPS_SET                     source/implicit/imp_mumps.F   
Chd|        MUMPS_SET2                    source/implicit/imp_mumps.F   
Chd|        NDOFI_NSL                     source/mpi/implicit/imp_fri.F 
Chd|        PR_DEB                        source/implicit/imp_solv.F    
Chd|        PR_SOLNFO                     source/implicit/imp_solv.F    
Chd|        ROWFR_DIM                     source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_DIM11                   source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_DIM24                   source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND                     source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND11                   source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND24                   source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRK                      source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRK1                     source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRUD                     source/mpi/implicit/imp_fri.F 
Chd|        SET_IND_FR                    source/mpi/implicit/imp_fri.F 
Chd|        SPMD_IFC1                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFCD                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFCF                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRF                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRF_GPU                 source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRI                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRU                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRU_GPU                 source/mpi/implicit/imp_spmd.F
Chd|        TAG_INTML                     source/mpi/implicit/imp_fri.F 
Chd|        TAG_INTS                      source/mpi/implicit/imp_fri.F 
Chd|        TRA_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_I7CP                      share/modules/imp_intm.F      
Chd|-- called by -----------
Chd|        CP_IFRONT                     source/implicit/produt_v.F    
Chd|        CP_IMPBUF                     source/implicit/produt_v.F    
Chd|        IMP_STIF24                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
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

