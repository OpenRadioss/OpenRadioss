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
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|-- called by -----------
Chd|        APPLYSORT2FVM                 source/airbag/fvmesh0.F       
Chd|        C_FVBAG                       source/airbag/c_fvbag.F       
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        FVBRIC                        source/airbag/fvbric.F        
Chd|        FVDEAL                        source/airbag/fvmesh.F        
Chd|        FVDIM                         source/airbag/fvmesh.F        
Chd|        FVELINTE                      source/airbag/fvelinte.F      
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        FVNODBR                       source/airbag/fvmbag1.F       
Chd|        FVWRESTP                      source/restart/ddsplit/fvwrestp.F
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        NAME_FVBAG                    source/output/th/hm_thgrki_vent.F
Chd|        W_FRONT                       source/restart/ddsplit/w_front.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE FVBAG_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE FVBAG_DATA
C-------------------------------
C     Courant number (DT scale factor) & Time Step options
C-------------------------------
      my_real CFL_COEF, DTMIN
      my_real LAMBDA       ! /DT/FVMBAG param 3
      my_real DTOLD        ! FVMBAG time step from previous cycle
      INTEGER L_TYPE       ! FVMBAG characteristic length option      
      INTEGER ID_DT_OPTION ! /DT/FVMBAG/[ID_DT_OPTION]
C------------------------------------------------
C     Dispersion of pressure around mean pressure
C------------------------------------------------
      my_real PDISP_OLD, PDISP
C-------------------------------
C Donnees briques intersectantes
C-------------------------------
         INTEGER, DIMENSION(:,:), POINTER :: BRIC, TBRIC 
         my_real,
     .            DIMENSION(:,:), POINTER :: XB
         my_real,
     .            DIMENSION(:,:,:), POINTER :: SFAC
C-----------
C Dimensions
C-----------
         INTEGER NNS, NNTR, LENP, NPOLY, LENH, NPOLH
C----------------
C Anim polyhedres
C----------------
         INTEGER NPOLH_ANIM, NNS_ANIM, ID
         INTEGER, DIMENSION(:), POINTER :: IFVPOLY_ANIM, IFVTADR_ANIM,
     .                                     IFVPOLH_ANIM, IFVPADR_ANIM,
     .                                     REDIR_ANIM
         INTEGER, DIMENSION(:,:), POINTER :: IFVTRI_ANIM
         my_real
     .          , DIMENSION(:,:), POINTER :: NOD_ANIM
C------------------------
C Polygones et polyhedres
C------------------------
         INTEGER, DIMENSION(:), POINTER :: IFVPOLY, IFVTADR,
     .                                     IFVPOLH, IFVPADR, IDPOLH,
     .                                     IBPOLH
         INTEGER, DIMENSION(:,:), POINTER :: IFVNOD, IFVTRI
         my_real
     .          , DIMENSION(:,:), POINTER :: RFVNOD
C------------------------
C Variables volumes finis
C------------------------
         my_real
     .          , DIMENSION(:), POINTER :: MPOLH,   EPOLH,   PPOLH,
     .                                     RPOLH,   GPOLH,   TPOLH,
     .                                     CPAPOLH, CPBPOLH, CPCPOLH, 
     .                                     CPDPOLH, CPEPOLH, CPFPOLH,
     .                                     RMWPOLH, VPOLH_INI
         my_real
     .          , DIMENSION(:,:), POINTER :: QPOLH
C---------------------------
C Variables stability
C---------------------------
         my_real
     .           DLH
         my_real
     .          , DIMENSION(:), POINTER :: DTPOLH
C
      END TYPE FVBAG_DATA
C---------------------------
C Data SPMD
C---------------------------
      TYPE FVBAG_SPMD
          INTEGER NN_L, NNA_L, NNSA, NNSA_L, NSA, NELSA, PMAIN, NNI_L
          INTEGER, DIMENSION(:,:), POINTER :: IBUF_L, IBUFA_L, IBUFSA_L,
     .                                        IXSA, ELEMSA
      END TYPE FVBAG_SPMD
c      INTEGER, DIMENSION(:,:), ALLOCATABLE :: FVBAG_SAV_ELEM
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s   D e f i n i t i o n
C-----------------------------------------------
      INTEGER NFVBAG
      TYPE(FVBAG_DATA), DIMENSION(:), ALLOCATABLE :: FVDATA
      TYPE(FVBAG_SPMD), DIMENSION(:), ALLOCATABLE :: FVSPMD
      INTEGER, DIMENSION(:), ALLOCATABLE :: FVID

c after bug detected on fvmbags qa after starter parallelization
!$OMP THREADPRIVATE(FVSPMD) 

C
      END MODULE FVBAG_MOD
