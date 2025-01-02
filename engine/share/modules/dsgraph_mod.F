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
      !||    dsgraph_mod      ../engine/share/modules/dsgraph_mod.F
      !||--- called by ------------------------------------------------------
      !||    dsrrest          ../engine/source/output/restart/rdresb.F
      !||    dswrest          ../engine/source/output/restart/wrrest.F
      !||    eigcond          ../engine/stub/eigcond.F
      !||    imp_buck         ../engine/source/implicit/imp_buck.F
      !||    imp_chkm         ../engine/source/implicit/imp_solv.F
      !||    imp_dsfext       ../engine/source/implicit/imp_dsfext.F
      !||    imp_dsolv        ../engine/source/implicit/imp_dsolv.F
      !||    imp_dsolv_iter   ../engine/source/implicit/imp_dsolv.F
      !||    imp_lanzp        ../engine/source/implicit/imp_lanz.F
      !||    imp_pcgh         ../engine/source/implicit/imp_pcg.F
      !||    imp_ppcgh        ../engine/source/implicit/imp_pcg.F
      !||    imp_solv         ../engine/source/implicit/imp_solv.F
      !||    lin_solv         ../engine/source/implicit/lin_solv.F
      !||    lin_solv2        ../engine/source/implicit/lin_solv.F
      !||    lin_solvh0       ../engine/source/implicit/lin_solv.F
      !||    lin_solvh1       ../engine/source/implicit/lin_solv.F
      !||    lin_solvhm       ../engine/source/implicit/lin_solv.F
      !||    lin_solvih2      ../engine/source/implicit/lin_solv.F
      !||    lin_solvp2       ../engine/source/implicit/lin_solv.F
      !||    mumpslb          ../engine/source/implicit/lin_solv.F
      !||    mumpslb_hp       ../engine/source/implicit/lin_solv.F
      !||    nl_solv          ../engine/source/implicit/nl_solv.F
      !||    prec_solv        ../engine/source/implicit/prec_solv.F
      !||    prec_solvh       ../engine/source/implicit/prec_solv.F
      !||    prec_solvp       ../engine/source/implicit/prec_solv.F
      !||    radioss2         ../engine/source/engine/radioss2.F
      !||    resol            ../engine/source/engine/resol.F
      !||    resol_head       ../engine/source/engine/resol_head.F
      !||====================================================================
      MODULE DSGRAPH_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE SUPEL
         INTEGER :: NDDL_I, NDDL_F, NSDMAX,
     .              NDDL_M, NMDYN, KREC, MREC, MDREC, KIREC, 
     .              NDDL_I_INI, NDDL_F_INI
         INTEGER, DIMENSION(:), POINTER :: DDLS, DDLS_INI, CHILD, CMDYN,
     .                                     IFACM, IFACM_INI
         INTEGER, DIMENSION(:,:), POINTER :: IFAC, IFAC_INI
         my_real
     .          , DIMENSION(:,:), POINTER :: B, IFI, D, DM
      END TYPE SUPEL
C      
      TYPE PRGRAPH
         INTEGER :: NDDL, NDDL_INI, NSUPEL, IFILE, NSLEVEL, NSDEC,
     .              NSVMAX, IPRI, NDDL_GLOB, NNRP, NNCP, NDDLC
         my_real
     .           :: CUTFREQ
         INTEGER, DIMENSION(:,:), POINTER :: LSDDL, LSDDL_INI,
     .                                       IPROW, IPCOL
         INTEGER, DIMENSION(:), POINTER :: LSDDL_GLOB, LSDDL_GLOB_INI
         TYPE(SUPEL), DIMENSION(:), POINTER :: DGRAPH
      END TYPE PRGRAPH
C
      END MODULE DSGRAPH_MOD
      
