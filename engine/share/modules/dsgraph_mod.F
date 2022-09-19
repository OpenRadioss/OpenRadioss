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
Cf51e6 Nouvelle routine
Chd|====================================================================
Chd|  DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|-- called by -----------
Chd|        DSACTI1                       source/implicit/dsolve/dsacti.F
Chd|        DSACTI2                       source/implicit/dsolve/dsacti.F
Chd|        DSALLOC                       source/implicit/dsolve/dsgest.F
Chd|        DSCALN                        source/implicit/dsolve/dscaln.F
Chd|        DSCDF                         source/implicit/dsolve/dscdf.F
Chd|        DSCDKG                        source/implicit/dsolve/dscdkg.F
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|        DSDEAL                        source/implicit/dsolve/dsgest.F
Chd|        DSDESC                        source/implicit/dsolve/dsdesc.F
Chd|        DSEXTR2                       source/implicit/dsolve/dsextr.F
Chd|        DSGRI7                        source/implicit/dsolve/dsgri7.F
Chd|        DSMAJI7                       source/implicit/dsolve/dsgri7.F
Chd|        DSOPEN                        source/implicit/dsolve/dsgest.F
Chd|        DSPGRID                       source/implicit/dsolve/dspgrid.F
Chd|        DSRECU                        source/implicit/dsolve/dsrecu.F
Chd|        DSRENUM                       source/implicit/dsolve/dsrenum.F
Chd|        DSRENUM0                      source/implicit/dsolve/dsrenum.F
Chd|        DSRESOL                       source/implicit/dsolve/dsresol.F
Chd|        DSRESOLP                      source/implicit/dsolve/dsresolp.F
Chd|        DSRESOLP_BUCK                 source/implicit/dsolve/dsresolp.F
Chd|        DSRESOLP_ITER                 source/implicit/dsolve/dsresolp.F
Chd|        DSRESOL_BUCK                  source/implicit/dsolve/dsresol.F
Chd|        DSRESOL_ITER                  source/implicit/dsolve/dsresol.F
Chd|        DSRREST                       source/output/restart/rdresb.F
Chd|        DSWREST                       source/output/restart/wrrest.F
Chd|        DSXKG2                        source/implicit/dsolve/dsextr.F
Chd|        EIGCOND                       stub/eigcond.F                
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_DSFEXT                    source/implicit/imp_dsfext.F  
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|        IMP_LANZP                     source/implicit/imp_lanz.F    
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        LIN_SOLV2                     source/implicit/lin_solv.F    
Chd|        LIN_SOLVH0                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|        MUMPSLB                       source/implicit/lin_solv.F    
Chd|        MUMPSLB_HP                    source/implicit/lin_solv.F    
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|        PREC_SOLV                     source/implicit/prec_solv.F   
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|        PREC_SOLVP                    source/implicit/prec_solv.F   
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|-- calls ---------------
Chd|====================================================================
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

