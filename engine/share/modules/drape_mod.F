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
Chd|  DRAPE_MOD                     share/modules/drape_mod.F     
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CMAIN3PINCH                   source/elements/shell/coqueba/cmain3pinch.F
Chd|        CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|        CNCOEFORT                     source/elements/sh3n/coquedk/cncoef3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        C_TF_NE                       source/output/sty/c_tf_ne.F   
Chd|        DRAPE_RREST                   source/output/restart/rdresb.F
Chd|        DRAPE_WREST                   source/output/restart/wrrest.F
Chd|        DYNAIN_C_STRAG                source/output/dynain/dynain_c_strag.F
Chd|        DYNAIN_C_STRSG                source/output/dynain/dynain_c_strsg.F
Chd|        EIG                           stub/eig.F                    
Chd|        EIG1                          stub/eig1.F                   
Chd|        EIGCOND                       stub/eigcond.F                
Chd|        EIGP                          stub/eigp.F                   
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        OUTP_C_TF                     source/output/sty/outp_c_t.F  
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
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
        integer                                 :: NDIM
        integer, DIMENSION(:),    ALLOCATABLE   :: INDX
        integer, DIMENSION(:) ,   ALLOCATABLE   :: NPT
        integer, DIMENSION(:) ,   ALLOCATABLE   :: IDRAPE
        my_real, DIMENSION(:,:) , ALLOCATABLE   :: RDRAPE  
      END TYPE DRAPE_   
C=======================================================================
      END MODULE DRAPE_MOD
