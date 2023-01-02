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
Chd|  HM_READ_MAT51_IFORM11         source/materials/mat/mat051/hm_read_mat51_iform11.F
Chd|-- called by -----------
Chd|        HM_READ_MAT51                 source/materials/mat/mat051/hm_read_mat51.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT51_IFORM11(
     .    AV,     RHO0_,     E0,       PM,     C0 ,
     .    C1,     C2,        C3,       C4,     C5 ,
     .    IPLA,   GG,        Y,        BB,     N  ,
     .    CC,     EPDR,      CM,       T0,     TMELT,
     .    THETL,  SPH,       PLAMX,    SIGMX,  XKA,
     .    XKB,    NU,        A0,       A1,     A2 ,
     .    AMX,    BUNL,      MUMX,    
     .    B1    , B2    ,    R1     ,  R2     ,W,
     .    VDET  , PCJ   ,    IBFRAC ,  PEXT   ,VIS,
     .    VISV   ,IS_ENCRYPTED,LSUBMODEL,UNITAB) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"     
C-----------------------------------------------    
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "mmale51_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IPLA(4), J
      my_real
     .    AV(4),   RHO0_(4), E0(4),    PM(4),    C0(4)   ,
     .    C1(4),   C2(4),   C3(4),    C4(4),    C5(4)   ,
     .    GG(4),   Y(4),    BB(4),    N(4)    ,
     .    CC(4),   EPDR(4), CM(4),    T0(4),    TMELT(4),
     .    THETL(4),SPH(4) , PLAMX(4), SIGMX(4), XKA(4)  ,
     .    XKB(4),  NU(4),   A0(4),    A1(4),    A2(4)   ,
     .    AMX(4),  BUNL(4), MUMX(4), 
     .    B1,B2,R1,R2,W,VDET,PCJ,IBFRAC,
     .    PEXT,VIS,VISV
      my_real BID
      LOGICAL,INTENT(INOUT) :: IS_ENCRYPTED
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   P a r a m e t e r s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE
      INTEGER :: I
C-----------------------------------------------
C   P r e c o n d i t i o n
C-----------------------------------------------
      !PRECONDITION
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      CALL HM_GET_FLOATV('PEXT'       ,PEXT         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,VIS          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Lamda'  ,VISV         ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      I=1
      IF(IPLA(I)==0)THEN                                                                                                    
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETAN' ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BETA' ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E0'   ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PC'   ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0'   ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1'   ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C2'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3'   ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4'   ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C5'   ,C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPS'  ,GG(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_M'    ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_TMAX' ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BULK' ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('DAMP1'    ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A0'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A1'   ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A2'   ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_AMAX' ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
      ELSEIF(IPLA(I)==1)THEN                                                                                           
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETAN' ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BETA' ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E0'   ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PC'   ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0'   ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1'   ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C2'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3'   ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4'   ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C5'   ,C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPS'  ,GG(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EA'   ,Y(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EB'   ,BB(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EC'   ,N(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PRAB' ,CC(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PRBC' ,EPDR(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PRCA' ,CM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_M'    ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_TMAX' ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BULK' ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('DAMP1'    ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A0'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A1'   ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A2'   ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_AMAX' ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
      ELSEIF(IPLA(I)==2)THEN                                                                                    
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETAN' ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BETA' ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E0'   ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PC'   ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0'   ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1'   ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C2'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3'   ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4'   ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C5'   ,C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPS'  ,A0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EA'   ,A1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EB'   ,A2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EC'   ,AMX(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)            
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PRAB' ,Y(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PRBC' ,NU(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_M'    ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_TMAX' ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BULK' ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('DAMP1'    ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A0'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A1'   ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_A2'   ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_AMAX' ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
      ENDIF                                                                                                          

      I=2
      IF(IPLA(I)==0)THEN                                                                                                    
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGYC1'  ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGYC2'  ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGT12'  ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGT23'  ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGC12'  ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGC23'  ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EFIB'    ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_REDFACT' ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_K_UNLOAD',C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_FtFc'    ,GG(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_FbFc'    ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_F2Fc'    ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SoFc'    ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PPRES'   ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_YPRES'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BPMOD'   ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DIL_Y'   ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DIL_F'   ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
      ELSEIF(IPLA(I)==1)THEN                                                                                           
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGYC1'  ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGYC2'  ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGT12'  ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGT23'  ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGC12'  ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGC23'  ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EFIB'    ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_REDFACT' ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_K_UNLOAD',C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_FtFc'    ,GG(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_COMPAC'  ,Y(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CAP_BEG' ,BB(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CAP_END' ,N(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_TPMOD'  ,CC(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E2'      ,EPDR(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_G0'      ,CM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_FbFc'    ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_F2Fc'    ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SoFc'    ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PPRES'   ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_YPRES'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BPMOD'   ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DIL_Y'   ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DIL_F'   ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
      ELSEIF(IPLA(I)==2)THEN                                                                                    
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGYC1'  ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGYC2'  ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGT12'  ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGT23'  ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGC12'  ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SIGC23'  ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EFIB'    ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_REDFACT' ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_K_UNLOAD',C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_FtFc'    ,A0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_COMPAC'  ,A1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CAP_BEG' ,A2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CAP_END' ,AMX(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)            
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_TPMOD'   ,Y(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E2'      ,NU(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_FbFc'    ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_F2Fc'    ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SoFc'    ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PPRES'   ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_YPRES'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_BPMOD'   ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DIL_Y'   ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DIL_F'   ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                    
      ENDIF                  

      I=3
      IF(IPLA(I)==0)THEN                                                                                                    
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SSIG'    ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SETAN'   ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR1'   ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR2'   ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR3'   ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPST1'   ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSM1'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPST2'   ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSM2'   ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('WPMAX'       ,C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSF1'   ,GG(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSF2'   ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DAMAGE2' ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P0'      ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PHI  '   ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_GAMA0'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E1'      ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETA1'    ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETA2'    ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
      ELSEIF(IPLA(I)==1)THEN                                                                                           
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SSIG'    ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SETAN'   ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR1'   ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR2'   ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR3'   ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPST1'   ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSM1'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPST2'   ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSM2'   ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('WPMAX'       ,C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSF1'   ,GG(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_GI'      ,Y(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DECAY'   ,BB(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CO1'     ,N(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CO2'     ,CC(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CO3'     ,EPDR(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_NUt'     ,CM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSF2'   ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DAMAGE2' ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P0'      ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PHI  '   ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_GAMA0'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E1'      ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETA1'    ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETA2'    ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                    
      ELSEIF(IPLA(I)==2)THEN                                                                                    
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SSIG'    ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SETAN'   ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR1'   ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR2'   ,PM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                     
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PDIR3'   ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPST1'   ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSM1'   ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPST2'   ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSM2'   ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('WPMAX'       ,C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSF1'   ,A0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                       
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_GI'      ,A1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DECAY'   ,A2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CO1'     ,AMX(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)            
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CO2'     ,Y(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_CO3'     ,NU(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_EPSF2'   ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_DAMAGE2' ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P0'      ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_PHI  '   ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_GAMA0'   ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E1'      ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETA1'    ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                      
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ETA2'    ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)                   
      ENDIF       

      I=4
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_ALPHA'            ,AV(4)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_Rho'              ,RHO0_(4),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_E0'               ,E0(4)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_Pmin'             ,PM(4)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_C0'               ,C0(4)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_B1'               ,B1      ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_B2'               ,B2      ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_R1'               ,R1      ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_R2'               ,R2      ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_W'                ,W       ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_D'                ,VDET    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_PCJ'              ,PCJ     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_C14'              ,C1(4)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_INT_ARRAY_INDEX  ('MAT_IBFRAC'              ,IBFRAC  ,I ,IS_AVAILABLE, LSUBMODEL)        
C-----------------------------------------------
      RETURN 
      END SUBROUTINE
