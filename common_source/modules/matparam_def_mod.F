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
Chd|  MATPARAM_DEF_MOD              modules/matparam_def_mod.F    
Chd|-- called by -----------
Chd|        C3DERII                       starter/source/elements/sh3n/coque3n/c3derii.F
Chd|        C3GRTAILS                     starter/source/elements/sh3n/coque3n/c3grtails.F
Chd|        C3INIT3                       starter/source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      starter/source/elements/shell/coqueba/cbainit3.F
Chd|        CDKDERII                      starter/source/elements/sh3n/coquedk/cdkderii.F
Chd|        CDKINIT3                      starter/source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CDLENI                        starter/source/elements/shell/coque/cdleni.F
Chd|        CGRTAILS                      starter/source/elements/shell/coque/cgrtails.F
Chd|        CINIT3                        starter/source/elements/shell/coque/cinit3.F
Chd|        CNDLENI                       starter/source/elements/shell/coqueba/cndleni.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        FILL_BUFFER_51_0              starter/source/materials/mat/mat051/fill_buffer_51_0.F
Chd|        FUNC_COMP                     starter/source/materials/mat/mat076/law76_upd.F
Chd|        HM_READ_MAT                   starter/source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MAT01                 starter/source/materials/mat/mat001/hm_read_mat01.F
Chd|        HM_READ_MAT02                 starter/source/materials/mat/mat002/hm_read_mat02.F
Chd|        HM_READ_MAT05                 starter/source/materials/mat/mat005/hm_read_mat05.F
Chd|        HM_READ_MAT100                starter/source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT101                starter/source/materials/mat/mat101/hm_read_mat101.F
Chd|        HM_READ_MAT102                starter/source/materials/mat/mat102/hm_read_mat102.F
Chd|        HM_READ_MAT103                starter/source/materials/mat/mat103/hm_read_mat103.F
Chd|        HM_READ_MAT104                starter/source/materials/mat/mat104/hm_read_mat104.F
Chd|        HM_READ_MAT106                starter/source/materials/mat/mat106/hm_read_mat106.F
Chd|        HM_READ_MAT107                starter/source/materials/mat/mat107/hm_read_mat107.F
Chd|        HM_READ_MAT109                starter/source/materials/mat/mat109/hm_read_mat109.F
Chd|        HM_READ_MAT110                starter/source/materials/mat/mat110/hm_read_mat110.F
Chd|        HM_READ_MAT111                starter/source/materials/mat/mat111/hm_read_mat111.F
Chd|        HM_READ_MAT112                starter/source/materials/mat/mat112/hm_read_mat112.F
Chd|        HM_READ_MAT115                starter/source/materials/mat/mat115/hm_read_mat115.F
Chd|        HM_READ_MAT119                starter/source/materials/mat/mat119/hm_read_mat119.F
Chd|        HM_READ_MAT12                 starter/source/materials/mat/mat012/hm_read_mat12.F
Chd|        HM_READ_MAT120                starter/source/materials/mat/mat120/hm_read_mat120.F
Chd|        HM_READ_MAT121                starter/source/materials/mat/mat121/hm_read_mat121.F
Chd|        HM_READ_MAT124                starter/source/materials/mat/mat124/hm_read_mat124.F
Chd|        HM_READ_MAT13                 starter/source/materials/mat/mat013/hm_read_mat13.F
Chd|        HM_READ_MAT14                 starter/source/materials/mat/mat014/hm_read_mat14.F
Chd|        HM_READ_MAT15                 starter/source/materials/mat/mat015/hm_read_mat15.F
Chd|        HM_READ_MAT16                 starter/source/materials/mat/mat016/hm_read_mat16.F
Chd|        HM_READ_MAT18                 starter/source/materials/mat/mat018/hm_read_mat18.F
Chd|        HM_READ_MAT19                 starter/source/materials/mat/mat019/hm_read_mat19.F
Chd|        HM_READ_MAT22                 starter/source/materials/mat/mat022/hm_read_mat22.F
Chd|        HM_READ_MAT23                 starter/source/materials/mat/mat023/hm_read_mat23.F
Chd|        HM_READ_MAT25                 starter/source/materials/mat/mat025/hm_read_mat25.F
Chd|        HM_READ_MAT28                 starter/source/materials/mat/mat028/hm_read_mat28.F
Chd|        HM_READ_MAT32                 starter/source/materials/mat/mat032/hm_read_mat32.F
Chd|        HM_READ_MAT33                 starter/source/materials/mat/mat033/hm_read_mat33.F
Chd|        HM_READ_MAT34                 starter/source/materials/mat/mat034/hm_read_mat34.F
Chd|        HM_READ_MAT36                 starter/source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT37                 starter/source/materials/mat/mat037/hm_read_mat37.F
Chd|        HM_READ_MAT38                 starter/source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT40                 starter/source/materials/mat/mat040/hm_read_mat40.F
Chd|        HM_READ_MAT41                 starter/source/materials/mat/mat041/hm_read_mat41.F.F
Chd|        HM_READ_MAT42                 starter/source/materials/mat/mat042/hm_read_mat42.F
Chd|        HM_READ_MAT43                 starter/source/materials/mat/mat043/hm_read_mat43.F
Chd|        HM_READ_MAT46                 starter/source/materials/mat/mat046/hm_read_mat46.F
Chd|        HM_READ_MAT48                 starter/source/materials/mat/mat048/hm_read_mat48.F
Chd|        HM_READ_MAT50                 starter/source/materials/mat/mat050/hm_read_mat50.F
Chd|        HM_READ_MAT51                 starter/source/materials/mat/mat051/hm_read_mat51.F
Chd|        HM_READ_MAT53                 starter/source/materials/mat/mat053/hm_read_mat53.F
Chd|        HM_READ_MAT57                 starter/source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT59                 starter/source/materials/mat/mat059/hm_read_mat59.F
Chd|        HM_READ_MAT60                 starter/source/materials/mat/mat060/hm_read_mat60.F
Chd|        HM_READ_MAT62                 starter/source/materials/mat/mat062/hm_read_mat62.F
Chd|        HM_READ_MAT63                 starter/source/materials/mat/mat063/hm_read_mat63.F
Chd|        HM_READ_MAT64                 starter/source/materials/mat/mat064/hm_read_mat64.F
Chd|        HM_READ_MAT66                 starter/source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT68                 starter/source/materials/mat/mat068/hm_read_mat68.F
Chd|        HM_READ_MAT69                 starter/source/materials/mat/mat069/hm_read_mat69.F
Chd|        HM_READ_MAT70                 starter/source/materials/mat/mat070/hm_read_mat70.F
Chd|        HM_READ_MAT71                 starter/source/materials/mat/mat071/hm_read_mat71.F
Chd|        HM_READ_MAT72                 starter/source/materials/mat/mat072/hm_read_mat72.F
Chd|        HM_READ_MAT73                 starter/source/materials/mat/mat073/hm_read_mat73.F
Chd|        HM_READ_MAT74                 starter/source/materials/mat/mat074/hm_read_mat74.F
Chd|        HM_READ_MAT75                 starter/source/materials/mat/mat075/hm_read_mat75.F
Chd|        HM_READ_MAT76                 starter/source/materials/mat/mat076/hm_read_mat76.F
Chd|        HM_READ_MAT77                 starter/source/materials/mat/mat077/hm_read_mat77.F
Chd|        HM_READ_MAT78                 starter/source/materials/mat/mat078/hm_read_mat78.F
Chd|        HM_READ_MAT79                 starter/source/materials/mat/mat079/hm_read_mat79.F
Chd|        HM_READ_MAT80                 starter/source/materials/mat/mat080/hm_read_mat80.F
Chd|        HM_READ_MAT81                 starter/source/materials/mat/mat081/hm_read_mat81.F
Chd|        HM_READ_MAT82                 starter/source/materials/mat/mat082/hm_read_mat82.F
Chd|        HM_READ_MAT84                 starter/source/materials/mat/mat084/hm_read_mat84.F
Chd|        HM_READ_MAT87                 starter/source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT88                 starter/source/materials/mat/mat088/hm_read_mat88.F
Chd|        HM_READ_MAT90                 starter/source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MAT92                 starter/source/materials/mat/mat092/hm_read_mat92.F
Chd|        HM_READ_MAT93                 starter/source/materials/mat/mat093/hm_read_mat93.F
Chd|        HM_READ_MAT94                 starter/source/materials/mat/mat094/hm_read_mat94.F
Chd|        HM_READ_MAT95                 starter/source/materials/mat/mat095/hm_read_mat95.F
Chd|        HM_READ_MAT97                 starter/source/materials/mat/mat097/hm_read_mat97.F
Chd|        INIRIG_MAT                    starter/source/elements/initia/inirig_mat.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        INIT_MAT_KEYWORD              starter/source/materials/mat/init_mat_keyword.F
Chd|        INI_MATPARAM                  starter/source/materials/mat/ini_matparam.F
Chd|        LAW104_UPD                    starter/source/materials/mat/mat104/law104_upd.F
Chd|        LAW76_UPD                     starter/source/materials/mat/mat076/law76_upd.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        MATPARAM_R2R_VOID             starter/source/elements/elbuf_init/matparam_r2r_void.F
Chd|        OUTPART5                      starter/source/elements/initia/initia.F
Chd|        READ_MATERIAL_MODELS          starter/source/materials/read_material_models.F
Chd|        SET_ELGROUP_PARAM             starter/source/elements/shell/coque/set_elgroup_param.F
Chd|        SGRTAILS                      starter/source/elements/solid/solide/sgrtails.F
Chd|        UPDMAT                        starter/source/materials/updmat.F
Chd|        WRITE_ELGROUP_PARAM           starter/source/restart/ddsplit/write_elgroup_param.F
Chd|        WRITE_MATPARAM                starter/source/materials/mat/write_matparam.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        BFORC2                        engine/source/ale/bimat/bforc2.F
Chd|        C3FORC3                       engine/source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   engine/source/elements/xfem/c3forc3_crk.F
Chd|        CBAFORC3                      engine/source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     engine/source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      engine/source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        engine/source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    engine/source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        engine/source/materials/mat_share/cmain3.F
Chd|        CMAIN3PINCH                   engine/source/elements/shell/coqueba/cmain3pinch.F
Chd|        CZFORC3                       engine/source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   engine/source/elements/xfem/czforc3_crk.F
Chd|        FORINT                        engine/source/elements/forint.F
Chd|        FORINTC                       engine/source/elements/forintc.F
Chd|        FORINTP                       engine/source/elements/forintp.F
Chd|        IG3DUFORC3                    engine/source/elements/ige3d/ig3duforc3.F
Chd|        MMAIN                         engine/source/materials/mat_share/mmain.F
Chd|        MULAW                         engine/source/materials/mat_share/mulaw.F
Chd|        MULAWC                        engine/source/materials/mat_share/mulawc.F
Chd|        Q4FORC2                       engine/source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        engine/source/elements/solid_2d/quad/qforc2.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        READ_ELGROUP_PARAM            engine/source/output/restart/read_elgroup_param.F
Chd|        READ_MATPARAM                 engine/source/output/restart/read_matparam.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        S10FORC3                      engine/source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      engine/source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      engine/source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       engine/source/elements/solid/solide4/s4forc3.F
Chd|        S4VOLN_M                      engine/source/elements/solid/solide4_sfem/s4voln_m.F
Chd|        S6CFORC3                      engine/source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      engine/source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      engine/source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      engine/source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      engine/source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       engine/source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        engine/source/elements/solid/solide/sforc3.F
Chd|        SIGEPS76                      engine/source/materials/mat/mat076/sigeps76.F
Chd|        SIGEPS76C                     engine/source/materials/mat/mat076/sigeps76c.F
Chd|        SPSTRES                       engine/source/elements/sph/spstres.F
Chd|        SZFORC3                       engine/source/elements/solid/solidez/szforc3.F
Chd|        WRITE_ELGROUP_PARAM           engine/source/output/restart/write_elgroup_param.F
Chd|        WRITE_MATPARAM                engine/source/output/restart/write_matparam.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        TABLE_MOD                     starter/share/modules1/table_mod.F
Chd|        TABLE_MOD                     engine/share/modules/table_mod.F
Chd|====================================================================
      MODULE MATPARAM_DEF_MOD
      USE TABLE_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
#include "nchar_c.inc"
c=======================================================================      
c define type MATPARAM_STRUCT_ for general material data structure table
c allocatable dimension : NUMMAT
c=======================================================================      
c 
      TYPE MATPARAM_STRUCT_
        integer     :: ILAW    
        integer     :: MAT_ID
        character   :: TITLE*nchartitle
        integer     :: NTABLE
        integer     :: COMPRESSIBILITY      ! "compressible","incompressible","elasto_plastic"
        integer     :: SMSTR                ! "small_strain", "large_strain"
        integer     :: STRAIN_FORMULATION   ! "total", "incremental"
        integer     :: IPRES                ! "hydrostatic",hydro_eos","hook"
        integer     :: ORTHOTROPY           ! "isotropic", "orthotropic", "anisotropic"
        TYPE (TTABLE) ,DIMENSION(:) ,ALLOCATABLE :: TABLE   ! local table of functions
      END TYPE MATPARAM_STRUCT_
c=======================================================================      
c define type GRP_PARAM_STRUCT_ for element group parameters depending on property/material
c allocatable dimension : NGROUP
c=======================================================================      
c 
      TYPE GRP_PARAM_STRUCT_
c        integer     :: IMAT    
c        integer     :: IPROP
        my_real     :: VISC_DN
        my_real     :: VISC_DM
      END TYPE GRP_PARAM_STRUCT_
c
c---------------
      END MODULE MATPARAM_DEF_MOD
