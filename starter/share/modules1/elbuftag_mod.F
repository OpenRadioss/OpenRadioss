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
Chd|  ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|-- called by -----------
Chd|        DEALLOCATE_ELBUF              source/elements/elbuf_init/deallocate_buffer.F
Chd|        ELBUF_INI                     source/elements/elbuf_init/elbuf_ini.F
Chd|        HM_PROP_READ21                source/properties/thickshell/hm_read_prop21.F
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|        HM_READ_EOS_COMPACTION        source/materials/eos/hm_read_eos_compaction.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FAIL_ALTER            source/materials/fail/windshield_alter/hm_read_fail_alter.F
Chd|        HM_READ_INJECT1               source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_INTER_TYPE10          source/interfaces/int10/hm_read_inter_type10.F
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MAT00                 source/materials/mat/mat000/hm_read_mat00.F
Chd|        HM_READ_MAT02                 source/materials/mat/mat002/hm_read_mat02.F
Chd|        HM_READ_MAT03                 source/materials/mat/mat003/hm_read_mat03.F
Chd|        HM_READ_MAT04                 source/materials/mat/mat004/hm_read_mat04.F
Chd|        HM_READ_MAT05                 source/materials/mat/mat005/hm_read_mat05.F
Chd|        HM_READ_MAT06                 source/materials/mat/mat006/hm_read_mat06.F
Chd|        HM_READ_MAT06_KEPS            source/materials/mat/mat006/hm_read_mat06_keps.F
Chd|        HM_READ_MAT10                 source/materials/mat/mat010/hm_read_mat10.F
Chd|        HM_READ_MAT100                source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT102                source/materials/mat/mat102/hm_read_mat102.F
Chd|        HM_READ_MAT103                source/materials/mat/mat103/hm_read_mat103.F
Chd|        HM_READ_MAT104                source/materials/mat/mat104/hm_read_mat104.F
Chd|        HM_READ_MAT106                source/materials/mat/mat106/hm_read_mat106.F
Chd|        HM_READ_MAT107                source/materials/mat/mat107/hm_read_mat107.F
Chd|        HM_READ_MAT108                source/materials/mat/mat108/hm_read_mat108.F
Chd|        HM_READ_MAT109                source/materials/mat/mat109/hm_read_mat109.F
Chd|        HM_READ_MAT11                 source/materials/mat/mat011/hm_read_mat11.F
Chd|        HM_READ_MAT110                source/materials/mat/mat110/hm_read_mat110.F
Chd|        HM_READ_MAT111                source/materials/mat/mat111/hm_read_mat111.F
Chd|        HM_READ_MAT112                source/materials/mat/mat112/hm_read_mat112.F
Chd|        HM_READ_MAT113                source/materials/mat/mat113/hm_read_mat113.F
Chd|        HM_READ_MAT114                source/materials/mat/mat114/hm_read_mat114.F
Chd|        HM_READ_MAT115                source/materials/mat/mat115/hm_read_mat115.F
Chd|        HM_READ_MAT116                source/materials/mat/mat116/hm_read_mat116.F
Chd|        HM_READ_MAT117                source/materials/mat/mat117/hm_read_mat117.F
Chd|        HM_READ_MAT119                source/materials/mat/mat119/hm_read_mat119.F
Chd|        HM_READ_MAT11_K_EPS           source/materials/mat/mat011/hm_read_mat11_k_eps.F
Chd|        HM_READ_MAT12                 source/materials/mat/mat012/hm_read_mat12.F
Chd|        HM_READ_MAT120                source/materials/mat/mat120/hm_read_mat120.F
Chd|        HM_READ_MAT121                source/materials/mat/mat121/hm_read_mat121.F
Chd|        HM_READ_MAT13                 source/materials/mat/mat013/hm_read_mat13.F
Chd|        HM_READ_MAT14                 source/materials/mat/mat014/hm_read_mat14.F
Chd|        HM_READ_MAT15                 source/materials/mat/mat015/hm_read_mat15.F
Chd|        HM_READ_MAT151                source/materials/mat/mat151/hm_read_mat151.F
Chd|        HM_READ_MAT16                 source/materials/mat/mat016/hm_read_mat16.F
Chd|        HM_READ_MAT18                 source/materials/mat/mat018/hm_read_mat18.F
Chd|        HM_READ_MAT19                 source/materials/mat/mat019/hm_read_mat19.F
Chd|        HM_READ_MAT20                 source/materials/mat/mat020/hm_read_mat20.F
Chd|        HM_READ_MAT21                 source/materials/mat/mat021/hm_read_mat21.F
Chd|        HM_READ_MAT22                 source/materials/mat/mat022/hm_read_mat22.F
Chd|        HM_READ_MAT23                 source/materials/mat/mat023/hm_read_mat23.F
Chd|        HM_READ_MAT24                 source/materials/mat/mat024/hm_read_mat24.F
Chd|        HM_READ_MAT25                 source/materials/mat/mat025/hm_read_mat25.F
Chd|        HM_READ_MAT26                 source/materials/mat/mat026/hm_read_mat26.F
Chd|        HM_READ_MAT27                 source/materials/mat/mat027/hm_read_mat27.F
Chd|        HM_READ_MAT28                 source/materials/mat/mat028/hm_read_mat28.F
Chd|        HM_READ_MAT32                 source/materials/mat/mat032/hm_read_mat32.F
Chd|        HM_READ_MAT33                 source/materials/mat/mat033/hm_read_mat33.F
Chd|        HM_READ_MAT34                 source/materials/mat/mat034/hm_read_mat34.F
Chd|        HM_READ_MAT35                 source/materials/mat/mat035/hm_read_mat35.F
Chd|        HM_READ_MAT36                 source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT37                 source/materials/mat/mat037/hm_read_mat37.F
Chd|        HM_READ_MAT38                 source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT40                 source/materials/mat/mat040/hm_read_mat40.F
Chd|        HM_READ_MAT43                 source/materials/mat/mat043/hm_read_mat43.F
Chd|        HM_READ_MAT44                 source/materials/mat/mat044/hm_read_mat44.F
Chd|        HM_READ_MAT46                 source/materials/mat/mat046/hm_read_mat46.F
Chd|        HM_READ_MAT48                 source/materials/mat/mat048/hm_read_mat48.F
Chd|        HM_READ_MAT49                 source/materials/mat/mat049/hm_read_mat49.F
Chd|        HM_READ_MAT50                 source/materials/mat/mat050/hm_read_mat50.F
Chd|        HM_READ_MAT51                 source/materials/mat/mat051/hm_read_mat51.F
Chd|        HM_READ_MAT52                 source/materials/mat/mat052/hm_read_mat52.F
Chd|        HM_READ_MAT53                 source/materials/mat/mat053/hm_read_mat53.F
Chd|        HM_READ_MAT54                 source/materials/mat/mat054/hm_read_mat54.F
Chd|        HM_READ_MAT57                 source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT58                 source/materials/mat/mat058/hm_read_mat58.F
Chd|        HM_READ_MAT59                 source/materials/mat/mat059/hm_read_mat59.F
Chd|        HM_READ_MAT60                 source/materials/mat/mat060/hm_read_mat60.F
Chd|        HM_READ_MAT63                 source/materials/mat/mat063/hm_read_mat63.F
Chd|        HM_READ_MAT64                 source/materials/mat/mat064/hm_read_mat64.F
Chd|        HM_READ_MAT65                 source/materials/mat/mat065/hm_read_mat65.F
Chd|        HM_READ_MAT66                 source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT68                 source/materials/mat/mat068/hm_read_mat68.F
Chd|        HM_READ_MAT69                 source/materials/mat/mat069/hm_read_mat69.F
Chd|        HM_READ_MAT70                 source/materials/mat/mat070/hm_read_mat70.F
Chd|        HM_READ_MAT71                 source/materials/mat/mat071/hm_read_mat71.F
Chd|        HM_READ_MAT72                 source/materials/mat/mat072/hm_read_mat72.F
Chd|        HM_READ_MAT73                 source/materials/mat/mat073/hm_read_mat73.F
Chd|        HM_READ_MAT74                 source/materials/mat/mat074/hm_read_mat74.F
Chd|        HM_READ_MAT75                 source/materials/mat/mat075/hm_read_mat75.F
Chd|        HM_READ_MAT76                 source/materials/mat/mat076/hm_read_mat76.F
Chd|        HM_READ_MAT77                 source/materials/mat/mat077/hm_read_mat77.F
Chd|        HM_READ_MAT78                 source/materials/mat/mat078/hm_read_mat78.F
Chd|        HM_READ_MAT79                 source/materials/mat/mat079/hm_read_mat79.F
Chd|        HM_READ_MAT80                 source/materials/mat/mat080/hm_read_mat80.F
Chd|        HM_READ_MAT81                 source/materials/mat/mat081/hm_read_mat81.F
Chd|        HM_READ_MAT83                 source/materials/mat/mat083/hm_read_law83.F
Chd|        HM_READ_MAT84                 source/materials/mat/mat084/hm_read_mat84.F
Chd|        HM_READ_MAT87                 source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT88                 source/materials/mat/mat088/hm_read_mat88.F
Chd|        HM_READ_MAT90                 source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MAT92                 source/materials/mat/mat092/hm_read_mat92.F
Chd|        HM_READ_MAT93                 source/materials/mat/mat093/hm_read_mat93.F
Chd|        HM_READ_MAT94                 source/materials/mat/mat094/hm_read_mat94.F
Chd|        HM_READ_MAT95                 source/materials/mat/mat095/hm_read_mat95.F
Chd|        HM_READ_MAT97                 source/materials/mat/mat097/hm_read_mat97.F
Chd|        HM_READ_PROP0                 source/properties/void/hm_read_prop0.F
Chd|        HM_READ_PROP01                source/properties/shell/hm_read_prop01.F
Chd|        HM_READ_PROP02                source/properties/truss/hm_read_prop02.F
Chd|        HM_READ_PROP03                source/properties/beam/hm_read_prop03.F
Chd|        HM_READ_PROP04                source/properties/spring/hm_read_prop04.F
Chd|        HM_READ_PROP06                source/properties/solid/hm_read_prop06.F
Chd|        HM_READ_PROP08                source/properties/spring/hm_read_prop08.F
Chd|        HM_READ_PROP09                source/properties/shell/hm_read_prop09.F
Chd|        HM_READ_PROP10                source/properties/shell/hm_read_prop10.F
Chd|        HM_READ_PROP11                source/properties/shell/hm_read_prop11.F
Chd|        HM_READ_PROP12                source/properties/spring/hm_read_prop12.F
Chd|        HM_READ_PROP13                source/properties/spring/hm_read_prop13.F
Chd|        HM_READ_PROP14                source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP14F               source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP15                source/properties/solid/hm_read_prop15.F
Chd|        HM_READ_PROP16                source/properties/shell/hm_read_prop16.F
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP18                source/properties/beam/hm_read_prop18.F
Chd|        HM_READ_PROP20                source/properties/thickshell/hm_read_prop20.F
Chd|        HM_READ_PROP22                source/properties/thickshell/hm_read_prop22.F
Chd|        HM_READ_PROP23                source/properties/spring/hm_read_prop23.F
Chd|        HM_READ_PROP25                source/properties/spring/hm_read_prop25.F
Chd|        HM_READ_PROP26                source/properties/spring/hm_read_prop26.F
Chd|        HM_READ_PROP27                source/properties/spring/hm_read_prop27.F
Chd|        HM_READ_PROP28                source/properties/xelem/hm_read_prop28.F
Chd|        HM_READ_PROP32                source/properties/spring/hm_read_prop32.F
Chd|        HM_READ_PROP33                source/properties/spring/hm_read_prop33.F
Chd|        HM_READ_PROP34                source/properties/sph/hm_read_prop34.F
Chd|        HM_READ_PROP35                source/properties/spring/hm_read_prop35.F
Chd|        HM_READ_PROP36                source/properties/spring/hm_read_prop36.F
Chd|        HM_READ_PROP43                source/properties/solid/hm_read_prop43.F
Chd|        HM_READ_PROP44                source/properties/spring/hm_read_prop44.F
Chd|        HM_READ_PROP45                source/properties/spring/hm_read_prop45.F
Chd|        HM_READ_PROP46                source/properties/spring/hm_read_prop46.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        HM_READ_THERM_STRESS          source/materials/therm/hm_read_therm_stress.F
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|        INITVARS_AUTO                 source/elements/elbuf_init/initvars_auto.F
Chd|        INIT_MLAW_TAG                 source/elements/elbuf_init/init_mlaw_tag.F
Chd|        INI_EOS_VARS                  source/elements/elbuf_init/ini_eos_vars.F
Chd|        INI_MLAW_VARS                 source/elements/elbuf_init/ini_mlaw_vars.F
Chd|        INI_PROP_VARS                 source/elements/elbuf_init/ini_prop_vars.F
Chd|        LAW104_UPD                    source/materials/mat/mat104/law104_upd.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        M20DCOD                       source/system/fsdcod.F        
Chd|        MTAG_R2R_VOID                 source/elements/elbuf_init/mtag_r2r_void.F
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|        ZEROVARS_AUTO                 source/elements/elbuf_init/zerovars_auto.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ELBUFTAG_MOD
c-----------------------------------------------------------------------
c     add here new variables used by material laws preceded by L_ or G_prefix
c-----------------------------------------------------------------------
c
      TYPE MLAW_TAG_    ! define element variables used by each material law
        INTEGER ::  G_GAMA         !    
        INTEGER ::  G_EINT         !  
        INTEGER ::  G_EINS         !     
        INTEGER ::  G_RHO          !         
        INTEGER ::  G_DELTAX       !  
        INTEGER ::  G_TEMP         !  
        INTEGER ::  G_PLA          !     
        INTEGER ::  G_EPSD         !  
        INTEGER ::  G_TB           !  Time Burn   
        INTEGER ::  G_RK           !  Turbulent Energy   (K - kinetic)
        INTEGER ::  G_RE           !  Turbulent Dissipation (epsilon)
        INTEGER ::  G_BFRAC        !  Burn fraction   law 5, 51, 41 
        INTEGER ::  G_SIGI
        INTEGER ::  G_EPSQ         !  lo10 -51 Drucker-Prager
        INTEGER ::  G_IERR
        INTEGER ::  G_DAMDL
        INTEGER ::  G_SEQ          ! equivalent stress (/= VON MISES)
        INTEGER ::  G_ABURN        !  Additionnal Burn Rate for (post)combustion : exemple : Miller's extension of JWL 
        INTEGER ::  G_MU           !  Additionnal Burn Rate for (post)combustion : exemple : Miller's extension of JWL 
        INTEGER ::  G_MAXFRAC
        INTEGER ::  G_MAXEPS
        INTEGER ::  G_DMG          !  
c for spring with mat
        INTEGER ::  G_TOTDEPL
        INTEGER ::  G_TOTROT
        INTEGER ::  G_DEP_IN_TENS  ! DEPX
        INTEGER ::  G_DEP_IN_COMP  ! DEPX2
        INTEGER ::  G_ROT_IN_TENS  ! RPX
        INTEGER ::  G_ROT_IN_COMP  ! RPX2
        INTEGER ::  G_POSX
        INTEGER ::  G_POSY
        INTEGER ::  G_POSZ
        INTEGER ::  G_POSXX
        INTEGER ::  G_POSYY
        INTEGER ::  G_POSZZ
        INTEGER ::  G_YIELD
        INTEGER ::  G_RUPTCRIT
        INTEGER ::  G_NUVAR
        INTEGER ::  G_MASS
        INTEGER ::  G_SKEW_ID
c for seatbelt elements
        INTEGER ::  G_SLIPRING_ID
        INTEGER ::  G_SLIPRING_FRAM_ID
        INTEGER ::  G_SLIPRING_STRAND
        INTEGER ::  G_RETRACTOR_ID
        INTEGER ::  G_RINGSLIP
        INTEGER ::  G_ADD_NODE
        INTEGER ::  G_UPDATE
        INTEGER ::  G_FRAM_FACTOR
        INTEGER ::  G_INTVAR
        INTEGER ::  G_DFS

c       layer variables 
c
        INTEGER ::  LY_DIRA
c
c       local variables (per integration point)
c
        INTEGER ::  L_GAMA
        INTEGER ::  L_EINT  
        INTEGER ::  L_EINS  
        INTEGER ::  L_RHO   
        INTEGER ::  L_DP_DRHO            
        INTEGER ::  L_STRA     
        INTEGER ::  L_DELTAX
        INTEGER ::  L_PLA   
        INTEGER ::  L_TB    
        INTEGER ::  L_RK    
        INTEGER ::  L_TEMP  
        INTEGER ::  L_EPSA  
        INTEGER ::  L_EPSD  
        INTEGER ::  L_EPSQ  
        INTEGER ::  L_EPSF  
        INTEGER ::  L_VK    
        INTEGER ::  L_SF       
        INTEGER ::  L_DAM      
        INTEGER ::  L_SIGL     
        INTEGER ::  L_SIGV     
        INTEGER ::  L_SIGA     
        INTEGER ::  L_SIGB     
        INTEGER ::  L_SIGC     
        INTEGER ::  L_SIGD     
        INTEGER ::  L_SIGF     
        INTEGER ::  L_RE    
        INTEGER ::  L_DSUM     
        INTEGER ::  L_ROB   
        INTEGER ::  L_DGLO     
        INTEGER ::  L_CRAK    
        INTEGER ::  L_ANG      
        INTEGER ::  L_EPE      
        INTEGER ::  L_EPC      
        INTEGER ::  L_XST      
        INTEGER ::  L_SSP      
        INTEGER ::  L_Z
        INTEGER ::  L_FRAC        !    pourcentage de phase (multimat) 
        INTEGER ::  L_BFRAC
        INTEGER ::  L_DMG
        INTEGER ::  L_ABURN
        INTEGER ::  L_MU

c---    uvar            
        INTEGER ::  NUVAR           
        INTEGER ::  NVARTMP      
        INTEGER ::  L_SEQ       ! equivalent stress (/= VON MISES)
        INTEGER ::  L_FAC_YLD       
      END TYPE MLAW_TAG_     

      TYPE EOS_TAG_    ! define element variables used by each eos
        INTEGER ::  G_MU        !  old mu (old rho) for reloading (compaction eos)   
        INTEGER ::  G_TB        !  explosive eos
        INTEGER ::  G_ABURN     !  afterburning   
        INTEGER ::  G_BFRAC     !  burn fraction   

        INTEGER ::  L_MU        !  old mu (old rho) for reloading (compaction eos)   
        INTEGER ::  L_TB        !  explosive eos
        INTEGER ::  L_ABURN     !  afterburning   
        INTEGER ::  L_BFRAC     !  burn fraction      
      END TYPE EOS_TAG_              

C 
      TYPE PROP_TAG_    ! element buffer variables depending on property
c       global variables 
        INTEGER ::  G_GAMA 
        INTEGER ::  G_EINT  
        INTEGER ::  G_EINS  
        INTEGER ::  G_OFF
        INTEGER ::  G_NOFF 
        INTEGER ::  G_HOURG
        INTEGER ::  G_SMSTR
        INTEGER ::  G_SIG
        INTEGER ::  G_STRA
        INTEGER ::  G_FOR
        INTEGER ::  G_MOM
        INTEGER ::  G_THK
        INTEGER ::  G_VOL 
        INTEGER ::  G_QVIS
        INTEGER ::  G_FORPG
        INTEGER ::  G_MOMPG
C
        INTEGER ::  G_FORPGPINCH
        INTEGER ::  G_MOMPGPINCH
        INTEGER ::  G_EPGPINCHXZ
        INTEGER ::  G_EPGPINCHYZ
        INTEGER ::  G_EPGPINCHZZ
C
        INTEGER ::  G_STRPG
        INTEGER ::  G_FORTH
        INTEGER ::  G_EINTTH
        INTEGER ::  G_FILL
        INTEGER ::  G_COR_NF
        INTEGER ::  G_COR_FR
        INTEGER ::  G_COR_XR
        INTEGER ::  G_BETAORTH
C---
C    - 1D - elem buffer (TRUSS, BEAM, SPRING, NSTRAND)
        INTEGER ::  G_AREA
        INTEGER ::  G_SKEW
        INTEGER ::  G_LENGTH
        INTEGER ::  G_TOTDEPL
        INTEGER ::  G_TOTROT
        INTEGER ::  G_FOREP
        INTEGER ::  G_MOMEP
        INTEGER ::  G_DEP_IN_TENS  ! DEPX
        INTEGER ::  G_DEP_IN_COMP  ! DEPX2
        INTEGER ::  G_ROT_IN_TENS  ! RPX
        INTEGER ::  G_ROT_IN_COMP  ! RPX2
        INTEGER ::  G_POSX
        INTEGER ::  G_POSY
        INTEGER ::  G_POSZ
        INTEGER ::  G_POSXX
        INTEGER ::  G_POSYY
        INTEGER ::  G_POSZZ
        INTEGER ::  G_YIELD
        INTEGER ::  G_LENGTH_ERR
        INTEGER ::  G_DV  ! filtrage des vitesses
        INTEGER ::  G_DFS   ! dF = E*dF + F*dE
        INTEGER ::  G_SKEW_ERR
        INTEGER ::  G_E6
        INTEGER ::  G_RUPTCRIT
        INTEGER ::  G_MASS
        INTEGER ::  G_V_REPCVT
        INTEGER ::  G_VR_REPCVT
        INTEGER ::  G_NUVAR
        INTEGER ::  G_NUVARN
        INTEGER ::  G_INIFRIC
        INTEGER ::  G_DEFINI
        INTEGER ::  G_FORINI
        INTEGER ::  G_SKEW_ID
C---
c       layer variables 
        INTEGER ::  LY_DMG
        INTEGER ::  LY_GAMA 
        INTEGER ::  LY_DIRA
        INTEGER ::  LY_DIRB
        INTEGER ::  LY_PLAPT
        INTEGER ::  LY_SIGPT
        INTEGER ::  LY_HOURG
c       local variables (per integration point)
        INTEGER ::  L_GAMA 
        INTEGER ::  L_EINT  
        INTEGER ::  L_EINS  
        INTEGER ::  L_OFF
        INTEGER ::  L_STRA     
        INTEGER ::  L_SIG
        INTEGER ::  L_SIGPLY
        INTEGER ::  L_SIGL
        INTEGER ::  L_SIGD
        INTEGER ::  L_SIGV
        INTEGER ::  L_FOR
        INTEGER ::  L_MOM
        INTEGER ::  L_THK
        INTEGER ::  L_VOL 
        INTEGER ::  L_QVIS
        INTEGER ::  L_FORTH
        INTEGER ::  L_EINTTH    
c---    uvar            
        INTEGER ::   NUVAR      
      END TYPE PROP_TAG_               
C
      TYPE FAIL_TAG_   
        INTEGER ::   NUVAR 
        INTEGER ::   LF_DAM
        INTEGER ::   LF_DAMMX  
        INTEGER ::   LF_TDEL  
        INTEGER ::   LF_INDX 
        INTEGER ::   LF_OFF
        INTEGER ::   LF_DIR
      END TYPE FAIL_TAG_               
 
C
      TYPE VISC_TAG_   
        INTEGER ::   NUVAR
      END TYPE VISC_TAG_               
c---------------
      END MODULE ELBUFTAG_MOD
