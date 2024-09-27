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
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||--- called by ------------------------------------------------------
      !||    ale_euler_init           ../starter/source/materials/ale/ale_euler_init.F
      !||    deallocate_elbuf         ../starter/source/elements/elbuf_init/deallocate_buffer.F
      !||    elbuf_ini                ../starter/source/elements/elbuf_init/elbuf_ini.F
      !||    hm_prop_read21           ../starter/source/properties/thickshell/hm_read_prop21.F
      !||    hm_read_eos              ../starter/source/materials/eos/hm_read_eos.F
      !||    hm_read_eos_compaction   ../starter/source/materials/eos/hm_read_eos_compaction.F
      !||    hm_read_eos_tillotson    ../starter/source/materials/eos/hm_read_eos_tillotson.F
      !||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_fail_alter       ../starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
      !||    hm_read_fail_chang       ../starter/source/materials/fail/changchang/hm_read_fail_chang.F
      !||    hm_read_fail_connect     ../starter/source/materials/fail/connect/hm_read_fail_connect.F
      !||    hm_read_fail_gene1       ../starter/source/materials/fail/gene1/hm_read_fail_gene1.F
      !||    hm_read_fail_hashin      ../starter/source/materials/fail/hashin/hm_read_fail_hashin.F
      !||    hm_read_fail_hoffman     ../starter/source/materials/fail/hoffman/hm_read_fail_hoffman.F
      !||    hm_read_fail_maxstrain   ../starter/source/materials/fail/max_strain/hm_read_fail_maxstrain.F
      !||    hm_read_fail_nxt         ../starter/source/materials/fail/nxt/hm_read_fail_nxt.F
      !||    hm_read_fail_puck        ../starter/source/materials/fail/puck/hm_read_fail_puck.F
      !||    hm_read_fail_tsaihill    ../starter/source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
      !||    hm_read_fail_tsaiwu      ../starter/source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
      !||    hm_read_inject1          ../starter/source/properties/injector/hm_read_inject1.F
      !||    hm_read_inject2          ../starter/source/properties/injector/hm_read_inject2.F
      !||    hm_read_inter_type10     ../starter/source/interfaces/int10/hm_read_inter_type10.F
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F
      !||    hm_read_mat00            ../starter/source/materials/mat/mat000/hm_read_mat00.F
      !||    hm_read_mat02            ../starter/source/materials/mat/mat002/hm_read_mat02.F
      !||    hm_read_mat03            ../starter/source/materials/mat/mat003/hm_read_mat03.F
      !||    hm_read_mat04            ../starter/source/materials/mat/mat004/hm_read_mat04.F
      !||    hm_read_mat05            ../starter/source/materials/mat/mat005/hm_read_mat05.F
      !||    hm_read_mat06            ../starter/source/materials/mat/mat006/hm_read_mat06.F
      !||    hm_read_mat06_keps       ../starter/source/materials/mat/mat006/hm_read_mat06_keps.F
      !||    hm_read_mat10            ../starter/source/materials/mat/mat010/hm_read_mat10.F
      !||    hm_read_mat100           ../starter/source/materials/mat/mat100/hm_read_mat100.F
      !||    hm_read_mat102           ../starter/source/materials/mat/mat102/hm_read_mat102.F
      !||    hm_read_mat103           ../starter/source/materials/mat/mat103/hm_read_mat103.F
      !||    hm_read_mat104           ../starter/source/materials/mat/mat104/hm_read_mat104.F
      !||    hm_read_mat106           ../starter/source/materials/mat/mat106/hm_read_mat106.F
      !||    hm_read_mat107           ../starter/source/materials/mat/mat107/hm_read_mat107.F
      !||    hm_read_mat108           ../starter/source/materials/mat/mat108/hm_read_mat108.F
      !||    hm_read_mat109           ../starter/source/materials/mat/mat109/hm_read_mat109.F
      !||    hm_read_mat11            ../starter/source/materials/mat/mat011/hm_read_mat11.F
      !||    hm_read_mat110           ../starter/source/materials/mat/mat110/hm_read_mat110.F
      !||    hm_read_mat111           ../starter/source/materials/mat/mat111/hm_read_mat111.F
      !||    hm_read_mat112           ../starter/source/materials/mat/mat112/hm_read_mat112.F
      !||    hm_read_mat113           ../starter/source/materials/mat/mat113/hm_read_mat113.F
      !||    hm_read_mat114           ../starter/source/materials/mat/mat114/hm_read_mat114.F
      !||    hm_read_mat115           ../starter/source/materials/mat/mat115/hm_read_mat115.F
      !||    hm_read_mat116           ../starter/source/materials/mat/mat116/hm_read_mat116.F
      !||    hm_read_mat117           ../starter/source/materials/mat/mat117/hm_read_mat117.F
      !||    hm_read_mat119           ../starter/source/materials/mat/mat119/hm_read_mat119.F
      !||    hm_read_mat11_k_eps      ../starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
      !||    hm_read_mat12            ../starter/source/materials/mat/mat012/hm_read_mat12.F
      !||    hm_read_mat120           ../starter/source/materials/mat/mat120/hm_read_mat120.F
      !||    hm_read_mat121           ../starter/source/materials/mat/mat121/hm_read_mat121.F
      !||    hm_read_mat122           ../starter/source/materials/mat/mat122/hm_read_mat122.F
      !||    hm_read_mat124           ../starter/source/materials/mat/mat124/hm_read_mat124.F
      !||    hm_read_mat125           ../starter/source/materials/mat/mat125/hm_read_mat125.F90
      !||    hm_read_mat126           ../starter/source/materials/mat/mat126/hm_read_mat126.F90
      !||    hm_read_mat127           ../starter/source/materials/mat/mat127/hm_read_mat127.F90
      !||    hm_read_mat13            ../starter/source/materials/mat/mat013/hm_read_mat13.F
      !||    hm_read_mat14            ../starter/source/materials/mat/mat014/hm_read_mat14.F
      !||    hm_read_mat15            ../starter/source/materials/mat/mat015/hm_read_mat15.F
      !||    hm_read_mat151           ../starter/source/materials/mat/mat151/hm_read_mat151.F
      !||    hm_read_mat158           ../starter/source/materials/mat/mat158/hm_read_mat158.F
      !||    hm_read_mat16            ../starter/source/materials/mat/mat016/hm_read_mat16.F
      !||    hm_read_mat169_arup      ../starter/source/materials/mat/mat169/hm_read_mat169.F90
      !||    hm_read_mat18            ../starter/source/materials/mat/mat018/hm_read_mat18.F
      !||    hm_read_mat19            ../starter/source/materials/mat/mat019/hm_read_mat19.F
      !||    hm_read_mat190           ../starter/source/materials/mat/mat190/hm_read_mat190.F
      !||    hm_read_mat20            ../starter/source/materials/mat/mat020/hm_read_mat20.F
      !||    hm_read_mat21            ../starter/source/materials/mat/mat021/hm_read_mat21.F
      !||    hm_read_mat22            ../starter/source/materials/mat/mat022/hm_read_mat22.F
      !||    hm_read_mat23            ../starter/source/materials/mat/mat023/hm_read_mat23.F
      !||    hm_read_mat24            ../starter/source/materials/mat/mat024/hm_read_mat24.F
      !||    hm_read_mat25            ../starter/source/materials/mat/mat025/hm_read_mat25.F
      !||    hm_read_mat26            ../starter/source/materials/mat/mat026/hm_read_mat26.F
      !||    hm_read_mat27            ../starter/source/materials/mat/mat027/hm_read_mat27.F
      !||    hm_read_mat28            ../starter/source/materials/mat/mat028/hm_read_mat28.F
      !||    hm_read_mat32            ../starter/source/materials/mat/mat032/hm_read_mat32.F
      !||    hm_read_mat33            ../starter/source/materials/mat/mat033/hm_read_mat33.F
      !||    hm_read_mat34            ../starter/source/materials/mat/mat034/hm_read_mat34.F
      !||    hm_read_mat35            ../starter/source/materials/mat/mat035/hm_read_mat35.F
      !||    hm_read_mat36            ../starter/source/materials/mat/mat036/hm_read_mat36.F
      !||    hm_read_mat37            ../starter/source/materials/mat/mat037/hm_read_mat37.F
      !||    hm_read_mat38            ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat40            ../starter/source/materials/mat/mat040/hm_read_mat40.F
      !||    hm_read_mat41            ../starter/source/materials/mat/mat041/hm_read_mat41.F
      !||    hm_read_mat43            ../starter/source/materials/mat/mat043/hm_read_mat43.F
      !||    hm_read_mat44            ../starter/source/materials/mat/mat044/hm_read_mat44.F
      !||    hm_read_mat46            ../starter/source/materials/mat/mat046/hm_read_mat46.F
      !||    hm_read_mat48            ../starter/source/materials/mat/mat048/hm_read_mat48.F
      !||    hm_read_mat49            ../starter/source/materials/mat/mat049/hm_read_mat49.F
      !||    hm_read_mat50            ../starter/source/materials/mat/mat050/hm_read_mat50.F
      !||    hm_read_mat51            ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    hm_read_mat52            ../starter/source/materials/mat/mat052/hm_read_mat52.F
      !||    hm_read_mat53            ../starter/source/materials/mat/mat053/hm_read_mat53.F
      !||    hm_read_mat54            ../starter/source/materials/mat/mat054/hm_read_mat54.F
      !||    hm_read_mat57            ../starter/source/materials/mat/mat057/hm_read_mat57.F
      !||    hm_read_mat58            ../starter/source/materials/mat/mat058/hm_read_mat58.F
      !||    hm_read_mat59            ../starter/source/materials/mat/mat059/hm_read_mat59.F
      !||    hm_read_mat60            ../starter/source/materials/mat/mat060/hm_read_mat60.F
      !||    hm_read_mat63            ../starter/source/materials/mat/mat063/hm_read_mat63.F
      !||    hm_read_mat64            ../starter/source/materials/mat/mat064/hm_read_mat64.F
      !||    hm_read_mat65            ../starter/source/materials/mat/mat065/hm_read_mat65.F
      !||    hm_read_mat66            ../starter/source/materials/mat/mat066/hm_read_mat66.F
      !||    hm_read_mat68            ../starter/source/materials/mat/mat068/hm_read_mat68.F
      !||    hm_read_mat69            ../starter/source/materials/mat/mat069/hm_read_mat69.F
      !||    hm_read_mat70            ../starter/source/materials/mat/mat070/hm_read_mat70.F
      !||    hm_read_mat71            ../starter/source/materials/mat/mat071/hm_read_mat71.F
      !||    hm_read_mat72            ../starter/source/materials/mat/mat072/hm_read_mat72.F
      !||    hm_read_mat73            ../starter/source/materials/mat/mat073/hm_read_mat73.F
      !||    hm_read_mat74            ../starter/source/materials/mat/mat074/hm_read_mat74.F
      !||    hm_read_mat75            ../starter/source/materials/mat/mat075/hm_read_mat75.F
      !||    hm_read_mat76            ../starter/source/materials/mat/mat076/hm_read_mat76.F
      !||    hm_read_mat77            ../starter/source/materials/mat/mat077/hm_read_mat77.F
      !||    hm_read_mat78            ../starter/source/materials/mat/mat078/hm_read_mat78.F
      !||    hm_read_mat79            ../starter/source/materials/mat/mat079/hm_read_mat79.F
      !||    hm_read_mat80            ../starter/source/materials/mat/mat080/hm_read_mat80.F
      !||    hm_read_mat81            ../starter/source/materials/mat/mat081/hm_read_mat81.F
      !||    hm_read_mat83            ../starter/source/materials/mat/mat083/hm_read_mat83.F
      !||    hm_read_mat84            ../starter/source/materials/mat/mat084/hm_read_mat84.F
      !||    hm_read_mat87            ../starter/source/materials/mat/mat087/hm_read_mat87.F
      !||    hm_read_mat88            ../starter/source/materials/mat/mat088/hm_read_mat88.F
      !||    hm_read_mat90            ../starter/source/materials/mat/mat090/hm_read_mat90.F
      !||    hm_read_mat92            ../starter/source/materials/mat/mat092/hm_read_mat92.F
      !||    hm_read_mat93            ../starter/source/materials/mat/mat093/hm_read_mat93.F
      !||    hm_read_mat94            ../starter/source/materials/mat/mat094/hm_read_mat94.F
      !||    hm_read_mat95            ../starter/source/materials/mat/mat095/hm_read_mat95.F
      !||    hm_read_mat97            ../starter/source/materials/mat/mat097/hm_read_mat97.F
      !||    hm_read_nonlocal         ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    hm_read_part             ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_prop0            ../starter/source/properties/void/hm_read_prop0.F
      !||    hm_read_prop01           ../starter/source/properties/shell/hm_read_prop01.F
      !||    hm_read_prop02           ../starter/source/properties/truss/hm_read_prop02.F
      !||    hm_read_prop03           ../starter/source/properties/beam/hm_read_prop03.F
      !||    hm_read_prop04           ../starter/source/properties/spring/hm_read_prop04.F
      !||    hm_read_prop06           ../starter/source/properties/solid/hm_read_prop06.F
      !||    hm_read_prop08           ../starter/source/properties/spring/hm_read_prop08.F
      !||    hm_read_prop09           ../starter/source/properties/shell/hm_read_prop09.F
      !||    hm_read_prop10           ../starter/source/properties/shell/hm_read_prop10.F
      !||    hm_read_prop11           ../starter/source/properties/shell/hm_read_prop11.F
      !||    hm_read_prop12           ../starter/source/properties/spring/hm_read_prop12.F
      !||    hm_read_prop13           ../starter/source/properties/spring/hm_read_prop13.F
      !||    hm_read_prop14           ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop14f          ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop15           ../starter/source/properties/solid/hm_read_prop15.F
      !||    hm_read_prop16           ../starter/source/properties/shell/hm_read_prop16.F
      !||    hm_read_prop17           ../starter/source/properties/shell/hm_read_prop17.F
      !||    hm_read_prop18           ../starter/source/properties/beam/hm_read_prop18.F
      !||    hm_read_prop20           ../starter/source/properties/thickshell/hm_read_prop20.F
      !||    hm_read_prop22           ../starter/source/properties/thickshell/hm_read_prop22.F
      !||    hm_read_prop23           ../starter/source/properties/spring/hm_read_prop23.F
      !||    hm_read_prop25           ../starter/source/properties/spring/hm_read_prop25.F
      !||    hm_read_prop26           ../starter/source/properties/spring/hm_read_prop26.F
      !||    hm_read_prop27           ../starter/source/properties/spring/hm_read_prop27.F
      !||    hm_read_prop28           ../starter/source/properties/xelem/hm_read_prop28.F
      !||    hm_read_prop32           ../starter/source/properties/spring/hm_read_prop32.F
      !||    hm_read_prop33           ../starter/source/properties/spring/hm_read_prop33.F
      !||    hm_read_prop34           ../starter/source/properties/sph/hm_read_prop34.F
      !||    hm_read_prop35           ../starter/source/properties/spring/hm_read_prop35.F
      !||    hm_read_prop36           ../starter/source/properties/spring/hm_read_prop36.F
      !||    hm_read_prop43           ../starter/source/properties/solid/hm_read_prop43.F
      !||    hm_read_prop44           ../starter/source/properties/spring/hm_read_prop44.F
      !||    hm_read_prop45           ../starter/source/properties/spring/hm_read_prop45.F
      !||    hm_read_prop46           ../starter/source/properties/spring/hm_read_prop46.F
      !||    hm_read_prop51           ../starter/source/properties/shell/hm_read_prop51.F
      !||    hm_read_prop_generic     ../starter/source/properties/hm_read_prop_generic.F
      !||    hm_read_prop_user        ../starter/source/properties/user_spring_solid/hm_read_prop_user.F
      !||    hm_read_properties       ../starter/source/properties/hm_read_properties.F
      !||    hm_read_stack            ../starter/source/stack/hm_read_stack.F
      !||    hm_read_therm_stress     ../starter/source/materials/therm/hm_read_therm_stress.F
      !||    hm_read_visc             ../starter/source/materials/visc/hm_read_visc.F
      !||    ini_eos_vars             ../starter/source/elements/elbuf_init/ini_eos_vars.F
      !||    ini_mlaw_vars            ../starter/source/elements/elbuf_init/ini_mlaw_vars.F
      !||    ini_prop_vars            ../starter/source/elements/elbuf_init/ini_prop_vars.F
      !||    init_mlaw_tag            ../starter/source/elements/elbuf_init/init_mlaw_tag.F
      !||    initvars_auto            ../starter/source/elements/elbuf_init/initvars_auto.F
      !||    law104_upd               ../starter/source/materials/mat/mat104/law104_upd.F
      !||    law36_upd                ../starter/source/materials/mat/mat036/law36_upd.F
      !||    lecstack_ply             ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lectur                   ../starter/source/starter/lectur.F
      !||    m20dcod                  ../starter/source/system/fsdcod.F
      !||    read_material_models     ../starter/source/materials/read_material_models.F
      !||    updmat                   ../starter/source/materials/updmat.F
      !||    zerovars_auto            ../starter/source/elements/elbuf_init/zerovars_auto.F
      !||====================================================================
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
        INTEGER ::  G_ABURN        !  Additional Burn Rate for (post)combustion : example : Miller's extension of JWL 
        INTEGER ::  G_MU           !  Additional Burn Rate for (post)combustion : example : Miller's extension of JWL 
        INTEGER ::  G_MAXFRAC
        INTEGER ::  G_MAXEPS
        INTEGER ::  G_DMG          ! 
        INTEGER ::  G_PLANL
        INTEGER ::  G_EPSDNL 
        INTEGER ::  G_DMGSCL
        INTEGER ::  G_TSAIWU
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
c for cohesif solid material
        INTEGER ::  G_THK


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
        INTEGER ::  L_PLANL
        INTEGER ::  L_EPSDNL
        INTEGER ::  L_DMGSCL
        INTEGER ::  L_TSAIWU

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
        
        INTEGER ::  NVAR        !  EOS%VAR buffer size     
      END TYPE EOS_TAG_    
      
      !EOS PARAM
      INTEGER,SAVE :: MAXEOS
      DATA MAXEOS /20/          !Number of EOS types :'LINEAR', 'POLYNOMIAL', 'IDEAL-GAS' , etc ...
                

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
        INTEGER ::   LF_DAM
        INTEGER ::   LF_DAMMX  
        INTEGER ::   LF_DAMINI
        INTEGER ::   LF_TDEL  
        INTEGER ::   LF_INDX 
        INTEGER ::   LF_OFF
        INTEGER ::   LF_DIR
      END TYPE FAIL_TAG_               
c---------------
      END MODULE ELBUFTAG_MOD

