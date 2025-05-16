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
      !||    hm_get_floatv_dim            ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_bem                  ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_cload                ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_convec               ../starter/source/loads/thermic/hm_read_convec.F
      !||    hm_read_eig                  ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_eos_compaction_tab   ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
      !||    hm_read_fail_alter           ../starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
      !||    hm_read_fail_biquad          ../starter/source/materials/fail/biquad/hm_read_fail_biquad.F
      !||    hm_read_fail_energy          ../starter/source/materials/fail/energy/hm_read_fail_energy.F
      !||    hm_read_fail_gene1           ../starter/source/materials/fail/gene1/hm_read_fail_gene1.F
      !||    hm_read_fail_orthbiquad      ../starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
      !||    hm_read_fail_orthstrain      ../starter/source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
      !||    hm_read_fail_sahraei         ../starter/source/materials/fail/sahraei/hm_read_fail_sahraei.F
      !||    hm_read_fail_snconnect       ../starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
      !||    hm_read_fail_syazwan         ../starter/source/materials/fail/syazwan/hm_read_fail_syazwan.F
      !||    hm_read_fail_tab1            ../starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
      !||    hm_read_fail_tab2            ../starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
      !||    hm_read_fail_tab_old         ../starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
      !||    hm_read_fail_tensstrain      ../starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
      !||    hm_read_grav                 ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_impacc               ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_impflux              ../starter/source/constraints/thermic/hm_read_impflux.F
      !||    hm_read_imptemp              ../starter/source/constraints/thermic/hm_read_imptemp.F
      !||    hm_read_inject1              ../starter/source/properties/injector/hm_read_inject1.F
      !||    hm_read_inject2              ../starter/source/properties/injector/hm_read_inject2.F
      !||    hm_read_inter_type02         ../starter/source/interfaces/int02/hm_read_inter_type02.F
      !||    hm_read_inter_type07         ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type11         ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type21         ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type25         ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    hm_read_load_centri          ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure        ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_mat02                ../starter/source/materials/mat/mat002/hm_read_mat02.F
      !||    hm_read_mat100               ../starter/source/materials/mat/mat100/hm_read_mat100.F
      !||    hm_read_mat107               ../starter/source/materials/mat/mat107/hm_read_mat107.F
      !||    hm_read_mat108               ../starter/source/materials/mat/mat108/hm_read_mat108.F
      !||    hm_read_mat109               ../starter/source/materials/mat/mat109/hm_read_mat109.F
      !||    hm_read_mat11                ../starter/source/materials/mat/mat011/hm_read_mat11.F
      !||    hm_read_mat110               ../starter/source/materials/mat/mat110/hm_read_mat110.F
      !||    hm_read_mat111               ../starter/source/materials/mat/mat111/hm_read_mat111.F
      !||    hm_read_mat112               ../starter/source/materials/mat/mat112/hm_read_mat112.F
      !||    hm_read_mat113               ../starter/source/materials/mat/mat113/hm_read_mat113.F
      !||    hm_read_mat114               ../starter/source/materials/mat/mat114/hm_read_mat114.F
      !||    hm_read_mat116               ../starter/source/materials/mat/mat116/hm_read_mat116.F
      !||    hm_read_mat117               ../starter/source/materials/mat/mat117/hm_read_mat117.F
      !||    hm_read_mat11_k_eps          ../starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
      !||    hm_read_mat120               ../starter/source/materials/mat/mat120/hm_read_mat120.F
      !||    hm_read_mat121               ../starter/source/materials/mat/mat121/hm_read_mat121.F
      !||    hm_read_mat122               ../starter/source/materials/mat/mat122/hm_read_mat122.F
      !||    hm_read_mat128               ../starter/source/materials/mat/mat128/hm_read_mat128.F90
      !||    hm_read_mat133               ../starter/source/materials/mat/mat133/hm_read_mat133.F90
      !||    hm_read_mat158               ../starter/source/materials/mat/mat158/hm_read_mat158.F
      !||    hm_read_mat16                ../starter/source/materials/mat/mat016/hm_read_mat16.F
      !||    hm_read_mat163               ../starter/source/materials/mat/mat163/hm_read_mat163.F90
      !||    hm_read_mat190               ../starter/source/materials/mat/mat190/hm_read_mat190.F
      !||    hm_read_mat21                ../starter/source/materials/mat/mat021/hm_read_mat21.F
      !||    hm_read_mat26                ../starter/source/materials/mat/mat026/hm_read_mat26.F
      !||    hm_read_mat28                ../starter/source/materials/mat/mat028/hm_read_mat28.F
      !||    hm_read_mat33                ../starter/source/materials/mat/mat033/hm_read_mat33.F
      !||    hm_read_mat35                ../starter/source/materials/mat/mat035/hm_read_mat35.F
      !||    hm_read_mat36                ../starter/source/materials/mat/mat036/hm_read_mat36.F
      !||    hm_read_mat38                ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat42                ../starter/source/materials/mat/mat042/hm_read_mat42.F
      !||    hm_read_mat43                ../starter/source/materials/mat/mat043/hm_read_mat43.F
      !||    hm_read_mat44                ../starter/source/materials/mat/mat044/hm_read_mat44.F
      !||    hm_read_mat50                ../starter/source/materials/mat/mat050/hm_read_mat50.F90
      !||    hm_read_mat51                ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    hm_read_mat53                ../starter/source/materials/mat/mat053/hm_read_mat53.F
      !||    hm_read_mat54                ../starter/source/materials/mat/mat054/hm_read_mat54.F
      !||    hm_read_mat58                ../starter/source/materials/mat/mat058/hm_read_mat58.F
      !||    hm_read_mat59                ../starter/source/materials/mat/mat059/hm_read_mat59.F
      !||    hm_read_mat60                ../starter/source/materials/mat/mat060/hm_read_mat60.F
      !||    hm_read_mat64                ../starter/source/materials/mat/mat064/hm_read_mat64.F
      !||    hm_read_mat65                ../starter/source/materials/mat/mat065/hm_read_mat65.F
      !||    hm_read_mat66                ../starter/source/materials/mat/mat066/hm_read_mat66.F
      !||    hm_read_mat68                ../starter/source/materials/mat/mat068/hm_read_mat68.F
      !||    hm_read_mat69                ../starter/source/materials/mat/mat069/hm_read_mat69.F
      !||    hm_read_mat73                ../starter/source/materials/mat/mat073/hm_read_mat73.F
      !||    hm_read_mat74                ../starter/source/materials/mat/mat074/hm_read_mat74.F
      !||    hm_read_mat76                ../starter/source/materials/mat/mat076/hm_read_mat76.F
      !||    hm_read_mat77                ../starter/source/materials/mat/mat077/hm_read_mat77.F
      !||    hm_read_mat80                ../starter/source/materials/mat/mat080/hm_read_mat80.F
      !||    hm_read_mat81                ../starter/source/materials/mat/mat081/hm_read_mat81.F90
      !||    hm_read_mat83                ../starter/source/materials/mat/mat083/hm_read_mat83.F
      !||    hm_read_mat87                ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||    hm_read_mat88                ../starter/source/materials/mat/mat088/hm_read_mat88.F
      !||    hm_read_mat92                ../starter/source/materials/mat/mat092/hm_read_mat92.F
      !||    hm_read_mat93                ../starter/source/materials/mat/mat093/hm_read_mat93.F
      !||    hm_read_mat95                ../starter/source/materials/mat/mat095/hm_read_mat95.F
      !||    hm_read_matgas               ../starter/source/materials/mat/matgas/hm_read_matgas.F
      !||    hm_read_monvol_type10        ../starter/source/airbag/hm_read_monvol_type10.F
      !||    hm_read_monvol_type3         ../starter/source/airbag/hm_read_monvol_type3.F
      !||    hm_read_monvol_type4         ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5         ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type6         ../starter/source/airbag/hm_read_monvol_type6.F
      !||    hm_read_monvol_type7         ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8         ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9         ../starter/source/airbag/hm_read_monvol_type9.F
      !||    hm_read_pcyl                 ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_pfluid               ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_pload                ../starter/source/loads/general/pload/hm_read_pload.F
      !||    hm_read_prop04               ../starter/source/properties/spring/hm_read_prop04.F
      !||    hm_read_prop08               ../starter/source/properties/spring/hm_read_prop08.F
      !||    hm_read_prop12               ../starter/source/properties/spring/hm_read_prop12.F
      !||    hm_read_prop13               ../starter/source/properties/spring/hm_read_prop13.F
      !||    hm_read_prop19               ../starter/source/properties/shell/hm_read_prop19.F
      !||    hm_read_prop25               ../starter/source/properties/spring/hm_read_prop25.F
      !||    hm_read_prop26               ../starter/source/properties/spring/hm_read_prop26.F
      !||    hm_read_prop27               ../starter/source/properties/spring/hm_read_prop27.F
      !||    hm_read_prop32               ../starter/source/properties/spring/hm_read_prop32.F
      !||    hm_read_prop35               ../starter/source/properties/spring/hm_read_prop35.F
      !||    hm_read_prop44               ../starter/source/properties/spring/hm_read_prop44.F
      !||    hm_read_prop46               ../starter/source/properties/spring/hm_read_prop46.F
      !||    hm_read_radiation            ../starter/source/loads/thermic/hm_read_radiation.F
      !||    hm_read_retractor            ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_rwall_therm          ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_slipring             ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_sphio                ../starter/source/loads/sph/hm_read_sphio.F
      !||    hm_read_visc_prony           ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    read_impdisp                 ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_impdisp_fgeo            ../starter/source/constraints/general/impvel/read_impdisp_fgeo.F
      !||    read_impvel                  ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_fgeo             ../starter/source/constraints/general/impvel/read_impvel_fgeo.F
      !||    read_impvel_lagmul           ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||    read_mat25_crasurv           ../starter/source/materials/mat/mat025/read_mat25_crasurv.F90
      !||    read_mat25_tsaiwu            ../starter/source/materials/mat/mat025/read_mat25_tsaiwu.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod                  ../starter/share/message_module/message_mod.F
      !||    submodel_mod                 ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_GET_FLOATV_DIM(NAME,DIM_FAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE
C   ASK (REAL) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C   RETURN DIMENSION FACTOR
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     DIM_FAC      UNIT DIMENSION FACTOR 
C     IS_AVAILABLE VALUE AVAILABLE IN MODEL OR NOT
C     LSUBMODEL    SUBMODEL STRUCTURE       
C     UNITAB       UNIT ARRAY            
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_BOOL

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      CHARACTER*(*),INTENT(IN)::NAME
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      my_real,
     .  INTENT(OUT)::DIM_FAC
      LOGICAL,INTENT(OUT)::IS_AVAILABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,VALUE_TYPE,SUB_ID,IFLAGUNIT,UID
      my_real
     .   FAC_L,FAC_M,FAC_T,FAC
      REAL*8 DVAL,LENGTH_DIM,MASS_DIM,TIME_DIM
      LOGICAL(KIND=C_BOOL) :: C_IS_AVAILABLE

C-----------------------------------------------
        C_IS_AVAILABLE = .FALSE.
        LENGTH_DIM = ZERO
        MASS_DIM = ZERO
        TIME_DIM = ZERO
        FAC = ONE
C--------------------------------------------------
        CALL CPP_GET_FLOATV_FLOATD(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),DVAL,C_IS_AVAILABLE,
     .                    LENGTH_DIM,MASS_DIM,TIME_DIM,UID,SUB_ID)
        IS_AVAILABLE = C_IS_AVAILABLE
C--------------------------------------------------
C      ID OFFSETS FOR //SUBMODEL 
C--------------------------------------------------
        IF(SUB_ID /= 0 .AND. UID == 0)THEN
          IF(LSUBMODEL(SUB_ID)%UID /= 0)THEN
            UID = LSUBMODEL(SUB_ID)%UID
          ENDIF
        ENDIF
C--------------------------------------------------
c      APPLY UNIT SYSTEM
C--------------------------------------------------
        IFLAGUNIT = 0  
        FAC_M = ZERO        
        FAC_L = ZERO        
        FAC_T = ZERO                                        
        DO J=1,UNITAB%NUNITS                         
          IF (UNITAB%UNIT_ID(J) == UID) THEN    
            FAC_M = UNITAB%FAC_M(J)
            FAC_L = UNITAB%FAC_L(J)
            FAC_T = UNITAB%FAC_T(J)
            IFLAGUNIT = 1                      
            EXIT                              
          ENDIF        
        ENDDO
        IF (FAC_M  /= ZERO) FAC = FAC * (FAC_M ** MASS_DIM  )
        IF (FAC_L  /= ZERO) FAC = FAC * (FAC_L ** LENGTH_DIM)
        IF (FAC_T  /= ZERO) FAC = FAC * (FAC_T ** TIME_DIM  )     
C-------------------------------------------------- 
        DIM_FAC = FAC
c        print *,'dim factor=',NAME(1:LEN_TRIM(NAME)),DIM_FAC,LENGTH_DIM,MASS_DIM,TIME_DIM
C--------------------------------------------------                             
      RETURN
C
      END
