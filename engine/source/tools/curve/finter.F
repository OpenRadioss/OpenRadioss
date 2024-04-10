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
#include "my_real.inc"
Chd|====================================================================
Chd|  FINTER                        source/tools/curve/finter.F   
Chd|-- called by -----------
Chd|        AIRBAGA                       source/airbag/airbag1.F       
Chd|        AIRBAGA1                      source/airbag/airbaga1.F      
Chd|        ALEFVM_GRAV_INIT              source/ale/alefvm/alefvm_grav_init.F
Chd|        CFIELD_1                      source/loads/general/load_centri/cfield.F
Chd|        CFIELD_IMP                    source/loads/general/load_centri/cfield_imp.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CONVEC                        source/constraints/thermic/convec.F
Chd|        DAASOLV                       source/fluid/daasolv.F        
Chd|        DAASOLVP                      source/fluid/daasolvp.F       
Chd|        DAM_FLD_SOL                   source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
Chd|        FAIL_BIQUAD_B                 source/materials/fail/biquad/fail_biquad_b.F
Chd|        FAIL_BIQUAD_C                 source/materials/fail/biquad/fail_biquad_c.F
Chd|        FAIL_BIQUAD_IB                source/materials/fail/biquad/fail_biquad_ib.F
Chd|        FAIL_BIQUAD_S                 source/materials/fail/biquad/fail_biquad_s.F
Chd|        FAIL_CONNECT                  source/materials/fail/connect/fail_connect.F
Chd|        FAIL_ENERGY_B                 source/materials/fail/energy/fail_energy_b.F
Chd|        FAIL_ENERGY_C                 source/materials/fail/energy/fail_energy_c.F
Chd|        FAIL_ENERGY_IB                source/materials/fail/energy/fail_energy_ib.F
Chd|        FAIL_ENERGY_S                 source/materials/fail/energy/fail_energy_s.F
Chd|        FAIL_FABRIC_C                 source/materials/fail/fabric/fail_fabric_c.F
Chd|        FAIL_FLD_TSH                  source/materials/fail/fld/fail_fld_tsh.F
Chd|        FAIL_FLD_XFEM                 source/materials/fail/fld/fail_fld_xfem.F
Chd|        FAIL_GENE1_C                  source/materials/fail/gene1/fail_gene1_c.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_NXT_C                    source/materials/fail/nxt/fail_nxt_c.F
Chd|        FAIL_ORTHBIQUAD_C             source/materials/fail/orthbiquad/fail_orthbiquad_c.F
Chd|        FAIL_ORTHBIQUAD_S             source/materials/fail/orthbiquad/fail_orthbiquad_s.F
Chd|        FAIL_ORTHSTRAIN               source/materials/fail/orthstrain/fail_orthstrain_s.F
Chd|        FAIL_ORTHSTRAIN_C             source/materials/fail/orthstrain/fail_orthstrain_c.F
Chd|        FAIL_SAHRAEI_S                source/materials/fail/sahraei/fail_sahraei_s.F
Chd|        FAIL_SNCONNECT                source/materials/fail/snconnect/fail_snconnect.F
Chd|        FAIL_SYAZWAN_C                source/materials/fail/syazwan/fail_syazwan_c.F
Chd|        FAIL_SYAZWAN_S                source/materials/fail/syazwan/fail_syazwan_s.F
Chd|        FAIL_TAB2_C                   source/materials/fail/tabulated/fail_tab2_c.F
Chd|        FAIL_TAB2_S                   source/materials/fail/tabulated/fail_tab2_s.F
Chd|        FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|        FAIL_TAB_OLD_C                source/materials/fail/tabulated/fail_tab_old_c.F
Chd|        FAIL_TAB_OLD_XFEM             source/materials/fail/tabulated/fail_tab_old_xfem.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TAB_XFEM                 source/materials/fail/tabulated/fail_tab_xfem.F
Chd|        FAIL_TENSSTRAIN_B             source/materials/fail/tensstrain/fail_tensstrain_b.F
Chd|        FAIL_TENSSTRAIN_C             source/materials/fail/tensstrain/fail_tensstrain_c.F
Chd|        FAIL_TENSSTRAIN_IB            source/materials/fail/tensstrain/fail_tensstrain_ib.F
Chd|        FAIL_TENSSTRAIN_S             source/materials/fail/tensstrain/fail_tensstrain_s.F
Chd|        FAIl_TAB_OLD_S                source/materials/fail/tabulated/fail_tab_old_s.F
Chd|        FIXFLUX                       source/constraints/thermic/fixflux.F
Chd|        FORCE                         source/loads/general/force.F  
Chd|        FORCEFINGEO                   source/loads/general/forcefingeo.F
Chd|        FORCEPINCH                    source/loads/general/forcepinch.F
Chd|        FRICTIONPARTS_MODEL_ISOT      source/interfaces/int07/frictionparts_model.F
Chd|        FVBAG1                        source/airbag/fvbag1.F        
Chd|        FVINJT6                       source/airbag/fvinjt6.F       
Chd|        FVINJT8                       source/airbag/fvinjt8.F       
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|        FXBODFP2                      source/constraints/fxbody/fxbodfp.F
Chd|        FXGRVCOR                      source/constraints/fxbody/fxgrvcor.F
Chd|        GRAVIT                        source/loads/general/grav/gravit.F
Chd|        GRAVIT_FVM_FEM                source/loads/general/grav/gravit_fvm_fem.F
Chd|        GRAVIT_IMP                    source/loads/general/grav/gravit_imp.F
Chd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        I11THERM                      source/interfaces/int11/i11therm.F
Chd|        I21FOR3                       source/interfaces/int21/i21for3.F
Chd|        I21THERM                      source/interfaces/int21/i21therm.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        I25THERM                      source/interfaces/int25/i25therm.F
Chd|        I6ASS3                        source/interfaces/inter3d/i6ass3.F
Chd|        I7THERM                       source/interfaces/int07/i7therm.F
Chd|        IDX_FLD_SOL                   source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|        INCPFLOW                      source/fluid/incpflow.F       
Chd|        LAG_FXV                       source/tools/lagmul/lag_fxv.F 
Chd|        LAG_FXVP                      source/tools/lagmul/lag_fxv.F 
Chd|        LOAD_PRESSURE                 source/loads/general/load_pressure/load_pressure.F
Chd|        M11LAW                        source/materials/mat/mat011/m11law.F
Chd|        M21LAW                        source/materials/mat/mat021/m21law.F
Chd|        MAT87C_TABULATED_PLAS_SR      source/materials/mat/mat087/mat87c_tabulated_plas_sr.F
Chd|        MAT87C_TABULATED_TOTALSR      source/materials/mat/mat087/mat87c_tabulated_totalsr.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        PFLUID                        source/loads/general/pfluid/pfluid.F
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|        R26SIG                        source/elements/spring/r26sig.F
Chd|        R27DEF3                       source/elements/spring/r27def3.F
Chd|        RADIATION                     source/constraints/thermic/radiation.F
Chd|        RUPTINT2                      source/interfaces/interf/ruptint2.F
Chd|        SIGEPS105                     source/materials/mat/mat105/sigeps105.F
Chd|        SIGEPS106                     source/materials/mat/mat106/sigeps106.F
Chd|        SIGEPS111                     source/materials/mat/mat111/sigeps111.F
Chd|        SIGEPS117                     source/materials/mat/mat117/sigeps117.F
Chd|        SIGEPS187                     source/materials/mat/mat187/sigeps187.F
Chd|        SIGEPS33                      source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS35                      source/materials/mat/mat035/sigeps35.F
Chd|        SIGEPS35C                     source/materials/mat/mat035/sigeps35c.F
Chd|        SIGEPS36                      source/materials/mat/mat036/sigeps36.F
Chd|        SIGEPS36G                     source/materials/mat/mat036/sigeps36g.F
Chd|        SIGEPS36PI                    source/materials/mat/mat036/sigeps36pi.F
Chd|        SIGEPS37                      source/materials/mat/mat037/sigeps37.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|        SIGEPS43C                     source/materials/mat/mat043/sigeps43c.F
Chd|        SIGEPS43G                     source/materials/mat/mat043/sigeps43g.F
Chd|        SIGEPS44P                     source/materials/mat/mat044/sigeps44p.F
Chd|        SIGEPS44T                     source/materials/mat/mat044/sigeps44t.F
Chd|        SIGEPS50                      source/materials/mat/mat050/sigeps50.F
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|        SIGEPS57C                     source/materials/mat/mat057/sigeps57c.F
Chd|        SIGEPS58C                     source/materials/mat/mat058/sigeps58c.F
Chd|        SIGEPS59                      source/materials/mat/mat059/sigeps59.F
Chd|        SIGEPS60                      source/materials/mat/mat060/sigeps60.F
Chd|        SIGEPS60C                     source/materials/mat/mat060/sigeps60c.F
Chd|        SIGEPS60G                     source/materials/mat/mat060/sigeps60g.F
Chd|        SIGEPS66                      source/materials/mat/mat066/sigeps66.F
Chd|        SIGEPS66C                     source/materials/mat/mat066/sigeps66c.F
Chd|        SIGEPS69                      source/materials/mat/mat069/sigeps69.F
Chd|        SIGEPS73C                     source/materials/mat/mat073/sigeps73c.F
Chd|        SIGEPS74                      source/materials/mat/mat074/sigeps74.F
Chd|        SIGEPS77                      source/materials/mat/mat077/sigeps77.F
Chd|        SIGEPS78                      source/materials/mat/mat078/sigeps78.F
Chd|        SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|        SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
Chd|        SIGEPS81                      source/materials/mat/mat081/sigeps81.F
Chd|        SIGEPS83                      source/materials/mat/mat083/sigeps83.F
Chd|        SIGEPS88                      source/materials/mat/mat088/sigeps88.F
Chd|        SIGEPS88C                     source/materials/mat/mat088/sigeps88c.F
Chd|        SMS_GRAVIT                    source/ams/sms_gravit.F       
Chd|        THERMEXPC                     source/materials/mat_share/thermexpc.F
Chd|        VOLPFV                        source/airbag/volpfv.F        
Chd|        VOLPRE                        source/airbag/volpres.F       
Chd|        VOLPREP                       source/airbag/volpresp.F      
Chd|        VOLP_LFLUID                   source/airbag/volp_lfluid.F   
Chd|-- calls ---------------
Chd|====================================================================
        my_real FUNCTION FINTER(IFUNC,XX,NPF,TF,DERI)
!$COMMENT
!       FINTER description :
!              FINTER function interpolates XX on TF curve
!               and computes also the derivative DERI
!       FINTER organization :
!               two cases :
!                    - constant function --> direct interpolation
!                    - non-constant function :
!                        (1): if the number of point in the function is < MIN_GAP, then
!                             the interpolation is computed 
!                        (2): if the number of point in the function is > MIN_GAP, then
!                             a dichotmy reduces the point interval and a interpolation
!                             is performed on the reduced point interval
!                             if the dichotomy fails, then the classical interpolation 
!                             is used
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
!       ----------------------------------------
!       Global variables
!       ----------------------------------------
        INTEGER IFUNC,NPF(*)
        my_real TF(*),DERI,XX
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       IFUNC   : integer ; function number
!       NPF     : integer ; dimension=SUM( (number of point[IFUNC])*2), IFUNC=1:MAX_FUNC
!                 pointer for the function points x + function value f(x)
!       TF      : my_real ; dimension=SUM( (number of point[IFUNC])*2), IFUNC=1:MAX_FUNC
!                 gives the function points x + function value f(x)
!       DERI    : my_real ; function derivative
!       XX      : my_real ; x value to be interpolate 
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       ----------------------------------------
!       Local variables
!       ----------------------------------------
        LOGICAL :: BOOL
        INTEGER :: I,J,POINT_NBR
        INTEGER :: FIRST, LAST, MIDDLE
        INTEGER :: MIN_GAP
        INTEGER :: COUNTER
        my_real :: DX1,DX2,DX2_FIRST,DX2_LAST,DX2_MIDDLE
        my_real :: PRODUCT_FM,PRODUCT_ML
        my_real :: DIV, DIV0

!       ----------------------------------------
        MIN_GAP = 20
        DX2 = TF(NPF(IFUNC)) - XX
        !       -----------------------------
        IF ((NPF(IFUNC+1)-NPF(IFUNC))==2) THEN
        !       constant function
                FINTER = TF(NPF(IFUNC)+1)
                RETURN
        ELSE
        !       -----------------------------
        !       check the number of point in the function:
        !       if point_nbt < MIN_GAP --> classical interpolation
        !       if point_nbt > MIN_GAP --> dichotomy in order to reduce the point number interval
        !                            and then when the interval is < MIN_GAP --> classical interpolation
        !       -----------------------------
                POINT_NBR = ( NPF(IFUNC+1)-2 - (NPF(IFUNC)+2) ) / 2 + 1
                IF(POINT_NBR<MIN_GAP) THEN
                        !       -----------------------------
                        !       classical interpolation
                        DO I=NPF(IFUNC)+2,NPF(IFUNC+1)-2,2
                                DX1 = -DX2
                                DX2 = TF(I) - XX
                                IF(DX2>=ZERO.OR.I==NPF(IFUNC+1)-2)THEN
                                        DIV0 = TF(I) - TF(I-2)
                                        DIV = MAX(ABS(DIV0),EM16)
                                        DIV = SIGN(DIV,DIV0)
                                        DERI = (TF(I+1) - TF(I-1)) / DIV
                                        IF(DX1<=DX2)THEN
                                                FINTER = TF(I-1) + DX1 * DERI
                                        ELSE
                                                FINTER = TF(I+1) - DX2 * DERI
                                        ENDIF
                                        RETURN
                                ENDIF
                        ENDDO
                        !       -----------------------------
                ELSE
                        !       -----------------------------
                        !       dichotomy
                        !       first shot : (a) we check the first value 

                        DX2 = TF(NPF(IFUNC)) - XX
                        I=NPF(IFUNC)+2
                        DX1 = -DX2
                        DX2 = TF(I) - XX
                        IF(DX2>=ZERO.OR.I==NPF(IFUNC+1)-2)THEN
                                DIV0 = TF(I) - TF(I-2)
                                DIV = MAX(ABS(DIV0),EM16)
                                DIV = SIGN(DIV,DIV0)
                                DERI = (TF(I+1) - TF(I-1)) / DIV
                                IF(DX1<=DX2)THEN
                                        FINTER = TF(I-1) + DX1 * DERI
                                ELSE
                                        FINTER = TF(I+1) - DX2 * DERI 
                                ENDIF
                                RETURN
                        ENDIF

                        !       first shot : (b) we check the last value 
                        DX2 = TF(NPF(IFUNC+1)-2) - XX
                        I=NPF(IFUNC+1)-2
                        DX1 = -DX2
                        DX2 = TF(I) - XX
                        IF(DX2 <= ZERO)THEN
                                DIV0 = TF(I) - TF(I-2)
                                DIV = MAX(ABS(DIV0),EM16)
                                DIV = SIGN(DIV,DIV0)
                                DERI = (TF(I+1) - TF(I-1)) / DIV
                                IF(DX1 == ZERO .AND. DX2 == ZERO) THEN
                                  FINTER = TF(I+1)
                                ELSEIF(DX1<=DX2)THEN
                                        FINTER = TF(I-1) + DX1 * DERI
                                ELSE
                                        FINTER = TF(I+1) - DX2 * DERI
                                ENDIF
                                RETURN
                        ENDIF
                

                        !       -----------------------------
                        !       second shot : (a) dichotomy in order to reduce the gap
                        FIRST = 1 
                        LAST = POINT_NBR 
                        BOOL=.TRUE.
                        DX2 = TF(NPF(IFUNC)) - XX    
                        COUNTER = 0     

                        DO WHILE(BOOL)
                                MIDDLE = (LAST - FIRST) / 2 + FIRST
                                DX2_FIRST= TF(NPF(IFUNC)+2*FIRST) - XX
                                DX2_LAST= TF(NPF(IFUNC)+2*LAST) - XX
                                DX2_MIDDLE= TF(NPF(IFUNC)+2*MIDDLE) - XX

        
                                PRODUCT_FM = DX2_FIRST*DX2_MIDDLE
                                PRODUCT_ML = DX2_MIDDLE*DX2_LAST

                                IF(PRODUCT_FM<0) THEN
                                        LAST=MIDDLE
                                ELSEIF(PRODUCT_ML<0) THEN
                                        FIRST=MIDDLE
                                ELSE
                                        BOOL=.FALSE.
                                ENDIF
                                IF( (LAST-FIRST)<MIN_GAP) BOOL=.FALSE.
                                !   ---------------------
                                !   check in order to avoid infinite loop
                                COUNTER = COUNTER + 1
                                IF( COUNTER>POINT_NBR ) THEN
                                    !   INFINITE LOOP DETECTED
                                    COUNTER = -1
                                    BOOL=.FALSE.
                                ENDIF
                                !   ---------------------
                        ENDDO
                        !       -----------------------------                
                        !       second shot : (b) classical interpolation with reduced interval

                        !   ------------------------
                        !   INFINITE LOOP DETECTED
                        !   the dichotomy failed to reduce the interval, back to old treatment
                        IF( COUNTER == -1 ) THEN
                                FIRST = 1
                                LAST = POINT_NBR 
                        ENDIF
                        !   ------------------------
                        DX2 = TF(NPF(IFUNC)+2*FIRST-2) - XX            
                        DO J=FIRST,LAST
                                I=NPF(IFUNC)+2*J
                                DX1 = -DX2
                                DX2 = TF(I) - XX
                                IF(DX2>=ZERO.OR.J==LAST)THEN
                                        DIV0 = TF(I) - TF(I-2)
                                        DIV = MAX(ABS(DIV0),EM16)
                                        DIV = SIGN(DIV,DIV0)

                                        DERI = (TF(I+1) - TF(I-1)) / DIV
                                        IF(DX1<=DX2)THEN
                                                FINTER = TF(I-1) + DX1 * DERI
                                        ELSE
                                                FINTER = TF(I+1) - DX2 * DERI
                                        ENDIF
                                        RETURN
                                ENDIF
                        ENDDO
                        !       -----------------------------
                ENDIF   !       end of point_nbr < MIN_GAP
        END IF  !       end of constant function
C
        RETURN
        END
!       ----------------------------------------
