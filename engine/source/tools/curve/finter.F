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
      !||====================================================================
      !||    finter                     ../engine/source/tools/curve/finter.F
      !||--- called by ------------------------------------------------------
      !||    airbaga                    ../engine/source/airbag/airbag1.F
      !||    airbaga1                   ../engine/source/airbag/airbaga1.F
      !||    aleflow                    ../engine/source/ale/porous/aleflow.F
      !||    alefvm_grav_init           ../engine/source/ale/alefvm/alefvm_grav_init.F
      !||    cfield_1                   ../engine/source/loads/general/load_centri/cfield.F
      !||    cfield_imp                 ../engine/source/loads/general/load_centri/cfield_imp.F
      !||    cmain3                     ../engine/source/materials/mat_share/cmain3.F
      !||    cmain3pinch                ../engine/source/elements/shell/coqueba/cmain3pinch.F
      !||    convec                     ../engine/source/constraints/thermic/convec.F
      !||    daasolv                    ../engine/source/fluid/daasolv.F
      !||    daasolvp                   ../engine/source/fluid/daasolvp.F
      !||    dam_fld_sol                ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
      !||    fail_biquad_b              ../engine/source/materials/fail/biquad/fail_biquad_b.F
      !||    fail_biquad_c              ../engine/source/materials/fail/biquad/fail_biquad_c.F
      !||    fail_biquad_ib             ../engine/source/materials/fail/biquad/fail_biquad_ib.F
      !||    fail_biquad_s              ../engine/source/materials/fail/biquad/fail_biquad_s.F
      !||    fail_connect               ../engine/source/materials/fail/connect/fail_connect.F
      !||    fail_energy_b              ../engine/source/materials/fail/energy/fail_energy_b.F
      !||    fail_energy_c              ../engine/source/materials/fail/energy/fail_energy_c.F
      !||    fail_energy_ib             ../engine/source/materials/fail/energy/fail_energy_ib.F
      !||    fail_energy_s              ../engine/source/materials/fail/energy/fail_energy_s.F
      !||    fail_fabric_c              ../engine/source/materials/fail/fabric/fail_fabric_c.F
      !||    fail_fld_c                 ../engine/source/materials/fail/fld/fail_fld_c.F
      !||    fail_fld_tsh               ../engine/source/materials/fail/fld/fail_fld_tsh.F
      !||    fail_fld_xfem              ../engine/source/materials/fail/fld/fail_fld_xfem.F
      !||    fail_gene1_c               ../engine/source/materials/fail/gene1/fail_gene1_c.F
      !||    fail_gene1_s               ../engine/source/materials/fail/gene1/fail_gene1_s.F
      !||    fail_nxt_c                 ../engine/source/materials/fail/nxt/fail_nxt_c.F
      !||    fail_orthbiquad_c          ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_c.F
      !||    fail_orthbiquad_s          ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_s.F
      !||    fail_orthstrain            ../engine/source/materials/fail/orthstrain/fail_orthstrain_s.F
      !||    fail_orthstrain_c          ../engine/source/materials/fail/orthstrain/fail_orthstrain_c.F
      !||    fail_sahraei_s             ../engine/source/materials/fail/sahraei/fail_sahraei_s.F
      !||    fail_snconnect             ../engine/source/materials/fail/snconnect/fail_snconnect.F
      !||    fail_syazwan_c             ../engine/source/materials/fail/syazwan/fail_syazwan_c.F
      !||    fail_syazwan_s             ../engine/source/materials/fail/syazwan/fail_syazwan_s.F
      !||    fail_tab2_c                ../engine/source/materials/fail/tabulated/fail_tab2_c.F
      !||    fail_tab2_s                ../engine/source/materials/fail/tabulated/fail_tab2_s.F
      !||    fail_tab_c                 ../engine/source/materials/fail/tabulated/fail_tab_c.F
      !||    fail_tab_old_c             ../engine/source/materials/fail/tabulated/fail_tab_old_c.F
      !||    fail_tab_old_s             ../engine/source/materials/fail/tabulated/fail_tab_old_s.F
      !||    fail_tab_old_xfem          ../engine/source/materials/fail/tabulated/fail_tab_old_xfem.F
      !||    fail_tab_s                 ../engine/source/materials/fail/tabulated/fail_tab_s.F
      !||    fail_tab_xfem              ../engine/source/materials/fail/tabulated/fail_tab_xfem.F
      !||    fail_tensstrain_b          ../engine/source/materials/fail/tensstrain/fail_tensstrain_b.F
      !||    fail_tensstrain_c          ../engine/source/materials/fail/tensstrain/fail_tensstrain_c.F
      !||    fail_tensstrain_ib         ../engine/source/materials/fail/tensstrain/fail_tensstrain_ib.F
      !||    fail_tensstrain_s          ../engine/source/materials/fail/tensstrain/fail_tensstrain_s.F
      !||    fixflux                    ../engine/source/constraints/thermic/fixflux.F
      !||    force                      ../engine/source/loads/general/force.F90
      !||    forcefingeo                ../engine/source/loads/general/forcefingeo.F
      !||    forcepinch                 ../engine/source/loads/general/forcepinch.F
      !||    frictionparts_model_isot   ../engine/source/interfaces/int07/frictionparts_model.F
      !||    fv_up_switch               ../engine/source/airbag/fv_up_switch.F
      !||    fvbag1                     ../engine/source/airbag/fvbag1.F
      !||    fvinjt6                    ../engine/source/airbag/fvinjt6.F
      !||    fvinjt8                    ../engine/source/airbag/fvinjt8.F
      !||    fxbodfp2                   ../engine/source/constraints/fxbody/fxbodfp.F
      !||    fxgrvcor                   ../engine/source/constraints/fxbody/fxgrvcor.F
      !||    get_preload_axial          ../engine/source/elements/spring/preload_axial.F90
      !||    gravit                     ../engine/source/loads/general/grav/gravit.F
      !||    gravit_fvm_fem             ../engine/source/loads/general/grav/gravit_fvm_fem.F
      !||    gravit_imp                 ../engine/source/loads/general/grav/gravit_imp.F
      !||    h3d_pre_skin_scalar        ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_vector            ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
      !||    i11therm                   ../engine/source/interfaces/int11/i11therm.F
      !||    i21for3                    ../engine/source/interfaces/int21/i21for3.F
      !||    i21therm                   ../engine/source/interfaces/int21/i21therm.F
      !||    i23mainf                   ../engine/source/interfaces/int23/i23mainf.F
      !||    i25therm                   ../engine/source/interfaces/int25/i25therm.F
      !||    i6ass3                     ../engine/source/interfaces/inter3d/i6ass3.F
      !||    i7therm                    ../engine/source/interfaces/int07/i7therm.F
      !||    idx_fld_sol                ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
      !||    imp_kpres                  ../engine/source/implicit/imp_glob_k.F
      !||    incpflow                   ../engine/source/fluid/incpflow.F
      !||    lag_fxv                    ../engine/source/tools/lagmul/lag_fxv.F
      !||    lag_fxvp                   ../engine/source/tools/lagmul/lag_fxv.F
      !||    load_pressure              ../engine/source/loads/general/load_pressure/load_pressure.F
      !||    m11law                     ../engine/source/materials/mat/mat011/m11law.F
      !||    m21law                     ../engine/source/materials/mat/mat021/m21law.F
      !||    mat87c_tabulated_plas_sr   ../engine/source/materials/mat/mat087/mat87c_tabulated_plas_sr.F
      !||    mat87c_tabulated_totalsr   ../engine/source/materials/mat/mat087/mat87c_tabulated_totalsr.F
      !||    mmain                      ../engine/source/materials/mat_share/mmain.F90
      !||    pfluid                     ../engine/source/loads/general/pfluid/pfluid.F
      !||    pforc3                     ../engine/source/elements/beam/pforc3.F
      !||    r26sig                     ../engine/source/elements/spring/r26sig.F
      !||    r27def3                    ../engine/source/elements/spring/r27def3.F
      !||    radiation                  ../engine/source/constraints/thermic/radiation.F
      !||    ruptint2                   ../engine/source/interfaces/interf/ruptint2.F
      !||    sigeps101                  ../engine/source/materials/mat/mat101/sigeps101.F
      !||    sigeps105                  ../engine/source/materials/mat/mat105/sigeps105.F
      !||    sigeps106                  ../engine/source/materials/mat/mat106/sigeps106.F
      !||    sigeps111                  ../engine/source/materials/mat/mat111/sigeps111.F
      !||    sigeps117                  ../engine/source/materials/mat/mat117/sigeps117.F
      !||    sigeps187                  ../engine/source/materials/mat/mat187/sigeps187.F
      !||    sigeps28                   ../engine/source/materials/mat/mat028/sigeps28.F
      !||    sigeps33                   ../engine/source/materials/mat/mat033/sigeps33.F
      !||    sigeps34c                  ../engine/source/materials/mat/mat034/sigeps34c.F
      !||    sigeps35                   ../engine/source/materials/mat/mat035/sigeps35.F
      !||    sigeps35c                  ../engine/source/materials/mat/mat035/sigeps35c.F
      !||    sigeps36                   ../engine/source/materials/mat/mat036/sigeps36.F
      !||    sigeps36g                  ../engine/source/materials/mat/mat036/sigeps36g.F
      !||    sigeps36pi                 ../engine/source/materials/mat/mat036/sigeps36pi.F
      !||    sigeps37                   ../engine/source/materials/mat/mat037/sigeps37.F
      !||    sigeps38                   ../engine/source/materials/mat/mat038/sigeps38.F
      !||    sigeps40                   ../engine/source/materials/mat/mat040/sigeps40.F
      !||    sigeps42                   ../engine/source/materials/mat/mat042/sigeps42.F
      !||    sigeps43c                  ../engine/source/materials/mat/mat043/sigeps43c.F
      !||    sigeps43g                  ../engine/source/materials/mat/mat043/sigeps43g.F
      !||    sigeps44p                  ../engine/source/materials/mat/mat044/sigeps44p.F
      !||    sigeps44t                  ../engine/source/materials/mat/mat044/sigeps44t.F
      !||    sigeps45                   ../engine/source/materials/mat/mat045/sigeps45.F
      !||    sigeps45c                  ../engine/source/materials/mat/mat045/sigeps45c.F
      !||    sigeps48                   ../engine/source/materials/mat/mat048/sigeps48.F
      !||    sigeps50                   ../engine/source/materials/mat/mat050/sigeps50.F
      !||    sigeps51                   ../engine/source/materials/mat/mat051/sigeps51.F
      !||    sigeps52c                  ../engine/source/materials/mat/mat052/sigeps52c.F
      !||    sigeps55c                  ../engine/source/materials/mat/mat055/sigeps55c.F
      !||    sigeps56c                  ../engine/source/materials/mat/mat056/sigeps56c.F
      !||    sigeps57c                  ../engine/source/materials/mat/mat057/sigeps57c.F
      !||    sigeps58c                  ../engine/source/materials/mat/mat058/sigeps58c.F
      !||    sigeps59                   ../engine/source/materials/mat/mat059/sigeps59.F
      !||    sigeps60                   ../engine/source/materials/mat/mat060/sigeps60.F
      !||    sigeps60c                  ../engine/source/materials/mat/mat060/sigeps60c.F
      !||    sigeps60g                  ../engine/source/materials/mat/mat060/sigeps60g.F
      !||    sigeps62                   ../engine/source/materials/mat/mat062/sigeps62.F
      !||    sigeps62c                  ../engine/source/materials/mat/mat062/sigeps62c.F
      !||    sigeps63c                  ../engine/source/materials/mat/mat063/sigeps63c.F
      !||    sigeps64c                  ../engine/source/materials/mat/mat064/sigeps64c.F
      !||    sigeps65                   ../engine/source/materials/mat/mat065/sigeps65.F
      !||    sigeps65c                  ../engine/source/materials/mat/mat065/sigeps65c.F
      !||    sigeps66                   ../engine/source/materials/mat/mat066/sigeps66.F
      !||    sigeps66c                  ../engine/source/materials/mat/mat066/sigeps66c.F
      !||    sigeps67                   ../engine/source/materials/mat/mat067/sigeps67.F
      !||    sigeps68                   ../engine/source/materials/mat/mat068/sigeps68.F
      !||    sigeps69                   ../engine/source/materials/mat/mat069/sigeps69.F
      !||    sigeps70                   ../engine/source/materials/mat/mat070/sigeps70.F
      !||    sigeps71                   ../engine/source/materials/mat/mat071/sigeps71.F
      !||    sigeps71c                  ../engine/source/materials/mat/mat071/sigeps71c.F
      !||    sigeps73c                  ../engine/source/materials/mat/mat073/sigeps73c.F
      !||    sigeps74                   ../engine/source/materials/mat/mat074/sigeps74.F
      !||    sigeps75                   ../engine/source/materials/mat/mat075/sigeps75.F
      !||    sigeps77                   ../engine/source/materials/mat/mat077/sigeps77.F
      !||    sigeps78                   ../engine/source/materials/mat/mat078/sigeps78.F
      !||    sigeps80                   ../engine/source/materials/mat/mat080/sigeps80.F
      !||    sigeps80c                  ../engine/source/materials/mat/mat080/sigeps80c.F
      !||    sigeps81                   ../engine/source/materials/mat/mat081/sigeps81.F
      !||    sigeps82                   ../engine/source/materials/mat/mat082/sigeps82.F
      !||    sigeps82c                  ../engine/source/materials/mat/mat082/sigeps82c.F
      !||    sigeps83                   ../engine/source/materials/mat/mat083/sigeps83.F
      !||    sigeps84                   ../engine/source/materials/mat/mat084/sigeps84.F
      !||    sigeps85c_void             ../engine/source/materials/mat/mat085/sigeps85c_void.F
      !||    sigeps86c                  ../engine/source/materials/mat/mat086/sigeps86c.F
      !||    sigeps87c                  ../engine/source/materials/mat/mat087/sigeps87c.F
      !||    sigeps88                   ../engine/source/materials/mat/mat088/sigeps88.F
      !||    sigeps88c                  ../engine/source/materials/mat/mat088/sigeps88c.F
      !||    sigeps92                   ../engine/source/materials/mat/mat092/sigeps92.F
      !||    sigeps94                   ../engine/source/materials/mat/mat094/sigeps94.F
      !||    sigeps95                   ../engine/source/materials/mat/mat095/sigeps95.F
      !||    sigeps97                   ../engine/source/materials/mat/mat097/sigeps97.F
      !||    sms_gravit                 ../engine/source/ams/sms_gravit.F
      !||    thermexpc                  ../engine/source/materials/mat_share/thermexpc.F
      !||    volp_lfluid                ../engine/source/airbag/volp_lfluid.F
      !||    volpfv                     ../engine/source/airbag/volpfv.F
      !||    volpre                     ../engine/source/airbag/volpres.F
      !||    volprep                    ../engine/source/airbag/volpresp.F
      !||====================================================================
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
        FINTER = ZERO
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
