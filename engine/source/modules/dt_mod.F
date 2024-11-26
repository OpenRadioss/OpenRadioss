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
      !||    dt_mod                ../engine/source/modules/dt_mod.F
      !||--- called by ------------------------------------------------------
      !||    alemain               ../engine/source/ale/alemain.F
      !||    bforc2                ../engine/source/ale/bimat/bforc2.F
      !||    c3forc3               ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk           ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3              ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cdk6forc3             ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3              ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3                ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk            ../engine/source/elements/xfem/cforc3_crk.F
      !||    cmain3                ../engine/source/materials/mat_share/cmain3.F
      !||    czforc3               ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk           ../engine/source/elements/xfem/czforc3_crk.F
      !||    eng_qaprint_driver    ../engine/source/output/qaprint/eng_qaprint_driver.F
      !||    eng_qaprint_dtinput   ../engine/source/output/qaprint/eng_qaprint_dtinput.F
      !||    fail_gene1_c          ../engine/source/materials/fail/gene1/fail_gene1_c.F
      !||    forint                ../engine/source/elements/forint.F
      !||    forintc               ../engine/source/elements/forintc.F
      !||    forintp               ../engine/source/elements/forintp.F
      !||    freform               ../engine/source/input/freform.F
      !||    ig3duforc3            ../engine/source/elements/ige3d/ig3duforc3.F
      !||    lecinp                ../engine/source/input/lecinp.F
      !||    lectur                ../engine/source/input/lectur.F
      !||    mmain                 ../engine/source/materials/mat_share/mmain.F90
      !||    mulaw                 ../engine/source/materials/mat_share/mulaw.F90
      !||    pforc3                ../engine/source/elements/beam/pforc3.F
      !||    q4forc2               ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qforc2                ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    radioss2              ../engine/source/engine/radioss2.F
      !||    resol                 ../engine/source/engine/resol.F
      !||    resol_head            ../engine/source/engine/resol_head.F
      !||    s10forc3              ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s10mallgeo3           ../engine/source/elements/solid/solide10/s10mallgeo3.F
      !||    s16forc3              ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20forc3              ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4forc3               ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cforc3              ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8cforc3              ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3              ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8sforc3              ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3              ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scforc3               ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    sforc3                ../engine/source/elements/solid/solide/sforc3.F
      !||    sgeodel3              ../engine/source/elements/solid/solide/sgeodel3.F
      !||    smallgeo3             ../engine/source/elements/solid/solide/smallgeo3.F
      !||    sortie_main           ../engine/source/output/sortie_main.F
      !||    spstres               ../engine/source/elements/sph/spstres.F
      !||    szforc3               ../engine/source/elements/solid/solidez/szforc3.F
      !||    tforc3                ../engine/source/elements/truss/tforc3.F
      !||    tshgeodel3            ../engine/source/elements/thickshell/solidec/tshgeodel3.F
      !||    usermat_shell         ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid         ../engine/source/materials/mat_share/usermat_solid.F
      !||====================================================================
      MODULE DT_MOD
      
        IMPLICIT NONE
#include      "my_real.inc"
        
        !TYPE definition
        TYPE DT_
          INTEGER IDEL_BRICK
          INTEGER NODADT
          MY_REAL BRICK_CST_DEFV_MIN,BRICK_CST_COL_MIN   ! /DT/BRICK/CST/1
          MY_REAL BRICK_DEL_DEFV_MIN,BRICK_DEL_DEFV_MAX, ! /DT/BRICK/DEL/1
     .            BRICK_DEL_ASP_MAX,BRICK_DEL_COL_MIN
        END TYPE DT_
        
        CONTAINS        
          
C INITIALISATION : CALL DT%INIT_IDEL_BRICK()
      !||====================================================================
      !||    init_idel_brick   ../engine/source/modules/dt_mod.F
      !||--- called by ------------------------------------------------------
      !||    freform           ../engine/source/input/freform.F
      !||====================================================================
          SUBROUTINE INIT_IDEL_BRICK(THIS)
            IMPLICIT NONE
#include    "constant.inc" 
           
            TYPE (DT_), INTENT(INOUT) :: THIS

            THIS%IDEL_BRICK = 0
            IF ((THIS%BRICK_DEL_DEFV_MIN+THIS%BRICK_DEL_DEFV_MAX+
     .          THIS%BRICK_DEL_ASP_MAX+THIS%BRICK_DEL_COL_MIN)>ZERO) THIS%IDEL_BRICK = 1 
                                                            
          END SUBROUTINE    
               
      END MODULE DT_MOD

