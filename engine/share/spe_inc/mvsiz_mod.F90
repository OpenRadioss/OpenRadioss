!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.

!||====================================================================
!||    mvsiz_mod                       ../engine/share/spe_inc/mvsiz_mod.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||    funct_python_update_elements    ../engine/source/tools/curve/funct_python_update_elements.F90
!||    geom_vec                        ../engine/source/ale/alemuscl/geom_vec.F90
!||    h3d_oned_scalar                 ../engine/source/output/h3d/h3d_results/h3d_oned_scalar.F90
!||    h3d_quad_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
!||    init_ale_boundary_condition     ../engine/source/ale/init_ale_boundary_condition.F90
!||    mmain                           ../engine/source/materials/mat_share/mmain.F90
!||    mulaw8                          ../engine/source/materials/mat_share/mulaw8.F90
!||    redef3                          ../engine/source/elements/spring/redef3.F90
!||    redef_seatbelt                  ../engine/source/tools/seatbelts/redef_seatbelt.F90
!||    s10get_x0                       ../engine/source/elements/solid/solide10/s10get_x0.F90
!||    s6chour_ctl                     ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
!||    s6fint_reg                      ../engine/source/elements/solid/solide6z/s6fint_reg.F90
!||    s6for_distor                    ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
!||    s6get_xv                        ../engine/source/elements/thickshell/solide6c/s6get_xv.F90
!||    s6zderi3                        ../engine/source/elements/solid/solide6z/s6zderi3.F90
!||    s6zfint3                        ../engine/source/elements/solid/solide6z/s6zfint3.F90
!||    s6zforc3                        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||    s6zhour3                        ../engine/source/elements/solid/solide6z/s6zhourg3.F90
!||    sdistor_ini                     ../engine/source/elements/solid/solide/sdistror_ini.F90
!||    sfor_3n2s3                      ../engine/source/elements/solid/solide/sfor_4n2s4.F90
!||    sfor_4n2s4                      ../engine/source/elements/solid/solide/sfor_4n2s4.F90
!||    sfor_ns2s4                      ../engine/source/elements/solid/solide/sfor_ns2s4.F90
!||    sfor_visn6                      ../engine/source/elements/thickshell/solide6c/sfor_visn6.F90
!||    shour_ctl                       ../engine/source/elements/solid/solidez/shour_ctl.F90
!||    sigeps130                       ../engine/source/materials/mat/mat130/sigeps130.F90
!||    sigeps88                        ../engine/source/materials/mat/mat088/sigeps88.F90
!||    sigeps88c                       ../engine/source/materials/mat/mat088/sigeps88c.F90
!||    sz_dt1                          ../engine/source/elements/solid/solidez/sz_dt1.F90
!||====================================================================
      module mvsiz_mod
      implicit none
#include "mvsiz_p.inc"


      end module mvsiz_mod
