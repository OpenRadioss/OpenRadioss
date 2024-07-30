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
      !||    zeror          ../engine/source/system/zero.F
      !||--- called by ------------------------------------------------------
      !||    imp_buck       ../engine/source/implicit/imp_buck.F
      !||    imp_chkm       ../engine/source/implicit/imp_solv.F
      !||    imp_init       ../engine/source/implicit/imp_init.F
      !||    imp_restarcp   ../engine/source/implicit/imp_sol_init.F
      !||    int_matv       ../engine/source/implicit/imp_int_k.F
      !||    int_matvp      ../engine/source/implicit/imp_int_k.F
      !||    monv_m3        ../engine/source/airbag/monv_imp0.F
      !||    mv_matv        ../engine/source/airbag/monv_imp0.F
      !||    resol          ../engine/source/engine/resol.F
      !||    resol_init     ../engine/source/engine/resol_init.F
      !||    rgwal0_imp     ../engine/source/constraints/general/rwall/rgwal0.F
      !||====================================================================
      SUBROUTINE ZEROR(A,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,N
       A(1,I)=ZERO
       A(2,I)=ZERO
       A(3,I)=ZERO
      ENDDO
      RETURN
      END
      !||====================================================================
      !||    zero1           ../engine/source/system/zero.F
      !||--- called by ------------------------------------------------------
      !||    autspc          ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    clceig          ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    i2recu0         ../engine/source/interfaces/interf/i2_imp2.F
      !||    i2recu2         ../engine/source/interfaces/interf/i2_imp2.F
      !||    imp_chkm        ../engine/source/implicit/imp_solv.F
      !||    imp_sol_init    ../engine/source/implicit/imp_sol_init.F
      !||    imp_solv        ../engine/source/implicit/imp_solv.F
      !||    ind_sld         ../engine/source/mpi/implicit/imp_fri.F
      !||    nrmlzauspc      ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    rbe3cl          ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rbe3t1          ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rbe3uf          ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rbe3um          ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    resol           ../engine/source/engine/resol.F
      !||    resol_init      ../engine/source/engine/resol_init.F
      !||    set_ind_fr      ../engine/source/mpi/implicit/imp_fri.F
      !||    sms_rbe3_prec   ../engine/source/ams/sms_rbe3.F
      !||    sms_rbe3t1      ../engine/source/ams/sms_rbe3.F
      !||====================================================================
      SUBROUTINE ZERO1(A,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,N
       A(I)=ZERO
      ENDDO
      RETURN
      END
