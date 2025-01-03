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
      !||    nod2el_mod              ../starter/share/modules1/nod2el_mod.F
      !||--- called by ------------------------------------------------------
      !||    i2tid3                  ../starter/source/interfaces/inter3d1/i2tid3.F
      !||    lectur                  ../starter/source/starter/lectur.F
      !||    merge_node              ../starter/source/elements/nodes/merge_node.F
      !||    r2r_check_seg           ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_count               ../starter/source/coupling/rad2rad/r2r_count.F
      !||    r2r_prelec              ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_split               ../starter/source/coupling/rad2rad/r2r_split.F
      !||    tag_elem_void_r2r       ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||    tag_elem_void_r2r_lin   ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||====================================================================
      MODULE NOD2EL_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C     DO IN=1,NUMNOD
C     DO K=KNOD2ELS(IN)+1,KNOD2ELS(IN+1)
C       IELS=NOD2ELS(K)   ! Solid element number IELS contains node IN
C
C     DO IN=1,NUMNOD
C     DO K=KNOD2ELC(IN)+1,KNOD2ELC(IN+1)
C       IELC=NOD2ELC(K)   ! Shell element number IELC contains node IN
C
C     DO IN=1,NUMNOD
C     DO K=KNOD2ELTG(IN)+1,KNOD2ELTG(IN+1)
C       IELTG=NOD2ELTG(K)   ! 3-node shell element number IELTG contains node IN
C
C     DO IN=1,NUMNOD
C     DO K=KNOD2ELIG3D(IN)+1,KNOD2ELIG3D(IN+1)
C       IELIG3D=NOD2ELIG3D(K)   ! Solid isogeometric element number IELIG3D contains node IN
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: KNOD2ELS,KNOD2ELC,KNOD2ELTG,
     .                                      NOD2ELS ,NOD2ELC ,NOD2ELTG,
     .                                      KNOD2EL1D, NOD2EL1D,
     .                                      KNOD2ELIG3D,NOD2ELIG3D,
     .                                      KNOD2ELQ,NOD2ELQ
C-----------------------------------------------
      END MODULE NOD2EL_MOD
