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
Chd|  C_IEBCS                       source/restart/ddsplit/c_iebcs.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|====================================================================
       SUBROUTINE C_IEBCS(IXS, IXQ, IXTG,
     .     NUMELS, NUMELQ, NUMELTG,
     .     IPM, NEBCS, CEP, NUMEL, NUMEL_L, PROC,
     .     IEBCS_NELEM_L, IEBCS_TYPE, IEBCS_LISTELEM_L, IEBCS_LISTFAC_L,IEBCS_LISTDP0_L, LENGTH, N2D,
     .     MULTI_FVM_IS_USED,FLAG,EBCS_TAB)
       USE EBCS_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN), TARGET :: IXS(NIXS, NUMELS), IXQ(NIXQ, NUMELQ), IXTG(NIXTG, NUMELTG),
     .     NUMELS, NUMELQ, NUMELTG
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      INTEGER, INTENT(IN) :: NEBCS, CEP(*), NUMEL, NUMEL_L, PROC, N2D
      INTEGER, INTENT(INOUT) :: LENGTH, IEBCS_NELEM_L(NEBCS), IEBCS_TYPE(NEBCS),
     .     IEBCS_LISTELEM_L(*), IEBCS_LISTFAC_L(*)
      my_real, INTENT(INOUT) :: IEBCS_LISTDP0_L(*)
      LOGICAL, INTENT(IN) :: MULTI_FVM_IS_USED
      INTEGER, INTENT(IN) :: FLAG ! 0 = count, 1 = fill
      TYPE(t_ebcs_tab), TARGET, INTENT(IN) :: EBCS_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------   
      INTEGER :: I, KK, TYP, JBUF, K1, K2, K3, NSEG, NSEG_L, IELEM, ELEM_ID, ISEG
      INTEGER :: II
      INTEGER, DIMENSION(:), ALLOCATABLE  :: LOCALID
      INTEGER, DIMENSION(:, :), POINTER :: IX
      my_real :: DP0
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------   
      ALLOCATE(LOCALID(NUMEL))
      IF (N2D == 0) THEN
         IX => IXS(1:NIXS, 1:NUMELS)
      ELSEIF(NUMELQ /= 0) THEN
         IX => IXQ(1:NIXQ, 1:NUMELQ)
      ELSEIF (NUMELTG /= 0 .AND. MULTI_FVM_IS_USED) THEN
         IX => IXTG(1:NIXTG, 1:NUMELTG)
      ENDIF


      IELEM = 0
      DO I = 1, NUMEL
         IF (CEP(I) == PROC) THEN
            IELEM = IELEM + 1
            LOCALID(I) = IELEM
         ENDIF
      ENDDO

      LENGTH = 0

      DO I = 1, NEBCS
         TYP = EBCS_TAB%tab(i)%poly%type
         IEBCS_TYPE(I) = TYP
         NSEG = EBCS_TAB%tab(i)%poly%nb_elem
         NSEG_L = 0
         IF (EBCS_TAB%tab(i)%poly%has_ielem .AND. allocated(EBCS_TAB%tab(i)%poly%ielem) ) THEN  !ebcs staggered allocated on proc 1 only
            DO ISEG = 1, NSEG
               IELEM = EBCS_TAB%tab(i)%poly%ielem(ISEG)
               DP0=ZERO
               IF(EBCS_TAB%tab(i)%poly%has_dp0) DP0 = EBCS_TAB%tab(i)%poly%dp0(ISEG)
               !IF (IPM(2, IX(1, IELEM)) == 151) THEN
                  IF (CEP(IELEM)  == PROC) THEN
                     NSEG_L = NSEG_L + 1
                     IF(FLAG == 1) THEN
                       IEBCS_LISTELEM_L(LENGTH + NSEG_L) = LOCALID(IELEM)
                       IEBCS_LISTFAC_L(LENGTH + NSEG_L) = EBCS_TAB%tab(i)%poly%iface(ISEG)
                       IEBCS_LISTDP0_L(LENGTH + NSEG_L) = DP0
                     ENDIF
                  ENDIF
               !ENDIF
            ENDDO
         ENDIF
         IEBCS_NELEM_L(I) = NSEG_L
         LENGTH = LENGTH + NSEG_L
      ENDDO ! I = 1, NEBCS

      DEALLOCATE(LOCALID)
      RETURN
      END SUBROUTINE C_IEBCS
