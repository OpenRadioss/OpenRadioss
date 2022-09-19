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
Chd|  INIGRAV_M37                   source/initial_conditions/inigrav/inigrav_m37.F
Chd|-- called by -----------
Chd|        INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INIGRAV_M37(NELG, NEL, NG, MATID, IPM, GRAV0, DEPTH, PM, BUFMAT, ELBUF_TAB, PSURF, LIST)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
!     NPROPMI, NPROPM
#include      "param_c.inc"
!     NGROUP
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL, NG, MATID, IPM(NPROPMI, *), LIST(NEL),NELG
      my_real, INTENT(IN) :: GRAV0, DEPTH(*), PM(NPROPM, *), BUFMAT(*)
      my_real, INTENT(INOUT) :: PSURF
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET, INTENT(IN) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,ISOLVER, K
      my_real :: R1, C1, P0, PGRAV, RHO10, RHO20, RHO1, RHO2, GAM, RHO0,
     .     ALPHA1, ALPHA2,PSH
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C LIST IS SUBGROUP TO TREAT : ONLY ELEM WITH RELEVANT PARTS ARE KEPT
C NEL IS ISEZ OF LIST
C NELG IS SIZE OF ORIGINAL GROUP : needed to shift indexes in GBUF%SIG & MBUF%VAR

C     Global buffer
      GBUF => ELBUF_TAB(NG)%GBUF
C     Material buffer
      MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
C     EOS parameters, common
      P0 = BUFMAT(9)
      PSH= BUFMAT(16)
C     EOS parameters mat 1: 
      RHO10 = BUFMAT(11)
      C1 = BUFMAT(4)
      R1 = BUFMAT(6)
C     EOS parameters mat2:
      GAM = BUFMAT(5)
      RHO20 = BUFMAT(12)
      ISOLVER = BUFMAT(17)

      IF(PSURF==ZERO .AND. ISOLVER<=1)THEN
        PSURF=P0  !historical solver requires a total pressure formulation
        print *, "**WARNING : INIGRAV CARD, PREF PARAMETER MUST BE A TOTAL PRESSURE WITH LAW37, SETTING PREF=P0"
      ENDIF

      DO K = 1, NEL
         I = LIST(K)
         ALPHA1 = MBUF%VAR(I + (4 - 1) * NELG)
         ALPHA2 = ONE - ALPHA1
         RHO0 = ALPHA1 * RHO10 + ALPHA2 * RHO20
         PGRAV = PSURF - RHO0  * GRAV0 * DEPTH(K)
         RHO1 = (PGRAV-P0)/R1 + RHO10
         RHO2 = RHO20 * (PGRAV/P0) ** (ONE / GAM)
         GBUF%RHO(I) = ALPHA1 * RHO1 + ALPHA2 * RHO2
         MBUF%VAR(I + (4 - 1) * NELG) = ALPHA1
         MBUF%VAR(I + (5 - 1) * NELG) = ONE - ALPHA1
         MBUF%VAR(I + (2 - 1) * NELG) = RHO2
         MBUF%VAR(I + (3 - 1) * NELG) = RHO1
         MBUF%VAR(I + (1 - 1) * NELG) = ALPHA1 * RHO1
         GBUF%SIG(I)            = - (PGRAV-P0-PSH)
         GBUF%SIG(I + NELG)     = - (PGRAV-P0-PSH)
         GBUF%SIG(I + 2 * NELG) = - (PGRAV-P0-PSH)
      ENDDO

      END SUBROUTINE INIGRAV_M37
