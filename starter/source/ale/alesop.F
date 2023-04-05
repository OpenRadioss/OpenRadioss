Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  ALESOP                        source/ale/alesop.F           
Chd|-- called by -----------
Chd|        ALELEC                        source/ale/alelec.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALESOP(NALE,ALE_NN_CONNECT,ICODE,ITAB)
C-----------------------------------------------
C   Description
C-----------------------------------------------
C This subroutine is writing in Starter listing 
C  file the ALE nodal connectivities
C  precondition : IPRI >= 3  (from /IOFLAG Starter option)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: NALE, ICODE, ITAB
      TYPE(t_connectivity), INTENT(IN) :: ALE_NN_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ(12), N, IC, IC1, IC2, IC3, IC4, I, IAD1, IAD2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(IPRI >= 3)THEN
        WRITE(IOUT,1000)
        DO N=1,NUMNOD
          IF(NALE(N) /= 0)THEN
            IC  = ICODE(N)
            IC1 = IC/512
            IC2 = (IC-512*IC1)/64
            IC3 = (IC-512*IC1-64*IC2)/8
            IC4 = (IC-512*IC1-64*IC2-8*IC3)
            JJ( 1) = IC1/4
            JJ( 2) = (IC1-4*JJ(1))/2
            JJ( 3) = (IC1-4*JJ(1)-2*JJ(2))
            JJ( 4) = IC2/4
            JJ( 5) = (IC2-4*JJ(4))/2
            JJ( 6) = (IC2-4*JJ(4)-2*JJ(5))
            JJ( 7) = IC3/4
            JJ( 8) = (IC3-4*JJ(7))/2
            JJ( 9) = (IC3-4*JJ(7)-2*JJ(8))
            JJ(10) = IC4/4
            JJ(11) = (IC4-4*JJ(10))/2
            JJ(12) = (IC4-4*JJ(10)-2*JJ(11))
            IAD1 = ALE_NN_CONNECT%IAD_CONNECT(N)
            IAD2 = ALE_NN_CONNECT%IAD_CONNECT(N + 1) - 1
            WRITE(IOUT,*)ITAB(N),JJ,ITAB(ALE_NN_CONNECT%CONNECTED(IAD1:IAD2))
          ENDIF
        ENDDO !N=1,NUMNOD
      ENDIF !IF(IPRI >= 3)
C-----------------------------------------------
      RETURN
C-----------------------------------------------
 1000 FORMAT(//
     & '       A.L.E.  NODAL CONNECTIVITIES  ',/
     & '       ----------------------------  ',/
     & '      NODE TRA ROT GRI LAG      NODE')
      END
