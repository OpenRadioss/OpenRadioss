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
Chd|====================================================================
Chd|  INIEBCSP0                     source/boundary_conditions/ebcs/iniebcsp0.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INIEBCSP                      source/boundary_conditions/ebcs/iniebcsp.F
Chd|        INIEBCS_DP                    source/boundary_conditions/ebcs/iniebcs_dp.F
Chd|        INIEBCS_NRF_TCAR              source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale/ale_ebcs_mod.F
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INIGRAV                       share/modules1/inigrav_mod.F  
Chd|====================================================================
      SUBROUTINE INIEBCSP0(X, IPARG, ELBUF_STR, EBCS_TAB, IXS, IXQ, IXTG, IPARTS, IPARTQ, IPARTTG, PM, IPM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD  
      USE ALE_EBCS_MOD 
      USE EBCS_MOD
      USE INIGRAV
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPARG(NPARG,NGROUP),IXS(NIXS,NUMELS), IXQ(NIXQ,NUMELQ), IXTG(NIXTG,NUMELTG)
      INTEGER, INTENT(IN) :: IPARTS(NUMELS), IPARTQ(NUMELQ), IPARTTG(NUMELTG)

      my_real X(3,NUMNOD)
      TYPE (ELBUF_STRUCT_),DIMENSION(NGROUP) :: ELBUF_STR
      TYPE(t_ebcs_tab), TARGET, INTENT(INOUT) :: EBCS_TAB
      my_real,INTENT(IN)  :: PM(NPROPM,NUMMAT)
      INTEGER,INTENT(IN)  :: IPM(NPROPMI,NUMMAT)      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,TYP,NSEG,NOD,RBUF,JBUF,K1,K2,K3,K4,MK1,MK2,MK3,MK4,MK5,MK6
      CLASS(t_ebcs), POINTER :: EBCS
      my_real RHO
      LOGICAL lFOUND_EBCS_NRF
C=======================================================================
      lFOUND_EBCS_NRF = .FALSE.
      
      DO I = 1, NEBCS
         EBCS => EBCS_TAB%tab(i)%poly
         SELECT TYPE (twf => EBCS)
           TYPE IS (t_ebcs_inip)
             NSEG = twf%nb_elem
             NOD = twf%nb_node
             RHO = twf%rho
             CALL INIEBCSP(NSEG, NOD, twf%iseg, twf%ielem, twf%elem_list, twf%node_list,
     .            twf%la, IPARG, ELBUF_STR, twf%p0, RHO, X)
           TYPE IS (t_ebcs_nrf)
             lFOUND_EBCS_NRF = .TRUE.
             NSEG = twf%nb_elem
             CALL INIEBCS_DP(NSEG, NOD, twf%iseg, twf%ielem, twf%elem_list, twf%node_list,
     .                       IPARG, ELBUF_STR, X, IXS, IXQ, IXTG, twf%DP0,
     .                       IPARTS, IPARTQ, IPARTTG)    
           CLASS DEFAULT
             !
        END SELECT
      ENDDO
      
       IF(lFOUND_EBCS_NRF)THEN
          CALL INIEBCS_NRF_TCAR(EBCS_TAB,
     .                       IPARG, X, IXS, IXQ, IXTG,
     .                       IPARTS, IPARTQ, IPARTTG,
     .                       PM, IPM) 
       ENDIF
      
      CALL INIGRAV_PARTS%destroy()
c--------
      END
