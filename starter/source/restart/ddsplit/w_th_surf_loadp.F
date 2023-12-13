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

!> \brief Writting TH/SURF tabs in restart file
!! \details  Load pressures option

Chd|====================================================================
Chd|  W_TH_SURF_LOADP               source/restart/ddsplit/w_th_surf_loadp.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TH_SURF_MOD                   ../common_source/modules/interfaces/th_surf_mod.F
Chd|====================================================================
      SUBROUTINE W_TH_SURF_LOADP(ILOADP ,NODLOCAL ,PROC     ,LEN_IA   ,CEP    ,
     .                           TH_SURF,NUMNOD   ,SIZLOADP ,NLOADP   ,LEN_CEP,
     .                           OFF    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE TH_SURF_MOD , ONLY : TH_SURF_
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: NUMNOD   ,SIZLOADP ,NLOADP   ,LEN_CEP  !< Parameters for size tables ( number of nodes,number of parameters in ILOADP tab, number of load pressure, size of CEP)
      INTEGER , INTENT(IN) :: OFF               !< Index in table CEP
      INTEGER , INTENT(IN) :: PROC              !< Processor
      INTEGER , INTENT(INOUT) :: LEN_IA            !< Lenght of integer tables written in restart
      INTEGER , INTENT(IN) :: ILOADP(SIZLOADP,NLOADP)     !< Integer tabs for load pressures (/PFLUID, /PBLAST, /LOAD/PRESSURE )
      INTEGER , INTENT(IN) :: NODLOCAL(NUMNOD)   !< table of local nodes
      INTEGER , INTENT(IN) :: CEP(LEN_CEP)          !< Proc to which belong the element
      TYPE (TH_SURF_) , INTENT(INOUT) :: TH_SURF !< Type for /TH/SURF and load pressures output tabs
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NSEGP, NSEGPL, NL, NS, K, ADCEP
      INTEGER, DIMENSION(:),ALLOCATABLE :: KSEGS_L, SEGS_L
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C-----------------------------------------------
C         Writting TH/SURF tabs in restart file
C          /PFLUID , /PBLAST, /LOADP_HYD option
C-----------------------------------------------

      CALL MY_ALLOC(KSEGS_L,TH_SURF%S_LOADP_KSEGS)
      CALL MY_ALLOC(SEGS_L,TH_SURF%S_LOADP_SEGS)
      KSEGS_L = 0
      SEGS_L = 0
      NSEGP  = 0      
      NSEGPL = 0
      ADCEP  = 0
      DO I = 1, NLOADP
        NL = ILOADP(1,I)
        DO J = 1, NL/4
          NSEGP = NSEGP + 1
          IF(CEP(J+OFF+ADCEP)==PROC) THEN
            NSEGPL = NSEGPL + 1
            NS = TH_SURF%LOADP_KSEGS (NSEGP+1) - TH_SURF%LOADP_KSEGS (NSEGP)
            KSEGS_L(NSEGPL+1) =KSEGS_L(NSEGPL)+ NS
            DO K=1,NS
               SEGS_L(KSEGS_L(NSEGPL)+K)= TH_SURF%LOADP_SEGS(TH_SURF%LOADP_KSEGS (NSEGP)+K)
            ENDDO
          ENDIF
        ENDDO
        ADCEP = ADCEP + NL/4
      ENDDO

      CALL WRITE_I_C(NSEGPL+1,1)
      CALL WRITE_I_C(KSEGS_L,NSEGPL+1)
      CALL WRITE_I_C(KSEGS_L(NSEGPL+1),1)
      CALL WRITE_I_C(SEGS_L,KSEGS_L(NSEGPL+1))
      LEN_IA = LEN_IA + NSEGPL+3+ KSEGS_L(NSEGPL+1)

      DEALLOCATE(KSEGS_L,SEGS_L)

      RETURN
      END
