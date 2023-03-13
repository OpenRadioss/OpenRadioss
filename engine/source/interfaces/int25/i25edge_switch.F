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
Chd|  I25EDGE_SWITCH                source/interfaces/int25/i25edge_switch.F
Chd|-- called by -----------
Chd|        I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE I25EDGE_SWITCH(
     .                    JTASK,
     .                    STIFM,
     .                    STFE,
     .                    MVOISIN,
     .                    IEDGE,
     .                    NEDGE,
     .                    LEDGE
     .                    )
C-----------------------------------------------
C  In case of element deletion
C  LEDGE must be modified to keep the remaining
C  segment at the first position
C  If both element are deleted, STFE is set to negative 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "assert.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JTASK
      INTEGER, INTENT(IN) :: IEDGE, NEDGE
      INTEGER, INTENT(IN) :: MVOISIN(4,*)
      INTEGER, INTENT(INOUT) :: LEDGE(NLEDGE,*)
C     REAL
      my_real, INTENT(IN) :: STIFM(*)
      my_real, INTENT(INOUT) :: STFE(NEDGE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NEDG,NEDGFT,NEDGLT
      INTEGER :: IRM,IEDG,JRM,JEDG 
      my_real :: S1,S2
      IF(IEDGE/=0)THEN
C
        NEDGFT= 1+(JTASK-1)*NEDGE/ NTHREAD
        NEDGLT= JTASK*NEDGE/NTHREAD
        DO NEDG=NEDGFT,NEDGLT
          IRM =LEDGE(LEDGE_LEFT_SEG,NEDG)
          IEDG=LEDGE(LEDGE_LEFT_ID,NEDG)
          JRM =LEDGE(LEDGE_RIGHT_SEG,NEDG)
          JEDG=LEDGE(LEDGE_RIGHT_ID,NEDG)
          IF(JRM/=0)THEN

            IF(IRM > 0 .AND. JRM > 0) THEN
C not free, both segments are local to the SPMD domain
              S1 = STIFM(IRM)
              S2 = STIFM(JRM)
            ELSEIF(IRM < 0 .AND. JRM > 0) THEN
C if IRM is negative, then JRM must be zero
C because split_interface
              ASSERT(.FALSE.)
C edge at the boundary of spmd domains
C secnd side treated by the other domain 
              S1 = ONE ! exact stiffness not known, but it is nnz
C MVOISIN = 0 <=> STFM( VOISIN )) = 0
              IF(MVOISIN(JEDG,JRM) == 0 ) S1 = ZERO
              S2 = STIFM(JRM)
            ELSEIF(IRM > 0 .AND. JRM < 0) THEN
C edge at the boundary of SPMD domains
C secnd side treated by ISPMD
              S1 = STIFM(IRM)
!exact stiffness on the other side of the boundary is not known yet
! But we can determine if it is zero or not according to MVOISIN
              S2 = ONE 
              IF(MVOISIN(IEDG,IRM) == 0) S2 = ZERO
            ELSE
C not supposed to be there
              ASSERT(.FALSE.)
            ENDIF

            IF(S1 == ZERO)THEN
C first (left) segment is broken
C new left <= old right
C old right <= 0 
C in order to always have the unbroken segment in the first place
C            DEBUG_E2E(LEDGE(LEDGE_GLOBAL_ID,NEDG) == D_EM,S1)
C            DEBUG_E2E(LEDGE(LEDGE_GLOBAL_ID,NEDG) == D_ES,S1)

CC ???????????????????????????????????????????????
              IF(JRM > 0) THEN
                LEDGE(LEDGE_LEFT_SEG,NEDG) = JRM
                LEDGE(LEDGE_LEFT_ID,NEDG) = JEDG
              ELSEIF (JRM < 0)  THEN
                LEDGE(LEDGE_LEFT_SEG,NEDG) = -ABS(IRM) !1
C               LEDGE(LEDGE_LEFT_SEG,NEDG) = JEDG      !2
               ! LEDGE(LEDGE_LEFT_ID,NEDG) unchanged
              ENDIF
C             tag for comm in spmd_getstif25_edg
              IF(LEDGE(LEDGE_WEIGHT,NEDG) == 1) THEN
                LEDGE(LEDGE_GLOBAL_ID,NEDG) = -ABS(LEDGE(LEDGE_GLOBAL_ID,NEDG))
              ENDIF
CC ???????????????????????????????????????????????

              LEDGE(LEDGE_SEG1_IM,NEDG) = LEDGE(LEDGE_SEG2_IM,NEDG)
              LEDGE(LEDGE_SEG1_I1,NEDG) = LEDGE(LEDGE_SEG2_I1,NEDG)
              LEDGE(LEDGE_SEG1_I2,NEDG) = LEDGE(LEDGE_SEG2_I2,NEDG)
              LEDGE(LEDGE_RIGHT_SEG,NEDG) = 0

              IF(JRM >= 0) THEN
                LEDGE(LEDGE_RIGHT_ID,NEDG) = 0
              ELSE

              ENDIF

            ELSEIF(S2 == ZERO)THEN
C left segment has broken
C             DEBUG_E2E(LEDGE(LEDGE_GLOBAL_ID,NEDG) == D_EM,S2)
C             DEBUG_E2E(LEDGE(LEDGE_GLOBAL_ID,NEDG) == D_ES,S2)
              LEDGE(LEDGE_RIGHT_SEG,NEDG) = 0
              LEDGE(LEDGE_RIGHT_ID,NEDG) = 0

C             tag for comm in spmd_getstif25_edg
              IF(LEDGE(LEDGE_WEIGHT,NEDG) == 1) THEN
                LEDGE(LEDGE_GLOBAL_ID,NEDG) = -ABS(LEDGE(LEDGE_GLOBAL_ID,NEDG))
              ENDIF
 

            END IF          
          END IF
        END DO

C=======================================================================
        DO NEDG = NEDGFT,NEDGLT
          IRM = LEDGE(LEDGE_LEFT_SEG,NEDG)
          JRM = LEDGE(LEDGE_RIGHT_SEG,NEDG)
C         DEBUG_E2E(LEDGE(LEDGE_GLOBAL_ID,NEDG) == D_ES,STFE(NEDG))

          IF(IRM > 0) THEN
C on peut passer ici: cas d edge de bord
C dont le irect casse (o ne passe pas dans le switch (1,2) <- (3,4) au dessus
C           DEBUG_E2E(LEDGE(LEDGE_GLOBAL_ID,NEDG) == D_ES,STIFM(IRM))
            IF(STIFM(IRM)==ZERO)THEN
              IF(STFE(NEDG) > ZERO) THEN
                 STFE(NEDG) = -STFE(NEDG)
              ENDIF
            ENDIF
          ELSE IF(IRM < 0) THEN
! Edge frontiere 
! The only way that IRM < 0  is when
C   -  the a boundary edge owned by ISPMD
!   -  the local segment is broken

C 1      -1 IRM 
C 1       1 IEDG
C -1      0 JRM         
C 1       0 JEDG 
            S1 = ONE
            IEDG=LEDGE(LEDGE_LEFT_ID ,NEDG)
            IF(JRM ==  0) THEN
              IF(MVOISIN(IEDG,ABS(IRM))==0) S1 = ZERO
C  When IRM < 0 and JRM == 0
C  STFE is 0 if the other side is broken too 
C Because we always put the remaining segment first (IRM,IEDG) 
C we need to check
C           ELSE IF( ) THEN
C             S1 = 0
            ELSE
              ASSERT(.FALSE.)
            ENDIF
           
            IF(S1==ZERO)THEN
              IF(STFE(NEDG) > ZERO) THEN
                 STFE(NEDG) = -STFE(NEDG)
              ENDIF
            ENDIF

          ELSEIF (IRM == 0) THEN
            ASSERT(.FALSE.) ! 
            IF(STFE(NEDG) > ZERO) THEN
               STFE(NEDG) = -STFE(NEDG)
            END IF          
          END IF
        END DO

        CALL MY_BARRIER
      END IF ! IEDGE
      RETURN
      END
