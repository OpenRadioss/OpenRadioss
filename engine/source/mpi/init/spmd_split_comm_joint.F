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
Chd|  SPMD_SPLIT_COMM_JOINT         source/mpi/init/spmd_split_comm_joint.F
Chd|-- called by -----------
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|-- calls ---------------
Chd|        JOINT_MOD                     share/modules/joint_mod.F     
Chd|====================================================================
        SUBROUTINE SPMD_SPLIT_COMM_JOINT(  )
!$COMMENT
!       SPMD_SPLIT_COMM_JOINT :
!              SPMD_SPLIT_COMM_JOINT creates new communicators
!              for /CYL_JOINT
!       SPMD_SPLIT_COMM_JOINT organization :
!              - loop over the joints in order to
!                tag all the processors
!              - then create a communicator for a given 
!                joint
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE JOINT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI

        integer :: KEY,CODE,I,P
        INTEGER :: KK

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        P = ISPMD + 1
!           -----------------------------
!       loop over the interface        
        DO KK=1,NJOINT
!           -----------------------------
            !   color the processor
            IF(CYL_JOIN(KK)%NUMBER_NODE>0.OR.CYL_JOIN(KK)%NUMBER_MAIN_NODE>0) THEN
                        CYL_JOIN(KK)%COMM_MPI%COLOR=1
                        KEY = 1
            ELSE
                        CYL_JOIN(KK)%COMM_MPI%COLOR=0
                        KEY = 0
            ENDIF       
            !   create the communicator
            CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,CYL_JOIN(KK)%COMM_MPI%COLOR,P,CYL_JOIN(KK)%COMM_MPI%COMM,CODE)
            IF(CYL_JOIN(KK)%COMM_MPI%COLOR==1) THEN
                CALL MPI_COMM_RANK(CYL_JOIN(KK)%COMM_MPI%COMM,CYL_JOIN(KK)%COMM_MPI%RANK,CODE)
            ENDIF
        ENDDO
!           -----------------------------
#endif
        RETURN
        END SUBROUTINE SPMD_SPLIT_COMM_JOINT
