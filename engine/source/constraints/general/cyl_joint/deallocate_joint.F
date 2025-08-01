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
!||    deallocate_joint   ../engine/source/constraints/general/cyl_joint/deallocate_joint.F
!||--- called by ------------------------------------------------------
!||    resol              ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    joint_mod          ../engine/share/modules/joint_mod.F
!||====================================================================
        SUBROUTINE DEALLOCATE_JOINT()
!$COMMENT
!       DEALLOCATE_JOINT description
!       deallocation of the joint structure 
!       
!       DEALLOCATE_JOINT organization :
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: N
        IF(NJOINT>0) THEN
            !   ------------------------------        
            DO N=1,NJOINT     
                IF(CYL_JOIN(N)%NUMBER_NODE>0) THEN
                    IF(ALLOCATED(CYL_JOIN(N)%LIST_PROC) )DEALLOCATE( CYL_JOIN(N)%LIST_PROC )
                    IF( ALLOCATED(CYL_JOIN(N)%NODE) ) DEALLOCATE( CYL_JOIN(N)%NODE )
                    IF( ALLOCATED(CYL_JOIN(N)%WEIGHT) ) DEALLOCATE( CYL_JOIN(N)%WEIGHT )
                ENDIF
                IF(ALLOCATED(CYL_JOIN(N)%MAIN_NODE) )DEALLOCATE( CYL_JOIN(N)%MAIN_NODE )
                IF(ALLOCATED(CYL_JOIN(N)%COMM_MPI%SIZE_SR) )DEALLOCATE( CYL_JOIN(N)%COMM_MPI%SIZE_SR )
            ENDDO
            !   ------------------------------
        ENDIF
        IF(ALLOCATED(CYL_JOIN) )DEALLOCATE( CYL_JOIN )
C
        RETURN
        END SUBROUTINE DEALLOCATE_JOINT
