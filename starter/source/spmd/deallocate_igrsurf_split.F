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
!> \brief This routine deallocates the local IGSURF_PROC arrays
!!
!! \details loop over the NVOLU airbaig and NSURF surfaces to deallote the structure
!!
!||====================================================================
!||    deallocate_igrsurf_split   ../starter/source/spmd/deallocate_igrsurf_split.F
!||--- called by ------------------------------------------------------
!||    lectur                     ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||    monvol_struct_mod          ../starter/share/modules1/monvol_struct_mod.F
!||====================================================================
        SUBROUTINE DEALLOCATE_IGRSURF_SPLIT(T_MONVOL,IGRSURF_PROC)
!$COMMENT
!       DEALLOCATE_IGRSURF_SPLIT description
!       DEALLOCATE_IGRSURF_SPLIT deallocates the local IGSURF_PROC arrays
!       
!       DEALLOCATE_IGRSURF_SPLIT organization :
!       -loop over NVOLU in order to deallocate the array
!$ENDCOMMENT
        
        USE GROUPDEF_MOD
        USE MONVOL_STRUCT_MOD
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
        TYPE(SURF_), DIMENSION(NSURF,NSPMD), INTENT(INOUT) :: IGRSURF_PROC
        TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL

!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       IGRSURF_PROC : SURF_ ; dimension=NSURF*NSPMD
!                 local surface property array (=IGRSURF for each proc)
!                 %ELTYP --> type of element (shell, triangle...)
!                 %ELEM  --> element id
!                 %NSEG --> total element number
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NV,IS
        INTEGER :: PROC,K1
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
        ! --------------------------------------
        ! Airbag
        K1 = 1
        DO NV=1,NVOLU                   !       NVOLU = number of volume
                IS = T_MONVOL(NV)%EXT_SURFID  
                DO PROC=1,NSPMD
                        !       several MONVOL can refer to the same surface ID
                        IF(IGRSURF_PROC(IS,PROC)%NSEG>0.AND.ALLOCATED(IGRSURF_PROC(IS,PROC)%ELTYP) ) THEN
                                DEALLOCATE( IGRSURF_PROC(IS,PROC)%ELTYP ) 
                                DEALLOCATE( IGRSURF_PROC(IS,PROC)%ELEM ) 
                        ENDIF
                ENDDO
                K1 = K1 + NIMV
                ! --------------------
                ! deallocation of NUMBER_TRI_PER_PROC :
                 DEALLOCATE( T_MONVOL(NV)%NUMBER_TRI_PER_PROC )
                ! --------------------
        ENDDO   
        ! --------------------------------------  


        ! --------------------------------------    
        ! Surface 
        DO IS=1,NSURF
            DO PROC=1,NSPMD
                IF(ALLOCATED( IGRSURF_PROC(IS,PROC)%LOCAL_SEG )) DEALLOCATE( IGRSURF_PROC(IS,PROC)%LOCAL_SEG )
                IF(ALLOCATED( IGRSURF_PROC(IS,PROC)%ELTYP ))DEALLOCATE( IGRSURF_PROC(IS,PROC)%ELTYP ) 
                IF(ALLOCATED( IGRSURF_PROC(IS,PROC)%ELEM ))DEALLOCATE( IGRSURF_PROC(IS,PROC)%ELEM ) 
            ENDDO
        ENDDO
        ! --------------------------------------
        RETURN
        END SUBROUTINE DEALLOCATE_IGRSURF_SPLIT
