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
Chd|  ALEFVM_INIT                   source/ale/alefvm/alefvm_init.F
Chd|-- called by -----------
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_INIT()
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandoned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALEFVM_MOD , only:ALEFVM_Param
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE ptrARRAY
        INTEGER, POINTER :: ptr
      END TYPE
      
      TYPE(ptrARRAY),ALLOCATABLE,DIMENSION(:) :: FLAG
      
      INTEGER                                 :: NVAR, I 
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------      
C This subroutine initializes parameter for
C full FVM formulation. Especially output options.
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(ALEFVM_Param%IEnabled == 0)    RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      
      ALLOCATE(FLAG(32))
      NVAR = 0
      
      !----------------------------!
      !    ALL OUTPUTS             !
      !    0: all off              !
      !    1: all according below  !
      !----------------------------!      
      ALEFVM_Param%IOUTP        = 0
      !----------------------------!
      !  SPECIFIC OUTPUTS          !
      !    0 OFF                   !
      !   -1 all elem in group     !
      !   >0 only given user id    !
      !----------------------------!     
      ALEFVM_Param%IOUTP_GRAV   =  0     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_GRAV
      ALEFVM_Param%IOUTP_STRESS = -0     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_STRESS
      ALEFVM_Param%IOUTP_FINT   = -0     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_FINT
      ALEFVM_Param%IOUTP_FLUX   = -0     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_FLUX
      ALEFVM_Param%IOUTP_CONV   = -0     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_CONV
      ALEFVM_Param%IOUTP_EPSDOT =  0     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_EPSDOT
      ALEFVM_Param%IOUTP_SCHEME = -1     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_SCHEME
      ALEFVM_Param%IOUTP_BCS    =  0     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_BCS
      ALEFVM_Param%IOUTP_TFEXT  =  0     ; NVAR=NVAR+1; FLAG(NVAR)%ptr => ALEFVM_Param%IOUTP_TFEXT      

      DO I=1,NVAR
        FLAG(I)%ptr =FLAG(I)%ptr * ALEFVM_Param%IOUTP
      ENDDO
      
      !----------------------------!
      !  FVM FORMULATION           !
      !    IFORM=2 Mom. Weighted   !   
      !    IFORM=3 Roe-averaged    ! 
      !    IFORM=4 Centered        ! obsolete
      !    IFORM=5 Interpolated    ! obsolete
      !----------------------------!
      ALEFVM_Param%IFORM       = 0      ;  !Now set through control card /EULER/MAT or /ALE/MAT
      
      !----------------------------!
      !  FVM FORMULATION           !
      !    IFORM =0   +0*WFEXT     !
      !    IFORM =1   +1*WFEXT     ! 
      !----------------------------!
      ALEFVM_Param%IWFEXT       = 0      ;      
      !----------------------------!




C-----------------------------------------------
      RETURN
      END
