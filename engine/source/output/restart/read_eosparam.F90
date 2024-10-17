!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      SUBROUTINE READ_EOSPARAM(EOS)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE MESSAGE_MOD
      USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
      USE NAMES_AND_TITLES_MOD
      USE MY_ALLOC_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      TYPE(EOS_PARAM_) ,INTENT(INOUT) :: EOS
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: I,NUPARAM,NIPARAM,NUMTABL,IAD,LEN,NUMFUNC
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(1) :: ILEN
      INTEGER ,DIMENSION(:), ALLOCATABLE :: IBUF
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      ! read eos model parameters
      CALL READ_I_C(ILEN, 1)
      LEN = ILEN(1)
      ALLOCATE (IBUF(LEN) )
      CALL READ_I_C(IBUF, LEN)
      
      IAD = 0
      IAD = IAD+1 ; EOS%NUPARAM = IBUF(IAD)
      IAD = IAD+1 ; EOS%NIPARAM = IBUF(IAD)
      IAD = IAD+1 ; EOS%NUVAR   = IBUF(IAD)
      IAD = IAD+1 ; EOS%NFUNC   = IBUF(IAD)
      IAD = IAD+1 ; EOS%NTABLE  = IBUF(IAD)

      DEALLOCATE( IBUF )
      
      ! read material title                      
      CALL READ_C_C(NAME,NCHARTITLE)             
      DO I=1,NCHARTITLE                          
        EOS%TITLE(I:I) = CHAR(NAME(I))           
      END DO                                     
      
      ! read eos parameter arrays          
      NUPARAM = EOS%NUPARAM                      
      NIPARAM = EOS%NIPARAM                      
      CALL MY_ALLOC(EOS%UPARAM ,NUPARAM)         
      CALL MY_ALLOC(EOS%IPARAM ,NIPARAM)         

      IF (NUPARAM > 0) THEN                      
        CALL READ_DB(EOS%UPARAM  ,NUPARAM)       
      END IF                                     
      IF (NIPARAM > 0) THEN                      
        CALL READ_I_C(EOS%IPARAM ,NIPARAM)       
      END IF                                     

      ! read eos law function                    
      NUMFUNC  = EOS%NFUNC                       
      IF (NUMFUNC > 0) THEN                      
        ALLOCATE (EOS%FUNC(NUMFUNC))             
        CALL READ_I_C(EOS%FUNC, NUMFUNC)         
      END IF                                     

      ! read eos law tables                      
      NUMTABL  = EOS%NTABLE                      
      IF (NUMTABL > 0) THEN                      
        ALLOCATE (EOS%TABLE(NUMTABL))            
        CALL READ_MAT_TABLE(EOS%TABLE, NUMTABL)  
      END IF                                     
        

!-----------
      RETURN
      END
