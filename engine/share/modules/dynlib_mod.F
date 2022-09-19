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
Chd|  DYNLIB_MOD                    share/modules/dynlib_mod.F    
Chd|-- called by -----------
Chd|        ARRET                         source/system/arret.F         
Chd|        RADIOSS0                      source/engine/radioss0.F      
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      MODULE DYNLIB_MOD
!$COMMENT
!       DYNLIB_MOD :
!           * define a structure DLIB_STRUCT in order to manage the user routine calls :
!               * up to 100 user routines (--> NBR_DLIB variable)
!               * DLIB_NAME_LEN : maximal user routine name lenght
!               * DLIB_NAME : name of the user routine
!               * DLIB_BOOL : boolean : true = user routine available
!           * initialize the DLIB_STRUCT structure
!               * pointer to the DLIB_STRUCT : ID_name_of_the_user_routine
!               * add the name of the user routine (%DLIB_NAME)
!               * the DLIB_BOOL boolean is initialized with the array DLIB_ARRAY :
!                               - DLIB_ARRAY is initialized in dyn_userlib.c
!
!           For a new user routine :
!              o create the pointer to DLIB_STRUCT struct : add a new ID_name_of_the_user_routine
!              o add the name of the user routine (%DLIB_NAME)
!              o manage the DLIB_ARRAY in dyn_userlib.c
!
!           DLIB_STRUCT organization :
!               (1) --> ENGINE_USER_CHECK
!               (2) --> ENGINE_USER_FINALIZE
!               (3) --> ENGINE_USER_INITIALIZE
!               (4:100) --> empty, not used
!$ENDCOMMENT
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
        INTEGER, PARAMETER :: NBR_DLIB=100
        INTEGER, PARAMETER :: DLIB_NAME_LEN=100
        INTEGER, DIMENSION(NBR_DLIB) :: DLIB_ARRAY

        TYPE DYN_LIB_TYPE
            CHARACTER(len=DLIB_NAME_LEN) :: DLIB_NAME
            LOGICAL :: DLIB_BOOL
        END TYPE DYN_LIB_TYPE

        TYPE(DYN_LIB_TYPE),DIMENSION(:), ALLOCATABLE :: DLIB_STRUCT

        !   local user variables
        INTEGER :: ISPMD_USER   !   --> ispmd : proc ID
        INTEGER :: NSPMD_USER   !   --> nspmd : total number of mpi tasks
        INTEGER :: NTASK_USER   !   --> ntask : total number of openmp tasks
        INTEGER :: MSTOP_USER    !   --> mstop : 0 = ok ; 1 = stop the run
        INTEGER :: NCYCLE_USER  !   --> ncycle : number of cycle
        REAL(kind=8) :: TSTOP_USER   !   --> tstop : stop time
        REAL(kind=8) :: TT_USER      !   --> tt : current time

        !   pointer to the DLIB_STRUCT : ID_name_of_the_user_routine
        INTEGER, PARAMETER :: ID_ENGINE_USER_CHECK=1
        INTEGER, PARAMETER :: ID_ENGINE_USER_FINALIZE = 2
        INTEGER, PARAMETER :: ID_ENGINE_USER_INITIALIZE = 3

        CONTAINS
!       -----------------------------------------------
Chd|====================================================================
Chd|  INIT_DYN_LIB_STRUC            share/modules/dynlib_mod.F    
Chd|-- called by -----------
Chd|        RADIOSS0                      source/engine/radioss0.F      
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE INIT_DYN_LIB_STRUC(MODE)
!$COMMENT
!           INIT_DYN_LIB_STRUC description
!           INIT_DYN_LIB_STRUC initializes the name of user routines and
!           the DLIB_BOOL flag available / non-available
!$ENDCOMMENT
!       -----------------------------------------------
!           I m p l i c i t   T y p e s
!       -----------------------------------------------
#include      "implicit_f.inc"
        INTEGER :: I,MODE
!       -----------------------------------------------
        IF(MODE==0) THEN
            DO I=1,NBR_DLIB
                DLIB_STRUCT(I)%DLIB_NAME(1:DLIB_NAME_LEN) = ''
                DLIB_STRUCT(I)%DLIB_BOOL=.FALSE.
            ENDDO

        ELSE
            DO I=1,NBR_DLIB
                IF(DLIB_ARRAY(I)/=0) DLIB_STRUCT(I)%DLIB_BOOL=.TRUE.
            ENDDO

            DLIB_STRUCT(ID_ENGINE_USER_CHECK)%DLIB_NAME(1:17) = "ENGINE_USER_CHECK"
            DLIB_STRUCT(ID_ENGINE_USER_FINALIZE)%DLIB_NAME(1:20) = "ENGINE_USER_FINALIZE"
            DLIB_STRUCT(ID_ENGINE_USER_INITIALIZE)%DLIB_NAME(1:22) = "ENGINE_USER_INITIALIZE"
        ENDIF

        RETURN
        END SUBROUTINE INIT_DYN_LIB_STRUC


      END MODULE DYNLIB_MOD
