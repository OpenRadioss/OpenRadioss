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
Chd|  WRITE_TH                      source/output/th/write_th.F   
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        TH_MOD                        share/modules/th_mod.F        
Chd|====================================================================
        SUBROUTINE WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      ELTYPE_STRUCT,WA_ELTYPE_P0)
        USE TH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: N,NSPMD,NN,NVAR,ITTYP
        TYPE(TH_WA_REAL), INTENT(in) ::WA_ELTYPE_P0
        TYPE(TH_PROC_TYPE), INTENT(in) :: ELTYPE_STRUCT
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       N : integer ; current TH group 
!       NSPMD : integer ; number of MPI domains
!       NN : integer, number of element group
!       NVAR : integer ; number of value per element group
!       ITTYP : integer ; type of TH group
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: BOOL
        INTEGER :: I,J,K,II,IJK,LOCAL_SIZE
        INTEGER :: NEXT,CURRENT,SIZE_N
        INTEGER :: CURRENT_J,REST
        my_real, DIMENSION(:), ALLOCATABLE :: WA_LOCAL
!       ----------------------------------------
!$COMMENT
!       WRITE_TH_COQ description
!       write all the values for shell element
!       and for a given group N by the PROC0
!       
!       WRITE_TH_COQ organization :
!        * loop over the NSPMD processor and:
!           - check if a processor must write its values (SIZE_N>0)
!           - compute the number of group for a given processor (REST)
!           - compute the position in the TH file (CURRENT_J)
!           - initialization of the writen values WA_LOCAL with the position CURRENT_J + J
!        * write the WA_LOCAL values 
!$ENDCOMMENT

!       allocation + initialization of local array
        ALLOCATE( WA_LOCAL(NN*NVAR) )
        WA_LOCAL(1:NN*NVAR) = ZERO



        DO I=1,NSPMD
            LOCAL_SIZE = ELTYPE_STRUCT%TH_PROC(I)%TH_ELM_SIZE
            BOOL=.TRUE.
            DO K=1,LOCAL_SIZE
                IF(BOOL.EQV..TRUE.) THEN
                    IF(ELTYPE_STRUCT%TH_PROC(I)%TH_ELM(K,2)==N) THEN
                        BOOL=.FALSE.
                        IJK=K
                    ENDIF
                ENDIF
            ENDDO
            IF(BOOL.EQV..FALSE.) THEN
                CURRENT = ELTYPE_STRUCT%TH_PROC(I)%TH_ELM(IJK,1) !   index of the current proc and N
                NEXT = ELTYPE_STRUCT%TH_PROC(I)%TH_ELM(IJK+1,1)  !   index of the next proc and N
                SIZE_N = NEXT-CURRENT   !   nbr of WA element of the current proc and for the current N
!                IF( SIZE_N>0 ) THEN     !   if SIZE_N>0, must write some data
                REST = SIZE_N / (NVAR+1)    ! nbr of th group for the current proc
                II = 0
                DO K=1,REST
                    CURRENT_J = NINT(WA_ELTYPE_P0%WA_REAL( CURRENT+K*(NVAR+1) ) ) ! the position in WA_LOCAL for the current TH 
                    DO J=1,NVAR   
                        II = II + 1   
                        WA_LOCAL(CURRENT_J+J) = WA_ELTYPE_P0%WA_REAL(CURRENT+II)                
                    ENDDO
                    II = II + 1  ! skip the NVAR+1 value = the position in WA_LOCAL
                ENDDO
!                ENDIF
            ENDIF
        ENDDO

!       write the data
        CALL WRTDES(WA_LOCAL,WA_LOCAL,NN*NVAR,ITTYP,1)
!       deallocation
        DEALLOCATE( WA_LOCAL )

!       ----------------------------------------
        RETURN
        END SUBROUTINE WRITE_TH
!       ----------------------------------------
