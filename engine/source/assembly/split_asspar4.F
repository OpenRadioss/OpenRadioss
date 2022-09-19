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
Chd|  SPLIT_ASSPAR4                 source/assembly/split_asspar4.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SPLIT_ASSPAR4(ADDCNE,NUMNOD,NTHREADS,FIRST,LAST,SADSKY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

!       -----------------------------------------------
!       Global arguments
        INTEGER, INTENT(IN) :: SADSKY,NUMNOD,NTHREADS
        INTEGER, DIMENSION(*), INTENT(IN) :: ADDCNE
        INTEGER, DIMENSION(NTHREADS) :: FIRST,LAST

!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       SADSKY : integer ; dimension of ADDCNE
!       NUMNOD : integer ; Number of node
!       NTHREADS : integer ; number of OpenMP threads
!       ADDCNE : integer ; dimension=NUMNOD+2 (0:NUMNOD+1)
!               address for assembling
!       FIRST : integer ; dimension=NTHREADS
!               address of the first node for each thread
!       LAST : integer ; dimension=NTHREADS
!               address of the last node for each thread
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       -----------------------------------------------
        INTEGER :: NC,SUM_NC,ITHREAD,N
        INTEGER, DIMENSION(NTHREADS) :: SUM_NC_THREAD
        my_real :: MEAN_NC
!       -----------------------------------------------

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
!$COMMENT
!       SPLIT_ASSPAR4 description
!       initialization of the bounds for each thread
!       a fixed split (ie. use NUMNOD/NTHREADS bounds
!       does not guarantee a optimized split in asspar4)
!
!       SPLIT_ASSPAR4 organization :
!       - compute the total number of contribution
!       - compute FIRST and LAST bounds according to
!         the nnumber of contribution
!$ENDCOMMENT

        !       FIRST and LAST initializations
        DO ITHREAD=1,NTHREADS
                FIRST(ITHREAD)= 1+(ITHREAD-1)*NUMNOD/NTHREADS
                LAST(ITHREAD)= ITHREAD*NUMNOD/NTHREADS
        ENDDO

        IF(SADSKY>0) THEN

                !       compute the total number of contribution + mean
                SUM_NC = 0
                DO N = 1,NUMNOD
                        NC = ADDCNE(N+1)-ADDCNE(N)
                        SUM_NC = SUM_NC + NC
                ENDDO

                MEAN_NC = SUM_NC / NTHREADS

                SUM_NC = 0
                IF(NUMNOD>0) THEN
                        !       first case : NUMNOD>NTHREADS --> try to split according to the number of contribution
                        IF(NUMNOD>NTHREADS) THEN
                                ITHREAD = 1
                                FIRST(ITHREAD) = 1
                                !       ------------------------
                                DO N = 1,NUMNOD
                                        NC = ADDCNE(N+1)-ADDCNE(N)
                                        SUM_NC = SUM_NC + NC
                                        IF(SUM_NC> 0.95*MEAN_NC.AND.(ITHREAD<NTHREADS+1) ) THEN
                                                SUM_NC_THREAD(ITHREAD) = SUM_NC
                                                SUM_NC = 0
                                                LAST(ITHREAD) = N
                                                ITHREAD = ITHREAD + 1
                                                IF(ITHREAD<NTHREADS+1) FIRST(ITHREAD) = N+1
                                        ENDIF
                                ENDDO
                                !       ------------------------
                                IF(ITHREAD<NTHREADS) THEN
                                        !       fail to split according to the nbr of contribution
                                        !       --> back to a classical splitting (static splitting)
                                        DO ITHREAD=1,NTHREADS
                                                FIRST(ITHREAD)= 1+(ITHREAD-1)*NUMNOD/NTHREADS
                                                LAST(ITHREAD)= ITHREAD*NUMNOD/NTHREADS
                                        ENDDO
                                ELSE
                                        !       splitting succes : force the last bound
                                        ITHREAD = NTHREADS
                                        LAST(ITHREAD) = NUMNOD
                                        SUM_NC_THREAD(ITHREAD) = SUM_NC
                                ENDIF
                                !       ------------------------
                        ELSE
                                !       ------------------------
                                !       second case : NUMNOD<NTHREADS --> classical splitting (static splitting)
                                DO ITHREAD=1,NTHREADS
                                        FIRST(ITHREAD)= 1+(ITHREAD-1)*NUMNOD/NTHREADS
                                        LAST(ITHREAD)= ITHREAD*NUMNOD/NTHREADS
                                ENDDO
                                !       ------------------------

                        ENDIF
                ENDIF
        ENDIF

        RETURN
        END SUBROUTINE SPLIT_ASSPAR4
