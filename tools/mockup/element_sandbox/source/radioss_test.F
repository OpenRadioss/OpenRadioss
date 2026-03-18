Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!       ------------------------------------
       PROGRAM RADIOSS_TEST

        USE OMP_LIB
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "comlock.inc"
#include      "com01_c.inc"       
!       ------------------------------------
        INTEGER ::  NBR_GROUP,TT
        INTEGER, DIMENSION(:), ALLOCATABLE :: NGL
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: IGROUP
        real(kind=8) :: T1,T2
        my_real, DIMENSION(:), ALLOCATABLE :: EINT,OFF,SIGY,DT1C,VOL0
        my_real, DIMENSION(:), ALLOCATABLE :: GS,DPLA,PLA,SIGO,SIGN,DEP,FOR
        my_real, DIMENSION(:,:), ALLOCATABLE :: PM,A
!       ------------------------------------
        INTEGER :: I,NG,NTHREAD,THREAD_ID,IT
       INTEGER :: FIRST,LAST
       my_real :: FAKE_VARIABLE
!       ------------------------------------

!$OMP PARALLEL
        NTHREAD = OMP_GET_NUM_THREADS()
!$OMP END PARALLEL

        DO I = 1, 2*INTSEG
          CALL OMP_INIT_LOCK(LLOCK(1,I))
        ENDDO

        ZERO=0.d+00
        UN = 1.d+00
        DEUX=2.d+00
        TROIS=3.d+00
        QUATRE=4.d+00
        CINQ=5.d+00
        QCINQ=UN/CINQ
        QUART=UN/QUATRE
        TQUART=TROIS*QUART
        UNDEMI=UN/DEUX
        EM20=10.d-20
        EM01=10.d-01
        EM15=10.d-15
        EM07=10.d-07
       PMSIZE = 25

!       -------------------------------
        !       Number of group
        NBR_GROUP = 4000
        !       Number of element
        NUMNOD = NBR_GROUP*(MVSIZ-1)
!       -------------------------------
        print*,' ----------------------------------- '
        print*,' Number of OpenMP threads:',NTHREAD
        print*,' Total number of element:',NUMNOD
        print*,' Total number of element group:',NBR_GROUP
        print*,' ----------------------------------- '


        ALLOCATE( IGROUP(11,NBR_GROUP) )
        ALLOCATE( NGL(NUMNOD) )

        CALL INIT(NBR_GROUP,IGROUP,NUMNOD,NGL)

        ALLOCATE( PM(25,NBR_GROUP) )
        ALLOCATE( EINT( 2*NUMNOD ) )
        ALLOCATE( OFF( NUMNOD ) )
        ALLOCATE( SIGY( NUMNOD ) )
        ALLOCATE( DT1C( NUMNOD ) )
        ALLOCATE( VOL0( NUMNOD ) )
        ALLOCATE( GS( NUMNOD ) )
        ALLOCATE( DPLA( NUMNOD ) )
        ALLOCATE( PLA( NUMNOD ) )
        ALLOCATE( SIGO( 5*NUMNOD ) )
        ALLOCATE( SIGN( 5*NUMNOD ) )
        ALLOCATE( DEP( 5*NUMNOD ) )
        ALLOCATE( FOR( 5*NUMNOD ) )
        ALLOCATE( A(5,NUMNOD) )

        CALL INIT_DATA(NBR_GROUP,PM,EINT,OFF,SIGY,DT1C,VOL0,GS,DPLA,PLA,FOR,SIGO,SIGN,DEP,A)
!$OMP PARALLEL
!        CALL CPU_TIME(T1)
       t1 = omp_get_wtime( )
!$OMP END PARALLEL



        DO TT=1,1000

!$OMP PARALLEL PRIVATE(THREAD_ID)
                THREAD_ID = OMP_GET_THREAD_NUM()
                CALL GROUP_COMPUTATION(NBR_GROUP,IGROUP,PM,EINT,
     .                     OFF,SIGY,DT1C,VOL0,GS,DPLA,PLA,FOR,SIGO,SIGN,DEP,NGL,THREAD_ID)

!$OMP END PARALLEL
              

!$OMP PARALLEL PRIVATE(THREAD_ID,FIRST,LAST)
                THREAD_ID = OMP_GET_THREAD_NUM()
              !       acceleration update
              FIRST = 1+THREAD_ID * NUMNOD / NTHREAD
              LAST = (THREAD_ID+1) * NUMNOD / NTHREAD
              CALL UPDATE_ACC(FIRST, LAST, A, FOR )
!$OMP END PARALLEL
        ENDDO

!$OMP PARALLEL
!        CALL CPU_TIME(T2)
       t2 = omp_get_wtime( )
!$OMP END PARALLEL

       FAKE_VARIABLE = ZERO
       DO I=1,NUMNOD
              FAKE_VARIABLE = FAKE_VARIABLE + A(1,I) + A(2,I) + A(3,I)
       ENDDO
       PRINT*,' '
       PRINT*,' ----------------------------------- '     
       PRINT*,' time =',T2-T1
       PRINT*,' fake variable =',FAKE_VARIABLE
       PRINT*,' ----------------------------------- '


        DEALLOCATE( IGROUP )
        DEALLOCATE( NGL )
        DEALLOCATE( EINT )
        DEALLOCATE( OFF )
        DEALLOCATE( SIGY )
        DEALLOCATE( DT1C )
        DEALLOCATE( VOL0 )
        DEALLOCATE( GS )
        DEALLOCATE( DPLA )
        DEALLOCATE( PLA )
        DEALLOCATE( SIGO )
        DEALLOCATE( SIGN )
        DEALLOCATE( DEP )
       DEALLOCATE( A )
        


!       ------------------------------------
       END PROGRAM RADIOSS_TEST
!       ------------------------------------
