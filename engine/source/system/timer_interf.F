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
Chd|  OPEN_INTERF_TIME              source/system/timer_interf.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|====================================================================
      SUBROUTINE OPEN_INTERF_TIME(ISPMD)
      USE METRIC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
#include    "chara_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: ISPMD
C-----------------------------------------------
      INTEGER I, J
      CHARACTER FILNAM*100
      CHARACTER PROCNAM*4
C-----------------------------------------------
       WRITE(PROCNAM,'(I4.4)')ISPMD+1
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//PROCNAM//'.int'
       OPEN(unit=4999,file=FILNAM(1:ROOTLEN+15),action='write',
     .          ACCESS='SEQUENTIAL',
     .         FORM='FORMATTED',STATUS='UNKNOWN')
      END SUBROUTINE

Chd|====================================================================
Chd|  CLOSE_INTERF_TIME             source/system/timer_interf.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|====================================================================
      SUBROUTINE CLOSE_INTERF_TIME()
      USE METRIC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I, J
      CHARACTER FILNAM*100
C-----------------------------------------------
      CLOSE(unit=FILE_ID)
      END SUBROUTINE

Chd|====================================================================
Chd|  INT_STARTIME                  source/system/timer_interf.F  
Chd|-- called by -----------
Chd|        INTER_SORT                    source/interfaces/generic/inter_sort.F
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|====================================================================
      SUBROUTINE INT_STARTIME(THIS,EVENT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE METRIC_MOD
C     routine d'incrementation du timer event
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(METRIC_STRUCT_) :: THIS
      INTEGER :: EVENT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      DOUBLE PRECISION :: tic

#ifdef MPI
      tic = MPI_WTIME()
      THIS%TIME(EVENT,2)=tic
#else
      THIS%TIME(EVENT,2)=0.0D0
#endif
      RETURN
      END SUBROUTINE
Chd|====================================================================
Chd|  INT_STOPTIME                  source/system/timer_interf.F  
Chd|-- called by -----------
Chd|        INTER_SORT                    source/interfaces/generic/inter_sort.F
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|====================================================================
      SUBROUTINE INT_STOPTIME(THIS,EVENT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE METRIC_MOD
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(METRIC_STRUCT_) :: THIS
      INTEGER :: EVENT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      DOUBLE PRECISION :: toc
C
#ifdef MPI
      toc = MPI_WTIME()
      THIS%TIME(EVENT,1)=THIS%TIME(EVENT,1) + (toc - THIS%TIME(EVENT,2))
#else
      toc = 0.0D0
      THIS%TIME(EVENT,1)=THIS%TIME(EVENT,1) + (toc - THIS%TIME(EVENT,2))
#endif
      RETURN
      END SUBROUTINE
Chd|====================================================================
Chd|  PRINTIME_INTERF               source/system/timer_interf.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_DGATHER                  source/mpi/output/spmd_gather.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|====================================================================
      SUBROUTINE PRINTIME_INTERF(INTBUF_TAB,IPARI,INTLIST,NBINTC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE METRIC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include        "units_c.inc"
#include      "task_c.inc"
#include      "timerr_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NBINTC
      INTEGER, INTENT(IN) :: INTLIST(NBINTC),IPARI(NPARI,NINTER)
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(IN) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SENDBUF
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RECVBUF
      INTEGER ::  I,J,N,P
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: RATIO
      DOUBLE PRECISION,DIMENSION(:,:,:), ALLOCATABLE :: SUMMARY
      DOUBLE PRECISION :: TRESHOLD,PMAX,TOTAL_TIME
      INTEGER :: NB_PRINTED_INTERF


      TRESHOLD = 0.01D0 * REALTIME(1,1)

      IF(NSPMD > 0) THEN
        ALLOCATE(SENDBUF(SIZE_METRIC * NBINTC))
        SENDBUF(1:SIZE_METRIC*NBINTC) = 0
        IF(ISPMD == 0) THEN
          ALLOCATE(RECVBUF(SIZE_METRIC*NBINTC,NSPMD))
          ALLOCATE(RATIO(NBINTC))
          ALLOCATE(SUMMARY(SIZE_METRIC,3,NBINTC))
        ELSE
          ALLOCATE(RECVBUF(1,1))
          ALLOCATE(RATIO(1))
          ALLOCATE(SUMMARY(1,1,1))
        ENDIF

        DO I = 1,NBINTC
          N = INTLIST(I)
          J = (I-1) * SIZE_METRIC
          SENDBUF(J + I_MAIN_CRIT_TRI)=INTBUF_TAB(N)%METRIC%TIME(I_MAIN_CRIT_TRI,1)
          SENDBUF(J + I_MAIN_TRI     )=INTBUF_TAB(N)%METRIC%TIME(I_MAIN_TRI,1)
          SENDBUF(J + I_MAIN_OPT_TRI )=INTBUF_TAB(N)%METRIC%TIME(I_MAIN_OPT_TRI,1)
          SENDBUF(J + I_MAIN_FORCES  )=INTBUF_TAB(N)%METRIC%TIME(I_MAIN_FORCES,1)
          SENDBUF(J + I_NOINT        )=DBLE(INTBUF_TAB(N)%METRIC%NOINT   )
          SENDBUF(J + I_MULTIMP      )=DBLE(INTBUF_TAB(N)%METRIC%MULTIMP )
          SENDBUF(J + I_NSN          )=DBLE(INTBUF_TAB(N)%METRIC%NSN     )
          SENDBUF(J + I_NSNR         )=DBLE(INTBUF_TAB(N)%METRIC%NSNR    )
          SENDBUF(J + I_NCONT        )=DBLE(INTBUF_TAB(N)%METRIC%NCONT   )
        ENDDO
        CALL SPMD_DGATHER(SENDBUF,SIZE_METRIC*NBINTC,RECVBUF,0)

C=================== PRINT ON PROC 0        
        IF( ISPMD == 0) THEN

          NB_PRINTED_INTERF = 0
          RATIO(1:NBINTC) = 0.0D0
          SUMMARY(1:SIZE_METRIC,1:3,1:NBINTC) = 0.0D0
          DO I = 1,NBINTC
            PMAX = 0.0D0
            DO P = 1,NSPMD
              J = (I-1) * SIZE_METRIC

              RATIO(I) =  MAX(RATIO(I),
     .                    RECVBUF(J + I_MAIN_CRIT_TRI,P)
     .                 +  RECVBUF(J + I_MAIN_TRI     ,P)
     .                 +  RECVBUF(J + I_MAIN_OPT_TRI ,P)
     .                 +  RECVBUF(J + I_MAIN_FORCES ,P))

C ===========  SUM OVER PROCESSORS
              SUMMARY(I_MAIN_OPT_TRI,1,I) = SUMMARY(I_MAIN_OPT_TRI,1,I) + RECVBUF(J + I_MAIN_OPT_TRI,P)
              SUMMARY(I_MAIN_CRIT_TRI,1,I)= SUMMARY(I_MAIN_CRIT_TRI,1,I)+ RECVBUF(J + I_MAIN_CRIT_TRI,P)
              SUMMARY(I_MAIN_TRI,1,I)     = SUMMARY(I_MAIN_TRI,1,I)     + RECVBUF(J + I_MAIN_TRI,P)
              SUMMARY(I_MAIN_FORCES,1,I)     = SUMMARY(I_MAIN_FORCES,1,I)     + RECVBUF(J + I_MAIN_FORCES,P)

              SUMMARY(I_NSN,1,I) =   SUMMARY(I_NSN,1,I)     + RECVBUF(J + I_NSN,P)
              SUMMARY(I_NSNR,1,I) =  SUMMARY(I_NSNR,1,I)     + RECVBUF(J + I_NSNR,P)


C ===========  MAX OVER PROCESSORS
              TOTAL_TIME = RECVBUF(I_MAIN_OPT_TRI+J,P)
     .                   + RECVBUF(I_MAIN_CRIT_TRI+J,P)
     .                   + RECVBUF(I_MAIN_TRI+J,P)
     .                   + RECVBUF(I_MAIN_FORCES+J,P)
              IF(TOTAL_TIME  > PMAX) THEN
                SUMMARY(I_MAIN_OPT_TRI,2,I) =  RECVBUF(J + I_MAIN_OPT_TRI,P)
                SUMMARY(I_MAIN_CRIT_TRI,2,I)=  RECVBUF(J + I_MAIN_CRIT_TRI,P)
                SUMMARY(I_MAIN_TRI,2,I)     =  RECVBUF(J + I_MAIN_TRI,P)
                SUMMARY(I_MAIN_FORCES,2,I)  =  RECVBUF(J + I_MAIN_FORCES,P)
                PMAX = TOTAL_TIME
              ENDIF
            ENDDO !P
            IF(RATIO(I) > TRESHOLD) NB_PRINTED_INTERF = NB_PRINTED_INTERF + 1
          ENDDO ! I
          IF(NB_PRINTED_INTERF > 0) THEN
            WRITE(IOUT,*)' '
            WRITE(IOUT,*)
     .      '                           ** INTERFACE SUMMARY **'

            DO I = 1,NBINTC
              IF(RATIO(I) > TRESHOLD) THEN
                N = INTLIST(I)
                WRITE(IOUT,*)' '
                WRITE(IOUT,*)
     .       'INTERFACE ID:',INTBUF_TAB(N)%METRIC%NOINT,' TYPE: ',IPARI(7,N)
                WRITE(IOUT,'(A,F12.3,A)')
     .       'REMOTE SECONDARY:',SUMMARY(I_NSNR,1,I)/SUMMARY(I_NSN,1,I),'%'
                WRITE(IOUT,*) 'TIME (s)            MAX         AVG'
                WRITE(IOUT,'(A,2F12.2)')
     .       '   SORT.MAIN:',SUMMARY(I_MAIN_TRI,2,I),SUMMARY(I_MAIN_TRI,1,I)/NSPMD
                WRITE(IOUT,'(A,2F12.3)')
     .       '   SORT.CRIT:',SUMMARY(I_MAIN_CRIT_TRI,2,I),SUMMARY(I_MAIN_CRIT_TRI,1,I)/NSPMD
                WRITE(IOUT,'(A,2F12.3)')
     .       '    SORT.OPT:',SUMMARY(I_MAIN_OPT_TRI,2,I),SUMMARY(I_MAIN_OPT_TRI,1,I)/NSPMD
                WRITE(IOUT,'(A,2F12.3)')
     .       '      FORCES:',SUMMARY(I_MAIN_FORCES,2,I),SUMMARY(I_MAIN_FORCES,1,I)/NSPMD
              ENDIF
            ENDDO !I = 1,NBINTC
          ENDIF
        ENDIF
      ENDIF


      DEALLOCATE(RATIO,SENDBUF,RECVBUF,SUMMARY)
      RETURN
      END SUBROUTINE
