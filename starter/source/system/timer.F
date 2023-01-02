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
Chd|  INITIME                       source/system/timer.F         
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INITIME()
C     initialisation des timers
C     timer                       signification
C
C     1                           translator
C     2                           lecture total (including ddsplit)
c     3                           ddsplit
c     4                           freeform
c     5                           IPARI_L_INI
c     6                           INTBUF_INI
c     7				  SPLIT_INTERFACES
c     8                           W_FI
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "timer_c.inc"
      COMMON /ICLOCK/CLOCK0
      INTEGER CLOCK0
      COMMON /RCLOCK/ELAPSED
      DOUBLE PRECISION ELAPSED

#if _OPENMP
       INTEGER OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
       EXTERNAL OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
       REAL(kind=8) OMP_get_wtime
       external OMP_get_wtime
#endif
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C-----------------------------------------------
      DO I =1, NTHMAX
        DO J = 1, NTIMAX 
          CPUTIME(J,I) = ZERO
          REALTIME(J,I) = ZERO
        ENDDO 
      ENDDO
      CALL SYSTEM_CLOCK(CLOCK0)   ! recuperation du temps de debut dans clock0
      ELAPSED = ZERO  ! initialisation a zero de l'elapsed time

#if _OPENMP
        OMP_STARTING_TIME = OMP_GET_WTIME( )
#endif

      RETURN
      END
Chd|====================================================================
Chd|  STARTIME                      source/system/timer.F         
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        MY_ETIME                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE STARTIME(EVENT,ITASK)
C     routine d'incrementation du timer event
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "timer_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EVENT,ITASK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#if _OPENMP
       REAL(kind=8) OMP_GET_WTIME
       external OMP_GET_WTIME
#endif
C
      CALL MY_ETIME(TIMER(1,EVENT,ITASK))
#if _OPENMP
      OMP_INITIME(EVENT,ITASK) = OMP_GET_WTIME( )
#else
      CALL SYSTEM_CLOCK(CLOCKINI(EVENT,ITASK))
#endif
      RETURN
      END
Chd|====================================================================
Chd|  STOPTIME                      source/system/timer.F         
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        MY_ETIME                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE STOPTIME(EVENT,ITASK)
C     routine incrementation du timer event
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "timer_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EVENT,ITASK

      DOUBLE PRECISION  SECS
      INTEGER CLOCK1, CLOCKRATE, NBMAX
      REAL(KIND=8) :: OMP_ENDING_TIME
#if _OPENMP
       REAL(kind=8) OMP_GET_WTIME
       external OMP_GET_WTIME
#endif
C-----------------------------------------------
      CALL MY_ETIME(TIMER(3,EVENT,ITASK))
      CPUTIME(EVENT,ITASK) = CPUTIME(EVENT,ITASK) +
     .                       TIMER(3,EVENT,ITASK)-TIMER(1,EVENT,ITASK)

#if _OPENMP
      OMP_ENDING_TIME = OMP_GET_WTIME( )
      SECS = OMP_ENDING_TIME - OMP_INITIME(EVENT,ITASK)
#else
      CALL SYSTEM_CLOCK(COUNT=CLOCK1, COUNT_RATE=CLOCKRATE,
     +                  COUNT_MAX=NBMAX                   )
      SECS = CLOCK1-CLOCKINI(EVENT,ITASK)    ! diff temps courant - temps initial
      IF(SECS<ZERO) SECS = SECS + NBMAX   ! cas depassement nb de periode maximum
      SECS = SECS/CLOCKRATE
#endif
      REALTIME(EVENT,ITASK)=REALTIME(EVENT,ITASK)+SECS

      RETURN
      END
Chd|====================================================================
Chd|  PRINTIME                      source/system/timer.F         
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        ELAPSTIME                     source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRINTIME(ITASK,GOT_TIMER,STARTDATE,STARTTIME)
C     routine d'affichage des timers
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "scr05_c.inc"
#include       "timer_c.inc"
#include       "units_c.inc"
        COMMON /ICLOCK/CLOCK0
        INTEGER CLOCK0
        COMMON /RCLOCK/ELAPSED
        DOUBLE PRECISION ELAPSED
        CHARACTER(len=8), INTENT(IN) ::  STARTDATE
        CHARACTER(len=10), INTENT(IN) :: STARTTIME
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ITASK,GOT_TIMER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        DOUBLE PRECISION  SECS
        CHARACTER(len=8) ::  DATE
        CHARACTER(len=10) ::  TIME
        INTEGER :: IH,IM,IS
C-----------------------------------------------
        
        CALL ELAPSTIME(SECS)
        IH=SECS/3600
        IM=(SECS-IH*3600)/60
        IS=SECS-IH*3600-IM*60

        CALL DATE_AND_TIME(DATE,TIME) 

        WRITE(IOUT,*)  ' '
        WRITE(ISTDO,*) ' '
        WRITE(IOUT,6100) 
        WRITE(ISTDO,6100)
        IF(GOT_TIMER>0) THEN
                WRITE(IOUT,*)' '
                WRITE(IOUT,*)'CPU USER TIME'
                WRITE(IOUT,*)'-------------'

                WRITE(IOUT,1000)
                WRITE(IOUT,1100)
     .                  CPUTIME(1,ITASK),CPUTIME(4,ITASK),CPUTIME(2,ITASK)-
     .                  CPUTIME(3,ITASK),CPUTIME(3,ITASK),
     .                  CPUTIME(5,ITASK),CPUTIME(6,ITASK),CPUTIME(7,ITASK),
     .                  CPUTIME(8,ITASK)

                WRITE(ISTDO,*)' '
                WRITE(ISTDO,*)'CPU USER TIME'
                WRITE(ISTDO,*)'-------------'

                WRITE(ISTDO,1000)
                WRITE(ISTDO,1100)
     .                  CPUTIME(1,ITASK),CPUTIME(4,ITASK),CPUTIME(2,ITASK)-
     .                  CPUTIME(3,ITASK),CPUTIME(3,ITASK),
     .                  CPUTIME(5,ITASK),CPUTIME(6,ITASK),CPUTIME(7,ITASK),
     .                  CPUTIME(8,ITASK)

C---------------------------
C Elapsed time & Estimated Speedup
C---------------------------

c---output file
                WRITE(IOUT,*)' '
                WRITE(IOUT,*)' '
                WRITE(IOUT,*)'ELAPSED TIME'
                WRITE(IOUT,*)'------------'

                WRITE(IOUT,1000)
                WRITE(IOUT,1100)                                        
     .                  REALTIME(1,1),REALTIME(4,1),REALTIME(2,1)-
     .                  REALTIME(3,1),REALTIME(3,1),
     .                  REALTIME(5,1),REALTIME(6,1),REALTIME(7,1),
     .                  REALTIME(8,1)

c---Standard output

                WRITE(ISTDO,*)' '
                WRITE(ISTDO,*)' '
                WRITE(ISTDO,*)'ELAPSED TIME'
                WRITE(ISTDO,*)'------------'
  
                WRITE(ISTDO,1000)
                WRITE(ISTDO,1100)                                        
     .                  REALTIME(1,1),REALTIME(4,1),REALTIME(2,1)-
     .                  REALTIME(3,1),REALTIME(3,1),
     .                  REALTIME(5,1),REALTIME(6,1),REALTIME(7,1),
     .                  REALTIME(8,1)

                WRITE(ISTDO,'(A)') ' ' 
                WRITE(ISTDO,'(A,E9.4)') '                  CONVERTER : ', REALTIME(15,1)
                WRITE(ISTDO,'(A,E9.4)') '             RD INPUT BUILD : ', REALTIME(16,1)
                WRITE(ISTDO,'(A)') ' ' 
                WRITE(ISTDO,'(A,E9.4)') '       RD GROUPS & SURFACES : ', REALTIME(19,1)
                WRITE(ISTDO,'(A)') ' ' 
                WRITE(ISTDO,'(A,E9.4)') '                       SETS : ', REALTIME(17,1)
                WRITE(ISTDO,'(A)') ' ' 
                WRITE(ISTDO,'(A,E9.4)') 'IDDLEVEL=0 - INTERFACE READ : ', REALTIME(10,1)
                WRITE(ISTDO,'(A,E9.4)') 'IDDLEVEL=1 - INTERFACE READ : ', REALTIME(11,1)
                WRITE(ISTDO,'(A)') ' ' 
                WRITE(ISTDO,'(A,E9.4)') 'IDDLEVEL=0 - INTERFACE INIT : ', REALTIME(12,1)
                WRITE(ISTDO,'(A,E9.4)') 'IDDLEVEL=1 - INTERFACE INIT : ', REALTIME(13,1)
                WRITE(ISTDO,'(A)') ' ' 
                WRITE(ISTDO,'(A,E9.4)') 'IDDLEVEL=1 - I7REMNODE      : ', REALTIME(18,1)
                WRITE(ISTDO,'(A)') ' ' 
                WRITE(ISTDO,'(A,E9.4)') '                 ELBUF INIT : ', REALTIME(14,1)

        ENDIF

      WRITE(IOUT,*) ' '
      WRITE(IOUT,*)
     .  '                  ** COMPUTE TIME INFORMATION **'
      WRITE(IOUT,*)' '

      WRITE(IOUT,6200)STARTDATE(1:4),STARTDATE(5:6),STARTDATE(7:8),
     .                STARTTIME(1:2),STARTTIME(3:4),STARTTIME(5:6)
      WRITE(IOUT,6300)DATE(1:4),DATE(5:6),DATE(7:8),
     .                TIME(1:2),TIME(3:4),TIME(5:6)
      WRITE(IOUT,*)' '

      WRITE(ISTDO,*)  ' '
      WRITE(ISTDO,*)
     .  '                  ** COMPUTE TIME INFORMATION **'
      WRITE(ISTDO,*)' '

      WRITE(ISTDO,6200)STARTDATE(1:4),STARTDATE(5:6),STARTDATE(7:8),
     .                STARTTIME(1:2),STARTTIME(3:4),STARTTIME(5:6)
      WRITE(ISTDO,6300)DATE(1:4),DATE(5:6),DATE(7:8),
     .                TIME(1:2),TIME(3:4),TIME(5:6)
      WRITE(ISTDO,*)' '

      WRITE(ISTDO,6000)SECS
      WRITE(ISTDO,6050) IH,IM,IS
      WRITE(ISTDO,*)' '

      WRITE(IOUT,6000)SECS
      WRITE(IOUT,6050)IH,IM,IS
      WRITE(IOUT,*)' '

        WRITE(IOUT,6100) 
        WRITE(ISTDO,6100)



c---Format
 1000 FORMAT(' TRANSLATOR  ','FREEFORM    ','LECTURE     ',
     .'DDSPLIT     ','IPARI_L_INI ','INTBUF_INI  ','SPL_INTS    ',
     .'W_FI        ')
 1100 FORMAT(E9.4,3x,E9.4,3x,E9.4,3x,E9.4,3x,E9.4,3x,E9.4
     .,3x,E9.4,3x,E9.4)

 6000 FORMAT(' ELAPSED TIME...........=',F14.2,' s')
 6050 FORMAT('                               ',I2.2,':',I2.2,':',I2.2)
 6100 FORMAT('------------------------------------------------------------------------')
 6200 FORMAT(' EXECUTION STARTED      :      ',
     .        A4,'/',A2,'/',A2,'  ',A2,':',A2,':',A2)
 6300 FORMAT(' EXECUTION COMPLETED    :      ',
     .        A4,'/',A2,'/',A2,'  ',A2,':',A2,':',A2)


      RETURN
      END
Chd|====================================================================
Chd|  ELAPSTIME                     source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIME                      source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE ELAPSTIME(ETIME)
C       reset timer elapsed time et calcul du temps elapsed courant
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "timer_c.inc"
        COMMON /ICLOCK/CLOCK0
        INTEGER CLOCK0
        COMMON /RCLOCK/ELAPSED
        DOUBLE PRECISION ELAPSED
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        DOUBLE PRECISION ETIME
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER CLOCK1, CLOCKRATE, NBMAX
        DOUBLE PRECISION  SECS
        REAL(KIND=8) :: OMP_ENDING_TIME
#if _OPENMP
        REAL(kind=8) OMP_GET_WTIME
        external OMP_GET_WTIME
#endif
C-----------------------------------------------
        CALL SYSTEM_CLOCK(COUNT=CLOCK1, COUNT_RATE=CLOCKRATE,
     +                  COUNT_MAX=NBMAX                   )   


#if  _OPENMP
        OMP_ENDING_TIME = OMP_GET_WTIME( )
        SECS = OMP_ENDING_TIME-OMP_STARTING_TIME
        ELAPSED = SECS
#else    
        SECS = CLOCK1-CLOCK0         ! diff temps courant - temps initial (qui peut ne pas etre 0 !)
        IF(SECS<ZERO) SECS = SECS + NBMAX   ! cas depassement nb de periode maximum
        SECS = SECS/CLOCKRATE
        ELAPSED = ELAPSED + SECS
#endif

        CLOCK0 = CLOCK1              ! recuperation du temps courant dans clock0
        ETIME = ELAPSED

        RETURN
        END SUBROUTINE ELAPSTIME

Chd|====================================================================
Chd|  ELAPSTIME_OMP                 source/system/timer.F         
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE ELAPSTIME_OMP(ETIME)
!       compute the current elapsed time 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "timer_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        real(kind=8) :: ETIME
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER CLOCK1, CLOCKRATE, NBMAX
        real(kind=8) :: SECS
        real(kind=8) :: OMP_ENDING_TIME
        real(kind=8) :: OMP_GET_WTIME
        external OMP_GET_WTIME
C-----------------------------------------------  


        OMP_ENDING_TIME = OMP_GET_WTIME( )
        SECS = OMP_ENDING_TIME-OMP_STARTING_TIME

        ETIME = SECS

        RETURN
        END SUBROUTINE ELAPSTIME_OMP
