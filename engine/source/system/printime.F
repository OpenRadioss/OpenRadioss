!||====================================================================
!||    printime          ../engine/source/system/timer.F
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    close_c           ../common_source/tools/input_output/write_routines.c
!||    cur_fil_c         ../common_source/tools/input_output/write_routines.c
!||    elapstime         ../engine/source/system/timer.F
!||    fseek_end_c       ../common_source/tools/input_output/write_routines.c
!||    get_mem_map_var   ../engine/source/system/timer.F
!||    map_memory        ../common_source/comm/memory_use_c.c
!||    open_c            ../common_source/tools/input_output/write_routines.c
!||    print_summary     ../engine/source/system/timer.F
!||    rad_spmd_recv     ../engine/source/mpi/generic/rad_spmd_recv.F
!||    rad_spmd_send     ../engine/source/mpi/generic/rad_spmd_send.F
!||    spmd_glob_isum9   ../engine/source/mpi/interfaces/spmd_th.F
!||    write_dpdb        ../common_source/tools/input_output/write_db.F
!||--- uses       -----------------------------------------------------
!||    inoutfile_mod     ../common_source/modules/inoutfile_mod.F
!||    output_mod        ../common_source/modules/output/output_mod.F90
!||    time_mod          ../engine/share/modules/time_mod.F
!||    timer_mod         ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE PRINTIME(T,ITHERM,OUTPUT)
C     timers display routine
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE TIME_MOD
      USE INOUTFILE_MOD
      USE OUTPUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include        "macro.inc"
#include        "com01_c.inc"
#include        "com04_c.inc"
#include        "scr05_c.inc"
#include        "sms_c.inc"
#include        "task_c.inc"
#include        "timerc_c.inc"
#include        "timeri_c.inc"
!#include        "timerr_c.inc"
#include        "units_c.inc"
#include        "filescount_c.inc"
      COMMON /ICLOCK/CLOCK0
      INTEGER CLOCK0
      COMMON /RCLOCK/ELAPSED
      DOUBLE PRECISION ELAPSED
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: T
      INTEGER ,INTENT(IN) :: ITHERM
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NPROC, MSGTAG, IRSIZE, J, IH, IM, IS
      DOUBLE PRECISION  SECS, TOTAL, CUMUL(MAX_NB_TIMER),
     .        AVERAGE(MAX_NB_TIMER), DEVIATION(MAX_NB_TIMER),
     .        DMIN(MAX_NB_TIMER), DMAX(MAX_NB_TIMER),CUMUL_MAT,CUMUL_ELEM,
     .        CUMUL_FAIL,ARRAY(NSPMD),X_MINVALUE,X_MAXVALUE,Y_MAXVALUE
      INTEGER MEMUSE(NSPMD),MEM_L,MMIN,MMAX,MTOT,MAVR,
     .        OUTSIZE,RFILESIZE,NB_INTERV
      INTEGER VMPEAK, VMSIZE,VMRSS,VMHWM, VMSTK,MEMSTAT(5,NSPMD),
     .        GOT_MEM_MAP
      CHARACTER(LEN=9) TITLES(MAX_NB_TIMER)
 
      REAL ZEROS
      INTEGER :: LEN_RST_NAME
      CHARACTER*8  DATE
      CHARACTER*10  TIME
      LOGICAL :: BOOL_FILE
      INTEGER :: CURRENT_RUN,OFFSET
      INTEGER, DIMENSION(2148) :: RST_NAME
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
      INTEGER :: INDICE
      INTEGER, PARAMETER :: INTSIZE = 4
      REAL, DIMENSION(:,:), ALLOCATABLE :: CPUTIME
      REAL, DIMENSION(:,:), ALLOCATABLE :: SYSTIME
      REAL, DIMENSION(:,:), ALLOCATABLE :: REALTIME
 

      TITLES(MACRO_TIMER_RESOL      ) = "RESOL"
      TITLES(MACRO_TIMER_CONTSORT   ) = "CONTSORT"
      TITLES(MACRO_TIMER_CONTFOR    ) = "CONTFOR"
      TITLES(MACRO_TIMER_ELEMENT    ) = "ELEMENT"
      TITLES(MACRO_TIMER_KIN        ) = "KINCOND"
      TITLES(MACRO_TIMER_INTEG      ) = "INTEG"
      TITLES(MACRO_TIMER_P0         ) = "T0"
      TITLES(MACRO_TIMER_IO         ) = "IO"
      TITLES(MACRO_TIMER_ASM        ) = "ASM"
      TITLES(MACRO_TIMER_AMS        ) = "SMS"


      TITLES(MACRO_TIMER_ALEMAIN    ) = "ALEMAIN"
      TITLES(MACRO_TIMER_IFSUB0     ) = "IFSUB0"
      TITLES(MACRO_TIMER_MULTIFVM   ) = "MULTIFVM"
      TITLES(MACRO_TIMER_SPMDCFD    ) = "SPMDCFD"
      TITLES(MACRO_TIMER_MUSCL    ) = "MUSCL"
      titles(timer_ale_elm) = "ALE_ELM"

      
      TITLES(31) = '[K]SETUP'
      TITLES(32) = '[M]SETUP'
      TITLES(33) = 'IMP_SOLV'
      TITLES(34) = 'IMP_TOT'
      TITLES(36) = 'CRITER'
      TITLES(37) = 'FOR'
      TITLES(38) = 'VEL'
C
      ZEROS=ZERO
      IF (ICRAY==1) THEN
        IRSIZE = 8
      ELSE
        IRSIZE = 4
      ENDIF
C
      VMPEAK=-1
      VMSIZE=-1
      VMRSS=-1
      VMHWM=-1
      VMSTK=-1
      CALL MAP_MEMORY(VMPEAK, VMSIZE,VMRSS,VMHWM, VMSTK)
      NPROC = NSPMD
C nproc = number of domains, all SMP tasks count as one proc
CC      NPROC = MAX(NTHREAD,NSPMD)
CC smp nproc = ntask, spmd nproc = NSPMD

      ALLOCATE(CPUTIME(MAX_NB_TIMER,NSPMD))
      ALLOCATE(SYSTIME(MAX_NB_TIMER,NSPMD))
      ALLOCATE(REALTIME(MAX_NB_TIMER,NSPMD))

      DO I = 1, MAX_NB_TIMER
       ! copy local timers 
        CPUTIME(I,1) = T%CPUTIME(I)
        SYSTIME(I,1) = T%SYSTIME(I)
        REALTIME(I,1) = T%REALTIME(I)
      ENDDO

      MEMUSE(ISPMD+1)=VMPEAK
      MEMSTAT(1,ISPMD+1) = VMPEAK
      MEMSTAT(2,ISPMD+1) = VMSIZE
      MEMSTAT(3,ISPMD+1) = VMRSS
      MEMSTAT(4,ISPMD+1) = VMHWM
      MEMSTAT(5,ISPMD+1) = VMSTK

      IF (NSPMD > 1) THEN
C gather cpu and sys time for spmd
        MSGTAG = 1000
        IF (ISPMD/=0) THEN
          CALL RAD_SPMD_SEND(T%CPUTIME,MAX_NB_TIMER*IRSIZE,0,IT_SPMD,
     .                       MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND(T%SYSTIME,MAX_NB_TIMER*IRSIZE,0,IT_SPMD,
     .                       MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND(T%REALTIME,MAX_NB_TIMER*IRSIZE,0,IT_SPMD,
     .                       MSGTAG+ISPMD+1,INTSIZE)
        ELSE
          DO I = 2, NSPMD
            CALL RAD_SPMD_RECV(CPUTIME(1,I),MAX_NB_TIMER*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(SYSTIME(1,I),MAX_NB_TIMER*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(REALTIME(1,I),MAX_NB_TIMER*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
          ENDDO
        ENDIF

C Gather memory used
        MSGTAG = 1100
        MEMUSE=0
        MEM_L=0

        IF (ISPMD/=0) THEN
          CALL RAD_SPMD_SEND( MEMUSE(ISPMD+1),1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND( VMSIZE,1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND( VMRSS,1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND( VMHWM,1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND( VMSTK,1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
        ELSE
          DO I = 2, NSPMD
            CALL RAD_SPMD_RECV(MEMUSE(I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            MEMSTAT(1,I) = MEMUSE(I)
            CALL RAD_SPMD_RECV(MEMSTAT(2,I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(MEMSTAT(3,I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(MEMSTAT(4,I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(MEMSTAT(5,I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            
          ENDDO

          INDICE = 1
          IF(MEMSTAT(4,1)/=-1) THEN
             INDICE = 4    
          ELSEIF(MEMSTAT(1,1)/=-1) THEN
             INDICE = 1
          ELSEIF(MEMSTAT(2,1)/=-1) THEN
             INDICE = 2
          ELSEIF(MEMSTAT(3,1)/=-1) THEN
             INDICE = 3
          ENDIF

          MMIN = MEMSTAT(INDICE,1)
          MMAX = MEMSTAT(INDICE,1)
          MTOT = MEMSTAT(INDICE,1)
          DO I=2,NSPMD
            IF (MMIN > MEMSTAT(INDICE,I) ) THEN
              MMIN = MEMSTAT(INDICE,I)
            ENDIF
            IF (MMAX < MEMSTAT(INDICE,I) ) THEN
              MMAX = MEMSTAT(INDICE,I)
            ENDIF
            MTOT = MTOT+ MEMSTAT(INDICE,I)
          ENDDO
          MAVR=MTOT/NSPMD
        ENDIF
      ELSE
C Monoproc run
C Management of used memory
        IF(VMHWM/=-1) THEN
            MEM_L = VMHWM
        ELSEIF(VMPEAK/=-1) THEN
            MEM_L = VMPEAK
        ELSEIF(VMSIZE/=-1) THEN
            MEM_L = VMSIZE
        ELSE
            MEM_L = -1
        ENDIF
        MMIN = MEM_L
        MMAX = MEM_L
        MTOT = MEM_L
        MAVR = MEM_L
      ENDIF
C
C Gather the size of restart files
      IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(RESTARTFILESIZE,1)
C Gather Size of Multiple Restart files
      IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(MULTIRESTS,26)
C


C
      IF (ISPMD/=0) RETURN

      WRITE(ISTDO,*)' '
      WRITE(ISTDO,*)'                          ** CPU USER TIME **'
      WRITE(ISTDO,*)' '

        CALL PRINT_SUMMARY(1,MAX_NB_TIMER,CPUTIME,TITLES,ISTDO,ITHERM) 

      WRITE(IOUT,*)' '
      WRITE(IOUT,*)'                          ** CPU USER TIME **'
      WRITE(IOUT,*)' '

        
        CALL PRINT_SUMMARY(1,MAX_NB_TIMER,CPUTIME,TITLES,IOUT,ITHERM) 




C
C
      IF(IMON_MAT==1)THEN
          CUMUL_MAT=ZERO
          CUMUL_ELEM=ZERO
          CUMUL_FAIL=ZERO
          DO I = 1, NPROC
            CUMUL_MAT = CUMUL_MAT +CPUTIME(35,I)
            CUMUL_ELEM= CUMUL_ELEM+CPUTIME(3,I)
            CUMUL_FAIL= CUMUL_FAIL+CPUTIME(121,I)
          ENDDO

          WRITE(IOUT,*)' '
          WRITE(IOUT,*)'                  ** MATERIAL LAWS COST **'
          WRITE(IOUT,*)' '
          WRITE(ISTDO,*)' '
          WRITE(ISTDO,*)'                  ** MATERIAL LAWS COST **'
          WRITE(ISTDO,*)' '
          WRITE(IOUT, '(A)')' #PROC  ELEM COST  MAT COST    FAIL COST'
          WRITE(ISTDO,'(A)')' #PROC  ELEM COST  MAT COST    FAIL COST'
          DO I = 1, NPROC
             WRITE(IOUT, '(I4,3x,E9.4,3x,E9.4,3x,E9.4)') I,CPUTIME(3,I),CPUTIME(35,I),CPUTIME(121,I)
             WRITE(ISTDO,'(I4,3x,E9.4,3x,E9.4,3x,E9.4)') I,CPUTIME(3,I),CPUTIME(35,I),CPUTIME(121,I)
          ENDDO
          WRITE(IOUT,*)' '
          WRITE(ISTDO,*)' '
          WRITE(IOUT,'(A,E9.4)')  ' TOTAL ELEMENT COST..............: ',CUMUL_ELEM
          WRITE(IOUT,'(A,E9.4)')  ' TOTAL MATERIAL COST.............: ',CUMUL_MAT
          WRITE(IOUT,'(A,E9.4)')  ' TOTAL FAIL COST.................: ',CUMUL_FAIL

          WRITE(ISTDO,'(A,E9.4)') ' TOTAL ELEMENT COST..............: ',CUMUL_ELEM
          WRITE(ISTDO,'(A,E9.4)') ' TOTAL MATERIAL COST.............: ',CUMUL_MAT
          WRITE(ISTDO,'(A,E9.4)') ' TOTAL FAIL COST.................: ',CUMUL_FAIL
          WRITE(IOUT,*)' '
          WRITE(ISTDO,*)' '
      ENDIF
C
      WRITE(IOUT,*)' '
C---------------------------
C Global timings
C---------------------------
      DO I = 1, MAX_NB_TIMER
        CUMUL(I) = ZERO
      END DO
      DO I = 1,NPROC
        DO J = 1, MAX_NB_TIMER
         CUMUL(J) = CUMUL(J) + CPUTIME(J,I)
        ENDDO
      ENDDO
      IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
        TOTAL = CUMUL(1)-CUMUL(2)-CUMUL(8)-CUMUL(3)-CUMUL(4)-CUMUL(5)
     -                  -CUMUL(9)-CUMUL(MACRO_TIMER_ALEMAIN)
      ELSE
        TOTAL = CUMUL(1)-CUMUL(2)-CUMUL(8)-CUMUL(3)-CUMUL(4)-CUMUL(5)
     -                  -CUMUL(9)-CUMUL(39)-CUMUL(MACRO_TIMER_ALEMAIN) 
      END IF
      IF(CUMUL(1)<=ZERO)CUMUL(1)=EM10


        WRITE(ISTDO,*) ''

      WRITE(ISTDO,*)
     .  '                  ** CUMULATIVE CPU TIME SUMMARY **'
      WRITE(ISTDO,*)' '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' CONTACT SORTING.............: ',
     +  CUMUL(2),100*CUMUL(2)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' CONTACT FORCES..............: ',
     +  CUMUL(8),100*CUMUL(8)/CUMUL(1),' % '
      IF(NINTER25/=0)                  
     * WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' ..INCLUDING CONTACT NORMALS: ',
     *  CUMUL(106),100*CUMUL(106)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' ELEMENT FORCES..............: ',
     +  CUMUL(3),100*CUMUL(3)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' KINEMATIC COND..............: ',
     +  CUMUL(4),100*CUMUL(4)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' INTEGRATION.................: ',
     +  CUMUL(5),100*CUMUL(5)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' ASSEMBLING..................: ',
     +  CUMUL(9),100*CUMUL(9)/CUMUL(1),' % '
      IF(IDTMINS/=0.OR.IDTMINS_INT/=0)THEN
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' AMS.........................: ',
     +  CUMUL(39),100*CUMUL(39)/CUMUL(1),' % '
      END IF
      IF(IALE+IEULER+ITHERM/=0) THEN
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' ALE.........................: ',
     + CUMUL(MACRO_TIMER_ALEMAIN),
     + 100*CUMUL(MACRO_TIMER_ALEMAIN)/CUMUL(1),' % '
      ENDIF

      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' OTHERS (including I/O)......: ',
     +  TOTAL,100*TOTAL/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' TOTAL.......................: ',
     +  CUMUL(1),100*CUMUL(1)/CUMUL(1),' % '
c     IF(NUMSPH /=0) THEN
c     WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' SPH TOTAL...................: ',
c    + CUMUL(48),
c    + 100*CUMUL(48)/CUMUL(1),' % '
c     ENDIF



      WRITE(ISTDO,*)' '

        WRITE(IOUT,*) ''
      WRITE(IOUT,*)
     .  '                  ** CUMULATIVE CPU TIME SUMMARY **'
      WRITE(IOUT,*)' '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' CONTACT SORTING.............: ',
     +  CUMUL(2),100*CUMUL(2)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' CONTACT FORCES..............: ',
     +  CUMUL(8),100*CUMUL(8)/CUMUL(1),' % '
      IF(NINTER25/=0)
     *  WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' .. INCLUDING CONTACT NORMALS: ',
     *  CUMUL(106),100*CUMUL(106)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' ELEMENT FORCES..............: ',
     +  CUMUL(3),100*CUMUL(3)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' KINEMATIC COND..............: ',
     +  CUMUL(4),100*CUMUL(4)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' INTEGRATION.................: ',
     +  CUMUL(5),100*CUMUL(5)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' ASSEMBLING..................: ',
     +  CUMUL(9),100*CUMUL(9)/CUMUL(1),' % '
      IF(IDTMINS/=0.OR.IDTMINS_INT/=0)THEN
        WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' AMS.........................: ',
     +  CUMUL(39),100*CUMUL(39)/CUMUL(1),' % '
      END IF
      IF(IALE+IEULER+ITHERM/=0) THEN
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' ALE.........................: ',
     +  CUMUL(MACRO_TIMER_ALEMAIN)
     +  ,100*CUMUL(MACRO_TIMER_ALEMAIN)/CUMUL(1),' % '
        ENDIF
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' OTHERS (including I/O)......: ',
     +  TOTAL,100*TOTAL/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' TOTAL.......................: ',
     +  CUMUL(1),100*CUMUL(1)/CUMUL(1),' % '
      WRITE(IOUT,*)' '

      
C
      
       WRITE(IOUT,*)' '                                               
       WRITE(IOUT,*)'                          ** ELAPSED TIME **'    
       WRITE(IOUT,*)' '                                               
        
        CALL PRINT_SUMMARY(2,MAX_NB_TIMER,REALTIME,TITLES,IOUT,ITHERM) 

       IF(.FALSE.)THEN
        WRITE(IOUT,*)' '
        WRITE(IOUT,*)'               ** ADDITIONAL DEBUG TIMERS (2) **'
        WRITE(IOUT,*)' '
        DO I = 1, NPROC
           WRITE(IOUT,5550)I,
     .      REALTIME(96,I),REALTIME(97,I),REALTIME(98,I),REALTIME(99,I),
     .      REALTIME(100,I),REALTIME(101,I),REALTIME(102,I)
        ENDDO
       END IF
C
      WRITE(IOUT,*)' '

      WRITE(ISTDO,*)
     .  '                  ** MEMORY USAGE STATISTICS **'
      WRITE(ISTDO,*)' '
      WRITE(ISTDO,'(A,A,I8,A)')' TOTAL MEMORY USED ',
     +                   '.........................: ',MTOT,' MB'
      WRITE(ISTDO,'(A,A,I8,A)')' MAXIMUM MEMORY PER PROCESSOR'  ,
     *          '...............: ',  MMAX,' MB'
      WRITE(ISTDO,'(A,A,I8,A)')' MINIMUM MEMORY PER PROCESSOR',
     *          '...............: ',MMIN,' MB'
      WRITE(ISTDO,'(A,A,I8,A)')' AVERAGE MEMORY PER PROCESSOR',
     *          '...............: ', MAVR,' MB'
      WRITE(ISTDO,*)' '
     
      IF (IMONM > 0)THEN
        WRITE(ISTDO,'(A)') ' #PROC     MEMORY USED'
        DO I = 1, NPROC
           WRITE(ISTDO,'(I4,A,I8,A)') I,'       ',MEMUSE(I),' MB'
        ENDDO
        WRITE(ISTDO,*)' '
      ENDIF


C Output file
      WRITE(IOUT,*)
     .  '                  ** MEMORY USAGE STATISTICS **'
      WRITE(IOUT,*)' '
      WRITE(IOUT,'(A,A,I8,A)')' TOTAL MEMORY USED ',
     +                   '.........................: ',MTOT,' MB'
      WRITE(IOUT,'(A,A,I8,A)')' MAXIMUM MEMORY PER PROCESSOR'  ,
     *          '...............: ',  MMAX,' MB'
      WRITE(IOUT,'(A,A,I8,A)')' MINIMUM MEMORY PER PROCESSOR',
     *          '...............: ',MMIN,' MB'
      WRITE(IOUT,'(A,A,I8,A)')' AVERAGE MEMORY PER PROCESSOR',
     *          '...............: ', MAVR,' MB'
      WRITE(IOUT,*)' '

c      IF (IMONM > 0)THEN
c        WRITE(IOUT,'(A)') ' #PROC   MEMORY USE'
c        DO I = 1, NPROC
c           WRITE(IOUT,'(I4,A,I8,A)') I,'       ',MEMUSE(I),' MB'
c        ENDDO
c        WRITE(IOUT,*)' '
c      ENDIF

C workaround / one variable in commandline.inc conflicts with scr07.inc
      CALL GET_MEM_MAP_VAR(GOT_MEM_MAP)
      IF(GOT_MEM_MAP==1 .OR. IMONM > 0)THEN
        WRITE(IOUT,'(A)') '                  ** PROCESS MEMORY MAPPING'
        WRITE(IOUT,*)' '
        WRITE(IOUT,'(A)') ' #PROC    VMPEAK     VMSIZE      VMRSS      VMHWM      VMSTK'
        DO I = 1, NPROC
           WRITE(IOUT,'(I4,A,I10,A,I10,A,I10,A,I10,A,I10)') 
     *       I,'  ',MEMSTAT(1,I),' ',MEMSTAT(2,I),' ',MEMSTAT(3,I),' ',
     *              MEMSTAT(4,I),' ',MEMSTAT(5,I)
        ENDDO
        WRITE(ISTDO,*)' '
         
      ENDIF

C Engine Disk usage Statistics
!     ANIMTOTALSIZE : kb
!     THFILESIZE : kb
!     OUTPFILESIZE : kb
!     H3DTOTALSIZE : mb --> kb
      H3DTOTALSIZE = H3DTOTALSIZE*1024
      OUTSIZE=ANIMTOTALSIZE+THFILESIZE+OUTPFILESIZE+H3DTOTALSIZE
      DO I=1,9
        OUTSIZE = OUTSIZE+MULTITHFILESIZE(I)
      ENDDO
        RFILESIZE=RESTARTFILESIZE
      DO I=1,26
        RFILESIZE = RFILESIZE+MULTIRESTS(I)
      ENDDO
      TOTALFILECOUNT = (OUTSIZE)+RFILESIZE+MUMPSFILESIZE+
     *                 BCSFILESIZE
      TOTALFILECOUNT = TOTALFILECOUNT/1024
      OUTSIZE = OUTSIZE / 1024
      RFILESIZE = RFILESIZE / 1024
      MUMPSFILESIZE = MUMPSFILESIZE / 1024
      BCSFILESIZE = BCSFILESIZE / 1024
      WRITE(ISTDO,*)
     .  '                  ** DISK USAGE STATISTICS **'
      WRITE(ISTDO,*)' '
      WRITE(ISTDO,'(A,A,I10,A)')' TOTAL DISK SPACE USED ',
     *                    '.....................: ',TOTALFILECOUNT,
     *                   ' MB'
      WRITE(ISTDO,'(A,A,I10,A)')' ANIMATION/H3D/TH/OUTP SIZE ',
     *                 '................: ',OUTSIZE,
     *                   ' MB'
      WRITE(ISTDO,'(A,A,I10,A)')' RESTART FILE SIZE ',
     *                   '.........................: ',
     *                  RFILESIZE,' MB'
      IF (MUMPSFILESIZE > 0)THEN
        WRITE(ISTDO,'(A,A,I10,A)')' MUMPS INTERNAL FILE USAGE ',
     *                   '.................: ',
     *                  MUMPSFILESIZE,' MB'
      ENDIF
      IF (BCSFILESIZE > 0) THEN
        WRITE(ISTDO,'(A,A,I10,A)')' BCS INTERNAL FILE USAGE ',
     *                   '...................: ',
     *                  BCSFILESIZE,' MB'
      ENDIF


      WRITE(ISTDO,*)' '


      WRITE(IOUT,*)
     .  '                  ** DISK USAGE STATISTICS **'
      WRITE(IOUT,*)' '
      WRITE(IOUT,'(A,A,I10,A)')' TOTAL DISK SPACE USED ',
     *                   '.....................: ',TOTALFILECOUNT,
     *                   ' MB'
      WRITE(IOUT,'(A,A,I10,A)')' ANIMATION/H3D/TH/OUTP SIZE ',
     *                   '................: ',OUTSIZE,
     *                   ' MB'
      WRITE(IOUT,'(A,A,I10,A)')' RESTART FILE SIZE ',
     *                   '.........................: ',
     *                  RFILESIZE,' MB'
      IF (MUMPSFILESIZE > 0)THEN
        WRITE(IOUT,'(A,A,I10,A)')' MUMPS INTERNAL FILE USAGE ',
     *                   '.................: ',
     *                  MUMPSFILESIZE/1024,' MB'
      ENDIF
      IF (BCSFILESIZE > 0) THEN
        WRITE(IOUT,'(A,A,I10,A)')' BCS INTERNAL FILE USAGE ',
     *                   '...................: ',
     *                  BCSFILESIZE/1024,' MB'
      ENDIF

      WRITE(IOUT,*)' '

C---------------------------
C Elapsed time & Estimated Speedup
C---------------------------
      CALL ELAPSTIME(T,SECS)
      CALL DATE_AND_TIME(DATE,TIME)
      OUTPUT%CHECKSUM%DATE = DATE
      OUTPUT%CHECKSUM%TIME = TIME
!     ---------------------------
      IF(ISPMD==0) THEN
        CURRENT_RUN = GLOBAL_COMP_TIME%RUN_NBR
        GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN) = SECS
        LEN_RST_NAME = LEN_TRIM(GLOBAL_COMP_TIME%RST_NAME)
        LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_RST_NAME
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//GLOBAL_COMP_TIME%RST_NAME(1:LEN_RST_NAME)

        DO I = 1, LEN_TMP_NAME
                RST_NAME(I) = ICHAR(TMP_NAME(I:I))
        END DO
        BOOL_FILE=.FALSE.
        INQUIRE(FILE=TMP_NAME(1:LEN_TMP_NAME), EXIST=BOOL_FILE)
!       check if *_0000.rst exists
        IF(BOOL_FILE) THEN
            CALL CUR_FIL_C(20)
            CALL OPEN_C(RST_NAME,LEN_TMP_NAME,2)
!           write the starter + engine elapsed time
            OFFSET= - STORAGE_SIZE(GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN))/8        
            CALL FSEEK_END_C(OFFSET)
            CALL WRITE_DPDB(GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN),1)
            CALL CLOSE_C
        ENDIF
      ENDIF
!      ---------------------------        


C      CALL SYSTEM_CLOCK(COUNT=CLOCK1, COUNT_RATE=CLOCKRATE,
C     +                  COUNT_MAX=NBMAX                   )
C      SECS = Clock1-Clock0!Final time - Initial time (which may not be 0!)
C      IF(SECS<ZERO) SECS = SECS + NBMAX   ! cas depassement nb de periode maximum
C      SECS = SECS/CLOCKRATE

      IH=INT(SECS/3600.0D0)
      IM=INT((SECS-IH*3600.0D0)/60.0D0)
      IS=INT(SECS-IH*3600.0D0-IM*60.0D0)

      WRITE(IOUT,*)
     .  '                  ** COMPUTE TIME INFORMATION **'
      WRITE(IOUT,*)
     .  '                       ** CURRENT ENGINE **'
      WRITE(IOUT,*)' '

      WRITE(IOUT,6200)STARTDATE(1:4),STARTDATE(5:6),STARTDATE(7:8),
     .                STARTTIME(1:2),STARTTIME(3:4),STARTTIME(5:6)
      WRITE(IOUT,6300)DATE(1:4),DATE(5:6),DATE(7:8),
     .                TIME(1:2),TIME(3:4),TIME(5:6)
      WRITE(IOUT,*)' '

      WRITE(ISTDO,6000)SECS
      WRITE(ISTDO,6100)IH,IM,IS
      IF(NPROC>1.AND.SECS>ZERO)WRITE(ISTDO,8000)CUMUL(1)/SECS
      WRITE(ISTDO,*)' '
      WRITE(IOUT,6000)SECS
      WRITE(IOUT,6100)IH,IM,IS
      IF(NPROC>1.AND.SECS>ZERO)WRITE(IOUT,8000)CUMUL(1)/SECS

 1000 FORMAT(' ** PROCESSOR NUMBER **',16(I8,5x,"%",2x))
 2000 FORMAT(' #PROC  ','CONT.SORT  ','CONT. F   ','ELEMENT  ',
     .       'KIN.COND.  ','INTEGR.','    I/O     ','TASK0     ',
     .       'ASSEMB.   ','RESOL  ')
 3000 FORMAT(' #PROC  ','FORCES    ','RBY.FOR.  ','RBY.VEL.  ',
     .       'VELOCITIES','   TOTAL   ','% CPU')
 5550 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     +       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     +       1x,E9.4,1x,E9.4)
 6000 FORMAT(' ELAPSED TIME     =',F14.2,' s')
 6100 FORMAT('                   ',I8,':',I2.2,':',I2.2)
 6200 FORMAT(' EXECUTION STARTED .........................:      ',
     .        A4,'/',A2,'/',A2,'  ',A2,':',A2,':',A2)
 6300 FORMAT(' EXECUTION COMPLETED .......................:      ',
     .        A4,'/',A2,'/',A2,'  ',A2,':',A2,':',A2)
 8000 FORMAT(' ESTIMATED SPEEDUP=',F14.2)
C
      RETURN
      END
