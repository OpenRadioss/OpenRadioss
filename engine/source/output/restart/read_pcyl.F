Chd|====================================================================
Chd|  READ_PCYL                     source/output/restart/read_pcyl.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|====================================================================
      SUBROUTINE READ_PCYL(LOADS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE LOADS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (LOADS_)  :: LOADS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NSEG,LLINT,IAD,NLOAD_CYL
      INTEGER :: MY_SIZE
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: ITMP
      my_real, DIMENSION(3) :: RTMP
C-----------------------------------------------
c         -> PCYL_ID
c         -> NSEG
c         -> SEGNOD(NSEG,4)  (N1,N2,NB3,N4) by segment
c         -> FRAME_ID
c         -> SENS_ID
c         -> TABLE_ID
c         -> XSCALE_R
c         -> XSCALE_T
c         -> YSCALE_P
c=======================================================================
      NLOAD_CYL = LOADS%NLOAD_CYL
      ALLOCATE(LOADS%LOAD_CYL(NLOAD_CYL))
c      
      DO I=1,NLOAD_CYL
        CALL READ_I_C(NSEG,1)
        LLINT = 4 + NSEG*4
        ALLOCATE (ITMP(LLINT))
        CALL READ_I_C(ITMP,LLINT)
        
        LOADS%LOAD_CYL(I)%ID       = ITMP(1)
        LOADS%LOAD_CYL(I)%ISENS    = ITMP(2)
        LOADS%LOAD_CYL(I)%ITABLE   = ITMP(3)
        LOADS%LOAD_CYL(I)%IFRAME   = ITMP(4)
c
        ALLOCATE(LOADS%LOAD_CYL(I)%SEGNOD(NSEG,4))
        IAD = 4
        LOADS%LOAD_CYL(I)%NSEG = NSEG
        DO J=1,NSEG
          LOADS%LOAD_CYL(I)%SEGNOD(J,1) = ITMP(IAD+1)
          LOADS%LOAD_CYL(I)%SEGNOD(J,2) = ITMP(IAD+2)
          LOADS%LOAD_CYL(I)%SEGNOD(J,3) = ITMP(IAD+3)
          LOADS%LOAD_CYL(I)%SEGNOD(J,4) = ITMP(IAD+4)
          IAD = IAD + 4
        END DO
c
        CALL READ_DB(RTMP,3)
        LOADS%LOAD_CYL(I)%XSCALE_R = RTMP(1) 
        LOADS%LOAD_CYL(I)%XSCALE_T = RTMP(2) 
        LOADS%LOAD_CYL(I)%YSCALE   = RTMP(3) 

        ! ---------------
        ! load/pcyl option : adress of FSKY array
        MY_SIZE = LOADS%LOAD_CYL(I)%NSEG
        ALLOCATE(LOADS%LOAD_CYL(I)%SEGMENT_ADRESS(4,MY_SIZE))
        CALL READ_I_C(LOADS%LOAD_CYL(I)%SEGMENT_ADRESS,4*MY_SIZE)
        ! ---------------
c        
        DEALLOCATE(ITMP)
      ENDDO
c-----------
      RETURN
      END
