Chd|====================================================================
Chd|  WRITE_PCYL                    source/output/restart/write_pcyl.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_PCYL(LOADS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE LOADS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (LOADS_) ,INTENT(IN) :: LOADS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NSEG,LINT,IAD,NLOAD_CYL
      INTEGER :: MY_SIZE
      INTEGER, DIMENSION(:),ALLOCATABLE :: ITMP
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
      DO I=1,NLOAD_CYL
        NSEG = LOADS%LOAD_CYL(I)%NSEG
        LINT = 5 + NSEG*4
        ALLOCATE (ITMP(LINT))
        RTMP(1) = LOADS%LOAD_CYL(I)%XSCALE_R
        RTMP(2) = LOADS%LOAD_CYL(I)%XSCALE_T
        RTMP(3) = LOADS%LOAD_CYL(I)%YSCALE
c
        ITMP(1) = NSEG
        ITMP(2) = LOADS%LOAD_CYL(I)%ID
        ITMP(3) = LOADS%LOAD_CYL(I)%ISENS
        ITMP(4) = LOADS%LOAD_CYL(I)%ITABLE
        ITMP(5) = LOADS%LOAD_CYL(I)%IFRAME
        IAD     = 5
        DO J=1,NSEG
          ITMP(IAD+1) = LOADS%LOAD_CYL(I)%SEGNOD(J,1)
          ITMP(IAD+2) = LOADS%LOAD_CYL(I)%SEGNOD(J,2)
          ITMP(IAD+3) = LOADS%LOAD_CYL(I)%SEGNOD(J,3)
          ITMP(IAD+4) = LOADS%LOAD_CYL(I)%SEGNOD(J,4)
          IAD = IAD + 4
        ENDDO
c        
        CALL WRITE_I_C(ITMP,IAD)
        CALL WRITE_DB(RTMP,3)
        ! ---------------
        ! load/pcyl option : adress of FSKY array
        MY_SIZE = LOADS%LOAD_CYL(I)%NSEG
        CALL WRITE_I_C(LOADS%LOAD_CYL(I)%SEGMENT_ADRESS,4*MY_SIZE)
        ! ---------------
c        
        DEALLOCATE(ITMP)
      ENDDO
c-----------
      RETURN
      END
