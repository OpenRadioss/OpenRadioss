Chd|====================================================================
Chd|  SPMD_EXCHANGE_GRAD            source/mpi/fluid/spmd_exchange_grad.F
Chd|-- called by -----------
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_EXCHANGE_GRAD(DIM,DIM1,DIM2,
     1                       PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS,LENCOM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include "macro.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: DIM,DIM1,DIM2,LENCOM
      INTEGER, INTENT(IN) :: NERCVOIS(SNERCVOIS), NESDVOIS(SNESDVOIS)
      INTEGER, INTENT(IN) :: LERCVOIS(SLERCVOIS), LESDVOIS(SLESDVOIS)
      my_real, DIMENSION(DIM1,DIM2), INTENT(INOUT) :: PHI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, NDIM, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN
      DATA MSGOFF/3004/
      my_real
     .     WA(DIM, LENCOM)
C-----------------------------------------------
C
C Updating Phi on adjacent elements
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*DIM,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            DO NDIM = 1, DIM
               WA(NDIM,IDEB2+N) = PHI(NN,NDIM) ! nn,dim
            ENDDO
          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*DIM,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          DO NDIM = 1, DIM
             PHI(NN,NDIM) = WA(NDIM,IDEB+N)
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END SUBROUTINE SPMD_EXCHANGE_GRAD
