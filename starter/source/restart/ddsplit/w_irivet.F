Chd|====================================================================
Chd|  W_IRIVET                      source/restart/ddsplit/w_irivet.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_IRIVET(IXRI,NODLOCAL,PROC,LEN_IA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, LEN_IA,
     .          IXRI(4,*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N_L, N, N1, N2, IXRI_L(4,NRIVET)
C
      N_L = 0
      DO N = 1, NRIVET
        N1=IXRI(2,N)
        N2=IXRI(3,N)
        IF(NLOCAL(N1,PROC+1)==1.AND.
     +     NLOCAL(N2,PROC+1)==1)THEN
          IXRI_L(1,N) = IXRI(1,N)
          IXRI_L(2,N) = NODLOCAL(N1)
          IXRI_L(3,N) = NODLOCAL(N2)
          IXRI_L(4,N) = IXRI(4,N)
        ELSE
          IXRI_L(1,N) = IXRI(1,N)
          IXRI_L(2,N) = -N1
          IXRI_L(3,N) = N2
          IXRI_L(4,N) = IXRI(4,N)
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IXRI_L,NRIVET*4)
      LEN_IA = LEN_IA + NRIVET*4
C
      RETURN
      END
