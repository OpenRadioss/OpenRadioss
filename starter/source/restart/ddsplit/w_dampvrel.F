Chd|====================================================================
Chd|  W_DAMPING                       source/restart/ddsplit/w_seatbelts.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE W_DAMPVREL(DAMPR,IGRNOD,IDAMP_VREL_L,NDAMP_VREL_L,LEN_IA,
     .                      NGRNOD,NDAMP,NRDAMP,NSPMD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD     
C---------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NDAMP_VREL_L,NGRNOD,NDAMP,NRDAMP,NSPMD
      INTEGER, INTENT(IN) :: IDAMP_VREL_L(NDAMP)
      INTEGER, INTENT(INOUT) :: LEN_IA
      my_real, INTENT(IN) :: DAMPR(NRDAMP,NDAMP)
      TYPE (GROUP_),DIMENSION(NGRNOD),INTENT(IN) :: IGRNOD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IGR,PROC,PMAIN,CPT
      INTEGER FR_DAMP_VREL(NSPMD+2,NDAMP_VREL_L),ID_DAMP_VREL(NDAMP_VREL_L)
C----------------------------------------------- 
C
      CPT = 0
      DO I=1,NDAMP
        IF (IDAMP_VREL_L(I) > 0) THEN
          CPT = CPT+1  
          ID_DAMP_VREL(CPT) = I    
        ENDIF      
      ENDDO        
C
      FR_DAMP_VREL(1:NSPMD+2,1:NDAMP_VREL_L) = 0
      DO I=1,NDAMP_VREL_L
        IGR   = NINT(DAMPR(2,ID_DAMP_VREL(I)))
C--     Tag of procs having nodes in damping                 
        DO PROC = 1,NSPMD
          DO J = 1,IGRNOD(IGR)%NENTITY
            IF (NLOCAL(IGRNOD(IGR)%ENTITY(J),PROC)==1) THEN
              FR_DAMP_VREL(PROC,I) = 1   
            ENDIF        
          ENDDO 
        ENDDO
C--     Find main proc for damping (smallest proc id)          
        DO PROC = 1,NSPMD
          IF (FR_DAMP_VREL(PROC,I) == 1) THEN
            PMAIN =  PROC
            exit
          ENDIF   
        ENDDO
        FR_DAMP_VREL(NSPMD+1,I) = ZERO ! Not used for now
        FR_DAMP_VREL(NSPMD+2,I) = PMAIN
      ENDDO
C
      CALL WRITE_I_C(ID_DAMP_VREL,NDAMP_VREL_L)
      LEN_IA = LEN_IA + NDAMP_VREL_L
C      
      CALL WRITE_I_C(FR_DAMP_VREL,NDAMP_VREL_L*(NSPMD+2)) 
      LEN_IA = LEN_IA + NDAMP_VREL_L*(NSPMD+2)
C                
! --------------------------------------
      RETURN
      END
