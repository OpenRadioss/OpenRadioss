Chd|====================================================================
Chd|  BCS3V                         source/ale/bcs3.F             
Chd|-- called by -----------
Chd|        ALESUB1                       source/ale/alesub1.F          
Chd|        ALESUB2                       source/ale/alesub2.F          
Chd|        ALEWDX                        source/ale/alewdx.F           
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS3V(NINDX,INDX,ISKEW,ICODT,W,V,B)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX,INDX(*),ISKEW(*),ICODT(*)
      my_real W(3,NUMNOD), V(3,NUMNOD), B(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, K, ISK, LCOD
      my_real AA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
#include "vectorize.inc"
      DO K = 1, NINDX
        N   = INDX(K)
        ISK =ISKEW(N)
        LCOD=ICODT(N)
        IF(ISK == 1) THEN
C------------------
C     GLOBAL FRAME
C------------------
          IF(LCOD == 1)THEN
            W(3,N)=V(3,N)
          ELSEIF(LCOD == 2)THEN
            W(2,N)=V(2,N)
          ELSEIF(LCOD == 3)THEN
            W(2,N)=V(2,N)
            W(3,N)=V(3,N)
          ELSEIF(LCOD == 4)THEN
            W(1,N)=V(1,N)
          ELSEIF(LCOD == 5)THEN
            W(1,N)=V(1,N)
            W(3,N)=V(3,N)
          ELSEIF(LCOD == 6)THEN
            W(1,N)=V(1,N)
            W(2,N)=V(2,N)
          ELSEIF(LCOD == 7)THEN
            W(1,N)=V(1,N)
            W(2,N)=V(2,N)
            W(3,N)=V(3,N)
          ENDIF
        ELSE
C-------------------
C     OBLIQUE FRAME
C-------------------
          IF(LCOD == 1)THEN
            AA =B(7,ISK)*(W(1,N)-V(1,N))+B(8,ISK)*
     .          (W(2,N)-V(2,N))+B(9,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(7,ISK)*AA
            W(2,N)=W(2,N)-B(8,ISK)*AA
            W(3,N)=W(3,N)-B(9,ISK)*AA
          ELSEIF(LCOD == 2)THEN
            AA=B(4,ISK)*(W(1,N)-V(1,N))+B(5,ISK)*
     .         (W(2,N)-V(2,N))+B(6,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(4,ISK)*AA
            W(2,N)=W(2,N)-B(5,ISK)*AA
            W(3,N)=W(3,N)-B(6,ISK)*AA
          ELSEIF(LCOD == 3)THEN
            AA=B(7,ISK)*(W(1,N)-V(1,N))+B(8,ISK)*
     .         (W(2,N)-V(2,N))+B(9,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(7,ISK)*AA
            W(2,N)=W(2,N)-B(8,ISK)*AA
            W(3,N)=W(3,N)-B(9,ISK)*AA
            AA=B(4,ISK)*(W(1,N)-V(1,N))+B(5,ISK)*
     .         (W(2,N)-V(2,N))+B(6,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(4,ISK)*AA
            W(2,N)=W(2,N)-B(5,ISK)*AA
            W(3,N)=W(3,N)-B(6,ISK)*AA
          ELSEIF(LCOD == 4)THEN
            AA=B(1,ISK)*(W(1,N)-V(1,N))+B(2,ISK)*
     .         (W(2,N)-V(2,N))+B(3,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(1,ISK)*AA
            W(2,N)=W(2,N)-B(2,ISK)*AA
            W(3,N)=W(3,N)-B(3,ISK)*AA
          ELSEIF(LCOD == 5)THEN
            AA=B(7,ISK)*(W(1,N)-V(1,N))+B(8,ISK)*
     .         (W(2,N)-V(2,N))+B(9,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(7,ISK)*AA
            W(2,N)=W(2,N)-B(8,ISK)*AA
            W(3,N)=W(3,N)-B(9,ISK)*AA
            AA=B(1,ISK)*(W(1,N)-V(1,N))+B(2,ISK)*
     .         (W(2,N)-V(2,N))+B(3,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(1,ISK)*AA
            W(2,N)=W(2,N)-B(2,ISK)*AA
            W(3,N)=W(3,N)-B(3,ISK)*AA
          ELSEIF(LCOD == 6)THEN
            AA=B(1,ISK)*(W(1,N)-V(1,N))+B(2,ISK)*
     .         (W(2,N)-V(2,N))+B(3,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(1,ISK)*AA
            W(2,N)=W(2,N)-B(2,ISK)*AA
            W(3,N)=W(3,N)-B(3,ISK)*AA
            AA=B(4,ISK)*(W(1,N)-V(1,N))+B(5,ISK)*
     .         (W(2,N)-V(2,N))+B(6,ISK)*(W(3,N)-V(3,N))
            W(1,N)=W(1,N)-B(4,ISK)*AA
            W(2,N)=W(2,N)-B(5,ISK)*AA
            W(3,N)=W(3,N)-B(6,ISK)*AA
          ELSEIF(LCOD == 7)THEN
            W(1,N)=V(1,N)
            W(2,N)=V(2,N)
            W(3,N)=V(3,N)
          ENDIF
        END IF
      ENDDO
C
      RETURN
      END
