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
Chd|  INT2POFF                      source/interfaces/interf/int2poff.F
Chd|-- called by -----------
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|-- calls ---------------
Chd|        I2IPOFF                       source/interfaces/interf/int2poff.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INT2POFF(IPARI,X    ,V     ,
     .                   A     ,VR   ,AR   ,MS   ,IN    ,
     .                   WEIGHT,STIFN,STIFR,MCP  ,CONDN  ,
     .                   FTHE,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), WEIGHT(*)
C     REAL
      my_real
     .   X(*), V(*), A(*), 
     .   MS(*),IN(*),AR(*),VR(*),STIFN(*),STIFR(*),
     .   CONDN(*), MCP(*),FTHE(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   K10, K11, K12, K13, K14, KFI, J10, J11, J12, J21, J22,
     .   JFI,NSN,NMN,ILEV,NRTS,NRTM
C-----------------------------------------------
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      ILEV  =IPARI(20)
C
      K10=1
      K11=K10+4*NRTS
      K12=K11+4*NRTM
      K13=K12+NSN
      K14=K13+NMN
      KFI=K14+NSN
      J10=1
      J11=J10+1
      J12=J11+NPARIR
      J21=J12+2*NSN
      J22=J21+7*NSN
      JFI=J22+NMN
C
      CALL I2IPOFF(
     1   NMN       ,A      ,INTBUF_TAB%MSR   ,MS       ,WEIGHT    ,
     2   AR        ,IN     ,STIFN            ,STIFR    ,ILEV      ,
     3   MCP       ,CONDN  ,INTBUF_TAB%NMAS  ,FTHE)
C
      RETURN
      END
C
Chd|====================================================================
Chd|  INT2POFFH                     source/interfaces/interf/int2poff.F
Chd|-- called by -----------
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|-- calls ---------------
Chd|        I2IPOFFH                      source/interfaces/interf/int2poff.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INT2POFFH(IPARI,X     ,V     ,
     .                   A     ,VR   ,AR    ,MS    ,IN    ,
     .                   WEIGHT,STIFN,STIFR ,TAGNOD,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), WEIGHT(*), TAGNOD(*)
C     REAL
      my_real
     .   X(*), V(*), A(*), 
     .   MS(*),IN(*),AR(*),VR(*),STIFN(*),STIFR(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   NSN,NMN,ILEV,NRTS,NRTM
C-----------------------------------------------
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      ILEV  =IPARI(20)
C
      CALL I2IPOFFH(
     1   NMN       ,A         ,INTBUF_TAB%MSR,MS    ,WEIGHT    ,
     2   AR        ,IN        ,STIFN         ,STIFR ,ILEV      ,
     3   INTBUF_TAB%NMAS,TAGNOD)
C
      RETURN
      END

Chd|====================================================================
Chd|  I2IPOFF                       source/interfaces/interf/int2poff.F
Chd|-- called by -----------
Chd|        INT2POFF                      source/interfaces/interf/int2poff.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2IPOFF(NMN  ,A     ,MSR   ,MS    ,WEIGHT,
     2                   AR   ,IN    ,STIFN ,STIFR ,ILEV  ,
     3                   MCP  ,CONDN ,MMASS ,FTHE  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN,ILEV,
     .        MSR(*), WEIGHT(*)
C     REAL
      my_real
     .   A(3,*) ,AR(3,*) ,MMASS(*) , 
     .   MS(*)  ,IN(*)   ,STIFN(*) ,STIFR(*),
     .   MCP(*) ,CONDN(*),FTHE(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr18_c.inc"

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
C-----------------------------------------------
      IF((ILEV == 1.OR.ILEV == 3).AND.IRODDL/=0)THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=IN(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)
        ENDDO
      ELSEIF((ILEV == 1.OR.ILEV == 3).AND.IRODDL == 0)THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
        ENDDO
      ELSEIF((ILEV == 0.OR.ILEV == 2.OR.ILEV == 4).AND.IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)
          MS(I)=MS(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          IN(I)=MAX(EM20,IN(I))
          IN(I)=IN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
        ENDDO
      ELSEIF((ILEV == 0.OR.ILEV == 2.OR.ILEV == 4).AND.IRODDL == 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)
          MS(I)=MS(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
        ENDDO
      ELSEIF ((ILEV == 10.OR.ILEV == 11.OR.ILEV == 12 .OR.
     .         ILEV == 20.OR.ILEV == 21.OR.ILEV == 22).AND.  
     .         IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=MAX(EM20,IN(I))
          IN(I)=IN(I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
        ENDDO
      ELSEIF ((ILEV == 25  .or. ILEV == 26) .AND. IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=IN(I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
        ENDDO
      ELSEIF ((ILEV == 27) .AND. IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)                   
          A(1,I)=A(1,I)*WEIGHT(I)     
          A(2,I)=A(2,I)*WEIGHT(I)     
          A(3,I)=A(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)             
          MS(I)=MS(I)*WEIGHT(I)       
          IN(I)=IN(I)*WEIGHT(I)       
          AR(1,I)=AR(1,I)*WEIGHT(I)   
          AR(2,I)=AR(2,I)*WEIGHT(I)   
          AR(3,I)=AR(3,I)*WEIGHT(I)   
          STIFN(I)=STIFN(I)*WEIGHT(I) 
          STIFR(I)=STIFR(I)*WEIGHT(I) 
        ENDDO
      ELSEIF ((ILEV == 28) .AND. IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)                   
          A(1,I)=A(1,I)*WEIGHT(I)     
          A(2,I)=A(2,I)*WEIGHT(I)     
          A(3,I)=A(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)             
          MS(I)=MS(I)*WEIGHT(I)       
          IN(I)=IN(I)*WEIGHT(I)       
          AR(1,I)=AR(1,I)*WEIGHT(I)   
          AR(2,I)=AR(2,I)*WEIGHT(I)   
          AR(3,I)=AR(3,I)*WEIGHT(I)   
          STIFN(I)=STIFN(I)*WEIGHT(I) 
          STIFR(I)=STIFR(I)*WEIGHT(I) 
        ENDDO
      ELSEIF ((ILEV == 10 .OR. ILEV == 11 .OR. ILEV == 12 .OR.
     .         ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22 .OR.
     .         ILEV == 25 .or. ILEV == 26 .or. ILEV == 28 .OR.
     .         ILEV == 27 ) .AND. IRODDL == 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
        ENDDO
      ELSEIF(ILEV == 30 .AND. IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=MAX(EM20,IN(I))
          IN(I)=IN(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
        ENDDO
C---
      ENDIF
C-----------------------------------------------
C  Thermal Analysis (quelque soit ILEV et IRODDL)
C-----------------------------------------------
      IF(ITHERM_FE > 0) THEN
        DO II=1,NMN
           I=MSR(II)
           FTHE(I) = FTHE(I) * WEIGHT(I)
        ENDDO
      ENDIF
      IF(NODADT_THERM == 1) THEN
        DO II=1,NMN
           I=MSR(II)
           CONDN(I) = CONDN(I) * WEIGHT(I)
        ENDDO
      ENDIF
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I2IPOFFH                      source/interfaces/interf/int2poff.F
Chd|-- called by -----------
Chd|        INT2POFFH                     source/interfaces/interf/int2poff.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2IPOFFH(NMN  ,A    ,MSR  ,MS    ,WEIGHT,
     2                   AR   ,IN   ,STIFN,STIFR ,ILEV  ,
     3                   MMASS,TAGNOD )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN,ILEV,
     .        MSR(*), WEIGHT(*), TAGNOD(*)
C     REAL
      my_real
     .   A(3,*),AR(3,*), MMASS(*), 
     .   MS(*), IN(*),STIFN(*),STIFR(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
C-----------------------------------------------
      IF((ILEV == 1.OR.ILEV == 3).AND.IRODDL/=0)THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=IN(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)
          TAGNOD(I)=1
        ENDDO
      ELSEIF((ILEV == 1.OR.ILEV == 3).AND.IRODDL == 0)THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          TAGNOD(I)=1
        ENDDO
      ELSEIF((ILEV == 0.OR.ILEV == 2.OR.ILEV == 4).AND.IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I) =A(1,I) *WEIGHT(I)
          A(2,I) =A(2,I) *WEIGHT(I)
          A(3,I) =A(3,I) *WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=MAX(EM20,IN(I))
          IN(I)=IN(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
          TAGNOD(I)=1
        ENDDO
      ELSEIF((ILEV == 0.OR.ILEV == 2.OR.ILEV == 4).AND.IRODDL == 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)
          MS(I)=MS(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          TAGNOD(I)=1
        ENDDO
      ELSEIF ((ILEV == 10.OR.ILEV == 11.OR.ILEV == 12  .OR.
     .         ILEV == 20.OR.ILEV == 21.OR.ILEV == 22) .AND. 
     .         IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=MAX(EM20,IN(I))
          IN(I)=IN(I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
          TAGNOD(I)=1
        ENDDO
      ELSEIF ((ILEV == 25  .or. ILEV == 26) .AND. IRODDL /= 0)THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=IN(I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
          TAGNOD(I)=1
        ENDDO
      ELSEIF (ILEV == 27 .AND. IRODDL /= 0)THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=IN(I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
          TAGNOD(I)=1
        ENDDO
      ELSEIF (ILEV == 28 .AND. IRODDL /= 0)THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=IN(I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
          TAGNOD(I)=1
        ENDDO
      ELSEIF ((ILEV == 10.OR.ILEV == 11.OR.ILEV == 12 .OR.
     .         ILEV == 20.OR.ILEV == 21.OR.ILEV == 22 .OR.
     .         ILEV == 25.or.ILEV == 26.or.ILEV == 28) 
     .        .AND. IRODDL == 0)THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          MS(I)=MS(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          TAGNOD(I)=1
        ENDDO
      ELSEIF(ILEV == 30 .AND. IRODDL /= 0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          A(1,I)=A(1,I)*WEIGHT(I)
          A(2,I)=A(2,I)*WEIGHT(I)
          A(3,I)=A(3,I)*WEIGHT(I)
          AR(1,I)=AR(1,I)*WEIGHT(I)
          AR(2,I)=AR(2,I)*WEIGHT(I)
          AR(3,I)=AR(3,I)*WEIGHT(I)
          MMASS(II)=MS(I)
          MS(I)=MS(I)*WEIGHT(I)
          IN(I)=MAX(EM20,IN(I))
          IN(I)=IN(I)*WEIGHT(I)
          STIFN(I)=STIFN(I)*WEIGHT(I)
          STIFR(I)=STIFR(I)*WEIGHT(I)       
        ENDDO
C---
      ENDIF
C-----------
      RETURN
      END
