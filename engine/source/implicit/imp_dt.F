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
Chd|  IMP_DT2                       source/implicit/imp_dt.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_DT2(DT_E)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .  DT_E
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  TREST
C--------------------------------------------
C         DT_MIN=MAX(DT_E,DT_MIN)
         TREST=TSTOP-TT
         IF (IMCONV==1.AND.TREST>EM10)  THEN
          DT_E = MIN(DT_IMP,TREST)
	  IF (IDYNA>0) DT_E = DT_IMP
          DT1_IMP= DT_E
          IF (NCYCLE==0.AND.INCONV==1) DT0_IMP= DT_E
         ELSE
          DT_E = DT1
          DT0_IMP= DT_E
         ENDIF
         IF (NCYCLE==0) THEN
	  DT1= DT_E
          DT1_IMP= DT_E
	 ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DTN                       source/implicit/imp_dt.F      
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_DTF                       source/implicit/imp_dt.F      
Chd|====================================================================
      SUBROUTINE IMP_DTN(IT,UL2,FAC,CUMUL_ALEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "scr05_c.inc" 
#include      "task_c.inc" 
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IT
      my_real
     .  FAC,UL2,CUMUL_ALEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFIX0
      my_real
     .  TMP,FACL,LU,FACT,TREST
C-----------------------------------
      FAC=ONE
      IF (IT==0.AND.IMCONV>0) RETURN
      IF (IMCONV<0)  THEN
       IF (DT_IMP>DT_MIN)  THEN
        FAC=SCAL_DTN
        TMP=DT_IMP
        DT_IMP=DT_IMP*FAC
        DT_IMP=MAX(DT_IMP,DT_MIN)
        FAC=DT_IMP/TMP
        IF (IDTC==2.AND.ALEN0/=ZERO)ALEN=ALEN*SCAL_DTN
        IF (IDTC==3) THEN
	 ALEN=ALEN*SCAL_DTN
         FACT = ONE
	END IF
       ENDIF
      ELSEIF (IMCONV==1)  THEN
       IF (IDTFIX>0)  THEN
        DT_IMP = DTIMPF(101)
       ELSEIF (IDTC==1)  THEN
        IF (IT<=NL_DTP.AND.DT_IMP<DT_MAX)  THEN
C--------increase----------------------
         FAC=SCAL_DTP
         TMP=DT_IMP
         DT_IMP=DT_IMP*FAC
         DT_IMP=MIN(DT_IMP,DT_MAX)
         FAC=DT_IMP/TMP
        ENDIF
       ELSEIF (IDTC==2)  THEN
        LU=SQRT(UL2)
        IF (NCYCLE==1)  THEN
         ALEN=LU
         IF (ALEN0>ZERO) ALEN=ALEN0
        ELSE
         IF (ALEN<=EM20) ALEN=LU
         IF (ALEN0>ZERO) ALEN=ALEN0
         TMP=ONE*NL_DTP/IT
         IF (IT>NL_DTP.AND.SCAL_DTN/=ONE) THEN
          IF (ALEN0==ZERO) THEN
           TMP=(NL_DTN-NL_DTP)/(ONE-SCAL_DTN)
           FACL=(TMP-IT+NL_DTP)/TMP
           ALEN=ALEN*FACL
           ALEN=MAX(ALEN,EM01*LU)
           ALEN=MIN(ALEN,LU)
          ENDIF
          FAC=ALEN/MAX(EM20,LU)
          FAC=MIN(ONE,FAC)
         ELSEIF (IT<NL_DTP.AND.NL_DTP/=1) THEN
          IF (ALEN0==ZERO) THEN
           FACL=ONE+(NL_DTP-IT)/(NL_DTP-ONE)
           ALEN=ALEN*FACL
           ALEN=MIN(ALEN,LU*TWO)
          ENDIF
          FAC=ALEN/MAX(EM20,LU)
          FAC=MAX(ONE,FAC)
         ELSEIF (IT==NL_DTP.AND.ALEN0==ZERO) THEN
          ALEN=LU
         ENDIF
        ENDIF 
        TMP=DT_IMP
        IF (FAC>ONE) THEN
         FAC=MIN(SCAL_DTP,FAC)
         DT_IMP=DT_IMP*FAC
         DT_IMP=MIN(DT_IMP,DT_MAX)
         FAC=DT_IMP/TMP
        ELSEIF (FAC<ONE) THEN
         FAC=MAX(SCAL_DTN,FAC)
         DT_IMP=DT_IMP*FAC
         DT_IMP=MAX(DT_IMP,DT_MIN)
         FAC=DT_IMP/TMP
        ENDIF
       ELSEIF (IDTC==3)  THEN
        ILAST=0
        IF (NCYCLE==1)  THEN
         IF (ALEN0>ZERO) THEN
          ALEN=ALEN0
          FAC = ALEN0/SQRT(UL2)
          DT_IMP=DT_IMP*FAC
         ELSE
          ALEN=SQRT(UL2)
         ENDIF
         FACT = ONE
	ELSE !(NCYCLE>1)
C----------time correction ------
         TT = TT + DLA_RIKS
         FACT = ONE+DLA_RIKS/DT2
C------last step-------	 
	 IF (TT>TSTOP) THEN
          ILAST=1
          TMP=DT_IMP
          DT_IMP = MAX(DT_MIN,TMP*EM3)
          FAC=DT_IMP/TMP
          TT =TSTOP-DT_IMP
	 ELSE
          TREST=TSTOP-TT
          IF (ALEN0>ZERO) THEN
           ALEN=ALEN0
           FAC = ONE
          ELSE         
           FACL=SQRT(ONE*NL_DTP/IT)
           TMP=DT_IMP
           IF (FACL>ONE) THEN
            FACL=MIN(SCAL_DTP,FACL)
            DT_IMP=DT_IMP*FACL
            DT_IMP=MIN(DT_IMP,DT_MAX)
            FACL=MIN(DT_IMP/TMP,FACL)
            DT_IMP=MIN(DT_IMP,TREST)
            FAC=DT_IMP/TMP
           ELSEIF (FACL<ONE) THEN
            FACL=MAX(SCAL_DTN,FACL)
            DT_IMP=DT_IMP*FACL
            DT_IMP=MAX(DT_IMP,DT_MIN)
            DT_IMP=MIN(DT_IMP,TREST)
            FAC=DT_IMP/TMP
           ENDIF
	   ALEN=FACL*ALEN
          END IF !IF (ALEN0>ZERO)
         ENDIF !IF (TT>TSTOP) THEN
        ENDIF !(NCYCLE>1)
        DLA_RIKS = FACT*DT2
       ENDIF
      ENDIF
C----------reput old dt du to fix_p ------
      IFIX0 = IDTFIX
      IF (IFIX0>0) THEN
       IDTFIX = 0
      ELSE     
       CALL IMP_DTF(FAC)
      ENDIF
      IF (IMACH/=3.OR.ISPMD==0) THEN
      IF (NPRINT/=0) THEN
       WRITE(IOUT,*)
       IF(NPRINT<0)WRITE(ISTDO,*)
       IF (IDTC==3)  THEN
        IF (FACT/=ONE) THEN
	 WRITE(IOUT,1005) DLA_RIKS
         IF (NPRINT<0)WRITE(ISTDO,1005) DLA_RIKS
        END IF 
       ENDIF
       IF (IFIX0>0.AND.IMCONV==1) THEN
         WRITE(IOUT,1004) 
         IF(NPRINT<0)WRITE(ISTDO,1004) 
       ELSE
        IF (IDTFIX>0) THEN
         WRITE(IOUT,1003) 
         IF(NPRINT<0)WRITE(ISTDO,1003) 
        ENDIF
        IF (FAC>ONE)  THEN
         WRITE(IOUT,1001) FAC
         IF(NPRINT<0)WRITE(ISTDO,1001) FAC
        ELSEIF (FAC<ONE)  THEN
         WRITE(IOUT,1002) FAC
         IF(NPRINT<0)WRITE(ISTDO,1002) FAC
        ENDIF
       ENDIF
      ENDIF
      ENDIF
      IF (IDTC==3.AND.ILAST>0) DLA_RIKS=FAC
      IF (IMCONV<0) FAC=ONE 
      CUMUL_ALEN = CUMUL_ALEN + ALEN
C---------------------------
 1001 FORMAT(5X,'--NEXT TIMESTEP IS INCREASED BY--',E11.4/)
 1002 FORMAT(5X,'--NEXT TIMESTEP IS DECREASED BY--',E11.4/)
 1003 FORMAT(5X,'--NEXT TIMESTEP IS DETERMINED BY INPUT FIX POINT--')
 1004 FORMAT(5X,'--RESET TIMESTEP DUE TO INPUT FIX POINT--')
 1005 FORMAT(5X,'--TIMESTEP IS ADJUSTED BY RIKS METHOD TO:',E11.4/)
 1006 FORMAT(5X,'--TERMINAL LOADING TIME BY RIKS METHOD IS:',E11.4/)
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DTF                       source/implicit/imp_dt.F      
Chd|-- called by -----------
Chd|        IMP_DTN                       source/implicit/imp_dt.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_DTF(FAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFIX
C     REAL
      my_real
     .  FAC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      my_real
     .  TMP,TOLD,TREST
C-----------------------------------------------
      IDTFIX = 0
      K = 0
      DO I=1,NDTFIX
       IF (TT<DTIMPF(I)) THEN
        K = I
        GOTO 100
       ENDIF
      ENDDO
 100  IF (K==0) RETURN
      TREST= TT+DT_IMP- DTIMPF(K)
      TMP = DT_IMP*EM02 
      IF (ABS(TREST)<=TMP) THEN
       FAC = FAC*(DTIMPF(K)-TT)/DT_IMP
       DT_IMP = DTIMPF(K)-TT
      ELSEIF (TREST>0) THEN
       DTIMPF(101)=DT_IMP
       FAC = FAC*(DTIMPF(K)-TT)/DT_IMP
       DT_IMP = DTIMPF(K)-TT
       IDTFIX = 1
      ENDIF                  
C      
      RETURN
      END
