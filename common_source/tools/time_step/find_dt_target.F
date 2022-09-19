Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  FIND_DT_TARGET                src/lectur1/add_mass_stat.F   
Chd|-- called by -----------
Chd|        ADD_MASS_STAT                 src/lectur1/add_mass_stat.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FIND_DT_TARGET(MS,STIFN,TARGET_DT_TAB,PER_ADM_TAB,DT,TMP,DTSCA,TOTMAS,NVAL,NNOD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVAL,NNOD
      my_real
     .   MS(*),STIFN(*),TOTMAS,TARGET_DT_TAB(*),PER_ADM_TAB(*),DTSCA,DT(*),TMP(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,COMPT,K
      INTEGER :: CPT
      my_real SUMK,SUMK_OLD,SUMM,SUMM_OLD,PER_ADM,TARGET_DT,SEUIL,CHUNK
C=======================================================================
C
C--------------------------------------------------------------------------------------
C     EXTRACTED FROM ADD_MASS_STAT in starter - used in both starter and engine
C--------------------------------------------------------------------------------------
C
      SUMK = ZERO
      SUMM = ZERO
      SUMK_OLD = ZERO
      SUMM_OLD = ZERO
      COMPT = 1
      SEUIL = PER_ADM_TAB(1)
C
      DO I=1,NNOD
        N=NINT(TMP(I))
      ENDDO
C
      DO I=1,NNOD
        IF (I > 1) THEN
          IF (DT(I) > DT(I-1)) THEN
            SUMK_OLD = SUMK
            SUMM_OLD = SUMM
          ENDIF
        ENDIF
        N=NINT(TMP(I))
        PER_ADM = (DT(I)*SUMK_OLD - SUMM_OLD)/(MAX(EM20,TOTMAS))
        IF (I>1) THEN
          DO WHILE ((PER_ADM > SEUIL).AND.(COMPT<=NVAL))
            TARGET_DT_TAB(COMPT) = DTSCA*SQRT(TWO*(TOTMAS*SEUIL+SUMM_OLD)/MAX(EM20,SUMK_OLD))
            COMPT = COMPT+1
            IF(COMPT<=NVAL) SEUIL = PER_ADM_TAB(COMPT)
          ENDDO
          IF (COMPT > NVAL) EXIT
        ENDIF
        SUMK = SUMK + STIFN(N)
        SUMM = SUMM + MS(N)
C-- If thresold are not crossed target dt values must be computed for the last node
        IF (I==NNOD) THEN
          DO K=COMPT,NVAL
            TARGET_DT_TAB(K) = DTSCA*SQRT(TWO*(TOTMAS*SEUIL+SUMM)/MAX(EM20,SUMK))
            IF(K+1 <= NVAL) SEUIL = PER_ADM_TAB(K+1)
          ENDDO
        ENDIF
      ENDDO
C

      RETURN
      END
