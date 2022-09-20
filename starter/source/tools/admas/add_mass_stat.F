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
Chd|  ADD_MASS_STAT                 source/tools/admas/add_mass_stat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FIND_DT_TARGET                ../common_source/tools/time_step/find_dt_target.F
Chd|        PLOT_CURVE                    ../common_source/sortie/plot_curve.F
Chd|        TRIRAP                        ../common_source/tools/sort/trirap.F
Chd|        PLOT_CURVE_MOD                ../common_source/modules/plot_curve_mod.F
Chd|====================================================================
      SUBROUTINE ADD_MASS_STAT(MS,IN,STIFN,STIFR,ITAB,TOTMAS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLOT_CURVE_MOD
C-----------------------------------------------
C     TRI DES DT NODAUX ET IMPRESSIONS
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
      INTEGER ITAB(*)
      my_real
     .   MS(NUMNOD),IN(NUMNOD),STIFN(NUMNOD),STIFR(NUMNOD),TOTMAS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,COMPT,K,NVAL
      my_real, DIMENSION(NUMNOD*2),TARGET ::
     .   DT
      my_real, DIMENSION(:),POINTER ::
     .   TMP
      my_real DTSCA,TARGET_DT_TAB(20),PER_ADM_TAB(20),TARGET_DT_TAB2(20),SUMM,SUMK
C=======================================================================
C
C--------------------------------------------------------------------------------------
C     DUPLICATED FROM OUTRIN - STATS ON ADDED MASS + TARGET NODAL TIME STEP ESTIMATION
C--------------------------------------------------------------------------------------
C
      MASS0_START = TOTMAS
      DTSCA = ZEP9
C
      TMP => DT(NUMNOD+1:NUMNOD*2)
C 
      DO I=1,NUMNOD
        IF((MS(I)/=ZERO).AND.(STIFN(I)>EM20))THEN
          DT(I)=MS(I)/STIFN(I)
        ELSE
          DT(I)=EP30       ! -1- free nodes dt=1e30 instead of dt=1416=sqrt(1e6).
                           ! -2- nodal time step from VOID elem (stifn<em20) is dt=EP30 too.
        ENDIF
      ENDDO
C
C  --- Sorting
C
      SUMM = ZERO
      SUMK = ZERO

      DO I=1,NUMNOD
        TMP(I)=I
        IF (DT(I) < EP30) THEN
          SUMM = SUMM + MS(I)
          SUMK = SUMK + STIFN(I)
        ENDIF
      ENDDO
C
      CALL TRIRAP(NUMNOD,DT,NUMNOD)
C
      NVAL = 20
      DO I=1,20
        PER_ADM_TAB(I) = 0.005*I
      ENDDO
C
C----- Step 1 - determination target time step for targeted added mass (from 1% to 10%) - dtsca = 0.9
C
      DTSCA = ZEP9
      CALL FIND_DT_TARGET(MS,STIFN,TARGET_DT_TAB,PER_ADM_TAB,DT,TMP,DTSCA,TOTMAS,NVAL,NUMNOD)
C
C----- Step 2 - determination target time step for targeted added mass (from 1% to 10%) - dtsca = 0.67
C
      DTSCA = ZEP67
      CALL FIND_DT_TARGET(MS,STIFN,TARGET_DT_TAB2,PER_ADM_TAB,DT,TMP,DTSCA,TOTMAS,NVAL,NUMNOD)
C
C  -------------------------------------------------------------------------------------------------------------------------
C
      IF (DT(1) < EP20) THEN       
C
C----- Visual output of added mass estimation - only if nodal time step is significant
C
        WRITE(IOUT,2001)
        WRITE(IOUT,2002)
        CALL PLOT_CURVE(TARGET_DT_TAB, PER_ADM_TAB, 20, INPUT_SIZE_X=60, INPUT_SIZE_Y=24,
     .                  INPUT_TXT_X="TARGET TIME STEP",INPUT_TXT_Y="DM/M - at first cycle - scaling factor = 0.9",INPUT_SYMBOL="*")
C
        WRITE(IOUT,3000) '            DM/M at first cycle        ','Target time step - scaling factor = 0.9',
     .                   'Target time step - scaling factor = 0.67'
        WRITE(IOUT,3000) '  -------------------------------------','---------------------------------------',
     .                   '---------------------------------------'
C
        DO I=1,5
          K = 2*I
          WRITE(IOUT,3001) PER_ADM_TAB(K),TARGET_DT_TAB(K),TARGET_DT_TAB2(K)
        ENDDO
C
        WRITE(IOUT,4001)
        WRITE(IOUT,4002)
C
      ENDIF

C-----------

C-----------
 2001 FORMAT(//,'     INITIAL ADDED MASS ESTIMATION for /DT/NODA/CST')
 2002 FORMAT(   '     ----------------------------------------------',//)
C
 3000 FORMAT(2X,A,2X,'|',2X,A,2X,'|',2X,A)
 3001 FORMAT(17X,1PG9.3,17X,'|',17X,1PG9.3,17X,'|',17X,1PG9.3)
C
 4001 FORMAT(//,'    WARNING : These added mass estimations are valid at first cycle only.')
 4002 FORMAT('    Because of element deformation or contacts the added mass can increase during the run',//)
C----------

      RETURN
      END