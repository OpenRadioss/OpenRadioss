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
Chd|  PLOT_CURVE                    sortie/plot_curve.F           
Chd|-- called by -----------
Chd|        ADD_MASS_STAT                 starter/source/tools/admas/add_mass_stat.F
Chd|        OUTRIN                        starter/source/materials/time_step/outri.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   starter/share/message_module/message_mod.F
Chd|        MESSAGE_MOD                   engine/share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE PLOT_CURVE(X, Y, NB_POINTS, INPUT_SIZE_X, INPUT_SIZE_Y, 
     .                      INPUT_X_MINVALUE, INPUT_Y_MINVALUE, INPUT_X_MAXVALUE, 
     .                      INPUT_Y_MAXVALUE, INPUT_SYMBOL, INPUT_CURVE_TYPE,
     .                      INPUT_TXT_X, INPUT_TXT_Y)
c
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     . X(NB_POINTS),Y(NB_POINTS)
      INTEGER NB_POINTS,INPUT_SIZE_X,INPUT_SIZE_Y, INPUT_CURVE_TYPE
      my_real
     . INPUT_X_MINVALUE, INPUT_X_MAXVALUE, INPUT_Y_MINVALUE, INPUT_Y_MAXVALUE
      CHARACTER*1 INPUT_SYMBOL
      CHARACTER(*) INPUT_TXT_X, INPUT_TXT_Y
C-----------------------------------------------
      OPTIONAL ::
     .                 INPUT_SIZE_X, INPUT_SIZE_Y, INPUT_X_MINVALUE, INPUT_Y_MINVALUE, 
     .                 INPUT_X_MAXVALUE, INPUT_Y_MAXVALUE, INPUT_SYMBOL, 
     .                 INPUT_CURVE_TYPE, INPUT_TXT_X, INPUT_TXT_Y
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(len=60) :: FMTA,FMTB,FMTC
      INTEGER KK,FLAG_FMT,I,J,K,IGS,ID,FLAG_FMT_TMP,IFIX_TMP,
     .   ISU,NUMA,J10(10),IGAUSS,IFUNC,UID,SUB_ID,MAX_PART,
     .   CPT_PART,NG,ITY,NEL,NFT,II,MAX_CURVE,CURVE(NB_POINTS),
     .   SIZE_X,SIZE_Y,CURVE_TYPE,COORDX,COORDY,LENA
      my_real
     . MEAN,SD,MEAN_INPUT,SD_INPUT,TEMP,INTERV,
     . VALUE,INCR_X,INCR_Y,
     . X_MINVALUE, X_MAXVALUE, Y_MINVALUE, Y_MAXVALUE
      CHARACTER*ncharline CHAR(100)
      CHARACTER*ncharline CHAR1(100)
      CHARACTER*ncharline CHAR2
      CHARACTER*1 SYMBOL
      CHARACTER*ncharline TXT_X, TXT_Y
      INTEGER, DIMENSION(:,:),  ALLOCATABLE :: GRID
C--------------------------------------------------------------
C GENERIC ROUTINE TO PLOT CURVES OR HISTOGRAMS IN .out files
C--------------------------------------------------------------
C    MANDATORY 
C       NB_POINTS  : NUMBER OF POINTS OF THE CURVE TO PLOT
C       X          : X COORDS
C       Y          : Y COORDS
C    OPTIONAL : 
C       INPUT_SIZE_X : X SIZE (NUMBER OF CHAR) OF THE PLOT, DEFAULT = 100
C       INPUT_SIZE_Y : Y SIZE (NUMBER OF CHAR) OF THE PLOT, DEFAULT = 30
C       INPUT_X_MINVALUE : MIN VALUE OF THE ABSCISSA , DEFAULT : VALUE IS SET AUTOMATICALLY
C       INPUT_Y_MINVALUE : MIN VALUE OF THE ORDINATE , DEFAULT : VALUE IS SET AUTOMATICALLY
C       INPUT_X_MAXVALUE : MAX VALUE OF THE ABSCISSA , DEFAULT : VALUE IS SET AUTOMATICALLY
C       INPUT_Y_MAXVALUE : MAX VALUE OF THE ORDINATE , DEFAULT : VALUE IS SET AUTOMATICALLY
C       INPUT_SYMBOL : CHARACTER TO PLOT THE CURVE/HISTOGRAM, DEFAULT = '#'
C       INPUT_CURVE_TYPE  , DEFAULT = 0'
C             0 : PLOT CURVE
C             1 : PLOT HISTOGRAM
C       INPUT_TXT_X : ABSCISSA LEGEND , DEFAULT : NONE
C       INPUT_TXT_Y : ORDINATE LEGEND , DEFAULT : NONE
C=======================================================================
       IF ( .NOT. PRESENT(INPUT_SIZE_X)) THEN 
         SIZE_X = 100
       ELSE
         SIZE_X = INPUT_SIZE_X
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_SIZE_Y)) THEN
         SIZE_Y = 30
       ELSE
         SIZE_Y = INPUT_SIZE_Y
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_X_MINVALUE)) THEN
         X_MINVALUE = EP20
         DO I=1,NB_POINTS
           X_MINVALUE = MIN(X(I),X_MINVALUE)
         ENDDO
       ELSE
         X_MINVALUE = INPUT_X_MINVALUE
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_Y_MINVALUE)) THEN
         Y_MINVALUE = EP20
         DO I=1,NB_POINTS
           Y_MINVALUE = MIN(Y(I),Y_MINVALUE)
         ENDDO
       ELSE
         Y_MINVALUE = INPUT_Y_MINVALUE
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_X_MAXVALUE)) THEN
         X_MAXVALUE = -EP20
         DO I=1,NB_POINTS
           X_MAXVALUE = MAX(X(I),X_MAXVALUE)
         ENDDO
       ELSE
         X_MAXVALUE = INPUT_X_MAXVALUE
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_Y_MAXVALUE)) THEN
         Y_MAXVALUE = -EP20
         DO I=1,NB_POINTS
           Y_MAXVALUE = MAX(Y(I),Y_MAXVALUE)
         ENDDO
       ELSE
         Y_MAXVALUE = INPUT_Y_MAXVALUE
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_SYMBOL)) THEN
         SYMBOL(1:1) = '#'
       ELSE
         SYMBOL(1:1) = INPUT_SYMBOL(1:1)
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_CURVE_TYPE)) THEN
         CURVE_TYPE = 0
       ELSE
         CURVE_TYPE = INPUT_CURVE_TYPE
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_TXT_X)) THEN
         TXT_X = ''
       ELSE
         TXT_X = ''
         TXT_X(1:LEN_TRIM(INPUT_TXT_X)) = INPUT_TXT_X(1:LEN_TRIM(INPUT_TXT_X))
       ENDIF
c
       IF ( .NOT. PRESENT(INPUT_TXT_Y)) THEN
         TXT_Y = ''
       ELSE
         TXT_Y = ''
         TXT_Y(1:LEN_TRIM(INPUT_TXT_Y)) = INPUT_TXT_Y(1:LEN_TRIM(INPUT_TXT_Y))
       ENDIF
C=======================================================================
       CHAR=''
       CHAR1=''
       CHAR2=''
       CURVE(1:NB_POINTS) = 0
       INCR_X = (X_MAXVALUE-X_MINVALUE)/(SIZE_X-1)
       INCR_Y = (Y_MAXVALUE-Y_MINVALUE)/(SIZE_Y-1)
c
       ALLOCATE(GRID(SIZE_X,SIZE_Y))
       DO J = 1, SIZE_Y
         DO I = 1, SIZE_X
           GRID(I,J) = 0
         ENDDO
       ENDDO
c
       IF(CURVE_TYPE == 0)THEN
         DO I=1,NB_POINTS
           COORDX = MIN(SIZE_X,MAX(1,INT((X(I) - X_MINVALUE)/INCR_X)))
           COORDY = MIN(SIZE_Y,MAX(1,INT((Y(I) - Y_MINVALUE)/INCR_Y)))
           GRID(COORDX,COORDY) = 1
         ENDDO
       ELSEIF(CURVE_TYPE == 1)THEN
         DO I=1,NB_POINTS
           COORDX = MIN(SIZE_X,MAX(1,INT((X(I) - X_MINVALUE)/INCR_X)))
           COORDY = MIN(SIZE_Y,MAX(1,INT((Y(I) - Y_MINVALUE)/INCR_Y)))
           DO J=1,COORDY
             GRID(COORDX,J) = 1
           ENDDO
         ENDDO
       ENDIF
c
       DO J = 1, SIZE_Y
         CHAR(J)= " "
       ENDDO
       DO J = 1, SIZE_Y
         DO I = 1, SIZE_X
           IF( GRID(I,SIZE_Y+1-J) == 1) CHAR(J)(I:I)= SYMBOL(1:1)
         ENDDO
       ENDDO

       DEALLOCATE(GRID)

       IF(PRESENT(INPUT_TXT_Y)) THEN
         WRITE(IOUT,'(12X,A)') TXT_Y(1:LEN_TRIM(TXT_Y))
       ENDIF
C
       WRITE(IOUT,'(19X,A)') '^'
       WRITE(IOUT,'(19X,A,A)') '|',CHAR(1)(1:SIZE_X)
       WRITE(IOUT,'(9X,F7.3,1X,A,A,A)')   Y_MAXVALUE,'--','|',CHAR(2)(1:SIZE_X)
       DO I = 3,SIZE_Y-1
         WRITE(IOUT,'(19X,A,A)') '|',CHAR(I)(1:SIZE_X)
       ENDDO
       WRITE(IOUT,'(9X,F7.3,1X,A,A,A)')   Y_MINVALUE,'--','|',CHAR(SIZE_Y)(1:SIZE_X)
C
       DO II = 1,SIZE_X+2
          CHAR2(II:II) = '-'
       ENDDO
       CHAR2(SIZE_X+2:SIZE_X+3) = '>'
       IF(PRESENT(INPUT_TXT_X) ) THEN
         CHAR2(SIZE_X+4:SIZE_X+3+LEN_TRIM(TXT_X)) = TXT_X(1:LEN_TRIM(TXT_X))
       ENDIF
C
       LENA = LEN_TRIM(CHAR2)
       WRITE(IOUT,'(20X,A)') CHAR2(1:LENA)
c
       CHAR2 = ' '
       CHAR2(1:1)=  '|'
       CHAR2(SIZE_X:SIZE_X)=  '|'
       LENA = LEN_TRIM(CHAR2)
       WRITE(IOUT,'(20X,A)') CHAR2(1:LENA)
C
       IF (SIZE_X <= 10) THEN 
         FMTA='(14X,1PG11.3,5X,1PG11.3)'
       ELSEIF (SIZE_X <= 20) THEN 
         FMTA='(14X,1PG11.3,8X,1PG11.3)'
       ELSEIF (SIZE_X <= 30) THEN 
         FMTA='(14X,1PG11.3,18X,1PG11.3)'
       ELSEIF (SIZE_X <= 40) THEN 
         FMTA='(14X,1PG11.3,28X,1PG11.3)'
       ELSEIF (SIZE_X <= 50) THEN 
         FMTA='(14X,1PG11.3,38X,1PG11.3)'
       ELSEIF (SIZE_X <= 60) THEN 
         FMTA='(14X,1PG11.3,48X,1PG11.3)'
       ELSEIF (SIZE_X <= 70) THEN 
         FMTA='(14X,1PG11.3,58X,1PG11.3)'
       ELSEIF (SIZE_X <= 80) THEN 
         FMTA='(14X,1PG11.3,68X,1PG11.3)'
       ELSEIF (SIZE_X <= 90) THEN 
         FMTA='(14X,1PG11.3,78X,1PG11.3)'
       ELSE
         FMTA='(14 X,1PG11.3,85X,1PG11.3)'
       ENDIF
c
       WRITE(IOUT,FMT=FMTA) X_MINVALUE,X_MAXVALUE
c
       WRITE(IOUT,*) ' '
       WRITE(IOUT,*) ' '
C------------------------------ 
      RETURN
      END
