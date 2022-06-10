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
Cgw_clean      SUBROUTINE M27CRAK(JFT,JLT,PM,DAMT,DIR,MATLY,ILAYER,OFF)
Chd|====================================================================
Chd|  M27CRAK                       source/materials/mat/mat027/m27crak.F
Chd|-- called by -----------
Chd|        SIGEPS27C                     source/materials/mat/mat027/sigeps27c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M27CRAK(JFT    ,JLT    ,PM     ,DAMT   ,DIR    ,
     2                   IMAT   ,ILAYER ,OFF    ,NGL    ,NEL    ,
     3                   STRN1  ,STRN2  ,STRN3  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,ILAYER,NEL
      INTEGER NGL(MVSIZ),IMAT
C     REAL
      my_real
     .   PM(NPROPM,*),DAMT(NEL,2),DIR(NEL,2),OFF(*),
     .   STRN1(MVSIZ),STRN2(MVSIZ),STRN3(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,IDIR
      INTEGER INDX(MVSIZ)
C     REAL
      my_real
     .   CC,BB,CR,NORM,
     .   EPST1(MVSIZ),EPST2(MVSIZ),EPSM1(MVSIZ),EPSM2(MVSIZ),
     .   STRP1(MVSIZ),STRP2(MVSIZ),DMAX1(MVSIZ),DMAX2(MVSIZ),
     .   EPSF1(MVSIZ),EPSF2(MVSIZ)
C-----------------------------------------------
      DO I=JFT,JLT
        EPST1(I) = PM(60,IMAT)
        EPST2(I) = PM(61,IMAT)
        EPSM1(I) = PM(62,IMAT)
        EPSM2(I) = PM(63,IMAT)
        DMAX1(I) = PM(64,IMAT)
        DMAX2(I) = PM(65,IMAT)
        EPSF1(I) = PM(66,IMAT)
        EPSF2(I) = PM(67,IMAT)
      ENDDO
C
      DO I=JFT,JLT
        CC = (STRN1(I)+STRN2(I))*HALF
        BB = (STRN1(I)-STRN2(I))*HALF
        CR = SQRT(BB**2+STRN3(I)**2)
        STRP1(I) = CC + CR
        STRP2(I) = CC - CR
      ENDDO
C
C.....GATHER DIRECTION 1
C
      NINDX=0
      DO I=JFT,JLT
        IF (STRP1(I) < EPST1(I) .OR. DAMT(I,1) > ZERO
     .                          .OR. OFF(I) == ZERO) CYCLE
        NINDX=NINDX+1
        INDX(NINDX)=I
      ENDDO
C
      DO I=JFT,JLT
        IF (EPSF1(I) > ZERO .AND. STRP1(I) > EPSF1(I)
     .                      .AND. OFF(I) == ONE) THEN
          OFF(I) = ZERO
#include "lockon.inc"
          WRITE(IOUT, *)
     +        ' TOTAL ELEMENT TENSION FAILURE, ELEMENT #',NGL(I)
#include "lockoff.inc"
          IDEL7NOK = 1
        ENDIF
      ENDDO
C
C.....1.FIRST FAILURE DIRECTION 1
C
      IF (NINDX > 0) THEN
        IDIR = 1
        DO J=1,NINDX
          I = INDX(J)
          DIR(I,1) = STRN3(I)
          DIR(I,2) = STRP1(I) - STRN1(I)
          NORM = SQRT(DIR(I,1)*DIR(I,1)+DIR(I,2)*DIR(I,2))
          IF (NORM < EM8) THEN
            DIR(I,1) = ONE
            DIR(I,2) = ZERO
          ELSE
            DIR(I,1) = DIR(I,1)/NORM
            DIR(I,2) = DIR(I,2)/NORM
          ENDIF
          DAMT(I,1) = (STRP1(I)-EPST1(I))/(EPSM1(I)-EPST1(I))
          DAMT(I,1) =  MIN(DAMT(I,1),DMAX1(I))
#include "lockon.inc"
          WRITE(IOUT, '(A,I1,A,I10,5X,A,I3)')
     +        ' FAILURE-',IDIR,' ELEMENT #',NGL(I),
     +        ' LAYER #',ILAYER     
#include "lockoff.inc"
        ENDDO
      ENDIF      
C
      RETURN
      END
