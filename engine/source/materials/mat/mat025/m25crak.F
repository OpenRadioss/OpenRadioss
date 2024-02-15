Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  M25CRAK                       source/materials/mat/mat025/m25crak.F
Chd|-- called by -----------
Chd|        SIGEPS25C                     source/materials/mat/mat025/sigeps25c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M25CRAK(JFT   ,JLT   ,OFF   ,DAMT  ,DIR ,
     2                   ILAYER,IPG   ,THLY  ,DAMCR ,DMAXT ,
     3                   NGL   ,STRP1 ,STRP2 ,PLY_ID,IGTYP ,NEL ,
     4                   EPST1 ,EPST2 ,EPSM1 ,EPSM2 ,DMAX  )
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
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(MVSIZ)
      INTEGER JFT,JLT,IPG,ILAYER,PLY_ID,IGTYP,NEL
      my_real ,INTENT(IN) :: EPST1,EPST2,EPSM1,EPSM2,DMAX
      my_real
     .   DAMT(NEL,2), DIR(NEL,2), OFF(*),
     .   THLY(*), DAMCR(MVSIZ,2), DMAXT(*),
     .   STRP1(*),STRP2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MX, NINDX, IDIR
      INTEGER INDX(MVSIZ)
      my_real :: DAM1, DAM2
C-----------------------------------------------

C.....STRAINS IN ORTHOTROPIC DIRECTIONS
C      DO 20 I=JFT,JLT
C      STRP1(I) = DIR(I,1)*DIR(I,1)*STRN1(I)
C     .          +DIR(I,2)*DIR(I,2)*STRN2(I)
C     .       +2.*DIR(I,1)*DIR(I,2)*STRN3(I)
C      STRP2(I) = DIR(I,2)*DIR(I,2)*STRN1(I)
C     .          +DIR(I,1)*DIR(I,1)*STRN2(I)
C     .       -2.*DIR(I,2)*DIR(I,1)*STRN3(I)
C   20 CONTINUE
C.....GATHER DIRECTION 1
      NINDX=0
      DO 40 I=JFT,JLT
      IF(STRP1(I)<EPST1
     +  .OR.DAMT(I,1)>ZERO.OR.OFF(I)<ONE) GO TO 40
       NINDX=NINDX+1
       INDX(NINDX)=I
   40 CONTINUE
C.....1.FIRST FAILURE DIRECTION 1
      IF(NINDX>0)THEN
       IDIR=1
       DO 50 J=1,NINDX
       I=INDX(J)
       DAM1=(STRP1(I)-EPST1)/(EPSM1-EPST1)
       DAM2= DAM1*EPSM1/STRP1(I)
       DAMT(I,1)= MIN(DAM2,DMAX)
       IF(DAMT(I,1)==DMAX.AND.IMCONV==1)THEN
!
#include "lockon.inc"
         IF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
           WRITE(IOUT, '(A,I1,A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' FAILURE-',IDIR,', ELEMENT #',NGL(I),', INTEGRATION POINT #',IPG,
     +        ', (PLY #',PLY_ID,'), TIME=',TT 
         ELSE
           WRITE(IOUT, '(A,I1,A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' FAILURE-',IDIR,', ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +        ', INTEGRATION POINT #',IPG,', TIME=',TT      
         ENDIF
#include "lockoff.inc"
!
       ENDIF      
   50  CONTINUE
      ENDIF      
C.....GATHER DIRECTION 2
      NINDX=0
      DO 70 I=JFT,JLT
      IF(STRP2(I)<EPST2
     +  .OR.DAMT(I,2)>ZERO.OR.OFF(I)<ONE) GO TO 70
       NINDX=NINDX+1
       INDX(NINDX)=I
   70 CONTINUE
C.....1.FIRST FAILURE DIRECTION 2
      IF(NINDX>0)THEN
       IDIR=2
       DO 80 J=1,NINDX
       I=INDX(J)
       DAM1=(STRP2(I)-EPST2)/(EPSM2-EPST2)
       DAM2= DAM1*EPSM2/STRP2(I)
       DAMT(I,2)= MIN(DAM2,DMAX)
       IF(DAMT(I,2)==DMAX.AND.IMCONV==1)THEN
!
#include "lockon.inc"
         IF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
           WRITE(IOUT, '(A,I1,A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' FAILURE-',IDIR,', ELEMENT #',NGL(I),', INTEGRATION POINT #',IPG,
     +        ', (PLY #',PLY_ID,'), TIME=',TT 
         ELSE
           WRITE(IOUT, '(A,I1,A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' FAILURE-',IDIR,', ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +        ', INTEGRATION POINT #',IPG,', TIME=',TT  
         ENDIF    
#include "lockoff.inc"
!
       ENDIF      
   80  CONTINUE
      ENDIF      
C.....DOMMAGE GLOBAL
      DO 100 I=JFT,JLT
      DAMCR(I,1)=DAMCR(I,1)+THLY(I)*DAMT(I,1)
      DAMCR(I,2)=DAMCR(I,2)+THLY(I)*DAMT(I,2)
      DMAXT(I)  =DMAXT(I)  +THLY(I)*DMAX
  100 CONTINUE
C            
      RETURN
      END
