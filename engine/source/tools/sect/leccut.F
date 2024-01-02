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
Chd|  LECCUT                        source/tools/sect/leccut.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        SYSFUS                        source/system/sysfus.F        
Chd|====================================================================
      SUBROUTINE LECCUT(ICUT,XCUT,ITABM1)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICUT(44,*),ITABM1(*)
C     REAL
      my_real
     .   XCUT(7,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scrcut_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,ITYP
C     REAL
      my_real
     . X0(3),VN(3),VIT,VNN  
      CHARACTER IDTITL*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real
     .   SYSFUS
C-----------------------------------------------
      WRITE(IOUT,2000)NCUTS
      DO 100 I=1,NCUTS
        READ(IIN,'(I8,A40)')ITYP,IDTITL
        IF(ITYP==0)ITYP=1
        WRITE(IOUT,1100)IDTITL,ITYP
        ICUT(1,I)=ITYP
        DO 10 J=1,40
 10     ICUT(4+J,I) = ICHAR(IDTITL(J:J))
        IF(ITYP<3)THEN
          READ(IIN,'(7E16.0)')(X0(J),J=1,3),(VN(J),J=1,3),VIT
          IF(ITYP==2)VIT=0
          XCUT(7,I)=VIT
          DO 20 J=1,3
 20         XCUT(J,I)=X0(J)
          VNN=SQRT(VN(1)**2+VN(2)**2+VN(3)**2)
          IF(VNN==ZERO)THEN
            WRITE(ISTDO,1000) IDTITL
            VN(1)=1.
          ELSE
            VN(1)=VN(1)/VNN
            VN(2)=VN(2)/VNN
            VN(3)=VN(3)/VNN
          ENDIF
          DO 30 J=1,3
            ICUT(J+1,I)=0
 30         XCUT(J+3,I)=VN(J)
          WRITE(IOUT,1200)(X0(J),J=1,3),(VN(J),J=1,3),VIT
         ELSE
          READ(IIN,'(3I8)')(ICUT(J,I),J=2,4)
          WRITE(IOUT,1300)(ICUT(J,I),J=2,4)          
          DO40 J=1,7
 40         XCUT(J,I)=ZERO
          DO45 J=2,4
            ICUT(J,I)=SYSFUS(ICUT(J,I),ITABM1,NUMNOD,' ** ERROR ** CUT')
 45       CONTINUE 
         ENDIF
 100  CONTINUE     
C
      RETURN
C
 1000 FORMAT(//' ** ERROR CUT ',A40,' ZERO VECTOR NORM')
 2000 FORMAT(' NUMBER OF SOLID CUTS . . . . . . .', I8 //)
 1100 FORMAT(//' CUT ',A40/
     .          ' TYPE  . . . . . . . . . . . . =',I5/
     .          '     =1 PLANE CUT OF DEFORMED GEOMETRY'/
     .          '     =2 DEFORMED CUT OF UNDEFORMED GEOMETRY'/
     .          '     =3 PLANE CUT GIVEN BY 3 REFERENCE NODES')
 1200 FORMAT(
     .          ' REFERENCE COORDINATES . . . X =',E12.4/
     .          ' . . . . . . . . . .  . . . .Y =',E12.4/
     .          ' . . . . . . . . . .  . . . .Z =',E12.4/
     .          ' REFERENCE VECTOR  . . . . . X =',E12.4/
     .          ' . . . . . . . . . . . . . . Y =',E12.4/
     .          ' . . . . . . . . . . . . . . Z =',E12.4/
     .          ' VELOCITY  . . . . . . . . VIT =',E12.4)
 1300 FORMAT(
     .          ' REFERENCE NODE . . . . . . 1 =',I8/
     .          ' . . . . . . . . . . . . . .2 =',I8/
     .          ' . . . . . . . . . . . . . .3 =',I8)
      END
