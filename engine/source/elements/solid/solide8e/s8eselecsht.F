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
Chd|  S8ESELECSHT                   source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ESELECSHT(
     1   NPTR,    NPTS,    NPTT,    NNPT,
     2   MFXX,    MFXY,    MFXZ,    MFYX,
     3   MFYY,    MFYZ,    MFZX,    MFZY,
     4   MFZZ,    BXX,     BYY,     BZZ,
     5   BXY,     BYZ,     BXZ,     I_SH,
     6   OFFG,    NEL,     ISMSTR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER NPTR,NPTS,NPTT,NNPT ,I_SH
C     REAL
      my_real
     .  MFXX(MVSIZ,NNPT),MFXY(MVSIZ,NNPT),MFXZ(MVSIZ,NNPT),
     .  MFYX(MVSIZ,NNPT),MFYY(MVSIZ,NNPT),MFYZ(MVSIZ,NNPT),
     .  MFZX(MVSIZ,NNPT),MFZY(MVSIZ,NNPT),MFZZ(MVSIZ,NNPT),
     .  BXX(MVSIZ,NNPT),BYY(MVSIZ,NNPT),BZZ(MVSIZ,NNPT),
     .  BXY(MVSIZ,NNPT),BXZ(MVSIZ,NNPT),BYZ(MVSIZ,NNPT),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IR,IS,IT,IP
      my_real
     .  B0XY(MVSIZ,2),B0XZ(MVSIZ,2),B0YZ(MVSIZ,2),
     .  B0YX(MVSIZ,2),B0ZX(MVSIZ,2),B0ZY(MVSIZ,2)
C------------
       IF (ISMSTR/=11) THEN      
         DO IP = 1,NNPT
           DO I=1,NEL
C ----[B]=[MF+1][MF+1]^t -1 
            IF (ISMSTR==12.AND.OFFG(I) >ONE) CYCLE 
            BXX(I,IP)=MFXX(I,IP)*(TWO+MFXX(I,IP))+
     .                MFXY(I,IP)*MFXY(I,IP)+MFXZ(I,IP)*MFXZ(I,IP) 
            BYY(I,IP)=MFYY(I,IP)*(TWO+MFYY(I,IP))+
     .                MFYX(I,IP)*MFYX(I,IP)+MFYZ(I,IP)*MFYZ(I,IP)
            BZZ(I,IP)=MFZZ(I,IP)*(TWO+MFZZ(I,IP))+
     .                MFZX(I,IP)*MFZX(I,IP)+MFZY(I,IP)*MFZY(I,IP)
            BXY(I,IP)=MFXY(I,IP)+MFYX(I,IP)+MFXX(I,IP)*MFYX(I,IP)+
     .                MFXY(I,IP)*MFYY(I,IP)+MFXZ(I,IP)*MFYZ(I,IP)
            BXZ(I,IP)=MFXZ(I,IP)+MFZX(I,IP)+MFXX(I,IP)*MFZX(I,IP)+
     .                MFXY(I,IP)*MFZY(I,IP)+MFXZ(I,IP)*MFZZ(I,IP)
            BYZ(I,IP)=MFZY(I,IP)+MFYZ(I,IP)+MFZX(I,IP)*MFYX(I,IP)+
     .                MFZY(I,IP)*MFYY(I,IP)+MFZZ(I,IP)*MFYZ(I,IP)  
           ENDDO
         END DO !IP = 1
       END IF !(ISMSTR/=11) THEN               
C	
        DO IR = 1,2
          B0XY(1:NEL,IR) = ZERO
          B0XZ(1:NEL,IR) = ZERO
          B0YZ(1:NEL,IR) = ZERO
        END DO !IR = 1,2
C	
      IF (ISMSTR==11) THEN                           
        DO IR = 1,2
          B0YX(1:NEL,IR) = ZERO
          B0ZX(1:NEL,IR) = ZERO
          B0ZY(1:NEL,IR) = ZERO
        END DO !IR = 1,2
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
C------ moyen in 3 directions
         DO I=1,NEL
           B0XY(I,IR) = B0XY(I,IR)+MFXY(I,IP)
           B0XZ(I,IT) = B0XZ(I,IT)+MFXZ(I,IP)
           B0YZ(I,IS) = B0YZ(I,IS)+MFYZ(I,IP)
           B0YX(I,IR) = B0YX(I,IR)+MFYX(I,IP)
           B0ZX(I,IT) = B0ZX(I,IT)+MFZX(I,IP)
           B0ZY(I,IS) = B0ZY(I,IS)+MFZY(I,IP)
         END DO
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
C       
       DO IR = 1,2
         DO I=1,NEL
           B0XY(I,IR) = B0XY(I,IR)*FOURTH
           B0XZ(I,IR) = B0XZ(I,IR)*FOURTH
           B0YZ(I,IR) = B0YZ(I,IR)*FOURTH
           B0YX(I,IR) = B0YX(I,IR)*FOURTH
           B0ZX(I,IR) = B0ZX(I,IR)*FOURTH
           B0ZY(I,IR) = B0ZY(I,IR)*FOURTH
         END DO
       END DO !IR = 1,2
C---------takes the selective mean values-----
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
         DO I=1,NEL
           MFXY(I,IP) = B0XY(I,IR)
           MFXZ(I,IP) = B0XZ(I,IT)
           MFYZ(I,IP) = B0YZ(I,IS)
           MFYX(I,IP) = B0YX(I,IR)
           MFZX(I,IP) = B0ZX(I,IT)
           MFZY(I,IP) = B0ZY(I,IS)
         END DO
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
      ELSE                                     
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
C------ moyen in 3 directions
         DO I=1,NEL
           IF (ISMSTR==12.AND.OFFG(I) >ONE) CYCLE 
           B0XY(I,IR) = B0XY(I,IR)+BXY(I,IP)
           B0XZ(I,IT) = B0XZ(I,IT)+BXZ(I,IP)
           B0YZ(I,IS) = B0YZ(I,IS)+BYZ(I,IP)
         END DO
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
C       
       DO IR = 1,2
         DO I=1,NEL
           IF (ISMSTR==12.AND.OFFG(I) >ONE) CYCLE 
           B0XY(I,IR) = B0XY(I,IR)*FOURTH
           B0XZ(I,IR) = B0XZ(I,IR)*FOURTH
           B0YZ(I,IR) = B0YZ(I,IR)*FOURTH
         END DO
       END DO !IR = 1,2
C---------takes the selective mean values-----
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
         DO I=1,NEL
           IF (ISMSTR==12.AND.OFFG(I) >ONE) CYCLE 
           BXY(I,IP) = B0XY(I,IR)
           BXZ(I,IP) = B0XZ(I,IT)
           BYZ(I,IP) = B0YZ(I,IS)
         END DO
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
      END IF !(ISMSTR==11) THEN  
C       
      RETURN
       END
