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
Chd|  MVOLUDT                       source/airbag/mvoludt.F       
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MVOLUDT(MONVOL,VOLMON)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*)
      my_real
     .        VOLMON(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITYP, K1, KK1
C     REAL ou REAL*8
      my_real
     .   DTX, DTY, GAMA, P, V, S, ROT, AL
C-----------------------------------------------
C
      DTX=UN
      DTY=UN
      K1 = 1
      KK1 = 0
      DO I=1,NVOLU
       ITYP=MONVOL(K1+1)
C communication dans resol juste avant mvoludt et envoi a proc0 des volmon
C evite difference en parallele quand processeur ne traite pas ts les airbags
C voir formules sur min dty / max dtx
C       IF(IMACH.EQ.3) THEN
C debranchement a part pour le proc main (proc ayant a jour p,v,s)
C         IF(FR_MV(NSPMD+2,I).NE.ISPMD+1) GOTO 100
C       END IF
       IF(ITYP.NE.1)THEN
        GAMA=VOLMON(KK1+1)
        AL=VOLMON(KK1+2)
        P=VOLMON(KK1+12)
        V=VOLMON(KK1+16)-VOLMON(KK1+5)
        S=VOLMON(KK1+18)
        ROT=VOLMON(KK1+21)
        DTY=MIN(SQRT(UN+AL)-AL,DTY)
        IF(ROT.EQ.ZERO) THEN
          DTX=DTY
        ELSE
          DTX=MAX(SQRT(UN+DT2**2*GAMA*P*S/V/ROT),DTX)
        ENDIF
       ENDIF
 100   CONTINUE
       K1  = K1  + NIMV
       KK1 = KK1 + NRVOLU
      ENDDO
C
      DT2=DT2*DTY/DTX
      RETURN
      END
