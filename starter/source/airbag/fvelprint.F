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
Chd|  FVELPRINT                     source/airbag/fvelprint.F     
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVELPRINT(IXC,  IXTG,  NEL,  ITAGEL,  ELTG, 
     .                     IBAGHOL,  ILVOUT,  IFLAG )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*)
      INTEGER, DIMENSION(NEL), INTENT(IN) :: ELTG
      INTEGER, DIMENSION(NEL), INTENT(IN) :: ITAGEL
      INTEGER NEL, ILVOUT, IFLAG
      INTEGER IBAGHOL(NIBHOL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IFOUND
      INTEGER IVENT,IVENTYP
C--------------------------
C OUTPUT
C--------------------------
      WRITE(IOUT,1000)
      DO I=1,NEL
         IFOUND=ELTG(I)
         IF (IFOUND.GT.NUMELQ.AND.IFOUND.LE.NUMELQ+NUMELC) THEN
            IF (IFLAG == 0.AND.ITAGEL(I) == 0.AND.ILVOUT >= 2) THEN
                WRITE(IOUT,1100) I,IXC(NIXC,IFOUND-NUMELQ)
            ELSEIF (ITAGEL(I) > 0) THEN
                WRITE(IOUT,1101) I,IXC(NIXC,IFOUND-NUMELQ),ITAGEL(I)
            ELSEIF (IFLAG == 0.AND.ITAGEL(I) < 0) THEN
                IVENT  =-ITAGEL(I)
                IVENTYP=IBAGHOL(13,IVENT)
                IF(IVENTYP == 0) THEN
                   WRITE(IOUT,1102) I,IXC(NIXC,IFOUND-NUMELQ),IVENT
                ELSE
                   WRITE(IOUT,1103) I,IXC(NIXC,IFOUND-NUMELQ),IVENT
                ENDIF
            ENDIF
         ELSEIF (IFOUND.GT.NUMELQ+NUMELC) THEN
            IF (IFLAG == 0.AND.ITAGEL(I) == 0.AND.ILVOUT >= 2) THEN
                WRITE(IOUT,1110) I,IXTG(NIXTG,IFOUND-NUMELQ-NUMELC)
            ELSEIF (ITAGEL(I) > 0) THEN
                WRITE(IOUT,1111) 
     .                I,IXTG(NIXTG,IFOUND-NUMELQ-NUMELC),ITAGEL(I)
            ELSEIF (IFLAG == 0.AND.ITAGEL(I) < 0) THEN
                IVENT  =-ITAGEL(I)
                IVENTYP=IBAGHOL(13,IVENT)
                IF(IVENTYP == 0) THEN
			       WRITE(IOUT,1112) I,IXTG(NIXTG,IFOUND-NUMELQ-NUMELC),IVENT
                ELSE
			       WRITE(IOUT,1113) I,IXTG(NIXTG,IFOUND-NUMELQ-NUMELC),IVENT
                ENDIF
            ENDIF
         ENDIF
      ENDDO
C
 1000 FORMAT(/5X,'FVMBAG - AIRBAG ELEMENTS <-> SHELL or SH3N ELEMENTS'
     .       /5X,'---------------------------------------------------')
 1100 FORMAT(20X,'ELEM: ',I8,' <-> SHELL: ',I10)
 1101 FORMAT(20X,'ELEM: ',I8,' <-> SHELL: ',I10,' - INFLATOR : ',I8)
 1102 FORMAT(20X,'ELEM: ',I8,' <-> SHELL: ',I10,' - VENT HOLE: ',I8)
 1103 FORMAT(20X,'ELEM: ',I8,' <-> SHELL: ',I10,' - POROUS SURFACE: ',I8)
 1110 FORMAT(20X,'ELEM: ',I8,' <-> SH3N : ',I10)
 1111 FORMAT(20X,'ELEM: ',I8,' <-> SH3N : ',I10,' - INFLATOR : ',I8)
 1112 FORMAT(20X,'ELEM: ',I8,' <-> SH3N : ',I10,' - VENT HOLE: ',I8)
 1113 FORMAT(20X,'ELEM: ',I8,' <-> SH3N : ',I10,' - POROUS SURFACE: ',I8)
C     
      RETURN
      END
