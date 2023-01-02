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
Chd|  I21XSAVE                      source/interfaces/int21/i21xsave.F
Chd|-- called by -----------
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE I21XSAVE(
     1       X     ,NSV  ,NSN   ,ITASK ,XSAV ,
     2       DD    ,IROT ,ROT   ,BRACKET)
C sauvegarde des XSAV
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,ITASK,NSV(*),IROT
      my_real
     .   X(3,*), XSAV(3,*), DD(3), ROT(9), BRACKET(3)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NSNL,I, J, II
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      NSNF=1+ITASK*NSN/NTHREAD
      NSNL=(ITASK+1)*NSN/NTHREAD
C
      DO I=NSNF,NSNL
C                J=NSV(I)    ! shooting nodes int11
       J=ABS(NSV(I))
       XSAV(1,I)=X(1,J)
       XSAV(2,I)=X(2,J)
       XSAV(3,I)=X(3,J)
      END DO
C
      XSAV(1,NSN+1)=DD(1)
      XSAV(2,NSN+1)=DD(2)
      XSAV(3,NSN+1)=DD(3)
      IF(IROT/=0)THEN
        XSAV(1,NSN+2)=
     .     ROT(1)*BRACKET(1)+ROT(4)*BRACKET(2)+ROT(7)*BRACKET(3)
        XSAV(2,NSN+2)=
     .     ROT(2)*BRACKET(1)+ROT(5)*BRACKET(2)+ROT(8)*BRACKET(3)
        XSAV(3,NSN+2)=
     .     ROT(3)*BRACKET(1)+ROT(6)*BRACKET(2)+ROT(9)*BRACKET(3)
      ELSE
        XSAV(1,NSN+2)=ZERO
        XSAV(2,NSN+2)=ZERO
        XSAV(3,NSN+2)=ZERO 
      END IF
C
      RETURN
      END
