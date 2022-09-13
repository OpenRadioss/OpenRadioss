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
Cel|         I10MAIN_TRI                       /intsort/i10main_tri.F
Chd|====================================================================
Chd|  I10XSAVE                      source/interfaces/intsort/i10xsave.F
Chd|-- called by -----------
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I10XSAVE(
     1       X     ,NSV  ,MSR   ,NSN   ,NMN     ,
     2       ITASK ,XSAV ,XMIN  ,YMIN  ,ZMIN    ,
     3       XMAX  ,YMAX ,ZMAX  )
C sauvegarde des XSAV et calcul borne domaine
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,ITASK,
     .        NSV(*),MSR(*)
      my_real
     .        XMAX, YMAX, ZMAX, XMIN, YMIN, ZMIN,
     .        X(3,*), XSAV(3,*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NMNF,NSNL,NMNL,I, J, II
      my_real
     .        XXX, YYY, ZZZ 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      NSNF=1+ITASK*NSN/NTHREAD
      NSNL=(ITASK+1)*NSN/NTHREAD
      NMNF=1+ITASK*NMN/NTHREAD
      NMNL=(ITASK+1)*NMN/NTHREAD
C
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30
C
      IF(NSN+NMN < NUMNOD)THEN
        DO I=NSNF,NSNL
          J=NSV(I)
          IF(J>0)THEN
            XSAV(1,I)=X(1,J)
            XSAV(2,I)=X(2,J)
            XSAV(3,I)=X(3,J)
          END IF
        END DO
        DO I=NMNF,NMNL
          II = I+NSN
          J=MSR(I)
C msr < 0 <=> shooting nodes
          IF(J>0) THEN 
            XMIN= MIN(XMIN,X(1,J))
            YMIN= MIN(YMIN,X(2,J))
            ZMIN= MIN(ZMIN,X(3,J))
            XMAX= MAX(XMAX,X(1,J))
            YMAX= MAX(YMAX,X(2,J))
            ZMAX= MAX(ZMAX,X(3,J))
            XSAV(1,II)=X(1,J)
            XSAV(2,II)=X(2,J)
            XSAV(3,II)=X(3,J)
          ENDIF
        END DO
      ELSE
C
        DO I=NSNF,NSNL
          J=NSV(I)
          IF(J>0)THEN
            XSAV(1,J)=X(1,J)
            XSAV(2,J)=X(2,J)
            XSAV(3,J)=X(3,J)
          END IF
        END DO
        DO I=NMNF,NMNL
          J=MSR(I)
C msr < 0 <=> shooting nodes
          IF(J>0) THEN 
            XMIN= MIN(XMIN,X(1,J))
            YMIN= MIN(YMIN,X(2,J))
            ZMIN= MIN(ZMIN,X(3,J))
            XMAX= MAX(XMAX,X(1,J))
            YMAX= MAX(YMAX,X(2,J))
            ZMAX= MAX(ZMAX,X(3,J))
            XSAV(1,J)=X(1,J)
            XSAV(2,J)=X(2,J)
            XSAV(3,J)=X(3,J)
          END IF
        END DO
      ENDIF
C
      RETURN
      END
