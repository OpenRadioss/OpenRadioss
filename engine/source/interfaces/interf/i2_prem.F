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
Chd|  I2_PREM                       source/interfaces/interf/i2_prem.F
Chd|-- called by -----------
Chd|        DIM_NDOF_I                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2_PREM(IPARI,KHIE,NI2,II2,NSMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NI2,II2(*),NSMAX
      INTEGER KHIE
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY,NSN
C-----------------------------------------------
       DO 200 N=NINTER,1,-1
        NTY   =IPARI(7,N)
       IF(NTY.EQ.2.AND.IPARI(26,N).EQ.KHIE) THEN
       NSN = IPARI(5,N)
        IF (NSN.GT.0) THEN
         NI2=NI2+1
         II2(NI2)=N
         NSMAX=NSMAX+NSN
        ENDIF
       ENDIF
  200  CONTINUE
      RETURN
      END

