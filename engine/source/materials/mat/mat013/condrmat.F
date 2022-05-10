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
Chd|  CONDRMAT                      source/materials/mat/mat013/condrmat.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CONDRMAT(ICODT,ICODR,IRBYM,LNRBYM,ICODRBYM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ICODT(*), ICODR(*), IRBYM(NIRBYM,*),LNRBYM(*),
     .         ICODRBYM(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, M, NSLT, ITX,ITY,ITZ,IRX,IRY,IRZ,
     .        NSL,I,LCOD
C-----------------------------------------------
      NSLT= 0
      DO  M=1,NRBYM
C
          IRX = 0
          IRY = 0
          IRZ = 0 
          ITX = 0
          ITY = 0
          ITZ = 0        
          NSL = IRBYM(2,M)
          DO  I=1, NSL
            N = LNRBYM(NSLT + I)
C rotation bcs             
            LCOD = ICODR(N)
            IF(LCOD > 0) THEN
               ICODR(N)  = 0
               IF(LCOD == 7) THEN
                ICODRBYM(1,M) = LCOD
                IRX = 4
                IRY = 2
                IRZ = 1
               ELSE
                 IF(LCOD == 4 .OR. LCOD == 5 .OR. LCOD == 6) IRX = 4
                 IF(LCOD == 2 .OR. LCOD == 3 .OR. LCOD == 6) IRY = 2
                 IF(LCOD == 1 .OR. LCOD == 3 .OR. LCOD == 5) IRZ = 1
               ENDIF  
            ENDIF          
C translation
            LCOD = ICODT(N)
            IF(LCOD > 0) THEN
              ICODT(N) = 0
               IF(LCOD == 7) THEN
                ICODRBYM(2,M) = LCOD
                ITX = 4
                ITY = 2
                ITZ = 1
               ELSE
                 IF(LCOD == 4 .OR. LCOD == 5 .OR. LCOD == 6) ITX = 4
                 IF(LCOD == 2 .OR. LCOD == 3 .OR. LCOD == 6) ITY = 2
                 IF(LCOD == 1 .OR. LCOD == 3 .OR. LCOD == 5) ITZ = 1
               ENDIF 
            ENDIF 
          ENDDO 
C rotation       
           LCOD = IRX + IRY + IRZ
           ICODRBYM(1,M) = LCOD
C C translation          
           LCOD = ITX + ITY + ITZ
           ICODRBYM(2,M) = LCOD
          NSLT =    NSLT +  NSL   
      ENDDO
C
      RETURN
      END
