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
Chd|  NEW_LINK                      source/coupling/rad2rad/new_link.F
Chd|-- called by -----------
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- calls ---------------
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE NEW_LINK(NUM,N,K)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr15_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUM,N,K
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NUM_LINK
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IEX_TEMP        
C-----------------------------------------------     

C----- Stockage de IEXLNK dans IEX_TEMP---------------------------C

       ALLOCATE(IEX_TEMP(5,NR2RLNK+1))

       DO I=1,NR2RLNK
         DO J=1,5
            IEX_TEMP(J,I)=IEXLNK(5*(I-1)+J)
         END DO
       END DO       
       
C----- Stockage des infos du nouveau link

       IEX_TEMP(1,NR2RLNK+1) = NUM 
       IEX_TEMP(3,NR2RLNK+1) = 0
       IEX_TEMP(4,NR2RLNK+1) = N	    	    	                 
       IEX_TEMP(5,NR2RLNK+1) = 4
       IF (K == 2) IEX_TEMP(5,NR2RLNK+1) = 40
       IF ((K == 3).OR.(K == 4)) THEN
         IF (FLG_SWALE.EQ.0) THEN
C--> cas contact avec elts voids cote subdomain - on inverse l'ordre de domaines      
           IEX_TEMP(3,NR2RLNK+1) = N
           IEX_TEMP(4,NR2RLNK+1) = 0       
           IEX_TEMP(5,NR2RLNK+1) = 5
           IF (K == 4) IEX_TEMP(5,NR2RLNK+1) = 50
         ELSE
           IEX_TEMP(5,NR2RLNK+1) = 60
         ENDIF
       ENDIF
                
C----- Generation d'un numero de link

       NUM_LINK = 1
       DO I=1,NR2RLNK
          IF (NUM_LINK.LE.IEX_TEMP(2,I)) NUM_LINK = IEX_TEMP(2,I)+1
       END DO

       IEX_TEMP(2,NR2RLNK+1) = NUM_LINK 
       
C----- Stockage de IEX_TEMP dans IEXLNK---------------------------C
       DEALLOCATE(IEXLNK)
       ALLOCATE(IEXLNK(5*(NR2RLNK+1)))
       NR2RLNK = NR2RLNK+1
	
       DO I=1,NR2RLNK
         DO J=1,5
            IEXLNK(5*(I-1)+J)=IEX_TEMP(J,I)
         END DO
       END DO

C------------------------------------------------------------------

       DEALLOCATE(IEX_TEMP)

C-----------
      RETURN
      END                             
