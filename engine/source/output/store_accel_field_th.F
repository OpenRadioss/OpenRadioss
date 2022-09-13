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
Chd|  STORE_ACCEL_FIELD_TH          source/output/store_accel_field_th.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STORE_ACCEL_FIELD_TH(NODFT ,NODLT ,A     ,AR    ,MS    ,
     2                                IN    ,FTHREAC ,IFLAG ,NODREAC)
C-----------------------------------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT, IFLAG
      INTEGER NODREAC(*)
      my_real
     .   A(3,NUMNOD) , AR(3,NUMNOD), MS(NUMNOD), IN(IRODDL*NUMNOD), 
     .   FTHREAC(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !-------------------------------------------------------------!
      !       COMPUTE REACTION FORCES for /TH/NODE (FINALIZATION)   !
      !-------------------------------------------------------------!

      DO N=NODFT,NODLT
        IF (NODREAC(N) > 0) THEN
          FTHREAC(1,NODREAC(N)) = FTHREAC(1,NODREAC(N)) + IFLAG * MS(N)*A(1,N)*DT12
          FTHREAC(2,NODREAC(N)) = FTHREAC(2,NODREAC(N)) + IFLAG * MS(N)*A(2,N)*DT12
          FTHREAC(3,NODREAC(N)) = FTHREAC(3,NODREAC(N)) + IFLAG * MS(N)*A(3,N)*DT12
        ENDIF ! IF (NODREAC(N) > 0)
      ENDDO

      IF (IRODDL/=0) THEN
        DO N=NODFT,NODLT
          IF (NODREAC(N) > 0) THEN
            FTHREAC(4,NODREAC(N)) = FTHREAC(4,NODREAC(N)) + IFLAG * IN(N)*AR(1,N)*DT12
            FTHREAC(5,NODREAC(N)) = FTHREAC(5,NODREAC(N)) + IFLAG * IN(N)*AR(2,N)*DT12
            FTHREAC(6,NODREAC(N)) = FTHREAC(6,NODREAC(N)) + IFLAG * IN(N)*AR(3,N)*DT12
          ENDIF ! IF (NODREAC(N) > 0)
        ENDDO
      END IF

      !------------------------------------------------!

      RETURN
      END
