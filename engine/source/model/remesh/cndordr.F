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
Chd|  CNDORDR                       source/model/remesh/cndordr.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE CNDORDR(IPART,IPARTC,IPARTTG,SH4TREE,SH3TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .        SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,NN,LEVEL,IP,PTR,SON
C-----------------------------------------------
      PSH4UPL(0)=0
      PTR=0
      DO N=1,NUMELC
        IP=IPARTC(N)
        IF(IPART(10,IP) > 0)THEN
          IF(SH4TREE(3,N) == -1)THEN
            PTR=PTR+1
            LSH4UPL(PTR)=N
          END IF
        END IF
      END DO

      DO LEVEL=0,LEVELMAX-1
        PSH4UPL(LEVEL+1)=PTR

        DO NN=PSH4UPL(LEVEL)+1,PSH4UPL(LEVEL+1)
          N    =LSH4UPL(NN)
          SON  =SH4TREE(2,N)
          IF(SH4TREE(3,SON) < 0)THEN
            PTR=PTR+1
            LSH4UPL(PTR)=SON
          END IF
          IF(SH4TREE(3,SON+1) < 0)THEN
            PTR=PTR+1
            LSH4UPL(PTR)=SON+1
          END IF
          IF(SH4TREE(3,SON+2) < 0)THEN
            PTR=PTR+1
            LSH4UPL(PTR)=SON+2
          END IF
          IF(SH4TREE(3,SON+3) < 0)THEN
            PTR=PTR+1
            LSH4UPL(PTR)=SON+3
          END IF
        END DO

      END DO
C--------------------------------------------
C     TRIANGLES
C--------------------------------------------
      PSH3UPL(0)=0
      PTR=0
      DO N=1,NUMELTG
        IP=IPARTTG(N)
        IF(IPART(10,IP) > 0)THEN
          IF(SH3TREE(3,N) == -1)THEN
            PTR=PTR+1
            LSH3UPL(PTR)=N
          END IF
        END IF
      END DO

      DO LEVEL=0,LEVELMAX-1
        PSH3UPL(LEVEL+1)=PTR

        DO NN=PSH3UPL(LEVEL)+1,PSH3UPL(LEVEL+1)
          N    =LSH3UPL(NN)
          SON  =SH3TREE(2,N)
          IF(SH3TREE(3,SON) < 0)THEN
            PTR=PTR+1
            LSH3UPL(PTR)=SON
          END IF
          IF(SH3TREE(3,SON+1) < 0)THEN
            PTR=PTR+1
            LSH3UPL(PTR)=SON+1
          END IF
          IF(SH3TREE(3,SON+2) < 0)THEN
            PTR=PTR+1
            LSH3UPL(PTR)=SON+2
          END IF
          IF(SH3TREE(3,SON+3) < 0)THEN
            PTR=PTR+1
            LSH3UPL(PTR)=SON+3
          END IF
        END DO
      END DO

      RETURN
      END
