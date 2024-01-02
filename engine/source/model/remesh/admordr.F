Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ADMORDR                       source/model/remesh/admordr.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMORDR(SH4TREE,SH3TREE,IXC,IXTG)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),
     .        IXC(NIXC,*), IXTG(NIXTG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,NN,LEVEL,IP,NLEV,PTR,SON,
     .        J,NI,IERR
      INTEGER LTMP4(NSH4ACT),LTMP3(NSH3ACT)
      INTEGER WORK(70000),
     .  ITRI(MAX(NSH4ACT,NSH3ACT)), INDEX(2*MAX(NSH4ACT,NSH3ACT))
C-----------------------------------------------
      IF(IPARIT/=0)THEN

        DO NN = 1, NSH4ACT
          ITRI(NN) = IXC(NIXC,LSH4ACT(NN))
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NSH4ACT,1)
        LTMP4(1:NSH4ACT)=LSH4ACT(1:NSH4ACT)
        DO NN = 1, NSH4ACT
          LSH4ACT(NN)=LTMP4(INDEX(NN))
        END DO

        DO NN = 1, NSH3ACT
          ITRI(NN) = IXTG(NIXTG,LSH3ACT(NN))
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NSH3ACT,1)
        LTMP3(1:NSH3ACT)=LSH3ACT(1:NSH3ACT)
        DO NN = 1, NSH3ACT
          LSH3ACT(NN)=LTMP3(INDEX(NN))
        END DO

      END IF
    
      PSH4ACT=0

      DO NN=1,NSH4ACT
        N    =LSH4ACT(NN)
        LEVEL=SH4TREE(3,N)
        PSH4ACT(LEVEL)=PSH4ACT(LEVEL)+1
        LTMP4(NN)=N
      END DO 
C
      DO LEVEL=1,LEVELMAX
        PSH4ACT(LEVEL)=PSH4ACT(LEVEL)+PSH4ACT(LEVEL-1)
      END DO
C
      DO LEVEL=LEVELMAX+1,1,-1
        PSH4ACT(LEVEL)=PSH4ACT(LEVEL-1)
      END DO
      PSH4ACT(0)=0
C
      DO NN=1,NSH4ACT
        N    =LTMP4(NN)
        LEVEL=SH4TREE(3,N)
        PSH4ACT(LEVEL)=PSH4ACT(LEVEL)+1
        LSH4ACT(PSH4ACT(LEVEL))=N
      END DO
C
      DO LEVEL=LEVELMAX+1,1,-1
        PSH4ACT(LEVEL)=PSH4ACT(LEVEL-1)
      END DO
      PSH4ACT(0)=0
C
      PSH4KIN(0)=0
      LSH4KIN(1:PSH4ACT(1))=LSH4ACT(1:PSH4ACT(1))

      PTR = PSH4ACT(1)
      DO LEVEL=0,LEVELMAX-1
        PSH4KIN(LEVEL+1)=PTR
        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)
          SON=SH4TREE(2,N)
          PTR=PTR+1
          LSH4KIN(PTR)=SON
          PTR=PTR+1
          LSH4KIN(PTR)=SON+1
          PTR=PTR+1
          LSH4KIN(PTR)=SON+2
          PTR=PTR+1
          LSH4KIN(PTR)=SON+3
        END DO
        DO NN=PSH4ACT(LEVEL+1)+1,PSH4ACT(LEVEL+2)
          PTR=PTR+1
          LSH4KIN(PTR)=LSH4ACT(NN)
        END DO
      END DO
      PSH4KIN(LEVELMAX+1)=PTR

C--------------------------------------------
C     TRIANGLES
C--------------------------------------------
      PSH3ACT=0

      DO NN=1,NSH3ACT
        N    =LSH3ACT(NN)
        LEVEL=SH3TREE(3,N)
        PSH3ACT(LEVEL)=PSH3ACT(LEVEL)+1
        LTMP3(NN)=N
      END DO 
C
      DO LEVEL=1,LEVELMAX
        PSH3ACT(LEVEL)=PSH3ACT(LEVEL)+PSH3ACT(LEVEL-1)
      END DO
C
      DO LEVEL=LEVELMAX+1,1,-1
        PSH3ACT(LEVEL)=PSH3ACT(LEVEL-1)
      END DO
      PSH3ACT(0)=0
C
      DO NN=1,NSH3ACT
        N    =LTMP3(NN)
        LEVEL=SH3TREE(3,N)
        PSH3ACT(LEVEL)=PSH3ACT(LEVEL)+1
        LSH3ACT(PSH3ACT(LEVEL))=N
      END DO
C
      DO LEVEL=LEVELMAX+1,1,-1
        PSH3ACT(LEVEL)=PSH3ACT(LEVEL-1)
      END DO
      PSH3ACT(0)=0
C
      PSH3KIN(0)=0
      LSH3KIN(1:PSH3ACT(1))=LSH3ACT(1:PSH3ACT(1))

      PTR = PSH3ACT(1)
      DO LEVEL=0,LEVELMAX-1
        PSH3KIN(LEVEL+1)=PTR
        DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
          N    =LSH3KIN(NN)
          SON=SH3TREE(2,N)
          PTR=PTR+1
          LSH3KIN(PTR)=SON
          PTR=PTR+1
          LSH3KIN(PTR)=SON+1
          PTR=PTR+1
          LSH3KIN(PTR)=SON+2
          PTR=PTR+1
          LSH3KIN(PTR)=SON+3
        END DO
        DO NN=PSH3ACT(LEVEL+1)+1,PSH3ACT(LEVEL+2)
          PTR=PTR+1
          LSH3KIN(PTR)=LSH3ACT(NN)
        END DO
      END DO
      PSH3KIN(LEVELMAX+1)=PTR

      RETURN
      END     
