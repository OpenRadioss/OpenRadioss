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
Chd|  C_EIG                         source/restart/ddsplit/c_eig.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_EIG(EIGIPM   , EIGIBUF, EIGRPM, NODLOCAL, EIGIPM_L,
     .                 EIGIBUF_L, LEIBUF_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "eigcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EIGIPM(NEIPM,*), EIGIBUF(*), NODLOCAL(*), 
     .        EIGIPM_L(NEIPM,*),EIGIBUF_L(*), LEIBUF_L
      my_real
     .        EIGRPM(NERPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADB, IADB_L, IEIG, I, NN1, NN2, NN1_L, NN2_L, II
C
      IADB=0
      IADB_L=0
      DO IEIG=1,NEIG
         DO I=1,NEIPM
            EIGIPM_L(I,IEIG)=EIGIPM(I,IEIG)
         ENDDO
         NN1=EIGIPM(10,IEIG)
         NN2=EIGIPM(11,IEIG)
         NN1_L=0
         NN2_L=0
         DO I=1,NN1
            II=NODLOCAL(EIGIBUF(IADB+I))
            IF (II/=0) THEN
               NN1_L=NN1_L+1
               EIGIBUF_L(IADB_L+NN1_L)=II
            ENDIF
         ENDDO
         DO I=1,NN2
            II=NODLOCAL(EIGIBUF(IADB+NN1+I))
            IF (II/=0) THEN
               NN2_L=NN2_L+1
               EIGIBUF_L(IADB_L+NN1_L+NN2_L)=II
            ENDIF
         ENDDO
         EIGIPM_L(10,IEIG)=NN1_L
         EIGIPM_L(11,IEIG)=NN2_L
         IADB=IADB+NN1+NN2
         IADB_L=IADB_L+NN1_L+NN2_L
      ENDDO
      LEIBUF_L=IADB_L
C
      RETURN
      END
      
