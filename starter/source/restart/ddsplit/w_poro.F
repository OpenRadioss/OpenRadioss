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
C
Chd|====================================================================
Chd|  W_PORO                        source/restart/ddsplit/w_poro.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_PORO(PORNOD,PROC,NUMPOR_L,GEO,NODLOCAL,LEN_IA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------   

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, NUMPOR_L, LEN_IA,
     .          PORNOD(*),NODLOCAL(*)
       my_real
     .          GEO(NPROPG,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, NAD, IG, J, N_L, PORNOD_L(NUMPOR_L)
C
      NAD=0
      N_L = 0
      DO IG = 1, NUMGEO
        IF(INT(GEO(12,IG))==15)THEN
          N=INT(GEO(31,IG))
          IF(N>0)THEN
            DO J = NAD+1, NAD+N
              NN = PORNOD(J)
              IF(NLOCAL(NN,PROC+1)==1)THEN            
                N_L = N_L + 1
                PORNOD_L(N_L)=NODLOCAL(NN)
              END IF
            END DO
            NAD = NAD + N
          END IF
        END IF
      END DO
      if(n_l/=NUMPOR_L)print*,'error decomp porosity :',n_l,NUMPOR_L
C
      CALL WRITE_I_C(PORNOD_L,NUMPOR_L)
      LEN_IA = LEN_IA + NUMPOR_L
C
      RETURN
      END
