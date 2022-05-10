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
Chd|  W_IBFV                        source/restart/ddsplit/w_ibfv.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_IBFV(IBFV    ,NC  ,NODLOCAL,LEN_IA,
     .                  NFXVEL_L,PROC)
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
      INTEGER NC, LEN_IA, PROC, NFXVEL_L,
     .        IBFV(NIFV,*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IBTMP(NIFV,NFXVEL_L), N_L, N,NCUR
C
      N_L = 0
      DO I = 1, NC
        N = IABS(IBFV(1,I))
        NCUR = IBFV(3,I)
        IF(NLOCAL(N,PROC+1)==1.AND.NCUR>0)THEN
          N_L = N_L + 1
          IBTMP(1,N_L) = SIGN(NODLOCAL(N),IBFV(1,I))
          DO J = 2, NIFV
            IBTMP(J,N_L) = IBFV(J,I)
          ENDDO
          
          IF(IBFV(13,I) == 2) THEN                    ! IBFV(13,I)=FINGEO, /IMPVEL/FGEO option
            IBTMP(14,N_L) = NODLOCAL( IBFV(14,I) )    ! IBFV(14,I)= Second Node
          ENDIF

        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IBTMP,NIFV*NFXVEL_L)
      LEN_IA = LEN_IA + NIFV*NFXVEL_L
C
      RETURN
      END
