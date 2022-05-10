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
Chd|  W_ELMX                        source/restart/ddsplit/w_elmx.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_ELMX(KXX,NUMELX_L,ISUMNX_L,IXX,CEP,
     +                  PROC,NODLOCAL,LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr23_c.inc"
#include      "rnur_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMELX_L, PROC, ISUMNX_L,LEN_IA,
     .        KXX(NIXX,*), CEP(*), NODLOCAL(*), IXX(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NN, IAD, IAD_L, J, NB_L, NNOD, IE_L, OFF
      INTEGER, DIMENSION(:), ALLOCATABLE :: IXX_L
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KXX_L
C-----------------------------------------------
C
! ----------------------------
!     allocate 1d array
      ALLOCATE( IXX_L(ISUMNX_L) ) 
!     allocate 2d array
      ALLOCATE( KXX_L(NIXX,NUMELX_L) )
! ----------------------------
      OFF = NUMELC+NUMELTG+NUMELS+NUMELQ+NUMELT+NUMELP+NUMELR+NUMELUR
      IE_L = 0
      NB_L = 0
C
      DO I = 1, NUMELX
        IF(CEP(OFF+I).EQ.PROC) THEN
          NNOD  = KXX(3,I)
          IAD   = KXX(4,I)
C
          IE_L = IE_L + 1
          DO J = 1, NIXX
            KXX_L(J,IE_L) = KXX(J,I)
          ENDDO
          IAD_L = NB_L+1
          KXX_L(4,IE_L) = IAD_L
          NB_L = NB_L + NNOD
          DO J = 1, NNOD
            IXX_L(IAD_L+J-1) = NODLOCAL(IXX(IAD+J-1))
          ENDDO
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(KXX_L,NUMELX_L*NIXX)
      CALL WRITE_I_C(IXX_L,ISUMNX_L)
      LEN_IA = LEN_IA + NUMELX_L*NIXX + ISUMNX_L
C
! ----------------------------
!     deallocate 1d array
      DEALLOCATE( IXX_L ) 
!     deallocate 2d array
      DEALLOCATE( KXX_L )
! ----------------------------
      RETURN
      END
