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
Chd|  SPMD_USERWIS_SPMD             source/user_interface/userwis_ini_spmd.F
Chd|-- called by -----------
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_USERWIS_SPMD(IUVAR,USERNODS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IUVAR(*),USERNODS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ND,I,NUVARI,NWING,NFUSE
C======================================================================|
C Nodes 1 2 3 4
      ND=IUVAR(1)
      USERNODS(ND)=1
      ND=IUVAR(2)
      USERNODS(ND)=1
      ND=IUVAR(3)
      USERNODS(ND)=1
      ND=IUVAR(4)
      USERNODS(ND)=1

      NUVARI = 4
      NUVARI = NUVARI + 1
      NWING = IUVAR(NUVARI)
      NUVARI = NUVARI + 1
      NFUSE = IUVAR(NUVARI)
      NUVARI = NUVARI + 1

      DO I=1, 2*NWING+NFUSE
        ND=IUVAR(NUVARI)
        USERNODS(ND)=1
        NUVARI = NUVARI + 1
      ENDDO

C WGR
      ND=IUVAR(NUVARI)
      USERNODS(ND)=1
      NUVARI=NUVARI+1
C WGL
      ND=IUVAR(NUVARI)
      USERNODS(ND)=1
      NUVARI=NUVARI+1
C NEPR
      ND=IUVAR(NUVARI)
      USERNODS(ND)=1
      NUVARI=NUVARI+1
C NEPL
      ND=IUVAR(NUVARI)
      USERNODS(ND)=1
      NUVARI=NUVARI+1
C NEGR
      ND=IUVAR(NUVARI)
      USERNODS(ND)=1
      NUVARI=NUVARI+1
C NEGL
      ND=IUVAR(NUVARI)
      USERNODS(ND)=1
      NUVARI=NUVARI+1
C NTAIL
      ND=IUVAR(NUVARI)
      USERNODS(ND)=1
      NUVARI=NUVARI+1
C NVTAIL
      ND=IUVAR(NUVARI)
      USERNODS(ND)=1
      NUVARI=NUVARI+1

      END
Chd|====================================================================
Chd|  SPMD_USERWI_REST              source/user_interface/userwis_ini_spmd.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_USERWI_REST(IUVAR,IUVAR_L,NODLOCAL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IUVAR(*),IUVAR_L(*),NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ND,I,NUVARI,NWING,NFUSE
C======================================================================|
C Nodes 1 2 3 4
      ND=IUVAR(1)
      IUVAR_L(1)=NODLOCAL(ND)
      ND=IUVAR(2)
      IUVAR_L(2)=NODLOCAL(ND)
      ND=IUVAR(3)
      IUVAR_L(3)=NODLOCAL(ND)
      ND=IUVAR(4)
      IUVAR_L(4)=NODLOCAL(ND)

      NUVARI = 4
      NUVARI = NUVARI + 1
      NWING = IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NWING
      NUVARI = NUVARI + 1
      NFUSE = IUVAR(NUVARI)
      IUVAR_L(NUVARI)= NFUSE
      NUVARI = NUVARI + 1

      DO I=1, 2*NWING+NFUSE
        ND=IUVAR(NUVARI)
        IUVAR_L(NUVARI)=NODLOCAL(ND)
        NUVARI = NUVARI + 1
      ENDDO

C WGR
      ND=IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NODLOCAL(ND)
      NUVARI=NUVARI+1
C WGL

      ND=IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NODLOCAL(ND)
      NUVARI=NUVARI+1
C NEPR
      ND=IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NODLOCAL(ND)
      NUVARI=NUVARI+1
C NEPL
      ND=IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NODLOCAL(ND)
      NUVARI=NUVARI+1
C NEGR
      ND=IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NODLOCAL(ND)
      NUVARI=NUVARI+1
C NEGL
      ND=IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NODLOCAL(ND)
      NUVARI=NUVARI+1
C NTAIL
      ND=IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NODLOCAL(ND)
      NUVARI=NUVARI+1
C NVTAIL
      ND=IUVAR(NUVARI)
      IUVAR_L(NUVARI)=NODLOCAL(ND)
      NUVARI=NUVARI+1

      END
Chd|====================================================================
Chd|  USERWIS_FRONT                 source/user_interface/userwis_ini_spmd.F
Chd|-- called by -----------
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|====================================================================
      SUBROUTINE USERWIS_FRONT(USERNODS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER USERNODS(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C======================================================================|

      DO I=1,NUMNOD
        IF (USERNODS(I)==1)THEN
	  CALL IFRONTPLUS(I,1)
        ENDIF
      ENDDO


      END
