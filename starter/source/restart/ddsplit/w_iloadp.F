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
Chd|  W_ILOADP                      source/restart/ddsplit/w_iloadp.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_ILOADP(
     .  ILOADP,LLOADP,NODLOCAL,PROC,LLLOADP_L,LEN_IA,P,CEP, ITAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "rnur_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, LEN_IA, LLLOADP_L,P,
     .        ILOADP(SIZLOADP,*), LLOADP(*),NODLOCAL(*),
     .        CEP(*)
      INTEGER,INTENT(IN) :: ITAB(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IAD, IADL, NL, N, NLL, NN, IGRTMP(SIZLOADP,NLOADP),
     .        LGRTMP(LLLOADP_L),OFF,N1,N2,N3,N4
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      OFF = NUMELC+NUMELTG+NUMELS+NUMELQ+NUMELT+NUMELP+NUMELR+NUMELUR+NUMELX+NCONLD+NUMCONV+NUMRADIA+NFXFLUX
      IADL = 1
      DO I = 1, NLOADP
        NL = ILOADP(1,I)
        IAD= ILOADP(4,I)
        DO J = 1, SIZLOADP
          IGRTMP(J,I) = ILOADP(J,I)
        ENDDO
        NLL = 0
        DO J = 1, NL/4
          N1= LLOADP(IAD+(J-1)*4)
          N2= LLOADP(IAD+(J-1)*4+1)
          N3= LLOADP(IAD+(J-1)*4+2)
          N4= LLOADP(IAD+(J-1)*4+3)
          IF(CEP(J+OFF).EQ.PROC) THEN
            LGRTMP(IADL+NLL) = NODLOCAL(N1)
            LGRTMP(IADL+NLL+1) = NODLOCAL(N2)
            LGRTMP(IADL+NLL+2) = NODLOCAL(N3)
            IF (N4 == 0) THEN
              LGRTMP(IADL+NLL+3) = 0
            ELSE
              LGRTMP(IADL+NLL+3) = NODLOCAL(N4)
            ENDIF
            NLL = NLL + 4
          ENDIF
        ENDDO
        IGRTMP(1,I) = NLL
        IGRTMP(4,I) = IADL
        IADL = IADL+NLL
        OFF = OFF + NL/4
      ENDDO

      CALL WRITE_I_C(IGRTMP,SIZLOADP*NLOADP)
      CALL WRITE_I_C(LGRTMP,LLLOADP_L)
      LEN_IA = LEN_IA + SIZLOADP*NLOADP + IADL-1

      RETURN
      END
