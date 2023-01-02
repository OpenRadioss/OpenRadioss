Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  INSLIN                        source/interfaces/interf1/inslin.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|        COUNT3                        source/interfaces/interf1/count3.F
Chd|====================================================================
      SUBROUTINE INSLIN(NRT,MSN,IRECT,NOINT,
     .                  SLIN_NODES,ITAB,MSV,NTAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------------------------------
C     LECTURE DES SURFACES ET DECOMPTE DES SEGMENTS
C           ENTREE : 
C                    NRT NOMBRE DE RENSEIGNEMENTS A LIRE
C           SORTIE : 
C                    IRECT 
C-----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NOINT,MSN
      INTEGER IRECT(4,*),ITAB(*),MSV(*),SLIN_NODES(NRT,2)
      INTEGER, DIMENSION(2*NUMNOD+1), INTENT(INOUT) ::  NTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IT, J, NSGOLD,ITG,K,L,IR1
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C=======================================================================
        DO J=1,NRT
          DO K=1,2
            IRECT(K,J)=SLIN_NODES(J,K)
          ENDDO
          IRECT(3,J)=0
          IRECT(4,J)=0
        ENDDO
        IF(IPRI>=1) THEN
          WRITE(IOUT,'(/,A,/)')' SEGMENTS USED FOR SURFACE DEFINITION'
          K=1
          DO I=1,NRT
            WRITE(IOUT,FMT=FMW_4I)(ITAB(IRECT(K,I)),K=1,2)
          ENDDO      
        ENDIF
        IF (NRT > 0) CALL COUNT3(IRECT,MSV,MSN,NRT,NTAG)
C------------------------------------------------------------
      RETURN
      END
