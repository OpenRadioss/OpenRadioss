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
Chd|  INIT_REAC_NOD                 source/output/th/init_reac_nod.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIT_REAC_NOD(CPTREAC,NODREAC,NTHGRP,ITHGRP,ITHBUF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CPTREAC,NTHGRP,NODREAC(*),ITHBUF(*),ITHGRP(NITHGR,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,NN,IAD,NVAR,IADV
C-----------------------------------------------
C
      DO I = 1,NUMNOD
        NODREAC(I)=0
      ENDDO
        DO N=1,NTHGRP
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          NVAR=ITHGRP(6,N)
          IADV=ITHGRP(7,N)
          DO J=IAD,IAD+NN-1
            K = 0
            DO L=IADV,IADV+NVAR-1
              K=ITHBUF(L)
              IF (K == 620 .OR. K == 621 .OR. K == 622 .OR.
     &            K == 623 .OR. K == 624 .OR. K == 625) THEN
                CPTREAC = CPTREAC + 1
                IF (ITHBUF(J)>0)  NODREAC(ITHBUF(J)) = CPTREAC
                EXIT
              ENDIF

            ENDDO
          ENDDO
        ENDDO
C
      RETURN
      END
