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
C
Chd|====================================================================
Chd|  W_TH                          source/restart/ddsplit/w_th.F 
Chd|-- called by -----------
Chd|        WRITE_TH_BUFFER               source/restart/ddsplit/write_th_buffer.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_TH(ITHGRP ,ITHBUF ,LENTHG,NODLOCAL,CEL   ,
     2                NTHGRP0,NTHGRP2,PROC    ,
     3                CELSPH, NUMLOCCLUSTER ,NBR_TH_MONVOL)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, LENTHG, NTHGRP0, NTHGRP2,
     .          ITHGRP(NITHGR,*), ITHBUF(*), NODLOCAL(*),
     .          CEL(*), CELSPH(*), NUMLOCCLUSTER(*)
       INTEGER :: NBR_TH_MONVOL ! number of /TH/MONV
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL         
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NT, IH, ITYP, NNE, IAD, K, P, ESHIFT,
     .        ITHBUF_L(LENTHG)
C
C Header recopie
C
      CALL WRITE_I_C(ITHGRP,NITHGR*(NTHGRP0+NBR_TH_MONVOL))
C
C buffer renumerote localement
C
      DO NT = 1, LENTHG
        ITHBUF_L(NT) = ITHBUF(NT)
      ENDDO
C
      DO NT = 1, NTHGRP2
        ITYP=ITHGRP(2,NT)
        NNE =ITHGRP(4,NT)
        IAD =ITHGRP(5,NT)
        IF ((ITYP>=1.AND.ITYP<=7).OR.
     .       ITYP==50.OR.ITYP==51.OR.ITYP==100) THEN
C   elements
          DO IH = 1, NNE
            K = ITHBUF_L(IAD-1+IH)
            P = ITHBUF_L(IAD+NNE-1+IH)+1
            IF(ITYP==1) THEN
              ESHIFT = 0
            ELSEIF(ITYP==2) THEN
              ESHIFT = NUMELS
            ELSEIF(ITYP==3) THEN
              ESHIFT = NUMELS+NUMELQ
            ELSEIF(ITYP==4) THEN
              ESHIFT = NUMELS+NUMELQ+NUMELC
            ELSEIF(ITYP==5) THEN
              ESHIFT = NUMELS+NUMELQ+NUMELC+NUMELT
            ELSEIF(ITYP==6) THEN
              ESHIFT = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP
            ELSEIF(ITYP==7) THEN
              ESHIFT = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
            ELSEIF(ITYP==100) THEN
              ESHIFT = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG
            ENDIF
            IF(P==PROC) THEN
              IF(ITYP==51)THEN
               ITHBUF_L(IAD-1+IH) = CELSPH(K)
              ELSE
               ITHBUF_L(IAD-1+IH) = CEL(K+ESHIFT)
              ENDIF
            ENDIF
          ENDDO
        ELSEIF (ITYP==0) THEN
C   noeuds
          DO IH = 1, NNE
            K = ITHBUF_L(IAD-1+IH)
            IF(NLOCAL(K,PROC)==1) THEN
C   noeud present => nodlocal
              ITHBUF_L(IAD-1+IH) = NODLOCAL(K)
            ELSE
C   noeud abscent => 0
              ITHBUF_L(IAD-1+IH) = 0
            ENDIF
          ENDDO
        ELSEIF (ITYP==109) THEN
        ELSEIF (ITYP == 114) THEN
          DO IH=IAD,IAD+NNE-1
c            local number of cluster, or zero
             ITHBUF_L(IH) = NUMLOCCLUSTER(ITHBUF(IH))
          ENDDO               
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(ITHBUF_L,LENTHG)
C
      RETURN
      END
