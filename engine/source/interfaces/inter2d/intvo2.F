Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    intvo2          ../engine/source/interfaces/inter2d/intvo2.F
!||--- called by ------------------------------------------------------
!||    intfop1         ../engine/source/interfaces/interf/intfop1.F
!||--- calls      -----------------------------------------------------
!||    i3for2          ../engine/source/interfaces/inter2d/i3for2.F
!||    invoi2          ../engine/source/interfaces/inter2d/invoi2.F
!||--- uses       -----------------------------------------------------
!||    h3d_mod         ../engine/share/modules/h3d_mod.F
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE INTVO2(IPARI,INTBUF_TAB ,X   ,A    ,
     .                  ICODT,FSAV       ,FSKYI,ISKY,
     .                  FCONT,FNCONT    , FTCONT,H3D_DATA      )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD 
      USE H3D_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), ICODT(*), ISKY(*)
C     REAL
      my_real
     .   X(*), A(*), FSAV(*),FSKYI(*),
     .   FCONT(3,*),FNCONT(3,*), FTCONT(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   STARTT,PTMAX

      INTEGER IBC,NSN,NMN,NTY
C-----------------------------------------------
C
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      IBC   =IPARI(11)
C
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      IF(STARTT>TT) RETURN
C
      PTMAX=INTBUF_TAB%VARIABLES(16)
      IF(NTY==3) THEN
         PTMAX = PTMAX*HALF
      ENDIF
      CALL INVOI2(X    ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1    INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN     )
      IF(NTY==3)THEN
       CALL INVOI2(X      ,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,INTBUF_TAB%MSR,
     1          INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,NMN       )
      ENDIF
      CALL I3FOR2(X      ,A         ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1          INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%STFM,NSN   ,
     2          NMN       ,NTY       ,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     3          INTBUF_TAB%VARIABLES(1),INTBUF_TAB%STFNS,IBC       ,ICODT     ,1         ,
     4          FSAV      ,FSKYI     ,ISKY             ,PTMAX      , INTBUF_TAB%AREAS    ,
     5          FCONT     ,FNCONT    , FTCONT, H3D_DATA          )
      IF(NTY==3)THEN
       CALL I3FOR2(X      ,A         ,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     1          INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%STFS,NMN       ,
     2          NSN       ,NTY       ,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%FRICOM,
     3          INTBUF_TAB%VARIABLES(1),INTBUF_TAB%STFNM,0         ,ICODT     ,-1        ,
     4          FSAV      ,FSKYI       ,ISKY            ,PTMAX     ,INTBUF_TAB%AREAM     ,
     5          FCONT     ,FNCONT    , FTCONT, H3D_DATA          )
      ENDIF
C
      RETURN
      END
