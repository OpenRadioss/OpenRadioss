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
Chd|  I9WALE                        source/interfaces/int09/i9wale.F
Chd|-- called by -----------
Chd|        INTAL3                        source/ale/inter/intal3.F     
Chd|-- calls ---------------
Chd|        I9WAL2                        source/interfaces/int09/i9wal2.F
Chd|        I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I9WALE(IPARI,INTBUF_TAB,X        ,V      ,
     2                  A    ,VG      ,ICODE     ,ISKEW    ,SKEW   ,
     3                  IXS  ,IXQ     ,ELBUF_TAB,IPARG  ,
     4                  PM   ,NALE    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD      
      USE INTBUFDEF_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), ICODE(*), ISKEW(*) ,IXS(NIXS,*),
     .        IXQ(NIXQ,*),IPARG(NPARG,*),
     .        NALE(*)
C     REAL
      my_real
     .   A(*), X(*), V(*), VG(*), SKEW(LSKEW,*),PM(NPROPM,*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INTTH, IEULT, ISIZES, ISIZEM
      my_real
     .    UPW, STENS
      INTEGER :: NRTS,NRTM,NMN,NSN
C-----------------------------------------------
C
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      INTTH =IPARI(12)
      IEULT =IPARI(14)
C
      UPW   = INTBUF_TAB%VARIABLES(14)
      STENS = INTBUF_TAB%VARIABLES(15)
      ISIZES = NRTS
      ISIZEM = NRTM
      IF(N2D==0)THEN
        CALL I9WAL3
     1  (X         ,V         ,VG        ,A         ,INTBUF_TAB%CSTS,
     2   INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,ICODE ,ISKEW,
     3   SKEW      ,INTBUF_TAB%MSR,INTBUF_TAB%LNSV,INTBUF_TAB%NSEGS,INTBUF_TAB%IRECTS,
     4   INTBUF_TAB%IRECTM,UPW       ,IXS       ,ELBUF_TAB ,
     5   IPARG     ,PM        ,NALE      ,INTBUF_TAB%STFNS,INTBUF_TAB%IELES,
     6   INTBUF_TAB%IELEM,INTBUF_TAB%STFAC(1),INTTH     ,IEULT     ,STENS     ,
     7   INTBUF_TAB%N,ISIZES    ,ISIZEM, NRTS, NRTM, NSN,NMN    )
      ELSE
        CALL I9WAL2
     1  (X         ,V         ,VG        ,A         ,INTBUF_TAB%CSTS,
     2   INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,ICODE     ,ISKEW     ,
     3   SKEW      ,INTBUF_TAB%MSR,INTBUF_TAB%LNSV,INTBUF_TAB%NSEGS,INTBUF_TAB%IRECTS,
     4   INTBUF_TAB%IRECTM,UPW       ,IXQ ,ELBUF_TAB ,
     5   IPARG     ,PM        ,NALE      ,INTBUF_TAB%STFNS,INTBUF_TAB%IELES,
     6   INTBUF_TAB%IELEM,INTBUF_TAB%STFAC(1),INTTH     ,IEULT     ,STENS     ,
     7   ISIZES    ,ISIZEM, NRTS,NRTM,NSN,NMN    )
      ENDIF
C
      RETURN
      END
