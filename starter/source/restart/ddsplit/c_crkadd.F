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
C
Chd|====================================================================
Chd|  C_CRKADD                      source/restart/ddsplit/c_crkadd.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_CRKADD(
     .        ELCUTC   ,NODENR    ,KXFENOD2ELC  ,ENRTAG  ,
     .        ELCUTC_L ,NODENR_L  ,KXFENOD2ELC_L,ENRTAG_L,
     .        NUMELC_L ,NUMELTG_L ,NUMNOD_L     ,NODGLOB ,INOD_CRKXFEM ,
     .        P        ,CEP       ,ELCUTTG_L ,INCRKXFEM_L,INDEX_CRKXFEM)
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
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ELCUTC(2,*),NODENR(*),KXFENOD2ELC(*),ENRTAG(NUMNOD,*),
     .        ELCUTC_L(2,*),NODENR_L(*),
     .        KXFENOD2ELC_L(*),ENRTAG_L(NUMNOD_L,*),
     .        NUMELC_L,NUMELTG_L,NCRKXFE_L,NUMNOD_L,NODGLOB(*),
     .        INOD_CRKXFEM(*),P,CEP(*),ELCUTTG_L(2,*),INCRKXFEM_L(*),
     .        INDEX_CRKXFEM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,PROC,OFFC,OFFTG,NOD,NOD_CRK,IEL_L,NL_L
C-----------------------------------------------
      OFFC = NUMELS + NUMELQ
      OFFTG = OFFC + NUMELC + NUMELT + NUMELP + NUMELR
C
      IEL_L = 0
      DO I=1,NUMELC
        IF (CEP(I+OFFC) == P) THEN
          IEL_L = IEL_L + 1
          DO J=1,2
            ELCUTC_L(J,IEL_L) = ELCUTC(J,I)
          ENDDO
        ENDIF
      ENDDO
C
      IEL_L = 0
      DO I=1,NUMELTG
        IF (CEP(I+OFFTG) == P) THEN
          IEL_L = IEL_L + 1
          DO J=1,2
            ELCUTTG_L(J,IEL_L) = ELCUTC(J,I+NUMELC)
          ENDDO
        ENDIF
      ENDDO
C---
      NL_L = 0
      DO I=1,NUMNOD_L
        NOD = NODGLOB(I)
cc        IF (NOD > 0 .and. INOD_CRKXFEM(NOD) > 0) THEN
        IF (INCRKXFEM_L(I) > 0) THEN
          NL_L = NL_L + 1
          NOD_CRK = INOD_CRKXFEM(NOD)
          NODENR_L(NL_L) = NODENR(NOD_CRK)
          KXFENOD2ELC(NL_L) = KXFENOD2ELC(NOD_CRK)
        ENDIF
c
c   or even:
c
c        NL_L = INCRKXFEM_L(I)
c        IF(NL_L > 0)THEN
c          NOD_CRK = INDEX_CRKXFEM(NL_L)
c          NODENR_L(NL_L) = NODENR(NOD_CRK)
c          KXFENOD2ELC(NL_L) = KXFENOD2ELC(NOD_CRK)
c        ENDIF
        DO J=1,IENRNOD
          ENRTAG_L(I,J) = ENRTAG(NOD,J)
        ENDDO
      ENDDO
C-----------
      RETURN
      END
