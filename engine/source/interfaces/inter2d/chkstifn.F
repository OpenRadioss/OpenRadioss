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
!||    chkstifn        ../engine/source/interfaces/inter2d/chkstifn.F
!||--- called by ------------------------------------------------------
!||    resol           ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    chkslv          ../engine/source/interfaces/inter2d/chkstifn.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE CHKSTIFN(IPARI,MS,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)
C     REAL
      my_real 
     .   MS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NMN, NSN,
     .        NRTS, NRTM, NTY
C-----------------------------------------------
C
      DO N=1,NINTER
       NTY   =IPARI(7,N)
       IF(NTY==5) THEN
C interface type 5 non // en spmd
         IF(ISPMD==0) THEN
           NRTS   = IPARI(3,N)
           NRTM   = IPARI(4,N)
           NSN    = IPARI(5,N)
           NMN    = IPARI(6,N)
           CALL CHKSLV(NSN,INTBUF_TAB(N)%NSV,INTBUF_TAB(N)%STFNS,MS)
         ENDIF
       ELSE
C autres types d'interfaces
       ENDIF
      ENDDO
C
      RETURN
      END


!||====================================================================
!||    chkslv     ../engine/source/interfaces/inter2d/chkstifn.F
!||--- called by ------------------------------------------------------
!||    chkstifn   ../engine/source/interfaces/inter2d/chkstifn.F
!||====================================================================
      SUBROUTINE CHKSLV(NSN,NSV,STIFN,MS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSV(*)
C     REAL
      my_real
     .        STIFN(*), MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
      DO I = 1, NSN
C si somme masse nulle sur noeuds secnds alors stifn = -1
        IF (MS(NSV(I))==ZERO) THEN
          STIFN(I) = -ONE
        ENDIF
      ENDDO
      RETURN
      END
