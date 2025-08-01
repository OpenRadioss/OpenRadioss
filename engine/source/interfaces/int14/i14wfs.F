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
!||    i14wfs          ../engine/source/interfaces/int14/i14wfs.F
!||--- called by ------------------------------------------------------
!||    resol           ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    groupdef_mod    ../common_source/modules/groupdef_mod.F
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||    output_mod      ../common_source/modules/output/output_mod.F90
!||====================================================================
      SUBROUTINE I14WFS(IPARI,INTBUF_TAB,IGRSURF,FSAV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
      USE OUTPUT_MOD , ONLY : WFEXT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)
C     REAL
      my_real
     .   FSAV(NTHVKI,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  NTY, N, NRTM, KSURF, NRTS
C-----------------------------------------------
C     REAL
      my_real
     .   DE
C-----------------------------------------------
       DO 100 N=1,NINTER
         NTY=IPARI(7,N)
         IF (NTY/=14.AND.NTY/=15) GOTO 100
C        Interface Stifnesses at nodes ...
C        J11+8 : Sauve DE pour calcul Travail des Forces Secnds.
         DE=INTBUF_TAB(N)%VARIABLES(9)
         FSAV(7,N)=FSAV(7,N)+DE*DT2*HALF
C
         NRTM=IPARI(4,N)
C        Warning : DE doit etre stocke Surface / Surface, dans l'Interface
C                  actuellement, NRTM == 1.
         KSURF=INTBUF_TAB(N)%KSURF(1)
         IF (IGRSURF(KSURF)%TYPE==100) THEN
C          Travail des interfaces avec Madymo.
           WFEXT=WFEXT+DE*DT2*HALF
         ENDIF
C-----------------------------------------------
 100  CONTINUE
      RETURN
      END
