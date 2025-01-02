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
      !||    prelecig3d       ../starter/source/elements/ige3d/prelecig3d.F
      !||--- called by ------------------------------------------------------
      !||    lectur           ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||    format_mod       ../starter/share/modules1/format_mod.F90
      !||    reader_old_mod   ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE PRELECIG3D(NUM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE FORMAT_MOD , ONLY : FMT_5I
        USE READER_OLD_MOD , ONLY : KCUR, IREC, LINE, KIGE3D, KOPTAD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,IDX,IDY,IDZ,NCTRL 
C=======================================================================
        NUM  = 0
        KCUR = KIGE3D
        IREC = KOPTAD(KCUR)
        IREC=IREC+1
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        DO WHILE( LINE(1:1) /= '/' .OR. LINE(1:6) == '/IGE3D')
         IF (LINE(1:6) == '/IGE3D')THEN
           IREC=IREC+1
           READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
         ENDIF
         READ(LINE,ERR=999,FMT=FMT_5I)ID,IDX,IDY,IDZ,NCTRL
         NUM = NUM + NCTRL                              
         IREC = IREC + ((NCTRL-1)/10)+2
         READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        ENDDO
C-------------------------------------
      RETURN
  999 CALL FREERR(3)
      RETURN
      END SUBROUTINE PRELECIG3D
