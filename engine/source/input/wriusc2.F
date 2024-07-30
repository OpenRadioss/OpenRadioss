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
      !||====================================================================
      !||    wriusc2                ../engine/source/input/wriusc2.F
      !||--- called by ------------------------------------------------------
      !||    create_h3d_input       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_input.F
      !||    fraleonoff             ../engine/source/input/fraleonoff.F
      !||    fralnk                 ../engine/source/input/fralnk.F
      !||    fralnk2                ../engine/source/input/fralnk.F
      !||    freabf                 ../engine/source/input/freabf.F
      !||    freanim                ../engine/source/output/anim/reader/freanim.F
      !||    frecpl                 ../engine/source/input/frecpl.F
      !||    fredamp                ../engine/source/input/fredamp.F
      !||    fredebug               ../engine/source/input/fredebug.F
      !||    fredli                 ../engine/source/input/fredli.F
      !||    fredynain              ../engine/source/input/fredynain.F
      !||    freflw                 ../engine/source/input/freflw.F
      !||    freform                ../engine/source/input/freform.F
      !||    frefunc                ../engine/source/input/frefunc.F
      !||    frefvbag               ../engine/source/input/frefvbag.F
      !||    frefxinp               ../engine/source/input/frefxinp.F
      !||    freimpl                ../engine/source/input/freimpl.F
      !||    freiniv                ../engine/source/input/freiniv.F
      !||    freint                 ../engine/source/input/freint.F
      !||    frenois                ../engine/source/input/frenois.F
      !||    freoutp                ../engine/source/input/freoutp.F
      !||    frestat                ../engine/source/input/frestat.F
      !||    frethermal             ../engine/source/constraints/thermic/frethermal.F
      !||    freupwind              ../engine/source/input/freupwind.F
      !||    freupwm                ../engine/source/input/freupwm.F
      !||    h3d_read               ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../engine/source/output/message/message.F
      !||    arret                  ../engine/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      SUBROUTINE WRIUSC2(IREC,NBC,KEY0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IREC, NBC
      CHARACTER KEY0*(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real REC, ERR
      CHARACTER(LEN=NCHARLINE100) :: CARTE
C-----------------------------------------------
      REWIND(IUSC2)
      DO I=1,NBC
        READ(IUSC1,'(A)',REC=IREC,ERR=999)CARTE
        WRITE(IUSC2,'(A)')CARTE
      ENDDO
      REWIND(IUSC2)
      RETURN
 999  CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0)
      CALL ARRET(0)
      END
