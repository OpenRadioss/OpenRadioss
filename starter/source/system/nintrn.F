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
      !||    nintrn                 ../starter/source/system/nintrn.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_beam           ../starter/source/elements/reader/hm_read_beam.F
      !||    hm_read_quad           ../starter/source/elements/reader/hm_read_quad.F
      !||    hm_read_sh3n           ../starter/source/elements/reader/hm_read_sh3n.F
      !||    hm_read_shell          ../starter/source/elements/reader/hm_read_shell.F
      !||    hm_read_spring         ../starter/source/elements/reader/hm_read_spring.F
      !||    hm_read_thgrne         ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrns         ../starter/source/output/th/hm_read_thgrns.F
      !||    hm_read_thgrsurf       ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_read_tria           ../starter/source/elements/reader/hm_read_tria.F
      !||    hm_read_truss          ../starter/source/elements/reader/hm_read_truss.F
      !||    hm_thgrki_vent         ../starter/source/output/th/hm_thgrki_vent.F
      !||    thprin                 ../starter/source/output/th/thprin.F
      !||    thprin_sub             ../starter/source/output/th/thprin.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION NINTRN (IEXT,NTN,M,N,ID,TITR)
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXT, M, N
      INTEGER NTN(M,N)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,N
        IF(NTN(M,I)==IEXT)THEN
          NINTRN=I
          RETURN
        ENDIF
      ENDDO
      NINTRN=0                       
      CALL ANCMSG(MSGID=69,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            I2=IEXT)
C------------------------------------------- 
      RETURN
      END
