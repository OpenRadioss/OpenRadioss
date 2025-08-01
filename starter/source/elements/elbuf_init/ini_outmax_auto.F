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
!||    ini_outmax_auto   ../starter/source/elements/elbuf_init/ini_outmax_auto.F
!||--- called by ------------------------------------------------------
!||    elbuf_ini         ../starter/source/elements/elbuf_init/elbuf_ini.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE INI_OUTMAX_AUTO(ELBUF_STR,ITY,NG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE OUTMAX_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN):: ITY,NG
      TYPE(ELBUF_STRUCT_),TARGET  :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      TYPE(G_BUFEL_)     , POINTER :: GBUF
C=======================================================================

      GBUF => ELBUF_STR%GBUF
C----
        SELECT CASE (ITY)
          CASE(1)
           GBUF%G_TM_YIELD=  IKEYMAX(1)*IPART_OK(NG,1)
           GBUF%G_TM_SEQ  =  IKEYMAX(2)*IPART_OK(NG,2)
           GBUF%G_TM_EINT =  IKEYMAX(3)*IPART_OK(NG,3)
           GBUF%G_TM_DMG  =  IKEYMAX(4)*IPART_OK(NG,4)
           GBUF%G_TM_SIG  =6*IKEYMAX(5)*IPART_OK(NG,5)
           GBUF%G_TM_STRA =6*IKEYMAX(6)*IPART_OK(NG,6)
          CASE(3,7)
           GBUF%G_TM_YIELD=  IKEYMAX(1)*IPART_OK(NG,1)
           GBUF%G_TM_SEQ  =  IKEYMAX(2)*IPART_OK(NG,2)
           GBUF%G_TM_EINT =  IKEYMAX(3)*IPART_OK(NG,3)
           GBUF%G_TM_DMG  =  IKEYMAX(4)*IPART_OK(NG,4)
           GBUF%G_TM_SIG  =3*IKEYMAX(5)*IPART_OK(NG,5)
           GBUF%G_TM_STRA =3*IKEYMAX(6)*IPART_OK(NG,6)
        END SELECT
C-----------------------------------------------
      RETURN
      END SUBROUTINE INI_OUTMAX_AUTO
