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
      !||    csens3                ../engine/source/elements/shell/coque/csens3.F
      !||--- called by ------------------------------------------------------
      !||    c3forc3               ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk           ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cforc3                ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk            ../engine/source/elements/xfem/cforc3_crk.F
      !||--- calls      -----------------------------------------------------
      !||    get_u_numsens         ../engine/source/user_interface/usensor.F
      !||    get_u_sens_fpar       ../engine/source/user_interface/usensor.F
      !||    get_u_sens_value      ../engine/source/user_interface/usensor.F
      !||    set_u_sens_maxvalue   ../engine/source/user_interface/usensor.F
      !||    set_u_sens_value      ../engine/source/user_interface/usensor.F
      !||====================================================================
       SUBROUTINE CSENS3 (JFT    ,JLT    ,PID    ,IGEO   ,EPSP   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IGEO(NPROPGI,*), PID(MVSIZ)
      my_real
     .    EPSP(MVSIZ)
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      INTEGER  GET_U_NUMSENS,GET_U_SENS_FPAR,
     .         GET_U_SENS_VALUE,SET_U_SENS_VALUE,
     .         SET_U_SENS_MAXVALUE
      EXTERNAL GET_U_NUMSENS,GET_U_SENS_FPAR,
     .         GET_U_SENS_VALUE,SET_U_SENS_VALUE,
     .         SET_U_SENS_MAXVALUE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,ISEN,IVAR,IERR
C======================================================================|
      IVAR = 1    
      DO I=JFT,JLT
        ID = IGEO(3,PID(I))
        IF (ID /= 0) THEN
          ISEN = GET_U_NUMSENS(ID)
          IERR = SET_U_SENS_MAXVALUE(ISEN,IVAR,EPSP(I))
        ENDIF
      ENDDO
C-------------------------
      RETURN
      END
