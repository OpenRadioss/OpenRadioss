Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  SPINIH                        source/elements/sph/spinih.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F
Chd|-- calls ---------------
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|====================================================================
      SUBROUTINE SPINIH(KXSP,IPART,IPARTSP,SPBUF,PM )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER KXSP(NISP,*), IPART(LIPART1,*), IPARTSP(*)
        my_real
     .          SPBUF(NSPBUF,*),PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER N, IPRT, I0, IMAT
        my_real
     .          DIST, MP, RHO, VOL, SQ2
C-----------------------------------------------
        my_real
     .          GET_U_GEO
C-----------------------------------------------
        SQ2 = SQRT(TWO)
        DO N = 1, NUMSPH
          IPRT=IPARTSP(N)
          IMAT=IPART(1,IPRT)
          I0  =IPART(2,IPRT)
          DIST=GET_U_GEO(6,I0)
          IF(DIST==ZERO) THEN
            MP  =GET_U_GEO(1,I0)
            RHO=PM(1,IMAT)
            VOL=MP/RHO
            DIST=(SQ2*VOL)**THIRD
          END IF
          SPBUF(1,N)=DIST
        END DO
C
        RETURN
      END
