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
!||    dtnoda_stifint   ../starter/source/interfaces/inter3d1/dtnoda_stifint.F
!||--- called by ------------------------------------------------------
!||    initia           ../starter/source/elements/initia/initia.F
!||--- uses       -----------------------------------------------------
!||    message_mod      ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE DTNODA_STIFINT( MS    ,STIFN  ,DT_STIFINT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real , INTENT(INOUT) ::  DT_STIFINT
      my_real , INTENT(IN) ::  MS(NUMNOD) ,STIFN(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   N
      my_real 
     .   DTNOD, DTFAC, DTN
C----------------------------------------------------------
C    Estimation Time step needed for contact stiffness computation 
C----------------------------------------------------------

        DTFAC = ZEP9
        DTNOD = EP20
        DO N=1,NUMNOD
           IF(STIFN(N)>ZERO.AND.MS(N)>ZERO)THEN 
             DTN = DTFAC*SQRT(TWO * MS(N) / STIFN(N)) 
             DTNOD = MIN(DTNOD,DTN) 
           ENDIF                      
        ENDDO
C
        DT_STIFINT = DTNOD

      RETURN
      END
