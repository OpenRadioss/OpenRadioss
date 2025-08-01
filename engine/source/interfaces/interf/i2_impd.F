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
!||    i2_impd         ../engine/source/interfaces/interf/i2_impd.F
!||--- called by ------------------------------------------------------
!||    recukin         ../engine/source/implicit/recudis.F
!||--- calls      -----------------------------------------------------
!||    int2_imp2       ../engine/source/interfaces/interf/i2_imp2.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE I2_IMPD(NINT2 ,IINT2,IPARI,INTBUF_TAB  ,
     .                   X     ,MS   ,IN   ,WEIGHT,NDOF ,
     .                   D     ,DR  )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), WEIGHT(*),NDOF(*),
     .        NINT2 ,IINT2(*)
C     REAL
      my_real
     .   X(*), MS(*),IN(*),D(3,*), DR(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C-----------------------------------------------
      DO I=NINT2,1,-1
       N=IINT2(I)
       CALL INT2_IMP2(
     .  IPARI(1,N),INTBUF_TAB(N) ,X  ,MS  ,  IN,WEIGHT,
     .  NDOF      ,D    ,DR  )
      ENDDO 
C
      RETURN
      END
