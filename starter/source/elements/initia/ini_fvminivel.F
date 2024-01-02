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
Chd|====================================================================
Chd|  INI_FVMINIVEL                 source/elements/initia/ini_fvminivel.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE INI_FVMINIVEL(FVM_INIVEL ,MULTI_FVM ,IGRBRIC ,IGRQUAD ,IGRSH3N)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
!LIGNR1
!INVEL
#include      "com04_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(FVM_INIVEL_STRUCT), INTENT(IN) :: FVM_INIVEL(*)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IGRS, NOSYS, J
      
      DO I = 1, NINVEL
         IF (FVM_INIVEL(I)%FLAG) THEN
C     Bric groups
            IF (FVM_INIVEL(I)%GRBRICID /= -1) THEN
               IGRS = FVM_INIVEL(I)%GRBRICID
               DO J=1,IGRBRIC(IGRS)%NENTITY
                  NOSYS=IGRBRIC(IGRS)%ENTITY(J)
                  MULTI_FVM%VEL(1, NOSYS) = FVM_INIVEL(I)%VX
                  MULTI_FVM%VEL(2, NOSYS) = FVM_INIVEL(I)%VY
                  MULTI_FVM%VEL(3, NOSYS) = FVM_INIVEL(I)%VZ
               ENDDO
            ENDIF
C     Quad groups
            IF (FVM_INIVEL(I)%GRQUADID /= -1) THEN
               IGRS = FVM_INIVEL(I)%GRQUADID
               DO J=1,IGRQUAD(IGRS)%NENTITY
                  NOSYS=IGRQUAD(IGRS)%ENTITY(J)
                  MULTI_FVM%VEL(1, NOSYS) = FVM_INIVEL(I)%VX
                  MULTI_FVM%VEL(2, NOSYS) = FVM_INIVEL(I)%VY
                  MULTI_FVM%VEL(3, NOSYS) = FVM_INIVEL(I)%VZ
               ENDDO                 
            ENDIF
C     SH3N groups
            IF (FVM_INIVEL(I)%GRSH3NID /= -1) THEN
               IGRS = FVM_INIVEL(I)%GRSH3NID
               DO J=1,IGRSH3N(IGRS)%NENTITY
                  NOSYS=IGRSH3N(IGRS)%ENTITY(J)
                  MULTI_FVM%VEL(1, NOSYS) = FVM_INIVEL(I)%VX
                  MULTI_FVM%VEL(2, NOSYS) = FVM_INIVEL(I)%VY
                  MULTI_FVM%VEL(3, NOSYS) = FVM_INIVEL(I)%VZ
               ENDDO                 
            ENDIF
         ENDIF
      ENDDO
      END SUBROUTINE INI_FVMINIVEL
