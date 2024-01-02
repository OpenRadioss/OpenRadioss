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
Chd|  H3D_ONED_OFF                  source/output/h3d/spmd/spmd_h3d_oned_off.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
       SUBROUTINE H3D_ONED_OFF(ELBUF_TAB, IPARG, IXT, IXP, IXR, ONED_SCALAR, ID_ELEM, ITY_ELEM,
     .                         IPART , IPARTT  ,IPARTP  ,IPARTR)
C----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      INTEGER IPARG(NPARG,*), IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*), 
     .   ID_ELEM(*), ITY_ELEM(*),IPART(LIPART1,*), IPARTT(*) ,IPARTP(*) ,IPARTR(*)
      my_real
     .   ONED_SCALAR(*)

C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER OFFSET,ITY,MLW,NEL,NFT,NG,I
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------

      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        NFT   =IPARG(3,NG)

        IF (ITY == 4) OFFSET = 0
        IF (ITY == 5) OFFSET = NUMELT
        IF (ITY == 6) OFFSET = NUMELT+NUMELP
c
        IF(ITY == 4 .OR. ITY == 5 .OR. ITY == 6)THEN
          DO I=1,NEL 
            IF (ITY == 4) THEN
              ID_ELEM(OFFSET+NFT+I) = IXT(NIXT,NFT+I)
              ITY_ELEM(OFFSET+NFT+I) = 4
            ELSEIF (ITY == 5) THEN 
              ID_ELEM(OFFSET+NFT+I) = IXP(NIXP,NFT+I)
              ITY_ELEM(OFFSET+NFT+I) = 5
            ELSEIF (ITY == 6) THEN 
              ID_ELEM(OFFSET+NFT+I) = IXR(NIXR,NFT+I)
              ITY_ELEM(OFFSET+NFT+I) = 6
            ENDIF
          ENDDO
        ENDIF

        IF(ITY == 4 )THEN
          IF(MLW == 0 .OR. MLW == 13)THEN
            DO I=1,NEL
              ONED_SCALAR(OFFSET+NFT+I) = ONE
            ENDDO
          ELSE
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=1,NEL
              ONED_SCALAR(OFFSET+NFT+I) = MIN(ABS(GBUF%OFF(I)),ONE)
            ENDDO
          ENDIF
        ELSEIF(ITY == 5)THEN
          IF(MLW == 0 .OR. MLW == 13)THEN
            DO I=1,NEL
              ONED_SCALAR(OFFSET+NFT+I) = ONE
            ENDDO
          ELSE
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=1,NEL
              ONED_SCALAR(OFFSET+NFT+I) = MIN(ABS(GBUF%OFF(I)),ONE)
            ENDDO
          ENDIF
        ELSEIF(ITY == 6)THEN
          IF(MLW == 0 .OR. MLW == 13)THEN
            DO I=1,NEL
              ONED_SCALAR(OFFSET+NFT+I) = ONE
            ENDDO
          ELSE
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=1,NEL
              ONED_SCALAR(OFFSET+NFT+I) = MIN(ABS(GBUF%OFF(I)),ONE)
            ENDDO
          ENDIF
        ENDIF
       ENDDO
       RETURN
       END
