Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  H3D_SHELL_OFF                 source/output/h3d/spmd/spmd_h3d_shell_off.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
       SUBROUTINE H3D_SHELL_OFF(ELBUF_TAB, IPARG, IXC, IXTG,NUMELC, SHELL_SCALAR, ID_ELEM, ITY_ELEM,
     .                          IPART , IPARTC  ,IPARTTG)
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
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      INTEGER NUMELC
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*), ID_ELEM(*), ITY_ELEM(*),
     .        IPART(LIPART1,*), IPARTC(*), IPARTTG(*)
      my_real
     .   SHELL_SCALAR(*)

C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER OFFSET,ITY,MLW,NEL,NFT,ISOLNOD,NG,I,ILEV
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------

      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        NFT   =IPARG(3,NG)
        ISOLNOD = IPARG(28,NG)
        ILEV  =IPARG(45,NG)

        IF (ITY == 3) OFFSET = 0
        IF (ITY == 7) OFFSET = NUMELC
c
        IF(ITY == 3 .OR. ITY == 7)THEN
          DO I=1,NEL 
            IF (ITY == 3) THEN
              ID_ELEM(OFFSET+NFT+I) = IXC(NIXC,NFT+I)
              ITY_ELEM(OFFSET+NFT+I) = 3
            ELSEIF (ITY == 7) THEN 
              ID_ELEM(OFFSET+NFT+I) = IXTG(NIXTG,NFT+I)
              ITY_ELEM(OFFSET+NFT+I) = 7
            ENDIF
          ENDDO
        ENDIF

        IF(ITY == 3 )THEN
          IF(MLW == 0 .OR. MLW == 13)THEN
            DO I=1,NEL
              SHELL_SCALAR(OFFSET+NFT+I) = ONE
            ENDDO
          ELSE
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=1,NEL

              IF(IPART(10,IPARTC(NFT+I)) .NE.0 .AND. NINT(MIN(GBUF%OFF(I),ONE)) .NE. ONE) THEN
                SHELL_SCALAR(OFFSET+NFT+I) = ZERO
              ELSE
                SHELL_SCALAR(OFFSET+NFT+I) = NINT(MIN(GBUF%OFF(I),ONE))
              ENDIF

            ENDDO
          ENDIF
        ELSEIF(ITY == 7 )THEN
          IF(MLW == 0 .OR. MLW == 13)THEN
            DO I=1,NEL
              SHELL_SCALAR(OFFSET+NFT+I) = ONE
            ENDDO
          ELSE
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=1,NEL

              IF(IPART(10,IPARTTG(NFT+I)) .NE.0 .AND. NINT(MIN(GBUF%OFF(I),ONE)) .NE. ONE) THEN
                SHELL_SCALAR(OFFSET+NFT+I) = ZERO
              ELSE
                SHELL_SCALAR(OFFSET+NFT+I) = NINT(MIN(GBUF%OFF(I),ONE))
              ENDIF

            ENDDO
          ENDIF
        ENDIF
       ENDDO
       RETURN
       END
