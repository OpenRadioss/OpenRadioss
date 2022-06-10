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
Chd|  THICK_ILEV                    source/elements/xfem/thick_ilev.F
Chd|-- called by -----------
Chd|        CBUFXFE                       source/elements/xfem/cbufxfe.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THICK_ILEV(ELBUF_STR,XFEM_STR,
     .                      LFT      ,LLT  ,NLAY  ,IR  ,IS  ,
     .                      NPTT     ,IXFEM,THKLY ,POSLY,IREP,IXEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
#include  "mvsiz_p.inc"
#include  "param_c.inc"
#include  "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLAY,NPTT,LFT,LLT,IXFEM,IR,IS,IREP,IXEL
      my_real 
     .   THKLY(*),POSLY(MVSIZ,*)
C
      TYPE(ELBUF_STRUCT_), TARGET, intent(inout) :: ELBUF_STR
      TYPE(ELBUF_STRUCT_), TARGET, intent(inout)  :: XFEM_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,ILAY,IT,J,IPT,L_DIRA,L_DIRB
      my_real 
     .   THKT
      my_real,
     .  DIMENSION(:) ,POINTER  ::  DIR1,DIR2,DIR1X,DIR2X
C
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
C=======================================================================
c     layer thickness
c
      IF (NLAY > 1) THEN  ! multilayer
        DO ILAY=1,NLAY
          DO I=LFT,LLT
            J = (ILAY-1)*LLT + I
            THKT = ELBUF_STR%GBUF%THK(I)   !  total element thickness
            XFEM_STR%BUFLY(ILAY)%LBUF(1,1,1)%THK(I) = THKLY(J)*THKT
          ENDDO
        ENDDO  !  DO ILAY=1,NLAY
      ELSE  
c       monolayer : already filled within "C1BUF3.F"
      ENDIF  !  IF(NLAY > 1)THEN
c
c     layer orthotropy directions - copy to XFEM buffer
c
      L_DIRA = ELBUF_STR%BUFLY(1)%LY_DIRA
      L_DIRB = ELBUF_STR%BUFLY(1)%LY_DIRB
      DO ILAY=1,NLAY
        DIR1  => ELBUF_STR%BUFLY(ILAY)%DIRA(1:L_DIRA*LLT)
        DIR2  => ELBUF_STR%BUFLY(ILAY)%DIRB(1:L_DIRB*LLT)
        DIR1X => XFEM_STR%BUFLY(ILAY)%DIRA(1:L_DIRA*LLT)
        DIR2X => XFEM_STR%BUFLY(ILAY)%DIRB(1:L_DIRB*LLT)
        DO I=LFT,LLT
          II = 2*(I-1)
          IF (L_DIRA == 0) THEN
            CONTINUE
          ELSEIF (IREP == 0 .OR. IREP == 1) THEN
            DIR1X(II+1) = DIR1(II+1)
            DIR1X(II+2) = DIR1(II+2)
          ELSEIF (IREP == 2) THEN
            DIR1X(II+1) = DIR1(II+1)
            DIR1X(II+2) = DIR1(II+2)
            DIR2X(II+1) = DIR2(II+1)
            DIR2X(II+2) = DIR2(II+2)
          ENDIF
        ENDDO
      ENDDO  !  DO ILAY=1,NLAY
C---
      IF (NLAY > 1) THEN  ! multilayer
        DO ILAY=1,NLAY
          LBUF => XFEM_STR%BUFLY(ILAY)%LBUF(1,1,1)
          DO I=LFT,LLT
            LBUF%OFF(I) = -ONE
          ENDDO
        ENDDO
      ELSE  ! monolayer
        GBUF => XFEM_STR%GBUF
        DO I=LFT,LLT
          GBUF%OFF(I) = -ONE
        ENDDO
      ENDIF  !  IF(NLAY > 1)THEN
C---
      RETURN
      END
