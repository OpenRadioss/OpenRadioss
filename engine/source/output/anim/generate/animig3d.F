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
Chd|  ANIMIG3D                      source/output/anim/generate/animig3d.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        PROJECIG3D                    source/elements/ige3d/projecig3d.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ANIMIG3D(ELBUF_TAB,IPARG    ,X     ,D        , V       ,A     ,
     .                    WIGE     ,KXIG3D   ,IXIG3D,IG3DSOLID,NANIM3D_L,X_TEMP,
     .                    D_TEMP   ,V_TEMP   ,A_TEMP,TABSTRESL,IGEO     ,KNOT  ,
     .                    ITAB     ,IPARTIG3D,IPART , CONT, CONT_TEMP,
     .                    FINT, FINT_TEMP, FEXT, FEXT_TEMP, FREAC, FREAC_TEMP,KNOTLOCPC,KNOTLOCEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "nsvis_c.inc"
#include      "param_c.inc"
#include      "fluid_scr_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXIG3D(NIXIG3D,*),IXIG3D(*),IGEO(NPROPGI,*),
     .        NANIM3D_L, IPARG(NPARG,*), ITAB(*)  ! DISCUTER AVEC SEB DE COMMENT RENVOYER NANIM3D_L
      INTEGER IG3DSOLID(8,27,*),IPARTIG3D(*),IPART(LIPART1,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      my_real
     .   X(3,*),D(3,*),V(3,*),A(3,*),WIGE(*),KNOT(*),
     .   X_TEMP(3,*),D_TEMP(3,*),V_TEMP(3,*),
     .   A_TEMP(3,*),TABSTRESL(6,*),CONT(3,*),CONT_TEMP(3,*),
     .   FINT(3,*),FINT_TEMP(3,*),FEXT(3,*),FEXT_TEMP(3,*),
     .   FREAC(6,*),FREAC_TEMP(3,*),KNOTLOCPC(*),KNOTLOCEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NISOELCUT, NG, NBG, NCTRL, ITEST, NEL_P, I, PX,PY,PZ
C----------------------------------------------------------
      NISOELCUT = 0
      NG=1
      DO WHILE (NG<=NGROUP)
       NEL_P = 0
       ITEST = 0
       IF (IPARG(5,NG)==101) THEN
        NFT=IPARG(3,NG)
        IF (NG==NGROUP) THEN
         NEL_P = IPARG(2,NG)
        ELSE
         I=NFT+1
         NEL_P=NEL_P+1
         DO WHILE (I<NUMELIG3D)
          IF (IPARTIG3D(I+1)==IPARTIG3D(I)) THEN
            NEL_P=NEL_P+1
            I=I+1
          ELSE
            EXIT
          END IF
         END DO
        ENDIF
        NBG  = (NEL_P-1)/NVSIZ + 1
        NCTRL = IPARG(75,NG)
        PX = IGEO(41,IPARG(62,NG))
        PY = IGEO(42,IPARG(62,NG))
        PZ = IGEO(43,IPARG(62,NG))
        CALL PROJECIG3D(ELBUF_TAB, IPARG    , X     , D, V,A,
     .                  WIGE     , KXIG3D   , IXIG3D,
     .                  IG3DSOLID, NANIM3D_L, X_TEMP, D_TEMP, V_TEMP,A_TEMP,
     .                  TABSTRESL, IGEO     , KNOT  ,
     .                  NG       , NBG      , NISOELCUT,
     .                  NCTRL    , NEL_P    , ITAB  ,CONT , CONT_TEMP,
     .                  FINT, FINT_TEMP, FEXT, FEXT_TEMP, FREAC, FREAC_TEMP,
     .                  PX, PY, PZ,KNOTLOCPC,KNOTLOCEL)
        NG=NG+NBG
       ELSE
        NG=NG+1
       ENDIF
      ENDDO
C-----------
      RETURN
      END
