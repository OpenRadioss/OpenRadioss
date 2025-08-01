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
!||    agrad0                 ../engine/source/ale/agrad0.F
!||--- called by ------------------------------------------------------
!||    alethe                 ../engine/source/ale/alethe.F
!||--- calls      -----------------------------------------------------
!||    agrad2                 ../engine/source/ale/ale2d/agrad2.F
!||    agrad3                 ../engine/source/ale/ale3d/agrad3.F
!||    egrad2                 ../engine/source/ale/euler2d/egrad2.F
!||    egrad3                 ../engine/source/ale/euler3d/egrad3.F
!||    my_barrier             ../engine/source/system/machine.F
!||    tgrad2                 ../engine/source/ale/ale2d/tgrad2.F
!||    tgrad3                 ../engine/source/ale/ale3d/tgrad3.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE AGRAD0(IPARG    ,IXS      ,IXQ       ,X       ,ALE_CONNECT   ,
     .                  GRAD     ,VEUL     ,ELBUF_STR ,ITASK   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS), IXQ(NIXQ,NUMELQ)
      my_real X(3,NUMNOD),GRAD(*), VEUL(*)
      INTEGER IPARG(NPARG,NGROUP),ITASK
      TYPE (ELBUF_STRUCT_), DIMENSION (NGROUP), TARGET :: ELBUF_STR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,NEL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
        GBUF => ELBUF_STR(NG)%GBUF
        JTUR=IPARG(12,NG)
        JTHE=IPARG(13,NG)
        ITY =IPARG(5,NG)
        IF((JTUR == 1 .OR. JTHE == 1) .AND. ITY /= 51 .AND. IPARG(8,NG) /= 1) THEN
          MTN=IPARG(1,NG)
          JALE=IPARG(7,NG)
          JEUL=IPARG(11,NG)
          JLAG=IPARG(14,NG)
          LLT=IPARG(2,NG)
          NFT=IPARG(3,NG)
          LFT=1
          NEL = LLT
          IF(N2D == 0)THEN
            IF(JALE+JLAG /= 0)THEN
              CALL AGRAD3(IXS,X,ALE_CONNECT,GRAD(6*NFT+1))
            ELSEIF(JEUL == 1)THEN
              CALL EGRAD3(IXS,X,ALE_CONNECT,GRAD(6*NFT+1),VEUL)
            ELSE
              CALL TGRAD3(GBUF%SIG,GRAD(6*NFT+1),NEL)
            ENDIF
          ELSE
            IF(JLAG+JALE /= 0)THEN
              CALL AGRAD2(IXQ,X,ALE_CONNECT,GRAD(4*NFT+1))
            ELSEIF(JEUL == 1)THEN
              CALL EGRAD2(IXQ,X,ALE_CONNECT,GRAD(4*NFT+1))
            ELSE
              CALL TGRAD2(GBUF%SIG,GRAD(4*NFT+1),NEL)
            ENDIF
          ENDIF
        ENDIF
      ENDDO!next NG
C
      CALL MY_BARRIER
C
      RETURN
      END
