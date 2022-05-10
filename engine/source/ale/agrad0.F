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
Chd|  AGRAD0                        source/ale/agrad0.F           
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        AGRAD2                        source/ale/ale2d/agrad2.F     
Chd|        AGRAD3                        source/ale/ale3d/agrad3.F     
Chd|        EGRAD2                        source/ale/euler2d/egrad2.F   
Chd|        EGRAD3                        source/ale/euler3d/egrad3.F   
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        TGRAD2                        source/ale/ale2d/tgrad2.F     
Chd|        TGRAD3                        source/ale/ale3d/tgrad3.F     
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE AGRAD0(IPARG    ,IXS      ,IXQ      ,X       ,ALE_CONNECT   ,
     .                  GRAD     ,VEUL     ,ELBUF_STR,ITASK   )
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS, IXQ
C     REAL ou REAL*8
      my_real
     .   X
      INTEGER IPARG(NPARG,*),ITASK
C     REAL ou REAL*8
      my_real
     .   GRAD(*), VEUL(*)
      TYPE (ELBUF_STRUCT_), DIMENSION (NGROUP), TARGET :: ELBUF_STR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,NEL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
C
      DO 100 NG=ITASK+1,NGROUP,NTHREAD

        GBUF => ELBUF_STR(NG)%GBUF
        JTUR=IPARG(12,NG)
        JTHE=IPARG(13,NG)
        ITY =IPARG(5,NG)
        IF((JTUR.NE.1.AND.JTHE.NE.1).OR.ITY == 51) GO TO 100
        IF(IPARG(8,NG) == 1) GO TO 100
        MTN=IPARG(1,NG)
        JALE=IPARG(7,NG)
        JEUL=IPARG(11,NG)
        JLAG=IPARG(14,NG)
        LLT=IPARG(2,NG)
        NFT=IPARG(3,NG)
        LFT=1
        NEL = LLT
        IF(N2D == 0)THEN
          IF(JALE+JLAG.NE.0)THEN
            CALL AGRAD3(IXS,X,ALE_CONNECT,GRAD(6*NFT+1))
          ELSEIF(JEUL == 1)THEN
            CALL EGRAD3(IXS,X,ALE_CONNECT,GRAD(6*NFT+1),VEUL)
          ELSE
            CALL TGRAD3(GBUF%SIG,GRAD(6*NFT+1),NEL)
          ENDIF
        ELSE
          IF(JLAG+JALE.NE.0)THEN
            CALL AGRAD2(IXQ,X,ALE_CONNECT,GRAD(4*NFT+1))
          ELSEIF(JEUL == 1)THEN
            CALL EGRAD2(IXQ,X,ALE_CONNECT,GRAD(4*NFT+1))
          ELSE
            CALL TGRAD2(GBUF%SIG,GRAD(4*NFT+1),NEL)
          ENDIF
        ENDIF
 100  CONTINUE
C
      CALL MY_BARRIER
C
      RETURN
      END
