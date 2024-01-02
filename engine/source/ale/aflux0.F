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
Chd|  AFLUX0                        source/ale/aflux0.F           
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        A4FLUX3                       source/ale/ale3d/a4flux3.F    
Chd|        AFLUX2                        source/ale/ale2d/aflux2.F     
Chd|        AFLUX3                        source/ale/ale3d/aflux3.F     
Chd|        AFLUX3_INT22_FVM              source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
Chd|        ALEFVM_AFLUX3                 source/ale/alefvm/alefvm_aflux3.F
Chd|        ALEFVM_EFLUX3                 source/ale/alefvm/alefvm_eflux3.F
Chd|        EFLUX2                        source/ale/euler2d/eflux2.F   
Chd|        EFLUX3                        source/ale/euler3d/eflux3.F   
Chd|        EFLUX3_INT22_FVM              source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE AFLUX0(IPARG       ,ELBUF_STR ,PM    ,IXS         ,IXQ  ,
     .                  V           ,W         ,X     ,FLUX        ,FLU2 ,
     .                  MS          ,VEUL      ,FILL  ,DFILL       ,VOL  ,
     .                  ALE_CONNECT ,ALPH      ,ITASK ,NV46        ,IPM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
      USE I22TRI_MOD        
      USE ALE_CONNECTIVITY_MOD
      USE ALEFVM_MOD , only:ALEFVM_Param
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real PM, V, W, X
      INTEGER IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(7,NUMELQ), IPM(NPROPMI,NUMMAT)
      INTEGER ITASK,NV46
      my_real FLUX(*), FLU2(*), MS(*), VEUL(*), FILL(NUMNOD,*),DFILL(NUMNOD,*), VOL(*), ALPH(2,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_STR    
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, NF1,ISOLNOD,NSG,NEL
      my_real UDT
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF1,LBUF2    
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(ITASK == 0) THEN
       IF (NMULT /= 0) THEN
         DO NG=1,NGROUP
C     ALE ON / OFF
            IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
           CALL INITBUF(IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          IF (JMULT == 0) CYCLE
          GBUF  => ELBUF_STR(NG)%GBUF
          LBUF1 => ELBUF_STR(NG)%BUFLY(1)%LBUF(1,1,1)
          LBUF2 => ELBUF_STR(NG)%BUFLY(2)%LBUF(1,1,1)
          LFT=1
          IF (DT1 > ZERO)THEN
            UDT=ONE/DT1
                ELSE
                 UDT=ZERO
                ENDIF
                DO I=LFT,LLT
                  ALPH(1,I+NFT)=LBUF1%FRAC(I)
                  ALPH(2,I+NFT)=LBUF2%FRAC(I)
                  VOL(I+NFT)=GBUF%VOL(I)*UDT
          ENDDO
        ENDDO
c
       ENDIF
      ENDIF
C--------------------
      CALL MY_BARRIER
C--------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF 
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
           CALL INITBUF(IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF (JALE+JEUL == 0)   CYCLE
        IF (IPARG(8,NG) == 1) CYCLE
        GBUF => ELBUF_STR(NG)%GBUF
        NSG = IPARG(10,NG)
        ISOLNOD = IPARG(28,NG)
        LFT=1
        NF1=NFT+1
        NEL = LLT
c
        IF(N2D == 0.AND.JEUL == 0.AND. ISOLNOD == 4)THEN
         CALL A4FLUX3(PM,IXS,V,W,X,FLUX(6*NFT+1),FLU2(NF1),ALE_CONNECT)
        ELSEIF(N2D == 0.AND.JEUL == 0)THEN
         IF(ALEFVM_Param%IEnabled==0)THEN        
           CALL AFLUX3(PM,IXS,V,W,X,FLUX(6*NFT+1),FLU2(NF1),ALE_CONNECT,NSG,GBUF%TAG22)
         ELSE
           CALL ALEFVM_AFLUX3(PM       ,IXS  ,V    ,W           ,FLUX(6*NFT+1),
     2                        FLU2(NF1),VEUL ,ALE_CONNECT,GBUF%TAG22   ,
     3                        IPM      ,NV46 ,GBUF%MOM  ,ITASK  ,X            ,
     4                        NEL      )        
         ENDIF
        ELSEIF(N2D /= 0.AND.JEUL == 0)THEN
         CALL AFLUX2(PM,IXQ,V,W,X,
     .               FLUX(4*NFT+1),FLU2(NF1),
     .               FILL,DFILL,VOL,ALE_CONNECT,ALPH)
        ELSEIF(N2D == 0.AND.JEUL /= 0)THEN
         IF(ALEFVM_Param%IEnabled==0)THEN
           CALL EFLUX3(PM,IXS,V,FLUX(6*NFT+1),FLU2(NF1),VEUL,ALE_CONNECT,GBUF%TAG22)
         ELSE
           CALL ALEFVM_EFLUX3(PM   ,IXS     ,V           ,FLUX(6*NFT+1),FLU2(NF1),
     2                        VEUL ,ALE_CONNECT   ,GBUF%TAG22  ,IPM      ,
     3                        NV46 ,GBUF%MOM,ITASK       ,X            ,NEL      )
         ENDIF
        ELSEIF(N2D /= 0.AND.JEUL /= 0)THEN
         CALL EFLUX2(PM,IXQ,V,X,FLUX(4*NFT+1),FLU2(NF1),FILL,DFILL,VOL,ALE_CONNECT,ALPH)
        ENDIF
      ENDDO
      
C-----------
      CALL MY_BARRIER
C-----------      
      
      IF(INT22 > 0)THEN
        IF(I22_ALEUL == 2)THEN
          CALL EFLUX3_INT22_FVM(
     .      PM         , IXS          , V            , FLUX(1)   , FLU2(1)   ,
     .      VEUL       , IPARG        , ELBUF_STR    , ITASK     ,
     .      NV46       , IPM          , X)         
        ELSE
          CALL AFLUX3_INT22_FVM(
     .      PM         , IXS          , V            , FLUX(1)   , FLU2(1)   ,
     .      VEUL       , IPARG        , ELBUF_STR    , ITASK     ,
     .      NV46       , IPM          , X            , W)         
       ENDIF
      ENDIF
      
C-----------      
      CALL MY_BARRIER      
C-----------

      RETURN
      END SUBROUTINE AFLUX0
