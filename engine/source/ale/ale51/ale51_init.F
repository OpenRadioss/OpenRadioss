Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  ALE51_INIT                    source/ale/ale51/ale51_init.F 
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        ALE51_UPWIND2                 source/ale/ale51/ale51_upwind2.F
Chd|        ALE51_UPWIND3                 source/ale/ale51/ale51_upwind3.F
Chd|        ALE51_UPWIND3_INT22           source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE ALE51_INIT(IPARG  ,PM      ,IXS     ,IXQ           ,
     2                      V      ,W       ,X       ,FLUX          ,FLU2    , 
     3                      MS     ,VEUL    ,ALPHA   ,ALE_CONNECT   ,ITASK   , 
     4                      ITRIMAT,FLUX_SAV,QMV     ,NV46          ,ELBUF_TAB)          
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD           
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
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB      
      my_real PM(NPROPM,NUMMAT), V(3,NUMNOD), W(3,NUMNOD), X(3,NUMNOD),
     .        FLUX(NV46,*), FLU2(*), MS(*), VEUL(*), 
     .        ALPHA(*), FLUX_SAV(NV46,*), QMV(*)
      INTEGER IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(7,NUMELQ), ITASK,ITRIMAT,NV46, J
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NG, K, II, NF1
      my_real :: BID(1)
      INTEGER :: NIN, IB, NBF, NBL, IE, MLW
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      CALL MY_BARRIER

C-----------------------------------------------
C     VOLUME FLUXES BACKUP
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        CALL INITBUF (IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        !--------------------!
        ! UNPLUG CONDITIONS  !
        !--------------------!
        IF(JALE+JEUL == 0)    CYCLE
        IF(IPARG(8,NG) == 1)  CYCLE
        IF(IPARG(1,NG) /= 51) CYCLE
        !------------------------------!
        LFT=1                   
        DO K=1,NV46
            DO II=NFT+LFT,NFT+LLT
              FLUX_SAV(K,II)=FLUX(K,II)
            ENDDO
        ENDDO        
      END DO !next NG

C--------------------
      CALL MY_BARRIER
C--------------------

C-----------------------------------------------
C    UPDATE VOLUME FLUXES & UPWIND
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        CALL INITBUF (IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF(JALE+JEUL == 0)    CYCLE
        IF(IPARG(8,NG) == 1)  CYCLE
        IF(IPARG(1,NG) /= 51) CYCLE
        LFT=1
        NF1=NFT+1
        !------------------------------!
        ! UPWIND, QMV, DDVOL           !
        !------------------------------!
        IF(N2D == 0)THEN
          CALL ALE51_UPWIND3(PM,IXS,FLUX(1,NF1),FLU2(NF1),ALE_CONNECT,
     +                       0 ,BID,QMV(12*NFT+1),1,
     +                       NV46 )
        ELSE
          CALL ALE51_UPWIND2(PM,X,IXQ,FLUX(1,NF1),FLU2(NF1),ALE_CONNECT,
     +                       0,BID,QMV(8*NFT+1),1)
        ENDIF
      END DO !next NG

C--------------------
      CALL MY_BARRIER
C--------------------

      IF(INT22 /= 0)THEN !OBSOLETE
        !Restore Direct Fluxes
        NIN = 1
        NBF = 1+ITASK*NB/NTHREAD
        NBL = (ITASK+1)*NB/NTHREAD
        NBL = MIN(NBL,NB)
        DO IB=NBF,NBL
          IE  = BRICK_LIST(NIN,IB)%ID 
          MLW = BRICK_LIST(NIN,IB)%MLW 
          IF(MLW /= 51)CYCLE
          DO J=1,6
          BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_UpwFLUX(1) = BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_FLUX(1)
          BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_UpwFLUX(2) = BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_FLUX(2)
          BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_UpwFLUX(3) = BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_FLUX(3)
          BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_UpwFLUX(4) = BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_FLUX(4)
          BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_UpwFLUX(5) = BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(J)%Adjacent_FLUX(5)
          ENDDO
        ENDDO   
        
        !Computing Upwind fluxes
        CALL ALE51_UPWIND3_INT22
     +                      (PM  , IXS  ,   0     ,  1, 
     +                       NV46, IPARG,   ELBUF_TAB  ,ITASK     )
      ENDIF

C-----------------------------------------------
      RETURN
      END
C
