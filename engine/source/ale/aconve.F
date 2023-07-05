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
Chd|  ACONVE                        source/ale/aconve.F           
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        A22CONV3                      source/ale/alefvm/cut_cells/a22conv3.F
Chd|        A4CONV3                       source/ale/ale3d/a4conv3.F    
Chd|        ACONV2                        source/ale/ale2d/aconv2.F     
Chd|        ACONV3                        source/ale/ale3d/aconv3.F     
Chd|        BCONV2                        source/ale/ale2d/bconv2.F     
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        VARCONDEC                     source/interfaces/interf/varcondec.F
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE ACONVE(
     1  IPARG       ,ELBUF_TAB ,FLUX    ,FLU1     ,PHI     ,
     2  ALE_CONNECT ,NVAR      ,ITASK   ,NERCVOIS ,NESDVOIS,
     3  LERCVOIS    ,LESDVOIS  ,LENCOM  ,SEGVAR   ,BHOLE   ,
     4  ITRIMAT     ,QMV       ,IFLG    ,IXS      ,IXQ     ,
     5  PM          ,IPM       ,BUFMAT  ,X  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE I22TRI_MOD
      USE I22BUFBRIC_MOD
      USE SEGVAR_MOD      
      USE ALE_CONNECTIVITY_MOD
      USE ALEFVM_MOD , only:ALEFVM_Param
      USE ALE_MOD
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
#include      "spmd_c.inc"
#include      "inter22.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB

      INTEGER IPARG(NPARG,NGROUP),NVAR,ITRIMAT, 
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*), LESDVOIS(*),
     .        BHOLE(*),IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ), LENCOM,
     .        IFLG, ITASK, BUFMAT(*),IPM(NPROPMI,NUMMAT) 

      my_real FLUX(*), FLU1(*)          , PHI(*)     , 
     .        QMV(*) , PM(NPROPM,NUMMAT), X(3, NUMNOD)
     
      TYPE(t_segvar) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NMN, NM, NG, JMUL, IADR,  I, J, K, NF1,ISILENT,NFX,  IOFF, IMAT
      INTEGER JCODV(ALE%GLOBAL%LCONV),CODTOT,NGSEG,ISEG,ISOLNOD
      INTEGER ADD0, ADD

      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(G_BUFEL_)  ,POINTER :: GBUF           
      TYPE(BUF_MAT_)  ,POINTER :: MBUF 

      my_real, DIMENSION(:), POINTER ::  VAR, pRHO , pVOL , pEINT, pIAD22 
      INTEGER                        :: ICELLv,IB,IBv,NIN,NUM, MCELL, IDX, NDIM
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------  
C--------------------
      CALL MY_BARRIER
C--------------------

      NULLIFY (VAR)

C=======================================================================
C PHI IS DEFINED DEPENDING ON NVAR VALUE
C=======================================================================

      NMN=MAX(1,NMULT)
      
      DO NM=1,NMN
        DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
          CALL INITBUF(IPARG    ,NG      ,
     2         MTN     ,LLT     ,NFT     ,IADR    ,ITY     ,
     3         NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4         JTHE    ,JLAG    ,JMUL    ,JHBE    ,JIVF    ,
     5         NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6         IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7         ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
         ISILENT = IPARG(64,NG)
         !-----------------------------!
         ! UNPLUG CONDITIONS           !
         !-----------------------------!
         IF (JALE+JEUL == 0)              CYCLE
         IF (IPARG(8,NG) == 1)            CYCLE
         IF (MAX(1,JMUL) < NM)            CYCLE
         IF (ITRIMAT /= 0.AND.MTN /= 51)  CYCLE
         !-----------------------------!
         ! POINTERS FOR CURRENT GROUP  !
         !-----------------------------!
         GBUF => ELBUF_TAB(NG)%GBUF
         LBUF => ELBUF_TAB(NG)%BUFLY(NM)%LBUF(1,1,1)
         MBUF => ELBUF_TAB(NG)%BUFLY(NM)%MAT(1,1,1)

         CALL VARCONDEC(JCODV,IPARG(34,NG),CODTOT)
         IF (JCODV(NVAR) /= 0) THEN
           ISOLNOD = IPARG(28,NG)
           IF (JMUL /= 0) MTN =IPARG(24+NM,NG)
           LFT=1
          !----------------------------!
          !        N V A R = 1         !
          !           in : RHO         !
          !          out : RHO * VOL   !
          !----------------------------!
          IF (NVAR == 1) THEN
            IF(ITRIMAT==0)THEN
              pRHO => LBUF%RHO(1:LLT) 
              pVOL => LBUF%VOL(1:LLT)
            ELSE
              !USE PHASIS DATA
              ADD0 =  N0PHAS + (ITRIMAT-1)*NVPHAS                
              ADD  =  ADD0 + 9                                   ! ADD+9 => RHO
              K    =  LLT*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I)
              pRHO => MBUF%VAR(K+1:K+LLT)
              ADD  =  ADD0 + 11                                  ! ADD+11 => VOL
              K    =  LLT*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I) 
              pVOL => MBUF%VAR(K+1:K+LLT)
              !dbKEY="R/V TRIMAT "
            END IF
#include "vectorize.inc"
            DO I=LFT,LLT                            
              J=I+NFT     
              PHI(J)=pRHO(I)                    
            ENDDO                                   
            DO I=LFT,LLT  
              pRHO(I) = pRHO(I)*pVOL(I)
            ENDDO   
          !----------------------------!
          !        N V A R = 2         !
          !           in : E     (J/m3)!
          !          out : EINT  (J)   !
          !----------------------------!
          ELSEIF (NVAR == 2) THEN 
            IF(ITRIMAT==0)THEN
              pEINT=> LBUF%EINT(1:LLT)
              pVOL => LBUF%VOL(1:LLT)
            ELSE
              !USE PHASIS DATA
              ADD0 =  N0PHAS + (ITRIMAT-1)*NVPHAS                
              ADD  =  ADD0 + 8                                   ! ADD+8 => EINT
              K    =  LLT*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I)
              pEINT=> MBUF%VAR(K+1:K+LLT)
              ADD  =  ADD0 + 11                                  ! ADD+11 => VOL
              K    =  LLT*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I) 
              pVOL => MBUF%VAR(K+1:K+LLT)
            END IF                 
#include "vectorize.inc"
            DO I=LFT,LLT                              
              J=I+NFT                                 
              PHI(J)=pEINT(I)                     
            ENDDO                                     
            DO I=LFT,LLT                              
              pEINT(I) = pEINT(I)*pVOL(I)  
            ENDDO                                     
          !----------------------------!
          !        N V A R = 3         !
          !----------------------------!
          ELSEIF (NVAR == 3) THEN   !  JTUR > 0              
#include "vectorize.inc"
            DO I=LFT,LLT                              
              J=I+NFT                                 
              PHI(J)=LBUF%RK(I)                     
            ENDDO                                     
            DO I=LFT,LLT                              
              LBUF%RK(I) = LBUF%RK(I)*LBUF%VOL(I)  
            ENDDO                                     
          !----------------------------!
          !        N V A R = 4         !
          !----------------------------!
          ELSEIF (NVAR == 4) THEN   !  JTUR > 0                  
#include "vectorize.inc"
            DO I=LFT,LLT                              
              J=I+NFT                                 
              PHI(J)=LBUF%RE(I)                     
            ENDDO                                     
            DO I=LFT,LLT                              
              LBUF%RE(I) = LBUF%RE(I)*LBUF%VOL(I)  
            ENDDO                                     
          !----------------------------!
          !        N V A R = 5         !
          !----------------------------!
          !UVAR(I,1) : rho_liq*V_liq/V 
          !UVAR(I,2) : density of gas
          !UVAR(I,3) : density of liquid
          !UVAR(I,4) : volumetric fraction of liquid
          !UVAR(I,5) : volumetric fraction of gas         
          ELSEIF (NVAR == 5) THEN   !  Law 37 or law 41              
            VAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(1:LLT)  ! <---- UVAR(1:LLT,1)
            IF (MTN  ==  41) THEN
               pRHO => GBUF%RHO
#include "vectorize.inc"
               DO I=LFT,LLT                              
                  J=I+NFT                                 
                  PHI(J) = VAR(I) * pRHO(I)                  
               ENDDO                                     
               DO I=LFT,LLT                              
                  VAR(I) = VAR(I) * pRHO(I) * LBUF%VOL(I)  
               ENDDO    
            ELSE
#include "vectorize.inc"
               DO I=LFT,LLT                              
                  J=I+NFT                                 
                  PHI(J) = VAR(I)                     
               ENDDO                                     
               DO I=LFT,LLT                              
                  VAR(I) = VAR(I)*LBUF%VOL(I)  
               ENDDO     
            ENDIF
          !-------------------------------!
          !        N V A R = 6            !
          !           in : RHO*v_x        !
          !          out : RHO*v_x *VOL   !         
          !-------------------------------!   
          ELSEIF (NVAR == 6) THEN
            IF(ITRIMAT==0)THEN
              IDX      = 1
              NDIM     = 3
              VAR => GBUF%MOM(1:LLT*NDIM)  
#include "vectorize.inc"
              DO I=LFT,LLT                              
                J      = I+NFT      
                K      = LLT*(IDX-1) + I 
                PHI(J) = VAR(K)  
                VAR(K) = VAR(K) *LBUF%VOL(I)   ![rhoU]*VOL, U needed for fluxes calculation                  
              ENDDO                                     
            ENDIF            
          
          !-----------------------------!
          !        N V A R = 7          !  
          !           in : RHO*v_y      !
          !          out : RHO*v_y *VOL !               
          !-----------------------------!         
          ELSEIF (NVAR == 7) THEN
            IF(ITRIMAT==0)THEN         
              IDX      = 2
              NDIM     = 3
              VAR => GBUF%MOM(1:LLT*NDIM)  
#include "vectorize.inc"
              DO I=LFT,LLT                              
                J      = I+NFT      
                K      = LLT*(IDX-1) + I 
                PHI(J) = VAR(K)  
                VAR(K) = VAR(K) *LBUF%VOL(I)   ![rhoU]*VOL, U needed for fluxes calculation                                                    
              ENDDO                                     
            ENDIF  

          !-----------------------------!
          !        N V A R = 8          !  
          !           in : RHO*v_z      !
          !          out : RHO*v_z *VOL !               
          !-----------------------------!                     
          ELSEIF (NVAR == 8) THEN
            IF(ITRIMAT==0)THEN
              IDX      = 3
              NDIM     = 3
              VAR      => GBUF%MOM(1:LLT*NDIM)  
#include "vectorize.inc"
              DO I=LFT,LLT                              
                J      = I+NFT      
                K      = LLT*(IDX-1) + I 
                PHI(J) = VAR(K)                     
                VAR(K) = VAR(K) *LBUF%VOL(I)   ![rhoU]*VOL, U needed for fluxes calculation                                 
              ENDDO                                     
            ENDIF   
                       
          ELSEIF (NVAR == 9 .AND. ISILENT==1) THEN
          ELSEIF (NVAR ==10 .AND. ISILENT==1) THEN
          ELSE
            !--------------------------!
            !    DEFAULT 3D CASE       !
            !--------------------------! 
            IF (N2D == 0) THEN
#include "vectorize.inc"
              DO I=LFT,LLT
               J=I+NFT
               IMAT=IXS(1,J)
               PHI(J)=PM(180+NVAR,IMAT)*LBUF%RHO(I)
              END DO
            !--------------------------!
            !    DEFAULT 2D CASE       !
            !--------------------------! 
            ELSE
#include "vectorize.inc"
              DO I=LFT,LLT
                J=I+NFT
                IMAT=IXQ(1,J)
                PHI(J)=PM(180+NVAR,IMAT)*LBUF%RHO(I)
              END DO
            END IF
          END IF
         ELSE
           DO I=LFT,LLT
             J=I+NFT
             PHI(J)=ZERO
           ENDDO
         ENDIF
         
          !--------------------------!
          !    INTERFACE 22          !
          !--------------------------! 
         !Fill Secnd cells linked to current main cell with main cell value.
         IF(INT22>0)THEN
           NIN = 1
           pIAD22 => ELBUF_TAB(NG)%GBUF%TAG22(LFT:LLT) 
           DO I=LFT,LLT                   
             J                                      = I+NFT          
             IB                                     = pIAD22(I)
             IF(IB==0)CYCLE
             NUM                                    = BRICK_LIST(NIN,IB)%SecndList%Num
             MCELL                                  = BRICK_LIST(NIN,IB)%mainID
             BRICK_LIST(NIN,IB)%POLY(MCELL)%PHI     = PHI(J)            
             DO K=1,NUM
               IBV                                  = BRICK_LIST(NIN,IB)%SecndList%IBV(K)
               ICELLv                               = BRICK_LIST(NIN,IB)%SecndList%ICELLv(K)
               BRICK_LIST(NIN,IBv)%POLY(ICELLv)%PHI = PHI(J)
             ENDDO
           ENDDO!next I
         ENDIF
         
        ENDDO !next NG
        
        
        !--------------------------!
        !    EBCS ****             !
        !--------------------------! 
        IOFF = 0
        IF(NSEGFLU > 0)THEN
          IOFF = NUMELS+NUMELQ+NUMELTG
          IF(NSPMD > 1) THEN
            IOFF = IOFF + NSVOIS
          ENDIF
          NGSEG=NSEGFLU/NVSIZ
          IF(NSEGFLU-NGSEG*NVSIZ  > 0)NGSEG=NGSEG+1
          DO I=ITASK+1,NGSEG,NTHREAD
            ISEG=(I-1)*NVSIZ
            SELECT CASE(NVAR)
              !RHO
              CASE(1)
                IF(ITRIMAT==0)THEN
                  DO J=ISEG+1,MIN(ISEG+NVSIZ,NSEGFLU)
                     PHI(IOFF+J)=SEGVAR%RHO(J)
                  ENDDO
                ELSE
                  DO J=ISEG+1,MIN(ISEG+NVSIZ,NSEGFLU)
                     PHI(IOFF+J)=SEGVAR%PHASE_RHO(ITRIMAT,J)
                  ENDDO                
                ENDIF
              !EINT
              CASE(2)
                IF(ITRIMAT==0)THEN
                  DO J=ISEG+1,MIN(ISEG+NVSIZ,NSEGFLU)
                     PHI(IOFF+J)=SEGVAR%EINT(J)
                  ENDDO
                ELSE
                  DO J=ISEG+1,MIN(ISEG+NVSIZ,NSEGFLU)
                     PHI(IOFF+J)=SEGVAR%PHASE_EINT(ITRIMAT,J)
                  ENDDO                
                ENDIF
              !RK    
              CASE(3)
                DO J=ISEG+1,MIN(ISEG+NVSIZ,NSEGFLU)
                   PHI(IOFF+J)=SEGVAR%RK(J)
                ENDDO 
              !RE
              CASE(4)
                DO J=ISEG+1,MIN(ISEG+NVSIZ,NSEGFLU)
                   PHI(IOFF+J)=SEGVAR%RE(J)
                ENDDO 
              !UVAR                  
              CASE(5)
                DO J=ISEG+1,MIN(ISEG+NVSIZ,NSEGFLU)
                   PHI(IOFF+J)=SEGVAR%UVAR(J)
                ENDDO 
             END SELECT                                       
          ENDDO
        ENDIF
C----------------------
        CALL MY_BARRIER    
C----------------------
        !-----------------------------------------------!
        !    SPMD EXCHANGES                             !
        !-----------------------------------------------! 
        IF (NSPMD > 1) THEN
!$OMP SINGLE
          CALL SPMD_E1VOIS(PHI,NERCVOIS,NESDVOIS,LERCVOIS, LESDVOIS,LENCOM  )
!$OMP END SINGLE
        END IF


C=======================================================================
C CONVECTION DEPENDING ON NVAR VALUE
C=======================================================================

        ! CONVECTION OFF WITH DEBUG OPTION /DEBUG/CONV0/[cycle]
        IF(DEBUG(10) /= 0)THEN
          IF(NCYCLE >= DEBUG(10))THEN
            !write (*,*) "ADVECTION SET OFF"
            CYCLE
          ENDIF
        ENDIF

        !----------------------------!
        !     CUT CELL (INTER22)     !
        !----------------------------!
        !TREAT FIRST POLYHEDRA FROM CUT CELL BUFFER
        !ELEMENTARY BUFFER IS ALSO UPDATED AND USED LATER
        !TO TREAT USUAL BRICK ELEMS WHILE DEALING WITH ADJACENT VALUES      
        IF(INT22 > 0)THEN
          NF1=NFT+1+(NM-1)*NUMELS
          NFX=NFT+(NM-1)*NUMELS      
          CALL A22CONV3(
     1             PHI      , FLUX(6*NFX+1), FLU1(NF1)    ,
     2             IOFF         , QMV(12*NFX+1), IFLG     ,
     3             ITRIMAT  , NVAR         , ITASK        ,
     4             ELBUF_TAB, IXS          , IPARG)      
        ENDIF

        DO NG=ITASK+1,NGROUP,NTHREAD                                                       
C     ALE ON / OFF
           IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
           CALL VARCONDEC(JCODV,IPARG(34,NG),CODTOT)                                         
           IF (JCODV(NVAR) == 0) CYCLE                                                       
           CALL INITBUF(IPARG    ,NG      ,                                                 
     2           MTN     ,LLT     ,NFT     ,IADR    ,ITY     ,                               
     3           NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,                               
     4           JTHE    ,JLAG    ,JMUL    ,JHBE    ,JIVF    ,                               
     5           NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,                               
     6           IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,                               
     7           ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                               
           ISILENT = IPARG(64,NG)                                                            
           IF (ISILENT==1)                   CYCLE  !silent boundaries law 11 & 51           
           IF (IPARG(8,NG) == 1)             CYCLE                                           
           IF (MAX(1,JMUL) < NM)             CYCLE                                           
           IF (ITRIMAT /= 0 .AND. MTN /= 51) CYCLE                                           

           ISOLNOD = IPARG(28,NG)                                                            
                                                                                             
           !-----------------------------!                                                   
           ! POINTERS FROM CURRENT GROUP !                                                   
           !-----------------------------!                                                   
           GBUF => ELBUF_TAB(NG)%GBUF                                                        
           LBUF => ELBUF_TAB(NG)%BUFLY(NM)%LBUF(1,1,1)                                       
           MBUF => ELBUF_TAB(NG)%BUFLY(NM)%MAT(1,1,1)                                        
           !-----------------------------!                                                   
           IF (JMUL /= 0) THEN                                                               
             MTN =IPARG(24+NM,NG)                                                            
           ENDIF                                                                             
           LFT=1                                                                             

           !----------------------------!                                                    
           !        N V A R = 1         !                                                    
           !----------------------------!                                                    
           IF (NVAR == 1) THEN                                                               
              IF(ITRIMAT==0)THEN                                                             
                pRHO => LBUF%RHO(1:LLT)                                                      
              ELSE                                                                           
                !USE PHASIS DATA                                                             
                ADD0 =  N0PHAS + (ITRIMAT-1)*NVPHAS                                          
                ADD  =  ADD0 + 9                                   ! ADD+9 => RHO            
                K    =  LLT*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I) 
                pRHO => MBUF%VAR(K+1:K+LLT)                                                  
              END IF                                                                         
             VAR => pRHO                                                                     
           !----------------------------!                                                    
           !        N V A R = 2         !                                                    
           !----------------------------!                                                    
           ELSEIF (NVAR == 2) THEN                                                           
              IF(ITRIMAT==0)THEN                                                             
                pEINT=> LBUF%EINT(1:LLT)                                                     
              ELSE                                                                           
                !USE PHASIS DATA                                                             
                ADD0 =  N0PHAS + (ITRIMAT-1)*NVPHAS                                          
                ADD  =  ADD0 + 8                                   ! ADD+9 => RHO            
                K    =  LLT*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I) 
                pEINT => MBUF%VAR(K+1:K+LLT)                                                 
              END IF                                                                         
             VAR => pEINT                                                                    
           !----------------------------!                                                    
           !        N V A R = 3         !                                                    
           !----------------------------!                                                    
           ELSEIF (NVAR == 3) THEN                                                           
             VAR => ELBUF_TAB(NG)%BUFLY(NM)%LBUF(1,1,1)%RK(1:LLT)                            
           !----------------------------!                                                    
           !        N V A R = 4         !                                                    
           !----------------------------!                                                    
           ELSEIF (NVAR == 4) THEN                                                           
             VAR => ELBUF_TAB(NG)%BUFLY(NM)%LBUF(1,1,1)%RE(1:LLT)                            
           !----------------------------!                                                    
           !        N V A R = 5         !                                                    
           !----------------------------!                                                    
           ELSEIF (NVAR == 5) THEN                                                           
             VAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(1:LLT)                             
           !----------------------------!                                                    
           !        N V A R = 6         !                                                    
           !----------------------------!                                                    
           ELSEIF (NVAR == 6) THEN                                                           
             IF(ALEFVM_Param%IEnabled==0)THEN                                                              
               IF (MTN == 51 .AND. ITRIMAT /= 0) THEN                                        
                 VAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(5*LLT+1:6*LLT)                 
               ELSE                                                                          
                 VAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(LLT+1:2*LLT)                   
               ENDIF                                                                         
             ELSE                                                                                                                                           
               VAR => ELBUF_TAB(NG)%GBUF%MOM(  1 : LLT )                                                                                   
             ENDIF                                                                           
           !----------------------------!                                                    
           !        N V A R = 7         !                                                    
           !----------------------------!                                                    
           ELSEIF (NVAR == 7) THEN                                                           
             IF(ALEFVM_Param%IEnabled==0)THEN                                                              
               VAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(2*LLT+1:3*LLT)                   
             ELSE                                                                                                                                          
               VAR => ELBUF_TAB(NG)%GBUF%MOM(  LLT*1+1 : LLT*1+LLT )                                                         
             ENDIF                                                                           
           !----------------------------!                                                    
           !        N V A R = 8         !                                                    
           !----------------------------!                                                    
           ELSEIF (NVAR == 8) THEN                                                           
             IF(ALEFVM_Param%IEnabled == 0)THEN                                                            
               VAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(3*LLT+1:4*LLT)                   
             ELSE                                                                            
               VAR => ELBUF_TAB(NG)%GBUF%MOM( LLT*2+1 : LLT*2+LLT )                                                        
             ENDIF                                                                           
           !----------------------------!                                                    
           !        N V A R = 9         !                                                    
           !----------------------------!                                                    
           ELSEIF (NVAR == 9) THEN                                                           
             VAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(4*LLT+1:5*LLT)                     
           ENDIF                                                                             

           !----------------------------!                                                    
           !        CALL ACONVE3        !                                                    
           !----------------------------!                                                    
           pIAD22 => ELBUF_TAB(NG)%GBUF%TAG22(LFT:LLT)                                       
           IF (N2D == 0) THEN                                                                
             NF1=NFT+1+(NM-1)*NUMELS                                                         
             NFX=NFT+(NM-1)*NUMELS                                                           
             pVOL => LBUF%VOL(1:LLT)                                                         
             IF (ISOLNOD /= 4) THEN                                                          
               CALL ACONV3(                                                                  
     1              VAR   , PHI    ,FLUX(6*NFX+1), FLU1(NF1) ,IXS    ,                       
     2              ALE_CONNECT , IOFF   ,QMV(12*NFX+1), IFLG      ,ITRIMAT,                       
     3              pIAD22, NVAR   ,ITASK)                                                            
             ELSE                                                                            
               CALL A4CONV3(                                                                 
     1              VAR ,PHI,FLUX(6*NFX+1),FLU1(NF1),                                        
     2              ALE_CONNECT  ,IOFF   )                                                         
             ENDIF                                                                           
           !----------------------------!                                                    
           !        CALL ACONVE2        !                                                    
           !----------------------------!                                                    
           ELSE                                                                              
             NF1=NFT+1+(NM-1)*NUMELQ                                                         
             NFX=NFT+(NM-1)*NUMELQ                                                           
             IF (NMULT == 0) THEN                                                            
               CALL ACONV2(VAR ,PHI,FLUX(4*NFX+1),FLU1(NF1),                                 
     .                     ALE_CONNECT  ,QMV(8*NFX+1),IFLG, IXQ, X, NVAR,IOFF,ITRIMAT)                                
             ELSE                                                                            
               CALL BCONV2(VAR ,PHI  ,FLUX(4*NFX+1),FLU1(NF1),                               
     .                     ALE_CONNECT  ,BHOLE  ,NM )                                              
             ENDIF                                                                           
           ENDIF !(N2D == 0)                                                                 
           !----------------------------!         
        ENDDO  ! next NG
      
      
C----------------------      
        CALL MY_BARRIER 
C----------------------
      END DO !next NM

C-----------------------------------------------  
      RETURN
      END
