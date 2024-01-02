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
Chd|  STAT_INIMAP2D_SPMD            source/output/sta/stat_inimap2d_spmd.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SPMD_STATE_INIMAP2D_EXCH_DATA source/output/sta/spmd_state_inimap2d_exch_data.F
Chd|        SPMD_STATE_INIMAP_EXCH_SIZ    source/output/sta/spmd_state_inimap_exch_siz.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        STATE_INIMAP_MOD              share/modules/state_inimap_mod.F
Chd|====================================================================
      SUBROUTINE STAT_INIMAP2D_SPMD(
     .                        X        , V        , ITAB  , IPART_STATE, NODTAG,
     .                        IPART    , IPARTS   , IPARTQ, IPARTTG    ,
     .                        IGEO     , IPARG    , IXS   , IXQ        , IXTG  , 
     .                        ELBUF_TAB, MULTI_FVM, BUFMAT, IPM)
      USE MESSAGE_MOD
      USE STATE_INIMAP_MOD
C-----------------------------------------------
C   Description
C-----------------------------------------------
C  This subroutine is generating mapping data to be used with /INIMAP2D option.
C  include file is incremented starting from ROOT_2D_0001.inimap
C  It contains 2D fonctions for submaterial data (volume fraction, mass density, energy density)
C  and also function for global velocity
C  User can use the generated file in a second run using #include command in the new Starter input file (target mesh)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE STATE_INIMAP_MOD
      USE MULTI_FVM_MOD
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
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "chara_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: ITAB(NUMNOD), IPART(LIPART1,*),IGEO(NPROPGI,NUMGEO), IXS(NIXS,NUMELS), IPART_STATE(NPART),
     .                        IPARG(NPARG,*),IXQ(NIXQ,NUMELQ),IXTG(NIXTG,NUMELTG)
      INTEGER, INTENT(INOUT) :: NODTAG(NUMNOD),
     .                          IPM(NPROPMI,*)
      INTEGER, TARGET :: IPARTS(*),IPARTQ(*),IPARTTG(*)
      my_real,INTENT(IN) :: X(3,NUMNOD),V(3,NUMNOD)
      my_real, INTENT(IN), TARGET :: BUFMAT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET, INTENT(IN) :: ELBUF_TAB
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, JJ,J, IPRT0, IPRT, K, STAT_NUMELS_1, KK, INOD
      INTEGER NG, NEL, NFT, LFT, LLT, ITY, ISOLNOD, IOFF,NPT
      INTEGER NUM_CENTROIDS, IPOS,MLW,IFORM,NBMAT,NB2,ISUBMAT,NNOD,NNOD2
      INTEGER NUVAR
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      my_real MIN_X,MIN_Y,MIN_Z,MAX_X,MAX_Y,MAX_Z,P0(3),P0_INF(3),P0_SUP(3),LENGTH
      my_real SHIFT_Cy,SHIFT_Cz
      my_real SHIFT_Ny,SHIFT_NZ
      my_real LX,LY,LZ
      my_real DX,DY,DZ
      my_real DOTPROD,TOL,XYZ(3),VEL(3),DIST,VECT(3)
      CHARACTER FILNAM*100, CHSTAT*4
      INTEGER XMIN_CELL_ID,YMIN_CELL_ID,ZMIN_CELL_ID,XMAX_CELL_ID,YMAX_CELL_ID,ZMAX_CELL_ID
      INTEGER IS_ITY_1, IS_ITY_2, IS_ITY_7, LAST_CELL,FIRST_CELL,IMAT,NPAR,IADBUF
      INTEGER, POINTER,DIMENSION(:) :: IPART_PTR
      my_real, POINTER,DIMENSION(:) :: UPARAM
      TYPE(BUF_MAT_)  ,POINTER :: MBUF  
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: GET_CELL_FOM_CENTROID        !(NG,I+NFT)
      INTEGER NPTS(NSPMD),NCELL(NSPMD),NPTS_TOT,NCELL_TOT
      my_real, DIMENSION(:,:), ALLOCATABLE :: WORK
      my_real :: LEN_(NSPMD),LEN_TOT
      my_real :: SHIFT_Cy_MIN, SHIFT_Ny_MIN, SHIFT_Cz_MIN, SHIFT_Nz_MIN
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IDX, NODTAG_G
      INTEGER :: CELL_ID,IDX1(21),IDX2(21),IDX3(21)
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      !IF(NUMELS+NUMELQ+NUMELTG == 0)RETURN
      
      IF(N2D == 0)THEN
      
        !N2D not yet set when running frestat.F ; check must be done later, here.
        IF(.NOT.IS_STAT_INIMAP_MSG_ALREADY_DISPLAYED)THEN
          IF(ISPMD == 0)THEN
             CALL ANCMSG(MSGID=288,ANMODE=ANINFO)
             IS_STAT_INIMAP_MSG_ALREADY_DISPLAYED = .TRUE.
          ENDIF
        ENDIF 
             
        RETURN
        
      ENDIF
   
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      
      !---INITIALIZATION
      !
      STATE_INIMAP_CALL_NUMBER = STATE_INIMAP_CALL_NUMBER +1 !number of written files ROOT_INIMAP_00**.rad
      NUM_CENTROIDS = 0  
      MLW=0  
      !box containing user domain :
      MIN_X = EP20                                                                                                              
      MIN_Y = EP20                                                                                                              
      MIN_Z = EP20                                                                                                              
      MAX_X = -EP20                                                                                                             
      MAX_Y = -EP20                                                                                                             
      MAX_Z = -EP20                        
      !detecting elem types to manager error messages :                                                                                                 
      IS_ITY_1 = 0
      IS_ITY_2 = 0
      IS_ITY_7 = 0
 
      !---ALLOCATIONS
      !
      IF(.NOT.(ALLOCATED(STATE_INIMAP_BUF))) THEN
        IF(ISPMD/=0)THEN
          ALLOCATE(STATE_INIMAP_BUF(1))                                                                      
        ELSE
          ALLOCATE(STATE_INIMAP_BUF(NSPMD))  !process 0 will gather all data
        ENDIF       
      ENDIF
                                                                                                                  
      !---ENUMARATION : ELEM TYPES AND BOX DIMENSION
      !
      DO NG=1,NGROUP                                                                                                          
        ITY   =IPARG(5,NG)                                                                                                    
        ISOLNOD = IPARG(28,NG)                                                                                                
        NEL   =IPARG(2,NG)                                                                                                    
        NFT   =IPARG(3,NG)                                                                                                    
        GBUF => ELBUF_TAB(NG)%GBUF                                                                                            
        MLW = IPARG(1,NG)                                                                                                     
        LFT=1                                                                                                                 
        LLT=NEL                                                                                                               
        NPT=0                                                                                                                 
        IF(ITY == 1) THEN                                                                                                     
          !---bricks                                                                                                          
          IS_ITY_1=1                                                                                                          
          NPT=ISOLNOD                                                                                                         
          IPART_PTR => IPARTS(1:NUMELS)                                                                                       
        ELSEIF(ITY == 2)THEN                                                                                                  
          !---quads                                                                                                           
          IS_ITY_2=1                                                                                                          
          NPT=4                                                                                                               
          IPART_PTR => IPARTQ(1:NUMELQ)                                                                                       
        ELSEIF(ITY == 7 .AND. N2D /= 0)THEN                                                                                   
          !---Triangles                                                                                                       
          IS_ITY_7=1                                                                                                          
          NPT=3                                                                                                               
          IPART_PTR => IPARTTG(1:NUMELTG)                                                                                     
        ENDIF                                                                                                                 
        IF(NPT /= 0)THEN                                                                                                      
          DO I=LFT,LLT                                                                                                        
            N  = I + NFT                                                                                                      
            IPRT=IPART_PTR(N)                                                                                                 
            IMAT =IPART(1,IPRT)                                                                                               
            IF(IPART_STATE(IPRT)==0)CYCLE                                                                                     
              NUM_CENTROIDS = NUM_CENTROIDS +1                                                                                
              DO K=1,NPT                                                                                                      
                IF(IS_ITY_1==1)INOD=IXS(1+K,N)                                                                                
                IF(IS_ITY_2==1)INOD=IXQ(1+K,N)  !faire passer IX (pointeur, en argument) et NIX => routine generique   
                IF(IS_ITY_7==1)INOD=IXTG(1+K,N)  !faire passer IX (pointeur, en argument) et NIX => routine generique                          
                IF(IS_ITY_1==1)NODTAG(IXS(1+K,N)) = 1
                IF(IS_ITY_2==1)NODTAG(IXQ(1+K,N)) = 1
                IF(IS_ITY_7==1)NODTAG(IXTG(1+K,N)) = 1                                                                       
                IF(X(1,INOD)<MIN_X)THEN                                                                                       
                  MIN_X=X(1,INOD)                                                                                             
                  XMIN_CELL_ID = N                                                                                            
                ENDIF                                                                                                         
                IF(X(2,INOD)<MIN_Y)THEN                                                                                       
                  MIN_Y=X(2,INOD)                                                                                             
                  YMIN_CELL_ID = N                                                                                            
                ENDIF                                                                                                         
                IF(X(3,INOD)<MIN_Z)THEN                                                                                       
                  MIN_Z=X(3,INOD)                                                                                             
                  ZMIN_CELL_ID = N                                                                                            
                ENDIF                                                                                                         
                IF(X(1,INOD)>MAX_X)THEN                                                                                       
                  MAX_X=X(1,INOD)                                                                                             
                  XMAX_CELL_ID = N                                                                                            
                ENDIF                                                                                                         
                IF(X(2,INOD)>MAX_Y)THEN                                                                                       
                  MAX_Y=X(2,INOD)                                                                                             
                  YMAX_CELL_ID = N                                                                                            
                ENDIF                                                                                                         
                IF(X(3,INOD)>MAX_Z)THEN                                                                                       
                  MAX_Z=X(3,INOD)                                                                                             
                  ZMAX_CELL_ID = N                                                                                            
                ENDIF                                                                                                         
              ENDDO                                                                                                           
          END DO                                                                                                              
        ELSE                                                                                                                  
          !no related cells: bricks, quads, and triangles only                                                                
        END IF                                                                                                                
      END DO! next NG                                                                                                         

      !---NUMBERING : 
      !   +--CENTROIDS
      STATE_INIMAP_BUF(1)%NUM_CENTROIDS = NUM_CENTROIDS                                                                          
      STATE_INIMAP_BUF(1)%NUM_POINTS = 0                                                                         
      !   +--FIRST ESTIMATION OF NODE NUMBER (DUPLICATED PROJECTIONS AT THIS STEP)
      NNOD=0                                                                                                                    
      DO I=1,NUMNOD                                                                                                           
         IF(NODTAG(I) == 1)THEN                                                                              
           NNOD=NNOD+1                                                                                                            
         ENDIF                                                                                                                
      ENDDO                                                                                   
                                                                                                               
      !---ANOTHER ALLOCATIONS  
      !                                                                                                                    
      IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%CELL_IDS))ALLOCATE(STATE_INIMAP_BUF(1)%CELL_IDS(NUM_CENTROIDS)) 
      IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%POS_CENTROIDS))ALLOCATE(STATE_INIMAP_BUF(1)%POS_CENTROIDS(NUM_CENTROIDS)) 
      IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%POS2_CENTROIDS))ALLOCATE(STATE_INIMAP_BUF(1)%POS2_CENTROIDS(NUM_CENTROIDS)) 
      IF(.NOT.ALLOCATED(GET_CELL_FOM_CENTROID))THEN
        ALLOCATE(GET_CELL_FOM_CENTROID(2,NUM_CENTROIDS))
      ENDIF
  
      LX=ZERO
      LY=ZERO
      LZ=ZERO
      IF(NUM_CENTROIDS > 0)THEN
        !---BOX CONTAINING USER-2D-DOMAIN   
        !                                                                                      
        LX=MAX_X-MIN_X                                                                                                          
        LY=MAX_Y-MIN_Y                                    
        LZ=MAX_Z-MIN_Z                                                                                                          
        VECT(1:3)=(/LX,LY,LZ/)                                                                                                  
        LENGTH = SQRT(VECT(2)*VECT(2) + VECT(3)*VECT(3))  !1d axis length (needed to normalize following dot products) 
        IF(IS_ITY_1==1)THEN
          P0_INF(2) = SUM( X(2,IXS(2:9,YMIN_CELL_ID)) ) / NPT                                                      
          P0_INF(3) = SUM( X(3,IXS(2:9,ZMIN_CELL_ID)) ) / NPT  
        ELSEIF(IS_ITY_2==1)THEN
          P0_INF(2) = SUM( X(2,IXQ(2:5,YMIN_CELL_ID)) ) / NPT                                                      
          P0_INF(3) = SUM( X(3,IXQ(2:5,ZMIN_CELL_ID)) ) / NPT  
        ELSEIF(IS_ITY_7==1)THEN
          P0_INF(2) = SUM( X(2,IXTG(2:4,YMIN_CELL_ID)) ) / NPT                                                      
          P0_INF(3) = SUM( X(3,IXTG(2:4,ZMIN_CELL_ID)) ) / NPT  
        ENDIF      
        SHIFT_Cy = P0_INF(2)
        SHIFT_Cz = P0_INF(3)
        STATE_INIMAP_BUF(1)%SHIFT_Cy = SHIFT_Cy
        STATE_INIMAP_BUF(1)%SHIFT_Cz = SHIFT_Cz      
        STATE_INIMAP_BUF(1)%LENGTH = LENGTH
      ELSE
        STATE_INIMAP_BUF(1)%SHIFT_Cy = ZERO
        STATE_INIMAP_BUF(1)%SHIFT_Cz = ZERO      
        STATE_INIMAP_BUF(1)%LENGTH = ZERO
      ENDIF

      !---STARTING POINT OF FIRST NODE IN GENERAL FRAME (SHIFT_N)
      !   SHIFT_N : nodal posisiton (first node along axis)
      !   SHIFT_C : cell centroid position (first centroid along axis)
      !   by nature : SHIFT_N < SHIFT_C
      !
      SHIFT_Ny=MIN_Y 
      SHIFT_Nz=MIN_Z      
      STATE_INIMAP_BUF(1)%SHIFT_Ny = SHIFT_Ny
      STATE_INIMAP_BUF(1)%SHIFT_Nz = SHIFT_Nz
      
      !---ABSCISSA : CENTROIDS POSITION LIST ( STATE_INIMAP_BUF(1)%POS_CENTROIDS(1:NUM_CENTROIDS) )
      !
      ALLOCATE(WORK(NUM_CENTROIDS,3))
      K=1                                                                                              
      DO NG=1,NGROUP                                                                                   
        ITY   =IPARG(5,NG)                                                                             
        ISOLNOD = IPARG(28,NG)                                                                         
        NEL   =IPARG(2,NG)                                                                             
        NFT   =IPARG(3,NG)                                                                             
        GBUF => ELBUF_TAB(NG)%GBUF    
        MLW = IPARG(1,NG)                                                                    
        LFT=1                                                                                          
        LLT=NEL                                                                                        
        IF(NPT /= 0)THEN                                                                                     
          DO I=LFT,LLT                                                                                 
            N  = I + NFT                                                                               
            IPRT=IPART_PTR(N)                                                                             
            IF(IPART_STATE(IPRT)==0)CYCLE 
            !pointer here
            IF(IS_ITY_1==1)THEN
               P0(2) = SUM( X(2,IXS(2:9,N)) ) / NPT                                                      
               P0(3) = SUM( X(3,IXS(2:9,N)) ) / NPT 
               CELL_ID = IXS(NIXS,N)
             ELSEIF(IS_ITY_2==1)THEN
               P0(2) = SUM( X(2,IXQ(2:5,N)) ) / NPT                                                      
               P0(3) = SUM( X(3,IXQ(2:5,N)) ) / NPT 
               CELL_ID = IXQ(NIXQ,N)               
             ELSEIF(IS_ITY_7==1)THEN
               P0(2) = SUM( X(2,IXTG(2:4,N)) ) / NPT                                                      
               P0(3) = SUM( X(3,IXTG(2:4,N)) ) / NPT 
               CELL_ID = IXTG(NIXTG,N)                              
             ENDIF                                                                   
             !STATE_INIMAP_BUF(1)%POS_CENTROIDS(K) = P0(2)   
             !STATE_INIMAP_BUF(1)%POS2_CENTROIDS(K) = P0(3)                                                           
             !STATE_INIMAP_BUF(1)%CELL_IDS(K) = CELL_ID 
             WORK(K,1) = P0(2)   
             WORK(K,2) = P0(3)                                                           
             WORK(K,3) = CELL_ID              
             GET_CELL_FOM_CENTROID(1,K) = NG                                              
             GET_CELL_FOM_CENTROID(2,K) = I                                                                     
             K=K+1                                                                                     
          END DO                                                                                       
        END IF                                                                                         
      END DO! next NG 
      

      !---SORTING : UNIQUE OUTPUT NOT DEPENDING ON DOMAIN DECOMPOSITION    (by cell_ids)
      IF(.NOT.ALLOCATED(IDX))ALLOCATE(IDX(NUM_CENTROIDS))
      DO K=1,NUM_CENTROIDS ; IDX(K)=K; ENDDO
      IF(NUM_CENTROIDS>0) CALL QUICKSORT(WORK(:,3), IDX, 1, NUM_CENTROIDS) 

      DO K=1,NUM_CENTROIDS
        STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)  = WORK(IDX(K),1)
        STATE_INIMAP_BUF(1)%POS2_CENTROIDS(K) = WORK(IDX(K),2)                                                       
        STATE_INIMAP_BUF(1)%CELL_IDS(K)       = WORK(K,3)      
      ENDDO
      IF(ALLOCATED(WORK))DEALLOCATE(WORK)


      !---CELL DATA TREATMENT
      !    storing submaterial data : vfrac,rho,E
      !
      IF(NUM_CENTROIDS > 0)THEN
        IF(MLW==151)THEN
          NBMAT = MULTI_FVM%NBMAT
        ELSEIF(MLW==51)THEN
          NBMAT = 4
        ELSE
          NBMAT = 1
        ENDIF
        STATE_INIMAP_BUF(1)%MLW = MLW      
        STATE_INIMAP_BUF(1)%NSUBMAT = NBMAT
        ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(NBMAT))
        DO I=1,NBMAT
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(I)%VFRAC(NUM_CENTROIDS))    
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(I)%RHO(NUM_CENTROIDS)) 
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(I)%E(NUM_CENTROIDS))                   
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(I)%PRES(NUM_CENTROIDS))                   
        ENDDO        
        IF(MLW==151)THEN
          !velocities
          STATE_INIMAP_BUF(1)%NUM_POINTS = NUM_CENTROIDS
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%POS_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%POS_NODES(NUM_CENTROIDS))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%POS2_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%POS2_NODES(NUM_CENTROIDS))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%VEL_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES(NUM_CENTROIDS))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%VEL2_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%VEL2_NODES(NUM_CENTROIDS))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%NODE_IDS))ALLOCATE(STATE_INIMAP_BUF(1)%NODE_IDS(NUM_CENTROIDS))
          
          DO K=1, NUM_CENTROIDS   
            NG = GET_CELL_FOM_CENTROID(1,IDX(K))
            I = GET_CELL_FOM_CENTROID(2,IDX(K))
            NFT = IPARG(3,NG)                     
            STATE_INIMAP_BUF(1)%POS_NODES(K) = STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)
            STATE_INIMAP_BUF(1)%POS2_NODES(K) = STATE_INIMAP_BUF(1)%POS2_CENTROIDS(K)
            STATE_INIMAP_BUF(1)%VEL_NODES(K) = MULTI_FVM%VEL(2,I+NFT)
            STATE_INIMAP_BUF(1)%VEL2_NODES(K) = MULTI_FVM%VEL(3,I+NFT) 
            STATE_INIMAP_BUF(1)%NODE_IDS(K) = STATE_INIMAP_BUF(1)%CELL_IDS(K) 
          ENDDO
          !submat
          DO ISUBMAT=1,NBMAT
            DO K=1, NUM_CENTROIDS   
              NG = GET_CELL_FOM_CENTROID(1,IDX(K))
              I = GET_CELL_FOM_CENTROID(2,IDX(K))
              NFT = IPARG(3,NG)                     
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%VFRAC(K) = MULTI_FVM%PHASE_ALPHA(ISUBMAT,I+NFT)
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%RHO(K) = MULTI_FVM%PHASE_RHO(ISUBMAT,I+NFT)
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%E(K) = MULTI_FVM%PHASE_EINT(ISUBMAT,I+NFT) 
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%PRES(K) = MULTI_FVM%PRES(I+NFT)
            ENDDO
          ENDDO
        ELSEIF(MLW==51)THEN
          NB2=0
          DO ISUBMAT=1,NBMAT
            DO K=1, NUM_CENTROIDS   
              NG = GET_CELL_FOM_CENTROID(1,IDX(K))
              I = GET_CELL_FOM_CENTROID(2,IDX(K))       
              NFT = IPARG(3,NG)  
              NEL = IPARG(2,NG)
              N  = I + NFT  
              IPRT=IPART_PTR(N) 
              IMAT =IPART(1,IPRT)  
              IF(IPM(20+ISUBMAT,IMAT) == 0)EXIT   !submaterial not defined. 
              NB2=MAX(NB2,IPM(5,IMAT))
              IADBUF = IPM(7,IMAT)                                                                                              
              NPAR = IPM(9,IMAT)                                                                                                
              NUVAR = IPM(8,IMAT)                                                                                               
              UPARAM => BUFMAT(IADBUF:IADBUF+NPAR)             
              KK = N0PHAS + (UPARAM(276+ISUBMAT)-1)*NVPHAS  
              MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)  
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%VFRAC(K) = MBUF%VAR(NEL*(01+KK-1)+I)
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%RHO(K) = MBUF%VAR(NEL*(12+KK-1)+I)
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%E(K)  = MBUF%VAR(NEL*(08+KK-1)+I)                        
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%PRES(K)  = MBUF%VAR(NEL*(18+KK-1)+I)   
            ENDDO
          ENDDO
          STATE_INIMAP_BUF(1)%NSUBMAT = NB2
        ELSE !---mono-material laws
          DO K=1, NUM_CENTROIDS                                                        
            NG = GET_CELL_FOM_CENTROID(1,IDX(K))                                       
            I = GET_CELL_FOM_CENTROID(2,IDX(K))                                        
            GBUF => ELBUF_TAB(NG)%GBUF         
            NEL   =IPARG(2,NG)                                                     
            STATE_INIMAP_BUF(1)%SUBMAT(1)%VFRAC(K) = 1.D00                             
            STATE_INIMAP_BUF(1)%SUBMAT(1)%RHO(K) = GBUF%RHO(I)                         
            STATE_INIMAP_BUF(1)%SUBMAT(1)%E(K)  = GBUF%EINT(I)      
            STATE_INIMAP_BUF(1)%SUBMAT(1)%PRES(K) = -THIRD * (GBUF%SIG(I) + GBUF%SIG(I + NEL) + GBUF%SIG(I + 2 * NEL))
          ENDDO                                                                        
        ENDIF
      ENDIF
      
      !---VELOCITY TREATMENT FOR STAGGERED SCHEME
      !
      IF(NUM_CENTROIDS > 0)THEN
        IF(MLW /= 151)THEN
          ALLOCATE(WORK(NUMNOD,4))
          !---VELOCITY : WRITE IN DOMAIN BUFFER (SORTED)
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%POS_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%POS_NODES(NNOD))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%POS2_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%POS2_NODES(NNOD))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%VEL_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES(NNOD))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%VEL2_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%VEL2_NODES(NNOD))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%NODE_IDS))ALLOCATE(STATE_INIMAP_BUF(1)%NODE_IDS(NNOD))
          NNOD=0
          DO I=1,NUMNOD
            IF(NODTAG(I) == 1)THEN
              NNOD=NNOD+1           
              !STATE_INIMAP_BUF(1)%POS_NODES(NNOD) = X(2,I)
              !STATE_INIMAP_BUF(1)%POS2_NODES(NNOD) = X(3,I)
              !STATE_INIMAP_BUF(1)%VEL_NODES(NNOD) = V(2,I)
              !STATE_INIMAP_BUF(1)%VEL2_NODES(NNOD) = V(3,I)  
              WORK(NNOD,1) = X(2,I)
              WORK(NNOD,2) = X(3,I)
              WORK(NNOD,3) = V(2,I)
              WORK(NNOD,4) = V(3,I)           
              STATE_INIMAP_BUF(1)%NODE_IDS(NNOD) = ITAB(I)       
            ENDIF 
          ENDDO   
          STATE_INIMAP_BUF(1)%NUM_POINTS=NNOD
          !---SORTING : UNIQUE OUTPUT NOT DEPENDING ON DOMAIN DECOMPOSITION
           IF(ALLOCATED(IDX))DEALLOCATE(IDX)    
          IF(.NOT.ALLOCATED(IDX))ALLOCATE(IDX(NNOD))
          DO K=1,NNOD ; IDX(K)=K; ENDDO
          CALL QUICKSORT_I2(STATE_INIMAP_BUF(1)%NODE_IDS(:), IDX, 1, NNOD) 
          DO K=1,NNOD
            STATE_INIMAP_BUF(1)%POS_NODES(K) = WORK(IDX(K),1)
            STATE_INIMAP_BUF(1)%POS2_NODES(K) = WORK(IDX(K),2)
            STATE_INIMAP_BUF(1)%VEL_NODES(K) = WORK(IDX(K),3)
            STATE_INIMAP_BUF(1)%VEL2_NODES(K) = WORK(IDX(K),4)           
          ENDDO
          IF(ALLOCATED(WORK))DEALLOCATE(WORK)
          
          
        ELSE
          !LAW 151 (colocated)
          !already doneabove            
        ENDIF
      ENDIF

C-----------------------------------------------
C   S P M D   E x c h a n g e
C-----------------------------------------------
      IF(NSPMD > 1)THEN
        CALL SPMD_STATE_INIMAP_EXCH_SIZ()
        CALL SPMD_STATE_INIMAP2D_EXCH_DATA()
        !
        IF(ISPMD == 0)THEN
          SHIFT_Cy_MIN = STATE_INIMAP_BUF(1)%SHIFT_Cy
          SHIFT_Ny_MIN = STATE_INIMAP_BUF(1)%SHIFT_Ny
          SHIFT_Cz_MIN = STATE_INIMAP_BUF(1)%SHIFT_Cz
          SHIFT_Nz_MIN = STATE_INIMAP_BUF(1)%SHIFT_Nz
          DO I=2,NSPMD
           IF(STATE_INIMAP_BUF(I)%NUM_CENTROIDS==0)CYCLE
            SHIFT_Cy_MIN =MIN(SHIFT_Cy_MIN, STATE_INIMAP_BUF(I)%SHIFT_Cy)
            SHIFT_Ny_MIN =MIN(SHIFT_Ny_MIN, STATE_INIMAP_BUF(I)%SHIFT_Ny)          
            SHIFT_Cz_MIN =MIN(SHIFT_Cz_MIN, STATE_INIMAP_BUF(I)%SHIFT_Cz)
            SHIFT_Nz_MIN =MIN(SHIFT_Nz_MIN, STATE_INIMAP_BUF(I)%SHIFT_Nz)          
          ENDDO
          STATE_INIMAP_BUF(1)%SHIFT_Cy = SHIFT_Cy_MIN
          STATE_INIMAP_BUF(1)%SHIFT_Ny = SHIFT_Ny_MIN
          STATE_INIMAP_BUF(1)%SHIFT_Cz = SHIFT_Cz_MIN
          STATE_INIMAP_BUF(1)%SHIFT_Nz = SHIFT_Nz_MIN
        ENDIF
      ENDIF

C-----------------------------------------------
C   S P M D - G a t h e r i n g   &   S o r t i n g 
C-----------------------------------------------      
      LEN_TOT=STATE_INIMAP_BUF(1)%LENGTH ;
      NCELL_TOT = STATE_INIMAP_BUF(1)%NUM_CENTROIDS ; 
      IF(ISPMD == 0 .AND. NSPMD > 1)THEN
        !--cumulated dimensions
        !      
        NPTS_TOT = 0
        NCELL_TOT = 0
        LEN_TOT = ZERO
        DO I=1,NSPMD
          IF(STATE_INIMAP_BUF(I)%NUM_CENTROIDS==0)THEN
            NPTS(I)=0
            LEN_(I)=ZERO
            NCELL(I)=0
            CYCLE
          ENDIF        
          NPTS(I)= STATE_INIMAP_BUF(I)%NUM_POINTS ; 
          NPTS_TOT=NPTS_TOT+NPTS(I)  
          LEN_(I)=STATE_INIMAP_BUF(I)%LENGTH      ; 
          LEN_TOT=LEN_TOT+LEN_(I) 
          NCELL(I)= STATE_INIMAP_BUF(I)%NUM_CENTROIDS ;  
          NCELL_TOT = NCELL_TOT + NCELL(I)         
        ENDDO
        ALLOCATE(WORK(NPTS_TOT,5))
        !stat_inimap1d_mp.F
        !--gathering velocity into working_array
        !
        J=0
        DO I=1,NSPMD
          DO K=1,NPTS(I)
            J=J+1
            WORK(J,1) = STATE_INIMAP_BUF(I)%POS_NODES(K)  
            WORK(J,2) = STATE_INIMAP_BUF(I)%POS2_NODES(K)                 
            WORK(J,3) = STATE_INIMAP_BUF(I)%VEL_NODES(K)  
            WORK(J,4) = STATE_INIMAP_BUF(I)%VEL2_NODES(K)  
            WORK(J,5) = STATE_INIMAP_BUF(I)%NODE_IDS(K)                 
          ENDDO
        ENDDO
        
        !---SORTING : UNIQUE OUTPUT NOT DEPENDING ON DOMAIN DECOMPOSITION   
        IF(ALLOCATED(IDX))DEALLOCATE(IDX) 
        IF(.NOT.ALLOCATED(IDX))ALLOCATE(IDX(NPTS_TOT))
        DO K=1,NPTS_TOT ; IDX(K)=K; ENDDO
        CALL QUICKSORT(WORK(:,5), IDX, 1, NPTS_TOT) 
        
        
        !---remove duplicates (possible common nodes on adjacent domains)
        ! STAGGERED SCHEME ONLY
        !
        IF(MLW /= 151)THEN
          ALLOCATE(NODTAG_G(NPTS_TOT))
          NODTAG_G(1:NPTS_TOT)=1
          K=0
          DO J=2,NPTS_TOT
            IF(WORK(J,5) == WORK(J-1,5))THEN
              NODTAG_G(J)=0
              K=K+1
            ENDIF
          ENDDO
        ELSE
          K=NPTS_TOT
        ENDIF
        !
        !---store in relevant buffer (reallocate)
        !
        STATE_INIMAP_BUF(1)%NUM_POINTS=K
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%VEL_NODES))DEALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES)
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%VEL2_NODES))DEALLOCATE(STATE_INIMAP_BUF(1)%VEL2_NODES)
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS_NODES))DEALLOCATE(STATE_INIMAP_BUF(1)%POS_NODES)
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS2_NODES))DEALLOCATE(STATE_INIMAP_BUF(1)%POS2_NODES)
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%NODE_IDS))DEALLOCATE(STATE_INIMAP_BUF(1)%NODE_IDS)      
        ALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES(NPTS_TOT), STATE_INIMAP_BUF(1)%VEL2_NODES(NPTS_TOT))  !k : npt_tot without duplicated
        ALLOCATE(STATE_INIMAP_BUF(1)%POS_NODES(NPTS_TOT), STATE_INIMAP_BUF(1)%POS2_NODES(NPTS_TOT))
        ALLOCATE(STATE_INIMAP_BUF(1)%NODE_IDS(NPTS_TOT))
        J=0 
        DO K=1,NPTS_TOT
          IF(MLW /= 151)THEN
            IF(NODTAG_G(K)==0)CYCLE
          ENDIF
          J=J+1
          STATE_INIMAP_BUF(1)%POS_NODES(J)=WORK(IDX(K),1)
          STATE_INIMAP_BUF(1)%POS2_NODES(J)=WORK(IDX(K),2)  
          STATE_INIMAP_BUF(1)%VEL_NODES(J)=WORK(IDX(K),3)
          STATE_INIMAP_BUF(1)%VEL2_NODES(J)=WORK(IDX(K),4) 
          STATE_INIMAP_BUF(1)%NODE_IDS(J)=WORK(K,5)  !already sorted
        ENDDO
        NPTS_TOT = J
        STATE_INIMAP_BUF(1)%NUM_POINTS=NPTS_TOT
        IF(ALLOCATED(WORK))DEALLOCATE(WORK)
        IF(ALLOCATED(NODTAG_G))DEALLOCATE(NODTAG_G)
        
        NBMAT=1
        DO I=1,NSPMD
          NBMAT=MAX(NBMAT,STATE_INIMAP_BUF(I)%NSUBMAT)
        ENDDO
        ALLOCATE(WORK(NCELL_TOT,3+4*NBMAT))
        !
        !--gathering submaterial data into working_array (duplicates are not possible with centroids)
        !
        J=0
        DO I=1,NSPMD
          DO K=1,NCELL(I)
            J=J+1
            WORK(J,1) = STATE_INIMAP_BUF(I)%POS_CENTROIDS(K) 
            WORK(J,2) = STATE_INIMAP_BUF(I)%POS2_CENTROIDS(K)              
            WORK(J,3) = FLOAT(STATE_INIMAP_BUF(I)%CELL_IDS(K))
            NBMAT = STATE_INIMAP_BUF(I)%NSUBMAT
            DO JJ=1,NBMAT
              WORK(J,3+ 4*(JJ-1)+1) = STATE_INIMAP_BUF(I)%SUBMAT(JJ)%VFRAC(K)
              WORK(J,3+ 4*(JJ-1)+2) = STATE_INIMAP_BUF(I)%SUBMAT(JJ)%RHO(K)
              WORK(J,3+ 4*(JJ-1)+3) = STATE_INIMAP_BUF(I)%SUBMAT(JJ)%E(K)
              WORK(J,3+ 4*(JJ-1)+4) = STATE_INIMAP_BUF(I)%SUBMAT(JJ)%PRES(K)
            ENDDO                
          ENDDO
        ENDDO
        !
        !---store in relevant buffer (reallocate)
        !
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS_CENTROIDS))DEALLOCATE(STATE_INIMAP_BUF(1)%POS_CENTROIDS)
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS2_CENTROIDS))DEALLOCATE(STATE_INIMAP_BUF(1)%POS2_CENTROIDS)
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%CELL_IDS))DEALLOCATE(STATE_INIMAP_BUF(1)%CELL_IDS)
        NBMAT = STATE_INIMAP_BUF(1)%NSUBMAT
        DO JJ=1,NBMAT
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%VFRAC))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%VFRAC)
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%RHO  ))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%RHO)
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%E    ))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%E)                    
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%PRES))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%PRES)                    
        ENDDO
        ALLOCATE(STATE_INIMAP_BUF(1)%CELL_IDS(NCELL_TOT))
        ALLOCATE(STATE_INIMAP_BUF(1)%POS_CENTROIDS(NCELL_TOT))
        ALLOCATE(STATE_INIMAP_BUF(1)%POS2_CENTROIDS(NCELL_TOT))
        DO JJ=1,NBMAT
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%VFRAC(NCELL_TOT))
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%RHO(NCELL_TOT))
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%E(NCELL_TOT))
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%PRES(NCELL_TOT))
        ENDDO
        !---SORTING : UNIQUE OUTPUT NOT DEPENDING ON DOMAIN DECOMPOSITION   
        IF(ALLOCATED(IDX))DEALLOCATE(IDX) 
        IF(.NOT.ALLOCATED(IDX))ALLOCATE(IDX(NCELL_TOT))
        DO K=1,NCELL_TOT ; IDX(K)=K; ENDDO
        CALL QUICKSORT(WORK(:,3), IDX, 1, NCELL_TOT) 
        
        DO J=1,NCELL_TOT
          STATE_INIMAP_BUF(1)%POS_CENTROIDS(J)=WORK(IDX(J),1)
          STATE_INIMAP_BUF(1)%POS2_CENTROIDS(J)=WORK(IDX(J),2)
          STATE_INIMAP_BUF(1)%CELL_IDS(J)=INT(WORK(J,3))
          NBMAT = STATE_INIMAP_BUF(1)%NSUBMAT
          DO JJ=1,NBMAT
           STATE_INIMAP_BUF(1)%SUBMAT(JJ)%VFRAC(J)=WORK(IDX(J),3+ 4*(JJ-1)+1)
           STATE_INIMAP_BUF(1)%SUBMAT(JJ)%RHO(J)=WORK(IDX(J),3+ 4*(JJ-1)+2)
           STATE_INIMAP_BUF(1)%SUBMAT(JJ)%E(J)=WORK(IDX(J),3+ 4*(JJ-1)+3)                      
           STATE_INIMAP_BUF(1)%SUBMAT(JJ)%PRES(J)=WORK(IDX(J),3+ 4*(JJ-1)+4)                      
          ENDDO
        ENDDO
        STATE_INIMAP_BUF(1)%NUM_CENTROIDS = NCELL_TOT
        STATE_INIMAP_BUF(1)%LENGTH = LEN_TOT
        
      ENDIF! IF(ISPMD == 0 .AND. NSPMD > 1)THEN
      
      IF(ISPMD == 0)THEN      
        IF(NCELL_TOT == 0 .OR. LEN_TOT == ZERO)THEN
            CALL ANCMSG(MSGID=284,ANMODE=ANINFO,C1="   -- 2D DOMAIN IS NOT DETECTED : CHECK X-PROJECTION") 
            return     
        ELSEIF(LEN_TOT > 0 .AND. NCELL_TOT > 0)THEN
          IF(LX/LEN_TOT > EM06)THEN
            CALL ANCMSG(MSGID=284,ANMODE=ANINFO,C1="   -- 2D DOMAIN IS NOT DETECTED.") 
            return
          ENDIF     
        ENDIF        
      ENDIF
      
C-----------------------------------------------
C   O u t p u t   F i l e
C-----------------------------------------------

      !---OUTPUT FILE HEADER 
      !  
      IF(ISPMD == 0)THEN   
        WRITE(CHSTAT,'(I4.4)')STATE_INIMAP_CALL_NUMBER
        FILNAM=ROOTNAM(1:ROOTLEN)//'_2D_'//CHSTAT//'.inimap'
        OPEN(UNIT=220582,FILE=FILNAM(1:LEN(TRIM(FILNAM))),ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')      
        WRITE(UNIT=220582,FMT='(A,I10)')   '#state file for mappgin with /INIMAP2D, iteration = ',STATE_INIMAP_CALL_NUMBER
        WRITE(UNIT=220582,FMT='(A,A)')     '#  ROOTNAME = ',ROOTNAM(1:ROOTLEN)
        WRITE(UNIT=220582,FMT='(A,I0)')    '#  VERSION  = ',ST_INVERS        
        WRITE(UNIT=220582,FMT='(A,F20.13)')'#  TIME     = ',TT
        WRITE(UNIT=220582,FMT='(A,I10)')   '#  NCYCLE   = ',NCYCLE     
        WRITE(UNIT=220582,FMT='(A,I10)')   '#  NCELL    = ',NCELL_TOT     
        !WRITE(UNIT=220582,FMT='(A)')'#//SUBMODEL/1'
        !WRITE(UNIT=220582,FMT='(A)')'#MAPPING DATA (FUNCTIONS)'
        !WRITE(UNIT=220582,FMT='(A)')"##  off_def   off_nod   off_ele  off_part   off_mat  off_type   off_sub"      
        !WRITE(UNIT=220582,FMT='(A1,7I10)')'#',1000, 0, 0, 0, 0, 0, 0      
      ENDIF
             
      IF(ISPMD == 0)THEN
        !--- OUTPUT FUNCTION FROM CELL DATA BUFFER ---!
        NBMAT = STATE_INIMAP_BUF(1)%NSUBMAT
        SHIFT_Cy = STATE_INIMAP_BUF(1)%SHIFT_Cy
        SHIFT_Ny = STATE_INIMAP_BUF(1)%SHIFT_Ny      
        SHIFT_Cz = STATE_INIMAP_BUF(1)%SHIFT_Cz
        SHIFT_Nz = STATE_INIMAP_BUF(1)%SHIFT_Nz 
        NUM_CENTROIDS = STATE_INIMAP_BUF(1)%NUM_CENTROIDS
        !---volume fractions                                                                        
        IPOS=0 !1:1+21                                                                              
        DO ISUBMAT = 1,NBMAT                                                                        
          WRITE(UNIT=220582,FMT=2001)IPOS+ISUBMAT,ISUBMAT                                           
          DO K=1, NUM_CENTROIDS                                                                     
             WRITE(UNIT=220582,FMT='(3E20.12)') STATE_INIMAP_BUF(1)%POS_CENTROIDS(K) 
     .                                         ,STATE_INIMAP_BUF(1)%POS2_CENTROIDS(K)        
     .                                         ,STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%VFRAC(K)         
          ENDDO                                                                                     
          !WRITE(UNIT=220582,FMT=1500)IPOS+ISUBMAT,IPOS+ISUBMAT,1.00,1.00,-SHIFT_Ny,-SHIFT_Nz   
        ENDDO                                                                                       
        !---mass densities                                                                          
        IPOS=100 !31:31+21                                                                           
        DO ISUBMAT = 1,NBMAT                                                                        
          WRITE(UNIT=220582,FMT=2002)IPOS+ISUBMAT,ISUBMAT                                           
          DO K=1, NUM_CENTROIDS                                                                     
             WRITE(UNIT=220582,FMT='(3E20.12)') STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)   
     .                                         ,STATE_INIMAP_BUF(1)%POS2_CENTROIDS(K)                  
     .                                         ,STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%RHO(K)          
          ENDDO                                                                                     
          !WRITE(UNIT=220582,FMT=1500)IPOS+ISUBMAT,IPOS+ISUBMAT,1.00,1.00,-SHIFT_Ny,-SHIFT_Nz   
        ENDDO                                                                                       
        !---energy density                                                                          
        IPOS=200 !31:31+21                                                                           
        DO ISUBMAT = 1,NBMAT                                                                        
          WRITE(UNIT=220582,FMT=2003)IPOS+ISUBMAT,ISUBMAT                                           
          DO K=1, NUM_CENTROIDS                                                                     
             WRITE(UNIT=220582,FMT='(3E20.12)') STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)  
     .                                         ,STATE_INIMAP_BUF(1)%POS2_CENTROIDS(K)                   
     .                                         ,STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%E(K)            
          ENDDO                                                                                     
          !WRITE(UNIT=220582,FMT=1500)IPOS+ISUBMAT,IPOS+ISUBMAT,1.00,1.00,-SHIFT_Ny,-SHIFT_Nz    
        ENDDO        
        !---pressure fraction  
        IPOS=300 !91:91+21                                                                                 
        DO ISUBMAT = 1,NBMAT                                                                              
          WRITE(UNIT=220582,FMT=2004)IPOS+ISUBMAT,ISUBMAT                                                 
          DO K=1, NUM_CENTROIDS                                                                           
             WRITE(UNIT=220582,FMT='(3E20.12)') STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)                      
     .                                         ,STATE_INIMAP_BUF(1)%POS2_CENTROIDS(K)                     
     .                                         ,STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%PRES(K)              
          ENDDO                                                                                           
          !WRITE(UNIT=220582,FMT=1500)IPOS+ISUBMAT,IPOS+ISUBMAT,1.00,1.00,-SHIFT_Ny,-SHIFT_Nz             
        ENDDO                                                                                             
        !--- OUTPUT VELOCITY FUNCTION ---!
        !      
        WRITE(UNIT=220582,FMT=3000)    
        DO JJ=1,STATE_INIMAP_BUF(1)%NUM_POINTS
          WRITE(UNIT=220582,FMT='(4E20.12,I10)')
     .     STATE_INIMAP_BUF(1)%POS_NODES(JJ) ,STATE_INIMAP_BUF(1)%POS2_NODES(JJ),
     .     STATE_INIMAP_BUF(1)%VEL_NODES(JJ), STATE_INIMAP_BUF(1)%VEL2_NODES(JJ)
        ENDDO  
        !WRITE(UNIT=220582,FMT=1500)400,400,1.00,1.00,-SHIFT_Ny,-SHIFT_Nz   
      ENDIF                     
             
C-----------------------------------------------
C   D e a l l o c a t e   &   C l o s e
C-----------------------------------------------
      IF(ISPMD == 0)THEN
        !---DEALLOCATE 
        IF(ALLOCATED(GET_CELL_FOM_CENTROID))DEALLOCATE(GET_CELL_FOM_CENTROID)
        DO JJ=1,NSPMD
          NBMAT = STATE_INIMAP_BUF(JJ)%NSUBMAT
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT))THEN
            DO I=1,NBMAT
              IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%VFRAC))DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%VFRAC)    
              IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%RHO))  DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%RHO) 
              IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%E))    DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%E)
            ENDDO
          ENDIF
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT       ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT)
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%POS_NODES    ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%POS_NODES)          
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%VEL_NODES    ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%VEL_NODES)
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%POS2_NODES   ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%POS2_NODES)          
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%VEL2_NODES   ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%VEL2_NODES)
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%NODE_IDS     ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%NODE_IDS)
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%POS_CENTROIDS))DEALLOCATE(STATE_INIMAP_BUF(JJ)%POS_CENTROIDS)   
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%POS_CENTROIDS))DEALLOCATE(STATE_INIMAP_BUF(JJ)%POS2_CENTROIDS)   
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%CELL_IDS))DEALLOCATE(STATE_INIMAP_BUF(JJ)%CELL_IDS)   
        ENDDO
        
        !---OUTPUT FILE : FOOTER & CLOSE    
        WRITE(UNIT=220582,FMT=1000)                                                                                             
        !WRITE(UNIT=220582,FMT='(A)')'#//ENDSUB'

 
        IDX1=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21/)    !vfrac
        IDX2=100+IDX1                                                     !rho
        IDX3=300+IDX1                                                     !pressure
        IF(IS_STAT_INIMAP_VP)THEN
          WRITE(UNIT=220582,FMT='(A)') '#/INIMAP2D/VP/1'
        ELSE
          WRITE(UNIT=220582,FMT='(A)') '#/INIMAP2D/VE/1'
        ENDIF
        WRITE(UNIT=220582,FMT='(A)') '#default input to update from /STATE/INIMAP2D'        
        WRITE(UNIT=220582,FMT='(A)') '##    Node1     Node2     Node3' 
        WRITE(UNIT=220582,FMT='(A)') '#         0         0         0'
        WRITE(UNIT=220582,FMT='(A)') '##   Grbric    Grquad    Grtria' 
        WRITE(UNIT=220582,FMT='(A)') '#         0         0         0'
        WRITE(UNIT=220582,FMT='(A)') '##    Fct_v            Fscale_v' 
        WRITE(UNIT=220582,FMT='(A)') '#       400                 1.0'
        DO IMAT=1,MIN(21,NBMAT)
        WRITE(UNIT=220582,FMT='(A)') '##   Fct_vf   Fct_rho          Fscale_rho     Fct_p            Fscale_p' 
        WRITE(UNIT=220582,FMT='(A1,I10,2(I10,F20.0))')'#', IDX1(IMAT),IDX2(IMAT),1.0,IDX3(IMAT),1.0
        ENDDO
        WRITE(UNIT=220582,FMT=1000) 

        WRITE (IOUT,500)  FILNAM(1:LEN(TRIM(FILNAM)))
        WRITE (ISTDO,500) FILNAM(1:LEN(TRIM(FILNAM)))  

        CLOSE(UNIT=220582)
      
      ENDIF  
      
      IF(ALLOCATED(STATE_INIMAP_BUF))DEALLOCATE(STATE_INIMAP_BUF)
      

 
       
      
C-----------------------------------------------
C   O u t p u t   F o r m a t
C-----------------------------------------------

  500 FORMAT (4X,' STATE FILE:',1X,A,' WRITTEN')
 
 1000 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')

 1500 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/MOVE_FUNCT/',I0,/,
     .       'move_function__',I0,/,
     .       '#            ASCALEx             FSCALEy             ASHIFTx             FSHIFTy',/,
     .       (6X,E14.7) )     
 
 2001 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNC_2D/',I0,/,
     .       'volume fraction submaterial_',I0,/,
     .       '         1',/,
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')
 2002 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNC_2D/',I0,/,
     .       'mass density submaterial_',I0,/,
     .       '         1',/, 
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')
 2003 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNC_2D/',I0,/,
     .       'energy density submaterial_',I0,/,
     .       '         1',/,     
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')
 2004 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNC_2D/',I0,/,
     .       'pressure submaterial_',I0,/,
     .       '         1',/,     
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')

 3000 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNC_2D/400',/,
     .       'velocity_function'/,
     .       '         2',/,     
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')    
C-----------------------------------------------
      RETURN            
      END SUBROUTINE STAT_INIMAP2D_SPMD

     
     
