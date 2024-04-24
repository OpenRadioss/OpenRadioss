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
Chd|  STAT_INIMAP1D_SPMD            source/output/sta/stat_inimap1d_spmd.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SPMD_STATE_INIMAP1D_EXCH_DATA source/output/sta/spmd_state_inimap1d_exch_data.F
Chd|        SPMD_STATE_INIMAP_EXCH_SIZ    source/output/sta/spmd_state_inimap_exch_siz.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        STATE_INIMAP_MOD              share/modules/state_inimap_mod.F
Chd|====================================================================
      SUBROUTINE STAT_INIMAP1D_SPMD(
     .                        X        , V        , ITAB  , IPART_STATE, NODTAG,
     .                        IPART    , IPARTS   , IPARTQ, IPARTTG    ,
     .                        IGEO     , IPARG    , IXS   , IXQ        , IXTG  , 
     .                        ELBUF_TAB, MULTI_FVM, BUFMAT, IPM)
C-----------------------------------------------
C   Description
C-----------------------------------------------
C  This subroutine is generating mapping data to be used with /INIMAP1D option.
C  include file is incremented starting from ROOT_1D_0001.inimap
C  It contains 1D fonctions for submaterial data (volume fraction, mass density, energy density)
C  and also function for global velocity
C  User can use the generated file in a second run using #include command in the new Starter input file (target mesh)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER,INTENT(IN) :: ITAB(NUMNOD), IPART(LIPART1,*),IGEO(NPROPGI,NUMGEO),
     .  IXS(NIXS,NUMELS), IPART_STATE(NPART),
     .  IPARG(NPARG,*),IXQ(NIXQ,NUMELQ),IXTG(NIXTG,NUMELTG)
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
      INTEGER NUVAR,NUM_CELL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      my_real P0(3),P0_INF(3),P0_SUP(3),SHIFT_C,SHIFT_N,LENGTH
      my_real MAX_XC,MAX_YC,MAX_ZC,MIN_XC,MIN_YC,MIN_ZC
      my_real LX,LY,LZ
      my_real DX,DY,DZ
      my_real DOTPROD,TOL,XYZ(3),VEL(3),DIST,VECT(3)
      CHARACTER FILNAM*100, CHSTAT*4
      INTEGER XMIN_CELL_ID,YMIN_CELL_ID,ZMIN_CELL_ID,XMAX_CELL_ID,YMAX_CELL_ID,ZMAX_CELL_ID
      INTEGER IS_ITY_1, IS_ITY_2, IS_ITY_7, LAST_CELL,FIRST_CELL,IMAT,NPAR,IADBUF
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IDX     
      INTEGER, POINTER,DIMENSION(:) :: IPART_PTR
      my_real, POINTER,DIMENSION(:) :: UPARAM
      TYPE(BUF_MAT_)  ,POINTER :: MBUF  
      my_real, ALLOCATABLE, DIMENSION(:,:) :: MAP_NODES
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: GET_CELL_FOM_CENTROID        !(NG,I+NFT)
      INTEGER NPTS(NSPMD),NCELL(NSPMD),NPTS_TOT,NCELL_TOT
      my_real, DIMENSION(:,:), ALLOCATABLE :: WORK
      INTEGER, DIMENSION(:), ALLOCATABLE :: WORK_INDX
      my_real :: LEN_(NSPMD),LEN_TOT,SHIFT_C_MIN,SHIFT_N_MIN
      INTEGER :: IDX1(21),IDX2(21),IDX3(21)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      
      !---INITIALIZATION
      !
      STATE_INIMAP_CALL_NUMBER = STATE_INIMAP_CALL_NUMBER +1 !number of written files ROOT_INIMAP_00**.rad
      NUM_CENTROIDS = 0   
      MLW=0 
      !box containing user domain :
      MIN_XC = EP20 
      MIN_YC = EP20         
      MIN_ZC = EP20                         
      MAX_XC = -EP20                            
      MAX_YC = -EP20
      MAX_ZC = -EP20      
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
              IF(IS_ITY_1==1)THEN
                P0(1) = SUM( X(1,IXS(2:9,N)) ) / NPT                                                      
                P0(2) = SUM( X(2,IXS(2:9,N)) ) / NPT                                                      
                P0(3) = SUM( X(3,IXS(2:9,N)) ) / NPT  
              ELSEIF(IS_ITY_2==1)THEN
                P0(1) = SUM( X(1,IXQ(2:5,N)) ) / NPT                                                      
                P0(2) = SUM( X(2,IXQ(2:5,N)) ) / NPT                                                      
                P0(3) = SUM( X(3,IXQ(2:5,N)) ) / NPT  
              ENDIF
              IF(MIN_XC>P0(1))THEN
                MIN_XC=P0(1)
                XMIN_CELL_ID = N                  
              ENDIF
              IF(MIN_YC>P0(2))THEN
                MIN_YC=P0(2)
                YMIN_CELL_ID = N                                                                                            
              ENDIF
              IF(MIN_ZC>P0(3))THEN
                MIN_ZC=P0(3) 
                ZMIN_CELL_ID = N                                                                                            
              ENDIF    
              IF(MAX_XC<P0(1))THEN
                MAX_XC=P0(1)
                XMAX_CELL_ID = N                                                                                            
              ENDIF
              IF(MAX_YC<P0(2))THEN
                MAX_YC=P0(2)
                YMAX_CELL_ID = N                                                                                            
              ENDIF
              IF(MAX_ZC<P0(3))THEN
                MAX_ZC=P0(3)                               
                ZMAX_CELL_ID = N                                                                                            
              ENDIF
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
      IF(.NOT.ALLOCATED(MAP_NODES))ALLOCATE(MAP_NODES(3,NNOD))          
      IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%POS_CENTROIDS))ALLOCATE(STATE_INIMAP_BUF(1)%POS_CENTROIDS(NUM_CENTROIDS)) 
      IF(.NOT.ALLOCATED(GET_CELL_FOM_CENTROID))THEN
        ALLOCATE(GET_CELL_FOM_CENTROID(2,NUM_CENTROIDS))
      ENDIF
  
      LX=ZERO
      LY=ZERO
      LZ=ZERO
      IF(NUM_CENTROIDS > 0)THEN
        !---BOX CONTAINING USER-1D-DOMAIN  &  BUILDING 1D AXIS  
        !    use centroids otherwise with spmd decomposition 1d portion may be smaller than orthogonal directions. (S >> dl)    
        LX=MAX_XC-MIN_XC                                                                                                          
        LY=MAX_YC-MIN_YC                                                                                                          
        LZ=MAX_ZC-MIN_ZC                                                                                                          
        VECT(1:3)=(/LX,LY,LZ/)                                                                                                  
        !---first and last cell along this user 1D domain
        IPOS = MAXLOC(VECT(1:3),1)         
        SELECT CASE(IPOS)
          CASE(1)
            FIRST_CELL = XMIN_CELL_ID
            LAST_CELL  = XMAX_CELL_ID
          CASE(2)
            FIRST_CELL = YMIN_CELL_ID
            LAST_CELL  = YMAX_CELL_ID        
          CASE(3)
            FIRST_CELL = ZMIN_CELL_ID
            LAST_CELL  = ZMAX_CELL_ID
        END SELECT
        !---first and last centroid position
        IF(IS_ITY_1==1)THEN
          P0_INF(1) = SUM( X(1,IXS(2:9,FIRST_CELL)) ) / NPT                                                      
          P0_INF(2) = SUM( X(2,IXS(2:9,FIRST_CELL)) ) / NPT                                                      
          P0_INF(3) = SUM( X(3,IXS(2:9,FIRST_CELL)) ) / NPT  
          P0_SUP(1) = SUM( X(1,IXS(2:9,LAST_CELL)) ) / NPT                                                      
          P0_SUP(2) = SUM( X(2,IXS(2:9,LAST_CELL)) ) / NPT                                                      
          P0_SUP(3) = SUM( X(3,IXS(2:9,LAST_CELL)) ) / NPT  
        ELSEIF(IS_ITY_2==1)THEN
          P0_INF(1) = SUM( X(1,IXQ(2:5,FIRST_CELL)) ) / NPT                                                      
          P0_INF(2) = SUM( X(2,IXQ(2:5,FIRST_CELL)) ) / NPT                                                      
          P0_INF(3) = SUM( X(3,IXQ(2:5,FIRST_CELL)) ) / NPT  
          P0_SUP(1) = SUM( X(1,IXQ(2:5,LAST_CELL)) ) / NPT                                                      
          P0_SUP(2) = SUM( X(2,IXQ(2:5,LAST_CELL)) ) / NPT                                                      
          P0_SUP(3) = SUM( X(3,IXQ(2:5,LAST_CELL)) ) / NPT  
        ENDIF
        !---first and last centroid position are determining 1D axis :
        VECT(1:3)=(/P0_SUP(1)-P0_INF(1),P0_SUP(2)-P0_INF(2),P0_SUP(3)-P0_INF(3)/)    
        LX=VECT(1)
        LY=VECT(2)
        LZ=VECT(3)
        LENGTH = SQRT(VECT(1)*VECT(1) + VECT(2)*VECT(2) + VECT(3)*VECT(3))  !1d axis length (needed to normalize following dot products) 
        SHIFT_C=ZERO 
        IF(LENGTH > ZERO)SHIFT_C = (P0_INF(1)*LX + P0_INF(2)*LY + P0_INF(3)*LZ) / LENGTH  !distance from origin at first centroid position
        STATE_INIMAP_BUF(1)%SHIFT_Cy = SHIFT_C
        STATE_INIMAP_BUF(1)%SHIFT_Cz = ZERO
        STATE_INIMAP_BUF(1)%LENGTH = LENGTH
      ELSE
        STATE_INIMAP_BUF(1)%SHIFT_Cy = ZERO
        STATE_INIMAP_BUF(1)%SHIFT_Cz = ZERO
        STATE_INIMAP_BUF(1)%LENGTH = ZERO      
      ENDIF
      !---ERROR MESSAGES  
      !   
      IF(IS_ITY_7 > 0)THEN                                                                                                    
        CALL ANCMSG(MSGID=284,ANMODE=ANINFO,C1="   -- 1D DOMAIN MUST BE MESHED WITH BRICKS OR QUADS ONLY") 
        return                                                                                                                
      ENDIF           

      !---STARTING POINT OF FIRST NODE IN GENERAL FRAME (SHIFT_N)
      !   SHIFT_N : nodal posisiton (first node along axis)
      !   SHIFT_C : cell centroid position (first centroid along axis)
      !   by nature : SHIFT_N < SHIFT_C
      !
      IF(IS_ITY_1==1)THEN
        DX = X(1,IXS(2,FIRST_CELL))
        DY = X(2,IXS(2,FIRST_CELL))
        DZ = X(3,IXS(2,FIRST_CELL))
        DO JJ=3,NPT
          IF(X(1,IXS(JJ,FIRST_CELL)) < DX)DX=X(1,IXS(JJ,FIRST_CELL))
          IF(X(2,IXS(JJ,FIRST_CELL)) < DY)DY=X(2,IXS(JJ,FIRST_CELL))
          IF(X(3,IXS(JJ,FIRST_CELL)) < DZ)DZ=X(3,IXS(JJ,FIRST_CELL))                    
        ENDDO
      ELSEIF(IS_ITY_2==1)THEN
        DX = X(1,IXQ(2,FIRST_CELL))
        DY = X(2,IXQ(2,FIRST_CELL))
        DZ = X(3,IXQ(2,FIRST_CELL))
        DO JJ=3,NPT
          IF(X(1,IXQ(JJ,FIRST_CELL)) < DX)DX=X(1,IXQ(JJ,FIRST_CELL))
          IF(X(2,IXQ(JJ,FIRST_CELL)) < DX)DY=X(2,IXQ(JJ,FIRST_CELL))
          IF(X(3,IXQ(JJ,FIRST_CELL)) < DX)DZ=X(3,IXQ(JJ,FIRST_CELL))                    
        ENDDO
      ENDIF
      !first point projection on 1d axis
      SHIFT_N = ZERO
      IF(LENGTH > ZERO)SHIFT_N=(DX*LX + DY*LY + DZ*LZ) / LENGTH   
      STATE_INIMAP_BUF(1)%SHIFT_Ny = SHIFT_N
      STATE_INIMAP_BUF(1)%SHIFT_Nz = ZERO
      
      !---ABSCISSA : CENTROIDS POSITION LIST ( STATE_INIMAP_BUF(1)%POS_CENTROIDS(1:NUM_CENTROIDS) )
      !
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
            IF(IS_ITY_1==1)THEN
               P0(1) = SUM( X(1,IXS(2:9,N)) ) / NPT                                                      
               P0(2) = SUM( X(2,IXS(2:9,N)) ) / NPT                                                      
               P0(3) = SUM( X(3,IXS(2:9,N)) ) / NPT 
             ELSEIF(IS_ITY_2==1)THEN
               P0(1) = SUM( X(1,IXQ(2:5,N)) ) / NPT                                                      
               P0(2) = SUM( X(2,IXQ(2:5,N)) ) / NPT                                                      
               P0(3) = SUM( X(3,IXQ(2:5,N)) ) / NPT 
             ENDIF               
             DX = P0(1)-P0_INF(1)                                                     
             DY = P0(2)-P0_INF(2)
             DZ = P0(3)-P0_INF(3)
             !DIST = SQRT(DX*DX+DY*DY+DZ*DZ)
             DOTPROD = ZERO
             IF(LENGTH > ZERO)DOTPROD = (LX*DX + LY*DY + LZ*DZ) / LENGTH
             STATE_INIMAP_BUF(1)%POS_CENTROIDS(K) = DOTPROD + STATE_INIMAP_BUF(1)%SHIFT_Cy                                                  
             GET_CELL_FOM_CENTROID(1,K) = NG                                              
             GET_CELL_FOM_CENTROID(2,K) = I                                                                     
             K=K+1                                                                                     
          END DO                                                                                       
        END IF                                                                                         
      END DO! next NG 
      !---ABSCISSA : CENTROIDS POSITION - SORTING     
      IF(.NOT.ALLOCATED(IDX))ALLOCATE(IDX(NUM_CENTROIDS))
      DO K=1,NUM_CENTROIDS ; IDX(K)=K; ENDDO
      IF(NUM_CENTROIDS>0)CALL QUICKSORT(STATE_INIMAP_BUF(1)%POS_CENTROIDS(:), IDX, 1, NUM_CENTROIDS) 

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
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%VEL_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES(NUM_CENTROIDS))
          DO K=1, NUM_CENTROIDS   
            NG = GET_CELL_FOM_CENTROID(1,IDX(K))
            I = GET_CELL_FOM_CENTROID(2,IDX(K))
            NFT = IPARG(3,NG)                     
            STATE_INIMAP_BUF(1)%POS_NODES(K) = STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)
            XYZ(1:3) = MULTI_FVM%VEL(1:3,I+NFT)
            DOTPROD=ZERO
            IF(LENGTH > ZERO)DOTPROD = (LX*XYZ(1) + LY*XYZ(2) + LZ*XYZ(3)) / LENGTH
            STATE_INIMAP_BUF(1)%VEL_NODES(K) = DOTPROD
          ENDDO
          !submat
          DO ISUBMAT=1,NBMAT
            DO K=1, NUM_CENTROIDS   
              NG = GET_CELL_FOM_CENTROID(1,IDX(K))
              I = GET_CELL_FOM_CENTROID(2,IDX(K))
              NFT = IPARG(3,NG)                     
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%VFRAC(K) = MULTI_FVM%PHASE_ALPHA(ISUBMAT,I+NFT)
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%RHO(K) = MULTI_FVM%PHASE_RHO(ISUBMAT,I+NFT)
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%E(K)  = MULTI_FVM%PHASE_EINT(ISUBMAT,I+NFT)
              STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%PRES(K)  = MULTI_FVM%PRES(I+NFT)                     
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
        ELSE
          DO K=1, NUM_CENTROIDS                                                        
            NG = GET_CELL_FOM_CENTROID(1,IDX(K))
            NEL = IPARG(2,NG)
            I = GET_CELL_FOM_CENTROID(2,IDX(K))                                        
            GBUF => ELBUF_TAB(NG)%GBUF                                                 
            STATE_INIMAP_BUF(1)%SUBMAT(1)%VFRAC(K) = 1.D00                             
            STATE_INIMAP_BUF(1)%SUBMAT(1)%RHO(K) = GBUF%RHO(I)                         
            STATE_INIMAP_BUF(1)%SUBMAT(1)%E(K)  = GBUF%EINT(I) 
            STATE_INIMAP_BUF(1)%SUBMAT(1)%PRES(K) = -THIRD * (GBUF%SIG(I) + GBUF%SIG(I + NEL) + GBUF%SIG(I + 2 * NEL))
          ENDDO                                                                        
        ENDIF
      ENDIF
                                                                                    
      !---VELOCITY TREATMENT FOR STAGGERED SCHEME
      !   law 51 only 
      !
      ! MAP_NODES(1,...) : node_id
      ! MAP_NODES(2,...) : 1d-position
      ! MAP_NODES(3,...) : vel_1d-value            
      !
      IF(NUM_CENTROIDS > 0)THEN
        IF(MLW /= 151)THEN
          K=1      
          DO I=1,NUMNOD
             IF(NODTAG(I) == 1)THEN
               MAP_NODES(1,K)=I
               XYZ(1:3)=X(1:3,I)
               DOTPROD=ZERO
               IF(LENGTH > ZERO)DOTPROD = (LX*XYZ(1) + LY*XYZ(2) + LZ*XYZ(3)) / LENGTH
               MAP_NODES(2,K)=DOTPROD
               K=K+1
             ENDIF
          ENDDO
          IF(ALLOCATED(IDX))DEALLOCATE(IDX)
          ALLOCATE(IDX(NNOD))
          DO K=1,NNOD ; IDX(K)=K; ENDDO
          CALL QUICKSORT(MAP_NODES(2,:), IDX, 1, NNOD) 
          TOL=EM10*LENGTH
          !---check duplicates and remove
          NNOD2=NNOD
          DO I=2,NNOD
            DIST = ABS(MAP_NODES(2,I)-MAP_NODES(2,I-1))
            IF(DIST <= TOL) MAP_NODES(1,IDX(I)) = ZERO
          ENDDO
          K=0
          DO I=1,NNOD
            IF(MAP_NODES(1,IDX(I)) /=  ZERO)THEN
              K=K+1
            ENDIF   
          ENDDO      
          !---VELOCITY : WRITE IN DOMAIN BUFFER (SORTED)
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%POS_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%POS_NODES(K))
          IF(.NOT.ALLOCATED(STATE_INIMAP_BUF(1)%VEL_NODES))ALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES(K))
          K=0
          DO I=1,NNOD
            IF(MAP_NODES(1,IDX(I)) /=  ZERO)THEN
              K=K+1
              STATE_INIMAP_BUF(1)%POS_NODES(K) = MAP_NODES(2,I) !already sorted k<=i nothing is erased
              VEL(1:3)=V(1:3,INT(MAP_NODES(1,IDX(I))))
              DOTPROD=ZERO
              IF(LENGTH > ZERO)DOTPROD = (LX*VEL(1) + LY*VEL(2) + LZ*VEL(3)) / LENGTH
              STATE_INIMAP_BUF(1)%VEL_NODES(K)=DOTPROD    
            ENDIF   
          ENDDO   
          STATE_INIMAP_BUF(1)%NUM_POINTS=K
        ELSE
      
        ENDIF
      ENDIF

C-----------------------------------------------
C   S P M D   E x c h a n g e
C-----------------------------------------------
      IF(NSPMD > 1)THEN
        CALL SPMD_STATE_INIMAP_EXCH_SIZ()
        CALL SPMD_STATE_INIMAP1D_EXCH_DATA()
        !
        IF(ISPMD == 0)THEN
          SHIFT_C_MIN = STATE_INIMAP_BUF(1)%SHIFT_Cy
          SHIFT_N_MIN = STATE_INIMAP_BUF(1)%SHIFT_Ny
          DO I=2,NSPMD
            IF(STATE_INIMAP_BUF(I)%NUM_CENTROIDS==0)CYCLE
            SHIFT_C_MIN =MIN(SHIFT_C_MIN, STATE_INIMAP_BUF(I)%SHIFT_Cy)
            SHIFT_N_MIN =MIN(SHIFT_N_MIN, STATE_INIMAP_BUF(I)%SHIFT_Ny)          
          ENDDO
          STATE_INIMAP_BUF(1)%SHIFT_Cy = SHIFT_C_MIN
          STATE_INIMAP_BUF(1)%SHIFT_Ny = SHIFT_N_MIN
        ENDIF
      ENDIF

C-----------------------------------------------
C   S P M D - G a t h e r i n g   &   S o r t i n g 
C-----------------------------------------------      
      NCELL_TOT = STATE_INIMAP_BUF(1)%NUM_CENTROIDS
      LEN_TOT = STATE_INIMAP_BUF(1)%LENGTH 
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
        ALLOCATE(WORK(NPTS_TOT,3),WORK_INDX(NPTS_TOT))
        !stat_inimap1d_mp.F
        !--gathering velocity into working_array
        !
        J=0
        DO I=1,NSPMD
          DO K=1,NPTS(I)
            J=J+1
            WORK(J,1) = STATE_INIMAP_BUF(I)%POS_NODES(K)  
            WORK(J,3) = STATE_INIMAP_BUF(I)%VEL_NODES(K)   !tmp                  
          ENDDO
        ENDDO
        !
        !sorting velocity
        !
        WORK_INDX(1:NPTS_TOT) = (/(J,J=1,NPTS_TOT)/)
        CALL QUICKSORT(WORK(:,1), WORK_INDX, 1, NPTS_TOT)
        !sort velocity consequently      
        DO I=1,NPTS_TOT
          WORK(I,2)=WORK(WORK_INDX(I),3) !sorted vel
        ENDDO
        TOL=EM10*LEN_TOT
        WORK_INDX(1:NPTS_TOT) = 0
        !
        !---remove duplicates (possible common nodes on adjacent domains)
        ! STAGGERED SCHEME ONLY
        !
        IF(MLW /= 151)THEN
          DO I=2,NPTS_TOT
            DIST = ABS(WORK(I,1)-WORK(I-1,1))
            IF(DIST <= TOL) THEN
               WORK_INDX(I) = 1
            ENDIF
          ENDDO
          K=0
          DO I=1,NPTS_TOT
            IF(WORK_INDX(I) ==0 )THEN
              K=K+1
              WORK(K,1)=WORK(I,1) !abscicca
              WORK(K,2)=WORK(I,2) !ordinates  
            ENDIF   
          ENDDO 
          DO I=K+1,NPTS_TOT ; WORK(I,1:2)=ZERO ; ENDDO
          NPTS_TOT=K
        ENDIF
        !
        !---store in relevant buffer (reallocate)
        !
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%VEL_NODES))DEALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES)
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS_NODES))DEALLOCATE(STATE_INIMAP_BUF(1)%POS_NODES)
        ALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES(NPTS_TOT))
        ALLOCATE(STATE_INIMAP_BUF(1)%POS_NODES(NPTS_TOT)) 
        STATE_INIMAP_BUF(1)%NUM_POINTS=NPTS_TOT
        STATE_INIMAP_BUF(1)%POS_NODES(1:NPTS_TOT)=WORK(1:NPTS_TOT,1)
        STATE_INIMAP_BUF(1)%VEL_NODES(1:NPTS_TOT)=WORK(1:NPTS_TOT,2)  
        IF(ALLOCATED(WORK))DEALLOCATE(WORK)
        IF(ALLOCATED(WORK_INDX))DEALLOCATE(WORK_INDX)
        
        
        NBMAT=1
        DO I=1,NSPMD
          NBMAT=MAX(NBMAT,STATE_INIMAP_BUF(I)%NSUBMAT)
        ENDDO
        ALLOCATE(WORK(NCELL_TOT,1+4*NBMAT))
        ALLOCATE(WORK_INDX(NCELL_TOT))
        !
        !--gathering submaterial data into working_array (duplicates are not possible with centroids)
        !
        J=0
        DO I=1,NSPMD
          DO K=1,NCELL(I)
            J=J+1
            WORK(J,1) = STATE_INIMAP_BUF(I)%POS_CENTROIDS(K) 
            NBMAT = STATE_INIMAP_BUF(I)%NSUBMAT
            DO JJ=1,NBMAT
              WORK(J,1+ 4*(JJ-1)+1) = STATE_INIMAP_BUF(I)%SUBMAT(JJ)%VFRAC(K)
              WORK(J,1+ 4*(JJ-1)+2) = STATE_INIMAP_BUF(I)%SUBMAT(JJ)%RHO(K)
              WORK(J,1+ 4*(JJ-1)+3) = STATE_INIMAP_BUF(I)%SUBMAT(JJ)%E(K)
              WORK(J,1+ 4*(JJ-1)+4) = STATE_INIMAP_BUF(I)%SUBMAT(JJ)%PRES(K)
           ENDDO                
          ENDDO
        ENDDO
        !
        !sorting
        !
        WORK_INDX(1:NCELL_TOT) = (/(J,J=1,NCELL_TOT)/)
        CALL QUICKSORT(WORK(:,1), WORK_INDX, 1, NCELL_TOT)
        !
        !---store in relevant buffer (reallocate)
        !
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS_CENTROIDS))DEALLOCATE(STATE_INIMAP_BUF(1)%POS_CENTROIDS)
        NBMAT = STATE_INIMAP_BUF(1)%NSUBMAT
        DO JJ=1,NBMAT
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%VFRAC))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%VFRAC)
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%RHO))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%RHO)
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%E))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%E)                    
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%PRES))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%PRES)   
        ENDDO
        ALLOCATE(STATE_INIMAP_BUF(1)%POS_CENTROIDS(NCELL_TOT))
        DO JJ=1,NBMAT
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%VFRAC(NCELL_TOT))
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%RHO(NCELL_TOT))
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%E(NCELL_TOT))
          ALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(JJ)%PRES(NCELL_TOT))
        ENDDO
        DO J=1,NCELL_TOT
          STATE_INIMAP_BUF(1)%POS_CENTROIDS(J)=WORK(J,1) !-SHIFT_C_MIN
          DO JJ=1,NBMAT
           STATE_INIMAP_BUF(1)%SUBMAT(JJ)%VFRAC(J)=WORK(WORK_INDX(J),1+ 4*(JJ-1)+1)
           STATE_INIMAP_BUF(1)%SUBMAT(JJ)%RHO(J)  =WORK(WORK_INDX(J),1+ 4*(JJ-1)+2)
           STATE_INIMAP_BUF(1)%SUBMAT(JJ)%E(J)    =WORK(WORK_INDX(J),1+ 4*(JJ-1)+3)                      
           STATE_INIMAP_BUF(1)%SUBMAT(JJ)%PRES(J)=WORK(WORK_INDX(J),1+ 4*(JJ-1)+4)                      
          ENDDO
        ENDDO
        STATE_INIMAP_BUF(1)%NUM_CENTROIDS = NCELL_TOT
        STATE_INIMAP_BUF(1)%LENGTH = LEN_TOT
        
        
      ENDIF! IF(ISPMD == 0 .AND. NSPMD > 1)THEN

      IF(ISPMD == 0)THEN            
        IF(NCELL_TOT == 0 .OR. LEN_TOT == ZERO)THEN
            print *, "** ERROR WITH /STATE/INIMAP"
            print *, "   -- SITUATION NOT EXPECTED" 
            print *, "   -- 1D DOMAIN IS NOT DETECTED."           
            return    
        ENDIF
      ENDIF
      
C-----------------------------------------------
C   O u t p u t   F i l e
C-----------------------------------------------
       
      IF(ISPMD == 0)THEN   
        WRITE(CHSTAT,'(I4.4)')STATE_INIMAP_CALL_NUMBER
        FILNAM=ROOTNAM(1:ROOTLEN)//'_1D_'//CHSTAT//'.inimap'
        OPEN(UNIT=220582,FILE=FILNAM(1:LEN(TRIM(FILNAM))),ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')      
        WRITE(UNIT=220582,FMT='(A,I10)')   '#state file for mappgin with /INIMAP1D, iteration = ',STATE_INIMAP_CALL_NUMBER
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
        SHIFT_C = STATE_INIMAP_BUF(1)%SHIFT_Cy
        SHIFT_N = STATE_INIMAP_BUF(1)%SHIFT_Ny      
        NUM_CENTROIDS = STATE_INIMAP_BUF(1)%NUM_CENTROIDS
        !---volume fractions                                                                        
        IPOS=0 !1:1+21                                                                              
        DO ISUBMAT = 1,NBMAT                                                                        
          WRITE(UNIT=220582,FMT=2001)IPOS+ISUBMAT,ISUBMAT                                           
          DO K=1, NUM_CENTROIDS                                                                     
             WRITE(UNIT=220582,FMT='(2E20.12)') STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)       
     .                                         ,STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%VFRAC(K)        
          ENDDO                                                                                     
          WRITE(UNIT=220582,FMT=1500)IPOS+ISUBMAT,IPOS+ISUBMAT,1.00,1.00,-SHIFT_N,0.00   
        ENDDO                                                                                       
        !---mass densities                                                                          
        IPOS=100 !31:31+21                                                                           
        DO ISUBMAT = 1,NBMAT                                                                        
          WRITE(UNIT=220582,FMT=2002)IPOS+ISUBMAT,ISUBMAT                                           
          DO K=1, NUM_CENTROIDS                                                                     
             WRITE(UNIT=220582,FMT='(2E20.12)') STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)      
     .                                         ,STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%RHO(K)          
          ENDDO                                                                                     
          WRITE(UNIT=220582,FMT=1500)IPOS+ISUBMAT,IPOS+ISUBMAT,1.00,1.00,-SHIFT_N,0.00   
        ENDDO                                                                                       
        !---energy density                                                                          
        IPOS=200 !31:31+21                                                                           
        DO ISUBMAT = 1,NBMAT                                                                        
          WRITE(UNIT=220582,FMT=2003)IPOS+ISUBMAT,ISUBMAT                                           
          DO K=1, NUM_CENTROIDS                                                                     
             WRITE(UNIT=220582,FMT='(2E20.12)') STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)      
     .                                         ,STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%E(K)            
          ENDDO                                                                                     
          WRITE(UNIT=220582,FMT=1500)IPOS+ISUBMAT,IPOS+ISUBMAT,1.00,1.00,-SHIFT_N,0.00   
        ENDDO        
        !---pressure fraction                                                                          
        IPOS=300 !91:91+21                                                                           
        DO ISUBMAT = 1,NBMAT                                                                        
          WRITE(UNIT=220582,FMT=2004)IPOS+ISUBMAT,ISUBMAT                                           
          DO K=1, NUM_CENTROIDS                                                                     
             WRITE(UNIT=220582,FMT='(2E20.12)') STATE_INIMAP_BUF(1)%POS_CENTROIDS(K)      
     .                                         ,STATE_INIMAP_BUF(1)%SUBMAT(ISUBMAT)%PRES(K)            
          ENDDO                                                                                     
          WRITE(UNIT=220582,FMT=1500)IPOS+ISUBMAT,IPOS+ISUBMAT,1.00,1.00,-SHIFT_N,0.00   
        ENDDO                
        !--- OUTPUT VELOCITY FUNCTION ---!
        !      
        WRITE(UNIT=220582,FMT=3000)    
        DO JJ=1,STATE_INIMAP_BUF(1)%NUM_POINTS
          WRITE(UNIT=220582,FMT='(2E20.12)')STATE_INIMAP_BUF(1)%POS_NODES(JJ) , STATE_INIMAP_BUF(1)%VEL_NODES(JJ)     
        ENDDO  
        WRITE(UNIT=220582,FMT=1500)400,400,1.00,1.00,-SHIFT_N,0.00   
      ENDIF                     
             
C-----------------------------------------------
C   D e a l l o c a t e   &   C l o s e
C-----------------------------------------------
      IF(ISPMD == 0)THEN
        !---DEALLOCATE 
        IF(ALLOCATED(MAP_NODES))DEALLOCATE(MAP_NODES)
        IF(ALLOCATED(GET_CELL_FOM_CENTROID))DEALLOCATE(GET_CELL_FOM_CENTROID)
        IF(ALLOCATED(IDX))DEALLOCATE(IDX)
        DO JJ=1,NSPMD
          NBMAT = STATE_INIMAP_BUF(JJ)%NSUBMAT
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT))THEN        
            DO I=1,NBMAT
              IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%VFRAC))DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%VFRAC)    
              IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%RHO))DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%RHO) 
              IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%E))DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%E)
              IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%PRES))DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT(I)%PRES)
            ENDDO
          ENDIF            
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT       ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT)
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%POS_NODES    ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%POS_NODES)          
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%VEL_NODES    ))DEALLOCATE(STATE_INIMAP_BUF(JJ)%VEL_NODES)
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%POS_CENTROIDS))DEALLOCATE(STATE_INIMAP_BUF(JJ)%POS_CENTROIDS)                              
          IF(ALLOCATED(STATE_INIMAP_BUF(JJ)%SUBMAT))DEALLOCATE(STATE_INIMAP_BUF(JJ)%SUBMAT)
        ENDDO

        
        !---OUTPUT FILE : FOOTER & CLOSE    
        WRITE(UNIT=220582,FMT=1000)                                                                                             
        !WRITE(UNIT=220582,FMT='(A)')'#//ENDSUB'

        IDX1=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21/)   !vfrac
        IDX2=100+IDX1                                                     !rho
        IDX3=300+IDX1                                                     !pressure
        IF(IS_STAT_INIMAP_VP)THEN
          WRITE(UNIT=220582,FMT='(A)') '#/INIMAP1D/VP/1'
        ELSE
          WRITE(UNIT=220582,FMT='(A)') '#/INIMAP1D/VE/1'
        ENDIF
        WRITE(UNIT=220582,FMT='(A)') '#default input to update from /STATE/INIMAP1D' 
        WRITE(UNIT=220582,FMT='(A)') '##     Type' 
        WRITE(UNIT=220582,FMT='(A)') '#         1'         
        WRITE(UNIT=220582,FMT='(A)') '##    Node1     Node2' 
        WRITE(UNIT=220582,FMT='(A)') '#         0         0'
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
     .       4(6X,E14.7) )     
 
 2001 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNCT/',I0,/,
     .       'volume fraction submaterial_',I0,/,
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')
 2002 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNCT/',I0,/,
     .       'mass density submaterial_',I0,/,
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')
 2003 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNCT/',I0,/,
     .       'energy density submaterial_',I0,/,
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')
 2004 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNCT/',I0,/,
     .       'pressure submaterial_',I0,/,
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')

 3000 FORMAT('#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|',/,
     .       '/FUNCT/400',/,
     .       'velocity_function'/,
     .       '#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|')    
C-----------------------------------------------
      RETURN            
      END SUBROUTINE STAT_INIMAP1D_SPMD

     
     
