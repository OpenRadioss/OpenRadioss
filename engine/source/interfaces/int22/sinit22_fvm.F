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
Chd|  SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        DESTROY_CELL                  source/interfaces/int22/destroy_cell.F
Chd|        GET_GROUP_ID                  source/interfaces/int22/get_group_id.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SIGEPS37_SINGLE_CELL          source/interfaces/int22/sigeps37_single_cell.F
Chd|        weighting_Cell_Nodes          source/interfaces/int22/weighting_cell_nodes.F
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE SINIT22_FVM (
     .                    IXS    , ELBUF_TAB ,IPARG ,ITAB  ,ITASK   ,
     .                    BUFBRIC, NBRIC_L   ,X     ,ALE_CONNECTIVITY,V       ,
     .                    NV46   , VEUL      ,IGRNOD,IPARI ,IGRTRUSS,
     .                    IXT    , BUFMAT    ,IPM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C

! This subroutine is called for FSI interface 22
! It handles intersected bricks when lagrangian
! surface moved in this new cycle.
!S T E P S
      !------------------------------------------------------------!
      ! @00. OUTPUT CUT CELL BUFFER LIST                           !
      ! @01. CINEATIC TIME STEP                                    !
      ! @02. INIT/PARAMETERS                                       !
      ! @03. INIT/MULTITHREADING                                   !
      ! @04. INIT/ALLOCATION                                       !
      ! @05. RESET TAG22                                           !
      ! @06. COMPUTE UNEXTENDED main CELL VOLUMES                  !
      ! @07. BUILDING BIJECTION APPLICATION for cut cells          !
      ! @08. ADJACENT BRICKS DATA IN CUT CELL BUFFER               !
      ! @09. AREA and VOLUME RATIO UPDATE for cut cells            !
      ! @10. BUILD CELL 9                                          !
      ! @11. BUILD UNCUT CELL IN CUT CELL BUFFER                   !
      ! @12. STORING BRICK REFERENCE VOLUME (UNCUT)                !
      ! @13. POLY 9 : MARK IF NO NODE ON A FACE                    !
      ! @14. FILL MATERIAL BUFFER : %bakMAT%...                    !
      ! @15. FINDING & STORING ADJACENT CELLS                      !
      ! @16. BUILD POLYGONAL TRACE FOR POLY9                       !
      ! @17. PRE-TREATMENT FOR DBLE SECONDARY WITH BOTH V=0        !
      ! @18. TAGGING SECONDARY CUT CELL                            !
      ! @19. LINKING SECONDARY CELL TO A main ONE                  !
      ! @20. LIST ALL SECONDARY CELLS LINKED TO A GIVEN main CELL  !
      ! @21. INDIRECT LINKING                                      !
      ! @22. INDIRECT LINKING                                      !
      ! @23. TAG SECONDARY CELLS WHICH ARE BASED ON FORMER main NODE !
      ! @24. VOLUME MERGING BETWEEN SECONDARY CELL & ITS main ONE  !
      ! @25. MATERIAL MERGING (IF main HAS TO BE DEPORTED)         !
      ! @26. DEMERGING CRITERIA                                    !
      ! @27. MATERIAL DEMERGING                                    !
      ! @28. COMPUTE VOLD_Vnew_Cell for newly cell in buffer       !
      ! @29. COMPUTE VOLUME CHANGE FOR EACH CELL                   !
      ! @30. LINK SWITCHING                                        !
      ! @31. LEVEL2 NEIGHBORHOOD (FOR FLUXES & CONVECTION)         !
      ! @32. COUNT POINTS ON A GIVEN POLYHEDRON FACE               !
      ! @33. LOCATE WHERE WAS main FOR NEXT CYCLE                  !
      ! @34. SET IIAD22(1:NUMELS) = %ELBUG_TAB(NG)%TAG22(I)        !
      ! @35. SET MLW                                               !
      ! @36. RESET OLD SECONDARY LIST CONNECTIVITY                 !
      ! @37. SET main STRONG NODE                                  !
      ! @38. DRAW SUPERCELL CONNEXION WITH 1D TRUSSES              !
      ! @39. COMPUTE DVOL PREDICTION FOR EACH CELL                 !
      ! @40. BUILD SUPERCELL DVOL (PREDICTION)                     !
      ! @41. FIX SUPERCELL DVOL (CORRECTION)                       !
      ! @42. TIME ZERO VOLUME INIT (NOT DONE IN STARTER)           !
      ! @43. DEBUG OUTPUT                                          !
      ! @44. DEBUG - WRITE CUT CELL BUFFER                         !
      ! @45. SUPERCELL CENTERS                                     !
      ! @46. MARK SUPER-CELL CENTERS WITH ORPHAN NODES             !
      ! @47. UNCUT CELLS + POLY 9 : CENTERS                        !
      ! @48. MARK CELL CENTERS WITH ORPHAN NODES                   !
      ! @49. MARK FACE CENTERS WITH ORPHAN NODES                   !
      ! @50. DEALLOCATE                                            !
      !------------------------------------------------------------!

!
! BUFFERS :
!         BRICK_LIST     : BUT CELL BUFFER(NIN,IB) : NIN interface id, IB:cut cell id
C-----------------------------------------------
C   N o t e s  F o r  L a t e r
C-----------------------------------------------
C -1- Reset tag22 only for brick in previous buffer (check performances)
C -2- remove pointers
C -3- mettre les allocatables dans un module
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INITBUF_MOD
      USE I22BUFBRIC_MOD 
      USE I22TRI_MOD
      USE ELBUFDEF_MOD
      USE ALEFVM_MOD
      USE GROUPDEF_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"    
#include      "inter22.inc"
#include      "mvsiz_p.inc"
#include      "comlock.inc"
#include      "subvolumes.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)                             :: IXS(NIXS,*)   ,IPARG(NPARG,*),ITAB(*)   ,NV46  ,BUFBRIC(*),
     .                                                  IPARI(*)      ,IXT(NIXT,*)  ,ITASK ,IPM(NPROPMI,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      my_real,INTENT(IN)                             :: V(3,*), VEUL(LVEUL,*)
      my_real,INTENT(IN),TARGET                      :: BUFMAT(*)
      my_real,INTENT(INOUT)                          :: X(3,*)
!
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(L_BUFEL_)  ,POINTER            :: LBUF1,LBUF2  
      INTEGER                             :: I,J,K0,K1,K2,JV,IDV, NBCUT, NBCUTv, NBCUTprev, NEL,NG,NFL,NBF,NBL,NBL1,NBRIC_L,NIN
      INTEGER                             :: brickID,tNB,NTAG,IV,NGv,IAD22,NCELLv,ICV,IGR
      INTEGER                             :: IB,IBV,IBv2,IBv_i,IBo,ICELL,ICELL2,MCELL,NCELL,MNOD,ID,ITAG(66)
      INTEGER                             :: IPOS, LLT_, LLT_o,LLT_v, IDLOCv, IPOSf, IPOSiadj,ICELLv,ICELLv2
      INTEGER                             :: INODES(8),INOD,INOD2,INODE,NNODES, NNODES2, ADD, ADD0, ITRIMAT, K, KV, Ko
      LOGICAL                             :: lDONE,lStillTruss,lFOUND,lCYCLE,lTARGET,lStillNode, lCOND1, lCOND2
      my_real                             :: VolCELL, VolCELL51(TRIMAT), VolSECND51(TRIMAT),VolSECND,VolCELL51_OLD(TRIMAT)
      my_real                             :: adjMAIN_vol(6), adjMAIN_face(6), adjMAIN_centroid(3,6),N(3,6), FAC
      INTEGER                             :: IDadj_MCELLv(6), IVadj_MCELLv(6), IBadj_MCELLv(6)
      my_real,DIMENSION(:,:),ALLOCATABLE  :: F
      my_real, POINTER                    :: pVAR, pVARv, pVARo
      my_real, DIMENSION(:), POINTER      :: pVAR3      
      TYPE(G_BUFEL_)  ,POINTER            :: GBUF, GBUFv, GBUFo   
      TYPE(L_BUFEL_)  ,POINTER            :: LBUF 
      TYPE(POLY_ENTITY),DIMENSION(:),    POINTER    :: pIsMain  ,pIsMainV
      my_real,DIMENSION(:)  ,  POINTER    :: pFullFACE
      TYPE(NODE_ENTITY),DIMENSION(:)  ,  POINTER    :: pNodWasMain
      !INTEGER,DIMENSION(:,:),  POINTER    :: pNAdjCELL      
      INTEGER,DIMENSION(:,:),  POINTER    :: pAdjBRICK, pAdjBRICKv
      TYPE(NODE_ENTITY),DIMENSION(:)  ,  POINTER    :: pWhereWasMain              
      TYPE(NODE_ENTITY), DIMENSION(:), POINTER :: pWhichCellNod,pWhichCellNodv
      INTEGER               ,  POINTER    :: pMainID      
      my_real,DIMENSION(:)  ,  POINTER    :: UPARAM !=> BRICK_ENTITY%POLY()%Vnew 
      TYPE(POLY_ENTITY), DIMENSION(:), POINTER :: pSUBVOLv
      my_real               ,  POINTER    :: pSUBVOLD
      TYPE(POLY_ENTITY), DIMENSION(:), POINTER :: pSUBVOL
      CHARACTER*6                         :: char1     
      LOGICAL                             :: bool1, bool2
      CHARACTER*10                        :: debugMAINSECND
      CHARACTER*10 ,ALLOCATABLE           :: debugMAINSECNDv(:,:,:)
      INTEGER,ALLOCATABLE                 :: IsMainV(:,:,:)      
      TYPE(BUF_MAT_)  ,POINTER            :: MBUF,MBUFv, MBUFo 
      INTEGER                             :: ICRIT_MAT_DEPORTATION, ICRIT_DEMERGE
!      INTEGER,ALLOCATABLE,DIMENSION(:,:,:):: TARGET_DATA !index1:facet - index2:adjacent_cell_number - index3:ib(cut_cell_id)
      INTEGER,ALLOCATABLE,DIMENSION(:,:,:):: ORIGIN_DATA !index1:facet - index2:adjacent_cell_number - index3:ib(cut_cell_id),J           
      INTEGER,ALLOCATABLE,DIMENSION(:)    :: Ntarget,Norigin
      INTEGER                             :: MTN_,ITAR, IORIG, MAIN_TAG(6,9) !index1:Face !index2:icv=cell number
      INTEGER                             :: IPLA, ISOLNOD, FM, IBm,IBmCur,IBMo,IBMold, IDM, IE, IN, MT
      INTEGER                             :: ICELLTAG(9), ICELLTAG_OLD(9),IC, NAdjCELL,NAdjCELLv, IFV, FV, FV2
      INTEGER                             :: IVv,MCELLv, MCELLvv, NGvv,IDLOCvv, IBvv,IFVv
      INTEGER                             :: GET_UNIQUE_MAIN_CELL, LINK_WITH_UNIQUE_MAIN_CELL,IADJ,NADJ,NADJv,IE_M
      INTEGER                             :: NsecndNOD, NP, NP_NODE, JJ, NINTP(6,9), NNOD(6,9), NN, SECid, IDLOC, NewMnod(8), MLW
      INTEGER                             :: ITASKv, NTRUS, NPOINT, IRES(2), NewInBuffer
      INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: DESTROY_DATA
      INTEGER                             :: I22LOOP,IT,IUNLINK,ITASKB, J1,J2, NTAR, INod_W, IDEMERGE, INod_W_old, ISECND, IAD0,II
      INTEGER                             :: FV_old, NumSECND, NumSECND2, IC1, IC2, ITARGET, NumTarget, WasCut, LID, IFAC, IEDG
      INTEGER                             :: Cod1, Cod2, Cod3, Icompl, Poly9woNodes, Poly9woNodesV, ISGN(2), ICODE, OLD_ICODE
      INTEGER                             :: mainID,NC(8),MOLD,MNEW, IB2,NUM,IADBUF,NUPARAM,NGm,IDLOCm,ICELLm, NP_(9),NODE_ID
      LOGICAL                             :: debug_outp,debug_outp2, lSKIP

      my_real,ALLOCATABLE, DIMENSION(:,:) :: VOL51, VOL51v  !index1:IB  index2:ITRIMAT
      my_real,ALLOCATABLE, DIMENSION(:)   :: UVAR,UVAR_ADV   !(N0PHAS+TRIMAT*NVPHAS+I22LAW37) !NUVAR=N0PHAS+TRIMAT*NVPHAS
      my_real                             :: Vfrac(4),SOM, SOM_(4), SOMi, AdjFACE(NV46,NADj_F), DELTA, pPOINT(3)
      my_real                             :: sumFACE, VECtmp(3), VOLorig(24), PointTMP(3), Point0(3), CUT_Point(3)
      my_real                             :: Cut_Vel(3)
      my_real                             :: EINT, EINTv, RHO,RHO_U(3),MOM(3),RHOv,SIGv(6), SIG(6),VOLv,VOL,VOL_M,VOL_S
      my_real                             :: VAR, VAR_,VAR__, VAR_6(6), VAR_VF(4), Vold_phase(4)
      my_real                             :: TMP(6),DXMIN,VMAX,RatioF,RatioFv,RATIO,RATIOv,UncutVOL,UncutVOLv
      my_real                             :: Vuncut, Vi,Vj, M(9,9) !index1:polyhedra in current conformation, index2:polyhedra in previous confromation
      my_real                             :: VSUM(3) , N_(3), VNEW, VOLD, DVOL_NUMERIC, DVOL_PREDIC
      my_real                             :: SGN, DVi, DVii, FACE, FACE9, M_TOT, M_LIQ, M_TOTo,M_LIQo, RHO10, RHO20, MFRAC
      my_real                             :: Center(3,8)
      
      my_real                             :: DET, DF11, DF12, DF21, DF22, F1, F2, P1, P2, DRHO1, DRHO2, ERROR, TOL, R1, PSH, PMIN
      my_real                             :: C1, GAM, P0, P,  RHO1, RHO2, MAS, MAS1, MAS2, SSP, SSP1, SSP2, RHOC2_1, RHOC2_2, RHOC2
      my_real                             :: ALP, ALPo, BETA, VOLCELLOLD, VEL_M(3), SURF_S, NORM_S(3), ADV, NORM, VM, VS
      my_real                             :: RHO_ADV, EINT_ADV, SIG_ADV(6), MOM_ADV(3), ZM(3),ZS(3), VOLratio
      INTEGER                             :: ITER, NITER, LLTo, LLTm, NGo, IADV
      INTEGER, ALLOCATABLE, DIMENSION(:)  :: ORDER, VALUE

      INTEGER                             :: NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8
      INTEGER :: IAD2, IAD3, LGTH2, LGTH3
     
      INTERFACE
        FUNCTION I22AERA(NPTS, P, C)      
        my_real                      :: I22AERA(3)
        INTEGER, INTENT(IN)          :: NPTS
        my_real, INTENT(IN)          :: P(3,NPTS)     
        my_real, INTENT(INOUT)       :: C(3)  
        END FUNCTION
      END INTERFACE
      
      TYPE POINTER_ARRAY_R
        my_real,DIMENSION(:),  POINTER      :: p
      END TYPE

      TYPE POINTER_ARRAY_I
        INTEGER,DIMENSION(:),  POINTER      :: p
      END TYPE
            
      TYPE(POLY_ENTITY), DIMENSION(:), POINTER      :: pFACE     !   WARNING :  all pointers begin with 1 !!!  index 0 impossible  pFACE(1:7,:)=>%Face_Cell(0:6,:) <
      TYPE(POINTER_ARRAY_R)      :: pFACEv(9)          
      TYPE(POINTER_ARRAY_I)      :: pListNodID(9) 
      TYPE(POINTER_ARRAY_I)      :: pListNodIDv(9)
      !TYPE(POINTER_ARRAY_I)      :: pAdjCELL

      INTEGER ICF(4,6)
      DATA ICF/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
      
      my_real ::      
     .   AJ7(MVSIZ), AJ8(MVSIZ) , AJ9(MVSIZ),
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   AJ12, AJ45, AJ78,
     .   A17 , A28 ,
     .   B17 , B28 ,
     .   C17 , C28 ,
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),X7(MVSIZ),X8(MVSIZ),
     .   Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),
     .   Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .   DETT(MVSIZ),
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   AJ4(MVSIZ),AJ5(MVSIZ),AJ6(MVSIZ),HXP,HYP,HZP,
     .   X1_,X2_,X3_,X4_,X5_,X6_,X7_,X8_,
     .   Y1_,Y2_,Y3_,Y4_,Y5_,Y6_,Y7_,Y8_,
     .   Z1_,Z2_,Z3_,Z4_,Z5_,Z6_,Z7_,Z8_
      
C-----------------------------------------------
C   P r e c o n d i t i o n s
C----------------------------------------------- 
      !IF(INT22 == 0) RETURN     !already checked if this subroutine is called
C-----------------------------------------------
C   S o u r c e    L i n e s
C-----------------------------------------------
      !------------------------------------------------------------!
      ! @00. OUTPUT CUT CELL BUFFER LIST                           !
      !------------------------------------------------------------!
      IF(IBUG22_ccBufList /= 0)THEN
        NIN = 1
        IF(ITASK==0)THEN
          write (*,FMT='(A, 1000I7)')"CUT CELL BUFFER :   ", ixs(11,brick_list(nin,1:NB)%id)
        ENDIF
      ENDIF
      
      !------------------------------------------------------------!
      ! @01. CINEATIC TIME STEP                                    !
      !------------------------------------------------------------!
      IDT_INT22     = 1 !kinematic time step by default. Will be replaced by 0 if exist i such as DTIX(I) < DT22_MIN (mqviscb.F)
      DXMIN         = MINVAL (DX22MIN_L(0:NTHREAD-1))
      VMAX          = MAXVAL (V22MAX_L(0:NTHREAD-1)) 
      IF(VMAX == ZERO)THEN       
        dt22_min = EP30
      ELSE
        dt22_min = DXMIN/nCross22 / VMAX
      ENDIF
      if(ibug22_dtmin==1)print *, "dt22_min = ", dt22_min
      !!sIF(DT1==ZERO .AND. ITASK==0)  print *, "INTER22, Tolerance ratio ",RATIO22

      !------------------------------------------------------------!
      ! @02. INIT/PARAMETERS                                       !
      !------------------------------------------------------------!
      IADV           = 0 ! 0: advection step off     1: advection step on     !When demerging a new main, advection is done from old main cell (possibly relocated due to a merging process) to new one.    
      NIN            = 1
      dbVOL          = ZERO
      dbMASS         = ZERO 
      I22LOOP        = 0    !infinite loop detected
      IMERGEL(ITASK) = 0
        IF(TRIMAT<=0)THEN
          ALLOCATE(UVAR (I22LAW37))
          ALLOCATE(UVAR_ADV (I22LAW37))
        ELSE
          ALLOCATE(UVAR (N0PHAS+TRIMAT*NVPHAS))
          ALLOCATE(UVAR_ADV (N0PHAS+TRIMAT*NVPHAS))
        ENDIF

      !------------------------------------------------------------!
      ! @03. INIT/MULTITHREADING                                   !
      !------------------------------------------------------------!
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
      NBL = MIN(NBL,NB)
      tNB = NBL-NBF+1
      I22_DEGENERATED = 0
      
      !!!!!!!!!!!!!!!!!!!!!!DEBUG OUTPUT!!!!!!!!!!!!!!!!!!!!!!!
      debug_outp = .false.
      debug_outp2= .false.      
      
      do ib=nbf,nbl
        ie = brick_list(nin,ib)%id
        if (ie == ibug22_sinit .or. ibug22_sinit == -1)then
          debug_outp = .true.
          exit
        endif
      enddo
      
      if(itask==0.AND.debug_outp)then
        print *, ""
        print *, "  |----------i22sinit.F-----------|"
        print *, "  |  INITIALIZATION SUBROUTINE    |"
        print *, "  |-------------------------------|"
        Char1 = '      '
      end if
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      !------------------------------------------------------------!
      ! @04. INIT/ALLOCATION                                       !
      !------------------------------------------------------------!      
      ALLOCATE (debugMAINSECNDv(NBL-NBF+1,6,9))     
!      ALLOCATE (TARGET_DATA(tNB,9,1:2))          !index1:IB, index2:1:Ntarget<=9, index3:1=localface_2=adjacent_cell
      ALLOCATE (ORIGIN_DATA(NBF:NBL,9,1:3))       !index1:IB, index2:1:Ntarget<=9, index3:1=localface_2=adjacent_cell            
      ALLOCATE (IsMainV(NBF:NBL,6,9))       
      ALLOCATE (F(6,NBF:NBL))
      ALLOCATE (VOL51(NBF:NBL,TRIMAT),VOL51v(NBF:NBL,TRIMAT))
c      ALLOCATE (Ntarget(NBF:NBL))       
      ALLOCATE (Norigin(NBF:NBL))       
      !-------------------!
        CALL MY_BARRIER()
      !-------------------!          
      !------------------------------------------------------------!
      ! @05. RESET TAG22                                           !
      !------------------------------------------------------------!
      !TAG22(I) is adress in cut cell buffer "BRICK_LIST(NIN,TAG22(I))"
      !for interface number NIN, and element I from current group.
      !TAG22==0 means elem I is not in cut cell buffer.
      DO NG=ITASK+1,NGROUP,NTHREAD                              
         IF(IPARG(8,NG) /= 1) THEN                              
          CALL INITBUF (
     1       IPARG    ,NG      ,                     
     2       MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,      
     3       NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,      
     4       JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,      
     5       NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,      
     6       IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,      
     7       ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )      
            IF(JLAG /= 1 .AND. ITY<=2) THEN                                       
             IF (MTN /= 0  .AND.  IPARG(64,NG)==0) THEN         
              LFT     = 1                                             
              LLT     = NEL                                      
              ISOLNOD = IPARG(28,NG)     
              IF (ITY == 1 .AND. ISOLNOD /= 4) THEN             
                GBUF => ELBUF_TAB(NG)%GBUF                      
                GBUF%TAG22(LFT:LLT) = 0                         
              ENDIF                                             
             ENDIF                                              
            ENDIF                                                
         ENDIF                                                  
       ENDDO    
      !------------------------------------------------------------!
      ! @06. COMPUTE UNEXTENDED main CELL VOLUMES                !
      !------------------------------------------------------------!
      DO NG=ITASK+1,NGROUP,NTHREAD                              
         IF(IPARG(8,NG) /= 1) THEN                              
          CALL INITBUF (
     1       IPARG    ,NG      ,                     
     2       MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,      
     3       NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,      
     4       JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,      
     5       NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,      
     6       IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,      
     7       ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )      
            IF(JLAG /= 1 .AND. ITY<=2) THEN                                       
             IF (MTN /= 0  .AND.  IPARG(64,NG)==0) THEN         
              LFT     = 1                                             
              LLT     = NEL          
              ISOLNOD = IPARG(28,NG)                                                    
              IF (ITY == 1 .AND. ISOLNOD /= 4) THEN             
                GBUF => ELBUF_TAB(NG)%GBUF                      
                IF(JEUL/=0)THEN
                  IF(INTEG8==0)THEN
                    GBUF%VOL(LFT:LLT) = VEUL(32,NFT+LFT:NFT+LLT)                   
                  ELSEIF(INTEG8==1)THEN
                    GBUF%VOL(LFT:LLT) = VEUL(52,NFT+LFT:NFT+LLT)             
                  ENDIF!INTEG8
                ELSE
                  DO I=LFT,LLT
                    II    = I+NFT
                    X1(I) = X(1,IXS(2,II)) ; Y1(I) = X(2,IXS(2,II)) ; Z1(I) = X(3,IXS(2,II)) ;
                    X2(I) = X(1,IXS(3,II)) ; Y2(I) = X(2,IXS(3,II)) ; Z2(I) = X(3,IXS(3,II)) ;
                    X3(I) = X(1,IXS(4,II)) ; Y3(I) = X(2,IXS(4,II)) ; Z3(I) = X(3,IXS(4,II)) ;
                    X4(I) = X(1,IXS(5,II)) ; Y4(I) = X(2,IXS(5,II)) ; Z4(I) = X(3,IXS(5,II)) ;
                    X5(I) = X(1,IXS(6,II)) ; Y5(I) = X(2,IXS(6,II)) ; Z5(I) = X(3,IXS(6,II)) ;
                    X6(I) = X(1,IXS(7,II)) ; Y6(I) = X(2,IXS(7,II)) ; Z6(I) = X(3,IXS(7,II)) ;
                    X7(I) = X(1,IXS(8,II)) ; Y7(I) = X(2,IXS(8,II)) ; Z7(I) = X(3,IXS(8,II)) ;
                    X8(I) = X(1,IXS(9,II)) ; Y8(I) = X(2,IXS(9,II)) ; Z8(I) = X(3,IXS(9,II)) ;
                  ENDDO
                  DO I=LFT,LLT
                    X17=X7(I)-X1(I)
                    X28=X8(I)-X2(I)
                    X35=X5(I)-X3(I)
                    X46=X6(I)-X4(I)
                    Y17=Y7(I)-Y1(I)
                    Y28=Y8(I)-Y2(I)
                    Y35=Y5(I)-Y3(I)
                    Y46=Y6(I)-Y4(I)
                    Z17=Z7(I)-Z1(I)
                    Z28=Z8(I)-Z2(I)
                    Z35=Z5(I)-Z3(I)
                    Z46=Z6(I)-Z4(I)            
                    AJ1(I)=X17+X28-X35-X46
                    AJ2(I)=Y17+Y28-Y35-Y46
                    AJ3(I)=Z17+Z28-Z35-Z46
                    A17=X17+X46
                    A28=X28+X35
                    B17=Y17+Y46
                    B28=Y28+Y35
                    C17=Z17+Z46
                    C28=Z28+Z35
                    AJ4(I)=A17+A28
                    AJ5(I)=B17+B28
                    AJ6(I)=C17+C28
                    AJ7(I)=A17-A28
                    AJ8(I)=B17-B28
                    AJ9(I)=C17-C28
                  ENDDO
                  DO I=LFT,LLT      
                    JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
                    JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
                    JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
                  ENDDO
                  DO I=LFT,LLT    
                    DETT(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
                  ENDDO
                  GBUF%VOL(LFT:LLT) = DETT(LFT:LLT)
                ENDIF!JEUL/=0
              ENDIF                                             
             ENDIF                                              
            ENDIF                                                
         ENDIF                                                  
       ENDDO                                                          
      !------------------------------------------------------------!
      ! @07. BUILDING BIJECTION APPLICATION for cut cells          !
      !    (local array) <-->  (global buffer)                     !
      !     NBRIC        <-->   (I,NG)                             !
      !------------------------------------------------------------!
      !-------------------!
        CALL MY_BARRIER()     !need %TAG22 to be init to 0. (initialisation is multithreading segmenting 1:NG, not 1:NB
      !-------------------!
      DO IB=NBF,NBL !1,NB
        IDB(IB)                  = BRICK_LIST(NIN,IB)%ID
        BRICK_LIST(NIN,IB)%ITASK = ITASK
      ENDDO        
      DO IB=NBF,NBL !1,NB
        !recuperer NG,I
        !From IB get I and NG
        lDONE = .FALSE.
        DO NG=1,NGROUP
          NEL=IPARG(2,NG)
          NFL=IPARG(3,NG)
          IF((IDB(IB)>NFL).AND.(IDB(IB)<=NFL+NEL))THEN
            IF(IPARG(11,NG)==1) JEUL = 1
            IF(IPARG(7,NG)==1)  JALE = 1
            IDLOCB(IB) = IDB(IB) - NFL
            NGB(IB)    = NG 
            BRICK_LIST(NIN,IB)%NG    = NGB(IB)
            BRICK_LIST(NIN,IB)%IDLOC = IDLOCB(IB)            
            NELB(IB)   = NEL
            lDONE      =.TRUE. 
            GBUF       => ELBUF_TAB(NGB(IB))%GBUF
            NBCUT      =  BRICK_LIST(NIN,IB)%NBCUT
            GBUF%TAG22(IDLOCB(IB)) = IB           ! les tags ont deja ete remis a zero       
            EXIT    !next I
          ELSE
            CYCLE   !next NG  
          ENDIF
          if (.NOT.(lDONE))then
                   write( *,*) "int 22 : error in group sorting"
            stop 2201
          end if
        ENDDO !next NG
      ENDDO !next i
      !-------------------!
        CALL MY_BARRIER()
      !-------------------!
      !------------------------------------------------------------!
      ! @08. ADJACENT BRICKS DATA IN CUT CELL BUFFER               !
      !------------------------------------------------------------!      
      DO IB=NBF,NBL
        brickID = IDB(IB)
        IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(brickID)
        LGTH2 = ALE_CONNECTIVITY%ee_connect%iad_connect(brickID+1) - 
     .       ALE_CONNECTIVITY%ee_connect%iad_connect(brickID)
        DO J=1,LGTH2
           IDV = ALE_CONNECTIVITY%ee_connect%connected(IAD2 + J - 1)
          BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1) = IDV  !IV
          BRICK_LIST(NIN,IB)%Adjacent_Brick(J,2) = 0    !NGV
          BRICK_LIST(NIN,IB)%Adjacent_Brick(J,3) = 0    !IDLOCV            
          BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4) = 0    !IBV         
          BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5) = 0    !IFV     
          IF(IDV>0)THEN
             IAD3 = ALE_CONNECTIVITY%ee_connect%iad_connect(IDV)
             LGTH3 = ALE_CONNECTIVITY%ee_connect%iad_connect(IDV+1) - 
     .       ALE_CONNECTIVITY%ee_connect%iad_connect(IDV)
            DO JV=1,LGTH3
              IF(ALE_CONNECTIVITY%ee_connect%connected(IAD3 + JV - 1)==brickID)THEN
                BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5) = JV
                EXIT          
              ENDIF
            ENDDO!next JV
            CALL GET_GROUP_ID(IDV,NGv,IDLOCv,IPARG)
            GBUFv => ELBUF_TAB(NGv)%GBUF
            IAD22 = GBUFv%TAG22(IDLOCv)
            BRICK_LIST(NIN,IB)%Adjacent_Brick(J,2) = NGv
            BRICK_LIST(NIN,IB)%Adjacent_Brick(J,3) = IDLOCv
            BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4) = IAD22
            IF (IAD22==0 .AND. BRICK_LIST(NIN,IB)%NBCUT>0)THEN
              print *, "**error : inter22 : Lagrangian Surface seems to"
              print *, "          reach eulerian boundary domain.     "
              print *, "          Check Surface location and GRBRICK definition"
              print *, "          near related Brick_ID =", IXS(11,brick_list(nin,ib)%id)
              !print *, "ITASK=", ITASK
              
              stop 2202 !means that a real cut cell has an adjacent brick which is not in cut cell buffer : NOT EXPECTED unless if /INTER/TYPE22 GrBrick is not correctly defined.
            ENDIF 
          ELSEIF(IDV==0)THEN
            BRICK_LIST(NIN,IB)%Adjacent_Brick(J,2) = 0
            BRICK_LIST(NIN,IB)%Adjacent_Brick(J,3) = 0
            BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4) = 0                  
          ELSEIF(IDV<0)THEN
            write (*,*) "unavailable in SPMD"
            stop 2203
          ENDIF
        ENDDO
      ENDDO
      
      !------------------------------------------------------------!
      ! @09. AREA and VOLUME RATIO UPDATE for cut cells            !
      !------------------------------------------------------------!
      DO IB=NBF,NBL 
        brickID = IDB(IB)  
        IF(I22_ALEUL==2)THEN          
          N(1:3,1) = (/ VEUL(14,brickID) , VEUL(20,brickID) , VEUL(26,brickID) /)
          N(1:3,2) = (/ VEUL(15,brickID) , VEUL(21,brickID) , VEUL(27,brickID) /)
          N(1:3,3) = (/ VEUL(16,brickID) , VEUL(22,brickID) , VEUL(28,brickID) /)
          N(1:3,4) = (/ VEUL(17,brickID) , VEUL(23,brickID) , VEUL(29,brickID) /)
          N(1:3,5) = (/ VEUL(18,brickID) , VEUL(24,brickID) , VEUL(30,brickID) /)
          N(1:3,6) = (/ VEUL(19,brickID) , VEUL(25,brickID) , VEUL(31,brickID) /)
          BRICK_LIST(NIN,IB)%N(1,1:3) = N(1:3,1) / SQRT(SUM(N(1:3,1)*N(1:3,1)))                                                  
          BRICK_LIST(NIN,IB)%N(2,1:3) = N(1:3,2) / SQRT(SUM(N(1:3,2)*N(1:3,2))) 
          BRICK_LIST(NIN,IB)%N(3,1:3) = N(1:3,3) / SQRT(SUM(N(1:3,3)*N(1:3,3))) 
          BRICK_LIST(NIN,IB)%N(4,1:3) = N(1:3,4) / SQRT(SUM(N(1:3,4)*N(1:3,4))) 
          BRICK_LIST(NIN,IB)%N(5,1:3) = N(1:3,5) / SQRT(SUM(N(1:3,5)*N(1:3,5))) 
          BRICK_LIST(NIN,IB)%N(6,1:3) = N(1:3,6) / SQRT(SUM(N(1:3,6)*N(1:3,6))) 
        ELSEIF(I22_ALEUL==1)THEN      
          II      = brickID 
          !---8 local node numbers NC1 TO NC8 for solid element I ---! 
          NC1=IXS(2,II)                                             
          NC2=IXS(3,II)                                             
          NC3=IXS(4,II)                                             
          NC4=IXS(5,II)                                             
          NC5=IXS(6,II)                                             
          NC6=IXS(7,II)                                             
          NC7=IXS(8,II)                                             
          NC8=IXS(9,II)                                             
          ! 
          !---Coordinates of the 8 nodes                               
          X1_=X(1,NC1)                                            
          Y1_=X(2,NC1)                                            
          Z1_=X(3,NC1)                                            
          !                                                      
          X2_=X(1,NC2)                                            
          Y2_=X(2,NC2)                                            
          Z2_=X(3,NC2)                                            
          !                                                      
          X3_=X(1,NC3)                                            
          Y3_=X(2,NC3)                                            
          Z3_=X(3,NC3)                                            
          !                                                      
          X4_=X(1,NC4)                                            
          Y4_=X(2,NC4)                                            
          Z4_=X(3,NC4)                                            
          !                                                      
          X5_=X(1,NC5)                                            
          Y5_=X(2,NC5)                                            
          Z5_=X(3,NC5)                                            
          !                                                      
          X6_=X(1,NC6)                                            
          Y6_=X(2,NC6)                                            
          Z6_=X(3,NC6)                                            
          !                                                      
          X7_=X(1,NC7)                                            
          Y7_=X(2,NC7)                                            
          Z7_=X(3,NC7)                                            
          !                                                      
          X8_=X(1,NC8)                                            
          Y8_=X(2,NC8)                                            
          Z8_=X(3,NC8) 
          !
          ! Face-1
          N(1,1)=(Y3_-Y1_)*(Z2_-Z4_) - (Z3_-Z1_)*(Y2_-Y4_)
          N(2,1)=(Z3_-Z1_)*(X2_-X4_) - (X3_-X1_)*(Z2_-Z4_)
          N(3,1)=(X3_-X1_)*(Y2_-Y4_) - (Y3_-Y1_)*(X2_-X4_)
          ! Face-2
          N(1,2)=(Y7_-Y4_)*(Z3_-Z8_) - (Z7_-Z4_)*(Y3_-Y8_)
          N(2,2)=(Z7_-Z4_)*(X3_-X8_) - (X7_-X4_)*(Z3_-Z8_)
          N(3,2)=(X7_-X4_)*(Y3_-Y8_) - (Y7_-Y4_)*(X3_-X8_)
          ! Face-3
          N(1,3)=(Y6_-Y8_)*(Z7_-Z5_) - (Z6_-Z8_)*(Y7_-Y5_)
          N(2,3)=(Z6_-Z8_)*(X7_-X5_) - (X6_-X8_)*(Z7_-Z5_)
          N(3,3)=(X6_-X8_)*(Y7_-Y5_) - (Y6_-Y8_)*(X7_-X5_)
          ! Face-4
          N(1,4)=(Y2_-Y5_)*(Z6_-Z1_) - (Z2_-Z5_)*(Y6_-Y1_)
          N(2,4)=(Z2_-Z5_)*(X6_-X1_) - (X2_-X5_)*(Z6_-Z1_)
          N(3,4)=(X2_-X5_)*(Y6_-Y1_) - (Y2_-Y5_)*(X6_-X1_)
          ! Face-5
          N(1,5)=(Y7_-Y2_)*(Z6_-Z3_) - (Z7_-Z2_)*(Y6_-Y3_)
          N(2,5)=(Z7_-Z2_)*(X6_-X3_) - (X7_-X2_)*(Z6_-Z3_)
          N(3,5)=(X7_-X2_)*(Y6_-Y3_) - (Y7_-Y2_)*(X6_-X3_)
          ! Face-6
          N(1,6)=(Y8_-Y1_)*(Z4_-Z5_) - (Z8_-Z1_)*(Y4_-Y5_)
          N(2,6)=(Z8_-Z1_)*(X4_-X5_) - (X8_-X1_)*(Z4_-Z5_)
          N(3,6)=(X8_-X1_)*(Y4_-Y5_) - (Y8_-Y1_)*(X4_-X5_) 
          !                                                       
          BRICK_LIST(NIN,IB)%N(1,1:3) = N(1:3,1) / SQRT(SUM(N(1:3,1)*N(1:3,1)))                                                  
          BRICK_LIST(NIN,IB)%N(2,1:3) = N(1:3,2) / SQRT(SUM(N(1:3,2)*N(1:3,2))) 
          BRICK_LIST(NIN,IB)%N(3,1:3) = N(1:3,3) / SQRT(SUM(N(1:3,3)*N(1:3,3))) 
          BRICK_LIST(NIN,IB)%N(4,1:3) = N(1:3,4) / SQRT(SUM(N(1:3,4)*N(1:3,4))) 
          BRICK_LIST(NIN,IB)%N(5,1:3) = N(1:3,5) / SQRT(SUM(N(1:3,5)*N(1:3,5))) 
          BRICK_LIST(NIN,IB)%N(6,1:3) = N(1:3,6) / SQRT(SUM(N(1:3,6)*N(1:3,6))) 
        ENDIF
                                                  
        DO J=1,NV46                           
          ! compute Brick Face New Area !suplicated eflux3/aflux3, to optimize                           
          F(J,IB)        =  HALF * SQRT( SUM( N(:,J) * N(:,J) )  ) ! N is 2Sn                          
        ENDDO                                 
        pFACE(1:9)      => BRICK_LIST(NIN,IB)%POLY(1:9)                         
!        pFACE(2)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(2)%FACE(1:6)%Surf                                
!        pFACE(3)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(3)%FACE(1:6)%Surf                                
!        pFACE(4)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(4)%FACE(1:6)%Surf                                
!        pFACE(5)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(5)%FACE(1:6)%Surf                                
!        pFACE(6)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(6)%FACE(1:6)%Surf                                
!        pFACE(7)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(7)%FACE(1:6)%Surf                                
!        pFACE(8)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(8)%FACE(1:6)%Surf                                
!        pFACE(9)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Surf                                
        pFullFACE        => BRICK_LIST(NIN,IB)%Face_BRICK(1:6)                                           
        pFullFACE(1:6)   =  F(1:6,IB)         
        IF(BRICK_LIST(NIN,IB)%NBCUT==0)THEN 
          pFACE(1)%FACE(1:6)%Surf =  F(1:6,IB)        
          CYCLE                               
        ENDIF                                 
        NCELL            =  BRICK_LIST(NIN,IB)%NBCUT          !cell 9 not yet taken into account.        
        DO ICELL=1,NCELL                      
          !FACES UPDATE                       
          IF(BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew<ZERO)THEN                                            
            DO J=1,NV46                       
                pFACE(ICELL)%FACE(J)%Surf = F(J,IB) + pFACE(ICELL)%FACE(J)%Surf
                if(pFACE(ICELL)%FACE(J)%Surf<ZERO)then                                                        
                  write (*,*) "**error : inter22 negative cell face"                                     
                endif                         
            ENDDO! next J                     
           ENDIF                              
          !VOLUME UPDATE                      
          VOL = BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew                                                      
          IF(VOL<ZERO)THEN                 
            GBUF => ELBUF_TAB(NGB(IB))%GBUF   
            BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew = GBUF%VOL(IDLOCB(IB)) + VOL                             
          ENDIF                               
          VOLratio = BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew / ELBUF_TAB(NGB(IB))%GBUF%VOL(IDLOCB(IB))       
          !  THRESHOLD VOLUME : Smax = O(V**(2/3)), dh= EM03 * O(V**(1/3)) => Veps = EM04*O(V) => VOLratio = Veps/V = EM04     (EM04 => dh = 0.01 % * average_edge) 
          IF(ABS(VOLratio) <= EM04)THEN     
             I22_DEGENERATED = 1              
          ENDIF                               
        ENDDO!next ICELL                      
      ENDDO !next IB (cut cell)
  

      
      
      !------------------------------------------------------------!
      ! @10. BUILD CELL 9                                          !
      !     which is complementary one :                           !
      !    (faces,volumes,brick nodes and  num points)             !
      !------------------------------------------------------------! 
      DO IB=NBF,NBL
         pListNodID(1)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)
         pListNodID(2)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(2)%ListNodID(1:8) 
         pListNodID(3)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(3)%ListNodID(1:8)
         pListNodID(4)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(4)%ListNodID(1:8)
         pListNodID(5)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(5)%ListNodID(1:8)
         pListNodID(6)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(6)%ListNodID(1:8) 
         pListNodID(7)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(7)%ListNodID(1:8) 
         pListNodID(8)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(8)%ListNodID(1:8)  
         pListNodID(9)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(9)%ListNodID(1:8)   
         pSUBVOL(1:9)         => BRICK_LIST(NIN,IB)%POLY(1:9)
         pWhichCellNod(1:8)   => BRICK_LIST(NIN,IB)%NODE(1:8)
         pFACE(1:9)      => BRICK_LIST(NIN,IB)%POLY(1:9)!%FACE(1:6)%Surf
!         pFACE(2)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(2)%FACE(1:6)%Surf  
!         pFACE(3)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(3)%FACE(1:6)%Surf  
!         pFACE(4)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(4)%FACE(1:6)%Surf  
!         pFACE(5)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(5)%FACE(1:6)%Surf  
!         pFACE(6)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(6)%FACE(1:6)%Surf  
!         pFACE(7)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(7)%FACE(1:6)%Surf  
!         pFACE(8)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(8)%FACE(1:6)%Surf  
!         pFACE(9)%p(1:6)      => BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Surf    
         ID                   =  BRICK_LISt(NIN,IB)%ID              
         NCELL                =  BRICK_LIST(NIN,IB)%NBCUT
         Cod1                 =  IABS(BRICK_LIST(NIN,IB)%SecID_CELL(1))
         Cod2                 =  IABS(BRICK_LIST(NIN,IB)%SecID_CELL(2))         
         IF(NCELL/=0)THEN                                   !si decoupage de brique.
           !---VOLUME AND CONNECTIVITY                       
           BRICK_LIST(NIN,IB)%POLY(9)%FACE(1)%Surf      = F(1,IB)-SUM( (/ (BRICK_LIST(NIN,IB)%POLY(K)%FACE(1)%Surf,K=1,NCELL)/) )
           BRICK_LIST(NIN,IB)%POLY(9)%FACE(2)%Surf      = F(2,IB)-SUM( (/ (BRICK_LIST(NIN,IB)%POLY(K)%FACE(2)%Surf,K=1,NCELL)/) ) 
           BRICK_LIST(NIN,IB)%POLY(9)%FACE(3)%Surf      = F(3,IB)-SUM( (/ (BRICK_LIST(NIN,IB)%POLY(K)%FACE(3)%Surf,K=1,NCELL)/) ) 
           BRICK_LIST(NIN,IB)%POLY(9)%FACE(4)%Surf      = F(4,IB)-SUM( (/ (BRICK_LIST(NIN,IB)%POLY(K)%FACE(4)%Surf,K=1,NCELL)/) ) 
           BRICK_LIST(NIN,IB)%POLY(9)%FACE(5)%Surf      = F(5,IB)-SUM( (/ (BRICK_LIST(NIN,IB)%POLY(K)%FACE(5)%Surf,K=1,NCELL)/) ) 
           BRICK_LIST(NIN,IB)%POLY(9)%FACE(6)%Surf      = F(6,IB)-SUM( (/ (BRICK_LIST(NIN,IB)%POLY(K)%FACE(6)%Surf,K=1,NCELL)/) )                                           
           BRICK_LIST(NIN,IB)%POLY(9)%FACE0%Surf        =         SUM( (/ (BRICK_LIST(NIN,IB)%POLY(K)%FACE0%Surf,K=1,NCELL)  /) )           !sum of cut surfaces                
           BRICK_LIST(NIN,IB)%POLY(9)%Vnew              = -SUM(BRICK_LIST(NIN,IB)%POLY(1:NCELL)%Vnew)
           !print *, "pSUBVOL(9)=", pSUBVOL(9)
           BRICK_LIST(NIN,IB)%POLY(9)%NumNOD       = 0                                  !should noty be needed                 
           BRICK_LIST(NIN,IB)%POLY(9)%NumPOINT  = 0                                  !should noty be needed                 
           MNOD           = 0              
           pPOINT(1:3)    = ZERO           
           !saving brick nodes used by remaining polyhedron (complementary one)                                                          
           DO J=1,8                        
             IF(pWhichCellNod(J)%WhichCell/=0) CYCLE   !a tagged brick node is already used by a polyhedron
             pWhichCellNod(J)%WhichCell      = 9       
             MNOD                  = MNOD + 1
             pListNodID(9)%p(MNOD) = J !IXS(J+1,ID)                                                     
             pPOINT(1)             = pPOINT(1) + X(1,IXS(1+J,ID))                                       
             pPOINT(2)             = pPOINT(2) + X(2,IXS(1+J,ID))                                       
             pPOINT(3)             = pPOINT(3) + X(3,IXS(1+J,ID))                                       
           ENDDO                           
           BRICK_LIST(NIN,IB)%POLY(9)%NumNOD  =  MNOD           
           BRICK_LIST(NIN,IB)%POLY(9)%NumPOINT  = 
     .         SUM(BRICK_LIST(NIN,IB)%POLY(1:NCELL)%NumPOINT ) -  SUM(BRICK_LIST(NIN,IB)%POLY(1:NCELL)%NumNOD) + MNOD               
           GBUF           => ELBUF_TAB(NGB(IB))%GBUF                                                  
           BRICK_LIST(NIN,IB)%POLY(9)%Vnew     =  GBUF%VOL(IDLOCB(IB)) + BRICK_LIST(NIN,IB)%POLY(9)%Vnew                                        
           UNCUTvol       =  GBUF%VOL(IDLOCB(IB))                                       
           IF(ABS(BRICK_LIST(NIN,IB)%POLY(9)%Vnew /UNCUTVOL) <EM04 
     .   .OR. ABS(BRICK_LIST(NIN,IB)%POLY(1)%Vnew /UNCUTVOL) <EM04)THEN                                               
             I22_DEGENERATED = 1                                                 
           ENDIF
           !---AND ALSO CELL CENTER 
           !MNOD brick nodes above + K1 intersection points below.       
           NPOINT = BRICK_LIST(NIN,IB)%POLY(9)%NumPOINT  
           K1 = 0        
           DO J=1,12                       
             NBCUT = BRICK_LIST(NIN,IB)%EDGE(J)%NBCUT                                                 
             DO I=1,NBCUT                  
               PointTMP(1:3) = BRICK_LIST(NIN,IB)%EDGE(J)%CUTPOINT(1:3,I)                             
               pPOINT(1)     = pPOINT(1) + PointTMP(1)                                                
               pPOINT(2)     = pPOINT(2) + PointTMP(2)                                                
               pPOINT(3)     = pPOINT(3) + PointTMP(3)  
               K1 = K1 + 1 
             ENDDO                         
           ENDDO                           
           IF(NPOINT/=0)THEN             
             pPOINT(1) = pPOINT(1) / (K1+MNOD) !NPOINT
             pPOINT(2) = pPOINT(2) / (K1+MNOD) !NPOINT
             pPOINT(3) = pPOINT(3) / (K1+MNOD) !NPOINT
             BRICK_LIST(NIN,IB)%POLY(9)%CellCENTER(1:3) = pPOINT(1:3)                                       
           ENDIF                           
         ENDIF!NCELL/=0 
       ENDDO!next IB   
       
      !------------------------------------------------------------!
      ! @11. BUILD UNCUT CELL IN CUT CELL BUFFER                   !
      !------------------------------------------------------------!   
      DO IB=NBF,NBL    
        IF(BRICK_LIST(NIN,IB)%NBCUT == 0)THEN      
          pSUBVOL(1:9)      => BRICK_LIST(NIN,IB)%POLY(1:9)
          pSUBVOL(1:9)%Vnew =  0
          GBUF         => ELBUF_TAB(NGB(IB))%GBUF
          pSUBVOL(1)%Vnew   =  GBUF%VOL(IDLOCB(IB))
          BRICK_LIST(NIN,IB)%Vnew_SCell = GBUF%VOL(IDLOCB(IB))
        ENDIF     
      ENDDO 
        
      !------------------------------------------------------------!
      ! @12. STORING BRICK REFERENCE VOLUME (UNCUT)                !
      !------------------------------------------------------------! 
      DO IB=NBF,NBL
        BRICK_LIST(NIN,IB)%UncutVol = ELBUF_TAB(NGB(IB))%GBUF%VOL(IDLOCB(IB))           
      ENDDO   
      
    9 CONTINUE

      
      !------------------------------------------------------------!
      ! @13. POLY 9 : MARK IF NO NODE ON A FACE                    !
      !------------------------------------------------------------!       
      !Poly9woNodes
      DO IB=NBF,NBL
        NCELL = BRICK_LIST(NIN,IB)%NBCUT
        BRICK_LIST(NIN,IB)%Poly9woNodes(1:6,1:2) = 0
        IF(NCELL<= 1) CYCLE
        DO J=1,6                      
          FACE = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Surf                                               
          IF(FACE ==ZERO) CYCLE     
          !loop on nodes (1,2,3,4,5,6,7,8) composing face J                                      
          lFOUND = .FALSE.            
          DO K=1,4                    
            INOD  = INT22_BUF%nodFACE(J,K)      
            ICELL = BRICK_LIST(NIN,IB)%NODE(INOD)%WhichCell                                   
            IF(ICELL == 9) lFOUND = .TRUE.                                                       
          ENDDO                       
          IF(lFOUND)CYCLE ! CELL 9 IS SHARING A NODE SO ITS ADJACENT CELL WAS FOUND ABOVE (See 14. FINDING & STORING ADJACENT CELLS)              
          !Here Cell 9 has a trace on face J but has no nodes on this face. An adjacent celle must be find (otherwise internal forces issue, missing area)  
          BRICK_LIST(NIN,IB)%Poly9woNodes(J,1) = 1    
        ENDDO!next J                  
        !UPDATE FLUX FOR THIS CASE : NO NODES => NO FLUXES (CLOSED SURFACE HYPOTHESIS)
      ENDDO! next IB



      DO IB=NBF,NBL
        BRICK_LIST(NIN,IB)%IsMergeTarget    = 0
        !print *, "0, ismergetarget", ixs(11,brick_list(nin,ib)%id)
      ENDDO


      !------------------------------------------------------------!
      ! @14. FILL MATERIAL BUFFER : %bakMAT%...                    !
      !------------------------------------------------------------!      
      DO IB=NBF,NBL
        NCELL                    =  BRICK_LIST(NIN,IB)%NBCUT  
        MCELL                    =  BRICK_LIST(NIN,IB)%MainID
        ID                       =  BRICK_LIST(NIN,IB)%ID
        NG                       =  BRICK_LIST(NIN,IB)%NG
        IDLOC                    =  BRICK_LIST(NIN,IB)%IDLOC   
        MLW                      =  BRIcK_LIST(NIN,IB)%MLW  
        GBUF                     => ELBUF_TAB(NG)%GBUF
        LBUF                     => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
        MBUF                     => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)                        
        LLT_                     =  IPARG(2,NG)        
        ICELL                    =  0 
        IF(MCELL==0)THEN
          BRICK_LIST(NIN,IB)%bakMAT%RHO     = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%rhoE    = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%rhoU(1) = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%rhoU(2) = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%rhoU(3) = ZERO
          BRICK_LIST(NIN,IB)%bakMAT%ssp     = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%SIG(1)  = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%SIG(2)  = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%SIG(3)  = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%SIG(4)  = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%SIG(5)  = ZERO 
          BRICK_LIST(NIN,IB)%bakMAT%SIG(6)  = ZERO 
          !------MULTI-MATERIAL-----------!
          IF(I22LAW37+I22LAW51 == 0) CYCLE
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(1) = ZERO
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(2) = ZERO
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(3) = ZERO
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(4) = ZERO
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(5) = ZERO
          IF(I22LAW51 == 0) CYCLE 
          DO K=6, I22LAW51
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(K) =  ZERO       
          ENDDO!next K         
        ELSE
          BRICK_LIST(NIN,IB)%bakMAT%RHO     = GBUF%RHO(IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%rhoE    = GBUF%EINT(IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%rhoU(1) = GBUF%MOM(LLT_*(1-1) +IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%rhoU(2) = GBUF%MOM(LLT_*(2-1) +IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%rhoU(3) = GBUF%MOM(LLT_*(3-1) +IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%ssp     = LBUF%SSP(IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%SIG(1)  = GBUF%SIG(LLT_*(1-1) +IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%SIG(2)  = GBUF%SIG(LLT_*(2-1) +IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%SIG(3)  = GBUF%SIG(LLT_*(3-1) +IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%SIG(4)  = GBUF%SIG(LLT_*(4-1) +IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%SIG(5)  = GBUF%SIG(LLT_*(5-1) +IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%SIG(6)  = GBUF%SIG(LLT_*(6-1) +IDLOC)
          !------MULTI-MATERIAL-----------!
          !IF(I22LAW37+I22LAW51 == 0) CYCLE
          IF(MLW/=37 .AND. MLW/=51)CYCLE
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(1) = MBUF%VAR((1-1)*LLT_+IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(2) = MBUF%VAR((2-1)*LLT_+IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(3) = MBUF%VAR((3-1)*LLT_+IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(4) = MBUF%VAR((4-1)*LLT_+IDLOC)
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(5) = MBUF%VAR((5-1)*LLT_+IDLOC)
          IF(I22LAW51 == 0) CYCLE 
          DO K=6, I22LAW51
          BRICK_LIST(NIN,IB)%bakMAT%UVAR(K) = MBUF%VAR((K-1)*LLT_+IDLOC)        
          ENDDO!next K 
        ENDIF 
      ENDDO!next IB

      !-------------------!
        CALL MY_BARRIER()
      !-------------------!
     
      !------------------------------------------------------------!
      ! @15. FINDING & STORING ADJACENT CELLS                      !
      !------------------------------------------------------------!
      DO IB=NBF,NBL
        pFACE(1:9)       => BRICK_LIST(NIN,IB)%POLY(1:9)
!        pFACE(2)%p(1:6)       => BRICK_LIST(NIN,IB)%POLY(2)%FACE(1:6)%Surf  
!        pFACE(3)%p(1:6)       => BRICK_LIST(NIN,IB)%POLY(3)%FACE(1:6)%Surf  
!        pFACE(4)%p(1:6)       => BRICK_LIST(NIN,IB)%POLY(4)%FACE(1:6)%Surf  
!        pFACE(5)%p(1:6)       => BRICK_LIST(NIN,IB)%POLY(5)%FACE(1:6)%Surf  
!        pFACE(6)%p(1:6)       => BRICK_LIST(NIN,IB)%POLY(6)%FACE(1:6)%Surf  
!        pFACE(7)%p(1:6)       => BRICK_LIST(NIN,IB)%POLY(7)%FACE(1:6)%Surf  
!        pFACE(8)%p(1:6)       => BRICK_LIST(NIN,IB)%POLY(8)%FACE(1:6)%Surf  
!        pFACE(9)%p(1:6)       => BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Surf         
        !pNAdjCELL             => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%NAdjCell        
         pAdjBRICK             => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5)
         pListNodID(1)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)
         pListNodID(2)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(2)%ListNodID(1:8) 
         pListNodID(3)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(3)%ListNodID(1:8)
         pListNodID(4)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(4)%ListNodID(1:8)
         pListNodID(5)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(5)%ListNodID(1:8)
         pListNodID(6)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(6)%ListNodID(1:8) 
         pListNodID(7)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(7)%ListNodID(1:8) 
         pListNodID(8)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(8)%ListNodID(1:8)  
         pListNodID(9)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(9)%ListNodID(1:8)     
        ID                    =  BRICK_LIST(NIN,IB)%ID
        NCELL                 =  BRICK_LIST(NIN,IB)%NBCUT
        !pNAdjCELL(1:9,1:6)    =  0  
        DO K=1,6      
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Adjacent_Cell(1) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Adjacent_Cell(2) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Adjacent_Cell(3) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Adjacent_Cell(4) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Adjacent_Cell(5) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%NAdjCell         = 0
        ENDDO
        ICELL                 =  0                        
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9          
          DO J=1,NV46 ! loop on brick face
            IF(pFACE(ICELL)%FACE(J)%Surf>ZERO)THEN ! if polyhedron has a non zero subface on this brick face 
              IV =  pAdjBRICK(J,1)
              IF(IV==0)CYCLE !if there is no adjacent brick on this face
              !if there is an adjacent brick on this face
              IAD22 = pAdjBRICK(J,4) 
              IF(IAD22 == 0)THEN
              !adjacent brick is in cut cell buffer : it also uncut
                BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell         = 1
                BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(1) = 1 !necessarily ICELLv=1                
              ELSEIF(BRICK_LIST(NIN,IAD22)%NBCUT==0)THEN
              !adjacent brick is in cut cell buffer but is uncut
                BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell         = 1
                BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(1) = 1 !necessarily ICELLv=1    
              ELSE
             
                pWhichCellNodv(1:8) => BRICK_LIST(NIN,IAD22)%NODE(1:8)
                !---brick nodes used by reference cell, must lay on current face J, and are identified by internal id : IXS(1+.,ID)
                NNODES = 0
                DO K1=1,BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD  !loop on polyhedron nodes   !TODO : might be optimized by looping on nodFACE and cycle with pNNODCELL
                  DO K2=1,4               !loop on face nodes
                    IF(pListNodID(ICELL)%p(K1) == INT22_BUF%nodFACE(J,K2))THEN !polyhedron node on the current face
                      NNODES         = NNODES +1
                      INODES(NNODES) = IXS(1+INT22_BUF%nodFACE(J,K2), ID)
                    ENDIF
                  ENDDO
                ENDDO
                !---search for local nodes id in adjacent brick.
                ICELLTAG(1:9) = 0
                JV            = pAdjBRICK(J,5)
                DO K0=1,4 !loop on adjacent 'local' nodes (II,J)<->(IV,JV)
                  K1 = INT22_BUF%nodFACE(JV,K0)
                  DO IN=1,NNODES
                    IF(IXS(1+K1,IV)==INODES(IN) )THEN
                      ICELLv = pWhichCellNodv(K1)%WhichCell
                      if (ICELLv==0)then
                        print *, "ITASK,ICELLv,pWhichCellNodv(K1)",ITASK,ICELLv,pWhichCellNodv(K1)%WhichCell
                        stop 2245
                      endif
                      IF(ICELLTAG(ICELLv)==0)THEN !check if cell is already stored as an adjacent cell, if not store it
                        ICELLTAG( ICELLv )                = 1
                        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell  = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell + 1
                        NAdjCell                                         = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell
                        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(NadjCell) = ICELLv
                      ENDIF
                    ENDIF
                  ENDDO!next IN (polyhedron node on face)
                ENDDO!next K0 (adjacent 'local' node)
              ENDIF!testing IV & IBV           
            ENDIF!(pFACE(ICELL)%p(J)>ZERO)

            !COMPLEMENTARY POLYHEDRON WITH NO NODES ON A GIVEN FACE
            Poly9woNodes = BRICK_LIST(NIN,IB)%Poly9woNodes(J,1)
            ! forcemment IBv /= 0
            IF(Poly9woNodes == 1)THEN
              IV                                                   = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1)    
              IBv                                                  = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4) 
              IF(IBv==0)THEN
                  BRICK_LIST(NIN,IB)%Poly9woNodes(J,2)             = 0 
                  CYCLE
              ENDIF   
              JV                                                   = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)    
              Poly9woNodesV                                        = BRICK_LIST(NIN,IBv)%Poly9woNodes(Jv,1)      
              IF(Poly9woNodesV==0)THEN
                NBCUTv                                             = BRICK_LIST(NIN,IBv)%NBCUT                 
                IF(NBCUTv == 0)THEN
                  !PAS DE VOISIN HYPOTHESE SURFACE FERMEE
                  !le polyedre voisin est le polyedre 9
                  !    +--------+--+-----------+
                  !    |         \1|           !
                  !    |           |           |
                  !    |     9     |        1  |
                  !    |           |           |
                  !    |         / |           |
                  !    |       / 2 |           |
                  !    +------+----+-----------+                  
                  !BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell               = BRICK_LIST(NIN,IB)%PoLY(9)%FACE(J)%NAdjCell + 1      
                  !NAdjCell                                        = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell         
                  !BRICK_LIST(NIN,IB)%Adjacent_Brick(J,9,NadjCell) = 1  
                  BRICK_LIST(NIN,IB)%Poly9woNodes(J,2)             = 0 
C                  !DESTRUCTION POLYEDRE VOISIN DETECTE AU DEBUT DU PARAGRAPHE @14.
C                  pNAdjCell(ICELL,J)  = 0
C                  pAdjCELL(J,ICELL,1) = 0 !necessarily ICELLv=1                                                         
C  non sinon cell trapped error et aussi on ne peut pas trouver de pression voisin poru le contact
                ELSE
                  !le polyedre voisin utilise le point d'intersection le plus proche du noeud sommet => le polyedre voisin deveint systematiquement le polyedre 9. (faire dessins)
                  !    +--------+--+-----------+       +--------+--+-----------+     
                  !    |         \1|           !       |         \1|       1   !     
                  !    |           +           !       |           +-----------+     
                  !    |     9     |        9  |       |     9     |        9  |     
                  !    |           +-----------+       |           |           |     
                  !    |         / |        1  |       |         / |           |     
                  !    |       / 2 |           |       |       / 2 |           |     
                  !    +------+----+-----------+       +------+----+-----------+     
                  !autre hypothese  => ... = 1
                  BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell      = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell + 1   
                  NAdjCell                                         = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell       
                  BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Adjacent_Cell(NadjCell)   = 9  
                  BRICK_LIST(NIN,IB)%Poly9woNodes(J,2)             = 9                                                      
                ENDIF
              ELSEIF(Poly9woNodesV/=0)THEN
                  !le polyedre voisin est le polyedre 9
                  !    +--------+--+-----------+
                  !    |         \1|       2   !
                  !    |           +-----------+
                  !    |     9     |        9  |
                  !    |           +-----------+
                  !    |         / |        1  |
                  !    |       / 2 |           |
                  !    +------+----+-----------+
              
                  ! SIMPLY CONNECTING poly_9 <-> polyV_9
                  BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell      = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell + 1   
                  NAdjCell                                         = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell      
                  BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Adjacent_Cell(NadjCell)   = 9 
                  BRICK_LIST(NIN,IB)%Poly9woNodes(J,2)             = 9                                                       
              ENDIF!(Poly9woNodesV==0)                  
            ENDIF
          ENDDO!next J (face)
        ENDDO!next ICELL
      ENDDO!next IB


      !------------------------------------------------------------!
      ! @16. BUILD POLYGONAL TRACE FOR POLY9                       !
      !------------------------------------------------------------! 
      !CUT CELL BUFFER REMINDER (check common source for complete & update source)  
      !my_real                                   :: N(3)                    !normal vector                                   
      !my_real                                   :: B(3)                    !basis                                           
      !my_real                                   :: SCUT(1)                 !Cut Area                                        
      !my_real                                   :: SEFF(1)                 !Efficace Section = Wetted Surface : Cut Aera - Holes         (computed by suming lagrangian projection)    
      !my_real                                   :: P(3,8)                  !Cut Surface Polygon (coordinates)               
      !INTEGER                                   :: NP                      !Cut Surface Polygon (number of points) ! NP<=6 | NP<=8 for additional Scut (closed surface hypothesis) 
      !BRICK_LIST(NIN,IB)%Edge(J)%NODE(1:2)       = EDGE_LIST(NIN,K)%NODE(1:2)
      !BRICK_LIST(NIN,IB)%Edge(J)%NBCUT           = EDGE_LIST(NIN,K)%NBCUT
      !BRICK_LIST(NIN,IB)%Edge(J)%CUTSHELL(1:2)   = EDGE_LIST(NIN,K)%CUTSHELL(1:2)
      !BRICK_LIST(NIN,IB)%Edge(J)%CUTCOOR(1:2)    = EDGE_LIST(NIN,K)%CUTCOOR(1:2)                   
      !BRICK_LIST(NIN,IB)%Edge(J)%CUTPOINT(1:3,1) = EDGE_LIST(NIN,K)%CUTPOINT(1:3,1)
      !BRICK_LIST(NIN,IB)%Edge(J)%CUTPOINT(1:3,2) = EDGE_LIST(NIN,K)%CUTPOINT(1:3,2)
      !BRICK_LIST(NIN,IB)%Edge(J)%CUTVEL(1:3,1)   = EDGE_LIST(NIN,K)%CUTVEL(1:3,1)
      !BRICK_LIST(NIN,IB)%Edge(J)%CUTVEL(1:3,2)   = EDGE_LIST(NIN,K)%CUTVEL(1:3,2) 
      !BRICK_LIST(NIN,IB)%Edge(J)%VECTOR(1:3)     = EDGE_LIST(NIN,K)%VECTOR(1:3)                             
      !BRICK_LIST(NIN,IB)%Edge(J)%LEN             = EDGE_LIST(NIN,K)%LEN 
      !
      !                  
      !    +--------+--+ +-------+---+
      !    |         \1| |       | \ |
      !    |           +    Check WhichCellNode for each node of the face           |       |   +
      !    |     9     |      if digits 1,2, and 9 tagged then a polygon has   =>   |       | P |
      !    |           +      to be computed                                        |       |   +
      !    |         / | |       |  /|
      !    |       / 2 | |       |/  |
      !    +------+----+ +------+----+
      !
      ! 
      !must be counterclockwise from outward ( clockwise from point inside the brick  )
      DO IB=NBF,NBL
        IE                                             = BRICK_LIST(NIN,IB)%ID
        BRICK_LIST(NIN,IB)%ClosedSurf(1:6)             = 0       
        DO J=1,6
        ICODE                                          = 0
        IV                                             = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1)
        IF(IV==0)CYCLE
          DO K=1,4 
            INOD  = INT22_BUF%nodFACE(J,K)
            ICELL = BRICK_LIST(NIN,IB)%NODE(INOD)%WhichCell
            ICODE = IBSET(ICODE,ICELL)
          ENDDO
          
          !ON EN PROFITE POUR MARQUER LES CAS DE FERMETURE (CLOSURE HYPOTHESIS)
          IF(ICODE == 518 .OR. ICODE == 6) THEN
            FACE9 = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Surf
            IBV   = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
            !necessairement IBV>0 sinon on ne peut pas avoir ICODE=518 (autrement dit : la brique IB est intersectee donc la voisine est dans le buffer)
            NBCUTv = BRICK_LIST(NIN,IBv)%NBCUT
            IF(NBCUTv == 0 .AND. FACE9>ZERO) THEN
              !DISTINCITON IMPORTANTE CAR ON A BESOIN DU VOISIN POUR LES FORCES DE COTNACT MAIS SINON IL DOIT ETRE IGNORE POUR FINT ET CONVECTION
              BRICK_LIST(NIN,IB)%ClosedSurf(J) = 1
C              print *, "brick_id=",ixs(11,iv),"    face=",J, "  has a closed surface"
C              print *, "sinit22_fvm marked closure hypothesis."
            ENDIF
          ENDIF
          
          Poly9woNodes                                 = BRICK_LIST(NIN,IB)%Poly9woNodes(J,1)
          ICELLv                                       = BRICK_LIST(NIN,IB)%Poly9woNodes(J,2)          
          IF(Poly9woNodes /=0 .AND. ICELLv /= 0)CYCLE       
          IF(Poly9woNodes==0 .AND. ICODE /= 518)CYCLE                                
          ! poly 1,2,9 on this current face => ICODE = b1000000110 = 518   (bits 1,2,9)
          NP                                           = 0
          pPOINT(1:3)                                  = ZERO
          CUT_Vel(1:3)                                 = ZERO
          NBCUTprev                                    = 0
          DO INOD=4,1,-1
            !base point
            pointTMP(1:3)                              = X(1:3,IXS(1+INT22_BUF%NodFace(J,INOD),IE))
            pPOINT(1)                                  = pPOINT(1) + pointTMP(1)
            pPOINT(2)                                  = pPOINT(2) + pointTMP(2)
            pPOINT(3)                                  = pPOINT(3) + pointTMP(3)
            !polygon point                    
            iEDG                                       = INT22_BUF%iLeftEdge(J,INOD)
            ISGN(1:2)                                  = (/1,2/)
            IF(iEDG < 0) ISGN(1:2)                     = (/2,1/)
            iEDG                                       = IABS(iEDG)
            NBCUT                                      = BRICK_LIST(NIN,IB)%Edge(iEDG)%NBCUT
            IF(NBCUT == 0)THEN
              CYCLE
            ELSEIF(NBCUT == 1)THEN
              NP                                       = NP + 1
              CUT_Point(1:3)                           = BRICK_LIST(NIN,IB)%Edge(iEDG)%CUTPOINT(1:3,1)
              BRICK_LIST(NIN,IB)%PCUT(8+J)%P(1:3,NP)   = CUT_Point(1:3)              
              CUT_Vel(1:3)                             = CUT_Vel(1:3) + BRICK_LIST(NIN,IB)%Edge(iEDG)%CUTVEL(1:3,1)
!              IF(NBCUTprev==1)THEN
!                !rajouter le point sommet au polygone 9
!                NP                                     = NP + 1
!                NP _NODE                               = NP_NODE + 1
!                CUT_Point(1:3)                         = X(1:3,IXS(1+iEDGE(ISGN(1),iEDG) ,IE))
!                BRICK_LIST(NIN,IB)%PCUT(8+J)%P(1:3,NP) = CUT_Point(1:3)              
!                !do not increment CUTVEL
!                print *, "pause-22- "
!                pause
!              ENDIF
            ELSE
              NP                                       = NP + 1
              CUT_Point(1:3)                           = BRICK_LIST(NIN,IB)%Edge(iEDG)%CUTPOINT(1:3,ISGN(1))
              BRICK_LIST(NIN,IB)%PCUT(8+J)%P(1:3,NP)   = CUT_Point(1:3)
              CUT_Vel(1:3)                             = CUT_Vel(1:3) + BRICK_LIST(NIN,IB)%Edge(iEDG)%CUTVEL(1:3,ISGN(1))              
              NP                                       = NP + 1              
              CUT_Point(1:3)                           = BRICK_LIST(NIN,IB)%Edge(iEDG)%CUTPOINT(1:3,ISGN(2))
              CUT_Vel(1:3)                             = CUT_Vel(1:3) + BRICK_LIST(NIN,IB)%Edge(iEDG)%CUTVEL(1:3,ISGN(2))                            
              BRICK_LIST(NIN,IB)%PCUT(8+J)%P(1:3,NP)   = CUT_Point(1:3)                            
            ENDIF
            NBCUTprev                                  = NBCUT
          ENDDO!next INOD
          BRICK_LIST(NIN,IB)%PCUT(8+J)%NP              = NP
          BRICK_LIST(NIN,IB)%PCUT(8+J)%B(1)            = FOURTH * pPOINT(1)
          BRICK_LIST(NIN,IB)%PCUT(8+J)%B(2)            = FOURTH * pPOINT(2)
          BRICK_LIST(NIN,IB)%PCUT(8+J)%B(3)            = FOURTH * pPOINT(3)                   
          BRICK_LIST(NIN,IB)%PCUT(8+J)%N(1:3)          = BRICK_LIST(NIN,IB)%N(J,1:3) * BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Surf
          BRICK_LIST(NIN,IB)%PCUT(8+J)%SEFF            = ZERO 
          !BRICK_LIST(NIN,IB)%POLY(9)%FACE0%Vel(1:3)    = CUT_Vel(1:3) / (NP*ONE)
          BRICK_LIST(NIN,IB)%PCUT(8+J)%Vel(1:3)        = CUT_Vel(1:3) / (NP*ONE)
          pPOINT(1:3)                                  = BRICK_LIST(NIN,IB)%PCUT(8+J)%B(1:3)
          VecTMP(1:3)                                  = I22AERA(NP, BRICK_LIST(NIN,IB)%PCUT(8+J)%P(1:3,1:NP), pPOINT(1:3) )
          FACE                                         = SQRT(SUM(VecTMP(1:3)*VecTMP(1:3)))
          BRICK_LIST(NIN,IB)%PCUT(8+J)%SCUT            = FACE          
        ENDDO!next J
      ENDDO!next IB

            
      !-------------------!
        CALL MY_BARRIER()
      !-------------------!
      
      !------------------------------------------------------------!
      ! @17. PRE-TREATMENT FOR DBLE SECND WITH BOTH V=0            !
      !------------------------------------------------------------!
      NBL1 = NBL
      IF(I22_DEGENERATED == 1)THEN
        ALLOCATE (DESTROY_DATA(7,2*NB)) !on each thread
      ELSE
        NBL1 = 0
      ENDIF
      !-------------------!
        CALL MY_BARRIER()
      !-------------------!
      IPOS = 0
      DO IB=NBF,NBL1
        NCELL                 =  BRICK_LIST(NIN,IB)%NBCUT
        ICELL                 =  0                        
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9          
            VOL      = BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew
            UNCUTVOL = BRICK_LIST(NIN,IB)%UncutVol
            RATIO    = VOL / UNCUTVOL
            IF(ABS(RATIO)>EM04)CYCLE    !Vol=1.01e-05, UNCUTVOL=10 => ratio > EM06. So EM04 is retained.   THRESHOLD VOLUME : Smax = O(V**(2/3)), dh= EM03 * O(V**(1/3)) => Veps = EM04*O(V) => ratio=Veps/V = EM04 
            IDLOC    = MAXLOC(BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%SURF,1)
            RatioF   = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(IDLOC)%SURF / BRICK_LIST(NIN,IB)%Face_Brick(IDLOC) 
            !volume degenere
            !on cherche une correspondance voisine
            J = IDLOC
              NAdjCELL    = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell
              DO IADJ     = 1,NAdjCELL
                ICELLv    = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(IADJ)
                IBV       = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
                JV        = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)                
                ITASKv    = BRICK_LIST(NIN,IBv)%ITASK
                VOLv      = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%Vnew
                UNCUTVOLv = BRICK_LIST(NIN,IBv)%UncutVOL
                RATIOv    = VOLv/UncutVOLv
                !Vol=1.01e-05, UNCUTVOL=10 => ratio > EM06. So EM04 is retained.   THRESHOLD VOLUME : Smax = O(V**(2/3)), dh= EM03 * O(V**(1/3)) => Veps = EM04*O(V) => ratio=Veps/V = EM04 
                IF(ABS(RATIOv)>EM04  .OR. IBv>IB )CYCLE 
                  !IF( (ITASKv==ITASK.AND.IB>IBv) .OR. (ITASKv<ITASK) )THEN
                    IPOS                 = IPOS + 1
                    DESTROY_DATA(1,IPOS) = NIN
                    DESTROY_DATA(2,IPOS) = IB
                    DESTROY_DATA(3,IPOS) = ICELL
                    DESTROY_DATA(4,IPOS) = ICELLv
                    DESTROY_DATA(5,IPOS) = IBv
                    DESTROY_DATA(6,IPOS) = J
                    DESTROY_DATA(7,IPOS) = Jv
                 ! ENDIF
              ENDDO!next ICV
            !J was IDLOC
        ENDDO!next I
      ENDDO!next IB  
      
      !detach from the previous loop and launch after barrier to avoid multi-threading issue
      if(itask==0 .AND. ibug22_destroy==1 .and. IPOS>0)then
        print *, ""
        print *, "  |------i22_destroy_cell.F-------|"
        print *, "  |  INITIALIZATION SUBROUTINE    |"
        print *, "  |-------------------------------|"        
        print *, ""
      endif

      !-------------------!
        CALL MY_BARRIER()
      !-------------------!

      DO I=1,IPOS
        IB     = DESTROY_DATA(2,I)
        ICELL  = DESTROY_DATA(3,I)
        ICELLv = DESTROY_DATA(4,I)
        IBv    = DESTROY_DATA(5,I)
        J      = DESTROY_DATA(6,I)
        Jv     = DESTROY_DATA(7,I)
        !print *, "destroy", ixs(11,brick_list(nin,ib)%id),ICELL
        CALL DESTROY_CELL( NIN, IB,ICELL,ICELLv,IBv,J,Jv, IXS, ITASK)
      ENDDO
        
      IF(I22_DEGENERATED == 1)THEN ! this is activated if SECONDARY/SECONDARY adjacency with both 0-volumes
      !-------------------!
        CALL MY_BARRIER()
      !-------------------!

        I22LOOP = I22LOOP + 1 
        if(I22LOOP >= 2)then
          print *, "**error : inter22, unexpected situation."
        endif    
        I22_DEGENERATED = 0          
        GOTO 9 !can be optimized later by updating adjacency in destroy_cell.F or looping only on bricks related to destroyed cell. (need to reach finish line)
      ENDIF

      
      !------------------------------------------------------------!
      ! @18. TAGGING SECONDARY CUT CELL                                !
      !------------------------------------------------------------!
        DO IB=NBF,NBL
          NG = BRICK_LIST(NIN,IB)%NG
            pMainID      => BRICK_LIST(NIN,IB)%MainID
            pIsMain(1:9) => BRICK_LIST(NIN,IB)%POLY(1:9)
            pIsMain(1:9)%IsMain =  0           
            pMainID      =  0            
            IF(BRICK_LIST(NIN,IB)%NBCUT==0)THEN
              pIsMain(1)%IsMain = 1
              pMainID    = 1            
              CYCLE
            ENDIF      
            GBUF   => ELBUF_TAB(NGB(IB))%GBUF              
            VOL    =  BRICK_LIST(NIN,IB)%UncutVol
            NCELL  =  BRICK_LIST(NIN,IB)%NBCUT
            ICELL  =  0
            !tag les potentiel mains
            DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
              ICELL = ICELL +1
              IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9          
              !if lower than 50% then it is a SECONDARY.
              VolCELL = BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew
              FAC     = VolCELL / VOL
              IF(FAC>CritMerge22)THEN
                !print *, "CritMerge22=", CritMerge22
                pIsMain(ICELL)%IsMain = 1
              ENDIF
            ENDDO!next ICELL
            K = SUM(pIsMain(1:9)%IsMain)
            !In case of several potential main :Unicity with Reproductibility (indepandant from numbering)          
            IF(K==1)THEN
              pMainID = MAXLOC(pIsMain(1:9)%IsMain,1)
            ELSEIF(K>=2)THEN
              MCELL = GET_UNIQUE_MAIN_CELL(NIN,IB,K)
              pIsMain(1:9)%IsMain   = 0
              pIsMain(MCELL)%IsMain = 1
              pMainID        = MCELL
            !In case of no cell satisfy criteria, choose cell 9 to avoid problem with partial cut
            ELSEIF(K==0)THEN
              pIsMain(9)%IsMain     = 1
              pMainID        = 9
            ENDIF                 
        ENDDO!next IB  

      !-------------------!
        CALL MY_BARRIER()
      !-------------------!
      !------------------------------------------------------------!
      ! @19. LINKING SECONDARY CELL TO A main ONE                    !
      !------------------------------------------------------------! 
      N_UNLINKED_L(ITASK) = 0      
      !if no main cell is found, it becomes itself a main cell (example : INTERF/INT_22/1BRICK-EOS/CEL_only)                   
      DO IB=NBF,NBL
        IE             =  BRICK_LIST(NIN,IB)%ID
        NCELL          =  BRICK_LIST(NIN,IB)%NBCUT 
        pAdjBRICK      => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5)
        !pAdjCELL       => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%Adjacent_Cell(1:5) 
        !pNAdjCell      => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%NAdjCell       
        pIsMain(1:9) => BRICK_LIST(NIN,IB)%POLY(1:9)   
        pMainID      => BRICK_LIST(NIN,IB)%MainID 
        MCELL          =  pMainID        
        ICELL          =  0
        BRICK_LIST(NIN,IB)%POLY(1:9)%WhereIsMain(1) = 0 
        BRICK_LIST(NIN,IB)%POLY(1:9)%WhereIsMain(2) = 0 
        BRICK_LIST(NIN,IB)%POLY(1:9)%WhereIsMain(3) = 0 
        BRICK_LIST(NIN,IB)%POLY(1:9)%WhereIsMain(4) = 0 
        IF(NCELL == 0) THEN
          BRICK_LIST(NIN,IB)%POLY(MCELL)%WhereIsMain(3)   = IE        
          BRICK_LIST(NIN,IB)%POLY(MCELL)%WhereIsMain(4)   = IB
          CYCLE
        ENDIF
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9          
          !--------------------------------------!        
          !if cell needs to be linked to a main one
          IF(ICELL == MCELL)THEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1:2) = 0           
            BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(3)   = IE
            BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(4)   = IB            
          ELSE
            adjMAIN_face(1:6)         = ZERO 
            adjMAIN_centroid(1:3,1:6) = ZERO           
            IDadj_MCELLv(1:6)           = 0                      
            IVadj_MCELLv(1:6)           = 0                                  
            IBadj_MCELLv(1:6)           = 0  
            !Loop on face to list adjacent main cells
            IF(SUM(BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%NAdjCell) == 0)THEN
              print *, "**error : inter22 - Cell trapped. Check Lagrangian Surface surrounding BRICK ID:",
     .                 IXS(11,BRICK_LIST(NIN,IB)%ID) ; stop 2206
              CYCLE 
            ENDIF
            DO J=1,NV46
              NAdjCell                  = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell
              IF(NAdjCell == 0)CYCLE
              IV                        = pAdjBRICK(J,1)
              IBV                       = pAdjBRICK(J,4)
              IF(IBV==0)THEN
                adjMAIN_face(J)       = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Surf
                IDadj_MCELLv(J)         = 1 
                IVadj_MCELLv(J)         = IV 
                IBadj_MCELLv(J)         = 0               
                adjMAIN_centroid(1,J) = SUM(X(1,IXS(2:9,IV))) / EIGHT          
                adjMAIN_centroid(2,J) = SUM(X(2,IXS(2:9,IV))) / EIGHT          
                adjMAIN_centroid(3,J) = SUM(X(3,IXS(2:9,IV))) / EIGHT                                          
              ELSE
                DO K=1,NAdjCell
                  ICELLv = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(K)
                  JV     = pAdjBRICK(J,5)
                  IF(BRICK_LIST(NIN,IBV)%POLY(ICELLv)%IsMain   == 1)THEN
                    adjMAIN_face(J)         = MIN (BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Surf, 
     .                                               BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(JV)%Surf)
                    adjMAIN_centroid(1:3,J) = BRICK_LIST(NIN,IBV)%POLY(ICELLv)%CellCenter(1:3)
                    IDadj_MCELLv(J)           = ICELLv
                    IVadj_MCELLv(J)           = IV 
                    IBadj_MCELLv(J)           = IBV                   
                    EXIT !no more main cell out this face (only one)
                  ENDIF
                ENDDO!next K
              ENDIF
            ENDDO
            sumFACE = SUM(adjMAIN_face(1:6))
            IF(sumFACE==ZERO)THEN
c              print *, "**Warning inter22 : no main cell to link with cell from BRICK ID:",IXS(11,BRICK_LIST(NIN,IB)%ID)
              N_UNLINKED_L(ITASK)                 = N_UNLINKED_L(ITASK) + 1
              UNLINKED_CELLS_L(ITASK,1,N_UNLINKED_L(ITASK)) = IB
              UNLINKED_CELLS_L(ITASK,2,N_UNLINKED_L(ITASK)) = ICELL 
              CYCLE !number of adjacent cell on given face J : no adjacent cell on face J means no need to search out there             
            ENDIF
!           adjMAIN_face(1:6) = adjMAIN_face(1:6) / sumFACE
            !IPOS = LINK_WITH_UNIQUE_MAIN_CELL(adjMAIN_face, adjMAIN_centroid)
            IPOS = MAXLOC(adjMAIN_face(1:6),1)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1) = IPOS 
            BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(2) = IDadj_MCELLv(IPOS)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(3) = IVadj_MCELLv(IPOS)                           
            BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(4) = IBadj_MCELLv(IPOS)          
          ENDIF       
        ENDDO!next ICELL
      ENDDO     
      !-----------------------------------------------------------!
      CALL MY_BARRIER()                                           !
      !-----------------------------------------------------------!
      if((ibug22_UndirectLink==1.OR.ibug22_UndirectLink==-1) .AND. N_UNLINKED_L(ITASK)>0)
     .  print *, "  **SINIT** : UNLINKED CELL SYNTHESIS  "
      DO I=1,N_UNLINKED_L(ITASK)
        IB    = UNLINKED_CELLS_L(ITASK,1,I)
        ICELL = UNLINKED_CELLS_L(ITASK,2,I)        
        if(ibug22_UndirectLink==1.OR.ibug22_UndirectLink==-1)write(*,FMT='(I10,A1,I1)') IXS(11,BRICK_LIST(NIN,IB)%ID),".",ICELL
      ENDDO
      if(ibug22_UndirectLink==1 .AND. N_UNLINKED_L(ITASK)>0)print *, ""
      
 
      !------------------------------------------------------------!
      ! @20. LIST ALL SECONDARY CELLS LINKED TO A GIVEN main CELL  !
      !------------------------------------------------------------!
      DO IB=NBF,NBL
        NBCUT         =  BRICK_LIST(NIN,IB)%NBCUT
        MCELL         =  BRICK_LIST(NIN,IB)%MainID
        IF(MCELL == 0) CYCLE
        !pNAdjCELL     => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%NAdjCell
        IE            =  BRICK_LIST(NIN,IB)%ID        
        NsecndNOD     =  0
        NumSECND      =  0
        BRICK_LIST(NIN,IB)%SecndList%Num = 0      
        BRICK_LIST(NIN,IB)%SecndList%VOL_Unmerged = BRICK_LIST(NIN,IB)%POLY(MCELL)%Vnew
        DO J=1,NV46
          Iv     = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1) 
          NGv    = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,2) 
          IDLOCv = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,3)             
          IBv    = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4) !necessarily different from zero because a cut cell (NBCUT>0) has all its neighbor in cut cell buffer (TZINF increased)
          IFv    = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)        
          NADJ   = BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%NAdjCell
          IF(IBv==0)CYCLE
          DO IADJ = 1,NADJ
            ICELLv    =  BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%Adjacent_Cell(IADJ)
            IE_M      =  BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(3)
            IF(IE_M/=IE)CYCLE
            !secnd cell IBV.ICELLv is added into the list since it is linked to IB.MCELL
            NumSECND = NumSECND + 1
            NsecndNOD   = NsecndNOD + BRICK_LIST(NIN,IBv)%POLY(ICELLv)%NumNOD
            BRICK_LIST(NIN,IB)%SecndList%FM(NumSecnd)            = J
            BRICK_LIST(NIN,IB)%SecndList%FV(NumSecnd)            = IFV
            BRICK_LIST(NIN,IB)%SecndList%IV(NumSecnd)            = IV
            BRICK_LIST(NIN,IB)%SecndList%IBV(NumSecnd)           = IBV
            BRICK_LIST(NIN,IB)%SecndList%ICELLv(NumSecnd)        = ICELLv                               
            BRICK_LIST(NIN,IB)%SecndList%VOL(NumSecnd)           = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%vnew
            BRICK_LIST(NIN,IB)%SecndList%NumNOD_cell(NumSecnd)   = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%NumNOD                             
            BRICK_LIST(NIN,IB)%SecndList%ListNodID(NumSecnd,1:8) = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%ListNodID(1:8)
            BRICK_LIST(NIN,IB)%SecndList%SURF_v(NumSecnd)        = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(IFv)%SURF                                                  
          ENDDO!next IADJ
          BRICK_LIST(NIN,IB)%SecndList%Num = NumSECND        
        ENDDO!next J
        BRICK_LIST(NIN,IB)%SecndList%NumSecndNodes      = NsecndNOD         
      ENDDO
      
      !------------------------------------------------------------!
      ! @21. INDIRECT LINKING                                      !
      !      TAG RELATED CELL AND STORE ITS INDIRECT LINK          !
      !------------------------------------------------------------!
      !CREER LES LIAISON DANS LE BUFFER CUT CELL
      DO IUNLINK=1,N_UNLINKED_L(ITASK)
        IB              =  UNLINKED_CELLS_L(ITASK,1,IUNLINK)
        ICELL           =  UNLINKED_CELLS_L(ITASK,2,IUNLINK)
        AdjFACE(:,:)    =  ZERO   
        IPOs            =  0   
        DO J=1,NV46         
          NAdj          =  BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell
          IBV           =  BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4) 
             
          DO IADJ=1,NAdj
            JV                = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)
            ICELLv            = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(IADJ)          
            IPOS              = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(1)   
            IF(IPOS==0)CYCLE    
            IF(IBV/=0)THEN     
              AdjFACE(J,IADJ) = MIN (BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Surf, 
     .                               BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Surf)
            ELSE
              AdjFACE(J,IADJ) = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Surf
            ENDIF
          ENDDO
        ENDDO
        IRES(1:2) = MAXLOC(AdjFACE)
        IPOSf     = IRES(1) !face number
        IPOSiadj  = IRES(2) !adjacent cell number
        IBV       = BRICK_LIST(NIN,IB)%Adjacent_Brick(IPOSf,4)
        ICELLv    = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(IPOSf)%Adjacent_Cell(IPOSiadj) 
        IPOS      = 0
        IF(ICELLv/=0)
     .  IPOS      = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(1) 
        IF(IPOS==0 .OR. AdjFACE(IRES(1),IRES(2))==ZERO)THEN
          print *, "***error : inter22 unable to treat cell ",IXS(11,BRICK_LIST(NIN,IB)%ID)
          print *, "           Cell seems trapped with no adjacent cells"
          stop 2207
        ENDIF
        IF(IPOS>=10)THEN
          print *, "***error : inter22 unable to treat cell ",IXS(11,BRICK_LIST(NIN,IB)%ID)
          print *, "           Cell seems trapped with no adjacent cells"
          print *, "           Fluid mesh in interaction area should be refined"
          stop 2208
        ENDIF
        J         = IPOSf*10 + IPOS
        BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1) = J 
        BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(2) = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(2) 
        BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(3) = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(3)
        BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(4) = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(4)       
        IF((ibug22_UndirectLink==1.OR.ibug22_UndirectLink==-1) .AND. N_UNLINKED_L(ITASK)>0)THEN
          write(*,FMT='(A,I10,A1,I1,A,I2,I2,A1,I2,A2)') "unlinked cell:", IXS(11,BRICK_LIST(NIN,IB)%ID),
     .    ".",ICELL, " is now linked to faces ", IPOSf, IPOS,"(",J," )"
        ENDIF
      ENDDO  

      !------------------------------------------------------------!
      ! @22. INDIRECT LINKING                                      !
      !      UPDATE main CELL BUFFER                               !
      !------------------------------------------------------------!
      !need to wait all thread finishing to link before updating their supercell volume and their buffer
      !-------------------!
        CALL MY_BARRIER()
      !-------------------!
  
      !MERGE DES VOLUMES ET MISE A JOUR BUFFER main (main secnd peut etre sur un autre thread : traitement OpenMP)
      DO IT=0,NTHREAD-1
        DO IUNLINK=1,N_UNLINKED_L(IT)
          IB     = UNLINKED_CELLS_L(IT,1,IUNLINK)
          ICELL  = UNLINKED_CELLS_L(IT,2,IUNLINK)  
          IBM    = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(4)        
          ITASKB = BRICK_LIST(NIN,IBM)%ITASK
          
!          write (*,FMT='(A,I5,A,I5,I5,A,I5)') 
!     .    "traitement ib=", ib, "itask,itaskb=", itask, itaskb,"   N_UNLINKED_L(IT)",N_UNLINKED_L(IT)
      IF(ITASKB/=ITASK)CYCLE
          !Link Unlinked secnd cell to main IB,ICELL
          VolCELL   = BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew
          ID        = BRICK_LIST(NIN,IB)%ID
          J         = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1)
          J1        = J/10
          J2        = MOD(J,10)
          IBV       = BRICK_LIST(NIN,IB)%Adjacent_Brick(J1,4) !cell directly linked to a main
          IF(IBv==0)THEN
            print *, "inter22 :Error lagrangian surface is escaping eulerian domain."
            !with CALL ARRET() : truss might be with len=0 => time step issue.
            stop
          ENDIF          
          FM                                                    = BRICK_LIST(NIN,IBV)%Adjacent_Brick(J2,5)                   
          NumSECND                                              = BRICK_LIST(NIN,IBM)%SecndList%Num                          
          NsecndNOD                                             = BRICK_LIST(NIN,IBM)%SecndList%NumSecndNodes                
          NumSECND                                              = NumSECND + 1                                               
          NsecndNOD                                             = NsecndNOD + BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD 
          BRICK_LIST(NIN,IBM)%SecndList%Num                     = NumSECND
          BRICK_LIST(NIN,IBM)%SecndList%NumSecndNodes           = NsecndNOD
          BRICK_LIST(NIN,IBM)%SecndList%FM(NumSECND)            = FM  
          BRICK_LIST(NIN,IBM)%SecndList%FV(NumSECND)            = J !J>10
          BRICK_LIST(NIN,IBM)%SecndList%IV(NumSECND)            = ID
          BRICK_LIST(NIN,IBM)%SecndList%IBV(NumSECND)           = IB
          BRICK_LIST(NIN,IBM)%SecndList%ICELLv(NumSECND)        = ICELL
          BRICK_LIST(NIN,IBM)%SecndList%VOL(NumSECND)           = VOLcell   
          BRICK_LIST(NIN,IBM)%SecndList%NumNOD_cell(NumSECND)   = BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD
          BRICK_LIST(NIN,IBM)%SecndList%ListNodID(NumSECND,1:8) = BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:8)  
          BRICK_LIST(NIN,IBM)%SecndList%SURF_v(NumSecnd)        = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J1)%SURF
        ENDDO!next IUNLINK
      ENDDO!next IT      

      !augmenter le volume main 
      !done later


      !------------------------------------------------------------!
      ! @23. TAG SECND CELLS WHICH ARE BASED ON FORMER main NODE   !
      !------------------------------------------------------------!
!      TARGET_DATA(:,:,:) = 0 !tag main cells linked to these secnd ones.
!      Ntarget(NBF:NBL)   = 0
      DO IB=NBF,NBL
        BRICK_LIST(NIN,IB)%Ntarget = 0
        NBCUT                      =  BRICK_LIST(NIN,IB)%NBCUT
        NCELL                      =  NBCUT
        ICELL                      =  0
        pNodWasMain(1:8)         => BRICK_LIST(NIN,IB)%NODE(1:8)!%NodWasMain
         pListNodID(1)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)
         pListNodID(2)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(2)%ListNodID(1:8)
         pListNodID(3)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(3)%ListNodID(1:8)
         pListNodID(4)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(4)%ListNodID(1:8)
         pListNodID(5)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(5)%ListNodID(1:8)
         pListNodID(6)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(6)%ListNodID(1:8)
         pListNodID(7)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(7)%ListNodID(1:8)
         pListNodID(8)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(8)%ListNodID(1:8)
         pListNodID(9)%p(1:8)      => BRICK_LIST(NIN,IB)%POLY(9)%ListNodID(1:8)
        MCELL                      =  BRICK_LIST(NIN,IB)%MainID
        NTAG                       =  0   
        MAIN_TAG(:,:)            =  0
        
        BRICK_LIST(NIN,IB)%MergeTarget(1:3,1:5) = 0
        
        INod_W = BRICK_LIST(NIN,IB)%OldMainStrongNode
        IF( Inod_W*DT1 > ZERO)THEN
          IF (BRICK_LIST(NIN,IB)%NODE(INOD_W)%WhichCell==MCELL)NCELL = -1
        ENDIF
        !Finding secnd cell inside current brick element, which are based on former main cell nodes
        !------LOOP--------!
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9          
          IF(ICELL==MCELL)CYCLE
          MNOD                    =  BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD
          CALL weighting_Cell_Nodes(NIN,IB,ICELL,INod_W, IDEMERGE)
          IF(SUM(pNodWasMain(pListNodID(ICELL)%p(1:MNOD))%NodWasMain)==0)CYCLE  !if none of the polyhedra nodes is based on a former main cell node : nothing to do
          IF(pNodWasMain(INod_W)%NodWasMain<=0) CYCLE                          !(negatif en debug si IB nouveau dans le buffer)
          !ICELL has at least one node from the previous main cell
          ! lets get its main cell
          
          !Continuity Criteria To avoid unexpected merging.
          !Consistent only with kinematic time step "several cycle to fully cross an element".
          VOL_S    = BRICK_LIST(NIN,IB)%POLY(ICELL)%vnew
          UncutVOL = BRICK_LIST(NIN,IB)%UncutVol
          VAR      = VOL_S /  (UncutVOL)
          IF (VAR < CritDVol22)CYCLE
          INod_W = BRICK_LIST(NIN,IB)%OldMainStrongNode
          IF(INod_W==0)CYCLE !nothing to merge if no main or no cut cell in previous cycle.
          
          IPOS                    = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1)
          ICV                     = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(2)
          IBM                     = BRICK_LISt(NIN,IB)%POLY(ICELL)%WhereIsMain(4)
          NTAG                    = NTAG + 1  !number of secnd cells wet by former main cell
c          TARGET_DATA(IB,NTAG,1)  = IPOS      !facette localisant le plus grand main
c          TARGET_DATA(IB,NTAG,2)  = ICV       !numero de cellule de ce main
          BRICK_LIST(NIN,IB)%MergeTarget(1,NTAG) = IPOS
          BRICK_LIST(NIN,IB)%MergeTarget(2,NTAG) = ICV
          BRICK_LIST(NIN,IB)%MergeTarget(3,NTAG) = IBM         
        ENDDO! next ICELL
        !----END-LOOP------! 
c        Ntarget(IB)                = NTAG ! number of secnd cell whose nodes were wet by the former main cell.= number of main to do material deportation (if one main per secnd cell)
        BRICK_LIST(NIN,IB)%NTarget = NTAG
        ! Ntarget always lower or equal to 9 (max number of secnd cell if one main per secnd)
        ! enumeration of the target
        !Warning : what happen if 2 secnd cells have the same main cell. Duplicates are not possible by tagging main target
      ENDDO!next IB

      !------------------------------------------------------------!
      ! @24. VOLUME MERGING BETWEEN SECND CELL & ITS main ONE      !
      !      UNCONDITIONAL                                         !
      !      for law51 old volume need also to be merged           !
      !------------------------------------------------------------!
      !loop on intersected bricks.
      DO IB=NBF,NBL
        NCELL          =  BRICK_LIST(NIN,IB)%NBCUT  
        !pAdjCELL       => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%Adjacent_Cell(1:5)          
        !pNAdjCELL      => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%NAdjCell                  
        pAdjBRICK      => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5) 
        pSUBVOL(1:9)   => BRICK_LIST(NIN,IB)%POLY(1:9)
        MBUF           => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1)                         
        MCELL          =  BRICK_LIST(NIN,IB)%MainID
        ID             =  BRICK_LIST(NIN,IB)%ID
        NG             =  BRICK_LIST(NIN,IB)%NG
        IDLOC          =  BRICK_LIST(NIN,IB)%IDLOC       
        ICELL          =  0
        BRICK_LIST(NIN,IB)%Vnew_SCell = BRICK_LIST(NIN,IB)%POLY(MCELL)%Vnew     
        !------LOOP--------!
        NumSECND = BRICK_LIST(NIN,IB)%SecndList%Num
        DO IC=1,NumSECND
          ICELLv = BRICK_LIST(NIN,IB)%SecndList%ICELLv(IC)  
          IBv    = BRICK_LIST(NIN,IB)%SecndList%IBv(IC)     
          NGv    = BRICK_LIST(NIN,IBv)%NG                   
          IDLOCv = BRICK_LIST(NIN,IBv)%IDLOC                
          IV     = BRICK_LIST(NIN,IBv)%ID                   
          !---MONO-MATERIAL---!
          pSUBVOLv(1:9)                 => BRICK_LIST(NIN,IBV)%POLY(1:9)
          VOLv                          =  pSUBVOLv(ICELLv)%Vnew
          !pSUBVOL(MCELL)                =  pSUBVOL(MCELL) + VOLv !no longer cumulated here
          BRICK_LIST(NIN,IB)%Vnew_SCell = BRICK_LIST(NIN,IB)%Vnew_SCell + VOLv
        ENDDO!next IC
        !initialisation de V1OLD
        IF(DT1==ZERO)THEN
        !---MULTI-MATERIAL----!                                                                                                         
        MTN_ = IPARG(1,NGB(IB))                                                                                                         
        IF(MTN_==51)THEN                                                                                                              
          LLT_  = IPARG(2,NGB(IB))                                                                                                      
          !in sigesp51.F:                                                                                                               
          !V1OLD = V1OLD - TIMESTEP*DDVOL1                                                                                              
          !Volume change correspond to DDVOL=DV/DT = f(fluxes). Gird deformation influences relative material velocity ans also fluxes  
          DO ITRIMAT=1,TRIMAT                                                                                                           
            IPOS                   = 1                                                                                                  
            K1                     = N0PHAS + (ITRIMAT-1)*NVPHAS +IPOS-1   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}         
            K                      = K1 * LLT_                                                                                          
            VFRAC(ITRIMAT)         = MBUF%VAR(K+IDLOCB(IB))        !backup for material merging                                         
            IPOS                   = 11                                                                                                 
            K1                     = N0PHAS + (ITRIMAT-1)*NVPHAS +IPOS-1   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}         
            K                      = K1 * LLT_                                                                                          
            VAR                    = BRICK_LIST(NIN,IB)%Vnew_SCell*VFRAC(itrimat)                                                       
            MBUF%VAR(K+IDLOCB(IB)) = VAR                                                                                                
          ENDDO!next ITRIMAT                                                                                                            
        ENDIF!(MTN_==51)
        ENDIF!TIME==ZERO                                                                                                              
      ENDDO!next IB
      
      
      !------------------------------------------------------------!
      ! @25. MATERIAL MERGING (IF main HAS TO BE DEPORTED)       !
      !------------------------------------------------------------! 
      ! if Ntarget > 0 then material deporation has to be done from former main cell buffer (GBUF)
      ! to the N=Ntarget target main cells.
      NBL1 = NBL
      IF(DT1==ZERO)NBL1 = 0
      if(IBUG22_merge/=0)then
        if(itask==0)allocate(TMP22ARRAY(7,NB))
        call my_barrier
        TMP22ARRAY(1:7,NBF:NBL1)=ZERO
      endif
      DO IB=NBF,NBL1
        NBCUT            =  BRICK_LIST(NIN,IB)%NBCUT        
        NCELL            =  NBCUT
        ICELL            =  0 
        GBUF             => ELBUF_TAB(NGB(IB))%GBUF
        !pAdjCELL         => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%Adjacent_Cell(1:5)          
        pAdjBRICK        => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5)
        MBUF             => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1)
        IDLOC            =  BRICK_LIST(NIN,IB)%IDLOC                                 
        VOL              =  ZERO
        VOLv             =  ZERO
        NTAR             =  BRICK_LIST(NIN,IB)%Ntarget
        LLT_             =  IPARG(2,NGB(IB))
        !------LOOP--------!
        !loop on a main target to distribute former main data
        DO ITAR = 1,NTAR
          J               =  BRICK_LIST(NIN,IB)%MergeTarget(1,ITAR)
          ICV             =  BRICK_LIST(NIN,IB)%MergeTarget(2,ITAR) 
          IF(J>=10)THEN
            J1     = J/10
            J2     = MOD(J,10)
            IBv_i  = BRICK_LIST(NIN,IB   )%Adjacent_Brick(J1,4)
            NGv    = BRICK_LIST(NIN,IBv_i)%Adjacent_Brick(J2,2)
            IDLOCv = BRICK_LIST(NIN,IBv_i)%Adjacent_Brick(J2,3)              
            IBv    = BRICK_LISt(NIN,IBv_i)%Adjacent_Brick(J2,4)
          ELSE
            NGv    = BRICK_LIST(NIN,IB )%Adjacent_Brick(J,2)
            IDLOCv = BRICK_LIST(NIN,IB )%Adjacent_Brick(J,3)
            IBv    = BRICK_LIST(NIN,IB )%Adjacent_Brick(J,4)        !TARGET CELL DATA IS STORED IN EINT_L(...,IBv), RHOL, UVARL...     
          ENDIF          
          GBUFv           => ELBUF_TAB(NGv)%GBUF  
          RATIO           =  ONE/Ntar !in the loop but avoid conditional tests "IF(Ntar>0)" for NBF:NBL if not applicable 
          ITASKv          =  BRICK_LIST(NIN,IBv)%ITASK
          IMERGEL(ITASKv) = 1
          BRICK_LIST(NIN,IBv)%IsMergeTarget = 1
          !print *, "set isMergetarget ", ixs(11,brick_list(nin,ibv)%id)
          !---debug---!  
          Cod1=ixs(11,brick_list(nin,ib)%id)
          Cod2=ixs(11,brick_list(nin,ibv)%id)        
          if (IBUG22_merge==-1)then
              !!!write(*,FMT='(A,I8,A,I8)')      "  MERGING : id=", Cod1, "   to idv:", Cod2    
              TMP22ARRAY(1,IB)=Cod1 
              TMP22ARRAY(2,IB)=Cod2       
          endif
          !-----------!
          !---------------------!  
          !  MATERIAL MERGING   !  !only if main cell loose all its nodes, main (which became secnd) contents is sent to the adjacent main.
          !---------------------!
          ! example:
          !  EINTv : target main on which secnd cell is linked
          !  EINT  : old main cell buffer
          !-----------------------MONO-MATERIAL------------------------! 
          VOL                         =  BRICK_LIST(NIN,IB)%Vold_SCell
          SuperCellVOL_L(ITASK,0,IBv) =  SuperCellVOL_L(ITASK,0,IBv) + RATIO * VOL
          !------ENERGIE--------! 
          EINT                        = RATIO * VOL * BRICK_LIST(NIN,IB)%bakMAT%rhoE
          EINT_L(ITASK,IBv)           = EINT_L(ITASK,IBv) + EINT
          !------MASS----------!      
          if (IBUG22_merge==-1)then
              !!!write(*,FMT='(A,E30.16)')    "         RATIO=", RATIO
              !!!write(*,FMT='(A,E30.16)')    "          +rho=", GBUF%RHO(IDLOCB(IB))
              !!!write(*,FMT='(A,E30.16)')    "        +rhoUx=",GBUF%MOM(LLT_*(1-1) + IDLOCB(IB))            
              !!!write(*,FMT='(A,E30.16)')    "          +Vol=", VOL   
              TMP22ARRAY(3,IB)=RATIO 
              TMP22ARRAY(4,IB)=GBUF%RHO(IDLOCB(IB)) 
              TMP22ARRAY(5,IB)=GBUF%MOM(LLT_*(1-1) + IDLOCB(IB)) 
              TMP22ARRAY(6,IB)=VOL                            
          endif          
          RHO                         = RATIO * VOL * BRICK_LIST(NIN,IB)%bakMAT%RHO
          RHO_L(ITASK,IBv)            = RHO_L(ITASK,IBv) + RHO
          !------TENSOR--------! 
          DO J=1,NV46
            SIG(J)                    = RATIO * VOL * BRICK_LIST(NIN,IB)%bakMAT%SIG(J)
            SIG_L(ITASK,J,IBv)        = SIG_L(ITASK,J,IBv) + SIG(J)
          ENDDO
          !----MOMENTUM--------!
          !momentum is (rho.Ux, rho.Uy, rho.Uz)
          MOM(1)                      = RATIO * VOL * BRICK_LIST(NIN,IB)%bakMAT%rhoU(1)
          MOM_L(1,ITASK,IBv)          = MOM_L(1,ITASK,IBv) + MOM(1)          
          MOM(2)                      = RATIO * VOL * BRICK_LIST(NIN,IB)%bakMAT%rhoU(2)
          MOM_L(2,ITASK,IBv)          = MOM_L(2,ITASK,IBv) + MOM(2)
          MOM(3)                      = RATIO * VOL * BRICK_LIST(NIN,IB)%bakMAT%rhoU(3)
          MOM_L(3,ITASK,IBv)          = MOM_L(3,ITASK,IBv) + MOM(3)                    
          !-----------------------MULTI-MATERIAL-----------------------! 
          MTN_ = IPARG(1,NGB(IB))  
          IF(MTN_==37)THEN
            !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multiplying by element volume in aleconve.F)
            !UVAR(I,2) : density of gas
            !UVAR(I,3) : density of liquid
            !UVAR(I,4) : volumetric fraction of liquid  
            !UVAR(I,5) : volumetric fraction of gas
            LLT_               = IPARG(2,NGB(IB))
c            print *, "merge-adding ratio uvar5="  , BRICK_LIST(NIN,IB)%bakMAT%UVAR(5) 
c            print *, "merge-adding ratio uvar4="  , BRICK_LIST(NIN,IB)%bakMAT%UVAR(4) 
c            print *, "merge-adding ratio uvar3="  , BRICK_LIST(NIN,IB)%bakMAT%UVAR(3) 
c            print *, "merge-adding ratio uvar2="  , BRICK_LIST(NIN,IB)%bakMAT%UVAR(2) 
c            print *, "merge-adding ratio uvar1="  , BRICK_LIST(NIN,IB)%bakMAT%UVAR(1)                                                 
      
            UVARL(ITASK,IBv,5) = UVARL(ITASK,IBv,5)+RATIO*VOL* BRICK_LIST(NIN,IB)%bakMAT%UVAR(5) 
            UVARL(ITASK,IBv,4) = UVARL(ITASK,IBv,4)+RATIO*VOL* BRICK_LIST(NIN,IB)%bakMAT%UVAR(4) 
            UVARL(ITASK,IBv,3) = UVARL(ITASK,IBv,3)+RATIO*VOL* BRICK_LIST(NIN,IB)%bakMAT%UVAR(3)*BRICK_LIST(NIN,IB)%bakMAT%UVAR(4)
            UVARL(ITASK,IBv,2) = UVARL(ITASK,IBv,2)+RATIO*VOL* BRICK_LIST(NIN,IB)%bakMAT%UVAR(2)*BRICK_LIST(NIN,IB)%bakMAT%UVAR(5)            
            UVARL(ITASK,IBv,1) = UVARL(ITASK,IBv,1)+RATIO*VOL* BRICK_LIST(NIN,IB)%bakMAT%UVAR(1)
            SuperCellVOL_L(ITASK,1,IBv) =  SuperCellVOL_L(ITASK,1,IBv) + UVARL(ITASK,IBv,4)
            SuperCellVOL_L(ITASK,2,IBv) =  SuperCellVOL_L(ITASK,2,IBv) + UVARL(ITASK,IBv,5)    
            
          ELSEIF(MTN_==51)THEN
            LLT_  = IPARG(2,NGB(IB))
            !in sigesp51.F:
            !V1OLD = V1OLD - TIMESTEP*DDVOL1            
            !Volume change correspond to DDVOL=DV/DT = f(fluxes). Gird deformation influences relative material velocity ans also fluxes
            DO ITRIMAT=1,TRIMAT
              IPOS               = 11
              K                  = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS)     ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}                                             
              VOL51(IB,ITRIMAT)  = BRICK_LIST(NIN,IB)%bakMAT%UVAR(K)         !backup for material merging                                                    
              VAR                = RATIO * VOL51(IB,ITRIMAT)
              IF(VAR/=ZERO) SuperCellVOL_L(ITASK,ITRIMAT,IBv) = SuperCellVOL_L(ITASK,ITRIMAT,IBv) + VAR
            ENDDO!next ITRIMAT 
            UVARL(ITASK,IBv,1) =  UVARL(ITASK,IBv,1) + BRICK_LIST(NIN,IB)%bakMAT%UVAR(1)
            UVARL(ITASK,IBv,2) =  UVARL(ITASK,IBv,2) + BRICK_LIST(NIN,IB)%bakMAT%UVAR(2)
            UVARL(ITASK,IBv,3) =  UVARL(ITASK,IBv,3) + BRICK_LIST(NIN,IB)%bakMAT%UVAR(3)  
c            print *, "adding 1 "   , BRICK_LIST(NIN,IB)%bakMAT%UVAR(1), UVARL(ITASK,IBv,1) 
c            print *, "adding 2 "   , BRICK_LIST(NIN,IB)%bakMAT%UVAR(2), UVARL(ITASK,IBv,2) 
c            print *, "adding 3 "   , BRICK_LIST(NIN,IB)%bakMAT%UVAR(3), UVARL(ITASK,IBv,3)                   
            DO ITRIMAT=1,TRIMAT
              VAR =  RATIO*VOL51(IB,ITRIMAT)
              IF(VAR==ZERO)CYCLE
              DO IPOS = 1,NVPHAS   
                IF(IPOS==11)CYCLE                                                !volume already treated merged during geomeetrical merging.    
                K                     =  ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                pVAR                  => BRICK_LIST(NIN,IB)%bakMAT%UVAR(K)                                                  
                UVARL(ITASK,IBv,K)    =  UVARL(ITASK,IBv,K) + pVAR *  VAR  
c                 print *, "adding j "   , BRICK_LIST(NIN,IB)%bakMAT%UVAR(K) ,VAR, UVARL(ITASK,IBv,K)             
              ENDDO!next IPOS 
           ENDDO!next ITRIMAT 
          ENDIF!(MTN_==51)         
          !------------------------------------------------------------! 
        ENDDO! next ITAR
        !----END-LOOP------! 
      ENDDO!next IB

      !--------------!
      CALL MY_BARRIER
      !--------------! 
 
 
       if(IBUG22_merge/=0 .and.DT1>ZERO)then
        if(itask==0)then
          do ib=1,nb
            cod1=NINT(TMP22ARRAY(1,IB))
            cod2=NINT(TMP22ARRAY(2,IB))
            if(cod1<=0)cycle
            write(*,FMT='(A,I8,A,I8)')   "  MERGING : id=", Cod1, "   to idv:", Cod2   
            write(*,FMT='(A,E30.16)')    "         RATIO=", TMP22ARRAY(3,IB)
            write(*,FMT='(A,E30.16)')    "          +rho=", TMP22ARRAY(4,IB)
            write(*,FMT='(A,E30.16)')    "        +rhoUx=", TMP22ARRAY(5,IB)           
            write(*,FMT='(A,E30.16)')    "          +Vol=", TMP22ARRAY(6,IB)
            TMP22ARRAY(:,IB)  = ZERO  
          enddo
        endif
        call my_barrier !otherwise TMP22ARRAY IS MODIFIED
      endif
 
      
      !OPENMP STACKING  
      
!      IF(IMERGEL(ITASK)==0)THEN 
!        NBL1=0
!      ELSE
!        IF(TRIMAT<0)THEN
!          ALLOCATE(UVAR (I22LAW37))
!        ELSE
!          ALLOCATE(UVAR (N0PHAS+TRIMAT*NVPHAS))
!        ENDIF
!      ENDIF

      DO IB=NBF,NBL1
        NBCUT                            =  BRICK_LIST(NIN,IB)%NBCUT        
        NCELL                            =  NBCUT
        ICELL                            =  0 
        GBUF                             => ELBUF_TAB(NGB(IB))%GBUF
        LBUF                             => ELBUF_TAB(NGB(IB))%BUFLY(1)%LBUF(1,1,1) 
        !pAdjCELL                         => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%Adjacent_Cell(1:5) 
        pAdjBRICK                        => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5)
        MBUF                             => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1) 
        MLW                              =  BRICK_LIST(NIN,IB)%MLW
        SOM                              =  SUM(SuperCellVOL_L(0:NTHREAD-1,0,IB))
        LLT_                             =  IPARG(2,NGB(IB))
        IF(MLW==37 .OR. MLW==51)THEN            
          SOM_(1)                        =  SUM(SuperCellVOL_L(0:NTHREAD-1,1,IB))
          SOM_(2)                        =  SUM(SuperCellVOL_L(0:NTHREAD-1,2,IB))
          SOM_(3)                        =  SUM(SuperCellVOL_L(0:NTHREAD-1,3,IB))
          SOM_(4)                        =  SUM(SuperCellVOL_L(0:NTHREAD-1,4,IB)) 
        ENDIF                       
        IF(SOM==ZERO)CYCLE
        NG                               =  BRICK_LIST(NIN,IB)%NG
        IDLOC                            =  BRICK_LIST(NIN,IB)%IDLOC
        brickID                          =  BRICK_LIST(NIN,IB)%ID

        IF(BRICK_LIST(NIN,IB)%Ntarget>0)THEN
          DELTA                      =  ZERO
        ELSE
          DELTA                      =  ONE      
        ENDIF

          if (IBUG22_merge==-1)then
              !!!write(*,FMT='(A,E30.16)')    "        TARGET=", IXS(11,brick_list(nin,ib)%id)
              !!!write(*,FMT='(A,E30.16)')    "         DELTA=", DELTA
              !!!write(*,FMT='(A,E30.16)')    "           rho=", BRICK_LIST(NIN,IB)%bakMAT%RHO
              !!!write(*,FMT='(A,3E30.16)')    "        +rhoUx=", BRICK_LIST(NIN,IB)%bakMAT%rhoU(1:3)            
              !!!write(*,FMT='(A,E30.16)')    "          +Vol=", BRICK_LIST(NIN,IB)%Vold_SCell   
              TMP22ARRAY(1,IB)   = IXS(11,brick_list(nin,ib)%id)     
              TMP22ARRAY(2,IB)   = DELTA
              TMP22ARRAY(3,IB)   = BRICK_LIST(NIN,IB)%bakMAT%RHO
              TMP22ARRAY(4:6,IB) = BRICK_LIST(NIN,IB)%bakMAT%rhoU(1:3) 
              TMP22ARRAY(7,IB)   = BRICK_LIST(NIN,IB)%Vold_SCell      
          endif 
        
        SOM                              = SOM + DELTA * BRICK_LIST(NIN,IB)%Vold_SCell          
        SuperCellVOL_L(0:NTHREAD-1,0,IB) = ZERO
                
        EINT                   = DELTA*BRICK_LIST(NIN,IB)%bakMAT%rhoE*BRICK_LIST(NIN,IB)%Vold_SCell  +SUM(EINT_L(0:NTHREAD-1,IB))                    
        EINT                   = EINT / SOM                                 
        EINT_L(0:NTHREAD-1,IB) = ZERO !to avoid full reinit                 
                       
        RHO                    = DELTA*BRICK_LIST(NIN,IB)%bakMAT%RHO*BRICK_LIST(NIN,IB)%Vold_SCell   +SUM(RHO_L(0:NTHREAD-1,IB))                     
        RHO                    = RHO / SOM                                  
        RHO_L(0:NTHREAD-1,IB)  = ZERO  !to avoid full reinit                    

        !MomX
        MOM(1)                 = DELTA*BRICK_LIST(NIN,IB)%bakMAT%rhoU(1)*BRICK_LIST(NIN,IB)%Vold_SCell+SUM(MOM_L(1,0:NTHREAD-1,IB))           
        MOM(1)                 = MOM(1) / SOM                               
        MOM_L(1,0:NTHREAD-1,IB)= ZERO  !to avoid full reinit                
        !MomY
        MOM(2)                 = DELTA*BRICK_LIST(NIN,IB)%bakMAT%rhoU(2)*BRICK_LIST(NIN,IB)%Vold_SCell+SUM(MOM_L(2,0:NTHREAD-1,IB))           
        MOM(2)                 = MOM(2) / SOM                               
        MOM_L(2,0:NTHREAD-1,IB)= ZERO  !to avoid full reinit                
        !MomZ
        MOM(3)                 = DELTA*BRICK_LIST(NIN,IB)%bakMAT%rhoU(3)*BRICK_LIST(NIN,IB)%Vold_SCell+SUM(MOM_L(3,0:NTHREAD-1,IB))           
        MOM(3)                 = MOM(3) / SOM                               
        MOM_L(3,0:NTHREAD-1,IB)= ZERO  !to avoid full reinit                

        DO J=1,NV46
        SIG(J)                 = DELTA*BRICK_LIST(NIN,IB)%bakMAT%SIG(J)*BRICK_LIST(NIN,IB)%Vold_SCell+SUM(SIG_L(0:NTHREAD-1,J,IB))      
        SIG(J)                 = SIG(J) / SOM                               
        SIG_L(0:NTHREAD-1,J,IB)= ZERO  !to avoid full reinit                
        ENDDO 
        
        GBUF%EINT(IDLOC)             = EINT                                            
        GBUF%RHO(IDLOC)              = RHO    ;         ALEFVM_Buffer%FCELL(4, BRICK_LIST(NIN,IB)%ID ) = RHO                                                    
        GBUF%MOM(LLT_*(1-1) + IDLOC) = MOM(1) ;         ALEFVM_Buffer%FCELL(1, BRICK_LIST(NIN,IB)%ID ) = MOM(1)                                                 
        GBUF%MOM(LLT_*(2-1) + IDLOC) = MOM(2) ;         ALEFVM_Buffer%FCELL(2, BRICK_LIST(NIN,IB)%ID ) = MOM(2)                                                 
        GBUF%MOM(LLT_*(3-1) + IDLOC) = MOM(3) ;         ALEFVM_Buffer%FCELL(3, BRICK_LIST(NIN,IB)%ID ) = MOM(3)           
        !FCELL(5,.)  voir bloc conditionnel ci-dessous
                                                   
        DO J=1,NV46
          GBUF%SIG(LLT_*(J-1)+IDLOC)    = SIG(J)                                     
        ENDDO

         ! Cod1=ixs(11,brick_list(nin,ib)%id)      
         ! if (IBUG22_merge==Cod1 .or. IBUG22_merge==-1)then
         !     write(*,FMT='(A,I8,A,I8)')      "  AFTER MERGING : id=", Cod1, "   is going to be updated" 
         !     write(*,FMT='(A,E30.16)')    "                    rho=", RHO
         !     write(*,FMT='(A,E30.16)')    "                 rho.Ux=", MOM(1)  
         !     print *,"-------------------------"            
         ! endif
        
        MTN_ = IPARG(1,NGB(IB))  
        IF(MTN_==37)THEN
          !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multiplying by element volume in aleconve.F)
          !UVAR(I,2) : density of gas
          !UVAR(I,3) : density of liquid
          !UVAR(I,4) : volumetric fraction of liquid  
          !UVAR(I,5) : volumetric fraction of gas  
          LLT_      =  IPARG(2,NGB(IB))
                   
          SOM_(1)   = SOM_(1) + DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(4)*BRICK_LIST(NIN,IB)%Vold_SCell       
          SOM_(2)   = SOM_(2) + DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(5)*BRICK_LIST(NIN,IB)%Vold_SCell    
          
          
          UVAR(5)   = DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(5)*BRICK_LIST(NIN,IB)%Vold_SCell + SUM(UVARL(0:NTHREAD-1,IB,5))
          UVAR(4)   = DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(4)*BRICK_LIST(NIN,IB)%Vold_SCell + SUM(UVARL(0:NTHREAD-1,IB,4)) 
          UVAR(3)   = DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(3)*BRICK_LIST(NIN,IB)%bakMAT%UVAR(4)*BRICK_LIST(NIN,IB)%Vold_SCell 
          UVAR(3)   = UVAR(3) + SUM(UVARL(0:NTHREAD-1,IB,3))  
          UVAR(2)   = DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(2)*BRICK_LIST(NIN,IB)%bakMAT%UVAR(5)*BRICK_LIST(NIN,IB)%Vold_SCell 
          UVAR(2)   = UVAR(2) + SUM(UVARL(0:NTHREAD-1,IB,2))                 
          UVAR(1)   = DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(1)*BRICK_LIST(NIN,IB)%Vold_SCell + SUM(UVARL(0:NTHREAD-1,IB,1))         
          UVARL(0:NTHREAD-1,IB,1:5) = ZERO
      
c          print *, "merge uvar-5", BRICK_LIST(NIN,IB)%bakMAT%UVAR(5),UVAR(1) / SOM
c          print *, "merge uvar-4", BRICK_LIST(NIN,IB)%bakMAT%UVAR(4),UVAR(2) / SOM
c          print *, "merge uvar-3", BRICK_LIST(NIN,IB)%bakMAT%UVAR(3),UVAR(3) / SOM
c          print *, "merge uvar-2", BRICK_LIST(NIN,IB)%bakMAT%UVAR(2),UVAR(4) / SOM
c          print *, "merge uvar-1", BRICK_LIST(NIN,IB)%bakMAT%UVAR(1),UVAR(5) / SOM                                        
c          print *, ""

          MBUF%VAR((0*LLT_ + IDLOC))           = UVAR(1) / SOM    !SOM=VOL
          IF(SOM_(2)>EM20)THEN
            MBUF%VAR((1*LLT_ + IDLOC))         = UVAR(2) / SOM_(2)  !SOM2 = VOL_G =sum (vol_g , cell)
            MBUF%VAR((4*LLT_ + IDLOC))         = UVAR(5) / SOM                     
          ENDIF
          IF(SOM_(1)>EM20)THEN
            MBUF%VAR((2*LLT_ + IDLOC))         = UVAR(3) / SOM_(1)  !SOM3 = VOL_L  =sum (vol_l , cell)                
            MBUF%VAR((3*LLT_ + IDLOC))         = UVAR(4) / SOM  
          ENDIF
          SuperCellVOL_L(0:NTHREAD-1,1:4,IB) = ZERO 
          
          
          !APPEL SIGEPS37 POUR EQUILIBRE PRESSION
          brickID = BRICK_LIST(NIN,IB)%ID
          IDLOC    = IDLOCB(IB)
          NG       = NGB(IB)
          VOL      = SOM
          CALL SIGEPS37_SINGLE_CELL (
     1                                ELBUF_TAB, IXS,  BUFMAT,  IPARG, IPM,
     2                                IDLOC    , NG ,  brickID, VOL  
     .                                )          

          
                                        
        ELSEIF(MTN_==51)THEN
          LLT_                      =  IPARG(2,NGB(IB))  
          UVAR(1)                   =  DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(1) + SUM(UVARL(0:NTHREAD-1,IB,1))
          UVAR(2)                   =  DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(2) + SUM(UVARL(0:NTHREAD-1,IB,2))
          UVAR(3)                   =  DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(3) + SUM(UVARL(0:NTHREAD-1,IB,3))
          UVARL(0:NTHREAD-1,IB,1:3) = ZERO
          MBUF%VAR((0*LLT_ + IDLOC))= UVAR(1) / SOM
          MBUF%VAR((1*LLT_ + IDLOC))= UVAR(2) / SOM
          MBUF%VAR((2*LLT_ + IDLOC))= UVAR(3) / SOM                    
          DO ITRIMAT=1,TRIMAT
            SOMi                                   =  SUM(SuperCellVOL_L(0:NTHREAD-1,ITRIMAT,IB))
            SuperCellVOL_L(0:NTHREAD-1,ITRIMAT,IB) =  ZERO
            IPOS                                   =  11
            K                                      =  ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS)  
            SOMi                                   =  SOMi + DELTA*BRICK_LIST(NIN,IB)%bakMAT%UVAR(K) 
            VOL51(IB,ITRIMAT)                      =  BRICK_LIST(NIN,IB)%bakMAT%UVAR(K)                 
            UVAR(:)                                =  ZERO
            IF(SOMi>ZERO)THEN
              DO IPOS=1,NVPHAS
                IF(IPOS==11)THEN
                  K1                               =  ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! ici UVAR(1:N0PHAS+TRIMAT*NVPHAS)
                  K                                =  K1 * LLT_                                                  
                  MBUF%VAR(K+IDLOCB(IB))           =  SOMi                                     
                  CYCLE
                ELSEIF(IPOS==1)THEN
                  K1                               =  ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! ici UVAR(1:N0PHAS+TRIMAT*NVPHAS)
                  K                                =  K1 * LLT_                                                  
                  MBUF%VAR(K+IDLOCB(IB))           =  SOMi / SOM                                 
                  CYCLE            
               ENDIF
                K                                  =  ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS)
                K1                                 = (K-1)*LLT_
                pVAR                               => MBUF%VAR(K1+IDLOCB(IB))
                UVAR(K)                            =  SUM(UVARL(0:NTHREAD-1,IB,K)) + pVAR * VOL51(IB,ITRIMAT) * DELTA
                UVARL(0:NTHREAD-1,IB,K)            =  ZERO
                UVAR(K)                            =  UVAR(K) / SOMi 
                IF(IPOS/=1)pVar                    =  UVAR(K)          
              ENDDO!next IPOS
            ELSE
              !vfrac_i=0 Vol_i=0
              IPOS                                 =  1                                         
              K1                                   =  ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)      
              K                                    =  K1 * LLT_                                 
              MBUF%VAR(K+IDLOCB(IB))               =  ZERO                                      
              IPOS                                 =  11                                        
              K1                                   =  ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)     
              K                                    =  K1 * LLT_                                 
              MBUF%VAR(K+IDLOCB(IB))               =  ZERO                                      
            ENDIF!(SOMi>ZERO)
          ENDDO!next ITRIMAT
          
        ENDIF! (MTN_==51) 
        
        BRICK_LIST(NIN,IB)%Vold_Scell= SOM

                     
      ENDDO!next IB      




      if(IBUG22_merge/=0 )then
        call my_barrier
        if(itask==0)then
          if(DT1>ZERO)then
            do ib=1,nb            
                 if (TMP22ARRAY(1,IB)==zero )cycle                                           
                 write(*,FMT='(A,E30.16)')    "        TARGET=", NINT(TMP22ARRAY(1,IB))      
                 write(*,FMT='(A,E30.16)')    "         DELTA=", TMP22ARRAY(2,IB)            
                 write(*,FMT='(A,E30.16)')    "           rho=", TMP22ARRAY(3,IB)            
                 write(*,FMT='(A,3E30.16)')   "        +rhoUx=", TMP22ARRAY(4,IB)            
                 write(*,FMT='(A,E30.16)')    "          +Vol=", TMP22ARRAY(5,IB)            
            enddo                 
          endif!if(DT1>ZERO)
          !print *, "deallocate"
          deallocate(TMP22ARRAY)
        endif
      endif
      
      !------------------------------------------------------------!
      ! @26. DEMERGING CRITERIA                                    !
      !         (IF main HAS TO BE DEPORTED THEN                 !
      !         DEMERGING WILL OCCURS IN NEW main CELL)          !
      !       STEP 1/2 - listing origin cell                       !
      !------------------------------------------------------------! 
      ! if Ntar > 0 then material deporation has to be done from former main cell buffer (GBUF)
      ! to the N=Ntar target main cells.
      NBL1 = NBL
      IF(DT1==ZERO)NBL1       = 0
      ORIGIN_DATA(:,:,:)        = 0 !tag main cells linked to these secnd ones.      
      Norigin(NBF:NBL)          = 0 
      DO IB=NBF,NBL1
        NBCUT            =  BRICK_LIST(NIN,IB)%NBCUT        
        NCELL            =  NBCUT
        ICELL            =  0 
        MCELL            =  BRICK_LIST(NIN,IB)%mainID
        GBUF             => ELBUF_TAB(NGB(IB))%GBUF
        pNodWasMain(1:8)    => BRICK_LIST(NIN,IB)%NODE(1:8)!%NodWasMain
        pWhereWasMain(1:8)  => BRICK_LIST(NIN,IB)%NODE(1:8)!%WhereWasMain
        brickID          =  BRICK_LIST(NIN,IB)%ID
        MBUF             => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1)         
        NTAG             =  0
        MTN_             =  IPARG(1,NGB(IB)) 
        NewMnod(1:8)     =  0     
        ITAG(:)          =  0
        IF(MCELL == 0)CYCLE
        MNOD             = BRICK_LIST(NIN,IB)%POLY(MCELL)%NumNOD 
ccc        print *, "demerging criteria for id=", ixs(11,brick_list(nin,ib)%id)
        !------CHECK NODES--------!
        DO K=1,MNOD
          INOD = BRICK_LIST(NIN,IB)%POLY(MCELL)%ListNodID(K)
          IF(pNodWasMain(INOD)%NodWasMain==1)CYCLE !node not concerned
          !Here node was secnd and just became main
          NewMnod(INOD) = 1
        ENDDO
        
        !Continuity Criteria To avoid unexpected demerging.
        !Consistent only with kinematic time step "several cycle to fully cross an element".
        VOL_M    = BRICK_LIST(NIN,IB)%secndlist%VOL_unmerged !BRICK_LIST(NIN,IB)%POLY(MCELL)vnew
        UncutVOL = BRICK_LIST(NIN,IB)%UncutVol
        VAR      = VOL_M / UncutVOL
ccc        print *, "    VAR > 1-CritDVol22 : CYCLE", VAR > 1-CritDVol22, VAR, VOL_M
ccc        print *, "    MNOD==0)           : CYCLE", MNOD==0
!not correct with sharp edges        
!        IF (VAR > 1-CritDVol22)CYCLE
        IF(MNOD==0)            CYCLE       
        CALL weighting_Cell_Nodes(NIN,IB,MCELL,INod_W,IDEMERGE)
        
       ! IF(pNodWasMain(BRICK_LIST(NIN,IB)%OldMainStrongNode) == 1) IDEMERGE = 0
       
        INod_W_old = BRICK_LIST(NIN,IB)%OldMainStrongNode
ccc        print *, "    INOD_W_old==0 : CYCLE", INOD_W_old==0
        IF(INOD_W_old<=0)CYCLE
        IF(BRICK_LIST(NIN,IB)%NODE(INod_W_old)%WhichCell == MCELL) IDEMERGE = 0  
ccc        print *, "    IDEMERGE= : ", IDEMERGE
        
        IF( IDEMERGE==1 ) THEN           !(negatif en debug si IB nouveau dans le buffer)        
!        IF(pNodWasMain(INod_W)<=0 ) THEN           !(negatif en debug si IB nouveau dans le buffer)       
!        IF(SUM(NewMnod)>=MNOD/2 .AND. MNOD/=0)THEN         !DEMERGE criteria : all main nodes were secndo ones
        !get related main cell location (through its face)
          DO K=1,MNOD
            INOD = BRICK_LIST(NIN,IB)%POLY(MCELL)%ListNodID(K)        
            J    = pWhereWasMain(INOD)%WhereWasMain
            IF(J==0)CYCLE        !means node was already main
            IF(ITAG(J)==1)CYCLE  !list without doublon
            ITAG(J) = 1   
            !Check if main cell was moved (merged)
            !Get main cut cell address
            IF(J>=10)THEN
              J1    = J/10
              J2    = MOD(J,10)
              IBv_i = BRICK_LIST(NIN,IB   )%Adjacent_Brick(J1,4)
              IBv   = BRICK_LISt(NIN,IBv_i)%Adjacent_Brick(J2,4)
            ELSE
              IBv  = BRICK_LIST(NIN,IB )%Adjacent_Brick(J,4)
            ENDIF
            Ntar = BRICK_LIST(NIN,IBv)%Ntarget
            IF(Ntar == 0)THEN
              NTAG = NTAG + 1                     
              ORIGIN_DATA(IB,NTAG,1) =  IBv  
              ORIGIN_DATA(IB,NTAG,2) =  J             
              ORIGIN_DATA(IB,NTAG,3) =  IBv              
            ELSE
              DO Itar=1,NTar    
                NTAG = NTAG + 1                
                ORIGIN_DATA(IB,NTAG,1) = BRICK_LIST(NIN,IBv)%MergeTarget(3,ITAR)
                ORIGIN_DATA(IB,NTAG,2) = J
                ORIGIN_DATA(IB,NTAG,3) = IBv                
              ENDDO
            ENDIF
          ENDDO
          Norigin(IB) = NTAG 
          IF(NTAG==0)THEN
            print *, "**error : inter22, topology issue."
          ENDIF 
        ENDIF  
      ENDDO! next IB
      
      !--------------!
      CALL MY_BARRIER
      !--------------!      

      !------------------------------------------------------------!
      ! @27. MATERIAL DEMERGING                                    !
      ! STEP 2 : filling main cell with data from origin cells   !
      !------------------------------------------------------------! 
      ! if Ntar > 0 then material deporation has to be done from former main cell buffer (GBUF)
      ! to the N=Ntar target main cells.
      NBL1 = NBL
      IF(DT1==ZERO)NBL1 = 0
      
      if(IBUG22_merge/=0)then
        if(itask==0)ALLOCATE (TMP22ARRAY(8,NB))
        IN22 = 0
        call my_barrier
        TMP22ARRAY(1:8,NBF:NBL1)=ZERO
      endif
        
        
      DO IB=NBF,NBL1
        IF(Norigin(IB)==0)CYCLE
        NBCUT             =  BRICK_LIST(NIN,IB)%NBCUT
        IF(NBCUT==0)CYCLE        
        NCELL             =  NBCUT
        ICELL             =  0 
        MCELL             =  BRICK_LIST(NIN,IB)%mainID
        GBUF              => ELBUF_TAB(NGB(IB))%GBUF
        LBUF              => ELBUF_TAB(NGB(IB))%BUFLY(1)%LBUF(1,1,1)
        !pAdjCELL          => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%Adjacent_Cell(1:5)          
        pAdjBRICK         => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:3)
        pNodWasMain(1:8)     => BRICK_LIST(NIN,IB)%NODE(1:8)!%NodWasMain
        pWhereWasMain(1:8)   => BRICK_LIST(NIN,IB)%NODE(1:8)!%WhereWasMain
        pSUBVOL(1:9)      => BRICK_LIST(NIN,IB)%POLY(1:9)
        brickID           =  BRICK_LIST(NIN,IB)%ID
        IDLOC             =  BRICK_LIST(NIN,IB)%IDLOC
        MTN_              =  IPARG(1,NGB(IB)) 
        LLT_              =  IPARG(2,NGB(IB)) 
        MBUF              => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1)         
        NTAG              =  0
        VOLorig(1:24)     =  ZERO
        VOLcell           =  ZERO
        VOLcell51(:)      =  ZERO
        VolSECND51(:)     =  ZERO
        VOL               =  ZERO
        RHO               =  ZERO
        EINT              =  ZERO
        SIG(1:6)          =  ZERO
        MOM(1:3)          =  ZERO
        SSP               =  ZERO
        UVAR(:)           =  ZERO
        RHO_ADV           =  ZERO
        EINT_ADV          =  ZERO
        SIG_ADV(1:6)      =  ZERO
        MOM_ADV(1:3)      =  ZERO
        UVAR_ADV(:)       =  ZERO

        !-----------------------------------------------!  
        !  INIT CELL CONTENT IF IT CAME FROM A MERGE    !          
        !-----------------------------------------------!        
        IF(Brick_list(nin,ib)%IsMergeTarget==1)THEN
       !   pause
          !!!print *, "ismergeTarget , Norigin", ixs(11,brickID), Norigin(IB), BRICK_LIST(NIN,IB)%Ntarget
          ! This cell received material from a merge.it also has to be taken into account
          VolCell   = BRICK_LIST(NIN,IB)%Vold_Scell
          RHO       = VolCell* GBUF%RHO(IDLOC)
          EINT      = VolCell* GBUF%EINT(IDLOC)
          MOM(1)    = VolCell* GBUF%MOM(LLT_*(1-1) + IDLOC)
          MOM(2)    = VolCell* GBUF%MOM(LLT_*(2-1) + IDLOC)
          MOM(3)    = VolCell* GBUF%MOM(LLT_*(3-1) + IDLOC)         
          SIG(1)    = VolCell* GBUF%SIG(LLT_*(1-1)+IDLOC)     
          SIG(2)    = VolCell* GBUF%SIG(LLT_*(2-1)+IDLOC)     
          SIG(3)    = VolCell* GBUF%SIG(LLT_*(3-1)+IDLOC)     
          SIG(4)    = VolCell* GBUF%SIG(LLT_*(4-1)+IDLOC)     
          SIG(5)    = VolCell* GBUF%SIG(LLT_*(5-1)+IDLOC)     
          SIG(6)    = VolCell* GBUF%SIG(LLT_*(6-1)+IDLOC)
          SSP       = VolCell* LBUF%SSP(IDLOC)
          VolCell   = ZERO
          IF(MTN_==37)THEN
            UVAR(5) = VolCell*MBUF%VAR(((5-1)*LLT_ + IDLOC))
            UVAR(4) = VolCell*MBUF%VAR(((4-1)*LLT_ + IDLOC))
            UVAR(3) = VolCell*MBUF%VAR(((3-1)*LLT_ + IDLOC))*MBUF%VAR(((4-1)*LLT_ + IDLOC))
            UVAR(2) = VolCell*MBUF%VAR(((2-1)*LLT_ + IDLOC))*MBUF%VAR(((5-1)*LLT_ + IDLOC))  
            UVAR(1) = VolCell*MBUF%VAR(((1-1)*LLT_ + IDLOC))
          ELSEIF(MTN_==51)THEN
            IPOS          = 11                                          
            volSECND51(1) = MBUF%VAR(IDLOC  + ((N0PHAS + (1-1)*NVPHAS )+IPOS-1)*LLT_)                                              
            volSECND51(2) = MBUF%VAR(IDLOC  + ((N0PHAS + (2-1)*NVPHAS )+IPOS-1)*LLT_)                                              
            volSECND51(3) = MBUF%VAR(IDLOC  + ((N0PHAS + (3-1)*NVPHAS )+IPOS-1)*LLT_)                                              
            volSECND51(4) = MBUF%VAR(IDLOC  + ((N0PHAS + (4-1)*NVPHAS )+IPOS-1)*LLT_)                                              
            DO ITRIMAT=1,TRIMAT                                         
              DO IPOS = 1 , NVPHAS                                      
                IF(IPOS==11) CYCLE                                       
                K               = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}   
                K1              = K * LLT_                             
                UVAR(K+1)       = volSECND51(ITRIMAT) * MBUF%VAR(K1+IDLOC)                                           
              ENDDO!next IPOS                                           
            ENDDO!next ITRIMAT                                          
            volSECND51(1:4)     = ZERO                                  
          ENDIF
        ENDIF
        
        ! Velocity of main is velocity wherever inside the supercell. It will be used to make an advection step get pressure gradient which did not occur on previous step due to homogeneity.
        VEL_M(1)    = GBUF%MOM(3*(IDLOC-1)+1) / GBUF%RHO(IDLOC)
        VEL_M(2)    = GBUF%MOM(3*(IDLOC-1)+2) / GBUF%RHO(IDLOC)
        VEL_M(3)    = GBUF%MOM(3*(IDLOC-1)+3) / GBUF%RHO(IDLOC)   
        
        NTAR              =  BRICK_LIST(NIN,IB)%Ntarget
        !-----------------------------------------------!  
        !  LOOP ON ORIGIN CELLS                         !          
        !-----------------------------------------------!        
        DO IORIG = 1, Norigin(IB)
          !IORIG data regarding IB 
          IBm             = ORIGIN_DATA(IB,IORIG,1)    !real target
          IBo             = ORIGIN_DATA(IB,IORIG,3)    !original brick before knowing it was burged below
          IV              = BRICK_LIST(NIN,IBm)%ID
          NGv             = BRICK_LIST(NIN,IBm)%NG
          IDLOCv          = BRICK_LIST(NIN,IBm)%IDLOC
          J               = ORIGIN_DATA(IB,IORIG,2)
          IF(IBm==IB)CYCLE           
          IF(J>=10)THEN
            J1  = J/10
            J2  = MOD(J,10)
            IBv = BRICK_LIST(NIN,IB )%Adjacent_Brick(J1,4)
            JV  = BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,5)
          ELSE
            JV  = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)
          ENDIF           

          NumSECND        = OLD_SecndList(NIN,IBo)%Num  !this is list from previous cycle
          !-----------------------------------------------!
          !  GET SECND CONNECTED TO IT                    !
          !-----------------------------------------------!
          DO K=1,NumSECND
            IF (OLD_SecndList(NIN,IBo)%IBv(K) /= IB)CYCLE 
            IF (OLD_SecndList(NIN,IBo)%FM(K) /= JV)CYCLE   !next secnd : this one is not connecter through main face id %FM
            
          !---debug---!
          cod1 = ixs(11,brick_list(nin,ib)%id)
          cod2 = ixs(11,brick_list(nin,ibo)%id)
          if (IBUG22_merge==-1)then
#include "lockon.inc"
            if(IBm==ibo)then
 !             write(*,FMT='(A,I8,A,I8)')      "DEMERGING : id=", Cod1, 
 !    .                               " from idv:", ixs(11,brick_list(nin,ibo)%id)

              IN22 = IN22 + 1
              tmp22array(1,IN22)=Cod1
              tmp22array(2,IN22)=ixs(11,brick_list(nin,ibo)%id)
              tmp22array(3,IN22)=0

            else
 !             write(*,FMT='(A,I8,A,I8,A,I8)') "DEMERGING : id=", Cod1, 
 !    .                               " from idv:", ixs(11,brick_list(nin,ibo)%id)," moved to", ixs(11,brick_list(nin,IBm)%id)

              IN22 = IN22 + 1
              tmp22array(1,IN22)=Cod1
              tmp22array(2,IN22)=ixs(11,brick_list(nin,ibo)%id)
              tmp22array(3,IN22)=ixs(11,brick_list(nin,IBm)%id)

            endif
 !           write (*,FMT='(I10,A,F30.16,A,F30.16)')
 !    .        Cod1," Vold=",brick_list(nin,ib)%Vold_scell, " Vnew=",brick_list(nin,ib)%Vnew_scell
 !           write (*,FMT='(I10,A,F30.16,A,F30.16)')
 !    .        Cod2," Vold=",brick_list(nin,ibo)%Vold_scell," Vnew=",brick_list(nin,ibo)%Vnew_scell 

               tmp22array(4,IN22)= zero !brick_list(nin,ib)%Vold_scell    !value not consistent, old main was merge elsewhere so scell has change of side since a demerge process is emerging a new scell
               tmp22array(5,IN22)= brick_list(nin,ib)%Vnew_scell
               tmp22array(6,IN22)= zero !brick_list(nin,ibo)%Vold_scell
               tmp22array(7,IN22)= brick_list(nin,ibo)%Vnew_scell
               tmp22array(8,IN22)=tmp22array(8,IN22)+1
#include "lockoff.inc" 
          endif
          !-----------!
                                
            !here secnd is merged on the correct face, and also to the correct target main (IB)
  !!!!!!!!!!!!!          VOLsecnd = OLD_SecndList(NIN,IBo)%VOL(K)
           !-----------------------------------------------!
           !  START TO CALCULATE SECND NEW QUANTITIES      !
           !-----------------------------------------------!
            VOLsecnd    = OLD_SecndList(NIN,IBo)%VOL(K)
           !!! VolsecndOLD = OLD_SecndList(NIN,IBo)%VOL(K)
            VOLcell  = VOLcell + VOLsecnd                   !cumulative salve volume from previous cycle = old volume for new main cell     
           !!! VOLcellOLD = VOLcell + VOLsecndOLD       
            GBUF     => ELBUF_TAB(NGv)%GBUF 
            LBUF     => ELBUF_TAB(NGv)%BUFLY(1)%LBUF(1,1,1) 
            LLT_v    = IPARG(2,NGv)
            !secnd cell surf     
            SURF_S   = OLD_SecndList(NIN,IBo)%SURF_V(K)  
            FV       = OLD_SecndList(NIN,IBo)%FV(K)
            IF(FV<10)THEN
              NORM_S(1:3) = BRICK_LIST(NIN,IB)%N(FV,1:3) 
            ELSE
              !FV          = FV/10
              !NORM_S(1:3) = BRICK_LIST(NIN,IB)%N(FV,1:3)
                     !IL Y A PLUSIEURS FACETTES POSSIBLE POUR LES VOISINS INDIRECT. IL FAUDRAIT SAUVEGARGER J>10 pour chaque chemin, puis advection            
                     !ou alors changer la normale en prenant le vecteur centroid_M-centroid_S
              ZM(1:3)     = BRICK_LIST(NIN,IBo)%SCellCenter(1:3)
              ZS(1:3)     = BRICK_LIST(NIN,IB)%POLY(MCELL)%CellCenter(1:3)
              NORM_S(1)   = ZM(1)-ZS(1)
              NORM_S(2)   = ZM(2)-ZS(2)
              NORM_S(3)   = ZM(3)-ZS(3)
              NORM        = SQRT(NORM_S(1)*NORM_S(1)+NORM_S(2)*NORM_S(2)+NORM_S(3)*NORM_S(3))
              NORM_S(1)   = NORM_S(1) / NORM
              NORM_S(2)   = NORM_S(2) / NORM
              NORM_S(3)   = NORM_S(3) / NORM
            ENDIF 
            
            
            !----------------------------------! 
            !  ADVECTION STEP IN DEMERGED CELL !
            !----------------------------------!
            ADV = ZERO
            IF(IADV==1)THEN
            !MCELL         = BRICK_LIST(NIN,IB)%MainID
            VM            = ONE ! BRICK_LIST(NIN,IBm)%POLY(MCELL)%Vnew 
            ADV           = -HALF* HALF* DT1*VM*SURF_S* (VEL_M(1)*NORM_S(1)+VEL_M(2)*NORM_S(2)+VEL_M(3)*NORM_S(3)) 
            RHO_ADV       = RHO_ADV    + ADV * GBUF%RHO(IDLOCv)
            EINT_ADV      = EINT       + ADV * GBUF%EINT(IDLOCv)
            MOM_ADV(1)    = MOM_ADV(1) + ADV * GBUF%MOM(3*(IDLOCv-1)+1)
            MOM_ADV(2)    = MOM_ADV(2) + ADV * GBUF%MOM(3*(IDLOCv-1)+2)
            MOM_ADV(3)    = MOM_ADV(3) + ADV * GBUF%MOM(3*(IDLOCv-1)+3)                        
            SIG_ADV(1)    = SIG_ADV(1) + ADV * GBUF%SIG(LLT_v*(1-1)+IDLOCv)       
            SIG_ADV(2)    = SIG_ADV(2) + ADV * GBUF%SIG(LLT_v*(2-1)+IDLOCv)       
            SIG_ADV(3)    = SIG_ADV(3) + ADV * GBUF%SIG(LLT_v*(3-1)+IDLOCv)       
            SIG_ADV(4)    = SIG_ADV(4) + ADV * GBUF%SIG(LLT_v*(4-1)+IDLOCv)       
            SIG_ADV(5)    = SIG_ADV(5) + ADV * GBUF%SIG(LLT_v*(5-1)+IDLOCv)       
            SIG_ADV(6)    = SIG_ADV(6) + ADV * GBUF%SIG(LLT_v*(6-1)+IDLOCv)
            ENDIF 
            !-------------------------------! 
            !  MONO-MATERIAL CUMULATION     !
            !-------------------------------!                                
            RHO    = RHO    + VOLsecnd * GBUF%RHO(IDLOCv)
            EINT   = EINT   + VOLsecnd * GBUF%EINT(IDLOCv)
            MOM(1) = MOM(1) + VOLsecnd * GBUF%MOM(LLT_v*(1-1)+IDLOCv)
            MOM(2) = MOM(2) + VOLsecnd * GBUF%MOM(LLT_v*(2-1)+IDLOCv)
            MOM(3) = MOM(3) + VOLsecnd * GBUF%MOM(LLT_v*(3-1)+IDLOCv)                        
            SIG(1) = SIG(1) + VOLsecnd * GBUF%SIG(LLT_v*(1-1)+IDLOCv)       
            SIG(2) = SIG(2) + VOLsecnd * GBUF%SIG(LLT_v*(2-1)+IDLOCv)       
            SIG(3) = SIG(3) + VOLsecnd * GBUF%SIG(LLT_v*(3-1)+IDLOCv)       
            SIG(4) = SIG(4) + VOLsecnd * GBUF%SIG(LLT_v*(4-1)+IDLOCv)       
            SIG(5) = SIG(5) + VOLsecnd * GBUF%SIG(LLT_v*(5-1)+IDLOCv)       
            SIG(6) = SIG(6) + VOLsecnd * GBUF%SIG(LLT_v*(6-1)+IDLOCv)
            SSP    = SSP    + VOLsecnd * LBUF%SSP(IDLOCv)
            !-------------------------------!
            !  MULTI-MATERIAL CUMULULATION  !
            !-------------------------------!           
            IF(MTN_==37)THEN
              !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multiplying by element volume in aleconve.F)
              !UVAR(I,2) : density of gas
              !UVAR(I,3) : density of liquid
              !UVAR(I,4) : volumetric fraction of liquid  
              !UVAR(I,5) : volumetric fraction of gas  
              MBUFv       => ELBUF_TAB(NGv)%BUFLY(1)%MAT(1,1,1)  
              LLT_v       = IPARG(2,NGv)              
              UVAR(5)     = UVAR(5) +VOLsecnd*MBUFv%VAR(((5-1)*LLT_v + IDLOCv))
              UVAR(4)     = UVAR(4) +VOLsecnd*MBUFv%VAR(((4-1)*LLT_v + IDLOCv))
              UVAR(3)     = UVAR(3) +VOLsecnd*MBUFv%VAR(((3-1)*LLT_v + IDLOCv))*MBUFv%VAR(((4-1)*LLT_v + IDLOCv))
              UVAR(2)     = UVAR(2) +VOLsecnd*MBUFv%VAR(((2-1)*LLT_v + IDLOCv))*MBUFv%VAR(((5-1)*LLT_v + IDLOCv))          
              UVAR(1)     = UVAR(1) +VOLsecnd*MBUFv%VAR(((1-1)*LLT_v + IDLOCv))
              UVAR_ADV(1) = UVAR_ADV(1) +ADV * MBUFv%VAR(((1-1)*LLT_v + IDLOCv))

            ELSEIF(MTN_==51)THEN
              MBUFv  => ELBUF_TAB(NGv)%BUFLY(1)%MAT(1,1,1)           
              LLT_  = IPARG(2,NGB(IB))
              LLT_v = IPARG(2,NGv)
              !in sigesp51.F: 
              !V1OLD = V1OLD - TIMESTEP*DDVOL1            
              !Volume change correspond to DDVOL=DV/DT = f(fluxes). Gird deformation influences relative material velocity ans also fluxes
              DO ITRIMAT=1,TRIMAT
                !get,store Volumetric Fraction
                IPOS                 = 1
                K1                   = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                Kv                   = K1 * LLT_v
                Vfrac(ITRIMAT)       = MBUFv%VAR(Kv+IDLOCv)
                VolSECND51(ITRIMAT)  = Vfrac(ITRIMAT) * VOLsecnd     !submaterial volume in secnd cell 
                VolCELL51(ITRIMAT)   = VolCELL51(ITRIMAT) + VolSECND51(ITRIMAT)                  !submaterial volume in ALL secnd cell 
              ENDDO!next ITRIMAT 
              UVAR(1) = UVAR(1) + MBUFv%VAR((0*LLT_v + IDLOCv)) * VOLsecnd
              UVAR(2) = UVAR(2) + MBUFv%VAR((1*LLT_v + IDLOCv)) * VOLsecnd
              UVAR(3) = UVAR(3) + MBUFv%VAR((2*LLT_v + IDLOCv)) * VOLsecnd   
              UVAR_ADV(1) = UVAR_ADV(1) + MBUFv%VAR((0*LLT_v + IDLOCv)) * ADV
              UVAR_ADV(2) = UVAR_ADV(2) + MBUFv%VAR((1*LLT_v + IDLOCv)) * ADV
              UVAR_ADV(3) = UVAR_ADV(3) + MBUFv%VAR((2*LLT_v + IDLOCv)) * ADV 
              IF(IADV==1)print *, "to do compute/get vfrac on face" !; pause                          
              DO ITRIMAT=1,TRIMAT
                DO IPOS = 1 , NVPHAS
                  Ko              = ((N0PHAS + (ITRIMAT-1)*NVPHAS ))
                  K1              = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                  Kv              = K1 * LLT_v
                  IF(IPOS==11)THEN
                    UVAR(K1+1) = UVAR(K1+1) + volSECND51(ITRIMAT)                 
                  ELSE                  
                    UVAR(K1+1) = UVAR(K1+1) + volSECND51(ITRIMAT) * MBUFv%VAR(Kv+IDLOCv)
                    UVAR_ADV(K1+1) = UVAR_ADV(K1+1) + ADV * MBUFv%VAR(Kv+IDLOCv)
                    IF(IADV==1)print *, "to do compute/get vfrac on face" !; pause
                  ENDIF
                ENDDO!next IPOS
              ENDDO!next ITRIMAT
            ENDIF!(MTN_==51)   
            !---------------------------                             
#include "lockon.inc"    
            !lock method : to be update later can be removed
            !SUBSTRACTING SECND VOLUME FROM  SUPERCELL TO DEMERGE
            !--MONOMAT
            BRICK_LIST(NIN,IBm)%Vold_Scell = BRICK_LIST(NIN,IBm)%Vold_Scell - VOLsecnd 
            !--MULTIMAT
            IF(MTN_==51)THEN            
              DO ITRIMAT=1,TRIMAT
                !remove submaterial volume in detached secnd cell
                IPOS                 = 11
                K1                   = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}                                                  
                Kv                   = K1 * LLT_v
                MBUFv%VAR(Kv+IDLOCv) = MBUFv%VAR(Kv+IDLOCv) - VolSECND51(ITRIMAT)
              ENDDO!next ITRIMAT  
            ENDIF          
#include "lockoff.inc"           
          ENDDO!next K
        ENDDO!next IORIG

        IF(VOLcell==ZERO)CYCLE

        !-----------------------------------------------!
        ! GET VALUE FROM CUMULATIVE ONE                 !
        !---MONOMAT-------------------------------------! 
          RHO     =  (RHO    +IADV*RHO_ADV   )/ VOLcell
          EINT    =  (EINT   +IADV*EINT_ADV  )/ VOLcell
          MOM(1)  =  (MOM(1) +IADV*MOM_ADV(1))/ VOLcell        
          MOM(2)  =  (MOM(2) +IADV*MOM_ADV(2))/ VOLcell        
          MOM(3)  =  (MOM(3) +IADV*MOM_ADV(3))/ VOLcell                        
          SIG(:)  =  (SIG(:) +IADV*SIG_ADV(:))/ VOLcell
          SSP     =  SSP                      / VOLcell   ! -> run EOS
        !---MULTIMAT -----------------------------------!
        IF(MTN_==37)THEN
          UVAR(1) = (UVAR(1)+ IADV*UVAR_ADV(1)) / VOLcell                   
          UVAR(2) =  UVAR(2) / VOLcell
          UVAR(3) =  UVAR(3) / VOLcell
          UVAR(4) =  UVAR(4) / VOLcell
          UVAR(5) =  UVAR(5) / VOLcell     
          !call sigeps37_single_cell before storing in MBUF%VAR              
        ELSEIF(MTN_==51)THEN
          UVAR(1) = (UVAR(1) + IADV*UVAR_ADV(1)) / VOLcell  !sum of full volume because UVAR(1:3) are homogenized data, not submaterial one.
          UVAR(2) = (UVAR(2) + IADV*UVAR_ADV(2)) / VOLcell
          UVAR(3) = (UVAR(3) + IADV*UVAR_ADV(3)) / VOLcell      
          DO ITRIMAT=1,TRIMAT
            Ko = N0PHAS + (ITRIMAT-1)*NVPHAS
            IF(VOLcell51(ITRIMAT)/=ZERO)THEN
              UVAR(Ko+01:Ko+10)     =  (UVAR(Ko+01:Ko+10)     + IADV*UVAR_ADV(Ko+01:Ko+10))     / VOLcell51(ITRIMAT)   !submaterial data are averaged with sum of submaterial secnd volume      
              UVAR(Ko+12:Ko+NVPHAS) =  (UVAR(Ko+12:Ko+NVPHAS) + IADV*UVAR_ADV(Ko+12:Ko+NVPHAS)) / VOLcell51(ITRIMAT)   !except ipos=11 (volume)      
            ELSE
              UVAR(Ko+01) = ZERO         
              UVAR(Ko+11) = ZERO              
            ENDIF
          ENDDO    
        ENDIF!(MTN_==51)
        !-----------------------------------------------!

        !-----------------------------------------------!
        ! STORE VALUES IN DEMERGED CELLS                !
        !---MONOMAT-------------------------------------!    
        NG                                      =  BRICK_LIST(NIN,IB)%NG
        IDLOC                                   =  BRICK_LIST(NIN,IB)%IDLOC       
        LLT_                                    =  IPARG(2,NG)
        GBUF                                    => ELBUF_TAB(NG)%GBUF
        LBUF                                    => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
        GBUF%RHO(IDLOC)                         =  RHO      
        GBUF%EINT(IDLOC)                        =  EINT     
        GBUF%MOM(LLT_*(1-1)+IDLOC)              =  MOM(1)   
        GBUF%MOM(LLT_*(2-1)+IDLOC)              =  MOM(2)   
        GBUF%MOM(LLT_*(3-1)+IDLOC)              =  MOM(3)   
        GBUF%SIG(LLT_*(1-1)+IDLOC)              =  SIG(1)
        GBUF%SIG(LLT_*(2-1)+IDLOC)              =  SIG(2)
        GBUF%SIG(LLT_*(3-1)+IDLOC)              =  SIG(3)
        GBUF%SIG(LLT_*(4-1)+IDLOC)              =  SIG(4)
        GBUF%SIG(LLT_*(5-1)+IDLOC)              =  SIG(5)
        GBUF%SIG(LLT_*(6-1)+IDLOC)              =  SIG(6) 
        LBUF%SSP(IDLOC)                         =  SSP      
        ALEFVM_Buffer%FCELL(4, BRICK_LIST(NIN,IB)%ID )        = RHO
        ALEFVM_Buffer%FCELL(1, BRICK_LIST(NIN,IB)%ID )        = MOM(1)
        ALEFVM_Buffer%FCELL(2, BRICK_LIST(NIN,IB)%ID )        = MOM(2)
        ALEFVM_Buffer%FCELL(3, BRICK_LIST(NIN,IB)%ID )        = MOM(3)         
        ALEFVM_Buffer%FCELL(6, BRICK_LIST(NIN,IB)%ID )        = -THIRD*(SIG(1)+SIG(2)+SIG(3)) 
        ALEFVM_Buffer%FCELL(5, BRICK_LIST(NIN,IB)%ID )        = SSP
        
        !---MUKLTIMAT-------------------------------------!
        IF(MTN_==37)THEN
          LLT_                       =  IPARG(2,NGB(IB))
          IDLOC                      =  IDLOCB(IB)
          MBUF                       => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1) 
          MT                         =  IXS(1,brickID)
          IADBUF                     =  IPM(7,MT) 
          RHO10                      =  BUFMAT(IADBUF-1+11)
          RHO20                      =  BUFMAT(IADBUF-1+12)
c          print *, ""
c          print *, "demerge-5",  MBUF%VAR((5-1)*LLT_+IDLOC) , UVAR(5)
c          print *, "demerge-4",  MBUF%VAR((4-1)*LLT_+IDLOC) , UVAR(4)
c          print *, "demerge-3",  MBUF%VAR((3-1)*LLT_+IDLOC) , UVAR(3)
c          print *, "demerge-2",  MBUF%VAR((2-1)*LLT_+IDLOC) , UVAR(2)
c          print *, "demerge-1",  MBUF%VAR((1-1)*LLT_+IDLOC) , UVAR(1) 
          IF(UVAR(3) == ZERO)   UVAR(3) = RHO10 
          IF(UVAR(2) == ZERO)   UVAR(2) = RHO20                                       
          MBUF%VAR((5-1)*LLT_+IDLOC) = UVAR(5)
          MBUF%VAR((4-1)*LLT_+IDLOC) = UVAR(4)
          MBUF%VAR((3-1)*LLT_+IDLOC) = UVAR(3)
          MBUF%VAR((2-1)*LLT_+IDLOC) = UVAR(2)
          MBUF%VAR((1-1)*LLT_+IDLOC) = UVAR(1)                                        
        ELSEIF(MTN_==51)THEN
          LLT_  =  IPARG(2,NGB(IB))
          IDLOC =  IDLOCB(IB)
          MBUF  => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1)         
          DO ITRIMAT=1,TRIMAT
            IF(VOLcell51(ITRIMAT)/=ZERO)THEN         
              !get,store Volumetric Fraction
              DO IPOS=1,NVPHAS
                K0                 = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                K1                 = K0 * LLT_
                MBUF%VAR(K1+IDLOC) = UVAR(N0PHAS+(ITRIMAT-1)*NVPHAS+IPOS)  
              ENDDO

              IPOS=1
                K0                 = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                K1                 = K0 * LLT_
                MBUF%VAR(K1+IDLOC) = VolCELL51(ITRIMAT)/VolCELL            
              IPOS=11
                K0                 = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                K1                 = K0 * LLT_
                MBUF%VAR(K1+IDLOC) = VolCELL51(ITRIMAT)           
            ELSE
              IPOS=1
                K0                 = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                K1                 = K0 * LLT_
                MBUF%VAR(K1+IDLOC) = ZERO           
              IPOS=11
                K0                 = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                K1                 = K0 * LLT_
                MBUF%VAR(K1+IDLOC) = ZERO               
            ENDIF
          ENDDO  
        ENDIF!(MTN_==51)
        !-----------------------------------------------!
        
        !-----------------------------------------------!
        !  NEW main CELL UPDATE                       ! 
        !-----------------------------------------------!              
        BRICK_LIST(NIN,IB)%Vold_SCell = VOLcell
        
        !APPEL SIGEPS37 POUR EQUILIBRE PRESSION
        brickID  = BRICK_LIST(NIN,IB)%ID
        IDLOC    = IDLOCB(IB)
        NG       = NGB(IB)
        VOL      = VOLcell
        MTN_   = IPARG(1,NG) 
        IF(MTN_==37)THEN
          CALL SIGEPS37_SINGLE_CELL (
     1                                ELBUF_TAB, IXS,  BUFMAT,  IPARG, IPM,
     2                                IDLOC    , NG ,  brickID, VOL  
     .                               )
         ENDIF 

        !
        !VOLcell = sum (VOLslacve) : each volsecnd is removed from 
        !its former main cell and cumulative value is set in new 
        !main cell to demerge.
        !-----------------------------------------------------!
        ! ADVECTION STEP UPDATE ORIGIN CELL (PREVIOUS MAIN) !
        !-----------------------------------------------------!
        IF(IADV==1)THEN
          NG    = BRICK_LIST(NIN,IBm)%NG
          IDLOC = BRICK_LIST(NIN,IBm)%IDLOC 
          GBUF  => ELBUF_TAB(NG)%GBUF
          LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          GBUF%RHO(IDLOC)             = GBUF%RHO(IDLOC)             -    RHO_ADV/VolCEll
          GBUF%EINT(IDLOC)            = GBUF%EINT(IDLOC)            -   EINT_ADV/VolCEll
          GBUF%MOM(3*(IDLOC-1) +1)    = GBUF%MOM(3*(IDLOC-1) +1)    - MOM_ADV(1)/VolCELL
          GBUF%MOM(3*(IDLOC-1) +2)    = GBUF%MOM(3*(IDLOC-1) +2)    - MOM_ADV(2)/VolCELL
          GBUF%MOM(3*(IDLOC-1) +3)    = GBUF%MOM(3*(IDLOC-1) +3)    - MOM_ADV(3)/VolCELL
          GBUF%SIG(LLT_*(1-1) +IDLOC) = GBUF%SIG(LLT_*(1-1) +IDLOC) - SIG_ADV(1)/VolCELL
          GBUF%SIG(LLT_*(2-1) +IDLOC) = GBUF%SIG(LLT_*(2-1) +IDLOC) - SIG_ADV(2)/VolCELL
          GBUF%SIG(LLT_*(3-1) +IDLOC) = GBUF%SIG(LLT_*(3-1) +IDLOC) - SIG_ADV(3)/VolCELL
          GBUF%SIG(LLT_*(4-1) +IDLOC) = GBUF%SIG(LLT_*(4-1) +IDLOC) - SIG_ADV(4)/VolCELL
          GBUF%SIG(LLT_*(5-1) +IDLOC) = GBUF%SIG(LLT_*(5-1) +IDLOC) - SIG_ADV(5)/VolCELL
          GBUF%SIG(LLT_*(6-1) +IDLOC) = GBUF%SIG(LLT_*(6-1) +IDLOC) - SIG_ADV(6)/VolCELL
          MTN_   = IPARG(1,NG) 
          IF(MTN_==37)THEN
            LLT_                       =  IPARG(2,NG)
            IDLOC                      =  IDLOCB(IBm)
            MBUF                       => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
            brickID                    =  BRICK_LIST(NIN,IBm)%ID 
            MT                         =  IXS(1,brickID)
            IADBUF                     =  IPM(7,MT) 
            RHO10                      =  BUFMAT(IADBUF-1+11)
            RHO20                      =  BUFMAT(IADBUF-1+12) 
            IF(UVAR(3) == ZERO)   UVAR(3) = RHO10 
            IF(UVAR(2) == ZERO)   UVAR(2) = RHO20                                       
              !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multiplying by element volume in aleconve.F)
              !UVAR(I,2) : density of gas
              !UVAR(I,3) : density of liquid
              !UVAR(I,4) : volumetric fraction of liquid  
              !UVAR(I,5) : volumetric fraction of gas  
            MBUF%VAR((1-1)*LLT_+IDLOC) = UVAR(1) - UVAR_ADV(1)/VolCELL  

            !APPEL SIGEPS37 POUR EQUILIBRE PRESSION
            brickID  = BRICK_LIST(NIN,IBm)%ID
            IDLOC    = IDLOCB(IBm)
            NG       = NGB(IBm)
            VOL      = BRICK_LIST(NIN,IBm)%Vold_Scell
            MTN_     = IPARG(1,NG) 
            IF(MTN_==37)THEN
              CALL SIGEPS37_SINGLE_CELL (
     1                                    ELBUF_TAB, IXS,  BUFMAT,  IPARG, IPM,
     2                                    IDLOC    , NG ,  brickID, VOL  
     .                                   )
             ENDIF             
         
          ELSEIF(MTN_==51)THEN
          !  print *, "todo" ; pause
          ENDIF
        ENDIF

        
        !-----------------------------------------------!        
      ENDDO!next IB

          if (IBUG22_merge==-1)then
            call my_barrier
            if(itask==0 .and. DT1>ZERO)then
             allocate(order(IN22),value(IN22))
             DO Ib=1,in22
                VALUE(ib)=tmp22array(1,ib)
             ENDDO
             order=0
             !CALL QUICKSORT_I2 !(ORDER,IN22,value)
             do ib2=1,IN22
              ib=order(ib2)
              cod1=tmp22array(1,ib)
              cod2=tmp22array(2,ib)
              cod3=tmp22array(3,ib)
              !if(cod1==0)cycle
              if(cod3==0)then
                 write(*,FMT='(A,I8,A,I8)')      "DEMERGING : id=", Cod1, " from idv:", cod2
              else
                 write(*,FMT='(A,I8,A,I8,A,I8)') "DEMERGING : id=", Cod1, " from idv:", cod2," moved to", cod3           
              endif
              write (*,FMT='(I10,A,F30.16,A,F30.16)') Cod1," Vold=", tmp22array(4,ib), " Vnew=", tmp22array(5,ib)
              write (*,FMT='(I10,A,F30.16,A,F30.16)') Cod2," Vold=", tmp22array(6,ib), " Vnew=", tmp22array(7,ib) 
              write (*,FMT='(A,F30.16)') " Number of access =", tmp22array(8,ib)
             enddo 
            endif 
            call my_barrier
            if(itask==0)then
              if(allocated(tmp22array))deallocate(tmp22array)
              if(DT1>ZERO .and. allocated(order) )deallocate(order)    
            endif 
          endif


      !------------------------------------------------------------!
      ! @28. COMPUTE VOLD_Vnew_Cell for newly cell in buffer       !
      !------------------------------------------------------------!
      !OLD_Vnew_Cell(1:9) are old cell volumes for a cut cell.
      !It is taken from previous cycle.
      !If brick was not cut on previous cycle then it must be initialized here
      IF(DT1==ZERO)NBL1 = 0
      DO IB=NBF,NBL1  
        NBCUT                                     = BRICK_LIST(NIN,IB)%NBCUT 
        WASCUT                                    = BRICK_LIST(NIN,IB)%WasCut   
        NewInBuffer                               = BRICK_LIST(NIN,IB)%NewInBuffer                                            
CCC        IF(WASCUT==0.and. NBCUT==0)THEN
        IF( NewInBuffer == 1 )THEN        

          MCELL                                   = 1
          BRICK_LIST(NIN,IB)%POLY(MCELL)%OLD_Vnew = BRICK_LIST(NIN,IB)%POLY(MCELL)%Vnew
          BRICK_LIST(NIN,IB)%POLY(2:9)%OLD_Vnew   = ZERO
!          print *, "new brick in cut cell buffer:", IXS(11,brick_list(nin,ib)%id)          
!          print *, "new brick in cut cell buffer: BRICK_LIST(NIN,IB)%POLY(MCELL)%OLD_Vnew=",BRICK_LIST(NIN,IB)%POLY(MCELL)%Vnew

          NG                                       =  BRICK_LIST(NIN,IB)%NG           
          IDLOC                                    =  BRICK_LIST(NIN,IB)%IDLOC        
          ID                                       =  BRICK_LIST(NIN,IB)%ID          
          GBUF                                     => ELBUF_TAB(NG)%GBUF              
          BRICK_LIST(NIN,IB)%Vold_Scell            = GBUF%VOL(IDLOC)                  

c            print *, "NEW IN BUFFER : ", ixs(11,id)
c            print *, "   vold_scell : ", brick_list(nin,ib)%vold_scell
c            pause     

        ENDIF
      ENDDO!next IB

      !------------------------------------------------------------!
      ! @29. COMPUTE VOLUME CHANGE FOR EACH CELL                   !
      !------------------------------------------------------------!
      IF(IBUG22_TRACKING>0 .OR. IBUG22_TRACKING==-1.AND.ITASK==0)print *, "=== TOPOLOGICAL TRACKING ==="      
      !topology is changing so this is not trivial and must be taken into account
      !example : a single secnd hexa can be split into {1 penta + 2 tetra} so
      !it is not possible to just make difference between two polyhedra volumes.
      NBL1   = NBL
      IF(DT1==ZERO)THEN
        NBL1 = 0
        DO IB=NBF,NBL 
          BRICK_LIST(NIN,IB)%POLY(1:9)%DVOL(1) = ZERO
          BRICK_LIST(NIN,IB)%DVOL              = ZERO
        ENDDO
      ENDIF      
      DO IB=NBF,NBL1  
        ID     = BRICK_LIST(NIN,IB)%ID                        
        MCELL  = BRICK_LIST(NIN,IB)%mainID            
        NCELL  = BRICK_LIST(NIN,IB)%NBCUT
        ICELL  = 0 
        M(:,:) = ZERO 
        WasCut = BRICK_LIST(NIN,IB)%WasCut
        MTN_   = IPARG(1,NGB(IB))  
        
       IF(WasCut>0)THEN   !forcemment M(9,JJ)>ZERO, sorti du test conditionel
         !filling relation matrix                
         DO INOD = 1,8
           JJ       = BRICK_LIST(NIN,IB)%NODE(INOD)%WhichCell
           II       = BRICK_LIST(NIN,IB)%NODE(INOD)%OLD_WhichCell
           IF(II <0) II = 1 !just being cut on this cycle so whichcell node was not set on previous cycle
           M(II,JJ) = BRICK_LIST(NIN,IB)%POLY(JJ)%Vnew !instead of setting kronecker symbol, volume is directly stored. V=0 <=> delta_ij=0
         ENDDO!next INOD
         IF(NBCUT>2 .AND. M(9,9)==ZERO)M(9,9) = BRICK_LIST(NIN,IB)%POLY(JJ)%Vnew
         !cas particulier cellule 9 et cellule i (sigma_col>1)
         !les 2 ne peuvent etre liees car pas le meme cote de la surface.
         ICELL = 9                     
         JJ    = ICELL                 
         VAR   = ZERO                  
         DO II=1,9                     
           IF(M(II,JJ)>ZERO)VAR=VAR+ONE                                                         
         ENDDO!next II                 
         IF(NCELL==0)THEN              
           II=MAXLOC(BRICK_LIST(NIN,IB)%POLY(1:9)%OLD_Vnew,1)                                     
           DO I=1,9                    
             IF (I==II) CYCLE          
             M(I,JJ) = ZERO            
           ENDDO                       
         ELSE                          
           IF(VAR>TWO  )THEN       
             M(1:8,JJ) = ZERO    !plusieurs polyedres disparraissent, on ne les compte plus.      
           ELSEIF(VAR==TWO   )THEN  
             !garder le volume le plus gros                                                       
cc             DO II=1,8    ! 1st value is between 1,8 ; last one is at index 9            
cc               VAR = BRICK_LIST(NIN,IB)%POLY(II)%OLD_Vnew                                         
cc               IF(VAR>ZERO)EXIT     
cc             ENDDO                     
cc             IF(VAR>BRICK_LIST(NIN,IB)%POLY(9)%OLD_Vnew)THEN                                   
cc                M(9,JJ)  = ZERO        
cc             ELSE                      
cc                M(II,JJ) = ZERO        
cc             ENDIF                     

             !essai correction 20 avril 2010
             VAR    = BRICK_LIST(NIN,IB)%POLY(1)%OLD_Vnew
             VAR_   = BRICK_LIST(NIN,IB)%POLY(9)%OLD_Vnew            
             lCOND1 = VAR<VAR_         
             VAR    = BRICK_LIST(NIN,IB)%POLY(1)%Vnew 
             VAR_   = BRICK_LIST(NIN,IB)%POLY(9)%Vnew             
             lCOND2 = VAR<VAR_    

             M(9,9) = ZERO  
             
!             IF(lCOND1/=lCOND2)THEN
!               VAR=M(9,1)
!               M(9,1) = M(1,9)
!               M(1,9) = VAR 
!             ENDIF
     
          
                                      
           ENDIF                       
         ENDIF!NCELL==0                
          
       ELSEIF(WASCUT == 0)THEN
         MCELL      = BRICK_LIST(NIN,IB)%MainID
         M(1:9,1:9) = ZERO
         M(1,MCELL) = BRICK_LIST(NIN,IB)%POLY(MCELL)%Vnew
       
       ENDIF       
        
c       IF(IBUG22_TRACKING>0 .OR. IBUG22_TRACKING==-1)then
c          !display matrix    
c          if (ncell > 0)then    
c            write(*,FMT=('(A,9I15           )'))  "                  ", 1,2,3,4,5,6,7,8,9
c            do II=1,9
c              write(*,FMT=('(A,9F15.5       )'))  "                 |", M(II,1:9 ) 
c            enddo    
c          endif
c       ENDIF

        
        
        !calculating old_volume for each cell
        ICELL                 =  0                        
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL           = ICELL + 1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9
          VAR             = ZERO
          VAR_            = ZERO
          JJ              = ICELL
          VFRAC(1:4)      = ZERO
          VAR_VF(1:4)     = ZERO
          VOLD_phase(1:4) = ZERO
          Vj              = BRICK_LIST(NIN,IB)%POLY(JJ)%Vnew
          DO II=1,9
            IF(M(II,JJ)==ZERO)CYCLE
            Vi            = BRICK_LIST(NIN,IB)%POLY(II)%OLD_Vnew
            !Vi : Vprev,  Vj: Vnew for ratio
            VAR           = VAR            +          Vi *Vj / SUM(M(II,:))         !this is not a sum of kronecker symbol since volume were stored instead
          ENDDO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%Vold    =  VAR

c          IF(IBUG22_TRACKING==-1 .OR. IBUG22_TRACKING==IXS(11,brick_list(nin,ib)%id))then
c            write(*,FMT=('(A,I8,A1,I1         )'))  "     brickID     =", ixs(11,id),".",ICELL
c            write(*,FMT=('(A,I8               )'))  "          NBCUT  =", brick_list(nin,ib)%nbcut
c            write(*,FMT=('(A,I8               )'))  "          WASCUT =", brick_list(nin,ib)%wascut          
c            write(*,FMT=('(A,F30.16           )'))  "          VOLD   =", BRICK_LIST(NIN,IB)%POLY(ICELL)%Vold
c            write(*,FMT=('(A,F30.16           )'))  "          VNEW   =", BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew
c            var_              = zero
c            var__             = zero
c            icellTag(1:9)     = 0
c            icellTag_old(1:9) = 0
c            do ii=1,9
c              if(M(ii,ICELL)>ZERO)then
c                var_=var_+ONE
c                icellTag(ii) = 1
c              endif
c            enddo
c            if(var_==ONE)then
c              var__=zero
c              do ii=1,9
c                K = MAXLOC(icellTag,1)
c                if(   M(  K  ,ii)    >  ZERO   )then
c                  var__=var__+ONE
c                  icelltag_old(ii) = 1
c                endif
c              enddo          
c            endif
c            if ( var_ >= TWO )then
c              SOM        = ZERO
c              VFRAC(1:4) = ZERO
c              DO k=1,9
c                if(icelltag(k)==0)CYCLE
c                Vi   = BRICK_LIST(NIN,IB)%POLY(K)%OLD_Vnew
c                SOM = SOM + Vi 
c              enddo
c              write(*,FMT=('(A,I8)'))               "          merge from poly:", IcellTag(1:9)
c            elseif (var_ == ONE .AND. var__==ONE)then     
c              write(*,FMT=('(A,I8)'))               "          cell is stable (same number as previous cycle)", Icelltag(1)               
c            elseif (var__ >= TWO)then         
c              K = MAXLOC(icellTag,1)
c              write(*,FMT=('(A,I8)'))               "          cell is a fragment of a previous polyhedron   ",K             !1seul dans la colonne (var_) et sur la ligne (var__)
c            endif
c          ENDIF   
 
        ENDDO!next ICELL        
      ENDDO !next IB 



      !------------------------------------------------------------!
      ! @30. LINK SWITCHING                                        !
      !      link has change then transportation must occur        !
      !------------------------------------------------------------!
      NBL1 = NBL
      IN   = 0
      debug_outp2 = .false.
      if(IBUG22_LINK_SWITCH/=0)then
        if(itask==0)ALLOCATE (TMP22ARRAY(6,NB))
        call my_barrier
        TMP22ARRAY(1:6,NBF:NBL)=ZERO
        do ib=nbf,nbl
          ie = brick_list(nin,ib)%id
          if(ixs(11,ie)==IBUG22_LINK_SWITCH .OR. IBUG22_LINK_SWITCH==-1)then
            debug_outp2 = .true.
            exit
          endif
        enddo 
      endif!(IBUG22_LINK_SWITCH/=0)
      IF(DT1==ZERO)NBL1=0      
      DO IB=NBF,NBL1
        NCELL               =  BRICK_LIST(NIN,IB)%NBCUT  
        !pAdjCELL            => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%Adjacent_Cell(1:5)          
        !pNAdjCELL           => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%NAdjCell                  
        pAdjBRICK           => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5) 
        pSUBVOL(1:9)        => BRICK_LIST(NIN,IB)%POLY(1:9)
        MBUF                => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1)                         
        MCELL               =  BRICK_LIST(NIN,IB)%MainID
        ID                  =  BRICK_LIST(NIN,IB)%ID
        NG                  =  BRICK_LIST(NIN,IB)%NG
        IDLOC               =  BRICK_LIST(NIN,IB)%IDLOC
        GBUF                => ELBUF_TAB(NGB(IB))%GBUF  
        LBUF                => ELBUF_TAB(NGB(IB))%BUFLY(1)%LBUF(1,1,1)                        
        ICELL               =  0  
c        print *, "check switch in brickID=", ixs(11,brick_list(nin,ib)%id)
        !------LOOP--------!
        NumSECND   =  BRICK_LIST(NIN,IB)%SecndList%Num
        !BRICK_LIST(NIN,IB)%SecndList%ISWITCH(1:NumSECND) = 0
        DO IC=1,NumSECND
          ICELLv   =  BRICK_LIST(NIN,IB)%SecndList%ICELLv(IC)
          IBv      =  BRICK_LIST(NIN,IB)%SecndList%IBv(IC)
          FM       =  BRICK_LIST(NIN,IB)%SecndList%FM(IC)
          FV       =  BRICK_LIST(NIN,IB)%SecndList%FV(IC)
          brickID  =  BRICK_LIST(NIN,IB)%ID
          IBv      =  IBv
          NGv      =  BRICK_LIST(NIN,IBv)%NG
          IDLOCv   =  BRICK_LIST(NIN,IBv)%IDLOC
          IV       =  BRICK_LIST(NIN,IBv)%ID
          ITASKv   =  BRICK_LIST(NIN,IBv)%ITASK
          ! loop over the nodes of the adjacent secondary cell
          NNODES   = BRICK_LIST(NIN,IBv)%POLY(ICELLV)%NumNOD
          J1       = 0
          J2       = 0 
          DO K1=1,NNODES !Nodes attached to IB
            INOD   = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%ListNodID(K1)
            FV_old = BRICK_LIST(NIN,IBv)%NODE(INOD)%WhereWasMain !FACE
            IF (FV_old == 0)CYCLE !the node was wet by main cell : MERGE PROCESS APPLIES (see related chapter)
            !OLD_M
            IF (FV_old == FV)CYCLE ! NO LINK CHANGE : this is still linked to the same direction
            !else FV_old /= FV : THERE WAS A SWITCH
            ! GET PREVIOUS FACE TO GET PREVIOUS main CELL
            IF(FV_old<=NV46)THEN
              IBmo   = BRICK_LIST(NIN,IBv)%Adjacent_Brick(FV_old,4)
            ELSE
              J      = FV_old
              J1     = J/10
              J2     = MOD(J,10)
              IBv    = BRICK_LIST(NIN,IBv)%Adjacent_Brick(J1,4)
              IBmo   = BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,4)                           
            ENDIF
                        
            IBm    = IB
            LLTm   = IPARG(2,NGB(IB))
            ! IF(IBm==IBmo)CYCLE : impossible it meansFV_old=FV
            lCYCLE  = .FALSE.
            NumTarget = BRICK_LISt(NIN,IBmo)%Ntarget
            IF(NumTarget>=2)print *, "**inter22 - Warning Multiple targets",ixs(11,BRICK_LIST(NIN,IBv)%ID), ICELLv
            DO ITARGET=1,NumTarget
              IF (BRICK_LISt(NIN,IBmo)%MergeTarget(3,ITarget)==IBm)THEN
                lCYCLE = .TRUE.
                EXIT
              ENDIF
              IBmo = BRICK_LIST(NIN,IBmo)%MergeTarget(3,ITarget)   !en tout rigueur il faudrait prelever dans toutes les target, ici on prend le premier
              print *, "**inter22 : multiple targets", ixs(11,BRICK_LIST(NIN,IBv)%ID), ICELLv
            ENDDO
            if(lCYCLE)CYCLE
            lCYCLE = .FALSE.
            !remove salve cell volume from IBMo(Vold) and add it in IBo(Vold)
            !addition is treated now since only current thread is handling this cell IBm \in [NBF,NBL]
            !search corresponding secnd cell in IBmo%old_secnds_lit
            lFOUND    = .FALSE. !if found old salve cell attached to INOD
            NumSecnd2 = OLD_SecndList(NIN,IBmo)%Num
            NGo       = BRICK_LIST(NIN,IBmo)%NG
            LLTo      = IPARG(2,NGo)
            DO IC2 = 1, NumSecnd2
              !get previous secnd cell
              FV2      = OLD_SecndList(NIN,IBmo)%FV(IC2)
              ICELLv2  = OLD_SecndList(NIN,IBmo)%ICELLv(IC2)              
              IBv2     = OLD_SecndList(NIN,IBmo)%IBv(IC2) 
              !Si la cellule escalve etait liee par une autre face que FV_old alors ce n'etait pas la bonne cellule main et on passe outres
              IF(FV2 /= FV_old )CYCLE
              !la cellule second. est attachee a la cellule main qui nous interesse IBmo
              !si on arrive ici c'est que la cellule second. (0, en partie, ou en totalite) change de liaison
              !on verifie si un de ses noeuds correspond au noeud INOD de la nouvelle cellule escalve dans IBv
              !on boucle sur les noeuds sommets de l'ancienne  cellule pour voir quel noeud sommet a commute
              VolCELL  = ZERO
              VolSECND = ZERO
              NNODES2  = OLD_SecndList(NIN,IBmo)%NumNOD_Cell(IC2)
              DO K2=1,NNODES2
                INOD2  = OLD_SecndList(NIN,IBmo)%ListNodID(IC2,K2)
                ICELL2 = BRICK_LIST(NIN,IBv2)%NODE(INOD2)%WhichCell
                IBmCur = BRICK_LIST(NIN,IBv2)%POLY(ICELL2)%WhereIsMain(4)
                IF(IBmCur == IB) VolSECND = VolSECND + ONE/NNODES*BRICK_LIST(NIN,IBv2)%POLY(ICELL2)%Vnew   !example brick 20 devient main dans cas 15.14 : le penta dans 41 a deux noeuds, si on boucle 2 fois on prend 2 fois son volume pour l'injecter dans bric 30.
                VolCELL = VolCELL + ONE/NNODES*BRICK_LIST(NIN,IBv2)%POLY(ICELL2)%Vnew
              ENDDO
              if (VolSECND == ZERO)CYCLE
              !BRICK_LIST(NIN,IB)%SecndList%ISWITCH(IC) = 1
              !print *, ""
              !print *, "VolCell                          =", VolCell
              !print *, "VolSECND                         =", VolSECND
              RATIO =  VolSECND/VolCELL !est le ratio de prelevement dans le volume de l'ancienne cellule second.            
              !here IC2(cycle-1) corresponds to IC(cycle+0)
              VOLcell =  RATIO * OLD_SecndList(NIN,IBmo)%VOL(IC2)
              VOLD    =  BRICK_LIST(NIN,IBm)%Vold_SCell
              GBUFo   => ELBUF_TAB(NGB(IBmo))%GBUF  
              !---A.MATERIAL MERGING---!
              !Ratio is <1 in case 1 secnd split into several secnd cells which different main links, then collect only relevant quantity not all from old secnd cell.
              EINT                            = VOLcell * GBUFo%EINT(IDLOCB(IBmo))            
              RHO                             = VOLcell * GBUFo%RHO(IDLOCB(IBmo))             
              MOM(1)                          = VOLcell * GBUFo%MOM(LLTo*(1-1) + IDLOCB(IBmo))         
              MOM(2)                          = VOLcell * GBUFo%MOM(LLTo*(2-1) + IDLOCB(IBmo))         
              MOM(3)                          = VOLcell * GBUFo%MOM(LLTo*(3-1) + IDLOCB(IBmo))                                     
              DO J=1,NV46
                SIG(J)                        = VOLcell * GBUFo%SIG(LLTo*(J-1)+IDLOCB(IBmo))     
              ENDDO
              GBUF%EINT(IDLOCB(IBm))          = (GBUF%EINT(IDLOCB(IBm)) * VOLD + EINT) / (VOLD+VOLcell)
              GBUF%RHO(IDLOCB(IBm))           = (GBUF%RHO(IDLOCB(IBm))  * VOLD + RHO)  / (VOLD+VOLcell)              
              GBUF%MOM(LLTm*(1-1) + IDLOCB(IBm)) = (GBUF%MOM(LLTm*(1-1) + IDLOCB(IBm))  * VOLD + MOM(1))  / (VOLD+VOLcell)
              GBUF%MOM(LLTm*(2-1) + IDLOCB(IBm)) = (GBUF%MOM(LLTm*(2-1) + IDLOCB(IBm))  * VOLD + MOM(2))  / (VOLD+VOLcell)
              GBUF%MOM(LLTm*(3-1) + IDLOCB(IBm)) = (GBUF%MOM(LLTm*(3-1) + IDLOCB(IBm))  * VOLD + MOM(3))  / (VOLD+VOLcell)
              ALEFVM_Buffer%FCELL(4, brick_list(nin,ibm)%id )= GBUF%RHO(IDLOCB(IBm))                                                       
              ALEFVM_Buffer%FCELL(1, brick_list(nin,ibm)%id )= GBUF%MOM(LLTm*(1-1) + IDLOCB(IBm))
              ALEFVM_Buffer%FCELL(2, brick_list(nin,ibm)%id )= GBUF%MOM(LLTm*(2-1) + IDLOCB(IBm))
              ALEFVM_Buffer%FCELL(3, brick_list(nin,ibm)%id )= GBUF%MOM(LLTm*(3-1) + IDLOCB(IBm))
              
              !FCELL(5, brick_list(nin,ibm)%id) = ! see mtn 37 conditional block below
              !FCELL(6, brick_list(nin,ibm)%id) = ! idem
              
              DO J=1,NV46
                GBUF%SIG(LLTm*(J-1)+IDLOCB(IBm)) = (GBUF%SIG(LLTm*(J-1)+IDLOCB(IBm))  * VOLD + SIG(J))  / (VOLD+VOLcell)    
              ENDDO
              
              if(debug_outp2)then
              !!!write (*,FMT='(A)') " === LINK SWITCH ==="
              IN = IN + 1
              endif
      
             if(IBUG22_LINK_SWITCH==ixs(11,brick_list(nin,ib)%id) .or. IBUG22_LINK_SWITCH==-1 )then
               ! print *, "brick target                =", ixs(11,brick_list(nin,ib)%id)
               ! print *, "brick origin                =", ixs(11,brick_list(nin,ibmo)%id)
               ! print *, "adding",VOLcell          ,'to',BRICK_LIST(NIN,IBm)%Vold_SCell 
               ! print *, "updated target -old volume- =",BRICK_LIST(NIN,IBm)%Vold_SCell +VOLcell 
                TMP22ARRAY(1,IBm)= ixs(11,brick_list(nin,ib)%id)
                TMP22ARRAY(2,IBm)= ixs(11,brick_list(nin,ibmo)%id)
                TMP22ARRAY(3,IBm)= ixs(11,brick_list(nin,ibm)%id)
                TMP22ARRAY(4,IBm)= BRICK_LIST(NIN,IBm)%Vold_SCell
                TMP22ARRAY(5,IBm)= BRICK_LIST(NIN,IBm)%Vold_SCell +VOLcell 
                TMP22ARRAY(6,IBm)= VOLcell                    
              endif
                            
              !GBUF%VOL(IDLOCB(IBm)) = GBUF%VOL(IDLOCB(IBm)) + VOLcell
              BRICK_LIST(NIN,IBm)%Vold_SCell = BRICK_LIST(NIN,IBm)%Vold_SCell + VOLcell 

              MTN_ = IPARG(1,NGB(IBmo)) 
              VOLcell51(1:TRIMAT) = ZERO 
              IF(MTN_==37)THEN
                MBUF                       => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1) 
                MT                         =  IXS(1,brickID)
                IADBUF                     =  IPM(7,MT) 
                RHO10                      =  BUFMAT(IADBUF-1+11)
                RHO20                      =  BUFMAT(IADBUF-1+12)              
                !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multiplying by element volume in aleconve.F)
                !UVAR(I,2) : density of gas
                !UVAR(I,3) : density of liquid
                !UVAR(I,4) : volumetric fraction of liquid  
                !UVAR(I,5) : volumetric fraction of gas  
                MBUF   => ELBUF_TAB(NGB(IBm))%BUFLY(1)%MAT(1,1,1)  
                MBUFo  => ELBUF_TAB(NGB(IBmo))%BUFLY(1)%MAT(1,1,1)
                LLT_   = IPARG(2,NGB(IBm)) 
                LLT_o  = IPARG(2,NGB(IBmo))
                  
                ! UVAR(5) - alpha_gas
                pVAR   =>  MBUF%VAR((5-1)*LLT_ +IDLOCB(IBm)) 
                pVARo  => MBUFo%VAR((5-1)*LLT_o+IDLOCB(IBmo)) 
C                print *, "link switch 5", pVAR, (pVAR * VOLD + VOLcell * pVARo)/ (VOLD+VOLcell)               
                pVAR   = (pVAR * VOLD + VOLcell * pVARo)/ (VOLD+VOLcell)
                pVAR   = MAX(pVAR,ZERO)
                
                ! UVAR(4) - alpha_liquid
                pVAR   =>  MBUF%VAR((4-1)*LLT_ +IDLOCB(IBm)) 
                pVARo  => MBUFo%VAR((4-1)*LLT_o+IDLOCB(IBmo))   
C                print *, "link switch 4", pVAR, (pVAR * VOLD + VOLcell * pVARo)/ (VOLD+VOLcell)                             
                pVAR   = (pVAR * VOLD + VOLcell * pVARo)/ (VOLD+VOLcell)
                pVAR   = MAX(pVAR,ZERO)
                
                ! UVAR(3) - rho_liquid
                pVAR   =>  MBUF%VAR((3-1)*LLT_ +IDLOCB(IBm)) 
                pVARo  => MBUFo%VAR((3-1)*LLT_o+IDLOCB(IBmo))    
                ALP    =   MBUF%VAR((4-1)*LLT_ +IDLOCB(IBm))
                ALPo   =  MBUFo%VAR((4-1)*LLT_o+IDLOCB(IBmo))      
C                print *, "link switch 3", pVAR, (pVAR*ALP*VOLD+VOLcell*pVARo*ALPo  /(VOLD+VOLcell) 
                pVAR   = (pVAR*ALP*VOLD+VOLcell*pVARo*ALPo)
                IF(pVAR>ZERO)pVAR=pVAR/(ALP*VOLD+ALPo*VOLcell)        
                pVAR   = MAX(pVAR,ZERO)                       
                !cts14june2017!
                IF( pVAR == ZERO) pVAR = RHO10  
                
                ! UVAR(2) - rho_gas
                pVAR   =>  MBUF%VAR((2-1)*LLT_ +IDLOCB(IBm)) 
                pVARo  => MBUFo%VAR((2-1)*LLT_o+IDLOCB(IBmo))    
                ALP    =   MBUF%VAR((5-1)*LLT_ +IDLOCB(IBm))
                ALPo   =  MBUFo%VAR((5-1)*LLT_o+IDLOCB(IBmo))           
C                print *, "link switch 2", pVAR, (pVAR*ALP*VOLD+VOLcell*pVARo*ALPo  /(VOLD+VOLcell) 
                pVAR   = (pVAR*ALP*VOLD+VOLcell*pVARo*ALPo)
                IF(pVAR>ZERO)pVAR=pVAR/(ALP*VOLD+ALPo*VOLcell)       
                pVAR   = MAX(pVAR,ZERO)      
                !cts14june2017!
                IF( pVAR == ZERO) pVAR = RHO20 
                       
                ! UVAR(1) - rho1.V1/V = rho1.alpha_liquid
                pVAR   =>  MBUF%VAR((1-1)*LLT_ +IDLOCB(IBm)) 
                pVARo  => MBUFo%VAR((1-1)*LLT_o+IDLOCB(IBmo))  
C                print *, "link switch 1", pVAR, (pVAR * VOLD + VOLcell * pVARo)/ (VOLD+VOLcell)
                pVAR   = (pVAR * VOLD + VOLcell * pVARo)/ (VOLD+VOLcell)  
                pVAR   = MAX(pVAR,ZERO) 
                
                !GBUF%RHO(IDLOCB(IBm))           = (GBUF%RHO(IDLOCB(IBm))  * VOLD + RHO)  / (VOLD+VOLcell)
                


               !APPEL SIGEPS37 POUR EQUILIBRE PRESSION
               brickID = BRICK_LIST(NIN,IBm)%ID
               IDLOC    = IDLOCB(IBm)
               NG       = NGB(IBm)
               VOL      = VOLD+VOLcell
               CALL SIGEPS37_SINGLE_CELL (
     1                                     ELBUF_TAB, IXS,  BUFMAT,  IPARG, IPM,
     2                                     IDLOC    , NG ,  brickID, VOL  
     .                                     )   
     
                
              ELSEIF(MTN_==51)THEN
                MBUF  => ELBUF_TAB(NGB(IBm))%BUFLY(1)%MAT(1,1,1)               
                LLT_  = IPARG(2,NGB(IBm))               
                MBUFo  => ELBUF_TAB(NGB(IBmo))%BUFLY(1)%MAT(1,1,1)               
                LLT_o  = IPARG(2,NGB(IBmo))    
                          
                DO ITRIMAT=1,TRIMAT
                  !IBm_old - volume fraction!                
                  IPOS                     = 1                                 
                  K0                       = N0PHAS + (ITRIMAT-1)*NVPHAS +IPOS-1   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}   
                  K                        = K0 * LLT_o                         
                  VFrac(ITRIMAT)           = MBUFo%VAR(K+IDLOCB(IBmo))            
                ENDDO!next ITRIMAT 
                
                DO ITRIMAT=1,TRIMAT
                  IPOS                     = 11                                
                  K0                       = N0PHAS + (ITRIMAT-1)*NVPHAS +IPOS-1   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}   
                  K                        = K0 * LLT_
                  VOLcell51(ITRIMAT)       = VOLcell * VFrac(ITRIMAT)         
                  MBUF%VAR(K+IDLOCB(IBm))   = MBUF%VAR(K+IDLOCB(IBm)) + VOLcell51(ITRIMAT)        
                ENDDO!next ITRIMAT 
                
                DO ITRIMAT=1,TRIMAT
                  !IBm - volume fraction!
                  IPOS                     = 1                                 
                  K0                       = N0PHAS + (ITRIMAT-1)*NVPHAS +IPOS-1   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}   
                  K                        = K0 * LLT_                         
                  VFrac(ITRIMAT)           = MBUF%VAR(K+IDLOCB(IBm))                   
                  IF(VOLcell51(ITRIMAT)<=ZERO)CYCLE  !unplug if nothiong to add (otherwise div by zero)
                  DO IPOS = 1,NVPHAS   
                    IF(IPOS==11)CYCLE                                                  !volume already treated   
                    K2                    =  ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)} 
                    K                     =  LLT_ * K2                                              
                    Ko                    =  LLT_o * K2                     
                    pVAR                  => MBUF%VAR(K+IDLOCB(IBm))  
                    pVAR                  =  ( pVAR * VFrac(ITRIMAT)*VOLD + VOLcell51(ITRIMAT)*MBUFo%VAR(Ko+IDLOCB(IBmo)) ) 
                    pVAR                  =  pVAR / (VFrac(ITRIMAT)*VOLD + VOLcell51(ITRIMAT))
                   ENDDO!next IPOS 
                ENDDO!next ITRIMAT                 
 
              ENDIF! law37 | law51
              !---THREAD TAG---!
C              print *, " --- set VOLD_L += VOLcell, IB, ID ", VOLcell , IBMo, ixs(11,brick_list(nin,ibmo)%ID)
              VOLD_L(ITASK,0,IBmo)          = VOLD_L(ITASK,0,IBmo)        + VOLcell                           
              VOLD_L(ITASK,1:TRIMAT,IBmo)   = VOLD_L(ITASK,1:TRIMAT,IBmo) + VOLcell51(1:TRIMAT)           
            ENDDO!next IC2
            EXIT !next IC (since this one is treated)
          ENDDO!next K1         
        ENDDO!next IC
      ENDDO!next IB
      
      CALL MY_BARRIER
      
             if(IBUG22_LINK_SWITCH/=0 .AND. ITASK==0 .and. DT1>ZERO)THEN
                write (*,FMT='(A)') " === LINK SWITCH ==="
                DO IB=1,NB
                  IF(TMP22ARRAY(1,IB)==0)CYCLE
                   print *, "brick target                =", TMP22ARRAY(1,IB)
                   print *, "brick origin                =", TMP22ARRAY(2,IB)
                   print *, "brick main                =", TMP22ARRAY(3,IB)
                   print *, "adding",TMP22ARRAY(6,IB)   ,'to', TMP22ARRAY(4,IB)
                   print *, "updated target -old volume- =", TMP22ARRAY(5,IB)                          
                ENDDO                
              endif
              call my_barrier

      !print
      if(itask==0)then
      if(IBUG22_LINK_SWITCH/=0)then  
      DO IB=1,NB           
        DO IT = 0,NTHREAD-1
          IF (VOLD_L(IT,0,IB) == ZERO)CYCLE
            print *, ""        
            print *, "  brick ID           =", ixs(11,brick_list(nin,ib)%id)
            print *, "  removing            ",VOLD_L(IT,0,IB),'from',BRICK_LIST(NIN,IB)%Vold_SCell
            print *, "  new origin volume  =", ixs(11,brick_list(nin,ib)%id)
            print *, "  %vold, %vnew        " , BRICK_LIST(NIN,IB)%Vold_SCell- VOLD_L(IT,0,IB), BRICK_LIST(NIN,IB)%Vnew_SCell
            print *, ""
        ENDDO
      ENDDO
      endif
      endif
      call my_barrier
      
      !---B.OLD VOLUME UPDATE---!
      !substract only on current thread
      DO IB=NBF,NBL1            
        !GBUF             => ELBUF_TAB(NGB(IB))%GBUF
        DO IT = 0,NTHREAD-1
          IF (VOLD_L(IT,0,IB) == ZERO)CYCLE
            if(IBUG22_LINK_SWITCH==ixs(11,brick_list(nin,ib)%id)  .or. IBUG22_LINK_SWITCH==-1 )then  
            !print *, ""        
            !print *, "  brick ID           =", ixs(11,brick_list(nin,ib)%id)
            !print *, "  removing            ",VOLD_L(IT,0,IB),'from',BRICK_LIST(NIN,IB)%Vold_SCell
            !print *, "  new origin volume  =", ixs(11,brick_list(nin,ib)%id)
            !print *, "  %vold, %vnew        " , BRICK_LIST(NIN,IB)%Vold_SCell- VOLD_L(IT,0,IB), BRICK_LIST(NIN,IB)%Vnew_SCell
            !print *, ""
          endif
          
          BRICK_LIST(NIN,IB)%Vold_SCell = BRICK_LIST(NIN,IB)%Vold_SCell - VOLD_L(IT,0,IB)
                    
          !!!!!!!!!!!!BRICK_LIST(NIN,IB)%Vold_SCell = BRICK_LIST(NIN,IB)%Vold_SCell - VOLD_L(IT,0,IB)
          MTN_ = IPARG(1,NGB(IB)) 
          IF(MTN_==51)THEN             
            MBUF  => ELBUF_TAB(NGB(IB))%BUFLY(1)%MAT(1,1,1)               
            LLT_  = IPARG(2,NGB(IB))  
            DO ITRIMAT=1,TRIMAT                                                         
              IPOS                     = 11                                             
              K0                       = N0PHAS + (ITRIMAT-1)*NVPHAS +IPOS-1   ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}                
              K                        = K0 * LLT_       
              MBUF%VAR(K+IDLOCB(IB))   = MBUF%VAR(K+IDLOCB(IB)) - VOLD_L(IT,ITRIMAT,IB)
            ENDDO!next ITRIMAT      
          ENDIF!(MTN_==51)


ccc          if(IBUG22_LINK_SWITCH==ixs(11,brick_list(nin,ib)%id) .or. IBUG22_LINK_SWITCH==-1 )then          
ccc              print *, " --reset VOLD_L  =    ZERO, IB, ID ", ZERO , IB, ixs(11,brick_list(nin,ib)%ID)
ccc          endif                                                  
          VOLD_L(IT,0:MAX(0,TRIMAT),IB)     = ZERO  !reset VOLD_L for next cycle          
        ENDDO!next IT
      ENDDO!next IB
     
     
     
     
      if(IBUG22_LINK_SWITCH/=0)THEN
        call my_barrier
        if(itask==0)DEALLOCATE (TMP22ARRAY)
      endif
                    
      !------------------------------------------------------------!
      ! @32. LEVEL2 NEIGHBORHOOD (FOR FLUXES & CONVECTION)         !
      !------------------------------------------------------------!          
      DO IB=NBF,NBL
        NBCUT          =  BRICK_LIST(NIN,IB)%NBCUT  
        pIsMain(1:9)  => BRICK_LIST(NIN,IB)%POLY(1:9)     
        !pAdjCELL       => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%Adjacent_Cell(1:5)  
        pAdjBRICK      => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5) 
        pMainID      => BRICK_LIST(NIN,IB)%MainID                               
        NCELL          =  NBCUT
        ICELL          =  0  
        IE             =  BRICK_LIST(NIN,IB)%ID
        !on traite les cellules mains pour eviter les lockon/lockoff (en evitant boucle sur les escalves)
        MCELL          = pMainID
        IF(MCELL == 0)CYCLE ! next hexahedron cell
        BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num  = 0      
        DO J=1,6
          IV         =  pAdjBRICK(J,1)
          NGv        =  pAdjBRICK(J,2)
          IDLOCv     =  pAdjBRICK(J,3)
          IBV        =  pAdjBrick(J,4)
          IF(IBV>0)THEN !if adjacent brick to face J is in cut cell buffer
            NBCUTv = BRICK_LIST(NIN,IBv)%NBCUT
            IF(NBCUTv==0)CYCLE
            NadjCell =  BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%NAdjCell
            DO IC=1,NadjCell            
              ICELLv          =  BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%Adjacent_Cell(IC)
              MCELLv          =  BRICK_LIST(NIN,IBv)%MainID
              IF(ICELLv == MCELLv)CYCLE
              !this adjacent cell is a secnd one, use it as bridge to a new adjacent main cell.
              !pWhereIsMainV => BRICK_LIST(NIN,IBv)%POLY(1:9)%WhereIsMain(1:4)
              JV              =  BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(1) !   pWhereIsMainV(ICELLv,1)           
              IDM             =  BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(3)  !   pWhereIsMainV(ICELLv,3)  
              IF(IE/=IDM)THEN !secnd cell merged to the main IB
               !prendre tous les voisins main de la cellule salve sauf face JV
               !ajouter un seul voisin celui au quel la secnd cell est mergee
                BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num          = BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num + 1
                K                                         = BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num
                BRICK_LIST(NIN,IB)%ADJ_ELEMS%IV(K)        = IDM
                BRICK_LIST(NIN,IB)%ADJ_ELEMS%IB(K)        = IBM
                BRICK_LIST(NIN,IB)%ADJ_ELEMS%ICELL(K)     = ICELLv                        
                BRICK_LIST(NIN,IB)%ADJ_ELEMS%SecndFACE(K) = JV   !to retrieve cut face area
              ELSE! !secnd cell merged to main /= IB
               pAdjBRICKv  => BRICK_LIST(NIN,IBv)%Adjacent_Brick(1:6,1:5)               
               DO J2=1,6 !loop on IV.icellv (secnd) adjacent bricks
                 IF (J2==JV)CYCLE !behind JV there is IB itself, so skip it
                 IVv     =  pAdjBRICK(J2,1)
                 NGvv    =  pAdjBRICK(J2,2)
                 IDLOCvv =  pAdjBRICK(J2,3)
                 IBvv    =  pAdjBRICK(J2,4)
                 IFvv    =  pAdjBRICK(J2,5) 
                 IF(IVV == 0)CYCLE
                 
                 IF(IBvv > 0)THEN
                   MCELLvv = BRICK_LIST(NIN,IBvv)%MainID  
                   !if main IVV.MCELvv has connexion with IV.ICELLv then add it
                   NAdjCellv = BRICK_LIST(NIN,IBvv)%POLY(MCELLvv)%FACE(IFvv)%NAdjCell
                   DO K=1,NAdjCellv
                     ICv = BRICK_LIST(NIN,IBvv)%POLY(MCELLvv)%FACE(IFvv)%Adjacent_Cell(K)
                     IF(ICv/=ICELLv)CYCLE
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num           = BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num + 1
                       K2                                         = BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%IV(K2)        = Ivv
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%IB(K2)        = IBvv
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%ICELL(K2)     = ICELLv !=ICv                        
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%SecndFACE(K2) = J2  !to retrieve cut face area
                       EXIT
                   ENDDO!next K
                 ELSEIF(IBvv == 0)THEN                   
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num           = BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num + 1
                       K2                                         = BRICK_LIST(NIN,IB)%ADJ_ELEMS%Num
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%IV(K2)        = Ivv
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%IB(K2)        = IBvv
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%ICELL(K2)     = 1                        
                       BRICK_LIST(NIN,IB)%ADJ_ELEMS%SecndFACE(K2) = J2  !to retrieve cut face area                  
                 ENDIF
               ENDDO!next J2
              ENDIF!(IE==IDM)
            ENDDO!next IC         
          ENDIF 
        ENDDO!next J      
      ENDDO
      


      !------------------------------------------------------------!
      ! @32. COUNT POINTS ON A GIVEN POLYHEDRON FACE               !
      !------------------------------------------------------------!
      DO IB=NBF,NBL
        NBCUT          = BRICK_LIST(NIN,IB)%NBCUT
        MCELL          = BRICK_LIST(NIN,IB)%MainID
        NCELL          = BRICK_LIST(NIN,IB)%NBCUT
        ICELL          = 0  
        NINTP(1:6,1:9) = 0
        NNOD(1:6,1:9)  = 0
        IF(NBCUT == 0) THEN
          BRICK_LIST(NIN,IB)%POLY(1)%FACE(1:6)%NumPOINT=(/4,4,4,4,4,4/)
          CYCLE        
        ENDIF                             
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL + 1   
          IF (ICELL>NCELL .AND. NCELL/=0)THEN
            ICELL=9 
            CYCLE
          ENDIF
          SECid = BRICK_LIST(NIN,IB)%SECID_Cell(ICELL)          
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%NumPOINT = 0          
          DO J=1,6         
            JJ = Gface(J,SECid)
            IF(JJ==0)EXIT            
            NP = GNPt(J,SECid)
            NN = GNnod(J,SECid)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(JJ)%NumPOINT = NP
            NINTP(JJ,ICELL) = NP-NN !only intersection point
            NNOD(JJ,ICELL)    = NN
          ENDDO     
        ENDDO!next ICELL 
        IF(ICELL==9)THEN
          DO J=1,6
            !sommets
            NN = SUM( NNOD(J,1:NCELL) )
            NN = 4 - NN !complementary
            !points dintersection
            NP = SUM( NINTP(J,1:NCELL) )
            !nombre total de point = sommets + intersection pt
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NumPOINT = NP + NN
          ENDDO
        ENDIF      
      ENDDO   
      
      !------------------------------------------------------------!
      ! @33. LOCATE WHERE WAS main FOR NEXT CYCLE                !
      !      & TAG ITS BRICK NODES   (update in i22intersect.F)    !
      !------------------------------------------------------------! 
      DO IB=NBF,NBL
         pWhereWasMain(1:8) => BRICK_LIST(NIN,IB)%NODE(1:8)!%WhereWasMain
        !pWhereIsMain  => BRICK_LIST(NIN,IB)%POLY(1:9)%WhereIsMain(1:4)        
         pListNodID(1)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)
         pListNodID(2)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(2)%ListNodID(1:8) 
         pListNodID(3)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(3)%ListNodID(1:8)
         pListNodID(4)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(4)%ListNodID(1:8)
         pListNodID(5)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(5)%ListNodID(1:8)
         pListNodID(6)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(6)%ListNodID(1:8) 
         pListNodID(7)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(7)%ListNodID(1:8) 
         pListNodID(8)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(8)%ListNodID(1:8)  
         pListNodID(9)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(9)%ListNodID(1:8)          
        pNodWasMain(1:8)   => BRICK_LIST(NIN,IB)%NODE(1:8)!%NodWasMain                 
        NCELL           =  BRICK_LIST(NIN,IB)%NBCUT
        ICELL           =  0   
        MCELL           =  BRICK_LIST(NIN,IB)%MainID
        ! nodes wet by main cell
        MNOD = BRICK_LIST(NIN,IB)%POLY(MCELL)%NumNOD           
        pNodWasMain(1:8)%NodWasMain   = 0
        pWhereWasMain(1:8)%WhereWasMain = 0
        DO J=1,MNOD
          pNodWasMain(pListNodID(MCELL)%p(J))%NodWasMain = 1        
        ENDDO
        ! locate main cell for nodes of a given secnd cell
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9          
          IF(ICELL == MCELL)CYCLE   
          IPOS = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1)  !pWhereIsMain(ICELL,1)           
          MNOD = BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD
          !All polyhedron nodes have the same main cell : face=IPOS
          DO J=1,MNOD
            pWhereWasMain(pListNodID(ICELL)%p(J))%WhereWasMain = IPOS
          ENDDO!next J
        ENDDO!next ICELL 
      ENDDO      
      
                    

      !------------------------------------------------------------!
      ! @34. SET IIAD22(1:NUMELS) = %ELBUG_TAB(NG)%TAG22(I)        !
      !------------------------------------------------------------!
      DO IB=NBF,NBL  
        IE = BRICK_LIST(NIN,IB)%ID
        IIAD22(NIN, IE) = IB
      ENDDO                                                      

      !------------------------------------------------------------!
      ! @35. SET MLW                                               !
      !------------------------------------------------------------!
      DO IB=NBF,NBL  
        MLW = IPARG(1,NGB(IB)) 
        BRICK_LIST(NIN,IB)%MLW = MLW
      ENDDO    
      
      !------------------------------------------------------------!
      ! @36. RESET OLD SECND LIST CONNECTIVITY                     !
      !      WILL BE DEFINED IN I22_GET_PRV_DATA()                 !
      !------------------------------------------------------------!
      !RESET ONLY NON ZERO VALUES
      DO IB=NBF,NBL 
        NumSECND = OLD_SecndList(NIN,IB)%Num
        IF (NumSECND==0)CYCLE
        OLD_SecndList(NIN,IB)%Num                = 0
        OLD_SecndList(NIN,IB)%NumSecndNodes      = 0      
        DO J=1,24
          OLD_SecndList(NIN,IB)%FM(J)            = 0            
          OLD_SecndList(NIN,IB)%FV(J)            = 0
          OLD_SecndList(NIN,IB)%IV(J)            = 0
          OLD_SecndList(NIN,IB)%IBV(J)           = 0
          OLD_SecndList(NIN,IB)%ICELLv(J)        = 0
          OLD_SecndList(NIN,IB)%VOL(J)           = ZERO
          OLD_SecndList(NIN,IB)%NumNOD_Cell(J)   = 0
          OLD_SecndList(NIN,IB)%ListNodID(J,1:8) = 0          
        ENDDO             
      ENDDO!next IB      

      !------------------------------------------------------------!
      ! @37. SET main STRONG NODE                                !
      !------------------------------------------------------------!
      DO IB=NBF,NBL 
        MCELL = BRICK_LIST(NIN,IB)%mainID 
        CALL weighting_Cell_Nodes(NIN,IB,MCELL,INod_W, IDEMERGE)
        IF(MCELL==0)THEN
          BRICK_LIST(NIN,IB)%OldMainStrongNode = 0
        ELSE
          BRICK_LIST(NIN,IB)%OldMainStrongNode = INOD_W
        ENDIF
      ENDDO  

      !------------------------------------------------------------!
      ! @38. DRAW SUPERCELL CONNEXION WITH 1D TRUSSES              !
      !------------------------------------------------------------!
      lStillTruss = .TRUE.
      IGR         = IPARI(52) !reserved GRTRUS  ---> truss group identifier
      NTRUS       = IPARI(53 )!reserved GRTRUS  ---> truss group NEL
      IF( ITASK==0 .AND. NTRUS/=0 )THEN
        II        = 1  ! first node of group node
        DO IB=NBF,NBL 
          MCELL = BRICK_LIST(NIN,IB)%mainID
          IF (MCELL==0              ) CYCLE
          IF (.NOT.lStillTruss) CYCLE
          Point0(1:3) = BRICK_LIST(NIN,IB)%POLY(MCELL)%CellCenter(1:3)

          !LOOP ON SECND CELLS TO DRAW CONNEXION
          NumSECND = BRICK_LIST(NIN,IB)%SecndList%Num
          DO ISecnd=1,NumSecnd
            IBv           = BRICK_LIST(NIN,IB)%SecndList%IBV(ISecnd)   
            ICELLv        = BRICK_LIST(NIN,IB)%SecndList%ICELLv(ISecnd)
            PointTMP(1:3) = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%CellCenter(1:3)
            !check if group has still one truss 
            IF (II>NTRUS) THEN
              lStillTruss=.FALSE.
              EXIT
              print *, "** Warning inter22 : no more truss in group to mark cell links"
            ENDIF
            X(1:3,IXT(2,IGRTRUSS(IGR)%ENTITY(II))) = Point0(1:3)
            X(1:3,IXT(3,IGRTRUSS(IGR)%ENTITY(II))) = PointTMP(1:3)
            !write (*,*) "set TRUS_id=", IXT(NIXT,IGRTRUSS(IGR)%ENTITY(II)) ,"  corr=(", Point0(1:3), ")-(" ,PointTMP(1:3) ,")"
            if(ibug22_truss==1)then
              write (*,FMT='(A,I10,A,I10,A,I10)') "set TRUS_id=", IXT(NIXT,IGRTRUSS(IGR)%ENTITY(II)) ,
     .        " main=", ixs(11,brick_list(nin,ib)%id)  ," secnd=", ixs(11,brick_list(nin,ibv)%id)
            endif   
            II = II + 1 !next truss 
          ENDDO                                
        ENDDO !next IB 
        DO II = 1,NTRUS
            !print *, "reset TRUS_id=", IXT(NIXT,IGRTRUSS(IGR)%ENTITY(II))
            X(1:3,IXT(2,IGRTRUSS(IGR)%ENTITY(II))) = (/ZERO, ZERO, ZERO/)
            X(1:3,IXT(3,IGRTRUSS(IGR)%ENTITY(II))) = (/  ONE, ZERO, ZERO/)
        ENDDO
      ENDIF !ITASK==0


      !------------------------------------------------------------!
      ! @39. COMPUTE DVOL PREDICTION FOR EACH CELL                 !
      !------------------------------------------------------------!
      DO IB=NBF,NBL      
        NBCUT       = BRICK_LIST(NIN,IB)%NBCUT                                      
        ICELL       = 0  
        NCELL       = NBCUT
        IF(NBCUT == 0)THEN
          ICELL = 1
          BRICK_LIST(NIN,IB)%POLY(ICELL)%DVOL(1) = ZERO    
          CYCLE
        ENDIF                      
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9          
          IF(ICELL == 9)THEN
            BRICK_LIST(NIN,IB)%POLY(9)%DVOL(1) = -SUM (BRICK_LIST(NIN,IB)%POLY(1:NBCUT)%DVOL(1))
          ELSE
            ! V_face
            VSUM(1)     =  BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1)    
            VSUM(2)     =  BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(2)
            VSUM(3)     =  BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(3)
            !VSUM(1)     =  BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%VEL(1)                      
            !VSUM(2)     =  BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%VEL(2)                      
            !VSUM(3)     =  BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%VEL(3)                      
            !not unitary : NN= 2Sn                                                    
            N_(1)       =  BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1)                         
            N_(2)       =  BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(2)                         
            N_(3)       =  BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(3)                         
            DVOL_PREDIC =  DT1*(VSUM(1)*N_(1) + VSUM(2)*N_(2) + VSUM(3)*N_(3))
            BRICK_LIST(NIN,IB)%POLY(ICELL)%DVOL(1) = DVOL_PREDIC   !for next CYCLE               
          ENDIF  
        ENDDO!next ICELL
      ENDDO!next IB 
      !!!!!!!!
      !output!
      !!!!!!!!
 !     IF(IBUG22_PREDICTION/=0)THEN
 !      print *, "==PREDICTION DVOL"
 !       DO IB=NBF,NBL
 !         IF(IBUG22_PREDICTION>0 .AND. IBUG22_PREDICTION/=IXS(11,brick_list(nin,ib)%id))cycle
 !         NCELL                                   = BRICK_LIST(NIN,IB)%NBCUT                    
 !         ICELL                                   = 0                                           
 !         DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
 !           ICELL                                 = ICELL +1
 !           IF (ICELL>NCELL .AND. NCELL/=0)ICELL  = 9   
 !             write(*,FMT=('(A,I8,A,I1,A,F30.16)'))"brickID=", ixs(11,brick_list(nin,ib)%id),".",ICELL,"=",
 !    .                                              BRICK_LIST(NIN,IB)%POLY(ICELL)%DVOL(1)
 !         ENDDO!next ICELL
 !       ENDDO!next IB
 !     ENDIF
      !------------------------------------------------------------!
      ! @40. BUILD SUPERCELL DVOL (PREDICTION)                     !
      !------------------------------------------------------------!
      !--------------!
      CALL MY_BARRIER
      !--------------!      
      !THEN COMPUTE SUPERCELL GEOMETRICAL DVOL
      DO IB=NBF,NBL
        NCELL                               = BRICK_LIST(NIN,IB)%NBCUT                    
        MCELL                               = BRICK_LIST(NIN,IB)%MainID                 
        BRICK_LIST(NIN,IB)%DVOL             = BRICK_LIST(NIN,IB)%POLY(MCELL)%DVOL(1)   
        !------LOOP--------!
        NumSECND                            = BRICK_LIST(NIN,IB)%SecndList%Num
        DO IC=1,NumSECND
          ICELLv                            = BRICK_LIST(NIN,IB)%SecndList%ICELLv(IC)  
          IBv                               = BRICK_LIST(NIN,IB)%SecndList%IBv(IC)     
          NGv                               = BRICK_LIST(NIN,IBv)%NG                   
          IDLOCv                            = BRICK_LIST(NIN,IBv)%IDLOC                
          IV                                = BRICK_LIST(NIN,IBv)%ID                       !'0' PREDICTION with old conformation
          BRICK_LIST(NIN,IB)%DVOL           = BRICK_LIST(NIN,IB)%DVOL + BRICK_LIST(NIN,IBv)%POLY(ICELLv)%DVOL(1)    !'1' PREDICTION with new conformation
        ENDDO!next IC
      ENDDO!next IB

      
      !attention faire traitemtn identique en loi 51 pour V1OLD,V2OLD..
      
!!!!!!!!!!!!!!!!!!!!!!!!      attention en modifiant ici les vold_scell, on perturbe le critre predictif ci-dessous.   

 
        

      !------------------------------------------------------------!
      ! @41. FIX SUPERCELL DVOL (CORRECTION)                       !
      !------------------------------------------------------------!

      lSKIP = .FALSE.
      
c      if(IBUG22_dvol /= 0)then
c        print *, "+---Sinit22_fvm : DVOL majoration"
c      endif
      if(DT1==ZERO)then
        do ib=nbf,nbl
          BRICK_LIST(NIN,IB)%Vold_SCell = BRICK_LIST(NIN,IB)%Vnew_SCell             
        enddo
      endif
CCC      RATIO22 = 1.02D00

      if(.NOT.lSKIP)then 
      DO IB=NBF,1*NBL 
        ICODE        = BRICK_LIST(NIN,IB)%ICODE
        OLD_ICODE    = BRICK_LIST(NIN,IB)%OLD_ICODE
        if (BRICK_LIST(NIN,IB)%Vold_SCell==-111.D00) BRICK_LIST(NIN,IB)%Vold_SCell = BRICK_LIST(NIN,IB)%Vnew_SCell   !never init.  
        VNEW         = BRICK_LIST(NIN,IB)%Vnew_SCell
        VOLD         = BRICK_LIST(NIN,IB)%Vold_SCell 
        DVOL_NUMERIC = VNEW-VOLD
        DVOL_PREDIC  = brick_list(nin,ib)%dvol
        UncutVOL = BRICK_LIST(NIN,IB)%UncutVOL
        !IF GEOMETRICAL VALUE IS RISEN ABOVE ITS PREDICTION : CORRECT 
c         if((IBUG22_prediction ==-1 .OR. IBUG22_prediction==ixs(11,brick_list(nin,ib)%id)) )then
c              print *, "+"            
c              print *, "+------elem_id           =",ixs(11,brick_list(nin,ib)%id)
c              print *, "+--------old_icode       =",OLD_ICODE
c              print *, "+--------icode           =",ICODE           
c              print *, "+--------dvol_prediction =",brick_list(nin,ib)%dvol
c              print *, "+--------dvol_numeric    =",VNEW-VOLD
c              print *, "+--------vnew            =",VNEW
c              print *, "+--------ratio22         =",RATIO22
c              print *, "+--------DVOL_NUM/V      =",ABS(DVOL_NUMERIC)/UncutVOL              
c            endif
               
        IF(ABS(DVOL_NUMERIC) > RATIO22*ABS(DVOL_PREDIC) .AND. DVOL_PREDIC /= ZERO .AND. RATIO22 < 1000 )THEN  
            IF((ICODE /= OLD_ICODE ) )THEN 
!            IF( (ICODE /= OLD_ICODE )  .OR.  (ICODE==OLD_ICODE>ZERO .AND. ABS(DVOL_NUMERIC/DVOL_PREDIC)>1.5d00 ))THEN             
c              if((IBUG22_prediction ==-1 .OR. IBUG22_prediction==ixs(11,brick_list(nin,ib)%id)) )then
c                print *, "+"            
c                print *, "+------elem_id           =",ixs(11,brick_list(nin,ib)%id)
c                print *, "+--------old_icode       =",OLD_ICODE
c                print *, "+--------icode           =",ICODE           
c                print *, "+--------dvol_prediction =",brick_list(nin,ib)%dvol
c                print *, "+--------dvol_numeric    =",VNEW-VOLD
c                print *, "+--------vnew            =",VNEW   
c              endif           
              BRICK_LIST(NIN,IB)%Vold_SCell = BRICK_LIST(NIN,IB)%Vnew_SCell - DVOL_PREDIC
              NG     = BRICK_LIST(NIN,IB)%NG
              IDLOC  = BRICK_LIST(NIN,IB)%IDLOC 
              GBUF   => ELBUF_TAB(NG)%GBUF
C              write (*,FMT='(A,F30.16)'  ) "   UncutVOL =", brick_list(nin,ib)%uncutvol
C              write (*,FMT='(A,F30.16)'  ) "   ABS(DVOL_NUMERIC)/UncutVOL =",ABS(DVOL_NUMERIC)/UncutVOL 
              !################################################################!
              !#           TRAITEMENT MULTIMATERIAU LAW51                     #!
              !################################################################!
              MLW    = BRICK_LIST(NIN,IB)%MLW 
              NG     = BRICK_LIST(NIN,IB)%NG
              IDLOC  = BRICK_LIST(NIN,IB)%IDLOC
              IF(MLW==51)THEN
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)           
                LLT  = IPARG(2,NG)           
                DO ITRIMAT=1,TRIMAT
                  !IPOS=01 : fractions volumiques
                  !IPOS=11 : anciens volumes de chaque phase              
                  IPOS                   = 1                
                  K                      = LLT * (N0PHAS + (ITRIMAT-1)*NVPHAS +IPOS-1)
                  VFRAC(ITRIMAT)         = MBUF%VAR(K+IDLOC)
                  IPOS                   = 11                
                  K                      = LLT * (N0PHAS + (ITRIMAT-1)*NVPHAS +IPOS-1)
                  VolCELL51_OLD(ITRIMAT) = MBUF%VAR(K+IDLOC)                
                  MBUF%VAR(K+IDLOC)      = MAX(ZERO, (VNEW-DVOL_PREDIC)*VFRAC(ITRIMAT)  )
                  VolCELL51(ITRIMAT)     = MBUF%VAR(K+IDLOC)
                ENDDO!next ITRIMAT             
              ENDIF
               if(IBUG22_prediction ==-1 .OR. IBUG22_prediction==ixs(11,brick_list(nin,ib)%id))then                        
c                print *, "+--------vold            =",VOLD             ,"->",  BRICK_LIST(NIN,IB)%Vold_SCell                 
!                print *, "+--------vold_1          =",VolCELL51_OLD(1) ,"->",  VolCELL51(1)                                  
!                print *, "+--------vold_2          =",VolCELL51_OLD(2) ,"->",  VolCELL51(2)                                  
!                print *, "+--------vold_3          =",VolCELL51_OLD(3) ,"->",  VolCELL51(3)                                   
!                print *, "+--------vold_4          =",VolCELL51_OLD(4) ,"->",  VolCELL51(4)                                           
              endif                                               
            ENDIF!IF(ICODE /= OLD_ICODE .AND. OLD_ICODE /= 0)
        ENDIF!IF(ABS(DVOL_NUMERIC) > RATIO22*ABS(DVOL_PREDIC) .AND. RATIO22 < 1000 .AND. ABS(DVOL_NUMERIC)/UncutVOL>0.05)
      ENDDO!next IB
      endif   

      IF( (IBUG22_prediction ==-1  ))THEN
        call my_barrier
        IF(ITASK==0)THEN
        DO IB=1,NB
          ICODE        = BRICK_LIST(NIN,IB)%ICODE
          OLD_ICODE    = BRICK_LIST(NIN,IB)%OLD_ICODE
          VNEW         = BRICK_LIST(NIN,IB)%Vnew_SCell
          VOLD         = BRICK_LIST(NIN,IB)%Vold_SCell 
          DVOL_NUMERIC = VNEW-VOLD
          DVOL_PREDIC  = brick_list(nin,ib)%dvol
          UncutVOL = BRICK_LIST(NIN,IB)%UncutVOL
          print *, "+------elem_id           =",ixs(11,brick_list(nin,ib)%id)
          print *, "+--------old_icode       =",OLD_ICODE
          print *, "+--------icode           =",ICODE           
          print *, "+--------dvol_prediction =",brick_list(nin,ib)%dvol
          print *, "+--------dvol_numeric    =",VNEW-VOLD
          print *, "+--------vnew            =",VNEW   
          print *, "+--------vold            =",VOLD             ,"->",  BRICK_LIST(NIN,IB)%Vold_SCell                           
        ENDDO
        ENDIF
      ENDIF
      

      !------------------------------------------------------------!
      ! @42. TIME ZERO VOLUME INIT (NOT DONE IN STARTER)           !
      !      needed for convection subroutines                     !
      !      sforc3>voln22 not called before convection            !
      !------------------------------------------------------------! 
      IF(DT1==ZERO)THEN
        DO IB=NBF,NBL     
           pSUBVOL(1:9)   => BRICK_LIST(NIN,IB)%POLY(1:9)
           pSUBVOLD  => BRICK_LIST(NIN,IB)%Vold_SCell
           MCELL     =  BRICK_LIST(NIN,IB)%MainID                                 
           IF(MCELL==0)CYCLE 
           pSUBVOLD  = brick_list(nin,ib)%Vnew_Scell                                 
        ENDDO                  
      ENDIF
      DO IB=NBF,NBL     
         pSUBVOL(1:9)     => BRICK_LIST(NIN,IB)%POLY(1:9)                       
         pSUBVOLD    => BRICK_LIST(NIN,IB)%Vold_SCell                          
         MCELL       =  BRICK_LIST(NIN,IB)%MainID                                 
         !IF(MCELL==0)CYCLE                                 
         GBUF                 => ELBUF_TAB(NGB(IB))%GBUF                         
         IF(pSUBVOLD>ZERO)GBUF%VOL(IDLOCB(IB)) =  pSUBVOLD
C         write (*,FMT='(A,I10,A,F30.16,A,F30.16)')"brickID=", IXS(11,brick_list(nin,ib)%id),
C     .          "  vold=",brick_list(nin,ib)%vold_scell,"  vnew=",brick_list(nin,ib)%vnew_scell
      ENDDO   
      
      
      !------------------------------------------------------------!
      ! @43. DEBUG OUTPUT                                          !
      !------------------------------------------------------------!    
      if (debug_outp .AND. IBUG22_sinit/=0)then       
        do IB=NBF,NBL   
         pSUBVOL(1:9)         => BRICK_LIST(NIN,IB)%POLY(1:9)
         pListNodID(1)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)
         pListNodID(2)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(2)%ListNodID(1:8) 
         pListNodID(3)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(3)%ListNodID(1:8)
         pListNodID(4)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(4)%ListNodID(1:8)
         pListNodID(5)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(5)%ListNodID(1:8)
         pListNodID(6)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(6)%ListNodID(1:8) 
         pListNodID(7)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(7)%ListNodID(1:8) 
         pListNodID(8)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(8)%ListNodID(1:8)  
         pListNodID(9)%p(1:8) => BRICK_LIST(NIN,IB)%POLY(9)%ListNodID(1:8)                               
          !pAdjCELL        => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%Adjacent_Cell(1:5)                    
          !pNAdjCELL       => BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(1:6)%NAdjCell
          pAdjBRICK       => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5)                       
          pIsMain(1:9)  => BRICK_LIST(NIN,IB)%POLY(1:9)                                 
          pMainID       => BRICK_LIST(NIN,IB)%MainID                                      
          ID              =  BRICK_LIST(NIN,IB)%ID                                            
          icell           =  0     
          MCELL           =  MCELL 
          NCELL           =  BRICK_LIST(NIN,IB)%NBCUT                                         
          GBUF            => ELBUF_TAB(NGB(IB))%GBUF                                          
          vol             =  GBUF%VOL(IDLOCB(IB))                                             
          brickID         =  IDB(IB) 
          if(IBUG22_sinit/=ixs(11,brickID) .AND. IBUG22_sinit/=-1) CYCLE
          write (*,FMT='(A,I12)')     "+=== BRICK ID===",  IXS(11,ID)                         
          if(ncell==0)then       
            write (*,FMT='(A )')        "|        uncut: "                                    
            write (*,FMT='(A,1F30.20)') "|       volume: ",  vol                              
            write (*,FMT='(A,1F30.20)') "|  ext. volume: ",  brick_list(nin,ib)%Vnew_Scell                      
            write (*,FMT='(A,1F30.20)') "|        masse: ",  GBUF%VOL(IDLOCB(IB))  *  GBUF%RHO(IDLOCB(IB))                                               
            if(brick_list(nin,ib)%SecndList%Num>0)then
              do j=1,brick_list(nin,ib)%SecndList%Num
                write (*,FMT='(A )')            "|          secnd list : " 
                write (*,FMT='(A,I10 )')        "|          +        J : ", brick_list(nin,ib)%SecndList%FM(j)         
                write (*,FMT='(A,I10 )')        "|          +       IB : ", brick_list(nin,ib)%SecndList%IBv(j)
                write (*,FMT='(A,I10 )')        "|          +  brickID : ", ixs(11,brick_list(nin,ib)%SecndList%IV(j))
              enddo        
            endif
            cycle                
          endif                    
          write (*,FMT='(A,1F30.20)') "|       volume: ",  vol                                
          write (*,FMT='(A,6F30.20)') "|        faces: ",  F(1:6,IB)                          
          write (*,FMT='(A,1F30.20)') "|        masse: ",  GBUF%VOL(IDLOCB(IB))  *  GBUF%RHO(IDLOCB(IB))                                                 
          DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}                      
           ICELL = ICELL +1        
           IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9          
           debugMAINSECND = '.........|'                                                    
            mnod = BRICK_LIST(NIN,IB)%POLY(icell)%NumNOD
            write (*,FMT='(A )')    "|"                                                       
            if(icell/=9)then       
              write (*,FMT='(A,I1,A,A,A1,I2,A,A6)')                                            
     .                                             "+== ICELL= ", ICELL , ", SecType=", BRICK_LIST(NIN,IB)%SECTYPE(ICELL) ,                              
     .                                             "(",  BRICK_LIST(NIN,IB)%SecID_Cell(ICELL)  , ")   -  ",  Char1                                       
            else                   
              write (*,FMT='(A,A6)')                "+== REMAINING POLYHEDRON  -  ", Char1     
            endif                  
                                   
            write (*,FMT='(A )')                   "|  |"                                     
            write (*,FMT='(A,I1)')                 "|  +===Main/Secnd=",  pIsMain(ICELL)%IsMain                                          
            write (*,FMT='(A,F30.20)')             "|  +======SUVOLUMES=",  pSUBVOL(ICELL)%Vnew
            write (*,FMT='(A,6F30.20)')            "|  +=======SUBFACES=",  BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Surf 
            write (*,FMT='(A,F30.20)')             "|  +=======CUT AERA=",  BRICK_LISt(NIN,IB)%PCUT(ICELL)%SCUT(1)      
            write (*,FMT='(A,A,I2)')               "|  +======NUM POINT=",  "  ",BRICK_LIST(NIN,IB)%POLY(ICELL)%NumPOINT                                                     
            write (*,FMT='(A,A,I1,A,8I12)')        "|  +======NODE LIST=",                    
     .                                             "  (",MNOD,") ", pListNodID(ICELL)%p(1:MNOD) 
            write (*,FMT='(A,A,8I12)')             "|  |         radIDs=",                    
     .                                             "      ",      IXS(1+pListNodID(ICELL)%p(1:MNOD),ID)                                                    
            write (*,FMT='(A,A,8I12)')             "|  |        userIDs=",                    
     .                                             "      ", ITAB(IXS(1+pListNodID(ICELL)%p(1:MNOD),ID))                                                   
            IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(brickID)
            LGTH2 = ALE_CONNECTIVITY%ee_connect%iad_connect(brickID+1) - 
     .           ALE_CONNECTIVITY%ee_connect%iad_connect(brickID)
            If(SUM(IABS(ALE_CONNECTIVITY%ee_connect%connected(IAD2:IAD2 + LGTH2 - 1)))/=0)then                                          
              write (*,FMT='(A,6I10)')               "|  +===Adj Brick(i)=",   pAdjBRICK(1:6,1)                                                          
                DO J=1,6           
                IF( pAdjBRICK(J,1)/=0 )THEN                                                   
                  write (*,FMT='(A,6I10)')               "|  +===Adj Brick(u)=",   IXS(11,pAdjBRICK(J,1))                                                
                ENDIF              
              ENDDO                
              DO J=1,6           
                NadjCell = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell                                                    
                IF(NadjCell/=0)THEN                                                         
                  write (*,FMT='(A,I1,A,5I3)')               "|  +======Adj Cells, face=",J, " :", 
     .                                                     BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(1:NadjCell)                      
                ENDIF            
              ENDDO              
            ENDIF                                 
          enddo!next icell       
          write (*,FMT='(A )')    " "      
        enddo!next IB
        if(itask==0.AND.debug_outp)then
          write (*,FMT='(A )')    "  ----sini22_end----"       
          write (*,FMT='(A )')    " "         
        endif
      endif! (IBUG22_sinit/=0)

      !------------------------------------------------------------!
      ! @44. DEBUG - WRITE CUT CELL BUFFER                         !
      !------------------------------------------------------------! 
      ! CALL WRITE_CUT_CELL_BUFFER()  !post/debug

      !------------------------------------------------------------!
      ! @45. SUPERCELL CENTERS                                     !
      !------------------------------------------------------------!    
      DO IB=NBF,NBL
        NCELL         = BRICK_LIST(NIN,IB)%NBCUT  
        NumSECND      = BRICK_LIST(NIN,IB)%SecndList%Num
        MainID      = BRICK_LIST(NIN,IB)%mainID
        IF (MainID ==0) CYCLE
        VolCELL       = BRICK_LIST(NIN,IB)%POLY(mainID)%Vnew
        VOL           = VolCELL !cumul
        pPOINT(1:3)   = BRICK_LIST(NIN,IB)%POLY(mainID)%CellCenter(1:3) * VolCELL
        DO IC=1,NumSECND
          ICELLv      = BRICK_LIST(NIN,IB)%SecndList%ICELLv(IC)  
          IBv         = BRICK_LIST(NIN,IB)%SecndList%IBv(IC)     
          VolCELL     = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%Vnew
          Point0(1:3) = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%CellCenter(1:3)
          pPOINT(1)   = pPOINT(1) + VolCELL * Point0(1)          
          pPOINT(2)   = pPOINT(2) + VolCELL * Point0(2)
          pPOINT(3)   = pPOINT(3) + VolCELL * Point0(3)                    
          VOL         = VOL + VolCELL
        ENDDO!next IC
          pPOINT(1)   = pPOINT(1) / VOL 
          pPOINT(2)   = pPOINT(2) / VOL
          pPOINT(3)   = pPOINT(3) / VOL 
          BRICK_LIST(NIN,IB)%SCellCenter(1:3) = pPOINT(1:3)                   
      ENDDO!next IB      
      
      !------------------------------------------------------------!
      ! @46. MARK SUPER-CELL CENTERS WITH ORPHAN NODES             !
      !------------------------------------------------------------!
      lStillNode = .TRUE.
      IF(IPARI(70)/=0)THEN
        NNODES      = IGRNOD(IPARI(70))%NENTITY
        !!!WRITE(*,*)(ITAB(IBUFSSG(J)),J=IAD0,IAD0+NNODES-1)
        IF( ITASK==0 .AND. NNODES/=0 )THEN
          II        = 1  ! first node of group node
          DO IB=NBF,NBL 
            MCELL = BRICK_LIST(NIN,IB)%mainID
            IF (MCELL==0              ) CYCLE
            IF (.NOT.lStillNode) CYCLE
            Point0(1:3) = BRICK_LIST(NIN,IB)%SCellCenter(1:3)
            IF (II>NNODES) THEN
              lStillNode = .FALSE. 
              print *, "** Warning inter22 : no more node in group to mark cell center. Last one was"  ,
     .                     ITAB(IGRNOD(IPARI(70))%ENTITY(NNODES))
              EXIT                        
            ENDIF                         
            X(1:3,IGRNOD(IPARI(70))%ENTITY(II)) = Point0(1:3)
            if(IBUG22_OrphanNodes == 1)then       
           write (*,FMT='(A,I10,A,I10,A,I10)')"set orphan_node_id=",ITAB(IGRNOD(IPARI(70))%ENTITY(II))
            endif                         
            II = II + 1 !next node        
          ENDDO !next IB 
          DO II = 1, NNODES
              !print *, "reset SPHCEL_id=", IXT(NIXT,IGRNOD(IPARI(70))%ENTITY(II))
              X(1:3,IGRNOD(IPARI(70))%ENTITY(II)) = (/ZERO, ZERO, ZERO/)
          ENDDO
        ENDIF !ITASK==0
      ENDIF

      !------------------------------------------------------------!
      ! @47. UNCUT CELLS + POLY 9 : CENTERS                        !
      !------------------------------------------------------------!  
      !FOR UNCUT BRICKS, Centers for polyehedra faces computed in i22subvol  
      DO IB=NBF,NBL
        NCELL     = BRICK_LIST(NIN,IB)%NBCUT 
        IF(NCELL==0)THEN
          IE        = BRICK_LIST(NIN,IB)%ID
          NC(1:8)   = IXS(2:9,IE)
          DO J=1,6
            BRICK_LIST(NIN,IB)%POLY(1)%FACE(J)%Center(1) = FOURTH * SUM(   X(1, NC(ICF(1:4,J))  )   ) 
            BRICK_LIST(NIN,IB)%POLY(1)%FACE(J)%Center(2) = FOURTH * SUM(   X(2, NC(ICF(1:4,J))  )   ) 
            BRICK_LIST(NIN,IB)%POLY(1)%FACE(J)%Center(3) = FOURTH * SUM(   X(3, NC(ICF(1:4,J))  )   )                     
          ENDDO
        ELSE
          !simplification, car approximation precendente invalide lorsque limit(FACE9) = 0
          IE        = BRICK_LIST(NIN,IB)%ID
          NC(1:8)   = IXS(2:9,IE)
          !
          !ICELL 9 only since polyhedra face centers were computed in i22subvol.
          ! Poly9 is complementary polyhedron, it is built without graph but deduced by boolean operation from full brick.
          ! its face centers must be computed to display face center (velocity post-treatment)
          ! face center are only known for cut polyhedra (A & C below), center for poly9 must be deduced (poly B below)
          !
          !      N4                    N3
          !       !------------+-------!
          !       !            | I3    !
          !       !            |       !
          !       !            |       !              Ni : nodes
          !       !            |       !              Ii : intersection points
          !       ! I4    B    |   C   !             A,C : cut polyhedra
          !       !\           |       !               B : complemntary polyhedron
          !       ! \          |       !        Ca,Cb,Cc : face centers for each A,B,C polyhedra
          !       !  \         |       !     Npa,Npb,Npc : number of points for polygon on face
          !       ! A \ I1     | I2    !
          !       !----+-------|-------!
          !      N1                    N2
          !
          !                Ca = (N1+I1+I4)/3 
          !                Cb = (N4+I1+I2+I3+I4)/5
          !                Cc = (N2+N3+I2+I3)/4
          !   3Ca + 5Cb + 4Cc = Sum(Ni) + 2 Sum(Ii)
          !   =>   poly9   C9 = [-Npa.Ca -Npc.Cc + Sum(Ni) +2.Sum(Ii)] / Np9 
          !
          !    
          Do J=1,6
            FACE                  = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Surf
            NP_(9)                = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NumPOINT
            IF(ABS(FACE)<=EM10 .OR. NP_(9)==0)THEN
              BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Center(1:3) = ZERO
            ELSE
              NCELL               = BRICK_LIST(NIN,IB)%NBCUT
              DO ICELL = 1 ,NCELL
                NP_(ICELL)        = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NumPOINT
                Center(1:3,ICELL) = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Center(1:3)
              ENDDO
              !NP_ is number of points (intersection + nodes)
              ! pPoint is sum of nodes
              !CUT_point is sum of intersection points
              ! Center : are face center
              ! Point0 : is center of poly 9
              NP_(NCELL+1:9)      = 0
              NP_(9)              = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NumPOINT
              pPOINT(1)           = SUM(   X(1, NC(ICF(1:4,J))  )   )     
              pPOINT(2)           = SUM(   X(2, NC(ICF(1:4,J))  )   )   
              pPOINT(3)           = SUM(   X(3, NC(ICF(1:4,J))  )   )                            
              CUT_point(1:3)      = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Center(1:3) !stored there in i22subvol.F    it is SUM(cutpoints on the face J)
              Point0(1)           = pPOINT(1) + TWO*CUT_point(1) 
     .                              - Np_(1)*Center(1,1)- Np_(2)*Center(1,2)- Np_(3)*Center(1,3)- Np_(4)*Center(1,4)
     .                              - Np_(5)*Center(1,5)- Np_(6)*Center(1,6)- Np_(7)*Center(1,7)- Np_(8)*Center(1,8)  
              Point0(2)           = pPOINT(2) + TWO*CUT_point(2) 
     .                              - Np_(1)*Center(2,1)- Np_(2)*Center(2,2)- Np_(3)*Center(2,3)- Np_(4)*Center(2,4)
     .                              - Np_(5)*Center(2,5)- Np_(6)*Center(2,6)- Np_(7)*Center(2,7)- Np_(8)*Center(2,8)
              Point0(3)           = pPOINT(3) + TWO*CUT_point(3) 
     .                              - Np_(1)*Center(3,1)- Np_(2)*Center(3,2)- Np_(3)*Center(3,3)- Np_(4)*Center(3,4)
     .                              - Np_(5)*Center(3,5)- Np_(6)*Center(3,6)- Np_(7)*Center(3,7)- Np_(8)*Center(3,8)   
              Point0(1:3)         = Point0(1:3) / NP_(9) 
              BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Center(1:3) = Point0(1:3)
              !print *, "poly9 center, ixs(11,IE),  face J  ", ixs(11,IE), J
              !print *, BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Center(1:3)     
            ENDIF
          ENDDO!next J
        ENDIF
      ENDDO!next IB       



      !------------------------------------------------------------!
      ! @48. MARK CELL CENTERS WITH ORPHAN NODES                   !
      !------------------------------------------------------------!
      lStillNode = .TRUE.
      IF(IPARI(81)/=0)THEN
        NNODES      = IGRNOD(IPARI(81))%NENTITY
        !!!WRITE(*,*)(ITAB(IBUFSSG(J)),J=IAD0,IAD0+NNODES-1)
        IF( ITASK==0 .AND. NNODES/=0 )THEN
          II        = 1  ! first node of group node
          DO IB=NBF,NBL           
            ICELL   =  0  
            NCELL   = BRICK_LIST(NIN,IB)%NBCUT                     
            DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
              ICELL = ICELL +1
              IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9   
                IF (.NOT.lStillNode) CYCLE
                Point0(1:3) = BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1:3)
                IF(II>NNODES)THEN
                  lStillNode  = .FALSE. 
                  print *, "** Warning inter22 : no more node in group to mark cell center",
     .                      ITAB(IGRNOD(IPARI(81))%ENTITY(NNODES))
                  EXIT                        
                ENDIF
                BRICK_LIST(NIN,IB)%POLY(ICELL)%ID_FREE_NODE = IGRNOD(IPARI(81))%ENTITY(II)
                X(1:3,IGRNOD(IPARI(81))%ENTITY(II)) = Point0(1:3)
                if(IBUG22_OrphanNodes == 1)then     
                  write (*,FMT='(A,I10,A,I10,A,I10)')"set orphan_node_id=",
     .            ITAB(IGRNOD(IPARI(81))%ENTITY(II)),"in brick_id=",ixs(11,brick_list(nin,ib)%id)
                endif                         
                II = II + 1 !next node                      
            ENDDO            
          ENDDO !next IB      
          DO II = 1,NNODES
            !print *, "reset SPHCEL_id=", IXT(NIXT,IGRNOD(IPARI(81))%ENTITY(II))
            X(1:3,IGRNOD(IPARI(81))%ENTITY(II)) = ZERO
          ENDDO
        ENDIF !ITASK==0
      ENDIF      


       !------------------------------------------------------------!
       ! @49. MARK FACE CENTERS WITH ORPHAN NODES                   !
       !------------------------------------------------------------!
       IF(IPARI(82)/=0)THEN
         NNODES      = IGRNOD(IPARI(82))%NENTITY
         IF(NNODES==0)RETURN
         II        = 1  ! first node of group node
         DO IB=NBF,NBL    
           IE                    = BRICK_LIST(NIN,IB)%ID
           ICELL                 =  0     
           NCELL                 = BRICK_LIST(NIN,IB)%NBCUT                                             
           DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}               
             ICELL               = ICELL +1
             IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9   
             IF(.NOT.lStillNode) CYCLE
             DO J=1, 6
               IF (II>NNODES) THEN
                 lStillNode  = .FALSE. 
                 print *, "** Warning inter22 : no more node in group to mark face centers." ,
     .                     ITAB(IGRNOD(IPARI(82))%ENTITY(NNODES))
                 EXIT                        
               ENDIF  
               NODE_ID           = IGRNOD(IPARI(82))%ENTITY(II)
               X(1:3,NODE_ID)    = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Center(1:3)
               II                = II + 1 !next node  
             ENDDO             
           ENDDO !next ICELL          
         ENDDO!next IB  
      ENDIF

        
        
        !print *, "SINIT22_FVM : NB=", NBL
        !do ib=nbf,nbl
        !  id = brick_list(nin,ib)%id
        !  print *, "ixs      =", ixs(11,id)
        !  print *, "nbcut    = ", brick_list(1,ib)%nbcut
        !  print *, "sectype1 =", brick_list(1,ib)%sectype(1)
        !  print *, "sectype2 =", brick_list(1,ib)%sectype(2)
        !  print *, "---------------------"
        !enddo
        
        
      !------------------------------------------------------------!
      ! @50. DEALLOCATE                                            !
      !------------------------------------------------------------!      
      IF(ALLOCATED(debugMAINSECNDv))DEALLOCATE (debugMAINSECNDv)
      IF(ALLOCATED(IsMainV))DEALLOCATE (IsMainV)  
      IF(ALLOCATED(F))DEALLOCATE (F)      
      IF(ALLOCATED(VOL51))DEALLOCATE (VOL51,VOL51v)          
      IF(ALLOCATED(ORIGIN_DATA))DEALLOCATE (ORIGIN_DATA)      
      IF(ALLOCATED(DESTROY_DATA))DEALLOCATE (DESTROY_DATA)       
      IF(ALLOCATED(Norigin))DEALLOCATE (Norigin)  


      RETURN
      END
