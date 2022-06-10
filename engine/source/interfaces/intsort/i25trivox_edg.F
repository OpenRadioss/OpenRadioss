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
Chd|  I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|-- called by -----------
Chd|        I25BUCE_EDG                   source/interfaces/intsort/i25buce_edg.F
Chd|-- calls ---------------
Chd|        I25STO_E2S                    source/interfaces/intsort/i25sto_e2s.F
Chd|        I25STO_EDG                    source/interfaces/intsort/i25sto_edg.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        INT_CHECKSUM                  share/modules/debug_mod.F     
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        REALLOC_MOD                   share/modules/realloc_mod.F   
Chd|        TRI11                         share/modules/tri11_mod.F     
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25TRIVOX_EDG(
     1      I_MEM   ,VMAXDT  ,INACTI ,IRECT   ,
     2      X       ,V       ,STF    ,STFE    ,XYZM     ,
     3      II_STOK ,CANDS_E2E ,ESHIFT  ,NEDGE_T ,CANDM_E2E   ,
     4      MULNSNE,NOINT   ,BGAPEMX ,SSHIFT  ,NRTM_T   ,
     5      VOXEL  ,NBX     ,NBY     ,NBZ      ,
     6      IGAP   ,GAP_M  ,GAP_M_L ,DRAD     ,MARGE    ,
     7      ITASK   ,ITAB   ,LL_STOK ,MULNSNS  ,
     8      MBINFLG ,EBINFLG,ILEV    ,CAND_A   ,CAND_P   ,
     9      FLAGREMNODE,KREMNODE_EDG,REMNODE_EDG,KREMNODE_E2S,
     .                                            REMNODE_E2S,
     A      IEDGE  ,NEDGE  ,LEDGE   ,MSEGTYP   ,IGAP0    ,
     B      ADMSR,EDG_BISECTOR,VTX_BISECTOR,
     C      CANDM_E2S,CANDS_E2S,CAND_B,CAND_PS,GAPE ,
     D      GAP_E_L,NEDGE_LOCAL,IFQ,CANDE2E_FX ,CANDE2E_FY,
     E   CANDE2E_FZ,CANDE2S_FX ,CANDE2S_FY,CANDE2S_FZ,IFPEN_E,IFPEN_E2S,
     F   KREMNODE_EDG_SIZ,REMNODE_EDG_SIZ,KREMNODE_E2S_SIZ,REMNODE_E2S_SIZ) 
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE REALLOC_MOD
      USE TRI25EBOX
      USE TRI7BOX
      USE TRI11
#ifdef WITH_ASSERT
      USE DEBUG_MOD
#endif
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
c     parameter setting the size for the vector (orig version is 128)
      INTEGER NVECSZ 
      PARAMETER (NVECSZ = MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "assert.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   CLASSE LES EDGES DANS DES BOITES 
C   RECHERCHE POUR CHAQUE FACETTE DES BOITES CONCERNES
C   RECHERCHE DES CANDIDATS
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM(2),INACTI,ITASK,IGAP,IEDGE,NEDGE,ESHIFT,NEDGE_T,SSHIFT,NRTM_T,IGAP0,
     .        MULNSNE,MULNSNS,NOINT,NBX,NBY,NBZ,IFQ,
     .        CANDS_E2E(*),CANDM_E2E(*),
     .        IRECT(4,*), VOXEL(NBX+2,NBY+2,NBZ+2),II_STOK,LL_STOK,ITAB(*),
     .        MBINFLG(*),EBINFLG(*),ILEV,CAND_A(*),LEDGE(NLEDGE,*),ADMSR(4,*),MSEGTYP(*),
     .        CANDM_E2S(*),CANDS_E2S(*),CAND_B(*),IFPEN_E(*),IFPEN_E2S(*)
C     INTEGER :: NEDGE_REMOTE_OLD, RENUM(*)
      INTEGER , INTENT(IN) :: KREMNODE_EDG_SIZ,REMNODe_EDG_SIZ,KREMNODE_E2S_SIZ,REMNODE_E2S_SIZ,
     .        FLAGREMNODE, KREMNODE_EDG(KREMNODE_EDG_SIZ), REMNODE_EDG(REMNODE_EDG_SIZ),
     .        KREMNODE_E2S(KREMNODE_E2S_SIZ), REMNODE_E2S(REMNODE_E2S_SIZ)
C     REAL
      my_real
     .   X(3,*),V(3,*),XYZM(6),STF(*), STFE(NEDGE), GAP_M(*), GAP_M_L(*), GAPE(*), GAP_E_L(*),
     .   CAND_P(*),CAND_PS(*),MARGE,BGAPEMX,VMAXDT,DRAD,
     .   CANDE2E_FX(*) ,CANDE2E_FY(*),CANDE2E_FZ(*),
     .   CANDE2S_FX(4,*) ,CANDE2S_FY(4,*),CANDE2S_FZ(4,*)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
      INTEGER, INTENT(IN) :: NEDGE_LOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,I_STOK, SOL_EDGE, SH_EDGE, 
     .        N1,N2,NN,NE,K,L,J_STOK,II,JJ,NA,NB,
     .        PROV_S(MVSIZ),PROV_M(MVSIZ),
     .        M,NS1,NS2,NSE,NS,SIZE,Z_FIRST,Z_LAST
C     REAL
      my_real
     .   DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, GAPL,
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS
c provisoire
      INTEGER  IX,IY,IZ,IEDG,IE,
     .         M1, M2, M3, M4, MM1,MM2,MM3,MM4,SS1,SS2,
     .         IMS1,IMS2,ISS1,ISS2,
     .         AM1,AM2,AS1,AS2,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2,REMOVE_REMOTE
      INTEGER, DIMENSION(3) :: TMIN,TMAX
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,AAA,
     .   XMAX_EDGS, XMIN_EDGS,   !cotes min/max des aretes seconds et mains
     .   YMAX_EDGS, YMIN_EDGS,
     .   ZMAX_EDGS, ZMIN_EDGS,
     .   XMAX_EDGM, XMIN_EDGM,
     .   YMAX_EDGM, YMIN_EDGM,
     .   ZMAX_EDGM, ZMIN_EDGM   
      my_real :: G ! gap
      INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGEDG
      INTEGER :: EDGE_TYPE
      INTEGER :: EID
      INTEGER FIRST_ADD, PREV_ADD, CHAIN_ADD, CURRENT_ADD, MAX_ADD
      INTEGER BITGET
      EXTERNAL BITGET

C-----------------------------------------------
      INTEGER IDS(4), PROV_IDS(2,MVSIZ)

C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGREMLINE    
C-----------------------------------------------
C
      IF(FLAGREMNODE==2) THEN
        ALLOCATE(TAGREMLINE(NEDGE))
        TAGREMLINE(1:NEDGE) = 0
      ENDIF

      IDS(1:4) = 0
      PROV_IDS(1:2,1:MVSIZ) = 0

      SOL_EDGE =IEDGE/10 ! solids
      SH_EDGE  =IEDGE-10*SOL_EDGE ! shells

      MIN_IX=NBX+2
      MIN_IY=NBY+2
      MIN_IZ=NBZ+2
      MAX_IX=1
      MAX_IY=1
      MAX_IZ=1      
      
      !---------------------------------------------------------!
      ! Allocation des tableaux chaines                         !
      !---------------------------------------------------------!
      MAX_ADD = MAX(1,4*(NEDGE+NEDGE_REMOTE))
      IF(ITASK==0)THEN
        ALLOCATE(LCHAIN_ELEM(1:MAX_ADD))
        ALLOCATE(LCHAIN_NEXT(1:MAX_ADD))
        ALLOCATE(LCHAIN_LAST(1:MAX_ADD))
      END IF

C     Barrier to wait init voxel and allocation 
      CALL MY_BARRIER
C
      XMIN = XYZM(1)  
      YMIN = XYZM(2)
      ZMIN = XYZM(3)
      XMAX = XYZM(4)
      YMAX = XYZM(5)
      ZMAX = XYZM(6)

c     dev future: xminb plus grand que xmin...
      XMINB = XMIN
      YMINB = YMIN
      ZMINB = ZMIN
      XMAXB = XMAX
      YMAXB = YMAX
      ZMAXB = ZMAX

c     IF( NSPMD > 1) THEN
c       CALL SPMD_OLDNUMCD(RENUM,OLDNUM,ISZNSNR,NSNROLD)
c     ENDIF
C=======================================================================
C 1   mise des edges dans les boites
C=======================================================================
      IF(ITASK == 0)THEN

       CURRENT_ADD=1 ! premiere adresse
       DO I=1,NEDGE + NEDGE_REMOTE

        IF(I <= NEDGE_LOCAL) THEN
          NE = LEDGE(1,I)
C         PRINTIF((STFE(I) == 0 .AND.LEDGE(6,I) == D_ES),STFE(I))
          IF(STFE(I)==ZERO) CYCLE   ! on ne retient pas les facettes detruites

          IF(LEDGE(7,I) < 0) CYCLE   ! larete nest pas une arete second
          N1 = LEDGE(5,I)
          N2 = LEDGE(6,I)
          EID = LEDGE(8,I) 
          
          XX1=X(1,N1)
          XX2=X(1,N2)
          YY1=X(2,N1)
          YY2=X(2,N2) 
          ZZ1=X(3,N1)
          ZZ2=X(3,N2)
          DEBUG_E2E(EID == D_ES,EID)
        ELSE IF(I > NEDGE) THEN
          XX1=XREM_EDGE(E_X1,I-NEDGE) 
          XX2=XREM_EDGE(E_X2,I-NEDGE)      
          YY1=XREM_EDGE(E_Y1,I-NEDGE)      
          YY2=XREM_EDGE(E_Y2,I-NEDGE)      
          ZZ1=XREM_EDGE(E_Z1,I-NEDGE)      
          ZZ2=XREM_EDGE(E_Z2,I-NEDGE)      
          EID = IREM_EDGE(E_GLOBAL_ID,I-NEDGE)
          DEBUG_E2E(EID == D_ES,EID)
        ELSE
          ! Secondary edge is boundary between domains
          ! ISPMD is not the owner of this edge
          ASSERT(NSPMD > 1)
          CYCLE 
        ENDIF
        DEBUG_E2E(EID==D_ES,IGAP0)
       
        IF(IGAP0 == 0)THEN
          XMAX_EDGS=MAX(XX1,XX2);
          XMIN_EDGS=MIN(XX1,XX2);             
          YMAX_EDGS=MAX(YY1,YY2);
          YMIN_EDGS=MIN(YY1,YY2);         
          ZMAX_EDGS=MAX(ZZ1,ZZ2);
          ZMIN_EDGS=MIN(ZZ1,ZZ2);
          DEBUG_E2E(EID==D_ES,XMIN_EDGS)
          DEBUG_E2E(EID==D_ES,YMIN_EDGS)
          DEBUG_E2E(EID==D_ES,ZMIN_EDGS)
          DEBUG_E2E(EID==D_ES,XMAX_EDGS)
          DEBUG_E2E(EID==D_ES,YMAX_EDGS)
          DEBUG_E2E(EID==D_ES,ZMAX_EDGS)
          DEBUG_E2E(EID==D_ES,XMIN)
          DEBUG_E2E(EID==D_ES,YMIN)
          DEBUG_E2E(EID==D_ES,ZMIN)
          DEBUG_E2E(EID==D_ES,XMAX)
          DEBUG_E2E(EID==D_ES,YMAX)
          DEBUG_E2E(EID==D_ES,ZMAX)
          IF(XMAX_EDGS < XMIN) CYCLE
          IF(XMIN_EDGS > XMAX) CYCLE
          IF(YMAX_EDGS < YMIN) CYCLE
          IF(YMIN_EDGS > YMAX) CYCLE
          IF(ZMAX_EDGS < ZMIN) CYCLE
          IF(ZMIN_EDGS > ZMAX) CYCLE

        ELSE
          IF(I <= NEDGE) THEN 
             G = GAPE(I)
          ELSE
             G = XREM_EDGE(E_GAP,I-NEDGE) 
          END IF


C         REACTIVER LES CYCLES
c         XMAX_EDGS=MAX(XX1,XX2)+G; IF(XMAX_EDGS < XMIN) CYCLE
c         XMIN_EDGS=MIN(XX1,XX2)-G; IF(XMIN_EDGS > XMAX) CYCLE 
c         YMAX_EDGS=MAX(YY1,YY2)+G; IF(YMAX_EDGS < YMIN) CYCLE
c         YMIN_EDGS=MIN(YY1,YY2)-G; IF(YMIN_EDGS > YMAX) CYCLE 
c         ZMAX_EDGS=MAX(ZZ1,ZZ2)+G; IF(ZMAX_EDGS < ZMIN) CYCLE
c         ZMIN_EDGS=MIN(ZZ1,ZZ2)-G; IF(ZMIN_EDGS > ZMAX) CYCLE

          XMAX_EDGS=MAX(XX1,XX2)+G;
          XMIN_EDGS=MIN(XX1,XX2)-G; 
          YMAX_EDGS=MAX(YY1,YY2)+G;
          YMIN_EDGS=MIN(YY1,YY2)-G; 
          ZMAX_EDGS=MAX(ZZ1,ZZ2)+G;
          ZMIN_EDGS=MIN(ZZ1,ZZ2)-G;


C         IF(XMAX_EDGS < XMIN) CYCLE
C         IF(XMIN_EDGS > XMAX) CYCLE
C         IF(YMAX_EDGS < YMIN) CYCLE
C         IF(YMIN_EDGS > YMAX) CYCLE
C         IF(ZMAX_EDGS < ZMIN) CYCLE
C         IF(ZMIN_EDGS > ZMAX) CYCLE
        DEBUG_E2E(EID==D_ES,XMIN_EDGS)
        DEBUG_E2E(EID==D_ES,YMIN_EDGS)
        DEBUG_E2E(EID==D_ES,ZMIN_EDGS)
        DEBUG_E2E(EID==D_ES,XMAX_EDGS)
        DEBUG_E2E(EID==D_ES,YMAX_EDGS)
        DEBUG_E2E(EID==D_ES,ZMAX_EDGS)


        END IF



        !-------------------------------------------!
        !  VOXEL OCCUPIED BY THE EDGE               !
        !-------------------------------------------!
        !Voxel_lower_left_bound for this edge
        IX1=INT(NBX*(XMIN_EDGS-XMINB)/(XMAXB-XMINB))
        IY1=INT(NBY*(YMIN_EDGS-YMINB)/(YMAXB-YMINB))
        IZ1=INT(NBZ*(ZMIN_EDGS-ZMINB)/(ZMAXB-ZMINB))
        IX1=MAX(1,2+MIN(NBX,IX1))
        IY1=MAX(1,2+MIN(NBY,IY1))
        IZ1=MAX(1,2+MIN(NBZ,IZ1))
        !Voxel_upper_right_bound for this edge
        IX2=INT(NBX*(XMAX_EDGS-XMINB)/(XMAXB-XMINB))
        IY2=INT(NBY*(YMAX_EDGS-YMINB)/(YMAXB-YMINB))
        IZ2=INT(NBZ*(ZMAX_EDGS-ZMINB)/(ZMAXB-ZMINB))
        IX2=MAX(1,2+MIN(NBX,IX2))
        IY2=MAX(1,2+MIN(NBY,IY2))
        IZ2=MAX(1,2+MIN(NBZ,IZ2))
        
        !pour reset des voxel
        MIN_IX = MIN(MIN_IX,IX1)
        MIN_IY = MIN(MIN_IY,IY1)
        MIN_IZ = MIN(MIN_IZ,IZ1)          
        MAX_IX = MAX(MAX_IX,IX2)
        MAX_IY = MAX(MAX_IY,IY2)
        MAX_IZ = MAX(MAX_IZ,IZ2)                                     
 
        !----------------------------------------------!
        ! EDGE STORAGE FOR EACH VOXEL (CHAINED ARRAY)  !
        !----------------------------------------------!
C
C       VOXEL(i,j,k) LCHAIN_LAST(FIRST)
C       +-----------+------------+ 
C       |  =>FIRST  |   =>LAST   |                    
C       +--+--------+--+---------+
C          |           |
C          |           |
C          |           |
C          |           |   LCHAIN_ELEM(*) LCHAIN_NEXT(*) 
C          |           |   +------------+-----------+
C          +-------------->| edge_id    |   iadd 3  |  1:FIRST --+
C                      |   +------------+-----------+            |
C                      |   |            |              |  2           |
C                      |   +------------+-----------+            |
C                      |   | edge_id    |   iadd 4  |  3 <-------+
C                      |   +------------+-----------+            |
C                      |   | edge_id    |   iadd 6  |  4 <-------+
C                      |   +------------+-----------+            |
C                    |   |            |              |  5           |
C                      |   +------------+-----------+            |
C                      +-->| edge_id    |   0       |  6:LAST <--+
C                        +------------+-----------+
C                          |            |              |  MAX_ADD
C                          +------------+-----------+
        DO IZ = IZ1,IZ2
         DO IY = IY1,IY2
          DO IX = IX1,IX2
 
                FIRST_ADD = VOXEL(IX,IY,IZ)

                IF(FIRST_ADD == 0)THEN
                !voxel encore vide
                 VOXEL(IX,IY,IZ)          = CURRENT_ADD ! adresse dans le tableau chaine de la premiere eddge trouvee occupant le voxel
                 LCHAIN_LAST(CURRENT_ADD) = CURRENT_ADD ! dernier=adresse pour l edge courante
                 LCHAIN_ELEM(CURRENT_ADD) = I               ! edge ID
                 LCHAIN_NEXT(CURRENT_ADD) = 0               ! pas de suivant car dernier de la liste   ! 
                ELSE
                 !voxel contenant deja une edge
                 PREV_ADD                 = LCHAIN_LAST(FIRST_ADD) ! devient l'avant-dernier
                 LCHAIN_LAST(FIRST_ADD)   = CURRENT_ADD                ! maj du dernier
                 LCHAIN_ELEM(CURRENT_ADD) = I                          ! edge ID
                 LCHAIN_NEXT(PREV_ADD)    = CURRENT_ADD                ! maj du suivant 0 -> CURRENT_ADD
                 LCHAIN_NEXT(CURRENT_ADD) = 0                          ! pas de suivant car dernier de la liste
                ENDIF

                CURRENT_ADD = CURRENT_ADD+1 

                IF( CURRENT_ADD.GE.MAX_ADD)THEN
                 !OPTIMISATION : suprresion du deallocate/GOTO debut.
                 !REALLOCATE SI PAS ASSEZ DE PLACE : inutile de recommencer de 1 a MAX_ADD-1, on poursuit de MAX_ADD a 2*MAX_ADD
                 MAX_ADD = 2 * MAX_ADD        
                 !print *, "reallocate"
                 LCHAIN_NEXT => IREALLOCATE(LCHAIN_NEXT, MAX_ADD)
                 LCHAIN_ELEM => IREALLOCATE(LCHAIN_ELEM, MAX_ADD)
                 LCHAIN_LAST => IREALLOCATE(LCHAIN_LAST, MAX_ADD)                               
                ENDIF                         

           ENDDO !IX
          ENDDO !IY
         ENDDO !IZ

       ENDDO

      END IF
C Barrier to wait task0 treatment
      CALL MY_BARRIER
C
!  Attention: allocation en NTHREADS x (NEDGE+NEDGE_REMOTE)
!  comprendre a quoi sert ce tableau
      ALLOCATE(TAGEDG(1:NEDGE+NEDGE_REMOTE))
      TAGEDG(1:NEDGE+NEDGE_REMOTE)=0
C=======================================================================
C     Sorting vs main shell edges
C=======================================================================
      IF(SH_EDGE==0) GOTO 300
C=======================================================================
C 3   A partir des voxels occupes par une edge main, on est en
C     mesure de connaitre toutes les edges escalves dans ce voisinage.
C     Ce qui permet de creer des couples cancidats pour le contact
C     Si la penetration est positive.
C=======================================================================

      J_STOK = 0

      DO I=1,NEDGE_T

        IEDG=ESHIFT+I

        IF(STFE(IEDG)==ZERO) CYCLE   ! on ne retient pas les facettes detruites
        NE=LEDGE(1,IEDG)

        IF(IABS(LEDGE(7,IEDG))==1) CYCLE ! Main solid edge

        !-------------------------------------------!
        !    (N1,N2) is the current main edge     !
        !-------------------------------------------!

        AAA = MARGE+BGAPEMX+GAPE(IEDG)

        N1 = LEDGE(5,IEDG)
        N2 = LEDGE(6,IEDG)
        MM1 = ITAB(N1)
        MM2 = ITAB(N2)
        AM1 = MIN(MM1,MM2)
        AM2 = MAX(MM1,MM2)

        IF(ILEV==2)THEN
          IMS1 = BITGET(EBINFLG(IEDG),0)
          IMS2 = BITGET(EBINFLG(IEDG),1)
        END IF

        !-------------------------------------------!
        !     X-coordinates of the four nodes       !
        !-------------------------------------------!

        XX1=X(1,N1)
        XX2=X(1,N2)
        YY1=X(2,N1)
        YY2=X(2,N2)
        ZZ1=X(3,N1)
        ZZ2=X(3,N2)
        XMAX_EDGM=MAX(XX1,XX2)+GAPE(IEDG) ! +TZINF
        XMIN_EDGM=MIN(XX1,XX2)-GAPE(IEDG) ! -TZINF
        YMAX_EDGM=MAX(YY1,YY2)+GAPE(IEDG) ! +TZINF
        YMIN_EDGM=MIN(YY1,YY2)-GAPE(IEDG) ! -TZINF
        ZMAX_EDGM=MAX(ZZ1,ZZ2)+GAPE(IEDG) ! +TZINF
        ZMIN_EDGM=MIN(ZZ1,ZZ2)-GAPE(IEDG) ! -TZINF
        !-------------------------------------------!
        !  VOXEL OCCUPIED BY THE BRICK              !
        !-------------------------------------------!
        !Voxel_lower_left_bound for this element---+
        IX1=INT(NBX*(XMIN_EDGM-AAA-XMINB)/(XMAXB-XMINB))
        IY1=INT(NBY*(YMIN_EDGM-AAA-YMINB)/(YMAXB-YMINB))
        IZ1=INT(NBZ*(ZMIN_EDGM-AAA-ZMINB)/(ZMAXB-ZMINB))
        IX1=MAX(1,2+MIN(NBX,IX1))
        IY1=MAX(1,2+MIN(NBY,IY1))
        IZ1=MAX(1,2+MIN(NBZ,IZ1))
        !Voxel_upper_right_bound for this element---+
        IX2=INT(NBX*(XMAX_EDGM+AAA-XMINB)/(XMAXB-XMINB))
        IY2=INT(NBY*(YMAX_EDGM+AAA-YMINB)/(YMAXB-YMINB))
        IZ2=INT(NBZ*(ZMAX_EDGM+AAA-ZMINB)/(ZMAXB-ZMINB))
        IX2=MAX(1,2+MIN(NBX,IX2))
        IY2=MAX(1,2+MIN(NBY,IY2))
        IZ2=MAX(1,2+MIN(NBZ,IZ2))

C--- IREMGAP - tag of deactivated lines
        IF(FLAGREMNODE==2)THEN
          K = KREMNODE_EDG(2*(IEDG-1)+1)
          L = KREMNODE_EDG(2*(IEDG-1)+2)-1
          DO M=K,L
            TAGREMLINE(REMNODE_EDG(M)) = 1
          ENDDO
        ENDIF

        DO IZ = IZ1,IZ2
         DO IY = IY1,IY2
          DO IX = IX1,IX2        
          
            CHAIN_ADD = VOXEL(IX,IY,IZ)       ! adresse dans le tableau chaine de la premiere edge stoquee dans le voxel
            DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT
              JJ = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant

              IF(TAGEDG(JJ)/=0)THEN ! edge deja traitee vs cette arete main

                CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                CYCLE
              END IF
              TAGEDG(JJ)=1

              !secnd edge nodes, exclure couples avec noeud commun
              IF (JJ.LE.NEDGE)THEN
                SS1= ITAB(LEDGE(5,JJ))
                SS2= ITAB(LEDGE(6,JJ))
                EID = LEDGE(8,JJ) 
              ELSE 
                SS1=IREM_EDGE(E_NODE1_GLOBID,JJ-NEDGE)
                SS2=IREM_EDGE(E_NODE2_GLOBID,JJ-NEDGE) 
                EID = IREM_EDGE(E_GLOBAL_ID,JJ-NEDGE)
              END IF
              
              IF( (SS1.EQ.MM1).OR.(SS1.EQ.MM2).OR.
     .            (SS2.EQ.MM1).OR.(SS2.EQ.MM2)    )THEN
                CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                CYCLE
              END IF
              
              IF(ILEV==2)THEN
                IF(JJ <= NEDGE) THEN
                  ISS1=BITGET(EBINFLG(JJ),0)
                  ISS2=BITGET(EBINFLG(JJ),1)
                ELSE
C                 double-check
                  ISS1 = BITGET(IREM_EDGE(E_EBINFLG,JJ-NEDGE),0) 
                  ISS2 = BITGET(IREM_EDGE(E_EBINFLG,JJ-NEDGE),1) 
                ENDIF

                IF(.NOT.((IMS1 == 1 .and. ISS2==1).or.
     .                   (IMS2 == 1 .and. ISS1==1)))THEN
                  CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                  CYCLE
                ENDIF
              ENDIF

              IF( JJ <= NEDGE) THEN
                EDGE_TYPE = LEDGE(7,JJ)  
              ELSE
                EDGE_TYPE = IREM_EDGE(E_TYPE ,JJ - NEDGE)
              ENDIF

              IF(IABS(LEDGE(7,IEDG))/=1 .AND. EDGE_TYPE .NE. 1 )THEN
                ! attention les traitements de i25dst3e pour les solides 
                ! ne sont pas symetriques main second.
                AS1 = MIN(SS1,SS2)
                AS2 = MAX(SS1,SS2)
                ! unicite des couples   
                IF(AM1 < AS1 .OR. (AM1 == AS1 .AND. AM2 < AS2))THEN
                  CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                  CYCLE
                ENDIF
              ENDIF


C             IREMPGAP
              IF (FLAGREMNODE == 2) THEN
                IF (JJ <= NEDGE) THEN
C-                Local Taged lines are removed
                  IF(TAGREMLINE(JJ)==1) THEN
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                  ENDIF

                  IF(TAGREMLINE(JJ)==0) THEN
C-                Even if it is not Remote lines have to be looked in remote list: Edge oin 2 procs
                     K = KREMNODE_EDG(2*(IEDG-1)+2)
                     L = KREMNODE_EDG(2*(IEDG-1)+3)-1
                     REMOVE_REMOTE = 0
                     DO M=K,L,2
                        IF ((SS1==REMNODE_EDG(M)).AND.(SS2==REMNODE_EDG(M+1))) REMOVE_REMOTE = 1
                     ENDDO
                     IF (REMOVE_REMOTE==1) THEN
                        CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                        CYCLE
                     ENDIF
                  ENDIF
                ELSE
C-                Remote lines are identified by nodes
                  K = KREMNODE_EDG(2*(IEDG-1)+2)
                  L = KREMNODE_EDG(2*(IEDG-1)+3)-1
                  REMOVE_REMOTE = 0
                  DO M=K,L,2
                    IF ((SS1==REMNODE_EDG(M)).AND.(SS2==REMNODE_EDG(M+1))) REMOVE_REMOTE = 1
                  ENDDO
                  IF (REMOVE_REMOTE==1) THEN
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                  ENDIF
                ENDIF
              ENDIF  

              J_STOK = J_STOK + 1 !on dispose d'un candidat
              ASSERT(JJ > 0) 
              ASSERT(JJ <= NEDGE + NEDGE_REMOTE)
              PROV_S(J_STOK) = JJ      !edge secnd
              PROV_M(J_STOK) = IEDG    !edge main
                                                            
              DEBUG_E2E(LEDGE(8,IEDG) ==  D_EM .AND. EID == D_ES,EID)
                                                            
c              IF(DEJA==0) NEDG = NEDG + 1     !nombre d edges candidate au calcul de contact (debug)                 
c              DEJA=1                          !l edge main IEDG fait l'objet d'une ecriture de candidat. On compte les edges main faisant l'objet dun couple candidate : on ne doit plus incrementer NEDG pour les autres edge secnd testees.
              CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
C-----------------------------------------------------
              IF(J_STOK.EQ.NVSIZ)THEN
                CALL I25STO_EDG(
     1               NVSIZ ,IRECT  ,X      ,II_STOK,INACTI,
     2               CANDS_E2E,CANDM_E2E ,MULNSNE,NOINT  ,MARGE ,
     3               I_MEM(1) ,PROV_S ,PROV_M ,IGAP0,CAND_A,
     4               NEDGE ,LEDGE  ,ITAB   ,DRAD   ,IGAP  ,
     5               GAPE  ,GAP_E_L,ADMSR  ,EDG_BISECTOR,VTX_BISECTOR ,
     6               CAND_P,IFQ,CANDE2E_FX ,CANDE2E_FY,CANDE2E_FZ,IFPEN_E)
                IF(I_MEM(1)/=0) GOTO 300
                J_STOK = 0
              ENDIF                 
C-----------------------------------------------------                   

              ENDDO ! WHILE(CHAIN_ADD /= 0)

          ENDDO   !NEXT IZ
         ENDDO    !NEXT IY
        ENDDO     !NEXT IZ 

C       Reset TAGEDG
        DO IZ = IZ1,IZ2
         DO IY = IY1,IY2
          DO IX = IX1,IX2        
 
            CHAIN_ADD = VOXEL(IX,IY,IZ)      
            DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT

              JJ = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant
              TAGEDG(JJ)=0

              CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)

            END DO

          ENDDO   !NEXT IZ
         ENDDO    !NEXT IY
        ENDDO     !NEXT IZ 


C--- IREMGAP - clean of tagremline
        IF(FLAGREMNODE==2)THEN
          K = KREMNODE_EDG(2*(IEDG-1)+1)
          L = KREMNODE_EDG(2*(IEDG-1)+2)-1
          DO M=K,L
            TAGREMLINE(REMNODE_EDG(M)) = 0
          ENDDO
        ENDIF

      ENDDO       !NEXT IEDG


C-------------------------------------------------------------------------
C     FIN DU TRI vs Main shell edges
C-------------------------------------------------------------------------
      IF(J_STOK.NE.0)CALL I25STO_EDG(
     1                 J_STOK ,IRECT  ,X     ,II_STOK,INACTI,
     2                 CANDS_E2E,CANDM_E2E ,MULNSNE,NOINT  ,MARGE ,
     3                 I_MEM(1) ,PROV_S ,PROV_M ,IGAP0,CAND_A,
     4                 NEDGE ,LEDGE  ,ITAB   ,DRAD   ,IGAP  ,
     5                 GAPE  ,GAP_E_L,ADMSR  ,EDG_BISECTOR,VTX_BISECTOR ,
     6                 CAND_P,IFQ,CANDE2E_FX ,CANDE2E_FY,CANDE2E_FZ,IFPEN_E)
 
 300  CONTINUE
C=======================================================================
C     Sorting vs main solid edges
C=======================================================================
      IF(SOL_EDGE==0) GOTO 400
C=======================================================================
C 3bis A partir des voxels occupes par une edge main, on est en
C     mesure de connaitre toutes les edges escalves dans ce voisinage.
C     Ce qui permet de creer des couples cancidats pour le contact
C     Si la penetration est positive.
C=======================================================================

      J_STOK = 0

      DO I=1,NRTM_T

        NE  =SSHIFT+I

        IF(MSEGTYP(NE)/=0) CYCLE ! not a solid edge
        IF(STF(NE)==ZERO) CYCLE  ! on ne retient pas les facettes detruites

        M1 = IRECT(1,NE)
        M2 = IRECT(2,NE)
        M3 = IRECT(3,NE)
        M4 = IRECT(4,NE)

        MM1= ITAB(M1)
        MM2= ITAB(M2)
        MM3= ITAB(M3)
        MM4= ITAB(M4)

        XX1=X(1,M1)
        YY1=X(2,M1)
        ZZ1=X(3,M1)
        XX2=X(1,M2)
        YY2=X(2,M2)
        ZZ2=X(3,M2)
        XX3=X(1,M3)
        YY3=X(2,M3)
        ZZ3=X(3,M3)
        XX4=X(1,M4)
        YY4=X(2,M4)
        ZZ4=X(3,M4)

        XMAX_EDGM=MAX(XX1,XX2,XX3,XX4) ! +TZINF
        XMIN_EDGM=MIN(XX1,XX2,XX3,XX4) ! -TZINF
        YMAX_EDGM=MAX(YY1,YY2,YY3,YY4) ! +TZINF
        YMIN_EDGM=MIN(YY1,YY2,YY3,YY4) ! -TZINF
        ZMAX_EDGM=MAX(ZZ1,ZZ2,ZZ3,ZZ4) ! +TZINF
        ZMIN_EDGM=MIN(ZZ1,ZZ2,ZZ3,ZZ4) ! -TZINF

        DX=EM02*(XMAX_EDGM-XMIN_EDGM)
        DY=EM02*(YMAX_EDGM-YMIN_EDGM)
        DZ=EM02*(ZMAX_EDGM-ZMIN_EDGM)
        XMAX_EDGM=XMAX_EDGM+DX
        XMIN_EDGM=XMIN_EDGM-DX
        YMAX_EDGM=YMAX_EDGM+DY
        YMIN_EDGM=YMIN_EDGM-DY
        ZMAX_EDGM=ZMAX_EDGM+DZ
        ZMIN_EDGM=ZMIN_EDGM-DZ

        AAA = MARGE+BGAPEMX ! filtrer vs GAPE(JJ) dans i25pen3_edg ! 

        !-------------------------------------------!
        !  VOXEL OCCUPIED BY THE BRICK              !
        !-------------------------------------------!
        !Voxel_lower_left_bound for this element---+
        IX1=INT(NBX*(XMIN_EDGM-AAA-XMINB)/(XMAXB-XMINB))
        IY1=INT(NBY*(YMIN_EDGM-AAA-YMINB)/(YMAXB-YMINB))
        IZ1=INT(NBZ*(ZMIN_EDGM-AAA-ZMINB)/(ZMAXB-ZMINB))
        IX1=MAX(1,2+MIN(NBX,IX1))
        IY1=MAX(1,2+MIN(NBY,IY1))
        IZ1=MAX(1,2+MIN(NBZ,IZ1))
        !Voxel_upper_right_bound for this element---+
        IX2=INT(NBX*(XMAX_EDGM+AAA-XMINB)/(XMAXB-XMINB))
        IY2=INT(NBY*(YMAX_EDGM+AAA-YMINB)/(YMAXB-YMINB))
        IZ2=INT(NBZ*(ZMAX_EDGM+AAA-ZMINB)/(ZMAXB-ZMINB))
        IX2=MAX(1,2+MIN(NBX,IX2))
        IY2=MAX(1,2+MIN(NBY,IY2))
        IZ2=MAX(1,2+MIN(NBZ,IZ2))

        IF(ILEV==2)THEN
          IMS1 = BITGET(MBINFLG(NE),0)
          IMS2 = BITGET(MBINFLG(NE),1)
        END IF

#ifdef WITH_ASSERT
C debug only
        IDS(1) = ITAB(IRECT(1,NE))
        IDS(2) = ITAB(IRECT(2,NE))
        IDS(3) = ITAB(IRECT(3,NE))
        IDS(4) = ITAB(IRECT(4,NE))
        DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM,XMIN_EDGM)
        DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM,YMIN_EDGM)
        DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM,ZMIN_EDGM)
        DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM,XMAX_EDGM)
        DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM,YMAX_EDGM)
        DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM,ZMAX_EDGM)
#endif

C--- IREMGAP - tag of deactivated lines
        IF(FLAGREMNODE==2)THEN
          K = KREMNODE_E2S(2*(NE-1)+1)
          L = KREMNODE_E2S(2*(NE-1)+2)-1
          DO M=K,L
            TAGREMLINE(REMNODE_E2S(M)) = 1
          ENDDO
        ENDIF

        DO IZ = IZ1,IZ2
         DO IY = IY1,IY2
          DO IX = IX1,IX2        
          
            CHAIN_ADD = VOXEL(IX,IY,IZ)       ! adresse dans le tableau chaine de la premiere edge stoquee dans le voxel
            DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT
              JJ = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant


              IF (JJ.LE.NEDGE)THEN
                EID = LEDGE(8,JJ)
              ELSE
                EID = IREM_EDGE(E_GLOBAL_ID,JJ-NEDGE)
              END IF

              IF(TAGEDG(JJ)/=0)THEN ! edge deja traitee vs cette arete main
                CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                CYCLE
              END IF
              TAGEDG(JJ)=1

              !secnd edge nodes, exclure couples avec noeud commun
              IF (JJ.LE.NEDGE)THEN
                SS1= ITAB(LEDGE(5,JJ))
                SS2= ITAB(LEDGE(6,JJ))
              ELSE
                SS1=IREM_EDGE(E_NODE1_GLOBID,JJ-NEDGE)
                SS2=IREM_EDGE(E_NODE2_GLOBID,JJ-NEDGE) 
              END IF

              IF((SS1.EQ.MM1).OR.(SS1.EQ.MM2).OR.(SS1.EQ.MM3).OR.(SS1.EQ.MM4).OR.
     .           (SS2.EQ.MM1).OR.(SS2.EQ.MM2).OR.(SS2.EQ.MM3).OR.(SS2.EQ.MM4))THEN
                CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                CYCLE
              END IF
             
              IF(ILEV==2)THEN
                IF(JJ <= NEDGE) THEN
                  ISS1=BITGET(EBINFLG(JJ),0)
                  ISS2=BITGET(EBINFLG(JJ),1)
                ELSE
                  ISS1 = BITGET(IREM_EDGE(E_EBINFLG,JJ-NEDGE),0) 
                  ISS2 = BITGET(IREM_EDGE(E_EBINFLG,JJ-NEDGE),1) 
                ENDIF
                IF(.NOT.((IMS1 == 1 .and. ISS2==1).or.
     .                   (IMS2 == 1 .and. ISS1==1)))THEN
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                ENDIF
              ENDIF

C             IREMPGAP
              IF (FLAGREMNODE == 2) THEN
                 IF (JJ.LE.NEDGE)THEN
C-                Local Taged lines are removed
                    IF(TAGREMLINE(JJ)==1) THEN
                       CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                       CYCLE
                    ENDIF
                 ELSE
C-                  Remote lines are identified by nodes
                    K = KREMNODE_E2S(2*(NE-1)+2)
                    L = KREMNODE_E2S(2*(NE-1)+3)-1
                    REMOVE_REMOTE = 0
                    DO M=K,L,2
                       IF ((SS1==REMNODE_E2S(M)).AND.(SS2==REMNODE_E2S(M+1))) REMOVE_REMOTE = 1
                    ENDDO
                    IF (REMOVE_REMOTE==1) THEN
                       CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                       CYCLE
                     ENDIF
                 ENDIF
              ENDIF

CCC ================== DEBUG PRINT =====================
C               IF(JJ > NEDGE) THEN
C                 WRITE(6,"(A,X,2I20)") "VOX REM",
C     .            INT_CHECKSUM(IDS,4,1),IREM_EDGE(E_GLOBAL_ID,JJ-NEDGE)
C               ELSE
C                 WRITE(6,"(A,X,2I20)") "VOX LOC",
C     .            INT_CHECKSUM(IDS,4,1),LEDGE(8,JJ)
C               ENDIF
CCC ================== DEBUG PRINT =====================
C           DEBUG_E2E(EID==D_ES.AND.INT_CHECKSUM(IDS,4,1)==D_EM,0)


C ===================================================
C-----------------------------------------------------                   
              J_STOK = J_STOK + 1 !on dispose d'un candidat
              PROV_S(J_STOK) = JJ   !edge secnd
              PROV_M(J_STOK) = NE     !segment main


C DEBUG ONLY
#ifdef WITH_ASSERT
              PROV_IDS(2,J_STOK) = EID
              PROV_IDS(1,J_STOK) = INT_CHECKSUM(IDS,4,1)
#endif


              ASSERT(JJ > 0) 
              ASSERT(JJ <= NEDGE + NEDGE_REMOTE)
C-----------------------------------------------------
              IF(J_STOK.EQ.NVSIZ)THEN
                CALL I25STO_E2S(
     1               NVSIZ ,IRECT  ,X         ,LL_STOK,INACTI,
     2               CANDS_E2S,CANDM_E2S,MULNSNS,NOINT  ,MARGE ,
     3               I_MEM(2) ,PROV_S ,PROV_M   ,IGAP0  ,CAND_B,
     4               NEDGE ,LEDGE  ,ITAB   ,DRAD   ,IGAP  ,
     5               GAP_M ,GAP_M_L,GAPE  ,GAP_E_L,ADMSR  ,
     6               EDG_BISECTOR,VTX_BISECTOR ,CAND_PS,PROV_IDS,
     7               IFQ,CANDE2S_FX ,CANDE2S_FY,CANDE2S_FZ,IFPEN_E2S)
                 
                IF(I_MEM(2)/=0) GOTO 400
                J_STOK = 0
              ENDIF                 
C-----------------------------------------------------                   

              CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD) ! Next RTM

            ENDDO ! WHILE(CHAIN_ADD /= 0)

          ENDDO   !NEXT IZ
         ENDDO    !NEXT IY
        ENDDO     !NEXT IZ 

C       Reset TAGEDG
        DO IZ = IZ1,IZ2
         DO IY = IY1,IY2
          DO IX = IX1,IX2        
 
            CHAIN_ADD = VOXEL(IX,IY,IZ)      
            DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT

              JJ = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant
              TAGEDG(JJ)=0

              CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)

            END DO

          ENDDO   !NEXT IZ
         ENDDO    !NEXT IY
        ENDDO     !NEXT IZ 

C--- IREMGAP - clean of tagremline
        IF(FLAGREMNODE==2)THEN
          K = KREMNODE_E2S(2*(NE-1)+1)
          L = KREMNODE_E2S(2*(NE-1)+2)-1
          DO M=K,L
            TAGREMLINE(REMNODE_E2S(M)) = 0
          ENDDO
        ENDIF

      ENDDO       !NEXT IEDG
C-------------------------------------------------------------------------
C     FIN DU TRI vs Main solid edges
C-------------------------------------------------------------------------
      IF(J_STOK.NE.0)CALL I25STO_E2S(
     1                 J_STOK ,IRECT  ,X        ,LL_STOK,INACTI,
     2                 CANDS_E2S,CANDM_E2S,MULNSNS,NOINT  ,MARGE ,
     3                 I_MEM(2) ,PROV_S ,PROV_M   ,IGAP0  ,CAND_B,
     4                 NEDGE ,LEDGE  ,ITAB   ,DRAD   ,IGAP  ,
     5                 GAP_M ,GAP_M_L,GAPE  ,GAP_E_L,ADMSR  ,
     6                 EDG_BISECTOR,VTX_BISECTOR ,CAND_PS,PROV_IDS,
     7                 IFQ,CANDE2S_FX ,CANDE2S_FY,CANDE2S_FZ,IFPEN_E2S)
 
C=======================================================================
C 4   remise a zero des noeuds dans les boites
C=======================================================================

CC=============== DEBUG
C       DO I = 1, LL_STOK
C         JJ = CANDS_E2S(I)
C         NE = CANDM_E2S(I)
C         IDS(1) = ITAB(IRECT(1,NE))
C         IDS(2) = ITAB(IRECT(2,NE))
C         IDS(3) = ITAB(IRECT(3,NE))
C         IDS(4) = ITAB(IRECT(4,NE))
C         IF(JJ > NEDGE) THEN
C           CRITE(6,"(A,X,2I20)") "VOX REM",
C     .      INT_CHECKSUM(IDS,4,1),IREM_EDGE(E_GLOBAL_ID,JJ-NEDGE)
C         ELSE
C           WRITE(6,"(A,X,2I20)") "VOX LOC",
C     .      INT_CHECKSUM(IDS,4,1),LEDGE(8,JJ)
C         ENDIF
C       ENDDO
C=======================================================================


 400  CONTINUE
      


C Barrier to avoid reinitialization before end of sorting
      CALL MY_BARRIER

      TMIN(1) = MIN_IX
      TMIN(2) = MIN_IY
      TMIN(3) = MIN_IZ

      TMAX(1) = MAX_IX
      TMAX(2) = MAX_IY
      TMAX(3) = MAX_IZ
      
      IF (ITASK==0)THEN   
        !RESET VOXEL WITHIN USED RANGE ONLY
        DO K= TMIN(3),TMAX(3)
          DO J= TMIN(2),TMAX(2)
             DO I= TMIN(1),TMAX(1)
                VOXEL(I,J,K) = 0
             END DO
          END DO
        END DO 
        !CHAINED LIST DEALLOCATION
        DEALLOCATE(LCHAIN_NEXT)
        DEALLOCATE(LCHAIN_ELEM)
        DEALLOCATE(LCHAIN_LAST)   
        IF(FLAGREMNODE==2) DEALLOCATE(TAGREMLINE)               
      ENDIF

      DEALLOCATE(TAGEDG)

C=======================================================================

      RETURN
      END
