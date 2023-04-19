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
Chd|  INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CHECK_IS_ON_SEGMENT           source/initial_conditions/inigrav/inigrav_load.F
Chd|        INIGRAV_EOS                   source/initial_conditions/inigrav/inigrav_eos.F
Chd|        INIGRAV_M37                   source/initial_conditions/inigrav/inigrav_m37.F
Chd|        INIGRAV_M51                   source/initial_conditions/inigrav/inigrav_m51.F
Chd|        INIGRAV_PART_LIST             source/initial_conditions/inigrav/inigrav_part_list.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIGRAV                       share/modules1/inigrav_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE INIGRAV_LOAD(ELBUF_TAB ,IPART  ,IGRPART ,IPARG  ,IPARTTG  ,
     1                        IPARTS    ,IPARTQ ,X       ,IXS    ,IXQ      ,
     2                        IXTG      ,PM     ,IPM     ,BUFMAT ,MULTI_FVM,
     3                        ALE_CONNECTIVITY ,NV46   ,IGRSURF ,ITAB   ,EBCS_TAB,
     4                        NPF       ,TF)
C-----------------------------------------------
C   Description
C-----------------------------------------------
C This subroutine is computing distance from a given surface along vector provided by /GRAV option.
C    Surface can be planar or provided by user.
C
C REMINDER :
!     
!   IGRSURF(IGS)%ELEM(J)             :: element attached to the segment(J) of the surface
!   IGRSURF(IGS)%ELTYP(J)            :: type of element attached to the segment of the 
!     ITYP = 0  - surf of segments                                     
!     ITYP = 1  - surf of solids                                       
!     ITYP = 2  - surf of quads                                        
!     ITYP = 3  - surf of SH4N                                         
!     ITYP = 4  - line of trusses                                      
!     ITYP = 5  - line of beams                                        
!     ITYP = 6  - line of springs                                      
!     ITYP = 7  - surf of SH3N                                         
!     ITYP = 8  - line of XELEM (nstrand element)                      
!     ITYP = 101 - ISOGEOMETRIQUE                                      
!
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE INIGRAV
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE EBCS_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN),TARGET                   :: IPART(LIPART1,*)
      INTEGER, INTENT(IN)                          :: IPARTS(NUMELS), IPARTQ(NUMELQ), IPARTTG(NUMELTG)
      INTEGER, INTENT(IN)                          :: IPM(NPROPMI, NUMMAT),ITAB(NUMNOD)
      INTEGER, INTENT(IN)                          :: IPARG(NPARG,NGROUP),NV46
      INTEGER, INTENT(IN), TARGET                  :: IXS(NIXS, NUMELS), IXQ(NIXQ, NUMELQ), IXTG(NIXTG, NUMELTG)
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP),TARGET :: ELBUF_TAB
      my_real, INTENT(IN)                          :: X(3,NUMNOD), PM(NPROPM,NUMMAT), BUFMAT(*)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN)           :: MULTI_FVM
      TYPE (SURF_), DIMENSION(NSURF),INTENT(IN)    :: IGRSURF      
      TYPE(T_EBCS_TAB), INTENT(INOUT)              :: EBCS_TAB
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRPART)  :: IGRPART
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      INTEGER,INTENT(IN)::NPF(SNPC)
      my_real,INTENT(IN)::TF(STF)      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: GRAV0,BX,BY,BZ,NX,NY,NZ,DOTPROD,DOTP,RHO0,NGX,NGY,NGZ,ALPHA,INTERP(3),ValA,ValB,NORM
      my_real :: Z(3), DEPTH(MVSIZ), N(3,8),VEC(3), DELTA_P(MVSIZ),PSURF,PGRAV(MVSIZ),VFRAC,P(3,1:4)
      my_real :: LAMBDA,DIAG1(3),DIAG2(3),TOL,DIST,B(3),VOLFRAC
      my_real,ALLOCATABLE,DIMENSION(:,:) :: MIN_COOR, MAX_COOR, N_SURF, ZF
      INTEGER :: K,KK,IGRP,ISURF,IGRAV,NPART_IN_GROUP,MLW,NG,IAD0,II,NEL,J,IE,I,IERROR,IMAT,M(MVSIZ),Q,NSEG,ISEG
      INTEGER :: USERID
      INTEGER :: NOD(8), MATID, IADBUF,IFORM,NEL2,LIST(MVSIZ),NBMAT,MATLAW,NIX,UID,SURF_TYPE,IELTYP
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IP
      TYPE(G_BUFEL_) ,POINTER            :: GBUF
      TYPE(L_BUFEL_), POINTER            :: LBUF
      LOGICAL :: lCYCLE,lERROR,lTRIA,lFOUND_PROJ,lTEST,l_PLANAR_SURF,l_USER_SURF
      INTEGER, DIMENSION(:, :), POINTER  :: IX
      character*64                       :: ERRMSG
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      CALL INIGRAV_PART_LIST( IPART  ,IGRPART, EBCS_TAB ) !list related parts to apply gravity loading with /ebcs/nrf

      DO K=1,NINIGRAV     
        IGRP    = INIGRV(1,K)
        ISURF   = INIGRV(2,K)
        IGRAV   = INIGRV(3,K)  
        UID     = INIGRV(4,K)        
        BX      = LINIGRAV(01,K)
        BY      = LINIGRAV(02,K)
        BZ      = LINIGRAV(03,K)        
        NX      = LINIGRAV(04,K)
        NY      = LINIGRAV(05,K)
        NZ      = LINIGRAV(06,K)        
        GRAV0   = LINIGRAV(07,K)  
        NGX     = LINIGRAV(08,K)
        NGY     = LINIGRAV(09,K)
        NGZ     = LINIGRAV(10,K) 
        PSURF   = LINIGRAV(11,K)        
        IF (GRAV0==ZERO)CYCLE          !nothing to do if g=0 at time=0 => Delta_P=0 => P=P(t=0)=P0
        !READING GRPART DATA
        IF(IGRP/=0)THEN
         NPART_IN_GROUP = IGRPART(IGRP)%NENTITY
         ALLOCATE(IP(NPART_IN_GROUP),STAT=IERROR)
         IF(IERROR/=0)CALL ANCMSG(MSGID   = 268,
     .                            ANMODE  = ANINFO,
     .                            MSGTYPE = MSGERROR,
     .                            C1      = 'INIGRAV')
         IP(1:NPART_IN_GROUP) = IPART(4,IGRPART(IGRP)%ENTITY(1:NPART_IN_GROUP))
        ELSE
         !by default treat all parts
         IAD0 = 0
         NPART_IN_GROUP = 0
        ENDIF
        l_PLANAR_SURF = .FALSE.
        l_USER_SURF   = .FALSE.
        SURF_TYPE=-1   !-1:unaffected
        IF(ISURF>0)SURF_TYPE=IGRSURF(ISURF)%TYPE
        IF(SURF_TYPE==200 .OR. SURF_TYPE == -1)THEN
          l_PLANAR_SURF=.TRUE.
        ELSEIF(SURF_TYPE == 0) THEN  
          l_USER_SURF=.TRUE.
          NSEG = IGRSURF(ISURF)%NSEG
          !STORING DATA FROM USER SURFACE (BOX,NORMAL,CENTROID)
          ALLOCATE(MAX_COOR(3,NSEG),MIN_COOR(3,NSEG),N_SURF(3,NSEG),ZF(3,NSEG))
          DO ISEG=1,NSEG
            lTRIA=.FALSE.
            IF(IGRSURF(ISURF)%NODES(ISEG,4)==0)lTRIA=.TRUE.
            IF(lTRIA)THEN
                  P(1:3,4)=P(1:3,3)
            ENDIF
            !face coordinates
            P(1,1:4)=X(1,IGRSURF(ISURF)%NODES(ISEG,1:4))
            P(2,1:4)=X(2,IGRSURF(ISURF)%NODES(ISEG,1:4))
            P(3,1:4)=X(3,IGRSURF(ISURF)%NODES(ISEG,1:4))            
            !box (to skip cell centroid if too far from it)
            MIN_COOR(1,ISEG)=MIN(P(1,1),P(1,2),P(1,3),P(1,4))
            MIN_COOR(2,ISEG)=MIN(P(2,1),P(2,2),P(2,3),P(2,4))
            MIN_COOR(3,ISEG)=MIN(P(3,1),P(3,2),P(3,3),P(3,4))
            MAX_COOR(1,ISEG)=MAX(P(1,1),P(1,2),P(1,3),P(1,4))
            MAX_COOR(2,ISEG)=MAX(P(2,1),P(2,2),P(2,3),P(2,4))
            MAX_COOR(3,ISEG)=MAX(P(3,1),P(3,2),P(3,3),P(3,4))
            !face normal 
            DIAG1(1)=(P(1,3)-P(1,1))
            DIAG1(2)=(P(2,3)-P(2,1))
            DIAG1(3)=(P(3,3)-P(3,1))
            DIAG2(1)=(P(1,4)-P(1,2)) 
            DIAG2(2)=(P(2,4)-P(2,2))
            DIAG2(3)=(P(3,4)-P(3,2))
            N_SURF(1,ISEG)= DIAG1(2)*DIAG2(3)-DIAG1(3)*DIAG2(2)
            N_SURF(2,ISEG)=-DIAG1(1)*DIAG2(3)+DIAG1(3)*DIAG2(1)
            N_SURF(3,ISEG)= DIAG1(1)*DIAG2(2)-DIAG1(2)*DIAG2(1)                                      
            NORM = SQRT(N_SURF(1,ISEG)*N_SURF(1,ISEG)+N_SURF(2,ISEG)*N_SURF(2,ISEG)+N_SURF(3,ISEG)*N_SURF(3,ISEG))
            N_SURF(1,ISEG)=N_SURF(1,ISEG)/NORM
            N_SURF(2,ISEG)=N_SURF(2,ISEG)/NORM
            N_SURF(3,ISEG)=N_SURF(3,ISEG)/NORM  
            !face centroid
            IF(lTRIA)THEN
              ZF(1,ISEG)=THIRD*SUM(P(1,1:3))
              ZF(2,ISEG)=THIRD*SUM(P(2,1:3))
              ZF(3,ISEG)=THIRD*SUM(P(3,1:3))
            ELSE
              ZF(1,ISEG)=FOURTH*SUM(P(1,1:4))
              ZF(2,ISEG)=FOURTH*SUM(P(2,1:4))
              ZF(3,ISEG)=FOURTH*SUM(P(3,1:4))                    
            ENDIF                         
          ENDDO 
          !define a tolerance to check if cell centroid too far from face (box)
          ISEG=1
          TOL=EM02*MAX(MAX_COOR(1,ISEG)-MIN_COOR(1,ISEG), MAX_COOR(2,ISEG)-MIN_COOR(2,ISEG), MAX_COOR(3,ISEG)-MIN_COOR(3,ISEG))  
        ENDIF
         

        DO NG=1,NGROUP
          MLW   =  IPARG(1,NG)
          ITY   =  IPARG(5,NG)
          NEL   =  IPARG(2,NG)
          NFT   =  IPARG(3,NG)
          IAD   =  IPARG(4,NG) - 1
          GBUF  => ELBUF_TAB(NG)%GBUF
          PGRAV(1:NEL)=ZERO
          IF(MLW==0)CYCLE
          IF( (ITY == 1. .AND.N2D==0) .OR. ((ITY == 2. .OR. ITY == 7).AND.N2D/=0) )THEN      
            IF(ITY==1)THEN
              IMAT     = IXS(1,1+NFT)
              M(1:NEL) = IPARTS(1+NFT:NEL+NFT)
            ELSEIF(ITY==2)THEN
              IMAT     = IXQ(1,1+NFT)
              M(1:NEL) = IPARTQ(1+NFT:NEL+NFT)
            ELSEIF(ITY==7)THEN
              IMAT     = IXTG(1,1+NFT)
              M(1:NEL) = IPARTTG(1+NFT:NEL+NFT)
            ENDIF
            !check if PART must be treated or not
            LIST(1:MVSIZ) = 0
            NEL2 = 0
            !il peut y avoir plusieurs PARTS par groupe. Tous les elements ont meme mat et meme PROP.
            ! creation dune list LIST(1:NEL2) c  1:NEL  avec les elements des parts a traiter
            !===================================================================!
            !                     SELECTION DES ELEMENTS                        !
            !===================================================================! 
            IF(IGRP/=0)THEN
              lCYCLE = .TRUE.
              DO I=1,NEL
                DO Q=1,NPART_IN_GROUP
                  IF(IPART(4,M(I))==IP(Q))THEN
                    NEL2 = NEL2+1
                    LIST(NEL2) = I
                    lCYCLE = .FALSE.
                    EXIT
                  ENDIF
                ENDDO
              ENDDO!next I
              IF(lCYCLE)CYCLE
            ELSE
              NEL2=NEL
              DO I=1,NEL
               LIST(I) = I !(/1:NEL/) !identite
              ENDDO
            ENDIF
            ! LIST est la sous liste de 1:NEL des elements dont la PART est a traiter avec INIGRAV

            ! This group is going to be treated since PART_ID in GRPART_ID
            !===================================================================!
            !                     CALCUL DES CENTROIDES                         !
            !===================================================================! 
            DO KK=1,NEL2
              I = LIST(KK)
              IE                    = I + NFT
              USERID                = 0
              IF(N2D==0)THEN
                N(1:3,1)            = X(1:3,IXS(2,IE))
                N(1:3,2)            = X(1:3,IXS(3,IE))
                N(1:3,3)            = X(1:3,IXS(4,IE))
                N(1:3,4)            = X(1:3,IXS(5,IE))
                N(1:3,5)            = X(1:3,IXS(6,IE))
                N(1:3,6)            = X(1:3,IXS(7,IE))
                N(1:3,7)            = X(1:3,IXS(8,IE))
                N(1:3,8)            = X(1:3,IXS(9,IE))
                Z(1)                = ONE_OVER_8*(SUM(N(1,1:8)))
                Z(2)                = ONE_OVER_8*(SUM(N(2,1:8)))
                Z(3)                = ONE_OVER_8*(SUM(N(3,1:8)))
                USERID              = IXS(NIXS,IE)
              ELSEIF(ITY==2)THEN  
                N(1:3,1)            = X(1:3,IXQ(2,IE))
                N(1:3,2)            = X(1:3,IXQ(3,IE))
                N(1:3,3)            = X(1:3,IXQ(4,IE))
                N(1:3,4)            = X(1:3,IXQ(5,IE))
                Z(1)                = FOURTH*(SUM(N(1,1:4)))
                Z(2)                = FOURTH*(SUM(N(2,1:4)))
                Z(3)                = FOURTH*(SUM(N(3,1:4))) 
                USERID              = IXQ(NIXQ,IE)                 
              ELSEIF(ITY==7)THEN  
                N(1:3,1)            = X(1:3,IXTG(2,IE))
                N(1:3,2)            = X(1:3,IXTG(3,IE))
                N(1:3,3)            = X(1:3,IXTG(4,IE))
                N(1:3,4)            = ZERO
                Z(1)                = THIRD*(SUM(N(1,1:3)))
                Z(2)                = THIRD*(SUM(N(2,1:3)))
                Z(3)                = THIRD*(SUM(N(3,1:3))) 
                USERID              = IXTG(NIXTG,IE)                         
              ENDIF            
              !===================================================================! 
              IF(l_PLANAR_SURF)THEN
              !===================================================================!
              !                 --- CALCUL DES DISTANCES ---                      !
              !-PLANAR SURFACE :          IGRSURF(ISURF)%TYPE = 200 OR ISURF=0    !
              !===================================================================!               
              !---NORMAL : NX,NY,NZ 
              !---BASIS POINT : BX,BY,BZ
                 !intersection point such as INTERP = Z + ALPHA . NG   et   <B-INTERP,N> = 0, donc :
                 ALPHA                 = (BX-Z(1))*NX+(BY-Z(2))*NY+(BZ-Z(3))*NZ / (NGX*NX+NGY*NY+NGZ*NZ)  !non nul car verification a la lecture
                 INTERP(1)             = Z(1) + ALPHA*NGX
	         INTERP(2)             = Z(2) + ALPHA*NGY
	         INTERP(3)             = Z(3) + ALPHA*NGZ
                 ! profondeur = <INTERP_P , N>
                 !DEPTH(I) = (INTERP(1)-Z(1))*(INTERP(1)-Z(1))+(INTERP(2)-Z(2))*(INTERP(2)-Z(2))+(INTERP(3)-Z(3))*(INTERP(3)-Z(3))
                 !DEPTH(I) = SQRT(DEPTH(I))
                 DEPTH(KK)= (INTERP(1)-Z(1))*NGX+(INTERP(2)-Z(2))*NGY+(INTERP(3)-Z(3))*NGZ
                 DEPTH(KK) = -DEPTH(KK)  
                 lERROR=.FALSE.
                 lFOUND_PROJ=.TRUE.
              ELSEIF(l_USER_SURF) THEN             
              !===================================================================!
              !                 --- CALCUL DES DISTANCES ---                      !
              !-USER SURFACE :              IGRSURF(ISURF)%TYPE=0 ,   ISURF/=0    !
              !===================================================================!
                 !LOOP ON CELL, THEN LOOP ON FACE :  complexity is O(NCELL*NFACE), can be optimized with space partitioning
                 lERROR=.FALSE.
                 lFOUND_PROJ=.FALSE.
                 DO ISEG=1,NSEG
                
                   IF(NGX==ZERO)THEN
                     IF(Z(1)+TOL < MIN_COOR(1,ISEG)  .OR. Z(1)-TOL > MAX_COOR(1,ISEG))CYCLE
                   ENDIF
                   IF(NGY==ZERO)THEN
                     IF(Z(2)+TOL < MIN_COOR(2,ISEG)  .OR. Z(2)-TOL > MAX_COOR(2,ISEG))CYCLE
                   ENDIF
                   IF(NGZ==ZERO)THEN
                     IF(Z(3)+TOL < MIN_COOR(3,ISEG)  .OR. Z(3)-TOL > MAX_COOR(3,ISEG))CYCLE 
                   ENDIF                                     
                   DOTP = N_SURF(1,ISEG)*NGX + N_SURF(2,ISEG)*NGY + N_SURF(3,ISEG)*NGZ
                   IF(ABS(DOTP)<=EM04)THEN
                     CALL ANCMSG(MSGID   = 90,
     .                           ANMODE  = ANINFO,
     .                           MSGTYPE = MSGERROR,
     .                           I1=UID,
     .                           C1="INPUT SURFACE HAS INCOMPATIBLE SLOPE WITH GRAVITY DIRECTION"
     .                     )  
                      lERROR=.TRUE.
                      EXIT                   
                   ENDIF
                   P(1,1:4)=X(1,IGRSURF(ISURF)%NODES(ISEG,1:4))
                   P(2,1:4)=X(2,IGRSURF(ISURF)%NODES(ISEG,1:4))
                   P(3,1:4)=X(3,IGRSURF(ISURF)%NODES(ISEG,1:4))            
                   ! Let INTERP the normal projection point on plane generated by current face ISEG
                   ! INTERP = Z + LAMBDA*NG  (colinear to gravity direction , NG gravity vector, Z cell centroid, LAMBDA unknown scalar)
                   ! <INTERP-B,N_SURF> = 0   (must satisfying plane equation,  B basis point, Z cell centroid)
                   !  => INTERP = ...
                   B(1:3)=ZF(1:3,ISEG)
                   DIST=(B(1)-Z(1))*(B(1)-Z(1)) + (B(2)-Z(2))*(B(2)-Z(2)) + (B(3)-Z(3))*(B(3)-Z(3))
                   IF(DIST<=TOL*TOL)THEN
                     !use another basis point if face centroid superimposed with cell centroid
                     B(1:3)=P(1:3,1)                                         
                   ENDIF
                   LAMBDA = (B(1)-Z(1))*N_SURF(1,ISEG) + (B(2)-Z(2))*N_SURF(2,ISEG) + (B(3)-Z(3))*N_SURF(3,ISEG)
                   LAMBDA = LAMBDA / DOTP
                   INTERP(1) = Z(1)+LAMBDA*NGX
                   INTERP(2) = Z(2)+LAMBDA*NGY
                   INTERP(3) = Z(3)+LAMBDA*NGZ
                   !CHECK INTERP IS INSIDE FACE, OTHERWISE SKIP IT
                   CALL CHECK_IS_ON_SEGMENT(P(1:3,1:4),B,INTERP,lTRIA,lTEST)
                   IF(.NOT.lTEST)CYCLE
                   DEPTH(KK)= (INTERP(1)-Z(1))*NGX+(INTERP(2)-Z(2))*NGY+(INTERP(3)-Z(3))*NGZ
                   DEPTH(KK) = -DEPTH(KK)
                   lFOUND_PROJ=.TRUE.                                     
                 ENDDO           
                 IF(lERROR)EXIT
                 IF(.NOT.lFOUND_PROJ)THEN
                   ERRMSG = "UNABLE TO PROJECT ON SURFACE CENTROID FROM CELL ID=          "
                   WRITE(ERRMSG(52:62),FMT='(I10)')USERID
                   CALL ANCMSG(MSGID   = 90,
     .                         ANMODE  = ANINFO,
     .                         MSGTYPE = MSGERROR,
     .                         I1=UID,
     .                         C1=ERRMSG(1:62)
     .                     )  
                   EXIT
                 ENDIF
              ENDIF !l_PLANAR_SURF elif l_USER_SURF
            ENDDO !next I
            IF(lERROR)EXIT
            IF(.NOT.lFOUND_PROJ)EXIT
          !===================================================================!
          !                 INITIALISATION DE L'ETAT INITIAL                  !
          !===================================================================! 
           IF (N2D == 0) THEN
              MATID = IXS(1, 1 + NFT)
           ELSEIF(ITY==2)THEN
              MATID = IXQ(1, 1 + NFT)
           ELSEIF(ITY==7)THEN
              MATID = IXTG(1, 1 + NFT)              
           ENDIF
           IADBUF = MAX(1,IPM(7, MATID))   ! Adress of data for the material in the material buffer
           !NEL2 is size of subgroup in LIST(1:NEL2),  NEL is size of element group needed to shift indexes in MBUF%VAR and GBUF%SIG          
           SELECT CASE (MLW)
          !=======================================!
          !     3,4,6,49 (ANY EOS)                !
          !=======================================! 
           CASE (3, 4, 6, 10, 49)      !Material law defined with Equations Of States 
              RHO0 = PM(1,MATID)                  
              CALL INIGRAV_EOS(NEL,NEL2, NG, MATID, IPM, GRAV0, RHO0, DEPTH, PM, BUFMAT(IADBUF), 
     .                         ELBUF_TAB, PSURF, LIST , PGRAV, 0 ,MLW, NPF, TF)
          !=======================================!
          !     MM-ALE 37                         !
          !=======================================! 
           CASE (37)
              CALL INIGRAV_M37(NEL,NEL2, NG, MATID, IPM, GRAV0,  DEPTH, PM, BUFMAT(IADBUF), ELBUF_TAB, PSURF, LIST)
          !=======================================!
          !     MM-ALE 51                         !
          !=======================================! 
           CASE (51)
              IF (N2D == 0) THEN ! HEXA
                 IX => IXS(1:NIXS, 1:NUMELS)
                 NIX = NIXS
              ELSEIF (ITY == 2) THEN! QUADS
                 IX => IXQ(1:NIXQ, 1:NUMELQ)
                 NIX = NIXQ
              ELSEIF (ITY == 7) THEN! TRIAS
                 IX => IXTG(1:NIXTG, 1:NUMELTG)
                 NIX = NIXTG                 
              ENDIF
              IFORM = BUFMAT(IADBUF + 31 - 1)
              IF (IFORM /= 3) THEN
                 CALL INIGRAV_M51(NEL  , NEL2,     NG, MATID,  IPM, GRAV0, DEPTH, PM,    BUFMAT(IADBUF), ELBUF_TAB, 
     .                            PSURF, LIST,  ALE_CONNECTIVITY,   NV46, IX , NIX  , NFT  , BUFMAT, IPARG)
              ELSE
                CALL ANCMSG(MSGID   = 84,
     .                      ANMODE  = ANINFO,
     .                      MSGTYPE = MSGWARNING
     .                     )
              ENDIF
          !=======================================!
          !     MULTI-FLUID 151                   !
          !=======================================! 
           CASE(151)
            NBMAT = IPARG(20, NG)
            IF (N2D == 0) THEN ! HEXA
               IX => IXS(1:NIXS, LFT + NFT:LLT + NFT)
            ELSEIF (ITY == 2) THEN! QUADS
               IX => IXQ(1:NIXQ, LFT + NFT:LLT + NFT)
            ELSEIF (ITY == 7) THEN ! TRIAS
               IX => IXTG(1:NIXTG, LFT + NFT:LLT + NFT)
            ENDIF
              !mixture density : in case of inigrav with (water+vapor) vapor eos must use mixture density
              RHO0=ZERO
              DO IMAT=1,NBMAT           
                MATID  = IPM(20 + IMAT, IX(1, 1)) 
                VOLFRAC=  PM(20 + IMAT, IX(1, 1))
                MATLAW = IPM(2, MATID)
                RHO0   = RHO0+PM(1,MATID)*VOLFRAC 
              ENDDO           

              DO IMAT=1,NBMAT
                MATID  = IPM(20 + IMAT, IX(1, 1))  
                MATLAW = IPM(2, MATID)            
                CALL INIGRAV_EOS(NEL,   NEL2 , NG   , MATID, IPM, GRAV0, RHO0, DEPTH, PM, BUFMAT(IADBUF), ELBUF_TAB, 
     .                           PSURF, LIST , PGRAV, IMAT , MLW, NPF, TF)
              ENDDO
              ! GLOBAL STATE
              DO KK=1,NEL2
                I                      = LIST(KK)
                IF(NBMAT==1)THEN 
                  ! warning in this case GBUF => LBUF so dont reinit GBUF to 0.
                  !LBUF         => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
                  !GBUF%RHO(I)            = LBUF%RHO(I)
                  !GBUF%EINT(I)           = LBUF%EINT(I)
                  GBUF%SIG(I + 0 * NEL) = -PGRAV(KK)
                  GBUF%SIG(I + 1 * NEL) = -PGRAV(KK)
                  GBUF%SIG(I + 2 * NEL) = -PGRAV(KK)
                ELSE
                  GBUF%RHO(I)           = ZERO
                  GBUF%EINT(I)          = ZERO
                  DO IMAT=1,NBMAT
                    LBUF         => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
                    VFRAC        = LBUF%VOL(I) / GBUF%VOL(I)
                    GBUF%EINT(I) = GBUF%EINT(I) + VFRAC*LBUF%EINT(I)
                    GBUF%RHO(I)  = GBUF%RHO(I)  + VFRAC*LBUF%RHO(I)                    
                  ENDDO
                  GBUF%SIG(I + 0 * NEL) = -PGRAV(KK)
                  GBUF%SIG(I + 1 * NEL) = -PGRAV(KK)
                  GBUF%SIG(I + 2 * NEL) = -PGRAV(KK) 
                ENDIF
              ENDDO
          !=======================================!
          !              DEFAULT                  !
          !=======================================! 
           CASE DEFAULT  
              IF (N2D == 0) THEN ! HEXA
                 IX => IXS(1:NIXS, 1:NUMELS)
                 NIX = NIXS
              ELSEIF (ITY == 2) THEN! QUADS
                 IX => IXQ(1:NIXQ, 1:NUMELQ)
                 NIX = NIXQ
              ELSEIF (ITY == 7) THEN! TRIAS
                 IX => IXTG(1:NIXTG, 1:NUMELTG)
                 NIX = NIXTG                 
              ENDIF            
              CALL ANCMSG(MSGID   = 89,
     .                    ANMODE  = ANINFO,
     .                    MSGTYPE = MSGWARNING,
     .                    I1      = UID,
     .                    I2      = IPART(4,M(I)),
     .                    I3      = MLW)
              EXIT
           END SELECT

          ENDIF !(ITY == 1. .OR. ITY == 2) .AND.MLW /= 0)
        ENDDO   !next NG
            
        IF(ALLOCATED(MAX_COOR))DEALLOCATE(MAX_COOR)
        IF(ALLOCATED(MIN_COOR))DEALLOCATE(MIN_COOR)
        IF(ALLOCATED(N_SURF))DEALLOCATE(N_SURF)
        IF(ALLOCATED(ZF))DEALLOCATE(ZF)
        IF(IGRP/=0)THEN
          IF(ALLOCATED(IP))DEALLOCATE(IP)  
        ENDIF

      ENDDO    !next K

      RETURN
      END




Chd|====================================================================
Chd|  CHECK_IS_ON_SEGMENT           source/initial_conditions/inigrav/inigrav_load.F
Chd|-- called by -----------
Chd|        INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHECK_IS_ON_SEGMENT(P,Z,H,lTRIA,lTEST)
C-----------------------------------------------
C   Description
C-----------------------------------------------
C Check if given point H is on quadrangle P(:,1),P(:,2),P(:,3),P(:,4)
C using triangulation with midpoint Z(:)
!
!                   P(:,1)
!                   +
!                  /|\
!                 / | \
!                /  |  \
!               /   |   \
!              /  1 |  4 \
!             /     |     \
!     P(:,2) +------Z+-----+B = P(:,4)
!             \     |     /   
!              \  2 |  3 /    
!               \   |   /     
!                \  |  /      
!                 \ | /       
!                  \|/
!                   +
!                   A =  P(:,3)

!Il s'agit de savoir avec cette fonction s'il est a l'interieur
! ou a l'exterieur.
!  ANCIEN CRITERE
!                   1 :interieur
!                   0 :exterieur
!
!             Z+-----+B     CRITERIA :
!              | H  /       ----------
!              | + /          (ZA^ZH).(ZH^ZB) & (AZ^AH).(AH^AB) > 0
!              |  /     <=> / (ZA^U ).( U^ZB) & (AZ^V ).( V^AB) > 0
!              | /          | U=ZH
!              |/           \ V=AH
!             A+
!
! NOUVEAU CRITERE (bien meilleure performance CPU) :
!     Coordonnes de H dans le repere (ZA,ZB) doit etre
!        -TOL < x < 1+ TOL
!        -TOL < y < 1+ TOL

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(IN)  :: P(3,4),H(3),Z(3)
      LOGICAL,INTENT(IN)  :: lTRIA
      LOGICAL, INTENT(OUT) :: lTEST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISONTRIA(4),IA,IB,NTRIA,I
        my_real
     .                          PS1,PS2,PS3,ZH(3),ZB(3),ZA(3),
     .                          TOL,
     .                          NORM_ZA_2,NORM_ZB_2,
     .                          COEF,
     .                          t, s, TOLCRIT  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NTRIA=4
      IF(lTRIA)NTRIA=1
      ISONTRIA(1:4)=0
        TOLCRIT = EM06
        TOL = TOLCRIT      
      IA=0
      IB=1
      
      DO I=1,NTRIA
        IA=IA+1
        IB=IB+1
        IF(IA>4)IA=1
        IF(IB>4)IB=1
        ZA(1)=P(1,IA)-Z(1)
        ZA(2)=P(2,IA)-Z(2)
        ZA(3)=P(3,IA)-Z(3)
        ZB(1)=P(1,IB)-Z(1)
        ZB(2)=P(2,IB)-Z(2)
        ZB(3)=P(3,IB)-Z(3)                

        NORM_ZA_2 = (ZA(1)*ZA(1)+ZA(2)*ZA(2)+ZA(3)*ZA(3))
        NORM_ZB_2 = (ZB(1)*ZB(1)+ZB(2)*ZB(2)+ZB(3)*ZB(3))

        ZH(1) = H(1) - Z(1)
        ZH(2) = H(2) - Z(2)
        ZH(3) = H(3) - Z(3)      

        PS1 =  ZA(1)*ZH(1)+ZA(2)*ZH(2)+ZA(3)*ZH(3)
      
        PS2 =  ZB(1)*ZH(1)+ZB(2)*ZH(2)+ZB(3)*ZH(3)

        PS3 =  ZB(1)*ZH(1)+ZB(2)*ZA(2)+ZB(3)*ZA(3)
      
        COEF = ONE-PS3*PS3/NORM_ZA_2/NORM_ZB_2
        COEF = ONE / COEF
      
        t =  COEF * ( PS2/NORM_ZB_2 - PS3*PS1/NORM_ZA_2/NORM_ZB_2)
        s =  COEF * ( -PS3*PS2/NORM_ZA_2/NORM_ZB_2 + PS1/NORM_ZA_2)
  
        !IF(IFLG_DB == 1)THEN
        !  print *, "coor ZA,ZB =", t,s
        !  write(*,fmt='(A12,3F30.16)')"*createnode ",Z(1:3)
        !  write(*,fmt='(A12,3F30.16)')"*createnode ",H(1:3)
        !ENDIF
  
        IF(t>=-TOL)THEN
          IF(s>=-TOL )THEN
            IF(s+t<=ONE+TOL)THEN
              ISONTRIA(I) = 1
            ENDIF
          ENDIF       
        ENDIF      
      
      ENDDO
      
      lTEST=.FALSE.
      IF(SUM(ISONTRIA(1:NTRIA))>=1)lTEST=.TRUE.
      
      END SUBROUTINE CHECK_IS_ON_SEGMENT
