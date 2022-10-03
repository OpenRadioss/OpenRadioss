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
Chd|  INI_SEATBELT                  source/tools/seatbelts/ini_seatbelt.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        NEW_SEATBELT                  source/tools/seatbelts/new_seatbelt.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE INI_SEATBELT(IPARG,ELBUF_TAB,KNOD2EL1D,NOD2EL1D,IXR,
     .                        X,ITAB,IPM,ALEA,KNOD2ELC,
     .                        NOD2ELC,IXC,NPBY,LPBY,SLRBODY,
     .                        ICODE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SEATBELT_MOD
      USE ELBUFDEF_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "random_c.inc"
#include      "my_allocate.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP),KNOD2EL1D(*),NOD2EL1D(*),IXR(NIXR,*),ITAB(*),IPM(NPROPMI,*),
     .        KNOD2ELC(*),NOD2ELC(*),IXC(NIXC,*)
      INTEGER, INTENT(IN) :: NPBY(NNPBY,NRBODY),LPBY(SLRBODY),SLRBODY,ICODE(SICODE)
      my_real               :: X(3,*),ALEA(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,M,N,NG,NEL,ITY,IAD,LFT,LLT,NFT,MTN,NODE,NODE_NEXT,
     .        ELEM_NEXT,N1,N2,NTOOL,KK,NOT_USED,FLAG,ELEM_CUR,NN,ID,NNOD,
     .        MID,MTYP,P,NB_SHELL,NFRAM,N3,N4,MS,NODES,ISEATBELT,NSL,IDRB,
     .        NOD,NFOUND_RBY,NFOUND_BCS,BCS_X,BCS_Y,BCS_Z,IC,IC1,IC2
C
      my_real DIST1,DIST2,ALEA_MAX,TOLE_2
C
      INTEGER , DIMENSION(:), ALLOCATABLE:: TAG_RETRACTOR,TAGN_RETRACTOR,TAG_RES
C
      TYPE(G_BUFEL_),POINTER :: GBUF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
C-----Reset of TAG_RES from SEATBELT_TAB structure----------------------------------
      MY_ALLOCATE(TAG_RES,NUMELR)
      TAG_RES(1:NUMELR) = 0
      DO I=1,N_SEATBELT
        DO J=1,SEATBELT_TAB(I)%NSPRING
          TAG_RES(SEATBELT_TAB(I)%SPRING(J)) = I
        ENDDO 
      ENDDO

C-----------------------------------------------------------------------------------
C
C-----Check of rbodies for 2d sliprings  -------------------------------------------
C
      DO I=1,NSLIPRING
        IF (SLIPRING(I)%NFRAM > 1) THEN
          IDRB = 0
          NFOUND_RBY = 0
          NFOUND_BCS = 0
          DO J=1,SLIPRING(I)%NFRAM
            NOD = SLIPRING(I)%FRAM(J)%ANCHOR_NODE
C---        check of RBodies ---
            L = 0
            DO N=1,NRBODY
              NSL=NPBY(2,N)
              DO K=1,NSL              
                IF ((LPBY(K+L)==NOD).AND.((IDRB==0).OR.(IDRB == N))) THEN
                  NFOUND_RBY  = NFOUND_RBY  + 1
                  IDRB = N
                ENDIF
              ENDDO
              L = L+NSL
            ENDDO
C---        check of BCS ---
            IC = ICODE(NOD)
            IC1=IC/512
            IC2=(IC-512*IC1)/64
            BCS_X = IC1/4
            BCS_Y = (IC1-4*BCS_X)/2
            BCS_Z = IC1-4*BCS_X-2*BCS_Y
            IF (BCS_X*BCS_Y*BCS_Z > 0) NFOUND_BCS = NFOUND_BCS + 1
          ENDDO
          IF (NFOUND_RBY ==SLIPRING(I)%NFRAM) THEN
            SLIPRING(I)%RBODY = IDRB
          ELSEIF (NFOUND_BCS /= SLIPRING(I)%NFRAM) THEN
            CALL ANCMSG(MSGID=2081,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=SLIPRING(I)%ID) 
          ENDIF
        ENDIF
      ENDDO
C
C-----Preparation for SPMD computation  ---------------------------------------------
      IF (NSPMD > 1) THEN
C
        DO I=1,NSLIPRING
          DO J=1,SLIPRING(I)%NFRAM
            N2 = SLIPRING(I)%FRAM(J)%NODE(2)
            DO P=1,NSPMD
              IF ((NLOCAL(N2,P)==1).AND.(NLOCAL(SLIPRING(I)%FRAM(J)%ANCHOR_NODE,P)==0)) THEN
C--             Anchor node and orientation node must be stick on the proc of the slipring
                CALL IFRONTPLUS(SLIPRING(I)%FRAM(J)%ANCHOR_NODE,P)
                IF (SLIPRING(I)%FRAM(J)%ORIENTATION_NODE > 0)
     .             CALL IFRONTPLUS(SLIPRING(I)%FRAM(J)%ORIENTATION_NODE,P)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
        DO I=1,NRETRACTOR
          N2 = RETRACTOR(I)%NODE(2)
          DO P=1,NSPMD
            IF ((NLOCAL(N2,P)==1).AND.(NLOCAL(RETRACTOR(I)%ANCHOR_NODE,P)==0)) THEN
C--           Anchor node must be stick on the proc of the retractor
              CALL IFRONTPLUS(RETRACTOR(I)%ANCHOR_NODE,P)
            ENDIF
          ENDDO
        ENDDO
C
      ENDIF

C-----------------------------------------------------------------------------------

      MY_ALLOCATE(TAG_RETRACTOR,NUMELR)
      MY_ALLOCATE(TAGN_RETRACTOR,NUMNOD)
      TAG_RETRACTOR(1:NUMELR) = 0
      TAGN_RETRACTOR(1:NUMNOD) = 0
      FLAG = 1
      NOT_USED = 0
C
C----- Loop on retractors to identify elements in retractor to deactivate (same algorithm as in creat_seatbelt) --
C
      DO I=1,NRETRACTOR
C
        N1 = RETRACTOR(I)%NODE(1)
        N2 = RETRACTOR(I)%NODE(2)
C
C--     loop of elements initially inside the retractor - tag with negative value
C
        NNOD = 2
C
        DO K=KNOD2EL1D(N1)+1,KNOD2EL1D(N1+1)
          IF (NOD2EL1D(K) > NUMELT+NUMELP) THEN
            ELEM_CUR = NOD2EL1D(K)-NUMELT-NUMELP
            MID = IXR(5,ELEM_CUR)
            IF (MID > 0) THEN
              MTYP = IPM(2,MID)
              IF (((IXR(2,ELEM_CUR)==N2).OR.(IXR(3,ELEM_CUR)==N2)).AND.(MTYP == 114)) THEN
C--             Loop on belt inside the retractor
                CALL NEW_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,N1,
     .                            ELEM_CUR,TAG_RETRACTOR,TAGN_RETRACTOR,I,FLAG,
     .                            NOT_USED,IPM)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
      ENDDO
C
C--   Loop to initialise elements with seatbelt material 
C
      DO NG=1,NGROUP
C
        MTN = IPARG(1,NG)
        NEL = IPARG(2,NG)
        ITY = IPARG(5,NG)
        NFT = IPARG(3,NG)
        IAD = IPARG(4,NG)
        ISEATBELT = IPARG(91,NG)
        LFT = 1
        LLT = NEL
C
        IF ((ITY==6).AND.(MTN==114)) THEN
C
C---      1D seatbelts springs 
C
          GBUF => ELBUF_TAB(NG)%GBUF
C
          DO I=1,NEL
C
            J = NFT + I
            N1 = IXR(2,J)
            N2 = IXR(3,J)          
            NTOOL = 0
C
C----------------------------------------------------------------------------
C-          Detection if element is in slipring
C----------------------------------------------------------------------------
C
            DO K=1,NSLIPRING
              DO L=1,SLIPRING(K)%NFRAM
                IF (((N1==SLIPRING(K)%FRAM(L)%NODE(1)).AND.(N2==SLIPRING(K)%FRAM(L)%NODE(2))).OR.
     .              ((N1==SLIPRING(K)%FRAM(L)%NODE(2)).AND.(N2==SLIPRING(K)%FRAM(L)%NODE(1)))) THEN
CC-               element is strand number 1
                  NTOOL = NTOOL + 1
                  GBUF%SLIPRING_ID(I) = K
                  GBUF%SLIPRING_FRAM_ID(I) = L
                  GBUF%SLIPRING_STRAND(I) = 1
C
CC-               determinaton of slipring direction for strand1
                  IF (N1==SLIPRING(K)%FRAM(L)%NODE(1)) THEN
                    SLIPRING(K)%FRAM(L)%STRAND_DIRECTION(1) = 1
                  ELSE
                    SLIPRING(K)%FRAM(L)%STRAND_DIRECTION(1) = -1
                  ENDIF
C
C----------->     Update of third node------------
                  DO KK=1,3
                    IF ((SLIPRING(K)%FRAM(L)%NODE(KK)/=N1).AND.(SLIPRING(K)%FRAM(L)%NODE(KK)/=N2)) THEN
                      IXR(4,J)=SLIPRING(K)%FRAM(L)%NODE(KK)
                    ENDIF
                  ENDDO
C
                ELSEIF (((N1==SLIPRING(K)%FRAM(L)%NODE(2)).AND.(N2==SLIPRING(K)%FRAM(L)%NODE(3))).OR.
     .                  ((N1==SLIPRING(K)%FRAM(L)%NODE(3)).AND.(N2==SLIPRING(K)%FRAM(L)%NODE(2)))) THEN
CC-               element is strand number 2
                  NTOOL = NTOOL + 1
                  GBUF%SLIPRING_ID(I) = K
                  GBUF%SLIPRING_FRAM_ID(I) = L
                  GBUF%SLIPRING_STRAND(I) = 2
C
CC-               determinaton of slipring direction for strand2
                  IF (N1==SLIPRING(K)%FRAM(L)%NODE(2)) THEN
                    SLIPRING(K)%FRAM(L)%STRAND_DIRECTION(2) = 1
                  ELSE
                    SLIPRING(K)%FRAM(L)%STRAND_DIRECTION(2) = -1
                  ENDIF
C
C----------->     Update of third node------------
                  DO KK=1,3
                    IF ((SLIPRING(K)%FRAM(L)%NODE(KK)/=N1).AND.(SLIPRING(K)%FRAM(L)%NODE(KK)/=N2)) THEN
                      IXR(4,J)=SLIPRING(K)%FRAM(L)%NODE(KK)
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ENDDO
C
C----------------------------------------------------------------------------
C-          Detection if element is in retractor
C----------------------------------------------------------------------------
C
            DO K=1,NRETRACTOR
              IF (((N1==RETRACTOR(K)%NODE(1)).AND.(N2==RETRACTOR(K)%NODE(2))).OR.
     .            ((N2==RETRACTOR(K)%NODE(1)).AND.(N1==RETRACTOR(K)%NODE(2)))) THEN
CC-             element is mouth element of retractor - 1st direction
                NTOOL = NTOOL + 1
                GBUF%RETRACTOR_ID(I) = K
                GBUF%SLIPRING_STRAND(I) = -1
              ENDIF
            ENDDO
C
            IF (TAG_RETRACTOR(J) > 0) THEN
C-            element initially in the retractor
              GBUF%OFF(I) = ZERO
              K = TAG_RETRACTOR(J)
              ID = RETRACTOR(K)%ID
              NN = RETRACTOR(K)%ANCHOR_NODE 
              DIST1 = (X(1,NN)-X(1,N1))**2+(X(2,NN)-X(2,N1))**2+(X(3,NN)-X(3,N1))**2
              DIST2 = (X(1,NN)-X(1,N2))**2+(X(2,NN)-X(2,N2))**2+(X(3,NN)-X(3,N2))**2
C
C--           default tolerance
              TOLE_2 = EM10*RETRACTOR(K)%ELEMENT_SIZE*RETRACTOR(K)%ELEMENT_SIZE
C--           compatibility with random noise
              IF (NRAND > 0) THEN
                ALEA_MAX = ZERO
                DO J=1,NRAND
                  ALEA_MAX = MAX(ALEA_MAX,ALEA(J))
                ENDDO 
                TOLE_2 = MAX(TOLE_2,TEN*ALEA_MAX*ALEA_MAX)
              ENDIF

C--           tolerance if nodes are very close to anchorage node
              IF (DIST1 <= TOLE_2) THEN
                  X(1,N1) = X(1,NN)
                  X(2,N1) = X(2,NN)
                  X(3,N1) = X(3,NN)
                  DIST1 = ZERO
              ENDIF
              IF (DIST2 <= TOLE_2) THEN
                  X(1,N2) = X(1,NN)
                  X(2,N2) = X(2,NN)
                  X(3,N2) = X(3,NN)
                  DIST2 = ZERO
              ENDIF
C
              IF(DIST2 + DIST1 > EM30) CALL ANCMSG(MSGID=2011,
     .                                 MSGTYPE=MSGERROR,
     .                                 ANMODE=ANINFO,
     .                                 I1=ID,I2=IXR(NIXR,J),I3=ID)
C
              IF (TAGN_RETRACTOR(N1) > 0) THEN
                RETRACTOR(K)%INACTI_NNOD = RETRACTOR(K)%INACTI_NNOD + 1
                RETRACTOR(K)%INACTI_NODE(RETRACTOR(K)%INACTI_NNOD) = N1
                TAGN_RETRACTOR(N1) = 0
              ENDIF
C
              IF (TAGN_RETRACTOR(N2) > 0) THEN
                RETRACTOR(K)%INACTI_NNOD = RETRACTOR(K)%INACTI_NNOD + 1
                RETRACTOR(K)%INACTI_NODE(RETRACTOR(K)%INACTI_NNOD) = N2
                TAGN_RETRACTOR(N2) = 0
              ENDIF
C               
            ENDIF
C
C----------------------------------------------------------------------------
C
            IF(NTOOL > 1) CALL ANCMSG(MSGID=2006,
     .                                MSGTYPE=MSGERROR,
     .                                ANMODE=ANINFO,
     .                                I1=IXR(NIXR,J))
C
CCC-        Find node before node 1
            NODE = N1
            NODE_NEXT = 0
            DO K=KNOD2EL1D(NODE)+1,KNOD2EL1D(NODE+1)
              IF ((NOD2EL1D(K) /= J + NUMELT+NUMELP).AND.(NOD2EL1D(K) > NUMELT+NUMELP)) THEN
                ELEM_NEXT = NOD2EL1D(K)-NUMELT-NUMELP
                MID = IXR(5,ELEM_NEXT)
                IF (MID > 0) THEN
                  MTYP = IPM(2,MID)
                  IF (MTYP == 114) THEN
                    IF (IXR(2,ELEM_NEXT) == NODE) THEN
                      NODE_NEXT = IXR(3,ELEM_NEXT)
                    ELSE
                      NODE_NEXT = IXR(2,ELEM_NEXT)
                    ENDIF
                  ENDIF
                ENDIF                
              ENDIF
            ENDDO
            GBUF%ADD_NODE(I) = NODE_NEXT

CCC-        Find node after node 2
            NODE = N2
            NODE_NEXT = 0
            DO K=KNOD2EL1D(NODE)+1,KNOD2EL1D(NODE+1)
              IF ((NOD2EL1D(K) /= J + NUMELT+NUMELP).AND.(NOD2EL1D(K) > NUMELT+NUMELP)) THEN
                ELEM_NEXT = NOD2EL1D(K)-NUMELT-NUMELP
                MID = IXR(5,ELEM_NEXT)
                IF (MID > 0) THEN
                  MTYP = IPM(2,MID)
                  IF (MTYP == 114) THEN
                    IF (IXR(2,ELEM_NEXT) == NODE) THEN
                      NODE_NEXT = IXR(3,ELEM_NEXT)
                    ELSE
                      NODE_NEXT = IXR(2,ELEM_NEXT)
                    ENDIF
                  ENDIF
                ENDIF                
              ENDIF
            ENDDO
            GBUF%ADD_NODE(NEL+I) = NODE_NEXT
C
CCC-        Compute Fram factor for 2D seatbelt
C
            NB_SHELL = 0
            IF (GBUF%ADD_NODE(I) > 0) THEN
              NODE = N1
            ELSE
              NODE = N2
            ENDIF
            DO K=KNOD2ELC(NODE)+1,KNOD2ELC(NODE+1)
              ELEM_CUR = NOD2ELC(K)
              MID = IXC(1,ELEM_CUR)
              MTYP = IPM(2,MID)
              IF (MTYP == 119) NB_SHELL = NB_SHELL + 1                
            ENDDO
C
            NFRAM = SEATBELT_TAB(TAG_RES(J))%NFRAM
            IF ((NFRAM > 1).AND.(NB_SHELL==4)) THEN
C--           spring is on the edge of the 2D belt
              GBUF%FRAM_FACTOR(I) = ONE/(NFRAM-1)
            ELSEIF ((NFRAM > 1).AND.(NB_SHELL==2)) THEN
C--           spring is inside the 2D belt
              GBUF%FRAM_FACTOR(I) = HALF/(NFRAM-1)
            ELSE
C--           1D seatbelt
              GBUF%FRAM_FACTOR(I) = ONE
            ENDIF
C--         element mass and inertia scaled by frame factor for elementary time step
            GBUF%MASS(I) = GBUF%MASS(I)*GBUF%FRAM_FACTOR(I)
            GBUF%INTVAR(I) = GBUF%INTVAR(I)*GBUF%FRAM_FACTOR(I)
C
          ENDDO  
C
        ELSEIF ((ITY==3).AND.(ISEATBELT==1)) THEN
C
C---      2D seatbelts shells
C
          GBUF => ELBUF_TAB(NG)%GBUF
C
          DO I=1,NEL
C
            J = NFT + I        
C
            N1 = IXC(2,J)
            N2 = IXC(3,J)
            N3 = IXC(4,J)
            N4 = IXC(5,J)
C
C--         Scale factor on stress initialised 
            GBUF%INTVAR(I) = ONE
C
C--         Initial length of fram 1/2 and fram 4/3
            DIST1 = (X(1,N2)-X(1,N1))**2+(X(2,N2)-X(2,N1))**2+(X(3,N2)-X(3,N1))**2
            DIST2 = (X(1,N3)-X(1,N4))**2+(X(2,N3)-X(2,N4))**2+(X(3,N3)-X(3,N4))**2
            GBUF%INTVAR(I+2*NEL) = SQRT(DIST1)
            GBUF%INTVAR(I+3*NEL) = SQRT(DIST2)
C
            DO M=2,5
CCC-        Find node after node i
C
C--             spring either between node 1/2 - or 3/4
                IF (M==2) MS =3
                IF (M==3) MS =2
                IF (M==4) MS =5
                IF (M==5) MS =4
                NODE = IXC(M,J)
                NODES = IXC(MS,J)
C
                NODE_NEXT = 0
C
                DO K=KNOD2EL1D(NODE)+1,KNOD2EL1D(NODE+1)
                  IF (NOD2EL1D(K) > NUMELT+NUMELP) THEN          
                    ELEM_NEXT = NOD2EL1D(K)-NUMELT-NUMELP
                    IF (((IXR(2,ELEM_NEXT)==NODE).AND.(IXR(3,ELEM_NEXT)/=NODES)).OR.
     .                  ((IXR(3,ELEM_NEXT)==NODE).AND.(IXR(2,ELEM_NEXT)/=NODES))) THEN
                      MID = IXR(5,ELEM_NEXT)
                      IF (MID > 0) THEN
                        MTYP = IPM(2,MID)
                        IF (MTYP == 114) THEN
                          IF (IXR(2,ELEM_NEXT) == NODE) THEN
                            NODE_NEXT = IXR(3,ELEM_NEXT)
                          ELSE
                            NODE_NEXT = IXR(2,ELEM_NEXT)
                          ENDIF
                        ENDIF
                      ENDIF  
                    ENDIF              
                  ENDIF
                ENDDO
                GBUF%ADD_NODE((M-2)*NEL+I) = NODE_NEXT
C
            ENDDO
C
          ENDDO
C
        ENDIF
C
      ENDDO         
C
      DEALLOCATE(TAG_RES,TAG_RETRACTOR,TAGN_RETRACTOR)
C
      END SUBROUTINE INI_SEATBELT

