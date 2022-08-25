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
Chd|  UPDATE_SLIPRING               source/tools/seatbelts/update_slipring.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SHELL_LOC_COR                 source/tools/seatbelts/shell_loc_cor.F
Chd|        SHELL_REACTIVATION            source/tools/seatbelts/shell_reactivation.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE UPDATE_SLIPRING(IXR,IXC,IPARG,ELBUF_TAB,FLAG_SLIPRING_UPDATE,X)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE SEATBELT_MOD
      USE MESSAGE_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IXC(NIXC,NUMELC),IPARG(NPARG,NGROUP),FLAG_SLIPRING_UPDATE
      INTEGER ,INTENT(INOUT) :: IXR(NIXR,NUMELR)
      my_real ,INTENT(IN) :: X(3,NUMNOD)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ITYP,NG,JFT,JLT,NEL,
     .        NFT,N1,N2,N3,N4,MTN,NN1,NN2,NN3,II(6),SLIP,
     .        COMPT,FRA,NFOUND,FOUND_SLIP(2),FOUND_FRAM(2),FRAM1,FRAM2,
     .        NODE_FRAM1,NODE_FRAM2,IREP,FLAG_REACTIV,FLAG_R1,FLAG_R2,NUVAR,
     .        ISEATBELT,FRA1,FRA2,L_DIRA,NLAY,ISMSTR,NFRAM_MAX
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_NOD,CORES_SLIP,CORES_FRAM
C
      my_real 
     .        XL2,YL2,XL3,YL3,XL4,YL4,L0FRAM1,L0FRAM2,DIST,DISTB,OFFSET,N_DIR2(2)
C
      TYPE(G_BUFEL_),POINTER :: GBUF
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
C---------------------------------------------------------
C
C----------------------------------------------------------
C-    UPDATE OF SLIPRING
C----------------------------------------------------------
C----------------------------------------------------------
C
      IF (FLAG_SLIPRING_UPDATE /= 0) THEN
C      
        DO NG=1,NGROUP
C
          ITYP = IPARG(5,NG)
	  MTN = IPARG(1,NG)
	  NEL = IPARG(2,NG)
	  NFT = IPARG(3,NG) 
	  JFT = 1
	  JLT = MIN(NVSIZ,NEL)  
          GBUF => ELBUF_TAB(NG)%GBUF
C
          DO I=1,6
            II(I) = (I-1)*NEL + 1
          ENDDO
C
          NUVAR = 6
C
          IF ((ITYP==6).AND.(MTN==114)) THEN
C--------> Boucle sur les elements ressort seatbelt-------
            DO I=JFT,JLT
	      J = I + NFT	  
C    
                N1 = IXR(2,J)
                N2 = IXR(3,J)
C
                DO SLIP=1,NSLIPRING
C
                  DO FRA = 1,SLIPRING(SLIP)%NFRAM
C
                    IF (SLIPRING(SLIP)%FRAM(FRA)%UPDATE /= 0) THEN
C
                      NN1 = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(1)
                      NN2 = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(2)
                      NN3 = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(3)
C
                      IF (((N1==NN1).AND.(N2==NN2)).OR.((N2==NN1).AND.(N1==NN2))) THEN
                        GBUF%SLIPRING_ID(I) = SLIP
                        GBUF%SLIPRING_FRAM_ID(I) = FRA
                        GBUF%SLIPRING_STRAND(I) = 1
                        GBUF%UPDATE(I) = 1
                        IF (N2 == NN2) THEN
                          SLIPRING(SLIP)%FRAM(FRA)%STRAND_DIRECTION(1) = 1
                        ELSE
                          SLIPRING(SLIP)%FRAM(FRA)%STRAND_DIRECTION(1) = -1
                        ENDIF
                        SLIPRING(SLIP)%FRAM(FRA)%RESIDUAL_LENGTH(1) = GBUF%LENGTH(II(1)+I-1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR1(1) = GBUF%FOR(II(1)+I-1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR1(2) = GBUF%DEP_IN_TENS(II(1)+I-1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR1(3) = GBUF%YIELD(II(1)+I-1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR1(4) = GBUF%VAR(NUVAR*(I-1)+1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR1(5) = GBUF%FOREP(II(1)+I-1) 
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR1(6) = GBUF%POSX(I)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR1(7) = GBUF%INTVAR(II(2)+I-1)
C                       -> Update of third node -------
                        IXR(4,J) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(3)
                      ELSEIF (((N1==NN2).AND.(N2==NN3)).OR.((N2==NN2).AND.(N1==NN3))) THEN
                        GBUF%SLIPRING_ID(I) = SLIP
                        GBUF%SLIPRING_FRAM_ID(I) = FRA
                        GBUF%SLIPRING_STRAND(I) = 2
                        GBUF%UPDATE(I) = 1
                        IF (N1 == NN2) THEN
                          SLIPRING(SLIP)%FRAM(FRA)%STRAND_DIRECTION(2) = 1
                        ELSE
                          SLIPRING(SLIP)%FRAM(FRA)%STRAND_DIRECTION(2) = -1
                        ENDIF
                        SLIPRING(SLIP)%FRAM(FRA)%RESIDUAL_LENGTH(2) = GBUF%LENGTH(II(1)+I-1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR2(1) = GBUF%FOR(II(1)+I-1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR2(2) = GBUF%DEP_IN_TENS(II(1)+I-1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR2(3) = GBUF%YIELD(II(1)+I-1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR2(4) = GBUF%VAR(NUVAR*(I-1)+1)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR2(5) = GBUF%FOREP(II(1)+I-1) 
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR2(6) = GBUF%POSX(I)
                        SLIPRING(SLIP)%FRAM(FRA)%INTVAR_STR2(7) = GBUF%INTVAR(II(2)+I-1)
C                       -> Update of third node -------
                        IXR(4,J) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(1)
                      ELSEIF ((GBUF%SLIPRING_ID(I)==SLIP).AND.(GBUF%SLIPRING_FRAM_ID(I)==FRA)) THEN
                        GBUF%SLIPRING_ID(I) = 0
                        GBUF%SLIPRING_FRAM_ID(I) = 0
                        GBUF%SLIPRING_STRAND(I) = 0
                        SLIPRING(SLIP)%FRAM(FRA)%PREV_REF_LENGTH = GBUF%LENGTH(II(1)+I-1)
                      ENDIF
                    ENDIF
C
                  ENDDO
C
                ENDDO
C
            ENDDO
          ENDIF       
C	 
        ENDDO
C
C----------------------------------------------------------
C
        DO SLIP=1,NSLIPRING
          DO FRA = 1,SLIPRING(SLIP)%NFRAM
            IF (SLIPRING(SLIP)%FRAM(FRA)%UPDATE /= 0) THEN
              SLIPRING(SLIP)%FRAM(FRA)%NODE2_PREV = SLIPRING(SLIP)%FRAM(FRA)%NODE(2)
              SLIPRING(SLIP)%FRAM(FRA)%NODE(1) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(1)
              SLIPRING(SLIP)%FRAM(FRA)%NODE(2) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(2)
              SLIPRING(SLIP)%FRAM(FRA)%NODE(3) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(3)
            ENDIF
          ENDDO      
        ENDDO
C
      ENDIF
C
C----------------------------------------------------------
C----------------------------------------------------------
C--   Loop on shells for activation / deactivation
C----------------------------------------------------------
C----------------------------------------------------------
C
      IF ((N_SEATBELT_2D > 0).AND.((NCYCLE==0).OR.(FLAG_SLIPRING_UPDATE /= 0))) THEN
C
        NFRAM_MAX = 0
        DO SLIP=1,NSLIPRING
          NFRAM_MAX = MAX(NFRAM_MAX,SLIPRING(SLIP)%NFRAM)
        ENDDO
C
        MY_ALLOCATE(TAG_NOD,NUMNOD)
        MY_ALLOCATE(CORES_SLIP,NSLIPRING*NFRAM_MAX)
        MY_ALLOCATE(CORES_FRAM,NSLIPRING*NFRAM_MAX)
        TAG_NOD(1:NUMNOD) = 0
        CORES_SLIP(1:NSLIPRING*NFRAM_MAX) = 0
        CORES_FRAM(1:NSLIPRING*NFRAM_MAX) = 0
C 
        COMPT = 0
        DO SLIP=1,NSLIPRING
          DO FRA = 1,SLIPRING(SLIP)%NFRAM
            COMPT = COMPT + 1
            TAG_NOD(SLIPRING(SLIP)%FRAM(FRA)%NODE(2)) = COMPT
            CORES_SLIP(COMPT) = SLIP
            CORES_FRAM(COMPT) = FRA
            TAG_NOD(SLIPRING(SLIP)%FRAM(FRA)%NODE(1)) = -1
            TAG_NOD(SLIPRING(SLIP)%FRAM(FRA)%NODE(3)) = -1
          ENDDO
        ENDDO
C
        COMPT = 1
        DO NG=1,NGROUP
C
          ITYP = IPARG(5,NG)
	  MTN = IPARG(1,NG)
	  NEL = IPARG(2,NG)
	  NFT = IPARG(3,NG)
          ISEATBELT = IPARG(91,NG)
          IREP = IPARG(35,NG)
          ISMSTR = IPARG(9,NG)
C 
	  JFT = 1
	  JLT = MIN(NVSIZ,NEL)  
          GBUF => ELBUF_TAB(NG)%GBUF
          DO I=1,6
            II(I) = (I-1)*NEL + 1
          ENDDO
C
          IF ((ITYP == 3).AND.(ISEATBELT==1)) THEN
C
            BUFLY => ELBUF_TAB(NG)%BUFLY(1)
            NLAY   = ELBUF_TAB(NG)%NLAY
            L_DIRA = BUFLY%LY_DIRA
C
            DO I=JFT,JLT
C
	      J = I + NFT
C
              FLAG_REACTIV = 0
C	
              NFOUND = 0
              FOUND_SLIP(1:2) = 0
              FOUND_FRAM(1:2) = 0 
              FLAG_R1 = 0
              FLAG_R2 = 0            
C
C--           Get previous connection between element / slipring
              SLIP = GBUF%SLIPRING_ID(I)
              FRAM1 = MAX(0,GBUF%SLIPRING_FRAM_ID(II(1)+I-1))
              FRAM2 = MAX(0,GBUF%SLIPRING_FRAM_ID(II(2)+I-1))
C
              DO K=1,4
C--             Tag > 0 - node 2 of slipring
                IF (TAG_NOD(IXC(K+1,J)) > 0) THEN
                  NFOUND = NFOUND + 1
                  FOUND_SLIP(NFOUND) = CORES_SLIP(TAG_NOD(IXC(K+1,J)))
                  FOUND_FRAM(NFOUND) = CORES_FRAM(TAG_NOD(IXC(K+1,J)))
                  FLAG_R1 = 1
                ENDIF
              ENDDO
C
              IF (FLAG_R1 == 0) THEN
                DO K=2,5
C--               Tag < 0 - node 1 or 3 of slipring
                  IF (TAG_NOD(IXC(K,J)) < 0) FLAG_R2 = K              
                ENDDO
              ENDIF
C
C-----------------------------------------------------------------------------------------
C--           2nd rank of element behing/ahead slipring - scaling factor on stress
C-----------------------------------------------------------------------------------------
              GBUF%INTVAR(II(1)+I-1) = ONE
              IF ((FLAG_R2 > 0).AND.(GBUF%UPDATE(I) == 0)) THEN
                IF (SLIP == 0) THEN
C--               element will be slowly deactivated - stress factor -> 0 in mulawc
                  GBUF%UPDATE(I) = FLAG_R2
                  IF ((FLAG_R2==2).OR.(FLAG_R2==5)) THEN
                    N1 = IXC(2,J)
                    N2 = GBUF%ADD_NODE(I)
                    N3 = IXC(5,J)
                    N4 = GBUF%ADD_NODE(NEL*3+I)
                  ELSE
                    N1 = IXC(3,J)
                    N2 = GBUF%ADD_NODE(NEL+I)
                    N3 = IXC(4,J)
                    N4 = GBUF%ADD_NODE(NEL*2+I)
                  ENDIF
                  DIST = SQRT((X(1,N1)-X(1,N2))**2+(X(2,N1)-X(2,N2))**2+(X(3,N1)-X(3,N2))**2)
                  DISTB = SQRT(MAX(EM20,(X(1,N3)-X(1,N4))**2+(X(2,N3)-X(2,N4))**2+(X(3,N3)-X(3,N4))**2))
                  DIST = MIN(DIST,DISTB)
                  GBUF%INTVAR(II(2)+I-1) = HALF*DIST
                ELSE
C--               element will be slowly activated - stress factor -> 1 in mulawc
                  GBUF%UPDATE(I) = -FLAG_R2
                  N1 = IXC(2,J)
                  N2 = IXC(3,J)
                  DIST = (X(1,N1)-X(1,N2))**2+(X(2,N1)-X(2,N2))**2+(X(3,N1)-X(3,N2))**2
                  GBUF%INTVAR(II(2)+I-1) = THIRD*SQRT(DIST)
                ENDIF
              ELSEIF (FLAG_R2 == 0) THEN
                GBUF%UPDATE(I) = 0
              ENDIF
C
C-----------------------------------------------------------------------------------------
C--           1st rank of element connected to slipring - full reactivation / deactivation
C-----------------------------------------------------------------------------------------
              DO K=1,NFOUND
                IF ((FRAM1 == 0).AND.(FOUND_FRAM(K) /= FRAM2)) THEN
                  SLIP = FOUND_SLIP(K)
                  FRAM1 = FOUND_FRAM(K)
                  GBUF%SLIPRING_FRAM_ID(II(1)+I-1) = FRAM1
                ELSEIF ((FRAM2 == 0).AND.(FOUND_FRAM(K) /= FRAM1)) THEN
                  FRAM2 = FOUND_FRAM(K)
                  GBUF%SLIPRING_FRAM_ID(II(2)+I-1) = FRAM2
                ENDIF
              ENDDO
C
              IF ((FRAM1 > 0).AND.(FRAM1 /= FOUND_FRAM(1)).AND.(FRAM1 /= FOUND_FRAM(2))) THEN
C               disconnection with slipring fram1 - ringslip must be stored in bufel
                GBUF%SLIPRING_FRAM_ID(II(1)+I-1) = -GBUF%SLIPRING_FRAM_ID(II(1)+I-1)
                GBUF%POSX(II(1)+I-1) = SLIPRING(SLIP)%FRAM(FRAM1)%RINGSLIP
                GBUF%INTVAR(II(3)+I-1) = ABS(SLIPRING(SLIP)%FRAM(FRAM1)%PREV_REF_LENGTH)              
                DO K=1,4
                  IF (IXC(K+1,J) == SLIPRING(SLIP)%FRAM(FRAM1)%NODE2_PREV) THEN
                    GBUF%INTVAR(II(5)+I-1) = K
                  ENDIF
                ENDDO          
              ENDIF
C
              IF ((FRAM2 > 0).AND.(FRAM2 /= FOUND_FRAM(1)).AND.(FRAM2 /= FOUND_FRAM(2))) THEN
C               disconnection with slipring fram2 - ringslip must be stored in bufel
                GBUF%SLIPRING_FRAM_ID(II(2)+I-1) = -GBUF%SLIPRING_FRAM_ID(II(2)+I-1)
                GBUF%POSX(II(2)+I-1) = SLIPRING(SLIP)%FRAM(FRAM2)%RINGSLIP
                GBUF%INTVAR(II(4)+I-1) = ABS(SLIPRING(SLIP)%FRAM(FRAM2)%PREV_REF_LENGTH)
                DO K=1,4
                  IF (IXC(K+1,J) == SLIPRING(SLIP)%FRAM(FRAM2)%NODE2_PREV) THEN
                    GBUF%INTVAR(II(6)+I-1) = K
                  ENDIF
                ENDDO 
              ENDIF
C
              IF ((GBUF%SLIPRING_ID(I)==0).AND.(NFOUND > 0)) THEN
C--             shell connected to slipring - deactivated
                GBUF%OFF(I) = ZERO
                GBUF%SLIPRING_ID(I) = SLIP     
              ELSEIF ((GBUF%SLIPRING_ID(I) > 0).AND.(NFOUND == 0)) THEN
C--             shell fully released by slipring - reactivated - tag -1
                GBUF%OFF(I) = ONE
                GBUF%SLIPRING_ID(I) = 0 
                FLAG_REACTIV = 1
              ENDIF
C
              IF (FLAG_REACTIV == 1) THEN
C--             computation of local coordinates
                FRA1 = ABS(GBUF%SLIPRING_FRAM_ID(II(1)+I-1))
                FRA2 = ABS(GBUF%SLIPRING_FRAM_ID(II(2)+I-1))
                NN1 = SLIPRING(SLIP)%FRAM(FRA1)%ANCHOR_NODE
                NN2 = SLIPRING(SLIP)%FRAM(FRA2)%ANCHOR_NODE
                CALL SHELL_LOC_COR(X,IXC,J,XL2,YL2,XL3,YL3,XL4,YL4,IREP,NN1,NN2,N_DIR2)
C--             shell reactivation process - update of reference state -- reset of strain tensor
                L0FRAM1 = GBUF%INTVAR(II(3)+I-1)
                L0FRAM2 = GBUF%INTVAR(II(4)+I-1)
                OFFSET = GBUF%POSX(II(1)+I-1) - GBUF%POSX(II(2)+I-1)
                NODE_FRAM1 = NINT(GBUF%INTVAR(II(5)+I-1))
                NODE_FRAM2 = NINT(GBUF%INTVAR(II(6)+I-1))
                CALL SHELL_REACTIVATION(I,II,L0FRAM1,L0FRAM2,NODE_FRAM1,
     .                                  NODE_FRAM2,GBUF%STRA,NEL,XL2,YL2,
     .                                  XL3,YL3,XL4,YL4,OFFSET,
     .                                  N_DIR2,BUFLY%DIRA(I),BUFLY%DIRA(NEL+I),GBUF%SMSTR,ISMSTR)
              ENDIF
C
            ENDDO
          ENDIF
C
        ENDDO
C
        DEALLOCATE(TAG_NOD,CORES_SLIP,CORES_FRAM)
C
      ENDIF
C
C----------------------------------------------------------
C----------------------------------------------------------
C----------------------------------------------------------      
C
      RETURN
                
      END
