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
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE UPDATE_SLIPRING(IXR,IPARG,ELBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE SEATBELT_MOD
C----6---------------------------------------------------------------7---------8
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
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*),IPARG(NPARG,*)
C
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,M,FRA,IG,IGTYP,KAD,ITYP,NG,JFT,JLT,NEL,
     .        NB8,NFT,NV,N1,N2,MTN,NN1,NN2,NN3,II(6),SLIP
C
      TYPE(G_BUFEL_),POINTER :: GBUF
C---------------------------------------------------------
C
C----------------------------------------------------------
C-    UPDATE OF SLIPRING
C----------------------------------------------------------
C----------------------------------------------------------
C
C----------------------------------------------------------
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
C                     -> Update of third node -------
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
C                     -> Update of third node -------
                      IXR(4,J) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(1)
                    ELSEIF ((GBUF%SLIPRING_ID(I) == SLIP).AND.(GBUF%SLIPRING_FRAM_ID(I) == FRA)) THEN
                      GBUF%SLIPRING_ID(I) = 0
                      GBUF%SLIPRING_FRAM_ID(I) = 0
                      GBUF%SLIPRING_STRAND(I) = 0
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

C----------------------------------------------------------
C----------------------------------------------------------
C----------------------------------------------------------
      DO SLIP=1,NSLIPRING
        DO FRA = 1,SLIPRING(SLIP)%NFRAM
          IF (SLIPRING(SLIP)%FRAM(FRA)%UPDATE /= 0) THEN
            SLIPRING(SLIP)%FRAM(FRA)%NODE(1) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(1)
            SLIPRING(SLIP)%FRAM(FRA)%NODE(2) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(2)
            SLIPRING(SLIP)%FRAM(FRA)%NODE(3) = SLIPRING(SLIP)%FRAM(FRA)%NODE_NEXT(3)
          ENDIF
        ENDDO       
      ENDDO

C----------------------------------------------------------
C----------------------------------------------------------
C----------------------------------------------------------      
C
      RETURN
                
      END
