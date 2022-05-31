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
Chd|  ALEW6                         source/ale/alew6.F            
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/alewdx.F           
Chd|-- calls ---------------
Chd|        CENTROID2                     source/multifluid/centroid.F  
Chd|        CENTROID3                     source/multifluid/centroid.F  
Chd|        CENTROID3T                    source/multifluid/centroid.F  
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_ENVOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ALEW6( 
     1     X         ,D       ,V       ,W       ,WA      ,
     .     XCELL, XFACE,
     2     ALE_NN_CONNECT     ,ALE_NE_CONNECT, NALE    ,NODFT   ,NODLT   ,ITASK   ,
     3     NBRCVOIS  ,NBSDVOIS,LNRCVOIS,LNSDVOIS,ITAB,  NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS,
     4     ELBUF_TAB, IPARG, IXS, IXQ)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     VOLUME GRID SMOOTHING
C     Compute Grid for /ALE/GRID/VOLUME
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALE(*), NODFT, NODLT, ITASK,
     .     NBRCVOIS(*),NBSDVOIS(*),
     .     LNRCVOIS(*),LNSDVOIS(*),ITAB(*),IPARG(NPARG,*),IXS(NIXS,*),IXQ(NIXQ,*)
      my_real
     .     X(3,*), D(3,*), V(3,*), W(3,*), WA(3,*), XCELL(3, *), XFACE(3,6,*)  
      TYPE(t_connectivity), INTENT(IN) :: ALE_NN_CONNECT, ALE_NE_CONNECT
      INTEGER, INTENT(IN) :: NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, IA(6), L, LENCOM, K, ITER,NITER,NUM
      my_real
     .     DX, DY, DZ,FAC, VEC(3),LAMBDA,ETA,IFORM, WEIGHT(6),SOM,SOM2,MIN_DIST,MAX_DIST
      my_real, ALLOCATABLE,DIMENSION(:,:) :: DISP
      my_real, DIMENSION(:), ALLOCATABLE :: DIST
      INTEGER :: IAD, IAD1, IAD2, NODE_ID, NG, NEL, NFT, ITY, ISOLNOD, IEL, ELT_ID, II
      my_real :: SUM_VOL, Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4, A1, A2
C     ---------------------------------------------
C     B e g i n n i n g   o f   S u b r o u t i n e
C     ---------------------------------------------
      WA(1:3,NODFT:NODLT)=X(1:3,NODFT:NODLT)      
      CALL MY_BARRIER
      DO NG = ITASK + 1, NGROUP, NTHREAD
         NEL = IPARG(2, NG)
         NFT = IPARG(3, NG)
         ITY = IPARG(5, NG)
         ISOLNOD = IPARG(28, NG)
         IF (ITY == 1 .AND. ISOLNOD /= 4) THEN
            CALL CENTROID3(NEL, NEL, NFT, IXS, X, 
     .           XCELL(:, 1 + NFT : NEL + NFT), 
     .           XFACE(:, :, 1 + NFT : NEL + NFT))
         ELSEIF (ITY == 1 .AND. ISOLNOD == 4) THEN
            CALL CENTROID3T(NEL, NEL, NFT, IXS, X, 
     .           XCELL(:, 1 + NFT : NEL + NFT), 
     .           XFACE(:, :, 1 + NFT : NEL + NFT))
         ELSEIF (ITY == 2) THEN
            CALL CENTROID2(NEL, NEL, NFT, IXQ, X, 
     .           XCELL(:, 1 + NFT : NEL + NFT), 
     .           XFACE(:, :, 1 + NFT : NEL + NFT))
         ENDIF
         IF (ITY == 1 .OR. ITY == 2) THEN
C     Volume stored in XFACE for SMP reasons
            IF (N2D /= 1) THEN
               DO I = 1, NEL
                  XFACE(1,1,I + NFT) = ELBUF_TAB(NG)%GBUF%VOL(I)
               ENDDO
            ELSE
               DO I = 1, NEL
                  Y1 = X(2, IXQ(2, I + NFT))
                  Y2 = X(2, IXQ(3, I + NFT))
                  Y3 = X(2, IXQ(4, I + NFT))
                  Y4 = X(2, IXQ(5, I + NFT))
                  Z1 = X(3, IXQ(2, I + NFT))
                  Z2 = X(3, IXQ(3, I + NFT))
                  Z3 = X(3, IXQ(4, I + NFT))
                  Z4 = X(3, IXQ(5, I + NFT))
                  A1 =Y2*(Z3-Z4)+Y3*(Z4-Z2)+Y4*(Z2-Z3)
                  A2 =Y2*(Z4-Z1)+Y4*(Z1-Z2)+Y1*(Z2-Z4)
                  XFACE(1,1,I + NFT) = (A1+A2)*HALF
               ENDDO
            ENDIF
         ENDIF
      ENDDO
      CALL MY_BARRIER
      IF (NSPMD > 1) THEN
!$OMP MASTER
         LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
         CALL SPMD_E1VOIS(XFACE(2,1,1:NUMELS + NUMELQ + NE_NSVOIS + NE_NQVOIS),
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         CALL SPMD_ENVOIS(3, XCELL, 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         CALL SPMD_E1VOIS(XFACE(1,1,1:NUMELS + NUMELQ + NE_NSVOIS + NE_NQVOIS), 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
!$OMP END MASTER
      ENDIF
      CALL MY_BARRIER

      DO I = NODFT, NODLT
         IF (IABS(NALE(I)) == 1) THEN
            X(1:3, I) = ZERO
            SUM_VOL = ZERO
            IAD1 = ALE_NE_CONNECT%IAD_CONNECT(I)
            IAD2 = ALE_NE_CONNECT%IAD_CONNECT(I + 1) - 1
            DO IEL = IAD1, IAD2
               ELT_ID = ALE_NE_CONNECT%CONNECTED(IEL)
               X(1:3, I) = X(1:3, I) + XFACE(1,1,ELT_ID) * XCELL(1:3, ELT_ID)
               SUM_VOL = SUM_VOL + XFACE(1,1,ELT_ID)
            ENDDO
            X(1:3, I) = X(1:3, I) / SUM_VOL
         ENDIF
      ENDDO
      DO I = NODFT, NODLT
         IF ( IABS(NALE(I)) == 1 .AND. DT2 > ZERO) THEN
            W(1:3,I) = (X(1:3,I) - WA(1:3,I)) / DT2
         ELSE IF (NALE(I) == 0) THEN
            W(1:3,I) = V(1:3,I)
         ELSE 
            W(1:3, I) = ZERO
         ENDIF
         X(1:3,I) = WA(1:3,I)
      ENDDO
      CALL MY_BARRIER

      END
