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
Chd|  NODALVFRAC                    source/output/anim/generate/nodalvfrac.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE NODALVFRAC(IFUNC    , WA4, IFLOW, RFLOW, IPARG,
     .                      ELBUF_TAB, IX , NIX  , ITAB , NV46 )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This suroutine computes nodal volumetric fraction for
C     ALE elements. In case of CEL coupling (inter22)
C     result is also calcultated from cut cells.
C     Aim is to expand centroid values to nodal positions
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     Tested below during NG LOOP : IALEL > 0 
C        where IALEL =IPARG(7,NG)+IPARG(11,NG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE I22BUFBRIC_MOD
      USE I22EDGE_MOD
      USE I22TRI_MOD
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
#include      "scr05_c.inc"
#include      "flowcom.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IFUNC, IFLOW(*),IPARG(NPARG,*),IX(NIX,*),ITAB(*),NIX,NV46
      my_real,INTENT(IN) :: RFLOW(*)
      REAL,INTENT(INOUT) :: WA4(*)

      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(BUF_MAT_),POINTER                          :: MBUF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                            :: IADI, IADR, I, ITYP, NINOUT, NNO, NEL, II1, II2,K1,K,
     .                                      IR1, IR2, J, JJ, NNO_L, NNI_L, II3, II4, JJJ, NNI,
     .                                      IALEL,NNOD,IPOS,IV,NGv,J1,J2,IBV, MLW,NumNodCell,
     .                                      NG, KCVT, II, NBF, NBL, IB, ICELL, NIN, MCELL,
     .                                      IPHASE
      TYPE(G_BUFEL_)  ,POINTER           :: GBUF,GBUFv
      my_real, ALLOCATABLE, DIMENSION(:) :: COUNT_VOL
      my_real                            :: P,VF,D,V
      INTEGER,DIMENSION(:,:), POINTER    :: pAdjBRICK
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   This subroutine writes nodal VFRAC
C   /INTER/TYPE22  (only).
C
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      


         NNOD   = NIX-3   !8-node brick or 4-node quad
         IPHASE = IFUNC-19

         IF(INT22==0)THEN
         !---------------------------------------------------------!
         !         ALE STANDARD FORMULATION                        !
         !---------------------------------------------------------!
         !1. COMPUTE NODAL VALUE                                   !
         !---------------------------------------------------------!
           !---1. COMPUTE NODAL VFRAC---!
           ALLOCATE(COUNT_VOL(NUMNOD))
           COUNT_VOL(:) = 0
           DO NG = 1, NGROUP
             MLW   = IPARG(1,NG)
             NEL   = IPARG(2,NG)
             NFT   = IPARG(3,NG)
             ITYP  = IPARG(5,NG)
             IALEL = IPARG(7,NG)+IPARG(11,NG)
             IF(ITYP/=1 .AND. ITYP/=2)CYCLE
             IF(IALEL==0)CYCLE
             IF(MLW/=37.AND.MLW/=51)CYCLE
             GBUF => ELBUF_TAB(NG)%GBUF
             MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
             IF(MLW==37)THEN
               DO I=1,NEL
                 VF              = MBUF%VAR(NEL*(3+IPHASE-1)+I) !liquid or gas in uvar(I,4:5)
                 V               = GBUF%VOL(I)
                 DO J=2,NNOD+1
                   JJ            = IX(J,NFT+I)
                   WA4(JJ)       = WA4(JJ)+VF*V*ONE_OVER_8
                   COUNT_VOL(JJ) = COUNT_VOL(JJ) + V * ONE_OVER_8
                 ENDDO !next J
               ENDDO! next I
             ELSEIF(MLW==51)THEN
               DO I=1,NEL
                 IPOS             = 1
                 K1               = N0PHAS + (IPHASE-1)*NVPHAS +IPOS-1   ! exemple : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}
                 K                = K1 * NEL
                 VF               = MBUF%VAR(K+I)
                 V                = GBUF%VOL(I)
                 DO J=2,NNOD+1
                   JJ            = IX(J,NFT+I)
                   WA4(JJ)       = WA4(JJ)+VF*V*ONE_OVER_8
                   COUNT_VOL(JJ) = COUNT_VOL(JJ) + V * ONE_OVER_8
                 ENDDO !next J
               ENDDO! next I
             ENDIF
           ENDDO!next NG
           !applying weight factor
           DO I=1,NUMNOD
             IF(COUNT_VOL(I)/=ZERO)THEN
               WA4(I)=WA4(I)/COUNT_VOL(I)
             ENDIF
           ENDDO
           DEALLOCATE(COUNT_VOL)
         ELSEIF(INT22>0)THEN
         !---------------------------------------------------------!
         !         /INTER/TYPE22                                   !
         !---------------------------------------------------------!
         !1. TAG FOR CUT CELL                                      !
         !2. COMPUTE NODAL VALUE                                   !
         !    NOT INTERSECTED : NODAL Val COMPUTED FROM GLOBAL BUF !
         !---------------------------------------------------------!
           !---1. TAG FOR INTERSECTED BRICKS---!
           !NBF = 1+ITASK*NB/NTHREAD
           !NBL = (ITASK+1)*NB/NTHREAD
           NBF = 1
           NBL = NB
           NIN = 1
           !---1. COMPUTE NODAL PRESSURE---!
           ALLOCATE(COUNT_VOL(NUMNOD))
           COUNT_VOL = 0
           DO NG = 1, NGROUP
             MLW   = IPARG(1,NG)
             NEL   = IPARG(2,NG)
             NFT   = IPARG(3,NG)
             ITYP  = IPARG(5,NG)
             IALEL = IPARG(7,NG)+IPARG(11,NG)
             IF(ITYP/=1 .AND. ITYP/=2)CYCLE
             IF(IALEL==0)CYCLE
             IF(MLW/=37.AND.MLW/=51)CYCLE
             GBUF => ELBUF_TAB(NG)%GBUF
             MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
              DO I=1,NEL
                IB = NINT(GBUF%TAG22(I))
                !---------------------------!
                ! NOT A CUT CELL            !
                !---------------------------!
                IF(IB==0)THEN
                  IF(MLW==37)THEN
                    VF              = MBUF%VAR(NEL*(3+IPHASE-1)+I)  !liquid or gas in uvar(I,4:5)
                    V               = GBUF%VOL(I)
                    DO J=2,NNOD+1
                      JJ            = IX(J,NFT+I)
                      WA4(JJ)       = WA4(JJ)+VF*V*ONE_OVER_8
                      COUNT_VOL(JJ) = COUNT_VOL(JJ) + V * ONE_OVER_8 !cumulated volume
                    ENDDO !next J
                  ELSEIF(MLW==51)THEN
                    IPOS             = 1
                    K1               = N0PHAS + (IPHASE-1)*NVPHAS +IPOS-1   ! exemple : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}
                    K                = K1 * NEL
                    VF               = MBUF%VAR(K+I)
                    V                = GBUF%VOL(I)
                    DO J=2,NNOD+1
                      JJ            = IX(J,NFT+I)
                      WA4(JJ)       = WA4(JJ)+VF*V*ONE_OVER_8
                      COUNT_VOL(JJ) = COUNT_VOL(JJ) + V * ONE_OVER_8
                    ENDDO !next J
                  ENDIF
                !---------------------------!
                !        CUT CELL           !
                !---------------------------!
                ELSE
                  NIN             = 1
                  DO J=2,NNOD+1
                    JJ            = IX(J,NFT+I)
                    ICELL         = BRICK_LIST(NIN,IB)%NODE(J-1)%WhichCell
                    VF            = BRICK_LIST(NIN,IB)%POLY(ICELL)%VFRACm(IPHASE)
                   ! if(vf<zero .or. vf>un)then
                   !   print *,"**inter22, cell vfrac warning", vf
                   ! endif
                    NumNodCELL    = BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD
                    V             = BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew
                    WA4(JJ)       = WA4(JJ)+VF*V/NumNODCell
                    COUNT_VOL(JJ) = COUNT_VOL(JJ) + V / NumNodCELL
                  ENDDO  !next J
                ENDIF
              ENDDO!next I
           ENDDO!next NG
           DO I=1,NUMNOD
             IF(COUNT_VOL(I)/=ZERO)THEN
               WA4(I)=WA4(I)/COUNT_VOL(I)
             ENDIF
           ENDDO
           DEALLOCATE(COUNT_VOL)

         ENDIF!INT22


C-----------------------------------------------         
      RETURN
      END
