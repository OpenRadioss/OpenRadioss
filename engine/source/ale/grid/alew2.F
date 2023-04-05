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
Chd|  ALEW2                         source/ale/grid/alew2.F       
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|        SPMD_EXALEW                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXALEW_PON               source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE ALEW2(
     1   X       ,D      ,V       ,W     ,WA    ,
     2   NALE    ,IPARG  ,NC      ,WB    ,
     3   IAD_ELEM,FR_ELEM,FR_NBCC ,SIZEN ,ADDCNE,
     4   PROCNE  ,FSKY   ,FSKYV   ,IADS  ,ITAB)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Compute Grid for /ALE/GRID/SPRING
C
C     X,D,V are allocated to SX,SD,DV=3*(NUMNOD_L+NUMVVOIS_L)
C      in grid subroutine it may needed to access nodes which
C      are connected to a remote elem. They are sored in X(1:3,NUMNOD+1:)
C      Consequently X is defined here X(3,SX/3) instead of X(3,NUMNOD) as usually
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
! SPMD CASE : SX >= 3*NUMNOD    (SX = 3*(NUMNOD_L+NRCVVOIS_L))
! X(1:3,1:NUMNOD) : local nodes
!  (1:3, NUMNOD+1:) additional nodes (also on adjacent domains but connected to the boundary of the current domain)
!      idem with D(SD), and V(SV)
C-----------------------------------------------
      INTEGER NALE(NUMNOD), IPARG(NPARG,NGROUP), NC(11,*), ADDCNE(*), PROCNE(*),
     .        IAD_ELEM(2,*), FR_ELEM(*), FR_NBCC(2,*), IADS(8,*),ITAB(NUMNOD),
     .        SIZEN
      my_real X(3,SX/3), D(3,SD/3), V(3,SV/3), W(3,SW/3), WA(3,*), WB(3,*),
     .        FSKY(8,LSKY), FSKYV(LSKY,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICT(2,28), J1(MVSIZ), J2(MVSIZ), I, NG, NEL, NFT, ITY, MQE, ITR,
     .   NTR, N, NNC, NCT, K, II, SIZE, LENR, LENS
      my_real
     .   FAC(28), 
     .   DX(MVSIZ)   ,DY(MVSIZ) , DZ(MVSIZ), 
     .   XX(MVSIZ)   ,XY(MVSIZ) , XZ(MVSIZ),
     .   DL1(MVSIZ)  ,DDX(MVSIZ), DDY(MVSIZ), DDZ(MVSIZ), 
     .   DL(MVSIZ)   ,XL(MVSIZ) , DDL(MVSIZ), 
     .   BETA, DLM, DLMIN, GAM1, WBTMP(3,SIZEN)  
C-----------------------------------------------
      DATA FAC/1.,1.,1.,1.,
     A         1.,1.,1.,1.,
     A         1.,1.,1.,1.,
     D         1.,1.,1.,1.,
     D         1.,1.,1.,1.,
     D         1.,1.,1.,1.,
     I         0.,0.,0.,0./
      DATA ICT/1,2,2,3,3,4,4,1,
     A         5,6,6,7,7,8,8,5,
     A         1,5,2,6,3,7,4,8,
     D         1,3,2,4,5,7,6,8,
     D         1,6,2,5,2,7,3,6,
     D         3,8,4,7,1,8,4,5,
     I         1,7,2,8,3,5,4,6/
      DATA NTR/24/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DLMIN = EM01
      GAM1=HALF*(ALE%GRID%GAMMA-ONE)
      DO I=13,24
       FAC(I)=ALE%GRID%VGY
      ENDDO
      BETA=SIX*(ONE + TWO*ALE%GRID%VGY)
C-----------------------------------------------
C SPMD code makes difference between Parith/ON & OFF
C-----------------------------------------------
      IF (IPARIT /= 0) THEN
        DO I=1,NUMNOD
          IF(IABS(NALE(I)) == 1) THEN
            WA(1,I)=ZERO
            WA(2,I)=ZERO
            WA(3,I)=ZERO
          ENDIF
        ENDDO
      ELSE
C usage of WBTMP in P/OFF
        DO I=1,NUMNOD
          IF(IABS(NALE(I)) == 1) THEN
            WA(1,I)=ZERO
            WA(2,I)=ZERO
            WA(3,I)=ZERO
            WBTMP(1,I)=ZERO
            WBTMP(2,I)=ZERO
            WBTMP(3,I)=ZERO
         ENDIF
        ENDDO
      ENDIF
C
C zeroing FSKY for Parith/ON
C
      IF(IPARIT /= 0) THEN
         IF(IVECTOR == 0) THEN
          DO N=1,NUMNOD
           IF(IABS(NALE(N)) == 1) THEN
            NCT = ADDCNE(N)-1
            NNC = ADDCNE(N+1)-ADDCNE(N)
            DO K = NCT+1, NCT+NNC
              FSKY(1,K) = ZERO
              FSKY(2,K) = ZERO
              FSKY(3,K) = ZERO
              FSKY(4,K) = ZERO
              FSKY(5,K) = ZERO
              FSKY(6,K) = ZERO
            ENDDO
           ENDIF
          ENDDO
         ELSE
          DO N=1,NUMNOD
           IF(IABS(NALE(N)) == 1) THEN
            NCT = ADDCNE(N)-1
            NNC = ADDCNE(N+1)-ADDCNE(N)
            DO K = NCT+1, NCT+NNC
              FSKYV(K,1) = ZERO
              FSKYV(K,2) = ZERO
              FSKYV(K,3) = ZERO
              FSKYV(K,4) = ZERO
              FSKYV(K,5) = ZERO
              FSKYV(K,6) = ZERO
            ENDDO
           ENDIF
          ENDDO
         ENDIF
      ENDIF
C
      DLM=ZERO
      
      DO NG=1,NGROUP
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        ITY=IPARG(5,NG)
        MQE=IPARG(7,NG)

        IF (ITY == 1 .AND. MQE == 1) THEN
          DO ITR=1,NTR
            DO I=1,NEL
              J1(I)=NC(ICT(1,ITR)+1,NFT+I)
              J2(I)=NC(ICT(2,ITR)+1,NFT+I)
              DDX(I)=(W(1,J2(I))-W(1,J1(I)))*DT2
              DDY(I)=(W(2,J2(I))-W(2,J1(I)))*DT2
              DDZ(I)=(W(3,J2(I))-W(3,J1(I)))*DT2
              DX(I)=D(1,J2(I))-D(1,J1(I))
              DY(I)=D(2,J2(I))-D(2,J1(I))
              DZ(I)=D(3,J2(I))-D(3,J1(I))
              XX(I)=X(1,J2(I))-X(1,J1(I))
              XY(I)=X(2,J2(I))-X(2,J1(I))
              XZ(I)=X(3,J2(I))-X(3,J1(I))
            ENDDO!next I
C
            DO I=1,NEL
              XX(I)=XX(I)-DX(I)
              XY(I)=XY(I)-DY(I)
              XZ(I)=XZ(I)-DZ(I)
              XL(I)=SQRT(XX(I)**2+XY(I)**2+XZ(I)**2)
              DL(I)=(XX(I)*DX(I)+XY(I)*DY(I)+XZ(I)*DZ(I))/XL(I)
              DDL(I)=(XX(I)*DDX(I)+XY(I)*DDY(I)+XZ(I)*DDZ(I))/XL(I)
              DLM=MIN(DLM,DL(I)/XL(I))
              DL1(I)=ALE%GRID%GAMMA+GAM1*MIN(DL(I)/XL(I),ZERO)
              DL(I) = FAC(ITR)/XL(I) /ALE%GRID%ALPHA/ALE%GRID%ALPHA *DDL(I)*DL1(I)
              DDL(I)= FAC(ITR)/XL(I) * ALE%GRID%VGX/ALE%GRID%ALPHA*DDL(I)
            ENDDO!next I
           
            ! --- SPMD PARITH/OFF
            IF(IPARIT == 0) THEN
              DO I=1,NEL
               IF(IABS(NALE(J1(I))) == 1) THEN
                 WBTMP(1,J1(I))=WBTMP(1,J1(I))+DL(I)*XX(I)
                 WBTMP(2,J1(I))=WBTMP(2,J1(I))+DL(I)*XY(I)
                 WBTMP(3,J1(I))=WBTMP(3,J1(I))+DL(I)*XZ(I)
                 WA(1,J1(I))=WA(1,J1(I))+DDL(I)*XX(I)
                 WA(2,J1(I))=WA(2,J1(I))+DDL(I)*XY(I)
                 WA(3,J1(I))=WA(3,J1(I))+DDL(I)*XZ(I)
               ENDIF
               IF(IABS(NALE(J2(I))) == 1) THEN
                 WBTMP(1,J2(I))=WBTMP(1,J2(I))-DL(I)*XX(I)
                 WBTMP(2,J2(I))=WBTMP(2,J2(I))-DL(I)*XY(I)
                 WBTMP(3,J2(I))=WBTMP(3,J2(I))-DL(I)*XZ(I)
                 WA(1,J2(I))=WA(1,J2(I))-DDL(I)*XX(I)
                 WA(2,J2(I))=WA(2,J2(I))-DDL(I)*XY(I)
                 WA(3,J2(I))=WA(3,J2(I))-DDL(I)*XZ(I)
               ENDIF
              ENDDO
            ELSE ! => IPARIT /= 0
              ! --- SPMD PARITH/ON
              IF(IVECTOR == 0) THEN
                DO I=1,NEL
                  II = I+NFT
                  IF(IABS(NALE(J1(I))) == 1) THEN
                    K = IADS(ICT(1,ITR),II)
                    ! WB J1
                    FSKY(1,K)=FSKY(1,K)+DL(I)*XX(I)
                    FSKY(2,K)=FSKY(2,K)+DL(I)*XY(I)
                    FSKY(3,K)=FSKY(3,K)+DL(I)*XZ(I)
                    ! WA J1
                    FSKY(4,K)=FSKY(4,K)+DDL(I)*XX(I)
                    FSKY(5,K)=FSKY(5,K)+DDL(I)*XY(I)
                    FSKY(6,K)=FSKY(6,K)+DDL(I)*XZ(I)
                  ENDIF
                  IF(IABS(NALE(J2(I))) == 1) THEN
                    K = IADS(ICT(2,ITR),II)
                    ! WB J2
                    FSKY(1,K)=FSKY(1,K)-DL(I)*XX(I)
                    FSKY(2,K)=FSKY(2,K)-DL(I)*XY(I)
                    FSKY(3,K)=FSKY(3,K)-DL(I)*XZ(I)
                    ! WA J2
                    FSKY(4,K)=FSKY(4,K)-DDL(I)*XX(I)
                    FSKY(5,K)=FSKY(5,K)-DDL(I)*XY(I)
                    FSKY(6,K)=FSKY(6,K)-DDL(I)*XZ(I)
                  ENDIF
                ENDDO
              ELSE ! => (IVECTOR /= 0)
#include "vectorize.inc"
                DO I=1,NEL
                  II = I+NFT
                  IF(IABS(NALE(J1(I))) == 1) THEN
                    K = IADS(ICT(1,ITR),II)
                    ! WB J1
                    FSKYV(K,1)=FSKYV(K,1)+DL(I)*XX(I)
                    FSKYV(K,2)=FSKYV(K,2)+DL(I)*XY(I)
                    FSKYV(K,3)=FSKYV(K,3)+DL(I)*XZ(I)
                    ! WA J1
                    FSKYV(K,4)=FSKYV(K,4)+DDL(I)*XX(I)
                    FSKYV(K,5)=FSKYV(K,5)+DDL(I)*XY(I)
                    FSKYV(K,6)=FSKYV(K,6)+DDL(I)*XZ(I)
                  ENDIF
                  IF(IABS(NALE(J2(I))) == 1) THEN
                    K = IADS(ICT(2,ITR),II)
                    ! WB J2
                    FSKYV(K,1)=FSKYV(K,1)-DL(I)*XX(I)
                    FSKYV(K,2)=FSKYV(K,2)-DL(I)*XY(I)
                    FSKYV(K,3)=FSKYV(K,3)-DL(I)*XZ(I)
                    ! WA J2
                    FSKYV(K,4)=FSKYV(K,4)-DDL(I)*XX(I)
                    FSKYV(K,5)=FSKYV(K,5)-DDL(I)*XY(I)
                    FSKYV(K,6)=FSKYV(K,6)-DDL(I)*XZ(I)
                  ENDIF
                ENDDO!next I
              ENDIF !IVECTOR
            ENDIF!IPARIT 
          ENDDO!next ITR
        ENDIF!IF (ITY == 1 .AND. MQE == 1)
      ENDDO!next NG
C
      ALE%GRID%VGZ=DLM
C-----------------------------------------------
C CODE SPMD : CUMUL WA WB
C-----------------------------------------------
      IF(IPARIT == 0)THEN
        IF(NSPMD > 1)THEN
          SIZE = 6
          LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_EXALEW(WA,WBTMP,IAD_ELEM,FR_ELEM,NALE,SIZE,LENR)
        END IF
        DO I=1,NUMNOD
          IF(IABS(NALE(I)) == 1) THEN
            WB(1,I)=WB(1,I)+WBTMP(1,I)
            WB(2,I)=WB(2,I)+WBTMP(2,I)
            WB(3,I)=WB(3,I)+WBTMP(3,I)
          ENDIF
        ENDDO
      ELSE
        IF(NSPMD > 1)THEN
          SIZE = 6
          LENS = FR_NBCC(1,NSPMD+1)
          LENR = FR_NBCC(2,NSPMD+1)
          CALL SPMD_EXALEW_PON(
     1           FSKY  ,FSKYV ,IAD_ELEM,FR_ELEM,NALE,
     2           ADDCNE,PROCNE,FR_NBCC ,SIZE   ,LENR,
     3           LENS  )
        END IF
        IF(IVECTOR == 1) THEN
          DO N=1,NUMNOD
            IF(IABS(NALE(N)) == 1) THEN
              NCT = ADDCNE(N)-1
              NNC = ADDCNE(N+1)-ADDCNE(N)
              DO K = NCT+1, NCT+NNC
                WB(1,N)  = WB(1,N) + FSKYV(K,1)
                WB(2,N)  = WB(2,N) + FSKYV(K,2)
                WB(3,N)  = WB(3,N) + FSKYV(K,3)
                WA(1,N)  = WA(1,N) + FSKYV(K,4)
                WA(2,N)  = WA(2,N) + FSKYV(K,5)
                WA(3,N)  = WA(3,N) + FSKYV(K,6)
C
                FSKYV(K,1) = ZERO
                FSKYV(K,2) = ZERO
                FSKYV(K,3) = ZERO
                FSKYV(K,4) = ZERO
                FSKYV(K,5) = ZERO
                FSKYV(K,6) = ZERO
              ENDDO
            ENDIF
          ENDDO
        ELSE

          DO N=1,NUMNOD
            IF(IABS(NALE(N)) == 1) THEN
              NCT = ADDCNE(N)-1
              NNC = ADDCNE(N+1)-ADDCNE(N)
              DO K = NCT+1, NCT+NNC
                WB(1,N)  = WB(1,N) + FSKY(1,K)
                WB(2,N)  = WB(2,N) + FSKY(2,K)
                WB(3,N)  = WB(3,N) + FSKY(3,K)
                WA(1,N)  = WA(1,N) + FSKY(4,K)
                WA(2,N)  = WA(2,N) + FSKY(5,K)
                WA(3,N)  = WA(3,N) + FSKY(6,K)
C
                FSKY(1,K) = ZERO
                FSKY(2,K) = ZERO
                FSKY(3,K) = ZERO
                FSKY(4,K) = ZERO
                FSKY(5,K) = ZERO
                FSKY(6,K) = ZERO
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C-----------------------------------------------
      DO I=1,NUMNOD
        IF(IABS(NALE(I)) == 1)THEN
         W(1,I)= W(1,I)+(WB(1,I)*DT2+WA(1,I))/BETA
         W(2,I)= W(2,I)+(WB(2,I)*DT2+WA(2,I))/BETA
         W(3,I)= W(3,I)+(WB(3,I)*DT2+WA(3,I))/BETA
        ELSEIF(NALE(I) == 0)THEN
          W(1,I)=V(1,I)
          W(2,I)=V(2,I)
          W(3,I)=V(3,I)
        ELSEIF(IABS(NALE(I)) == 2)THEN
          W(1,I)=ZERO
          W(2,I)=ZERO
          W(3,I)=ZERO
        ENDIF
      ENDDO!next I
C
      RETURN
      END
