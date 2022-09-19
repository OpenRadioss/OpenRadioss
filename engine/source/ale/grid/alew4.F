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
Chd|  ALEW4                         source/ale/grid/alew4.F       
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|        SPMD_EXALEW                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXALEW_PON               source/mpi/fluid/spmd_cfd.F   
Chd|====================================================================
      SUBROUTINE ALEW4(
     1   X       ,D      ,V       ,W     ,WA    ,
     2   NALE    ,IPARG  ,NC      ,WB    ,
     3   IAD_ELEM,FR_ELEM,FR_NBCC ,SIZEN ,ADDCNE,
     4   PROCNE  ,FSKY   ,FSKYV   ,IADX  ,WMA   ,
     5   NIX,     NIADX  ,ITAB	)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Compute Grid for /ALE/GRID/STANDARD
C
C     X,D,V are allocated to SX,SD,DV=3*(NUMNOD_L+NUMVVOIS_L)
C      in grid subroutine it may needed to access nodes which
C      are connected to a remote elem. They are sored in X(1:3,NUMNOD+1:)
C      Consequently X is defined here X(3,SX/3) instead of X(3,NUMNOD) as usually
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
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
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
      INTEGER NALE(NUMNOD), IPARG(NPARG,NGROUP), NC(NIX,*), ADDCNE(*), PROCNE(*),ITAB(NUMNOD),
     .        IAD_ELEM(2,*), FR_ELEM(*), FR_NBCC(2,*), IADX(NIADX,*) ,
     .        SIZEN, NIX, NIADX
      my_real X(3,SX/3), D(3,SD/3), V(3,SV/3), W(3,SW/3), WA(3,*), WB(3,*),
     .        FSKY(8,LSKY), FSKYV(LSKY,8), WMA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   ICT(2,12), J1(MVSIZ), J2(MVSIZ), I   , NG, NEL, NFT, ITY      , MQE        , ITR     ,
     .   NTR      , N        , NNC      , NCT , K , II , ICT4(2,6), ICT8(2,12) ,ICT2(2,4),
     .   ISOLNOD  , KK       , SIZE     , LENS, LENR
      my_real
     .   DX(MVSIZ) , DY(MVSIZ) , DZ(MVSIZ)  , XX(MVSIZ) , XY(MVSIZ) , XZ(MVSIZ) ,
     .   DDX(MVSIZ), DDY(MVSIZ), DDZ(MVSIZ) , DL(MVSIZ) , DDL(MVSIZ),
     .   GAM1      , DLF       , DDLF       , XM(12)      ,
     .   YM(12)    , ZM(12)    , VX(MVSIZ,3),VY(MVSIZ,3),VZ(MVSIZ,3),VV         ,
     .   X13       , Y13       , Z13        ,X24        ,Y24        ,Z24        ,DT0X       ,
     .   DLF0      ,WBTMP(3,SIZEN)
C-----------------------------------------------
      DATA ICT8/1,2,4,3,8,7,5,6,
     A          1,4,5,8,6,7,2,3,
     B          1,5,2,6,3,7,4,8/
      DATA ICT4/1,3,3,6,6,1,1,5,3,5,6,5/
      DATA ICT2/1,2,4,3,1,4,2,3/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(ALPHA < ZERO)THEN
 	DO I=1,NUMNOD
 	  IF(IABS(NALE(I)) == 1) THEN
 	    WB(1,I)=ZERO
 	    WB(2,I)=ZERO
 	    WB(3,I)=ZERO
 	  ENDIF
 	ENDDO
C
         ALPHA=-ALPHA
      ENDIF
C
      DT0X=DT2/(-VGX+SQRT(VGX**2+ONE))/ALPHA
      IF(TT == ZERO)VGZ=DT0X
      VGZ=MAX(DT0X,(VGZ+HALF*DT0X)/THREE_HALF)
      IF(VGY0 == ZERO)THEN
       VGY0=VGY
       VGZ0=VGZ
      ENDIF
      VGY=VGY0*VGZ/VGZ0
      GAM1=GAMMA-ONE
C
C-----------------------------------------------
C SPMD
C-----------------------------------------------
      IF (IPARIT /= 0) THEN
#include "vectorize.inc"
     	DO I=1,NUMNOD
     	  IF(IABS(NALE(I)) == 1) THEN
     	    WA(1,I)=ZERO
     	    WA(2,I)=ZERO
     	    WA(3,I)=ZERO
     	  ENDIF
     	ENDDO
      ELSE
Cel traitement parith/off avec utilisation WBTMP
#include "vectorize.inc"
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
C mise a 0 de FSKY pour Parith/ON
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
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       ITY=IPARG(5,NG)
       MQE=IPARG(7,NG)
C
       IF ((ITY == 1 .OR. ITY == 2) .AND. MQE == 1) THEN
        ISOLNOD=IPARG(28,NG)

c nombre de ressort/noeud=3
        IF(ITY == 2)THEN
          NTR=4
          DO ITR=1,NTR
            ICT(1,ITR)=ICT2(1,ITR)
            ICT(2,ITR)=ICT2(2,ITR)
          ENDDO
          DO I=1,NEL
             DO ITR=1,NTR
               J1(I)=NC(ICT(1,ITR)+1,NFT+I)
               J2(I)=NC(ICT(2,ITR)+1,NFT+I)
               XM(ITR)=X(1,J1(I))+X(1,J2(I))
               YM(ITR)=X(2,J1(I))+X(2,J2(I))
               ZM(ITR)=X(3,J1(I))+X(3,J2(I))
             ENDDO
             VY(I,1)=  ZM(2)-ZM(1)
             VZ(I,1)=-(YM(2)-YM(1))
             VV=SQRT(VY(I,1)**2+VZ(I,1)**2)
             VX(I,1)=0.
             VY(I,1)=VY(I,1)/VV
             VZ(I,1)=VZ(I,1)/VV
             VY(I,2)=  ZM(4)-ZM(3)
             VZ(I,2)=-(YM(4)-YM(3))
             VV=SQRT(VY(I,2)**2+VZ(I,2)**2)
             VX(I,2)=0.
             VY(I,2)=VY(I,2)/VV
             VZ(I,2)=VZ(I,2)/VV
          ENDDO
        ELSEIF(ISOLNOD == 4)THEN
          NTR=6
          DO ITR=1,NTR
            ICT(1,ITR)=ICT4(1,ITR)
            ICT(2,ITR)=ICT4(2,ITR)
          ENDDO
        ELSE
          NTR=12
          DO ITR=1,NTR
            ICT(1,ITR)=ICT8(1,ITR)
            ICT(2,ITR)=ICT8(2,ITR)
          ENDDO
C middle
          DO I=1,NEL
             DO ITR=1,12
               J1(I)=NC(ICT(1,ITR)+1,NFT+I)
               J2(I)=NC(ICT(2,ITR)+1,NFT+I)
               XM(ITR)=X(1,J1(I))+X(1,J2(I))
               YM(ITR)=X(2,J1(I))+X(2,J2(I))
               ZM(ITR)=X(3,J1(I))+X(3,J2(I))
             ENDDO
             DO K=1,3
               KK=4*(K-1)
               X13=XM(KK+3)-XM(KK+1)
               Y13=YM(KK+3)-YM(KK+1)
               Z13=ZM(KK+3)-ZM(KK+1)
               X24=XM(KK+4)-XM(KK+2)
               Y24=YM(KK+4)-YM(KK+2)
               Z24=ZM(KK+4)-ZM(KK+2)
               VX(I,K)=Y13*Z24-Z13*Y24
               VY(I,K)=Z13*X24-X13*Z24
               VZ(I,K)=X13*Y24-Y13*X24
               VV=SQRT(VX(I,K)**2+VY(I,K)**2+VZ(I,K)**2)
               VX(I,K)=VX(I,K)/VV
               VY(I,K)=VY(I,K)/VV
               VZ(I,K)=VZ(I,K)/VV
             ENDDO
          ENDDO
        ENDIF
        DO ITR=1,NTR
         IF(ITY == 1)KK=(ITR+3)/4
         IF(ITY == 2)KK=(ITR+1)/2
         DO  I=1,NEL
C
 	  J1(I)=NC(ICT(1,ITR)+1,NFT+I)
 	  J2(I)=NC(ICT(2,ITR)+1,NFT+I)
C
 	  DDX(I)=(W(1,J2(I))-W(1,J1(I)))*DT2
 	  DDY(I)=(W(2,J2(I))-W(2,J1(I)))*DT2
 	  DDZ(I)=(W(3,J2(I))-W(3,J1(I)))*DT2
 	  DX(I)=D(1,J2(I))-D(1,J1(I))
 	  DY(I)=D(2,J2(I))-D(2,J1(I))
 	  DZ(I)=D(3,J2(I))-D(3,J1(I))
 	  XX(I)=X(1,J2(I))-X(1,J1(I))
 	  XY(I)=X(2,J2(I))-X(2,J1(I))
 	  XZ(I)=X(3,J2(I))-X(3,J1(I))
C
 	  DDLF=VX(I,KK)*DDX(I)+VY(I,KK)*DDY(I)+VZ(I,KK)*DDZ(I)
 	  DLF0=ABS(VX(I,KK)*XX(I)+VY(I,KK)*XY(I)+VZ(I,KK)*XZ(I))
 	  DLF=(DLF0-VGY)/VGY
 	  DLF=MIN(DLF,ZERO)
 	  DLF0=DLF0-0.2*VGY
 	  DLF0=MIN(DLF0,ZERO)
 	  DLF=GAMMA+GAM1*DLF*DLF*DLF
 	  DLF=MIN(DLF,ONE)
 	  DX(I)=DT2*DLF0*DLF/VGZ/VGZ
 	  DDL(I)= VGX/VGZ
 	  IF(DDLF > 0.)DLF=GAMMA
 	  DL(I) = 1. /VGZ/VGZ *DLF
c	   DL(I)=0.
C
         ENDDO
C
C-----------------------------------------------
C SPMD PARITH/OFF
C-----------------------------------------------
         IF(IPARIT == 0) THEN
          DO I=1,NEL
           IF(NALE(J1(I)) /= 0) THEN
 	    WBTMP(1,J1(I))=WBTMP(1,J1(I))+DDX(I)*DL(I)
 	    WBTMP(2,J1(I))=WBTMP(2,J1(I))+DDY(I)*DL(I)
 	    WBTMP(3,J1(I))=WBTMP(3,J1(I))+DDZ(I)*DL(I)
 	    WA(1,J1(I))=WA(1,J1(I))+DDX(I)*DDL(I)
 	    WA(2,J1(I))=WA(2,J1(I))+DDY(I)*DDL(I)
 	    WA(3,J1(I))=WA(3,J1(I))+DDZ(I)*DDL(I)
           ENDIF
C
 	   IF(NALE(J2(I)) /= 0) THEN
 	    WBTMP(1,J2(I))=WBTMP(1,J2(I))-DDX(I)*DL(I)
 	    WBTMP(2,J2(I))=WBTMP(2,J2(I))-DDY(I)*DL(I)
 	    WBTMP(3,J2(I))=WBTMP(3,J2(I))-DDZ(I)*DL(I)
 	    WA(1,J2(I))=WA(1,J2(I))-DDX(I)*DDL(I)
 	    WA(2,J2(I))=WA(2,J2(I))-DDY(I)*DDL(I)
 	    WA(3,J2(I))=WA(3,J2(I))-DDZ(I)*DDL(I)
 	   ENDIF
          ENDDO
         ELSE
C-----------------------------------------------
C SPMD PARITH/ON
C-----------------------------------------------
          IF(IVECTOR == 0) THEN
           DO I=1,NEL
            II = I+NFT
            IF(NALE(J1(I)) /= 0) THEN
              K = IADX(ICT(1,ITR),II)
C WB J1
              FSKY(1,K)=FSKY(1,K)+DL(I)*DDX(I)
              FSKY(2,K)=FSKY(2,K)+DL(I)*DDY(I)
              FSKY(3,K)=FSKY(3,K)+DL(I)*DDZ(I)
C WA J1
              FSKY(4,K)=FSKY(4,K)+DDL(I)*DDX(I)
              FSKY(5,K)=FSKY(5,K)+DDL(I)*DDY(I)
              FSKY(6,K)=FSKY(6,K)+DDL(I)*DDZ(I)
            ENDIF
C
            IF(NALE(J2(I)) /= 0) THEN
              K = IADX(ICT(2,ITR),II)
C WB J2
              FSKY(1,K)=FSKY(1,K)-DL(I)*DDX(I)
              FSKY(2,K)=FSKY(2,K)-DL(I)*DDY(I)
              FSKY(3,K)=FSKY(3,K)-DL(I)*DDZ(I)
C WA J2
              FSKY(4,K)=FSKY(4,K)-DDL(I)*DDX(I)
              FSKY(5,K)=FSKY(5,K)-DDL(I)*DDY(I)
              FSKY(6,K)=FSKY(6,K)-DDL(I)*DDZ(I)
            ENDIF
           ENDDO
          ELSE
#include "vectorize.inc"
           DO I=1,NEL
            II = I+NFT
            IF(NALE(J1(I)) /= 0) THEN
              K = IADX(ICT(1,ITR),II)
C WB J1
              FSKYV(K,1)=FSKYV(K,1)+DL(I)*DDX(I)
              FSKYV(K,2)=FSKYV(K,2)+DL(I)*DDY(I)
              FSKYV(K,3)=FSKYV(K,3)+DL(I)*DDZ(I)
C WA J1
              FSKYV(K,4)=FSKYV(K,4)+DDL(I)*DDX(I)
              FSKYV(K,5)=FSKYV(K,5)+DDL(I)*DDY(I)
              FSKYV(K,6)=FSKYV(K,6)+DDL(I)*DDZ(I)
            ENDIF
C
            IF(NALE(J2(I)) /= 0) THEN
              K = IADX(ICT(2,ITR),II)
C WB J2
              FSKYV(K,1)=FSKYV(K,1)-DL(I)*DDX(I)
              FSKYV(K,2)=FSKYV(K,2)-DL(I)*DDY(I)
              FSKYV(K,3)=FSKYV(K,3)-DL(I)*DDZ(I)
C WA J2
              FSKYV(K,4)=FSKYV(K,4)-DDL(I)*DDX(I)
              FSKYV(K,5)=FSKYV(K,5)-DDL(I)*DDY(I)
              FSKYV(K,6)=FSKYV(K,6)-DDL(I)*DDZ(I)
            ENDIF
           ENDDO
          ENDIF
C-----------------------------------------------
         ENDIF
        ENDDO!next ITR
       ENDIF
      ENDDO!next NG
C
C-----------------------------------------------
C SPMD : CUMULATION OF WA WB
C-----------------------------------------------
	IF(IPARIT == 0)THEN
        IF(NSPMD > 1)THEN
          SIZE = 6
          LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_EXALEW(WA,WBTMP,IAD_ELEM,FR_ELEM,NALE,SIZE,LENR)
        END IF
        DO I=1,NUMNOD
          IF(NALE(I) /= 0) THEN
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
C SPMD treatment  parith/on
        IF(IVECTOR == 1) THEN
          DO N=1,NUMNOD
            IF(NALE(N) /= 0) THEN
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
            IF(NALE(N) /= 0) THEN
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
         W(1,I)= W(1,I)+(WB(1,I)*DT2+WA(1,I))/WMA(I)
         W(2,I)= W(2,I)+(WB(2,I)*DT2+WA(2,I))/WMA(I)
         W(3,I)= W(3,I)+(WB(3,I)*DT2+WA(3,I))/WMA(I)
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
