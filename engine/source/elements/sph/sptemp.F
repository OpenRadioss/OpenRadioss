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
Chd|  SPGRADT                       source/elements/sph/sptemp.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F
Chd|-- calls ---------------
Chd|        WEIGHT1                       source/elements/sph/weight.F
Chd|        SPHBOX                        share/modules/sphbox.F
Chd|====================================================================
      SUBROUTINE SPGRADT(
     1   X,       MS,      SPBUF,   KXSP,
     2   IXSP,    NOD2SP,  ISPSYM,  XSPSYM,
     3   WA,      WACOMP,  WTEMP,   WTR,
     4   WGRADT,  LFT,     LLT,     NFT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(INOUT) :: LFT
        INTEGER, INTENT(INOUT) :: LLT
        INTEGER, INTENT(INOUT) :: NFT
        INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .          ISPSYM(NSPCOND,*)
        my_real
     .     X(3,*)    ,MS(*)   ,
     .     SPBUF(NSPBUF,*) ,XSPSYM(3,*) ,
     .     WA(KWASPH,*) ,WACOMP(16,*),
     .     WTEMP(*), WTR(*), WGRADT(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,N,INOD,JNOD,J,NVOIS,M,
     .          NVOISS,SM,JS,NC,NS,NN
        my_real
     .         XI,YI,ZI,DI,RHOI,XJ,YJ,ZJ,DJ,RHOJ,DIJ,
     .         VJ,WGHT,WGRAD(3),
     .   ALPHAI,ALPHAXI,ALPHAYI,ALPHAZI,
     .   BETAXI,BETAYI,BETAZI,
     .   BETAXXI,BETAYXI,BETAZXI,
     .   BETAXYI,BETAYYI,BETAZYI,
     .   BETAXZI,BETAYZI,BETAZZI,
     .   BETAX,WGRDX,WGRDY,WGRDZ,
     .   TI,TJ
C-------------------------------------------
        DO I=LFT,LLT
          N    =NFT+I
          WGRADT(1,N)=ZERO
          WGRADT(2,N)=ZERO
          WGRADT(3,N)=ZERO
        ENDDO
C-----------------------------------------------
C     Calcul du gradient de temperature.
C-----------------------------------------------
        DO 10 I=LFT,LLT
          N    =NFT+I
          IF(KXSP(2,N)<=0)GOTO 10
          INOD =KXSP(3,N)
          XI=X(1,INOD)
          YI=X(2,INOD)
          ZI=X(3,INOD)
          DI=SPBUF(1,N)
          TI=WTEMP(N)
          RHOI =WA(10,N)
C-----
          ALPHAI=WACOMP(1,N)
          BETAXI=WACOMP(2,N)
          BETAYI=WACOMP(3,N)
          BETAZI=WACOMP(4,N)
          ALPHAXI=WACOMP( 5,N)
          ALPHAYI=WACOMP( 6,N)
          ALPHAZI=WACOMP( 7,N)
          BETAXXI=WACOMP( 8,N)
          BETAYXI=WACOMP( 9,N)
          BETAZXI=WACOMP(10,N)
          BETAXYI=WACOMP(11,N)
          BETAYYI=WACOMP(12,N)
          BETAZYI=WACOMP(13,N)
          BETAXZI=WACOMP(14,N)
          BETAYZI=WACOMP(15,N)
          BETAZZI=WACOMP(16,N)
C------
          NVOIS=KXSP(4,N)
          DO J=1,NVOIS
            JNOD=IXSP(J,N)
            IF(JNOD>0)THEN
              M=NOD2SP(JNOD)
              XJ=X(1,JNOD)
              YJ=X(2,JNOD)
              ZJ=X(3,JNOD)
              DJ=SPBUF(1,M)
              DIJ=0.5*(DI+DJ)
              CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
              RHOJ=WA(10,M)
              VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
              TJ=WTEMP(M)
            ELSE
              NN = -JNOD
              XJ=XSPHR(3,NN)
              YJ=XSPHR(4,NN)
              ZJ=XSPHR(5,NN)
              DJ=XSPHR(2,NN)
              DIJ=0.5*(DI+DJ)
              CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
              RHOJ=XSPHR(7,NN)
              VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
              TJ=WTR(NN)
            END IF
            BETAX=ONE +BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
            WGRDX=
     .       WGRAD(1)*ALPHAI*BETAX
     .       +WGHT*
     .         (ALPHAXI*BETAX+ALPHAI*
     .           (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
            WGRDY=
     .       WGRAD(2)*ALPHAI*BETAX
     .       +WGHT*
     .         (ALPHAYI*BETAX+ALPHAI*
     .           (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
            WGRDZ=
     .       WGRAD(3)*ALPHAI*BETAX
     .       +WGHT*
     .         (ALPHAZI*BETAX+ALPHAI*
     .           (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
            WGRAD(1)=WGRDX
            WGRAD(2)=WGRDY
            WGRAD(3)=WGRDZ
C
            WGRADT(1,N)=WGRADT(1,N)+VJ*(TJ-TI)*WGRAD(1)
            WGRADT(2,N)=WGRADT(2,N)+VJ*(TJ-TI)*WGRAD(2)
            WGRADT(3,N)=WGRADT(3,N)+VJ*(TJ-TI)*WGRAD(3)
C--------
          END DO
C------
C      partie symetrique.
          NVOISS=KXSP(6,N)
          DO J=KXSP(5,N)+1,KXSP(5,N)+NVOISS
            JS=IXSP(J,N)
            IF(JS>0)THEN
              SM=JS/(NSPCOND+1)
              NC=MOD(JS,NSPCOND+1)
              JS=ISPSYM(NC,SM)
              XJ =XSPSYM(1,JS)
              YJ =XSPSYM(2,JS)
              ZJ =XSPSYM(3,JS)
              DJ  =SPBUF(1,SM)
              DIJ =HALF*(DI+DJ)
              RHOJ=WA(10,SM)
              CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
              JNOD=KXSP(3,SM)
              VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)
              TJ=WTEMP(SM)
            ELSE
              SM=-JS/(NSPCOND+1)
              NC=MOD(-JS,NSPCOND+1)
              JS=ISPSYMR(NC,SM)
              XJ =XSPSYM(1,JS)
              YJ =XSPSYM(2,JS)
              ZJ =XSPSYM(3,JS)
              DJ  =XSPHR(2,SM)
              DIJ =HALF*(DI+DJ)
              RHOJ=XSPHR(7,SM)
              CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
              JNOD=KXSP(3,SM)
              VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)
              TJ=WTR(SM)
            END IF
            BETAX=ONE + BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
            WGRDX=
     .       WGRAD(1)*ALPHAI*BETAX
     .       +WGHT*
     .         (ALPHAXI*BETAX+ALPHAI*
     .           (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
            WGRDY=
     .       WGRAD(2)*ALPHAI*BETAX
     .       +WGHT*
     .         (ALPHAYI*BETAX+ALPHAI*
     .           (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
            WGRDZ=
     .       WGRAD(3)*ALPHAI*BETAX
     .       +WGHT*
     .         (ALPHAZI*BETAX+ALPHAI*
     .           (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
            WGRAD(1)=WGRDX
            WGRAD(2)=WGRDY
            WGRAD(3)=WGRDZ
C
            WGRADT(1,N)=WGRADT(1,N)+VJ*(TJ-TI)*WGRAD(1)
            WGRADT(2,N)=WGRADT(2,N)+VJ*(TJ-TI)*WGRAD(2)
            WGRADT(3,N)=WGRADT(3,N)+VJ*(TJ-TI)*WGRAD(3)
          END DO
C------
 10     CONTINUE
C-----------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  SPLAPLT                       source/elements/sph/sptemp.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F
Chd|-- calls ---------------
Chd|        WEIGHT1                       source/elements/sph/weight.F
Chd|        SPHBOX                        share/modules/sphbox.F
Chd|====================================================================
      SUBROUTINE SPLAPLT(
     1   X,       MS,      SPBUF,   KXSP,
     2   IXSP,    NOD2SP,  ISPSYM,  XSPSYM,
     3   WA,      WACOMP,  WGRADT,  WGR,
     4   WGRADTSM,WLAPLT,  WSMCOMP, LAMBDA,
     5   LAMBDR,  LFT,     LLT,     NFT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(INOUT) :: LFT
        INTEGER, INTENT(INOUT) :: LLT
        INTEGER, INTENT(INOUT) :: NFT
        INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .          ISPSYM(NSPCOND,*)
        my_real
     .     X(3,*)    ,MS(*)   ,
     .     SPBUF(NSPBUF,*) ,XSPSYM(3,*) ,
     .     WA(KWASPH,*) ,WACOMP(16,*),
     .     WGRADT(3,*),WGR(3,*),WGRADTSM(3,*),
     .     WLAPLT(*),WSMCOMP(6,*),LAMBDA(*),LAMBDR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,N,INOD,JNOD,J,NVOIS,M,
     .          NVOISS,SM,JS,NC,NS,NN
        my_real
     .         XI,YI,ZI,DI,RHOI,XJ,YJ,ZJ,DJ,RHOJ,DIJ,
     .         VJ,WGHT,WGRAD(3),
     .   ALPHAI,ALPHAXI,ALPHAYI,ALPHAZI,
     .   BETAXI,BETAYI,BETAZI,
     .   BETAXXI,BETAYXI,BETAZXI,
     .   BETAXYI,BETAYYI,BETAZYI,
     .   BETAXZI,BETAYZI,BETAZZI,
     .   ALPHAJ,ALPHAXJ,ALPHAYJ,ALPHAZJ,
     .   BETAXJ,BETAYJ,BETAZJ,
     .   BETAXXJ,BETAYXJ,BETAZXJ,
     .   BETAXYJ,BETAYYJ,BETAZYJ,
     .   BETAXZJ,BETAYZJ,BETAZZJ,
     .   BETAX,WGRDX,WGRDY,WGRDZ,WGRD(3),
     .   GRADTXI,GRADTYI,GRADTZI,
     .   GRADTXJ,GRADTYJ,GRADTZJ
C-------------------------------------------
        DO I=LFT,LLT
          N    =NFT+I
          WLAPLT(N)=ZERO
        ENDDO
C-----------------------------------------------
C     Calcul du Laplacien(T).
C-----------------------------------------------
        DO 10 I=LFT,LLT
          N    =NFT+I
          IF(KXSP(2,N)<=0)GOTO 10
          INOD =KXSP(3,N)
          XI=X(1,INOD)
          YI=X(2,INOD)
          ZI=X(3,INOD)
          DI=SPBUF(1,N)
          GRADTXI=WGRADT(1,N)
          GRADTYI=WGRADT(2,N)
          GRADTZI=WGRADT(3,N)
          RHOI =WA(10,N)
C-----
          ALPHAI=WACOMP(1,N)
          BETAXI=WACOMP(2,N)
          BETAYI=WACOMP(3,N)
          BETAZI=WACOMP(4,N)
          ALPHAXI=WACOMP( 5,N)
          ALPHAYI=WACOMP( 6,N)
          ALPHAZI=WACOMP( 7,N)
          BETAXXI=WACOMP( 8,N)
          BETAYXI=WACOMP( 9,N)
          BETAZXI=WACOMP(10,N)
          BETAXYI=WACOMP(11,N)
          BETAYYI=WACOMP(12,N)
          BETAZYI=WACOMP(13,N)
          BETAXZI=WACOMP(14,N)
          BETAYZI=WACOMP(15,N)
          BETAZZI=WACOMP(16,N)
C------
          NVOIS=KXSP(4,N)
          DO J=1,NVOIS
            JNOD=IXSP(J,N)
            IF(JNOD>0)THEN
              M=NOD2SP(JNOD)
              XJ=X(1,JNOD)
              YJ=X(2,JNOD)
              ZJ=X(3,JNOD)
              DJ=SPBUF(1,M)
              DIJ=0.5*(DI+DJ)
              CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
              RHOJ=WA(10,M)
              VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
              GRADTXJ=WGRADT(1,M)
              GRADTYJ=WGRADT(2,M)
              GRADTZJ=WGRADT(3,M)
              WGRDX=WGRAD(1)
              WGRDY=WGRAD(2)
              WGRDZ=WGRAD(3)
              BETAX=ONE +BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
              WGRAD(1)=
     .         WGRDX*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAXI*BETAX+ALPHAI*
     .           (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
              WGRAD(2)=
     .         WGRDY*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAYI*BETAX+ALPHAI*
     .           (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
              WGRAD(3)=
     .         WGRDZ*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAZI*BETAX+ALPHAI*
     .           (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
C----------
C         noyau conjugue Grad[Wa(b)]
              ALPHAJ=WACOMP(1,M)
              BETAXJ=WACOMP(2,M)
              BETAYJ=WACOMP(3,M)
              BETAZJ=WACOMP(4,M)
              ALPHAXJ=WACOMP( 5,M)
              ALPHAYJ=WACOMP( 6,M)
              ALPHAZJ=WACOMP( 7,M)
              BETAXXJ=WACOMP( 8,M)
              BETAYXJ=WACOMP( 9,M)
              BETAZXJ=WACOMP(10,M)
              BETAXYJ=WACOMP(11,M)
              BETAYYJ=WACOMP(12,M)
              BETAZYJ=WACOMP(13,M)
              BETAXZJ=WACOMP(14,M)
              BETAYZJ=WACOMP(15,M)
              BETAZZJ=WACOMP(16,M)
C
              BETAX=ONE +BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
              WGRD(1)=
     .         -WGRDX*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAXJ*BETAX+ALPHAJ*
     .           (BETAXXJ*(XJ-XI)+BETAYXJ*(YJ-YI)+BETAZXJ*(ZJ-ZI)+BETAXJ))
              WGRD(2)=
     .         -WGRDY*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAYJ*BETAX+ALPHAJ*
     .           (BETAXYJ*(XJ-XI)+BETAYYJ*(YJ-YI)+BETAZYJ*(ZJ-ZI)+BETAYJ))
              WGRD(3)=
     .         -WGRDZ*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAZJ*BETAX+ALPHAJ*
     .           (BETAXZJ*(XJ-XI)+BETAYZJ*(YJ-YI)+BETAZZJ*(ZJ-ZI)+BETAZJ))
C
              WLAPLT(N)=WLAPLT(N)+VJ*(
     .        -LAMBDA(M)*(GRADTXJ*WGRD(1)+GRADTYJ*WGRD(2)+GRADTZJ*WGRD(3))
     .        +LAMBDA(N)*(GRADTXI*WGRAD(1)+GRADTYI*WGRAD(2)+GRADTZI*WGRAD(3)))
C--------
            ELSE
              NN = -JNOD
              XJ=XSPHR(3,NN)
              YJ=XSPHR(4,NN)
              ZJ=XSPHR(5,NN)
              DJ=XSPHR(2,NN)
              DIJ=0.5*(DI+DJ)
              CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
              RHOJ=XSPHR(7,NN)
              VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
              GRADTXJ=WGR(1,NN)
              GRADTYJ=WGR(2,NN)
              GRADTZJ=WGR(3,NN)
              WGRDX=WGRAD(1)
              WGRDY=WGRAD(2)
              WGRDZ=WGRAD(3)
              BETAX=1.+BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
              WGRAD(1)=
     .         WGRDX*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAXI*BETAX+ALPHAI*
     .           (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
              WGRAD(2)=
     .         WGRDY*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAYI*BETAX+ALPHAI*
     .           (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
              WGRAD(3)=
     .         WGRDZ*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAZI*BETAX+ALPHAI*
     .           (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
C----------
C         noyau conjugue Grad[Wa(b)]
              ALPHAJ=WACOMPR(1,NN)
              BETAXJ=WACOMPR(2,NN)
              BETAYJ=WACOMPR(3,NN)
              BETAZJ=WACOMPR(4,NN)
              ALPHAXJ=WACOMPR( 5,NN)
              ALPHAYJ=WACOMPR( 6,NN)
              ALPHAZJ=WACOMPR( 7,NN)
              BETAXXJ=WACOMPR( 8,NN)
              BETAYXJ=WACOMPR( 9,NN)
              BETAZXJ=WACOMPR(10,NN)
              BETAXYJ=WACOMPR(11,NN)
              BETAYYJ=WACOMPR(12,NN)
              BETAZYJ=WACOMPR(13,NN)
              BETAXZJ=WACOMPR(14,NN)
              BETAYZJ=WACOMPR(15,NN)
              BETAZZJ=WACOMPR(16,NN)
C
              BETAX=ONE +BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
              WGRD(1)=
     .         -WGRDX*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAXJ*BETAX+ALPHAJ*
     .           (BETAXXJ*(XJ-XI)+BETAYXJ*(YJ-YI)+BETAZXJ*(ZJ-ZI)+BETAXJ))
              WGRD(2)=
     .         -WGRDY*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAYJ*BETAX+ALPHAJ*
     .           (BETAXYJ*(XJ-XI)+BETAYYJ*(YJ-YI)+BETAZYJ*(ZJ-ZI)+BETAYJ))
              WGRD(3)=
     .         -WGRDZ*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAZJ*BETAX+ALPHAJ*
     .           (BETAXZJ*(XJ-XI)+BETAYZJ*(YJ-YI)+BETAZZJ*(ZJ-ZI)+BETAZJ))
              WLAPLT(N)=WLAPLT(N)+VJ*(
     .        -LAMBDR(NN)*(GRADTXJ*WGRD(1)+GRADTYJ*WGRD(2)+GRADTZJ*WGRD(3))
     .        +LAMBDA(N)*(GRADTXI*WGRAD(1)+GRADTYI*WGRAD(2)+GRADTZI*WGRAD(3)))
            END IF
C--------
          END DO
C------
C      partie symetrique.
          NVOISS=KXSP(6,N)
          DO J=KXSP(5,N)+1,KXSP(5,N)+NVOISS
            JS=IXSP(J,N)
            IF(JS>0)THEN
              SM=JS/(NSPCOND+1)
              NC=MOD(JS,NSPCOND+1)
              JS=ISPSYM(NC,SM)
              XJ =XSPSYM(1,JS)
              YJ =XSPSYM(2,JS)
              ZJ =XSPSYM(3,JS)
              DJ  =SPBUF(1,SM)
              DIJ =HALF*(DI+DJ)
              RHOJ=WA(10,SM)
              CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
              JNOD=KXSP(3,SM)
              VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)
              GRADTXJ=WGRADTSM(1,JS)
              GRADTYJ=WGRADTSM(2,JS)
              GRADTZJ=WGRADTSM(3,JS)
              WGRDX=WGRAD(1)
              WGRDY=WGRAD(2)
              WGRDZ=WGRAD(3)
              BETAX=ONE + BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
              WGRAD(1)=
     .         WGRDX*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAXI*BETAX+ALPHAI*
     .           (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
              WGRAD(2)=
     .         WGRDY*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAYI*BETAX+ALPHAI*
     .           (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
              WGRAD(3)=
     .         WGRDZ*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAZI*BETAX+ALPHAI*
     .           (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
C----------
C         noyau conjugue.
              ALPHAJ=WACOMP(1,SM)
              BETAXJ=WSMCOMP(1,JS)
              BETAYJ=WSMCOMP(2,JS)
              BETAZJ=WSMCOMP(3,JS)
              ALPHAXJ=WSMCOMP( 4,JS)
              ALPHAYJ=WSMCOMP( 5,JS)
              ALPHAZJ=WSMCOMP( 6,JS)
              BETAXXJ=WACOMP( 8,SM)
              BETAYXJ=WACOMP( 9,SM)
              BETAZXJ=WACOMP(10,SM)
              BETAXYJ=WACOMP(11,SM)
              BETAYYJ=WACOMP(12,SM)
              BETAZYJ=WACOMP(13,SM)
              BETAXZJ=WACOMP(14,SM)
              BETAYZJ=WACOMP(15,SM)
              BETAZZJ=WACOMP(16,SM)
C
              BETAX=ONE +BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
              WGRD(1)=
     .         -WGRDX*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAXJ*BETAX+ALPHAJ*
     .           (BETAXXJ*(XJ-XI)+BETAYXJ*(YJ-YI)+BETAZXJ*(ZJ-ZI)+BETAXJ))
              WGRD(2)=
     .         -WGRDY*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAYJ*BETAX+ALPHAJ*
     .           (BETAXYJ*(XJ-XI)+BETAYYJ*(YJ-YI)+BETAZYJ*(ZJ-ZI)+BETAYJ))
              WGRD(3)=
     .         -WGRDZ*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAZJ*BETAX+ALPHAJ*
     .         (BETAXZJ*(XJ-XI)+BETAYZJ*(YJ-YI)+BETAZZJ*(ZJ-ZI)+BETAZJ))
C
              WLAPLT(N)=WLAPLT(N)+VJ*(
     .        -LAMBDA(SM)*(GRADTXJ*WGRD(1)+GRADTYJ*WGRD(2)+GRADTZJ*WGRD(3))
     .        +LAMBDA(N)*(GRADTXI*WGRAD(1)+GRADTYI*WGRAD(2)+GRADTZI*WGRAD(3)))
            ELSE
              SM=-JS/(NSPCOND+1)
              NC=MOD(-JS,NSPCOND+1)
              JS=ISPSYMR(NC,SM)
              XJ =XSPSYM(1,JS)
              YJ =XSPSYM(2,JS)
              ZJ =XSPSYM(3,JS)
              DJ  =XSPHR(2,SM)
              DIJ =HALF*(DI+DJ)
              RHOJ=XSPHR(7,SM)
              CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
              JNOD=KXSP(3,SM)
              VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)
              GRADTXJ=WGRADTSM(1,JS)
              GRADTYJ=WGRADTSM(2,JS)
              GRADTZJ=WGRADTSM(3,JS)
              WGRDX=WGRAD(1)
              WGRDY=WGRAD(2)
              WGRDZ=WGRAD(3)
              BETAX=ONE +BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
              WGRAD(1)=
     .         WGRDX*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAXI*BETAX+ALPHAI*
     .           (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
              WGRAD(2)=
     .         WGRDY*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAYI*BETAX+ALPHAI*
     .           (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
              WGRAD(3)=
     .         WGRDZ*ALPHAI*BETAX
     .         +WGHT*
     .          (ALPHAZI*BETAX+ALPHAI*
     .           (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
C----------
C          noyau conjugue.
              ALPHAJ=WACOMPR(1,SM)
              BETAXJ=WSMCOMP(1,JS)
              BETAYJ=WSMCOMP(2,JS)
              BETAZJ=WSMCOMP(3,JS)
              ALPHAXJ=WSMCOMP( 4,JS)
              ALPHAYJ=WSMCOMP( 5,JS)
              ALPHAZJ=WSMCOMP( 6,JS)
              BETAXXJ=WACOMPR( 8,SM)
              BETAYXJ=WACOMPR( 9,SM)
              BETAZXJ=WACOMPR(10,SM)
              BETAXYJ=WACOMPR(11,SM)
              BETAYYJ=WACOMPR(12,SM)
              BETAZYJ=WACOMPR(13,SM)
              BETAXZJ=WACOMPR(14,SM)
              BETAYZJ=WACOMPR(15,SM)
              BETAZZJ=WACOMPR(16,SM)
C
              BETAX=ONE +BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
              WGRD(1)=
     .         -WGRDX*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAXJ*BETAX+ALPHAJ*
     .          (BETAXXJ*(XJ-XI)+BETAYXJ*(YJ-YI)+BETAZXJ*(ZJ-ZI)+BETAXJ))
              WGRD(2)=
     .         -WGRDY*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAYJ*BETAX+ALPHAJ*
     .           (BETAXYJ*(XJ-XI)+BETAYYJ*(YJ-YI)+BETAZYJ*(ZJ-ZI)+BETAYJ))
              WGRD(3)=
     .         -WGRDZ*ALPHAJ*BETAX
     .         +WGHT*
     .          (ALPHAZJ*BETAX+ALPHAJ*
     .           (BETAXZJ*(XJ-XI)+BETAYZJ*(YJ-YI)+BETAZZJ*(ZJ-ZI)+BETAZJ))
C
              WLAPLT(N)=WLAPLT(N)+VJ*(
     .        -LAMBDR(SM)*(GRADTXJ*WGRD(1)+GRADTYJ*WGRD(2)+GRADTZJ*WGRD(3))
     .        +LAMBDA(N)*(GRADTXI*WGRAD(1)+GRADTYI*WGRAD(2)+GRADTZI*WGRAD(3)))
            END IF
          END DO
C------
 10     CONTINUE
C-----------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  SPGTSYM                       source/elements/sph/sptemp.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F
Chd|====================================================================
      SUBROUTINE SPGTSYM(
     1   ISPCOND, XFRAME,  ISPSYM,  XSPSYM,
     2   WGRADT,  WGRADTSM,WASPACT, WGR,
     3   LFT,     LLT,     NFT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(INOUT) :: LFT
        INTEGER, INTENT(INOUT) :: LLT
        INTEGER, INTENT(INOUT) :: NFT
        INTEGER ISPCOND(NISPCOND,*), ISPSYM(NSPCOND,*), WASPACT(*)
        my_real
     .     XFRAME(NXFRAME,*) ,XSPSYM(3,*) , WGRADT(3,*),
     .     WGRADTSM(3,*), WGR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IC,NC,IS,SM,JS,ISLIDE,SS
        my_real
     .         SX,SY,SZ,
     .         NX,NY,NZ,TX,TY,TZ,NN,
     .         OX,OY,OZ,UX,UY,UZ,VX,VY,VZ,WX,WY,WZ
C-----------------------------------------------
C       Prepare le gradient de temperature sur les particules symetriques.
C-----------------------------------------------
        DO NC=1,NSPCOND
          IC=ISPCOND(2,NC)
          IS=ISPCOND(3,NC)
          ISLIDE=ISPCOND(5,NC)
          IF (IC==1) THEN
            OX=XFRAME(10,IS)
            OY=XFRAME(11,IS)
            OZ=XFRAME(12,IS)
            UX=XFRAME(1,IS)
            UY=XFRAME(2,IS)
            UZ=XFRAME(3,IS)
          ELSEIF (IC==2) THEN
            OX=XFRAME(10,IS)
            OY=XFRAME(11,IS)
            OZ=XFRAME(12,IS)
            UX=XFRAME(4,IS)
            UY=XFRAME(5,IS)
            UZ=XFRAME(6,IS)
          ELSEIF (IC==3) THEN
            OX=XFRAME(10,IS)
            OY=XFRAME(11,IS)
            OZ=XFRAME(12,IS)
            UX=XFRAME(7,IS)
            UY=XFRAME(8,IS)
            UZ=XFRAME(9,IS)
          ENDIF
          DO SS=1,NSPHACT
            SM=WASPACT(SS)
            JS=ISPSYM(NC,SM)
            IF(JS>0)THEN
              SX=WGRADT(1,SM)
              SY=WGRADT(2,SM)
              SZ=WGRADT(3,SM)
C         IF(ISLIDE==0)THEN
C----------
              NN=SX*UX+SY*UY+SZ*UZ
              NX=NN*UX
              NY=NN*UY
              NZ=NN*UZ
              TX=SX-NX
              TY=SY-NY
              TZ=SZ-NZ
              WGRADTSM(1,JS)=TX-NX
              WGRADTSM(2,JS)=TY-NY
              WGRADTSM(3,JS)=TZ-NZ
C         ELSE
C         ENDIF
            ENDIF
          ENDDO
C
C Particules symetriques de particules remotes
C
          DO SS=1,NSPHR
            JS=ISPSYMR(NC,SS)
            IF(JS>0)THEN
              SX=WGR(1,SS)
              SY=WGR(2,SS)
              SZ=WGR(3,SS)
C         IF(ISLIDE==0)THEN
C----------
              NN=SX*UX+SY*UY+SZ*UZ
              NX=NN*UX
              NY=NN*UY
              NZ=NN*UZ
              TX=SX-NX
              TY=SY-NY
              TZ=SZ-NZ
              WGRADTSM(1,JS)=TX-NX
              WGRADTSM(2,JS)=TY-NY
              WGRADTSM(3,JS)=TZ-NZ
C         ELSE
C         ENDIF
            ENDIF
          ENDDO
C----------------------------------
        ENDDO
        RETURN
      END
Chd|====================================================================
Chd|  SPTEMPEL                      source/elements/sph/sptemp.F
Chd|-- called by -----------
Chd|        SPSTRES                       source/elements/sph/spstres.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPTEMPEL(
     1   KXSP,    TEMP,    TEMPEL,  LFT,
     2   LLT,     NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(INOUT) :: LFT
        INTEGER, INTENT(INOUT) :: LLT
        INTEGER, INTENT(INOUT) :: NFT
        INTEGER KXSP(NISP,*)
        my_real TEMP(*),TEMPEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,N,INOD
C-----------------------------------------------
C       Temperature in element is equivalent to Nodal temperature for SPH
C-----------------------------------------------
        DO I=LFT,LLT
          N = NFT+I
          IF(KXSP(2,N)>0)THEN
            INOD = KXSP(3,N)
            TEMPEL(I)=TEMP(INOD)
          ENDIF
        ENDDO
C
        RETURN
      END
