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
Chd|  SPCOMPL                       source/elements/sph/spcompl.F 
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        PINVERS                       source/elements/sph/pinvers.F 
Chd|        WEIGHT0                       source/elements/sph/weight.F  
Chd|        WEIGHT1                       source/elements/sph/weight.F  
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPCOMPL(
     1    X       ,V       ,MS      ,SPBUF   ,ITAB    , 
     2    KXSP    ,IXSP    ,NOD2SP  ,ISPSYM  ,XSPSYM  , 
     3    VSPSYM  ,IPARG   ,WACOMP  ,ISPCOND ,          
     4    XFRAME  ,WSMCOMP ,GEO     ,IPART   ,IPARTSP , 
     5    WASPACT ,ITASK   )
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPSYM(NSPCOND,*),IPARG(NPARG,*),ISPCOND(NISPCOND,*),
     .        IPART(LIPART1,*),IPARTSP(*),WASPACT(*),
     .        ITASK
C     REAL
      my_real
     .   X(3,*) ,V(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,
     .   XSPSYM(3,*)   ,VSPSYM(3,*) ,WACOMP(16,*),
     .   XFRAME(NXFRAME,*)  ,WSMCOMP(6,*),
     .   GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,INOD,JNOD,J,NVOIS,M,NN,
     .        NVOISS,SM,JS,NC,IC,IS,
     .        I,IPRT,IGEO,IORDER,NMUN,NZERO,NUN,
     .        MWAMUN(NSPHACT),MWAUN(NSPHACT),MWAZERO(NSPHACT)
      my_real
     .       XI,YI,ZI,DI,RHOI,XJ,YJ,ZJ,DJ,RHOJ,DIJ,
     .       VJ,VJX,VJY,VJZ,
     .       WGHT,WGRAD(3),
     .       WCOMPI,WCOMPXI,WCOMPYI,WCOMPZI,
     .       WGRADXI,WGRADYI,WGRADZI,
     .       WGRADXXI,WGRADXYI,WGRADXZI,
     .       WGRADYYI,WGRADYZI,
     .       WGRADZZI,
     . LI11,LI12,LI13,LI22,LI23,LI33,DET,
     . TCOFA11,TCOFA12,TCOFA13,TCOFA22,TCOFA23,TCOFA33,
     . L(3,3),IL(3,3),
     . LXI11,LXI12,LXI13,LXI22,LXI23,LXI33,
     . LYI11,LYI12,LYI13,LYI22,LYI23,LYI33,
     . LZI11,LZI12,LZI13,LZI22,LZI23,LZI33,
     . MXI11,MXI12,MXI13,MXI21,MXI22,MXI23,MXI31,MXI32,MXI33,
     . MYI11,MYI12,MYI13,MYI21,MYI22,MYI23,MYI31,MYI32,MYI33,
     . MZI11,MZI12,MZI13,MZI21,MZI22,MZI23,MZI31,MZI32,MZI33,
     . ALPHAI,ALPHAXI,ALPHAYI,ALPHAZI,ALPHAI2,
     . BETAXI,BETAYI,BETAZI,
     . BETAXXI,BETAYXI,BETAZXI,
     . BETAXYI,BETAYYI,BETAZYI,
     . BETAXZI,BETAYZI,BETAZZI,
     . BETAX,WGRDX,WGRDY,WGRDZ,
     .       OX,OY,OZ,UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,
     .       S11,S12,S13,S22,S23,S33,
     .       ALPHAXJ,ALPHAYJ,ALPHAZJ,
     .       BETAXJ,BETAYJ,BETAZJ
C-----------------------------------------------
      my_real
     .         GET_U_GEO
      EXTERNAL GET_U_GEO
C-----------------------------------------------
      NMUN =0
      NZERO=0
      NUN  =0
      DO I=ITASK+1,NSPHACT,NTHREAD
       N=WASPACT(I)
       IPRT  =IPARTSP(N)
       IGEO  =IPART(2,IPRT)
       IORDER=NINT(GET_U_GEO(5,IGEO))
       IF(IORDER==-1)THEN
        NMUN=NMUN+1
        MWAMUN(NMUN)=N
       ELSEIF(IORDER== 0)THEN
        NZERO=NZERO+1
        MWAZERO(NZERO)=N
       ELSEIF(IORDER== 1)THEN
        NUN=NUN+1
        MWAUN(NUN)=N
       ENDIF
      ENDDO
C-----------------------------------------------
C     No kernel correction.
C-----------------------------------------------
      DO I=1,NMUN
       N=MWAMUN(I)
       WACOMP(1,N)=ONE
       WACOMP(2,N)=ZERO
       WACOMP(3,N)=ZERO
       WACOMP(4,N)=ZERO
       WACOMP(5,N)=ZERO
       WACOMP(6,N)=ZERO
       WACOMP(7,N)=ZERO
       WACOMP( 8,N)=ZERO
       WACOMP( 9,N)=ZERO
       WACOMP(10,N)=ZERO
       WACOMP(11,N)=ZERO
       WACOMP(12,N)=ZERO
       WACOMP(13,N)=ZERO
       WACOMP(14,N)=ZERO
       WACOMP(15,N)=ZERO
       WACOMP(16,N)=ZERO      
      ENDDO
C-----------------------------------------------
C     Kernel correction up to order 0.
C-----------------------------------------------
      DO I=1,NZERO
       N=MWAZERO(I)
       INOD =KXSP(3,N)
       XI=X(1,INOD)
       YI=X(2,INOD)
       ZI=X(3,INOD)
       DI  =SPBUF(1,N)
       RHOI=SPBUF(2,N)
C------
       CALL WEIGHT0(XI,YI,ZI,XI,YI,ZI,DI,WGHT)
       WCOMPI =SPBUF(12,N)*WGHT/MAX(EM20,RHOI)
       WGRADXI=ZERO
       WGRADYI=ZERO
       WGRADZI=ZERO    
C------
       NVOIS=KXSP(4,N)
       DO J=1,NVOIS
         JNOD=IXSP(J,N)
         IF(JNOD>0)THEN
           M=NOD2SP(JNOD)
           XJ=X(1,JNOD)
           YJ=X(2,JNOD)
           ZJ=X(3,JNOD)
           DJ  =SPBUF(1,M)
           RHOJ=SPBUF(2,M)
           VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
         ELSE
           NN = -JNOD
           XJ=XSPHR(3,NN)
           YJ=XSPHR(4,NN)
           ZJ=XSPHR(5,NN)
           DJ  =XSPHR(2,NN)
           RHOJ=XSPHR(7,NN)
           VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
         END IF
         DIJ=0.5*(DI+DJ)
         CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
         WCOMPI=WCOMPI+VJ*WGHT
         WGRADXI=WGRADXI+VJ*WGRAD(1)
         WGRADYI=WGRADYI+VJ*WGRAD(2)
         WGRADZI=WGRADZI+VJ*WGRAD(3)
       ENDDO
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
           RHOJ=SPBUF(2,SM)
           DIJ =HALF*(DI+DJ)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
           JNOD=KXSP(3,SM)
           VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)
         ELSE
           SM=-JS/(NSPCOND+1)
           NC=MOD(-JS,NSPCOND+1)
           JS=ISPSYMR(NC,SM)
           XJ =XSPSYM(1,JS)
           YJ =XSPSYM(2,JS)
           ZJ =XSPSYM(3,JS)
           DJ =XSPHR(2,SM)
           RHOJ=XSPHR(7,SM)
           DIJ =HALF*(DI+DJ)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
           VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)
         END IF
         WCOMPI=WCOMPI+VJ*WGHT
         WGRADXI=WGRADXI+VJ*WGRAD(1)
         WGRADYI=WGRADYI+VJ*WGRAD(2)
         WGRADZI=WGRADZI+VJ*WGRAD(3)
       ENDDO
C------
C      AlphaI
       ALPHAI=ONE/MAX(EM20,WCOMPI)
       WACOMP(1,N)=ALPHAI
C------
       ALPHAI2=ALPHAI*ALPHAI
       ALPHAXI=-WGRADXI*ALPHAI2
       ALPHAYI=-WGRADYI*ALPHAI2
       ALPHAZI=-WGRADZI*ALPHAI2
       WACOMP(5,N)=ALPHAXI
       WACOMP(6,N)=ALPHAYI
       WACOMP(7,N)=ALPHAZI
C------
       WACOMP(2,N)=ZERO
       WACOMP(3,N)=ZERO
       WACOMP(4,N)=ZERO
       WACOMP( 8,N)=ZERO
       WACOMP( 9,N)=ZERO
       WACOMP(10,N)=ZERO
       WACOMP(11,N)=ZERO
       WACOMP(12,N)=ZERO
       WACOMP(13,N)=ZERO
       WACOMP(14,N)=ZERO
       WACOMP(15,N)=ZERO
       WACOMP(16,N)=ZERO    
C------
      ENDDO
C------
C-----------------------------------------------
C     Kernel correction up to order 1
C     NON COMPATIBLE SPMD SUR PLUS D ONE DOMAINE
C-----------------------------------------------
      DO I=1,NUN
       N=MWAUN(I)
       INOD =KXSP(3,N)
       XI=X(1,INOD)
       YI=X(2,INOD)
       ZI=X(3,INOD)
       DI  =SPBUF(1,N)
       RHOI=SPBUF(2,N)
C------
       CALL WEIGHT0(XI,YI,ZI,XI,YI,ZI,DI,WGHT)
       WCOMPI =SPBUF(12,N)*WGHT/MAX(EM20,RHOI)
       WCOMPXI=ZERO
       WCOMPYI=ZERO
       WCOMPZI=ZERO
       WGRADXI=ZERO
       WGRADYI=ZERO
       WGRADZI=ZERO
       WGRADXXI=ZERO
       WGRADXYI=ZERO
       WGRADXZI=ZERO
       WGRADYYI=ZERO
       WGRADYZI=ZERO
       WGRADZZI=ZERO       
C      WGRADYXI=0.
C      WGRADZXI=0.
C      WGRADZYI=0.
C------
       NVOIS=KXSP(4,N)
       DO J=1,NVOIS
         JNOD=IXSP(J,N)
         IF(JNOD>0)THEN
           M=NOD2SP(JNOD)
           XJ=X(1,JNOD)
           YJ=X(2,JNOD)
           ZJ=X(3,JNOD)
           DJ  =SPBUF(1,M)
           RHOJ=SPBUF(2,M)
           VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
         ELSE
           NN = -JNOD
           XJ=XSPHR(3,NN)
           YJ=XSPHR(4,NN)
           ZJ=XSPHR(5,NN)
           DJ  =XSPHR(2,NN)
           RHOJ=XSPHR(7,NN)
           VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
         END IF
         DIJ=HALF*(DI+DJ)
         CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
         WCOMPI=WCOMPI+VJ*WGHT
         WCOMPXI=WCOMPXI+VJ*WGHT*(XI-XJ)
         WCOMPYI=WCOMPYI+VJ*WGHT*(YI-YJ)
         WCOMPZI=WCOMPZI+VJ*WGHT*(ZI-ZJ)
         WGRADXI=WGRADXI+VJ*WGRAD(1)
         WGRADYI=WGRADYI+VJ*WGRAD(2)
         WGRADZI=WGRADZI+VJ*WGRAD(3)
         WGRADXXI=WGRADXXI+VJ*WGRAD(1)*(XI-XJ)
         WGRADXYI=WGRADXYI+VJ*WGRAD(1)*(YI-YJ)
         WGRADXZI=WGRADXZI+VJ*WGRAD(1)*(ZI-ZJ)
         WGRADYYI=WGRADYYI+VJ*WGRAD(2)*(YI-YJ)
         WGRADYZI=WGRADYZI+VJ*WGRAD(2)*(ZI-ZJ)
         WGRADZZI=WGRADZZI+VJ*WGRAD(3)*(ZI-ZJ)
C        WGRADYXI=WGRADYXI+VJ*WGRAD(2)*(XI-XJ)
C        WGRADZXI=WGRADZXI+VJ*WGRAD(3)*(XI-XJ)
C        WGRADZYI=WGRADZYI+VJ*WGRAD(3)*(YI-YJ)
       ENDDO
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
           RHOJ=SPBUF(2,SM)
           DIJ =HALF*(DI+DJ)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
           JNOD=KXSP(3,SM)
           VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)
         ELSE
           SM=-JS/(NSPCOND+1)
           NC=MOD(-JS,NSPCOND+1)
           JS=ISPSYMR(NC,SM)
           XJ =XSPSYM(1,JS)
           YJ =XSPSYM(2,JS)
           ZJ =XSPSYM(3,JS)
           DJ  =XSPHR(2,SM)
           RHOJ=XSPHR(7,SM)
           DIJ =HALF*(DI+DJ)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
           VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)
         END IF
         WCOMPI=WCOMPI+VJ*WGHT
         WCOMPXI=WCOMPXI+VJ*WGHT*(XI-XJ)
         WCOMPYI=WCOMPYI+VJ*WGHT*(YI-YJ)
         WCOMPZI=WCOMPZI+VJ*WGHT*(ZI-ZJ)
         WGRADXI=WGRADXI+VJ*WGRAD(1)
         WGRADYI=WGRADYI+VJ*WGRAD(2)
         WGRADZI=WGRADZI+VJ*WGRAD(3)
         WGRADXXI=WGRADXXI+VJ*WGRAD(1)*(XI-XJ)
         WGRADXYI=WGRADXYI+VJ*WGRAD(1)*(YI-YJ)
         WGRADXZI=WGRADXZI+VJ*WGRAD(1)*(ZI-ZJ)
         WGRADYYI=WGRADYYI+VJ*WGRAD(2)*(YI-YJ)
         WGRADYZI=WGRADYZI+VJ*WGRAD(2)*(ZI-ZJ)
         WGRADZZI=WGRADZZI+VJ*WGRAD(3)*(ZI-ZJ)
C        WGRADYXI=WGRADYXI+VJ*WGRAD(2)*(XI-XJ)
C        WGRADZXI=WGRADZXI+VJ*WGRAD(3)*(XI-XJ)
C        WGRADZYI=WGRADZYI+VJ*WGRAD(3)*(YI-YJ)
       ENDDO
C------
       LI11=ZERO
       LI12=ZERO
       LI13=ZERO
       LI22=ZERO
       LI23=ZERO
       LI33=ZERO       
C      LI21=0.
C      LI31=0.
C      LI32=0.
C------
       LXI11=ZERO
       LXI12=ZERO
       LXI13=ZERO
       LXI22=ZERO
       LXI23=ZERO
       LXI33=ZERO       
C      LXI21=0.
C      LXI31=0.
C      LXI32=0.
C------
       LYI11=ZERO
       LYI12=ZERO
       LYI13=ZERO
       LYI22=ZERO
       LYI23=ZERO
       LYI33=ZERO       
C      LYI21=0.
C      LYI31=0.
C      LYI32=0.
C------
       LZI11=ZERO
       LZI12=ZERO
       LZI13=ZERO
       LZI22=ZERO
       LZI23=ZERO
       LZI33=ZERO      
C      LZI21=0.
C      LZI31=0.
C      LZI32=0.
C------
       NVOIS=KXSP(4,N)
       DO J=1,NVOIS
         JNOD=IXSP(J,N)
         IF(JNOD>0)THEN
           M=NOD2SP(JNOD)
           XJ=X(1,JNOD)
           YJ=X(2,JNOD)
           ZJ=X(3,JNOD)
           DJ  =SPBUF(1,M)
           RHOJ=SPBUF(2,M)
           DIJ=HALF*(DI+DJ)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
C------
           VJ=SPBUF(12,M)/MAX(EM20,RHOJ)*WGHT
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
C------
           LI11=LI11+VJX*(XI-XJ)
           LI12=LI12+VJX*(YI-YJ)
           LI13=LI13+VJX*(ZI-ZJ)
           LI22=LI22+VJY*(YI-YJ)
           LI23=LI23+VJY*(ZI-ZJ)
           LI33=LI33+VJZ*(ZI-ZJ)
C          LI21=LI21+VJY*(XI-XJ)
C          LI31=LI31+VJZ*(XI-XJ)
C          LI32=LI32+VJZ*(YI-YJ)
C------
           LXI11=LXI11+VJX
           LXI12=LXI12+VJY
           LXI13=LXI13+VJZ
           LXI11=LXI11+VJX
C          LXI21=LXI21+VJY
C          LXI31=LXI31+VJZ
C------
C          LYI21=LYI21+VJX
           LYI22=LYI22+VJY
           LYI23=LYI23+VJZ
           LYI12=LYI12+VJX
           LYI22=LYI22+VJY
C          LYI32=LYI32+VJZ
C------
C          LZI31=LZI31+VJX
C          LZI32=LZI32+VJY
           LZI33=LZI33+VJZ
           LZI13=LZI13+VJX
           LZI23=LZI23+VJY
           LZI33=LZI33+VJZ
C------
           VJ=SPBUF(12,M)/MAX(EM20,RHOJ)*WGRAD(1)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LXI11=LXI11+VJX*(XI-XJ)
           LXI12=LXI12+VJX*(YI-YJ)
           LXI13=LXI13+VJX*(ZI-ZJ)
C          LXI21=LXI21+VJY*(XI-XJ)
           LXI22=LXI22+VJY*(YI-YJ)
           LXI23=LXI23+VJY*(ZI-ZJ)
C          LXI31=LXI31+VJZ*(XI-XJ)
C          LXI32=LXI32+VJZ*(YI-YJ)
           LXI33=LXI33+VJZ*(ZI-ZJ)
C------    
           VJ=SPBUF(12,M)/MAX(EM20,RHOJ)*WGRAD(2)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LYI11=LYI11+VJX*(XI-XJ)
           LYI12=LYI12+VJX*(YI-YJ)
           LYI13=LYI13+VJX*(ZI-ZJ)
C          LYI21=LYI21+VJY*(XI-XJ)
           LYI22=LYI22+VJY*(YI-YJ)
           LYI23=LYI23+VJY*(ZI-ZJ)
C          LYI31=LYI31+VJZ*(XI-XJ)
C          LYI32=LYI32+VJZ*(YI-YJ)
           LYI33=LYI33+VJZ*(ZI-ZJ)
C------   
           VJ=SPBUF(12,M)/MAX(EM20,RHOJ)*WGRAD(3)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LZI11=LZI11+VJX*(XI-XJ)
           LZI12=LZI12+VJX*(YI-YJ)
           LZI13=LZI13+VJX*(ZI-ZJ)
C          LZI21=LZI21+VJY*(XI-XJ)
           LZI22=LZI22+VJY*(YI-YJ)
           LZI23=LZI23+VJY*(ZI-ZJ)
C          LZI31=LZI31+VJZ*(XI-XJ)
C          LZI32=LZI32+VJZ*(YI-YJ)
           LZI33=LZI33+VJZ*(ZI-ZJ)
        ELSE
           NN = -JNOD
           XJ=XSPHR(3,NN)
           YJ=XSPHR(4,NN)
           ZJ=XSPHR(5,NN)
           DJ  =XSPHR(2,NN)
           RHOJ=XSPHR(7,NN)
           DIJ=HALF*(DI+DJ)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
           VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)*WGHT
C------
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
C------
           LI11=LI11+VJX*(XI-XJ)
           LI12=LI12+VJX*(YI-YJ)
           LI13=LI13+VJX*(ZI-ZJ)
           LI22=LI22+VJY*(YI-YJ)
           LI23=LI23+VJY*(ZI-ZJ)
           LI33=LI33+VJZ*(ZI-ZJ)
C          LI21=LI21+VJY*(XI-XJ)
C          LI31=LI31+VJZ*(XI-XJ)
C          LI32=LI32+VJZ*(YI-YJ)
C------
           LXI11=LXI11+VJX
           LXI12=LXI12+VJY
           LXI13=LXI13+VJZ
           LXI11=LXI11+VJX
C          LXI21=LXI21+VJY
C          LXI31=LXI31+VJZ
C------
C          LYI21=LYI21+VJX
           LYI22=LYI22+VJY
           LYI23=LYI23+VJZ
           LYI12=LYI12+VJX
           LYI22=LYI22+VJY
C          LYI32=LYI32+VJZ
C------
C          LZI31=LZI31+VJX
C          LZI32=LZI32+VJY
           LZI33=LZI33+VJZ
           LZI13=LZI13+VJX
           LZI23=LZI23+VJY
           LZI33=LZI33+VJZ
C------

           VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)*WGRAD(1)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LXI11=LXI11+VJX*(XI-XJ)
           LXI12=LXI12+VJX*(YI-YJ)
           LXI13=LXI13+VJX*(ZI-ZJ)
C          LXI21=LXI21+VJY*(XI-XJ)
           LXI22=LXI22+VJY*(YI-YJ)
           LXI23=LXI23+VJY*(ZI-ZJ)
C          LXI31=LXI31+VJZ*(XI-XJ)
C          LXI32=LXI32+VJZ*(YI-YJ)
           LXI33=LXI33+VJZ*(ZI-ZJ)
C------
           VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)*WGRAD(2)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LYI11=LYI11+VJX*(XI-XJ)
           LYI12=LYI12+VJX*(YI-YJ)
           LYI13=LYI13+VJX*(ZI-ZJ)
C          LYI21=LYI21+VJY*(XI-XJ)
           LYI22=LYI22+VJY*(YI-YJ)
           LYI23=LYI23+VJY*(ZI-ZJ)
C          LYI31=LYI31+VJZ*(XI-XJ)
C          LYI32=LYI32+VJZ*(YI-YJ)
           LYI33=LYI33+VJZ*(ZI-ZJ)
C------
           VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)*WGRAD(3)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LZI11=LZI11+VJX*(XI-XJ)
           LZI12=LZI12+VJX*(YI-YJ)
           LZI13=LZI13+VJX*(ZI-ZJ)
C          LZI21=LZI21+VJY*(XI-XJ)
           LZI22=LZI22+VJY*(YI-YJ)
           LZI23=LZI23+VJY*(ZI-ZJ)
C          LZI31=LZI31+VJZ*(XI-XJ)
C          LZI32=LZI32+VJZ*(YI-YJ)
           LZI33=LZI33+VJZ*(ZI-ZJ)
         END IF
       ENDDO
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
           RHOJ=SPBUF(2,SM)
           DIJ =HALF*(DI+DJ)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
           JNOD=KXSP(3,SM)
C------
           VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)*WGHT
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
C------
           LI11=LI11+VJX*(XI-XJ)
           LI12=LI12+VJX*(YI-YJ)
           LI13=LI13+VJX*(ZI-ZJ)
           LI22=LI22+VJY*(YI-YJ)
           LI23=LI23+VJY*(ZI-ZJ)
           LI33=LI33+VJZ*(ZI-ZJ)
C          LI21=LI21+VJY*(XI-XJ)
C          LI31=LI31+VJZ*(XI-XJ)
C          LI32=LI32+VJZ*(YI-YJ)
C------
           LXI11=LXI11+VJX
           LXI12=LXI12+VJY
           LXI13=LXI13+VJZ
           LXI11=LXI11+VJX
C          LXI21=LXI21+VJY
C          LXI31=LXI31+VJZ
C------
C          LYI21=LYI21+VJX
           LYI22=LYI22+VJY
           LYI23=LYI23+VJZ
           LYI12=LYI12+VJX
           LYI22=LYI22+VJY
C          LYI32=LYI32+VJZ
C------
C          LZI31=LZI31+VJX
C          LZI32=LZI32+VJY
           LZI33=LZI33+VJZ
           LZI13=LZI13+VJX
           LZI23=LZI23+VJY
           LZI33=LZI33+VJZ
C------   
           VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)*WGRAD(1)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LXI11=LXI11+VJX*(XI-XJ)
           LXI12=LXI12+VJX*(YI-YJ)
           LXI13=LXI13+VJX*(ZI-ZJ)
C          LXI21=LXI21+VJY*(XI-XJ)
           LXI22=LXI22+VJY*(YI-YJ)
           LXI23=LXI23+VJY*(ZI-ZJ)
C          LXI31=LXI31+VJZ*(XI-XJ)
C          LXI32=LXI32+VJZ*(YI-YJ)
           LXI33=LXI33+VJZ*(ZI-ZJ)
C------
           VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)*WGRAD(2)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LYI11=LYI11+VJX*(XI-XJ)
           LYI12=LYI12+VJX*(YI-YJ)
           LYI13=LYI13+VJX*(ZI-ZJ)
C          LYI21=LYI21+VJY*(XI-XJ)
           LYI22=LYI22+VJY*(YI-YJ)
           LYI23=LYI23+VJY*(ZI-ZJ)
C          LYI31=LYI31+VJZ*(XI-XJ)
C          LYI32=LYI32+VJZ*(YI-YJ)
           LYI33=LYI33+VJZ*(ZI-ZJ)
C------    
           VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)*WGRAD(3)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LZI11=LZI11+VJX*(XI-XJ)
           LZI12=LZI12+VJX*(YI-YJ)
           LZI13=LZI13+VJX*(ZI-ZJ)
C          LZI21=LZI21+VJY*(XI-XJ)
           LZI22=LZI22+VJY*(YI-YJ)
           LZI23=LZI23+VJY*(ZI-ZJ)
C          LZI31=LZI31+VJZ*(XI-XJ)
C          LZI32=LZI32+VJZ*(YI-YJ)
           LZI33=LZI33+VJZ*(ZI-ZJ)
         ELSE
           SM=-JS/(NSPCOND+1)
           NC=MOD(-JS,NSPCOND+1)
           JS=ISPSYMR(NC,SM)
           XJ =XSPSYM(1,JS)
           YJ =XSPSYM(2,JS)
           ZJ =XSPSYM(3,JS)
           DJ  =XSPHR(2,SM)
           RHOJ=XSPHR(7,SM)
           DIJ =HALF*(DI+DJ)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
C------
           VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)*WGHT
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
C------
           LI11=LI11+VJX*(XI-XJ)
           LI12=LI12+VJX*(YI-YJ)
           LI13=LI13+VJX*(ZI-ZJ)
           LI22=LI22+VJY*(YI-YJ)
           LI23=LI23+VJY*(ZI-ZJ)
           LI33=LI33+VJZ*(ZI-ZJ)
C          LI21=LI21+VJY*(XI-XJ)
C          LI31=LI31+VJZ*(XI-XJ)
C          LI32=LI32+VJZ*(YI-YJ)
C------
           LXI11=LXI11+VJX
           LXI12=LXI12+VJY
           LXI13=LXI13+VJZ
           LXI11=LXI11+VJX
C          LXI21=LXI21+VJY
C          LXI31=LXI31+VJZ
C------
C          LYI21=LYI21+VJX
           LYI22=LYI22+VJY
           LYI23=LYI23+VJZ
           LYI12=LYI12+VJX
           LYI22=LYI22+VJY
C          LYI32=LYI32+VJZ
C------
C          LZI31=LZI31+VJX
C          LZI32=LZI32+VJY
           LZI33=LZI33+VJZ
           LZI13=LZI13+VJX
           LZI23=LZI23+VJY
           LZI33=LZI33+VJZ
C------   
           VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)*WGRAD(1)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LXI11=LXI11+VJX*(XI-XJ)
           LXI12=LXI12+VJX*(YI-YJ)
           LXI13=LXI13+VJX*(ZI-ZJ)
C          LXI21=LXI21+VJY*(XI-XJ)
           LXI22=LXI22+VJY*(YI-YJ)
           LXI23=LXI23+VJY*(ZI-ZJ)
C          LXI31=LXI31+VJZ*(XI-XJ)
C          LXI32=LXI32+VJZ*(YI-YJ)
           LXI33=LXI33+VJZ*(ZI-ZJ)
C------
           VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)*WGRAD(2)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LYI11=LYI11+VJX*(XI-XJ)
           LYI12=LYI12+VJX*(YI-YJ)
           LYI13=LYI13+VJX*(ZI-ZJ)
C          LYI21=LYI21+VJY*(XI-XJ)
           LYI22=LYI22+VJY*(YI-YJ)
           LYI23=LYI23+VJY*(ZI-ZJ)
C          LYI31=LYI31+VJZ*(XI-XJ)
C          LYI32=LYI32+VJZ*(YI-YJ)
           LYI33=LYI33+VJZ*(ZI-ZJ)
C------    
           VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)*WGRAD(3)
           VJX=VJ*(XI-XJ)
           VJY=VJ*(YI-YJ)
           VJZ=VJ*(ZI-ZJ)
           LZI11=LZI11+VJX*(XI-XJ)
           LZI12=LZI12+VJX*(YI-YJ)
           LZI13=LZI13+VJX*(ZI-ZJ)
C          LZI21=LZI21+VJY*(XI-XJ)
           LZI22=LZI22+VJY*(YI-YJ)
           LZI23=LZI23+VJY*(ZI-ZJ)
C          LZI31=LZI31+VJZ*(XI-XJ)
C          LZI32=LZI32+VJZ*(YI-YJ)
           LZI33=LZI33+VJZ*(ZI-ZJ)
         END IF
       ENDDO
C------
C     Inverse LI.
C------
C       Cofacteurs.
C       COFAC11=  LI22*LI33-LI32*LI23
C       COFAC12= -LI21*LI33+LI31*LI23
C       COFAC13=  LI21*LI32-LI31*LI22
C       COFAC21= -LI12*LI33+LI32*LI13
C       COFAC22=  LI11*LI33-LI31*LI13
C       COFAC23= -LI11*LI32+LI31*LI12
C       COFAC31=  LI12*LI23-LI22*LI13
C       COFAC32= -LI11*LI23+LI21*LI13
C       COFAC33=  LI11*LI22-LI21*LI12
C      Transposee des cofacteurs.
C       TCOFA11=  LI22*LI33-LI32*LI23
C       TCOFA21= -LI21*LI33+LI31*LI23
C       TCOFA31=  LI21*LI32-LI31*LI22
C       TCOFA12= -LI12*LI33+LI32*LI13
C       TCOFA22=  LI11*LI33-LI31*LI13
C       TCOFA32= -LI11*LI32+LI31*LI12
C       TCOFA13=  LI12*LI23-LI22*LI13
C       TCOFA23= -LI11*LI23+LI21*LI13
C       TCOFA33=  LI11*LI22-LI21*LI12
C      Determinant
       TCOFA11=  LI22*LI33-LI23*LI23
       TCOFA12= -LI12*LI33+LI23*LI13
       TCOFA13=  LI12*LI23-LI22*LI13
       DET=LI11*TCOFA11+LI12*TCOFA12+LI13*TCOFA13
C------
       IF(ABS(DET)<=EM20)THEN
C        CALL MY_LOCK
C         WRITE(ISTDO,*)' ** SPH NULL DETERMINANT.'
C         WRITE(IOUT,*) ' ** SPH NULL DETERMINANT, ID CELL=',KXSP(NISP,N)
C        CALL MY_FREE
        L(1,1)=LI11
        L(1,2)=LI12
        L(1,3)=LI13
        L(2,2)=LI22
        L(2,3)=LI23
        L(3,3)=LI33
        L(2,1)=LI12
        L(3,1)=LI13
        L(3,2)=LI23
C       L(2,1)=LI21
C       L(3,1)=LI31
C       L(3,2)=LI32
        CALL PINVERS(L,IL)
        LI11=IL(1,1)
        LI12=IL(1,2)
        LI13=IL(1,3)
        LI22=IL(2,2)
        LI23=IL(2,3)
        LI33=IL(3,3)
C       LI21=IL(2,1)
C       LI31=IL(3,1)
C       LI32=IL(3,2)
       ELSE
        DET=1./DET
        TCOFA22=  LI11*LI33-LI13*LI13
        TCOFA23= -LI11*LI23+LI12*LI13
        TCOFA33=  LI11*LI22-LI12*LI12
C       Inverse LI
        LI11=  TCOFA11*DET
        LI12=  TCOFA12*DET
        LI13=  TCOFA13*DET
        LI22=  TCOFA22*DET
        LI23=  TCOFA23*DET
        LI33=  TCOFA33*DET
C       LI21=  TCOFA21*DET
C       LI31=  TCOFA31*DET
C       LI32=  TCOFA32*DET
       ENDIF
C------
C      BetaI(3)
       BETAXI=-(LI11*WCOMPXI+LI12*WCOMPYI+LI13*WCOMPZI)
       BETAYI=-(LI12*WCOMPXI+LI22*WCOMPYI+LI23*WCOMPZI)
       BETAZI=-(LI13*WCOMPXI+LI23*WCOMPYI+LI33*WCOMPZI)
C      BETAYI=-(LI21*WCOMPXI+LI22*WCOMPYI+LI23*WCOMPZI)
C      BETAZI=-(LI31*WCOMPXI+LI32*WCOMPYI+LI33*WCOMPZI)
       WACOMP(2,N)=BETAXI
       WACOMP(3,N)=BETAYI
       WACOMP(4,N)=BETAZI
C------
C      AlphaI
       ALPHAI=WCOMPI+BETAXI*WCOMPXI+BETAYI*WCOMPYI+BETAZI*WCOMPZI
       ALPHAI=ONE/MAX(EM20,ALPHAI)
       WACOMP(1,N)=ALPHAI
C------
C      DBetaI(3)/dx (MXI=Inverse(LI)*LXI)
C      MXI11=LI11*LXI11+LI12*LXI21+LI13*LXI31
C      MXI12=LI11*LXI12+LI12*LXI22+LI13*LXI32
C      MXI13=LI11*LXI13+LI12*LXI23+LI13*LXI33
C      MXI21=LI21*LXI11+LI22*LXI21+LI23*LXI31
C      MXI22=LI21*LXI12+LI22*LXI22+LI23*LXI32
C      MXI23=LI21*LXI13+LI22*LXI23+LI23*LXI33
C      MXI31=LI31*LXI11+LI32*LXI21+LI33*LXI31
C      MXI32=LI31*LXI12+LI32*LXI22+LI33*LXI32
C      MXI33=LI31*LXI13+LI32*LXI23+LI33*LXI33
       MXI11=LI11*LXI11+LI12*LXI12+LI13*LXI13
       MXI12=LI11*LXI12+LI12*LXI22+LI13*LXI23
       MXI13=LI11*LXI13+LI12*LXI23+LI13*LXI33
       MXI21=LI12*LXI11+LI22*LXI12+LI23*LXI13
       MXI22=LI12*LXI12+LI22*LXI22+LI23*LXI23
       MXI23=LI12*LXI13+LI22*LXI23+LI23*LXI33
       MXI31=LI13*LXI11+LI23*LXI12+LI33*LXI13
       MXI32=LI13*LXI12+LI23*LXI22+LI33*LXI23
       MXI33=LI13*LXI13+LI23*LXI23+LI33*LXI33
       BETAXXI=-(MXI11*BETAXI+MXI12*BETAYI+MXI13*BETAZI)
     .         -(LI11*(WCOMPI+WGRADXXI)+LI12*WGRADXYI+LI13*WGRADXZI)
       BETAYXI=-(MXI21*BETAXI+MXI22*BETAYI+MXI23*BETAZI)
     .         -(LI12*(WCOMPI+WGRADXXI)+LI22*WGRADXYI+LI23*WGRADXZI)
C    .         -(LI21*(WCOMPI+WGRADXXI)+LI22*WGRADXYI+LI23*WGRADXZI)
       BETAZXI=-(MXI31*BETAXI+MXI32*BETAYI+MXI33*BETAZI)
     .         -(LI13*(WCOMPI+WGRADXXI)+LI23*WGRADXYI+LI33*WGRADXZI)
C    .         -(LI31*(WCOMPI+WGRADXXI)+LI32*WGRADXYI+LI33*WGRADXZI)
C------
C      DBetaI(3)/dy (MYI=Inverse(LI)*LYI)
C      MYI11=LI11*LYI11+LI12*LYI21+LI13*LYI31
C      MYI12=LI11*LYI12+LI12*LYI22+LI13*LYI32
C      MYI13=LI11*LYI13+LI12*LYI23+LI13*LYI33
C      MYI21=LI21*LYI11+LI22*LYI21+LI23*LYI31
C      MYI22=LI21*LYI12+LI22*LYI22+LI23*LYI32
C      MYI23=LI21*LYI13+LI22*LYI23+LI23*LYI33
C      MYI31=LI31*LYI11+LI32*LYI21+LI33*LYI31
C      MYI32=LI31*LYI12+LI32*LYI22+LI33*LYI32
C      MYI33=LI31*LYI13+LI32*LYI23+LI33*LYI33
       MYI11=LI11*LYI11+LI12*LYI12+LI13*LYI13
       MYI12=LI11*LYI12+LI12*LYI22+LI13*LYI23
       MYI13=LI11*LYI13+LI12*LYI23+LI13*LYI33
       MYI21=LI12*LYI11+LI22*LYI12+LI23*LYI13
       MYI22=LI12*LYI12+LI22*LYI22+LI23*LYI23
       MYI23=LI12*LYI13+LI22*LYI23+LI23*LYI33
       MYI31=LI13*LYI11+LI23*LYI12+LI33*LYI13
       MYI32=LI13*LYI12+LI23*LYI22+LI33*LYI23
       MYI33=LI13*LYI13+LI23*LYI23+LI33*LYI33
       BETAXYI=-(MYI11*BETAXI+MYI12*BETAYI+MYI13*BETAZI)
     .         -(LI11*WGRADXYI+LI12*(WCOMPI+WGRADYYI)+LI13*WGRADYZI)
C     .        -(LI11*WGRADYXI+LI12*(WCOMPI+WGRADYYI)+LI13*WGRADYZI)
       BETAYYI=-(MYI21*BETAXI+MYI22*BETAYI+MYI23*BETAZI)
     .         -(LI12*WGRADXYI+LI22*(WCOMPI+WGRADYYI)+LI23*WGRADYZI)
C    .         -(LI21*WGRADYXI+LI22*(WCOMPI+WGRADYYI)+LI23*WGRADYZI)
       BETAZYI=-(MYI31*BETAXI+MYI32*BETAYI+MYI33*BETAZI)
     .         -(LI13*WGRADXYI+LI23*(WCOMPI+WGRADYYI)+LI33*WGRADYZI)
C    .         -(LI31*WGRADYXI+LI32*(WCOMPI+WGRADYYI)+LI33*WGRADYZI)
C------
C      DBetaI(3)/dz (MZI=Inverse(LI)*LZI)
C      MZI11=LI11*LZI11+LI12*LZI21+LI13*LZI31
C      MZI12=LI11*LZI12+LI12*LZI22+LI13*LZI32
C      MZI13=LI11*LZI13+LI12*LZI23+LI13*LZI33
C      MZI21=LI21*LZI11+LI22*LZI21+LI23*LZI31
C      MZI22=LI21*LZI12+LI22*LZI22+LI23*LZI32
C      MZI23=LI21*LZI13+LI22*LZI23+LI23*LZI33
C      MZI31=LI31*LZI11+LI32*LZI21+LI33*LZI31
C      MZI32=LI31*LZI12+LI32*LZI22+LI33*LZI32
C      MZI33=LI31*LZI13+LI32*LZI23+LI33*LZI33
       MZI11=LI11*LZI11+LI12*LZI12+LI13*LZI13
       MZI12=LI11*LZI12+LI12*LZI22+LI13*LZI23
       MZI13=LI11*LZI13+LI12*LZI23+LI13*LZI33
       MZI21=LI12*LZI11+LI22*LZI12+LI23*LZI13
       MZI22=LI12*LZI12+LI22*LZI22+LI23*LZI23
       MZI23=LI12*LZI13+LI22*LZI23+LI23*LZI33
       MZI31=LI13*LZI11+LI23*LZI12+LI33*LZI13
       MZI32=LI13*LZI12+LI23*LZI22+LI33*LZI23
       MZI33=LI13*LZI13+LI23*LZI23+LI33*LZI33
       BETAXZI=-(MZI11*BETAXI+MZI12*BETAYI+MZI13*BETAZI)
     .         -(LI11*WGRADXZI+LI12*WGRADYZI+LI13*(WCOMPI+WGRADZZI))
C    .         -(LI11*WGRADZXI+LI12*WGRADZYI+LI13*(WCOMPI+WGRADZZI))
       BETAYZI=-(MZI21*BETAXI+MZI22*BETAYI+MZI23*BETAZI)
     .         -(LI12*WGRADXZI+LI22*WGRADYZI+LI23*(WCOMPI+WGRADZZI))
C    .         -(LI21*WGRADZXI+LI22*WGRADZYI+LI23*(WCOMPI+WGRADZZI))
       BETAZZI=-(MZI31*BETAXI+MZI32*BETAYI+MZI33*BETAZI)
     .         -(LI13*WGRADXZI+LI23*WGRADYZI+LI33*(WCOMPI+WGRADZZI))
C    .         -(LI31*WGRADZXI+LI32*WGRADZYI+LI33*(WCOMPI+WGRADZZI))
C------
       WACOMP( 8,N)=BETAXXI
       WACOMP( 9,N)=BETAYXI
       WACOMP(10,N)=BETAZXI
       WACOMP(11,N)=BETAXYI
       WACOMP(12,N)=BETAYYI
       WACOMP(13,N)=BETAZYI
       WACOMP(14,N)=BETAXZI
       WACOMP(15,N)=BETAYZI
       WACOMP(16,N)=BETAZZI
C------
       ALPHAI2=ALPHAI*ALPHAI
       ALPHAXI= WGRADXI
     .         +BETAXI*WGRADXXI+BETAYI*WGRADXYI+BETAZI*WGRADXZI
     .         +BETAXXI*WCOMPXI+BETAYXI*WCOMPYI+BETAZXI*WCOMPZI
     .         +BETAXI*WCOMPI
       ALPHAXI=-ALPHAXI*ALPHAI2
       ALPHAYI= WGRADYI
C    .         +BETAXI*WGRADYXI+BETAYI*WGRADYYI+BETAZI*WGRADYZI
     .         +BETAXI*WGRADXYI+BETAYI*WGRADYYI+BETAZI*WGRADYZI
     .         +BETAXYI*WCOMPXI+BETAYYI*WCOMPYI+BETAZYI*WCOMPZI
     .         +BETAYI*WCOMPI
       ALPHAYI=-ALPHAYI*ALPHAI2
       ALPHAZI= WGRADZI
C    .         +BETAXI*WGRADZXI+BETAYI*WGRADZYI+BETAZI*WGRADZZI
     .         +BETAXI*WGRADXZI+BETAYI*WGRADYZI+BETAZI*WGRADZZI
     .         +BETAXZI*WCOMPXI+BETAYZI*WCOMPYI+BETAZZI*WCOMPZI
     .         +BETAZI*WCOMPI
       ALPHAZI=-ALPHAZI*ALPHAI2
C------
       WACOMP(5,N)=ALPHAXI
       WACOMP(6,N)=ALPHAYI
       WACOMP(7,N)=ALPHAZI
C------
      ENDDO
C-----------------------------------------------
C       Prepare les corrections sur les particules symetriques.
C-----------------------------------------------
C-------------------------------------------
      RETURN
      END


Chd|====================================================================
Chd|  SPSCOMP                       source/elements/sph/spcompl.F 
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPSCOMP(
     1    ISPSYM  ,WACOMP  ,ISPCOND ,XFRAME  ,WSMCOMP ,
     2    GEO     ,IPART   ,IPARTSP ,WASPACT ,ITASK   )
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPSYM(NSPCOND,*), ISPCOND(NISPCOND,*),
     .        IPART(LIPART1,*), IPARTSP(*), WASPACT(*), ITASK
C     REAL
      my_real
     .   WACOMP(16,*), XFRAME(NXFRAME,*), WSMCOMP(6,*),
     .   GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MWAMUN(NSPHACT),MWAZERO(NSPHACT),MWAUN(NSPHACT) 
      INTEGER SM,JS,NC,IC,IS,
     .        I,N,IPRT,IGEO,IORDER,NMUN,NZERO,NUN
      my_real
     .       OX,OY,OZ,UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,
     .       S11,S12,S13,S22,S23,S33,
     .       ALPHAXI,ALPHAYI,ALPHAZI,
     .       BETAXI,BETAYI,BETAZI,
     .       ALPHAXJ,ALPHAYJ,ALPHAZJ,
     .       BETAXJ,BETAYJ,BETAZJ
C-----------------------------------------------
      my_real
     .         GET_U_GEO
      EXTERNAL GET_U_GEO
C-----------------------------------------------
        NMUN =0
        NZERO=0
        NUN  =0
        DO I=ITASK+1,NSPHACT,NTHREAD
         N=WASPACT(I)
         IPRT  =IPARTSP(N)
         IGEO  =IPART(2,IPRT)
         IORDER=NINT(GET_U_GEO(5,IGEO))
         IF(IORDER==-1)THEN
          NMUN=NMUN+1
          MWAMUN(NMUN)=N
         ELSEIF(IORDER== 0)THEN
          NZERO=NZERO+1
          MWAZERO(NZERO)=N
         ELSEIF(IORDER== 1)THEN
          NUN=NUN+1
          MWAUN(NUN)=N
         ENDIF
        ENDDO
C-----------------------------------------------
C     No kernel correction.
C-----------------------------------------------
        DO NC=1,NSPCOND
          IC=ISPCOND(2,NC)
          IS=ISPCOND(3,NC)
          DO I=1,NMUN
           SM=MWAMUN(I)
           JS=ISPSYM(NC,SM)
           IF(JS>0)THEN
            WSMCOMP(1,JS)=ZERO
            WSMCOMP(2,JS)=ZERO
            WSMCOMP(3,JS)=ZERO
            WSMCOMP(4,JS)=ZERO
            WSMCOMP(5,JS)=ZERO
            WSMCOMP(6,JS)=ZERO  
C           WACOMP( 8,JS)=0.
C           WACOMP( 9,JS)=0.
C           WACOMP(10,JS)=0.
C           WACOMP(11,JS)=0.
C           WACOMP(12,JS)=0.
C           WACOMP(13,JS)=0.
C    	    WACOMP(14,JS)=0.
C    	    WACOMP(15,JS)=0.
C    	    WACOMP(16,JS)=0.
     	   ENDIF
     	  ENDDO
     	ENDDO
C-----------------------------------------------
C  	Kernel correction up to order 0. or 1.
C-----------------------------------------------
     	DO NC=1,NSPCOND
     	  IC=ISPCOND(2,NC)
     	  IS=ISPCOND(3,NC)
     	  IF (IC==1) THEN
     	      OX=XFRAME(10,IS)
     	      OY=XFRAME(11,IS)
     	      OZ=XFRAME(12,IS)
     	      UX=XFRAME(1,IS)
     	      UY=XFRAME(2,IS)
     	      UZ=XFRAME(3,IS)
              VX=XFRAME(4,IS)
              VY=XFRAME(5,IS)
              VZ=XFRAME(6,IS)
              WX=XFRAME(7,IS)
              WY=XFRAME(8,IS)
              WZ=XFRAME(9,IS)
          ELSEIF (IC==2) THEN
              OX=XFRAME(10,IS)
              OY=XFRAME(11,IS)
              OZ=XFRAME(12,IS)
              UX=XFRAME(4,IS)
              UY=XFRAME(5,IS)
              UZ=XFRAME(6,IS)
              VX=XFRAME(7,IS)
              VY=XFRAME(8,IS)
              VZ=XFRAME(9,IS)
              WX=XFRAME(1,IS)
              WY=XFRAME(2,IS)
              WZ=XFRAME(3,IS)
          ELSEIF (IC==3) THEN
              OX=XFRAME(10,IS)
              OY=XFRAME(11,IS)
              OZ=XFRAME(12,IS)
              UX=XFRAME(7,IS)
              UY=XFRAME(8,IS)
              UZ=XFRAME(9,IS)
              VX=XFRAME(1,IS)
              VY=XFRAME(2,IS)
              VZ=XFRAME(3,IS)
              WX=XFRAME(4,IS)
              WY=XFRAME(5,IS)
              WZ=XFRAME(6,IS)
          ENDIF
          S11=-UX*UX+VX*VX+WX*WX
          S12=-UX*UY+VX*VY+WX*WY
          S13=-UX*UZ+VX*VZ+WX*WZ
          S22=-UY*UY+VY*VY+WY*WY
          S23=-UY*UZ+VY*VZ+WY*WZ
          S33=-UZ*UZ+VZ*VZ+WZ*WZ
C         S21=-UY*UX+VY*VX+WY*WX
C         S31=-UZ*UX+VZ*VX+WZ*WX
C         S32=-UZ*UY+VZ*VY+WZ*WY
C-----------------------------------------------
C  	Kernel correction up to order 0.
C-----------------------------------------------
          DO I=1,NZERO
           SM=MWAZERO(I)
           JS=ISPSYM(NC,SM)
           IF(JS>0)THEN
C           WACOMP(1,JS)=WACOMP(1,SM)
            WSMCOMP(1,JS)=ZERO
            WSMCOMP(2,JS)=ZERO
            WSMCOMP(3,JS)=ZERO  
            ALPHAXI=WACOMP( 5,SM)
            ALPHAYI=WACOMP( 6,SM)
            ALPHAZI=WACOMP( 7,SM)
            ALPHAXJ=S11*ALPHAXI+S12*ALPHAYI+S13*ALPHAZI
            ALPHAYJ=S12*ALPHAXI+S22*ALPHAYI+S23*ALPHAZI
            ALPHAZJ=S13*ALPHAXI+S23*ALPHAYI+S33*ALPHAZI
            WSMCOMP(4,JS)=ALPHAXJ
            WSMCOMP(5,JS)=ALPHAYJ
            WSMCOMP(6,JS)=ALPHAZJ
C           WACOMP( 8,JS)=0.
C           WACOMP( 9,JS)=0.
C           WACOMP(10,JS)=0.
C           WACOMP(11,JS)=0.
C           WACOMP(12,JS)=0.
C           WACOMP(13,JS)=0.
C           WACOMP(14,JS)=0.
C           WACOMP(15,JS)=0.
C           WACOMP(16,JS)=0.
           ENDIF
          ENDDO
C-----------------------------------------------
C     Kernel correction up to order 1
C     NON COMPATIBLE SPMD SUR PLUS D ONE DOMAINE
C-----------------------------------------------
     	  DO I=1,NUN
     	   SM=MWAUN(I)
     	   JS=ISPSYM(NC,SM)
     	   IF(JS>0)THEN
C    	    WACOMP(1,JS)=WACOMP(1,SM)
     	    BETAXI=WACOMP(2,SM)
     	    BETAYI=WACOMP(3,SM)
     	    BETAZI=WACOMP(4,SM)
     	    BETAXJ=S11*BETAXI+S12*BETAYI+S13*BETAZI
     	    BETAYJ=S12*BETAXI+S22*BETAYI+S23*BETAZI
            BETAZJ=S13*BETAXI+S23*BETAYI+S33*BETAZI
            WSMCOMP(1,JS)=BETAXJ
            WSMCOMP(2,JS)=BETAYJ
            WSMCOMP(3,JS)=BETAZJ
            ALPHAXI=WACOMP( 5,SM)
            ALPHAYI=WACOMP( 6,SM)
            ALPHAZI=WACOMP( 7,SM)
            ALPHAXJ=S11*ALPHAXI+S12*ALPHAYI+S13*ALPHAZI
            ALPHAYJ=S12*ALPHAXI+S22*ALPHAYI+S23*ALPHAZI
            ALPHAZJ=S13*ALPHAXI+S23*ALPHAYI+S33*ALPHAZI
            WSMCOMP( 4,JS)=ALPHAXJ
            WSMCOMP( 5,JS)=ALPHAYJ
            WSMCOMP( 6,JS)=ALPHAZJ
C           WACOMP( 8,JS)=WACOMP( 8,SM)
C           WACOMP( 9,JS)=WACOMP( 9,SM)
C           WACOMP(10,JS)=WACOMP(10,SM)
C           WACOMP(11,JS)=WACOMP(11,SM)
C           WACOMP(12,JS)=WACOMP(12,SM)
C           WACOMP(13,JS)=WACOMP(13,SM)
C           WACOMP(14,JS)=WACOMP(14,SM)
C           WACOMP(15,JS)=WACOMP(15,SM)
C           WACOMP(16,JS)=WACOMP(16,SM)
           ENDIF
          ENDDO
C
C Particules symetriques de particules remotes
C
          DO SM = 1+ITASK,NSPHR,NTHREAD
           JS=ISPSYMR(NC,SM)
           IF(JS>0)THEN
            WSMCOMP(1,JS)=ZERO
            WSMCOMP(2,JS)=ZERO
            WSMCOMP(3,JS)=ZERO  
            ALPHAXI=WACOMPR( 5,SM)
            ALPHAYI=WACOMPR( 6,SM)
            ALPHAZI=WACOMPR( 7,SM)
            ALPHAXJ=S11*ALPHAXI+S12*ALPHAYI+S13*ALPHAZI
            ALPHAYJ=S12*ALPHAXI+S22*ALPHAYI+S23*ALPHAZI
            ALPHAZJ=S13*ALPHAXI+S23*ALPHAYI+S33*ALPHAZI
            WSMCOMP(4,JS)=ALPHAXJ
            WSMCOMP(5,JS)=ALPHAYJ
            WSMCOMP(6,JS)=ALPHAZJ
           END IF
          END DO
        END DO

C-------------------------------------------
      RETURN
      END
