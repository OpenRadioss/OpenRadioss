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
Chd|  SPFORCP                       source/elements/sph/spforcp.F 
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        WEIGHT1                       source/elements/sph/weight.F  
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPFORCP(
     1    PM        ,GEO       ,X         ,V         ,MS        ,
     2    SPBUF     ,ITAB      ,PLD       ,BUFMAT    ,BUFGEO    ,
     3    PARTSAV   ,FSAV      ,DT2T      ,IPARG     ,NPC       ,
     4    KXSP      ,IXSP      ,NOD2SP    ,NELTST    ,ITYPTST   ,
     5    IPART     ,IPARTSP   ,ISPCOND   ,XFRAME    ,ISPSYM    ,
     6    XSPSYM    ,VSPSYM    ,WA        ,WASIGSM   ,WACOMP    ,
     7    WSMCOMP   ,WASPACT   ,WAR       ,STAB      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr02_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .   IPART(LIPART1,*) ,IPARTSP(*), NPC(*), IPARG(NPARG,*),
     .   NELTST,ITYPTST,ITAB(*) ,ISPCOND(NISPCOND,*),
     .   ISPSYM(NSPCOND,*), WASPACT(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*)  ,V(3,*)   ,MS(*)   ,
     .   PM(NPROPM,*), GEO(NPROPG,*),
     .   BUFMAT(*) ,BUFGEO(*) ,PLD(*)  ,FSAV(NTHVKI,*) ,
     .   SPBUF(NSPBUF,*) ,PARTSAV(NPSAV,*) ,DT2T ,
     .   XFRAME(NXFRAME,*) ,XSPSYM(3,*) ,VSPSYM(3,*), WA(KWASPH,*), 
     .   WASIGSM(6,*), WACOMP(16,*), WSMCOMP(6,*), WAR(10,*), STAB(7,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,INOD,JNOD,J,NVOIS,M,IPRT,IPROP,IMAT,I,
     .        NVOISS,IC,NC,IS,SM,JS,ISLIDE,SS,NSTAB,NN,KS,NR, 
     .        K,JPERM(KVOISPH),IERROR
      my_real
     .       XI,YI,ZI,DI,RHOI,XJ,YJ,ZJ,DJ,RHOJ,
     .       SXX,SXY,SXZ,SYY,SYZ,SZZ,
     .       TXX,TXY,TXZ,TYY,TYZ,TZZ,
     .       AX,AY,AZ,BX,BY,BZ,FX,FY,FZ,
     .       WGHT,WGRAD(3),
     .       DIJ,MM,
     .       VXI,VYI,VZI,VXJ,VYJ,VZJ,MUIJ,MUIJ2,PIJ,SSP,
     .       FACT,QA,QB,
     .       OX,OY,OZ,UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,
     .       UXX,UXY,UXZ,UYX,UYY,UYZ,UZX,UZY,UZZ,
     .       VXX,VXY,VXZ,VYY,VYZ,VZZ,
     .       DIVVI,DIVVJ,ROTVI,ROTVJ,FI,FJ,
     .       FVX,FVY,FVZ,WVIS,
     .       STIF,PJ,STIJ,SFAC,DLDT,L,CIJ,DZETA,WNORM,
     .        VI,VJ,VIJ,
     .        DRHOI,DRHOJ,SSP2I,SSP2J,STII,DRHOIDR,DRHOJDR,
     .        ALPHAI,ALPHAXI,ALPHAYI,ALPHAZI,
     .        BETAXI,BETAYI,BETAZI,
     .        BETAXXI,BETAYXI,BETAZXI,
     .        BETAXYI,BETAYYI,BETAZYI,
     .        BETAXZI,BETAYZI,BETAZZI,
     .        ALPHAJ,ALPHAXJ,ALPHAYJ,ALPHAZJ,
     .        BETAXJ,BETAYJ,BETAZJ,
     .        BETAXXJ,BETAYXJ,BETAZXJ,
     .        BETAXYJ,BETAYYJ,BETAZYJ,
     .        BETAXZJ,BETAYZJ,BETAZZJ,
     .        BETAX,WGRDX,WGRDY,WGRDZ,WGRD(3),
     .        VOLI,VOLJ,DXIJ,NXIJ,
     .        CX,CY,CZ,DX,DY,DZ,TX,TY,TZ,TFEXTT, WW, WI, WR, ZSTAB
C-----------------------------------------------
      TFEXTT=ZERO
      DO 10 I=LFT,LLT
        N    =NFT+I
        IF(KXSP(2,N).LE.0.AND.ISPH2SOL.EQ.0)GOTO 10
        IF(KXSP(2,N)==0.AND.ISPH2SOL/=0)GOTO 10
        INOD =KXSP(3,N)
        XI=X(1,INOD)
        YI=X(2,INOD)
        ZI=X(3,INOD)
        VXI=V(1,INOD)
        VYI=V(2,INOD)
        VZI=V(3,INOD)
        DI   =SPBUF(1,N)
        RHOI =SPBUF(2,N)
C
        SXX=WA(1,N)
        SYY=WA(2,N)
        SZZ=WA(3,N)
        SXY=WA(4,N)
        SYZ=WA(5,N)
        SXZ=WA(6,N)
        IPRT =IPARTSP(N)
        IPROP=IPART(2,IPRT)
        QA  = GEO(14,IPROP)
        QB  = GEO(15,IPROP)
C
C       for artificial viscosity computation.
        DIVVI=ABS(WA(13,N))
        ROTVI=WA(14,N)
        FI   =DIVVI/MAX(EM20,DIVVI+ROTVI)
C
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
C----------------------------------
C        FORCES + ARTIFICIAL VISCOUS FORCES + STABILITY:
C----------------------------------
        NVOIS=KXSP(4,N)
        DO J=1,NVOIS
         JNOD=IXSP(J,N)
         IF(JNOD.GT.0)THEN
          M=NOD2SP(JNOD)
C
C Solids to SPH, no interaction if both particles are inactive
          IF(KXSP(2,N).LE.0.AND.KXSP(2,M).LE.0)CYCLE
C
          XJ=X(1,JNOD)
          YJ=X(2,JNOD)
          ZJ=X(3,JNOD)
          VXJ=V(1,JNOD)
          VYJ=V(2,JNOD)
          VZJ=V(3,JNOD)
          DJ   =SPBUF(1,M)
          DIJ=UNDEMI*(DI+DJ)
          RHOJ =SPBUF(2,M)
          TXX=WA(1,M)
          TYY=WA(2,M)
          TZZ=WA(3,M)
          TXY=WA(4,M)
          TYZ=WA(5,M)
          TXZ=WA(6,M)
          CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
          WGRDX=WGRAD(1)
          WGRDY=WGRAD(2)
          WGRDZ=WGRAD(3)
          BETAX=1.+BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
          WGRAD(1)=
     .     WGRDX*ALPHAI*BETAX
     .     +WGHT*
     .      (ALPHAXI*BETAX+ALPHAI*
     .       (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
          WGRAD(2)=
     .     WGRDY*ALPHAI*BETAX
     .     +WGHT*
     .      (ALPHAYI*BETAX+ALPHAI*
     .       (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
          WGRAD(3)=
     .     WGRDZ*ALPHAI*BETAX
     .     +WGHT*
     .      (ALPHAZI*BETAX+ALPHAI*
     .       (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
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
          BETAX=UN +BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
          WGRD(1)=
     .     -WGRDX*ALPHAJ*BETAX
     .     +WGHT*
     .      (ALPHAXJ*BETAX+ALPHAJ*
     .       (BETAXXJ*(XJ-XI)+BETAYXJ*(YJ-YI)+BETAZXJ*(ZJ-ZI)+BETAXJ))
          WGRD(2)=
     .     -WGRDY*ALPHAJ*BETAX
     .     +WGHT*
     .      (ALPHAYJ*BETAX+ALPHAJ*
     .       (BETAXYJ*(XJ-XI)+BETAYYJ*(YJ-YI)+BETAZYJ*(ZJ-ZI)+BETAYJ))
          WGRD(3)=
     .     -WGRDZ*ALPHAJ*BETAX
     .     +WGHT*
     .      (ALPHAZJ*BETAX+ALPHAJ*
     .       (BETAXZJ*(XJ-XI)+BETAYZJ*(YJ-YI)+BETAZZJ*(ZJ-ZI)+BETAZJ))
C
          AX=SXX*WGRAD(1)+SXY*WGRAD(2)+SXZ*WGRAD(3)
          AY=SXY*WGRAD(1)+SYY*WGRAD(2)+SYZ*WGRAD(3)
          AZ=SXZ*WGRAD(1)+SYZ*WGRAD(2)+SZZ*WGRAD(3)
C          BX=TXX*WGRAD(1)+TXY*WGRAD(2)+TXZ*WGRAD(3)
C          BY=TXY*WGRAD(1)+TYY*WGRAD(2)+TYZ*WGRAD(3)
C          BZ=TXZ*WGRAD(1)+TYZ*WGRAD(2)+TZZ*WGRAD(3)
          BX=-(TXX*WGRD(1)+TXY*WGRD(2)+TXZ*WGRD(3))
          BY=-(TXY*WGRD(1)+TYY*WGRD(2)+TYZ*WGRD(3))
          BZ=-(TXZ*WGRD(1)+TYZ*WGRD(2)+TZZ*WGRD(3))
          MM=SPBUF(12,N)*SPBUF(12,M)
C-------- 
          VI =SPBUF(12,N)/MAX(EM20,RHOI)
          VJ =SPBUF(12,M)/MAX(EM20,RHOJ)
          VIJ=VI*VJ
          FX=VIJ*(AX+BX)
          FY=VIJ*(AY+BY)
          FZ=VIJ*(AZ+BZ)
C-------- 
          WI=ZERO
          IF(STAB(7,N)/=ZERO.AND.STAB(7,M)/=ZERO)THEN
            CX=STAB(1,N)*WGRAD(1)+STAB(4,N)*WGRAD(2)+STAB(6,N)*WGRAD(3)
            CY=STAB(4,N)*WGRAD(1)+STAB(2,N)*WGRAD(2)+STAB(5,N)*WGRAD(3)
            CZ=STAB(6,N)*WGRAD(1)+STAB(5,N)*WGRAD(2)+STAB(3,N)*WGRAD(3)
            DX=-(STAB(1,M)*WGRD(1)+STAB(4,M)*WGRD(2)+STAB(6,M)*WGRD(3))
            DY=-(STAB(4,M)*WGRD(1)+STAB(2,M)*WGRD(2)+STAB(5,M)*WGRD(3))
            DZ=-(STAB(6,M)*WGRD(1)+STAB(5,M)*WGRD(2)+STAB(3,M)*WGRD(3))
C           CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT)
            WW=WGHT*DIJ*DIJ*DIJ
            WR=UNDEMI*(STAB(7,N)+STAB(7,M))
            WI=WW*WW*WW*WW*WR
            TX=VIJ*WI*(CX+DX)
            TY=VIJ*WI*(CY+DY)
            TZ=VIJ*WI*(CZ+DZ)
            FX=FX+TX
            FY=FY+TY
            FZ=FZ+TZ
            TFEXTT=TFEXTT+(TX*V(1,INOD)+TY*V(2,INOD)+TZ*V(3,INOD))*DT1
          END IF
C--------
C         artitifial viscosity.
          DXIJ=(VXI-VXJ)*(XI-XJ)
     .        +(VYI-VYJ)*(YI-YJ)
     .        +(VZI-VZJ)*(ZI-ZJ)
          NXIJ=(XI-XJ)*(XI-XJ)
     .        +(YI-YJ)*(YI-YJ)
     .        +(ZI-ZJ)*(ZI-ZJ)
          MUIJ=DIJ*DXIJ/(NXIJ+EM02*DIJ*DIJ)
          DIVVJ=ABS(WA(13,M))
          ROTVJ=WA(14,M)
          FJ   =DIVVJ/MAX(EM20,DIVVJ+ROTVJ)
          MUIJ=MUIJ*(FI+FJ)*UNDEMI
          SSP=(WA(8,N)+WA(8,M))*UNDEMI
          MUIJ2=MIN(MUIJ,ZERO)*MIN(MUIJ,ZERO)
          PIJ =(QA*MUIJ2-QB*SSP*MUIJ)*DEUX/MAX(EM20,RHOI+RHOJ)
          FACT=MM*PIJ
C---------
C         FV(j,i)=-FV(i,j)
          WGRDX=(WGRAD(1)-WGRD(1))*UNDEMI
          WGRDY=(WGRAD(2)-WGRD(2))*UNDEMI
          WGRDZ=(WGRAD(3)-WGRD(3))*UNDEMI 
          FVX=-FACT*WGRDX
          FVY=-FACT*WGRDY
          FVZ=-FACT*WGRDZ
          IF((NODADT.NE.0).OR.(I7KGLO.NE.0))THEN
C
C           nodal stability
            DLDT=ABS(DXIJ)
            L=SQRT(NXIJ)
            DLDT=MAX(EM20,DLDT/MAX(EM20,L))
C
            VOLJ=SPBUF(12,M)/MAX(EM20,RHOJ)
            DRHOIDR= VOLJ
     .      * (WGRAD(1)*WGRAD(1)+WGRAD(2)*WGRAD(2)+WGRAD(3)*WGRAD(3))
            SSP2I=WA(9,N)*WA(9,N)
            STII = VOLJ*SPBUF(12,N)*SSP2I*DRHOIDR
C
            VOLI  =SPBUF(12,N)/MAX(EM20,RHOI)
            DRHOJDR= VOLI
     .      * (WGRD(1)*WGRD(1)+WGRD(2)*WGRD(2)+WGRD(3)*WGRD(3))
            SSP2J=WA(9,M)*WA(9,M)
            STIJ = VOLI*SPBUF(12,M)*SSP2J*DRHOJDR
C
            STIJ=DEUX*(STII+STIJ)
C
C           K*=FV divise par DL (C=FV divise par V)
            WNORM=SQRT(WGRDX*WGRDX+WGRDY*WGRDY+WGRDZ*WGRDZ)
            CIJ  =MM*ABS(PIJ)*WNORM/DLDT
            DZETA=CIJ/MAX(EM20,SQRT(DEUX*STIJ*SPBUF(12,N)))
            SFAC =SQRT(1.+DZETA*DZETA)-DZETA
            STIJ =STIJ/MAX(EM20,SFAC*SFAC)
            WA(7,N)=WA(7,N)+STIJ*(UN+WI)
          ENDIF
         ELSE                           ! cellule remote
          NN = -JNOD
C
C Solids to SPH, no interaction if both particles are inactive
          IF(KXSP(2,N).LE.0.AND.XSPHR(13,NN).LE.0)CYCLE
          XJ=XSPHR(3,NN)
          YJ=XSPHR(4,NN)
          ZJ=XSPHR(5,NN)
          VXJ=XSPHR(9,NN)
          VYJ=XSPHR(10,NN)
          VZJ=XSPHR(11,NN)
          DJ =XSPHR(2,NN)
          DIJ=UNDEMI*(DI+DJ)
          RHOJ =XSPHR(7,NN)
          TXX=WAR(1,NN)
          TYY=WAR(2,NN)
          TZZ=WAR(3,NN)
          TXY=WAR(4,NN)
          TYZ=WAR(5,NN)
          TXZ=WAR(6,NN)
          CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
          WGRDX=WGRAD(1)
          WGRDY=WGRAD(2)
          WGRDZ=WGRAD(3)
          BETAX=1.+BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
          WGRAD(1)=
     .     WGRDX*ALPHAI*BETAX
     .     +WGHT*
     .      (ALPHAXI*BETAX+ALPHAI*
     .       (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
          WGRAD(2)=
     .     WGRDY*ALPHAI*BETAX
     .     +WGHT*
     .      (ALPHAYI*BETAX+ALPHAI*
     .       (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
          WGRAD(3)=
     .     WGRDZ*ALPHAI*BETAX
     .     +WGHT*
     .      (ALPHAZI*BETAX+ALPHAI*
     .       (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
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
          BETAX=UN +BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
          WGRD(1)=
     .     -WGRDX*ALPHAJ*BETAX
     .     +WGHT*
     .      (ALPHAXJ*BETAX+ALPHAJ*
     .       (BETAXXJ*(XJ-XI)+BETAYXJ*(YJ-YI)+BETAZXJ*(ZJ-ZI)+BETAXJ))
          WGRD(2)=
     .     -WGRDY*ALPHAJ*BETAX
     .     +WGHT*
     .      (ALPHAYJ*BETAX+ALPHAJ*
     .       (BETAXYJ*(XJ-XI)+BETAYYJ*(YJ-YI)+BETAZYJ*(ZJ-ZI)+BETAYJ))
          WGRD(3)=
     .     -WGRDZ*ALPHAJ*BETAX
     .     +WGHT*
     .      (ALPHAZJ*BETAX+ALPHAJ*
     .       (BETAXZJ*(XJ-XI)+BETAYZJ*(YJ-YI)+BETAZZJ*(ZJ-ZI)+BETAZJ))
C
          AX=SXX*WGRAD(1)+SXY*WGRAD(2)+SXZ*WGRAD(3)
          AY=SXY*WGRAD(1)+SYY*WGRAD(2)+SYZ*WGRAD(3)
          AZ=SXZ*WGRAD(1)+SYZ*WGRAD(2)+SZZ*WGRAD(3)
C          BX=TXX*WGRAD(1)+TXY*WGRAD(2)+TXZ*WGRAD(3)
C          BY=TXY*WGRAD(1)+TYY*WGRAD(2)+TYZ*WGRAD(3)
C          BZ=TXZ*WGRAD(1)+TYZ*WGRAD(2)+TZZ*WGRAD(3)
          BX=-(TXX*WGRD(1)+TXY*WGRD(2)+TXZ*WGRD(3))
          BY=-(TXY*WGRD(1)+TYY*WGRD(2)+TYZ*WGRD(3))
          BZ=-(TXZ*WGRD(1)+TYZ*WGRD(2)+TZZ*WGRD(3))
          MM=SPBUF(12,N)*XSPHR(8,NN)
C-------- 
          VI =SPBUF(12,N)/MAX(EM20,RHOI)
          VJ =XSPHR(8,NN)/MAX(EM20,RHOJ)
          VIJ=VI*VJ
          FX=VIJ*(AX+BX)
          FY=VIJ*(AY+BY)
          FZ=VIJ*(AZ+BZ)
C-------- 
          WI=ZERO
          IF(STAB(7,N)/=ZERO.AND.STAB(7,NUMSPH+NN)/=ZERO)THEN
            CX=STAB(1,N)*WGRAD(1)+STAB(4,N)*WGRAD(2)+STAB(6,N)*WGRAD(3)
            CY=STAB(4,N)*WGRAD(1)+STAB(2,N)*WGRAD(2)+STAB(5,N)*WGRAD(3)
            CZ=STAB(6,N)*WGRAD(1)+STAB(5,N)*WGRAD(2)+STAB(3,N)*WGRAD(3)
            NR=NUMSPH+NN
            DX=-(STAB(1,NR)*WGRD(1)+STAB(4,NR)*WGRD(2)+STAB(6,NR)*WGRD(3))
            DY=-(STAB(4,NR)*WGRD(1)+STAB(2,NR)*WGRD(2)+STAB(5,NR)*WGRD(3))
            DZ=-(STAB(6,NR)*WGRD(1)+STAB(5,NR)*WGRD(2)+STAB(3,NR)*WGRD(3))
C           CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT)
            WW=WGHT*DIJ*DIJ*DIJ
            WR=UNDEMI*(STAB(7,N)+STAB(7,NUMSPH+NN))
            WI=WW*WW*WW*WW*WR
            TX=VIJ*WI*(CX+DX)
            TY=VIJ*WI*(CY+DY)
            TZ=VIJ*WI*(CZ+DZ)
            FX=FX+TX
            FY=FY+TY
            FZ=FZ+TZ
            TFEXTT=TFEXTT+(TX*V(1,INOD)+TY*V(2,INOD)+TZ*V(3,INOD))*DT1
          END IF
C--------
C         artitifial viscosity.
          DXIJ=(VXI-VXJ)*(XI-XJ)
     .        +(VYI-VYJ)*(YI-YJ)
     .        +(VZI-VZJ)*(ZI-ZJ)
          NXIJ=(XI-XJ)*(XI-XJ)
     .        +(YI-YJ)*(YI-YJ)
     .        +(ZI-ZJ)*(ZI-ZJ)
          MUIJ=DIJ*DXIJ/(NXIJ+EM02*DIJ*DIJ)
C WA(13) <=> WAR(9) ; WA(14) <=> WAR(10)
          DIVVJ=ABS(WAR(9,NN))
          ROTVJ=WAR(10,NN)
          FJ   =DIVVJ/MAX(EM20,DIVVJ+ROTVJ)
          MUIJ=MUIJ*(FI+FJ)*UNDEMI
C WA(8) <=> WAR(7)
          SSP=(WA(8,N)+WAR(7,NN))*UNDEMI
          MUIJ2=MIN(MUIJ,ZERO)*MIN(MUIJ,ZERO)
          PIJ =(QA*MUIJ2-QB*SSP*MUIJ)*DEUX/MAX(EM20,RHOI+RHOJ)
          FACT=MM*PIJ
C---------
C         FV(j,i)=-FV(i,j)
          WGRDX=(WGRAD(1)-WGRD(1))*UNDEMI
          WGRDY=(WGRAD(2)-WGRD(2))*UNDEMI
          WGRDZ=(WGRAD(3)-WGRD(3))*UNDEMI 
          FVX=-FACT*WGRDX
          FVY=-FACT*WGRDY
          FVZ=-FACT*WGRDZ
          IF((NODADT.NE.0).OR.(I7KGLO.NE.0))THEN
C
C           nodal stability
            DLDT=ABS(DXIJ)
            L=SQRT(NXIJ)
            DLDT=MAX(EM20,DLDT/MAX(EM20,L))
C
            VOLJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
            DRHOIDR= VOLJ
     .      * (WGRAD(1)*WGRAD(1)+WGRAD(2)*WGRAD(2)+WGRAD(3)*WGRAD(3))
            SSP2I=WA(9,N)*WA(9,N)
            STII = VOLJ*SPBUF(12,N)*SSP2I*DRHOIDR
C
            VOLI  =SPBUF(12,N)/MAX(EM20,RHOI)
            DRHOJDR= VOLI
     .      * (WGRD(1)*WGRD(1)+WGRD(2)*WGRD(2)+WGRD(3)*WGRD(3))
C WA(9) <=> WAR(8)
            SSP2J=WAR(8,NN)*WAR(8,NN)
            STIJ = VOLI*XSPHR(8,NN)*SSP2J*DRHOJDR
C
            STIJ=DEUX*(STII+STIJ)
C
C           K*=FV divise par DL (C=FV divise par V)
            WNORM=SQRT(WGRDX*WGRDX+WGRDY*WGRDY+WGRDZ*WGRDZ)
            CIJ  =MM*ABS(PIJ)*WNORM/DLDT
            DZETA=CIJ/MAX(EM20,SQRT(DEUX*STIJ*SPBUF(12,N)))
            SFAC =SQRT(1.+DZETA*DZETA)-DZETA
            STIJ =STIJ/MAX(EM20,SFAC*SFAC)
            WA(7,N)=WA(7,N)+STIJ*(UN+WI)
          ENDIF
         END IF
C
         FX=FX+FVX
         FY=FY+FVY
         FZ=FZ+FVZ
         WA(10,N)=WA(10,N)+FX
         WA(11,N)=WA(11,N)+FY
         WA(12,N)=WA(12,N)+FZ
C
C         pour travail des forces de visc. artificielle (par cellule).
         WVIS = MIN(ZERO,
     .          UNDEMI*(FVX*(VXI-VXJ)+FVY*(VYI-VYJ)+FVZ*(VZI-VZJ)))
         SPBUF(11,N)=SPBUF(11,N)+WVIS
        ENDDO
C----------------------------------
        NVOISS=KXSP(6,N)
        DO 200 J=KXSP(5,N)+1,KXSP(5,N)+NVOISS
          JS=IXSP(J,N)
          IF(JS.GT.0)THEN
           SM=JS/(NSPCOND+1)
C
C Solids to SPH, no interaction if both particles are inactive
           IF(KXSP(2,N).LE.0.AND.KXSP(2,SM).LE.0)CYCLE
C
           NC=MOD(JS,NSPCOND+1)
           JS=ISPSYM(NC,SM)
           XJ =XSPSYM(1,JS)
           YJ =XSPSYM(2,JS)
           ZJ =XSPSYM(3,JS)
           VXJ=VSPSYM(1,JS)
           VYJ=VSPSYM(2,JS)
           VZJ=VSPSYM(3,JS)
           DJ  =SPBUF(1,SM)
           DIJ =UNDEMI*(DI+DJ)
           RHOJ=SPBUF(2,SM)
           JNOD=KXSP(3,SM)
C
           TXX=WASIGSM(1,JS)
           TYY=WASIGSM(2,JS)
           TZZ=WASIGSM(3,JS)
           TXY=WASIGSM(4,JS)
           TYZ=WASIGSM(5,JS)
           TXZ=WASIGSM(6,JS)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
           WGRDX=WGRAD(1)
           WGRDY=WGRAD(2)
           WGRDZ=WGRAD(3)
           BETAX=UN +BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
           WGRAD(1)=
     .      WGRDX*ALPHAI*BETAX
     .      +WGHT*
     .       (ALPHAXI*BETAX+ALPHAI*
     .        (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
           WGRAD(2)=
     .      WGRDY*ALPHAI*BETAX
     .      +WGHT*
     .       (ALPHAYI*BETAX+ALPHAI*
     .        (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
           WGRAD(3)=
     .      WGRDZ*ALPHAI*BETAX
     .      +WGHT*
     .       (ALPHAZI*BETAX+ALPHAI*
     .        (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
C----------
C          noyau conjugue.
           ALPHAJ=WACOMP(1,SM)
C          BETAXJ=WACOMP(2,SM)
C          BETAYJ=WACOMP(3,SM)
C          BETAZJ=WACOMP(4,SM)
           BETAXJ=WSMCOMP(1,JS)
           BETAYJ=WSMCOMP(2,JS)
           BETAZJ=WSMCOMP(3,JS)
C          ALPHAXJ=WACOMP( 5,SM)
C          ALPHAYJ=WACOMP( 6,SM)
C          ALPHAZJ=WACOMP( 7,SM)
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
           BETAX=UN +BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
           WGRD(1)=
     .      -WGRDX*ALPHAJ*BETAX
     .      +WGHT*
     .       (ALPHAXJ*BETAX+ALPHAJ*
     .       (BETAXXJ*(XJ-XI)+BETAYXJ*(YJ-YI)+BETAZXJ*(ZJ-ZI)+BETAXJ))
           WGRD(2)=
     .      -WGRDY*ALPHAJ*BETAX
     .      +WGHT*
     .       (ALPHAYJ*BETAX+ALPHAJ*
     .        (BETAXYJ*(XJ-XI)+BETAYYJ*(YJ-YI)+BETAZYJ*(ZJ-ZI)+BETAYJ))
           WGRD(3)=
     .      -WGRDZ*ALPHAJ*BETAX
     .      +WGHT*
     .       (ALPHAZJ*BETAX+ALPHAJ*
     .        (BETAXZJ*(XJ-XI)+BETAYZJ*(YJ-YI)+BETAZZJ*(ZJ-ZI)+BETAZJ))
           AX=SXX*WGRAD(1)+SXY*WGRAD(2)+SXZ*WGRAD(3)
           AY=SXY*WGRAD(1)+SYY*WGRAD(2)+SYZ*WGRAD(3)
           AZ=SXZ*WGRAD(1)+SYZ*WGRAD(2)+SZZ*WGRAD(3)
           BX=-(TXX*WGRD(1)+TXY*WGRD(2)+TXZ*WGRD(3))
           BY=-(TXY*WGRD(1)+TYY*WGRD(2)+TYZ*WGRD(3))
           BZ=-(TXZ*WGRD(1)+TYZ*WGRD(2)+TZZ*WGRD(3))
           MM=SPBUF(12,N)*SPBUF(12,SM)
C-------- 
           VI =SPBUF(12,N)/MAX(EM20,RHOI)
           VJ =SPBUF(12,SM)/MAX(EM20,RHOJ)
           VIJ=VI*VJ
           FX=VIJ*(AX+BX)
           FY=VIJ*(AY+BY)
           FZ=VIJ*(AZ+BZ)
           WI=ZERO
           IF(STAB(7,N)/=ZERO.AND.STAB(7,SM)/=ZERO)THEN
             CX=STAB(1,N)*WGRAD(1)+STAB(4,N)*WGRAD(2)+STAB(6,N)*WGRAD(3)
             CY=STAB(4,N)*WGRAD(1)+STAB(2,N)*WGRAD(2)+STAB(5,N)*WGRAD(3)
             CZ=STAB(6,N)*WGRAD(1)+STAB(5,N)*WGRAD(2)+STAB(3,N)*WGRAD(3)
             KS=NUMSPH+NSPHR+JS
             DX=-(STAB(1,KS)*WGRD(1)+STAB(4,KS)*WGRD(2)+STAB(6,KS)*WGRD(3))
             DY=-(STAB(4,KS)*WGRD(1)+STAB(2,KS)*WGRD(2)+STAB(5,KS)*WGRD(3))
             DZ=-(STAB(6,KS)*WGRD(1)+STAB(5,KS)*WGRD(2)+STAB(3,KS)*WGRD(3))
C            CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT)
             WW=WGHT*DIJ*DIJ*DIJ
             WR=UNDEMI*(STAB(7,N)+STAB(7,SM))
             WI=WW*WW*WW*WW*WR
             TX=VIJ*WI*(CX+DX)
             TY=VIJ*WI*(CY+DY)
             TZ=VIJ*WI*(CZ+DZ)
             FX=FX+TX
             FY=FY+TY
             FZ=FZ+TZ
             TFEXTT=TFEXTT+(TX*V(1,INOD)+TY*V(2,INOD)+TZ*V(3,INOD))*DT1
           END IF
C-------- 
C          artitifial viscosity.
           DXIJ=(VXI-VXJ)*(XI-XJ)
     .         +(VYI-VYJ)*(YI-YJ)
     .         +(VZI-VZJ)*(ZI-ZJ)
           NXIJ=(XI-XJ)*(XI-XJ)
     .         +(YI-YJ)*(YI-YJ)
     .         +(ZI-ZJ)*(ZI-ZJ)
           MUIJ=DIJ*DXIJ/(NXIJ+EM02*DIJ*DIJ)
           DIVVJ=ABS(WA(13,SM))
           ROTVJ=WA(14,SM)
           FJ=DIVVJ/MAX(EM20,DIVVJ+ROTVJ)
           MUIJ=MUIJ*(FI+FJ)*UNDEMI
           SSP=(WA(8,N)+WA(8,SM))*UNDEMI
           MUIJ2=MIN(MUIJ,ZERO)*MIN(MUIJ,ZERO)
           PIJ =(QA*MUIJ2-QB*SSP*MUIJ)*DEUX/MAX(EM20,RHOI+RHOJ)
           FACT=MM*PIJ
C---------
           WGRDX=(WGRAD(1)-WGRD(1))*UNDEMI
           WGRDY=(WGRAD(2)-WGRD(2))*UNDEMI
           WGRDZ=(WGRAD(3)-WGRD(3))*UNDEMI 
           FVX=-FACT*WGRDX
           FVY=-FACT*WGRDY
           FVZ=-FACT*WGRDZ
           IF((NODADT.NE.0).OR.(I7KGLO.NE.0))THEN
C            nodal stability
             DLDT=ABS(DXIJ)
             L   =SQRT(NXIJ)
             DLDT=MAX(EM20,DLDT/MAX(EM20,L))
C 
             VOLJ=SPBUF(12,SM)/MAX(EM20,RHOJ)
             DRHOIDR= VOLJ
     .       * (WGRAD(1)*WGRAD(1)+WGRAD(2)*WGRAD(2)+WGRAD(3)*WGRAD(3))
             SSP2I=WA(9,N)*WA(9,N)
             STII = VOLJ*SPBUF(12,N)*SSP2I*DRHOIDR
C
             VOLI  =SPBUF(12,N)/MAX(EM20,RHOI)
             DRHOJDR= VOLI
     .       * (WGRD(1)*WGRD(1)+WGRD(2)*WGRD(2)+WGRD(3)*WGRD(3))
             SSP2J=WA(9,SM)*WA(9,SM)
             STIJ = VOLI*SPBUF(12,SM)*SSP2J*DRHOJDR
C   
             STIJ=DEUX*(STII+STIJ)
C   
C            K*=FV divise par DL (C=FV divise par V)
             WNORM=SQRT(WGRDX*WGRDX+WGRDY*WGRDY+WGRDZ*WGRDZ)
             CIJ  =MM*ABS(PIJ)*WNORM/DLDT
             DZETA=CIJ/MAX(EM20,SQRT(DEUX*STIJ*SPBUF(12,N)))
             SFAC =SQRT(UN +DZETA*DZETA)-DZETA
             STIJ =STIJ/MAX(EM20,SFAC*SFAC)
             WA(7,N)=WA(7,N)+STIJ*(UN+WI)
           ENDIF
           FX=FX+FVX
           FY=FY+FVY
           FZ=FZ+FVZ
           WA(10,N)=WA(10,N)+FX
           WA(11,N)=WA(11,N)+FY
           WA(12,N)=WA(12,N)+FZ
C 
C          pour travail des forces de visc. artificielle (par cellule).
           WVIS = MIN(ZERO,
     .            UNDEMI*(FVX*(VXI-VXJ)+FVY*(VYI-VYJ)+FVZ*(VZI-VZJ)))
           SPBUF(11,N)=SPBUF(11,N)+WVIS
          ELSE                      ! particule symetrique de particule remote
           SM=-JS/(NSPCOND+1)
C
C Solids to SPH, no interaction if both particles are inactive
          IF(KXSP(2,N).LE.0.AND.XSPHR(13,SM).LE.0)CYCLE
           NC=MOD(-JS,NSPCOND+1)
           JS=ISPSYMR(NC,SM)
           XJ =XSPSYM(1,JS)
           YJ =XSPSYM(2,JS)
           ZJ =XSPSYM(3,JS)
           VXJ=VSPSYM(1,JS)
           VYJ=VSPSYM(2,JS)
           VZJ=VSPSYM(3,JS)
           DJ  =XSPHR(2,SM)
           DIJ =UNDEMI*(DI+DJ)
           RHOJ=XSPHR(7,SM)
C
           TXX=WASIGSM(1,JS)
           TYY=WASIGSM(2,JS)
           TZZ=WASIGSM(3,JS)
           TXY=WASIGSM(4,JS)
           TYZ=WASIGSM(5,JS)
           TXZ=WASIGSM(6,JS)
           CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
           WGRDX=WGRAD(1)
           WGRDY=WGRAD(2)
           WGRDZ=WGRAD(3)
           BETAX=UN +BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
           WGRAD(1)=
     .      WGRDX*ALPHAI*BETAX
     .      +WGHT*
     .       (ALPHAXI*BETAX+ALPHAI*
     .        (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
           WGRAD(2)=
     .      WGRDY*ALPHAI*BETAX
     .      +WGHT*
     .       (ALPHAYI*BETAX+ALPHAI*
     .        (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
           WGRAD(3)=
     .      WGRDZ*ALPHAI*BETAX
     .      +WGHT*
     .       (ALPHAZI*BETAX+ALPHAI*
     .        (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
C----------
C          noyau conjugue.
           ALPHAJ=WACOMPR(1,SM)
C          BETAXJ=WACOMPR(2,SM)
C          BETAYJ=WACOMPR(3,SM)
C          BETAZJ=WACOMPR(4,SM)
           BETAXJ=WSMCOMP(1,JS)
           BETAYJ=WSMCOMP(2,JS)
           BETAZJ=WSMCOMP(3,JS)
C          ALPHAXJ=WACOMPR( 5,SM)
C          ALPHAYJ=WACOMPR( 6,SM)
C          ALPHAZJ=WACOMPR( 7,SM)
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
           BETAX=UN +BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
           WGRD(1)=
     .      -WGRDX*ALPHAJ*BETAX
     .      +WGHT*
     .       (ALPHAXJ*BETAX+ALPHAJ*
     .       (BETAXXJ*(XJ-XI)+BETAYXJ*(YJ-YI)+BETAZXJ*(ZJ-ZI)+BETAXJ))
           WGRD(2)=
     .      -WGRDY*ALPHAJ*BETAX
     .      +WGHT*
     .       (ALPHAYJ*BETAX+ALPHAJ*
     .        (BETAXYJ*(XJ-XI)+BETAYYJ*(YJ-YI)+BETAZYJ*(ZJ-ZI)+BETAYJ))
           WGRD(3)=
     .      -WGRDZ*ALPHAJ*BETAX
     .      +WGHT*
     .       (ALPHAZJ*BETAX+ALPHAJ*
     .        (BETAXZJ*(XJ-XI)+BETAYZJ*(YJ-YI)+BETAZZJ*(ZJ-ZI)+BETAZJ))
           AX=SXX*WGRAD(1)+SXY*WGRAD(2)+SXZ*WGRAD(3)
           AY=SXY*WGRAD(1)+SYY*WGRAD(2)+SYZ*WGRAD(3)
           AZ=SXZ*WGRAD(1)+SYZ*WGRAD(2)+SZZ*WGRAD(3)
           BX=-(TXX*WGRD(1)+TXY*WGRD(2)+TXZ*WGRD(3))
           BY=-(TXY*WGRD(1)+TYY*WGRD(2)+TYZ*WGRD(3))
           BZ=-(TXZ*WGRD(1)+TYZ*WGRD(2)+TZZ*WGRD(3))
           MM=SPBUF(12,N)*XSPHR(8,SM)
C--------
           VI =SPBUF(12,N)/MAX(EM20,RHOI)
           VJ =XSPHR(8,SM)/MAX(EM20,RHOJ)
           VIJ=VI*VJ
           FX=VIJ*(AX+BX)
           FY=VIJ*(AY+BY)
           FZ=VIJ*(AZ+BZ)
           WI=ZERO
           IF(STAB(7,N)/=ZERO.AND.STAB(7,NUMSPH+SM)/=ZERO)THEN
             CX=STAB(1,N)*WGRAD(1)+STAB(4,N)*WGRAD(2)+STAB(6,N)*WGRAD(3)
             CY=STAB(4,N)*WGRAD(1)+STAB(2,N)*WGRAD(2)+STAB(5,N)*WGRAD(3)
             CZ=STAB(6,N)*WGRAD(1)+STAB(5,N)*WGRAD(2)+STAB(3,N)*WGRAD(3)
             KS=NUMSPH+NSPHR+JS
             DX=-(STAB(1,KS)*WGRD(1)+STAB(4,KS)*WGRD(2)+STAB(6,KS)*WGRD(3))
             DY=-(STAB(4,KS)*WGRD(1)+STAB(2,KS)*WGRD(2)+STAB(5,KS)*WGRD(3))
             DZ=-(STAB(6,KS)*WGRD(1)+STAB(5,KS)*WGRD(2)+STAB(3,KS)*WGRD(3))
C            CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT)
             WW=WGHT*DIJ*DIJ*DIJ
             WR=UNDEMI*(STAB(7,N)+STAB(7,NUMSPH+SM))
             WI=WW*WW*WW*WW*WR
             TX=VIJ*WI*(CX+DX)
             TY=VIJ*WI*(CY+DY)
             TZ=VIJ*WI*(CZ+DZ)
             FX=FX+TX
             FY=FY+TY
             FZ=FZ+TZ
             TFEXTT=TFEXTT+(TX*V(1,INOD)+TY*V(2,INOD)+TZ*V(3,INOD))*DT1
           END IF
C-------- 
C          artitifial viscosity.
           DXIJ=(VXI-VXJ)*(XI-XJ)
     .         +(VYI-VYJ)*(YI-YJ)
     .         +(VZI-VZJ)*(ZI-ZJ)
           NXIJ=(XI-XJ)*(XI-XJ)
     .         +(YI-YJ)*(YI-YJ)
     .         +(ZI-ZJ)*(ZI-ZJ)
           MUIJ=DIJ*DXIJ/(NXIJ+EM02*DIJ*DIJ)
C WA(13) <=> WAR(9) ; WA(14) <=> WAR(10)
           DIVVJ=ABS(WAR(9,SM))
           ROTVJ=WAR(10,SM)
           FJ=DIVVJ/MAX(EM20,DIVVJ+ROTVJ)
           MUIJ=MUIJ*(FI+FJ)*UNDEMI
C WA(8) <=> WAR(7)
           SSP=(WA(8,N)+WAR(7,SM))*UNDEMI
           MUIJ2=MIN(MUIJ,ZERO)*MIN(MUIJ,ZERO)
           PIJ =(QA*MUIJ2-QB*SSP*MUIJ)*DEUX/MAX(EM20,RHOI+RHOJ)
           FACT=MM*PIJ
C---------
           WGRDX=(WGRAD(1)-WGRD(1))*UNDEMI
           WGRDY=(WGRAD(2)-WGRD(2))*UNDEMI
           WGRDZ=(WGRAD(3)-WGRD(3))*UNDEMI 
           FVX=-FACT*WGRDX
           FVY=-FACT*WGRDY
           FVZ=-FACT*WGRDZ
           IF((NODADT.NE.0).OR.(I7KGLO.NE.0))THEN
C            nodal stability
             DLDT=ABS(DXIJ)
             L   =SQRT(NXIJ)
             DLDT=MAX(EM20,DLDT/MAX(EM20,L))
C 
             VOLJ=XSPHR(8,SM)/MAX(EM20,RHOJ)
             DRHOIDR= VOLJ
     .       * (WGRAD(1)*WGRAD(1)+WGRAD(2)*WGRAD(2)+WGRAD(3)*WGRAD(3))
             SSP2I=WA(9,N)*WA(9,N)
             STII = VOLJ*SPBUF(12,N)*SSP2I*DRHOIDR
C
             VOLI  =SPBUF(12,N)/MAX(EM20,RHOI)
             DRHOJDR= VOLI
     .       * (WGRD(1)*WGRD(1)+WGRD(2)*WGRD(2)+WGRD(3)*WGRD(3))
C WA(9) <=> WAR(8)
             SSP2J=WAR(8,SM)*WAR(8,SM)
             STIJ = VOLI*XSPHR(8,SM)*SSP2J*DRHOJDR
C   
             STIJ=DEUX*(STII+STIJ)
C   
C            K*=FV divise par DL (C=FV divise par V)
             WNORM=SQRT(WGRDX*WGRDX+WGRDY*WGRDY+WGRDZ*WGRDZ)
             CIJ  =MM*ABS(PIJ)*WNORM/DLDT
             DZETA=CIJ/MAX(EM20,SQRT(DEUX*STIJ*SPBUF(12,N)))
             SFAC =SQRT(UN +DZETA*DZETA)-DZETA
             STIJ =STIJ/MAX(EM20,SFAC*SFAC)
             WA(7,N)=WA(7,N)+STIJ*(UN+WI)
           ENDIF
           FX=FX+FVX
           FY=FY+FVY
           FZ=FZ+FVZ
           WA(10,N)=WA(10,N)+FX
           WA(11,N)=WA(11,N)+FY
           WA(12,N)=WA(12,N)+FZ
C 
C          pour travail des forces de visc. artificielle (par cellule).
           WVIS = MIN(ZERO,
     .            UNDEMI*(FVX*(VXI-VXJ)+FVY*(VYI-VYJ)+FVZ*(VZI-VZJ)))
           SPBUF(11,N)=SPBUF(11,N)+WVIS
          END IF
 200    CONTINUE
C-------
 10   CONTINUE
#include "lockon.inc"
      TFEXT=TFEXT+TFEXTT
#include "lockoff.inc"
C----------------------------------
      RETURN
      END
