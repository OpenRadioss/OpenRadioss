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
Chd|  SPDENS                        source/elements/sph/spdens.F  
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        WEIGHT1                       source/elements/sph/weight.F  
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPDENS(
     1    X        ,V       ,MS      ,SPBUF   ,ITAB    ,
     2    KXSP     ,IXSP    ,NOD2SP  ,ISPSYM  ,XSPSYM  ,
     3    VSPSYM   ,IPARG   ,WA      ,WACOMP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr01_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPSYM(NSPCOND,*),IPARG(NPARG,*)
C     REAL ou REAL*8
      my_real
     .   X(3,*)    ,V(3,*)    ,MS(*)   ,
     .   SPBUF(NSPBUF,*) ,XSPSYM(3,*) ,VSPSYM(3,*) ,
     .   WA(KWASPH,*) ,WACOMP(16,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,INOD,JNOD,J,NVOIS,M,
     .        NVOISS,SM,JS,NC,NS,NN
      my_real
     .       XI,YI,ZI,DI,RHOI,XJ,YJ,ZJ,DJ,RHOJ,DIJ,
     .       VXI,VYI,VZI,VXJ,VYJ,VZJ,
     .       VJ,VJX,VJY,VJZ,
     .       DRHO,WGHT,WGRAD(3),DIVV,
     .       MUIJ,MUMAX,
     .       FACT,WUN,WVX,WVY,WVZ,WRHO,
     .       DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,DT1D2,
     .       EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,
     .       ROTVX,ROTVY,ROTVZ,ROTV,
     . ALPHAI,ALPHAXI,ALPHAYI,ALPHAZI,
     . BETAXI,BETAYI,BETAZI,
     . BETAXXI,BETAYXI,BETAZXI,
     . BETAXYI,BETAYYI,BETAZYI,
     . BETAXZI,BETAYZI,BETAZZI,
     . BETAX,WGRDX,WGRDY,WGRDZ
C-------------------------------------------
      DO I=LFT,LLT
       N    =NFT+I
       WA(1,N)=ZERO
       WA(2,N)=ZERO
       WA(3,N)=ZERO
       WA(4,N)=ZERO
       WA(5,N)=ZERO
       WA(6,N)=ZERO
       WA(7,N)=ZERO
       WA(8,N)=ZERO
       WA(9,N)=ZERO    
C      characteristic lenght = diameter.
       WA(11,N)=SPBUF(1,N)
       WA(12,N)=ZERO
      ENDDO
C-----------------------------------------------
C     Calcul des densites et tenseurs de deformations.
C-----------------------------------------------
      DT1D2=0.5*DT1
      DO 10 I=LFT,LLT
       N    =NFT+I
       IF(KXSP(2,N).LE.0)GOTO 10
       INOD =KXSP(3,N)
       XI=X(1,INOD)
       YI=X(2,INOD)
       ZI=X(3,INOD)
       DI=SPBUF(1,N)
       VXI=V(1,INOD)
       VYI=V(2,INOD)
       VZI=V(3,INOD)
       RHOI =WA(10,N)
C-----
C      for DT,SPCEL stability computation:
       MUMAX=ZERO
C
       ROTVX=ZERO
       ROTVY=ZERO
       ROTVZ=ZERO    
C------
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
        IF(JNOD.GT.0)THEN
          M=NOD2SP(JNOD)
          XJ=X(1,JNOD)
          YJ=X(2,JNOD)
          ZJ=X(3,JNOD)
          DJ=SPBUF(1,M)
          DIJ=0.5*(DI+DJ)
          CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
          VXJ=V(1,JNOD)
          VYJ=V(2,JNOD)
          VZJ=V(3,JNOD)
          RHOJ=WA(10,M)
          VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
        ELSE
          NN = -JNOD
          XJ=XSPHR(3,NN)
          YJ=XSPHR(4,NN)
          ZJ=XSPHR(5,NN)
          DJ=XSPHR(2,NN)
          DIJ=0.5*(DI+DJ)
          CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
          VXJ=XSPHR(9,NN)
          VYJ=XSPHR(10,NN)
          VZJ=XSPHR(11,NN)
          RHOJ=XSPHR(7,NN)
          VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
        END IF
        BETAX=ONE +BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
        WGRDX=
     .   WGRAD(1)*ALPHAI*BETAX
     .   +WGHT*
     .     (ALPHAXI*BETAX+ALPHAI*
     .       (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
        WGRDY=
     .   WGRAD(2)*ALPHAI*BETAX
     .   +WGHT*
     .     (ALPHAYI*BETAX+ALPHAI*
     .       (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
        WGRDZ=
     .   WGRAD(3)*ALPHAI*BETAX
     .   +WGHT*
     .     (ALPHAZI*BETAX+ALPHAI*
     .       (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
        WGRAD(1)=WGRDX
        WGRAD(2)=WGRDY
        WGRAD(3)=WGRDZ
        VJX=VJ*(VXI-VXJ)
        VJY=VJ*(VYI-VYJ)
        VJZ=VJ*(VZI-VZJ)
C
C       stores DXX,DYY,DZZ,DXY,DYZ,DXZ,DYX,DZY,DZX
C       2nd order correction :
        DXX=-VJX*WGRAD(1)
        DYY=-VJY*WGRAD(2)
        DZZ=-VJZ*WGRAD(3)
        DXY=-VJX*WGRAD(2)
        DYZ=-VJY*WGRAD(3)
        DXZ=-VJX*WGRAD(3)
        DYX=-VJY*WGRAD(1)
        DZY=-VJZ*WGRAD(2)
        DZX=-VJZ*WGRAD(1)
C--------
        EXY   = DXY
     .           -DT1D2*(DXX*DXY+DYX*DYY+DZX*DZY)
        EYZ   = DYZ
     .           -DT1D2*(DYY*DYZ+DZY*DZZ+DXY*DXZ)
        EXZ   = DXZ
     .           -DT1D2*(DZZ*DZX+DXZ*DXX+DYZ*DYX)
        EYX   = DYX
     .           -DT1D2*(DXX*DXY+DYX*DYY+DZX*DZY)
        EZY   = DZY
     .           -DT1D2*(DYY*DYZ+DZY*DZZ+DXY*DXZ)
        EZX   = DZX
     .           -DT1D2*(DZZ*DZX+DXZ*DXX+DYZ*DYX)
        EXX   = DXX
     .           -DT1D2*(DXX*DXX+DYX*DYX+DZX*DZX)
        EYY  = DYY
     .           -DT1D2*(DYY*DYY+DZY*DZY+DXY*DXY)
        EZZ  = DZZ
     .           -DT1D2*(DZZ*DZZ+DXZ*DXZ+DYZ*DYZ)
        WA(1,N)=WA(1,N)+EXX
        WA(2,N)=WA(2,N)+EYY
        WA(3,N)=WA(3,N)+EZZ
c       if(n==315)print*,'vj,VYJ,=',n,vj,VYJ
        WA(4,N)=WA(4,N)+EXY
        WA(5,N)=WA(5,N)+EYZ
        WA(6,N)=WA(6,N)+EXZ
        WA(7,N)=WA(7,N)+EYX
        WA(8,N)=WA(8,N)+EZY
        WA(9,N)=WA(9,N)+EZX
C--------
C       for stability computation into material routines:
        MUIJ=(VXI-VXJ)*(XI-XJ)
     .      +(VYI-VYJ)*(YI-YJ)
     .      +(VZI-VZJ)*(ZI-ZJ)
        MUIJ=DIJ*MUIJ/
     .      ((XI-XJ)*(XI-XJ)
     .      +(YI-YJ)*(YI-YJ)
C    .      +(ZI-ZJ)*(ZI-ZJ))
     .      +(ZI-ZJ)*(ZI-ZJ)+EM02*DIJ*DIJ)
        MUMAX=MAX(MUMAX,-MUIJ)
C--------
C       for artificial viscosity computation :
        ROTVX=ROTVX+VJY*WGRAD(3)-VJZ*WGRAD(2)
        ROTVY=ROTVY+VJZ*WGRAD(1)-VJX*WGRAD(3)
        ROTVZ=ROTVZ+VJX*WGRAD(2)-VJY*WGRAD(1)
       END DO
C------
C      partie symetrique.
       NVOISS=KXSP(6,N)
       DO J=KXSP(5,N)+1,KXSP(5,N)+NVOISS
        JS=IXSP(J,N)
        IF(JS.GT.0)THEN
          SM=JS/(NSPCOND+1)
          NC=MOD(JS,NSPCOND+1)
          JS=ISPSYM(NC,SM)
          XJ =XSPSYM(1,JS)
          YJ =XSPSYM(2,JS)
          ZJ =XSPSYM(3,JS)
          VXJ=VSPSYM(1,JS)
          VYJ=VSPSYM(2,JS)
          VZJ=VSPSYM(3,JS)
          DJ  =SPBUF(1,SM)
          DIJ =HALF*(DI+DJ)
          RHOJ=WA(10,SM)
          CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
          VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)
        ELSE
          SM=-JS/(NSPCOND+1)
          NC=MOD(-JS,NSPCOND+1)
          JS=ISPSYMR(NC,SM)
          XJ =XSPSYM(1,JS)
          YJ =XSPSYM(2,JS)
          ZJ =XSPSYM(3,JS)
          VXJ=VSPSYM(1,JS)
          VYJ=VSPSYM(2,JS)
          VZJ=VSPSYM(3,JS)
          DJ  =XSPHR(2,SM)
          DIJ =HALF*(DI+DJ)
          RHOJ=XSPHR(7,SM)
          CALL WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
          VJ=XSPHR(8,SM)/MAX(EM20,RHOJ)
        END IF
        BETAX=ONE + BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
        WGRDX=
     .   WGRAD(1)*ALPHAI*BETAX
     .   +WGHT*
     .     (ALPHAXI*BETAX+ALPHAI*
     .       (BETAXXI*(XI-XJ)+BETAYXI*(YI-YJ)+BETAZXI*(ZI-ZJ)+BETAXI))
        WGRDY=
     .   WGRAD(2)*ALPHAI*BETAX
     .   +WGHT*
     .     (ALPHAYI*BETAX+ALPHAI*
     .       (BETAXYI*(XI-XJ)+BETAYYI*(YI-YJ)+BETAZYI*(ZI-ZJ)+BETAYI))
        WGRDZ=
     .   WGRAD(3)*ALPHAI*BETAX
     .   +WGHT*
     .     (ALPHAZI*BETAX+ALPHAI*
     .       (BETAXZI*(XI-XJ)+BETAYZI*(YI-YJ)+BETAZZI*(ZI-ZJ)+BETAZI))
        WGRAD(1)=WGRDX
        WGRAD(2)=WGRDY
        WGRAD(3)=WGRDZ
        VJX=VJ*(VXI-VXJ)
        VJY=VJ*(VYI-VYJ)
        VJZ=VJ*(VZI-VZJ)
C
C       stores DXX,DYY,DZZ,DXY,DYZ,DXZ,DYX,DZY,DZX
C       2nd order correction :
        DXX=-VJX*WGRAD(1)
        DYY=-VJY*WGRAD(2)
        DZZ=-VJZ*WGRAD(3)
        DXY=-VJX*WGRAD(2)
        DYZ=-VJY*WGRAD(3)
        DXZ=-VJX*WGRAD(3)
        DYX=-VJY*WGRAD(1)
        DZY=-VJZ*WGRAD(2)
        DZX=-VJZ*WGRAD(1)
C--------
        EXY   = DXY
     .           -DT1D2*(DXX*DXY+DYX*DYY+DZX*DZY)
        EYZ   = DYZ
     .           -DT1D2*(DYY*DYZ+DZY*DZZ+DXY*DXZ)
        EXZ   = DXZ
     .           -DT1D2*(DZZ*DZX+DXZ*DXX+DYZ*DYX)
        EYX   = DYX
     .           -DT1D2*(DXX*DXY+DYX*DYY+DZX*DZY)
        EZY   = DZY
     .           -DT1D2*(DYY*DYZ+DZY*DZZ+DXY*DXZ)
        EZX   = DZX
     .           -DT1D2*(DZZ*DZX+DXZ*DXX+DYZ*DYX)
        EXX   = DXX
     .           -DT1D2*(DXX*DXX+DYX*DYX+DZX*DZX)
        EYY  = DYY
     .           -DT1D2*(DYY*DYY+DZY*DZY+DXY*DXY)
        EZZ  = DZZ
     .           -DT1D2*(DZZ*DZZ+DXZ*DXZ+DYZ*DYZ)
        WA(1,N)=WA(1,N)+EXX
        WA(2,N)=WA(2,N)+EYY
        WA(3,N)=WA(3,N)+EZZ
        WA(4,N)=WA(4,N)+EXY
        WA(5,N)=WA(5,N)+EYZ
        WA(6,N)=WA(6,N)+EXZ
        WA(7,N)=WA(7,N)+EYX
        WA(8,N)=WA(8,N)+EZY
        WA(9,N)=WA(9,N)+EZX
C-------
C       for stability computation into material routines:
        MUIJ=(VXI-VXJ)*(XI-XJ)
     .      +(VYI-VYJ)*(YI-YJ)
     .      +(VZI-VZJ)*(ZI-ZJ)
        MUIJ=DIJ*MUIJ/
     .      ((XI-XJ)*(XI-XJ)
     .      +(YI-YJ)*(YI-YJ)
C    .      +(ZI-ZJ)*(ZI-ZJ))
     .      +(ZI-ZJ)*(ZI-ZJ)+EM02*DIJ*DIJ)
        MUMAX=MAX(MUMAX,-MUIJ)
C-------
C       for artificial viscosity computation :
        ROTVX=ROTVX+VJY*WGRAD(3)-VJZ*WGRAD(2)
        ROTVY=ROTVY+VJZ*WGRAD(1)-VJX*WGRAD(3)
        ROTVZ=ROTVZ+VJX*WGRAD(2)-VJY*WGRAD(1)
       END DO
C------
C      for stability computation into material routines:
       WA(12,N)=MUMAX
C      characteristic lenght = diameter.
       WA(11,N)=SPBUF(1,N)
C------
C      actual density:
       DIVV       = WA(1,N)+WA(2,N)+WA(3,N)
       DRHO       =-DIVV*RHOI
c       if(drho/=0)print*,'DRHO=',DRHO
       SPBUF(2,N) = MAX(EM20,RHOI+DRHO*DT1)
C------
C      for h adaptation + artificial viscosity computation :
       WA(13,N)=DIVV
       ROTV =SQRT(ROTVX*ROTVX+ROTVY*ROTVY+ROTVZ*ROTVZ)
       WA(14,N)=ROTV
C------
 10    CONTINUE
C-----------------------------------------------
      RETURN
      END
