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
Chd|  SPONFV                        source/elements/sph/sponfv.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_SPHGETA                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETD                  source/mpi/elements/spmd_sph.F
Chd|        WEIGHT0                       source/elements/sph/weight.F  
Chd|        WEIGHT1                       source/elements/sph/weight.F  
Chd|        MOD_SPLISSV                   source/elements/sph/splissv.F 
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPONFV(X ,V        ,A       ,D       ,MS      ,
     2             SPBUF   ,ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,
     3             NPC     ,PLD     ,ISPHIO  ,VSPHIO  ,IPART   ,
     4             IPARTSP ,WASPACT ,WA      ,VNORMAL   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX    
      USE MOD_SPLISSV
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr01_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),NPC(*),
     .        ISPHIO(NISPHIO,*),IPART(LIPART1,*),IPARTSP(*),
     .        WASPACT(*)
C     REAL
      my_real
     .   X(3,*) ,V(3,*) ,A(3,*) ,D(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,
     .   PLD(*) ,VSPHIO(*), WA(*), VNORMAL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITYPE,
     .        II,IPT,JJ,NPF,IFVITS,
     .        NS,N,INOD,IACTIVE,
     .        IPPV,J,M,JNOD,IMPOSE,JMPOSE,
     .        NVOIS,IJ,NP,K,JMPOSE2,NN
C     REAL
      my_real
     .       TFEXTT,T05,
     .       PENTV,VX,VY,VZ,VN,VT,UX,UY,UZ,UN1,NX,NY,NZ,
     .       PS,
     .       XI,YI,ZI,XJ,YJ,ZJ,DMIN,DD,DTINV,
     .       DI,RHOI,DJ,RHOJ,DIJ,
     .       VXI,VYI,VZI,VXJ,VYJ,VZJ,
     .       VJ,VJX,VJY,VJZ,
     .       WGHT,WGRAD(3),WGRDX,WGRDY,WGRDZ,
     .       DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,DT1D2,
     .       EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,
     .       ALPHAI,ALPHAXI,ALPHAYI,ALPHAZI,ALPHAI2,XP,YP,ZP
     
      LOGICAL lBOOL

      my_real       ,DIMENSION(:,:), ALLOCATABLE :: DSPHR

C-----------------------------------------------
C     inlets.
C-----------------------------------------------
      T05=TT + HALF*DT2

      DO I=1,NSPHIO
       ITYPE=ISPHIO(1,I)
       IF(ITYPE==1)THEN
C------
       IFVITS=ISPHIO(8,I)
       VN    =ZERO
       IF(IFVITS/=0)THEN
        NPF = (NPC(IFVITS+1)-NPC(IFVITS))/2
        II  = NPC(IFVITS)
        IF (T05<=PLD(II)) THEN
          PENTV=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
          VN   =PLD(II+1)+PENTV*(T05-PLD(II))
        ELSEIF (T05>=PLD(II+2*(NPF-1))) THEN
          JJ=II+2*(NPF-1)
          PENTV=(PLD(JJ+1)-PLD(JJ-1))/(PLD(JJ)-PLD(JJ-2))
          VN   =PLD(JJ+1)+MAX(-PLD(JJ+1),PENTV*(T05-PLD(JJ)))
        ELSE
          DO IPT=1,NPF-1
           IF (PLD(II)<=T05.AND.T05<=PLD(II+2)) THEN
            PENTV=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
            VN   =PLD(II+1)+PENTV*(T05-PLD(II))
            GOTO 10
           ENDIF
           II=II+2
          ENDDO
 10       CONTINUE
        ENDIF
       ENDIF
       WA(I)=VN
       ENDIF
      ENDDO

C-----
      TFEXTT=ZERO
      DTINV =ONE/DT12
C
      DO NS=1,NSPHACT
      N=WASPACT(NS)
      IMPOSE=KXSP(2,N)/(NGROUP+1)
      IF(IMPOSE/=0)THEN
       ITYPE=ISPHIO(1,IMPOSE)
       IF(ITYPE==1)THEN
C------
          VN=WA(IMPOSE)
C------
          INOD=KXSP(3,N)
          UX=V(1,INOD)+A(1,INOD)*DT12
          UY=V(2,INOD)+A(2,INOD)*DT12
          UZ=V(3,INOD)+A(3,INOD)*DT12
          NX=VNORMAL(1,N)
          NY=VNORMAL(2,N)
          NZ=VNORMAL(3,N)
          UN1=UX*NX+UY*NY+UZ*NZ
          VX=UX+(VN-UN1)*NX
          VY=UY+(VN-UN1)*NY
          VZ=UZ+(VN-UN1)*NZ
          TFEXTT=TFEXTT+HALF*MS(INOD)*
     .     ((VX*VX+VY*VY+VZ*VZ)-(UX*UX+UY*UY+UZ*UZ))
          A(1,INOD)=(VX-V(1,INOD))*DTINV
          A(2,INOD)=(VY-V(2,INOD))*DTINV
          A(3,INOD)=(VZ-V(3,INOD))*DTINV
       ENDIF
      ENDIF
      ENDDO

C-----------------------------------------------
C Comm D et A sur cellules remotes
C-----------------------------------------------
      IF(NSPMD>1)THEN
          ALLOCATE(ASPHR(3,NSPHR))
          ALLOCATE(DSPHR(12,NSPHR))
          CALL SPMD_SPHGETA(KXSP,SPBUF,A,ASPHR)
          CALL SPMD_SPHGETD(KXSP,IXSP,ISPHIO,X,WASPACT,NOD2SP,
     .                      SPBUF,V,A,ASPHR,DSPHR)
      END IF
C-------------------------------------------
C     general outlet & silent boundary.
C-------------------------------------------
      DO NS=1,NSPHACT
       N=WASPACT(NS)
       IMPOSE=KXSP(2,N)/(NGROUP+1)
       lBOOL=.FALSE.
       IF(IMPOSE /= 0)THEN
         IF(ISPHIO(1,IMPOSE)==2.OR.ISPHIO(1,IMPOSE)==3)lBOOL=.TRUE.       
       ENDIF
       IF(lBOOL)THEN
          INOD=KXSP(3,N)
          XI=X(1,INOD)
          YI=X(2,INOD)
          ZI=X(3,INOD)
C-------
C         plus proche voisin en amont de l'outlet => IPPV.
          IPPV=0
          DMIN=1.E+20
          DO  K=1,KXSP(4,N)  
           JNOD=IXSP(K,N)
  
           IF(JNOD>0)THEN
             M   =NOD2SP(JNOD)
             JMPOSE=KXSP(2,M)/(NGROUP+1)
             lBOOL=.FALSE.
             IF(JMPOSE == 0)THEN
               lBOOL=.TRUE.
             ELSE
               IF(ISPHIO(1,JMPOSE) == 1)lBOOL=.TRUE.
             ENDIF
             IF(lBOOL)THEN
               XJ  =X(1,JNOD)
               YJ  =X(2,JNOD)
               ZJ  =X(3,JNOD)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD<DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
             ENDIF
           ELSE
             NN = -JNOD
             JMPOSE = NINT(XSPHR(12,NN))
             IF(JMPOSE>0)THEN
               JMPOSE2=ISPHIO(1,JMPOSE)
             ELSE
               JMPOSE2=0
             ENDIF
             IF(JMPOSE2==0.OR.JMPOSE2==1)THEN
               XJ  =XSPHR(3,NN)
               YJ  =XSPHR(4,NN)
               ZJ  =XSPHR(5,NN)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD<DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
             ENDIF
           ENDIF
          ENDDO
C-------
C         calcule grad.v en IPPV (approximation sph corrigee en amont de IPPV).
          IF(IPPV>0)THEN
            NP=NOD2SP(IPPV)
            XP=X(1,IPPV)
            YP=X(2,IPPV)
            ZP=X(3,IPPV)
            DI  =SPBUF(1,NP)
            RHOI=SPBUF(2,NP)

            CALL WEIGHT0(XP,YP,ZP,XP,YP,ZP,DI,WGHT)
            VJ=SPBUF(12,NP)/MAX(EM20,RHOI)
            ALPHAI=VJ*WGHT
            ALPHAXI=ZERO
            ALPHAYI=ZERO
            ALPHAZI=ZERO

          DO J=1,KXSP(4,NP)
           JNOD=IXSP(J,NP)

c partie JNOD > 0
           IF(JNOD>0)THEN          ! particule locale
             M=NOD2SP(JNOD)
             JMPOSE=KXSP(2,M)/(NGROUP+1)
             lBOOL=.FALSE.
             IF(JMPOSE == 0)THEN
               lBOOL=.TRUE.
             ELSE
               IF(ISPHIO(1,JMPOSE) == 1)lBOOL=.TRUE.
             ENDIF
             IF(lBOOL)THEN
              DJ  =SPBUF(1,M)
              XJ  =X(1,JNOD)
              YJ  =X(2,JNOD)
              ZJ  =X(3,JNOD)
               DIJ =(DJ+DI)*HALF
               RHOJ=SPBUF(2,M)
               CALL WEIGHT1(XP,YP,ZP,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
               VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
               ALPHAI =ALPHAI +VJ*WGHT
               ALPHAXI=ALPHAXI+VJ*WGRAD(1)
               ALPHAYI=ALPHAYI+VJ*WGRAD(2)
               ALPHAZI=ALPHAZI+VJ*WGRAD(3)
             ENDIF
           ELSE           ! particule remote
             NN = -JNOD
             JMPOSE = NINT(XSPHR(12,NN))

             IF(JMPOSE>0)THEN
               JMPOSE2=ISPHIO(1,JMPOSE)
             ELSE
                JMPOSE2=0
             ENDIF
             IF(JMPOSE2==0.OR.JMPOSE2==1)THEN
               DJ  =XSPHR(2,NN)
               XJ  =XSPHR(3,NN)
               YJ  =XSPHR(4,NN)
               ZJ  =XSPHR(5,NN)
               DIJ =(DJ+DI)*HALF
               RHOJ=XSPHR(7,NN)
               CALL WEIGHT1(XP,YP,ZP,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
               VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
               ALPHAI =ALPHAI +VJ*WGHT
               ALPHAXI=ALPHAXI+VJ*WGRAD(1)
               ALPHAYI=ALPHAYI+VJ*WGRAD(2)
               ALPHAZI=ALPHAZI+VJ*WGRAD(3)
             ENDIF
           ENDIF
          ENDDO
C------
          ALPHAI =ONE/MAX(EM20,ALPHAI)
          ALPHAI2=ALPHAI*ALPHAI
          ALPHAXI=-ALPHAXI*ALPHAI2
          ALPHAYI=-ALPHAYI*ALPHAI2
          ALPHAZI=-ALPHAZI*ALPHAI2
C------
          VX =V(1,IPPV)+DT12*A(1,IPPV)
          VY =V(2,IPPV)+DT12*A(2,IPPV)
          VZ =V(3,IPPV)+DT12*A(3,IPPV)
          DXX=ZERO
          DXY=ZERO
          DXZ=ZERO
          DYX=ZERO
          DYY=ZERO
          DYZ=ZERO
          DZX=ZERO
          DZY=ZERO
          DZZ=ZERO

          DO J=1,KXSP(4,NP)
           JNOD=IXSP(J,NP)
           IF(JNOD>0)THEN
             M=NOD2SP(JNOD)
             JMPOSE=KXSP(2,M)/(NGROUP+1)
             lBOOL=.FALSE.
             IF(JMPOSE == 0)THEN
               lBOOL=.TRUE.
             ELSE
               IF(ISPHIO(1,JMPOSE) == 1)lBOOL=.TRUE.
             ENDIF
             IF(lBOOL)THEN
              DJ  =SPBUF(1,M)
              XJ  =X(1,JNOD)
              YJ  =X(2,JNOD)
              ZJ  =X(3,JNOD)
               DIJ =(DJ+DI)*HALF
               RHOJ=SPBUF(2,M)
               CALL WEIGHT1(XP,YP,ZP,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
               WGRDX=WGRAD(1)*ALPHAI+WGHT*ALPHAXI
               WGRDY=WGRAD(2)*ALPHAI+WGHT*ALPHAYI
               WGRDZ=WGRAD(3)*ALPHAI+WGHT*ALPHAZI
               VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
               VXJ =V(1,JNOD)+DT12*A(1,JNOD)
               VYJ =V(2,JNOD)+DT12*A(2,JNOD)
               VZJ =V(3,JNOD)+DT12*A(3,JNOD)
               VJX=VJ*(VXJ-VX)
               VJY=VJ*(VYJ-VY)
               VJZ=VJ*(VZJ-VZ)
               DXX=DXX+VJX*WGRDX
               DXY=DXY+VJX*WGRDY
               DXZ=DXZ+VJX*WGRDZ
               DYX=DYX+VJY*WGRDX
               DYY=DYY+VJY*WGRDY
               DYZ=DYZ+VJY*WGRDZ
               DZX=DZX+VJZ*WGRDX
               DZY=DZY+VJZ*WGRDY
               DZZ=DZZ+VJZ*WGRDZ
             ENDIF
           ELSE
             NN=-JNOD
             JMPOSE = NINT(XSPHR(12,NN))
             IF(JMPOSE>0)THEN
               JMPOSE2=ISPHIO(1,JMPOSE)
             ELSE
               JMPOSE2=0
             ENDIF
             IF(JMPOSE2==0.OR.JMPOSE2==1)THEN
               DJ  =XSPHR(2,NN)
               XJ  =XSPHR(3,NN)
               YJ  =XSPHR(4,NN)
               ZJ  =XSPHR(5,NN)
               DIJ =(DJ+DI)*HALF
               RHOJ=XSPHR(7,NN)
               CALL WEIGHT1(XP,YP,ZP,XJ,YJ,ZJ,DIJ,WGHT,WGRAD)
               WGRDX=WGRAD(1)*ALPHAI+WGHT*ALPHAXI
               WGRDY=WGRAD(2)*ALPHAI+WGHT*ALPHAYI
               WGRDZ=WGRAD(3)*ALPHAI+WGHT*ALPHAZI
               VJ=XSPHR(8,NN)/MAX(EM20,RHOJ)
               VXJ =XSPHR(9,NN)+DT12*ASPHR(1,NN)
               VYJ =XSPHR(10,NN)+DT12*ASPHR(2,NN)
               VZJ =XSPHR(11,NN)+DT12*ASPHR(3,NN)
               VJX=VJ*(VXJ-VX)
               VJY=VJ*(VYJ-VY)
               VJZ=VJ*(VZJ-VZ)
               DXX=DXX+VJX*WGRDX
               DXY=DXY+VJX*WGRDY
               DXZ=DXZ+VJX*WGRDZ
               DYX=DYX+VJY*WGRDX
               DYY=DYY+VJY*WGRDY
               DYZ=DYZ+VJY*WGRDZ
               DZX=DZX+VJZ*WGRDX
               DZY=DZY+VJZ*WGRDY
               DZZ=DZZ+VJZ*WGRDZ
             ENDIF
           ENDIF
          ENDDO
C------           
          ELSEIF(IPPV<0)THEN! fin traitement IPPV > 0
c traitement IPPV negatif on utilise infos recuperres ds routine comm  
           DXX = DSPHR(1,-IPPV)
           DXY = DSPHR(2,-IPPV)
           DXZ = DSPHR(3,-IPPV)
           DYX = DSPHR(4,-IPPV)
           DYY = DSPHR(5,-IPPV)
           DYZ = DSPHR(6,-IPPV)
           DZX = DSPHR(7,-IPPV)
           DZY = DSPHR(8,-IPPV)
           DZZ = DSPHR(9,-IPPV)
           VX = DSPHR(10,-IPPV)
           VY = DSPHR(11,-IPPV)
           VZ = DSPHR(12,-IPPV)
           XP=XSPHR(3,-IPPV)
           YP=XSPHR(4,-IPPV)
           ZP=XSPHR(5,-IPPV)               
c          ELSE ! cas IPPV=0 error
c            print*,'IPPV nul ERROR'
          ENDIF! fin traitement IPPV < 0

          VX=VX+(DXX*(XI-XP)+DXY*(YI-YP)+DXZ*(ZI-ZP))
          VY=VY+(DYX*(XI-XP)+DYY*(YI-YP)+DYZ*(ZI-ZP))
          VZ=VZ+(DZX*(XI-XP)+DZY*(YI-YP)+DZZ*(ZI-ZP))

          PS=VX*VNORMAL(1,N)+VY*VNORMAL(2,N)+VZ*VNORMAL(3,N)

          IF(PS<0.)THEN
C          impose une vitesse sortante uniquement.
           VX=VX-PS*VNORMAL(1,N)
           VY=VY-PS*VNORMAL(2,N)
           VZ=VZ-PS*VNORMAL(3,N)
          ENDIF
          UX=V(1,INOD)+A(1,INOD)*DT12
          UY=V(2,INOD)+A(2,INOD)*DT12
          UZ=V(3,INOD)+A(3,INOD)*DT12

          VT=VX*VX+VY*VY+VZ*VZ
          TFEXTT=TFEXTT+HALF*MS(INOD)*(VT-(UX*UX+UY*UY+UZ*UZ))
          A(1,INOD)=(VX-V(1,INOD))*DTINV
          A(2,INOD)=(VY-V(2,INOD))*DTINV
          A(3,INOD)=(VZ-V(3,INOD))*DTINV
         ENDIF
      ENDDO
C-------------------------------------------
#include "atomic.inc"
       TFEXT=TFEXT+TFEXTT
#include "atomend.inc"
C-------------------------------------------
      IF(NSPMD>1)THEN
          DEALLOCATE(ASPHR,DSPHR)
      END IF
C-------------------------------------------
      RETURN
      END
