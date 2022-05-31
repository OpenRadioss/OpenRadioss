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
Chd|  SPONOF1                       source/elements/sph/sponof1.F 
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPHREQS                       source/elements/sph/sphreq.F  
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPONOF1(X ,V       ,D       ,MS      ,SPBUF   ,
     2             ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,NPC     ,
     3             PLD     ,IPARG   ,ELBUF_TAB,ISPHIO  ,VSPHIO  ,
     4             PM      ,IPART   ,IPARTSP ,IGRSURF ,
     5             LPRTSPH ,LONFSPH ,MWA     ,WA      ,VNORMAL ,
     6             XDP, IBUFSSG_IO  ,OFF_SPH_R2R)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "tabsiz_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
#include      "rad2r_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),NPC(*),
     .        IPARG(NPARG,*),ISPHIO(NISPHIO,*),IPART(LIPART1,*),
     .        LPRTSPH(2,0:NPART),LONFSPH(*),
     .        MWA(*), IBUFSSG_IO(SIBUFSSG_IO),IPARTSP(*),OFF_SPH_R2R(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*) ,V(3,*) ,D(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,
     .   PLD(*) ,VSPHIO(*) ,PM(NPROPM,*) ,WA(*),
     .   VNORMAL(3,*)
      DOUBLE PRECISION
     .   XDP(3,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION (NGROUP) :: ELBUF_TAB
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITYPE, IVAD,IX, N2ENTER,
     .        II,IPT,JJ,NPF,
     .        IFVITS,IFDENS,IFPRES,IFENER,
     .        INACTIV,J,N,INOD,IACTIVE,
     .        ISU,NSEG,IN1,IN2,IN3,IN4,
     .        IMAT,IPROP,IPRT,NEL,KAD,NG,K,
     .        KP,NP2SORT,NBAND,
     .        KNSX,KISX,KNSY,KISY,KNSZ,KISZ,
     .        KNPX,KIPX,KNPY,KIPY,KNPZ,KIPZ,KVNORM,KXPROJ,KF,
     .        IUN,IAD2,NSEGB
C     REAL ou REAL*8
      my_real
     .       PENTV,VT,VN,VV,
     .       PENTR,RHON,RHO0,PENTP,PN,PSHFT,PENTE,EN,EI,
     .       T05,SSEG,MP,TFEXTT,VOLO,
     .       X1,X2,X3,X4,XP,Y1,Y2,Y3,Y4,YP,Z1,Z2,Z3,Z4,ZP,
     .       X12,Y12,Z12,X23,Y23,Z23,X31,Y31,Z31,NX,NY,NZ,NN,
     .       XI,YI,ZI,DD,DIAM,DMAX,DPS,VOL,BID
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
      my_real
     .         GET_U_GEO
      EXTERNAL GET_U_GEO
      DATA IUN/1/
C-----------------------------------------------
      TFEXTT=ZERO
      BID=ZERO
C-------
C     reset active particles states.
      DO I=1,NSPHIO
       IPRT   =ISPHIO(2,I)
C      loop over active particles related to IPRT.
       DO IACTIVE=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
        N=LONFSPH(IACTIVE)
        KXSP(2,N)=MOD(KXSP(2,N),NGROUP+1)
       ENDDO
      ENDDO
C-----------------------------------------------
      DO I=1,NSPHIO
      ITYPE=ISPHIO(1,I)
      IF(ITYPE.EQ.1)THEN
       IPRT   =ISPHIO(2,I)
C------
C      general inlet.
       IVAD=ISPHIO(4,I)
       T05=TT - HALF*DT1
C
       IFVITS=ISPHIO(8,I)
       VN    =ZERO
       IF(IFVITS.NE.0)THEN
        NPF = (NPC(IFVITS+1)-NPC(IFVITS))/2
        II  = NPC(IFVITS)
        IF (T05.LE.PLD(II)) THEN
          PENTV=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
          VN   =PLD(II+1)+PENTV*(T05-PLD(II))
        ELSEIF (T05.GE.PLD(II+2*(NPF-1))) THEN
          JJ=II+2*(NPF-1)
          PENTV=(PLD(JJ+1)-PLD(JJ-1))/(PLD(JJ)-PLD(JJ-2))
          VN   =PLD(JJ+1)+MAX(-PLD(JJ+1),PENTV*(T05-PLD(JJ)))
        ELSE
          DO IPT=1,NPF-1
           IF (PLD(II).LE.T05.AND.T05.LE.PLD(II+2)) THEN
            PENTV=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
            VN   =PLD(II+1)+PENTV*(T05-PLD(II))
            GOTO 20
           ENDIF
           II=II+2
          ENDDO
 20       CONTINUE
        ENDIF
       ENDIF
C------
       VV =ABS(VN)
       VT =VV*VV
C------
       IFDENS=ISPHIO(5,I)
       IF(IFDENS.EQ.0)THEN
        RHON=VSPHIO(IVAD)
       ELSE
        NPF = (NPC(IFDENS+1)-NPC(IFDENS))/2
        II  = NPC(IFDENS)
        IF (TT.LE.PLD(II)) THEN
          PENTR=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
          RHON =PLD(II+1)+PENTR*(TT-PLD(II))
        ELSEIF (TT.GE.PLD(II+2*(NPF-1))) THEN
          JJ=II+2*(NPF-1)
          PENTR=(PLD(JJ+1)-PLD(JJ-1))/(PLD(JJ)-PLD(JJ-2))
          RHON =PLD(JJ+1)+MAX(-PLD(JJ+1),PENTR*(TT-PLD(JJ)))
        ELSE
          DO IPT=1,NPF-1
           IF (PLD(II).LE.TT.AND.TT.LE.PLD(II+2)) THEN
            PENTR=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
            RHON =PLD(II+1)+PENTR*(TT-PLD(II))
            GOTO 30
           ENDIF
           II=II+2
          ENDDO
 30       CONTINUE
        ENDIF
        RHON=RHON*VSPHIO(IVAD)
       ENDIF
C------
       IFENER=ISPHIO(7,I)
       IF(IFENER.EQ.0)THEN
        EN=VSPHIO(IVAD+2)
       ELSE
        NPF = (NPC(IFENER+1)-NPC(IFENER))/2
        II  = NPC(IFENER)
        IF (TT.LE.PLD(II)) THEN
          PENTE=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
          EN   =PLD(II+1)+PENTE*(TT-PLD(II))
        ELSEIF (TT.GE.PLD(II+2*(NPF-1))) THEN
          JJ=II+2*(NPF-1)
          PENTE=(PLD(JJ+1)-PLD(JJ-1))/(PLD(JJ)-PLD(JJ-2))
          EN   =PLD(JJ+1)+MAX(-PLD(JJ+1),PENTE*(TT-PLD(JJ)))
        ELSE
          DO IPT=1,NPF-1
           IF (PLD(II).LE.TT.AND.TT.LE.PLD(II+2)) THEN
            PENTE=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
            EN   =PLD(II+1)+PENTE*(TT-PLD(II))
            GOTO 50
           ENDIF
           II=II+2
          ENDDO
 50       CONTINUE
        ENDIF
        EN=EN*VSPHIO(IVAD+2)
       ENDIF
C------
       IPROP=IPART(2,IPRT)
       DIAM =GET_U_GEO(6,IPROP)
       IMAT =IPART(1,IPRT)
C      specific energy related to V0=MP/RHO0.
       RHO0 =PM(1 ,IMAT)
       EI   =EN*(RHO0/RHON)
       
c default value        
       IF(DIAM.EQ.ZERO) THEN
         MP  = GET_U_GEO(1,IPROP)
         VOL = MP/RHO0
         DIAM= (SQR2*VOL)**THIRD
       END IF             
C------
C      boite englobante / surface entiere.
       XBMIN =EP20
       YBMIN =EP20
       ZBMIN =EP20
       XBMAX=-EP20
       YBMAX=-EP20
       ZBMAX=-EP20       
C------
       ISU =ISPHIO(3,I)
c NSEGB on split surface (IBUFSSG)
       NSEGB=IGRSURF(ISU)%NSEG
c NSEG on complete surface (IBUFSSG_IO)
       NSEG=ISPHIO(10,I)
c IAD on split surface (IBUFSSG)
c IAD2 on complete surface (IBUFSSG_IO)
       IAD2 =ISPHIO(11,I)
       DMAX=VSPHIO(IVAD+3)

c complete surface is used (IBUFSSG_IO) as in sponof2
c call of SPHREQS with IBUFSSG_IO
       DO J=0,NSEG-1
          IN1=IBUFSSG_IO(IAD2+NIBSPH*J  )
          IN2=IBUFSSG_IO(IAD2+NIBSPH*J+1)
          IN3=IBUFSSG_IO(IAD2+NIBSPH*J+2)
          IN4=IBUFSSG_IO(IAD2+NIBSPH*J+3)
          XP=X(1,IN1)
          YP=X(2,IN1)
          ZP=X(3,IN1)
          XBMIN=MIN(XBMIN,XP)
          YBMIN=MIN(YBMIN,YP)
          ZBMIN=MIN(ZBMIN,ZP)
          XBMAX=MAX(XBMAX,XP)
          YBMAX=MAX(YBMAX,YP)
          ZBMAX=MAX(ZBMAX,ZP)
          XP=X(1,IN2)
          YP=X(2,IN2)
          ZP=X(3,IN2)
          XBMIN=MIN(XBMIN,XP)
          YBMIN=MIN(YBMIN,YP)
          ZBMIN=MIN(ZBMIN,ZP)
          XBMAX=MAX(XBMAX,XP)
          YBMAX=MAX(YBMAX,YP)
          ZBMAX=MAX(ZBMAX,ZP)
          XP=X(1,IN3)
          YP=X(2,IN3)
          ZP=X(3,IN3)
          XBMIN=MIN(XBMIN,XP)
          YBMIN=MIN(YBMIN,YP)
          ZBMIN=MIN(ZBMIN,ZP)
          XBMAX=MAX(XBMAX,XP)
          YBMAX=MAX(YBMAX,YP)
          ZBMAX=MAX(ZBMAX,ZP)
          IF(IN4.NE.IN3)THEN
           XP=X(1,IN4)
           YP=X(2,IN4)
           ZP=X(3,IN4)
           XBMIN=MIN(XBMIN,XP)
           YBMIN=MIN(YBMIN,YP)
           ZBMIN=MIN(ZBMIN,ZP)
           XBMAX=MAX(XBMAX,XP)
           YBMAX=MAX(YBMAX,YP)
           ZBMAX=MAX(ZBMAX,ZP)
          ENDIF
       ENDDO
       XBMIN=XBMIN-DMAX
       YBMIN=YBMIN-DMAX
       ZBMIN=ZBMIN-DMAX
       XBMAX=XBMAX+DMAX
       YBMAX=YBMAX+DMAX
       ZBMAX=ZBMAX+DMAX
C------
       NP2SORT=0
       DO IACTIVE=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
        N=LONFSPH(IACTIVE)
        INOD=KXSP(3,N)
        XI=X(1,INOD)
        YI=X(2,INOD)
        ZI=X(3,INOD)
        IF(     XI.GT.XBMIN.AND.YI.GT.YBMIN.AND.ZI.GT.ZBMIN
     .     .AND.XI.LT.XBMAX.AND.YI.LT.YBMAX.AND.ZI.LT.ZBMAX)THEN
         NP2SORT=NP2SORT+1
         MWA(NP2SORT)=IACTIVE
        ENDIF
       ENDDO
C------
C      bucket.
       DBUCS=DMAX
       DO J=1,NSEG
          IN1=IBUFSSG_IO(IAD2+NIBSPH*(J-1)  )
          IN2=IBUFSSG_IO(IAD2+NIBSPH*(J-1)+1)
          IN3=IBUFSSG_IO(IAD2+NIBSPH*(J-1)+2)
          IN4=IBUFSSG_IO(IAD2+NIBSPH*(J-1)+3)
          X1=X(1,IN1)
          Y1=X(2,IN1)
          Z1=X(3,IN1)
          X2=X(1,IN2)
          Y2=X(2,IN2)
          Z2=X(3,IN2)
          X3=X(1,IN3)
          Y3=X(2,IN3)
          Z3=X(3,IN3)
          DBUCS=MAX(DBUCS,ABS(X1-X2))
          DBUCS=MAX(DBUCS,ABS(Y1-Y2))
          DBUCS=MAX(DBUCS,ABS(Z1-Z2))
          DBUCS=MAX(DBUCS,ABS(X2-X3))
          DBUCS=MAX(DBUCS,ABS(Y2-Y3))
          DBUCS=MAX(DBUCS,ABS(Z2-Z3))
          DBUCS=MAX(DBUCS,ABS(X3-X1))
          DBUCS=MAX(DBUCS,ABS(Y3-Y1))
          DBUCS=MAX(DBUCS,ABS(Z3-Z1))
          IF(IN4.NE.IN3)THEN
           X4=X(1,IN4)
           Y4=X(2,IN4)
           Z4=X(3,IN4)
          ELSE
           X4=X3
           Y4=Y3
           Z4=Z3
          ENDIF
          DBUCS=MAX(DBUCS,ABS(X1-X4))
          DBUCS=MAX(DBUCS,ABS(Y1-Y4))
          DBUCS=MAX(DBUCS,ABS(Z1-Z4))
          DBUCS=MAX(DBUCS,ABS(X2-X4))
          DBUCS=MAX(DBUCS,ABS(Y2-Y4))
          DBUCS=MAX(DBUCS,ABS(Z2-Z4))
          DBUCS=MAX(DBUCS,ABS(X3-X4))
          DBUCS=MAX(DBUCS,ABS(Y3-Y4))
          DBUCS=MAX(DBUCS,ABS(Z3-Z4))
       ENDDO
C      if more than 1 box, each box size >= DBUCS
       NBOX =MAX(IUN,INT((XBMAX-XBMIN)/DBUCS))
       NBOY =MAX(IUN,INT((YBMAX-YBMIN)/DBUCS))
       NBOZ =MAX(IUN,INT((ZBMAX-ZBMIN)/DBUCS))
       NBAND=MAX(NBOX,NBOY,NBOZ)+1
       KNSX=NUMSPH+1
       KISX=KNSX+NBAND+1
       KNSY=KISX+4*NSEG
       KISY=KNSY+NBAND+1
       KNSZ=KISY+4*NSEG
       KISZ=KNSZ+NBAND+1
       KNPX=KISZ+4*NSEG
       KIPX=KNPX+NBAND+1
       KNPY=KIPX+3*NP2SORT
       KIPY=KNPY+NBAND+1
       KNPZ=KIPY+3*NP2SORT
       KIPZ=KNPZ+NBAND+1
       KVNORM=KIPZ+3*NP2SORT
C      KF    =KVNORM+3*NUMSPH
       CALL SPHREQS(NSEG ,IBUFSSG_IO(IAD2) ,X ,NP2SORT ,MWA ,
     2    LONFSPH  ,KXSP     ,WA(KNSX) ,WA(KISX) ,WA(KNSY) ,
     3    WA(KISY) ,WA(KNSZ) ,WA(KISZ) ,WA(KNPX) ,WA(KIPX) ,
     4    WA(KNPY) ,WA(KIPY) ,WA(KNPZ) ,WA(KIPZ) ,WA       ,
     5    WA(KVNORM),BID     ,BID      ,V        ,SPBUF    ,
     6    ISPHIO(1,I))
C------
C      check active particles within distance / re-impose kinematic condition.
       DO KP=1,NP2SORT
        DPS=WA(KP)
        IF(DPS.GE.0..AND.DPS.LT.DMAX)THEN
         IACTIVE=MWA(KP)
         N=LONFSPH(IACTIVE)
         INOD=KXSP(3,N)
         TFEXTT=TFEXTT + HALF*MS(INOD)*(VT-
     . (V(1,INOD)*V(1,INOD)+V(2,INOD)*V(2,INOD)+V(3,INOD)*V(3,INOD)))
         VNORMAL(1,N)=-WA(KVNORM+3*(N-1))
         VNORMAL(2,N)=-WA(KVNORM+3*(N-1)+1)
         VNORMAL(3,N)=-WA(KVNORM+3*(N-1)+2)
         V(1,INOD)=VN*VNORMAL(1,N)
         V(2,INOD)=VN*VNORMAL(2,N)
         V(3,INOD)=VN*VNORMAL(3,N)
         NG=MOD(KXSP(2,N),NGROUP+1)
         KXSP(2,N) =NG+I*(NGROUP+1)
        ENDIF
       ENDDO
C------
C      particles to enter.
       MP   =GET_U_GEO(1,IPROP)
       N2ENTER=0
       IX=IVAD+4
c loop on NSEGB (segments of split surface IBUFSSG) to avoid
c multiple count of cells to enter and to activate in SPMD
       DO J=0,(NSEGB-1)
        SSEG =VSPHIO(IX+1)
        VSPHIO(IX)=VSPHIO(IX)+RHON*SSEG*VV*DT1
        IF(VSPHIO(IX).GE.MP)THEN
         N2ENTER=N2ENTER+1
        ENDIF
        IX=IX+2
       ENDDO
C------
       IF(N2ENTER.GT.0)THEN
        ISPHBUC=1
C       find N2ENTER inactive particles related to same part.
        INACTIV=LPRTSPH(2,IPRT)-LPRTSPH(1,IPRT)
        IF(INACTIV.LT.N2ENTER)THEN
          CALL ANCMSG(MSGID=175,ANMODE=ANINFO,
     .            I1=ISPHIO(NISPHIO,I))
          CALL ARRET(2)
          RETURN
        ENDIF
C------
C       enter N2ENTER particles.
        IX=IVAD+4
        IACTIVE=LPRTSPH(1,IPRT)
        DO J=0,(NSEGB-1)
        IF(VSPHIO(IX).GE.MP)THEN
C--------
         IACTIVE=IACTIVE+1
          N     =LONFSPH(IACTIVE)
C         chainage.
          LPRTSPH(1,IPRT)=IACTIVE
C--------
          IN1=IGRSURF(ISU)%NODES(J+1,1)
          IN2=IGRSURF(ISU)%NODES(J+1,2)
          IN3=IGRSURF(ISU)%NODES(J+1,3)
          IN4=IGRSURF(ISU)%NODES(J+1,4)
c          IN1=IBUFSSG_IO(IAD2+NIBSPH*(J-1)  )
c          IN2=IBUFSSG_IO(IAD2+NIBSPH*(J-1)+1)
c          IN3=IBUFSSG_IO(IAD2+NIBSPH*(J-1)+2)
c          IN4=IBUFSSG_IO(IAD2+NIBSPH*(J-1)+3)
          X1=X(1,IN1)
          Y1=X(2,IN1)
          Z1=X(3,IN1)
          X2=X(1,IN2)
          Y2=X(2,IN2)
          Z2=X(3,IN2)
          X3=X(1,IN3)
          Y3=X(2,IN3)
          Z3=X(3,IN3)
         IF(IN4.NE.IN3)THEN
          X4=X(1,IN4)
          Y4=X(2,IN4)
          Z4=X(3,IN4)
          XP=FOURTH*(X1+X2+X3+X4)
          YP=FOURTH*(Y1+Y2+Y3+Y4)
          ZP=FOURTH*(Z1+Z2+Z3+Z4)	  
         ELSE
          X4=X3
          Y4=Y3
          Z4=Z3
          XP=THIRD*(X1+X2+X3)
          YP=THIRD*(Y1+Y2+Y3)
          ZP=THIRD*(Z1+Z2+Z3)
         ENDIF
C--------
         X12=HALF*(X2+X3-X1-X4)
         Y12=HALF*(Y2+Y3-Y1-Y4)
         Z12=HALF*(Z2+Z3-Z1-Z4)
         X23=HALF*(X3+X4-X1-X2)
         Y23=HALF*(Y3+Y4-Y1-Y2)
         Z23=HALF*(Z3+Z4-Z1-Z2)	 
         NX = Y12*Z23-Z12*Y23
         NY =-X12*Z23+Z12*X23
         NZ = X12*Y23-Y12*X23
         NN =ONE/MAX(EM20,SQRT(NX*NX+NY*NY+NZ*NZ))
         NX=NN*NX
         NY=NN*NY
         NZ=NN*NZ
C--------
         INOD=KXSP(3,N)
         D(1,INOD)=XP-X(1,INOD)
         D(2,INOD)=YP-X(2,INOD)
         D(3,INOD)=ZP-X(3,INOD)
         X(1,INOD)=XP
         X(2,INOD)=YP
         X(3,INOD)=ZP
         IF (IRESP.EQ.1) THEN
           XDP(1,INOD)=XP
           XDP(2,INOD)=YP
           XDP(3,INOD)=ZP
         END IF
         TFEXTT=TFEXTT+HALF*MP*VT
         VNORMAL(1,N)=NX
         VNORMAL(2,N)=NY
         VNORMAL(3,N)=NZ
         V(1,INOD)=VN*NX
         V(2,INOD)=VN*NY
         V(3,INOD)=VN*NZ
         IF (R2R_SIU.NE.0)  OFF_SPH_R2R(INOD) = 2
C--------
          NG=-KXSP(2,N)
          IPARG(8,NG) =0
          IPARG(10,NG)=IPARG(10,NG)+1
          CALL INITBUF (IPARG    ,NG      ,                   
     2       MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,    
     3       NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4       JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5       NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,    
     6       IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7       ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                      
          GBUF => ELBUF_TAB(NG)%GBUF
          K=N-NFT
          VOLO=GBUF%VOL(K)
          TFEXTT=TFEXTT+EI*VOLO
          GBUF%EINT(K) =EI
          GBUF%OFF(K)  =ONE
          GBUF%RHO(K)  =RHON
          SPBUF(1,N)=DIAM
          SPBUF(2,N) =RHON
          SPBUF(12,N)=MS(INOD)
          KXSP(2,N) =NG+I*(NGROUP+1)
C-------
C        reset MASS FLOW for this segment.
         VSPHIO(IX)=VSPHIO(IX)-MP
        ENDIF
        IX=IX+2
        ENDDO
       ENDIF
      ENDIF
      ENDDO
C-------------------------------------------
#include "atomic.inc"
       TFEXT=TFEXT+TFEXTT
#include "atomend.inc"
C-------------------------------------------
      RETURN
      END
