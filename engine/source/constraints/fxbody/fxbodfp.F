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
Chd|  FXBODFP1                      source/constraints/fxbody/fxbodfp.F
Chd|-- called by -----------
Chd|        FXBYFOR                       source/constraints/fxbody/fxbyfor.F
Chd|-- calls ---------------
Chd|        FXBDEPLA                      source/constraints/fxbody/fxbdepla.F
Chd|        FXBMAJP1                      source/constraints/fxbody/fxbodfp.F
Chd|        FXBSGMAJ                      source/constraints/fxbody/fxbsgmaj.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FXBODFP1(FXBIPM , FXBRPM, FXBNOD  , FXBMOD  , FXBDEP ,
     .                    FXBVIT , FXBACC, A       , AR      , NME    ,
     .                    NMOD   , ITN   , FXBELM  , FXBSIG  , ELBUF  ,
     .                    PARTSAV, X     , D       , IPARG   , NFX    ,
     .                    NSN    , MFEXT , IAD_ELEM, FR_ELEM , NSNT   ,
     .                    FSKYFXB, IADN  , IADSKY  ,ELBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(*), FXBNOD(*), NME, NMOD, ITN, FXBELM(*),
     .        IPARG(NPARG,*), NFX, NSN, IAD_ELEM(2,*), FR_ELEM(*),
     .        NSNT, IADN, IADSKY(*)
      my_real
     .        FXBRPM(*), FXBMOD(*), FXBDEP(*), FXBVIT(*), FXBACC(*),
     .        A(3,*), AR(3,*), FXBSIG(*), ELBUF(*), PARTSAV(NPSAV,*),
     .        X(3,*), D(3,*), MFEXT(*), FSKYFXB(NSNT,*)
      TYPE (ELBUF_STRUCT_), DIMENSION (NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFILE, LMOD, ISH, NELS, NELC, NELT, NELP, NELTG, LVSIG,
     .        IRCS, DN, IRCM, I, IAD, II, N, IFAC(NUMNOD), J, JJ
      my_real
     .        RT(9)
C=======================================================================
C
      CALL FXBMAJP1(FXBDEP, FXBVIT, FXBACC, FXBRPM, DT1 ,
     .              NME   , NMOD  , RT    )
C
      IFILE=FXBIPM(29)
      IF (IFILE==0) THEN
         LMOD=FXBIPM(3)*6
      ELSEIF (IFILE==1) THEN
         LMOD=NSN*6
      ENDIF
      ISH=FXBIPM(16)
C
      NELS=FXBIPM(21)
      NELC=FXBIPM(22)
      NELT=FXBIPM(34)
      NELP=FXBIPM(35)
      NELTG=FXBIPM(23)
      LVSIG=NELS*7+NELC*10+NELT*2+NELP*8+NELTG*10
      IRCS=FXBIPM(31)
      CALL FXBSGMAJ(ELBUF,      FXBELM,  FXBSIG, FXBDEP, FXBIPM,
     .              FXBRPM(15), PARTSAV, RT    , ITN   , IPARG ,
     .              NFX       , LVSIG  , IRCS  ,ELBUF_TAB)
      IF (ITN==0) THEN
         DN=FXBIPM(3)-FXBIPM(18)
         IRCM=FXBIPM(30)
         CALL FXBDEPLA(FXBDEP, FXBRPM, X,   D,    DN,   
     .                 NSN,    FXBNOD, NME, NMOD, FXBMOD,
     .                 ISH   , IFILE , NFX, IRCM)
      ENDIF
C-----------------------------------------------
C External modal forces
C-----------------------------------------------
      DO I=1,NUMNOD
         IFAC(I)=1
      ENDDO
      IF (NSPMD>1) THEN
         DO I=1,NSPMD
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               JJ=FR_ELEM(J)
               IFAC(JJ)=IFAC(JJ)+1
            ENDDO
         ENDDO
      ENDIF
C
      IF (IPARIT==0) THEN
         DO I=1,12
            MFEXT(I)=ZERO
            IAD=(I-1)*LMOD
            DO II=1,NSN
               N=FXBNOD(II)
               MFEXT(I)=MFEXT(I)+(A(1,N)*FXBMOD(IAD+1)
     .                           +A(2,N)*FXBMOD(IAD+2)
     .                           +A(3,N)*FXBMOD(IAD+3))/IFAC(N)
               IAD=IAD+6
            ENDDO
         ENDDO
         IF (ISH>0) THEN
            DO I=13,NME
               MFEXT(I)=ZERO
               IAD=(I-1)*LMOD
               DO II=1,NSN
                  N=FXBNOD(II)
                  MFEXT(I)=MFEXT(I)+(AR(1,N)*FXBMOD(IAD+4)
     .                              +AR(2,N)*FXBMOD(IAD+5)
     .                              +AR(3,N)*FXBMOD(IAD+6))/IFAC(N)
                  IAD=IAD+6
               ENDDO
            ENDDO
         ENDIF      
C
         IF (NMOD>0) THEN
            DO I=1,NMOD
               MFEXT(NME+I)=ZERO
               IAD=(NME+I-1)*LMOD
               DO II=1,NSN
                  N=FXBNOD(II)
                  MFEXT(NME+I)=MFEXT(NME+I)+
     .    (A(1,N)*(FXBRPM(2)*FXBMOD(IAD+1)+FXBRPM(3)*FXBMOD(IAD+2)+
     .             FXBRPM(4)*FXBMOD(IAD+3))
     .    +A(2,N)*(FXBRPM(5)*FXBMOD(IAD+1)+FXBRPM(6)*FXBMOD(IAD+2)+
     .             FXBRPM(7)*FXBMOD(IAD+3))
     .    +A(3,N)*(FXBRPM(8)*FXBMOD(IAD+1)+FXBRPM(9)*FXBMOD(IAD+2)+
     .             FXBRPM(10)*FXBMOD(IAD+3))
     .    +AR(1,N)*(FXBRPM(2)*FXBMOD(IAD+4)+FXBRPM(3)*FXBMOD(IAD+5)+
     .              FXBRPM(4)*FXBMOD(IAD+6))
     .    +AR(2,N)*(FXBRPM(5)*FXBMOD(IAD+4)+FXBRPM(6)*FXBMOD(IAD+5)+
     .              FXBRPM(7)*FXBMOD(IAD+6))
     .    +AR(3,N)*(FXBRPM(8)*FXBMOD(IAD+4)+FXBRPM(9)*FXBMOD(IAD+5)+
     .              FXBRPM(10)*FXBMOD(IAD+6)))/IFAC(N)
                  IAD=IAD+6
               ENDDO
            ENDDO
         ENDIF
      ELSE
         DO I=1,NSN
            FSKYFXB(IADN+I,1)=IADSKY(I)
         ENDDO
C            
         DO I=1,12
            IAD=(I-1)*LMOD
            DO II=1,NSN
               N=FXBNOD(II)
               FSKYFXB(IADN+II,1+I)=A(1,N)*FXBMOD(IAD+1)
     .                           +A(2,N)*FXBMOD(IAD+2)
     .                           +A(3,N)*FXBMOD(IAD+3)
               IAD=IAD+6
            ENDDO
         ENDDO
         IF (ISH>0) THEN
            DO I=13,NME
               IAD=(I-1)*LMOD
               DO II=1,NSN
                  N=FXBNOD(II)
                  FSKYFXB(IADN+II,1+I)=AR(1,N)*FXBMOD(IAD+4)
     .                                +AR(2,N)*FXBMOD(IAD+5)
     .                                +AR(3,N)*FXBMOD(IAD+6)
                  IAD=IAD+6
               ENDDO
            ENDDO
         ENDIF      
C
         IF (NMOD>0) THEN
            DO I=1,NMOD
               IAD=(NME+I-1)*LMOD
               DO II=1,NSN
                  N=FXBNOD(II)
                  FSKYFXB(IADN+II,1+NME+I)=
     .     A(1,N)*(FXBRPM(2)*FXBMOD(IAD+1)+FXBRPM(3)*FXBMOD(IAD+2)+
     .             FXBRPM(4)*FXBMOD(IAD+3))
     .    +A(2,N)*(FXBRPM(5)*FXBMOD(IAD+1)+FXBRPM(6)*FXBMOD(IAD+2)+
     .             FXBRPM(7)*FXBMOD(IAD+3))
     .    +A(3,N)*(FXBRPM(8)*FXBMOD(IAD+1)+FXBRPM(9)*FXBMOD(IAD+2)+
     .             FXBRPM(10)*FXBMOD(IAD+3))
     .    +AR(1,N)*(FXBRPM(2)*FXBMOD(IAD+4)+FXBRPM(3)*FXBMOD(IAD+5)+
     .              FXBRPM(4)*FXBMOD(IAD+6))
     .    +AR(2,N)*(FXBRPM(5)*FXBMOD(IAD+4)+FXBRPM(6)*FXBMOD(IAD+5)+
     .              FXBRPM(7)*FXBMOD(IAD+6))
     .    +AR(3,N)*(FXBRPM(8)*FXBMOD(IAD+4)+FXBRPM(9)*FXBMOD(IAD+5)+
     .              FXBRPM(10)*FXBMOD(IAD+6))
                  IAD=IAD+6
               ENDDO
            ENDDO
         ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE FXBODFP1
Chd|====================================================================
Chd|  FXBODFP2                      source/constraints/fxbody/fxbodfp.F
Chd|-- called by -----------
Chd|        FXBYFOR                       source/constraints/fxbody/fxbyfor.F
Chd|-- calls ---------------
Chd|        FXBMAJP2                      source/constraints/fxbody/fxbodfp.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FXBODFP2(FXBIPM , FXBRPM , FXBGLM, FXBCPM, FXBCPS ,
     .                    FXBLM  , FXBFLS , FXBDLS, FXBDEP, FXBVIT ,
     .                    NME    , NMOD   , MVN   , MCD   , SE     ,
     .                    SV     , FSAV   , FXBFP , TFEXT , FXBFC  ,
     .                    FXBGRVI, FXBGRVR, NLGRAV, IGRV  , NPC    ,
     .                    TF     , FXBGRP , TFGRAV, SENSOR_TAB, NSENSOR,
     .                    MFEXT  , AGRV   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER FXBIPM(*), NME, NMOD, FXBGRVI(*), NLGRAV, IGRV(NIGRV,*),
     .        NPC(*)
      my_real
     .        FXBRPM(*), FXBGLM(*), FXBCPM(*), FXBCPS(*), FXBLM(*),
     .        FXBFLS(*), FXBDLS(*), FXBDEP(*), FXBVIT(*), MVN(*), 
     .        MCD(NME,*), SE(*), SV(*), FSAV(*), FXBFP(*), TFEXT,
     .        FXBFC(*), FXBGRVR(*), TF(*), FXBGRP(*), TFGRAV,
     .        MFEXT(*), AGRV(LFACGRV,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IBLO, I, IADG, IADGR, IG, NL, IFUNC, ISENS, K, IIM, NMST,
     .        IAD, II, III, IIAD
      my_real
     .        MC(NME*NMOD), RC(NME*NMOD), TS, FI, ALPHA, BETA,
     .        MFINT(NME+NMOD), FDALPHA(NMOD), ENINT, DYDX,
     .        MFGRAV(NMOD+NME) 
      my_real FINTER 
      EXTERNAL FINTER
C
      IBLO=FXBIPM(28)
      IF (NMOD>0) CALL FXBMAJP2(MC,  RC,     MVN,    NME,   NMOD,
     .                             DT1, FXBCPM, FXBCPS, FXBLM, FXBRPM)
C-----------------------------------------------
C Projection of gravity forces
C-----------------------------------------------
      DO I=1,NME+NMOD
         MFGRAV(I)=ZERO
      ENDDO
C
      IF (NLGRAV>0) THEN
         IADG=0
         IADGR=0
         DO IG=1,NLGRAV
            NL=FXBGRVI(IADG+1)
            IFUNC=IGRV(3,NL)
            ISENS=0
            DO K=1,NSENSOR
               IF (IGRV(6,NL)==SENSOR_TAB(K)%SENS_ID) ISENS=K  ! do it in starter !!!
            ENDDO
            IF (ISENS==0) THEN
               TS=TT
            ELSE
               TS=TT-SENSOR_TAB(ISENS)%TSTART
               IF (TS<ZERO) CYCLE
            ENDIF
            IF (IFUNC > 0) THEN
              FI=AGRV(1,NL)*FINTER(IFUNC,TS,NPC,TF,DYDX)
            ELSE
              FI=AGRV(1,NL)
            ENDIF
            DO I=1,NME
               MFGRAV(I)=MFGRAV(I)+FI*FXBGRVR(IADGR+I)
            ENDDO
            IIM=0
            DO I=1,NMOD
               MFGRAV(NME+I)=MFGRAV(NME+I)
     .                      +FXBRPM(2)*FI*FXBGRVR(IADGR+NME+IIM+1)
     .                      +FXBRPM(3)*FI*FXBGRVR(IADGR+NME+IIM+2)
     .                      +FXBRPM(4)*FI*FXBGRVR(IADGR+NME+IIM+3)
     .                      +FXBRPM(5)*FI*FXBGRVR(IADGR+NME+IIM+4)
     .                      +FXBRPM(6)*FI*FXBGRVR(IADGR+NME+IIM+5)
     .                      +FXBRPM(7)*FI*FXBGRVR(IADGR+NME+IIM+6)
     .                      +FXBRPM(8)*FI*FXBGRVR(IADGR+NME+IIM+7)
     .                      +FXBRPM(9)*FI*FXBGRVR(IADGR+NME+IIM+8)
     .                      +FXBRPM(10)*FI*FXBGRVR(IADGR+NME+IIM+9)
               IIM=IIM+9
            ENDDO
            IADG=IADG+2+FXBGRVI(IADG+2)
            IADGR=IADGR+NME+9*NMOD
         ENDDO
      ENDIF
C-----------------------------------------------
C External forces work
C-----------------------------------------------
      TFGRAV=ZERO
      DO I=1,NME+NMOD
         TFGRAV=TFGRAV+HALF*(FXBGRP(I)+MFGRAV(I))*FXBVIT(I)*DT1
         MFEXT(I)=MFEXT(I)+MFGRAV(I)
         TFEXT=TFEXT+HALF*(FXBFP(I)+MFEXT(I))*FXBVIT(I)*DT1
         FXBGRP(I)=MFGRAV(I)
         FXBFP(I)=MFEXT(I)
      ENDDO
C-----------------------------------------------
C Internal modal forces
C-----------------------------------------------
      NMST=FXBIPM(5)
      ALPHA=FXBRPM(13)
      BETA=FXBRPM(14)
      IF (NMOD>0.AND.IBLO==0) THEN 
         DO I=1,NME
            MFINT(I)=ZERO
            IAD=NMOD*(I-1)
            DO II=1,NMOD
               MFINT(I)=MFINT(I)+RC(IAD+II)*FXBDEP(NME+II)
            ENDDO
         ENDDO
      ELSE
         DO I=1,NME
            MFINT(I)=ZERO
         ENDDO
      ENDIF
      IF (NMST>0) THEN
         DO I=1,NMST
            MFINT(NME+I)=ZERO
            FXBFC(I)=ZERO
            DO II=1,I-1
               IAD=(II-1)*(2*NMOD-II+2)/2+(I-II+1)
               MFINT(NME+I)=MFINT(NME+I)+FXBFLS(IAD)*FXBDEP(NME+II)
               FXBFC(I)=FXBFC(I)+FXBFLS(IAD)*BETA*FXBVIT(NME+II)
            ENDDO
            DO II=I,NMOD
               IAD=(I-1)*(2*NMOD-I+2)/2+(II-I+1)
               MFINT(NME+I)=MFINT(NME+I)+FXBFLS(IAD)*FXBDEP(NME+II)
               FXBFC(I)=FXBFC(I)+FXBFLS(IAD)*BETA*FXBVIT(NME+II)
            ENDDO
            FDALPHA(I)=ALPHA*FXBLM(I)*FXBVIT(NME+I)
         ENDDO
      ENDIF
      IF ((NMOD-NMST)>0) THEN
         DO I=1,NMOD-NMST
            III=NMST+I
            MFINT(NME+III)=ZERO
            FXBFC(III)=ZERO
            IF (NMST>0) THEN
               DO II=1,NMST
                  IAD=(II-1)*(2*NMOD-II+2)/2+(III-II+1)
                  MFINT(NME+III)=MFINT(NME+III)+FXBFLS(IAD)*
     .                           FXBDEP(NME+II)
                  FXBFC(III)=FXBFC(III)+FXBFLS(IAD)*BETA*FXBVIT(NME+II)
               ENDDO
            ENDIF
            MFINT(NME+III)=MFINT(NME+III)+FXBDLS(I)*FXBDEP(NME+III)
            FXBFC(III)=FXBFC(III)+FXBDLS(I)*BETA*FXBVIT(NME+III)
            FDALPHA(III)=ALPHA*FXBLM(III)*FXBVIT(NME+III)
         ENDDO
      ENDIF
      ENINT=ZERO
      IF (NMOD>0) THEN
         DO I=1,NMOD
            ENINT=ENINT+HALF*FXBDEP(NME+I)*MFINT(NME+I)
         ENDDO
      ENDIF
      FXBRPM(11)=ENINT
      FSAV(1)=ENINT
      FSAV(3)=TFEXT
C-----------------------------------------------
C Condensation of global unknowns
C-----------------------------------------------
      DO I=1,NME
         SE(I)=MFEXT(I)-MFINT(I)
      ENDDO
      IF (NMOD>0) THEN
         DO I=1,NMOD
            SV(I)=MFEXT(NME+I)-MFINT(NME+I)-FXBFC(I)-FDALPHA(I)
         ENDDO
         IF (IBLO==0) THEN
            DO I=1,NME
               IAD=NMOD*(I-1)
               DO II=1,NMOD
                  SE(I)=SE(I)-MVN(IAD+II)*SV(II)
               ENDDO
            ENDDO
         ENDIF
      ENDIF
      IF (IBLO==1) RETURN
      III=0
      DO I=1,NME
         DO II=I,NME
            III=III+1
            MCD(I,II)=FXBGLM(III)
            IF (I/=II) MCD(II,I)=MCD(I,II)
         ENDDO
      ENDDO
      IF (NMOD>0) THEN
         DO I=1,NME
            IAD=NMOD*(I-1)
            DO II=1,NME
               IIAD=NMOD*(II-1)
               DO III=1,NMOD
                  MCD(I,II)=MCD(I,II)-MVN(IAD+III)*MC(IIAD+III)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
C
      RETURN
      END SUBROUTINE FXBODFP2
Chd|====================================================================
Chd|  FXBMAJP1                      source/constraints/fxbody/fxbodfp.F
Chd|-- called by -----------
Chd|        FXBODFP1                      source/constraints/fxbody/fxbodfp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXBMAJP1(FXBDEP,FXBVIT,FXBACC,FXBRPM,DT1 ,
     .                    NME   ,NMOD  ,TMROT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NME,NMOD
      my_real
     .   FXBDEP(*),FXBVIT(*),FXBACC(*),FXBRPM(*),DT1,TMROT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II
      my_real
     .   DDEP(NME+NMOD)
C  
C-----------------------------------------------
C Update displacements and velocities
C-----------------------------------------------
      DO I=1,NME+NMOD
         FXBDEP(I)=FXBDEP(I)+DT1*FXBVIT(I)
         DDEP(I)=DT1*FXBVIT(I)
      ENDDO
C-----------------------------------------------
C Transfer matrix
C-----------------------------------------------
      DO I=1,3
         DO J=1,3
            FXBRPM(1+3*(I-1)+J)=FXBRPM(1+3*(I-1)+J)+
     .                      DDEP(3*(J-1)+I)-DDEP(9+I)
            TMROT(3*(J-1)+I)=FXBRPM(1+3*(I-1)+J)
         END DO
      END DO
C      
      RETURN
      END SUBROUTINE FXBMAJP1  
Chd|====================================================================
Chd|  FXBMAJP2                      source/constraints/fxbody/fxbodfp.F
Chd|-- called by -----------
Chd|        FXBODFP2                      source/constraints/fxbody/fxbodfp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXBMAJP2(MC , RC    , MVN   , NME  , NMOD  ,
     .                    DT1, FXBCPM, FXBCPS, FXBLM, FXBRPM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NME, NMOD
      my_real
     .        MC(*), RC(*), MVN(*), DT1, FXBCPM(*), FXBCPS(*), FXBLM(*),
     .        FXBRPM(*)            
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, J
      my_real
     .        ALPHA, FAC, TMROT(9)
C
      DO I=1,3
         DO J=1,3
            TMROT(3*(J-1)+I)=FXBRPM(1+3*(I-1)+J)
         ENDDO
      ENDDO
C-----------------------------------------------
C Mass and stiffness coupling
C-----------------------------------------------
      DO I=1,NME*NMOD
         RC(I)=ZERO
         MC(I)=ZERO
      END DO
C
      DO I=1,9
         II=NMOD*NME*(I-1)
         DO J=1,NME*NMOD
            MC(J)=MC(J)+FXBRPM(1+I)*FXBCPM(II+J)
            RC(J)=RC(J)+TMROT(I)*FXBCPS(II+J)
         END DO
      END DO
C
      ALPHA=FXBRPM(13)
      FAC=ONE+HALF*DT1*ALPHA
      DO I=1,NME
         DO J=1,NMOD
            II=NMOD*(I-1)+J
            MVN(II)=MC(II)/FXBLM(J)/FAC
         ENDDO
      ENDDO
C      
      RETURN
      END SUBROUTINE FXBMAJP2
