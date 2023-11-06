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
Chd|  SPONOF2                       source/elements/sph/sponof2.F
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F
Chd|        SPHREQS                       source/elements/sph/sphreq.F
Chd|        SPH_NODSEG                    source/elements/sph/sph_nodseg.F
Chd|        SPMD_SPHGETIMP                source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETVOIS_OFF           source/mpi/elements/spmd_sph.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F
Chd|        SPHBOX                        share/modules/sphbox.F
Chd|====================================================================
      SUBROUTINE SPONOF2(X ,V       ,D        ,MS      ,SPBUF   ,
     2             ITAB    ,KXSP    ,IXSP     ,NOD2SP  ,NPC     ,
     3             PLD     ,IPARG   ,ELBUF_TAB,ISPHIO  ,VSPHIO  ,
     4             PM      ,IPART   ,IPARTSP  ,IGRSURF ,
     5             LPRTSPH ,LONFSPH ,IWA      ,MWA     ,WA      ,
     6             VNORMAL ,SPHVELN ,XDP,IBUFSSG_IO    ,OFF_SPH_R2R)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INITBUF_MOD
        USE ELBUFDEF_MOD
        USE SPHBOX
        USE GROUPDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "scr05_c.inc"
#include      "tabsiz_c.inc"
#include      "scr17_c.inc"
#include      "rad2r_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),NPC(*),
     .          IPARG(NPARG,*),ISPHIO(NISPHIO,*),IPART(LIPART1,*),
     .          LPRTSPH(2,0:NPART),LONFSPH(*),
     .          IWA(*),MWA(*),IBUFSSG_IO(SIBUFSSG_IO),IPARTSP(*),OFF_SPH_R2R(*)
C     REAL
        my_real
     .     X(3,*) ,V(3,*) ,D(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,
     .     PLD(*) ,VSPHIO(*) ,PM(NPROPM,*),WA(*),
     .     VNORMAL(3,*), SPHVELN(2,*)
        DOUBLE PRECISION
     .     XDP(3,*)
        TYPE (ELBUF_STRUCT_), TARGET, DIMENSION (NGROUP) :: ELBUF_TAB
        TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, ITYPE, IVAD,
     .          II,IPT,JJ(6),NPF,
     .          IFVITSX,IFVITSY,IFVITSZ,IFDENS,IFPRES,IFENER,
     .          J,N,INOD,IACTIVE,
     .          ILASTON,NLASTON,I1STOFF,N1STOFF,
     .          ISU,NSEG,IN1,IN2,IN3,IN4,
     .          IMAT,IPROP,IPRT,NEL,KAD,NG,K,
     .          KP,NP2SORT,NBAND,
     .          KNSX,KISX,KNSY,KISY,KNSZ,KISZ,
     .          KNPX,KIPX,KNPY,KIPY,KNPZ,KIPZ,KVNORM,KF,
     .          IPPV,M,JNOD,KNOD,JK,NVOIS1,NVOIS2,NVOISS1,NVOISS2,
     .          IMPOSE,IUN,KK,I1,IAD2,
     .          NSEGB,IMPOSE2,INOD0,NN,OFF,TFLAG,NP2SORT_OLD,KOFF,IJ
C     REAL
        my_real
     .         TFEXTT,VOLO,
     .         X1,X2,X3,X4,XP,Y1,Y2,Y3,Y4,YP,Z1,Z2,Z3,Z4,ZP,
     .         XI,YI,ZI,XJ,YJ,ZJ,DI,DD,DMIN,DMAX,DPS,
     .         XG,YG,ZG,XX(12),XX_OLD(12),VMAX,VI,DPS_OLD,XI_OLD,
     .         YI_OLD,ZI_OLD,DT_OLD,VMAXS

        INTEGER, DIMENSION(:), ALLOCATABLE :: OFF_SPH, TAG_SPH
        TYPE(G_BUFEL_)  ,POINTER :: GBUF

        LOGICAL :: lBOOL
C-----------------------------------------------
        my_real
     .           GET_U_GEO
        EXTERNAL GET_U_GEO
        DATA IUN/1/
C-----------------------------------------------
        ALLOCATE(TAG_SPH(NSPHR))
        ALLOCATE(OFF_SPH(NUMSPH))
        OFF_SPH(1:NUMSPH) = ZERO
        TAG_SPH(1:NSPHR) = ZERO

        TFEXTT=ZERO
C-----------------------------------------------
        DO I=1,NSPHIO
          ITYPE=ISPHIO(1,I)
          VMAX = EM20
          IF(ITYPE/=1)THEN
C      outlets.
            IPRT   =ISPHIO(2,I)
            IVAD=ISPHIO(4,I)
            DO IACTIVE=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
              IWA(IACTIVE)=0
              IF (ITYPE==4) THEN
C            determination of max velocity
                N=LONFSPH(IACTIVE)
                INOD=KXSP(3,N)
                VI=SQRT(V(1,INOD)**2+V(2,INOD)**2+V(3,INOD)**2)
                VMAX = MAX(VMAX,VI)
              ENDIF
            ENDDO
            DT_OLD = VSPHIO(IVAD+14)
            IF (ITYPE==4) VSPHIO(IVAD+3)=VMAX*MAX(DT_OLD,DT12)
          ENDIF
        ENDDO
C-----------------------------------------------
        DO I=1,NSPHIO
          ITYPE=ISPHIO(1,I)
C-------
          IPRT   =ISPHIO(2,I)
          IF(LPRTSPH(1,IPRT)>LPRTSPH(2,IPRT-1))THEN
C-------
            IF ((ITYPE>1).AND.(ISPHIO(12,I)==0)) THEN
C-------
C      general outlets & silent boundary.
C-------
              IVAD=ISPHIO(4,I)
C-------
C       control up to DMAX
              DMAX=-VSPHIO(IVAD+3)
C-------
C       non interaction outside DI:
              DI=ZERO
              DO IACTIVE=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
                N   =LONFSPH(IACTIVE)
                INOD=KXSP(3,N)
                DI=MAX(DI,SPBUF(1,N))
              ENDDO
              DI=TWO*DI
C-------
              XBMIN =EP20
              YBMIN =EP20
              ZBMIN =EP20
              XBMAX=-EP20
              YBMAX=-EP20
              ZBMAX=-EP20
C------
              ISU =ISPHIO(3,I)
              NSEGB=IGRSURF(ISU)%NSEG
              NSEG=ISPHIO(10,I)
              IAD2 =ISPHIO(11,I)

              DO J=0,NSEG-1
                IN1=IBUFSSG_IO(IAD2+NIBSPH*J  )
                IN2=IBUFSSG_IO(IAD2+NIBSPH*J+1)
                IN3=IBUFSSG_IO(IAD2+NIBSPH*J+2)
                IN4=IBUFSSG_IO(IAD2+NIBSPH*J+3)

c          IN1=IBUFSSG(IAD+NISX*J  )
c          IN2=IBUFSSG(IAD+NISX*J+1)
c          IN3=IBUFSSG(IAD+NISX*J+2)
c          IN4=IBUFSSG(IAD+NISX*J+3)

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

                IF(IN4/=IN3)THEN
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

              XBMIN=XBMIN+DMAX
              YBMIN=YBMIN+DMAX
              ZBMIN=ZBMIN+DMAX
              XBMAX=XBMAX-DMAX
              YBMAX=YBMAX-DMAX
              ZBMAX=ZBMAX-DMAX
C------
              NP2SORT=0
              DO IACTIVE=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
                N=LONFSPH(IACTIVE)
                INOD=KXSP(3,N)
                XI=X(1,INOD)
                YI=X(2,INOD)
                ZI=X(3,INOD)
                IF(     XI>XBMIN.AND.YI>YBMIN.AND.ZI>ZBMIN
     .              .AND.XI<XBMAX.AND.YI<YBMAX.AND.ZI<ZBMAX)THEN
                  NP2SORT=NP2SORT+1
                  MWA(NP2SORT)=IACTIVE
                ENDIF
              ENDDO
C------
C       bucket.
c tri bucket
              DBUCS=-DMAX
              DO J=1,NSEG
                KK =IAD2+NIBSPH*(J-1)
c          KKB =IAD+NISX*(J-1)
                IN1=IBUFSSG_IO(KK  )
                IN2=IBUFSSG_IO(KK+1)
                IN3=IBUFSSG_IO(KK+2)
                IN4=IBUFSSG_IO(KK+3)
c          IN1=IBUFSSG(KKB  )
c          IN2=IBUFSSG(KKB+1)
c          IN3=IBUFSSG(KKB+2)
c          IN4=IBUFSSG(KKB+3)
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
                IF(IN4/=IN3)THEN
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
C       if more than 1 box, each box size >= DBUCS
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
C       KF    =KVNORM+3*NUMSPH

C-----------------------------------------------
C     Build buckets from segments & particles.
C-----------------------------------------------
              CALL SPHREQS(NSEG ,IBUFSSG_IO(IAD2) ,X ,NP2SORT ,MWA ,
     2          LONFSPH  ,KXSP     ,WA(KNSX) ,WA(KISX) ,WA(KNSY) ,
     3          WA(KISY) ,WA(KNSZ) ,WA(KISZ) ,WA(KNPX) ,WA(KIPX) ,
     4          WA(KNPY) ,WA(KIPY) ,WA(KNPZ) ,WA(KIPZ) ,WA       ,
     5          WA(KVNORM),VSPHIO(IVAD+14),VSPHIO(IVAD+13),V,SPBUF ,
     6          ISPHIO(1,I))
C
              VSPHIO(IVAD+14) = DT12
C------
            ELSEIF (ISPHIO(12,I)>0) THEN
C------
C      control section / new outlet - silent boundary
C------
              TFLAG = 0
              IVAD = ISPHIO(4,I)
              DMAX=-VSPHIO(IVAD+3)
C-------
              XBMIN =EP20
              YBMIN =EP20
              ZBMIN =EP20
              XBMAX=-EP20
              YBMAX=-EP20
              ZBMAX=-EP20
              VMAXS=-EP20
C-------
              DT_OLD = VSPHIO(IVAD+14)
              VSPHIO(IVAD+14) = DT12
C-------
              DO II=1,4
                IF (II<4) THEN
                  IF (ISPHIO(12,I)==1) THEN
                    OFF = 3*II
                    XI=VSPHIO(IVAD+OFF+1)
                    YI=VSPHIO(IVAD+OFF+2)
                    ZI=VSPHIO(IVAD+OFF+3)
                    VI=ZERO
                  ELSE
                    XI=X(1,ISPHIO(12+II,I))
                    YI=X(2,ISPHIO(12+II,I))
                    ZI=X(3,ISPHIO(12+II,I))
                    VI=SQRT(V(1,ISPHIO(12+II,I))**2+V(2,ISPHIO(12+II,I))**2+V(3,ISPHIO(12+II,I))**2)
                  ENDIF
                  XX(3*(II-1)+1) = XI
                  XX(3*(II-1)+2) = YI
                  XX(3*(II-1)+3) = ZI
                ELSE
C           calcul nod4
                  XI = XX(4)+XX(7)-XX(1)
                  YI = XX(5)+XX(8)-XX(2)
                  ZI = XX(6)+XX(9)-XX(3)
C           permutation nod3 nod4
                  XX(10) = XX(7)
                  XX(11) = XX(8)
                  XX(12) = XX(9)
                  XX(7) = XI
                  XX(8) = YI
                  XX(9) = ZI
                ENDIF
                XBMIN=MIN(XBMIN,XI)
                YBMIN=MIN(YBMIN,YI)
                ZBMIN=MIN(ZBMIN,ZI)
                XBMAX=MAX(XBMAX,XI)
                YBMAX=MAX(YBMAX,YI)
                ZBMAX=MAX(ZBMAX,ZI)
                VMAXS=MAX(VMAXS,VI)
              END DO
C-------
C       computation of old coordinates of the nodes of the segment
              IF (ISPHIO(12,I)==1) THEN
                DO II=1,12
                  XX_OLD(II) = XX(II)
                END DO
              ELSE
                DO II=1,3
                  XX_OLD(3*(II-1)+1) = X(1,ISPHIO(12+II,I))-DT_OLD*V(1,ISPHIO(12+II,I))
                  XX_OLD(3*(II-1)+2) = X(2,ISPHIO(12+II,I))-DT_OLD*V(2,ISPHIO(12+II,I))
                  XX_OLD(3*(II-1)+3) = X(3,ISPHIO(12+II,I))-DT_OLD*V(3,ISPHIO(12+II,I))
                END DO
                XX_OLD(10) = XX_OLD(7)
                XX_OLD(11) = XX_OLD(8)
                XX_OLD(12) = XX_OLD(9)
                XX_OLD(7) = XX_OLD(4)+XX_OLD(7)-XX_OLD(1)
                XX_OLD(8) = XX_OLD(5)+XX_OLD(8)-XX_OLD(2)
                XX_OLD(9) = XX_OLD(6)+XX_OLD(9)-XX_OLD(3)
              ENDIF
C-------
              IF (ISPHIO(1,I)==4) DMAX = DMAX + VMAXS*MAX(DT_OLD,DT12)
              XBMIN=XBMIN+DMAX
              YBMIN=YBMIN+DMAX
              ZBMIN=ZBMIN+DMAX
              XBMAX=XBMAX-DMAX
              YBMAX=YBMAX-DMAX
              ZBMAX=ZBMAX-DMAX
C------
              KVNORM=NUMSPH+1
C------
              NP2SORT=0
              DO IACTIVE=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
                N=LONFSPH(IACTIVE)
                INOD=KXSP(3,N)
                XI=X(1,INOD)
                YI=X(2,INOD)
                ZI=X(3,INOD)
                IF(     XI>XBMIN.AND.YI>YBMIN.AND.ZI>ZBMIN
     .             .AND.XI<XBMAX.AND.YI<YBMAX.AND.ZI<ZBMAX)THEN
                  NP2SORT=NP2SORT+1
                  MWA(NP2SORT)=IACTIVE
                  XI_OLD = XI - V(1,INOD)*DT_OLD
                  YI_OLD = YI - V(2,INOD)*DT_OLD
                  ZI_OLD = ZI - V(3,INOD)*DT_OLD
                  CALL SPH_NODSEG(XI_OLD,YI_OLD,ZI_OLD,XX_OLD,TFLAG,NP2SORT,LONFSPH,MWA,WA,WA(KVNORM),1)
                  DPS_OLD=WA(NP2SORT)
                  CALL SPH_NODSEG(XI,YI,ZI,XX,TFLAG,NP2SORT,LONFSPH,MWA,WA,WA(KVNORM),1)
                  DPS=WA(NP2SORT)
                  IF (DPS _OLD*DPS<ZERO) THEN
C       pur outlet/silent boundaries on permute le sens de comptage
                    IF (ISPHIO(1,I)/=4) DPS_OLD = -DPS_OLD
C       la particule a traverse la surface dans le sens +
                    IF (DPS_OLD>ZERO) VSPHIO(IVAD+13) = VSPHIO(IVAD+13)-SPBUF(12,N)
C       la particule a traverse la surface dans le sens -
                    IF (DPS_OLD<ZERO) VSPHIO(IVAD+13) = VSPHIO(IVAD+13)+SPBUF(12,N)
                  ENDIF
                ENDIF
              ENDDO
C------
            ENDIF
C------
            IF ((ITYPE==2).OR.(ITYPE==3)) THEN
C------
              DO KP=1,NP2SORT
                DPS=WA(KP)
                IACTIVE=MWA(KP)
                IF(DPS>DMAX.AND.DPS<ZERO)THEN
C         particule imposee :
C           conditions multiples => la derniere condition rencontree impose ..
                  N=LONFSPH(IACTIVE)
                  IWA(IACTIVE)=I
                  NG       =MOD(KXSP(2,N),NGROUP+1)
                  KXSP(2,N)=NG+I*(NGROUP+1)
                  IF(TT==ZERO)THEN
                    INOD=KXSP(3,N)
                    KK=KVNORM+3*(N-1)
                    SPHVELN(1,N)= WA(KK  )*V(1,INOD)
     .                           +WA(KK+1)*V(2,INOD)
     .                           +WA(KK+2)*V(3,INOD)
                    NG=MOD(KXSP(2,N),NGROUP+1)
                    CALL INITBUF(IPARG    ,NG      ,
     2                 MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,
     3                 NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4                 JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5                 NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6                 IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7                 ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS)
!
                    DO IJ=1,6
                      JJ(IJ) = NEL*(IJ-1)
                    ENDDO
!
                    GBUF => ELBUF_TAB(NG)%GBUF
                    K=N-NFT
!
                    SPHVELN(2,N)=-THIRD*
     .                           (GBUF%SIG(JJ(1)+K)+GBUF%SIG(JJ(2)+K)+GBUF%SIG(JJ(3)+K))
                  ENDIF
                  KK=KVNORM+3*(N-1)
                  VNORMAL(1,N)=WA(KK  )
                  VNORMAL(2,N)=WA(KK+1)
                  VNORMAL(3,N)=WA(KK+2)
                ELSEIF(DPS>=ZERO.AND.IWA(IACTIVE)==0)THEN
                  N=LONFSPH(IACTIVE)
                  INOD=KXSP(3,N)
                  KK=KVNORM+3*(N-1)
                  SPHVELN(1,N)= WA(KK  )*V(1,INOD)
     .                         +WA(KK+1)*V(2,INOD)
     .                         +WA(KK+2)*V(3,INOD)
                  NG=MOD(KXSP(2,N),NGROUP+1)
                  CALL INITBUF(IPARG    ,NG      ,
     2               MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,
     3               NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4               JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5               NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6               IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7               ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS)
!
                  DO IJ=1,6
                    JJ(IJ) = NEL*(IJ-1)
                  ENDDO
!
                  GBUF => ELBUF_TAB(NG)%GBUF
                  K=N-NFT
!
                  SPHVELN(2,N)=-THIRD*
     .                        (GBUF%SIG(JJ(1)+K)+GBUF%SIG(JJ(2)+K)+GBUF%SIG(JJ(3)+K))
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO    !fin DO I=1,NSPHIO
C-----------------------------------------------
        IF(NSPMD>1)THEN
C send IMPOSE value
          CALL SPMD_SPHGETIMP(KXSP)
        END IF
C     requete ppv <=> non zero interaction / impose
c     ppv: plus proche voisin
C-----------------------------------------------
        DO I=1,NSPHIO
          ITYPE=ISPHIO(1,I)
          IF ((ITYPE==2).OR.(ITYPE==3)) THEN
C-------
C      general outlets & silent boundary.
            IPRT   =ISPHIO(2,I)
            DO IACTIVE=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
              IF(IWA(IACTIVE)/=0)THEN
                N   =LONFSPH(IACTIVE)
                INOD=KXSP(3,N)
                XI=X(1,INOD)
                YI=X(2,INOD)
                ZI=X(3,INOD)
                IPPV=0
                DMIN=EP20

                DO J=1,KXSP(4,N)
                  JNOD=IXSP(J,N)

                  IF(JNOD>0)THEN      ! particule locale
                    M=NOD2SP(JNOD)
                    IF(KXSP(2,M)>=0)THEN
                      IMPOSE=KXSP(2,M)/(NGROUP+1)
                      lBOOL=.FALSE.
                      IF(IMPOSE == 0)THEN
                        lBOOL=.TRUE.
                      ELSE
                        IF(ISPHIO(1,IMPOSE)==1)lBOOL=.TRUE.
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
                    ENDIF
                  ELSE                   ! particule remote
                    NN = -JNOD
                    IMPOSE = NINT(XSPHR(12,NN))
                    IF(IMPOSE>0)THEN
                      IMPOSE2=ISPHIO(1,IMPOSE)
                    ELSE
                      IMPOSE2=0
                    ENDIF
                    IF(IMPOSE2==0.OR.IMPOSE2==1)THEN
                      XJ = XSPHR(3,NN)
                      YJ = XSPHR(4,NN)
                      ZJ = XSPHR(5,NN)
                      DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
                      IF(DD<DMIN)THEN
                        IPPV=JNOD
                        DMIN=DD
                      ENDIF
                    ENDIF
                  ENDIF
                ENDDO
C-------
                IF(IPPV==0)THEN
C           no interaction between the outgoing particle
C           and the particles inside the domain.
                  NG=MOD(ABS(KXSP(2,N)),NGROUP+1)
                  KXSP(2,N)=-NG
c elmt delete OFF_SPH set to 1
                  OFF_SPH(N)=1
                  IF (R2R_SIU/=0)  OFF_SPH_R2R(INOD) = -1
                ENDIF
              ENDIF
            ENDDO
          END IF
        ENDDO
C-----------------------------------------------
C      supprime particules n'inter-agissant plus avec l'interieur du domaine.
C-----------------------------------------------
        DO I1=1,NSPHIO
          ITYPE=ISPHIO(1,I1)
          IF ((ITYPE==2).OR.(ITYPE==3)) THEN
C-------
C      general outlets & silent boundary.
            IPRT   =ISPHIO(2,I1)
            IACTIVE=LPRTSPH(2,IPRT-1)+1
            DO WHILE(IACTIVE<=LPRTSPH(1,IPRT))
              IF(IWA(IACTIVE)/=0)THEN
                N   =LONFSPH(IACTIVE)
                IF(KXSP(2,N)<0)THEN
                  INOD=KXSP(3,N)
                  IF(TT/=ZERO)TFEXTT=TFEXTT-HALF*MS(INOD)
     .            *(V(1,INOD)*V(1,INOD)+V(2,INOD)*V(2,INOD)+V(3,INOD)*V(3,INOD))
                  V(1,INOD)=ZERO
                  V(2,INOD)=ZERO
                  V(3,INOD)=ZERO
                  I  =IWA(IACTIVE)
                  IF (ISPHIO(12,I)==0) THEN
                    ISU=ISPHIO(3,I)
                    IAD2=ISPHIO(11,I)
c            IN1=IBUFSSG_IO(IAD2  )
c            IN2=IBUFSSG_IO(IAD2+1)
c            IN1B=IBUFSSG(IAD  )
c            IN2B=IBUFSSG(IAD+1)
                    X1 =X(1,IN1)
                    Y1 =X(2,IN1)
                    Z1 =X(3,IN1)
                    X2 =X(1,IN2)
                    Y2 =X(2,IN2)
                    Z2 =X(3,IN2)
                  ELSEIF (ISPHIO(12,I)==1) THEN
                    IVAD = ISPHIO(4,I)
                    X1 =VSPHIO(IVAD+4)
                    Y1 =VSPHIO(IVAD+5)
                    Z1 =VSPHIO(IVAD+6)
                    X2 =VSPHIO(IVAD+7)
                    Y2 =VSPHIO(IVAD+8)
                    Z2 =VSPHIO(IVAD+9)
                  ELSE
                    X1 =X(1,ISPHIO(13,I))
                    Y1 =X(2,ISPHIO(14,I))
                    Z1 =X(3,ISPHIO(15,I))
                    X2 =X(1,ISPHIO(16,I))
                    Y2 =X(2,ISPHIO(17,I))
                    Z2 =X(3,ISPHIO(18,I))
                  ENDIF
                  XG =HALF*(X1+X2)
                  YG =HALF*(Y1+Y2)
                  ZG =HALF*(Z1+Z2)
                  IF(ITAB(INOD) >= 1000000000)THEN
                    D(1,INOD)=ZERO
                    D(2,INOD)=ZERO
                    D(3,INOD)=ZERO
                  ELSE
                    D(1,INOD)=XG-X(1,INOD)+D(1,INOD)
                    D(2,INOD)=YG-X(2,INOD)+D(2,INOD)
                    D(3,INOD)=ZG-X(3,INOD)+D(3,INOD)
                  ENDIF
                  X(1,INOD)=XG
                  X(2,INOD)=YG
                  X(3,INOD)=ZG
                  IF (IRESP==1) THEN
                    XDP(1,INOD)=XG
                    XDP(2,INOD)=YG
                    XDP(3,INOD)=ZG
                  END IF
                  NG=MOD(ABS(KXSP(2,N)),NGROUP+1)
                  CALL INITBUF(IPARG    ,NG      ,
     2               MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,
     3               NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4               JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5               NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6               IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7               ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS)
!
                  DO IJ=1,6
                    JJ(IJ) = NEL*(IJ-1)
                  ENDDO
!
                  GBUF => ELBUF_TAB(NG)%GBUF
                  K=N-NFT
                  VOLO=GBUF%VOL(K)
                  TFEXTT=TFEXTT-VOLO*GBUF%EINT(K)
                  GBUF%EINT(K)=ZERO
!
                  GBUF%SIG(JJ(1)+K)=ZERO
                  GBUF%SIG(JJ(2)+K)=ZERO
                  GBUF%SIG(JJ(3)+K)=ZERO
                  GBUF%SIG(JJ(4)+K)=ZERO
                  GBUF%SIG(JJ(5)+K)=ZERO
                  GBUF%SIG(JJ(6)+K)=ZERO
                  GBUF%OFF(K)=ZERO
C
                  KXSP(2,N)=-NG
                  IPARG(10,NG)=IPARG(10,NG)-1
                  IF(IPARG(10,NG)==0)IPARG(8,NG)=1
C           ISPHBUC=1
C           chainage:
                  ILASTON            =LPRTSPH(1,IPRT)
                  NLASTON            =LONFSPH(ILASTON)
                  LONFSPH(IACTIVE)   =NLASTON
                  IWA(IACTIVE)       =IWA(ILASTON)
                  LONFSPH(ILASTON)   =N
                  ILASTON            =ILASTON-1
                  LPRTSPH(1,IPRT)    =ILASTON

C           mise a jour des voisinages:
c 1) ON NE TRAITE QUE LES VOISINS LOCAUX

                  DO J=1,KXSP(5,N)

                    INOD0=IXSP(J,N)

                    IF(INOD0<0)THEN
c will be treated in SPMD_SPHGETVOIS_OFF
c TAG_SPH set to 1
                      TAG_SPH(-INOD0)=1
                    ELSE
                      M    =NOD2SP(INOD0)
                      NVOIS1=0
                      DO K=1,KXSP(4,M)
                        KNOD=IXSP(K,M)
                        IF(KNOD/=INOD)THEN
                          NVOIS1=NVOIS1+1
                          IXSP(NVOIS1,M)=KNOD
                        ENDIF
                      ENDDO

                      NVOIS2=NVOIS1
                      DO K=KXSP(4,M)+1,KXSP(5,M)
                        KNOD=IXSP(K,M)
                        IF(KNOD/=INOD)THEN
                          NVOIS2=NVOIS2+1
                          IXSP(NVOIS2,M)=KNOD
                        ENDIF
                      ENDDO

                      NVOISS1=0
                      DO K=KXSP(5,M)+1,KXSP(5,M)+KXSP(6,M)
                        JK  =IXSP(K,M)
                        IF(JK<0)THEN
                          NVOISS1=NVOISS1+1
                          IXSP(NVOIS2+NVOISS1,M)=JK
                        ELSE
                          KNOD=KXSP(3,JK/(NSPCOND+1))
                          IF(KNOD/=INOD)THEN
                            NVOISS1=NVOISS1+1
                            IXSP(NVOIS2+NVOISS1,M)=JK
                          ENDIF
                        ENDIF
                      ENDDO

                      NVOISS2=NVOISS1
                      DO K=KXSP(5,M)+KXSP(6,M)+1,KXSP(5,M)+KXSP(7,M)
                        JK  =IXSP(K,M)
                        IF(JK<0)THEN
                          NVOISS2=NVOISS2+1
                          IXSP(NVOIS2+NVOISS2,M)=JK
                        ELSE
                          KNOD=KXSP(3,JK/(NSPCOND+1))
                          IF(KNOD/=INOD)THEN
                            NVOISS2=NVOISS2+1
                            IXSP(NVOIS2+NVOISS2,M)=JK
                          ENDIF
                        ENDIF
                      ENDDO
                      KXSP(4,M)= NVOIS1
                      KXSP(5,M)= NVOIS2
                      KXSP(6,M)=NVOISS1
                      KXSP(7,M)=NVOISS2
                    ENDIF ! fin IF(INOD0<0)THEN

                  ENDDO !fin do J=1,KXSP(5,N)

                ELSE
                  IACTIVE=IACTIVE+1
                ENDIF
              ELSE
                IACTIVE=IACTIVE+1
              ENDIF
            ENDDO
          END IF
        ENDDO

c 2) treatment of remote neighbours only
        IF(NSPMD>1)
     .     CALL SPMD_SPHGETVOIS_OFF(OFF_SPH, TAG_SPH,
     .                                KXSP, IXSP,ITAB)
C-------------------------------------------
#include "atomic.inc"
        TFEXT=TFEXT+TFEXTT
#include "atomend.inc"
C-------------------------------------------
        DEALLOCATE(OFF_SPH,TAG_SPH)

        RETURN
      END
