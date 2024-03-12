Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  THQUAD                        source/output/th/thquad.F     
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        QROTA3                        source/output/anim/generate/qrota3.F
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE THQUAD(ELBUF_TAB,NTHGRP2 , ITHGRP , 
     1                  IPARG    ,ITHBUF   ,WA     ,
     2                  IPM      ,IXQ      ,IXTG   ,
     3                  X        ,MULTI_FVM,V      ,
     4                  W        ,
     .                  NUMELQ   ,NUMMAT   ,NUMNOD ,SITHBUF, NUMELTG)

C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C
C             /TH/QUAD : BUFFER FOR TIME HISTORY OUTPUT
C
C This subroutine is writing buffer related to /TH/QUAD option in
C order to be written in Time History files : T01, T02, etc...
C Each channel (index) is standing for a given physical quantity as desbibed below
C Time History file is requested with Engine option /TFILE
C
C-------------------------
C CHANNEL     KEY    DESCRIPTION   [MAT LAW]
C
C       1     OFF
C       2     SX     SIGX
C       3     SY     SIGY
C       4     SZ     SIGZ
C       5     SXY    SIGXY
C       6     SYZ    SIGYZ
C       7     SXZ    SIGZX
C       8     IE     INTERNAL ENERGIE / VOLUME0
C       9     DENS   DENSITY
C      10     BULK   BULK VISCOSITY
C      11     VOL    VOLUME (ALE) OR INITIAL VOLUME (LAG)
C      12     PLAS   EPS PLASTIQUE [2,3,4,7,8,9,16,22,23,26,33-38]
C      13     TEMP   TEMPERATURE   [4,6,7,8,9,11,16,17,26,33-38]
C      14     PLSR   STRAIN RATE   [4,7,8,9,16,26,33-38]
C      15     DAMA1  DAMAGE 1      [14]
C      16     DAMA2  DAMAGE 2      [14]
C      17     DAMA3  DAMAGE 3      [14]
C      18     DAMA4  DAMAGE 4      [14]
C      19     DAMA   DAMAGE        [24]
C      20(14) SA1    STRESS RE1    [24]
C      21(15) SA2    STRESS RE2    [24]
C      22(16) SA3    STRESS RE3    [24]
C      23(17) CR     CRACKS VOL    [24]
C      24(18) CAP    CAP PARAM     [24]
C      25(13) K0     HARD. PARAM   [24]
C      26(12) RK     TURBUL. ENER. [6,11,17]
C      27(14) TD     TURBUL. DISS. [6,11,17]
C      28(14) EFIB   FIBER STRAIN  [14]
C      29(16) ISTA   PHASE STATE   [16]
C      30(12) VPLA   VOL. EPS PLA. [10,21]
C      31     BFRAC  BURN FRACTION [5,41,51,97,151]
C      32(12) WPLA   PLAS. WORK    [14]
C        ...
C      239547     VX     X-VELOCITY (MEAN VALUE FOR STAGGERED SCHEME, CELL VALUE FOR COLOCATED SCHEME)
C      239548     VY     Y-VELOCITY (MEAN VALUE FOR STAGGERED SCHEME, CELL VALUE FOR COLOCATED SCHEME)
C      239549     VZ     Z-VELOCITY (MEAN VALUE FOR STAGGERED SCHEME, CELL VALUE FOR COLOCATED SCHEME)
C      239550     SSP    SOUND SPEED
C      239551     MACH   MACH NUMBER
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE ALEFVM_MOD , only:ALEFVM_Param   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NUMELQ, NUMMAT, NUMNOD ,SITHBUF, NUMELTG
      INTEGER,INTENT(IN) :: IPARG(NPARG,NGROUP),ITHBUF(SITHBUF),IXQ(NIXQ,NUMELQ),IPM(NPROPMI,NUMMAT),IXTG(NIXTG,NUMELTG)
      INTEGER, INTENT(IN) :: NTHGRP2
      INTEGER, DIMENSION(NITHGR,*), INTENT(IN) :: ITHGRP
      my_real,INTENT(INOUT) :: WA(*)
      my_real,INTENT(IN) :: X(3,NUMNOD), V(3,NUMNOD), W(3,NUMNOD)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, KRK, LL, I, J, K, L ,N, IH, IP, NG, MTE, NUVAR,
     .        NC1, NC2, NC3, NC4,  NEL, MTN1,KK(6),IJ,NPTR,NPTS,
     .        IR,IS,JJ(6),NITER,IADB,NN,IADV,NVAR,ITYP,IJK,IS_ALE
      my_real
     .        SY , SZ, TY , TZ, SUMA,
     .        Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .        R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .        G22,G23,G32,G33,
     .        T22,T23,T32,T33,
     .        S1,S2,S3,S4,
     .        T1,T2,T3,T4,CS,CT,EVAR(6),GAMA(6),
     .        TMP(3,4),VEL(3),SSP,BFRAC
      my_real, dimension(:), allocatable :: WWA
      TYPE(L_BUFEL_) ,POINTER :: LBUF,LBUF1,LBUF2
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        ALLOCATE(WWA(239552))
        IJK = 0
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IADB =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==2.OR.ITYP==117)THEN
!   -----------------------------
      NUVAR = 0
      II=0
      IH=IADB
      IF(ITYP == 117) ITYP = 7


C  IH shifting
      DO WHILE((ITHBUF(IH+NN)/=ISPMD).AND.(IH<IADB+NN))
          IH = IH + 1
        ENDDO
C----
        IF (IH>=IADB+NN) GOTO 666 
C----
c
      DO NG=1,NGROUP
       ITY=IPARG(5,NG)
       IS_ALE = IPARG(7,NG)
       
       DO I=1,6
         JJ(I) = NEL*(I-1)
       ENDDO  

       IF (ITY == ITYP) THEN
         GBUF => ELBUF_TAB(NG)%GBUF
         LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
         MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
c
         NPTR = ELBUF_TAB(NG)%NPTR                 
         NPTS = ELBUF_TAB(NG)%NPTS 
C------
        CALL INITBUF(IPARG    ,NG      ,                    
     2          MTE     ,NEL     ,NFT     ,IAD     ,ITY    ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )

         IF(MTE /= 13) THEN
C
          DO I=1,NEL
            N=I+NFT
            K=ITHBUF(IH)
            IP=ITHBUF(IH+NN)
!
            DO IJ=1,6
              KK(IJ) = NEL*(IJ-1)
            ENDDO

            EVAR(1:6) = ZERO
C
            IF (K==N)THEN
              IH=IH+1
              !spmd treatment
              IF (IMACH==3) THEN
                ! find related 'ii'
                II = ((IH-1) - IADB)*NVAR
                DO WHILE((ITHBUF(IH+NN)/=ISPMD).AND.(IH<IADB+NN))
                  IH = IH + 1
                ENDDO
              ENDIF
c-------------------------------------
              IF (IH > IADB+NN) GOTO 666
c-------------------------------------
              DO L=1,1000
                WWA(L)=ZERO
              ENDDO
              WWA(1) = GBUF%OFF(I)
              WWA(8) = GBUF%EINT(I)
              WWA(9) = GBUF%RHO(I)
              WWA(10)= GBUF%QVIS(I)
              WWA(11)= GBUF%VOL(I)
              IF (ISORTH == 0) THEN
                GAMA(1)=ONE
                GAMA(2)=ZERO
                GAMA(3)=ZERO
                GAMA(4)=ZERO
                GAMA(5)=ONE
                GAMA(6)=ZERO
              ELSE
                GAMA(1)=GBUF%GAMA(KK(1) + I)
                GAMA(2)=GBUF%GAMA(KK(2) + I)
                GAMA(3)=GBUF%GAMA(KK(3) + I)
                GAMA(4)=GBUF%GAMA(KK(4) + I)
                GAMA(5)=GBUF%GAMA(KK(5) + I)
                GAMA(6)=GBUF%GAMA(KK(6) + I)
              END IF
C-----------
C             SOUND SPEED, MATERIAL VELOCITY, AND MACH NUMBER.
C-----------
              VEL(1:3)=ZERO
              WWA(239547) = ZERO !VZ
              WWA(239548) = ZERO !VY
              WWA(239549) = ZERO !VZ
              WWA(239551) = ZERO !SSP
              WWA(239551) = ZERO !MACH
              IF(IS_ALE /= 0)THEN
c               ! ale
                IF(ITY == 2)THEN
                  TMP(1,1:4)=V(1,IXQ(2:5,N))-W(1,IXQ(2:5,N))
                  TMP(2,1:4)=V(2,IXQ(2:5,N))-W(2,IXQ(2:5,N))
                  TMP(3,1:4)=V(3,IXQ(2:5,N))-W(3,IXQ(2:5,N))
                  VEL(1) = SUM(TMP(1,1:4))*FOURTH
                  VEL(2) = SUM(TMP(2,1:4))*FOURTH
                  VEL(3) = SUM(TMP(3,1:4))*FOURTH
                ELSEIF(ITY == 7)THEN
                  TMP(1,1:3)=V(1,IXTG(2:4,N))-W(1,IXTG(2:4,N))
                  TMP(2,1:3)=V(2,IXTG(2:4,N))-W(2,IXTG(2:4,N))
                  TMP(3,1:3)=V(3,IXTG(2:4,N))-W(3,IXTG(2:4,N))
                  VEL(1) = SUM(TMP(1,1:3))*THIRD
                  VEL(2) = SUM(TMP(2,1:3))*THIRD
                  VEL(3) = SUM(TMP(3,1:3))*THIRD
                ENDIF
              ELSE
                 !euler and lagrange
                IF(ITY == 2)THEN
                  TMP(1,1:4)=V(1,IXQ(2:5,N))
                  TMP(2,1:4)=V(2,IXQ(2:5,N))
                  TMP(3,1:4)=V(3,IXQ(2:5,N))
                  VEL(1) = SUM(TMP(1,1:4))*FOURTH
                  VEL(2) = SUM(TMP(2,1:4))*FOURTH
                  VEL(3) = SUM(TMP(3,1:4))*FOURTH
                ELSE
                  TMP(1,1:3)=V(1,IXTG(2:4,N))
                  TMP(2,1:3)=V(2,IXTG(2:4,N))
                  TMP(3,1:3)=V(3,IXTG(2:4,N))
                  VEL(1) = SUM(TMP(1,1:3))*THIRD
                  VEL(2) = SUM(TMP(2,1:3))*THIRD
                  VEL(3) = SUM(TMP(3,1:3))*THIRD
                ENDIF
              ENDIF

              WWA(239547) = VEL(1)
              WWA(239548) = VEL(2)
              WWA(239549) = VEL(3)

              IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN
                WWA(239550)= LBUF%SSP(I) !sound speed
                WWA(239551)= SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)   !mach number
              ENDIF

              IF(ELBUF_TAB(NG)%GBUF%G_BFRAC /= 0)THEN
                WWA(31) = GBUF%BFRAC(I)
              ENDIF
C------------------------------------------------------------------------------
C       TH tab filling with stresses in the global (WA(2:7) 
C                                         and local system(WA(35:40)
C------------------------------------------------------------------------------
              DO J=1,6
                EVAR(J)=GBUF%SIG(KK(J)+I)
              ENDDO
              IF (JCVT <= 0) THEN
                DO J=1,6
                  WWA(2+J-1)=EVAR(J)
                ENDDO
                IF(ITY == 2) CALL QROTA3(X,IXQ(1,N),JCVT,EVAR,GAMA,ISORTH)
                DO J=1,6               
                  WWA(35+J-1)=EVAR(J)
                ENDDO
              ELSE
                DO J=1,6               
                  WWA(35+J-1)=EVAR(J)
                ENDDO
                IF(ITY == 2) CALL QROTA3(X,IXQ(1,N),JCVT,EVAR,GAMA,ISORTH)
                DO J=1,6
                  WWA(2+J-1)=EVAR(J)
                ENDDO
              ENDIF
c
              IF (MTE==2)THEN
                WWA(12)=GBUF%PLA(I)
              ELSEIF(MTE==3) THEN
                WWA(12)=GBUF%PLA(I)
                WWA(13)=GBUF%TEMP(I)
              ELSEIF (MTE==4) THEN
                WWA(12)=GBUF%PLA(I)
                WWA(13)=GBUF%TEMP(I)
                WWA(14)=GBUF%EPSD(I)
              ELSEIF (MTE==5) THEN
                WWA(31)=GBUF%BFRAC(I) 
                WWA(13)=GBUF%TEMP(I)
              ELSEIF (MTE==6) THEN
                WWA(13)=GBUF%TEMP(I)
                WWA(26)=LBUF%RK(I)
                WWA(27)=LBUF%RE(I)
              ELSEIF (MTE==7.OR.MTE==8.OR.MTE==9) THEN
                WWA(12)=ZERO
                WWA(13)=ZERO
                WWA(14)=ZERO
              ELSEIF (MTE==10) THEN
                WWA(12)=LBUF%EPSQ(I)
                WWA(30)=LBUF%PLA(I)
              ELSEIF (MTE==11) THEN
                WWA(13)=LBUF%TEMP(I)
                WWA(26)=LBUF%RK(I)
                WWA(27)=LBUF%RE(I)
              ELSEIF (MTE==14) THEN
                WWA(32)=LBUF%PLA(I)
                WWA(33)=LBUF%SIGF(I)
                WWA(28)=LBUF%EPSF(I)
                WWA(15)=LBUF%DAM(KK(1)+I)
                WWA(16)=LBUF%DAM(KK(2)+I)
                WWA(17)=LBUF%DAM(KK(3)+I)
                WWA(18)=LBUF%DAM(KK(4)+I)
                WWA(34)=LBUF%DAM(KK(5)+I)
              ELSEIF (MTE==16) THEN
                WWA(12)=LBUF%PLA(I)
                WWA(13)=LBUF%TEMP(I)
                WWA(14)=LBUF%XST(I)   
              ELSEIF (MTE==17) THEN
                IF (ITHERM > 0) WWA(13)=LBUF%TEMP(I)
                WWA(26)=LBUF%RK(I)
                WWA(27)=LBUF%RE(I)
              ELSEIF (MTE==18) THEN
                WWA(13)=LBUF%TEMP(I)
              ELSEIF (MTE==20) THEN    ! Bimat
                LBUF1 => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                LBUF2 => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                IF(GBUF%G_PLA>0) WWA(12)=GBUF%PLA(I)
                IF(GBUF%G_TEMP>0)WWA(13)=GBUF%TEMP(I)                            
                ! SUBMATERIAL 1
                !MTN1=IPARG(25,NG)
                DO J = 1,6
                  WWA(1624 + J) = LBUF1%SIG(KK(J)+I)
                ENDDO
                   WWA(1624 + 7 ) = LBUF1%EINT(I)
                   WWA(1624 + 8 ) = LBUF1%RHO(I)
                   WWA(1624 + 9 ) = LBUF1%VOL(I)
                IF(ELBUF_TAB(NG)%BUFLY(1)%L_TEMP>0) 
     .             WWA(1624 +11 )=LBUF1%TEMP(I)                 
                !SUBMATERIAL 2
                !MTN1=IPARG(26, NG)
                DO J = 1,6
                  WWA(1635 + J) = LBUF2%SIG(KK(J)+I)
                ENDDO
                      WWA(1635 + 7 ) = LBUF2%EINT(I)
                      WWA(1635 + 8 ) = LBUF2%RHO(I)
                      WWA(1635 + 9 ) = LBUF2%VOL(I)
                IF(ELBUF_TAB(NG)%BUFLY(2)%L_TEMP>0) 
     .             WWA(1635 +11 )=LBUF2%TEMP(I)                                 
              ELSEIF (MTE==21) THEN
                WWA(12)=LBUF%EPSQ(I) ! NB11
                WWA(30)=GBUF%PLA(I)  ! NB10
              ELSEIF (MTE==22.OR.MTE==23) THEN
                WWA(12)=LBUF%PLA(I)
              ELSEIF (MTE==24) THEN
                WWA(19)=LBUF%DAM(KK(1)+I)+LBUF%DAM(KK(2)+I)+LBUF%DAM(KK(3)+I)
                WWA(20)=LBUF%SIGA(KK(1)+I)
                WWA(21)=LBUF%SIGA(KK(2)+I)
                WWA(22)=LBUF%SIGA(KK(3)+I)
                WWA(23)=LBUF%CRAK(KK(1)+I)+LBUF%CRAK(KK(2)+I)+LBUF%CRAK(KK(3)+I)
                WWA(24)=LBUF%DSUM(I)
                WWA(25)=LBUF%VK(I)
              ELSEIF (MTE==26) THEN
                WWA(12)=LBUF%PLA(I)
                WWA(13)=LBUF%TEMP(I)
                WWA(14)=LBUF%Z(I)
              ELSEIF (MTE==32.OR.MTE==43) THEN   ! not solid compatible !!
                WWA(12)=ZERO
                WWA(13)=ZERO
                WWA(14)=ZERO
              ELSEIF (MTE==46.OR.MTE==47) THEN
                WWA(12)=MBUF%VAR(I)
                WWA(13)=MBUF%VAR(I+NEL)
c                WWA(14)=MBUF%VAR(I+NEL*2)    
              ELSEIF (MTE==49) THEN
                WWA(12)=LBUF%PLA(I)
                WWA(13)=LBUF%TEMP(I)
                WWA(14)=LBUF%EPSD(I)
              ELSEIF (MTE>=29.AND.MTE/=67) THEN
C               User laws for quads
                NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
                IF (NUVAR > 0) WWA(12)=MBUF%VAR(I)
                IF (NUVAR > 1) WWA(13)=MBUF%VAR(I+NEL)
                IF (NUVAR > 2) WWA(14)=MBUF%VAR(I+NEL*2)
              ELSEIF (MTE==67) THEN
C               Temperature
                WWA(12)=ZERO
                WWA(13)=MBUF%VAR(I)
                WWA(14)=ZERO
              ENDIF
              IF (MTE >= 29) THEN
                IF(ITY == 2) THEN
                  NUVAR  =IPM(8,IXQ(1,NFT+1))
                ELSEIF(ITY == 7) THEN
                  NUVAR  =IPM(8,IXTG(1,NFT+1))
                ENDIF
                DO J=1,NUVAR
                  WWA(136+J)=MBUF%VAR((J-1)*NEL+I)
                ENDDO
              ENDIF
C
C------------------------------------------------------------------------------
C       TH tab filling with stain in element and per intergation point
C              EPSXIJK,EPSYIJK,EPSZIJK,EPSXYIJK,EPSXZIJK,EPSYZIIJK => WWA(239060)
C              EPSXX,EPSYY,EPSZZ,EPSXY,EPSXZ,EPSYZ  =>  WWA(1618)
C              L_EPSXX,L_EPSYY,L_EPSZZ,L_EPSXY,LEPSXZ,LEPSYZ  => WWA(239030)
C------------------------------------------------------------------------------
c EPS
              EVAR(1:6)=ZERO         
              DO IS=1,NPTS                   
                DO IR=1,NPTR
                 LBUF1 =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)        
                 EVAR(1) = EVAR(1) + LBUF1%STRA(KK(1) + I)/NPT
                 EVAR(2) = EVAR(2) + LBUF1%STRA(KK(2) + I)/NPT
                 EVAR(4) = EVAR(4) + LBUF1%STRA(KK(4) + I)*HALF/NPT
                ENDDO
              ENDDO

              IF (JCVT == 0) THEN
C EPS IN THE GLOBAL SYSTEM
                DO J=1,6
                 WWA(1619+J-1)=EVAR(J)
                ENDDO 
                IF(ITY == 2) CALL QROTA3(X,IXQ(1,N),JCVT,EVAR,GAMA,ISORTH)
C LEPS IN THE LOCAL SYSTEM
                DO J=1,6
                 WWA(239030+J-1)=EVAR(J)
                ENDDO
               ELSE
C LEPS IN THE LOCAL SYSTEM
                DO J=1,6
                 WWA(239030+J-1)=EVAR(J)
                ENDDO
                IF(ITY == 2) CALL QROTA3(X,IXQ(1,N),JCVT,EVAR,GAMA,ISORTH)
C EPS IN THE GLOBAL SYSTEM
                DO J=1,6
                 WWA(1619+J-1)=EVAR(J)
                ENDDO     
              ENDIF
C EPS111, EPS121, EPS211, EPS221 IN THE GLOBAL SYSTEM
              DO IS=1,NPTS                   
                DO IR=1,NPTR
                 LBUF1 =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1) 
                 EVAR(1:6)=ZERO 
                 EVAR(1) = LBUF1%STRA(KK(1) + I)
                 EVAR(2) = LBUF1%STRA(KK(2) + I)
                 EVAR(4) = LBUF1%STRA(KK(4) + I)
                 IF (JCVT == 0) THEN
                   DO J=1,6      
                     WWA(239030+30+(IS-1)*6+(IR-1)*18+J) = EVAR(J)
                   ENDDO
                 ELSE
                   IF(ITY == 2) CALL QROTA3(X,IXQ(1,N),JCVT,EVAR,GAMA,ISORTH)
                   DO J=1,6      
                     WWA(239030+30+(IS-1)*6+(IR-1)*18+J) = EVAR(J)
                   ENDDO
                 ENDIF  
                ENDDO
              ENDDO
C
C                    
              IF (MTE==151) THEN !specific buffer with colocated scheme, generic storage from above are erased
C BFRAC
                IF(ALLOCATED(MULTI_FVM%BFRAC))THEN
                  BFRAC = ZERO
                  DO IR=1,MULTI_FVM%NBMAT
                    BFRAC = MAX(BFRAC, MULTI_FVM%BFRAC(IR,N))
                  ENDDO
                  WWA(31)=BFRAC
               ENDIF
C VX / VY / VZ
                WWA(239547)= MULTI_FVM%VEL(1, N)
                WWA(239548)= MULTI_FVM%VEL(2, N)
                WWA(239549)= MULTI_FVM%VEL(3, N)
C SSP
                WWA(239550)= MULTI_FVM%SOUND_SPEED(N)
C MACH NUMBER
                WWA(239551)= SQRT(MULTI_FVM%VEL(1, N)*MULTI_FVM%VEL(1, N)+
     .                       MULTI_FVM%VEL(2, N)*MULTI_FVM%VEL(2, N)+
     .                       MULTI_FVM%VEL(3, N)*MULTI_FVM%VEL(3, N)) / 
     .                       MULTI_FVM%SOUND_SPEED(N)

              ELSEIF(ALEFVM_Param%ISOLVER>1)THEN
C SSP
                WWA(239550)= LBUF%SSP(I)                              
                IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN   
                  VEL(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I) 
                  VEL(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)
                  VEL(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I) 
                  WWA(239547)= VEL(1)
                  WWA(239548)= VEL(2)
                  WWA(239549)= VEL(3)
                  WWA(239551)= SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I) 
                ENDIF

              ENDIF
c
              DO L=IADV,IADV+NVAR-1
                K=ITHBUF(L)
                IJK=IJK + 1
                WA(IJK)=WWA(K)
              ENDDO
              IJK=IJK + 1
              WA(IJK)=II
            ENDIF
          ENDDO
c --------------          
         ENDIF  ! mte /= 13 
        ENDIF  
      ENDDO ! groupe
!   -----------------------------
            ENDIF
 666    continue    
        ENDDO
      DEALLOCATE(WWA)
C-----------
      RETURN
      END
