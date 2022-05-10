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
Chd|  THSOL                         source/output/th/thsol.F      
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SCOOR431                      source/elements/solid/sconnect/scoor431.F
Chd|        SCORTHO31                     source/elements/thickshell/solidec/scortho31.F
Chd|        SORTHO31                      source/elements/solid/solide/sortho31.F
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE THSOL(ELBUF_TAB,NTHGRP2 , ITHGRP , 
     .                 IPARG   ,ITHBUF   ,WA      ,
     .                 IXS     ,X        ,IPM    ,PM     ,IGEO    ,
     .                 MULTI_FVM,V       ,W      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD  
      USE MULTI_FVM_MOD             
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "alefvm.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP),ITHBUF(*),
     .        IXS(NIXS,*), IPM(NPROPMI,*),IGEO(NPROPGI,*),NTHGRP2
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
      my_real
     .   WA(*), X(3,*)  ,PM(NPROPM,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,I,J,JJ,K,L,N, IH, NG, MTE,LWA,NEL,
     .   NUVAR, IP,IPT,ISOLNOD,ITENS,IPWWA,ISPAU,IUWWA,
     .   IT,IR,IS,J1,J2,J3,NPTG,NPTR,NPTT,NPTS,NLAY,NFAIL,NVARF,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,KHBE,KCVT,NUVARTH,
     .   CPT,PID,ISVIS,TSHELL,TSH_ORT,ICSIG,IVISC,NPTL,IL,KK(6)
      INTEGER :: NITER,IADB,NN,IADV,NVAR,ITYP,IJK,IS_ALE
      my_real
     .   S11,S22,S33,S12,S23,S13,
     .   R11,R22,R33,R12,R21,R23,R32,R13,R31,
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31,
     .   T11,T22,T33,T12,T21,T23,T32,T13,T31,
     .   L11,L22,L33,L12,L21,L23,L32,L13,L31,
     .   X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     .   X5,Y5,Z5,X6,Y6,Z6,X7,Y7,Z7,X8,Y8,Z8, CS,SN,VAR,PLAG,
     .   SSP
        my_real 
     .   WWA(239552),A_GAUSS(9,9),SIGP(7,81,9), USER(100),
     .   STRAIN(6),GAMA(6),EVAR_TMP(6),EVAR(6),SIGG(6),
     .   TMP(3,8),VEL(3),V(3,*),W(3,*),TMP_2(MVSIZ,3)
C----  
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
      TYPE(FAIL_LOC_),POINTER :: FBUF  
C--------------------------------------------    
            DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
C======================================================================|
C             BRICKS - ELEMENTS SOLIDES
C-------------------------
C VAR(OLD) KEY    DESCRIPTION   [MAT LAW]
C
C    1     OFF
C    2     SX     SIGX
C    3     SY     SIGY
C    4     SZ     SIGZ
C    5     SXY    SIGXY
C    6     SYZ    SIGYZ
C    7     SXZ    SIGZX
C    8     IE     INTERNAL ENERGIE / VOLUME0
C    9     DENS   DENSITY
C   10     BULK   BULK VISCOSITY
C   11     VOL    VOLUME (ALE) OR INITIAL VOLUME (LAG)
C   12     PLAS   EPS PLASTIQUE [2,3,4,7,8,9,16,22,23,26,33-38] 
C   13     TEMP   TEMPERATURE   [4,6,7,8,9,11,16,17,26,33-38] 
C   14     PLSR   STRAIN RATE   [4,7,8,9,16,26,33-38] 
C   15     DAMA1  DAMAGE 1      [14] 
C   16     DAMA2  DAMAGE 2      [14] 
C   17     DAMA3  DAMAGE 3      [14] 
C   18     DAMA4  DAMAGE 4      [14] 
C   19     DAMA   DAMAGE        [24] 
C   20(14) SA1    STRESS RE1    [24] 
C   21(15) SA2    STRESS RE2    [24] 
C   22(16) SA3    STRESS RE3    [24] 
C   23(17) CR     CRACKS VOL    [24] 
C   24(18) CAP    CAP PARAM     [24]   (ROB)
C   25(13) K0     HARD. PARAM   [24] 
C   26(12) RK     TURBUL. ENER. [6,11,17] ,VK  [24]
C   27(14) TD     TURBUL. DISS. [6,11,17]
C   28(14) EFIB   FIBER STRAIN  [14] 
C   29(16) ISTA   PHASE STATE   [16] 
C   30(12) VPLA   VOL. EPS PLA. [10,21] 
C   31(12) BFRAC  BURN FRACTION [5,51] 
C   32(12) WPLA   PLAS. WORK    [14] 
C   35     LSX    SIGMA-X IN LOCAL SYSTEM (ONLY 8-NODES BRICKS)
C   36     LSY    SIGMA-Y IN LOCAL SYSTEM (ONLY 8-NODES BRICKS)
C   37     LSZ    SIGMA-Z IN LOCAL SYSTEM (ONLY 8-NODES BRICKS)
C   38     LSXY   SIGMA-XY IN LOCAL SYSTEM (ONLY 8-NODES BRICKS)
C   39     LSYZ   SIGMA-YZ IN LOCAL SYSTEM (ONLY 8-NODES BRICKS)
C   40     LSXZ   SIGMA-XZ IN LOCAL SYSTEM (ONLY 8-NODES BRICKS)
C   137    UVAR   User variables
C======================================================================|
        IJK = 0
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IADB =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==1)THEN
!   -------------------------------


      DO J1=1,7
        DO J2=1,9
          DO J3=1,9
            SIGP(J1,J2,J3) = ZERO
          ENDDO
        ENDDO
      ENDDO 
      NUVAR = 0
      IH=IADB
                               
C     decalage IH                             
      DO WHILE((ITHBUF(IH+NN) /= ISPMD).AND.(IH < IADB+NN))                
        IH = IH + 1                           
      ENDDO                                   
      IF (IH >= IADB+NN) GOTO 666               
C
c      ENDIF
C----------------------------------------------------------
      DO NG=1,NGROUP
       ITY     = IPARG(5,NG)
       ISVIS   = IPARG(60,NG)
       IVISC   = IPARG(61,NG)
c
       IF (ITY == ITYP) THEN
         GBUF => ELBUF_TAB(NG)%GBUF
         LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
         MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
         NLAY = ELBUF_TAB(NG)%NLAY                       
         NPTR = ELBUF_TAB(NG)%NPTR                        
         NPTS = ELBUF_TAB(NG)%NPTS                        
         NPTT = ELBUF_TAB(NG)%NPTT
C------
         CALL INITBUF ( IPARG    ,NG      ,                    
     2          MTE     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,KHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )  
         TSHELL  = 0                                             
         TSH_ORT = 0                                             
         IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) TSHELL = 1 
         IF (IGTYP==21 .OR. IGTYP==22) TSH_ORT = 1               
!
         DO I=1,6
           KK(I) = NEL*(I-1)
         ENDDO
!
C--------
         IF (MTE /= 0 .AND. MTE /= 13) THEN
          ISOLNOD=IPARG(28,NG)
          IS_ALE = IPARG(7,NG)
C
C         KCVT  0 GLOBAL FORMULATION, ISOTROPIC CASE
C         KCVT -1 GLOBAL FORMULATION, ORTHOTROPIC CASE
C         KCVT  1 CO-ROTATIONAL FORMULATION, ISOTROPIC CASE
C         KCVT  2 CO-ROTATIONAL FORMULATION, ORTHOTROPIC CASE
          IF (KCVT == 0 .AND. ISORTH > 0) KCVT=-1
          IF (KCVT == 1 .AND. ISORTH > 0) KCVT= 2
          IF (MTE >=28) NUVAR  = IPM(8,IXS(1,NFT+1))
C------------------------------------
C-------------------------------------------------- 
          IF(IS_ALE /= 0)THEN   
          !ale
            TMP_2(1:MVSIZ,1:3) = ZERO     
            DO J=1,8          
             DO  I=1,NEL    
              TMP_2(I,1)=TMP_2(I,1) + V(1,IXS(J+1,I+NFT))-W(1,IXS(J+1,I+NFT))
              TMP_2(I,2)=TMP_2(I,2) + V(2,IXS(J+1,I+NFT))-W(2,IXS(J+1,I+NFT))
              TMP_2(I,3)=TMP_2(I,3) + V(3,IXS(J+1,I+NFT))-W(3,IXS(J+1,I+NFT))
             ENDDO
            ENDDO
          ELSE
          !euler and lagrange
            TMP_2(1:MVSIZ,1:3) = ZERO     
            DO J=1,8          
             DO  I=1,NEL  
              TMP_2(I,1)=TMP_2(I,1)+V(1,IXS(J+1,I+NFT))               
              TMP_2(I,2)=TMP_2(I,2)+V(2,IXS(J+1,I+NFT))               
              TMP_2(I,3)=TMP_2(I,3)+V(3,IXS(J+1,I+NFT))  
             ENDDO
            ENDDO 
          ENDIF
C------------------------------------
C
          DO I=1,NEL
            N =I+NFT
            K =ITHBUF(IH)
            IP=ITHBUF(IH+NN)
c
            EVAR(1:6) = ZERO
            EVAR_TMP(1:6) = ZERO
            STRAIN(1:6) = ZERO
C
            IF (K == N)THEN
              IH=IH+1
C             traitement specifique spmd
              IF (IMACH == 3) THEN
C               recherche du ii correct
                II = ((IH-1) - IADB)*NVAR
                DO WHILE((ITHBUF(IH+NN) /= ISPMD).AND.
     .                   (IH < IADB+NN))
                  IH = IH + 1
                ENDDO
              ENDIF
c-----------
             IF (IH > IADB+NN) GOTO 666
c-----------
c              N1=NB10+I-1
c              N2=NB11+I-1
c              N3=NB12+I-1
              DO L=1,239552
                WWA(L)=ZERO
              ENDDO
              WWA(1) = GBUF%OFF(I)
              WWA(8) = GBUF%EINT(I)
c
              IF( NFILSOL .NE. 0 .AND. GBUF%G_FILL .NE. 0 ) THEN
                WWA(8) = WWA(8) * GBUF%FILL(I)
              ENDIF
c
              WWA(9) = GBUF%RHO(I)
              IF (GBUF%G_QVIS > 0) WWA(10)= GBUF%QVIS(I)
              WWA(11)= GBUF%VOL(I)
              IF(JLAG==1 .AND. GBUF%RHO(I).GT.ZERO)THEN
                WWA(11)=GBUF%VOL(I) * PM(89,IXS(1,NFT+I))/GBUF%RHO(I)    ! GBUF%VOL(I) = V0 for lagrangian solids ; (rho is optionnal for void material law)
              ENDIF
C-----------
C             STRESSES COMPUTING IN GLOBAL OR CONVECTED SYSTEM.
C-----------
              S11 = GBUF%SIG(KK(1)+I)
              S22 = GBUF%SIG(KK(2)+I)
              S33 = GBUF%SIG(KK(3)+I)
              S12 = GBUF%SIG(KK(4)+I)
              S23 = GBUF%SIG(KK(5)+I)
              S13 = GBUF%SIG(KK(6)+I)
C             just for isotropic 
              IF (ISVIS == 1.AND. MTE >=28 )THEN
                S11=S11 + LBUF%SIGV(KK(1)+I)
                S22=S22 + LBUF%SIGV(KK(2)+I)
                S33=S33 + LBUF%SIGV(KK(3)+I)
                S12=S12 + LBUF%SIGV(KK(4)+I)
                S23=S23 + LBUF%SIGV(KK(5)+I)
                S13=S13 + LBUF%SIGV(KK(6)+I)
              ENDIF
              
              IF (IVISC > 0  )THEN
                S11=S11 + LBUF%VISC(KK(1)+I)
                S22=S22 + LBUF%VISC(KK(2)+I)
                S33=S33 + LBUF%VISC(KK(3)+I)
                S12=S12 + LBUF%VISC(KK(4)+I)
                S23=S23 + LBUF%VISC(KK(5)+I)
                S13=S13 + LBUF%VISC(KK(6)+I)
              ENDIF 
              NC1=IXS(2,N)
              NC2=IXS(3,N)
              NC3=IXS(4,N)
              NC4=IXS(5,N)
              NC5=IXS(6,N)
              NC6=IXS(7,N)
              NC7=IXS(8,N)
              NC8=IXS(9,N)
              X1=X(1,NC1)
              Y1=X(2,NC1)
              Z1=X(3,NC1)
              X2=X(1,NC2)
              Y2=X(2,NC2)
              Z2=X(3,NC2)
              X3=X(1,NC3)
              Y3=X(2,NC3)
              Z3=X(3,NC3)
              X4=X(1,NC4)
              Y4=X(2,NC4)
              Z4=X(3,NC4)
              X5=X(1,NC5)
              Y5=X(2,NC5)
              Z5=X(3,NC5)
              X6=X(1,NC6)
              Y6=X(2,NC6)
              Z6=X(3,NC6)
              X7=X(1,NC7)
              Y7=X(2,NC7)
              Z7=X(3,NC7)
              X8=X(1,NC8)
              Y8=X(2,NC8)
              Z8=X(3,NC8)
C-----------
C             TENSOR ROTATION.
C             KCVT  0 GLOBAL FORMULATION, ISOTROPIC CASE
C             KCVT -1 GLOBAL FORMULATION, ORTHOTROPIC OR ISOTROPIC CASE (gama(1)=1000)
C             KCVT  1 CO-ROTATIONAL FORMULATION, ISOTROPIC CASE
C             KCVT  2 CO-ROTATIONAL FORMULATION, ORTHOTROPIC OR ISOTROPIC CASE
C                                                                        (gama(1)=1000)
C------------------------------------------------------------------------------
C                 1- TH tab filling with stresses in the global (WA(2:7) 
C                                              and local system(WA(35:40)
C------------------------------------------------------------------------------
              IF (KCVT > 0) THEN
C
c               ELEMENT CO-ROTATIONNEL.
C
                IF (IGTYP == 43) THEN   ! solid spotweld
                  CALL SCOOR431(                                  
     .              X1, X2, X3, X4, X5, X6, X7, X8,               
     .              Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,               
     .              Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,               
     .              R11, R12, R13, R21, R22, R23, R31, R32, R33)
c
                  IF( NFILSOL .NE. 0 .AND. GBUF%G_FILL .NE. 0 ) THEN
                    S11 = S11 * GBUF%FILL(I)
                    S22 = S22 * GBUF%FILL(I)
                    S33 = S33 * GBUF%FILL(I)
                    S12 = S12 * GBUF%FILL(I)
                    S23 = S23 * GBUF%FILL(I)
                    S13 = S13 * GBUF%FILL(I)
                  ENDIF
c
                  WWA(35)=S11  ! mean stress in local skew
                  WWA(36)=S22
                  WWA(37)=S33
                  WWA(38)=S12
                  WWA(39)=S23
                  WWA(40)=S13
                  L11=S11*R11+S12*R12+S13*R13 
                  L12=S11*R21+S12*R22+S13*R23 
                  L13=S11*R31+S12*R32+S13*R33
                  L21=S12*R11+S22*R12+S23*R13
                  L22=S12*R21+S22*R22+S23*R23
                  L23=S12*R31+S22*R32+S23*R33 
                  L31=S13*R11+S23*R12+S33*R13
                  L32=S13*R21+S23*R22+S33*R23
                  L33=S13*R31+S23*R32+S33*R33
                  S11=R11*L11+R12*L21+R13*L31
                  S22=R21*L12+R22*L22+R23*L32
                  S33=R31*L13+R32*L23+R33*L33
                  S12=R11*L12+R12*L22+R13*L32
                  S23=R21*L13+R22*L23+R23*L33
                  S13=R11*L13+R12*L23+R13*L33
                  WWA(2)=S11  ! mean stress in global skew
                  WWA(3)=S22
                  WWA(4)=S33
                  WWA(5)=S12
                  WWA(6)=S23
                  WWA(7)=S13
                ELSEIF (KHBE /= 24 .AND. KHBE /= 14) THEN
                  IF (KHBE /= 15) THEN
                   CALL SORTHO31(
     .               X1, X2, X3, X4, X5, X6, X7, X8,
     .               Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .               Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .               R11, R12, R13, R21, R22, R23, R31, R32, R33)
                 ELSE
c                  KHBE=15 :valeurs moyennes est dans rep. corota
                   CALL SCORTHO31(
     .               X1, X2, X3, X4, X5, X6, X7, X8,
     .               Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .               Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .               R11, R12, R13, R21, R22, R23, R31, R32, R33)
                 END IF
c
                 IF (KCVT == 2) THEN
                   IF (ISORTH > 0) THEN
c                     REPERE ORTHOTROPE.
                      IF (KHBE /= 15) THEN
                       G11=GBUF%GAMA(KK(1)+I)
                       G21=GBUF%GAMA(KK(2)+I)
                       G31=GBUF%GAMA(KK(3)+I)
                       G12=GBUF%GAMA(KK(4)+I)
                       G22=GBUF%GAMA(KK(5)+I)
                       G32=GBUF%GAMA(KK(6)+I)
                       G13=G21*G32-G31*G22
                       G23=G31*G12-G11*G32
                       G33=G11*G22-G21*G12
                     ELSE
                       CS = GBUF%GAMA(KK(1)+I)
                       SN = GBUF%GAMA(KK(2)+I)
                       G11=CS
                       G12=SN
                       G13=ZERO
                       G21=-SN
                       G22=CS
                       G23=ZERO
                       G31=ZERO
                       G32=ZERO
                       G33=UN
                     END IF
C                    MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
                     T11=R11*G11+R12*G21+R13*G31
                     T12=R11*G12+R12*G22+R13*G32
                     T13=R11*G13+R12*G23+R13*G33
                     T21=R21*G11+R22*G21+R23*G31
                     T22=R21*G12+R22*G22+R23*G32
                     T23=R21*G13+R22*G23+R23*G33
                     T31=R31*G11+R32*G21+R33*G31
                     T32=R31*G12+R32*G22+R33*G32
                     T33=R31*G13+R32*G23+R33*G33
                     R11=T11
                     R12=T12
                     R13=T13
                     R21=T21
                     R22=T22
                     R23=T23
                     R31=T31
                     R32=T32
                     R33=T33
                   ENDIF
                  ENDIF  ! kcvt = 2
c
                 IF( NFILSOL .NE. 0 .AND. GBUF%G_FILL .NE. 0 ) THEN
                   S11 = S11 * GBUF%FILL(I)
                   S22 = S22 * GBUF%FILL(I)
                   S33 = S33 * GBUF%FILL(I)
                   S12 = S12 * GBUF%FILL(I)
                   S23 = S23 * GBUF%FILL(I)
                   S13 = S13 * GBUF%FILL(I)
                 ENDIF
c
                 WWA(35)=S11
                 WWA(36)=S22
                 WWA(37)=S33
                 WWA(38)=S12
                 WWA(39)=S23
                 WWA(40)=S13
                 L11=S11*R11+S12*R12+S13*R13 
                 L12=S11*R21+S12*R22+S13*R23 
                 L13=S11*R31+S12*R32+S13*R33
                 L21=S12*R11+S22*R12+S23*R13
                 L22=S12*R21+S22*R22+S23*R23
                 L23=S12*R31+S22*R32+S23*R33 
                 L31=S13*R11+S23*R12+S33*R13
                 L32=S13*R21+S23*R22+S33*R23
                 L33=S13*R31+S23*R32+S33*R33
                 S11=R11*L11+R12*L21+R13*L31
                 S22=R21*L12+R22*L22+R23*L32
                 S33=R31*L13+R32*L23+R33*L33
                 S12=R11*L12+R12*L22+R13*L32
                 S23=R21*L13+R22*L23+R23*L33
                 S13=R11*L13+R12*L23+R13*L33
                 WWA(2)=S11
                 WWA(3)=S22
                 WWA(4)=S33
                 WWA(5)=S12
                 WWA(6)=S23
                 WWA(7)=S13
               ELSE            !  KHBE == 24.OR.KHBE == 14
                 CALL SORTHO31(
     .            X1, X2, X3, X4, X5, X6, X7, X8,
     .            Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .            Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .            R12, R13, R11, R22, R23, R21, R32, R33, R31)
                 IF (KCVT == 2) THEN
                   IF (ISORTH > 0 .AND. KHBE/=14) THEN
C                    KHBE=14 :valeurs moyennes est dans rep. corota
                     G11=GBUF%GAMA(KK(1)+I)
                     G21=GBUF%GAMA(KK(2)+I)
                     G31=GBUF%GAMA(KK(3)+I)
                     G12=GBUF%GAMA(KK(4)+I)
                     G22=GBUF%GAMA(KK(5)+I)
                     G32=GBUF%GAMA(KK(6)+I)
                     G13=G21*G32-G31*G22
                     G23=G31*G12-G11*G32
                     G33=G11*G22-G21*G12
C                    MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
                     T11=R11*G11+R12*G21+R13*G31
                     T12=R11*G12+R12*G22+R13*G32
                     T13=R11*G13+R12*G23+R13*G33
                     T21=R21*G11+R22*G21+R23*G31
                     T22=R21*G12+R22*G22+R23*G32
                     T23=R21*G13+R22*G23+R23*G33
                     T31=R31*G11+R32*G21+R33*G31
                     T32=R31*G12+R32*G22+R33*G32
                     T33=R31*G13+R32*G23+R33*G33
                     R11=T11
                     R12=T12
                     R13=T13
                     R21=T21
                     R22=T22
                     R23=T23
                     R31=T31
                     R32=T32
                     R33=T33
                   END IF
                 END IF
c
                 IF( NFILSOL .NE. 0 .AND. GBUF%G_FILL .NE. 0 ) THEN
                   S11 = S11 * GBUF%FILL(I)
                   S22 = S22 * GBUF%FILL(I)
                   S33 = S33 * GBUF%FILL(I)
                   S12 = S12 * GBUF%FILL(I)
                   S23 = S23 * GBUF%FILL(I)
                   S13 = S13 * GBUF%FILL(I)
                 ENDIF
c
                 WWA(35)=S11
                 WWA(36)=S22
                 WWA(37)=S33
                 WWA(38)=S12
                 WWA(39)=S23
                 WWA(40)=S13
                 L11=S11*R11+S12*R12+S13*R13 
                 L12=S11*R21+S12*R22+S13*R23 
                 L13=S11*R31+S12*R32+S13*R33
                 L21=S12*R11+S22*R12+S23*R13
                 L22=S12*R21+S22*R22+S23*R23
                 L23=S12*R31+S22*R32+S23*R33 
                 L31=S13*R11+S23*R12+S33*R13
                 L32=S13*R21+S23*R22+S33*R23
                 L33=S13*R31+S23*R32+S33*R33
                 S11=R11*L11+R12*L21+R13*L31
                 S22=R21*L12+R22*L22+R23*L32
                 S33=R31*L13+R32*L23+R33*L33
                 S12=R11*L12+R12*L22+R13*L32
                 S23=R21*L13+R22*L23+R23*L33
                 S13=R11*L13+R12*L23+R13*L33
                 WWA(2)=S11
                 WWA(3)=S22
                 WWA(4)=S33
                 WWA(5)=S12
                 WWA(6)=S23
                 WWA(7)=S13
                END IF   ! igtyp, khbe
C------------------------------------
              ELSE    ! KCVT <= 0
C------------------------------
C
c               element non-corotationnel : no rotation SX,SY,SZ,SXY,SXZ,SYZ
C                      and LSX,LSY,LSZ,LSXY,LSXZ,LSYZ are in both in global system
C
c
                WWA(2)=S11
                WWA(3)=S22
                WWA(4)=S33
                WWA(5)=S12
                WWA(6)=S23
                WWA(7)=S13
c
                WWA(35)=S11
                WWA(36)=S22
                WWA(37)=S33
                WWA(38)=S12
                WWA(39)=S23
                WWA(40)=S13
C--------------------------------------------------------------------------
              ENDIF   ! KCVT
C--------------------------------------------------
c             LOOP NEL, Filling TH Buffer
C-----------------------------------------------------
              IF (MTE == 2) THEN
                WWA(12)=GBUF%PLA(I)
                IF ( JTHE  ==  0 ) THEN
                  WWA(13)= PM(54,IXS(1,NFT+I)) + 
     .            WWA(8) * WWA(11) * PM(53,IXS(1,NFT+I))
                ENDIF
              ELSEIF (MTE == 3) THEN
                WWA(12)=GBUF%PLA(I)
                WWA(13)=GBUF%TEMP(I)
              ELSEIF (MTE == 4) THEN
                WWA(12)=GBUF%PLA(I)
                WWA(13)=GBUF%TEMP(I)
                WWA(14)=GBUF%EPSD(I)
              ELSEIF (MTE == 5) THEN
                WWA(13)=GBUF%TEMP(I)
                WWA(31)=GBUF%BFRAC(I)
              ELSEIF (MTE == 6) THEN
                WWA(13)=GBUF%TEMP(I)
                WWA(26)=LBUF%RK(I)
                WWA(27)=LBUF%RE(I)
              ELSEIF (MTE == 7.OR.MTE == 8.OR.MTE == 9) THEN
                WWA(12)=ZERO
                WWA(13)=ZERO
                WWA(14)=ZERO
              ELSEIF (MTE == 10) THEN
                WWA(12)=GBUF%EPSQ(I)     !/TH (EPSP)
                WWA(30)=GBUF%PLA(I)      !/TH (VPLA)
              ELSEIF (MTE == 11) THEN
                WWA(13)=LBUF%TEMP(I)
                WWA(26)=LBUF%RK(I)
                WWA(27)=LBUF%RE(I)
              ELSEIF (MTE == 14) THEN
                WWA(32)=LBUF%PLA(I)      !N1
                WWA(33)=LBUF%SIGF(I)     !N2
                WWA(28)=LBUF%EPSF(I)     !N3
                WWA(15)=LBUF%DAM(KK(1)+I)   !N4
                WWA(16)=LBUF%DAM(KK(2)+I)
                WWA(17)=LBUF%DAM(KK(3)+I)
                WWA(18)=LBUF%DAM(KK(4)+I)
                WWA(34)=LBUF%DAM(KK(5)+I)
             ELSEIF (MTE == 16) THEN
                WWA(12)=LBUF%PLA(I)   !N1
                WWA(13)=LBUF%TEMP(I)  !N2
                WWA(14)=LBUF%EPSD(I)
              ELSEIF (MTE == 17) THEN
                IF (ITHERM > 0) WWA(13)=LBUF%TEMP(I)
                WWA(26)=LBUF%RK(I)
                WWA(27)=LBUF%RE(I)
              ELSEIF (MTE == 18) THEN
                WWA(13)=LBUF%TEMP(I)
              ELSEIF (MTE == 20) THEN
                WWA(12)=ZERO
                WWA(13)=ZERO
              ELSEIF (MTE == 21) THEN
                WWA(12)=LBUF%EPSQ(I) ! NB11
                WWA(30)=GBUF%PLA(I)  ! NB10
              ELSEIF (MTE == 22.OR.MTE == 23) THEN
                WWA(12)=LBUF%PLA(I)
              ELSEIF (MTE == 24) THEN
                WWA(15)=LBUF%DAM(KK(1)+I)
                WWA(16)=LBUF%DAM(KK(2)+I)
                WWA(17)=LBUF%DAM(KK(3)+I)
                WWA(19)=LBUF%DAM(KK(1)+I)+LBUF%DAM(KK(2)+I)+LBUF%DAM(KK(3)+I)
                WWA(20)=LBUF%SIGA(KK(1)+I)
                WWA(21)=LBUF%SIGA(KK(2)+I)
                WWA(22)=LBUF%SIGA(KK(3)+I)
                WWA(23)=LBUF%CRAK(KK(1)+I)+LBUF%CRAK(KK(2)+I)+LBUF%CRAK(KK(3)+I)
                WWA(24)=LBUF%ROB(I)
                WWA(25)=LBUF%VK(I)
                WWA(26)=LBUF%RK(I)
                WWA(12)=LBUF%PLA(I)
                WWA(30)=GBUF%PLA(I)                                 
              ELSEIF (MTE == 26) THEN
                WWA(12)=LBUF%PLA(I)
                WWA(13)=LBUF%TEMP(I)
                WWA(14)=LBUF%Z(I)
              ELSEIF (MTE == 32.OR.MTE == 43) THEN   ! not solid compatible !!
                WWA(12)=ZERO
                WWA(13)=ZERO
                WWA(14)=ZERO
              ELSEIF (MTE == 46.OR.MTE == 47) THEN
                WWA(12)=MBUF%VAR(I)
                WWA(13)=MBUF%VAR(I+NEL)
c                WWA(14)=MBUF%VAR(I+NEL*2)    
              ELSEIF (MTE == 49) THEN
                WWA(12)=LBUF%PLA(I)
                WWA(13)=LBUF%TEMP(I)
                WWA(14)=LBUF%EPSD(I)
              ELSEIF (MTE == 28) THEN
              ELSEIF (MTE == 33) THEN
              ELSEIF (MTE == 51) THEN              
               IF(GBUF%G_PLA>0)  WWA(12)=GBUF%PLA(I)
                                 WWA(13)=GBUF%TEMP(I)
               IF(GBUF%G_EPSD>0) WWA(14)=GBUF%EPSD(I)
               IF(GBUF%G_BFRAC>0)WWA(31)=GBUF%BFRAC(I) !Means Explosive submaterial is used
               IF(GBUF%G_EPSQ>0) THEN                  !Means Drucker-Prager criteria is used
                                 WWA(12)=GBUF%EPSQ(I)
                                 WWA(30)=GBUF%PLA(I)                                 
               ENDIF
              ELSEIF (MTE == 59) THEN
C               Solid spotwelds : damage  DAMA1 ...DAMA4               
                NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL
                DO J=1,NPTR
                  DO K=1,NFAIL
                    FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(J,1,1)%FLOC(K)
                    NVARF = FBUF%NVAR
                    DO L=1,NVARF
                      VAR = FBUF%VAR((L-1)*NEL+I)
                      WWA(136+L) = MAX(WWA(136+L), VAR)
                    ENDDO
                  ENDDO
                ENDDO
                VAR = MAX(WWA(15),WWA(16))
                VAR = MAX(WWA(17),VAR)
                VAR = MAX(WWA(18),VAR)
                WWA(19) = VAR    ! DAMA = max(dama1,dama2,dama3,dama4)
                WWA(14) = GBUF%EPSD(I)

              ELSEIF (MTE == 83) THEN
                WWA(12)=GBUF%PLA(I)
                NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
                DO J=1,NPTR
                  MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(J,1,1)
                  DO L=1,NUVAR                                                
                    VAR = MBUF%VAR((L-1)*NEL+I)
                    WWA(136+L) = MAX(WWA(136+L), VAR)                         
                  ENDDO                                                       
                ENDDO
              ELSEIF (MTE == 116) THEN
                WWA(12) = GBUF%PLA(I)
                NUVAR   = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
                DO J=1,NPTR
                  MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(J,1,1)
                  DO L=1,NUVAR                                                
                    VAR = MBUF%VAR((L-1)*NEL+I)
                    WWA(136+L) = MAX(WWA(136+L), VAR)                         
                  ENDDO                                                       
                ENDDO
              ELSEIF (MTE == 67) THEN
C               Temperature
                WWA(12)=ZERO
                WWA(13)=MBUF%VAR(I)
                WWA(14)=ZERO
              ELSEIF (MTE == 103) THEN
C             Hensel Spittel
                WWA(12)=LBUF%PLA(I)
                WWA(13)=MBUF%VAR(I)
                WWA(14)=LBUF%EPSD(I)
              ELSEIF (MTE > 28) THEN
                IF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA > 0)THEN
                  WWA(12)=LBUF%PLA(I)
                ELSE
                  WWA(12)=ZERO
                ENDIF
                IF (ELBUF_TAB(NG)%BUFLY(1)%L_TEMP > 0)THEN
                  WWA(13)=LBUF%TEMP(I)
                ELSE
                  WWA(13)=ZERO
                ENDIF
                IF (ELBUF_TAB(NG)%BUFLY(1)%L_EPSD > 0)THEN
                  WWA(14)=LBUF%EPSD(I)
                ELSE
                  WWA(14)=ZERO
                ENDIF
C               User laws for solids
c                WWA(12)=BUFEL(N1)
c                WWA(13)=BUFEL(N2)
c                WWA(14)=BUFEL(N3)
C               User laws for solids - max 60 user variables.
                NUVARTH = MIN(60,NUVAR)
                DO J=1,NUVARTH
                  WWA(136+J)=MBUF%VAR((J-1)*NEL+I)
                ENDDO
              ENDIF
C------------------------------------------------------------------------------
C                    
              IF (MTE==151) THEN
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

              ELSEIF(I_ALE_SOLVER.GT.1)THEN
C SSP
                WWA(239550)= LBUF%SSP(I)                                 
                IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP .NE. 0)THEN   
                  VEL(1) = GBUF%MOM(I) / GBUF%RHO(I) 
                  VEL(2) = GBUF%MOM(NEL + I) / GBUF%RHO(I)
                  VEL(3) = GBUF%MOM(2*NEL+ I) / GBUF%RHO(I) 
                  WWA(239547)= VEL(1)
                  WWA(239548)= VEL(2)
                  WWA(239549)= VEL(3)
                  WWA(239551)= SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I) 
                ENDIF                     

              ELSE IF (MTE==51) THEN
C SSP
                WWA(239550)= LBUF%SSP(I)                                                      
                IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP .NE. 0)THEN 
                   VEL(1) = TMP_2(I,1)*UNHUIT!SUM(TMP(1,1:8))*UNHUIT              
                   VEL(2) = TMP_2(I,2)*UNHUIT!SUM(TMP(2,1:8))*UNHUIT              
                   VEL(3) = TMP_2(I,3)*UNHUIT!SUM(TMP(3,1:8))*UNHUIT   
                   WWA(239547)= VEL(1)
                   WWA(239548)= VEL(2)
                   WWA(239549)= VEL(3)            
                   WWA(239551)=  SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)
                ENDIF
              ENDIF
C------------------------------------------------------------------------------
C                 2- TH tab filling with stresses  and stain in element 
C                           and per intergation point
C                    *** Property Type22: output SIG_IK_J =>  WA(120338)
C                                                EPS_IK_J =>  WA(1646)
C                    *** Solid 16 nodes, 20nodes, 8 nodes (KHBE 14,17), 8 nodes and 6 nodes
C                        SXIJK,SYIJK,SZIJK,SXYIJK,SXZIJK,SYZIIJK EPIJK  => WA(196)
C                        EPSXIJK,EPSYIJK,EPSZIJK,EPSXYIJK,EPSXZIJK,EPSYZIIJK => WWA(239060)
C                    ***  All solids 
C                        EPSXX,EPSYY,EPSZZ,EPSXY,EPSXZ,EPSYZ  =>  WWA(1618)
C                        L_EPSXX,L_EPSYY,L_EPSZZ,L_EPSXY,LEPSXZ,LEPSYZ  => WWA(239030)
C------------------------------------------------------------------------------
              IF (KCVT > 0) THEN   
C
c               ELEMENT CO-ROTATIONNEL.
C
                IF (ISOLNOD == 4) THEN
C
                ELSEIF (ISOLNOD == 10) THEN
C
                ELSEIF (ISOLNOD == 8.AND. IGTYP == 43) THEN  
c----------------------------------------------------------------------------
C------------------------Output EPS L_EPS---------------------
C---------------------------------------------------------------------------
                  DO IPT=1,NPT                                       
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)   
                    DO J=1,3
                       STRAIN(J) = STRAIN(J) + LBUF%EPE(KK(J)+I)/NPT 
                    ENDDO  
                  ENDDO
              
                  WWA(239030 + 3) = STRAIN(3) ! mean strain in local skew
                  WWA(239030 + 2) = STRAIN(2)
                  WWA(239030 + 1) = STRAIN(1)    
c---------------------Rotation to the global system for EPSXX.. ------
                  GAMA(1)=UN
                  GAMA(2)=ZERO
                  GAMA(3)=ZERO
                  GAMA(4)=ZERO
                  GAMA(5)=UN
                  GAMA(6)=ZERO

                  CALL SROTA6(X,IXS(1,N),KCVT,
     .                        STRAIN,GAMA, KHBE,IGTYP)       

                  DO J=1,3
                    WWA(1618 + J) = STRAIN(J)  ! mean stress in global skew
                  ENDDO  
c---------------
                ELSEIF (ISOLNOD==8 .AND. KHBE/=14 .AND. KHBE/=15 .AND.
     .                                   KHBE/=17) THEN     
c----------------------------------------------------------------------------
C------------------------Output SIJK EPS L_EPS-------------------------------
C---------------------------------------------------------------------------
c
c                 brick 8 noeuds std
c---------------
                  IF (NPT == 8)THEN
                    JJ = 6*(I-1) 
                    IF (ELBUF_TAB(NG)%BUFLY(1)%L_SIGL > 0) THEN
                      DO IPT=1,NPT
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
C-----------------------------------
c                     CO-ROTATIONAL FORMULATION ONLY:
c                      88+I     LSX au point de Gauss I
c                      96+I     LSY au point de Gauss I
c                     104+I     LSZ au point de Gauss I
c                     112+I     LSXY au point de Gauss I
c                     120+I     LSYZ au point de Gauss I
c                     128+I     LSXZ au point de Gauss I
C-----------------------------------
                        WWA( 88+IPT) = LBUF%SIGL(KK(1)+I)
                        WWA( 96+IPT) = LBUF%SIGL(KK(2)+I)
                        WWA(104+IPT) = LBUF%SIGL(KK(3)+I)
                        WWA(112+IPT) = LBUF%SIGL(KK(4)+I)
                        WWA(120+IPT) = LBUF%SIGL(KK(5)+I)
                        WWA(128+IPT) = LBUF%SIGL(KK(6)+I)
                      ENDDO
                    ELSE IF(KHBE == 12)THEN
                      DO IPT=1,NPT
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)
                        WWA( 88+IPT) = LBUF%SIG(KK(1)+I)
                        WWA( 96+IPT) = LBUF%SIG(KK(2)+I)
                        WWA(104+IPT) = LBUF%SIG(KK(3)+I)
                        WWA(112+IPT) = LBUF%SIG(KK(4)+I)
                        WWA(120+IPT) = LBUF%SIG(KK(5)+I)
                        WWA(128+IPT) = LBUF%SIG(KK(6)+I)
                      ENDDO
                      IF(IVISC > 0 ) THEN
                       DO IPT=1,NPT
                         LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)
                         WWA( 88+IPT) =WWA( 88+IPT) + LBUF%VISC(KK(1)+I)
                         WWA( 96+IPT) =WWA( 96+IPT) + LBUF%VISC(KK(2)+I)
                         WWA(104+IPT) =WWA(104+IPT) + LBUF%VISC(KK(3)+I)
                         WWA(112+IPT) =WWA(112+IPT) + LBUF%VISC(KK(4)+I)
                         WWA(120+IPT) =WWA(120+IPT) + LBUF%VISC(KK(5)+I)
                         WWA(128+IPT) =WWA(128+IPT) + LBUF%VISC(KK(6)+I)
                       ENDDO
                      ENDIF
                    ELSE
                      DO IPT=1,NPT
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
                        WWA( 88+IPT) = LBUF%SIG(KK(1)+I)
                        WWA( 96+IPT) = LBUF%SIG(KK(2)+I)
                        WWA(104+IPT) = LBUF%SIG(KK(3)+I)
                        WWA(112+IPT) = LBUF%SIG(KK(4)+I)
                        WWA(120+IPT) = LBUF%SIG(KK(5)+I)
                        WWA(128+IPT) = LBUF%SIG(KK(6)+I)
                      ENDDO
                      IF(IVISC > 0 ) THEN
                       DO IPT=1,NPT
                         LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
                         WWA( 88+IPT) =WWA( 88+IPT) + LBUF%VISC(KK(1)+I)
                         WWA( 96+IPT) =WWA( 96+IPT) + LBUF%VISC(KK(2)+I)
                         WWA(104+IPT) =WWA(104+IPT) + LBUF%VISC(KK(3)+I)
                         WWA(112+IPT) =WWA(112+IPT) + LBUF%VISC(KK(4)+I)
                         WWA(120+IPT) =WWA(120+IPT) + LBUF%VISC(KK(5)+I)
                         WWA(128+IPT) =WWA(128+IPT) + LBUF%VISC(KK(6)+I)
                       ENDDO
                      ENDIF
                    ENDIF
                    IF(KHBE == 12)THEN
                      DO IPT=1,NPT                                    
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)  
                        IF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                          STRAIN(1) = STRAIN(1) + LBUF%STRA(KK(1)+I)*UNHUIT
                          STRAIN(2) = STRAIN(2) + LBUF%STRA(KK(2)+I)*UNHUIT
                          STRAIN(3) = STRAIN(3) + LBUF%STRA(KK(3)+I)*UNHUIT
                          STRAIN(4) = STRAIN(4) + LBUF%STRA(KK(4)+I)*UNHUIT
                          STRAIN(5) = STRAIN(5) + LBUF%STRA(KK(5)+I)*UNHUIT
                          STRAIN(6) = STRAIN(6) + LBUF%STRA(KK(6)+I)*UNHUIT
                        ENDIF
                      ENDDO
                    ELSE
                      DO IPT=1,NPT                                    
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)  
                        IF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                          STRAIN(1) = STRAIN(1) + LBUF%STRA(KK(1)+I)*UNHUIT
                          STRAIN(2) = STRAIN(2) + LBUF%STRA(KK(2)+I)*UNHUIT
                          STRAIN(3) = STRAIN(3) + LBUF%STRA(KK(3)+I)*UNHUIT
                          STRAIN(4) = STRAIN(4) + LBUF%STRA(KK(4)+I)*UNHUIT
                          STRAIN(5) = STRAIN(5) + LBUF%STRA(KK(5)+I)*UNHUIT
                          STRAIN(6) = STRAIN(6) + LBUF%STRA(KK(6)+I)*UNHUIT
                        ENDIF
                      ENDDO
                    ENDIF
C-----------------in local ref : L_EPSX............-----------
                    DO J= 1,6
                       WWA(239030 + J) = STRAIN(J)
                    ENDDO
C-----------------in global ref : EPSX............-----------
                    IF(KCVT==2)THEN
                        GAMA(1)=GBUF%GAMA(KK(1) + I)
                        GAMA(2)=GBUF%GAMA(KK(2) + I)
                        GAMA(3)=GBUF%GAMA(KK(3) + I)
                        GAMA(4)=GBUF%GAMA(KK(4) + I)
                        GAMA(5)=GBUF%GAMA(KK(5) + I)
                        GAMA(6)=GBUF%GAMA(KK(6) + I)
                     ELSE
                        GAMA(1)=UN
                        GAMA(2)=ZERO
                        GAMA(3)=ZERO
                        GAMA(4)=ZERO
                        GAMA(5)=UN
                        GAMA(6)=ZERO
                     END IF

                     CALL SROTA6(X,IXS(1,N),KCVT,
     .                        STRAIN,GAMA, KHBE,IGTYP)       

                     DO J=1,3
                        WWA(1618 + J) = STRAIN(J)  ! mean strain in global skew
                     ENDDO
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                     WWA(1618 + 4) = STRAIN(4)
                     WWA(1618 + 5) = STRAIN(6)
                     WWA(1618 + 6) = STRAIN(5)  

c--------
                  ELSEIF (NPT == 1) THEN
c--------
                    DO J=1,6
                      STRAIN(J) = ZERO
                    ENDDO
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                    IF (MTE == 12 .OR. MTE == 14) THEN
                      DO J= 1,3
                        WWA(239030 + J) = LBUF%EPE(KK(J)+I)
                        STRAIN(J) = LBUF%EPE(KK(J)+I)
                      ENDDO
                    ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                      DO J= 1,3
                        WWA(239030 + J) = LBUF%STRA(KK(J)+I)
                        STRAIN(J) = LBUF%STRA(KK(J)+I)
                      ENDDO
                      DO J= 4,6
                        WWA(239030 + J) = LBUF%STRA(KK(J)+I)*UNDEMI
                        STRAIN(J) = LBUF%STRA(KK(J)+I)*UNDEMI
                      ENDDO

                    ENDIF 

C-----------------in local ref : L_EPSX............-----------
                    DO J= 1,6
                       WWA(239030 + J) = STRAIN(J)
                    ENDDO
C-----------------in global ref : EPSX............-----------
                    IF(KCVT==2)THEN
                        GAMA(1)=GBUF%GAMA(KK(1) + I)
                        GAMA(2)=GBUF%GAMA(KK(2) + I)
                        GAMA(3)=GBUF%GAMA(KK(3) + I)
                        GAMA(4)=GBUF%GAMA(KK(4) + I)
                        GAMA(5)=GBUF%GAMA(KK(5) + I)
                        GAMA(6)=GBUF%GAMA(KK(6) + I)
                    ELSE
                        GAMA(1)=UN
                        GAMA(2)=ZERO
                        GAMA(3)=ZERO
                        GAMA(4)=ZERO
                        GAMA(5)=UN
                        GAMA(6)=ZERO
                    END IF

                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                        STRAIN,GAMA, KHBE,IGTYP)       

                    DO J=1,3
                       WWA(1618 + J) = STRAIN(J)  ! mean stress in global skew
                    ENDDO  
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                     WWA(1618 + 4) = STRAIN(4)
                     WWA(1618 + 5) = STRAIN(6)
                     WWA(1618 + 6) = STRAIN(5)

                 ENDIF     ! NPT
c---------------
c                ELSEIF((ISOLNOD == 16.OR.(ISOLNOD == 8 .AND.KHBE == 14)
c     .            .OR.((ISOLNOD ==  6.OR. ISOLNOD == 8).AND.KHBE == 15))
c     .            .AND. IGTYP == 22) THEN

                ELSEIF (TSHELL == 1) THEN
c----------------------------------------------------------------------------
C------------------------Output SIG_IK_J SIJK EPS L_EPS---------------------
C---------------------------------------------------------------------------

                  PID=IXS(10,1 + NFT)
                  NPTG = NPTR * NPTS * NLAY
                  JJ = 6*(I-1)
                  DO IR=1,NPTR
                   DO IS=1,NPTS
                    DO IT=1,NLAY
                     IF (MTE == 12 .OR. MTE == 14)THEN
                       DO J=1,3
                        EVAR_TMP(J) = LBUF%EPE(KK(J)+I)
                       ENDDO 
                       EVAR_TMP(3:6) = ZERO
                     ENDIF       
       
                     IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR

                     IF (IPT  <= NPTG .AND. IR <= NPTR .AND. IS <= NPTS
     .                    .AND. IT <= NLAY) THEN
                      IF (ELBUF_TAB(NG)%BUFLY(IT)%L_STRA > 0) THEN 
                       LBUF => ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1)
                       EVAR_TMP(1) = LBUF%STRA(KK(1)+I)
                       EVAR_TMP(2) = LBUF%STRA(KK(2)+I)
                       EVAR_TMP(3) = LBUF%STRA(KK(3)+I)
                       EVAR_TMP(4) = LBUF%STRA(KK(4)+I)*UNDEMI
                       EVAR_TMP(5) = LBUF%STRA(KK(5)+I)*UNDEMI
                       EVAR_TMP(6) = LBUF%STRA(KK(6)+I)*UNDEMI
                      ENDIF

                      DO J = 1, 6                    
                           STRAIN(J) = STRAIN(J) + EVAR_TMP(J)/NPTG
                       ENDDO 

C                        STRAIN TENSOR IN GLOBAL SYSTEM
                       ICSIG=IPARG(17,NG)  
                       IF (KHBE == 14.AND.ICSIG > 0) THEN
                           SELECT CASE (ICSIG)                                            
                           CASE (1)          
                                 IF(KCVT==2)THEN
                                   GAMA(1)= ZERO
                                   GAMA(2)= LBUF%GAMA(KK(1)+I)
                                   GAMA(3)= LBUF%GAMA(KK(2)+I)
                                   GAMA(4)= ZERO
                                   GAMA(5)=-GAMA(2)
                                   GAMA(6)= GAMA(1)
                                 ELSE
                                   GAMA(1)=UN
                                   GAMA(2)=ZERO
                                   GAMA(3)=ZERO
                                   GAMA(4)=ZERO
                                   GAMA(5)=UN
                                   GAMA(6)=ZERO
                                 END IF
                           CASE (10)           
                                 IF(KCVT==2)THEN
                                   GAMA(1)= LBUF%GAMA(KK(1)+I)
                                   GAMA(2)= LBUF%GAMA(KK(2)+I)
                                   GAMA(3)= ZERO
                                   GAMA(4)=-GAMA(2)
                                   GAMA(5)= GAMA(1)
                                   GAMA(6)= ZERO
                                 ELSE
                                   GAMA(1)=UN
                                   GAMA(2)=ZERO
                                   GAMA(3)=ZERO
                                   GAMA(4)=ZERO
                                   GAMA(5)=UN
                                   GAMA(6)=ZERO
                                 END IF
                           CASE (100)          
                                 IF(KCVT==2)THEN
                                   GAMA(1)= LBUF%GAMA(KK(2)+I)
                                   GAMA(2)= ZERO
                                   GAMA(3)= LBUF%GAMA(KK(1)+I)
                                   GAMA(4)= GAMA(3)
                                   GAMA(5)= ZERO
                                   GAMA(6)=-GAMA(1)
                                 ELSE
                                   GAMA(1)=UN
                                   GAMA(2)=ZERO
                                   GAMA(3)=ZERO
                                   GAMA(4)=ZERO
                                   GAMA(5)=UN
                                   GAMA(6)=ZERO
                                 END IF
                           END SELECT     
                         ELSE

                           IF(KCVT==2)THEN				   
                             GAMA(1)=GBUF%GAMA(KK(1) + I)			
                             GAMA(2)=GBUF%GAMA(KK(2) + I)			
                             GAMA(3)=GBUF%GAMA(KK(3) + I)			
                             GAMA(4)=GBUF%GAMA(KK(4) + I)			
                             GAMA(5)=GBUF%GAMA(KK(5) + I)			
                             GAMA(6)=GBUF%GAMA(KK(6) + I)			
                           ELSE					   
                             GAMA(1)=UN				   
                             GAMA(2)=ZERO				   
                             GAMA(3)=ZERO				   
                             GAMA(4)=ZERO				   
                             GAMA(5)=UN				   
                             GAMA(6)=ZERO				   
                          END IF

                        ENDIF

                        CALL SROTA6(X,IXS(1,N),KCVT,
     .                         EVAR_TMP,GAMA, KHBE,IGTYP)   
                                                        
                         DO J = 1, 6                    
                           EVAR(J) = EVAR(J) + EVAR_TMP(J)/NPTG
                         ENDDO 
C
                        IF(IGTYP == 22) THEN
C------------------------Output SIG_IK_J---------------------
                         MBUF => ELBUF_TAB(NG)%BUFLY(IT)%MAT(IR,IS,1)
                         CPT=(IT-1)*9*9*6+((IR-1)*9+IS-1)*6
c S11
                         WWA(98846+CPT+1) = LBUF%SIG(KK(1)+I)
c S12
                         WWA(98846+CPT+2) = LBUF%SIG(KK(4)+I)
c S13
                         WWA(98846+CPT+3) = LBUF%SIG(KK(6)+I)
c S22
                         WWA(98846+CPT+4) = LBUF%SIG(KK(2)+I)
c S23
                         WWA(98846+CPT+5) = LBUF%SIG(KK(5)+I)
c S33
                         WWA(98846+CPT+6) = LBUF%SIG(KK(3)+I)
                         IF(IVISC > 0) THEN
                            WWA(98846+CPT+1)=WWA(98846+CPT+1) + LBUF%VISC(KK(1)+I)
                            WWA(98846+CPT+2)=WWA(98846+CPT+2) + LBUF%VISC(KK(4)+I)
                            WWA(98846+CPT+3)=WWA(98846+CPT+3) + LBUF%VISC(KK(6)+I)
                            WWA(98846+CPT+4)=WWA(98846+CPT+4) + LBUF%VISC(KK(2)+I)
                            WWA(98846+CPT+5)=WWA(98846+CPT+5) + LBUF%VISC(KK(5)+I)
                            WWA(98846+CPT+6)=WWA(98846+CPT+6) + LBUF%VISC(KK(3)+I)
                         ENDIF
                         IF (MTE == 12 .OR. MTE == 14) THEN
                            WWA(1646+CPT+1) = LBUF%EPE(KK(1)+I)  !NB14
                            WWA(1646+CPT+2) = LBUF%EPE(KK(2)+I)
                            WWA(1646+CPT+3) = LBUF%EPE(KK(3)+I)
                         ELSEIF (ELBUF_TAB(NG)%BUFLY(IT)%L_STRA > 0) THEN
                            WWA(1646+CPT+1) = LBUF%STRA(KK(1)+I)  ! NB13
                            WWA(1646+CPT+2) = LBUF%STRA(KK(2)+I)
                            WWA(1646+CPT+3) = LBUF%STRA(KK(3)+I)
                            WWA(1646+CPT+4) = LBUF%STRA(KK(4)+I)*UNDEMI
                            WWA(1646+CPT+5) = LBUF%STRA(KK(5)+I)*UNDEMI
                            WWA(1646+CPT+6) = LBUF%STRA(KK(6)+I)*UNDEMI
                         ELSE
                            WWA(1646+CPT+1)  =  ZERO
                            WWA(1646+CPT+2)  =  ZERO
                            WWA(1646+CPT+3)  =  ZERO
                            WWA(1646+CPT+4)  =  ZERO
                            WWA(1646+CPT+5)  =  ZERO
                            WWA(1646+CPT+6)  =  ZERO
                         ENDIF
C
                       ELSE ! No IGTYP 22 
C------------------------Output SIJK---------------------

                         MBUF => ELBUF_TAB(NG)%BUFLY(IT)%MAT(IR,IS,1)
                        IPWWA = (IR-1)*3*9*7 + (IT-1)*3*7 + (IS-1)*7          
                         DO J=1,6
                            SIGG(J) = LBUF%SIG(KK(J)+I)
                         ENDDO
                         IF(IVISC > 0) THEN
                           DO J=1,6
                              SIGG(J) = SIGG(J) + LBUF%VISC(KK(J)+I)
                           ENDDO
                         ENDIF

C                       Deformation plastique
                        IF (MTE >= 28) THEN
                          IF (NUVAR > 0) THEN    
                            PLAG = MBUF%VAR(I)
                          ENDIF
                        ELSE
                          IF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA > 0) THEN
                            PLAG = LBUF%PLA(I)
                          ENDIF
                        ENDIF     

                        CALL SROTA6(X,IXS(1,N),KCVT,
     .                               SIGG,GAMA, KHBE,IGTYP)

C-----------------in global ref : SXIJK............-----------                                          
                        DO J=1,6
                            WWA(196+IPWWA +J) = SIGG(J)
                        ENDDO
C-----------------in global ref : PLAIJK............-----------
                        WWA(196+IPWWA +7) = PLAG
C-----------------in global ref : EPSIJK............----------- 
                        DO J=1,6
                            WWA(239060+IPWWA +J) = EVAR_TMP(J)
                        ENDDO

                      ENDIF
                    ELSE
                       WWA(196+CPT +1)  = ZERO
                       WWA(196+CPT +2)  = ZERO
                       WWA(196+CPT +3)  = ZERO
                       WWA(196+CPT +4)  = ZERO
                       WWA(196+CPT +5)  = ZERO
                       WWA(196+CPT +6)  = ZERO

                       WWA(1646+CPT+1)  = ZERO
                       WWA(1646+CPT+2)  = ZERO
                       WWA(1646+CPT+3)  = ZERO
                       WWA(1646+CPT+4)  = ZERO
                       WWA(1646+CPT+5)  = ZERO
                       WWA(1646+CPT+6)  = ZERO

                       WWA(120338+CPT+1)= ZERO
                       WWA(120338+CPT+2)= ZERO
                       WWA(120338+CPT+3)= ZERO
                       WWA(120338+CPT+4)= ZERO
                       WWA(120338+CPT+5)= ZERO
                       WWA(120338+CPT+6)= ZERO
                    ENDIF

                 ENDDO
                ENDDO                     
               ENDDO  
C-----------------in local ref : L_EPSX............-----------      
               DO J= 1,6
                 WWA(239036+J) = STRAIN(J)
               ENDDO
C-----------------in golbal ref : EPSX............-----------

              DO J= 1,3
                 WWA(1618+J) = EVAR(J)
               ENDDO
C Problem of order of output EPSZX before EPSYZ (see THGROU)
               WWA(1618 + 4) = EVAR(4)
               WWA(1618 + 5) = EVAR(6)
               WWA(1618 + 6) = EVAR(5)

c---------------
              ELSEIF (ISOLNOD == 8.AND.(KHBE == 14.OR.KHBE == 17))THEN 
c----------------------------------------------------------------------------
C------------------------Output SIJK EPS L_EPS---------------------
C---------------------------------------------------------------------------
c---------------
                 JJ = 6*(I-1)
                 NPTG=NPTT*NPTS*NPTR                              
                  DO J=1, 100
                    USER(J) = ZERO
                  ENDDO        
C-----------------

                  DO IS=1,NPTS  
                    ISPAU= 1                    
                    DO IT=1,NPTT
                      DO IR=1,NPTR       
c               
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                        MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(IR,IS,IT)
c
C                       IPWWA calcule sur la base de 3*9*3 points d'integration (r*s*t)
                        IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C                       IPWWA calcule sur la base de 3*9*3 points d'integration (s*t*r)
                        IPWWA = (IR-1)*3*9*7 + (IT-1)*3*7 + (IS-1)*7 
                        IUWWA = (IR-1)*3*9*9 + (IT-1)*3*9 + (IS-1)*9
c 
                        DO ITENS=1,6
c                          WWA(196+IPWWA+ITENS)=LBUF%SIG(KK(ITENS)+I)
                          SIGP(ITENS,ISPAU,IS)=LBUF%SIG(KK(ITENS)+I)
                          SIGG(ITENS) =  LBUF%SIG(KK(ITENS)+I)
                        ENDDO  

                        IF(IVISC > 0) then
                          DO ITENS=1,6
                           SIGP(ITENS,ISPAU,IS)=SIGP(ITENS,ISPAU,IS) +
     .                                              LBUF%VISC(KK(ITENS)+I) 
                           SIGG(ITENS) =  SIGG(ITENS) + LBUF%VISC(KK(ITENS)+I) 
                         ENDDO  
                        ENDIF     
c   
C                       Deformation plastique
                        IF (MTE >= 28) THEN
                          IF (NUVAR > 0) THEN    
                            SIGP(7,ISPAU,IS)   = MBUF%VAR(I)
                            PLAG = MBUF%VAR(I)
                          ENDIF
C                          
C                         we can get just 9 user variables by integration point                                              
                          NUVARTH = MIN(9,NUVAR)                          
                          DO J = 1,NUVARTH
                            WWA(889+J+IUWWA) = MBUF%VAR((J-1)*NEL+I)
                          ENDDO
C                         we can get just 60 average user variables                                                
                          NUVARTH = MIN(60,NUVAR)
                          DO J=1, NUVARTH
                            USER(J) = USER(J) +
     .                              + MBUF%VAR(I + (J-1)*NEL )/NPT 
                            WWA(889 + J + IUWWA) =
     .                                MBUF%VAR(I + (J-1)*NEL )
                          ENDDO

                          IF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                             EVAR_TMP(1) = LBUF%STRA(KK(1)+I)
                             EVAR_TMP(2) = LBUF%STRA(KK(2)+I)
                             EVAR_TMP(3) = LBUF%STRA(KK(3)+I)
                             EVAR_TMP(4) = LBUF%STRA(KK(4)+I)*UNDEMI
                             EVAR_TMP(5) = LBUF%STRA(KK(5)+I)*UNDEMI
                             EVAR_TMP(6) = LBUF%STRA(KK(6)+I)*UNDEMI                             
                          ENDIF
                        ELSE
                          IF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA > 0) THEN
                            SIGP(7,ISPAU,IS)    = LBUF%PLA(I)
                            PLAG= LBUF%PLA(I)
                          ENDIF

                          IF (MTE == 12 .OR. MTE == 14)THEN
                            DO J=1,3
                               EVAR_TMP(J) = LBUF%EPE(KK(J)+I)
                            ENDDO     
                            EVAR_TMP(3:6) = ZERO             
                          ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                             EVAR_TMP(1) = LBUF%STRA(KK(1)+I)
                             EVAR_TMP(2) = LBUF%STRA(KK(2)+I)
                             EVAR_TMP(3) = LBUF%STRA(KK(3)+I)
                             EVAR_TMP(4) = LBUF%STRA(KK(4)+I)*UNDEMI
                             EVAR_TMP(5) = LBUF%STRA(KK(5)+I)*UNDEMI
                             EVAR_TMP(6) = LBUF%STRA(KK(6)+I)*UNDEMI                                                              
                          ENDIF
                        ENDIF
                        ISPAU=ISPAU+1

                        DO J = 1, 6                    
                           STRAIN(J) = STRAIN(J) + EVAR_TMP(J)/NPTG
                        ENDDO
C-----------------in global ref : SXIJK EPIJK............-----------

                        ICSIG=IPARG(17,NG)  
                        IF (KHBE == 14.AND.ICSIG > 0) THEN
                           SELECT CASE (ICSIG)                                            
                           CASE (1)          
                                 IF(KCVT==2)THEN
                                   GAMA(1)= ZERO
                                   GAMA(2)= LBUF%GAMA(KK(1)+I)
                                   GAMA(3)= LBUF%GAMA(KK(2)+I)
                                   GAMA(4)= ZERO
                                   GAMA(5)=-GAMA(2)
                                   GAMA(6)= GAMA(1)
                                 ELSE
                                   GAMA(1)=UN
                                   GAMA(2)=ZERO
                                   GAMA(3)=ZERO
                                   GAMA(4)=ZERO
                                   GAMA(5)=UN
                                   GAMA(6)=ZERO
                                 END IF
                           CASE (10)           
                                 IF(KCVT==2)THEN
                                   GAMA(1)= LBUF%GAMA(KK(1)+I)
                                   GAMA(2)= LBUF%GAMA(KK(2)+I)
                                   GAMA(3)= ZERO
                                   GAMA(4)=-GAMA(2)
                                   GAMA(5)= GAMA(1)
                                   GAMA(6)= ZERO
                                 ELSE
                                   GAMA(1)=UN
                                   GAMA(2)=ZERO
                                   GAMA(3)=ZERO
                                   GAMA(4)=ZERO
                                   GAMA(5)=UN
                                   GAMA(6)=ZERO
                                 END IF
                           CASE (100)          
                                 IF(KCVT==2)THEN
                                   GAMA(1)= LBUF%GAMA(KK(2)+I)
                                   GAMA(2)= ZERO
                                   GAMA(3)= LBUF%GAMA(KK(1)+I)
                                   GAMA(4)= GAMA(3)
                                   GAMA(5)= ZERO
                                   GAMA(6)=-GAMA(1)
                                 ELSE
                                   GAMA(1)=UN
                                   GAMA(2)=ZERO
                                   GAMA(3)=ZERO
                                   GAMA(4)=ZERO
                                   GAMA(5)=UN
                                   GAMA(6)=ZERO
                                 END IF
                           END SELECT   

                          ELSE

                           IF(KCVT==2)THEN				   
                             GAMA(1)=GBUF%GAMA(KK(1) + I)			
                             GAMA(2)=GBUF%GAMA(KK(2) + I)			
                             GAMA(3)=GBUF%GAMA(KK(3) + I)			
                             GAMA(4)=GBUF%GAMA(KK(4) + I)			
                             GAMA(5)=GBUF%GAMA(KK(5) + I)			
                             GAMA(6)=GBUF%GAMA(KK(6) + I)			
                           ELSE					   
                             GAMA(1)=UN				   
                             GAMA(2)=ZERO				   
                             GAMA(3)=ZERO				   
                             GAMA(4)=ZERO				   
                             GAMA(5)=UN				   
                             GAMA(6)=ZERO				   
                           END IF

                                           
                          ENDIF

                          CALL SROTA6(X,IXS(1,N),KCVT,
     .                               SIGG,GAMA, KHBE,IGTYP)
                          CALL SROTA6(X,IXS(1,N),KCVT,
     .                               EVAR_TMP,GAMA, KHBE,IGTYP)

C-----------------in global ref : SXIJK............-----------
                        DO J=1,6
                            WWA(196+IPWWA+J) = SIGG(J)
                        ENDDO
C-----------------in global ref : PLAIJK............-----------
                        WWA(196+IPWWA +7) = PLAG
C-----------------in global ref : EPSIJK............----------- 
                        DO J=1,6
                            WWA(239060+IPWWA+J) = EVAR_TMP(J)
                        ENDDO

                         DO J = 1, 6                    
                           EVAR(J) = EVAR(J) + EVAR_TMP(J)/NPTG
                         ENDDO 

                      ENDDO
                    ENDDO
                  ENDDO
C-----------------
                  IF (MTE >= 28)THEN
C we can get just 60 user variables 
                   NUVARTH = MIN(60,NUVAR)
                    DO J=1, NUVARTH
                      WWA(136 + J) = USER(J)
                    ENDDO
                  ENDIF


C
C-----------------in local ref : L_EPSX............-----------
                  DO J = 1, 6                    
                     WWA(239030 + J) = STRAIN(J)
                  ENDDO				   
C-----------------in global ref : EPSX............-----------
                  DO J = 1, 3                    
                     WWA(1618 + J) = EVAR(J)
                  ENDDO	
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                  WWA(1618 + 4) = EVAR(4)
                  WWA(1618 + 5) = EVAR(6)
                  WWA(1618 + 6) = EVAR(5)
C----------------------------------------
                ENDIF
C-----------------------------------
              ELSE  ! KCVT = 0
C-----------------------------------
C                        GLOBAL FORMULATION ONLY :
C               40+I     SX au point de Gauss I
C               48+I     SY au point de Gauss I
C               56+I     SZ au point de Gauss I
C               64+I     SXY au point de Gauss I
C               72+I     SYZ au point de Gauss I
C               80+I     SXZ au point de Gauss I
C-----------------------------------
                IF (ISOLNOD == 4) THEN
c----------------------------------------------------------------------------
C------------------------Output SXI .. EPSXI.. EPS L_EPS---------------------
C---------------------------------------------------------------------------
                  IF(ISROT == 1 )THEN
                    JJ = 6*(I-1) 
                    DO IPT=1,NPT
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
                      MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(IPT,1,1)
                      WWA(40+IPT)=LBUF%SIG(KK(1)+I)
                      WWA(48+IPT)=LBUF%SIG(KK(2)+I)
                      WWA(56+IPT)=LBUF%SIG(KK(3)+I)
                      WWA(64+IPT)=LBUF%SIG(KK(4)+I)
                      WWA(72+IPT)=LBUF%SIG(KK(5)+I)
                      WWA(80+IPT)=LBUF%SIG(KK(6)+I)
                      IF(IVISC > 0 ) THEN
                         WWA(40+IPT)=WWA(40+IPT) + LBUF%VISC(KK(1)+I)
                         WWA(48+IPT)=WWA(48+IPT) + LBUF%VISC(KK(2)+I)
                         WWA(56+IPT)=WWA(56+IPT) + LBUF%VISC(KK(3)+I)
                         WWA(64+IPT)=WWA(64+IPT) + LBUF%VISC(KK(4)+I)
                         WWA(72+IPT)=WWA(72+IPT) + LBUF%VISC(KK(5)+I)
                         WWA(80+IPT)=WWA(80+IPT) + LBUF%VISC(KK(6)+I)
                      ENDIF 

                      IF(MTE == 12 .OR. MTE == 14) THEN                                  
                         DO J = 1, 3                                          
                            STRAIN(J) = STRAIN(J) + LBUF%EPE(KK(J)+I)/NPT    !NB14
                         ENDDO
                         WWA(239036+IPT)=LBUF%STRA(KK(1)+I) 
                         WWA(239040+IPT)=LBUF%STRA(KK(2)+I)              
                         WWA(239044+IPT)=LBUF%STRA(KK(3)+I)                                      
                      ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                          DO J = 1, 6                          
                             STRAIN(J)= STRAIN(J) + LBUF%STRA(KK(J)+I)/NPT   
                          ENDDO  
C-----------------in global ref : EPSXI............-----------  
                          WWA(239036+IPT)=LBUF%STRA(KK(1)+I) 
                          WWA(239040+IPT)=LBUF%STRA(KK(2)+I)              
                          WWA(239044+IPT)=LBUF%STRA(KK(3)+I)    
                          WWA(239048+IPT)=LBUF%STRA(KK(4)+I) *UNDEMI                           
                          WWA(239052+IPT)=LBUF%STRA(KK(5)+I) *UNDEMI                            
                          WWA(239056+IPT)=LBUF%STRA(KK(6)+I) *UNDEMI                                
                      ENDIF  ! MTN
                    ENDDO    ! LOOP OVER NPT, ISOLNOD = 4 and Isrot =
c     

C-----------------in Global ref : EPSX............-----------
                    DO J = 1, 3                    
                        WWA(1618 + J) = STRAIN(J)
                    ENDDO
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                     WWA(1618 + 4) = STRAIN(4)
                     WWA(1618 + 5) = STRAIN(6)
                     WWA(1618 + 6) = STRAIN(5)
C-----------------in local ref : L_EPSX............-----------
                    DO J = 1, 6                    
                      WWA(239030 + J) = STRAIN(J)
                    ENDDO
c----

                  ELSEIF(ISROT == 0) THEN
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
c
                    IF (MTE == 12 .OR. MTE == 14) THEN
                      DO J= 1,3
                        STRAIN(J) = LBUF%EPE(KK(J)+I)
                      ENDDO                                           
                    ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                      DO J= 1,6                     
                        STRAIN(J) = LBUF%STRA(KK(J)+I)                  
                      ENDDO  
                   ENDIF

C-----------------in Global ref : EPSX............-----------
                   DO J = 1, 3                    
                     WWA(1618 + J) = STRAIN(J)
                   ENDDO
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                   WWA(1618 + 4) = STRAIN(4)
                   WWA(1618 + 5) = STRAIN(6)
                   WWA(1618 + 6) = STRAIN(5)
C-----------------in local ref : L_EPSX............-----------
                   DO J = 1, 6                    
                     WWA(239030 + J) = STRAIN(J)
                   ENDDO

                 ENDIF
c----
                ELSEIF (ISOLNOD == 10) THEN
c----------------------------------------------------------------------------
C------------------------Output SXI .. EPSXI.. EPS L_EPS---------------------
C---------------------------------------------------------------------------
                  JJ = 6*(I-1)
                  DO J=1,100
                    USER(J) = ZERO
                  ENDDO

                  DO IPT=1,NPT
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
                    MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(IPT,1,1)
                    WWA(40+IPT)=LBUF%SIG(KK(1)+I)
                    WWA(48+IPT)=LBUF%SIG(KK(2)+I)
                    WWA(56+IPT)=LBUF%SIG(KK(3)+I)
                    WWA(64+IPT)=LBUF%SIG(KK(4)+I)
                    WWA(72+IPT)=LBUF%SIG(KK(5)+I)
                    WWA(80+IPT)=LBUF%SIG(KK(6)+I)
                    IF(IVISC > 0 ) THEN
                       WWA(40+IPT)=WWA(40+IPT) + LBUF%VISC(KK(1)+I)
                       WWA(48+IPT)=WWA(48+IPT) + LBUF%VISC(KK(2)+I)
                       WWA(56+IPT)=WWA(56+IPT) + LBUF%VISC(KK(3)+I)
                       WWA(64+IPT)=WWA(64+IPT) + LBUF%VISC(KK(4)+I)
                       WWA(72+IPT)=WWA(72+IPT) + LBUF%VISC(KK(5)+I)
                       WWA(80+IPT)=WWA(80+IPT) + LBUF%VISC(KK(6)+I)
                    ENDIF 
                    IF (MTE >= 28) THEN                   
                      NUVARTH = MIN(60,NUVAR)
                      DO J=1, NUVARTH
                        USER(J) = USER(J) +
     .                  MBUF%VAR(I + (J-1)*NEL )/NPT 
                      ENDDO
                      IF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                        DO J = 1, 6                          
                          STRAIN(J)= STRAIN(J) + LBUF%STRA(KK(J)+I)/NPT   
                        ENDDO
                        WWA(239036+IPT)=LBUF%STRA(KK(1)+I) 
                        WWA(239040+IPT)=LBUF%STRA(KK(2)+I)              
                        WWA(239044+IPT)=LBUF%STRA(KK(3)+I)    
                        WWA(239048+IPT)=LBUF%STRA(KK(4)+I) *UNDEMI                           
                        WWA(239052+IPT)=LBUF%STRA(KK(5)+I) *UNDEMI                           
                        WWA(239056+IPT)=LBUF%STRA(KK(6)+I) *UNDEMI                                                  
                      ENDIF                                
                    ELSEIF(MTE == 12 .OR. MTE == 14) THEN                                  
                      DO J = 1, 3                                          
                        STRAIN(J) = STRAIN(J) + LBUF%EPE(KK(J)+I)/NPT    !NB14
                      ENDDO  
                        WWA(239036+IPT)=LBUF%EPE(KK(1)+I) 
                        WWA(239040+IPT)=LBUF%EPE(KK(2)+I)              
                        WWA(239044+IPT)=LBUF%EPE(KK(3)+I)                                                   
                    ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                       DO J= 1,6                               
                            STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)/NPTG                   
                       ENDDO 
  
                        WWA(239036+IPT)=LBUF%STRA(KK(1)+I) 
                        WWA(239040+IPT)=LBUF%STRA(KK(2)+I)              
                        WWA(239044+IPT)=LBUF%STRA(KK(3)+I)    
                        WWA(239048+IPT)=LBUF%STRA(KK(4)+I) *UNDEMI                            
                        WWA(239052+IPT)=LBUF%STRA(KK(5)+I) *UNDEMI                           
                        WWA(239056+IPT)=LBUF%STRA(KK(6)+I) *UNDEMI                                 
                    ENDIF  ! MTN
                  ENDDO    ! LOOP OVER NPT, ISOLNOD = 10
c
                  IF ( MTE >= 28) THEN
C                   User laws for solids we can have just 60 user variables.  
                    NUVARTH = MIN(60,NUVAR)                                   
                    DO J=1,NUVARTH                                            
                      WWA(136+J)= USER(J)
                    ENDDO
                  ENDIF        

C-----------------in Global ref : EPSX............-----------
                  DO J = 1, 3                    
                     WWA(1618 + J) = STRAIN(J)
                  ENDDO
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                  WWA(1618 + 4) = STRAIN(4)
                  WWA(1618 + 5) = STRAIN(6)
                  WWA(1618 + 6) = STRAIN(5)
C-----------------in local ref : L_EPSX............-----------
                  DO J = 1, 6                    
                     WWA(239030 + J) = STRAIN(J)
                  ENDDO

c----
                ELSEIF( ISOLNOD == 16.OR.ISOLNOD == 20.OR.
     .            (ISOLNOD == 8.AND.(KHBE == 14.OR.KHBE == 17)))THEN 
c----------------------------------------------------------------------------
C------------------------Output SIJK EPS L_EPS---------------------
C---------------------------------------------------------------------------
c 

                  JJ = 6*(I-1)
                  NPTG=NPTT*NPTS*NPTR*NLAY                            
                  DO J=1, 100
                    USER(J) = ZERO
                  ENDDO 
  
                  DO IL =1,NLAY
                
                   DO IS=1,NPTS  
                    ISPAU= 1                    
                    DO IT=1,NPTT
                      DO IR=1,NPTR                      
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                        MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
c
C                       IPWWA calcule sur la base de 3*9*3 points d'integration (r*s*t)
                         CPT=(IT-1)*99*6+((IR-1)*9+IS-1)*6

                        IPT   = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                        IPWWA = (IT-1)*3*9*7 + (IS-1)*3*7 + (IR-1)*7 
                        IUWWA = (IT-1)*3*9*9 + (IS-1)*3*9 + (IR-1)*9 
                        IF(ISOLNOD == 8)THEN
                          IPWWA = (IR-1)*3*9*7 + (IT-1)*3*7 + (IS-1)*7 
                          IUWWA = (IR-1)*3*9*9 + (IT-1)*3*9 + (IS-1)*9 
                        ENDIF
                        IF(ISOLNOD == 16)THEN
                           IPT   = IR + ( (IL-1) + (IT-1)*NLAY )*NPTR
                           IPWWA = (IT-1)*3*9*7 + (IL-1)*3*7 + (IR-1)*7 
                           IUWWA = (IT-1)*3*9*9 + (IL-1)*3*9 + (IR-1)*9 
                        ENDIF
c
                        DO ITENS=1,6
                           WWA(196+IPWWA+ITENS) = LBUF%SIG(KK(ITENS)+I)
                           SIGP(ITENS,ISPAU,IS) = LBUF%SIG(KK(ITENS)+I)
                        ENDDO 
C                       Deformation plastique
                        IF (MTE >= 28) THEN
                          IF (NUVAR > 0) THEN
                            WWA(196+IPWWA+7) = MBUF%VAR(I)
                            SIGP(7,ISPAU,IS) = MBUF%VAR(I) 
                          ENDIF
C                         just 9 user variables by integration point
                          NUVARTH = MIN(9,NUVAR)
                          DO J=1, NUVARTH
                            WWA(889 + J + IUWWA) = MBUF%VAR(I+(J-1)*NEL)
                          ENDDO
C                         60 average user variable
                          NUVARTH = MIN(60,NUVAR)
                          DO J=1, NUVARTH
                            USER(J) = USER(J)+MBUF%VAR(I+(J-1)*NEL)/NPTG
                            WWA(889+J+IUWWA) =MBUF%VAR(I+(J-1)*NEL)
                          ENDDO
                          IF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0)THEN
                            DO J = 1, 3                     
                              STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)/NPTG 
                              WWA(239060+IPWWA+J)=LBUF%STRA(KK(J)+I)              
                            ENDDO
                            DO J = 4, 6                     
                              STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)*UNDEMI/NPTG 
                              WWA(239060+IPWWA+J)=LBUF%STRA(KK(J)+I)  *UNDEMI            
                            ENDDO
                          ENDIF
c                          
                        ELSE ! IF MTE
c
                          IF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA > 0) THEN
                            WWA(196 + IPWWA + 7)=LBUF%PLA(I) 
                            SIGP(7,ISPAU,IS)=    LBUF%PLA(I)
                          ENDIF
                          IF (MTE==12 .OR. MTE == 14) THEN
                            DO J=1,3
                              STRAIN(J) = STRAIN(J) + LBUF%EPE(KK(J)+I)/NPTG 
                              WWA(239060+IPWWA+J)=LBUF%EPE(KK(J)+I)               
                            ENDDO 
                          ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0)THEN
                            DO J = 1, 3                     
                              STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)/NPTG 
                              WWA(239060+IPWWA+J)=LBUF%STRA(KK(J)+I)              
                            ENDDO
                            DO J = 4, 6                     
                              STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)*UNDEMI/NPTG 
                              WWA(239060+IPWWA+J)=LBUF%STRA(KK(J)+I)  *UNDEMI            
                            ENDDO
                          ENDIF
                        ENDIF
                        ISPAU=ISPAU+1
                      ENDDO
                    ENDDO
                  ENDDO
                 ENDDO


                  IF (MTE >= 28) THEN
C                   just 60 average user variable
                    NUVARTH =  MIN(60,NUVAR)                  
                    DO J=1, NUVARTH
                      WWA(136 + J) = USER(J)
                    ENDDO
                  ENDIF
C-----------------in local ref : L_EPSX............-----------
					   
                  DO J = 1, 6                    
                     WWA(239030 + J) = STRAIN(J)
                  ENDDO

                  IF (KHBE == 17) THEN
                    IF (MTE == 12 .OR. MTE == 14 .OR. MTE == 24 .OR.  
     .                MTE == 25 .OR.(MTE >= 28 .AND.MTE /= 49)) THEN 
                      IF (ISORTH > 0) KCVT = 2
                    ENDIF
 
                    IF (KCVT /= 0) THEN 
                      IF (KCVT==2)THEN
                        GAMA(1)=GBUF%GAMA(KK(1) + I)
                        GAMA(2)=GBUF%GAMA(KK(2) + I)
                        GAMA(3)=GBUF%GAMA(KK(3) + I)
                        GAMA(4)=GBUF%GAMA(KK(4) + I)
                        GAMA(5)=GBUF%GAMA(KK(5) + I)
                        GAMA(6)=GBUF%GAMA(KK(6) + I)
                      ELSE
                        GAMA(1)=UN
                        GAMA(2)=ZERO
                        GAMA(3)=ZERO
                        GAMA(4)=ZERO
                        GAMA(5)=UN
                        GAMA(6)=ZERO
                      ENDIF

                      CALL SROTA6(X,IXS(1,N),KCVT,
     .                            STRAIN,GAMA,KHBE,IGTYP)   
                    ENDIF
                  ENDIF

C-----------------in Global ref : EPSX............-----------
                  DO J = 1, 3                    
                     WWA(1618 + J) = STRAIN(J)
                  ENDDO
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                  WWA(1618 + 4) = STRAIN(4)
                  WWA(1618 + 5) = STRAIN(6)
                  WWA(1618 + 6) = STRAIN(5)

C
c                 CALCUL DES CONTRAINTES SUR LES FACES SUPERIEURE ET INFERIEURE
C    
                  IF(ISOLNOD == 16 ) THEN
                    NPTL = NLAY
                  ELSE
                    NPTL= NPTS
                  ENDIF


                 IF (NPT < 0) THEN
C                  POUR LES POINTS D'INTEGRATION DE LOBATTO
                   ISPAU=1
                   DO IT=1,NPTT
                      DO IR=1,NPTR 
                        IPWWA =  (IT-1)*3*7 + (IR-1)*7                  
                        DO ITENS=1,7
                          WWA(826+ITENS+IPWWA) = SIGP(ITENS,ISPAU,1)       
                          WWA(763+ITENS+IPWWA) = SIGP(ITENS,ISPAU,NPTS)
                        ENDDO
                        ISPAU=ISPAU+1
                      ENDDO  
                   ENDDO
                 ELSE
                  IF (NPTL > 2) THEN 
                    ISPAU=1                                       
                    DO IT=1,NPTT
                      DO IR=1,NPTR 
                        IPWWA =  (IT-1)*3*7 + (IR-1)*7                  
                        DO ITENS=1,7
c
                          WWA(826+ITENS+IPWWA) = SIGP(ITENS,ISPAU,1)  
     .                      +(SIGP(ITENS,ISPAU,2)-SIGP(ITENS,ISPAU,1))
     .                      *(-1 - A_GAUSS(1,NPTL))
     .                      /(A_GAUSS(2,NPTL)-A_GAUSS(1,NPTL))
c
                          WWA(763+ITENS+IPWWA)= SIGP(ITENS,ISPAU,NPTL-1)
     .                      +(SIGP(ITENS,ISPAU,NPTL)
     .                      - SIGP(ITENS,ISPAU,NPTL-1))
     .                      *(1 - A_GAUSS(NPTL-1,NPTL))
     .                      /(A_GAUSS(NPTL,NPTL)-A_GAUSS(NPTL-1,NPTL))
c
                        ENDDO
                        ISPAU=ISPAU+1
                      ENDDO  
                    ENDDO
                   ELSE
                    ISPAU=1     
                    DO IT=1,NPTT
                      DO IR=1,NPTR 
                        IPWWA =  (IT-1)*3*7 + (IR-1)*7        
                        DO ITENS=1,7
c
                          WWA(826+ITENS+IPWWA) 
     .                      = SIGP(ITENS,ISPAU,1) 
     .                      +(SIGP(ITENS,ISPAU,2)-SIGP(ITENS,ISPAU,1))
     .                      *(-1 - A_GAUSS(1,NPTL))
     .                      /(A_GAUSS(2,NPTL)-A_GAUSS(1,NPTL))
c
                             WWA(763 + ITENS + IPWWA) 
     .                      = SIGP(ITENS,ISPAU,1)  
     .                      +(SIGP(ITENS,ISPAU,2)-SIGP(ITENS,ISPAU,1))
     .                      *(1 - A_GAUSS(1,NPTL))
     .                      /(A_GAUSS(2,NPTL)-A_GAUSS(1,NPTL))
c
                        ENDDO
                        ISPAU=ISPAU+1
                      ENDDO  
                    ENDDO 
                  ENDIF
                ENDIF  

c---------------
              ELSEIF ((ISOLNOD==6 .OR. ISOLNOD==8) .AND. KHBE==15) THEN
c----------------------------------------------------------------------------
C------------------------Output SIJK EPS L_EPS---------------------
C---------------------------------------------------------------------------
C
                  JJ = 6*(I-1)
                  DO J=1, 100
                    USER(J) = ZERO
                  ENDDO         
                  NPTS = NPT                                 
C                   
                  DO IPT=1,NPTS                                       
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
                      MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(IPT,1,1)
c                   IPWWA calcule sur la base de 3*9*3 points d'integration (r*s*t)
c                   pour ce type de solide les sorties sont dans 1*NPTS*1,  NPTS = 9 (en max)                        
                    IPWWA =  (IPT-1)*3*7                  
                    IUWWA =  (IPT-1)*3*9                  
                    DO ITENS=1,6                          
                      WWA(196+IPWWA+ITENS)=LBUF%SIG(KK(ITENS)+I)
                      SIGP(ITENS,1,IPT)=   LBUF%SIG(KK(ITENS)+I)
                    ENDDO   
                    IF(IVISC > 0 ) THEN
                      DO ITENS=1,6                          
                        WWA(196+IPWWA+ITENS)= WWA(196+IPWWA+ITENS) + 
     .                                            LBUF%VISC(KK(ITENS)+I)		    
                        SIGP(ITENS,1,IPT)=  SIGP(ITENS,1,IPT)+ 
     .                                            LBUF%VISC(KK(ITENS)+I)		      
                      ENDDO  
                    ENDIF                
c                   Deformation plastique                                   
                    IF (MTE >= 28) THEN                                       
                      IF (NUVAR > 0) THEN                                  
                        WWA(196+IPWWA+7) = MBUF%VAR(I)
                        SIGP(7, 1   ,IPT)= MBUF%VAR(I)
                      ENDIF
C                     just 9 user variables par integration point                              
                      NUVARTH = MIN(9,NUVAR)                                              
                      DO J=1, NUVARTH                                                     
                        WWA(889  + J + IUWWA) = MBUF%VAR(I + (J-1)*NEL )                  
                      ENDDO                                                               
C                      just 60 average user variables                                     
                      NUVARTH = MIN(60,NUVAR)                                             
                      DO J=1, NUVARTH                                                     
                        USER(J) = USER(J) + MBUF%VAR(I+(J-1)*NEL)/NPT    
                        WWA(889+J+IUWWA)  = MBUF%VAR(I+(J-1)*NEL)        
                      ENDDO                                             
                      IF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                        DO J= 1,3                             
                          STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)/NPT    
                          WWA(239060+IPWWA+J)=LBUF%STRA(KK(J)+I)                                 
                        ENDDO  
                        DO J= 4,6                             
                          STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)*UNDEMI/NPT    
                          WWA(239060+IPWWA+J)=LBUF%STRA(KK(J)+I)*UNDEMI                                 
                        ENDDO  
                      ENDIF                          
                    ELSE  ! mte < 28                                                  
                      IF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA > 0) THEN                                  
                        WWA(196 + IPWWA + 7)= LBUF%PLA(I) 
                        SIGP(7, 1   ,IPT)   = LBUF%PLA(I) 
                      ENDIF
                      IF (MTE == 12 .OR. MTE == 14) THEN                           
                        DO J=1,3                                  
                          STRAIN(J) = STRAIN(J) + LBUF%EPE(KK(J)+I)/NPT  
                          WWA(239060+IPWWA+J)=LBUF%EPE(KK(J)+I)                           
                        ENDDO                                                                      
                      ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN                    
                        DO J= 1,3                             
                          STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)/NPT    
                          WWA(239060+IPWWA+J)=LBUF%STRA(KK(J)+I)                                 
                        ENDDO  
                        DO J= 4,6                             
                          STRAIN(J) = STRAIN(J) + LBUF%STRA(KK(J)+I)*UNDEMI/NPT    
                          WWA(239060+IPWWA+J)=LBUF%STRA(KK(J)+I)*UNDEMI                                 
                        ENDDO                                 
                     ENDIF
                    ENDIF  ! mte < 28
                  ENDDO  ! IPT
C
                  IF (MTE >= 28)THEN                
C we can get just 60 average user variables
                      NUVARTH = MIN(60,NUVAR)        
                      DO J=1, NUVARTH
                       WWA(136 + J) = USER(J)
                      ENDDO
                     ENDIF
C-----------------in Global ref : EPSX............-----------
                  DO J = 1, 3                    
                     WWA(1618 + J) = STRAIN(J)
                  ENDDO
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                  WWA(1618 + 4) = STRAIN(4)
                  WWA(1618 + 5) = STRAIN(6)
                  WWA(1618 + 6) = STRAIN(5)
C-----------------in local ref : L_EPSX............-----------
                  DO J = 1, 6                    
                     WWA(239030 + J) = STRAIN(J)
                  ENDDO
CC                        
                   IF(NPTS > 2) THEN                                                                       
                        IPWWA =  0                  
                        DO ITENS=1,7
                          WWA(826+ITENS + IPWWA) = SIGP(ITENS,1,1)  
     .                      +(SIGP(ITENS,1,2)-SIGP(ITENS,1,1))
     .                      *(-1 - A_GAUSS(1,NPTS))
     .                      /(A_GAUSS(2,NPTS)-A_GAUSS(1,NPTS))
                             WWA(763+ITENS+IPWWA) = SIGP(ITENS,1,NPTS-1)
     .                      +(SIGP(ITENS,1,NPTS)
     .                      - SIGP(ITENS,1,NPTS-1))
     .                      *(1 - A_GAUSS(NPTS-1,NPTS))
     .                      /(A_GAUSS(NPTS,NPTS)-A_GAUSS(NPTS-1,NPTS))
                        ENDDO        
                    ELSE
                        IPWWA =  0               
                        DO ITENS=1,7
                          WWA(826+ITENS+IPWWA) = SIGP(ITENS,1,1) 
     .                      +(SIGP(ITENS,1,2)-SIGP(ITENS,1,1))
     .                      *(-1 - A_GAUSS(1,NPTS))
     .                      /(A_GAUSS(2,NPTS)-A_GAUSS(1,NPTS))
                             WWA(763 + ITENS + IPWWA) = SIGP(ITENS,1,1)  
     .                      +(SIGP(ITENS,1,2)-SIGP(ITENS,1,1))
     .                      *(1 - A_GAUSS(1,NPTS))
     .                      /(A_GAUSS(2,NPTS)-A_GAUSS(1,NPTS))
                        ENDDO
                    ENDIF                                
c
                ELSEIF (ISOLNOD == 8.AND.KHBE /= 14.AND.KHBE /= 24) THEN
c
                  JJ = 6*(I-1)
                 IF (NPT == 8) THEN
                    NLAY = ELBUF_TAB(NG)%NLAY       
                    NPTR = ELBUF_TAB(NG)%NPTR       
                    NPTS = ELBUF_TAB(NG)%NPTS       
                    NPTT = ELBUF_TAB(NG)%NPTT       
                    NPT  = NPTR * NPTS * NPTT * NLAY
                    DO IT=1,NPTT   !1,2
                     DO IS=1,NPTS  !1,2
                      DO IR=1,NPTR !1,2
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                        MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(IR,IS,IT)
                        IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                        WWA(40+IPT)=LBUF%SIG(KK(1)+I)
                        WWA(48+IPT)=LBUF%SIG(KK(2)+I)
                        WWA(56+IPT)=LBUF%SIG(KK(3)+I)
                        WWA(64+IPT)=LBUF%SIG(KK(4)+I)
                        WWA(72+IPT)=LBUF%SIG(KK(5)+I)
                        WWA(80+IPT)=LBUF%SIG(KK(6)+I)
                        IF(IVISC > 0 ) THEN
                         WWA(40+IPT)=WWA(40+IPT) + LBUF%VISC(KK(1)+I)
                         WWA(48+IPT)=WWA(48+IPT) + LBUF%VISC(KK(2)+I)
                         WWA(56+IPT)=WWA(56+IPT) + LBUF%VISC(KK(3)+I)
                         WWA(64+IPT)=WWA(64+IPT) + LBUF%VISC(KK(4)+I)
                         WWA(72+IPT)=WWA(72+IPT) + LBUF%VISC(KK(5)+I)
                         WWA(80+IPT)=WWA(80+IPT) + LBUF%VISC(KK(6)+I)
                        ENDIF 
                        !tenseur de contrainte sur chaque points de gauss
                        IPWWA = (IT-1)*3*9*7 + (IS-1)*3*7 + (IR-1)*7
                        IUWWA = (IT-1)*3*9*9 + (IS-1)*3*9 + (IR-1)*9
                        DO ITENS = 1,6
                          JJ = 6*(I-1)
                          WWA(196+IPWWA+ITENS)=LBUF%SIG(KK(ITENS)+I)
                        ENDDO
                        IF(IVISC > 0 ) THEN
                         DO ITENS = 1,6
                            JJ = 6*(I-1)
                            WWA(196+IPWWA+ITENS)=WWA(196+IPWWA+ITENS) + 
     .                                                 LBUF%VISC(KK(ITENS)+I)
                          ENDDO
                        ENDIF 
                        IF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA > 0) 
     .                      WWA(196+IPWWA+ 7 ) = LBUF%PLA(I) 
                        IF (MTE >= 28) THEN                                                   
                          IF (NUVAR>0) THEN
                            WWA(196+IPWWA+ 7 ) = MBUF%VAR(I)
                          ENDIF
C we can get just 9 user variables by integration point                          
                          NUVARTH = MIN(9,NUVAR)
                          DO J=1,NUVARTH
                            WWA(889 + IUWWA + J) = MBUF%VAR(I+(J-1)*NEL)                       
                          ENDDO
                          IF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                            STRAIN(1)=STRAIN(1) + LBUF%STRA(KK(1)+I)*UNHUIT
                            STRAIN(2)=STRAIN(2) + LBUF%STRA(KK(2)+I)*UNHUIT
                            STRAIN(3)=STRAIN(3) + LBUF%STRA(KK(3)+I)*UNHUIT
                            STRAIN(4)=STRAIN(4) + LBUF%STRA(KK(4)+I)*UNHUIT
                            STRAIN(5)=STRAIN(5) + LBUF%STRA(KK(5)+I)*UNHUIT
                            STRAIN(6)=STRAIN(6) + LBUF%STRA(KK(6)+I)*UNHUIT                                  
                          ENDIF
                        ENDIF
                      ENDDO
                     ENDDO
                    ENDDO                              
c
                  ELSEIF(NPT == 1)THEN
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
c
                    IF (MTE == 12 .OR. MTE == 14) THEN
                      DO J= 1,3
                        STRAIN(J) = LBUF%EPE(KK(J)+I)
                      ENDDO
                                             
                    ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0) THEN
                      DO J= 1,3                     
                        STRAIN(J) = LBUF%STRA(KK(J)+I)                  
                      ENDDO  
                      DO J= 4,6                     
                        STRAIN(J) = LBUF%STRA(KK(J)+I) *UNDEMI                
                      ENDDO  
                   ENDIF
                  ENDIF   ! NPT 

C-----------------in local ref : L_EPSX............-----------
                  DO J = 1, 6                    
                     WWA(239030 + J) = STRAIN(J)
                  ENDDO

                  IF (MTE == 12 .OR. MTE == 14 .OR. MTE == 24 .OR.  
     .                MTE == 25 .OR.(MTE >= 28 .AND.MTE /= 49)) THEN 
                    IF (ISORTH > 0) KCVT = 2
                  ENDIF
 
                  IF (KCVT /= 0) THEN 
                    IF (KCVT==2)THEN
                      GAMA(1)=GBUF%GAMA(KK(1) + I)
                      GAMA(2)=GBUF%GAMA(KK(2) + I)
                      GAMA(3)=GBUF%GAMA(KK(3) + I)
                      GAMA(4)=GBUF%GAMA(KK(4) + I)
                      GAMA(5)=GBUF%GAMA(KK(5) + I)
                      GAMA(6)=GBUF%GAMA(KK(6) + I)
                    ELSE
                      GAMA(1)=UN
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=UN
                      GAMA(6)=ZERO
                    END IF

                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                          STRAIN,GAMA,KHBE,IGTYP)   
                  ENDIF

C-----------------in Global ref : EPSX............-----------
                  DO J = 1, 3                    
                     WWA(1618 + J) = STRAIN(J)
                  ENDDO
C Problem of order of output EPSZX before EPSYZ (see THGROU)
                  WWA(1618 + 4) = STRAIN(4)
                  WWA(1618 + 5) = STRAIN(6)
                  WWA(1618 + 6) = STRAIN(5)

  
                ENDIF     ! ISOLNOD
C---
              ENDIF  ! KCVT
C---
              DO L=IADV,IADV+NVAR-1
                K=ITHBUF(L)                                
                IJK=IJK+1
                WA(IJK)=WWA(K)
              ENDDO
              IJK=IJK+1
              WA(IJK) = II

C -----          
            ENDIF   ! element = ITHBUF()
          ENDDO ! NEL
          ISORTHG = ISORTH
C -----          
          ENDIF ! mte /= 13
        ENDIF   ! ITY
      ENDDO     ! groupe
 666    continue
!   -------------------------------
            ENDIF
        ENDDO
C-----------
      RETURN
      END
