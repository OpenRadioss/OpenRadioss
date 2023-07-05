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
Chd|  THCOQ                         source/output/th/thcoq.F      
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE THCOQ(ELBUF_TAB,MATPARAM_TAB,NTHGRP2 , ITHGRP , 
     .                 IPARG    ,ITHBUF  ,WA      ,
     .                 IPM      ,IGEO  ,IXC    ,IXTG   ,PM     ,
     .                 RTHBUF   ,THKE  ,STACK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE PINCHTYPE_MOD         
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITHBUF(*),IXC(NIXC,*),
     .   IXTG(NIXTG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*)
      INTEGER, INTENT(in) :: NTHGRP2
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
      my_real
     .   WA(*),PM(NPROPM,*),RTHBUF(*),THKE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,II,JJ,N, IH, NG, ITY, MTE,  M2, M3, M5,M8, 
     .   NPT,MPT,NPG,NPTR,NPTS,NPTT,NLAY,IP,IR,IS,IT,IL,IPT,
     .   LWA,NEL,NFT,I1,I2,I3,I4,IUV,IAA,IADR,N16,N16A,
     .   ISTRAIN,NU,NUVAR,NUVARV,NUVARD,IGTYP,IHBE,NBD1,NBD2,NBD3,
     .   IFAILURE,IADD,ISROT,IVISC,IPMAT,PTMAT,ISHPLYXFEM,IPMAT_IPLY,
     .   MAT_IPLY,NBDELM,IWA,NV,NGL,IIGEO,IADI,ISUBSTACK,ITHK,NPT_ALL,
     .   MATLY,KK(8),IPINCH,IPG,IMAT,MAT_ORTH
      INTEGER PID(MVSIZ),MAT(MVSIZ)
      INTEGER :: NITER,IAD,NN,IADV,NVAR,ITYP,IJK
      my_real :: WWA(50000),FUNC(6),SIG(5)
      my_real ,DIMENSION(MVSIZ) :: DAM1,DAM2,WPLA,DMAX,WPMAX,
     .   FAIL,FAIL1,FAIL2,FAIL3
      my_real :: F1,F2,F3,F4,F5,F11,F22,F33,F44,F55,CP,SP,MM1,MM2,MM3,
     .   MM11,MM22,MM33,D1,D2,D11,D12,D22,VAL_LY_IP,VAL_LY_AVERAGE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      my_real ,DIMENSION(:), POINTER  :: UVAR,DIR_A
      my_real ,DIMENSION(:,:), ALLOCATABLE  :: VAR 
      TYPE (STACK_PLY) :: STACK
C-------------------------
C           ELEMENTS COQUES
C=======================================================================  
        IJK = 0
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IAD =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==3.OR.ITYP==7)THEN
!   ------------------------------- 
      II=0
      IH=IAD
Cel specifique spmd
      IF (IMACH == 3) THEN
C decalage IH
        DO WHILE((ITHBUF(IH+NN)/=ISPMD).AND.(IH<IAD+NN))
          IH = IH + 1
        ENDDO
        IF (IH>=IAD+NN) GOTO 666 
      ENDIF
C-------------------
      DO NG=1,NGROUP
        ITY=IPARG(5,NG)
        IF (ITY == ITYP) THEN
          MTE=IPARG(1,NG)
          NEL=IPARG(2,NG)
          NFT=IPARG(3,NG)
          NPT = IPARG(6,NG)
          IGTYP   = IPARG(38,NG)
          ISTRAIN = IPARG(44,NG)
          IHBE = IPARG(23,NG)
          IFAILURE = IPARG(43,NG)
          ISHPLYXFEM  = IPARG(50,NG)
          ISUBSTACK = IPARG(71,NG)
          ITHK  =IPARG(28,NG)
          GBUF => ELBUF_TAB(NG)%GBUF   
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
cc         NPT  = NLAY*NPTT ! not compatible with PID51 (shell)
          MPT   = MAX(1,NPT)
!
          DO I=1,8  ! length max of GBUF%G_STRA = 8
            KK(I) = NEL*(I-1)
          ENDDO
!
C
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPT_ALL = 0
            DO IPT=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
            ENDDO
            IF (NLAY == 1) MPT  = MAX(1,NPT_ALL)
          ENDIF
C         
          IVISC = 0
          NUVAR = 0
          NUVARV = 0
          NUVARD = 0
c          
          IF (MTE /= 13 .and. MTE /= 0) THEN
c
            IF ((MTE>=29.AND.MTE<=31).OR.
     .        MTE == 35.OR.MTE == 36.OR.MTE == 43.OR.
     .        MTE == 44.OR.MTE == 45.OR.MTE == 48.OR.MTE>=50) THEN
              CONTINUE
c
	           ELSEIF (MTE == 25) THEN
C
              DO I=1,NEL               
                DAM1(I)=ZERO           
                DAM2(I)=ZERO           
                WPLA(I)=ZERO           
                FAIL(I)=ZERO           
                FAIL1(I)=ZERO          
                FAIL2(I)=ZERO          
                FAIL3(I)=ZERO          
              ENDDO				               
c
              IF (ITY == 3) THEN        
                DO I=1,NEL             
                  MAT(I)=IXC(1,NFT+I)  
                  PID(I)=IXC(6,NFT+I)  
                ENDDO				             
              ELSE                     
                DO I=1,NEL             
             	  MAT(I)=IXTG(1,NFT+I) 
                  PID(I)=IXTG(5,NFT+I) 
                ENDDO				             
              ENDIF                   
c---
              IF (IGTYP == 11) THEN
                IPMAT = 100
                DO N=1,MPT
            	  DO I=1,NEL
      	            MATLY = IGEO(IPMAT+N,PID(I))
                    NUVARV = MAX(NUVARV, IPM(225,MATLY)) 
                    IF (IPM(222,MATLY) > 0) IVISC = 1
            	  ENDDO  			      
                ENDDO				    
              ELSEIF (IGTYP == 9 .OR. IGTYP == 10) THEN 
                DO N=1,MPT
                  DO I=1,NEL
                    MATLY=MAT(I)
                    NUVARV =  MAX(NUVARV, IPM(225,MATLY)) 
                    IF (IPM(222,MATLY) > 0) IVISC = 1
            	  ENDDO  			      
                ENDDO	
              ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                IPMAT   = 2 + NLAY  
                DO N=1,NLAY
                  DO I=1,NEL
                    MATLY  = STACK%IGEO(IPMAT+N,ISUBSTACK)
                    NUVARV = MAX(NUVARV, IPM(225,MATLY)) 
                    IF (IPM(222,MATLY) > 0) IVISC = 1
            	     ENDDO  			      
                ENDDO
c
                IF (ISHPLYXFEM > 0) THEN
                  IPMAT_IPLY = IPMAT + MPT
                  DO J=1,NPT -1  
                    DO I=1,NEL 
                      MAT_IPLY = STACK%IGEO(IPMAT_IPLY + J ,ISUBSTACK)
                      NUVARD = MAX(NUVARD, IPM(221,MAT_IPLY)) 
                    ENDDO                                 
                  ENDDO 
                ENDIF	              			    
              ENDIF
            ENDIF     ! MTE
c---------
c
c---------
            DO I=1,NEL
              N=I+NFT
              K=ITHBUF(IH)
              IP=ITHBUF(IH+NN)
              IADR=ITHBUF(IH+3*NN) ! Adress of cos sin related to skew
C
              IF (K == N) THEN
                IH=IH+1
C             traitement specifique spmd
                IF (IMACH == 3) THEN
C               recherche du ii correct
                  II = ((IH-1) - IAD)*NVAR
                  DO WHILE((ITHBUF(IH+NN) /= ISPMD) .AND. (IH < IAD+NN))
                    IH = IH + 1
                  ENDDO
                ENDIF ! IMACH == 3
C
                IF (IH > IAD+NN) GOTO 666
C
                M5=5*(I-1)
                M8=8*(I-1)
c
                IF (IADR /= 0) THEN ! output with respect to a (non global) SKEW
                  CP=RTHBUF(IADR)
                  SP=RTHBUF(IADR+1)
c
                  F11 = GBUF%FOR(KK(1)+I)
                  F22 = GBUF%FOR(KK(2)+I) 
                  F33 = GBUF%FOR(KK(3)+I) 
                  F44 = GBUF%FOR(KK(4)+I) 
                  F55 = GBUF%FOR(KK(5)+I)
c
                  MM11 = GBUF%MOM(KK(1)+I)
                  MM22 = GBUF%MOM(KK(2)+I)
                  MM33 = GBUF%MOM(KK(3)+I)
c
                  F1 = CP*CP*F11
     .               + SP*SP*F22
     .               + TWO*CP*SP*F33
c
                  F2 = SP*SP*F11
     .               + CP*CP*F22
     .               - TWO*CP*SP*F33
c
                  F3 =-CP*SP*F11
     .               + CP*SP*F22
     .               + (CP*CP-SP*SP )*F33
c
                  F4 =-SP*F55+CP*F44
                  F5 = CP*F55+SP*F44
c
                  MM1 = CP*CP*MM11
     .               + SP*SP*MM22
     .               + TWO*CP*SP*MM33
c
                  MM2 = SP*SP*MM11
     .               + CP*CP*MM22
     .               - TWO*CP*SP*MM33
c
                  MM3 =-CP*SP*MM11
     .               + CP*SP*MM22
     .               + (CP*CP-SP*SP )*MM33
                ELSE                 !output with respect to the global SKEW.
                  F1 = GBUF%FOR(KK(1)+I)
                  F2 = GBUF%FOR(KK(2)+I) 
                  F3 = GBUF%FOR(KK(3)+I) 
                  F4 = GBUF%FOR(KK(4)+I) 
                  F5 = GBUF%FOR(KK(5)+I)
c
                  MM1 = GBUF%MOM(KK(1)+I)
                  MM2 = GBUF%MOM(KK(2)+I)
                  MM3 = GBUF%MOM(KK(3)+I)
                ENDIF
                WWA(1) = F1
                WWA(2) = F2
                WWA(3) = F3
                WWA(4) = F4 
                WWA(5) = F5
                WWA(6) = MM1
                WWA(7) = MM2
                WWA(8) = MM3
                WWA(9) = GBUF%EINT(I)
                WWA(10)= GBUF%EINT(I+NEL)
                WWA(11)= GBUF%OFF(I)
                IF (ITHK > 0) THEN
                  WWA(12)= GBUF%THK(I)
                ELSE
                  WWA(12)= THKE(N)
                ENDIF
                WWA(13)=ZERO
                WWA(14)=ZERO
                WWA(15)=ZERO
                WWA(16)=ZERO
                WWA(17)=ZERO
                WWA(18)=ZERO
                WWA(19)=ZERO
                WWA(20)=ZERO
                WWA(21)=ZERO
                WWA(22)=ZERO
                IF (GBUF%G_EPSD == 0) THEN
                  WWA(23)=ZERO
                ELSE
                  WWA(23)=GBUF%EPSD(I)
                ENDIF
                DO J = 24,50000
                  WWA(J)=ZERO
                ENDDO
c----------------------
c              Stress tensor
c----------------------
c----           mean stress over Gauss points in each layer
                DO IL = 1,NLAY                                        
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  IMAT  = BUFLY%IMAT                          
                  NPTT  = BUFLY%NPTT
                  SIG(1:5) = ZERO
                  K = 183 + (IL-1)*5
                  DO IR=1,NPTR                    
                    DO IS=1,NPTS       
                      DO IT=1,NPTT
                        LBUF => BUFLY%LBUF(IR,IS,IT)
                        DO J = 1,5
                          SIG(J) = SIG(J) + LBUF%SIG(KK(J) + I) / (NPTT*NPG)
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                  MAT_ORTH = MATPARAM_TAB(IMAT)%ORTHOTROPY
                  IF (MAT_ORTH == 1) THEN                 
                    DO J = 1,5
                      WWA(K + J) = SIG(J)
                    ENDDO
                  ELSE IF (MAT_ORTH == 2) THEN   !  rotate sig to global coord             
                    DIR_A => ELBUF_TAB(NG)%BUFLY(IL)%DIRA
                    D1  = DIR_A(I)
                    D2  = DIR_A(I+NEL)
                    D11 = D1*D1
                    D22 = D2*D2
                    D12 = D1*D2
                    WWA(K + 1) = D11*SIG(1) + D22*SIG(2) + TWO*D12 *SIG(3)
                    WWA(K + 2) = D22*SIG(1) + D11*SIG(2) - TWO*D12 *SIG(3)
                    WWA(K + 3) =-D12*SIG(1) + D12*SIG(2) +(D11-D22)*SIG(3)
                    WWA(K + 4) =-D2 *SIG(5) + D1 *SIG(4)
                    WWA(K + 5) = D1 *SIG(5) + D2 *SIG(4)
                  END IF
                ENDDO    ! DO IL=1,NLAY                                                                     
c
c------------   Viscous stress
c
                DO IL = 1,NLAY                                        
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  IMAT  = BUFLY%IMAT                          
                  IVISC = IPM(222,IMAT)    
                  NPTT  = BUFLY%NPTT
                  IF (IVISC > 0) THEN  
                    K = 30382+(IL-1)*5
                    FUNC(1:5) = ZERO
                    DO IR=1,NPTR                                          
                      DO IS=1,NPTS                                        
                        DO IT=1,NPTT
                          LBUF => BUFLY%LBUF(IR,IS,IT)
                          DO J = 1,5
                            FUNC(J) = FUNC(J) + LBUF%VISC(KK(J) + I) / NPTT
                          ENDDO
                        ENDDO
                        DO J = 1,5
                          WWA(K+J) = FUNC(J) / NPG
                        ENDDO
                      ENDDO ! DO IS=1,NPTS
                    ENDDO   ! DO IR=1,NPTR
c
                  ENDIF     ! IVISC > 0
                ENDDO       ! DO IL=1,NLAY                                                                     
c
c------------     Viscous stress
c
c
c------------  Max/Min Plastic strain
                IF (GBUF%G_PLA > 0) THEN 
                  WWA(13) = EP30
                  WWA(14) = ZERO
c
c                  IF (NPG == 1 .and. NLAY == 1) THEN
c                    DO IPT = 1, NPT
c                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)
c                      WWA(13) = MIN(WWA(13),LBUF%PLA(I))
c                      WWA(14) = MAX(WWA(14),LBUF%PLA(I))
c                    ENDDO             
c                  ELSEIF (NLAY > 1) THEN
c                    DO IPT=1,NLAY
c                      BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
c                      IF (BUFLY%L_PLA > 0) THEN
c                        WWA(13) = MIN(WWA(13),ABS(BUFLY%PLAPT(I)))
c                        WWA(14) = MAX(WWA(14),ABS(BUFLY%PLAPT(I)))
c                      ENDIF
c                    ENDDO
c                  ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%LY_PLAPT > 0) THEN
c                    DO IPT = 1, NPT
c                      BUFLY => ELBUF_TAB(NG)%BUFLY(1)
c                      JJ = (IPT-1)*NEL
c                      WWA(13) = MIN(WWA(13),ABS(BUFLY%PLAPT(JJ+I)))
c                      WWA(14) = MAX(WWA(14),ABS(BUFLY%PLAPT(JJ+I)))
c                    ENDDO             
c                  ENDIF 
                  IF (NLAY > 1) THEN
                    IF (NPG > 1) THEN
                      DO IPT = 1,NLAY
                        BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                        IF (BUFLY%L_PLA > 0) THEN
                          WWA(13) = MIN(WWA(13),ABS(BUFLY%PLAPT(I)))
                          WWA(14) = MAX(WWA(14),ABS(BUFLY%PLAPT(I)))
                        ENDIF
                      ENDDO
                    ELSE ! (NPG = 1)
                      DO IPT = 1,NLAY
                        BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                        NPTT = BUFLY%NPTT
                        IF (BUFLY%L_PLA > 0) THEN
                          FUNC(6) = ZERO
                          DO IT=1,NPTT
                            LBUF  => BUFLY%LBUF(1,1,IT)
                            FUNC(6) = FUNC(6) + ABS(LBUF%PLA(I))/NPTT
                          ENDDO
                          WWA(13) = MIN(WWA(13),FUNC(6))
                          WWA(14) = MAX(WWA(14),FUNC(6))
                        ENDIF
                      ENDDO
                    ENDIF ! IF (NPG > 1) THEN
                  ELSE ! (NLAY = 1)
                    IF (NPG > 1) THEN
                      BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                      NPTT = BUFLY%NPTT
                      DO IT=1,NPTT
                        I1 = (IT-1)*NEL
                        IF (BUFLY%L_PLA > 0) THEN
                          WWA(13) = MIN(WWA(13),ABS(BUFLY%PLAPT(I1+I)))
                          WWA(14) = MAX(WWA(14),ABS(BUFLY%PLAPT(I1+I)))
                        ENDIF
                      ENDDO
                    ELSE ! (NPG = 1)
                      BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                      NPTT = BUFLY%NPTT
                      DO IT=1,NPTT
                        LBUF  => BUFLY%LBUF(1,1,IT)
                        IF (BUFLY%L_PLA > 0) THEN
                          WWA(13) = MIN(WWA(13),ABS(LBUF%PLA(I)))
                          WWA(14) = MAX(WWA(14),ABS(LBUF%PLA(I)))
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDIF ! IF (NLAY > 1) THEN
c----
c------------   Plastic strain per layer
c----
                  IF (MTE == 25) THEN
                    IF (IFAILURE == 0)THEN
                      WWA(30279) = FAIL(I)
                      WWA(30280) = 100*FAIL(I)/NPT
                      WWA(30281) = FAIL1(I)
                      WWA(30282) = FAIL2(I)
                      WWA(30283) = FAIL3(I)
                    ENDIF ! IFAILURE == 0 
C                                      
                    DO IPT=1,NLAY  
                      IF(IPT > 99) EXIT  
                      BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                      NPTT = BUFLY%NPTT
                      VAL_LY_AVERAGE = ZERO
                      DO IR=1,NPTR                                        
                        DO IS=1,NPTS 
                          VAL_LY_IP = ZERO
                          DO IT=1,NPTT
                              LBUF => BUFLY%LBUF(IR,IS,IT)
                              VAL_LY_IP = VAL_LY_IP + LBUF%PLA(I)/NPTT
                          ENDDO
                          VAL_LY_AVERAGE = VAL_LY_AVERAGE + VAL_LY_IP/NPG
                        ENDDO ! DO IS=1,NPTS
                      ENDDO ! DO IR=1,NPTR
                       WWA(30283 + IPT ) = VAL_LY_AVERAGE
                    ENDDO ! DO IPT=1,NLAY 
                  ENDIF ! MTE == 25 
                ENDIF     ! GBUF%G_PLA > 0
c----
c------------ Non-local plastic strain and non-local plastic strain rate
c----
              IF (GBUF%G_PLANL  > 0) THEN 
                BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                WWA(37855) = ZERO
                NPTT = BUFLY%NPTT
                DO IR = 1,NPTR
                  DO IS = 1,NPTS
                    DO IT = 1,NPTT
                      WWA(37855) = WWA(37855) + 
     .                   BUFLY%LBUF(IR,IS,IT)%PLANL(I)/(NPTR*NPTS*NPTT)
                    ENDDO
                  ENDDO
                ENDDO
              ENDIF
              IF (GBUF%G_EPSDNL > 0) THEN 
                BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                WWA(37856) = ZERO
                NPTT = BUFLY%NPTT
                DO IR = 1,NPTR
                  DO IS = 1,NPTS
                    DO IT = 1,NPTT
                      WWA(37856) = WWA(37856) + 
     .                   BUFLY%LBUF(IR,IS,IT)%EPSDNL(I)/(NPTR*NPTS*NPTT)
                    ENDDO
                  ENDDO
                ENDDO
              ENDIF
c----
c------------ User variables
c----
                IF ((MTE>=29.AND.MTE<=31).OR.
     .               MTE==35.OR.MTE==36.OR.MTE==43.OR. 
     .               MTE==44.OR.MTE==45.OR.MTE==48.OR.MTE>=50) THEN
c                   
                  NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT 
                  ALLOCATE (VAR(NUVAR,MAX(1,MPT)))
                  VAR = ZERO                                                   
C---  
                  IF (MTE == 58 .or. MTE == 158) THEN
                    IF (NLAY > 1) THEN                                                  
                      DO IL=1,NLAY                                                     
                        NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT                        
                        NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                        DO IR=1,NPTR                                                   
                          DO IS=1,NPTS                                                 
                            K = NPTR*(IS-1) + IR        
                            DO IT=1,NPTT
                              UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR              
                              DO J = 1, NUVAR
                                I1 = (J-1)*NEL                             
                                IF (J==4 .OR. J==5) THEN    ! convert to eng strain                                     
                                  VAR(J,IL) = VAR(J,IL) + (EXP(UVAR(I1+I))-ONE)/NPG
                                ELSE
                                  VAR(J,IL) = VAR(J,IL) + UVAR(I1+I)/NPG
                                ENDIF               
                                WWA(6518 + (IL-1)*60*4 + (K-1)*60 + J) = 
     .                                                    UVAR(I1 + I)  
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDDO  
                      ENDDO
                    ELSE  ! NLAY = 1                                                    
                      NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
                      NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                      DO IPT=1,NPTT                                                     
                        DO IR=1,NPTR                                                    
                          DO IS=1,NPTS                                                  
                            UVAR=>ELBUF_TAB(NG)%BUFLY(1)%MAT(IR,IS,IPT)%VAR
                            K = NPTR*(IS-1) + IR                                        
                            DO J = 1, NUVAR
                              I1 = (J-1)*NEL 
                              IF (J==4 .OR. J==5) THEN        ! convert to eng strain 
                               VAR(J,IPT) = VAR(J, IPT) + (EXP(UVAR(I1+I))-ONE)/NPG               
                              ELSE                                          
                               VAR(J,IPT) = VAR(J, IPT) + UVAR(I1 + I)/NPG               
                              ENDIF                                          
                              WWA(6518 + (IPT-1)*60*4 + (K-1)*60 + J) =                
     .                                                   UVAR(I1 + I)                                  
                            ENDDO                                                       
                          ENDDO            
                        ENDDO  
                      ENDDO  
                    ENDIF  ! NLAY                                                  
                  ELSE ! (MTE /= 58)
                    IF (NLAY > 1) THEN                                                  
                      DO IL=1,NLAY                                                     
                        NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT                        
                        NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                        DO IR=1,NPTR                                                   
                          DO IS=1,NPTS                                                 
                            K = NPTR*(IS-1) + IR
                            DO IT=1,NPTT
                              UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR              
                              DO J = 1, NUVAR
                                I1 = (J-1)*NEL                                            
                                VAR(J,IL) = VAR(J,IL) + UVAR(I1+I)/NPG               
                                WWA(6518 + (IL-1)*60*4 + (K-1)*60 + J) = 
     .                                                    UVAR(I1 + I)                                  
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDDO  
                      ENDDO
                    ELSE  ! NLAY = 1                                                    
                      NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
                      NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                      DO IPT=1,NPTT                                                     
                        DO IR=1,NPTR                                                    
                          DO IS=1,NPTS                                                  
                            UVAR=>ELBUF_TAB(NG)%BUFLY(1)%MAT(IR,IS,IPT)%VAR
                            K = NPTR*(IS-1) + IR                                        
                            DO J = 1, NUVAR
                              I1 = (J-1)*NEL                                            
                              VAR(J,IPT) = VAR(J, IPT) + UVAR(I1 + I)/NPG               
                              WWA(6518 + (IPT-1)*60*4 + (K-1)*60 + J) =                
     .                                                   UVAR(I1 + I)                                  
                            ENDDO                                                       
                          ENDDO            
                        ENDDO  
                      ENDDO  
                    ENDIF  ! NLAY                                                  
                  ENDIF    ! MTE=58                                                
C---  
                  NU = MIN(60,NUVAR)
                  DO J = 1, NU  
                    WWA(23+J)=VAR(J,IABS(MPT)/2 + 1)  ! UVAR(NU) - membrane               
                    DO IPT = 1, MPT
                      IF (J <= 20) THEN                                                   
                        IF (IPT <= 5) THEN                                                
                          IUV = 83
                          IAA = 5                  
                        ELSEIF(IPT > 5)THEN                                           
                          IUV = 678
                          IAA = 94
                        ENDIF                      
                      ELSE
                        IUV = 2558
                        IAA = 99
                      ENDIF
                      WWA(IUV + (J - 1)*IAA + IPT) = VAR(J, IPT)  ! UVAR(NU, IPT)           
                    ENDDO ! IPT =1,MPT                                                   
                  ENDDO   ! J = 1, NU                                                         
c
                  DEALLOCATE (VAR)                                                      
                ENDIF       ! MTE user                                                         
c----
c------------ Strain
c----
                IF (ISTRAIN /= 0) THEN
                  WWA(15)=GBUF%STRA(KK(1)+I)
                  WWA(16)=GBUF%STRA(KK(2)+I)
                  WWA(17)=GBUF%STRA(KK(3)+I)
                  WWA(18)=GBUF%STRA(KK(4)+I)
                  WWA(19)=GBUF%STRA(KK(5)+I)
                  WWA(20)=GBUF%STRA(KK(6)+I)
                  WWA(21)=GBUF%STRA(KK(7)+I)
                  WWA(22)=GBUF%STRA(KK(8)+I)
                ENDIF
C pinching data
                IF(IHBE ==11.AND.NPINCH > 0) THEN
                  WWA(37848:37853) = ZERO                 
                  DO IPG=1,4 
                    WWA(37847+1) = WWA(37847+1) + FOURTH*GBUF%EPGPINCHXZ(4*(I-1)+IPG)    
                    WWA(37847+2) = WWA(37847+2) + FOURTH*GBUF%EPGPINCHYZ(4*(I-1)+IPG)
                    WWA(37847+3) = WWA(37847+3) + FOURTH*GBUF%EPGPINCHZZ(4*(I-1)+IPG)
                    WWA(37847+4) = WWA(37847+4) + FOURTH*GBUF%FORPGPINCH(4*(I-1)+IPG) 
                    WWA(37847+5) = WWA(37847+5) + FOURTH*GBUF%MOMPGPINCH(8*(I-1)+2*(IPG-1)+1)
                    WWA(37847+6) = WWA(37847+6) + FOURTH*GBUF%MOMPGPINCH(8*(I-1)+2*IPG)
                  ENDDO                  
                  WWA(37847+7) = GBUF%THK(I)
                ENDIF
C end of pinching data
c---------------------------------
                DO L=IADV,IADV+NVAR-1
                  K = ITHBUF(L)
                  IJK = IJK+1
                  WA(IJK)=WWA(K)
                ENDDO
                IJK = IJK+1
                WA(IJK) = II
c--------
              ENDIF ! K==N
            ENDDO   ! I=1,NEL
c--------
          ENDIF    ! MTE /= 13
        ENDIF     ! ITY == ITYP
      ENDDO       ! NG=1,NGROUP
!   -------------------------------
            ENDIF
 666    continue
        ENDDO

C-----------
      RETURN
      END
