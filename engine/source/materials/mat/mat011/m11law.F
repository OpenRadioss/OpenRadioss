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
Chd|  M11LAW                        source/materials/mat/mat011/m11law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        M11VS2                        source/materials/mat/mat011/m11vs2.F
Chd|        M11VS3                        source/materials/mat/mat011/m11vs3.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE M11LAW(PM   ,OFF ,SIG   ,EINT     ,RHO    ,
     2                  T    ,RK  ,RE    ,VO       ,
     3                  ALE_CONNECT,IX  ,IPARG ,ELBUF_TAB,V      ,
     4                  X    ,EIS ,STIF  ,W        ,VD2    ,
     5                  VDX  ,VDY ,VDZ   ,VIS      ,MAT    ,
     6                  VOL  ,QVIS,RHO0  ,BUFVOIS  ,IPM    ,
     7                  NPF  ,TF  ,SSP_EQ,MOM      ,NEL    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE ALE_CONNECTIVITY_MOD
      USE ALEFVM_MOD
C-----------------------------------------------
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(*), IPARG(*),MAT(*),IPM(NPROPMI,*),NEL
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), T(*), RK(*),
     .   RE(*), V(3,*), W(3,*),VIS(*),VOL(*),QVIS(*),
     .   X(3,*), VO(*), EIS(*),STIF(*),VD2(*),VDX(*) ,VDY(*) ,VDZ(*),
     .   RHO0(*),BUFVOIS(*),SSP_EQ(*), MOM(NEL,3)
      INTEGER :: NPF(*)
      my_real :: TF(*)
      my_real ,EXTERNAL :: FINTER
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, ITYP, IRFUN, IPFUN, IEFUN, ITFUN, IKFUN,
     .   ISFUN, INOD, IVxFUN, IVyFUN, IVzFUN, IALEFVM_FLG
      my_real
     .   GAM(MVSIZ), P0(MVSIZ)   , VCRT(MVSIZ), GAM1(MVSIZ), P(MVSIZ),
     .   VEL(MVSIZ), GAMRP(MVSIZ), RHOV(MVSIZ), PV(MVSIZ)  , EIV(MVSIZ), 
     .   REV(MVSIZ), RKV(MVSIZ)  , TV(MVSIZ)  , EXPT(MVSIZ), 
     .   VxV(MVSIZ), VyV(MVSIZ)  , VzV(MVSIZ) ,
     .   FAC, CART, E0, XE0, RHOF, PF, EF,  T0,
     .   XK0, PSH, RHON, PN, EN, GV, RSR, RV, C1, DC, ALP, DU, DP,
     .   RHOC,RHOC2,DYDX, VxN(MVSIZ), VyN(MVSIZ), VzN(MVSIZ),
     .   Vx0(MVSIZ), Vy0(MVSIZ), Vz0(MVSIZ),
     .   XmomV(MVSIZ), YmomV(MVSIZ), ZmomV(MVSIZ) ,
     .   XmomN,YmomN,ZmomN   
      my_real
     .   DVP(MVSIZ), VN(MVSIZ),VIMP (MVSIZ),SSP,TSCAL,
     .   RHO0_1, GAM_1, P0_1, VCRT_1, GAMRP_1, GAM1_1, Vx_1, Vy_1, Vz_1, MASS
C-----------------------------------------------
C=======================================================================

      !-----------------------------------!
      !  READING USER INPUTS              !
      !-----------------------------------!      
      MX          = MAT(LFT)
      RHO0_1      = PM( 1,MX)
      GAM_1       = PM(25,MX)
      P0_1        = PM(31,MX)
      VCRT_1      = PM(27,MX)
      GAMRP_1     = PM(28,MX)
      GAM1_1      = PM(29,MX)
      Vx_1        = PM(101,MX)     
      Vy_1        = PM(102,MX)
      Vz_1        = PM(103,MX) 
      IALEFVM_FLG = IPM(251,MX)    
      ITYP        = PM(50,MX)
      E0          = PM(23,MX)
      XE0         = PM(33,MX)
      RHOF        = PM(35,MX)
      PF          = PM(36,MX)
      EF          = PM(37,MX)
      IRFUN       = IPM(11,MX)
      IPFUN       = IPM(12,MX)
      IEFUN       = IPM(13,MX)
      ITFUN       = IPM(14,MX)
      IKFUN       = IPM(15,MX)
      ISFUN       = IPM(16,MX)
      IVxFUN      = IPM(18,MX)
      IVyFUN      = IPM(19,MX)
      IVzFUN      = IPM(20,MX)            
      TSCAL       = PM(40,MX)*TT
      INOD        = NINT(PM(51,MX))
      T0          = PM(79,MX)
      XK0         = PM(87,MX)
      PSH         = PM(88,MX)       
      
      !-----------------------------------!
      !  ELEM INIT. FROM USER INPUTS      !
      !-----------------------------------!       
      DO I=LFT,LLT
        RHO0(I)   = RHO0_1
        GAM(I)    = GAM_1
        P0(I)     = P0_1
        VCRT(I)   = VCRT_1
        GAMRP(I)  = GAMRP_1
        GAM1(I)   = GAM1_1
        Vx0(I)    = Vx_1
        Vy0(I)    = Vy_1
        Vz0(I)    = Vz_1     
        TV(I) = ZERO
      ENDDO

      DO I=LFT,LLT
        STIF(I)   = ZERO
        EIS(I)    = EIS(I) + EINT(I) / VOL(I)
      ENDDO

      IF(ALEFVM_Param%IEnabled == 1)THEN
        IF(ITYP /= 3)THEN        
          DO I=LFT,LLT
            IF(IVxFUN>0)THEN
              VxN(I) = Vx0(I)*FINTER(IVxFUN,TSCAL,NPF,TF,DYDX)
            ELSE
              VxN(I) = Vx0(I)
            ENDIF      
            IF(IVyFUN>0)THEN
              VyN(I) = Vy0(I)*FINTER(IVyFUN,TSCAL,NPF,TF,DYDX)
            ELSE
              VyN(I) = Vy0(I)          
            ENDIF    
            IF(IVzFUN>0)THEN
              VzN(I) = Vz0(I)*FINTER(IVzFUN,TSCAL,NPF,TF,DYDX)
            ELSE
              VzN(I) = Vz0(I)          
            ENDIF                    
          ENDDO
        ELSE!IF(ITYP /= 3)  
          DO I=LFT,LLT
            MASS   = RHO(I) * VOL(I)
            VxN(I) = MOM(I,1) / MASS
            VyN(I) = MOM(I,2) / MASS
            VzN(I) = MOM(I,3) / MASS			
          ENDDO          
        ENDIF
      ELSE
        VxN(I) = ZERO
        VyN(I) = ZERO
        VzN(I) = ZERO                
      ENDIF!IF(IALEFVM==1)

      !-----------------------------------!
      !  GET ADJACENT VALUES              !
      !-----------------------------------!  
      IF(N2D==0) THEN
       CALL M11VS3(PM    ,IPARG ,IX    ,ALE_CONNECT       ,ELBUF_TAB   ,V  ,
     2             X     ,DVP   ,VN    ,W           ,VEL         ,VD2,
     3             VDX   ,VDY   ,VDZ   ,MAT         ,RHOV        ,PV ,
     4             EIV   ,REV   ,RKV   ,TV          ,
     5             SSP_EQ,VxV   ,VyV   ,VzV         ,IALEFVM_FLG ,
     6             VxN   ,VyN   ,VzN   ,MOM         ,RHO         ,VOL,
     7             XmomV ,YmomV ,ZmomV ,BUFVOIS     ,NEL         )
       FAC=EIGHT
      ELSE
       CALL M11VS2(PM    ,IPARG ,IX    ,ALE_CONNECT       ,ELBUF_TAB   ,V     ,
     2             X     ,DVP   ,VN    ,W           ,VEL         ,VD2   ,
     3             VDY   ,VDZ   ,VIS   ,MAT         ,RHOV ,PV    ,
     4             EIV   ,REV   ,RKV   ,TV          ,
     5             SSP_EQ,VxV   ,VyV   ,VzV         ,IALEFVM_FLG ,
     6             VxN   ,VyN   ,VzN   ,MOM         ,RHO         ,VOL   ,
     7             XmomV ,YmomV ,ZmomV ,BUFVOIS     ,NEL         )     
       FAC=FOUR
      ENDIF

      MX          = MAT(LFT)
      DO I=LFT,LLT
        CART      = PM(34,MX)
        QVIS(I)   = ZERO
        EXPT(I)   = EXP(CART*TT**2)
      ENDDO


      !-----------------------------------!
      ! LOOP ON ELEMENTS                  !
      !-----------------------------------!
      DO I=LFT,LLT    
        
        IF(INOD/=0)THEN
         VEL(I)   = V(1,INOD)**2+V(2,INOD)**2+V(3,INOD)**2
        ENDIF

        !---------------------------------!
        !  IMPOSED STATE ITYP=0,1         !
        !---------------------------------! 
        IF(ITYP<2)THEN
          RHOV(I) = RHO0(I)
          PV(I)   = P0(I)
          EIV(I)  = E0
          VxV(I)  = Vx0(I)
          VyV(I)  = Vy0(I)
          VzV(I)  = Vz0(I)  
        !---------------------------------!
        !  OUTGOING FLUX ITYP=ALL         !
        !---------------------------------!                             
        ELSEIF(VN(I)<ZERO)THEN
          RHOV(I) = RHO(I)
          EIV(I)  = EINT(I)/VOL(I)
          RKV(I)  = RK(I)/VOL(I)
          REV(I)  = RE(I)/VOL(I)
          TV(I)   = T(I)
          IF(ALEFVM_Param%IEnabled == 1)THEN !otherwise %MOM is not allocated
            MASS    = RHO(I) * VOL(I)
            VxV(I)  = MOM(I,1)/MASS
            VyV(I)  = MOM(I,2)/MASS
            VzV(I)  = MOM(I,3)/MASS 
            XmomV(I)= MOM(I,1)
            YmomV(I)= MOM(I,2)
            ZmomV(I)= MOM(I,3)  			     
          ENDIF
        ENDIF

        IF(IRFUN>0)THEN
          RHON    = RHO0(I)*FINTER(IRFUN,TSCAL,NPF,TF,DYDX)
        ELSEIF(IRFUN<0)THEN
          RHON    = RHOF+(RHO0(I)-RHOF)*EXPT(I)
        ELSE
          RHON   = RHOV(I)
        ENDIF

        IF(IPFUN>0)THEN
          PN     = P0(I)*FINTER(IPFUN,TSCAL,NPF,TF,DYDX)
        ELSEIF(IPFUN<0)THEN
          PN     = PF+(P0(I)-PF)*EXPT(I)
        ELSE
          PN     = PV(I)
        ENDIF

        IF(ITYP==5 .OR. ITYP==6 )THEN
          IF(IPFUN>0)THEN
            VIMP = P0(I)*FINTER(IPFUN,TSCAL,NPF,TF,DYDX)
          ELSE
            VIMP(I)=P0(I)
          ENDIF
        ENDIF
        
        IF(IEFUN>0)THEN
          EN = E0*FINTER(IEFUN,TSCAL,NPF,TF,DYDX)
        ELSEIF(IEFUN<0)THEN
          EN     =EF+(E0-EF)*EXPT(I)
        ELSE
          EN     =EIV(I)
        ENDIF

        IF(JTUR/=0)THEN
          IF(IKFUN>0)THEN
           RK(I) = RHO0(I)*FINTER(IKFUN,TSCAL,NPF,TF,DYDX)
          ELSE
           RK(I) = RKV(I)
          ENDIF
          IF(ISFUN>0)THEN
           RE(I) = RHO0(I)*FINTER(ISFUN,TSCAL,NPF,TF,DYDX)
          ELSE
           RE(I) = REV(I)
          ENDIF
        ENDIF

        IF(ITFUN>0)THEN
          T(I) = T0*FINTER(ITFUN,TSCAL,NPF,TF,DYDX)
        ELSE
          T(I)   =TV(I)
        ENDIF
        
      IF(ALEFVM_Param%IEnabled==1)THEN   
        IF(IVxFUN>0)THEN
          XmomN = Vx0(I)*FINTER(IVxFUN,TSCAL,NPF,TF,DYDX)*RHON*VOL(I)
        ELSE
          XmomN = XmomV(I)
        ENDIF      
        IF(IVyFUN>0)THEN
          YmomN = Vy0(I)*FINTER(IVyFUN,TSCAL,NPF,TF,DYDX)*RHON*VOL(I)
        ELSE
          YmomN = YmomV(I)          
        ENDIF    
        IF(IVzFUN>0)THEN
          ZmomN = Vz0(I)*FINTER(IVzFUN,TSCAL,NPF,TF,DYDX)*RHON*VOL(I)
        ELSE
          ZmomN = ZmomV(I)          
        ENDIF                    
      ENDIF        
        
        !-------------------------------------------!
        ! CAS   0 et 1     STAGNATION POINT         !
        !-------------------------------------------!
        !ALEFVM no yet implemented
        IF(ITYP==0)THEN
C        GAZ
         VEL(I) = MIN(VEL(I),VCRT(I))
         DC=PM(99,MX)
         GV  = DC*GAMRP(I)*RHON*VEL(I)/(PN+PSH)
         IF(GV>EM4)THEN
          RSR    = (ONE - GV)**GAM1(I)
          P(I)   = (PN+PSH)*RSR**GAM(I)-PSH
         ELSE
          RV     = HALF*RHON*VEL(I)*DC
          RSR    = ONE - RV/((PN+PSH)*GAM(I))
          P(I)   = PN - RV
         ENDIF
         RHO(I) = RSR*RHON
         P(I)   = P(I) + TWO_THIRD*RK(I)
         EINT(I)= GAM1(I)*(P(I)+PSH)
         SSP_EQ(I) = SQRT(GAM1(I)*(PN+PSH)/RHON)
        ELSEIF(ITYP==1)THEN
C        LIQUIDE
         DC        = PM(99,MX)
         C1        = PM(32,MX)
         RHO(I)    = C1*RHON/(C1+HALF*DC*RHON*VEL(I))
         P(I)      = PN-HALF*DC*RHO(I)*VEL(I)
         EINT(I)   = (ONE - RHO(I)/RHON)*(P(I)+PSH)+EN
         SSP_EQ(I) = SQRT(C1/RHON)
         IF(IPFUN/=0.AND.JTUR/=0)P(I)=P(I)+ TWO_THIRD*RK(I)
         
        !-------------------------------------------!
        ! CAS   2    IMPOSED STATE                  !
        !-------------------------------------------!
        ELSEIF(ITYP==2.OR.ITYP==8)THEN
         RHO(I)     = RHON
         P(I)       = PN
         EINT(I)    = EN
         SSP_EQ(I)  = EP20
         IF(IPFUN/=0.AND.JTUR/=0)P(I)=P(I)+TWO_THIRD*RK(I)
         IF(ALEFVM_Param%IEnabled==1)THEN
           MOM(I,1)   = XmomN
           MOM(I,2)   = YmomN
           MOM(I,3)   = ZmomN			    
         ENDIF
         
        !-------------------------------------------!
        ! CAS   3   NON REFLECTING BOUNDARY         !
        !-------------------------------------------!
        ELSEIF(ITYP==3 .OR. ITYP==7)THEN
         RHO(I)    = RHON
         EINT(I)   = EN
         RHOC      = PM(97,MX)
         ALP       = PM(98,MX)*DT1
         IF(IPFUN/=0.AND.JTUR/=0)PN=PN + TWO_THIRD*RK(I)
         IF(IPFUN/=0 .AND. VN(I)<=0)PN=PN - HALF*RHON*VN(I)**2
         IF(ALEFVM_Param%IEnabled==1)THEN
           MOM(I,1) = XmomN
           MOM(I,2) = YmomN
           MOM(I,3) = ZmomN		    
         ENDIF         
C
         IF(TT>ZERO)THEN
          IF(IPFUN==0 .OR. VN(I)<=ZERO .OR. ITYP==3)THEN
            DU     = (ONE-ALP)*RHOC*(VN(I)-VO(I))
           ELSE
            DU     = (ONE - ALP)*RHOC*((VN(I)-VO(I))-VN(I)*DVP(I))
          ENDIF 
         ELSE
          DU       = ZERO
          SIG(I,1) = -PN
         ENDIF
         DP        = ALP*(SIG(I,1)+PN)
         VO(I)     = VN(I)
         P(I)      = -SIG(I,1)+DP+DU
         SSP_EQ(I) = RHOC / RHO0(I) !adjacent value can also be output but recquires SPMD treatment.
         
        !-------------------------------------------!
        ! CAS   4  NON REFLECTING BOUNDARY          !
        !           + TRANSVERSE RIGIDITY           !
        !-------------------------------------------!
        ELSEIF(ITYP==4)THEN
         RHO(I) = RHON
         EINT(I)= EN
         RHOC=PM(97,MX)
         RHOC2=RHON*(RHOC/RHO0(I))**2
         ALP=PM(98,MX)*DT1
         IF(IPFUN/=0.AND.JTUR/=0)PN=PN + TWO_THIRD*RK(I)
         IF(IPFUN/=0 .AND. VN(I)<=ZERO)PN=PN - HALF*RHON*VN(i)**2
         IF(TT>ZERO)THEN
          DU=(ONE - ALP)*(RHOC*(VN(I)-VO(I))-RHOC2*DVP(I))
         ELSE
          DU=ZERO
          SIG(I,1)=-PN
         ENDIF

         DP        = ALP*(SIG(I,1)+PN)
         VO(I)     = VN(I)
         P(I)      = -SIG(I,1)+DP+DU
         SSP_EQ(I) = RHOC / RHO0(I)
        !-------------------------------------------!
        ! CAS  5   IMPOSED VELOCITY                !
        !           + NON REFLECTING                !
        !-------------------------------------------!
        ELSEIF(ITYP==5)THEN
         IF(TT==ZERO)VO(I) = PV(I)
         RHO(I)    = RHON
         EINT(I)   = EN
         ALP       = PM(98,MX)*DT1
         RHOC      = PM(97,MX)
         VO(I)     = VO(I)+ALP*(PV(I)-VO(I))
         P(I)      = RHOC*(VN(I)-VIMP(I))+VO(I)
         SSP_EQ(I) = RHOC / RHO0(I)

        !-------------------------------------------!
        ! CAS   6   IMPOSED MASS FLOW               !
        !           + NRF                           !
        !           + CONSTANT SSP                  !
        !           + RHO = RO_ADJACENT             !
        !-------------------------------------------!
        ELSEIF(ITYP==6)THEN
         IF(TT==ZERO)VO(I) = PV(I)
         RHO(I)    = RHON
         EINT(I)   = EN
         ALP       = PM(98,MX)*DT1
         SSP       = PM(97,MX)/RHO0(I)
         VO(I)     = VO(I)+ALP*(PV(I)-VO(I))
         P(I)      = SSP*(RHOV(I)*VN(I)-VIMP(I))+VO(I)
         SSP_EQ(I) = SSP !adjacent value can also be output but recquires SPMD treatment.
        ENDIF
      ENDDO !next I

C-----------
      DO I=LFT,LLT
        EIS(I)   = EIS(I) - EINT(I) 
        SIG(I,1) = -P(I)
        SIG(I,2) = -P(I)
        SIG(I,3) = -P(I)
        SIG(I,4) = ZERO
        SIG(I,5) = ZERO
        SIG(I,6) = ZERO
      ENDDO
C-----------
      RETURN
      END
