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
Chd|  SIGEPS105                     source/materials/mat/mat105/sigeps105.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS105 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC    ,TBURN  ,
     2     NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM   ,BFRAC  ,
     3     RHO0   ,RHO    ,VOLUME  ,EINT    ,SIGY     ,DELTAX ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NFT      ,V      ,
     B     W      ,X      ,IX      ,N48     ,NIX      ,JTHE   ,
     C     GEO    ,PID    ,ILAY    ,NG      ,ELBUF_TAB,PM     , 
     D     IPARG  ,BUFVOIS ,IPM     ,BUFMAT   ,STIFN  ,
     E     VD2    ,VDX    ,VDY     ,VDZ     ,MAT      ,VOLN   ,
     F     QNEW   ,DDVOL  ,QOLD    )  
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine computes Powder Burn EOS and is based on (Atwood,Friis,Moxnes) publication.
C ELEMENT BUFFER
C   %BFRAC  : is ignition fraction (depends on flamme position regarding the cell)
C   UVAR(6) : is burn fracttion (depends on F(t) function and also on gas pressure and grains shape)
C EOS
C   GAS    : Exponantial EOS
C   POWDER : Linear EOS
C MATERIAL BUFFER
C   UPARAM(1:NUPARAM) : material parameters
C   UVAR  (1:NUVAR)   : cell buffer specific for this material law : Pgas, etc...
C
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD   
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NFT,N48,NIX,JTHE,
     .        IX(NIX,*), PID(*), ILAY, NG,PM(NPROPM,*),IPARG(*),MAT(*),
     .        IPM(NPROPMI,*)
      my_real 
     .   TIME         ,TIMESTEP   ,UPARAM(NUPARAM),
     .   SIGY(NEL)    ,VK(NEL)    ,
     .   RHO(NEL)     ,RHO0(NEL)  ,VOLUME(NEL),
     .   EINT(NEL)    ,BUFVOIS(*) ,QNEW(NEL)    ,
     .   DDVOL(NEL)   ,QOLD(NEL)  ,
     .   EPSPXX(NEL)  ,EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL)  ,EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL)  ,DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL)  ,DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL)   ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL)   ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL)  ,SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL)  ,SIGOYZ(NEL),SIGOZX(NEL),
     .   V(3,*)       ,W(3,*)     ,X(3,*)     ,
     .   GEO(NPROPG,*),BUFMAT(*)  ,
     .   STIFN(MVSIZ) ,VD2(*)     ,VDX(*)     ,
     .   VDY(*)       ,VDZ(*)     ,SSP        ,
     .   VOLN(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL), TBURN(NEL), BFRAC(NEL), DELTAX(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real TF(*)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real,EXTERNAL :: FINTER
      ! EXTERNAL FINTER
      !      Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
      !      Y       : y = f(x)
      !      X       : x
      !      DYDX    : f'(x) = dy/dx
      !      IFUNC(J): FUNCTION INDEX
      !            J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
      !      NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: P,Qa,QB,QAL,QBL, POLD,VOLD,EINC,PNEW
      my_real :: BULK, D, EG, Gr, C, ALPHA,BETA, C1, C2,FSCALE_G,FSCALE_B,FSCALE_P,FSCALE_RHO
      my_real :: P0,XL, DD, TFEXTT, PSH, VOLO, ESPE, E0, TOTAL_BFRAC, DELTA_BF, BRATE
      my_real :: PG,PG_,PS, MU_G, MU_P, MU, RHO_G,RHO_G_, RHO_S, RHOG, RHO_Si
      INTEGER :: funcb, funcg
      INTEGER :: IBID, IBFRAC, I , K     
      my_real :: MASS, DF, TB, DIFF, G_RHOC2, P_RHOC2, RHOC2, SSP_G, SSP_S, MASS_G, MASS_S, VS
      my_real :: VOL_G, VOL_S, fac,compac,INVrhog
      my_real :: m0,rho_s0,F,Y_old,Y,FUNC_DOT,FUNC,V0,TMP
      
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF      
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      TFEXTT =  ZERO
      GBUF   => ELBUF_TAB(NG)%GBUF           
      LBUF   => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
      BUFLY  => ELBUF_TAB(NG)%BUFLY(ILAY)      
C-----------------------------------------------
C   S t a n d a r d   J W L   E O S 
C-----------------------------------------------  
      BULK       = UPARAM(01)   
      P0         = UPARAM(02)          
      PSH        = UPARAM(03)   
      E0         = UPARAM(04)   
      D          = UPARAM(05)   
      EG         = UPARAM(06)   
      Gr         = UPARAM(07)   
      C          = UPARAM(08)   
      ALPHA      = UPARAM(09)  
      C1         = UPARAM(10)  
      C2         = UPARAM(11)         
      FSCALE_G   = UPARAM(12)  
      FSCALE_B   = UPARAM(13)  
      FSCALE_P   = UPARAM(14)  
      FSCALE_RHO = UPARAM(15)  
      funcb      = IFUNC(1)  
      funcg      = IFUNC(2) 
      
      compac=0.93
                 
      IF(DT1==ZERO)THEN
        DO I=1,NEL
          EINT(I) = EG*RHO0(I)*VOLUME(I)
          UVAR(I,1) = P0                 !PS             
          UVAR(I,2) = ZERO               !PG             
          UVAR(I,3) = RHO0(I)/compac     !RHO_S          
          UVAR(I,4) = ZERO               !RHO_G          
          UVAR(I,5) = P0                 !POLD           
          UVAR(I,6) = ZERO               !F(t_old)        
          UVAR(I,7) = RHO0(I)*VOLUME(I)  !Mass0             
        ENDDO
        SIGNXX(1:NEL) = - P0 
        SIGNYY(1:NEL) = - P0  
        SIGNZZ(1:NEL) = - P0 
        SIGNXY(1:NEL) = ZERO 
        SIGNYZ(1:NEL) = ZERO 
        SIGNZX(1:NEL) = ZERO 
        SIGVXX(1:NEL) = ZERO 
        SIGVYY(1:NEL) = ZERO 
        SIGVZZ(1:NEL) = ZERO 
        SIGVXY(1:NEL) = ZERO 
        SIGVYZ(1:NEL) = ZERO 
        SIGVZX(1:NEL) = ZERO 
        DO I=1,NEL
          SOUNDSP(I)= SQRT(BULK/RHO0(I))
        ENDDO
      ENDIF

      DO I=1,NEL
    
        !--------------------------------!
        ! INIT.                          !
        !--------------------------------!
        MASS = RHO(I)*VOLUME(I)      
        XL   = DELTAX(I)
        ESPE = EINT(I)/MASS 
        PS   =UVAR(I,1)          
        PG   =UVAR(I,2)          
        RHO_S=UVAR(I,3)
        RHO_G=UVAR(I,4)
        POLD =UVAR(I,5)  
                        
        !--------------------------------!
        ! IGNITION FRACTION              !
        !--------------------------------!
          !--> valid for const flamme velocity
        IF(BFRAC(I) < ONE) THEN
         TB = - TBURN(I)
         BFRAC(I) = ZERO
         IF(TIME > TB) BFRAC(I) = C1*(TIME-TB)*TWO_THIRD/XL 
         IF(BFRAC(I) < EM04) THEN
           BFRAC(I) = ZERO
         ELSEIF(BFRAC(I) > ONE) THEN
           BFRAC(I) = ONE
         ENDIF
        ENDIF


                
        !------------------------------------------------!
        ! BURN FRACTION                                  !
        !  BFRAC  : ignition fraction (flamme evolution) !
        !  UVAR(6): burn fraction (grain burn evolution) !
        !------------------------------------------------!
        IF(BFRAC(I)<=ZERO)THEN
          TOTAL_BFRAC = ZERO
          UVAR(I,6) = ZERO
        ELSEIF(UVAR(I,6)/=ONE)THEN  ! F<1
            PG_=PG 
            !PG_=PG*(ONE-MASS_S/VOLN(I)/RHO_S)  !effective gaz pressure Pg*(1-RHO_S_/rhop)
            BRATE       = FSCALE_B*FINTER(funcb,(PG_)/FSCALE_P,NPF,TF,DIFF)
            DELTA_BF    = Gr*EXP(C*LOG((ONE-ALPHA*UVAR(I,6))))*BRATE*TIMESTEP  !F_dot * DT           
            UVAR(I,6)   = UVAR(I,6) + DELTA_BF  !F
            UVAR(I,6)   = MAX(MIN(ONE,UVAR(I,6)),ZERO)
            TOTAL_BFRAC = BFRAC(I)*UVAR(I,6)
        ELSE
            ! avoid LOG(0)
            TOTAL_BFRAC = BFRAC(I)*UVAR(I,6)
        ENDIF       
        

        !--------------------------------!
        ! EOS SOLVING                    !
        !--------------------------------!
        MASS_S=(ONE-UVAR(I,6))*UVAR(I,7)    !(1-F)*M0
        MASS_G=UVAR(I,7)-MASS_S            !Mass_Gas=F(t)*Mass
        
        RHO_S=(ONE-UVAR(I,6))*RHO(I)   !MASS_S/VOLN(I)
        RHO_G=ZERO
        
        !in case of compressible solid (linear EOS)
        RHO_Si=RHO0(I)/compac*((UVAR(I,2)-P0)/BULK+ONE)
        
        !in case of constant solid density
        !rho_si=rho0(I)/compac
        
        
       IF(MASS_G>ZERO)RHO_G=MASS_G/(VOLN(I)-MASS_S/ max(EM10,RHO_Si)) 

         
        PG   = RHO_G*EG*EXP(RHO_G/D)
        
        PS   = BULK*(RHO_Si/RHO0(I))
        SSP_S= SQRT(BULK/RHO0(I))
        
        IF(RHO_G>ZERO)THEN
          SSP_G = RHO0(I)/D*Pg + EXP(RHO_G/D)* Pg / (RHO_G/RHO0(I))**2
          SSP_G = SQRT(ONE/RHO_G * SSP_G)
        ELSE
          SSP_G = ZERO
        ENDIF

   
        !--------------------------------!
        ! Global Pressure & SoundSpeed   !
        !--------------------------------! 
        SSP   = TOTAL_BFRAC*SSP_G + (ONE-TOTAL_BFRAC)*SSP_S        
        PNEW  = TOTAL_BFRAC*PG    + (ONE-TOTAL_BFRAC)*PS
        PNEW  = (PNEW-PSH)*OFF(I)
        !--------------------------------!
        ! Returning values               !
        !--------------------------------!     
        SIGNXX(I)  = -PNEW
        SIGNYY(I)  = -PNEW
        SIGNZZ(I)  = -PNEW
        SIGNXY(I)  = ZERO
        SIGNYZ(I)  = ZERO
        SIGNZX(I)  = ZERO
        SIGVXX(I)  = ZERO
        SIGVYY(I)  = ZERO
        SIGVZZ(I)  = ZERO
        SIGVXY(I)  = ZERO
        SIGVYZ(I)  = ZERO
        SIGVZX(I)  = ZERO   
        SOUNDSP(I) = SSP
        
        UVAR(I,1)=PS
        UVAR(I,2)=PG
        UVAR(I,3)=RHO_S                
        UVAR(I,4)=RHO_G        
        UVAR(I,5)=PNEW        
       !UVAR(I,6) -> F(t)
       !UVAR(I,7) -> Mass0  
       
          
        if(time==ZERO)then
        OPEN(UNIT=1,FILE='F.txt'    )
        OPEN(UNIT=2,FILE='Pg.txt'   )
        OPEN(UNIT=3,FILE='rhoS.txt' )
        OPEN(UNIT=4,FILE='Ifrac.txt' )
        else
        OPEN(UNIT=1,FILE='F.txt'   , position="append")
        OPEN(UNIT=2,FILE='Pg.txt'  , position="append")
        OPEN(UNIT=3,FILE='rhoS.txt', position="append")
        OPEN(UNIT=4,FILE='Ifrac.txt',position="append")
        endif

        WRITE(1,*)TIME,UVAR(I,6)
        WRITE(2,*)TIME,Pg
        WRITE(3,*)TIME,RHO_S
        WRITE(4,*)BFRAC(I)

        CLOSE(1)
        CLOSE(2)
        CLOSE(3)
         

      ENDDO! next I   




C-----------------------------------------------
      RETURN
C-----------------------------------------------
      END SUBROUTINE SIGEPS105
C-----------------------------------------------
