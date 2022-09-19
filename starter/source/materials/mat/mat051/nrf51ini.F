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
Chd|  NRF51INI                      source/materials/mat/mat051/nrf51ini.F
Chd|-- called by -----------
Chd|        M51INIT                       source/materials/mat/mat051/m51init.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE NRF51INI(IPM             ,     PM,      X, NIX,    IX,
     .                    ALE_CONNECTIVITY, BUFMAT, UPARAM, NV46, RHO0,
     .                    UVAR            , NUVAR , NEL   , RHO
     .                   )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines is automatically initializing
C law51-NRF parameter according state from adjacent
C element. User is no longer asked to copy/paste
C material parameters. Starter is doing it automatically
C
C NRF READS
C AV, RHO, E, PMIN, P0, SSP
C 0.0 means automatic initialisation
C otherwise use read
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr10_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "scr11_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: NIX,IX(NIX,NEL),IPM(NPROPMI,NUMMAT),NV46,NUVAR,NEL
      my_real               :: PM(NPROPM,NUMMAT), X(3,NUMNOD),  UPARAM(*),  RHO0
      my_real,TARGET        :: BUFMAT(*)
      my_real,INTENT(INOUT) :: UVAR(NEL,NUVAR), RHO(NEL)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IVMAT,IMAT, IID, MLN, J, J2, IADBUF,
     .        IFLGv, IE, IEV, NUPARAM,MUID,K,IFORM,IAD1,LGTH
      my_real
     .   C01,C02,C03,C04,         C01v,C02v,C03v,C04v,
     .   C11,C12,C13,C14,         C11v,C12v,C13v,C14v,
     .   RHO01,RHO02,RHO03,RHO04, RHO01v,RHO02v,RHO03v,RHO04v,    
     .   C21,C22,C23,C24,         C21v,C22v,C23v,C24v,       
     .   C31,C32,C33,C34,         C31v,C32v,C33v,C34v,       
     .   C41,C42,C43,C44,         C41v,C42v,C43v,C44v,       
     .   C51,C52,C53,C54,         C51v,C52v,C53v,C54v,       
     .   E01,E02,E03,E04,         E01v,E02v,E03v,E04v,       
     .   P01,P02,P03,P04,         P01v,P02v,P03v,P04v,       
     .   PM1,PM2,PM3,PM4,         PM1v,PM2v,PM3v,PM4v,       
     .   AV1,AV2,AV3,AV4,         AV1v,AV2v,AV3v,AV4v,       
     .   SSP1,SSP2,SSP3,SSP4,     SSP1v,SSP2v,SSP3v,SSP4v, 
     .   GG1v,GG2v,GG3v,
     .   DPDMU1v, DPDMU2v,DPDMU3v,
     .   PEXT, PEXTv, P0_NRF, P0_NRFv, AVSUM,
     .   LC,Xmin,Ymin,Zmin,Xmax,Ymax,Zmax, SSP0,TCP, TMP,
     .   AV(4)
     
      my_real,POINTER,DIMENSION(:) :: UPRM
     
      INTEGER lFOUND_51, lFOUND_other, lSET
C-----------------------------------------------

C=======================================================================
C   GET ADJACENT VALUES AND SET INITIAL STATE
C=======================================================================
        AV1        = UPARAM(04)                                                                                              
        AV2        = UPARAM(05)                                                                                              
        AV3        = UPARAM(06)                                                                                              
        AV4        = UPARAM(46)                                                                                              
        RHO01      = UPARAM(09)                                                                                              
        RHO02      = UPARAM(10)                                                                                              
        RHO03      = UPARAM(11)                                                                                              
        RHO04      = UPARAM(47)                                                                                              
        E01        = UPARAM(32)                                                                                              
        E02        = UPARAM(33)                                                                                              
        E03        = UPARAM(34)                                                                                              
        E04        = UPARAM(48)                                                                                              
        C11        = UPARAM(12)                                                                                              
        C12        = UPARAM(13)                                                                                              
        C13        = UPARAM(14)                                                                                              
        C14        = UPARAM(50)                                                                                              
        C21        = UPARAM(15)                                                                                              
        C22        = UPARAM(16)                                                                                              
        C23        = UPARAM(17)                                                                                              
        C24        = ZERO                                                                                                    
        C31        = UPARAM(18)                                                                                              
        C32        = UPARAM(19)                                                                                              
        C33        = UPARAM(20)                                                                                              
        C34        = ZERO                                                                                                    
        C41        = UPARAM(22)                                                                                              
        C42        = UPARAM(23)                                                                                              
        C43        = UPARAM(24)                                                                                              
        C44        = ZERO                                                                                                    
        C51        = UPARAM(25)                                                                                              
        C52        = UPARAM(26)                                                                                              
        C53        = UPARAM(27)                                                                                              
        C54        = ZERO                                                                                                    
        PM1        = UPARAM(39)                                                                                              
        PM2        = UPARAM(40)                                                                                              
        PM3        = UPARAM(41)                                                                                              
        PM4        = UPARAM(56)                                                                                              
        PEXT       = UPARAM(8)                                                                                               
        C01        = UPARAM(35)    !here C01v is EOS coefficient and C01 is initial pressure                                 
        C02        = UPARAM(36)                                                                                              
        C03        = UPARAM(37)                                                                                              
        C04        = UPARAM(49)                                                                                              
        SSP1       = UPARAM(124)                                                                                             
        SSP2       = UPARAM(174)                                                                                             
        SSP3       = UPARAM(224)                                                                                             
        SSP4       = UPARAM(273)                                                                                             
        P0_NRF     = AV1*(C01+C41*E01) +  AV2*(C02+C42*E02) + AV3*(C03+C43*E03) + AV4*(C04+C44*E04)    
        SSP0       = ZERO 
        P01        = C01+C41*E01
        P02        = C02+C42*E02
        P03        = C03+C43*E03
        P04        = C04     

        DO I=LFT,LLT
          IE                   = I + NFT
          IID                  = IX(NIX,IE)
          IMAT                 = IX(1,IE)
          lFOUND_51            = 0
          lFOUND_other         = 0
          NUPARAM              = IPM(9,IMAT)
          UVAR(I,4)            =  P0_NRF  ! default initialization for corner elems
          IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
          LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1) - IAD1
          DO J=1,LGTH
            IEV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
            IF(IEV == 0)CYCLE
            IVMAT              = ABS(IX(1,IEV))
            MLN                = NINT(PM(19,IVMAT))
            IFLGv              = 0
            IF(MLN/=51)lFOUND_other = lFOUND_other + 1
            IADBUF             = IPM(7,IVMAT)
            IFLGv              = NINT(BUFMAT(IADBUF-1+31))            
            IF(IFLGv>=2 .AND. IFLGv<=6 )CYCLE
            lFOUND_51          = lFOUND_51 + 1
            !-----------------------------------------!
            ! GET ADJACENT LAW51 PARAMETER            !
            !   necessarily defined - check by lecm51 !
            !-----------------------------------------!
            AV1v    = BUFMAT(IADBUF-1+04)    ; 
            AV2v    = BUFMAT(IADBUF-1+05)    ; 
            AV3v    = BUFMAT(IADBUF-1+06)    ; 
            AV4v    = BUFMAT(IADBUF-1+46)    ; 
            RHO01v  = BUFMAT(IADBUF-1+09)    ; 
            RHO02v  = BUFMAT(IADBUF-1+10)    ; 
            RHO03v  = BUFMAT(IADBUF-1+11)    ; 
            RHO04v  = BUFMAT(IADBUF-1+47)    ; 
            E01v    = BUFMAT(IADBUF-1+32)    ; 
            E02v    = BUFMAT(IADBUF-1+33)    ; 
            E03v    = BUFMAT(IADBUF-1+34)    ; 
            E04v    = BUFMAT(IADBUF-1+48)    ; 
            C11v    = BUFMAT(IADBUF-1+12)    ; 
            C12v    = BUFMAT(IADBUF-1+13)    ; 
            C13v    = BUFMAT(IADBUF-1+14)    ; 
            C14v    = BUFMAT(IADBUF-1+50)    ; 
            C21v    = BUFMAT(IADBUF-1+15)    ; 
            C22v    = BUFMAT(IADBUF-1+16)    ; 
            C23v    = BUFMAT(IADBUF-1+17)    ; 
            C24v    = ZERO                   ; 
            C31v    = BUFMAT(IADBUF-1+18)    ; 
            C32v    = BUFMAT(IADBUF-1+19)    ; 
            C33v    = BUFMAT(IADBUF-1+20)    ; 
            C34v    = ZERO                   ;  				
            C41v    = BUFMAT(IADBUF-1+22)    ; 
            C42v    = BUFMAT(IADBUF-1+23)    ; 
            C43v    = BUFMAT(IADBUF-1+24)    ; 
            C44v    = ZERO                   ; 
            C51v    = BUFMAT(IADBUF-1+25)    ; 
            C52v    = BUFMAT(IADBUF-1+26)    ; 
            C53v    = BUFMAT(IADBUF-1+27)    ; 
            C54v    = ZERO                   ; 
            PM1v    = BUFMAT(IADBUF-1+39)    ; 
            PM2v    = BUFMAT(IADBUF-1+40)    ; 
            PM3v    = BUFMAT(IADBUF-1+41)    ; 
            PM4v    = BUFMAT(IADBUF-1+56)    ; 
            PEXTv   = BUFMAT(IADBUF-1+8)     ; 
            C01v    = BUFMAT(IADBUF-1+35)    ; 
            C02v    = BUFMAT(IADBUF-1+36)    ; 
            C03v    = BUFMAT(IADBUF-1+37)    ; 
            C04v    = BUFMAT(IADBUF-1+49)    ; 
            P01v    = C01v + C41v*E01v
            P02v    = C02v + C42v*E02v
            P03v    = C03v + C43v*E03v
            P04v    = C04v                                    
            SSP1v   = ZERO
            SSP2v   = ZERO
            SSP3v   = ZERO
            SSP4v   = ZERO          
            DPDMU1v = (C11v+C51v*E01v) + C41v*(PEXTv+P01v)
            DPDMU2v = (C12v+C52v*E02v) + C42v*(PEXTv+P02v)
            DPDMU3v = (C13v+C53v*E03v) + C43v*(PEXTv+P03v)           
            GG1v    = BUFMAT(IADBUF-1+28)
            GG2v    = BUFMAT(IADBUF-1+29)
            GG3v    = BUFMAT(IADBUF-1+30)                                                          
            IF(RHO01v /= ZERO) SSP1v  = SQRT( (DPDMU1v + TWO_THIRD*GG1v) / RHO01v )   !warning GG1v = 2*G1v   =>  TWO_THIRD*GG = FOUR_OVER_3*G
            IF(RHO02v /= ZERO) SSP2v  = SQRT( (DPDMU2v + TWO_THIRD*GG2v) / RHO02v )
            IF(RHO03v /= ZERO) SSP3v  = SQRT( (DPDMU3v + TWO_THIRD*GG3v) / RHO03v )
                               SSP4v  = BUFMAT(IADBUF-1+42)  !VDETv            
            !-----------------------------------------!
            ! SET AUTOMATIC LAW51 PARAMETERS          !
            !-----------------------------------------!
            UVAR(I,4)  =  P0_NRF
            P0_NRFv    =  AV1v*P01v + AV2v*P02v + AV3v*P03v + AV4v*P04v
            IF(P0_NRF==ZERO)UVAR(I,4)  =  P0_NRFv

            AVSUM = AV1+AV2+AV3+AV4            
            IF(AVSUM==ZERO) THEN
               AV(1) = AV1v
               AV(2) = AV2v
               AV(3) = AV3v
               AV(4) = AV4v
              ! 
               K=N0PHAS+(1-1)*NVPHAS
               UVAR(I,K+01)=AV1v
               UVAR(I,K+23)=AV1v
               K=N0PHAS+(2-1)*NVPHAS
               UVAR(I,K+01)=AV2v
               UVAR(I,K+23)=AV2v
               K=N0PHAS+(3-1)*NVPHAS
               UVAR(I,K+01)=AV3v
               UVAR(I,K+23)=AV3v
               K=N0PHAS+(4-1)*NVPHAS
               UVAR(I,K+01)=AV4v
               UVAR(I,K+23)=AV4v
            ELSE
               AV(1) = AV1
               AV(2) = AV2
               AV(3) = AV3
               AV(4) = AV4
               K=N0PHAS+(1-1)*NVPHAS
               UVAR(I,K+01)=AV1
               UVAR(I,K+23)=AV1
               K=N0PHAS+(2-1)*NVPHAS
               UVAR(I,K+01)=AV2
               UVAR(I,K+23)=AV2
               K=N0PHAS+(3-1)*NVPHAS
               UVAR(I,K+01)=AV3
               UVAR(I,K+23)=AV3
               K=N0PHAS+(4-1)*NVPHAS
               UVAR(I,K+01)=AV4
               UVAR(I,K+23)=AV4
            ENDIF   

            K=N0PHAS+(1-1)*NVPHAS                          
            IF(RHO01==ZERO)THEN
              UVAR(I,K+09)= RHO01v
              UVAR(I,K+12)= RHO01v
              UVAR(I,K+20)= RHO01v
              RHO01 =RHO01v
            ELSE
              UVAR(I,K+09)= RHO01
              UVAR(I,K+12)= RHO01
              UVAR(I,K+20)= RHO01
            ENDIF
            IF(E01==ZERO)THEN
              UVAR(I,K+08)= E01v
              UVAR(I,K+21)= E01v
            ELSE
              UVAR(I,K+08)= E01
              UVAR(I,K+21)= E01
            ENDIF
            IF(C01==ZERO)THEN
              UVAR(I,K+18)= P01v
            ELSE
              UVAR(I,K+18)= P01
            ENDIF
            IF(SSP1==ZERO)THEN
              UVAR(I,K+22)= SSP1v
              SSP1 = SSP1v
              SSP0 = MAX(SSP0,SSP1)
            ELSE
              UVAR(I,K+22)= SSP1
              SSP0 = MAX(SSP0,SSP1)
            ENDIF

            K=N0PHAS+(2-1)*NVPHAS                          
            IF(RHO02==ZERO)THEN
              UVAR(I,K+09)= RHO02v
              UVAR(I,K+12)= RHO02v
              UVAR(I,K+20)= RHO02v
              RHO02 =RHO02v
            ELSE
              UVAR(I,K+09)= RHO02
              UVAR(I,K+12)= RHO02
              UVAR(I,K+20)= RHO02
            ENDIF
            IF(E02==ZERO)THEN
              UVAR(I,K+08)= E02v
              UVAR(I,K+21)= E02v
            ELSE
              UVAR(I,K+08)= E02
              UVAR(I,K+21)= E02
            ENDIF
            IF(C02==ZERO)THEN
              UVAR(I,K+18)= P02v
            ELSE
              UVAR(I,K+18)= P02
            ENDIF
            IF(SSP2==ZERO)THEN
              UVAR(I,K+22)= SSP2v
              SSP2 = SSP2v
              SSP0 = MAX(SSP0,SSP2)
            ELSE
              UVAR(I,K+22)= SSP2
              SSP0 = MAX(SSP0,SSP2)
            ENDIF         

            K=N0PHAS+(3-1)*NVPHAS                          
            IF(RHO03==ZERO)THEN
              UVAR(I,K+09)= RHO03v
              UVAR(I,K+12)= RHO03v
              UVAR(I,K+20)= RHO03v
              RHO03 =RHO03v
            ELSE
              UVAR(I,K+09)= RHO03
              UVAR(I,K+12)= RHO03
              UVAR(I,K+20)= RHO03
            ENDIF
            IF(E03==ZERO)THEN
              UVAR(I,K+08)= E03v
              UVAR(I,K+21)= E03v
            ELSE
              UVAR(I,K+08)= E03
              UVAR(I,K+21)= E03
            ENDIF
            IF(C03==ZERO)THEN
              UVAR(I,K+18)= P03v
            ELSE
              UVAR(I,K+18)= P03
            ENDIF
            IF(SSP3==ZERO)THEN
              UVAR(I,K+22)= SSP3v
              SSP3 = SSP3v
              SSP0 = MAX(SSP0,SSP3)
            ELSE
              UVAR(I,K+22)= SSP3
              SSP0 = MAX(SSP0,SSP3)
            ENDIF 

            K=N0PHAS+(4-1)*NVPHAS                          
            IF(RHO04==ZERO)THEN
              UVAR(I,K+09)= RHO04v
              UVAR(I,K+12)= RHO04v
              UVAR(I,K+20)= RHO04v
              RHO04 =RHO04v
            ELSE
              UVAR(I,K+09)= RHO04
              UVAR(I,K+12)= RHO04
              UVAR(I,K+20)= RHO04
            ENDIF
            IF(E04==ZERO)THEN
              UVAR(I,K+08)= E04v
              UVAR(I,K+21)= E04v
            ELSE
              UVAR(I,K+08)= E04
              UVAR(I,K+21)= E04
            ENDIF
            IF(C04==ZERO)THEN
              UVAR(I,K+18)= P04v
            ELSE
              UVAR(I,K+18)= P04
            ENDIF
            IF(SSP4==ZERO)THEN
              UVAR(I,K+22)= SSP4v
              SSP4 = SSP4v
              SSP0 = MAX(SSP0,SSP4)
            ELSE
              UVAR(I,K+22)= SSP4
              SSP0 = MAX(SSP0,SSP4)
            ENDIF 

            RHO0 = AV(1)*RHO01+AV(2)*RHO02+AV(3)*RHO03+AV(4)*RHO04
            PM( 1,IMAT) = RHO0 !NINT(PM(1,IVMAT))
            PM(89,IMAT) = RHO0 !NINT(PM(19,IVMAT))
            RHO(I)      = RHO0
                                                                                                                      
            EXIT
          ENDDO! next J          
          !checking input file
          !2 adjacent elem in computation domain: this is not expected. 1 boundary elem => 1  adjacent elem
          DO J2=J+1,LGTH
            IEV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J2 - 1)
            IF(IEV /= 0)THEN
              MLN        = NINT(PM(19,IVMAT))
              IFLGv      = 0
              IVMAT      = ABS(IX(1,IEV))              
              IF(MLN/=51 )lFOUND_other = lFOUND_other + 1
              IADBUF     = IPM(7,IVMAT)
              IFLGv      = NINT(BUFMAT(IADBUF-1+31))
              IF(IFLGv>=2 .AND. IFLGv<=6 )CYCLE 
              lFOUND_51  = lFOUND_51 + 1
            ENDIF
          ENDDO
          IF(lFOUND_other /= 0)THEN
              CALL ANCMSG(MSGID   = 122     ,
     .                    MSGTYPE = MSGERROR,
     .                    ANMODE  = ANINFO  ,
     .                    I1      = IID     ,
     .                    C1      = "MUST BE ADJACENT TO MM-ALE LAW51 PART" ) 
          ENDIF    
          IF(lFOUND_51 == 0)THEN
              CALL ANCMSG(MSGID   = 123     ,
     .                    MSGTYPE = MSGWARNING,
     .                    ANMODE  = ANINFO  ,
     .                    I1      = IID     ,
     .                    C1      = "HAS NO ADJACENT FACE IN COMPUTATION DOMAIN" )
          ENDIF              
          IF(lFOUND_51 >= 2)THEN
              CALL ANCMSG(MSGID   = 122     ,
     .                    MSGTYPE = MSGERROR,
     .                    ANMODE  = ANINFO  ,
     .                    I1      = IID     ,
     .                    C1      = "MUST HAVE ONLY ONE FACE ADJACENT TO COMPUTATION DOMAIN" )
          ENDIF
        ENDDO !next I

        SSP0MAX=MAX(SSP0MAX,SSP0)





C=======================================================================
C   AUTOMATIC CHARACTERISTIC LENGTH COMPUTATION
C=======================================================================
       
       !-----------------------------!
       ! SSP0MAX  (sound speed)      !
       !-----------------------------!
       IF(ILOOP_NRF51==0)THEN
          DO I=1,NUMELS+NUMELQ
            IMAT   = IX(1,I)
            MLN    = IPM(2,IMAT)
            IADBUF = IPM(7,IMAT)
            IF(MLN/=51)CYCLE
            UPRM => BUFMAT(IADBUF:IADBUF+275)
            TMP  = UPRM(31)
            IFORM = NINT(TMP)
            IF(IFORM>1)CYCLE
            SSP0 = UPRM(124)
            SSP0 = MAX(SSP0,UPRM(174))
            SSP0 = MAX(SSP0,UPRM(224))
            SSP0 = MAX(SSP0,UPRM(273))
          ENDDO
          SSP0MAX = SSP0 
         ILOOP_NRF51 = 1
       ENDIF    
       !-----------------------------!
       ! Characteristic Length LC    !
       !-----------------------------!
       IF(LC0MAX==ZERO)THEN
        Xmin   = X(1,1)
        Ymin   = X(2,1)
        Zmin   = X(3,1)
        Xmax   = X(1,1)
        Ymax   = X(2,1)
        Zmax   = X(3,1)
        DO I=1,NUMNOD
          Xmin = MIN(Xmin,X(1,I))
          Ymin = MIN(Ymin,X(2,I))
          Zmin = MIN(Zmin,X(3,I))
          Xmax = MAX(Xmax,X(1,I))
          Ymax = MAX(Ymax,X(2,I))
          Zmax = MAX(Zmax,X(3,I))
        ENDDO
        LC     = Xmax-Xmin
        LC     = MAX(LC,Ymax-Ymin)
        LC     = MAX(LC,Zmax-Zmin)
        LC0MAX = LC   
       ENDIF
       !-----------------------------!
       ! Tcp : characteric time      !
       !-----------------------------!       
       TCP_REF    = LC0MAX/TWO/SSP0MAX/LOG(TWO)       
       !Si Tcp=0 alors calcul automatique
       IF(UPARAM(70)==ZERO)THEN
         UPARAM(70) = TCP_REF
         lSET = 1
       ELSE
         lSET = 0
       ENDIF     
       
       if(lSET==1) lSET_51_IFLG6 = 1  
C----------------------------------------------- 
      RETURN
      END SUBROUTINE NRF51INI
