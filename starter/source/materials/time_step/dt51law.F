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
Chd|  DT51LAW                       source/materials/time_step/dt51law.F
Chd|-- called by -----------
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|-- calls ---------------
Chd|        DTEL                          source/materials/time_step/dtel.F
Chd|        DTSPH                         source/materials/time_step/dtsph.F
Chd|====================================================================
      SUBROUTINE DT51LAW (PM,GEO,PID,MAT, BUFMAT, IPM, DELTAX, AIRE, VOL, DTX)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     ELEMENTARY TIME STEPS FOR ALE MULTI MATERIAL LAW 51 (/MAT/MULTIMAT)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) :: PM(NPROPM, *), GEO(NPROPG, *), AIRE(*), VOL(*)
      my_real, INTENT(INOUT) :: DTX(*)
      my_real, INTENT(IN), DIMENSION(:), TARGET :: BUFMAT(*), DELTAX(*)
      INTEGER, INTENT(IN) :: PID(*),MAT(*),IPM(NPROPMI, *)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                                :: I, MX, IADBUF,IFLG(MVSIZ)
      my_real,DIMENSION(:),POINTER           :: UPARAM
      
      my_real
     .   SSP(MVSIZ)  , DPDM(MVSIZ) , RHO0(MVSIZ) , G(MVSIZ)    , 
     .   BULK(MVSIZ) , C1(MVSIZ)   , G43(MVSIZ)  , P           ,
     .   AV1(MVSIZ)  , AV2(MVSIZ)  , AV3(MVSIZ)  , AV4(MVSIZ)  ,
     .   RHO10(MVSIZ), RHO20(MVSIZ), RHO30(MVSIZ), RHO40(MVSIZ),     
     .   C01(MVSIZ)  , C02(MVSIZ)  , C03(MVSIZ)  , C04(MVSIZ)  ,
     .   C11(MVSIZ)  , C12(MVSIZ)  , C13(MVSIZ)  , C14(MVSIZ)  ,          
     .   C21(MVSIZ)  , C22(MVSIZ)  , C23(MVSIZ)  , C24(MVSIZ)  ,         
     .   C31(MVSIZ)  , C32(MVSIZ)  , C33(MVSIZ)  , C34(MVSIZ)  ,         
     .   C41(MVSIZ)  , C42(MVSIZ)  , C43(MVSIZ)  , C44(MVSIZ)  ,         
     .   C51(MVSIZ)  , C52(MVSIZ)  , C53(MVSIZ)  , C54(MVSIZ)  ,                             
     .   G1 (MVSIZ)  , G2(MVSIZ)   , G3 (MVSIZ)  , G4 (MVSIZ)  ,    
     .   E01(MVSIZ)  , E02(MVSIZ)  , E03(MVSIZ)  , E04(MVSIZ)  , 
     .   PM1(MVSIZ)  , PM2(MVSIZ)  , PM3(MVSIZ)  , PM4(MVSIZ)  ,    
     .   DPDM1(MVSIZ), DPDM2(MVSIZ), DPDM3(MVSIZ), DPDM4(MVSIZ),      
     .   PEXT        , PFAR        , VDET(MVSIZ) ,
     .   SSP1(MVSIZ) , SSP2(MVSIZ) , SSP3(MVSIZ) , SSP4(MVSIZ), 
     .   VIS(MVSIZ)  , RHO0_BAK(MVSIZ)
     
c-----------------------------------------------
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        IADBUF   = IPM(7,MAT(I))
        UPARAM   =>BUFMAT(IADBUF:IADBUF+280)
        AV1(I)   = UPARAM(04)  
        AV2(I)   = UPARAM(05)  
        AV3(I)   = UPARAM(06)  
        AV4(I)   = UPARAM(46)
        PFAR     = UPARAM(07)  
        PEXT     = UPARAM(08)  
        RHO10(I) = UPARAM(09)  
        RHO20(I) = UPARAM(10) 
        RHO30(I) = UPARAM(11) 
        RHO40(I) = UPARAM(47)
        C11(I)   = UPARAM(12) 
        C12(I)   = UPARAM(13) 
        C13(I)   = UPARAM(14) 
        C14(I)   = ZERO
        C21(I)   = UPARAM(15) 
        C22(I)   = UPARAM(16) 
        C23(I)   = UPARAM(17)
        C24(I)   = ZERO 
        C31(I)   = UPARAM(18) 
        C32(I)   = UPARAM(20) 
        C33(I)   = UPARAM(21)
        C34(I)   = ZERO 
        C41(I)   = UPARAM(22) 
        C42(I)   = UPARAM(23) 
        C43(I)   = UPARAM(24) 
        C44(I)   = ZERO
        C51(I)   = UPARAM(25) 
        C52(I)   = UPARAM(26) 
        C53(I)   = UPARAM(27) 
        C54(I)   = ZERO
        G1(I)    = UPARAM(28)*TWO_THIRD 
        G2(I)    = UPARAM(29)*TWO_THIRD 
        G3(I)    = UPARAM(30)*TWO_THIRD 
        G4(I)    = ZERO
        IFLG(I)  = UPARAM(31) 
        E01(I)   = UPARAM(32) 
        E02(I)   = UPARAM(33) 
        E03(I)   = UPARAM(34) 
        E04(I)   = UPARAM(48)      
        C01(I)   = UPARAM(35) 
        C02(I)   = UPARAM(36) 
        C03(I)   = UPARAM(37) 
        C04(I)   = UPARAM(49)      
        PM1(I)   = UPARAM(39) 
        PM2(I)   = UPARAM(40) 
        PM3(I)   = UPARAM(41)
        PM4(I)   = UPARAM(56) 
        VDET(I)  = UPARAM(42)
        VIS(I) = ZERO
      ENDDO!next I

      IF (IFLG(1) == 6) THEN
        DO I=LFT,LLT
          DTX(I)=EP20
        ENDDO
        RETURN 
      ENDIF

      DO I=LFT,LLT
        !---submat-1---!
        IF(AV1(I)>ZERO) THEN
          DPDM1(I) = C11(I)+C51(I)*E01(I)+C41(I)*(C01(I)+C41(I)*E01(I))
          DPDM1(I) = G1(I) + MAX(DPDM1(I), C11(I))
        ELSE
          DPDM1(I) = ZERO
        ENDIF
        !---submat-2---!
        IF(AV2(I)>ZERO) THEN
          DPDM2(I) = C12(I)+C52(I)*E02(I)+C42(I)*(C02(I)+C42(I)*E02(I))
          DPDM2(I) = G2(I) + MAX(DPDM2(I), C12(I))
        ELSE
          DPDM2(I) = ZERO
        ENDIF
        !---submat-3---!
        IF(AV3(I)>ZERO) THEN
          DPDM3(I) = C13(I)+C53(I)*E03(I)+C43(I)*(C03(I)+C43(I)*E03(I))
          DPDM3(I) = G3(I) + MAX(DPDM3(I), C13(I))
        ELSE
          DPDM3(I) = ZERO
        ENDIF
      ENDDO!next I

      DO I=LFT,LLT
        !---submat-1---!      
        IF(AV1(I)>ZERO) THEN
          SSP1(I) = SQRT(ABS(DPDM1(I))/RHO10(I))
        ELSE
          SSP1(I) = ZERO
        ENDIF
        !---submat-2---!        
        IF(AV2(I)>ZERO) THEN        
          SSP2(I)=SQRT(ABS(DPDM2(I))/RHO20(I))
        ELSE
          SSP2(I)=ZERO
        ENDIF
        !---submat-3---!        
        IF(AV3(I)>ZERO) THEN        
          SSP3(I)=SQRT(ABS(DPDM3(I))/RHO30(I))
        ELSE
          SSP3(I)=ZERO
        ENDIF        
        !---submat-3---!  
        IF(AV4(I)>ZERO) THEN        
          SSP4(I)=VDET(I)
        ELSE
          SSP4(I)=ZERO
        ENDIF                                          
      END DO!next I
      
      DO I=LFT,LLT
        RHO0(I) = AV1(I)*RHO10(I) + AV2(I)*RHO20(I) + AV3(I)*RHO30(I) + AV4(I)*RHO40(I)
      ENDDO
            
      DO I=LFT,LLT
        BULK(I) =   AV1(I)*RHO10(I)*SSP1(I)**2 
     .            + AV2(I)*RHO20(I)*SSP2(I)**2 
     .            + AV3(I)*RHO30(I)*SSP3(I)**2 
     .            + AV4(I)*RHO40(I)*SSP4(I)**2
        IF (RHO0(I) > ZERO) THEN
           SSP(I)  = SQRT(BULK(I)/RHO0(I))
           RHO0_BAK(I)=RHO0(I)
        ELSE
           SSP(I) = EM20
           RHO0_BAK(I)=EM20
        ENDIF
      ENDDO!next I
      
      !----------------------------------------------!
      !  ELEMENTARY TIME STEP (ARTIFICIAL VISCOSITY) !
      !----------------------------------------------!
      IF(JSPH==0)THEN
       CALL DTEL(SSP,PM,GEO,PID,MAT, RHO0_BAK, VIS, DELTAX, AIRE, VOL, DTX)
      ELSE
       CALL DTSPH(SSP,PM,GEO,PID,MAT, RHO0_BAK, VIS, DELTAX, VOL, DTX)
      ENDIF
C-----------
      RETURN
      END
