Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    m10law      ../engine/source/materials/mat/mat010/m10law.F
!||--- called by ------------------------------------------------------
!||    mmain       ../engine/source/materials/mat_share/mmain.F90
!||--- uses       -----------------------------------------------------
!||    debug_mod   ../engine/share/modules/debug_mod.F
!||====================================================================
      SUBROUTINE M10LAW(PM      ,OFF     ,SIG    ,EINT    ,RHO       ,
     2                  EPSQ    ,EPXE    ,VOL    ,MAT     ,SSP       ,
     3                  DVOL    ,VNEW    ,D1     ,D2      ,D3        ,
     4                  D4      ,D5      ,D6     ,SOLD1   ,SOLD2     ,
     5                  SOLD3   ,SOLD4   ,SOLD5  ,SOLD6   ,SIGY      ,
     6                  DEFP    ,PNEW    ,PSH    ,MU_NEW  ,SEQ_OUTPUT,
     7                  NEL     ,DPDM    ,DPLA   ,MU_BAK)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
Constitutive relations :
C  YIELD CRITERIA :  Drucker-Prager Yield  J2=A0+A1*P+A2*P**2
C  EOS            :  legacy input has compaction eos embedded.
C
C F = J2 - A0 + A1*P + A2*P**2 
C If F > 1 then deviatoric tensor is projected on F=1. Otherwise Elastic behavior if F<1
C Yield surface using scale factor RATIO(I).
C Pressure is cubic in compression linear in tension.
C Energy integration is made in MEINT subroutine, but eos is not energy dependent...
C
C
C VARIABLE DEFINITIONS :
C
C G0    : YIELD ENVELOPE
C AJ2   : 2ND INVARIANT FROM DEVIATORIC TENSOR
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
              use debug_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NEL
      INTEGER,INTENT(IN) :: MAT(NEL)
      my_real PM(NPROPM,*), SIG(NEL,6), MU_OLD(NEL), EPXE(NEL), EINT(NEL), RHO(NEL), VOL(NEL),SEQ_OUTPUT(NEL)
      my_real,INTENT(INOUT) :: PNEW(NEL)
      my_real,INTENT(INOUT) :: PSH(NEL)
      my_real,INTENT(IN) :: OFF(NEL)
      my_real,INTENT(INOUT) :: DPLA(NEL)
      my_real,INTENT(INOUT) :: EPSQ(NEL)
      my_real,INTENT(INOUT) :: MU_BAK(NEL)
      my_real VNEW(NEL), SSP(NEL), SIGY(NEL),DEFP(NEL),
     .         D1(NEL), D2(NEL), D3(NEL), D4(NEL), D5(NEL), D6(NEL),
     .         DVOL(NEL), MU_NEW(NEL),
     .         SOLD1(NEL), SOLD2(NEL), SOLD3(NEL),
     .         SOLD4(NEL), SOLD5(NEL), SOLD6(NEL),
     .         DPDM(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real T1(NEL), T2(NEL), T3(NEL), T4(NEL),
     .        T5(NEL), T6(NEL), POLD(NEL), P(NEL), PNE1(NEL),
     .        G(NEL), BULK(NEL), A0(NEL), A1(NEL),
     .        A2(NEL), AMX(NEL), AJ2(NEL), G0(NEL), GG(NEL),
     .        MU2(NEL), SVRT(NEL), RATIO(NEL),
     .        YIELD2(NEL), G43(NEL),
     .        RHO0(NEL),PTOT,PSTAR(NEL),
     .        G43_1,C0_1,C1_1,C2_1,C3_1,
     .        BULK_1,BULK2_1,MU_MAX_1,PSH_1,
     .        PSTAR_1,A0_1,A1_1,A2_1,AMX_1,
     .        RHO0_1,PFRAC

       integer nc
       nc = nc_debug
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      !----------------------------------------------------------------!
      !  PARAMETER INITIALIZATION                                      !
      !----------------------------------------------------------------!
      MX=MAT(1)
      G43_1    = ONEP333*PM(22,MX)   
      PSTAR_1  = PM(44,MX)          
      A0_1     = PM(38,MX)          
      A1_1     = PM(39,MX)          
      A2_1     = PM(40,MX)          
      AMX_1    = PM(41,MX)          
      RHO0_1   = PM(1,MX)   
      PSH_1    = PM(88,MX)  
      PFRAC    = PM(37,MX)
      DO I=1,NEL
        G(I)     = DT1*PM(22,MX)
        G43(I)   = G43_1
        GG(I)    = TWO*G(I)
        PSH(I)   = PSH_1
        PSTAR(I) = PSTAR_1    
        A0(I)    = A0_1
        A1(I)    = A1_1
        A2(I)    = A2_1
        AMX(I)   = AMX_1
        RHO0(I)  = RHO0_1
      ENDDO !next I
      
      !----------------------------------------------------------------!
      !  STATE INIT.                                                   !
      !----------------------------------------------------------------!        
      DO I=1,NEL
        POLD(I)=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        SVRT(I)= THIRD*(D1(I)+D2(I)+D3(I))
        MU2(I) = MU_NEW(I) * MAX(ZERO,MU_NEW(I))
      ENDDO !next I        

      !----------------------------------------------------------------!
      !  TEMPORARY DEVIATORIC STRESS TENSOR : T(1:6)                   !
      !----------------------------------------------------------------!  
      DO I=1,NEL
        T1(I)=SIG(I,1)+POLD(I)
        T2(I)=SIG(I,2)+POLD(I)
        T3(I)=SIG(I,3)+POLD(I)
        T4(I)=SIG(I,4)
        T5(I)=SIG(I,5)
        T6(I)=SIG(I,6)
      ENDDO !next I  

      !----------------------------------------------------------------!
      !  SOUND SPEED                                                   !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        DPDM(I) = G43(I) + DPDM(I)
        SSP(I)  = SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO !next I      

      !----------------------------------------------------------------!
      !  DEVIATORIC TENSOR - ELASTIC INCREMENT                         !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        T1(I)=T1(I)+GG(I)*(D1(I)-SVRT(I))
        T2(I)=T2(I)+GG(I)*(D2(I)-SVRT(I))
        T3(I)=T3(I)+GG(I)*(D3(I)-SVRT(I))
        T4(I)=T4(I)+G(I)*D4(I)
        T5(I)=T5(I)+G(I)*D5(I)
        T6(I)=T6(I)+G(I)*D6(I)
      ENDDO !next I 

      !----------------------------------------------------------------!
      !  YIELD SURFACE                                                 !
      !----------------------------------------------------------------!
      DO I=1,NEL
        AJ2(I)= HALF*(T1(I)**2+T2(I)**2+T3(I)**2)+T4(I)**2+T5(I)**2+T6(I)**2
        PTOT  = PNEW(I)+PSH(I)
        G0(I) = A0(I)+A1(I)*PTOT+A2(I)*PTOT*PTOT
        G0(I) = MIN(AMX(I),G0(I))
        G0(I) = MAX(ZERO,G0(I))
        IF(PNEW(I)<=PFRAC)G0(I)=ZERO
        IF(PTOT <= PSTAR(I))G0(I)=ZERO
        YIELD2(I)=AJ2(I)-G0(I)
      ENDDO !next I  
      
      !----------------------------------------------------------------!
      !  PROJECTION FACTOR ON YIELD SURFACE                            !
      !----------------------------------------------------------------!      
      DO  I=1,NEL
        RATIO(I)=ZERO
        IF(YIELD2(I)<=ZERO .AND. G0(I)>ZERO)THEN
          RATIO(I)=ONE
        ELSE
          RATIO(I)=SQRT(G0(I)/(AJ2(I)+ EM14))
        ENDIF
      ENDDO !next I 

      !----------------------------------------------------------------!
      !  UPDATE DEVIATORIC STRESS TENSOR IN SIG(:,:)                   !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        SIG(I,1)=RATIO(I)*T1(I)*OFF(I)
        SIG(I,2)=RATIO(I)*T2(I)*OFF(I)
        SIG(I,3)=RATIO(I)*T3(I)*OFF(I)
        SIG(I,4)=RATIO(I)*T4(I)*OFF(I)
        SIG(I,5)=RATIO(I)*T5(I)*OFF(I)
        SIG(I,6)=RATIO(I)*T6(I)*OFF(I)
        DPLA(I) = (ONE -RATIO(I))*SQRT(THREE*ABS(AJ2(I)))*DT1 / MAX(EM20,THREE*G(I))   !G <- G*DT1
      ENDDO !next I     

      !----------------------------------------------------------------!
      !  OUTPUT / MISC.                                                !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        SIGY(I) = G0(I)     !YIELD SURFACE
        EPXE(I) = EPXE(I) + DPLA(I)
        DEFP(I) = EPXE(I)
        EPSQ(I) = MU_BAK(I) ! updated if compaction EoS defined
      ENDDO !next I

      RETURN
      END
