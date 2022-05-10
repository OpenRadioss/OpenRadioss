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
Chd|  M21LAW                        source/materials/mat/mat021/m21law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE M21LAW (PM ,OFF ,SIG     ,EINT    ,RHO     ,
     2           EPX     ,EPSEQ   ,VOL     ,MAT    ,SSP     ,
     3           DVOL    ,VNEW    ,D1      ,D2     ,D3      ,
     4           D4      ,D5      ,D6      ,SOLD1  ,SOLD2   ,
     5           SOLD3   ,SOLD4   ,SOLD5   ,SOLD6  ,TF      ,
     6           NPF     ,SIGY    ,DEFP    ,IPM    ,PNEW    ,
     7           PSH     ,AMU     ,SEQ_OUTPUT,NEL  )
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
#include      "vect01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*),MAT(*),IPM(NPROPMI,*),NEL
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EPX(*), EPSEQ(*), EINT(*),
     .   RHO(*), VOL(*), TF(*), PNEW(*), PSH(*), SEQ_OUTPUT(*)
      my_real
     .   VNEW(*), SSP(*), SIGY(*), DEFP(*),
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   DVOL(*), AMU(*),
     .   SOLD1(MVSIZ), SOLD2(MVSIZ), SOLD3(MVSIZ),
     .   SOLD4(MVSIZ), SOLD5(MVSIZ), SOLD6(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, IFUNC, NPOINT
      my_real
     .   DPDM(MVSIZ), T1(MVSIZ), T2(MVSIZ), T3(MVSIZ), T4(MVSIZ), 
     .   T5(MVSIZ), T6(MVSIZ), POLD(MVSIZ), P(MVSIZ), PNE1(MVSIZ),
     .    G(MVSIZ), BULK(MVSIZ), PFRAC(MVSIZ), A0(MVSIZ), A1(MVSIZ),
     .   A2(MVSIZ), AMX(MVSIZ), AJ2(MVSIZ), G0(MVSIZ), GG(MVSIZ), 
     .   AMU2(MVSIZ), SVRT(MVSIZ), RATIO(MVSIZ), 
     .   YIELD2(MVSIZ), G43(MVSIZ), AMUMX(MVSIZ), 
     .   C0(MVSIZ), C1(MVSIZ),C2(MVSIZ), C3(MVSIZ),FACY(MVSIZ),
     .   BULK2(MVSIZ), RHO0(MVSIZ),FINTER,PSTAR(MVSIZ),PTOT,
     .   DELTA,G_1,G43_1,GG_1,C1_1,
     .   BULK_1,BULK2_1,AMUMX_1,PFRAC_1,PSH_1,
     .   PSTAR_1,A0_1,A1_1,A2_1,AMX_1,
     .   RHO0_1,FACY_1
      EXTERNAL FINTER
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine uses Drucker-Prager criteria
C F = J2 - A0 + A1*P + A2*P**2 
C to calculate deviatoric stresses.
C If F > 1 then deviatoric tensor is projected on
C Yield surface using scale factor RATIO(I).
C Pressure is calculated from user curves.
C Energy integration is made in MEINT subroutine
C
C VARIABLE DEFINITIONS :
C
C G0    : YIELD ENVELOPPE
C AJ2   : 2ND INVARIANT FROM DEVIATORIC TENSOR
C EPX   : OLD MU
C EPSEQ : VOLUMETRIC PLASTIC STRAIN
C-----------------------------------------------      
      !----------------------------------------------------------------!
      !  PARAMETER INIT.                                               !
      !----------------------------------------------------------------!  
      MX = MAT(LFT)
      G_1    =DT1*PM(22,MX)
      G43_1  =UNP333*PM(22,MX)
      GG_1   =DEUX*G_1
      C1_1   =PM(32,MX)
      BULK_1 =PM(35,MX)
      BULK2_1=PM(35,MX)
      AMUMX_1=PM(36,MX)
      PFRAC_1=PM(37,MX)
      PSH_1  =PM(43,MX)        
      PSTAR_1=PM(44,MX)        
      A0_1   =PM(38,MX)
      A1_1   =PM(39,MX)
      A2_1   =PM(40,MX)
      AMX_1  =PM(41,MX)
      RHO0_1 =PM(1,MX)
      FACY_1 =PM(42,MX)
      DO I=LFT,LLT
        G(I)    =G_1
        G43(I)  =G43_1
        GG(I)   =GG_1
        C1(I)   =C1_1
        BULK(I) =BULK_1
        BULK2(I)=BULK2_1
        AMUMX(I)=AMUMX_1
        PFRAC(I)=PFRAC_1
        PSH(I)  =PSH_1     
        PSTAR(I)=PSTAR_1      
        A0(I)   =A0_1
        A1(I)   =A1_1
        A2(I)   =A2_1
        AMX(I)  =AMX_1
        RHO0(I) =RHO0_1
        FACY(I) =FACY_1
      ENDDO

      !----------------------------------------------------------------!
      !  STATE INIT.                                                   !
      !----------------------------------------------------------------!   
      DO I=LFT,LLT
        POLD(I)=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        SVRT(I)= THIRD*(D1(I)+D2(I)+D3(I))
        AMU2(I) =AMU(I) * MAX(ZERO,AMU(I))
      ENDDO

      !----------------------------------------------------------------!
      !  DEVIATORIC STRESS TENSOR                                      !
      !----------------------------------------------------------------!  
      DO I=LFT,LLT
        T1(I)=SIG(I,1)+POLD(I)
        T2(I)=SIG(I,2)+POLD(I)
        T3(I)=SIG(I,3)+POLD(I)
        T4(I)=SIG(I,4)
        T5(I)=SIG(I,5)
        T6(I)=SIG(I,6)
      ENDDO

      !----------------------------------------------------------------!
      !  PRESSURE AND SOUND SPEED                                      !
      !----------------------------------------------------------------!   
      DO I=LFT,LLT
        IF(EPX(I).LT.0)BULK2(I)=C1(I)
        IF(AMU(I).LT.0)BULK(I) =C1(I)
        PNE1(I)=POLD(I)+AMU(I)*BULK2(I)-EPX(I)*BULK(I)
        P(I)=C1(I)*AMU(I)
        DPDM(I)=C1(I)
      ENDDO
       MX = MAT(LFT)
       IFUNC = IPM(11,MX)
      DO I=LFT,LLT
        IF(AMU(I).GT.ZERO)THEN
          P(I) = FACY(I)*FINTER(IFUNC,AMU(I),NPF,TF,DPDM(I))
        ENDIF
      ENDDO

      !----------------------------------------------------------------!
      !  UNLOADING CASE                                                !
      !----------------------------------------------------------------! 
      DO I=LFT,LLT
        IF(AMU(I).LT.AMUMX(I))P(I)= MIN(P(I),PNE1(I))
        IF(P(I).LT.PFRAC(I))THEN
         EPX(I) =EPX(I)+(PFRAC(I)-POLD(I))/BULK2(I)
         P(I)=PFRAC(I)
         A0(I)=ZERO
         A1(I)=ZERO
         A2(I)=ZERO
        ELSE
         EPX(I) =AMU(I)
        ENDIF
        DPDM(I)= G43(I) + MAX(BULK(I),DPDM(I))
      ENDDO

      DO I=LFT,LLT
        SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO

      !----------------------------------------------------------------!
      !  DEVIATORIC TENSOR - ELASTIC INCREMENT                         !
      !----------------------------------------------------------------! 
      DO I=LFT,LLT
        T1(I)=T1(I)+GG(I)*(D1(I)-SVRT(I))
        T2(I)=T2(I)+GG(I)*(D2(I)-SVRT(I))
        T3(I)=T3(I)+GG(I)*(D3(I)-SVRT(I))
        T4(I)=T4(I)+G(I)*D4(I)
        T5(I)=T5(I)+G(I)*D5(I)
        T6(I)=T6(I)+G(I)*D6(I)
      ENDDO

      !----------------------------------------------------------------!
      !  YIELD SURFACE                                                 !
      !----------------------------------------------------------------!  
      DO I=LFT,LLT
        AJ2(I)=UNDEMI*(T1(I)**2+T2(I)**2+T3(I)**2)+T4(I)**2+T5(I)**2+
     .                 T6(I)**2
        PTOT = P(I) + PSH(I)
        G0(I) =A0(I)+A1(I)*PTOT+A2(I)*PTOT*PTOT
        G0(I)= MIN(AMX(I),G0(I))
        G0(I)= MAX(ZERO,G0(I))
        IF(PTOT .LE. PSTAR(I))G0(I)=ZERO        
        YIELD2(I)=AJ2(I)-G0(I)
!!        SEQ_OUTPUT(I) = YIELD2(I)
      ENDDO

      !----------------------------------------------------------------!
      !  PROJECTION FACTOR ON YIELD SURFACE                            !
      !----------------------------------------------------------------!  
      DO I=LFT,LLT
        RATIO(I) = ZERO
        IF(YIELD2(I).LE.ZERO .AND. G0(I).GT.ZERO)THEN
          RATIO(I) = UN
        ELSE
          RATIO(I) = SQRT(G0(I)/(AJ2(I)+ EM14))
        ENDIF
      ENDDO

      !----------------------------------------------------------------!
      !  DEVIATORIC STRESS TENSOR                                      !
      !----------------------------------------------------------------!  
      DO I=LFT,LLT
        P(I)=P(I)*OFF(I)
        PNEW(I) = P(I)
        SIG(I,1)=RATIO(I)*T1(I)*OFF(I)
        SIG(I,2)=RATIO(I)*T2(I)*OFF(I)
        SIG(I,3)=RATIO(I)*T3(I)*OFF(I)
        SIG(I,4)=RATIO(I)*T4(I)*OFF(I)
        SIG(I,5)=RATIO(I)*T5(I)*OFF(I)
        SIG(I,6)=RATIO(I)*T6(I)*OFF(I)
        EPSEQ(I)=EPSEQ(I)+(UN-RATIO(I))*SQRT(AJ2(I))*DT1
     .           / MAX(EM20,TROIS*G(I))
      ENDDO
          
      !----------------------------------------------------------------!
      !  OUTPUT / MISC                                                 !
      !----------------------------------------------------------------!      
      DO I=LFT,LLT
       SIGY(I)=G0(I)     !YIELD SURFACE
       DEFP(I)=EPSEQ(I)  !VOLUMETRIC PLASTIC STRAIN
      ENDDO 
C
      RETURN
C
      END
