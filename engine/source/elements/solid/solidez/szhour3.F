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
Chd|  SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|        SZETFAC                       source/elements/solid/solidez/szhour3.F
Chd|        SZSTRAINHG                    source/elements/solid/solidez/szhour3.F
Chd|        SZSVM                         source/elements/solid/solidez/szhour3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SZHOUR3(ELBUF_STR,PM,RHO,OFF,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   PX1H1, PX1H2, PX1H3, PX1H4, 
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4, 
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   VOL,MAT,CXX,VIS,VD2,DELTAX,PID,GEO,PARTSAV,IPARTS,
     .   DXX, DYY, DZZ, D4, D5, D6 ,
     .   FHOUR,JR0,JS0,JT0,EINT,VOL0,SIGY,SIG0,SIGOLD,ICP,DEFP,
     .   MATVIS,ET  ,D_MAX,NEL,STRHG,ISTRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "cong1_c.inc"
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scrupw_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,ISTRAIN
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*), RHO(*),OFF(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   PX1H1(*), PX1H2(*), PX1H3(*), PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*), PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*), PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*), PX4H4(*),
     .   PARTSAV(NPSAV,*),
     .   VOL(*),CXX(*),VIS(*),VD2(*),DELTAX(*), 
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,EINT(*),
     .   DXX(*), DYY(*), DZZ(*), D4(*), D5(*), D6(*) ,
     .   SIGY(*) ,SIG0(NEL,6),VOL0(*),SIGOLD(NEL,6),DEFP(*),ET(*),
     .   D_MAX(*),STRHG(NEL,18)
      INTEGER MAT(*),PID(*),IPARTS(*),ICP,MATVIS
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,K,IET, MT
      my_real
     .   CAQ(MVSIZ), FCL(MVSIZ), FCQ(MVSIZ),DEINT(MVSIZ),
     .   H11(MVSIZ), H22(MVSIZ), H33(MVSIZ),
     .   H12(MVSIZ), H13(MVSIZ), H23(MVSIZ),
     .   HGX1(MVSIZ), HGX2(MVSIZ), HGX3(MVSIZ), HGX4(MVSIZ),
     .   HGY1(MVSIZ), HGY2(MVSIZ), HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ1(MVSIZ), HGZ2(MVSIZ), HGZ3(MVSIZ), HGZ4(MVSIZ),
     .   VX3478, VX2358, VX1467, VX1256, 
     .   VY3478, VY2358, VY1467, VY1256,
     .   VZ3478, VZ2358, VZ1467, VZ1256,
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46, 
     .   G_3DT(MVSIZ),NU,GG(MVSIZ),DE,DS,DSIG(6),
     .   SM1(MVSIZ),SM2(MVSIZ),SMO1(MVSIZ),SMO2(MVSIZ),SMO,
     .   JR_1(MVSIZ),JS_1(MVSIZ),JT_1(MVSIZ),NFHOUR(MVSIZ,3,4),
     .   DFHOUR(MVSIZ,3,4),FHOURT(3,4),DT05,RHO0,ETMAX,
     .   NU1(MVSIZ),NU2(MVSIZ),NU3(MVSIZ),NU4(MVSIZ),NEP,E0(MVSIZ),
     .   E_R,E_S,E_T,FAC,FAC1,FAC2,COEFH,HQ13P,HQ13N,HQ24P,HQ24N,FF,
     .   SIG0V(MVSIZ,6),DAMA_G(MVSIZ,6),G0,C1
C-----------------------------------------------
       IET =IINT
       COEFH = ZEP9999   
C---- attention : r->eta; s->zeta; t->ksi------------

C +++ MAT Visco-----
!      DO I=LFT,LLT
!        SIG0V(I,1) = SIG0(I,1)
!        SIG0V(I,2) = SIG0(I,2)
!        SIG0V(I,3) = SIG0(I,3)
!        SIG0V(I,4) = SIG0(I,4)
!        SIG0V(I,5) = SIG0(I,5)
!        SIG0V(I,6) = SIG0(I,6)
!      ENDDO
      MX = MAT(LFT)
      RHO0=PM(1,MX)
      NU=PM(21,MX)
      G0=PM(22,MX)
      C1=PM(32,MX)
      SELECT CASE (MTN)
C  +++ MAT Hydro---
        CASE (3,4,6,70) 
          DO I=LFT,LLT
           GG(I)=MAX(G0,VIS(I))    
          ENDDO        
C  +++ MAT Hyper-elas----Et=1
        CASE (62,95)
          DO I=LFT,LLT
           GG(I) = 0.75*(RHO(I)*CXX(I)*CXX(I)-C1)
          ENDDO
C  +++ MAT Hyper-elas----Et>1
        CASE (42,69,82,92,94)
          GG(LFT:LLT)=G0    
        CASE DEFAULT
          DO I=LFT,LLT
           GG(I)=HALF*RHO0*CXX(I)*CXX(I)*(ONE -TWO*NU)/(ONE-NU)     
          ENDDO
      END SELECT
C        
C
      IF (IET > 1 .AND. MATVIS>0 ) THEN
       CALL SZETFAC(LFT,LLT,IET,MTN,ET,GG )
      ELSEIF (MATVIS==1.AND.ISMSTR<10) THEN
       DO I=LFT,LLT
        FF=THIRD*(DXX(I)+DYY(I)+DZZ(I))
        DE =(DXX(I)-FF)*(DXX(I)-FF)+(DYY(I)-FF)*(DYY(I)-FF)+
     .      (DZZ(I)-FF)*(DZZ(I)-FF) + FOURTH*(D4(I)*D4(I)+
     .                               D5(I)*D5(I)+D6(I)*D6(I))
         DE = DE*DT1
         DSIG(1)=SIG0(I,1)-SIGOLD(I,1)
         DSIG(2)=SIG0(I,2)-SIGOLD(I,2)
         DSIG(3)=SIG0(I,3)-SIGOLD(I,3)
         DSIG(4)=SIG0(I,4)-SIGOLD(I,4)
         DSIG(5)=SIG0(I,5)-SIGOLD(I,5)
         DSIG(6)=SIG0(I,6)-SIGOLD(I,6)
         FF= THIRD*(DSIG(1)+DSIG(2)+DSIG(3))
         DSIG(1)=DSIG(1)-FF
         DSIG(2)=DSIG(2)-FF
         DSIG(3)=DSIG(3)-FF
         DS =DSIG(1)*DSIG(1)+DSIG(2)*DSIG(2)+DSIG(3)*DSIG(3)+
     .       DSIG(4)*DSIG(4)+DSIG(5)*DSIG(5)+DSIG(6)*DSIG(6) 
         GG(I)=MAX(FIVEEM2*GG(I),SQRT(DS/MAX(DE,EM30)))
       ENDDO
      ENDIF
C
      IF(INVSTR>=35)THEN
        MT = PID(LFT)
        DO I=LFT,LLT
          CAQ(I)=FOURTH*OFF(I)*GEO(13,MT)
        ENDDO
      ELSE
        MX = MAT(LFT)
        DO I=LFT,LLT
          CAQ(I)=FOURTH*OFF(I)*PM(4,MX)
        ENDDO
      ENDIF
      DO I=LFT,LLT
        G_3DT(I)=THIRD*OFF(I)*GG(I)*DT1
        E0(I)=TWO*(ONE+NU)*GG(I)
      ENDDO
C
C MATERIAUX NON FLUIDES CXX->SPP
      IF (IET > 1 ) THEN
C------now when IHKT=2, w.r.t. Isolid=1, fac=0.2*0.006666 ~0.001     
       DO I=LFT,LLT
         FCL(I)=ONEP1*CAQ(I)*RHO(I)*VOL(I)**THIRD
         FCL(I)=ZEP00666666667*FCL(I)*CXX(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         FCL(I)=CAQ(I)*RHO(I)*VOL(I)**THIRD
         FCL(I)=ZEP00666666667*FCL(I)*CXX(I)
       ENDDO
      END IF
C
C------ w/ Prony, use Icpre=0 anyway
      IF (MATVIS>2) THEN
       DO I=LFT,LLT
        NU1(I) =TWO/(ONE-NU)
        NU2(I) =NU*NU1(I)
        NU3(I) =TWO_THIRD*(ONE + NU)
        NU4(I) =NU
       ENDDO
      ELSEIF(ICP==1)THEN
       DO I=LFT,LLT
        NU1(I) =FOUR_OVER_3
        NU2(I) =-TWO_THIRD
        NU3(I) =ZEP444
        NU4(I) =ZERO
       ENDDO
      ELSEIF(ICP==2)THEN
       DO I=LFT,LLT
        FAC1 = SIGY(I)/E0(I)+DEFP(I)
        FAC2=ONE-DEFP(I)/FAC1
        FF =(ONE +NU)/(ONE -TWO*NU)*FAC2
        NU1(I) =TWO_THIRD*(TWO+FF)
        NU2(I) =TWO_THIRD*(FF-ONE)
        NU3(I) =ZEP222*(TWO+FF)
        NU4(I) =ZERO
       ENDDO
      ELSE
       DO I=LFT,LLT
        NU1(I) =TWO/(ONE-NU)
        NU2(I) =NU*NU1(I)
        NU3(I) =TWO_THIRD*(ONE + NU)
        NU4(I) =NU
       ENDDO
      ENDIF
      DT05 =HALF*DT1
      DO I=LFT,LLT
       VX3478=VX3(I)-VX4(I)-VX7(I)+VX8(I)
       VX2358=VX2(I)-VX3(I)-VX5(I)+VX8(I)
       VX1467=VX1(I)-VX4(I)-VX6(I)+VX7(I)
       VX1256=VX1(I)-VX2(I)-VX5(I)+VX6(I)
C
       VY3478=VY3(I)-VY4(I)-VY7(I)+VY8(I)
       VY2358=VY2(I)-VY3(I)-VY5(I)+VY8(I)
       VY1467=VY1(I)-VY4(I)-VY6(I)+VY7(I)
       VY1256=VY1(I)-VY2(I)-VY5(I)+VY6(I)
C
       VZ3478=VZ3(I)-VZ4(I)-VZ7(I)+VZ8(I)
       VZ2358=VZ2(I)-VZ3(I)-VZ5(I)+VZ8(I)
       VZ1467=VZ1(I)-VZ4(I)-VZ6(I)+VZ7(I)
       VZ1256=VZ1(I)-VZ2(I)-VZ5(I)+VZ6(I)
!
       HGX3(I)=(VX1467-VX2358)*ONE_OVER_8
       HGX1(I)=(VX1467+VX2358)*ONE_OVER_8
       HGX2(I)=(VX1256-VX3478)*ONE_OVER_8
       HGX4(I)=-(VX1256+VX3478)*ONE_OVER_8       
C
       HGY3(I)=(VY1467-VY2358)*ONE_OVER_8
       HGY1(I)=(VY1467+VY2358)*ONE_OVER_8
       HGY2(I)=(VY1256-VY3478)*ONE_OVER_8
       HGY4(I)=-(VY1256+VY3478)*ONE_OVER_8       
C
       HGZ3(I)=(VZ1467-VZ2358)*ONE_OVER_8
       HGZ1(I)=(VZ1467+VZ2358)*ONE_OVER_8
       HGZ2(I)=(VZ1256-VZ3478)*ONE_OVER_8
       HGZ4(I)=-(VZ1256+VZ3478)*ONE_OVER_8       
      ENDDO
      DO I=LFT,LLT
        VX17=VX1(I)-VX7(I)
        VX28=VX2(I)-VX8(I)
        VX35=VX3(I)-VX5(I)
        VX46=VX4(I)-VX6(I)
        VY17=VY1(I)-VY7(I)
        VY28=VY2(I)-VY8(I)
        VY35=VY3(I)-VY5(I)
        VY46=VY4(I)-VY6(I)
        VZ17=VZ1(I)-VZ7(I)
        VZ28=VZ2(I)-VZ8(I)
        VZ35=VZ3(I)-VZ5(I)
        VZ46=VZ4(I)-VZ6(I)
C   alpha =1 ->eta zeta   
C 1 1 -1 -1 -1 -1 1 1
        HGX1(I)= HGX1(I)
     &          -(PX1H1(I)*VX17+PX2H1(I)*VX28
     &            +PX3H1(I)*VX35+PX4H1(I)*VX46)
        HGY1(I)= HGY1(I)
     &          -(PX1H1(I)*VY17+PX2H1(I)*VY28
     &            +PX3H1(I)*VY35+PX4H1(I)*VY46)
        HGZ1(I)= HGZ1(I)
     &          -(PX1H1(I)*VZ17+PX2H1(I)*VZ28
     &            +PX3H1(I)*VZ35+PX4H1(I)*VZ46)
C
C   alpha =2 ->zeta ksi   
C 1 -1 -1 1 -1 1 1 -1
        HGX2(I)= HGX2(I)
     &          -(PX1H2(I)*VX17+PX2H2(I)*VX28
     &            +PX3H2(I)*VX35+PX4H2(I)*VX46)
        HGY2(I)= HGY2(I)
     &          -(PX1H2(I)*VY17+PX2H2(I)*VY28
     &            +PX3H2(I)*VY35+PX4H2(I)*VY46)
        HGZ2(I)= HGZ2(I)
     &          -(PX1H2(I)*VZ17+PX2H2(I)*VZ28
     &            +PX3H2(I)*VZ35+PX4H2(I)*VZ46)
C   alpha =3 ->ksi eta    
C 1 -1 1 -1 1 -1 1 -1
        HGX3(I)= HGX3(I)
     &          -(PX1H3(I)*VX17+PX2H3(I)*VX28
     &            +PX3H3(I)*VX35+PX4H3(I)*VX46)
        HGY3(I)= HGY3(I)
     &          -(PX1H3(I)*VY17+PX2H3(I)*VY28
     &            +PX3H3(I)*VY35+PX4H3(I)*VY46)
        HGZ3(I)= HGZ3(I)
     &          -(PX1H3(I)*VZ17+PX2H3(I)*VZ28
     &            +PX3H3(I)*VZ35+PX4H3(I)*VZ46)
C
C   alpha =4 ->ksi eta zeta
C -1 1 -1 1 1 -1 1 -1
        HGX4(I)= HGX4(I)
     &          -(PX1H4(I)*VX17+PX2H4(I)*VX28
     &            +PX3H4(I)*VX35+PX4H4(I)*VX46)
        HGY4(I)= HGY4(I)
     &          -(PX1H4(I)*VY17+PX2H4(I)*VY28
     &            +PX3H4(I)*VY35+PX4H4(I)*VY46)
        HGZ4(I)= HGZ4(I)
     &          -(PX1H4(I)*VZ17+PX2H4(I)*VZ28
     &            +PX3H4(I)*VZ35+PX4H4(I)*VZ46)
      ENDDO
!
      DO I=LFT,LLT
       JR_1(I) = ONE/MAX(EM20,JR0(I))
       JS_1(I) = ONE/MAX(EM20,JS0(I))
       JT_1(I) = ONE/MAX(EM20,JT0(I))
       H11(I) = JS0(I)*JT0(I)*JR_1(I)
       H22(I) = JR0(I)*JT0(I)*JS_1(I)
       H33(I) = JR0(I)*JS0(I)*JT_1(I)
       H12(I) = JT0(I)
       H13(I) = JS0(I)
       H23(I) = JR0(I)
C         SIGY(I) = 1.0E+30
      ENDDO
       DO I=LFT,LLT
        FHOUR(I,1,1) = FHOUR(I,1,1)*OFF(I)
        FHOUR(I,1,2) = FHOUR(I,1,2)*OFF(I)
        FHOUR(I,1,3) = FHOUR(I,1,3)*OFF(I)
        FHOUR(I,1,4) = FHOUR(I,1,4)*OFF(I)
        FHOUR(I,2,1) = FHOUR(I,2,1)*OFF(I)
        FHOUR(I,2,2) = FHOUR(I,2,2)*OFF(I)
        FHOUR(I,2,3) = FHOUR(I,2,3)*OFF(I)
        FHOUR(I,2,4) = FHOUR(I,2,4)*OFF(I)
        FHOUR(I,3,1) = FHOUR(I,3,1)*OFF(I)
        FHOUR(I,3,2) = FHOUR(I,3,2)*OFF(I)
        FHOUR(I,3,3) = FHOUR(I,3,3)*OFF(I)
        FHOUR(I,3,4) = FHOUR(I,3,4)*OFF(I)
       ENDDO
      IF (MATVIS==0) 
     .   CALL SZSVM(JR0,JS0,JT0,FHOUR,SIGY,SIGOLD,NU4,SMO1,SMO2,NEL) 
      !-----------For energy calculation------------
      IF(JLAG==1)THEN
       DO I=LFT,LLT
        FHOURT(1,1) = FHOUR(I,1,1)*JR0(I)+FCL(I)*HGX1(I)
        FHOURT(1,2) = FHOUR(I,1,2)*JR0(I)+FCL(I)*HGX2(I)
        FHOURT(1,3) = FHOUR(I,1,3)*JR0(I)+FCL(I)*HGX3(I)
        FHOURT(1,4) = FHOUR(I,1,4)*JR0(I)+FCL(I)*HGX4(I)
        FHOURT(2,1) = FHOUR(I,2,1)*JS0(I)+FCL(I)*HGY1(I)
        FHOURT(2,2) = FHOUR(I,2,2)*JS0(I)+FCL(I)*HGY2(I)
        FHOURT(2,3) = FHOUR(I,2,3)*JS0(I)+FCL(I)*HGY3(I)
        FHOURT(2,4) = FHOUR(I,2,4)*JS0(I)+FCL(I)*HGY4(I)
        FHOURT(3,1) = FHOUR(I,3,1)*JT0(I)+FCL(I)*HGZ1(I)
        FHOURT(3,2) = FHOUR(I,3,2)*JT0(I)+FCL(I)*HGZ2(I)
        FHOURT(3,3) = FHOUR(I,3,3)*JT0(I)+FCL(I)*HGZ3(I)
        FHOURT(3,4) = FHOUR(I,3,4)*JT0(I)+FCL(I)*HGZ4(I)
C
        NFHOUR(I,1,1) = (H22(I)+H33(I))*FHOURT(1,1)
     .                +H12(I)*FHOURT(2,2)+H13(I)*FHOURT(3,3)
        NFHOUR(I,2,2) = (H11(I)+H33(I))*FHOURT(2,2)
     .                +H23(I)*FHOURT(3,3)+H12(I)*FHOURT(1,1)
        NFHOUR(I,3,3) = (H11(I)+H22(I))*FHOURT(3,3)
     .                +H13(I)*FHOURT(1,1)+H23(I)*FHOURT(2,2)
        NFHOUR(I,1,2) = NU1(I)*H11(I)*FHOURT(1,2)
     .                 +NU2(I)*H12(I)*FHOURT(2,1)
        NFHOUR(I,1,3) = NU1(I)*H11(I)*FHOURT(1,3)
     .                 +NU2(I)*H13(I)*FHOURT(3,1)
        NFHOUR(I,2,1) = NU1(I)*H22(I)*FHOURT(2,1)
     .                 +NU2(I)*H12(I)*FHOURT(1,2)
        NFHOUR(I,3,1) = NU1(I)*H33(I)*FHOURT(3,1)
     .                 +NU2(I)*H13(I)*FHOURT(1,3)
C
        NFHOUR(I,2,3) = NU1(I)*H22(I)*FHOURT(2,3)
     .                 +NU2(I)*H23(I)*FHOURT(3,2)
        NFHOUR(I,3,2) = NU1(I)*H33(I)*FHOURT(3,2)
     .                 +NU2(I)*H23(I)*FHOURT(2,3)
        NFHOUR(I,1,4) = NU3(I)*H11(I)*FHOURT(1,4)
        NFHOUR(I,2,4) = NU3(I)*H22(I)*FHOURT(2,4)
        NFHOUR(I,3,4) = NU3(I)*H33(I)*FHOURT(3,4)
       ENDDO
C
       DO I=LFT,LLT
        DEINT(I)= 
     .   NFHOUR(I,3,1)*HGZ1(I) + NFHOUR(I,3,2)*HGZ2(I) + 
     .   NFHOUR(I,3,3)*HGZ3(I) + NFHOUR(I,3,4)*HGZ4(I) + 
     .   NFHOUR(I,1,1)*HGX1(I) + NFHOUR(I,1,2)*HGX2(I) + 
     .   NFHOUR(I,1,3)*HGX3(I) + NFHOUR(I,1,4)*HGX4(I) + 
     .   NFHOUR(I,2,1)*HGY1(I) + NFHOUR(I,2,2)*HGY2(I) + 
     .   NFHOUR(I,2,3)*HGY3(I) + NFHOUR(I,2,4)*HGY4(I) 
        EINT(I)= EINT(I)+DT05*DEINT(I)/MAX(EM20,VOL0(I))
       ENDDO
      ENDIF
C      
      IF (IET > 1 .AND. MTN == 24 ) THEN
       CALL MDAMA24(ELBUF_STR,LFT,LLT ,PM    ,MAT    ,DAMA_G )
       DO J=1,3
        DO I=LFT,LLT
         FAC1=ONE- DAMA_G(I,J)
c        FAC1=ONE - MAX(ZERO,MIN(ONE,D_MAX(I)))        !in [0 1]
         FHOUR(I,J,1:4) = FHOUR(I,J,1:4)*FAC1
        ENDDO
       ENDDO
      END IF !(IET > 1) THEN
C   -------elstic increament----
      DO I=LFT,LLT
       E_R =G_3DT(I)*JR_1(I)
       E_S =G_3DT(I)*JS_1(I)
       E_T =G_3DT(I)*JT_1(I)
       DFHOUR(I,1,1) = E_R*HGX1(I)
       DFHOUR(I,1,2) = E_R*HGX2(I)
       DFHOUR(I,1,3) = E_R*HGX3(I)
       DFHOUR(I,1,4) = E_R*HGX4(I)
       DFHOUR(I,2,1) = E_S*HGY1(I)
       DFHOUR(I,2,2) = E_S*HGY2(I)
       DFHOUR(I,2,3) = E_S*HGY3(I)
       DFHOUR(I,2,4) = E_S*HGY4(I)
       DFHOUR(I,3,1) = E_T*HGZ1(I)
       DFHOUR(I,3,2) = E_T*HGZ2(I)
       DFHOUR(I,3,3) = E_T*HGZ3(I)
       DFHOUR(I,3,4) = E_T*HGZ4(I)
C
       FHOUR(I,1,1) = FHOUR(I,1,1) + DFHOUR(I,1,1)
       FHOUR(I,1,2) = FHOUR(I,1,2) + DFHOUR(I,1,2)
       FHOUR(I,1,3) = FHOUR(I,1,3) + DFHOUR(I,1,3)
       FHOUR(I,1,4) = FHOUR(I,1,4) + DFHOUR(I,1,4)
       FHOUR(I,2,1) = FHOUR(I,2,1) + DFHOUR(I,2,1)
       FHOUR(I,2,2) = FHOUR(I,2,2) + DFHOUR(I,2,2)
       FHOUR(I,2,3) = FHOUR(I,2,3) + DFHOUR(I,2,3)
       FHOUR(I,2,4) = FHOUR(I,2,4) + DFHOUR(I,2,4)
       FHOUR(I,3,1) = FHOUR(I,3,1) + DFHOUR(I,3,1)
       FHOUR(I,3,2) = FHOUR(I,3,2) + DFHOUR(I,3,2)
       FHOUR(I,3,3) = FHOUR(I,3,3) + DFHOUR(I,3,3)
       FHOUR(I,3,4) = FHOUR(I,3,4) + DFHOUR(I,3,4)
      ENDDO
      IF (MATVIS==0) 
     .    CALL SZSVM(JR0,JS0,JT0,FHOUR,SIGY,SIG0,NU4,SM1,SM2,NEL) 
      !-----------ELASTIC-PLASTIC yield criterion------------
      IF (MATVIS==0) THEN
      DO I=LFT,LLT
        IF (SM1(I)>SIGY(I).AND.DEINT(I)>0) THEN
         SMO  = ZEP9*SMO1(I)+EM01*SMO2(I)
         FAC1 = SIGY(I)-SMO
         FAC2 = SM1(I)-SMO
         IF (FAC2<=EM20) THEN
           FAC=ZERO
         ELSE
           FAC = ONE - MAX(EM20,FAC1/FAC2)
         ENDIF
         IF (SM2(I)<SIGY(I)) THEN
           FAC1 =(SM1(I)-SIGY(I))/MAX((SM1(I)-SM2(I)),EM20)
           FAC1 =HALF + SQRT(FAC1)
           FAC = MIN(FAC1,ONE)*FAC
         ENDIF  
         FHOUR(I,1,1) = FHOUR(I,1,1) - FAC*DFHOUR(I,1,1)
         FHOUR(I,1,2) = FHOUR(I,1,2) - FAC*DFHOUR(I,1,2)
         FHOUR(I,1,3) = FHOUR(I,1,3) - FAC*DFHOUR(I,1,3)
         FHOUR(I,1,4) = FHOUR(I,1,4) - FAC*DFHOUR(I,1,4)
         FHOUR(I,2,1) = FHOUR(I,2,1) - FAC*DFHOUR(I,2,1)
         FHOUR(I,2,2) = FHOUR(I,2,2) - FAC*DFHOUR(I,2,2)
         FHOUR(I,2,3) = FHOUR(I,2,3) - FAC*DFHOUR(I,2,3)
         FHOUR(I,2,4) = FHOUR(I,2,4) - FAC*DFHOUR(I,2,4)
         FHOUR(I,3,1) = FHOUR(I,3,1) - FAC*DFHOUR(I,3,1)
         FHOUR(I,3,2) = FHOUR(I,3,2) - FAC*DFHOUR(I,3,2)
         FHOUR(I,3,3) = FHOUR(I,3,3) - FAC*DFHOUR(I,3,3)
         FHOUR(I,3,4) = FHOUR(I,3,4) - FAC*DFHOUR(I,3,4)
        ENDIF
      ENDDO
      ENDIF
      DO I=LFT,LLT
       FHOURT(1,1) = FHOUR(I,1,1)*JR0(I)+FCL(I)*HGX1(I)
       FHOURT(1,2) = FHOUR(I,1,2)*JR0(I)+FCL(I)*HGX2(I)
       FHOURT(1,3) = FHOUR(I,1,3)*JR0(I)+FCL(I)*HGX3(I)
       FHOURT(1,4) = FHOUR(I,1,4)*JR0(I)+FCL(I)*HGX4(I)
       FHOURT(2,1) = FHOUR(I,2,1)*JS0(I)+FCL(I)*HGY1(I)
       FHOURT(2,2) = FHOUR(I,2,2)*JS0(I)+FCL(I)*HGY2(I)
       FHOURT(2,3) = FHOUR(I,2,3)*JS0(I)+FCL(I)*HGY3(I)
       FHOURT(2,4) = FHOUR(I,2,4)*JS0(I)+FCL(I)*HGY4(I)
       FHOURT(3,1) = FHOUR(I,3,1)*JT0(I)+FCL(I)*HGZ1(I)
       FHOURT(3,2) = FHOUR(I,3,2)*JT0(I)+FCL(I)*HGZ2(I)
       FHOURT(3,3) = FHOUR(I,3,3)*JT0(I)+FCL(I)*HGZ3(I)
       FHOURT(3,4) = FHOUR(I,3,4)*JT0(I)+FCL(I)*HGZ4(I)
C
       NFHOUR(I,1,1) = (H22(I)+H33(I))*FHOURT(1,1)
     .                +H12(I)*FHOURT(2,2)+H13(I)*FHOURT(3,3)
       NFHOUR(I,2,2) = (H11(I)+H33(I))*FHOURT(2,2)
     .                +H23(I)*FHOURT(3,3)+H12(I)*FHOURT(1,1)
       NFHOUR(I,3,3) = (H11(I)+H22(I))*FHOURT(3,3)
     .                +H13(I)*FHOURT(1,1)+H23(I)*FHOURT(2,2)
        NFHOUR(I,1,2) = NU1(I)*H11(I)*FHOURT(1,2)
     .                 +NU2(I)*H12(I)*FHOURT(2,1)
        NFHOUR(I,1,3) = NU1(I)*H11(I)*FHOURT(1,3)
     .                 +NU2(I)*H13(I)*FHOURT(3,1)
        NFHOUR(I,2,1) = NU1(I)*H22(I)*FHOURT(2,1)
     .                 +NU2(I)*H12(I)*FHOURT(1,2)
        NFHOUR(I,3,1) = NU1(I)*H33(I)*FHOURT(3,1)
     .                 +NU2(I)*H13(I)*FHOURT(1,3)
        NFHOUR(I,2,3) = NU1(I)*H22(I)*FHOURT(2,3)
     .                 +NU2(I)*H23(I)*FHOURT(3,2)
        NFHOUR(I,3,2) = NU1(I)*H33(I)*FHOURT(3,2)
     .                 +NU2(I)*H23(I)*FHOURT(2,3)
        NFHOUR(I,1,4) = NU3(I)*H11(I)*FHOURT(1,4)
        NFHOUR(I,2,4) = NU3(I)*H22(I)*FHOURT(2,4)
        NFHOUR(I,3,4) = NU3(I)*H33(I)*FHOURT(3,4)
      ENDDO
      DO I=LFT,LLT
        HQ13P = (NFHOUR(I,1,1)+NFHOUR(I,1,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,1,1)-NFHOUR(I,1,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,1,2)+NFHOUR(I,1,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,1,2)-NFHOUR(I,1,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,1,1)-PX1H2(I)*NFHOUR(I,1,2)
     .      -PX1H3(I)*NFHOUR(I,1,3)-PX1H4(I)*NFHOUR(I,1,4)
        F11(I) =-(HQ13P+HQ24N+FF)
        F17(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,1,1)-PX2H2(I)*NFHOUR(I,1,2)
     .      -PX2H3(I)*NFHOUR(I,1,3)-PX2H4(I)*NFHOUR(I,1,4)
        F12(I) =-(HQ13N-HQ24N+FF)
        F18(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,1,1)-PX3H2(I)*NFHOUR(I,1,2)
     .      -PX3H3(I)*NFHOUR(I,1,3)-PX3H4(I)*NFHOUR(I,1,4)
        F13(I) =-(-HQ13N-HQ24P+FF)
        F15(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,1,1)-PX4H2(I)*NFHOUR(I,1,2)
     .      -PX4H3(I)*NFHOUR(I,1,3)-PX4H4(I)*NFHOUR(I,1,4)
        F14(I) =-(-HQ13P+HQ24P+FF)
        F16(I) =-(-HQ13P+HQ24N-FF)
      ENDDO
      DO I=LFT,LLT
        HQ13P = (NFHOUR(I,2,1)+NFHOUR(I,2,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,2,1)-NFHOUR(I,2,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,2,2)+NFHOUR(I,2,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,2,2)-NFHOUR(I,2,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,2,1)-PX1H2(I)*NFHOUR(I,2,2)
     .      -PX1H3(I)*NFHOUR(I,2,3)-PX1H4(I)*NFHOUR(I,2,4)
        F21(I) =-(HQ13P+HQ24N+FF)
        F27(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,2,1)-PX2H2(I)*NFHOUR(I,2,2)
     .      -PX2H3(I)*NFHOUR(I,2,3)-PX2H4(I)*NFHOUR(I,2,4)
        F22(I) =-(HQ13N-HQ24N+FF)
        F28(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,2,1)-PX3H2(I)*NFHOUR(I,2,2)
     .      -PX3H3(I)*NFHOUR(I,2,3)-PX3H4(I)*NFHOUR(I,2,4)
        F23(I) =-(-HQ13N-HQ24P+FF)
        F25(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,2,1)-PX4H2(I)*NFHOUR(I,2,2)
     .      -PX4H3(I)*NFHOUR(I,2,3)-PX4H4(I)*NFHOUR(I,2,4)
        F24(I) =-(-HQ13P+HQ24P+FF)
        F26(I) =-(-HQ13P+HQ24N-FF)
      ENDDO
      DO I=LFT,LLT
        HQ13P = (NFHOUR(I,3,1)+NFHOUR(I,3,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,3,1)-NFHOUR(I,3,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,3,2)+NFHOUR(I,3,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,3,2)-NFHOUR(I,3,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,3,1)-PX1H2(I)*NFHOUR(I,3,2)
     .      -PX1H3(I)*NFHOUR(I,3,3)-PX1H4(I)*NFHOUR(I,3,4)
        F31(I) =-(HQ13P+HQ24N+FF)
        F37(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,3,1)-PX2H2(I)*NFHOUR(I,3,2)
     .      -PX2H3(I)*NFHOUR(I,3,3)-PX2H4(I)*NFHOUR(I,3,4)
        F32(I) =-(HQ13N-HQ24N+FF)
        F38(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,3,1)-PX3H2(I)*NFHOUR(I,3,2)
     .      -PX3H3(I)*NFHOUR(I,3,3)-PX3H4(I)*NFHOUR(I,3,4)
        F33(I) =-(-HQ13N-HQ24P+FF)
        F35(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,3,1)-PX4H2(I)*NFHOUR(I,3,2)
     .      -PX4H3(I)*NFHOUR(I,3,3)-PX4H4(I)*NFHOUR(I,3,4)
        F34(I) =-(-HQ13P+HQ24P+FF)
        F36(I) =-(-HQ13P+HQ24N-FF)
      ENDDO
      !----hourglass energie est inclued in interal energie------
      IF(JLAG==1)THEN
       DO I=LFT,LLT
        EINT(I)= EINT(I)+DT05*(
     .   NFHOUR(I,3,1)*HGZ1(I) + NFHOUR(I,3,2)*HGZ2(I) + 
     .   NFHOUR(I,3,3)*HGZ3(I) + NFHOUR(I,3,4)*HGZ4(I) + 
     .   NFHOUR(I,1,1)*HGX1(I) + NFHOUR(I,1,2)*HGX2(I) + 
     .   NFHOUR(I,1,3)*HGX3(I) + NFHOUR(I,1,4)*HGX4(I) + 
     .   NFHOUR(I,2,1)*HGY1(I) + NFHOUR(I,2,2)*HGY2(I) + 
     .   NFHOUR(I,2,3)*HGY3(I) + NFHOUR(I,2,4)*HGY4(I) ) 
     .   /MAX(EM20,VOL0(I))
       ENDDO
      ENDIF
      IF(ISTRAIN>0 .AND.  
     .  ((ANIM_N(IAD_GPS+400+1) == 1) .OR. (ANIM_N(IAD_GPS+400+2) == 1) .OR. 
     .   (ANIM_N(IAD_GPS+400+3) == 1) .OR. (ANIM_N(IAD_GPS+400+4) == 1) .OR. 
     .   (ANIM_N(IAD_GPS+400+5) == 1) .OR. (ANIM_N(IAD_GPS+400+6) == 1)) )THEN
       DO I=LFT,LLT
        NU2(I) =HALF*NU2(I)
       ENDDO
       CALL SZSTRAINHG(JR_1,JS_1,JT_1,STRHG,NEL  ,
     .                      HGX1, HGX2, HGX3, HGX4,
     .                      HGY1, HGY2, HGY3, HGY4,
     .                      HGZ1, HGZ2, HGZ3, HGZ4,NU4,NU2)
      ENDIF

      RETURN
      END

Chd|====================================================================
Chd|  SZSVM                         source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSVM(JR0,JS0,JT0,FHOUR,SIGY,SIG0,NU,SVM1,SVM2,NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIGY(*) ,SIG0(NEL,6),SVM1(*),SVM2(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,K,IKT
      my_real
     .   S1,S2,S3,SVM0,SR1,SR2,SR3,SR4,
     .   SS1,SS2,SS3,SS4,ST1,ST2,ST3,ST4,SVMR,SVMS,SVMT,
     .   SVM,SVMRST,COEF,COEF1, JR_1,JS_1,JT_1,NU1,NU2,
     .   RS,ST,RT,VT1(4),VT2(4),MAX1,MIN1,MAX2,MIN2,MAX0,MIN0    
C         coef= (3*8*0.75)^2 coef1 =1/0.75
          COEF= THREEHUNDRED24
          COEF1= ONEP333
C-----------------------------------------------
      IKT = IINT
      IF (IKT ==2) THEN
        COEF1= COEF/(TEN*IKT)
        COEF= MAX(COEF1,EM01)
      ELSEIF (IKT > 2) THEN
        COEF1= COEF/(TEN*IKT)
        COEF= MAX(COEF1,EM01)
      ENDIF
      DO I=LFT,LLT
       SVM1(I) = ZERO
       SVM2(I) = ZERO
       IF (SIGY(I)<ZEP9EP30) THEN
        S1 =SIG0(I,1)-SIG0(I,2)
        S2 =SIG0(I,2)-SIG0(I,3)
        S3 =SIG0(I,1)-SIG0(I,3)
        SVM0 = (S1*S1+S2*S2+S3*S3)*HALF + THREE*(SIG0(I,4)*SIG0(I,4)
     .                +SIG0(I,5)*SIG0(I,5)+SIG0(I,6)*SIG0(I,6))
        NU1 =ONE/(ONE - NU(I))
        NU2 =NU(I)*NU1
        JR_1 = ONE/MAX(EM20,JR0(I))
        JS_1 = ONE/MAX(EM20,JS0(I))
        JT_1 = ONE/MAX(EM20,JT0(I))
        SR1 =NU1*FHOUR(I,1,3)+NU2*FHOUR(I,3,1)
        SR2 =NU1*FHOUR(I,3,1)+NU2*FHOUR(I,1,3)
        SR3 =FHOUR(I,1,3)-FHOUR(I,3,1)
        SR4 =JT0(I)*JR_1*FHOUR(I,3,3)+JR0(I)*JT_1*FHOUR(I,1,1)
        SVMR = TWO*(SR1*SR1+SR2*SR2+SR3*SR3)+3*SR4*SR4
        SS1 =FHOUR(I,1,2)-FHOUR(I,2,1)
        SS2 =NU1*FHOUR(I,2,1)+NU2*FHOUR(I,1,2)
        SS3 =NU1*FHOUR(I,1,2)+NU2*FHOUR(I,2,1)
        SS4 =JS0(I)*JR_1*FHOUR(I,2,2)+JR0(I)*JS_1*FHOUR(I,1,1)
        SVMS = TWO*(SS1*SS1+SS2*SS2+SS3*SS3) + THREE*SS4*SS4
        ST1 =NU1*FHOUR(I,2,3)+NU2*FHOUR(I,3,2)
        ST2 =FHOUR(I,2,3)-FHOUR(I,3,2)
        ST3 =NU1*FHOUR(I,3,2)+NU2*FHOUR(I,2,3)
        ST4 =JS0(I)*JT_1*FHOUR(I,2,2)+JT0(I)*JS_1*FHOUR(I,3,3)
        SVMT = TWO*(ST1*ST1+ST2*ST2+ST3*ST3) + THREE*ST4*ST4
C
        RS =SR1*SS1
        RT =SR1*ST1
        ST =SS1*ST1
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        RS =SR2*SS2
        RT =SR2*ST2
        ST =SS2*ST2
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX0+MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN0+MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        RS =SR3*SS3
        RT =SR3*ST3
        ST =SS3*ST3
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX0+MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN0+MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        SVMRST = SVM0+COEF*(SVMR+SVMS+SVMT)
        SVM1(I) = SQRT(ABS(SVMRST+COEF*MAX0))
        SVM2(I) = SQRT(MAX(SVMRST+COEF*MIN0,ZERO))
        SVM2(I) = MIN(SVM2(I),SVM0)
       ENDIF
      ENDDO 
C
      RETURN
      END

Chd|====================================================================
Chd|  SZETFAC                       source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZETFAC(LFT,LLT,IKT,MTN,ET,G  )
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
#include      "units_c.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,IKT,MTN
      my_real
     .   ET(*),G(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,K
      my_real
     .   FAC_L,FAC_U,FAC(MVSIZ),ETE,FAC_MIN,FAC_MAX
C-------IKT=2 for the moment----------
      FAC_L=FIVEEM2
      DO I=LFT,LLT
        FAC(I) = ONE
      END DO

      IF (IKT == 2 ) THEN
       SELECT CASE (MTN)
        CASE (2,36) 
        
        CASE (42,62,69,82,88,92,94,95,111)                                          
c          FAC_MIN=EP10
c          FAC_MAX=ZERO
         DO I=LFT,LLT
          IF (ET(I)<=ONE) THEN
           FAC(I)=MAX(FAC_L,ET(I))
          ELSE
C
            FAC(I)= ZEP2+ET(I)
          END IF
c           IF (FAC_MIN>FAC(I)) FAC_MIN=FAC(I)
c           IF (FAC_MAX<FAC(I)) FAC_MAX=FAC(I)
         END DO           
        CASE(38,70,90)
          FAC_L=EM02
c          FAC_MIN=EP10
c          FAC_MAX=ZERO
         DO I=LFT,LLT
          IF (ET(I) <= ONE) THEN
           FAC(I)=MAX(FAC_L,ET(I))
          ELSE
C-----    -G(i) is already calculated with EMAX          
           FAC(I)=ONE
          END IF         
c           IF (FAC_MIN>FAC(I)) FAC_MIN=FAC(I)
c           IF (FAC_MAX<FAC(I)) FAC_MAX=FAC(I)
         END DO 
c          write(iout,*),'FAC_L,FAC(1),ET(1)=',FAC_L,FAC(1),ET(1)
        CASE (71) 
         FAC_L=EM02
         DO I=LFT,LLT
          IF (ET(I)<=ONE) THEN
           FAC(I)=MAX(FAC_L,ET(I))
          END IF
         END DO 
       END SELECT
C-------IKT>2 hide option :limited by IKT ----------
      ELSE
C---------    
        FAC_U=ONE*IKT
        SELECT CASE (MTN)
         CASE (2,36) 
        
         CASE (42,62,69,82,88,92,94,111)                                          
          DO I=LFT,LLT
           IF (ET(I)<=ONE) THEN
            FAC(I)=MAX(FAC_L,ET(I))
           ELSE
            FAC(I)=MIN(FAC_U,ET(I))
           END IF
          ENDDO           
        CASE(38,70,90)  
          FAC_L=FAC_L/IKT
          DO I=LFT,LLT
           IF (ET(I)<=ONE) THEN
            FAC(I)=MAX(FAC_L,ET(I))
           ELSE
            FAC(I)=MIN(FAC_U,ET(I))
           END IF
          ENDDO 
        END SELECT
      END IF !(IKT ==2 THEN
C       
        DO I=LFT,LLT
         G(I)=FAC(I)*G(I)
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SZSIGPARA                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        STRS_TENSCOR3                 source/output/h3d/h3d_results/strs_tenscor3.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS_SKIN                  source/output/anim/generate/tensor6.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSIGPARA(JR0  ,JS0  ,JT0  ,FHOUR  ,SIG0 ,
     .                     SIG  ,KSI  ,ETA  ,ZETA   ,NU   ,
     .                     NEL  ,I  )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,I
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIG(MVSIZ,6) ,SIG0(NEL,6),NU(*),KSI  ,ETA  ,ZETA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   J
      my_real
     .   COEF,NU1,NU2,NU3,SXX,SYY,SZZ,SXY,SXZ,SYZ
C---JR0=-X1+X2+X3-X4-X5+X6+X7-X8 (coordon  s locaux)     
C---JS0=-Y1-Y2+Y3+Y4-Y5-Y6+Y7+Y8     
C---JT0=-Z1-Z2-Z3-Z4+Z5+Z6+Z7+Z8
C---NU : Poisson's Coef     
C-----------------------------------------------
        COEF=TWENTY4
c        DO I=LFT,LLT
         NU1=ONE/(ONE-NU(I))
         NU2=NU(I)*NU1
         NU3=ONE+NU(I)
         SXX=NU1*(FHOUR(I,1,2)*ZETA+FHOUR(I,1,3)*ETA)+
     .       NU2*(FHOUR(I,2,1)*ZETA+FHOUR(I,3,1)*ETA)+         
     .       NU3*FHOUR(I,1,4)*ZETA*ETA         
         SYY=NU1*(FHOUR(I,2,1)*ZETA+FHOUR(I,2,3)*KSI)+
     .       NU2*(FHOUR(I,1,2)*ZETA+FHOUR(I,3,2)*KSI)+         
     .       NU3*FHOUR(I,2,4)*ZETA*KSI         
         SZZ=NU1*(FHOUR(I,3,1)*ETA+FHOUR(I,3,2)*KSI)+
     .       NU2*(FHOUR(I,1,3)*ETA+FHOUR(I,2,3)*KSI)+         
     .       NU3*FHOUR(I,3,4)*ETA*KSI         
         SIG(I,1)=SIG0(I,1)+TWO*SXX*TWENTY4
         SIG(I,2)=SIG0(I,2)+TWO*SYY*TWENTY4
         SIG(I,3)=SIG0(I,3)+TWO*SZZ*TWENTY4
c        ENDDO
C        
c        DO I=LFT,LLT
         SXY=ZETA*(JR0(I)*FHOUR(I,1,1)/JS0(I)+
     .             JS0(I)*FHOUR(I,2,2)/JR0(I))         
         SXZ=ETA*(JR0(I)*FHOUR(I,1,1)/JT0(I)+
     .            JT0(I)*FHOUR(I,3,3)/JR0(I))
         SYZ=KSI*(JS0(I)*FHOUR(I,2,2)/JT0(I)+
     .            JT0(I)*FHOUR(I,3,3)/JS0(I))
         SIG(I,4)=SIG0(I,4)+SXY*TWENTY4         
         SIG(I,5)=SIG0(I,5)+SYZ*TWENTY4
         SIG(I,6)=SIG0(I,6)+SXZ*TWENTY4
c        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        GFHOUR_OR                     source/elements/solid/solidez/szhour3.F
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|        MMODUL                        source/elements/solid/solidez/szhour3.F
Chd|        MMOD_NORM                     source/elements/solid/solidez/szhour3.F
Chd|        SZETFAC                       source/elements/solid/solidez/szhour3.F
Chd|        SZSTRAINHG                    source/elements/solid/solidez/szhour3.F
Chd|        SZSVM_OR                      source/elements/solid/solidez/szhour3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SZHOUR3_OR(ELBUF_STR,PM,RHO,OFF,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   PX1H1, PX1H2, PX1H3, PX1H4, 
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4, 
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   VOL,MAT,CXX,VIS,VD2,DELTAX,PID,GEO,PARTSAV,IPARTS,
     .   DXX, DYY, DZZ, D4, D5, D6 ,
     .   FHOUR,JR0,JS0,JT0,EINT,VOL0,SIGY,SIG0,SIGOLD,ICP,DEFP,
     .   MATVIS,ET  ,D_MAX,NEL,GAMA  ,UPARAM,STRHG,STRAIN,ISTRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "cong1_c.inc"
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scrupw_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,ISTRAIN
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*), RHO(*),OFF(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   PX1H1(*), PX1H2(*), PX1H3(*), PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*), PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*), PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*), PX4H4(*),
     .   PARTSAV(NPSAV,*),
     .   VOL(*),CXX(*),VIS(*),VD2(*),DELTAX(*), 
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,EINT(*),
     .   DXX(*), DYY(*), DZZ(*), D4(*), D5(*), D6(*) ,
     .   SIGY(*) ,SIG0(NEL,6),VOL0(*),SIGOLD(NEL,6),DEFP(*),ET(*),
     .   D_MAX(*),STRHG(NEL,18),STRAIN(NEL,6)
      my_real
     .   UPARAM(*),GAMA(MVSIZ,6)
      INTEGER MAT(*),PID(*),IPARTS(*),ICP,MATVIS
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,K,IET, MT
      my_real
     .   CAQ(MVSIZ), FCL(MVSIZ), FCQ(MVSIZ),DEINT(MVSIZ),
     .   H11(MVSIZ), H22(MVSIZ), H33(MVSIZ),
     .   H12(MVSIZ), H13(MVSIZ), H23(MVSIZ),
     .   HGX1(MVSIZ), HGX2(MVSIZ), HGX3(MVSIZ), HGX4(MVSIZ),
     .   HGY1(MVSIZ), HGY2(MVSIZ), HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ1(MVSIZ), HGZ2(MVSIZ), HGZ3(MVSIZ), HGZ4(MVSIZ),
     .   VX3478, VX2358, VX1467, VX1256, 
     .   VY3478, VY2358, VY1467, VY1256,
     .   VZ3478, VZ2358, VZ1467, VZ1256,
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46, 
     .   G_3DT(MVSIZ),NU,GG(MVSIZ),DE,DS,DSIG(6),
     .   SM1(MVSIZ),SM2(MVSIZ),SMO1(MVSIZ),SMO2(MVSIZ),SMO,
     .   JR_1(MVSIZ),JS_1(MVSIZ),JT_1(MVSIZ),NFHOUR(MVSIZ,3,4),
     .   DFHOUR(MVSIZ,3,4),FHOURT(3,4),DT05,RHO0,ETMAX,
     .   NUS(MVSIZ),NU2(MVSIZ),NU4(MVSIZ),NEP,E0(MVSIZ),
     .   E_R,E_S,E_T,FAC,FAC1,FAC2,COEFH,HQ13P,HQ13N,HQ24P,HQ24N,FF,
     .   SIG0V(MVSIZ,6)
      my_real
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3),GM,GMIN,DAMA_G(MVSIZ,3)
C-----------------------------------------------
       IET =IINT
       COEFH = ZEP9999   
C---- attention : r->eta; s->zeta; t->ksi------------

C +++ MAT Visco-----
!      DO I=LFT,LLT
!        SIG0V(I,1) = SIG0(I,1)
!        SIG0V(I,2) = SIG0(I,2)
!        SIG0V(I,3) = SIG0(I,3)
!        SIG0V(I,4) = SIG0(I,4)
!        SIG0V(I,5) = SIG0(I,5)
!        SIG0V(I,6) = SIG0(I,6)
!      ENDDO
      MX = MAT(LFT)
      RHO0=PM(1,MX)
      NU=PM(21,MX)
      CALL MMODUL(LFT     ,LLT     ,PM    ,MAT    ,MTN    ,
     .            GAMA    ,UPARAM  ,CC    ,CG     ,G33    )
      DO I=LFT,LLT
       GM = THIRD*(G33(I,1,1)+G33(I,2,2)+G33(I,3,3))
       GG(I)=HALF*RHO0*CXX(I)*CXX(I)*(ONE -TWO*NU)/(ONE-NU)
C----- sometimes GM is too small       
       GMIN = GG(I)*EM04
       GG(I)=MIN(GG(I),GM)       
       GG(I)=MAX(GG(I),GMIN)   
       E0(I)=TWO*(ONE+NU)*GG(I)
      ENDDO
C  +++ MAT Hydro---
c      IF (MTN==3 .OR.MTN==4 .OR.MTN==6) THEN
c       MX = MAT(LFT)
c       DO I=LFT,LLT
c        GG(I)=PM(22,MX)
c        GG(I)=MAX(GG(I),VIS(I))
c       ENDDO
c      ENDIF 
C
      IF (IET > 1 .AND. MATVIS>0 ) THEN
       CALL SZETFAC(LFT,LLT,IET,MTN,ET,GG )
      ELSEIF (MATVIS==1.AND.ISMSTR<10) THEN
       DO I=LFT,LLT
        FF=THIRD*(DXX(I)+DYY(I)+DZZ(I))
        DE =(DXX(I)-FF)*(DXX(I)-FF)+(DYY(I)-FF)*(DYY(I)-FF)+
     .      (DZZ(I)-FF)*(DZZ(I)-FF) + FOURTH*(D4(I)*D4(I)+
     .                               D5(I)*D5(I)+D6(I)*D6(I))
         DE = DE*DT1
         DSIG(1)=SIG0(I,1)-SIGOLD(I,1)
         DSIG(2)=SIG0(I,2)-SIGOLD(I,2)
         DSIG(3)=SIG0(I,3)-SIGOLD(I,3)
         DSIG(4)=SIG0(I,4)-SIGOLD(I,4)
         DSIG(5)=SIG0(I,5)-SIGOLD(I,5)
         DSIG(6)=SIG0(I,6)-SIGOLD(I,6)
         FF= THIRD*(DSIG(1)+DSIG(2)+DSIG(3))
         DSIG(1)=DSIG(1)-FF
         DSIG(2)=DSIG(2)-FF
         DSIG(3)=DSIG(3)-FF
         DS =DSIG(1)*DSIG(1)+DSIG(2)*DSIG(2)+DSIG(3)*DSIG(3)+
     .       DSIG(4)*DSIG(4)+DSIG(5)*DSIG(5)+DSIG(6)*DSIG(6) 
         GG(I)=MAX(FIVEEM2*GG(I),SQRT(DS/MAX(DE,EM30)))
       ENDDO
      ENDIF
C
      IF(INVSTR>=35)THEN
        MT = PID(LFT)
        DO I=LFT,LLT
          CAQ(I)=FOURTH*OFF(I)*GEO(13,MT)
        ENDDO
      ELSE
        MX = MAT(LFT)
        DO I=LFT,LLT
          CAQ(I)=FOURTH*OFF(I)*PM(4,MX)
        ENDDO
      ENDIF
      DO I=LFT,LLT
        G_3DT(I)=THIRD*OFF(I)*GG(I)*DT1
      ENDDO
C---------normalized by GG-------      
      CALL MMOD_NORM(LFT,LLT  ,GG    ,CC     ,CG    ,G33     )
C
C MATERIAUX NON FLUIDES CXX->SPP
      IF (IET > 1 ) THEN
C------now when IHKT=2, w.r.t. Isolid=1, fac=0.2*0.006666 ~0.001     
       DO I=LFT,LLT
         FCL(I)=ONEP1*CAQ(I)*RHO(I)*VOL(I)**THIRD
         FCL(I)=ZEP00666666667*FCL(I)*CXX(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         FCL(I)=CAQ(I)*RHO(I)*VOL(I)**THIRD
         FCL(I)=ZEP00666666667*FCL(I)*CXX(I)
       ENDDO
      END IF
C
c      IF(ICP==1)THEN
C-------!!!curieux!!!!!!!!
c       DO I=LFT,LLT
c        NU1(I) =FOUR_OVER_3
c        NU2(I) =-TWO_THIRD
c        NU3(I) =ZEP444
c        NU4(I) =ZERO
c       ENDDO
c      ELSEIF(ICP==2)THEN
c       DO I=LFT,LLT
c        FAC1 = SIGY(I)/E0(I)+DEFP(I)
c        FAC2=ONE-DEFP(I)/FAC1
c        FF =(ONE +NU)/(ONE -TWO*NU)*FAC2
c        NU1(I) =TWO_THIRD*(TWO+FF)
c        NU2(I) =TWO_THIRD*(FF-ONE)
c        NU3(I) =ZEP222*(TWO+FF)
c        NU4(I) =ZERO
c       ENDDO
c      ELSE
c       DO I=LFT,LLT
c        NU1(I) =TWO/(ONE-NU)
c        NU2(I) =NU*NU1(I)
c        NU3(I) =TWO_THIRD*(ONE + NU)
c        NU4(I) =NU
c       ENDDO
c      ENDIF
      IF(ICP==1)THEN
       DO I=LFT,LLT
        NUS(I) =ZEP499
       ENDDO
      ELSEIF(ICP==2)THEN
       DO I=LFT,LLT
        FAC1 = SIGY(I)/E0(I)+DEFP(I)
        FAC2 = DEFP(I)/FAC1
        NUS(I)=NU+(HALF-NU)*FAC2
       ENDDO
      ELSE
       DO I=LFT,LLT
        NUS(I) =NU
       ENDDO
      ENDIF
      DO I=LFT,LLT
       NU2(I) =NUS(I)/(ONE-NUS(I))
       NU4(I) =NUS(I)
      ENDDO
      DT05 =HALF*DT1
      DO I=LFT,LLT
       VX3478=VX3(I)-VX4(I)-VX7(I)+VX8(I)
       VX2358=VX2(I)-VX3(I)-VX5(I)+VX8(I)
       VX1467=VX1(I)-VX4(I)-VX6(I)+VX7(I)
       VX1256=VX1(I)-VX2(I)-VX5(I)+VX6(I)
C
       VY3478=VY3(I)-VY4(I)-VY7(I)+VY8(I)
       VY2358=VY2(I)-VY3(I)-VY5(I)+VY8(I)
       VY1467=VY1(I)-VY4(I)-VY6(I)+VY7(I)
       VY1256=VY1(I)-VY2(I)-VY5(I)+VY6(I)
C
       VZ3478=VZ3(I)-VZ4(I)-VZ7(I)+VZ8(I)
       VZ2358=VZ2(I)-VZ3(I)-VZ5(I)+VZ8(I)
       VZ1467=VZ1(I)-VZ4(I)-VZ6(I)+VZ7(I)
       VZ1256=VZ1(I)-VZ2(I)-VZ5(I)+VZ6(I)
!
       HGX3(I)=(VX1467-VX2358)*ONE_OVER_8
       HGX1(I)=(VX1467+VX2358)*ONE_OVER_8
       HGX2(I)=(VX1256-VX3478)*ONE_OVER_8
       HGX4(I)=-(VX1256+VX3478)*ONE_OVER_8       
C
       HGY3(I)=(VY1467-VY2358)*ONE_OVER_8
       HGY1(I)=(VY1467+VY2358)*ONE_OVER_8
       HGY2(I)=(VY1256-VY3478)*ONE_OVER_8
       HGY4(I)=-(VY1256+VY3478)*ONE_OVER_8       
C
       HGZ3(I)=(VZ1467-VZ2358)*ONE_OVER_8
       HGZ1(I)=(VZ1467+VZ2358)*ONE_OVER_8
       HGZ2(I)=(VZ1256-VZ3478)*ONE_OVER_8
       HGZ4(I)=-(VZ1256+VZ3478)*ONE_OVER_8       
      ENDDO
      DO I=LFT,LLT
        VX17=VX1(I)-VX7(I)
        VX28=VX2(I)-VX8(I)
        VX35=VX3(I)-VX5(I)
        VX46=VX4(I)-VX6(I)
        VY17=VY1(I)-VY7(I)
        VY28=VY2(I)-VY8(I)
        VY35=VY3(I)-VY5(I)
        VY46=VY4(I)-VY6(I)
        VZ17=VZ1(I)-VZ7(I)
        VZ28=VZ2(I)-VZ8(I)
        VZ35=VZ3(I)-VZ5(I)
        VZ46=VZ4(I)-VZ6(I)
C   alpha =1 ->eta zeta   
C 1 1 -1 -1 -1 -1 1 1
        HGX1(I)= HGX1(I)
     &          -(PX1H1(I)*VX17+PX2H1(I)*VX28
     &            +PX3H1(I)*VX35+PX4H1(I)*VX46)
        HGY1(I)= HGY1(I)
     &          -(PX1H1(I)*VY17+PX2H1(I)*VY28
     &            +PX3H1(I)*VY35+PX4H1(I)*VY46)
        HGZ1(I)= HGZ1(I)
     &          -(PX1H1(I)*VZ17+PX2H1(I)*VZ28
     &            +PX3H1(I)*VZ35+PX4H1(I)*VZ46)
C
C   alpha =2 ->zeta ksi   
C 1 -1 -1 1 -1 1 1 -1
        HGX2(I)= HGX2(I)
     &          -(PX1H2(I)*VX17+PX2H2(I)*VX28
     &            +PX3H2(I)*VX35+PX4H2(I)*VX46)
        HGY2(I)= HGY2(I)
     &          -(PX1H2(I)*VY17+PX2H2(I)*VY28
     &            +PX3H2(I)*VY35+PX4H2(I)*VY46)
        HGZ2(I)= HGZ2(I)
     &          -(PX1H2(I)*VZ17+PX2H2(I)*VZ28
     &            +PX3H2(I)*VZ35+PX4H2(I)*VZ46)
C   alpha =3 ->ksi eta    
C 1 -1 1 -1 1 -1 1 -1
        HGX3(I)= HGX3(I)
     &          -(PX1H3(I)*VX17+PX2H3(I)*VX28
     &            +PX3H3(I)*VX35+PX4H3(I)*VX46)
        HGY3(I)= HGY3(I)
     &          -(PX1H3(I)*VY17+PX2H3(I)*VY28
     &            +PX3H3(I)*VY35+PX4H3(I)*VY46)
        HGZ3(I)= HGZ3(I)
     &          -(PX1H3(I)*VZ17+PX2H3(I)*VZ28
     &            +PX3H3(I)*VZ35+PX4H3(I)*VZ46)
C
C   alpha =4 ->ksi eta zeta
C -1 1 -1 1 1 -1 1 -1
        HGX4(I)= HGX4(I)
     &          -(PX1H4(I)*VX17+PX2H4(I)*VX28
     &            +PX3H4(I)*VX35+PX4H4(I)*VX46)
        HGY4(I)= HGY4(I)
     &          -(PX1H4(I)*VY17+PX2H4(I)*VY28
     &            +PX3H4(I)*VY35+PX4H4(I)*VY46)
        HGZ4(I)= HGZ4(I)
     &          -(PX1H4(I)*VZ17+PX2H4(I)*VZ28
     &            +PX3H4(I)*VZ35+PX4H4(I)*VZ46)
      ENDDO
!
      DO I=LFT,LLT
       JR_1(I) = ONE/MAX(EM20,JR0(I))
       JS_1(I) = ONE/MAX(EM20,JS0(I))
       JT_1(I) = ONE/MAX(EM20,JT0(I))
       H11(I) = JS0(I)*JT0(I)*JR_1(I)
       H22(I) = JR0(I)*JT0(I)*JS_1(I)
       H33(I) = JR0(I)*JS0(I)*JT_1(I)
       H12(I) = JT0(I)
       H13(I) = JS0(I)
       H23(I) = JR0(I)
C         SIGY(I) = 1.0E+30
      ENDDO
       DO I=LFT,LLT
        FHOUR(I,1,1) = FHOUR(I,1,1)*OFF(I)
        FHOUR(I,1,2) = FHOUR(I,1,2)*OFF(I)
        FHOUR(I,1,3) = FHOUR(I,1,3)*OFF(I)
        FHOUR(I,1,4) = FHOUR(I,1,4)*OFF(I)
        FHOUR(I,2,1) = FHOUR(I,2,1)*OFF(I)
        FHOUR(I,2,2) = FHOUR(I,2,2)*OFF(I)
        FHOUR(I,2,3) = FHOUR(I,2,3)*OFF(I)
        FHOUR(I,2,4) = FHOUR(I,2,4)*OFF(I)
        FHOUR(I,3,1) = FHOUR(I,3,1)*OFF(I)
        FHOUR(I,3,2) = FHOUR(I,3,2)*OFF(I)
        FHOUR(I,3,3) = FHOUR(I,3,3)*OFF(I)
        FHOUR(I,3,4) = FHOUR(I,3,4)*OFF(I)
       ENDDO
      IF (MATVIS==0) 
     .   CALL SZSVM_OR(JR0,JS0,JT0,CC,CG,G33,FHOUR,SIGY,SIGOLD,NU,SMO1,SMO2,NEL) 
      !-----------For energy calculation------------
      IF(JLAG==1)THEN
       CALL GFHOUR_OR(LFT,LLT,
     .   FHOUR,JR0,JS0,JT0,FCL,
     .   HGX1, HGX2, HGX3, HGX4,
     .   HGY1, HGY2, HGY3, HGY4,
     .   HGZ1, HGZ2, HGZ3, HGZ4,
     .   H11 , H22 , H33 , 
     .   H12 , H13 , H23 , 
     .   JR_1,JS_1 , JT_1, NU4,NU2 ,
     .   CC  ,CG   ,G33  ,NFHOUR,NEL)
C
       DO I=LFT,LLT
        DEINT(I)= 
     .   NFHOUR(I,3,1)*HGZ1(I) + NFHOUR(I,3,2)*HGZ2(I) + 
     .   NFHOUR(I,3,3)*HGZ3(I) + NFHOUR(I,3,4)*HGZ4(I) + 
     .   NFHOUR(I,1,1)*HGX1(I) + NFHOUR(I,1,2)*HGX2(I) + 
     .   NFHOUR(I,1,3)*HGX3(I) + NFHOUR(I,1,4)*HGX4(I) + 
     .   NFHOUR(I,2,1)*HGY1(I) + NFHOUR(I,2,2)*HGY2(I) + 
     .   NFHOUR(I,2,3)*HGY3(I) + NFHOUR(I,2,4)*HGY4(I) 
        EINT(I)= EINT(I)+DT05*DEINT(I)/MAX(EM20,VOL0(I))
       ENDDO
      ENDIF
C      
      IF (IET > 1 .AND. MTN == 24 ) THEN
       CALL MDAMA24(ELBUF_STR,LFT,LLT ,PM    ,MAT    ,DAMA_G )
       DO J=1,3
        DO I=LFT,LLT
         FAC1=ONE- DAMA_G(I,J)
         FHOUR(I,J,1:4) = FHOUR(I,J,1:4)*FAC1
        ENDDO
       ENDDO
      END IF !(IET > 1) THEN
C   -------elstic increament----
      DO I=LFT,LLT
       E_R =G_3DT(I)*JR_1(I)
       E_S =G_3DT(I)*JS_1(I)
       E_T =G_3DT(I)*JT_1(I)
       DFHOUR(I,1,1) = E_R*HGX1(I)
       DFHOUR(I,1,2) = E_R*HGX2(I)
       DFHOUR(I,1,3) = E_R*HGX3(I)
       DFHOUR(I,1,4) = E_R*HGX4(I)
       DFHOUR(I,2,1) = E_S*HGY1(I)
       DFHOUR(I,2,2) = E_S*HGY2(I)
       DFHOUR(I,2,3) = E_S*HGY3(I)
       DFHOUR(I,2,4) = E_S*HGY4(I)
       DFHOUR(I,3,1) = E_T*HGZ1(I)
       DFHOUR(I,3,2) = E_T*HGZ2(I)
       DFHOUR(I,3,3) = E_T*HGZ3(I)
       DFHOUR(I,3,4) = E_T*HGZ4(I)
C
       FHOUR(I,1,1) = FHOUR(I,1,1) + DFHOUR(I,1,1)
       FHOUR(I,1,2) = FHOUR(I,1,2) + DFHOUR(I,1,2)
       FHOUR(I,1,3) = FHOUR(I,1,3) + DFHOUR(I,1,3)
       FHOUR(I,1,4) = FHOUR(I,1,4) + DFHOUR(I,1,4)
       FHOUR(I,2,1) = FHOUR(I,2,1) + DFHOUR(I,2,1)
       FHOUR(I,2,2) = FHOUR(I,2,2) + DFHOUR(I,2,2)
       FHOUR(I,2,3) = FHOUR(I,2,3) + DFHOUR(I,2,3)
       FHOUR(I,2,4) = FHOUR(I,2,4) + DFHOUR(I,2,4)
       FHOUR(I,3,1) = FHOUR(I,3,1) + DFHOUR(I,3,1)
       FHOUR(I,3,2) = FHOUR(I,3,2) + DFHOUR(I,3,2)
       FHOUR(I,3,3) = FHOUR(I,3,3) + DFHOUR(I,3,3)
       FHOUR(I,3,4) = FHOUR(I,3,4) + DFHOUR(I,3,4)
      ENDDO
      IF (MATVIS==0) 
     .    CALL SZSVM_OR(JR0,JS0,JT0,CC,CG,G33,FHOUR,SIGY,SIG0,NU,SM1,SM2,NEL) 
      !-----------ELASTIC-PLASTIC yield criterion------------
      IF (MATVIS==0) THEN
      DO I=LFT,LLT
        IF (SM1(I)>SIGY(I).AND.DEINT(I)>0) THEN
         SMO  = ZEP9*SMO1(I)+EM01*SMO2(I)
         FAC1 = SIGY(I)-SMO
         FAC2 = SM1(I)-SMO
         IF (FAC2<=EM20) THEN
           FAC=ZERO
         ELSE
           FAC = ONE - MAX(EM20,FAC1/FAC2)
         ENDIF
         IF (SM2(I)<SIGY(I)) THEN
           FAC1 =(SM1(I)-SIGY(I))/MAX((SM1(I)-SM2(I)),EM20)
           FAC1 =HALF + SQRT(FAC1)
           FAC = MIN(FAC1,ONE)*FAC
         ENDIF  
         FHOUR(I,1,1) = FHOUR(I,1,1) - FAC*DFHOUR(I,1,1)
         FHOUR(I,1,2) = FHOUR(I,1,2) - FAC*DFHOUR(I,1,2)
         FHOUR(I,1,3) = FHOUR(I,1,3) - FAC*DFHOUR(I,1,3)
         FHOUR(I,1,4) = FHOUR(I,1,4) - FAC*DFHOUR(I,1,4)
         FHOUR(I,2,1) = FHOUR(I,2,1) - FAC*DFHOUR(I,2,1)
         FHOUR(I,2,2) = FHOUR(I,2,2) - FAC*DFHOUR(I,2,2)
         FHOUR(I,2,3) = FHOUR(I,2,3) - FAC*DFHOUR(I,2,3)
         FHOUR(I,2,4) = FHOUR(I,2,4) - FAC*DFHOUR(I,2,4)
         FHOUR(I,3,1) = FHOUR(I,3,1) - FAC*DFHOUR(I,3,1)
         FHOUR(I,3,2) = FHOUR(I,3,2) - FAC*DFHOUR(I,3,2)
         FHOUR(I,3,3) = FHOUR(I,3,3) - FAC*DFHOUR(I,3,3)
         FHOUR(I,3,4) = FHOUR(I,3,4) - FAC*DFHOUR(I,3,4)
        ENDIF
      ENDDO
      ENDIF
       CALL GFHOUR_OR(LFT,LLT,
     .   FHOUR,JR0,JS0,JT0,FCL,
     .   HGX1, HGX2, HGX3, HGX4,
     .   HGY1, HGY2, HGY3, HGY4,
     .   HGZ1, HGZ2, HGZ3, HGZ4,
     .   H11 , H22 , H33 , 
     .   H12 , H13 , H23 , 
     .   JR_1,JS_1 , JT_1, NU4,NU2 ,
     .   CC  ,CG   ,G33  ,NFHOUR,NEL)
      DO I=LFT,LLT
        HQ13P = (NFHOUR(I,1,1)+NFHOUR(I,1,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,1,1)-NFHOUR(I,1,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,1,2)+NFHOUR(I,1,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,1,2)-NFHOUR(I,1,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,1,1)-PX1H2(I)*NFHOUR(I,1,2)
     .      -PX1H3(I)*NFHOUR(I,1,3)-PX1H4(I)*NFHOUR(I,1,4)
        F11(I) =-(HQ13P+HQ24N+FF)
        F17(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,1,1)-PX2H2(I)*NFHOUR(I,1,2)
     .      -PX2H3(I)*NFHOUR(I,1,3)-PX2H4(I)*NFHOUR(I,1,4)
        F12(I) =-(HQ13N-HQ24N+FF)
        F18(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,1,1)-PX3H2(I)*NFHOUR(I,1,2)
     .      -PX3H3(I)*NFHOUR(I,1,3)-PX3H4(I)*NFHOUR(I,1,4)
        F13(I) =-(-HQ13N-HQ24P+FF)
        F15(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,1,1)-PX4H2(I)*NFHOUR(I,1,2)
     .      -PX4H3(I)*NFHOUR(I,1,3)-PX4H4(I)*NFHOUR(I,1,4)
        F14(I) =-(-HQ13P+HQ24P+FF)
        F16(I) =-(-HQ13P+HQ24N-FF)
      ENDDO
      DO I=LFT,LLT
        HQ13P = (NFHOUR(I,2,1)+NFHOUR(I,2,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,2,1)-NFHOUR(I,2,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,2,2)+NFHOUR(I,2,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,2,2)-NFHOUR(I,2,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,2,1)-PX1H2(I)*NFHOUR(I,2,2)
     .      -PX1H3(I)*NFHOUR(I,2,3)-PX1H4(I)*NFHOUR(I,2,4)
        F21(I) =-(HQ13P+HQ24N+FF)
        F27(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,2,1)-PX2H2(I)*NFHOUR(I,2,2)
     .      -PX2H3(I)*NFHOUR(I,2,3)-PX2H4(I)*NFHOUR(I,2,4)
        F22(I) =-(HQ13N-HQ24N+FF)
        F28(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,2,1)-PX3H2(I)*NFHOUR(I,2,2)
     .      -PX3H3(I)*NFHOUR(I,2,3)-PX3H4(I)*NFHOUR(I,2,4)
        F23(I) =-(-HQ13N-HQ24P+FF)
        F25(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,2,1)-PX4H2(I)*NFHOUR(I,2,2)
     .      -PX4H3(I)*NFHOUR(I,2,3)-PX4H4(I)*NFHOUR(I,2,4)
        F24(I) =-(-HQ13P+HQ24P+FF)
        F26(I) =-(-HQ13P+HQ24N-FF)
      ENDDO
      DO I=LFT,LLT
        HQ13P = (NFHOUR(I,3,1)+NFHOUR(I,3,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,3,1)-NFHOUR(I,3,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,3,2)+NFHOUR(I,3,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,3,2)-NFHOUR(I,3,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,3,1)-PX1H2(I)*NFHOUR(I,3,2)
     .      -PX1H3(I)*NFHOUR(I,3,3)-PX1H4(I)*NFHOUR(I,3,4)
        F31(I) =-(HQ13P+HQ24N+FF)
        F37(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,3,1)-PX2H2(I)*NFHOUR(I,3,2)
     .      -PX2H3(I)*NFHOUR(I,3,3)-PX2H4(I)*NFHOUR(I,3,4)
        F32(I) =-(HQ13N-HQ24N+FF)
        F38(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,3,1)-PX3H2(I)*NFHOUR(I,3,2)
     .      -PX3H3(I)*NFHOUR(I,3,3)-PX3H4(I)*NFHOUR(I,3,4)
        F33(I) =-(-HQ13N-HQ24P+FF)
        F35(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,3,1)-PX4H2(I)*NFHOUR(I,3,2)
     .      -PX4H3(I)*NFHOUR(I,3,3)-PX4H4(I)*NFHOUR(I,3,4)
        F34(I) =-(-HQ13P+HQ24P+FF)
        F36(I) =-(-HQ13P+HQ24N-FF)
      ENDDO
      !----hourglass energie est inclued in interal energie------
      IF(JLAG==1)THEN
       DO I=LFT,LLT
        EINT(I)= EINT(I)+DT05*(
     .   NFHOUR(I,3,1)*HGZ1(I) + NFHOUR(I,3,2)*HGZ2(I) + 
     .   NFHOUR(I,3,3)*HGZ3(I) + NFHOUR(I,3,4)*HGZ4(I) + 
     .   NFHOUR(I,1,1)*HGX1(I) + NFHOUR(I,1,2)*HGX2(I) + 
     .   NFHOUR(I,1,3)*HGX3(I) + NFHOUR(I,1,4)*HGX4(I) + 
     .   NFHOUR(I,2,1)*HGY1(I) + NFHOUR(I,2,2)*HGY2(I) + 
     .   NFHOUR(I,2,3)*HGY3(I) + NFHOUR(I,2,4)*HGY4(I) ) 
     .   /MAX(EM20,VOL0(I))
       ENDDO
      ENDIF
      IF(ISTRAIN>0 .AND.  
     .  ((ANIM_N(IAD_GPS+400+1) == 1) .OR. (ANIM_N(IAD_GPS+400+2) == 1) .OR. 
     .   (ANIM_N(IAD_GPS+400+3) == 1) .OR. (ANIM_N(IAD_GPS+400+4) == 1) .OR. 
     .   (ANIM_N(IAD_GPS+400+5) == 1) .OR. (ANIM_N(IAD_GPS+400+6) == 1)) )THEN
       CALL SZSTRAINHG(JR_1,JS_1,JT_1,STRHG,NEL  ,
     .                      HGX1, HGX2, HGX3, HGX4,
     .                      HGY1, HGY2, HGY3, HGY4,
     .                      HGZ1, HGZ2, HGZ3, HGZ4,NU4,NU2)
c       CALL PRSTRAIN(STRAIN,STRHG,CC ,CG  ,G33, GG,NEL  )
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  MMODUL                        source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|        C33STIF2EL                    source/elements/solid/solidez/szhour3.F
Chd|        GETTRANSV                     source/elements/solid/solidez/szhour3.F
Chd|        MSTIFORTHV                    source/elements/solid/solidez/szhour3.F
Chd|====================================================================
      SUBROUTINE MMODUL(JFT     ,JLT     ,PM    ,MAT    ,MTN    ,
     .                  GAMA    ,UPARAM  ,CC    ,CG     ,G33    )
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
#include      "com04_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  ,MTN
      INTEGER MAT(*)
C     REAL
      my_real
     .   PM(NPROPM,*),CC(MVSIZ,3,3),UPARAM(*),
     .   GAMA(MVSIZ,6),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IAD,J,K,ipr,NBDAMA
C     REAL
      my_real
     .   NU,LAMDA,GG,C1,QC(MVSIZ,9),QCG(MVSIZ,9),QG(MVSIZ,9),
     .   QGC(MVSIZ,9),G3(MVSIZ,3),TT,TV,CA,CB,CN,
     .   S1,NU12,NU21,EFAC,ARMA,ARM1,ARM2,ARM3,ET24
      my_real
     .   CC24(MVSIZ,3,3),C3(MVSIZ,3)
C-----------------------------------------------
       CALL GETTRANSV(JFT,JLT,GAMA,QC,QCG,QGC,QG)
       IF (MTN==14.OR.MTN==12) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(I,1,1) =PM(40,MX)
         CC(I,2,2) =PM(43,MX)
         CC(I,3,3) =PM(45,MX)
         CC(I,1,2) =PM(41,MX)
         CC(I,2,3) =PM(44,MX)
         CC(I,1,3) =PM(42,MX)
         G3(I,1) =PM(46,MX)
         G3(I,2) =PM(47,MX)
         G3(I,3) =PM(48,MX)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN==24) THEN
        MX  =MAT(1)
C-------ET24 for the moment is elastic, could be set more precisely     
        ET24 = PM(50,MX)
        ARM1 = PM(53,MX) 
        ARM2 = PM(54,MX) 
        ARM3 = PM(55,MX) 
        ARMA = ARM1 + ARM2 + ARM3
        GG  = PM(22,MX)
        TT  = PM(24,MX)
        TV  = PM(25,MX)
C-------damages are negliged for increment (consisting to isotropic case )
C-----will apply the dommage to generalized stress        
        DO I=JFT,JLT
         CC(I,1,1) = TT
         CC(I,2,2) = TT
         CC(I,3,3) = TT
         CC(I,1,2) = TV
         CC(I,2,3) = TV
         CC(I,1,3) = TV
         CC(I,2,1) = TV
         CC(I,3,2) = TV
         CC(I,3,1) = TV
c
         G3 (I,1:3)= GG
        END DO 
        IF (ARMA>ZERO) THEN
C--------assemble the two parts in reinforced system   
         DO I=JFT,JLT
          C3(I,1) =(ONE-ARM1)*CC(I,1,1)+ET24*ARM1
          C3(I,2) =(ONE-ARM2)*CC(I,2,2)+ET24*ARM2
          C3(I,3) =(ONE-ARM3)*CC(I,3,3)+ET24*ARM3
         ENDDO
C--------translate C3 to elem sys--(CC24,G33   ,CG)
        CALL C33STIF2EL(JLT   ,QC   ,QCG   ,QGC    ,QG     ,
     .                  C3    ,CC24 ,G33   ,CG     ) 
C--------final assemblage ---- 
         DO J = 1,3
          DO I=JFT,JLT
           CC(I,J,J) =CC24(I,J,J)
           G33(I,J,J)=G33(I,J,J)+G3(I,J)
          ENDDO
         ENDDO
          DO I=JFT,JLT
           CC(I,1,2) = CC(I,1,2) + CC24(I,1,2)
           CC(I,1,3) = CC(I,1,3) + CC24(I,1,3)
           CC(I,2,3) = CC(I,2,3) + CC24(I,2,3)
           CC(I,2,1) = CC(I,1,2) 
           CC(I,3,1) = CC(I,1,3) 
           CC(I,3,2) = CC(I,2,3) 
          ENDDO
        ELSE
C----- w/o armature :--> isotropic      
         DO I=JFT,JLT
          G33(I,1,1) = GG
          G33(I,2,2) = G33(I,1,1)
          G33(I,3,3) = G33(I,1,1)
          G33(I,1,2) = ZERO
          G33(I,2,3) = ZERO
          G33(I,1,3) = ZERO
          G33(I,2,1) = ZERO
          G33(I,3,2) = ZERO
          G33(I,3,1) = ZERO
          CG(I,1:3,1:3)=ZERO
         ENDDO
        END IF
       ELSEIF (MTN==25) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         NU12 =PM(35,MX)
         NU21 =PM(36,MX)
         S1 = ONE-NU12*NU21
         CC(I,1,1) =PM(33,MX)/MAX(EM20,S1)
         CC(I,2,2) =PM(34,MX)/MAX(EM20,S1)
         CC(I,3,3) =PM(186,MX)
         CC(I,1,2) =HALF*(NU21*CC(I,1,1)+NU12*CC(I,2,2))
         CC(I,2,3) =ZERO
         CC(I,1,3) =ZERO
         G3(I,1) =PM(37,MX)
         G3(I,2) =PM(38,MX)
         G3(I,3) =PM(39,MX)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN==28.OR.MTN==50.OR.MTN==68) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(I,1,1) = UPARAM(1)
         CC(I,2,2) = UPARAM(2)
         CC(I,3,3) = UPARAM(3)
         CC(I,1,2) = ZERO
         CC(I,2,3) = ZERO
         CC(I,1,3) = ZERO
         G3(I,1) = UPARAM(4)
         G3(I,2) = UPARAM(5)
         G3(I,3) = UPARAM(6)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN==53) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(I,1,1) = UPARAM(1)
         CC(I,2,2) = UPARAM(2)
         CC(I,3,3) = CC(I,2,2)
         CC(I,1,2) = ZERO
         CC(I,2,3) = ZERO
         CC(I,1,3) = ZERO
         G3(I,1) = UPARAM(3)
         G3(I,2) = UPARAM(4)
         G3(I,3) = G3(I,1)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN == 93) THEN     
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(I,1,1) = UPARAM(4)
         CC(I,2,2) = UPARAM(7)
         CC(I,3,3) = UPARAM(9)
         CC(I,1,2) = UPARAM(5)
         CC(I,2,3) = UPARAM(8)
         CC(I,1,3) = UPARAM(6)
         G3(I,1)   = UPARAM(10)
         G3(I,2)   = UPARAM(11)
         G3(I,3)   = UPARAM(12)
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     )        
       ELSEIF (MTN==107) THEN
        DO I=JFT,JLT
          MX = MAT(I)
          NU12 = UPARAM(4)
          NU21 = UPARAM(5)
          S1 = ONE-NU12*NU21
          CC(I,1,1) =UPARAM(1)/MAX(EM20,S1)
          CC(I,2,2) =UPARAM(2)/MAX(EM20,S1)
          CC(I,3,3) =UPARAM(3)
          CC(I,1,2) =HALF*(NU21*CC(I,1,1)+NU12*CC(I,2,2))
          CC(I,2,3) =ZERO
          CC(I,1,3) =ZERO
          G3(I,1)   = UPARAM(10)
          G3(I,2)   = UPARAM(11)
          G3(I,3)   = UPARAM(12) 
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     )
       ELSEIF (MTN==112) THEN
        DO I=JFT,JLT
          MX = MAT(I)
          NU12 = UPARAM(4)
          NU21 = UPARAM(5)
          S1 = ONE-NU12*NU21
          CC(I,1,1) =UPARAM(1)/MAX(EM20,S1)
          CC(I,2,2) =UPARAM(2)/MAX(EM20,S1)
          CC(I,3,3) =UPARAM(3)
          CC(I,1,2) =HALF*(NU21*CC(I,1,1)+NU12*CC(I,2,2))
          CC(I,2,3) =ZERO
          CC(I,1,3) =ZERO
          G3(I,1)   = UPARAM(10)
          G3(I,2)   = UPARAM(11)
          G3(I,3)   = UPARAM(12) 
        ENDDO
        CALL MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                  QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSE
C----------warring out isotrope effective       
        DO I=JFT,JLT
         MX  =MAT(I)
         NU =PM(21,MX)
         C1 =THREE*PM(32,MX)/(ONE+NU)
         LAMDA=C1*NU
C-------GG:2G---HH(2,I):G-------
         GG =C1*(ONE-TWO*NU)
         CC(I,1,1) = LAMDA+GG
         CC(I,2,2) = CC(I,1,1)
         CC(I,3,3) = CC(I,1,1)
         CC(I,1,2) = LAMDA
         CC(I,2,3) = LAMDA
         CC(I,1,3) = LAMDA
         CC(I,2,1) = LAMDA
         CC(I,3,2) = LAMDA
         CC(I,3,1) = LAMDA
c
         G33(I,1,1) = GG*HALF
         G33(I,2,2) = G33(I,1,1)
         G33(I,3,3) = G33(I,1,1)
         G33(I,1,2) = ZERO
         G33(I,2,3) = ZERO
         G33(I,1,3) = ZERO
         G33(I,2,1) = ZERO
         G33(I,3,2) = ZERO
         G33(I,3,1) = ZERO
C        
         CG(I,1:3,1:3)=ZERO
        ENDDO
       ENDIF
 
C
      RETURN
      END
Chd|====================================================================
Chd|  GETTRANSV                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        M24C2ARM                      source/elements/solid/solidez/szhour3.F
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|        MMODUL                        source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETTRANSV(JFT    ,JLT   ,GAMA ,QC  ,QCG  ,
     .                     QGC    ,QG   ) 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  
C     REAL
      my_real
     .   GAMA(MVSIZ,6),QC(MVSIZ,3,3),QCG(MVSIZ,3,3),
     .   QGC(MVSIZ,3,3),QG(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   E1(MVSIZ,3),E2(MVSIZ,3),E3(MVSIZ,3)
C-----------------------------------------------
       DO J= 1,3
        DO I=JFT,JLT
          E1(I,J)=GAMA(I,J)
          E2(I,J)=GAMA(I,J+3)
        ENDDO
       ENDDO
       DO I=JFT,JLT
          E3(I,1)=E1(I,2)*E2(I,3)-E1(I,3)*E2(I,2)
          E3(I,2)=E1(I,3)*E2(I,1)-E1(I,1)*E2(I,3)
          E3(I,3)=E1(I,1)*E2(I,2)-E1(I,2)*E2(I,1)
       ENDDO
C       
       DO J= 1,3
        DO I=JFT,JLT
         QC(I,1,J)=E1(I,J)*E1(I,J)
         QC(I,2,J)=E2(I,J)*E2(I,J)
         QC(I,3,J)=E3(I,J)*E3(I,J)
C
         QGC(I,1,J)=E1(I,J)*E2(I,J)
         QGC(I,2,J)=E2(I,J)*E3(I,J)
         QGC(I,3,J)=E1(I,J)*E3(I,J)
        ENDDO
       ENDDO
       DO J= 1,3
        K = J+1
        IF (K>3) K = 1
        DO I=JFT,JLT
         QG(I,1,J)=E1(I,J)*E2(I,K)+E2(I,J)*E1(I,K)
         QG(I,2,J)=E2(I,J)*E3(I,K)+E3(I,J)*E2(I,K)
         QG(I,3,J)=E3(I,J)*E1(I,K)+E1(I,J)*E3(I,K)
C
         QCG(I,1,J)=TWO*E1(I,J)*E1(I,K)
         QCG(I,2,J)=TWO*E2(I,J)*E2(I,K)
         QCG(I,3,J)=TWO*E3(I,J)*E3(I,K)
        ENDDO
       ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  MSTIFORTHV                    source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        M24C2ARM                      source/elements/solid/solidez/szhour3.F
Chd|        MMODUL                        source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|        CBATRAN3V                     source/elements/solid/solidez/szhour3.F
Chd|====================================================================
      SUBROUTINE MSTIFORTHV(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                      QG     ,CC    ,G3  ,G33  ,CG     ) 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  
C     REAL
      my_real
     .   QC(MVSIZ,3,3),QCG(MVSIZ,3,3),QGC(MVSIZ,3,3),QG(MVSIZ,3,3),
     .   G3(MVSIZ,3),CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ISYM
C     REAL
      my_real
     .   A(MVSIZ,3,3),B(MVSIZ,3,3)
C-----------------------------------------------
       ISYM = 0
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(I,J,K)=CC(I,J,K)
          B(I,J,K)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
       DO K= J+1,3
        DO I=JFT,JLT
          A(I,K,J)=CC(I,J,K)
          B(I,K,J)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(I,J,J)=G3(I,J)
        ENDDO
       ENDDO
      CALL CBATRAN3V(JFT   ,JLT    ,QC   ,A ,QCG,ISYM) 
      CALL CBATRAN3V(JFT   ,JLT    ,QGC  ,B ,QG ,ISYM) 
C      
       DO J= 1,3
       DO K= 1,3
        DO I=JFT,JLT
          CG(I,J,K)=HALF*A(I,J,K)+TWO*B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C---------------[G']->[G33]----
       ISYM = 1
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(I,J,K)=CC(I,J,K)
          B(I,J,K)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(I,J,J)=G3(I,J)
        ENDDO
       ENDDO
      CALL CBATRAN3V(JFT   ,JLT    ,QCG  ,A ,QCG,ISYM) 
      CALL CBATRAN3V(JFT   ,JLT    ,QG   ,B ,QG ,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          G33(I,J,K)=FOURTH*A(I,J,K)+B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C---------------[C']->[CC]----
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(I,J,K)=CC(I,J,K)
          B(I,J,K)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(I,J,J)=G3(I,J)
        ENDDO
       ENDDO
      CALL CBATRAN3V(JFT   ,JLT    ,QC   ,A ,QC ,ISYM) 
      CALL CBATRAN3V(JFT   ,JLT    ,QGC  ,B ,QGC,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          CC(I,J,K)=A(I,J,K)+FOUR*B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C       
       DO J= 1,3
       DO K= J+1,3
        DO I=JFT,JLT
          CC(I,K,J)=CC(I,J,K)
          G33(I,K,J)=G33(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  CBATRAN3V                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        C33STIF2EL                    source/elements/solid/solidez/szhour3.F
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|        MSTIFORTHV                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN3V(JFT   ,JLT    ,VQI   ,KK,VQJ,ISYM) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISYM
      my_real 
     .   VQI(MVSIZ,3,3), VQJ(MVSIZ,3,3),KK(MVSIZ,3,3)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(MVSIZ,3,3)
C-----------------------------------------------
       IF (ISYM==1) THEN
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K(EP,I,J)=VQI(EP,1,I)*(KK(EP,1,1)*VQJ(EP,1,J)+
     1                  KK(EP,1,2)*VQJ(EP,2,J)+KK(EP,1,3)*VQJ(EP,3,J))+
     2              VQI(EP,2,I)*(KK(EP,1,2)*VQJ(EP,1,J)+
     3                  KK(EP,2,2)*VQJ(EP,2,J)+KK(EP,2,3)*VQJ(EP,3,J))+
     4              VQI(EP,3,I)*(KK(EP,1,3)*VQJ(EP,1,J)+
     5                  KK(EP,2,3)*VQJ(EP,2,J)+KK(EP,3,3)*VQJ(EP,3,J))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KK(EP,I,J)= K(EP,I,J)
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(EP,I,J)=VQI(EP,1,I)*(KK(EP,1,1)*VQJ(EP,1,J)+
     1                  KK(EP,1,2)*VQJ(EP,2,J)+KK(EP,1,3)*VQJ(EP,3,J))+
     2              VQI(EP,2,I)*(KK(EP,2,1)*VQJ(EP,1,J)+
     3                  KK(EP,2,2)*VQJ(EP,2,J)+KK(EP,2,3)*VQJ(EP,3,J))+
     4              VQI(EP,3,I)*(KK(EP,3,1)*VQJ(EP,1,J)+
     5                  KK(EP,3,2)*VQJ(EP,2,J)+KK(EP,3,3)*VQJ(EP,3,J))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(EP,I,J)= K(EP,I,J)
         ENDDO
        ENDDO
        ENDDO
       ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  MMOD_NORM                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MMOD_NORM(JFT  ,JLT   ,GG    ,CC     ,CG    ,G33     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  
C     REAL
      my_real
     .   GG(*),CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   G_1(MVSIZ)
C-----------------------------------------------
C-------Normalized by GG------
        DO I=JFT,JLT
         G_1(I) =ONE/GG(I)
        END DO
C       
         DO J=1,3
          DO K=J,3
           DO I=JFT,JLT
            CC(I,J,K) = CC(I,J,K)*G_1(I)
            G33(I,J,K) = G33(I,J,K)*G_1(I)
            CC(I,K,J) = CC(I,J,K)
            G33(I,K,J) = G33(I,J,K)
           ENDDO
          END DO
         END DO
c
         DO J=1,3
          DO K=1,3
           DO I=JFT,JLT
            CG(I,J,K) = CG(I,J,K)*G_1(I)
           END DO
          END DO
         END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  SZSVM_OR                      source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSVM_OR(JR0,JS0,JT0,CC,CG,G33,FHOUR,SIGY,SIG0,NU,SVM1,SVM2,NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIGY(*) ,SIG0(NEL,6),SVM1(*),SVM2(*),NU,
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,K,IKT
      my_real
     .   S1,S2,S3,SVM0,SR1,SR2,SR3,SR4,
     .   SS1,SS2,SS3,SS4,ST1,ST2,ST3,ST4,SVMR,SVMS,SVMT,
     .   SVM,SVMRST,COEF,COEF1, JR_1,JS_1,JT_1,NU1,NU2,T1,T2,
     .   RS,ST,RT,VT1(4),VT2(4),MAX1,MIN1,MAX2,MIN2,MAX0,MIN0, 
     .   SR5,SR6,SS5,SS6, ST5,ST6
C-----------------------------------------------
C         coef= (3*8*0.75)^2 coef1 =1/0.75
          COEF= THREEHUNDRED24
          COEF1= ONEP333
      IKT = IINT
      IF (IKT ==2) THEN
        COEF1= COEF/(TEN*IKT)
        COEF= MAX(COEF1,EM01)
      ELSEIF (IKT > 2) THEN
        COEF1= COEF/(TEN*IKT)
        COEF= MAX(COEF1,EM01)
      ENDIF
        NU1 =ONE/(ONE - NU)
        NU2 =NU*NU1
      DO I=LFT,LLT
       SVM1(I) = ZERO
       SVM2(I) = ZERO
       IF (SIGY(I)<ZEP9EP30) THEN
        S1 =SIG0(I,1)-SIG0(I,2)
        S2 =SIG0(I,2)-SIG0(I,3)
        S3 =SIG0(I,1)-SIG0(I,3)
        SVM0 = (S1*S1+S2*S2+S3*S3)*HALF + THREE*(SIG0(I,4)*SIG0(I,4)
     .                +SIG0(I,5)*SIG0(I,5)+SIG0(I,6)*SIG0(I,6))
        JR_1 = ONE/MAX(EM20,JR0(I))
        JS_1 = ONE/MAX(EM20,JS0(I))
        JT_1 = ONE/MAX(EM20,JT0(I))
        J=2
        K=1
        T1 = NU2*(CC(I,J,2)-CC(I,K,2))
        T2 = JR0(I)*JT_1*FHOUR(I,1,1)+JT0(I)*JR_1*FHOUR(I,3,3)
        SR1 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,1)+
     .       (CG(I,J,2)-CG(I,K,2))*T2   
        J=3
        K=2
        T1 = NU2*(CC(I,J,2)-CC(I,K,2))
        SR2 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,1)+
     .       (CG(I,J,2)-CG(I,K,2))*T2   
        J=3
        K=1
        T1 = NU2*(CC(I,J,2)-CC(I,K,2))
        SR3 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,1)+
     .       (CG(I,J,2)-CG(I,K,2))*T2   
        J=1
        T1 = NU2*CG(I,2,J)
        SR4 =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,2)*T2
        J=2
        T1 = NU2*CG(I,2,J)
        SR5 =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,2)*T2
        J=3
        T1 = NU2*CG(I,2,J)
        SR6 =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,2)*T2
        SVMR =HALF*(SR1*SR1+SR2*SR2+SR3*SR3)+THREE*(SR4*SR4+SR5*SR5+SR6*SR6)
        J=2
        K=1
        T1 = NU2*(CC(I,J,3)-CC(I,K,3))
        T2 = JR0(I)*JS_1*FHOUR(I,1,1)+JS0(I)*JR_1*FHOUR(I,2,2)
        SS1 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,1)+
     .       (CG(I,J,1)-CG(I,K,1))*T2   
        J=3
        K=2
        T1 = NU2*(CC(I,J,3)-CC(I,K,3))
        SS2 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,1)+
     .       (CG(I,J,1)-CG(I,K,1))*T2   
        J=3
        K=1
        T1 = NU2*(CC(I,J,3)-CC(I,K,3))
        SS3 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,1)+
     .       (CG(I,J,1)-CG(I,K,1))*T2   
        J=1
        T1 = NU2*CG(I,3,J)
        SS4 =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        J=2
        T1 = NU2*CG(I,3,J)
        SS5 =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        J=3
        T1 = NU2*CG(I,3,J)
        SS6 =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        SVMS =HALF*(SS1*SS1+SS2*SS2+SS3*SS3)+THREE*(SS4*SS4+SS5*SS5+SS6*SS6)
        J=2
        K=1
        T1 = NU2*(CC(I,J,1)-CC(I,K,1))
        T2 = JS0(I)*JT_1*FHOUR(I,2,2)+JT0(I)*JS_1*FHOUR(I,3,3)
        ST1 =(CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,2)+
     .       (CG(I,J,3)-CG(I,K,3))*T2   
        J=3
        K=2
        T1 = NU2*(CC(I,J,1)-CC(I,K,1))
        ST2 =(CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,2)+
     .       (CG(I,J,3)-CG(I,K,3))*T2   
        J=3
        K=1
        T1 = NU2*(CC(I,J,1)-CC(I,K,1))
        ST3 =(CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,2)+
     .       (CG(I,J,3)-CG(I,K,3))*T2   
        J=1
        T1 = NU2*CG(I,1,J)
        ST4 =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,3)*T2
        J=2
        T1 = NU2*CG(I,1,J)
        ST5 =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,3)*T2
        J=3
        T1 = NU2*CG(I,1,J)
        ST6 =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,3)*T2
        SVMT =HALF*(ST1*ST1+ST2*ST2+ST3*ST3)+THREE*(ST4*ST4+ST5*ST5+ST6*ST6)
C
        RS =HALF*SR1*SS1
        RT =HALF*SR1*ST1
        ST =HALF*SS1*ST1
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        RS =HALF*SR2*SS2
        RT =HALF*SR2*ST2
        ST =HALF*SS2*ST2
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX0+MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN0+MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        RS =HALF*SR3*SS3
        RT =HALF*SR3*ST3
        ST =HALF*SS3*ST3
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX0+MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN0+MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        SVMRST = SVM0+COEF*(SVMR+SVMS+SVMT)
        SVM1(I) = SQRT(ABS(SVMRST+COEF*MAX0))
        SVM2(I) = SQRT(MAX(SVMRST+COEF*MIN0,ZERO))
        SVM2(I) = MIN(SVM2(I),SVM0)
       ENDIF
      ENDDO 
C
      RETURN
      END
Chd|====================================================================
Chd|  GFHOUR_OR                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GFHOUR_OR(LFT,LLT,
     .   FHOUR,JR0,JS0,JT0,FCL,
     .   HGX1, HGX2, HGX3, HGX4,
     .   HGY1, HGY2, HGY3, HGY4,
     .   HGZ1, HGZ2, HGZ3, HGZ4,
     .   H11 , H22 , H33 , 
     .   H12 , H13 , H23 , 
     .   JR_1,JS_1 , JT_1, NU ,NU2 ,
     .   CC  ,CG   ,G33  ,NFHOUR,NEL)
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
#include      "units_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NEL
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,NU(*),NU2(*),
     .   H11(*), H22(*), H33(*),FCL(*),
     .   H12(*), H13(*), H23(*),
     .   HGX1(*), HGX2(*), HGX3(*), HGX4(*),
     .   HGY1(*), HGY2(*), HGY3(*), HGY4(*),
     .   HGZ1(*), HGZ2(*), HGZ3(*), HGZ4(*),
     .   JR_1(*),JS_1(*),JT_1(*),NFHOUR(MVSIZ,3,4)
      my_real
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,K
      my_real
     .   T13,T22,T31,T32,SXY,SYZ,SXZ,SYX,SZY,SZX,
     .   S1211,S1222,S1311,S1333,S2322,S2333,
     .   T1211,T1222,T1311,T1333,T2322,T2333,T12,T23
      my_real
     .   FHOURT(3,4),NUCC(MVSIZ,3,3),R1,R2,R3
C-----------------------------------------------
       DO J=1,3
       DO K=J,3
       DO I=LFT,LLT
        NUCC(I,J,K) = NU2(I)*CC(I,J,K)
        NUCC(I,K,J) = NUCC(I,J,K)
       ENDDO
       ENDDO
       ENDDO
       DO I=LFT,LLT
        FHOURT(1,1) = FHOUR(I,1,1)*JR0(I)+FCL(I)*HGX1(I)
        FHOURT(1,2) = FHOUR(I,1,2)*JR0(I)+FCL(I)*HGX2(I)
        FHOURT(1,3) = FHOUR(I,1,3)*JR0(I)+FCL(I)*HGX3(I)
        FHOURT(1,4) = FHOUR(I,1,4)*JR0(I)+FCL(I)*HGX4(I)
        FHOURT(2,1) = FHOUR(I,2,1)*JS0(I)+FCL(I)*HGY1(I)
        FHOURT(2,2) = FHOUR(I,2,2)*JS0(I)+FCL(I)*HGY2(I)
        FHOURT(2,3) = FHOUR(I,2,3)*JS0(I)+FCL(I)*HGY3(I)
        FHOURT(2,4) = FHOUR(I,2,4)*JS0(I)+FCL(I)*HGY4(I)
        FHOURT(3,1) = FHOUR(I,3,1)*JT0(I)+FCL(I)*HGZ1(I)
        FHOURT(3,2) = FHOUR(I,3,2)*JT0(I)+FCL(I)*HGZ2(I)
        FHOURT(3,3) = FHOUR(I,3,3)*JT0(I)+FCL(I)*HGZ3(I)
        FHOURT(3,4) = FHOUR(I,3,4)*JT0(I)+FCL(I)*HGZ4(I)
C-----
        T31 = NU2(I)*CG(I,3,1)
        T12 = NU2(I)*CG(I,1,2)
        T23 = NU2(I)*CG(I,2,3)
        SXY = (CG(I,1,1)-T31)*FHOURT(1,2)
        SYX = (CG(I,2,1)-T31)*FHOURT(2,1)
        SYZ = (CG(I,2,2)-T12)*FHOURT(2,3)
        SZY = (CG(I,3,2)-T12)*FHOURT(3,2)
        SXZ = (CG(I,1,3)-T23)*FHOURT(1,3)
        SZX = (CG(I,3,3)-T23)*FHOURT(3,1)
        S1211 = JR0(I)*JS_1(I)*FHOURT(1,1)
        S1222 = JS0(I)*JR_1(I)*FHOURT(2,2)
        S1311 = JR0(I)*JT_1(I)*FHOURT(1,1)
        S1333 = JT0(I)*JR_1(I)*FHOURT(3,3)
        S2322 = JS0(I)*JT_1(I)*FHOURT(2,2)
        S2333 = JT0(I)*JS_1(I)*FHOURT(3,3)
        T1211 = S1211*G33(I,1,1)
        T1222 = S1222*G33(I,1,1)
C-------verify w/ Radioss convention    
        T1311 = S1311*G33(I,3,3)
        T1333 = S1333*G33(I,3,3)
        T2322 = S2322*G33(I,2,2)
        T2333 = S2333*G33(I,2,2)
        NFHOUR(I,1,1) = H12(I)*(SXY+T1211)+H22(I)*(SYX+T1222)+
     .                  H13(I)*(SXZ+T1311)+H33(I)*(SZX+T1333)
        NFHOUR(I,2,2) = H11(I)*(SXY+T1211)+H12(I)*(SYX+T1222)+
     .                  H23(I)*(SYZ+T2322)+H33(I)*(SZY+T2333)
        NFHOUR(I,3,3) = H11(I)*(SXZ+T1311)+H13(I)*(SZX+T1333)+
     .                  H22(I)*(SYZ+T2322)+H23(I)*(SZY+T2333)
c       
        NFHOUR(I,1,2) = H11(I)*FHOURT(1,2)*(
     .   (CC(I,1,1)-NUCC(I,1,3)-NU2(I)*(CC(I,3,1)-NUCC(I,3,3))))+
     .                  H12(I)*FHOURT(2,1)*(
     .   (CC(I,1,2)-NUCC(I,1,3)-NU2(I)*(CC(I,3,2)-NUCC(I,3,3))))+
     .   (CG(I,1,1)-T31)*(H11(I)*S1211+H12(I)*S1222)
        NFHOUR(I,1,3) =H11(I)*FHOURT(1,3)*(
     .   (CC(I,1,1)-NUCC(I,1,2)-NU2(I)*(CC(I,2,1)-NUCC(I,2,2))))+
     .                  H13(I)*FHOURT(3,1)*(
     .   (CC(I,1,3)-NUCC(I,1,2)-NU2(I)*(CC(I,2,3)-NUCC(I,2,2))))+
     .   (CG(I,1,3)-T23)*(H11(I)*S1311+H13(I)*S1333)
        NFHOUR(I,1,4) = THIRD*H11(I)*FHOURT(1,4)*(
     .       (CC(I,1,1)-NU(I)*(TWO*(CC(I,1,2)+CC(I,1,3)-NU(I)*CC(I,2,3))-
     .                         NU(I)*(CC(I,2,2)+CC(I,3,3)))))
C     
        NFHOUR(I,2,1) = H12(I)*FHOURT(1,2)*(
     .   (CC(I,2,1)-NUCC(I,2,3)-NU2(I)*(CC(I,3,1)-NUCC(I,3,3))))+
     .                  H22(I)*FHOURT(2,1)*(
     .   (CC(I,2,2)-NUCC(I,2,3)-NU2(I)*(CC(I,3,2)-NUCC(I,3,3))))+
     .   (CG(I,2,1)-T31)*(H12(I)*S1211+H22(I)*S1222)
        NFHOUR(I,2,3) = H22(I)*FHOURT(2,3)*(
     .   (CC(I,2,2)-NUCC(I,2,1)-NU2(I)*(CC(I,1,2)-NUCC(I,1,1))))+
     .                  H23(I)*FHOURT(3,2)*(
     .   (CC(I,2,3)-NUCC(I,2,1)-NU2(I)*(CC(I,1,3)-NUCC(I,1,1))))+
     .   (CG(I,2,2)-T12)*(H22(I)*S2322+H23(I)*S2333)
        NFHOUR(I,2,4) = THIRD*H22(I)*FHOURT(2,4)*(
     .       (CC(I,2,2)-NU(I)*(TWO*(CC(I,1,2)+CC(I,2,3)-NU(I)*CC(I,1,3))-
     .                         NU(I)*(CC(I,1,1)+CC(I,3,3)))))
c
        NFHOUR(I,3,1) = H13(I)*FHOURT(1,3)*(
     .   (CC(I,3,1)-NUCC(I,3,2)-NU2(I)*(CC(I,2,1)-NUCC(I,2,2))))+
     .                  H33(I)*FHOURT(3,1)*(
     .   (CC(I,3,3)-NUCC(I,3,2)-NU2(I)*(CC(I,2,3)-NUCC(I,2,2))))+
     .   (CG(I,3,3)-T23)*(H13(I)*S1311+H33(I)*S1333)
        NFHOUR(I,3,2) = H23(I)*FHOURT(2,3)*(
     .   (CC(I,3,2)-NUCC(I,3,1)-NU2(I)*(CC(I,1,2)-NUCC(I,1,1))))+
     .                  H33(I)*FHOURT(3,2)*(
     .   (CC(I,3,3)-NUCC(I,3,1)-NU2(I)*(CC(I,1,3)-NUCC(I,1,1))))+
     .   (CG(I,3,2)-T12)*(H23(I)*S2322+H33(I)*S2333)
        NFHOUR(I,3,4) = THIRD*H33(I)*FHOURT(3,4)*(
     .       (CC(I,3,3)-NU(I)*(TWO*(CC(I,1,3)+CC(I,2,3)-NU(I)*CC(I,1,2))-
     .                         NU(I)*(CC(I,1,1)+CC(I,2,2)))))
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SZSIGH_OR                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSIGH_OR(KSI,ETA,ZETA,JR0,JS0,JT0,CC,CG,G33,FHOUR,NU,SIG,NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIG(MVSIZ,6) ,KSI,ETA,ZETA,NU,
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,K
      my_real
     .   NU1,NU2,T1,T2,RS,ST,RT, 
     .   RXX,RYY,RZZ,RXY,RYZ,RXZ,SXX,SYY,SZZ,SXY,SYZ,SXZ,
     .   TXX,TYY,TZZ,TXY,TYZ,TXZ,QXX,QYY,QZZ,QXY,QYZ,QXZ,
     .   RS1,ST1,RT1,JR_1,JS_1,JT_1
C----------RXX,SXX,TXX:ETA,ZETA,KSI-------------------------------------
        NU1 =ONE/(ONE - NU)
        NU2 =NU*NU1
        RS = ETA*ZETA
        ST = ZETA*KSI
        RT = ETA*KSI
      DO I=LFT,LLT
        JR_1 = ONE/MAX(EM20,JR0(I))
        JS_1 = ONE/MAX(EM20,JS0(I))
        JT_1 = ONE/MAX(EM20,JT0(I))
        K=1
        T1 = NU2*CC(I,K,2)
        T2 = JR0(I)*JT_1*FHOUR(I,1,1)+JT0(I)*JR_1*FHOUR(I,3,3)
        RXX = (CC(I,K,1)-T1)*FHOUR(I,1,3)+     
     .        (CC(I,K,3)-T1)*FHOUR(I,3,1)+ CG(I,K,3)*T2
        K=2
        T1 = NU2*CC(I,K,2)
        RYY = (CC(I,K,1)-T1)*FHOUR(I,1,3)+     
     .        (CC(I,K,3)-T1)*FHOUR(I,3,1)+ CG(I,K,3)*T2
        K=3
        T1 = NU2*CC(I,K,2)
        RZZ = (CC(I,K,1)-T1)*FHOUR(I,1,3)+     
     .        (CC(I,K,3)-T1)*FHOUR(I,3,1)+ CG(I,K,3)*T2
        J=1
        T1 = NU2*CG(I,2,J)
        RXY =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,3)*T2
        J=2
        T1 = NU2*CG(I,2,J)
        RYZ =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,3)*T2
        J=3
        T1 = NU2*CG(I,2,J)
        RXZ =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,3)*T2
        K=1
        T1 = NU2*CC(I,K,3)
        T2 = JR0(I)*JS_1*FHOUR(I,1,1)+JS0(I)*JR_1*FHOUR(I,2,2)
        SXX =(CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,K,2)-T1)*FHOUR(I,2,1)+ CG(I,K,1)*T2   
        K=2
        T1 = NU2*CC(I,K,3)
        SYY =(CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,K,2)-T1)*FHOUR(I,2,1)+ CG(I,K,1)*T2   
        K=3
        T1 = NU2*CC(I,K,3)
        SZZ =(CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,K,2)-T1)*FHOUR(I,2,1)+ CG(I,K,1)*T2   
        J=1
        T1 = NU2*CG(I,3,J)
        SXY =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        J=2
        T1 = NU2*CG(I,3,J)
        SYZ =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        J=3
        T1 = NU2*CG(I,3,J)
        SXZ =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        K=1
        T1 = NU2*CC(I,K,1)
        T2 = JS0(I)*JT_1*FHOUR(I,2,2)+JT0(I)*JS_1*FHOUR(I,3,3)
        TXX =(CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,K,3)-T1)*FHOUR(I,3,2)+ CG(I,K,2)*T2   
        K=2
        T1 = NU2*CC(I,K,1)
        TYY =(CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,K,3)-T1)*FHOUR(I,3,2)+ CG(I,K,2)*T2   
        K=3
        T1 = NU2*CC(I,K,1)
        TZZ =(CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,K,3)-T1)*FHOUR(I,3,2)+ CG(I,K,2)*T2   
        J=1
        T1 = NU2*CG(I,1,J)
        TXY =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,2)*T2
        J=2
        T1 = NU2*CG(I,1,J)
        TYZ =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,2)*T2
        J=3
        T1 = NU2*CG(I,1,J)
        TXZ =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,2)*T2
        RS1 = FHOUR(I,1,4)*RS
        ST1 = FHOUR(I,2,4)*ST
        RT1 = FHOUR(I,3,4)*RT
        K = 1
        QXX =(CC(I,K,1)-NU*(CC(I,K,2)+CC(I,K,3)))*RS1+
     .       (CC(I,K,2)-NU*(CC(I,K,3)+CC(I,K,1)))*ST1+
     .       (CC(I,K,3)-NU*(CC(I,K,1)+CC(I,K,2)))*RT1     
        K = 2
        QYY =(CC(I,K,1)-NU*(CC(I,K,2)+CC(I,K,3)))*RS1+
     .       (CC(I,K,2)-NU*(CC(I,K,3)+CC(I,K,1)))*ST1+
     .       (CC(I,K,3)-NU*(CC(I,K,1)+CC(I,K,2)))*RT1     
        K = 3
        QZZ =(CC(I,K,1)-NU*(CC(I,K,2)+CC(I,K,3)))*RS1+
     .       (CC(I,K,2)-NU*(CC(I,K,3)+CC(I,K,1)))*ST1+
     .       (CC(I,K,3)-NU*(CC(I,K,1)+CC(I,K,2)))*RT1     
        J = 1
        QXY =(CG(I,1,J)-NU*(CG(I,1,J)+CG(I,1,J)))*RS1+
     .       (CG(I,2,J)-NU*(CG(I,2,J)+CG(I,2,J)))*ST1+
     .       (CG(I,3,J)-NU*(CG(I,3,J)+CG(I,3,J)))*RT1     
        J = 2
        QYZ =(CG(I,1,J)-NU*(CG(I,1,J)+CG(I,1,J)))*RS1+
     .       (CG(I,2,J)-NU*(CG(I,2,J)+CG(I,2,J)))*ST1+
     .       (CG(I,3,J)-NU*(CG(I,3,J)+CG(I,3,J)))*RT1     
        J = 3
        QXZ =(CG(I,1,J)-NU*(CG(I,1,J)+CG(I,1,J)))*RS1+
     .       (CG(I,2,J)-NU*(CG(I,2,J)+CG(I,2,J)))*ST1+
     .       (CG(I,3,J)-NU*(CG(I,3,J)+CG(I,3,J)))*RT1     
C     
        SIG(I,1) = RXX*ETA+SXX*ZETA+TXX*KSI+QXX
        SIG(I,2) = RYY*ETA+SYY*ZETA+TYY*KSI+QYY
        SIG(I,3) = RZZ*ETA+SZZ*ZETA+TZZ*KSI+QZZ
        SIG(I,4) = RXY*ETA+SXY*ZETA+TXY*KSI+QXY
        SIG(I,5) = RYZ*ETA+SYZ*ZETA+TYZ*KSI+QYZ
        SIG(I,6) = RXZ*ETA+SXZ*ZETA+TXZ*KSI+QXZ
      ENDDO 
C
      RETURN
      END
Chd|====================================================================
Chd|  MATV66                        source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MATV66(CC ,CG  ,G33 ,V ,U  )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3),
     .   V(MVSIZ,6),U(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
C-----------------------------------------------
       DO I=LFT,LLT
        U(I,1) =CC(I,1,1)*V(I,1)+CC(I,1,2)*V(I,2)+CC(I,1,3)*V(I,3)
        U(I,2) =CC(I,2,1)*V(I,1)+CC(I,2,2)*V(I,2)+CC(I,2,3)*V(I,3)
        U(I,3) =CC(I,3,1)*V(I,1)+CC(I,3,2)*V(I,2)+CC(I,3,3)*V(I,3)
       END DO
       DO I=LFT,LLT
        U(I,1) =U(I,1)+CG(I,1,1)*V(I,4)+CG(I,1,2)*V(I,5)+CG(I,1,3)*V(I,6)
        U(I,2) =U(I,2)+CG(I,2,1)*V(I,4)+CG(I,2,2)*V(I,5)+CG(I,2,3)*V(I,6)
        U(I,3) =U(I,3)+CG(I,3,1)*V(I,4)+CG(I,3,2)*V(I,5)+CG(I,3,3)*V(I,6)
       END DO
       DO I=LFT,LLT
        U(I,4) =CG(I,1,1)*V(I,1)+CG(I,2,1)*V(I,2)+CG(I,3,1)*V(I,3)
        U(I,5) =CG(I,1,2)*V(I,1)+CG(I,2,2)*V(I,2)+CG(I,3,2)*V(I,3)
        U(I,6) =CG(I,1,3)*V(I,1)+CG(I,2,3)*V(I,2)+CG(I,3,3)*V(I,3)
       END DO
       DO I=LFT,LLT
        U(I,4) =U(I,4)+G33(I,1,1)*V(I,4)+G33(I,1,2)*V(I,5)+G33(I,1,3)*V(I,6)
        U(I,5) =U(I,5)+G33(I,2,1)*V(I,4)+G33(I,2,2)*V(I,5)+G33(I,2,3)*V(I,6)
        U(I,6) =U(I,6)+G33(I,3,1)*V(I,4)+G33(I,3,2)*V(I,5)+G33(I,3,3)*V(I,6)
       END DO
C      
      RETURN
      END
Chd|====================================================================
Chd|  MMODUL24C                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MMODUL24C(NEL    ,PM     ,DAM    ,CRAK   ,
     .                     CDAM   ,G3     ,ANG    ,DAMANG ,NBDAMA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NBDAMA
      my_real PM(NPROPM)
      my_real, DIMENSION(NEL,3)   ,INTENT(IN):: DAM,CRAK
      my_real, DIMENSION(NEL,6)   ,INTENT(IN):: ANG
      my_real, DIMENSION(MVSIZ,3,3) ,INTENT(OUT):: CDAM
      my_real, DIMENSION(MVSIZ,3) ,INTENT(OUT):: G3
      my_real, DIMENSION(MVSIZ,6) ,INTENT(OUT):: DAMANG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,NBDAMA0
      INTEGER, DIMENSION(NEL) ::  DAMAI,DAMA0I
      my_real  YOUNG,NU,G,A11,A12,DE1,DE2,DE3,DE4,DE5,DE6,DEN,
     .         SCAL1,SCAL2,SCAL3,D_MAX
C=======================================================================
c     DEi = (1 - DAMi)       coefficients d endommagement de la matrice Hook
c-----------------------------------------------------------------------
      YOUNG = PM(20)
      NU    = PM(21)
      G     = PM(22)
      A11   = PM(24)
      A12   = PM(25)
c
c     test of damaged elements
c
      NBDAMA  = 0
      NBDAMA0 = 0
      D_MAX=ZERO
      DO I = 1,NEL
         IF (DAM(I,1) + DAM(I,2) + DAM(I,3) > ZERO) THEN
           NBDAMA = NBDAMA + 1
           DAMAI(NBDAMA)=I
           D_MAX =MAX(D_MAX,DAM(I,1),DAM(I,2),DAM(I,3))
         ELSE
           NBDAMA0 = NBDAMA0 + 1 
           DAMA0I(NBDAMA0)=I
         ENDIF
      ENDDO
c-----------------------------------------------------------
c     DAMAGED ELEMENT GROUP => modified Hook matrix
c-----------------------------------------------------------
      DO K=1,NBDAMA
        I = DAMAI(K)
c
        DE1  = ONE - MAX( ZERO , SIGN(DAM(I,1),CRAK(I,1)) )
        DE2  = ONE - MAX( ZERO , SIGN(DAM(I,2),CRAK(I,2)) )
        DE3  = ONE - MAX( ZERO , SIGN(DAM(I,3),CRAK(I,3)) )
        SCAL1= HALF + SIGN(HALF,DE1-ONE)
        SCAL2= HALF + SIGN(HALF,DE2-ONE)
        SCAL3= HALF + SIGN(HALF,DE3-ONE)    
        DE4  = SCAL1*SCAL2
        DE5  = SCAL2*SCAL3
        DE6  = SCAL3*SCAL1
c
c       NOUVELLE MATRICE ELASTIQUE DE HOOKE ENDOMMAGEE
c
        DEN = ONE - NU**2 *(DE4 + DE5 + DE6
     .      + TWO*NU*SCAL1*SCAL2*SCAL3)
C
        CDAM(I,1,1) = YOUNG*DE1*(ONE - NU**2*DE5)/DEN
        CDAM(I,2,2) = YOUNG*DE2*(ONE - NU**2*DE6)/DEN
        CDAM(I,3,3) = YOUNG*DE3*(ONE - NU**2*DE4)/DEN
        CDAM(I,1,2) = NU*YOUNG*DE4 *(ONE + NU*SCAL3)/DEN
        CDAM(I,1,3) = NU*YOUNG*DE6 *(ONE + NU*SCAL2)/DEN
        CDAM(I,2,3) = NU*YOUNG*DE5 *(ONE + NU*SCAL1)/DEN
        CDAM(I,2,1) = CDAM(I,1,2)
        CDAM(I,3,1) = CDAM(I,1,3)
        CDAM(I,3,2) = CDAM(I,2,3)
c
        G3(I,1) = G*DE4
        G3(I,2) = G*DE5
        G3(I,3) = G*DE6
        DAMANG(I,1:6) = ANG(I,1:6)
      ENDDO
c------------------------------------------------------------------------
c     NON DAMAGED ELEMENT GROUP => initial Hook matrix
c------------------------------------------------------------------------
      DO K=1,NBDAMA0
        I = DAMA0I(K)
        CDAM(I,1,1) = A11
        CDAM(I,2,2) = A11
        CDAM(I,3,3) = A11
        CDAM(I,1,2) = A12
        CDAM(I,2,1) = A12
        CDAM(I,1,3) = A12
        CDAM(I,3,1) = A12
        CDAM(I,2,3) = A12
        CDAM(I,3,2) = A12
        G3(I,1:3) = G
        DAMANG(I,1) = ONE
        DAMANG(I,2) = ZERO
        DAMANG(I,3) = ZERO
        DAMANG(I,4) = ZERO
        DAMANG(I,5) = ONE
        DAMANG(I,6) = ZERO
      ENDDO
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  M24C2ARM                      source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GETTRANSV                     source/elements/solid/solidez/szhour3.F
Chd|        MSTIFORTHV                    source/elements/solid/solidez/szhour3.F
Chd|====================================================================
      SUBROUTINE M24C2ARM(NEL    ,GAMA  ,ANGDAM ,CC     ,G3     ,
     .                    C3    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real, DIMENSION(MVSIZ,6) ,INTENT(IN):: GAMA
      my_real, DIMENSION(MVSIZ,6) ,INTENT(IN):: ANGDAM
      my_real, DIMENSION(MVSIZ,3,3) ,INTENT(IN):: CC
      my_real, DIMENSION(MVSIZ,3) ,INTENT(IN):: G3
      my_real, DIMENSION(MVSIZ,3) ,INTENT(INOUT):: C3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   QC(MVSIZ,9),QCG(MVSIZ,9),QG(MVSIZ,9),
     .   QGC(MVSIZ,9),GG33(MVSIZ,3,3),QCA(MVSIZ,6),
     .   CCA(MVSIZ,3,3),CG(MVSIZ,3,3)
      my_real
     .   G11,G21,G31,G12,G22,G32,G13,G23,G33,
     .   R11,R21,R31,R12,R22,R32,R13,R23,R33
C=======================================================================
        DO I = 1,NEL
          G11=GAMA(I,1)
          G21=GAMA(I,2)
          G31=GAMA(I,3)
          G12=GAMA(I,4)
          G22=GAMA(I,5)
          G32=GAMA(I,6)
          G13=G21*G32-G31*G22
          G23=G31*G12-G11*G32
          G33=G11*G22-G21*G12
          R11=ANGDAM(I,1)
          R21=ANGDAM(I,2)
          R31=ANGDAM(I,3)
          R12=ANGDAM(I,4)
          R22=ANGDAM(I,5)
          R32=ANGDAM(I,6)
          R13=R21*R32-R31*R22
          R23=R31*R12-R11*R32
          R33=R11*R22-R21*R12
          QCA(I,1)=R11*G11+R21*G21+R31*G31
          QCA(I,2)=R11*G12+R21*G22+R31*G32
          QCA(I,3)=R11*G13+R21*G23+R31*G33
          QCA(I,4)=R12*G11+R22*G21+R32*G31
          QCA(I,5)=R12*G12+R22*G22+R32*G32
          QCA(I,6)=R12*G13+R22*G23+R32*G33
        END DO 
       CALL GETTRANSV(1,NEL,QCA,QC,QCG,QGC,QG)
       DO I = 1,NEL
        CCA(I,1:3,1:3)= CC(I,1:3,1:3)
       END DO 
       CALL MSTIFORTHV(1  ,NEL   ,QC  ,QCG  ,QGC    ,
     .                 QG ,CCA   ,G3  ,GG33  ,CG     ) 
       DO J = 1,3
       DO I = 1,NEL
        C3(I,J)= CC(I,J,J)
       END DO 
       END DO 
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  C33STIF2EL                    source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        MMODUL                        source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|        CBATRAN3V                     source/elements/solid/solidez/szhour3.F
Chd|====================================================================
      SUBROUTINE C33STIF2EL(NEL   ,QC   ,QCG   ,QGC    ,QG     ,
     .                      C33   ,CC   ,G33   ,CG     ) 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL  
C     REAL
      my_real
     .   QC(MVSIZ,3,3),QCG(MVSIZ,3,3),QGC(MVSIZ,3,3),QG(MVSIZ,3,3),
     .   G3(MVSIZ,3),C33(MVSIZ,3),CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ISYM
C     REAL
      my_real
     .   B(MVSIZ,3,3)
C-----------------------------------------------
       ISYM = 0
C      
       DO I=1,NEL
        B(I,1:3,1:3)=ZERO
       ENDDO
       DO J= 1,3
        DO I=1,NEL
          B(I,J,J)=C33(I,J)
        ENDDO
       ENDDO
      CALL CBATRAN3V(1,NEL    ,QC   ,B ,QCG,ISYM) 
C      
       DO J= 1,3
       DO K= 1,3
        DO I=1,NEL
          CG(I,J,K)=HALF*B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C---------------[G']->[G33]----
       ISYM = 1
       DO I=1,NEL
        B(I,1:3,1:3)=ZERO
       ENDDO
       DO J= 1,3
        DO I=1,NEL
          B(I,J,J)=C33(I,J)
        ENDDO
       ENDDO
       CALL CBATRAN3V(1,NEL    ,QCG   ,B ,QCG,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=1,NEL
          G33(I,J,K)=FOURTH*B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C---------------[C']->[CC]----
       DO I=1,NEL
        B(I,1:3,1:3)=ZERO
       ENDDO
       DO J= 1,3
        DO I=1,NEL
          B(I,J,J)=C33(I,J)
        ENDDO
       ENDDO
       CALL CBATRAN3V(1,NEL   ,QC   ,B ,QC,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=1,NEL
          CC(I,J,K)=B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C       
       DO J= 1,3
       DO K= J+1,3
        DO I=1,NEL
          CC(I,K,J)=CC(I,J,K)
          G33(I,K,J)=G33(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C      
      RETURN
      END

Chd|====================================================================
Chd|  SZSTRAINHG                    source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSTRAINHG(JR_1,JS_1,JT_1,STRHG,NEL  ,
     .                      HGX1, HGX2, HGX3, HGX4,
     .                      HGY1, HGY2, HGY3, HGY4,
     .                      HGZ1, HGZ2, HGZ3, HGZ4,NU,NU1)
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
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   HGX1(MVSIZ), HGX2(MVSIZ), HGX3(MVSIZ), HGX4(MVSIZ),
     .   HGY1(MVSIZ), HGY2(MVSIZ), HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ1(MVSIZ), HGZ2(MVSIZ), HGZ3(MVSIZ), HGZ4(MVSIZ),
     .   JR_1(*),JS_1(*),JT_1(*) ,STRHG(NEL,3,6),NU(*),NU1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,IR,IS,IT,IP
      my_real
     .   HGX(MVSIZ,4), HGY(MVSIZ,4), HGZ(MVSIZ,4),
     .   KSI,ETA,ZETA,COEF,NJR_1(MVSIZ),NJS_1(MVSIZ),NJT_1(MVSIZ),
     .   JSR_1(MVSIZ),JRS_1(MVSIZ),JTR_1(MVSIZ),JRT_1(MVSIZ),JTS_1(MVSIZ),
     .   JST_1(MVSIZ),F1,F2,F3,F1X,F2Y,F3Z
C-----------------------------------------------
      COEF = EIGHT
      DO I=LFT,LLT
       NJR_1(I) = COEF*JR_1(I)*DT1
       NJS_1(I) = COEF*JS_1(I)*DT1
       NJT_1(I) = COEF*JT_1(I)*DT1
      ENDDO
      DO I=LFT,LLT
       HGX(I,1) = NJR_1(I)*HGX1(I)
       HGX(I,2) = NJR_1(I)*HGX2(I)
       HGX(I,3) = NJR_1(I)*HGX3(I)
       HGX(I,4) = NJR_1(I)*HGX4(I)
       HGY(I,1) = NJS_1(I)*HGY1(I)
       HGY(I,2) = NJS_1(I)*HGY2(I)
       HGY(I,3) = NJS_1(I)*HGY3(I)
       HGY(I,4) = NJS_1(I)*HGY4(I)
       HGZ(I,1) = NJT_1(I)*HGZ1(I)
       HGZ(I,2) = NJT_1(I)*HGZ2(I)
       HGZ(I,3) = NJT_1(I)*HGZ3(I)
       HGZ(I,4) = NJT_1(I)*HGZ4(I)
      ENDDO
C------ STRHG(NEL,3,j) : J=1,3: term ksi,eta,zeta of Eii; J=4: shear;   
C------                  J=5: mode 4 HGi4; J=6: nu*HGi4;   
         DO I=LFT,LLT
          F1X = HGX(I,4)
          F2Y = HGY(I,4)
          F3Z = HGZ(I,4)
          STRHG(I,1,1)= STRHG(I,1,1)-NU1(I)*(HGY(I,3)+HGZ(I,2))
          STRHG(I,1,2)= STRHG(I,1,2)+HGX(I,3)
          STRHG(I,1,3)= STRHG(I,1,3)+HGX(I,2)
          STRHG(I,1,4)= STRHG(I,1,4)+HGX1(I)*NJS_1(I)+HGY2(I)*NJR_1(I)
          STRHG(I,1,5)= STRHG(I,1,5)+F1X
          STRHG(I,1,6)= STRHG(I,1,6)-NU(I)*F1X
          STRHG(I,2,1)= STRHG(I,2,1)+HGY(I,3)
          STRHG(I,2,2)= STRHG(I,2,2)-NU1(I)*(HGX(I,3)+HGZ(I,1))
          STRHG(I,2,3)= STRHG(I,2,3)+HGY(I,1)
          STRHG(I,2,4)= STRHG(I,2,4)+HGX1(I)*NJT_1(I)+HGZ3(I)*NJR_1(I)
          STRHG(I,2,5)= STRHG(I,2,5)+F2Y
          STRHG(I,2,6)= STRHG(I,2,6)-NU(I)*F2Y
          STRHG(I,3,1)= STRHG(I,3,1)+HGZ(I,2)
          STRHG(I,3,2)= STRHG(I,3,2)+HGZ(I,1)
          STRHG(I,3,3)= STRHG(I,3,3)-NU1(I)*(HGX(I,2)+HGY(I,1))
          STRHG(I,3,4)= STRHG(I,3,4)+HGY2(I)*NJT_1(I)+HGZ3(I)*NJS_1(I)
          STRHG(I,3,5)= STRHG(I,3,5)+F3Z
          STRHG(I,3,6)= STRHG(I,3,6)-NU(I)*F3Z
         END DO
C      
      RETURN
      END
Chd|====================================================================
Chd|  SZSTRAINGPS                   source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        GPSSTRAIN_SKIN                source/output/anim/generate/tensgpstrain.F
Chd|        STRN_TENSCOR3                 source/output/h3d/h3d_results/h3d_strn_tenscor3.F
Chd|        TENSGPSTRAIN                  source/output/anim/generate/tensgpstrain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSTRAINGPS(STRAIN,STR_PI,STRHG,NEL  )
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
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   STRAIN(NEL,6) ,STR_PI(MVSIZ,6,8),STRHG(NEL,3,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,IR,IS,IT,IP
      my_real
     .   KSI,ETA,ZETA,COEF,F1,F2,F3,F1X,F2Y,F3Z
      my_real
     .  A_GAUSS(9,9)
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
      
C-----------------------------------------------
      DO IR=1,2
       DO IS=1,2
        DO IT=1,2
         IP = IR + ( (IS-1) + (IT-1)*2 )*2
         KSI = A_GAUSS(IR,2)
         ETA = A_GAUSS(IS,2)
         ZETA = A_GAUSS(IT,2)
         F1 = ETA*ZETA
         F2 = KSI*ZETA
         F3 = ETA*KSI
C
         DO I=LFT,LLT
          STR_PI(I,1,IP)=STRAIN(I,1)+
     +                   STRHG(I,1,1)*KSI+STRHG(I,1,2)*ETA+STRHG(I,1,3)*ZETA+
     +                   STRHG(I,1,5)*F1+STRHG(I,2,6)*F2+STRHG(I,3,6)*F3
          STR_PI(I,2,IP)=STRAIN(I,2)+
     +                   STRHG(I,2,1)*KSI+STRHG(I,2,2)*ETA+STRHG(I,2,3)*ZETA+
     +                   STRHG(I,2,5)*F2+STRHG(I,1,6)*F1+STRHG(I,3,6)*F3
          STR_PI(I,3,IP)=STRAIN(I,3)+
     +                   STRHG(I,3,1)*KSI+STRHG(I,3,2)*ETA+STRHG(I,3,3)*ZETA+
     +                   STRHG(I,3,5)*F3+STRHG(I,1,6)*F1+STRHG(I,2,6)*F2
          STR_PI(I,4,IP)=STRAIN(I,4)+STRHG(I,1,4)*ZETA
          STR_PI(I,6,IP)=STRAIN(I,5)+STRHG(I,2,4)*ETA
          STR_PI(I,5,IP)=STRAIN(I,6)+STRHG(I,3,4)*KSI
         END DO
        ENDDO 
       ENDDO 
      ENDDO
C      
      RETURN
      END
