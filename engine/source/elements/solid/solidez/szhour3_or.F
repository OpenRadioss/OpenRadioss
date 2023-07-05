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
Chd|  SZHOUR3_OR                    source/elements/solid/solidez/szhour3_or.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        GFHOUR_OR                     source/elements/solid/solidez/gfhour_or.F
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|        MMODUL                        source/elements/solid/solidez/mmodul.F
Chd|        MMOD_NORM                     source/elements/solid/solidez/mmod_norm.F
Chd|        SZETFAC                       source/elements/solid/solidez/szetfac.F
Chd|        SZSTRAINHG                    source/elements/solid/solidez/szstrainhg.F
Chd|        SZSVM_OR                      source/elements/solid/solidez/szsvm_or.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SZHOUR3_OR(
     1   ELBUF_STR,PM,       RHO,      OFF,
     2   VX1,      VX2,      VX3,      VX4,
     3   VX5,      VX6,      VX7,      VX8,
     4   VY1,      VY2,      VY3,      VY4,
     5   VY5,      VY6,      VY7,      VY8,
     6   VZ1,      VZ2,      VZ3,      VZ4,
     7   VZ5,      VZ6,      VZ7,      VZ8,
     8   F11,      F21,      F31,      F12,
     9   F22,      F32,      F13,      F23,
     A   F33,      F14,      F24,      F34,
     B   F15,      F25,      F35,      F16,
     C   F26,      F36,      F17,      F27,
     D   F37,      F18,      F28,      F38,
     E   PX1H1,    PX1H2,    PX1H3,    PX1H4,
     F   PX2H1,    PX2H2,    PX2H3,    PX2H4,
     G   PX3H1,    PX3H2,    PX3H3,    PX3H4,
     H   PX4H1,    PX4H2,    PX4H3,    PX4H4,
     I   VOL,      MAT,      CXX,      VIS,
     J   VD2,      DELTAX,   PID,      GEO,
     K   PARTSAV,  IPARTS,   DXX,      DYY,
     L   DZZ,      D4,       D5,       D6,
     M   FHOUR,    JR0,      JS0,      JT0,
     N   EINT,     VOL0,     SIGY,     SIG0,
     O   SIGOLD,   ICP,      DEFP,     MATVIS,
     P   ET,       D_MAX,    NEL,      GAMA,
     Q   UPARAM,   STRHG,    STRAIN,   ISTRAIN,
     R   MTN,      ISMSTR,   JLAG,     IINT)
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JLAG
      INTEGER, INTENT(IN) :: IINT
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
      INTEGER I, MX, J,K,IET, MT,IPLAST
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
       IPLAST = ELBUF_STR%GBUF%G_PLA
C---- attention : r->eta; s->zeta; t->ksi------------

C +++ MAT Visco-----
!      DO I=1,NEL
!        SIG0V(I,1) = SIG0(I,1)
!        SIG0V(I,2) = SIG0(I,2)
!        SIG0V(I,3) = SIG0(I,3)
!        SIG0V(I,4) = SIG0(I,4)
!        SIG0V(I,5) = SIG0(I,5)
!        SIG0V(I,6) = SIG0(I,6)
!      ENDDO
      MX = MAT(1)
      RHO0=PM(1,MX)
      NU=PM(21,MX)
      CALL MMODUL(1     ,NEL     ,PM    ,MAT    ,MTN    ,
     .            GAMA    ,UPARAM  ,CC    ,CG     ,G33    )
      DO I=1,NEL
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
c       MX = MAT(1)
c       DO I=1,NEL
c        GG(I)=PM(22,MX)
c        GG(I)=MAX(GG(I),VIS(I))
c       ENDDO
c      ENDIF 
C
      IF (IET > 1 .AND. MATVIS>0 ) THEN
       CALL SZETFAC(1,NEL,IET,MTN,ET,GG )
      ELSEIF (MATVIS==1.AND.ISMSTR<10) THEN
       DO I=1,NEL
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
        MT = PID(1)
        DO I=1,NEL
          CAQ(I)=FOURTH*OFF(I)*GEO(13,MT)
        ENDDO
      ELSE
        MX = MAT(1)
        DO I=1,NEL
          CAQ(I)=FOURTH*OFF(I)*PM(4,MX)
        ENDDO
      ENDIF
      DO I=1,NEL
        G_3DT(I)=THIRD*OFF(I)*GG(I)*DT1
      ENDDO
C---------normalized by GG-------      
      CALL MMOD_NORM(1,NEL  ,GG    ,CC     ,CG    ,G33     )
C
C MATERIAUX NON FLUIDES CXX->SPP
      IF (IET > 1 ) THEN
C------now when IHKT=2, w.r.t. Isolid=1, fac=0.2*0.006666 ~0.001     
       DO I=1,NEL
         FCL(I)=ONEP1*CAQ(I)*RHO(I)*VOL(I)**THIRD
         FCL(I)=ZEP00666666667*FCL(I)*CXX(I)
       ENDDO
      ELSE
       DO I=1,NEL
         FCL(I)=CAQ(I)*RHO(I)*VOL(I)**THIRD
         FCL(I)=ZEP00666666667*FCL(I)*CXX(I)
       ENDDO
      END IF
C
c      IF(ICP==1)THEN
C-------!!!curieux!!!!!!!!
c       DO I=1,NEL
c        NU1(I) =FOUR_OVER_3
c        NU2(I) =-TWO_THIRD
c        NU3(I) =ZEP444
c        NU4(I) =ZERO
c       ENDDO
c      ELSEIF(ICP==2)THEN
c       DO I=1,NEL
c        FAC1 = SIGY(I)/E0(I)+DEFP(I)
c        FAC2=ONE-DEFP(I)/FAC1
c        FF =(ONE +NU)/(ONE -TWO*NU)*FAC2
c        NU1(I) =TWO_THIRD*(TWO+FF)
c        NU2(I) =TWO_THIRD*(FF-ONE)
c        NU3(I) =ZEP222*(TWO+FF)
c        NU4(I) =ZERO
c       ENDDO
c      ELSE
c       DO I=1,NEL
c        NU1(I) =TWO/(ONE-NU)
c        NU2(I) =NU*NU1(I)
c        NU3(I) =TWO_THIRD*(ONE + NU)
c        NU4(I) =NU
c       ENDDO
c      ENDIF
      IF(ICP==1)THEN
       DO I=1,NEL
        NUS(I) =ZEP499
       ENDDO
      ELSEIF(ICP==2.AND.IPLAST>0)THEN
       DO I=1,NEL
        FAC1 = SIGY(I)/E0(I)+DEFP(I)
        FAC2 = DEFP(I)/FAC1
        NUS(I)=NU+(HALF-NU)*FAC2
       ENDDO
      ELSE
       DO I=1,NEL
        NUS(I) =NU
       ENDDO
      ENDIF
      DO I=1,NEL
       NU2(I) =NUS(I)/(ONE-NUS(I))
       NU4(I) =NUS(I)
      ENDDO
      DT05 =HALF*DT1
      DO I=1,NEL
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
      DO I=1,NEL
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
      DO I=1,NEL
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
       DO I=1,NEL
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
      IF (IPLAST==1) 
     .   CALL SZSVM_OR(
     1   JR0,     JS0,     JT0,     CC,
     2   CG,      G33,     FHOUR,   SIGY,
     3   SIGOLD,  NU,      SMO1,    SMO2,
     4   NEL,     IINT)
      !-----------For energy calculation------------
      IF(JLAG==1)THEN
       CALL GFHOUR_OR(1,NEL,
     .   FHOUR,JR0,JS0,JT0,FCL,
     .   HGX1, HGX2, HGX3, HGX4,
     .   HGY1, HGY2, HGY3, HGY4,
     .   HGZ1, HGZ2, HGZ3, HGZ4,
     .   H11 , H22 , H33 , 
     .   H12 , H13 , H23 , 
     .   JR_1,JS_1 , JT_1, NU4,NU2 ,
     .   CC  ,CG   ,G33  ,NFHOUR,NEL)
C
       DO I=1,NEL
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
       CALL MDAMA24(ELBUF_STR,1,NEL ,PM    ,MAT    ,DAMA_G )
       DO J=1,3
        DO I=1,NEL
         FAC1=ONE- DAMA_G(I,J)
         FHOUR(I,J,1:4) = FHOUR(I,J,1:4)*FAC1
        ENDDO
       ENDDO
      END IF !(IET > 1) THEN
C   -------elstic increament----
      DO I=1,NEL
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
      IF (IPLAST==1) 
     .    CALL SZSVM_OR(
     1   JR0,     JS0,     JT0,     CC,
     2   CG,      G33,     FHOUR,   SIGY,
     3   SIG0,    NU,      SM1,     SM2,
     4   NEL,     IINT)
      !-----------ELASTIC-PLASTIC yield criterion------------
      IF (IPLAST==1) THEN
      DO I=1,NEL
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
       CALL GFHOUR_OR(1,NEL,
     .   FHOUR,JR0,JS0,JT0,FCL,
     .   HGX1, HGX2, HGX3, HGX4,
     .   HGY1, HGY2, HGY3, HGY4,
     .   HGZ1, HGZ2, HGZ3, HGZ4,
     .   H11 , H22 , H33 , 
     .   H12 , H13 , H23 , 
     .   JR_1,JS_1 , JT_1, NU4,NU2 ,
     .   CC  ,CG   ,G33  ,NFHOUR,NEL)
      DO I=1,NEL
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
      DO I=1,NEL
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
      DO I=1,NEL
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
      !----hourglass energy is included in internal energy------
      IF(JLAG==1)THEN
       DO I=1,NEL
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
       CALL SZSTRAINHG(
     1   JR_1,    JS_1,    JT_1,    STRHG,
     2   NEL,     HGX1,    HGX2,    HGX3,
     3   HGX4,    HGY1,    HGY2,    HGY3,
     4   HGY4,    HGZ1,    HGZ2,    HGZ3,
     5   HGZ4,    NU4,     NU2)
c       CALL PRSTRAIN(STRAIN,STRHG,CC ,CG  ,G33, GG,NEL  )
      ENDIF
C
      RETURN
      END
