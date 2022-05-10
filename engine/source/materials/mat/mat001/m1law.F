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
Chd|  M1LAW                         source/materials/mat/mat001/m1law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|====================================================================
      SUBROUTINE M1LAW (PM,OFF   ,SIG     ,EINT    ,RHO     ,
     2           QOLD   ,VOL     ,STIFN   ,DT2T    ,NELTST  ,
     3           ITYPTST,OFFG    ,GEO     ,PID     ,
     5           MUMAX  ,MAT     ,NGL     ,SSP     ,DVOL    ,
     6           AIRE   ,VNEW    ,VD2     ,DELTAX  ,VIS     ,  
     7           D1     ,D2      ,D3      ,D4      ,D5      ,
     8           D6     ,PNEW    ,PSH     ,QNEW    ,SSP_EQ  ,
     9           SOLD1  ,SOLD2   ,SOLD3   ,SOLD4   ,SOLD5   ,
     A           SOLD6  ,MSSA    ,DMELS   ,CONDE   ,AMU     ,
     B           VOL_AVG,DTEL    ,G_DT    ,NEL     ,IPM     ,
     C           RHOREF ,RHOSP   )
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NELTST,ITYPTST,PID(*),G_DT,NEL
      INTEGER MAT(*),NGL(*), IPM(NPROPMI,*)
      my_real
     .   DT2T
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), QOLD(*),
     .   VOL(*),STIFN(*), OFFG(*),GEO(NPROPG,*),MUMAX(*)
      my_real
     .   VNEW(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .   PSH(*), PNEW(*),QNEW(*) ,SSP_EQ(*), DVOL(*),
     .   SOLD1(*), SOLD2(*), SOLD3(*), SOLD4(*), SOLD5(*), SOLD6(*),
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   MSSA(*), DMELS(*),CONDE(*),AMU(*),VOL_AVG(*),DTEL(*), RHOREF(*), RHOSP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,IBID
      my_real
     .   RHO0(MVSIZ), 
     .   G(MVSIZ), G1(MVSIZ), G2(MVSIZ),
     .   C1(MVSIZ),
     .   DF,DAV, DPDM, P,
     .   E1, E2, E3, E4, E5, E6, EINC, P2,
     .    BID1, BID2, BID3, DTA,  YM, DPDMP,FACQ0,
     .    RHO0_1,C1_1
C-----------------------------------------------
      FACQ0 = UN
      MX = MAT(LFT)
      RHO0_1 =PM( 1,MX)
      C1_1   =PM(32,MX)
      DO 10 I=LFT,LLT
      RHO0(I) =RHO0_1
      G(I)    =PM(22,MX)*OFF(I)
      C1(I)   =C1_1
   10 CONTINUE
C
      DO I=LFT,LLT
       G1(I)=DT1*G(I)
       G2(I)=DEUX*G1(I)
       !-----------------------
       !     CALCUL DE DP/DRHO
       !-----------------------
       !     P = C1 mu, mu = rho/rho0-1 (cf Hydrodynamic pressure)
       !     d(rho)/d(mu)  = rho0
       !-----------------------
       SSP(I)  =SQRT((UNP333*G(I)+C1(I))/RHO0(I))
       RHOSP(I)=RHO0(I)
       !-------------------------------
       !     CONTRAINTES DEVIATORIQUES
       !-------------------------------
       P  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
       DAV=-THIRD*(D1(I)+D2(I)+D3(I))
       SIG(I,1)=SIG(I,1)+P+G2(I)*(D1(I)+DAV)
       SIG(I,2)=SIG(I,2)+P+G2(I)*(D2(I)+DAV)
       SIG(I,3)=SIG(I,3)+P+G2(I)*(D3(I)+DAV)
       SIG(I,4)=SIG(I,4)+G1(I)*D4(I)
       SIG(I,5)=SIG(I,5)+G1(I)*D5(I)
       SIG(I,6)=SIG(I,6)+G1(I)*D6(I)
      ENDDO

      IF (JSPH.EQ.0)THEN
       CALL MQVISCB(PM,OFF     ,RHO    ,BID1   ,BID2    ,SSP    ,
     2        BID3    ,STIFN   ,DT2T   ,NELTST ,ITYPTST ,AIRE   ,
     3        OFFG    ,GEO     ,PID    ,VNEW   ,VD2     ,DELTAX , 
     4        VIS     ,D1      ,D2     ,D3     ,PNEW    ,PSH    ,
     5        MAT     ,NGL     ,QNEW    ,SSP_EQ ,VOL    ,MSSA   ,
     6        DMELS   ,IBID    ,FACQ0   ,CONDE  ,DTEL   ,G_DT   ,
     7        IPM     ,RHOREF  ,RHOSP   )
      ELSE
       CALL MDTSPH(PM ,OFF     ,RHO     ,BID1    ,BID2    ,
     2        BID3    ,STIFN   ,DT2T    ,NELTST  ,ITYPTST ,
     3        OFFG    ,GEO     ,PID     ,MUMAX   ,SSP     ,
     4        VNEW    ,VD2     ,DELTAX  ,VIS     ,D1      ,
     5        D2      ,D3      ,PNEW    ,PSH     ,MAT     ,
     6        NGL     ,QNEW    ,SSP_EQ  ,G_DT    ,DTEL)
      ENDIF

      DTA =UNDEMI*DT1

      DO I=LFT,LLT
       PNEW(I)=C1(I)*AMU(I)
       SIG(I,1)=(SIG(I,1)-PNEW(I))*OFF(I)
       SIG(I,2)=(SIG(I,2)-PNEW(I))*OFF(I)
       SIG(I,3)=(SIG(I,3)-PNEW(I))*OFF(I)
       SIG(I,4)=SIG(I,4)*OFF(I)
       SIG(I,5)=SIG(I,5)*OFF(I)
       SIG(I,6)=SIG(I,6)*OFF(I)
       P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))* THIRD 
       E1=D1(I)*(SOLD1(I)+SIG(I,1)+P2)
       E2=D2(I)*(SOLD2(I)+SIG(I,2)+P2)
       E3=D3(I)*(SOLD3(I)+SIG(I,3)+P2)
       E4=D4(I)*(SOLD4(I)+SIG(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6))
       EINC= VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - UNDEMI*DVOL(I)*(QOLD(I)+QNEW(I)+P2)
       EINT(I)=(EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL(I))
       QOLD(I)=QNEW(I)
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  M1LAWI                        source/materials/mat/mat001/m1law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|====================================================================
      SUBROUTINE M1LAWI (PM,OFF   ,SIG     ,EINT    ,RHO     ,
     2           QOLD   ,VOL     ,STIFN   ,DT2T    ,NELTST  ,
     3           ITYPTST,OFFG    ,GEO     ,PID     ,
     5           MUMAX  ,MAT     ,NGL     ,SSP     ,DVOL    ,
     6           AIRE   ,VNEW    ,VD2     ,DELTAX  ,VIS     ,  
     7           D1     ,D2      ,D3      ,D4      ,D5      ,
     8           D6     ,PNEW    ,PSH     ,QNEW    ,SSP_EQ  ,
     9           SOLD1  ,SOLD2   ,SOLD3   ,SOLD4   ,SOLD5   ,
     A           SOLD6  ,MSSA    ,DMELS   ,CONDE   ,AMU     ,
     B           VOL_AVG,DTEL    ,G_DT    ,NEL     ,IPM     ,
     C           RHOREF ,RHOSP  )
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NELTST,ITYPTST,PID(*),G_DT,NEL
      INTEGER MAT(*),NGL(*), IPM(NPROPMI,*)
      my_real
     .   DT2T

      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), QOLD(*),
     .   VOL(*),STIFN(*), OFFG(*),GEO(NPROPG,*),MUMAX(*)
      my_real
     .   VNEW(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .   PSH(*), PNEW(*),QNEW(*) ,SSP_EQ(*), DVOL(*),
     .   SOLD1(*), SOLD2(*), SOLD3(*), SOLD4(*), SOLD5(*), SOLD6(*),
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   MSSA(*), DMELS(*),CONDE(*),AMU(*),VOL_AVG(*),DTEL(*), RHOREF(*), RHOSP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,IBID
      my_real
     .   RHO0(MVSIZ), 
     .   G(MVSIZ), G1(MVSIZ), G2(MVSIZ),
     .   C1(MVSIZ),
     .   DF,DAV, DPDM, P,
     .   E1, E2, E3, E4, E5, E6, EINC, P2,
     .   BID1, BID2, BID3, DTA,  YM, DPDMP,FACQ0,
     .   RHO0_1,C1_1
      my_real
     .    lmb(mvsiz),epsm 
C-----------------------------------------------
      FACQ0 = UN
      MX = MAT(LFT)
      RHO0_1 =PM( 1,MX)
      C1_1   =PM(32,MX)
      DO 10 I=LFT,LLT
      RHO0(I) =RHO0_1
      G(I)    =PM(22,MX)*OFF(I)
      C1(I)   =C1_1
      lmb(i)  =(trois*c1(i)-deux*g(i))
   10 CONTINUE
C
       !print*,' sig avant '
       !write(*,'(6(1X,1pe10.3))')sig(10,1:6)
       !print*,' sold '
       !write(*,'(6(1X,1pe10.3))')sold1(10),sold2(10),sold3(10),sold4(10),sold5(10),sold6(10)
       !print*, 'dt1 ',dt1

      DO I=LFT,LLT
       G1(I)=DT1*G(I)
       G2(I)=DEUX*G1(I)
       !-----------------------
       !     CALCUL DE DP/DRHO 
       !-----------------------
       !     P = C1 mu, mu = rho/rho0-1
       !     d(rho)/d(mu)  = rho0
       !-----------------------
       SSP(I)=SQRT((UNP333*G(I)+C1(I))/RHO0(I))
       RHOSP(I)=RHO0(I)
       !-------------------------------
       !     CONTRAINTES DEVIATORIQUES
       !-------------------------------
       P  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
       DAV=-THIRD*(D1(I)+D2(I)+D3(I))
      ENDDO

      IF (JSPH.EQ.0)THEN
       CALL MQVISCB(PM,OFF     ,RHO    ,BID1   ,BID2    ,SSP    ,
     2        BID3    ,STIFN   ,DT2T   ,NELTST ,ITYPTST ,AIRE   ,
     3        OFFG    ,GEO     ,PID    ,VNEW   ,VD2     ,DELTAX ,
     4        VIS     ,D1      ,D2     ,D3     ,PNEW    ,PSH    ,
     5        MAT     ,NGL     ,QNEW   ,SSP_EQ ,VOL     ,MSSA   ,
     6        DMELS   ,IBID    ,FACQ0  ,CONDE  ,DTEL    ,G_DT   ,
     7        IPM     ,RHOREF  ,RHOSP   )
      ELSE
       CALL MDTSPH(PM ,OFF     ,RHO     ,BID1    ,BID2    ,
     2        BID3    ,STIFN   ,DT2T    ,NELTST  ,ITYPTST ,
     3        OFFG    ,GEO     ,PID     ,MUMAX   ,SSP     ,
     4        VNEW    ,VD2     ,DELTAX  ,VIS     ,D1      ,
     5        D2      ,D3      ,PNEW    ,PSH     ,MAT     ,
     6        NGL     ,QNEW    ,SSP_EQ  ,G_DT    ,DTEL)
      ENDIF

      DTA =UNDEMI*DT1

      DO I=LFT,LLT
       epsm = third*(D1(i)+d2(i)+d3(i))
       sig(i,1)=sig(i,1)+ (DEUX*G(i)*d1(i)+lmb(i)*epsm) *dt1
       sig(i,2)=sig(i,2)+ (DEUX*G(i)*d2(i)+lmb(i)*epsm) *dt1
       sig(i,3)=sig(i,3)+ (DEUX*G(i)*d3(i)+lmb(i)*epsm) *dt1
       sig(i,4)=sig(i,4)+ G(i)*d4(i) *dt1
       sig(i,5)=sig(i,5)+ G(i)*d5(i) *dt1
       sig(i,6)=sig(i,6)+ G(i)*d6(i) *dt1
       pnew(i) =- (sig(i,1)+sig(i,2)+sig(i,3))
       P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))* THIRD 
       E1=D1(I)*(SOLD1(I)+SIG(I,1)+P2)
       E2=D2(I)*(SOLD2(I)+SIG(I,2)+P2)
       E3=D3(I)*(SOLD3(I)+SIG(I,3)+P2)
       E4=D4(I)*(SOLD4(I)+SIG(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6))
       EINC= VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - UNDEMI*DVOL(I)*(QOLD(I)+QNEW(I)+P2)
       EINT(I)=(EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL(I))
       QOLD(I)=QNEW(I)
       
       !if (i == llt) then
       !print*,' DDD '
       !write(*,'(6(1X,1pe10.3))')d1(i),d2(i),d3(i),d4(i),d5(i),d6(i)
       !print*,' sig m1lawi '
       !write(*,'(6(1X,1pe10.3))')sig(i,1:6)
       !endif
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  M1LAWTOT                      source/materials/mat/mat001/m1law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        M1ISMSTR10_PON                source/materials/mat/mat001/m1law.F
Chd|        M1ISMSTR11                    source/materials/mat/mat001/m1law.F
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|====================================================================
      SUBROUTINE M1LAWTOT (PM,OFF   ,SIG     ,EINT    ,RHO     ,
     2           QOLD   ,VOL     ,STIFN   ,DT2T    ,NELTST  ,
     3           ITYPTST,OFFG    ,GEO     ,PID     ,AMU     ,
     5           MUMAX  ,MAT     ,NGL     ,SSP     ,DVOL    ,
     6           AIRE   ,VNEW    ,VD2     ,DELTAX  ,VIS     ,  
     7           D1     ,D2      ,D3      ,D4      ,D5      ,
     8           D6     ,PNEW    ,PSH     ,QNEW    ,SSP_EQ  ,
     9           SOLD1  ,SOLD2   ,SOLD3   ,SOLD4   ,SOLD5   ,
     A           SOLD6  ,MSSA    ,DMELS   ,CONDE   ,MFXX    ,
     B           MFXY   ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    , 
     C           MFZX   ,MFZY    ,MFZZ    ,OFFG0   ,VOL_AVG ,
     D           EPSTH  ,DTEL    ,G_DT    ,NEL     ,ETOTSH  ,
     E           ISELECT,IPM     ,RHOREF  ,RHOSP   ,SIGL   )
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NELTST,ITYPTST,PID(*),G_DT,NEL,ISELECT
      INTEGER MAT(*),NGL(*),IPM(NPROPMI,*)
      my_real
     .   DT2T

      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), QOLD(*),
     .   VOL(*),STIFN(*), OFFG(*),GEO(NPROPG,*),MUMAX(*),SIGL(NEL,6)
      my_real
     .   VNEW(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .   PSH(*), PNEW(*),QNEW(*) ,SSP_EQ(*), DVOL(*),
     .   SOLD1(*), SOLD2(*), SOLD3(*), SOLD4(*), SOLD5(*), SOLD6(*),
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   MSSA(*), DMELS(*),CONDE(*),
     .   MFXX(*)  ,MFXY(*)  ,MFXZ(*)  ,MFYX(*)  ,MFYY(*)  ,
     .   MFYZ(*)  ,MFZX(*)  ,MFZY(*)  ,MFZZ(*)  ,OFFG0(*) ,VOL_AVG(*),
     .   EPSTH(*),DTEL(*),ETOTSH(NEL,6), RHOREF(*), RHOSP(*),AMU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,IBID,NLAR,INDEX(MVSIZ),NSM
      my_real
     .   RHO0(MVSIZ), 
     .   G(MVSIZ), G1(MVSIZ), G2(MVSIZ),
     .   C1(MVSIZ), 
     .   ES1(MVSIZ), ES2(MVSIZ), ES3(MVSIZ), 
     .   ES4(MVSIZ), ES5(MVSIZ), ES6(MVSIZ),
     .    DF,DAV,EKK, DPDM, P,
     .    E1, E2, E3, E4, E5, E6, EINC,
     .    BID1, BID2, BID3, DTA,  YM, DPDMP,FACQ0,
     .    RHO0_1,C1_1,G2_11,
     .    SV(3),P3,P2,
     .    EPSTH11(MVSIZ),SIGTMP(MVSIZ,6)
C-----------------------------------------------
      FACQ0 = UN
      MX = MAT(LFT)
      RHO0_1 =PM( 1,MX)
      C1_1   =PM(32,MX)
      G2_11 = DEUX*PM(22,MX)
      DO I=LFT,LLT
        RHO0(I) =RHO0_1
        G(I)    =PM(22,MX)*OFF(I)
        C1(I)   =C1_1
      ENDDO

C------------[F] = [MF]+ 1 ----ISMSTR10,11,12 
      IF (ISMSTR == 10) THEN
       IF (ISELECT>0) THEN
        DO I=LFT,LLT
           ES1(I)=ETOTSH(I,1)
           ES2(I)=ETOTSH(I,2)
           ES3(I)=ETOTSH(I,3)
           ES4(I)=ETOTSH(I,4)
           ES6(I)=ETOTSH(I,6)
           ES5(I)=ETOTSH(I,5)
        ENDDO
       ELSE
        DO I=LFT,LLT
           ES1(I)=MFXX(I)*(DEUX+MFXX(I))+MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I)
           ES2(I)=MFYY(I)*(DEUX+MFYY(I))+MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
           ES3(I)=MFZZ(I)*(DEUX+MFZZ(I))+MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
           ES4(I)=MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I)
           ES6(I)=MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I)
           ES5(I)=MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I)
        ENDDO
       END IF !(ISELECT>0) THEN
        CALL M1ISMSTR10_PON(NEL   , ES1 , ES2 ,ES3 ,ES4  ,
     1                     ES5   ,ES6  ,EPSTH, C1  ,G   ,SIGTMP)
        SIG(LFT:LLT,1:6)=SIGTMP(LFT:LLT,1:6)
      ELSEIF (ISMSTR == 12) THEN
        NLAR = 0
       IF (ISELECT>0) THEN
        DO I=LFT,LLT
          IF (OFFG0(I)<=UN) THEN
           NLAR =NLAR+1
           INDEX(NLAR)=I
           ES1(NLAR)=ETOTSH(I,1)
           ES2(NLAR)=ETOTSH(I,2)
           ES3(NLAR)=ETOTSH(I,3)
           ES4(NLAR)=ETOTSH(I,4)
           ES6(NLAR)=ETOTSH(I,6)
           ES5(NLAR)=ETOTSH(I,5)
           EPSTH11(NLAR)=EPSTH(I)
          END IF !(OFFG0(I)<=UN)
        ENDDO
       ELSE
        DO I=LFT,LLT
          IF (OFFG0(I)<=UN) THEN
           NLAR =NLAR+1
           INDEX(NLAR)=I
           ES1(NLAR)=MFXX(I)*(DEUX+MFXX(I))+MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I)
           ES2(NLAR)=MFYY(I)*(DEUX+MFYY(I))+MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
           ES3(NLAR)=MFZZ(I)*(DEUX+MFZZ(I))+MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
           ES4(NLAR)=MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I)
           ES6(NLAR)=MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I)
           ES5(NLAR)=MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I)
           EPSTH11(NLAR)=EPSTH(I)
          END IF !(OFFG0(I)<=UN)
        ENDDO
       END IF !(ISELECT>0) THEN
C------------C1,G are const, otherwise to update 
        CALL M1ISMSTR10_PON(NLAR   , ES1 , ES2 ,ES3 ,ES4  ,
     1                     ES5   ,ES6  ,EPSTH11, C1  ,G   ,SIGTMP)
#include "vectorize.inc"
        DO J=1,NLAR
          I = INDEX(J)
          SIG(I,1:6) =SIGTMP(J,1:6)
        ENDDO

        IF (NLAR < LLT) THEN
C--------switch to Ismstr11
          NSM = 0
          IF (JCVT>0) THEN
            DO I=LFT,LLT
              IF (OFFG0(I)>UN) THEN
               NSM = NSM +1
               INDEX(NSM) = I            
               ES1(NSM)=MFXX(I)
               ES2(NSM)=MFYY(I)
               ES3(NSM)=MFZZ(I)
               ES4(NSM)=MFXY(I)+MFYX(I)
               ES6(NSM)=MFXZ(I)+MFZX(I)
               ES5(NSM)=MFZY(I)+MFYZ(I)
               EPSTH11(NSM)=EPSTH(I)
              END IF !(OFFG0(I)>UN)
            ENDDO
            DO I=1,NSM
              EKK=ES1(I)+ES2(I)+ES3(I)-EPSTH11(I)
              DAV=-THIRD*(ES1(I)+ES2(I)+ES3(I))
              SIGTMP(I,1)=C1_1*EKK+G2_11*(ES1(I)+DAV)
              SIGTMP(I,2)=C1_1*EKK+G2_11*(ES2(I)+DAV)
              SIGTMP(I,3)=C1_1*EKK+G2_11*(ES3(I)+DAV)
              SIGTMP(I,4)=G(I)*ES4(I)
              SIGTMP(I,5)=G(I)*ES5(I)
              SIGTMP(I,6)=G(I)*ES6(I)
            ENDDO
#include "vectorize.inc"
            DO J=1,NSM
             I = INDEX(J)
             SIG(I,1:6)=SIGL(I,1:6) + SIGTMP(J,1:6)
            END DO
          ELSE
		   IF (ISELECT>0) THEN
            DO I=LFT,LLT
              IF (OFFG0(I)>UN) THEN
               NSM = NSM +1
               INDEX(NSM) = I            
               ES1(NSM)=ETOTSH(I,1)
               ES2(NSM)=ETOTSH(I,2)
               ES3(NSM)=ETOTSH(I,3)
               ES4(NSM)=ETOTSH(I,4)
               ES6(NSM)=ETOTSH(I,6)
               ES5(NSM)=ETOTSH(I,5)
               EPSTH11(NSM)=EPSTH(I)
              END IF !(OFFG0(I)>UN)
            ENDDO
           ELSE
            DO I=LFT,LLT
             IF (OFFG0(I)>UN) THEN
              NSM = NSM +1
              INDEX(NSM) = I            
              ES1(NSM)=MFXX(I)*(DEUX+MFXX(I))+MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I)
              ES2(NSM)=MFYY(I)*(DEUX+MFYY(I))+MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
              ES3(NSM)=MFZZ(I)*(DEUX+MFZZ(I))+MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
              ES4(NSM)=MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I)
              ES6(NSM)=MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I)
              ES5(NSM)=MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I)
              EPSTH11(NSM)=EPSTH(I)
             END IF
            END DO 
           END IF !(ISELECT>0)
           CALL M1ISMSTR11(NSM ,
     1                    ES1 , ES2 ,ES3 ,ES4  ,ES5   ,
     2                    ES6 ,EPSTH11, C1_1 ,G2_11   ,SIGTMP )
#include "vectorize.inc"
           DO J=1,NSM
            I = INDEX(J)
            SIG(I,1:6)=SIGL(I,1:6) + SIGTMP(J,1:6)
            PNEW(I)= C1_1*AMU(I)
            P  =-THIRD*(SIGTMP(J,1)+SIGTMP(J,2)+SIGTMP(J,3))
            SIG(I,1:3)= SIG(I,1:3)-PNEW(I) + P
           END DO
          END IF !(JCVT>0) THEN
        END IF !(NLAR < LLT)

      ELSEIF (ISMSTR.EQ.11) THEN
          DO I=LFT,LLT
            ES1(I)=MFXX(I)
            ES2(I)=MFYY(I)
            ES3(I)=MFZZ(I)
            ES4(I)=MFXY(I)+MFYX(I)
            ES6(I)=MFXZ(I)+MFZX(I)
            ES5(I)=MFZY(I)+MFYZ(I)
          ENDDO
          DO I=LFT,LLT
           G1(I)=G(I)
           G2(I)=DEUX*G1(I)
           EKK=ES1(I)+ES2(I)+ES3(I)-EPSTH(I)
           DAV=-THIRD*(ES1(I)+ES2(I)+ES3(I))
           SIG(I,1)=C1(I)*EKK+G2(I)*(ES1(I)+DAV)
           SIG(I,2)=C1(I)*EKK+G2(I)*(ES2(I)+DAV)
           SIG(I,3)=C1(I)*EKK+G2(I)*(ES3(I)+DAV)
           SIG(I,4)=G1(I)*ES4(I)
           SIG(I,5)=G1(I)*ES5(I)
           SIG(I,6)=G1(I)*ES6(I)
          ENDDO
      ENDIF  ! ismstr
      IF(ISMSTR==12)THEN 
        DO I=LFT,LLT
          IF(ABS(OFFG0(I)) <= UN)THEN ! Large Strain
            SSP(I)=SQRT((UNP333*G(I)+C1(I))/RHO(I))
            RHOSP(I)=RHO(I)
          ELSE ! large to small strain: conservative
            SSP(I)=SQRT((UNP333*G(I)+C1(I))/RHOREF(I))
            RHOSP(I)=RHOREF(I)
          END IF
        ENDDO
      ELSEIF(IDTMINS/=2.OR.JSMS==0)THEN
        ! compatibilite ascendante
        DO I=LFT,LLT
          !-----------------------
          !     P = C1 mu, mu = rho/rho0-1
          !     d(rho)/d(mu)  = rho0
          !-----------------------
          SSP(I)=SQRT((UNP333*G(I)+C1(I))/RHO0(I))
          RHOSP(I)=RHO0(I)
        ENDDO
      ELSEIF(ISMSTR==11)THEN ! Small Strain, IF(ISMSTR==1 .OR. ISMSTR==11)
        DO I=LFT,LLT
          !-----------------------
          !     P = C1 mu, mu = rho/rho0-1
          !     d(rho)/d(mu)  = rho0
          !-----------------------
          SSP(I)=SQRT((UNP333*G(I)+C1(I))/RHO0(I))
          RHOSP(I)=RHO0(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          IF(ABS(OFFG0(I)) <= UN)THEN ! Large Strain
            !-----------------------
            !     P = C1 mu, mu = ln(rho/rho0), rho= rho0 exp(mu)
            !     d(rho)/d(mu)  = rho
            !-----------------------
            SSP(I)=SQRT((UNP333*G(I)+C1(I))/RHO(I))
            RHOSP(I)=RHO(I)
          ELSE ! Small Strain
           !-----------------------
           !     P = C1 mu, mu = rho/rho0-1
           !     d(rho)/d(mu)  = rho0
           !-----------------------
            SSP(I)=SQRT((UNP333*G(I)+C1(I))/RHO0(I))
            RHOSP(I)=RHO0(I)
          END IF
        ENDDO
      END IF
C
      IF (JSPH.EQ.0)THEN
       CALL MQVISCB(PM,OFF     ,RHO     ,BID1    ,BID2    ,SSP ,
     2        BID3    ,STIFN   ,DT2T    ,NELTST  ,ITYPTST ,AIRE  ,
     3        OFFG    ,GEO     ,PID     ,VNEW    ,VD2     ,DELTAX ,
     4        VIS     ,D1      ,D2      ,D3      ,PNEW    ,PSH   ,
     5        MAT     ,NGL     ,QNEW    ,SSP_EQ  ,VOL     ,MSSA   ,
     6        DMELS   ,IBID    ,FACQ0   ,CONDE   ,DTEL    ,G_DT   ,
     7        IPM     ,RHOREF  ,RHOSP   )
      ELSE
       CALL MDTSPH(PM ,OFF     ,RHO     ,BID1      ,BID2    ,
     2        BID3    ,STIFN   ,DT2T    ,NELTST    ,ITYPTST ,
     3        OFFG    ,GEO     ,PID     ,MUMAX     ,SSP     ,
     4        VNEW    ,VD2     ,DELTAX  ,VIS       ,D1      ,
     5        D2      ,D3      ,PNEW    ,PSH       ,MAT     ,
     6        NGL     ,QNEW    ,SSP_EQ  ,G_DT      ,DTEL)
      ENDIF

      DTA =UNDEMI*DT1
C---questionable to do PNEW(I)=C1(I)*AMU(I) for Ismstr10
      DO I=LFT,LLT
       SIG(I,1)=SIG(I,1)*OFF(I)
       SIG(I,2)=SIG(I,2)*OFF(I)
       SIG(I,3)=SIG(I,3)*OFF(I)
       SIG(I,4)=SIG(I,4)*OFF(I)
       SIG(I,5)=SIG(I,5)*OFF(I)
       SIG(I,6)=SIG(I,6)*OFF(I)
       P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))* THIRD 
       E1=D1(I)*(SOLD1(I)+SIG(I,1)+P2)
       E2=D2(I)*(SOLD2(I)+SIG(I,2)+P2)
       E3=D3(I)*(SOLD3(I)+SIG(I,3)+P2)
       E4=D4(I)*(SOLD4(I)+SIG(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6))
       EINC= VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - UNDEMI*DVOL(I)*(QOLD(I)+QNEW(I)+P2)
       EINT(I)=(EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL(I))
       QOLD(I)=QNEW(I)
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  PRINC_U                       source/materials/mat/mat001/m1law.F
Chd|-- called by -----------
Chd|        M1ISMSTR10_PON                source/materials/mat/mat001/m1law.F
Chd|-- calls ---------------
Chd|        VALPVECOP_V                   source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
              SUBROUTINE PRINC_U(
     1                            NEL   , ES1 , ES2 ,ES3 ,ES4 ,
     2                            ES5   ,ES6  ,EV   , DIRPRV  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL     
      my_real
     .   ES1(MVSIZ), ES2(MVSIZ), ES3(MVSIZ), 
     .   ES4(MVSIZ), ES5(MVSIZ), ES6(MVSIZ),
     .   EV(MVSIZ,3),DIRPRV(MVSIZ,3,3)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,K
      my_real
     .            AV(MVSIZ,6),EVV(MVSIZ,3)
C----------------------------------------------------------------
C--------- [B]=[F][F]^t -1 strain-----
      DO I=1,NEL
       AV(I,1) = ES1(I)
       AV(I,2) = ES2(I)
       AV(I,3) = ES3(I)
       AV(I,4) = ES4(I)
       AV(I,5) = ES5(I)
       AV(I,6) = ES6(I)
      ENDDO
C
C         for a simple precision executing
c      IF (IRESP.EQ.1) THEN
c        CALL VALPVECDP(AV,EVV,DIRPRV,NEL)
c      ELSE
c        CALL VALPVEC(AV,EVV,DIRPRV,NEL)
c      ENDIF
        CALL VALPVECOP_V(AV,EVV,DIRPRV,NEL)

      DO I=1,NEL
        EV(I,1)=UNDEMI*LOG(EVV(I,1)+ UN)
        EV(I,2)=UNDEMI*LOG(EVV(I,2)+ UN)
        EV(I,3)=UNDEMI*LOG(EVV(I,3)+ UN)
      ENDDO
C----------------
C      
      RETURN
      END
Chd|====================================================================
Chd|  M1ISMSTR10_PON                source/materials/mat/mat001/m1law.F
Chd|-- called by -----------
Chd|        M1LAWTOT                      source/materials/mat/mat001/m1law.F
Chd|-- calls ---------------
Chd|        PRINC_U                       source/materials/mat/mat001/m1law.F
Chd|====================================================================
              SUBROUTINE M1ISMSTR10_PON(NEL ,
     1                              ES1 , ES2 ,ES3 ,ES4  ,ES5   ,
     2                              ES6 ,EPSTH, C1 ,G    ,SIG   )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include      "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL     
      my_real
     .   ES1(*), ES2(*), ES3(*), 
     .   ES4(*), ES5(*), ES6(*),
     .   SIG(MVSIZ,6), EPSTH(*),C1(*),G(*)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,K,IFAST(MVSIZ),NPRIN,INDEX(MVSIZ),NFAST
      my_real
     .    SV(3),EV(MVSIZ,3),DIRPRV(MVSIZ,3,3),EKK,DAV,P3,G2(MVSIZ),
     .    ESPIN1(MVSIZ), ESPIN2(MVSIZ), ESPIN3(MVSIZ), 
     .    ESPIN4(MVSIZ), ESPIN5(MVSIZ), ESPIN6(MVSIZ),
     .    E1, E2, E3, E4, E5, E6,ESPIN(MVSIZ,6), 
     .    E1Q,E2Q,E3Q,E4Q,E5Q,E6Q,
     .    E1T,E2T,E3T,E4T,E5T,E6T,B2(6),EMIN,EMAX
C----------------------------------------------------------------
         G2(1:NEL)=DEUX*G(1:NEL)
          NPRIN = 0
          DO I=1,NEL
            IFAST(I) =1
            EMIN=MIN(ES1(I),ES2(I),ES3(I),ES4(I),ES5(I),ES6(I))
            EMAX=MAX(ES1(I),ES2(I),ES3(I),ES4(I),ES5(I),ES6(I))
            IF (EMIN<-EM01.OR.EMAX>EM01) THEN
             NPRIN = NPRIN + 1
             INDEX(NPRIN) = I
             IFAST(I) =0
             ESPIN1(NPRIN)=ES1(I)
             ESPIN2(NPRIN)=ES2(I)
             ESPIN3(NPRIN)=ES3(I)
             ESPIN4(NPRIN)=ES4(I)
             ESPIN6(NPRIN)=ES6(I)
             ESPIN5(NPRIN)=ES5(I)
            END IF
          ENDDO
C
          DO I=1,NEL
           IF (IFAST(I)==0) CYCLE
           E1=ES1(I)
           E2=ES2(I)
           E3=ES3(I)
           E4=ES4(I)
           E6=ES6(I)
           E5=ES5(I)
           E1Q = E1*E1
           E2Q = E2*E2
           E3Q = E3*E3
           E4Q = E4*E4
           E6Q = E6*E6
           E5Q = E5*E5
           B2(1)=E1Q+E4Q+E6Q
           B2(2)=E4Q+E2Q+E5Q
           B2(3)=E6Q+E5Q+E3Q
           B2(4)=E1*E4+E2*E4+E5*E6
           B2(6)=E1*E6+E4*E5+E3*E6
           B2(5)=E4*E6+E2*E5+E3*E5
           ES1(I)=UNDEMI*(E1-UNDEMI*B2(1))
           ES2(I)=UNDEMI*(E2-UNDEMI*B2(2))
           ES3(I)=UNDEMI*(E3-UNDEMI*B2(3))
           ES4(I)=E4-UNDEMI*B2(4)
           ES6(I)=E6-UNDEMI*B2(6)
           ES5(I)=E5-UNDEMI*B2(5)
          ENDDO
          DO I=1,NEL
           IF (IFAST(I)==0) CYCLE
            EKK=ES1(I)+ES2(I)+ES3(I)-EPSTH(I)
            DAV=-THIRD*(ES1(I)+ES2(I)+ES3(I))
            P3 = C1(I)*EKK
            SIG(I,1)=P3+G2(I)*(ES1(I)+DAV)
            SIG(I,2)=P3+G2(I)*(ES2(I)+DAV)
            SIG(I,3)=P3+G2(I)*(ES3(I)+DAV)
            SIG(I,4)=G(I)*ES4(I)
            SIG(I,5)=G(I)*ES5(I)
            SIG(I,6)=G(I)*ES6(I)
          ENDDO
C
       IF (NPRIN>0) THEN
        CALL PRINC_U(
     1                  NPRIN    , ESPIN1 , ESPIN2 ,ESPIN3 ,ESPIN4 ,
     2                  ESPIN5   ,ESPIN6  ,EV   , DIRPRV  )
#include "vectorize.inc"
        DO K=1,NPRIN
          I = INDEX(K)
          EKK=EV(K,1)+EV(K,2)+EV(K,3)
          DAV=-THIRD*EKK
          P3 = C1(I)*(EKK-EPSTH(I))
          SV(1)=P3 +G2(I)*(EV(K,1)+DAV)
          SV(2)=P3 +G2(I)*(EV(K,2)+DAV)
          SV(3)=P3 +G2(I)*(EV(K,3)+DAV)
           SIG(I,1) =  DIRPRV(K,1,1)*DIRPRV(K,1,1)*SV(1)
     .               + DIRPRV(K,1,2)*DIRPRV(K,1,2)*SV(2)
     .               + DIRPRV(K,1,3)*DIRPRV(K,1,3)*SV(3)     
           SIG(I,2) =  DIRPRV(K,2,2)*DIRPRV(K,2,2)*SV(2)
     .               + DIRPRV(K,2,3)*DIRPRV(K,2,3)*SV(3)
     .               + DIRPRV(K,2,1)*DIRPRV(K,2,1)*SV(1)     
           SIG(I,3) =  DIRPRV(K,3,3)*DIRPRV(K,3,3)*SV(3)
     .               + DIRPRV(K,3,1)*DIRPRV(K,3,1)*SV(1)
     .               + DIRPRV(K,3,2)*DIRPRV(K,3,2)*SV(2)     
           SIG(I,4) =  DIRPRV(K,1,1)*DIRPRV(K,2,1)*SV(1)
     .               + DIRPRV(K,1,2)*DIRPRV(K,2,2)*SV(2)
     .               + DIRPRV(K,1,3)*DIRPRV(K,2,3)*SV(3)     
           SIG(I,5) =  DIRPRV(K,2,2)*DIRPRV(K,3,2)*SV(2)
     .               + DIRPRV(K,2,3)*DIRPRV(K,3,3)*SV(3)
     .               + DIRPRV(K,2,1)*DIRPRV(K,3,1)*SV(1)     
           SIG(I,6) =  DIRPRV(K,3,3)*DIRPRV(K,1,3)*SV(3)
     .               + DIRPRV(K,3,1)*DIRPRV(K,1,1)*SV(1)
     .               + DIRPRV(K,3,2)*DIRPRV(K,1,2)*SV(2)             
        ENDDO
       END IF
C      
      RETURN
      END
Chd|====================================================================
Chd|  PRINC_U1                      source/materials/mat/mat001/m1law.F
Chd|-- called by -----------
Chd|        M1ISMSTR11                    source/materials/mat/mat001/m1law.F
Chd|-- calls ---------------
Chd|        VALPVECOP_V                   source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
              SUBROUTINE PRINC_U1(
     1                            NEL   , ES1 , ES2 ,ES3 ,ES4 ,
     2                            ES5   ,ES6  ,EV   , DIRPRV  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL     
      my_real
     .   ES1(MVSIZ), ES2(MVSIZ), ES3(MVSIZ), 
     .   ES4(MVSIZ), ES5(MVSIZ), ES6(MVSIZ),
     .   EV(MVSIZ,3),DIRPRV(MVSIZ,3,3)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,K
      my_real
     .            AV(MVSIZ,6),EVV(MVSIZ,3)
C----------------------------------------------------------------
C--------- [B]=[F][F]^t -1 strain-----
      DO I=1,NEL
       AV(I,1) = ES1(I)
       AV(I,2) = ES2(I)
       AV(I,3) = ES3(I)
       AV(I,4) = ES4(I)
       AV(I,5) = ES5(I)
       AV(I,6) = ES6(I)
      ENDDO
        CALL VALPVECOP_V(AV,EVV,DIRPRV,NEL)

      DO I=1,NEL
        EV(I,1:3)=SQRT(MAX(ZERO,(EVV(I,1:3)+ UN))) - UN
      ENDDO
C----------------
C      
      RETURN
      END
Chd|====================================================================
Chd|  M1ISMSTR11                    source/materials/mat/mat001/m1law.F
Chd|-- called by -----------
Chd|        M1LAWTOT                      source/materials/mat/mat001/m1law.F
Chd|-- calls ---------------
Chd|        PRINC_U1                      source/materials/mat/mat001/m1law.F
Chd|====================================================================
              SUBROUTINE M1ISMSTR11(NEL ,
     1                              ES1 , ES2 ,ES3 ,ES4  ,ES5   ,
     2                              ES6 ,EPSTH, C1 ,G2   ,SIG11 )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL     
      my_real
     .   ES1(*), ES2(*), ES3(*), 
     .   ES4(*), ES5(*), ES6(*),
     .   SIG11(MVSIZ,6), EPSTH(*),C1,G2
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,K,IFAST(MVSIZ),NPRIN,INDEX(MVSIZ),NFAST
      my_real
     .    SV(3),EV(MVSIZ,3),DIRPRV(MVSIZ,3,3),EKK,DAV,P3,
     .    ESPIN1(MVSIZ), ESPIN2(MVSIZ), ESPIN3(MVSIZ), 
     .    ESPIN4(MVSIZ), ESPIN5(MVSIZ), ESPIN6(MVSIZ),
     .    E1, E2, E3, E4, E5, E6,ESPIN(MVSIZ,6), 
     .    E1Q,E2Q,E3Q,E4Q,E5Q,E6Q,
     .    E1T,E2T,E3T,E4T,E5T,E6T,B2(6),EMIN,EMAX
C----------------------------------------------------------------
          NPRIN = 0
          DO I=1,NEL
            IFAST(I) =1
            EMIN=MIN(ES1(I),ES2(I),ES3(I),ES4(I),ES5(I),ES6(I))
            EMAX=MAX(ES1(I),ES2(I),ES3(I),ES4(I),ES5(I),ES6(I))
            IF (EMIN<-EM01.OR.EMAX>EM01) THEN
             NPRIN = NPRIN + 1
             INDEX(NPRIN) = I
             IFAST(I) =0
             ESPIN1(NPRIN)=ES1(I)
             ESPIN2(NPRIN)=ES2(I)
             ESPIN3(NPRIN)=ES3(I)
             ESPIN4(NPRIN)=ES4(I)
             ESPIN6(NPRIN)=ES6(I)
             ESPIN5(NPRIN)=ES5(I)
            END IF
          ENDDO
          DO I=1,NEL
           IF (IFAST(I)==0) CYCLE
           E1=ES1(I)
           E2=ES2(I)
           E3=ES3(I)
           E4=ES4(I)
           E6=ES6(I)
           E5=ES5(I)
           E1Q = E1*E1
           E2Q = E2*E2
           E3Q = E3*E3
           E4Q = E4*E4
           E6Q = E6*E6
           E5Q = E5*E5
           B2(1)=E1Q+E4Q+E6Q
           B2(2)=E4Q+E2Q+E5Q
           B2(3)=E6Q+E5Q+E3Q
           B2(4)=E1*E4+E2*E4+E5*E6
           B2(6)=E1*E6+E4*E5+E3*E6
           B2(5)=E4*E6+E2*E5+E3*E5
           ES1(I)=UNDEMI*(E1-QUART*B2(1))
           ES2(I)=UNDEMI*(E2-QUART*B2(2))
           ES3(I)=UNDEMI*(E3-QUART*B2(3))
           ES4(I)=UNDEMI*(E4-QUART*B2(4))
           ES6(I)=UNDEMI*(E6-QUART*B2(6))
           ES5(I)=UNDEMI*(E5-QUART*B2(5))
          ENDDO
C
          DO I=1,NEL
           IF (IFAST(I)==0) CYCLE
            EKK=ES1(I)+ES2(I)+ES3(I)-EPSTH(I)
            DAV=-THIRD*(ES1(I)+ES2(I)+ES3(I))
            P3 = C1*EKK
            SIG11(I,1)=P3+G2*(ES1(I)+DAV)
            SIG11(I,2)=P3+G2*(ES2(I)+DAV)
            SIG11(I,3)=P3+G2*(ES3(I)+DAV)
            SIG11(I,4)=G2*ES4(I)
            SIG11(I,5)=G2*ES5(I)
            SIG11(I,6)=G2*ES6(I)
          ENDDO
C
       IF (NPRIN>0) THEN
        CALL PRINC_U1(
     1                  NPRIN    , ESPIN1 , ESPIN2 ,ESPIN3 ,ESPIN4 ,
     2                  ESPIN5   ,ESPIN6  ,EV   , DIRPRV  )
#include "vectorize.inc"
        DO K=1,NPRIN
          I = INDEX(K)
          EKK=EV(K,1)+EV(K,2)+EV(K,3)
          DAV=-THIRD*EKK
          P3 = C1*(EKK-EPSTH(I))
          SV(1)=P3 +G2*(EV(K,1)+DAV)
          SV(2)=P3 +G2*(EV(K,2)+DAV)
          SV(3)=P3 +G2*(EV(K,3)+DAV)
           SIG11(I,1) = DIRPRV(K,1,1)*DIRPRV(K,1,1)*SV(1)
     .               + DIRPRV(K,1,2)*DIRPRV(K,1,2)*SV(2)
     .               + DIRPRV(K,1,3)*DIRPRV(K,1,3)*SV(3)     
           SIG11(I,2) = DIRPRV(K,2,2)*DIRPRV(K,2,2)*SV(2)
     .               + DIRPRV(K,2,3)*DIRPRV(K,2,3)*SV(3)
     .               + DIRPRV(K,2,1)*DIRPRV(K,2,1)*SV(1)     
           SIG11(I,3) = DIRPRV(K,3,3)*DIRPRV(K,3,3)*SV(3)
     .               + DIRPRV(K,3,1)*DIRPRV(K,3,1)*SV(1)
     .               + DIRPRV(K,3,2)*DIRPRV(K,3,2)*SV(2)     
           SIG11(I,4) = DIRPRV(K,1,1)*DIRPRV(K,2,1)*SV(1)
     .               + DIRPRV(K,1,2)*DIRPRV(K,2,2)*SV(2)
     .               + DIRPRV(K,1,3)*DIRPRV(K,2,3)*SV(3)     
           SIG11(I,5) = DIRPRV(K,2,2)*DIRPRV(K,3,2)*SV(2)
     .               + DIRPRV(K,2,3)*DIRPRV(K,3,3)*SV(3)
     .               + DIRPRV(K,2,1)*DIRPRV(K,3,1)*SV(1)     
           SIG11(I,6) = DIRPRV(K,3,3)*DIRPRV(K,1,3)*SV(3)
     .               + DIRPRV(K,3,1)*DIRPRV(K,1,1)*SV(1)
     .               + DIRPRV(K,3,2)*DIRPRV(K,1,2)*SV(2)             
        ENDDO
       END IF
C      
      RETURN
      END

