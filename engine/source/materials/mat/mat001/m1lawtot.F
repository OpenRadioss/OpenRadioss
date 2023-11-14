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
Chd|  M1LAWTOT                      source/materials/mat/mat001/m1lawtot.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        M1ISMSTR10_PON                source/materials/mat/mat001/m1ismstr10_pon.F
Chd|        M1ISMSTR11                    source/materials/mat/mat001/m1ismstr11.F
Chd|        M1TOT_STAB11                  source/elements/solid/solide8e/m1tot_stab11.F
Chd|        M1TOT_STAB18                  source/elements/solid/solide8e/m1tot_stab18.F
Chd|        M1TOT_STAB24                  source/elements/solid/solidez/m1tot_stab24.F
Chd|        M1TOT_STAB_P                  source/elements/solid/solide10/m1tot_stab_p.F
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|        NSVIS_STAB                    source/elements/solid/solide10/nsvis_stab.F
Chd|        NSVIS_STAB11                  source/elements/solid/solidez/nsvis_stab11.F
Chd|        NSVIS_STAB18                  source/elements/solid/solide8e/nsvis_stab18.F
Chd|====================================================================
      SUBROUTINE M1LAWTOT(
     1   PM,      OFF,     SIG,     EINT,
     2   RHO,     QOLD,    VOL,     STIFN,
     3   DT2T,    NELTST,  ITYPTST, OFFG,
     4   GEO,     PID,     AMU,     MUMAX,
     5   MAT,     NGL,     SSP,     DVOL,
     6   AIRE,    VNEW,    VD2,     DELTAX,
     7   VIS,     D1,      D2,      D3,
     8   D4,      D5,      D6,      PNEW,
     9   PSH,     QNEW,    SSP_EQ,  SOLD1,
     A   SOLD2,   SOLD3,   SOLD4,   SOLD5,
     B   SOLD6,   MSSA,    DMELS,   CONDE,
     C   MFXX,    MFXY,    MFXZ,    MFYX,
     D   MFYY,    MFYZ,    MFZX,    MFZY,
     E   MFZZ,    OFFG0,   VOL_AVG, EPSTH,
     F   DTEL,    G_DT,    NEL,     ETOTSH,
     G   ISELECT, IPM,     RHOREF,  RHOSP,
     H   SIGL,    ITY,     ISMSTR,  JTUR,
     I   JTHE,    JCVT,    JSPH,    JSMS,
     J   NPG )
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
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: JSPH
      INTEGER, INTENT(IN) :: JSMS,NPG
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
      INTEGER I, MX, J,IBID,NLAR,INDEX(MVSIZ),NSM,IPRES,ISTAB
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
     .    SV(3),P3,P2,NU_1,
     .    EPSTH11(MVSIZ),SIGTMP(MVSIZ,6),FACG,FF,FRHO
C-----------------------------------------------
      FACQ0 = ONE
      MX = MAT(1)
      RHO0_1 =PM( 1,MX)
      C1_1   =PM(32,MX)
      NU_1   =PM(21,MX)
      G2_11 = TWO*PM(22,MX)
      IPRES=0
      IF (NU_1>0.49)IPRES=1 
      ISTAB = 1
      IF (IPRES==1.OR.ISMSTR == 11) ISTAB = 0
      DO I=1,NEL
        RHO0(I) =RHO0_1
        G(I)    =PM(22,MX)*OFF(I)
        C1(I)   =C1_1
      ENDDO

C------------[F] = [MF]+ 1 ----ISMSTR10,11,12 
      IF (ISMSTR == 10) THEN
       IF (ISELECT>0) THEN
        DO I=1,NEL
           ES1(I)=ETOTSH(I,1)
           ES2(I)=ETOTSH(I,2)
           ES3(I)=ETOTSH(I,3)
           ES4(I)=ETOTSH(I,4)
           ES6(I)=ETOTSH(I,6)
           ES5(I)=ETOTSH(I,5)
        ENDDO
       ELSE
        DO I=1,NEL
           ES1(I)=MFXX(I)*(TWO+MFXX(I))+MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I)
           ES2(I)=MFYY(I)*(TWO+MFYY(I))+MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
           ES3(I)=MFZZ(I)*(TWO+MFZZ(I))+MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
           ES4(I)=MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I)
           ES6(I)=MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I)
           ES5(I)=MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I)
        ENDDO
       END IF !(ISELECT>0) THEN
        CALL M1ISMSTR10_PON(NEL   , ES1 , ES2 ,ES3 ,ES4  ,
     1                     ES5   ,ES6  ,EPSTH, C1  ,G   ,SIGTMP)
        IF(IPRES==1.AND.IRESP==1)THEN 
          DO I=1,NEL
           FF = -MIN(SIG(I,1),SIG(I,2),SIG(I,3))
           FACG = MAX(ONE,FF/G2_11)
           IF (FACG>ONE) FACG = ONEP2*FACG
           SIGTMP(I,4)= FACG*SIGTMP(I,4)
           SIGTMP(I,5)= FACG*SIGTMP(I,5)
           SIGTMP(I,6)= FACG*SIGTMP(I,6)
           G(I) = FACG*G(I)
          END DO
        END IF !(IPRES==1)THEN 
        SIG(1:NEL,1:6)=SIGTMP(1:NEL,1:6)
      ELSEIF (ISMSTR == 12) THEN
        NLAR = 0
       IF (ISELECT>0) THEN
        DO I=1,NEL
          IF (OFFG0(I)<=ONE) THEN
           NLAR =NLAR+1
           INDEX(NLAR)=I
           ES1(NLAR)=ETOTSH(I,1)
           ES2(NLAR)=ETOTSH(I,2)
           ES3(NLAR)=ETOTSH(I,3)
           ES4(NLAR)=ETOTSH(I,4)
           ES6(NLAR)=ETOTSH(I,6)
           ES5(NLAR)=ETOTSH(I,5)
           EPSTH11(NLAR)=EPSTH(I)
          END IF !(OFFG0(I)<=ONE)
        ENDDO
       ELSE
        DO I=1,NEL
          IF (OFFG0(I)<=ONE) THEN
           NLAR =NLAR+1
           INDEX(NLAR)=I
           ES1(NLAR)=MFXX(I)*(TWO+MFXX(I))+MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I)
           ES2(NLAR)=MFYY(I)*(TWO+MFYY(I))+MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
           ES3(NLAR)=MFZZ(I)*(TWO+MFZZ(I))+MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
           ES4(NLAR)=MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I)
           ES6(NLAR)=MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I)
           ES5(NLAR)=MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I)
           EPSTH11(NLAR)=EPSTH(I)
          END IF !(OFFG0(I)<=ONE)
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

        IF (NLAR < NEL) THEN
C--------switch to Ismstr11
          NSM = 0
          IF (JCVT>0) THEN
            DO I=1,NEL
              IF (OFFG0(I)>ONE) THEN
               NSM = NSM +1
               INDEX(NSM) = I            
               ES1(NSM)=MFXX(I)
               ES2(NSM)=MFYY(I)
               ES3(NSM)=MFZZ(I)
               ES4(NSM)=MFXY(I)+MFYX(I)
               ES6(NSM)=MFXZ(I)+MFZX(I)
               ES5(NSM)=MFZY(I)+MFYZ(I)
               EPSTH11(NSM)=EPSTH(I)
              END IF !(OFFG0(I)>ONE)
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
            IF(IPRES==1)THEN 
#include "vectorize.inc"
              DO J=1,NSM
               I = INDEX(J)
               FF = -MIN(SIG(I,1),SIG(I,2),SIG(I,3))
               FACG = MAX(ONE,FF/G2_11)
               IF (FACG>ONE) FACG = ONEP2*FACG
               FRHO = RHOREF(I)/RHO0_1
               IF (FRHO<ONEP01) FACG = ONE
               SIG(I,1:3)=SIGL(I,1:3) + SIGTMP(J,1:3)
               SIG(I,4:6)=SIGL(I,4:6) + FACG*SIGTMP(J,4:6)
               G(I) = FACG*G(I)
              END DO
            ELSE
#include "vectorize.inc"
              DO J=1,NSM
               I = INDEX(J)
               SIG(I,1:6)=SIGL(I,1:6) + SIGTMP(J,1:6)
              END DO
            END IF!(IPRES==1)THEN 
          ELSE
             IF (ISELECT>0) THEN
            DO I=1,NEL
              IF (OFFG0(I)>ONE) THEN
               NSM = NSM +1
               INDEX(NSM) = I            
               ES1(NSM)=ETOTSH(I,1)
               ES2(NSM)=ETOTSH(I,2)
               ES3(NSM)=ETOTSH(I,3)
               ES4(NSM)=ETOTSH(I,4)
               ES6(NSM)=ETOTSH(I,6)
               ES5(NSM)=ETOTSH(I,5)
               EPSTH11(NSM)=EPSTH(I)
              END IF !(OFFG0(I)>ONE)
            ENDDO
           ELSE
            DO I=1,NEL
             IF (OFFG0(I)>ONE) THEN
              NSM = NSM +1
              INDEX(NSM) = I            
              ES1(NSM)=MFXX(I)*(TWO+MFXX(I))+MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I)
              ES2(NSM)=MFYY(I)*(TWO+MFYY(I))+MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
              ES3(NSM)=MFZZ(I)*(TWO+MFZZ(I))+MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
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
        END IF !(NLAR < NEL)

      ELSEIF (ISMSTR==11) THEN
          DO I=1,NEL
            ES1(I)=MFXX(I)
            ES2(I)=MFYY(I)
            ES3(I)=MFZZ(I)
            ES4(I)=MFXY(I)+MFYX(I)
            ES6(I)=MFXZ(I)+MFZX(I)
            ES5(I)=MFZY(I)+MFYZ(I)
          ENDDO
          DO I=1,NEL
           G1(I)=G(I)
           G2(I)=TWO*G1(I)
           EKK=ES1(I)+ES2(I)+ES3(I)-EPSTH(I)
           DAV=-THIRD*(ES1(I)+ES2(I)+ES3(I))
           SIG(I,1)=C1(I)*EKK+G2(I)*(ES1(I)+DAV)
           SIG(I,2)=C1(I)*EKK+G2(I)*(ES2(I)+DAV)
           SIG(I,3)=C1(I)*EKK+G2(I)*(ES3(I)+DAV)
           SIG(I,4)=G1(I)*ES4(I)
           SIG(I,5)=G1(I)*ES5(I)
           SIG(I,6)=G1(I)*ES6(I)
          ENDDO
          IF (ISELECT>0) CALL M1TOT_STAB11(SIG,G2_11,G,NEL )
          IF (JCVT>0) CALL NSVIS_STAB11(SIG,C1_1 ,G2_11,VOL  ,D1   ,
     .                                  D2,D3  ,D4    ,D5   ,D6   ,
     .                                  RHO0,NPG  ,NEL) 
      ENDIF  ! ismstr
      IF (ISTAB>0) THEN
        IF (JCVT==0) THEN
          IF (NPG>1) THEN
            CALL M1TOT_STAB_P(SIG,G2_11,C1,NEL )
            CALL NSVIS_STAB(SIG,C1_1 ,G2_11,VOL  ,D1   ,
     .                     D2 ,D3  ,D4    ,D5   ,D6   ,
     .                     RHOREF ,G    ,NPG  ,NEL) 
          END IF
        ELSE
c          CALL M1TOT_STAB_S(SIG,G2_11,G,OFFG0,ISMSTR,NEL )
          IF (NPG>1) THEN
           CALL M1TOT_STAB18(SIG,SIGL,G2_11,G,OFFG,ISMSTR,NEL )
           CALL NSVIS_STAB18(SIG  ,C1  ,G2_11 ,VOL ,D1   ,
     .                      D2   ,D3  ,D4    ,D5   ,D6   ,
     .                      RHOREF,G  ,NPG  ,NEL   ) 
          ELSE
           CALL M1TOT_STAB24(SIG,SIGL,G2_11,G,OFFG,ISMSTR,NEL )
           CALL NSVIS_STAB(SIG,C1_1 ,G2_11   ,VOL ,D1   ,
     .                      D2   ,D3  ,D4    ,D5   ,D6   ,
     .                      RHOREF,G  ,NPG ,NEL   ) 
          END IF
        END IF
      ENDIF  
      IF(ISMSTR==12)THEN 
        DO I=1,NEL
          IF(ABS(OFFG0(I)) <= ONE)THEN ! Large Strain
            SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO(I))
            RHOSP(I)=RHO(I)
          ELSE ! large to small strain: conservative
            SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHOREF(I))
            RHOSP(I)=RHOREF(I)
          END IF
        ENDDO
      ELSEIF(IDTMINS/=2.OR.JSMS==0)THEN
        ! compatibilite ascendante
        DO I=1,NEL
          !-----------------------
          !     P = C1 mu, mu = rho/rho0-1
          !     d(rho)/d(mu)  = rho0
          !-----------------------
          SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO0(I))
          RHOSP(I)=RHO0(I)
        ENDDO
      ELSEIF(ISMSTR==11)THEN ! Small Strain, IF(ISMSTR==1 .OR. ISMSTR==11)
        DO I=1,NEL
          !-----------------------
          !     P = C1 mu, mu = rho/rho0-1
          !     d(rho)/d(mu)  = rho0
          !-----------------------
          SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO0(I))
          RHOSP(I)=RHO0(I)
        ENDDO
      ELSE
        DO I=1,NEL
          IF(ABS(OFFG0(I)) <= ONE)THEN ! Large Strain
            !-----------------------
            !     P = C1 mu, mu = ln(rho/rho0), rho= rho0 exp(mu)
            !     d(rho)/d(mu)  = rho
            !-----------------------
            SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO(I))
            RHOSP(I)=RHO(I)
          ELSE ! Small Strain
           !-----------------------
           !     P = C1 mu, mu = rho/rho0-1
           !     d(rho)/d(mu)  = rho0
           !-----------------------
            SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO0(I))
            RHOSP(I)=RHO0(I)
          END IF
        ENDDO
      END IF
C
      IF (JSPH==0)THEN
       CALL MQVISCB(
     1   PM,      OFF,     RHO,     BID1,
     2   BID2,    SSP,     BID3,    STIFN,
     3   DT2T,    NELTST,  ITYPTST, AIRE,
     4   OFFG,    GEO,     PID,     VNEW,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QNEW,    SSP_EQ,
     8   VOL,     MSSA,    DMELS,   IBID,
     9   FACQ0,   CONDE,   DTEL,    G_DT,
     A   IPM,     RHOREF,  RHOSP,   NEL,
     B   ITY,     ISMSTR,  JTUR,    JTHE,
     C   JSMS,    NPG  )
      ELSE
       CALL MDTSPH(
     1   PM,      OFF,     RHO,     BID1,
     2   BID2,    BID3,    STIFN,   DT2T,
     3   NELTST,  ITYPTST, OFFG,    GEO,
     4   PID,     MUMAX,   SSP,     VNEW,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QNEW,    SSP_EQ,
     8   G_DT,    DTEL,    NEL,     ITY,
     9   JTUR,    JTHE)
      ENDIF

      DTA =HALF*DT1
C---questionable to do PNEW(I)=C1(I)*AMU(I) for Ismstr10
      DO I=1,NEL
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
       EINC= VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - HALF*DVOL(I)*(QOLD(I)+QNEW(I)+P2)
       EINT(I)=(EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL(I))
       QOLD(I)=QNEW(I)
      ENDDO

      RETURN
      END
