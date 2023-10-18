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
Chd|  MULAW                         source/materials/mat_share/mulaw.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        SIGEPS35                      source/materials/mat/mat035/sigeps35.F
Chd|        SIGEPS37                      source/materials/mat/mat037/sigeps37.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|        SIGEPS70                      source/materials/mat/mat070/sigeps70.F
Chd|        SIGEPS90                      source/materials/mat/mat090/sigeps90.F
Chd|====================================================================
      SUBROUTINE MULAW(LFT   ,LLT   ,NFT    ,MTN   ,JCVT   ,
     2                 PM    ,OFF   ,SIG    ,EINT  ,RHO    ,
     3                        VOL   ,STRAIN ,       GAMA   ,
     4                 UVAR  ,BUFMAT,TF     ,NPF   ,
     5                 MAT   ,NGL   ,NUVAR  ,
     6                               GEO    ,PID   ,EPSD   ,
     8                                              WXX    ,
     9                 WYY   ,WZZ   ,JSPH   ,       SSP    ,
     A                        VOLN  ,               VIS    ,
     B                 D1    ,D2    ,D3     ,D4    ,D5     ,
     C                 D6    ,
     D                 DVOL  ,SOLD1 ,SOLD2  ,SOLD3 ,SOLD4  ,
     E                 SOLD5 ,SOLD6 ,RX     ,RY    ,RZ     ,
     F                 SX    ,SY    ,SZ     ,TX    ,TY     ,
     G                 TZ    ,                      ISMSTR ,
     H                 MFXX  ,MFXY  ,MFXZ   ,MFYX  ,MFYY   , 
     I                 MFYZ  ,MFZX  ,MFZY   ,MFZZ  ,IPM    ,
     J                 ISORTH,NEL   )
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NFT,MTN,NUVAR,JCVT,ISORTH,JSPH,ISMSTR,NEL
      INTEGER NPF(*), MAT(MVSIZ), NGL(MVSIZ),PID(MVSIZ),IPM(NPROPMI,NUMMAT)
C 
      my_real 
     .   PM(NPROPM,NUMMAT), GAMA(*), 
     .   OFF(*), SIG(NEL,6), STRAIN(NEL,6), EINT(*),
     .   RHO(*), VOL(*), UVAR(*), BUFMAT(*), TF(*),
     .   GEO(NPROPG,*),EPSD(MVSIZ),
     .   WXX(*), WYY(*), WZZ(*), SSP(*),
     .   VOLN(*) , VIS(MVSIZ) ,
     .   SOLD1(MVSIZ) ,SOLD2(MVSIZ) ,SOLD3(MVSIZ) ,
     .   SOLD4(MVSIZ) ,SOLD5(MVSIZ) ,SOLD6(MVSIZ),
     .   D1(*) ,D2(*),D3(*),D4(*),D5(*),D6(*) ,
     .   DVOL(*),
     .   RX(MVSIZ)    ,RY(MVSIZ)    ,RZ(MVSIZ),
     .   SX(MVSIZ)    ,SY(MVSIZ)    ,SZ(MVSIZ)    ,
     .   TX(MVSIZ)    ,TY(MVSIZ)    ,TZ(MVSIZ)   ,
     .   MFXX(*)  ,MFXY(*)  ,MFXZ(*)  ,MFYX(*) ,MFYY(*)   ,MFYZ(*)   ,
     .   MFZX(*)  ,MFZY(*)  ,MFZZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NVPHAS, N0PHAS, NPH, IFLG,NPAR, IADBUF, NFUNC, IFUNC(100) 
      my_real
     . EP1(MVSIZ), EP2(MVSIZ), EP3(MVSIZ),E7(MVSIZ),
     . EP4(2*MVSIZ),EP5(2*MVSIZ),EP6(2*MVSIZ),
     . S1(MVSIZ) ,S2(MVSIZ) ,S3(MVSIZ) ,
     . S4(2*MVSIZ) ,S5(2*MVSIZ) ,S6(2*MVSIZ),
     . DE1(MVSIZ),DE2(MVSIZ),DE3(MVSIZ),
     . DE4(2*MVSIZ),DE5(2*MVSIZ),DE6(2*MVSIZ),
     . SO1(MVSIZ),SO2(MVSIZ),SO3(MVSIZ),
     . SO4(2*MVSIZ),SO5(2*MVSIZ),SO6(2*MVSIZ),
     .ES1(MVSIZ),ES2(MVSIZ),ES3(MVSIZ),ES4(MVSIZ),ES5(MVSIZ),ES6(MVSIZ),
     .SV1(MVSIZ),SV2(MVSIZ),SV3(MVSIZ),SV4(MVSIZ),SV5(MVSIZ),SV6(MVSIZ),
     .R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),    
     .R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      my_real RHO0(MVSIZ),VOL_AVG(MVSIZ)
      my_real RBID, E1, E2, E3, E4, E5, E6, P2, TIME, TIMESTEP
C=======================================================================
      TIME    =ZERO
      TIMESTEP=ONE/FLOAT(NITRS)
C---------------------------
      NPAR   = IPM(9,MAT(1))
      IADBUF = IPM(7,MAT(1))
      NFUNC  = IPM(10,MAT(1))
      DO I=1,NFUNC
        IFUNC(I) = IPM(10+I,MAT(1))
      ENDDO
C---------------------------
      VIS = ZERO
      DO I=LFT,LLT
        RHO0(I)= PM( 1,MAT(I))
      END DO
C---------------------------
      EP1 = ZERO
      EP2 = ZERO
      EP3 = ZERO
      EP4 = ZERO
      EP5 = ZERO
      EP6 = ZERO
      EPSD= ZERO
      SV1 = ZERO
      SV2 = ZERO
      SV3 = ZERO
      SV4 = ZERO
      SV5 = ZERO
      SV6 = ZERO
      S1  = ZERO
      S2  = ZERO
      S3  = ZERO
      S4  = ZERO
      S5  = ZERO
      S6  = ZERO
      E7  = ZERO
C---------------------------
C     ISOTROPE ET ORTHOTROPE CONVECTED (JCVT>0)
C---------------------------
      DO I=LFT,LLT
       DE1(I) = D1(I)
       DE2(I) = D2(I)
       DE3(I) = D3(I)
       DE4(I) = D4(I)
       DE5(I) = D5(I)
       DE6(I) = D6(I)
       SO1(I) = SIG(I,1)
       SO2(I) = SIG(I,2)
       SO3(I) = SIG(I,3)
       SO4(I) = SIG(I,4)
       SO5(I) = SIG(I,5)
       SO6(I) = SIG(I,6)
      ENDDO
C---------------------------
      DO I=LFT,LLT
        STRAIN(I,1)= STRAIN(I,1) + DE1(I)
        STRAIN(I,2)= STRAIN(I,2) + DE2(I)
        STRAIN(I,3)= STRAIN(I,3) + DE3(I)
        STRAIN(I,4)= STRAIN(I,4) + DE4(I)
        STRAIN(I,5)= STRAIN(I,5) + DE5(I)
        STRAIN(I,6)= STRAIN(I,6) + DE6(I)
        ES1(I) = STRAIN(I,1)
        ES2(I) = STRAIN(I,2)
        ES3(I) = STRAIN(I,3)
        ES4(I) = STRAIN(I,4)
        ES5(I) = STRAIN(I,5)
        ES6(I) = STRAIN(I,6)
      ENDDO
C
C     unify w/ Engine-----
       IF (ISMSTR == 10.OR.ISMSTR == 12) THEN
          DO I=LFT,LLT
C Is like 42 ...etc we compute [B]=[F][F]^t - I strain----- e.g. 62,92,88  
            ES1(I)=MFXX(I)*(TWO+MFXX(I))+MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I) 
            ES2(I)=MFYY(I)*(TWO+MFYY(I))+MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
            ES3(I)=MFZZ(I)*(TWO+MFZZ(I))+MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
            ES4(I)=TWO*(MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I))
            ES6(I)=TWO*(MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I))
            ES5(I)=TWO*(MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I))  
          ENDDO
       ELSEIF(ISMSTR == 11)THEN
          DO I=LFT,LLT
           ES1(I)=MFXX(I)
           ES2(I)=MFYY(I)
           ES3(I)=MFZZ(I)
           ES4(I)=MFXY(I)+MFYX(I)
           ES6(I)=MFXZ(I)+MFZX(I)
           ES5(I)=MFZY(I)+MFYZ(I)
          ENDDO
       END IF
C          IF (JCVT <= 0 .AND. ISORTH /= 0) THEN
C---------------------------
C       ORTHOTROIC / GLOBAL
C---------------------------
C            CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
C     2                   R23 ,R31,R32,R33,
C     3                   RX  ,RY ,RZ ,SX ,SY ,SZ     ,
C     4                   TX  ,TY ,TZ )
C            CALL MROTENS(LFT,LLT,ES1,ES2,ES3,ES4,ES5,ES6,
C     .                   R11,R12,R13,
C     .                   R21,R22,R23,
C     .                   R31,R32,R33)
C          ENDIF
C ====================================================================
      IF(MTN==35)THEN
          CALL SIGEPS35(LLT ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TIME,TIMESTEP,BUFMAT(IADBUF),
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  )
      ELSEIF(MTN==37)THEN
          CALL SIGEPS37(LLT ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TIME,TIMESTEP,BUFMAT(IADBUF),
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  )     
      ELSEIF(MTN==38)THEN
          CALL SIGEPS38(LLT ,NPAR,NUVAR,NFUNC,IFUNC ,
     .                  NPF ,TF  ,TIME,TIMESTEP,BUFMAT(IADBUF),
     .                  RHO0,RHO ,VOLN ,EINT ,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,
     .                  RBID,ISMSTR,MFXX,MFXY,MFXZ,MFYX, 
     .                  MFYY ,MFYZ ,MFZX ,MFZY,MFZZ)
      ELSEIF(MTN==42)THEN
          CALL SIGEPS42(LLT ,NPAR,NUVAR,NFUNC,IFUNC ,
     2                  NPF ,TF  ,TIME,TIMESTEP,BUFMAT(IADBUF),
     3                  RHO0,RHO ,VOLN ,EINT ,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     A                  SSP ,VIS ,UVAR,OFF  ,WXX  ,WYY ,
     .                  WZZ ,ISMSTR,MFXX,MFXY,MFXZ,MFYX, 
     .                  MFYY ,MFYZ ,MFZX ,MFZY,MFZZ)
      ELSEIF(MTN==70)THEN
        CALL SIGEPS70(LLT ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TIME,TIMESTEP,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,
     B                  PM  ,IPM ,MAT ,EPSD )
      ELSEIF(MTN==90)THEN
        CALL SIGEPS90(LLT ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TIME,TIMESTEP,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,ISMSTR)  
      END IF
C ===================================================================
      DO I=LFT,LLT
        SIG(I,1) = S1(I)*OFF(I)
        SIG(I,2) = S2(I)*OFF(I)
        SIG(I,3) = S3(I)*OFF(I)
        SIG(I,4) = S4(I)*OFF(I)
        SIG(I,5) = S5(I)*OFF(I)
        SIG(I,6) = S6(I)*OFF(I)
      END DO
C--- Engine doesn't have this because generally dvol<<vol
      IF (ISMSTR == 1.OR.ISMSTR == 11) THEN
       DO I=LFT,LLT
        VOL_AVG(I) = VOL(I)
       END DO
      ELSE
       DO I=LFT,LLT
        VOL_AVG(I) = VOLN(I)- HALF*DVOL(I)
       END DO
      END IF
      DO I=LFT,LLT
       P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))
     .         * THIRD
       E1=D1(I)*(SOLD1(I)+SIG(I,1)+P2)
       E2=D2(I)*(SOLD2(I)+SIG(I,2)+P2)
       E3=D3(I)*(SOLD3(I)+SIG(I,3)+P2)
       E4=D4(I)*(SOLD4(I)+SIG(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6))
       EINT(I)=EINT(I)
     .  -P2*DVOL(I)*HALF
     .  +(E1+E2+E3+E4+E5+E6+E7(I))*VOL_AVG(I)*HALF
       EINT(I)=EINT(I)/MAX(VOL(I),EM20)
      END DO
C---
      RETURN
      END SUBROUTINE MULAW
