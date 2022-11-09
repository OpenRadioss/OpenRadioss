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
Chd|  M25CPLRC                      source/materials/mat/mat025/m25cplrc.F
Chd|-- called by -----------
Chd|        SIGEPS25C                     source/materials/mat/mat025/sigeps25c.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ROTOV                         source/airbag/roto.F          
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE M25CPLRC(JFT     ,JLT     ,PM      ,OFF     ,SIG   ,
     2                    WPLA    ,DIR     ,IMATLY  ,DAMT    ,CRAK  ,
     3                    NFIS1   ,NFIS2   ,NFIS3   ,ILAYER  ,SHF   ,
     4                    NGL     ,EPS     ,IGTYP   ,WPLAR   ,STRN1 ,
     5                    STRN2   ,STRN3   ,STRP1   ,STRP2   ,SIGE  ,
     6                    EPSP    ,ISRATE  ,OFFPLY  ,SIGY    ,ETSE  ,
     7                    OUTV    ,SEQ_OUTP,ISHPLYXFEM,LY_EXX,LY_EYY,
     8                    LY_EXY  ,SIGPLY  ,SIGPE   ,PLY_ID  ,NEL   ,
     9                    SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX,
     A                    IPG     ,TSAIWU  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(MVSIZ),NEL
      INTEGER JFT, JLT,ILAYER,
     .   NFIS1(*),NFIS2(*),NFIS3(*),IMATLY,ISRATE, ISHPLYXFEM,
     .   OUTV(*),PLY_ID,IGTYP,IPG
C     REAL
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,5), WPLA(*), DIR(NEL,2),
     .   DAMT(NEL,2), CRAK(NEL,2),SHF(*),EPSP(*),
     .   EPS(MVSIZ,5),WPLAR(MVSIZ),
     .   STRP1(MVSIZ),STRP2(MVSIZ),
     .   STRN1(MVSIZ),STRN2(MVSIZ),STRN3(MVSIZ),SIGE(MVSIZ,5),
     .   OFFPLY(*),SIGY(*),ETSE(*),SEQ_OUTP(*), 
     .   LY_EXX(*), LY_EYY(*), LY_EXY(*), SIGPLY(NEL,3),SIGPE(MVSIZ,5),
     .   SIGNXX(MVSIZ),SIGNYY(MVSIZ),SIGNXY(MVSIZ),SIGNYZ(MVSIZ),
     .   SIGNZX(MVSIZ),TSAIWU(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JFLAG,FAIL, ITER,JYEILD
      INTEGER ICC(MVSIZ),IFLAG(MVSIZ),FAIL_OLD(MVSIZ),IYEILD(MVSIZ),
     .         ISOFT(MVSIZ),IMODWP(MVSIZ)
      INTEGER NINDX,INDEX(MVSIZ),J,ICAS(MVSIZ)
      INTEGER IOFF(MVSIZ),ID,IFAIL0
C     REAL
      my_real
     .   DP1(MVSIZ), DP2(MVSIZ), DP3(MVSIZ),CB(MVSIZ),CN(MVSIZ),
     .   E11(MVSIZ), E22(MVSIZ), NU12(MVSIZ), NU21(MVSIZ),
     .   G12(MVSIZ), G23(MVSIZ), G31(MVSIZ), FMAX(MVSIZ),
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ), S4(MVSIZ), S5(MVSIZ),
     .   DS1(MVSIZ), DS2(MVSIZ), DS3(MVSIZ), DE(MVSIZ),
     .   DE1(MVSIZ), DE2(MVSIZ), WVEC(MVSIZ), T1(MVSIZ),
     .   T2(MVSIZ), T3(MVSIZ),LAMDA(MVSIZ), COEF(MVSIZ), 
     .   A11(MVSIZ), A12(MVSIZ), A22(MVSIZ),
     .   SO1(MVSIZ), SO2(MVSIZ), SO3(MVSIZ),WPLAMX(MVSIZ),
     . EPST1(MVSIZ),EPST2(MVSIZ),EPSM1(MVSIZ),EPSM2(MVSIZ),
     . EPS1T1(MVSIZ), EPS2T1(MVSIZ), SIGRST1(MVSIZ),
     . EPS1T2(MVSIZ), EPS2T2(MVSIZ), SIGRST2(MVSIZ),
     . EPS1C1(MVSIZ), EPS2C1(MVSIZ), SIGRSC1(MVSIZ),
     . EPS1C2(MVSIZ), EPS2C2(MVSIZ), SIGRSC2(MVSIZ),
     . EPS1T12(MVSIZ), EPS2T12(MVSIZ), SIGRST12(MVSIZ),
     . DMAX(MVSIZ),CC(MVSIZ),EPDR(MVSIZ), FYLD(MVSIZ),
     .   F1(MVSIZ), F2(MVSIZ), F12(MVSIZ), F11(MVSIZ), F22(MVSIZ), 
     .   F33(MVSIZ),BETA(MVSIZ),WPLAREF(MVSIZ),
     .   EPSF1(MVSIZ),EPSF2(MVSIZ),
     .   SCALE, CNN, SCALE1, SCALE2, DAM1, DAM2,SIGYT1,SIGYT2,SIGYC1,
     .   SIGYC2,SIGYT12,ALPHA,
     .   SOFT(3),STRP12,COEFA,COEFB,DELTA,DWPLA,WPLAMXT1(MVSIZ),
     .   WPLAMXT2(MVSIZ),WPLAMXC1(MVSIZ),WPLAMXC2(MVSIZ),
     .   WPLAMXT12(MVSIZ),WPLA1,WPLA2,WPLA3,HT,
     .   EPSPLY(MVSIZ,5), EPLY(MVSIZ,3)
C=======================================================================
      IF(TT == ZERO) THEN
        DO I=JFT,JLT
            OFFPLY(I) = ONE
        ENDDO  
      ENDIF      
!     JFLAG can be set to :
!     if JFLAG = JLT --> homogenous group
!     if JFLAG = 0
      JFLAG=0
      JYEILD = 0
      DO 10 I=JFT,JLT
        DE(I)=PM(44,IMATLY)
        E11(I)  =PM(33,IMATLY)
        E22(I)  =PM(34,IMATLY)
        NU12(I) =PM(35,IMATLY)
        NU21(I) =PM(36,IMATLY)
        G12(I)  =PM(37,IMATLY)
        G23(I)  =PM(38,IMATLY)
        G31(I)  =PM(39,IMATLY)
        IFLAG(I)=NINT(PM(40,IMATLY))
        JFLAG   =JFLAG+IFLAG(I)
        F1(I)   =PM(54,IMATLY)
        F2(I)   =PM(55,IMATLY)
        F11(I)  =PM(56,IMATLY)
        F22(I)  =PM(57,IMATLY)
        F33(I)  =PM(58,IMATLY)
        F12(I)  =PM(59,IMATLY)
        WPLAMX(I)=PM(41,IMATLY)
        IOFF(I) =NINT(PM(42,IMATLY))
        CB(I)   =PM(46,IMATLY)
        CN(I)   =PM(47,IMATLY)
        FMAX(I) =PM(49,IMATLY)
        CC(I)   =PM(50,IMATLY)
        EPDR(I) =PM(51,IMATLY)
        ICC(I)  =NINT(PM(53,IMATLY))
        EPST1(I)=PM(60,IMATLY)
        EPST2(I)=PM(61,IMATLY)
        EPSM1(I)=PM(62,IMATLY)
        EPSM2(I)=PM(63,IMATLY)
        DMAX (I)=PM(64,IMATLY)
        WPLAREF(I)=PM(68,IMATLY)
        EPSF1(I)=PM(98,IMATLY)
        EPSF2(I)=PM(99,IMATLY)     
        IYEILD(I)=NINT(PM(187,IMATLY))
        IMODWP(I) =  NINT(PM(189,IMATLY))
        JYEILD = JYEILD + IYEILD(I)
        ETSE(I)=ONE
        IF(IFLAG(I)==0)SIGY(I)=ONE/SQRT(MAX(EM20,F33(I)))
        ISOFT(I) = 0
 10   CONTINUE
C-------------------------------------------------------------------
C     old failure
C-----------------------------
      DO I=JFT,JLT
        FAIL_OLD(I)=0
        IF(DAMT(I,1)>=DMAX(I))FAIL_OLD(I) = FAIL_OLD(I) + 1
        IF(DAMT(I,2)>=DMAX(I))FAIL_OLD(I) = FAIL_OLD(I) + 2
        IF(WPLA(I)<0.)THEN
C         Wpla is negative in case of layer already reached failure-p :
          FAIL_OLD(I) = FAIL_OLD(I) + 4
          WPLA(I)     = -WPLA(I)
        END IF
        IF(CRAK(I,1)>=EPSF1(I)-EPST1(I))THEN
          FAIL_OLD(I) = FAIL_OLD(I) + 8
        END IF
        IF(CRAK(I,2)>=EPSF2(I)-EPST2(I))THEN
          FAIL_OLD(I) = FAIL_OLD(I) + 16
        END IF
C       IF(WPLA(I)>=WPLAMX(I))FAIL_OLD(I) = FAIL_OLD(I) + 4
      ENDDO
C
      NINDX=0
      DO I=JFT,JLT
C        IF(FAIL_OLD(I)<4)THEN
        IF( FAIL_OLD(I)<4.OR.
     .     (FAIL_OLD(I)<8.AND.IOFF(I)<0) )THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        END IF
      ENDDO
C-------------------------------------------------------------------
C     REDUCTION DE SIG SUR CRITERE WPLA_OLD >= WPLAMX
C-----------------------------
      DO I=JFT,JLT
        IF((FAIL_OLD(I)>=4.AND.IOFF(I)>=0).OR.
     .      FAIL_OLD(I)>=8)THEN
         SIG(I,1)=FOUR_OVER_5*SIG(I,1)
         SIG(I,2)=FOUR_OVER_5*SIG(I,2)
         SIG(I,3)=FOUR_OVER_5*SIG(I,3)
         SIG(I,4)=FOUR_OVER_5*SIG(I,4)
         SIG(I,5)=FOUR_OVER_5*SIG(I,5)
C
         IF(ABS(SIG(I,1))<EM20) SIG(I,1)=ZERO
         IF(ABS(SIG(I,2))<EM20) SIG(I,2)=ZERO
         IF(ABS(SIG(I,3))<EM20) SIG(I,3)=ZERO
         IF(ABS(SIG(I,4))<EM20) SIG(I,4)=ZERO
         IF(ABS(SIG(I,5))<EM20) SIG(I,5)=ZERO
         EPS(I,1)=ZERO
         EPS(I,2)=ZERO
         EPS(I,3)=ZERO
         EPS(I,4)=ZERO
         EPS(I,5)=ZERO
C
        ENDIF
      ENDDO
C
      IF(ISHPLYXFEM /= 0 .AND. IPLYXFEM==2)THEN
        DO I=JFT,JLT
          EPSPLY(I,1)=LY_EXX(I)
          EPSPLY(I,2)=LY_EYY(I)
          EPSPLY(I,3)=HALF*LY_EXY(I)
          EPSPLY(I,4)=ZERO
          EPSPLY(I,5)=ZERO
        ENDDO
        CALL ROTOV(JFT,JLT,EPSPLY,DIR,NEL)
        DO I=JFT,JLT
          EPSPLY(I,3)=TWO*EPSPLY(I,3)
c         EPSPLY(I,4)=TWO*EPSPLY(I,4)
c         EPSPLY(I,5)=TWO*EPSPLY(I,5)
        ENDDO
        DO I=JFT,JLT
          IF((FAIL_OLD(I)>=4.AND.IOFF(I)>=0).OR.
     .      FAIL_OLD(I)>=8)THEN
            SIGPLY(I,1)=FOUR_OVER_5*SIGPLY(I,1)
            SIGPLY(I,2)=FOUR_OVER_5*SIGPLY(I,2)
            SIGPLY(I,3)=FOUR_OVER_5*SIGPLY(I,3)
            IF(ABS(SIGPLY(I,1))<EM20) SIGPLY(I,1)=ZERO
            IF(ABS(SIGPLY(I,2))<EM20) SIGPLY(I,2)=ZERO
            IF(ABS(SIGPLY(I,3))<EM20) SIGPLY(I,3)=ZERO
            EPSPLY(I,1)=ZERO
            EPSPLY(I,2)=ZERO
            EPSPLY(I,3)=ZERO
          END IF
        ENDDO
      END IF
C-----------------------------------------------
C Dev CRASURV
C-----------------------------------------------
      IF(JFLAG/=0)THEN
        DO 20 I=JFT,JLT
          EPS1T1(I)  =PM(166,IMATLY)
          EPS2T1(I)  =PM(167,IMATLY)
          SIGRST1(I) =PM(168,IMATLY)
          WPLAMXT1(I)=PM(169,IMATLY)
          EPS1T2(I)  =PM(170,IMATLY)
          EPS2T2(I)  =PM(171,IMATLY)
          SIGRST2(I) =PM(172,IMATLY)
          WPLAMXT2(I)=PM(173,IMATLY)
          EPS1C1(I)  =PM(174,IMATLY)
          EPS2C1(I)  =PM(175,IMATLY)
          SIGRSC1(I) =PM(176,IMATLY)
          WPLAMXC1(I)=PM(177,IMATLY)
          EPS1C2(I)  =PM(178,IMATLY)
          EPS2C2(I)  =PM(179,IMATLY)
          SIGRSC2(I) =PM(180,IMATLY)
          WPLAMXC2(I)=PM(181,IMATLY)
          EPS1T12(I) =PM(182,IMATLY)
          EPS2T12(I) =PM(183,IMATLY)
          SIGRST12(I)=PM(184,IMATLY)
          WPLAMXT12(I)=PM(185,IMATLY)
 20     CONTINUE
      ENDIF
C-------------------------------------------------------------------
C     ROTATIONS DANS LE REPERE D ORTHOTROPIE SIG, EPS
C-----------------------------
      DO I=JFT,JLT
        EPS(I,3)=HALF*EPS(I,3)
        EPS(I,4)=HALF*EPS(I,4)
        EPS(I,5)=HALF*EPS(I,5)
      ENDDO
      CALL ROTOV(JFT,JLT,EPS,DIR,NEL)
      DO I=JFT,JLT
        EPS(I,3)=TWO*EPS(I,3)
        EPS(I,4)=TWO*EPS(I,4)
        EPS(I,5)=TWO*EPS(I,5)
      ENDDO
Cst CRASURV +++
C.....STRAINS IN ORTHOTROPIC DIRECTIONS
      DO 30 I=JFT,JLT
      STRP1(I) = DIR(I,1)*DIR(I,1)*STRN1(I)
     .          +DIR(I,2)*DIR(I,2)*STRN2(I)
     .     +TWO*DIR(I,1)*DIR(I,2)*STRN3(I)
      STRP2(I) = DIR(I,2)*DIR(I,2)*STRN1(I)
     .          +DIR(I,1)*DIR(I,1)*STRN2(I)
     .     -TWO*DIR(I,2)*DIR(I,1)*STRN3(I)
   30 CONTINUE
Cst CRASURV ---
C-------------------------------------------------------------------
C     DEFORMATIONS ELASTIQUES
C-----------------------------
      IF(ISHPLYXFEM /= 0 .AND. IPLYXFEM==2)THEN 
         DO  I=JFT,JLT
           DE1(I)  =ONE-MAX( ZERO , SIGN(DAMT(I,1),SIG(I,1)) )
           DE2(I)  =ONE-MAX( ZERO , SIGN(DAMT(I,2),SIG(I,2)) )
           SCALE   =(HALF
     .        +SIGN(HALF,DE1(I)-ONE))*(HALF+SIGN(HALF,DE2(I)-ONE))
           S1(I) = SIG(I,1)/DE1(I)-NU12(I)*SIG(I,2)*SCALE
           S2(I) = SIG(I,2)/DE2(I)-NU21(I)*SIG(I,1)*SCALE
           S1(I)=S1(I)/E11(I)
           S2(I)=S2(I)/E22(I)
           S3(I)=SIG(I,3)/DE1(I)/DE2(I)/G12(I)
           S4(I)=SIG(I,4)/MAX(DE2(I)*G23(I)*SHF(I),EM30)
           S5(I)=SIG(I,5)/MAX(DE1(I)*G31(I)*SHF(I),EM30)
CC relatif displacement
           EPLY(I,1) = SIGPLY(I,1)/DE1(I)-NU12(I)*SIGPLY(I,2)*SCALE
           EPLY(I,2) = SIGPLY(I,2)/DE2(I)-NU21(I)*SIGPLY(I,1)*SCALE
           EPLY(I,1)=EPLY(I,1)/E11(I)
           EPLY(I,2)=EPLY(I,2)/E22(I)
           EPLY(I,3)=SIGPLY(I,3)/DE1(I)/DE2(I)/G12(I)           
         ENDDO      
C
         DO  I=JFT,JLT
           S1(I)=S1(I)+EPS(I,1)
           S2(I)=S2(I)+EPS(I,2)
           S3(I)=S3(I)+EPS(I,3)
           S4(I)=S4(I)+EPS(I,4)
           S5(I)=S5(I)+EPS(I,5)
CC relatif displacement
           EPLY(I,1)=EPLY(I,1)+EPSPLY(I,1)
           EPLY(I,2)=EPLY(I,2)+EPSPLY(I,2)
           EPLY(I,3)=EPLY(I,3)+EPSPLY(I,3)     
        ENDDO  
       ELSE
          DO 40 I=JFT,JLT
           DE1(I)  =ONE-MAX( ZERO , SIGN(DAMT(I,1),SIG(I,1)) )
           DE2(I)  =ONE-MAX( ZERO , SIGN(DAMT(I,2),SIG(I,2)) )
           SCALE   =(HALF
     .        +SIGN(HALF,DE1(I)-ONE))*(HALF+SIGN(HALF,DE2(I)-ONE))
           S1(I) = SIG(I,1)/DE1(I)-NU12(I)*SIG(I,2)*SCALE
           S2(I) = SIG(I,2)/DE2(I)-NU21(I)*SIG(I,1)*SCALE
           S1(I)=S1(I)/E11(I)
           S2(I)=S2(I)/E22(I)
           S3(I)=SIG(I,3)/DE1(I)/DE2(I)/G12(I)
           S4(I)=SIG(I,4)/MAX(DE2(I)*G23(I)*SHF(I),EM30)
  40       S5(I)=SIG(I,5)/MAX(DE1(I)*G31(I)*SHF(I),EM30)      
C
           DO 60 I=JFT,JLT
             S1(I)=S1(I)+EPS(I,1)
             S2(I)=S2(I)+EPS(I,2)
             S3(I)=S3(I)+EPS(I,3)
             S4(I)=S4(I)+EPS(I,4)
  60         S5(I)=S5(I)+EPS(I,5) 
C  
      ENDIF ! iplxfem       
C   
      IF(ISHPLYXFEM /= 0 .AND. IPLYXFEM==2)THEN 
#include "vectorize.inc"
         DO J=1,NINDX
          I=INDEX(J)
          IF (DAMT(I,1)/=ZERO) THEN
           CRAK(I,1)= CRAK(I,1) + EPS(I,1)+ EPSPLY(I,1)
           DAM1 = CRAK(I,1)/(EPSM1(I)-EPST1(I))
           DAM2 = DAM1*EPSM1(I)/(CRAK(I,1)+EPST1(I))
           DAMT(I,1)= MAX(DAMT(I,1),DAM2)
           DAMT(I,1)= MIN(DAMT(I,1),DMAX(I))      
          ENDIF
         ENDDO
C
#include "vectorize.inc"
        DO J=1,NINDX
          I=INDEX(J)
C
          IF (DAMT(I,2)/=ZERO) THEN
           CRAK(I,2)= CRAK(I,2)+EPS(I,2)+ EPSPLY(I,2)
           DAM1 = CRAK(I,2)/(EPSM2(I)-EPST2(I))
           DAM2 = DAM1*EPSM2(I)/(CRAK(I,2)+EPST2(I))
           DAMT(I,2)= MAX(DAMT(I,2),DAM2)
           DAMT(I,2)= MIN(DAMT(I,2),DMAX(I))
          ENDIF
        ENDDO 
      ELSE
#include "vectorize.inc"
         DO J=1,NINDX
          I=INDEX(J)
          IF (DAMT(I,1)/=ZERO) THEN
           CRAK(I,1)= CRAK(I,1) + EPS(I,1) 
           DAM1 = CRAK(I,1)/(EPSM1(I)-EPST1(I))
           DAM2 = DAM1*EPSM1(I)/(CRAK(I,1)+EPST1(I))
           DAMT(I,1)= MAX(DAMT(I,1),DAM2)
           DAMT(I,1)= MIN(DAMT(I,1),DMAX(I))      
          ENDIF
         ENDDO
C
        DO J=1,NINDX
          I=INDEX(J)
C
          IF (DAMT(I,2)/=ZERO) THEN
           CRAK(I,2)= CRAK(I,2)+EPS(I,2)
           DAM1 = CRAK(I,2)/(EPSM2(I)-EPST2(I))
           DAM2 = DAM1*EPSM2(I)/(CRAK(I,2)+EPST2(I))
           DAMT(I,2)= MAX(DAMT(I,2),DAM2)
           DAMT(I,2)= MIN(DAMT(I,2),DMAX(I))
          ENDIF
        ENDDO       
      
      
      
      ENDIF
C
      DO 68 I=JFT,JLT
      DE1(I)=ONE- MAX( ZERO , SIGN(DAMT(I,1),SIG(I,1)) )
      DE2(I)=ONE- MAX( ZERO , SIGN(DAMT(I,2),SIG(I,2)) )
      SCALE1 =(HALF
     .        +SIGN(HALF,DE1(I)-ONE))*(HALF+SIGN(HALF,DE2(I)-ONE))
      SCALE2 =ONE-NU12(I)*NU21(I)*SCALE1
      A11(I)= E11(I)*DE1(I)/SCALE2
      A22(I)= E22(I)*DE2(I)/SCALE2
   68 A12(I)=NU21(I)*A11(I)*SCALE1
C-----------------------------
C     CONTRAINTES ELASTIQUES
C-----------------------------
      DO 70 I=JFT,JLT
        T1(I)   =A11(I)*S1(I)+A12(I)*S2(I)
        T2(I)   =A12(I)*S1(I)+A22(I)*S2(I)
        T3(I)   =DE1(I)*DE2(I)*G12(I)*S3(I)
        SIG(I,4)=DE2(I)*G23(I)*SHF(I)*S4(I)
        SIG(I,5)=DE1(I)*G31(I)*SHF(I)*S5(I)
  70  CONTINUE
C
       IF(ISHPLYXFEM /= 0 .AND. IPLYXFEM==2)THEN
        DO I=JFT,JLT
!!          SIGP(1,I)   =A11(I)*EPSPLY(I,1)+A12(I)*EPSPLY(I,2)
!!          SIGP(2,I)   =A12(I)*EPSPLY(I,1)+A22(I)*EPSPLY(I,2)
!!          SIGP(3,I)   =DE1(I)*DE2(I)*G12(I)*EPSPLY(I,3)
          SIGPLY(I,1)	=A11(I)*EPLY(I,1)+A12(I)*EPLY(I,2)
          SIGPLY(I,2)	=A12(I)*EPLY(I,1)+A22(I)*EPLY(I,2)
          SIGPLY(I,3)	=DE1(I)*DE2(I)*G12(I)*EPLY(I,3)
        END DO
       END IF
C  
       DO I=JFT,JLT
          IF(T1(I) > 0) THEN
            IF(T2(I) > 0) ICAS(I) = 1
            IF(T2(I) < 0) ICAS(I) = 4
          ELSE
            IF(T2(I) > 0) ICAS(I) = 2
            IF(T2(I) < 0) ICAS(I) = 3
          ENDIF
       ENDDO   
C-------------------------------------------------------------------
C     STRAIN RATE
C-------------------------------------------------------------------
      DO I=JFT,JLT
      IF (ISRATE==0)EPSP(I) = MAX(
     .                   ABS(EPS(I,1)),ABS(EPS(I,2)),ABS(EPS(I,3)),
     .                   ABS(EPS(I,4)),ABS(EPS(I,5))) / MAX(DT1,EM30)
        IF(EPSP(I)>EPDR(I)) THEN
          EPSP(I)=LOG(EPSP(I)/EPDR(I))
        ELSE
          EPSP(I)=0.
        ENDIF
        COEF(I)=ZERO
      ENDDO
C-------------------------------------------------------------------
C     JFLAG :
C     if JFLAG = JLT --> homogenous group
C     if 0 < JFLAG < JLT --> heterogenous group
C-------------------------------------------------------------------
      IF(JFLAG==JLT)THEN
C-------------------------------------------------------------------
C     IFLAG = 1 ; homogeneous group
C-------------------------------------------------------------------
        DO I=JFT,JLT
         IF(WPLA(I)/=ZERO) THEN
           SIGYT1 = PM(141,IMATLY)*
     .           (ONE+PM(142,IMATLY)*EXP(PM(143,IMATLY)*LOG(WPLA(I))))
           SIGYT2 =PM(146,IMATLY)*
     .           (ONE+PM(147,IMATLY)*EXP(PM(148,IMATLY)*LOG(WPLA(I))))
           SIGYC1 =PM(151,IMATLY)*
     .           (ONE+PM(152,IMATLY)*EXP(PM(153,IMATLY)*LOG(WPLA(I))))
           SIGYC2 =PM(156,IMATLY)*
     .           (ONE+PM(157,IMATLY)*EXP(PM(158,IMATLY)*LOG(WPLA(I))))
           SIGYT12=PM(161,IMATLY)*
     .           (ONE+PM(162,IMATLY)*EXP(PM(163,IMATLY)*LOG(WPLA(I))))
         ELSE
           SIGYT1 = PM(141,IMATLY)
           SIGYT2 = PM(146,IMATLY)
           SIGYC1 = PM(151,IMATLY)
           SIGYC2 = PM(156,IMATLY)
           SIGYT12= PM(161,IMATLY)
         ENDIF
         IF(ICC(I)==1.OR.ICC(I)==3)THEN
           SIGYT1=MIN(SIGYT1,PM(144,IMATLY))
           SIGYT2=MIN(SIGYT2,PM(149,IMATLY))
           SIGYC1=MIN(SIGYC1,PM(154,IMATLY))
           SIGYC2=MIN(SIGYC2,PM(159,IMATLY))
           SIGYT12=MIN(SIGYT12,PM(164,IMATLY))
           SIGYT1=SIGYT1*(1.+PM(145,IMATLY)*EPSP(I))
           SIGYT2=SIGYT2*(1.+PM(150,IMATLY)*EPSP(I))
           SIGYC1=SIGYC1*(1.+PM(155,IMATLY)*EPSP(I))
           SIGYC2=SIGYC2*(1.+PM(160,IMATLY)*EPSP(I))
           SIGYT12=SIGYT12*(ONE +PM(165,IMATLY)*EPSP(I))
         ELSE
           SIGYT1=SIGYT1*(1.+PM(145,IMATLY)*EPSP(I))
           SIGYT2=SIGYT2*(1.+PM(150,IMATLY)*EPSP(I))
           SIGYC1=SIGYC1*(1.+PM(155,IMATLY)*EPSP(I))
           SIGYC2=SIGYC2*(1.+PM(160,IMATLY)*EPSP(I))
           SIGYT12=SIGYT12*(ONE +PM(165,IMATLY)*EPSP(I))
           SIGYT1=MIN(SIGYT1,PM(144,IMATLY))
           SIGYT2=MIN(SIGYT2,PM(149,IMATLY))
           SIGYC1=MIN(SIGYC1,PM(154,IMATLY))
           SIGYC2=MIN(SIGYC2,PM(159,IMATLY))
           SIGYT12=MIN(SIGYT12,PM(164,IMATLY))
         ENDIF
Cst CRASURV +++
C-------------------------------------------------------------------
C     SOFTENING 
C-------------------------------------------------------------------
         SOFT(1)=ZERO
         SOFT(2)=ZERO
         SOFT(3)=ZERO
         ISOFT(I) = 0 
C Direction 1
         IF(STRP1(I)<=-EPS1C1(I)) THEN
          SOFT(1)=MIN(ONE,(STRP1(I)+EPS1C1(I))/(EPS1C1(I)-EPS2C1(I)))
         ELSEIF(STRP1(I)>=EPS1T1(I)) THEN
          SOFT(1)=MIN(ONE,(STRP1(I)-EPS1T1(I))/(EPS2T1(I)-EPS1T1(I)))         
         ENDIF
         SIGYT1=MIN(SIGYT1,(ONE -SOFT(1))*SIGYT1+SOFT(1)*SIGRST1(I))
         SIGYC1=MIN(SIGYC1,(ONE -SOFT(1))*SIGYC1+SOFT(1)*SIGRSC1(I))
C Direction 2        
         IF(STRP2(I)<=-EPS1C2(I)) THEN
          SOFT(2)=MIN(ONE,(STRP2(I)+EPS1C2(I))/(EPS1C2(I)-EPS2C2(I)))
         ELSEIF(STRP2(I)>=EPS1T2(I)) THEN
          SOFT(2)=MIN(ONE,(STRP2(I)-EPS1T2(I))/(EPS2T2(I)-EPS1T2(I)))
         ENDIF
         SIGYT2=MIN(SIGYT2,(ONE -SOFT(2))*SIGYT2+SOFT(2)*SIGRST2(I))
         SIGYC2=MIN(SIGYC2,(ONE -SOFT(2))*SIGYC2+SOFT(2)*SIGRSC2(I))
C Direction 12
         STRP12=-DIR(I,1)*DIR(I,2)*STRN1(I)
     .          +DIR(I,2)*DIR(I,1)*STRN2(I)
     .         +(DIR(I,1)*DIR(I,1)-DIR(I,2)*DIR(I,2))*STRN3(I)
         IF(STRP12<=-EPS1T12(I)) THEN
          SOFT(3)=MIN(ONE,(STRP12+EPS1T12(I))/(EPS1T12(I)-EPS2T12(I)))
         ELSEIF(STRP12>=EPS1T12(I)) THEN
          SOFT(3)=MIN(ONE,(STRP12-EPS1T12(I))/(EPS2T12(I)-EPS1T12(I)))
         ENDIF
         SIGYT12=MIN(SIGYT12,(ONE - SOFT(3))*SIGYT12 + SOFT(3)*SIGRST12(I))
         IF(SOFT(1) /= ZERO .OR. SOFT(2) /= ZERO .OR. SOFT(3) /= ZERO) 
     .         ISOFT(I) =1
Cst CRASURV ---
C
         F1(I)  = ONE/SIGYT1-ONE/SIGYC1
         F2(I)  = ONE/SIGYT2-ONE/SIGYC2
         F11(I) = ONE/(SIGYT1*SIGYC1)
         F22(I) = ONE/(SIGYT2*SIGYC2)
         F33(I) = ONE/(SIGYT12*SIGYT12)	 
         ALPHA = F12(I)
         F12(I) = -ALPHA/(TWO*SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2))
         SIGY(I)=MIN(SIGYT1,SIGYC1,SIGYT2,SIGYC2,SIGYT12)
C
         IF(IYEILD(I) > 0) THEN
           IF(ICAS(I) == 1)THEN
             F11(I)= ONE/(SIGYT1*SIGYT1)
             F22(I)= ONE/(SIGYT2*SIGYT2)
             F33(I)= ONE/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==2)THEN
             F11(I)= ONE/(SIGYC1*SIGYC1)
             F22(I)= ONE/(SIGYT2*SIGYT2)
             F33(I)= ONE/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==3) THEN
             F11(I)= ONE/(SIGYC1*SIGYC1)
             F22(I)= ONE/(SIGYC2*SIGYC2)
             F33(I)= ONE/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) == 4) THEN
             F11(I)= ONE/(SIGYT1*SIGYT1)
             F22(I)= ONE/(SIGYC2*SIGYC2)
             F33(I)= ONE/(SIGYT12*SIGYT12)
             F12(I) =  -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ENDIF
         ENDIF
         EPSP(I)=ONE + CC(I) * EPSP(I)
         IF(ICC(I)==3.OR.ICC(I)==4)THEN
           WPLAMX(I)    = WPLAMX(I)*EPSP(I)
           WPLAMXT1(I)  = WPLAMXT1(I)*EPSP(I)
           WPLAMXT2(I)  = WPLAMXT2(I)*EPSP(I)
           WPLAMXC1(I)  = WPLAMXC1(I)*EPSP(I)
           WPLAMXC2(I)  = WPLAMXC2(I)*EPSP(I)
           WPLAMXT12(I) = WPLAMXT12(I)*EPSP(I)
         ENDIF
         FYLD(I)= ONE
         FMAX(I)= ONE
        ENDDO
      ELSE
C-------------------------------------------------------------------
C     IFLAG = 0
C-------------------------------------------------------------------
       DO I=JFT,JLT
        EPSP(I)=ONE + CC(I) * EPSP(I)
        IF (WPLA(I)/=ZERO) THEN
           FYLD(I)=(ONE+CB(I)*EXP(CN(I)*LOG(WPLA(I))))*EPSP(I)
        ELSE
           FYLD(I)=EPSP(I)
        ENDIF
        IF(ICC(I)==1.OR.ICC(I)==3)THEN
          FMAX(I) = FMAX(I)*EPSP(I)
        ENDIF
        IF(ICC(I)==3.OR.ICC(I)==4)THEN
          WPLAMX(I) = WPLAMX(I)*EPSP(I)
        ENDIF
          FYLD(I)= MIN(FMAX(I),FYLD(I))
        IF(IYEILD(I) > 0) THEN
           ALPHA = F12(I)
           SIGYT1 =PM(141,IMATLY)
           SIGYT2 =PM(146,IMATLY)
           SIGYC1 =PM(151,IMATLY)
           SIGYC2 =PM(156,IMATLY)
           SIGYT12=PM(161,IMATLY)
c            ALPHA(I) =  
           IF(ICAS(I) == 1)THEN
             F11(I)= ONE/(SIGYT1*SIGYT1)
             F22(I)= ONE/(SIGYT2*SIGYT2)
             F33(I)= ONE/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==2)THEN
             F11(I)= ONE/(SIGYC1*SIGYC1)
             F22(I)= ONE/(SIGYT2*SIGYT2)
             F33(I)= ONE/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) ==3) THEN
             F11(I)= ONE/(SIGYC1*SIGYC1)
             F22(I)= ONE/(SIGYC2*SIGYC2)
             F33(I)= ONE/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ELSEIF(ICAS(I) == 4) THEN
             F11(I)= ONE/(SIGYT1*SIGYT1)
             F22(I)= ONE/(SIGYC2*SIGYC2)
             F33(I)= ONE/(SIGYT12*SIGYT12)
             F12(I) = -ALPHA/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
           ENDIF
          SIGY(I)=MIN(SIGYT1,SIGYC1,SIGYT2,SIGYC2,SIGYT12)
         ENDIF            
       ENDDO
      ENDIF
C-------------------------------------------------------------------
C     PLASTICITE
C-------------------------------------------------------------------
      IF(JYEILD == 0) THEN
C-----------------------------------------------------------------
C  TSAI wu criteria
C---------------------------------------------------------------     
         DO 100 I=JFT,JLT
           WVEC(I)=F1(I) *T1(I)       + F2(I) *T2(I) +
     .             F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I) +
     .             F33(I)*T3(I)*T3(I) + TWO*F12(I)*T1(I)*T2(I)
           TSAIWU(I) = MIN(WVEC(I)/FYLD(I),ONE)
!!c    equiv crit for output
!!           SEQ_OUTP(I) = WVEC(I)
 100     CONTINUE
C
        DO I=JFT,JLT
          IF (WVEC(I)>FYLD(I).AND.OFF(I)==ONE) COEF(I)=ONE    
          CNN=CN(I)-ONE
          WVEC(I)=ZERO
          IF(WPLA(I)>ZERO.AND.FYLD(I)<FMAX(I)) 
     .           WVEC(I)=EPSP(I)*EXP(CNN*LOG(WPLA(I)))
        ENDDO
C
Cst CRASURV +++
        DO 110 I=JFT,JLT                                                       
          BETA(I)= ONE                                                          
          IF(COEF(I)==ZERO.OR.IFLAG(I)==0) GO TO 110                       
          COEFA=F11(I)*SIG(I,1)*SIG(I,1)+F22(I)*SIG(I,2)*SIG(I,2) +            
     .          F33(I)*SIG(I,3)*SIG(I,3)+                                      
     .       TWO*F12(I)*SIG(I,1)*SIG(I,2)                                     
          COEFB=F1(I)*SIG(I,1)+F2(I)*SIG(I,2)                                  
              DELTA=COEFB*COEFB + FOUR*COEFA*FYLD(I)                         
          IF(DELTA<ZERO)THEN                                                
           IF(IMCONV==1 .AND. OUTV(I) == 0)THEN 
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER)
              OUTV(I) = 1
#include "lockoff.inc"                                                       
           ENDIF                                                               
           GO TO 110                                                           
          ELSE                                                                 
            DELTA=SQRT(DELTA)                                                  
          ENDIF                                                                
          IF(ABS(COEFA)<=EM15) THEN                                          
            IF(ABS(COEFB)<=EM15) THEN                                        
             IF(IMCONV==1 .AND. OUTV(I) == 0)THEN  
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER)
              OUTV(I) = 1
#include "lockoff.inc"                                                       
              ENDIF                                                             
               GO TO 110                                                       
              ELSE                                                             
               BETA(I)=FYLD(I)/COEFB                                           
             ENDIF                                                              
          ENDIF                                                                
          IF(ABS(ONE+(COEFB-DELTA)/TWO/COEFA)<=                              
     .       ABS(ONE+(COEFB+DELTA)/TWO/COEFA)) THEN                            
           BETA(I)=(-COEFB+DELTA)/TWO/COEFA                                   
          ELSE                                                                 
           BETA(I)=(-COEFB-DELTA)/TWO/COEFA                                   
          ENDIF                                                                
          DELTA=F1(I) *BETA(I)*SIG(I,1) +F2(I) *BETA(I)*SIG(I,2) +             
     .          F11(I)*BETA(I)*SIG(I,1)*BETA(I)*SIG(I,1) +                     
     .          F22(I)*BETA(I)*SIG(I,2)*BETA(I)*SIG(I,2) +                     
     .          F33(I)*BETA(I)*SIG(I,3)*BETA(I)*SIG(I,3) +                     
     .          TWO*F12(I)*BETA(I)*SIG(I,1)*BETA(I)*SIG(I,2)                  
C--1-----|---2---|---3---|---4---|---5---|---6---|---7---|---8---|---9---|--10-|
 3000     FORMAT('No real solution for DELTA =',F11.4)                         
 3010     FORMAT('Cannot project stresses on criteria, COEFB =',F11.4)         

 110    CONTINUE                                                               
        DO I=JFT,JLT
C        BETA(I)=1.
           SO1(I)=BETA(I)*SIG(I,1)
           SO2(I)=BETA(I)*SIG(I,2)
           SO3(I)=BETA(I)*SIG(I,3)
        ENDDO
Cst CRASURV ---
C
        DO 120 I=JFT,JLT
          DP1(I)=F1(I)+TWO*F11(I)*SO1(I)+TWO*F12(I)*SO2(I)
          DP2(I)=F2(I)+TWO*F22(I)*SO2(I)+TWO*F12(I)*SO1(I)
          DP3(I)=TWO*F33(I)*SO3(I)
 120    CONTINUE
      ELSEIF(JYEILD == JLT) THEN
C------------------------------------------------------------
C MODIFID TSIA HILL CRITERIA        
C ----------------------------------------------------------
        DO I=JFT,JLT
           WVEC(I)= F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I) +
     .              F33(I)*T3(I)*T3(I) + F12(I)*T1(I)*T2(I)
           TSAIWU(I) = MIN(WVEC(I)/FYLD(I),ONE)
!!c    equiv crit for output
!!           SEQ_OUTP(I) = WVEC(I)
        ENDDO
C
        DO I=JFT,JLT
          IF (WVEC(I)>FYLD(I).AND.OFF(I)==ONE) COEF(I)=ONE    
          CNN=CN(I)-ONE
          WVEC(I)=ZERO
          IF(WPLA(I)>ZERO.AND.FYLD(I)<FMAX(I)) 
     .           WVEC(I)=EPSP(I)*EXP(CNN*LOG(WPLA(I)))
        ENDDO
C
Cst CRASURV +++
        DO  111 I=JFT,JLT                                                       
          BETA(I)=ONE                                                           
          IF(COEF(I)==ZERO.OR.IFLAG(I)==0) GO TO 111                       
           COEFA= F11(I)*SIG(I,1)*SIG(I,1) + F22(I)*SIG(I,2)*SIG(I,2) + 
     .            F33(I)*SIG(I,3)*SIG(I,3) + F12(I)*SIG(I,1)*SIG(I,2)
           DELTA = FYLD(I) / COEFA
          IF(DELTA <ZERO)THEN                                                
             IF(IMCONV==1 .AND. OUTV(I) == 0)THEN 
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER) 
             OUTV(I) = 1
#include "lockoff.inc"                                                       
              ENDIF
              GO TO 111
          ELSE 
CC
           DELTA = SQRT(DELTA)
           IF(ABS(COEFA)<=EM15) THEN                                        
                IF(IMCONV==1 .AND. OUTV(I) == 0)THEN
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER) 
             OUTV(I) = 1
#include "lockoff.inc"                                                       
                ENDIF 
                GO TO 111                            
            ENDIF                                                              
          ENDIF                                                                
          IF(ABS(ONE-DELTA)<=                              
     .       ABS(ONE+DELTA)) THEN                            
           BETA(I)= DELTA                             
          ELSE                                                                 
           BETA(I)=-DELTA                                  
          ENDIF
 111    CONTINUE                                    
        DO I=JFT,JLT
C        BETA(I)=1.
           SO1(I)=BETA(I)*SIG(I,1)
           SO2(I)=BETA(I)*SIG(I,2)
           SO3(I)=BETA(I)*SIG(I,3)
        ENDDO
C
C
        DO  I=JFT,JLT
          DP1(I)=TWO*F11(I)*SO1(I) +  F12(I)*SO2(I)
          DP2(I)=TWO*F22(I)*SO2(I) +  F12(I)*SO1(I)
          DP3(I)=TWO*F33(I)*SO3(I)
        ENDDO      
      
      ELSE
C------------------------------------------------------------
C MODIFID TSIA HILL CRITERIA     OR TSAI-WU CRITERIA   
C ----------------------------------------------------------        
        DO I=JFT,JLT
          IF(IYEILD(I) > 0) THEN
             WVEC(I)= F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I)+
     .                F33(I)*T3(I)*T3(I) + F12(I)*T1(I)*T2(I)
           ELSE
             WVEC(I)=  F1(I) *T1(I)       + F2(I) *T2(I) +
     .                 F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I) +
     .                 F33(I)*T3(I)*T3(I) + TWO*F12(I)*T1(I)*T2(I)
           ENDIF
           TSAIWU(I) = MIN(WVEC(I)/FYLD(I),ONE)
!!c    equiv crit for output
!!           SEQ_OUTP(I) = WVEC(I)
        ENDDO      
C
        DO I=JFT,JLT
          IF (WVEC(I)>FYLD(I).AND.OFF(I)==ONE) COEF(I)=ONE    
          CNN=CN(I)-ONE
          WVEC(I)=ZERO
          IF(WPLA(I)>ZERO.AND.FYLD(I)<FMAX(I)) 
     .           WVEC(I)=EPSP(I)*EXP(CNN*LOG(WPLA(I)))
        ENDDO
CC
        DO  112 I=JFT,JLT                                                       
          BETA(I)=ONE                                                           
          IF(COEF(I)==ZERO.OR.IFLAG(I)==0) GO TO 112 
           IF(IYEILD(I) > 0) THEN     
C--------------------------------------------------------------
C  TSAI HILL
C--------------------------------------------------------------
             COEFA =F11(I)*SIG(I,1)*SIG(I,1) + F22(I)*SIG(I,2)*SIG(I,2)+ 
     .              F33(I)*SIG(I,3)*SIG(I,3) + F12(I)*SIG(I,1)*SIG(I,2)
             DELTA = FYLD(I) / COEFA
             IF(DELTA <ZERO)THEN  
               IF(IMCONV==1 .AND. OUTV(I) == 0)THEN 
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER) 
             OUTV(I) = 1
#include "lockoff.inc"                                                       
               ENDIF
                GO TO 112
             ELSE 
C
              DELTA = SQRT(DELTA)
               IF(ABS(COEFA)<=EM15) THEN 
                  IF(IMCONV==1 .AND. OUTV(I) == 0)THEN
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER)
                     OUTV(I) = 1
#include "lockoff.inc"                                                       
                 ENDIF 
                 GO TO 112                           
               ENDIF 
             ENDIF 
             IF(ABS(ONE-DELTA)<= ABS(ONE+DELTA)) THEN
                BETA(I)= DELTA                             
              ELSE 
                 BETA(I)=-DELTA                                  
              ENDIF
C             
             SO1(I)=BETA(I)*SIG(I,1)
             SO2(I)=BETA(I)*SIG(I,2)
             SO3(I)=BETA(I)*SIG(I,3)
C             
             DP1(I)= TWO*F11(I)*SO1(I) +  F12(I)*SO2(I)
             DP2(I)= TWO*F22(I)*SO2(I) +  F12(I)*SO1(I)
             DP3(I)= TWO*F33(I)*SO3(I)
          ELSE
C--------------------------------------------------------------------
C TSAI WU
C --------------------------------------------------------------------         
             COEFA=F11(I)*SIG(I,1)*SIG(I,1)+F22(I)*SIG(I,2)*SIG(I,2) + 
     .            F33(I)*SIG(I,3)*SIG(I,3)+TWO*F12(I)*SIG(I,1)*SIG(I,2) 
             COEFB=F1(I)*SIG(I,1)+F2(I)*SIG(I,2)
             DELTA=COEFB*COEFB + FOUR*COEFA*FYLD(I)                         
            IF(DELTA<ZERO)THEN 
              IF(IMCONV==1 .AND. OUTV(I) == 0)THEN 
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER) 
                 OUTV(I) = 1
#include "lockoff.inc"                                                       
              ENDIF 
              GO TO 112 
             ELSE 
              DELTA=SQRT(DELTA)
          ENDIF                                                                
          IF(ABS(COEFA)<=EM15) THEN                                          
            IF(ABS(COEFB)<=EM15) THEN                                        
             IF(IMCONV==1 .AND. OUTV(I) == 0)THEN 
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER)
             OUTV(I) = 1                                        
#include "lockoff.inc"                                                       
             ENDIF                                                             
               GO TO 112                                                       
              ELSE                                                             
               BETA(I)=FYLD(I)/COEFB                                           
            ENDIF                                                              
          ENDIF                                                                
          IF(ABS(ONE+(COEFB-DELTA)/TWO/COEFA)<=                              
     .       ABS(ONE+(COEFB+DELTA)/TWO/COEFA)) THEN                            
           BETA(I)=(-COEFB+DELTA)/TWO/COEFA                                   
          ELSE                                                                 
           BETA(I)=(-COEFB-DELTA)/TWO/COEFA                                   
          ENDIF
C
           SO1(I)=BETA(I)*SIG(I,1)
           SO2(I)=BETA(I)*SIG(I,2)
           SO3(I)=BETA(I)*SIG(I,3)           
C
           DP1(I)=F1(I)+TWO*F11(I)*SO1(I)+TWO*F12(I)*SO2(I)
           DP2(I)=F2(I)+TWO*F22(I)*SO2(I)+TWO*F12(I)*SO1(I)
           DP3(I)=TWO*F33(I)*SO3(I)
C                     
         ENDIF   
CC
 112    CONTINUE                   
C fin de des cirterias
      ENDIF
C      
      DO 130 I=JFT,JLT
          DS1(I)=T1(I)-SO1(I)
          DS2(I)=T2(I)-SO2(I)
          DS3(I)=T3(I)-SO3(I)
 130  CONTINUE
C 
      DO 160 I=JFT,JLT
          LAMDA(I)=(DP1(I)*DS1(I)+DP2(I)*DS2(I)+DP3(I)*DS3(I))*COEF(I)
          IF(LAMDA(I)==ZERO) GO TO 160
          LAMDA(I)=LAMDA(I)*COEF(I)/
     .         (DP1(I)*(A11(I)*DP1(I)+A12(I)*DP2(I))+
     .          DP2(I)*(A12(I)*DP1(I)+A22(I)*DP2(I))+
     .       TWO*DP3(I)*G12(I)*DE1(I)*DE2(I)*DP3(I)+
     .         (SO1(I)*DP1(I)+SO2(I)*DP2(I)+TWO*SO3(I)*DP3(I))
     .          *CN(I)*CB(I)*WVEC(I) )
 160  CONTINUE
C
      DO 170 I=JFT,JLT
          DP1(I)=LAMDA(I)*DP1(I)
          DP2(I)=LAMDA(I)*DP2(I)
          DP3(I)=LAMDA(I)*DP3(I)
 170  CONTINUE
C              
      DO 180 I=JFT,JLT
        T1(I)=T1(I)-A11(I)*DP1(I)-A12(I)*DP2(I)
        T2(I)=T2(I)-A12(I)*DP1(I)-A22(I)*DP2(I)
        T3(I)=T3(I)-G12(I)*DE1(I)*DE2(I)*DP3(I)*TWO
 180  CONTINUE
C
      DO 190 I=JFT,JLT                  
        DWPLA = HALF*
     .      (DP1(I)*(T1(I)+SO1(I))+
     .       DP2(I)*(T2(I)+SO2(I))+
     .       TWO*DP3(I)*(T3(I)+SO3(I)))
         DWPLA = MAX(DWPLA ,ZERO) / WPLAREF(I)
         WPLA(I)=WPLA(I)+ DWPLA
C needed for print         
         ICAS(I) = 0
 190  CONTINUE
C
C  for crusurv
C
      IF(JFLAG > 0 ) THEN
        DO I=JFT,JLT  
            SIGYT1 = PM(141,IMATLY)
            SIGYT2 = PM(146,IMATLY)
            SIGYC1 = PM(151,IMATLY)
            SIGYC2 = PM(156,IMATLY)
            SIGYT12= PM(161,IMATLY)
C                       
            IF(IFLAG(I) == 1 .AND. 
     .         (IMODWP(I) == 0 .OR. (IMODWP(I)> 0 .AND. ISOFT(I) == 0 ))) THEN
C Direction 1       
              WPLA1 = EP30
              IF(T1(I) >= SIGYT1 ) THEN
                 WPLA1 = WPLAMXT1(I)
                 ID = 1
              ELSEIF(T1(I) <= -SIGYC1) THEN
                  WPLA1 = WPLAMXC1(I)
                  ID =-1
              ENDIF 
C                           
             IF(WPLA1 < WPLAMX(I)) THEN
                WPLAMX(I) = WPLA1
                ICAS(I) = ID
             ENDIF
C Direction 2             
             WPLA2 = EP30
             IF(T2(I) >= SIGYT2  ) THEN
                WPLA2 = WPLAMXT2(I)
                ID =  2
             ELSEIF(T2(I) <= -SIGYC2)THEN
                WPLA2 = WPLAMXC2(I)
                ID = -2
             ENDIF
             IF(WPLA2 < WPLAMX(I) ) THEN
               WPLAMX(I) = WPLA2
               ICAS(I) = ID
             ENDIF
C shear 
             WPLA3 = EP30
             IF(ABS(T3(I)) >= SIGYT12) THEN
                WPLA3 = WPLAMXT12(I)
                ID = 3
             ENDIF          
             IF( WPLA3 < WPLAMX(I) )THEN               
               WPLAMX(I) = WPLA3
               ICAS(I) = ID
             ENDIF
          ELSEIF( IFLAG(I) == 1 .AND. IMODWP(I) > 0 ) THEN  
             ID = 1
             IF(ABS(T2(I)) > ABS(T1(I)))THEN
                ID = 2
                IF(ABS(T3(I)) > ABS(T2(I))) ID = 3
             ELSEIF( ABS(T3(I)) > ABS(T1(I))) THEN
                ID = 3
             ENDIF
             SELECT CASE(ID)
               CASE(1)
                ICAS(I) = SIGN(ONE,T1(I)) 
                IF(ICAS(I) > 0 ) THEN
                   WPLAMX(I) = MIN(WPLAMX(I),WPLAMXT1(I))
                ELSE
                   WPLAMX(I)  = MIN(WPLAMX(I),WPLAMXC1(I))
                ENDIF 
               CASE(2)
                ICAS(I) = SIGN(TWO,T2(I)) 
                IF(ICAS(I) > 0 ) THEN
                  WPLAMX(I) = MIN(WPLAMX(I),WPLAMXT2(I))
                ELSE
                  WPLAMX(I) = MIN(WPLAMX(I),WPLAMXC2(I))
                ENDIF 
               CASE(3)
                 ICAS(I) = 3 
                 WPLAMX(I) = MIN(WPLAMX(I),WPLAMXT12(I))
             END SELECT 
          ENDIF
        ENDDO 
      ENDIF 
C      
      DO I=JFT,JLT
C       
        IFAIL0  =  MOD(FAIL_OLD(I),8)
        IF(WPLA(I)>=WPLAMX(I) .OR.IFAIL0 >= 4 )WPLAR(I)= WPLAR(I)+ONE
        IF(WPLA(I)>=WPLAMX(I).OR.IFAIL0 >= 4 .OR. 
     .     DAMT(I,1)>=DMAX(I).OR.
     .     CRAK(I,1)>=EPSF1(I)-EPST1(I)) NFIS1(I)=NFIS1(I)+1
        IF(WPLA(I)>=WPLAMX(I).OR.IFAIL0 >= 4 .OR.
     .     DAMT(I,2)>=DMAX(I).OR.
     .     CRAK(I,2)>=EPSF2(I)-EPST2(I)) NFIS2(I)=NFIS2(I)+1
        IF(WPLA(I)>=WPLAMX(I).OR.IFAIL0 >= 4 .OR.
     .     DAMT(I,1)>=DMAX(I).OR.
     .     CRAK(I,1)>=EPSF1(I)-EPST1(I).OR.
     .     DAMT(I,2)>=DMAX(I).OR.
     .     CRAK(I,2)>=EPSF2(I)-EPST2(I)) NFIS3(I)=NFIS3(I)+1 
      ENDDO
C
C-------------------------------------------------------------------
C     failure
C-----------------------------
      DO I=JFT,JLT
        FAIL=0
        IF(DAMT(I,1)>=DMAX(I))FAIL = FAIL + 1
        IF(DAMT(I,2)>=DMAX(I))FAIL = FAIL + 2
        IF(WPLA(I)>=WPLAMX(I))FAIL = FAIL + 4
        IF(CRAK(I,1)>=EPSF1(I)-EPST1(I))FAIL=FAIL+8
        IF(CRAK(I,2)>=EPSF2(I)-EPST2(I))FAIL=FAIL+16
        IF(FAIL > 0 ) OFFPLY(I) = ZERO
         
        IF(FAIL/=FAIL_OLD(I).AND.OFF(I)==ONE) THEN
          FAIL = FAIL - FAIL_OLD(I)
         IF(IMCONV==1)THEN
#include "lockon.inc"
         IF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
          IF(FAIL==1.OR.FAIL==3.OR.FAIL==5)
     +         WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +         ' FAILURE-1 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT     
          IF(FAIL==2.OR.FAIL==3.OR.FAIL==6)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +         ' FAILURE-2 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT    
          ENDIF    
          IF(FAIL==4.OR.FAIL==5.OR.FAIL==6)THEN
             IF(ICAS(I) == 0)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +         ' FAILURE-P ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT 
             ELSEIF(ICAS(I) == 1)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +         ' FAILURE-P-T1 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT 
             ELSEIF(ICAS(I) == -1)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +         ' FAILURE-P-C1 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT 
             ELSEIF(ICAS(I) == 2)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +         ' FAILURE-P-T2 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT
             ELSEIF(ICAS(I) == -2)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +         ' FAILURE-P-C2 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT 
             ELSEIF(ICAS(I) == 3)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +         ' FAILURE-P-T12 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT
            ENDIF
           ENDIF      
          IF(FAIL>=16)THEN
            WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +        ' TOTAL FAILURE-2 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +        ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT      
          ELSEIF(FAIL>=8)THEN
            WRITE(IOUT, '(A,I10,A,I3,A,I3,A,I10,A,1PE11.4)')
     +        ' TOTAL FAILURE-1 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +        ', INTEGRATION POINT #',IPG,', (PLY #',PLY_ID,'), TIME=',TT      
          END IF
c
         ELSE
c
          IF(FAIL==1.OR.FAIL==3.OR.FAIL==5)
     +         WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-1 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', TIME=',TT     
          IF(FAIL==2.OR.FAIL==3.OR.FAIL==6)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-2 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', TIME=',TT    
          ENDIF    
          IF(FAIL==4.OR.FAIL==5.OR.FAIL==6)THEN
             IF(ICAS(I) == 0)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-P ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', TIME=',TT 
             ELSEIF(ICAS(I) == 1)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-P-T1 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', TIME=',TT 
             ELSEIF(ICAS(I) == -1)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-P-C1 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', TIME=',TT 
             ELSEIF(ICAS(I) == 2)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-P-T2 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', TIME=',TT
             ELSEIF(ICAS(I) == -2)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-P-C2 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', TIME=',TT 
             ELSEIF(ICAS(I) == 3)THEN
               WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +         ' FAILURE-P-T12 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +         ', INTEGRATION POINT #',IPG,', TIME=',TT
            ENDIF
           ENDIF      
          IF(FAIL>=16)THEN
            WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' TOTAL FAILURE-2 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +        ', INTEGRATION POINT #',IPG,', TIME=',TT      
          ELSEIF(FAIL>=8)THEN
            WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' TOTAL FAILURE-1 ELEMENT #',NGL(I),', LAYER #',ILAYER,
     +        ', INTEGRATION POINT #',IPG,', TIME=',TT      
          END IF
         ENDIF
#include "lockoff.inc"
         ENDIF
        ENDIF
C       Wpla negative for stresses reduction at next cycle in case of
C       failure-p :
        IF(WPLA(I)>=WPLAMX(I).OR.MOD(FAIL_OLD(I),8)>=4)
     .                WPLA(I)=-WPLA(I)
      ENDDO      
c
      DO I=JFT,JLT
        SIG(I,1)=T1(I)
        SIG(I,2)=T2(I)
        SIG(I,3)=T3(I)
      ENDDO      
C------for QEPH      
      DO I=JFT,JLT
          SIGY(I)=FYLD(I)*SIGY(I)
          IF (ABS(BETA(I)) < ONE) THEN
           IF(WPLA(I)>ZERO.AND.FYLD(I)<FMAX(I))THEN
             HT=CN(I)*CB(I)*EXP((CN(I)-ONE)*LOG(WPLA(I)))
           ELSE
             HT=EM10
           END IF
           ETSE(I)= HT/(HT+E11(I))
          END IF
      END DO 
C-------------------
C     PLASTICITY END
C-------------------
      DO I=JFT,JLT
        SIGNXX(I) = SIG(I,1)
        SIGNYY(I) = SIG(I,2)
        SIGNXY(I) = SIG(I,3)
        SIGNYZ(I) = SIG(I,4)
        SIGNZX(I) = SIG(I,5)
      ENDDO
C-------------------------------------------------------------------
C     RETOUR DANS LE REPERE COQUE
C-----------------------------
      DO I=JFT,JLT
        SIGE(I,1)=SIG(I,1)
        SIGE(I,2)=SIG(I,2)
        SIGE(I,3)=SIG(I,3)
        SIGE(I,4)=SIG(I,4)
        SIGE(I,5)=SIG(I,5)
      ENDDO
      CALL UROTOV(JFT,JLT,SIGE,DIR,NEL)
C
      IF(ISHPLYXFEM /= 0 .AND. IPLYXFEM==2)THEN
C-----------------------------------------------------------------
C  TSAI wu criteria
C---------------------------------------------------------------     
         DO  I=JFT,JLT
           T1(I) = SIGPLY(I,1)
           T2(I) = SIGPLY(I,2)
           T3(I) = SIGPLY(I,3)
           WVEC(I)=F1(I) *T1(I)       + F2(I) *T2(I) +
     .             F11(I)*T1(I)*T1(I) + F22(I)*T2(I)*T2(I) +
     .             F33(I)*T3(I)*T3(I) + TWO*F12(I)*T1(I)*T2(I)
         ENDDO
C
        DO I=JFT,JLT
          COEF(I) = ZERO
          IF (WVEC(I)>FYLD(I).AND.OFF(I)==ONE) COEF(I)=ONE    
          CNN=CN(I)-ONE
          WVEC(I)=ZERO
          IF(WPLA(I)>ZERO.AND.FYLD(I)<FMAX(I)) 
     .           WVEC(I)=EPSP(I)*EXP(CNN*LOG(WPLA(I)))
        ENDDO
C
Cst CRASURV +++
        DO 222 I=JFT,JLT                                                       
          BETA(I)= ONE                                                          
          IF(COEF(I)==ZERO.OR.IFLAG(I)==0) GO TO 222                       
           COEFA=F11(I)*T1(I)*T1(I)+F22(I)*T2(I)*T2(I) +            
     .          F33(I)*T3(I)*T3(I)+                                      
     .         TWO*F12(I)*T1(I)*T2(I)                                     
              COEFB=F1(I)*T1(I)+F2(I)*T2(I)                                  
              DELTA=COEFB*COEFB + FOUR*COEFA*FYLD(I)                         
          IF(DELTA<ZERO)THEN                                                
           IF(IMCONV==1 .AND. OUTV(I) == 0)THEN 
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER)
              OUTV(I) = 1
#include "lockoff.inc"                                                       
           ENDIF                                                               
           GO TO 222                                                           
          ELSE                                                                 
            DELTA=SQRT(DELTA)                                                  
          ENDIF                                                                
          IF(ABS(COEFA)<=EM15) THEN                                          
            IF(ABS(COEFB)<=EM15) THEN                                        
             IF(IMCONV==1 .AND. OUTV(I) == 0)THEN  
#include "lockon.inc"                                                        
              CALL ANCMSG(MSGID=244,ANMODE=ANINFO,
     *                             I1=NGL(I),I2=ILAYER)
              OUTV(I) = 1
#include "lockoff.inc"                                                       
              ENDIF                                                             
               GO TO 222                                                       
              ELSE                                                             
               BETA(I)=FYLD(I)/COEFB                                           
             ENDIF                                                              
          ENDIF                                                                
          IF(ABS(ONE+(COEFB-DELTA)/TWO/COEFA)<=                              
     .       ABS(ONE+(COEFB+DELTA)/TWO/COEFA)) THEN                            
           BETA(I)=(-COEFB+DELTA)/TWO/COEFA                                   
          ELSE                                                                 
           BETA(I)=(-COEFB-DELTA)/TWO/COEFA                                   
          ENDIF                                                                               
C--1-----|---2---|---3---|---4---|---5---|---6---|---7---|---8---|---9---|--10-  

 222    CONTINUE                                                               
        DO I=JFT,JLT
           SO1(I)=BETA(I)*T1(I)
           SO2(I)=BETA(I)*T2(I)
           SO3(I)=BETA(I)*T3(I)
        ENDDO
Cst CRASURV ---
      DO  I=JFT,JLT
          DP1(I)=F1(I)+TWO*F11(I)*SO1(I)+TWO*F12(I)*SO2(I)
          DP2(I)=F2(I)+TWO*F22(I)*SO2(I)+TWO*F12(I)*SO1(I)
          DP3(I)=TWO*F33(I)*SO3(I)
      ENDDO
      DO  I=JFT,JLT
          DS1(I)=T1(I)-SO1(I)
          DS2(I)=T2(I)-SO2(I)
          DS3(I)=T3(I)-SO3(I)
      ENDDO
C 
      DO  I=JFT,JLT
          LAMDA(I)=(DP1(I)*DS1(I)+DP2(I)*DS2(I)+DP3(I)*DS3(I))*COEF(I)
          IF(LAMDA(I) /= ZERO)THEN 
           LAMDA(I)=LAMDA(I)*COEF(I)/
     .         (DP1(I)*(A11(I)*DP1(I)+A12(I)*DP2(I))+
     .          DP2(I)*(A12(I)*DP1(I)+A22(I)*DP2(I))+
     .       TWO*DP3(I)*G12(I)*DE1(I)*DE2(I)*DP3(I)+
     .         (SO1(I)*DP1(I)+SO2(I)*DP2(I)+TWO*SO3(I)*DP3(I))
     .          *CN(I)*CB(I)*WVEC(I) )
         ENDIF 
      ENDDO
C
      DO  I=JFT,JLT
          DP1(I)=LAMDA(I)*DP1(I)
          DP2(I)=LAMDA(I)*DP2(I)
          DP3(I)=LAMDA(I)*DP3(I)
      ENDDO
C              
      DO  I=JFT,JLT
        SIGPLY(I,1)=T1(I)-A11(I)*DP1(I)-A12(I)*DP2(I)
        SIGPLY(I,2)=T2(I)-A12(I)*DP1(I)-A22(I)*DP2(I)
        SIGPLY(I,3)=T3(I)-G12(I)*DE1(I)*DE2(I)*DP3(I)*TWO
      ENDDO
C            
       DO I=JFT,JLT
           SIGPE(I,1) = SIGPLY(I,1)
           SIGPE(I,2) = SIGPLY(I,2)
           SIGPE(I,3) = SIGPLY(I,3)
           SIGPE(I,4) = ZERO
           SIGPE(I,5) = ZERO
           STRP1(I) = STRP1(I) + EPLY(I,1)
           STRP2(I) = STRP2(I) + EPLY(I,2)
        ENDDO
        CALL UROTOV(JFT,JLT,SIGPE,DIR,NEL)
!!old        DO I=JFT,JLT
!!old          SIGPLY(I,1)=SIGPLY(I,1)+SIGP(1,I)
!!old          SIGPLY(I,2)=SIGPLY(I,2)+SIGP(2,I)
!!old          SIGPLY(I,3)=SIGPLY(I,3)+SIGP(3,I)
!!old        ENDDO
      END IF
C----   
      RETURN
      END
