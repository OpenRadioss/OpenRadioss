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
!||    m25law                ../engine/source/materials/mat/mat025/m25law.F
!||--- called by ------------------------------------------------------
!||    mmain                 ../engine/source/materials/mat_share/mmain.F90
!||--- calls      -----------------------------------------------------
!||    mat25_crasurv_s       ../engine/source/materials/mat/mat025/mat25_crasurv_s.F90
!||    mat25_tsaiwu_s        ../engine/source/materials/mat/mat025/mat25_tsaiwu_s.F90
!||    mreploc               ../engine/source/materials/mat_share/mreploc.F
!||    mrotens               ../engine/source/materials/mat_share/mrotens.F
!||--- uses       -----------------------------------------------------
!||    mat25_crasurv_s_mod   ../engine/source/materials/mat/mat025/mat25_crasurv_s.F90
!||    mat25_tsaiwu_s_mod    ../engine/source/materials/mat/mat025/mat25_tsaiwu_s.F90
!||    matparam_def_mod      ../common_source/modules/mat_elem/matparam_def_mod.F90
!||====================================================================
      SUBROUTINE M25LAW(MAT_PARAM,
     1   PM,        OFF,       SIG,
     2   EINT,      S01,       S02,       S03,
     3   S04,       S05,       S06,       D1,
     4   D2,        D3,        D4,        D5,
     5   D6,        RX,        RY,        RZ,
     6   SX,        SY,        SZ,        TX,
     7   TY,        TZ,        GAMA,      VNEW,
     8   SSP,       VOL,       EPSD,      WPLA,
     9   EPST,      SIGL,      TSAIWU,          
     A   FLAY,      NGL,       NEL,       NFT,             
     B   ILAY,      NPT,       IPG,
     C   JCVT,      JSPH,      ISORTH,    DMG,
     D   L_DMG,     OUTV )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use matparam_def_mod
      use mat25_tsaiwu_s_mod
      use mat25_crasurv_s_mod
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
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: NPT
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: JSPH
      INTEGER, INTENT(IN) :: ISORTH
      INTEGER, INTENT(IN) :: L_DMG
      INTEGER NGL(MVSIZ),ILAY,NEL,IPG
      INTEGER, INTENT(INOUT) :: OUTV(NEL)
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: EPSD ! local equivalent strain rate
      my_real
     .   PM(NPROPM),OFF(*), SIG(NEL,6), WPLA(*), GAMA(MVSIZ,6), 
     .   EINT(*),RX(*) ,RY(*),RZ(*) ,SX(*),SY(*),SZ(*),TX(*) ,TY(*) ,TZ(*)
      my_real
     .   D1(*), D2(*), D3(*), D4(*),
     .   D5(*), D6(*),
     .   S01(*),S02(*),S03(*),S04(*),S05(*),s06(*),
     .   VOl(*),VNEW(*),SSP(*),EPST(NEL,6),SIGL(MVSIZ,6),FLAY(*),
     .   TSAIWU(NEL)
      type (matparam_struct_) ,intent(in) :: mat_param
      my_real, DIMENSION(NEL,L_DMG) :: DMG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFIS1(MVSIZ), NFIS2(MVSIZ), NFIS3(MVSIZ), INDX(MVSIZ)
      INTEGER I, J, JADR, J1, J2, J3, JJ, II, I2, NINDX, I1,
     .   IOFF,JOFF,IDIR,IFLAG
C     REAL
      my_real
     .   DEGMB(MVSIZ),DEGFX(MVSIZ),
     .   WPLAR(MVSIZ),STRN1(MVSIZ),STRN2(MVSIZ),STRN3(MVSIZ),
     .   DAMCR(MVSIZ,2), DMAXT(MVSIZ),STRP1(MVSIZ) ,STRP2(MVSIZ),
     .   EPSPL(MVSIZ),S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),
     .   S4(MVSIZ),S5(MVSIZ),S6(MVSIZ),
     .   EP1(MVSIZ),EP2(MVSIZ),EP3(MVSIZ),EP4(MVSIZ),EP5(MVSIZ),
     .   EP6(MVSIZ),EPST1(MVSIZ),EPST2(MVSIZ),EPSM1(MVSIZ),EPSM2(MVSIZ),
     .   DMAX(MVSIZ),R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),
     .   R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
       my_real 
     .   ASRATE,EPS_K2, EPS_M2, SIGT1, SIGT2, 
     .   ZT, WMC, VISC, DTINV ,EPD,DAV,DAM1,DAM2,DT5,
     .   EPST1_1,EPST2_1,EPSM1_1,EPSM2_1,DMAX_1,SSP_1
C=======================================================================
C     Formulation flag

      IFLAG = MAT_PARAM%IPARAM(1)    ! IFLAG=0 : Tsai-Wu  , IFLAG=1 : Crasurv

      EPST1_1 =  MAT_PARAM%UPARAM(12)       !  PM(60)
      EPST2_1 =  MAT_PARAM%UPARAM(13)       !  PM(61)
      EPSM1_1 =  MAT_PARAM%UPARAM(14)       !  PM(62)
      EPSM2_1 =  MAT_PARAM%UPARAM(15)       !  PM(63)
      DMAX_1  =  MAT_PARAM%UPARAM(18)       !  PM(64)

      ASRATE  = PM(9)*DT1     
      SSP_1   = PM(27)
C-----------------------------------------------------------
      DO  I=1,NEL
        WPLAR(I) = ZERO
        NFIS1(I) = 0
        NFIS2(I) = 0
        NFIS3(I) = 0
        DMAXT(I) = ZERO
      ENDDO  
C-----------------------------------------------------------
C     STRAIN RATE FILTERING (EQUIVALENT EPSD)
C-----------------------------------------------------------
      ASRATE = ASRATE / (ASRATE + ONE)   
      DO I=1,NEL                                                                                                    
        DAV = -THIRD*(D1(I)+D2(I)+D3(I))                         
        EPD = HALF*((D1(I)+DAV )**2 + (D2(I)+DAV)**2 + (D3(I)+DAV)**2)           
     .      + FOURTH*(D4(I)**2 + D5(I)**2 + D6(I)**2)
        EPD = SQRT(THREE*EPD)/THREE_HALF                        
        EPSD(I) = ASRATE*EPD + (ONE - ASRATE)*EPSD(I)   
      ENDDO                                                        
C--------------------------------------------
C     STRESS TRANSFORMATION (GLOBAL -> FIBER)
C--------------------------------------------        
      DO I=1,NEL
        EP1(I) = D1(I)*DT1
        EP2(I) = D2(I)*DT1
        EP3(I) = D3(I)*DT1
        EP4(I) = D4(I)*DT1
        EP5(I) = D5(I)*DT1
        EP6(I) = D6(I)*DT1 
        S1(I) = SIG(I,1)
        S2(I) = SIG(I,2)
        S3(I) = SIG(I,3)
        S4(I) = SIG(I,4)
        S5(I) = SIG(I,5)
        S6(I) = SIG(I,6)
      ENDDO
!
      IF (ISORTH > 0 .AND. JCVT == 0) THEN
        CALL MREPLOC(
     1   GAMA,    R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     RX,      RY,
     4   RZ,      SX,      SY,      SZ,
     5   TX,      TY,      TZ,      NEL,  JSPH)
        DO I=1,NEL
         EP4(I) = HALF*EP4(I)
         EP5(I) = HALF*EP5(I)
         EP6(I) = HALF*EP6(I)
        ENDDO
        CALL MROTENS(1,NEL,EP1,EP2,EP3,EP4,EP5,EP6,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
        DO I=1,NEL
         EP4(I) = TWO*EP4(I)
         EP5(I) = TWO*EP5(I)
         EP6(I) = TWO*EP6(I)
        ENDDO
C stress
        CALL MROTENS(1,NEL,S1,S2,S3,S4,S5,S6,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33) 
       ENDIF  
C-----------------------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C-----------------------------------------------------------
      IF (IFLAG == 0) THEN    ! Tsai-Wu
        CALL MAT25_TSAIWU_S(MAT_PARAM,
     1                    NEL    ,NGL   ,OFF  ,FLAY ,
     2                    S1     ,S2   ,S3    ,S4   ,S5   ,S6   ,
     3                    EP1    ,EP2  ,EP3   ,EP4  , EP5 ,EP6  ,
     4                    EPST   ,NFIS1,NFIS2 ,NFIS3,
     5                    WPLAR  ,EPSD ,WPLA  ,SIGL ,ILAY ,IPG  ,
     6                    TSAIWU ,TT   ,IMCONV,MVSIZ,IOUT ,DMG  ,
     7                    L_DMG  ,OUTV)
      
      ELSE    ! Crasurv 
      
        CALL MAT25_CRASURV_S(mat_param   ,
     1             NEL  ,NGL, OFF  ,FLAY ,
     2             S1   ,S2   ,S3  ,S4   ,S5   ,S6    ,
     3             EP1  ,EP2  ,EP3 ,EP4  , EP5 ,EP6,
     4             EPST ,NFIS1,NFIS2,NFIS3,
     5             WPLAR,EPSD, WPLA, SIGL, ILAY,
     6             IPG  ,TSAIWU,TT,IMCONV  ,MVSIZ   ,IOUT,
     7             DMG  ,L_DMG ,OUTV)

      END IF
!-----------------------------------------------------------------------
      IF (ISORTH > 0 .AND. JCVT == 0) THEN
        CALL MROTENS(1,NEL,S1 ,S2 ,S3 ,S4 ,S5 ,S6 ,
     .            R11,R21,R31,R12,R22,R32,R13,R23,R33 )
      ENDIF
     
      DO I=1,NEL
        SIG(I,1) = S1(I)*OFF(I)
        SIG(I,2) = S2(I)*OFF(I)
        SIG(I,3) = S3(I)*OFF(I)
        SIG(I,4) = S4(I)*OFF(I)
        SIG(I,5) = S5(I)*OFF(I)
        SIG(I,6) = S6(I)*OFF(I)
      ENDDO
C-----------------------
C     TENSILE RUPTURE
C-----------------------
      DO  I=1,NEL
         EPST1(I)  =EPST1_1
         EPST2(I)  =EPST2_1
         EPSM1(I)  =EPSM1_1
         EPSM2(I)  =EPSM2_1
         DMAX(I)   =DMAX_1
         SSP(I) = SSP_1
      ENDDO
C.....STRAINS IN ORTHOTROPIC DIRECTIONS
C
C.....GATHER DIRECTION 1
      NINDX=0
      DO  I=1,NEL
        IF(EPST(I,1) >= EPST1(I)
     +    .AND. DMG(I,2) == ZERO .AND. OFF(I) == ONE)THEN
          NINDX=NINDX+1
          INDX(NINDX)=I
        ENDIF  
      ENDDO
C.....1.FIRST FAILURE DIRECTION 1
      IF(NINDX>0)THEN
        IDIR=1
          DO  J=1,NINDX
            I=INDX(J)
            DAM1=(EPST(I,1)-EPST1(I))/(EPSM1(I)-EPST1(I))
            DAM2= DAM1*EPSM1(I)/EPST(I,1)
            DMG(I,2)= MIN(DAM2,DMAX(I))
            IF(DMG(I,2)==DMAX(I).AND.IMCONV==1)THEN
#include "lockon.inc"
               WRITE(IOUT, '(A,I1,A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' FAILURE-',IDIR,', ELEMENT #',NGL(I),
     +        ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +        ', TIME=',TT      
#include "lockoff.inc"
            ENDIF      
         ENDDO
       ENDIF      
C.....GATHER DIRECTION 2
       NINDX=0
      DO  I=1,NEL
        IF(EPST(I,2) >= EPST2(I)
     +         .AND. DMG(I,3) == ZERO .AND. OFF(I) == ONE) THEN
          NINDX=NINDX+1
          INDX(NINDX)=I
        ENDIF  
      ENDDO
C.....1.FIRST FAILURE DIRECTION 2
      IF(NINDX > 0)THEN
         IDIR=2
         DO  J=1,NINDX
          I=INDX(J)
          DAM1=(EPST(I,2)-EPST2(I))/(EPSM2(I)-EPST2(I))
          DAM2= DAM1*EPSM2(I)/EPST(I,2)
          DMG(I,3)= MIN(DAM2,DMAX(I))
          IF(DMG(I,3) == DMAX(I) .AND. IMCONV == 1)THEN
#include "lockon.inc"
              WRITE(IOUT, '(A,I1,A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' FAILURE-',IDIR,', ELEMENT #',NGL(I),
     +        ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +        ', TIME=',TT      
#include "lockoff.inc"
          ENDIF      
        ENDDO
      ENDIF      
C---------------------------
C     GLOBAL FAILURE INDEX
C---------------------------
      IF (IFLAG == 0) THEN    ! Tsai-Wu
        DO I=1,NEL
          DMG(I,1) = MAX(DMG(I,2),DMG(I,3),DMG(I,4))
        ENDDO
      ELSE                    ! Crasurv 
        DO I=1,NEL
          DMG(I,1) = MAX(DMG(I,2),DMG(I,3),DMG(I,4))
          IF (ABS(DMG(I,5)) >= ONE) THEN 
            DMG(I,1) = MAX(ABS(DMG(I,5))-ONE,DMG(I,1)) 
          ENDIF
          IF (ABS(DMG(I,6)) >= ONE) THEN
            DMG(I,1) = MAX(ABS(DMG(I,6))-ONE,DMG(I,1)) 
          ENDIF
          IF (DMG(I,7) >= ONE) THEN
            DMG(I,1) = MAX(DMG(I,7)-ONE,DMG(I,1)) 
          ENDIF
        ENDDO
      ENDIF       
C----------------------------
C     TEST DE RUPTURE 
C----------------------------
      DO  I=1,NEL
        IF(OFF(I) < EM01) OFF(I)=ZERO
        IF(OFF(I) < ONE  ) OFF(I)=OFF(I)*FOUR_OVER_5
      ENDDO

      NINDX=0
       DO  I=1,NEL
           IF(OFF(I)==ONE) THEN
             IOFF = mat_param%iparam(2)
             IF(IOFF < 0) IOFF=-(IOFF+1)
             JOFF=0
c             IF(IOFF == 0 .AND. WPLAR(I) >= ONE) JOFF=1
c             IF(IOFF == 1 .AND. NINT(WPLAR(I)) >= NPT) JOFF=1
c             IF(IOFF == 2 .AND. NFIS1(I) == NPT) JOFF=1
c             IF(IOFF == 3 .AND. NFIS2(I) == NPT) JOFF=1
c             IF(IOFF == 4 .AND. NFIS1(I) == NPT 
c     .                     .AND. NFIS2(I) == NPT) JOFF=1
c             IF(IOFF == 5 .AND. NFIS1(I) == NPT) JOFF=1
c             IF(IOFF == 5 .AND. NFIS2(I) == NPT) JOFF=1
c             IF(IOFF == 6 .AND. NFIS3(I) == NPT) JOFF=1
             IF(JOFF == ONE) THEN
                 OFF(I)=OFF(I)*FOUR_OVER_5
                 II=I+NFT
                 NINDX=NINDX+1
                 INDX(NINDX)=I
                 IF(IMCONV==1)THEN
#include "lockon.inc"
                    WRITE(IOUT,1000)  NGL(I)
                    WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
                 ENDIF
             ENDIF
          ENDIF
       ENDDO
      DT5=HALF*DT1
      DO I=1,NEL
        EINT(I)=EINT(I) + DT5*VNEW(I)*
     .           ( D1(I)*(S01(I)+SIG(I,1))
     .           + D2(I)*(S02(I)+SIG(I,2))
     .           + D3(I)*(S03(I)+SIG(I,3))
     .           + D4(I)*(S04(I)+SIG(I,4))
     .           + D5(I)*(S05(I)+SIG(I,5))
     .           + D6(I)*(S06(I)+SIG(I,6)))
        EINT(I)=EINT(I)/VOL(I)
      ENDDO
c----------------------------------------------------------------------       
 1000 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT :',I10,' AT TIME :',G11.4)
c-----------
      RETURN
      END
