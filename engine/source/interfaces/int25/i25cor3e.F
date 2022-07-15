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
Chd|  I25COR3E                      source/interfaces/int25/i25cor3e.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COR3E(
     1      JLT    ,LEDGE  ,IRECT  ,X      ,V      ,
     2      CAND_S ,CAND_M ,STFE   ,MS     ,EX     ,
     3      EY     ,EZ     ,FX     ,FY     ,FZ     ,
     4      STIF   ,XXS1   ,XXS2   ,XYS1   ,XYS2   ,
     5      XZS1   ,XZS2   ,XXM1   ,XXM2   ,XYM1   ,
     6      XYM2   ,XZM1   ,XZM2   ,VXS1   ,VXS2   ,
     7      VYS1   ,VYS2   ,VZS1   ,VZS2   ,VXM1   ,
     8      VXM2   ,VYM1   ,VYM2   ,VZM1   ,VZM2   ,
     9      MS1    ,MS2    ,MM1    ,MM2    ,N1     ,
     A      N2     ,M1     ,M2     ,NEDGE   ,NIN    ,
     C      STFAC  ,NODNX_SMS,NSMS ,GAPE   ,GAPVE  ,
     D      IEDGE  ,ADMSR  ,LBOUND ,EDG_BISECTOR   ,
     E      VTX_BISECTOR,IGAP0,
     F      IAM    ,JAM    ,IBM    ,JBM    ,IAS    ,
     G      JAS    ,IBS    ,JBS    ,ITAB   , EDGE_ID,
     H      INTFRIC,IPARTFRIC_E,IPARTFRICSI,IPARTFRICMI,
     I      IGAP   ,GAP_E_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
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
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "assert.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEDGE(NLEDGE,*), IRECT(4,*), CAND_M(*), CAND_S(*), ADMSR(4,*),
     .        LBOUND(*), JLT, NRTS, NIN, IEDGE, IGAP0,INTFRIC,IGAP,
     .        N1(MVSIZ), N2(MVSIZ), 
     .        M1(MVSIZ), M2(MVSIZ), 
     .        NODNX_SMS(*), NSMS(MVSIZ), 
     .        ITAB(*),              
     .        IAM(MVSIZ),JAM(MVSIZ),IBM(MVSIZ),JBM(MVSIZ),     
     .        IAS(MVSIZ),JAS(MVSIZ),IBS(MVSIZ),JBS(MVSIZ),
     .        IPARTFRIC_E(*),IPARTFRICSI(MVSIZ),IPARTFRICMI(MVSIZ)
      INTEGER :: EDGE_ID(2,MVSIZ)
      INTEGER NEDGE
C     REAL
      my_real
     .        X(3,*), STFE(*), MS(*), V(3,*), GAPE(*), 
     .        XXS1(*), XXS2(*), XYS1(*), XYS2(*),
     .        XZS1(*), XZS2(*), XXM1(*), XXM2(*),
     .        XYM1(*), XYM2(*), XZM1(*), XZM2(*),
     .        VXS1(*), VXS2(*), VYS1(*), VYS2(*),
     .        VZS1(*), VZS2(*), VXM1(*), VXM2(*),
     .        VYM1(*), VYM2(*), VZM1(*), VZM2(*),
     .        MS1(*),  MS2(*),  MM1(*),  MM2(*),
     .        STIF(*), STFAC, STS, STM, GAPVE(*),
     .        EX(*), EY(*), EZ(*), FX(*), FY(*), FZ(*),
     .        GAP_E_L(NEDGE)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NN, J, JRM, K, KRM, I1, J1, I2, J2, 
     .        EM, IE, ES, JE, SOL_EDGE, SH_EDGE, 
     .        TYPEDGS(MVSIZ), IM(MVSIZ), IS(MVSIZ)
      INTEGER ::  NOD1S(MVSIZ),NOD2S(MVSIZ)
      INTEGER ::  NOD1M(MVSIZ),NOD2M(MVSIZ)

      INTEGER :: ISR ! orientation SECONDARY edge
      my_real
     .   AAA, DX, DY, DZ, DD, NNI, NI2, INVCOS, GAPE_M(MVSIZ), GAPE_S(MVSIZ)
C-----------------------------------------------
      DO I=1,JLT

        EM    =CAND_M(I)
C debug ! global id of MAIN edge
        EDGE_ID(1,I) = LEDGE(8,EM)

        IAM(I) = ABS(LEDGE(1,EM))
        JAM(I)=LEDGE(2,EM)
        IBM(I)=LEDGE(3,EM)
        JBM(I)=LEDGE(4,EM)

        M1(I)=LEDGE(5,EM)
        M2(I)=LEDGE(6,EM)
        IM(I) = LEDGE(10,EM)
        NOD1M(I) = LEDGE(11,EM)
        NOD2M(I) = LEDGE(12,EM)

CC        IF(IAM(I) > 0) THEN
CC          IF(IRECT(JAM(I),IAM(I))==M1(I).AND.IRECT(MOD(JAM(I),4)+1,IAM(I))==M2(I))THEN
CC            IM(I)= 1
CC          ELSEIF(IRECT(JAM(I),IAM(I))==M2(I).AND.IRECT(MOD(JAM(I),4)+1,IAM(I))==M1(I))THEN
CC            IM(I)=-1
CC          ELSE
CC            print *,'i25cor3e - internal problem',EM,M1(I),M2(I),
CC     .                          IRECT(JAM(I),IAM(I)),IRECT(MOD(JAM(I),4)+1,IAM(I))
CC          END IF
CC        ELSE
CCC Faire le cas frontiere
CC          IM(I) = 1
CC        ENDIF
CC         ASSERT(IM(I) == LEDGE(10,EM))
c       IF ( IM(I) .NE. LEDGE(10,EM) ) THEN
c         WRITE(6,'(4I10)') I,LEDGE(10,EM),LEDGE(11,EM),IM(I)
c       ENDIF
        STM=STFE(EM)

        XXM1(I) = X(1,M1(I))
        XYM1(I) = X(2,M1(I))
        XZM1(I) = X(3,M1(I))
        XXM2(I) = X(1,M2(I))
        XYM2(I) = X(2,M2(I))
        XZM2(I) = X(3,M2(I))
        VXM1(I) = V(1,M1(I))
        VYM1(I) = V(2,M1(I))
        VZM1(I) = V(3,M1(I))
        VXM2(I) = V(1,M2(I))
        VYM2(I) = V(2,M2(I))
        VZM2(I) = V(3,M2(I))
        MM1(I) = MS(M1(I))
        MM2(I) = MS(M2(I))
C
        IF(CAND_S(I).LE.NEDGE) THEN

          ES    =CAND_S(I)
          EDGE_ID(2,I) = LEDGE(8,ES)
          IAS(I)=ABS( LEDGE(1,ES) )
          JAS(I)=LEDGE(2,ES)
          IBS(I)=LEDGE(3,ES)
          JBS(I)=LEDGE(4,ES)
          N1(I)=LEDGE(5,ES)
          N2(I)=LEDGE(6,ES)
          NOD1S(I) = LEDGE(11,ES)
          NOD2S(I) = LEDGE(12,ES)

          IS(I) = LEDGE(10,ES)
CC          IF(IAS(I) > 0) THEN
CC            IF(IRECT(JAS(I),IAS(I))==N1(I).AND.IRECT(MOD(JAS(I),4)+1,IAS(I))==N2(I))THEN
CC              IS(I)= 1
CC            ELSEIF(IRECT(JAS(I),IAS(I))==N2(I).AND.IRECT(MOD(JAS(I),4)+1,IAS(I))==N1(I))THEN
CC              IS(I)=-1
CC            ELSE
CC              print *,'i25cor3e - internal problem',ES,N1(I),N2(I),
CC     .                            IRECT(JAS(I),IAS(I)),IRECT(MOD(JAS(I),4)+1,IAS(I))
CC            END IF
CC          ELSE
CCC faire le SPMD
CC            IS(I) = 1
CC          ENDIF
CC          ASSERT(IS(I) == LEDGE(10,ES))

          STS=STFE(ES)
          STIF(I)=STS*STM / MAX(EM20,STS+STM)
          XXS1(I) = X(1,N1(I))
          XYS1(I) = X(2,N1(I))
          XZS1(I) = X(3,N1(I))
          XXS2(I) = X(1,N2(I))
          XYS2(I) = X(2,N2(I))
          XZS2(I) = X(3,N2(I))
          VXS1(I) = V(1,N1(I))
          VYS1(I) = V(2,N1(I))
          VZS1(I) = V(3,N1(I))
          VXS2(I) = V(1,N2(I))
          VYS2(I) = V(2,N2(I))
          VZS2(I) = V(3,N2(I))
          MS1(I) = MS(N1(I))
          MS2(I) = MS(N2(I))

C
          TYPEDGS(I)=LEDGE(7,ES)
C
        ELSE
          ! IBUF_EDGE(P)%p(E_LEFT_SEG     + L) = LEDGE(1,I)
          ! IBUF_EDGE(P)%p(E_LEFT_ID      + L) = LEDGE(2,I)
          ! IBUF_EDGE(P)%p(E_RIGHT_SEG    + L) = LEDGE(3,I)
          ! IBUF_EDGE(P)%p(E_RIGHT_ID     + L) = LEDGE(4,I)
          ! IBUF_EDGE(P)%p(E_NODE1_ID     + L) = LEDGE(5,I)
          ! IBUF_EDGE(P)%p(E_NODE2_ID     + L) = LEDGE(6,I)
          ! IBUF_EDGE(P)%p(E_TYPE         + L) = LEDGE(7,I) 

          NN = CAND_S(I) - NEDGE            
          IS(i) = LEDGE_FIE(NIN)%P(E_IM,NN) 
C         IF(IS(i)  == 1) THEN
          N1(I)=2*(NN-1)+1
          N2(I)=2*NN
c         ELSE
c           N2(I)=2*(NN-1)+1
c           N1(I)=2*NN
c         ENDIF

          EDGE_ID(2,I) = LEDGE_FIE(NIN)%P(E_GLOBAL_ID,NN)

          IAS(I)=ABS( LEDGE_FIE(NIN)%P(E_LEFT_SEG  ,NN) )
          JAS(I)=LEDGE_FIE(NIN)%P(E_LEFT_ID   ,NN)
          IBS(I)=LEDGE_FIE(NIN)%P(E_RIGHT_SEG ,NN)
          JBS(I)=LEDGE_FIE(NIN)%P(E_RIGHT_ID  ,NN)
          TYPEDGS(I)=LEDGE_FIE(NIN)%P(E_TYPE,NN)
          STS=STIFIE(NIN)%P(NN)
          STIF(I)=STS*STM / MAX(EM20,STS+STM)


c          STS=STFE(ES)
c          STIF(I)=STS*STM / MAX(EM20,STS+STM)
c          STIF(I)=ABS(STIFIE(NIN)%P(NN))*STFM
c                / MAX(EM20,ABS(STIFIE(NIN)%P(NN))+STM)
c
c          TYPEDGS(I)=LEDGE(7,CAND_S(I))
c
          XXS1(I) = XFIE(NIN)%P(1,N1(I))
          XYS1(I) = XFIE(NIN)%P(2,N1(I))
          XZS1(I) = XFIE(NIN)%P(3,N1(I))
          XXS2(I) = XFIE(NIN)%P(1,N2(I))
          XYS2(I) = XFIE(NIN)%P(2,N2(I))
          XZS2(I) = XFIE(NIN)%P(3,N2(I))
          VXS1(I) = VFIE(NIN)%P(1,N1(I))
          VYS1(I) = VFIE(NIN)%P(2,N1(I))
          VZS1(I) = VFIE(NIN)%P(3,N1(I))
          VXS2(I) = VFIE(NIN)%P(1,N2(I))
          VYS2(I) = VFIE(NIN)%P(2,N2(I))
          VZS2(I) = VFIE(NIN)%P(3,N2(I))
          MS1(I) = MSFIE(NIN)%P(N1(I))
          MS2(I) = MSFIE(NIN)%P(N2(I))
        END IF
c       DEBUG_E2E(EDGE_ID(1,I) ==  D_EM.AND. EDGE_ID(2,I) == D_ES,XXS1(i))
c       DEBUG_E2E(EDGE_ID(1,I) ==  D_EM.AND. EDGE_ID(2,I) == D_ES,XXS2(i))
      END DO

      SOL_EDGE=IEDGE/10 ! solids
      SH_EDGE =IEDGE-10*SOL_EDGE ! shells

      EX(1:JLT)=ZERO
      EY(1:JLT)=ZERO
      EZ(1:JLT)=ZERO
      FX(1:JLT)=ZERO
      FY(1:JLT)=ZERO
      FZ(1:JLT)=ZERO
      IF(SH_EDGE/=0)THEN
        DO I=1,JLT

CC BOUNDARY_EDGE :
C          IF(IAM(I) < 0) CYCLE
C
          EX(I) = EDG_BISECTOR(1,JAM(I),IAM(I))
          EY(I) = EDG_BISECTOR(2,JAM(I),IAM(I))
          EZ(I) = EDG_BISECTOR(3,JAM(I),IAM(I))
C
          IF(IABS(TYPEDGS(I))/=1)THEN
            IF(CAND_S(I).LE.NEDGE) THEN
              FX(I) = EDG_BISECTOR(1,JAS(I),IAS(I))
              FY(I) = EDG_BISECTOR(2,JAS(I),IAS(I))
              FZ(I) = EDG_BISECTOR(3,JAS(I),IAS(I))
            ELSE
              FX(I) = EDG_BISECTOR_FIE(NIN)%P(1,1,CAND_S(I)-NEDGE)
              FY(I) = EDG_BISECTOR_FIE(NIN)%P(2,1,CAND_S(I)-NEDGE)
              FZ(I) = EDG_BISECTOR_FIE(NIN)%P(3,1,CAND_S(I)-NEDGE)
            END IF
          END IF
        END DO
      END IF

      DO I=1,JLT
        GAPE_M(I)=GAPE(CAND_M(I))
        IF(CAND_S(I).LE.NEDGE) THEN
          GAPE_S(I)=GAPE(CAND_S(I))
        ELSE 
          GAPE_S(I)= GAPFIE(NIN)%P(CAND_S(I) - NEDGE)
        END IF
        DEBUG_E2E(EDGE_ID(1,I) ==  D_EM .AND. EDGE_ID(2,I) == D_ES,GAPE_S(I))
        GAPVE(I)=GAPE_M(I)+GAPE_S(I)
      END DO
      IF(IGAP == 3) THEN
        DO I=1,JLT
           GAPE_M(I)=MIN(GAPE_M(I),GAP_E_L(CAND_M(I)))
          IF(CAND_S(I).LE.NEDGE) THEN
              GAPE_S(I)=MIN(GAPE_S(I),GAP_E_L(CAND_S(I)))
             GAPVE(I)=MIN(GAP_E_L(CAND_M(I))+GAP_E_L(CAND_S(I)),GAPVE(I)) 
          ELSE
              GAPE_S(I)=MIN(GAPE_S(I),GAPE_L_FIE(NIN)%P(CAND_S(I) - NEDGE))
             GAPVE(I)=MIN(GAP_E_L(CAND_M(I))+GAPE_L_FIE(NIN)%P(CAND_S(I) - NEDGE),GAPVE(I)) 
          ENDIF
        ENDDO
      ENDIF
C
C     Always shift MAIN edge toward inner side
      DO I=1,JLT

        IF(IBM(I)/=0)CYCLE

        IE=IAM(I)
        JE=JAM(I)
 
        I1 = NOD1M(I)
        I2 = NOD2M(I)
        EX(I) = EDG_BISECTOR(1,JE,IE)
        EY(I) = EDG_BISECTOR(2,JE,IE)
        EZ(I) = EDG_BISECTOR(3,JE,IE)
C
C       DX, DY, DZ Direction for shifting
        DX = VTX_BISECTOR(1,1,I1)+VTX_BISECTOR(1,2,I1)
        DY = VTX_BISECTOR(2,1,I1)+VTX_BISECTOR(2,2,I1)
        DZ = VTX_BISECTOR(3,1,I1)+VTX_BISECTOR(3,2,I1)
C
        NNI = EX(I)*DX + EY(I)*DY + EZ(I)*DZ
        NI2 = DX*DX + DY*DY + DZ*DZ

        DEBUG_E2E( EDGE_ID(1,I) ==  D_EM.AND. EDGE_ID(2,I) == D_ES ,EX(I))
        DEBUG_E2E( EDGE_ID(1,I) ==  D_EM.AND. EDGE_ID(2,I) == D_ES ,EY(I))
        DEBUG_E2E( EDGE_ID(1,I) ==  D_EM.AND. EDGE_ID(2,I) == D_ES ,EZ(I))
        DEBUG_E2E( EDGE_ID(1,I) ==  D_EM.AND. EDGE_ID(2,I) == D_ES ,DX)
        DEBUG_E2E( EDGE_ID(1,I) ==  D_EM.AND. EDGE_ID(2,I) == D_ES ,DY)
        DEBUG_E2E( EDGE_ID(1,I) ==  D_EM.AND. EDGE_ID(2,I) == D_ES ,DZ)

        IF(NNI < ZERO)THEN
          DX=DX-TWO*NNI*EX(I)
          DY=DY-TWO*NNI*EY(I)
          DZ=DZ-TWO*NNI*EZ(I)
          NNI=-NNI
        END IF

        IF(TWO*NNI*NNI < NI2)THEN
c         scharp angle bound nodal normal to 45 from edge normal
          AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
          DX = DX + AAA*EX(I)
          DY = DY + AAA*EY(I)
          DZ = DZ + AAA*EZ(I)
        ENDIF
        DD=ONE/MAX(EM20,SQRT(DX*DX+DY*DY+DZ*DZ))
        DX = DX*DD
        DY = DY*DD
        DZ = DZ*DD
        INVCOS  = ONE / MAX(EM20,EX(I)*DX  + EY(I)*DY  + EZ(I)*DZ)
        DX = DX*INVCOS
        DY = DY*INVCOS
        DZ = DZ*INVCOS
C
        XXM1(I) = XXM1(I)-GAPE_M(I)*DX
        XYM1(I) = XYM1(I)-GAPE_M(I)*DY
        XZM1(I) = XZM1(I)-GAPE_M(I)*DZ
C
C       DX, DY, DZ Direction for shifting
        DX = VTX_BISECTOR(1,1,I2)+VTX_BISECTOR(1,2,I2)
        DY = VTX_BISECTOR(2,1,I2)+VTX_BISECTOR(2,2,I2)
        DZ = VTX_BISECTOR(3,1,I2)+VTX_BISECTOR(3,2,I2)
C
        NNI = EX(I)*DX  + EY(I)*DY  + EZ(I)*DZ
        NI2 = DX*DX + DY*DY + DZ*DZ

        IF(NNI < ZERO)THEN
          DX=DX-TWO*NNI*EX(I)
          DY=DY-TWO*NNI*EY(I)
          DZ=DZ-TWO*NNI*EZ(I)
          NNI=-NNI
        END IF

        IF(TWO*NNI*NNI < NI2)THEN
c         scharp angle bound nodal normal to 45 from edge normal
          AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
          DX = DX + AAA*EX(I)
          DY = DY + AAA*EY(I)
          DZ = DZ + AAA*EZ(I)
        ENDIF
        DD=ONE/MAX(EM20,SQRT(DX*DX+DY*DY+DZ*DZ))
        DX = DX*DD
        DY = DY*DD
        DZ = DZ*DD
        INVCOS  = ONE / MAX(EM20,EX(I)*DX  + EY(I)*DY  + EZ(I)*DZ)
        DX = DX*INVCOS
        DY = DY*INVCOS
        DZ = DZ*INVCOS
C
        XXM2(I) = XXM2(I)-GAPE_M(I)*DX
        XYM2(I) = XYM2(I)-GAPE_M(I)*DY
        XZM2(I) = XZM2(I)-GAPE_M(I)*DZ
C
      END DO

      IF(IGAP0/=0)THEN
C
C       Shift SECONDARY edge toward inner side
        DO I=1,JLT
          DEBUG_E2E(EDGE_ID(1,I) ==  D_EM .AND. EDGE_ID(2,I) == D_ES,IBS(I))

          IF(IBS(I)/=0)CYCLE

          IF(CAND_S(I).LE.NEDGE) THEN
          IE=IAS(I)
          JE=JAS(I)

          I1 = NOD1S(I)
          I2 = NOD2S(I)

          FX(I) = EDG_BISECTOR(1,JE,IE)
          FY(I) = EDG_BISECTOR(2,JE,IE)
          FZ(I) = EDG_BISECTOR(3,JE,IE)
C
C         DX, DY, DZ Direction for shifting
          DX = VTX_BISECTOR(1,1,I1)+VTX_BISECTOR(1,2,I1)
          DY = VTX_BISECTOR(2,1,I1)+VTX_BISECTOR(2,2,I1)
          DZ = VTX_BISECTOR(3,1,I1)+VTX_BISECTOR(3,2,I1)
            ASSERT(FX(I) == FX(I))
            ASSERT(FY(I) == FY(I))
            ASSERT(FZ(I) == FZ(I))
          ELSE ! edge SECONDARY remote
            FX(I) = EDG_BISECTOR_FIE(NIN)%P(1,1,CAND_S(I)-NEDGE)
            FY(I) = EDG_BISECTOR_FIE(NIN)%P(2,1,CAND_S(I)-NEDGE)
            FZ(I) = EDG_BISECTOR_FIE(NIN)%P(3,1,CAND_S(I)-NEDGE)

            ASSERT(FX(I) == FX(I))
            ASSERT(FY(I) == FY(I))
            ASSERT(FZ(I) == FZ(I))
C           DX, DY, DZ Direction for shifting
            DX = VTX_BISECTOR_FIE(NIN)%P(1,1,CAND_S(I)-NEDGE)+VTX_BISECTOR_FIE(NIN)%P(1,2,CAND_S(I)-NEDGE)
            DY = VTX_BISECTOR_FIE(NIN)%P(2,1,CAND_S(I)-NEDGE)+VTX_BISECTOR_FIE(NIN)%P(2,2,CAND_S(I)-NEDGE)
            DZ = VTX_BISECTOR_FIE(NIN)%P(3,1,CAND_S(I)-NEDGE)+VTX_BISECTOR_FIE(NIN)%P(3,2,CAND_S(I)-NEDGE)
            ASSERT(DX == DX)                         
            ASSERT(DY == DY)
            ASSERT(DZ == DZ)
          ENDIF

          NNI = FX(I)*DX + FY(I)*DY + FZ(I)*DZ
          NI2 = DX*DX + DY*DY + DZ*DZ

          IF(NNI < ZERO)THEN
            DX=DX-TWO*NNI*FX(I)
            DY=DY-TWO*NNI*FY(I)
            DZ=DZ-TWO*NNI*FZ(I)
            NNI=-NNI
          END IF

          IF(TWO*NNI*NNI < NI2)THEN
c           scharp angle bound nodal normal to 45 from edge normal
            AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
            DX = DX + AAA*FX(I)
            DY = DY + AAA*FY(I)
            DZ = DZ + AAA*FZ(I)
          ENDIF
          DD=ONE/MAX(EM20,SQRT(DX*DX+DY*DY+DZ*DZ))
          DX = DX*DD
          DY = DY*DD
          DZ = DZ*DD
          INVCOS  = ONE / MAX(EM20,FX(I)*DX  + FY(I)*DY  + FZ(I)*DZ)
          DX = DX*INVCOS
          DY = DY*INVCOS
          DZ = DZ*INVCOS
C
          XXS1(I) = XXS1(I)-GAPE_S(I)*DX
          XYS1(I) = XYS1(I)-GAPE_S(I)*DY
          XZS1(I) = XZS1(I)-GAPE_S(I)*DZ
C
          IF(CAND_S(I).LE.NEDGE) THEN
C         DX, DY, DZ Direction for shifting
          DX = VTX_BISECTOR(1,1,I2)+VTX_BISECTOR(1,2,I2)
          DY = VTX_BISECTOR(2,1,I2)+VTX_BISECTOR(2,2,I2)
          DZ = VTX_BISECTOR(3,1,I2)+VTX_BISECTOR(3,2,I2)
            ASSERT(DX == DX)
            ASSERT(DY == DY)
            ASSERT(DZ == DZ)
          ELSE
            DX = VTX_BISECTOR_FIE(NIN)%P(1,3,CAND_S(I)-NEDGE)+VTX_BISECTOR_FIE(NIN)%P(1,4,CAND_S(I)-NEDGE)
            DY = VTX_BISECTOR_FIE(NIN)%P(2,3,CAND_S(I)-NEDGE)+VTX_BISECTOR_FIE(NIN)%P(2,4,CAND_S(I)-NEDGE)
            DZ = VTX_BISECTOR_FIE(NIN)%P(3,3,CAND_S(I)-NEDGE)+VTX_BISECTOR_FIE(NIN)%P(3,4,CAND_S(I)-NEDGE)
            ASSERT(DX == DX)
            ASSERT(DY == DY)
            ASSERT(DZ == DZ)
          ENDIF
C
          NNI = FX(I)*DX  + FY(I)*DY  + FZ(I)*DZ
          NI2 = DX*DX + DY*DY + DZ*DZ

          IF(NNI < ZERO)THEN
            DX=DX-TWO*NNI*FX(I)
            DY=DY-TWO*NNI*FY(I)
            DZ=DZ-TWO*NNI*FZ(I)
            NNI=-NNI
          END IF

          IF(TWO*NNI*NNI < NI2)THEN
c           scharp angle bound nodal normal to 45 from edge normal
            AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
            DX = DX + AAA*FX(I)
            DY = DY + AAA*FY(I)
            DZ = DZ + AAA*FZ(I)
          ENDIF
          DD=ONE/MAX(EM20,SQRT(DX*DX+DY*DY+DZ*DZ))
          DX = DX*DD
          DY = DY*DD
          DZ = DZ*DD
          INVCOS  = ONE / MAX(EM20,FX(I)*DX  + FY(I)*DY  + FZ(I)*DZ)
          DX = DX*INVCOS
          DY = DY*INVCOS
          DZ = DZ*INVCOS
C
          XXS2(I) = XXS2(I)-GAPE_S(I)*DX
          XYS2(I) = XYS2(I)-GAPE_S(I)*DY
          XZS2(I) = XZS2(I)-GAPE_S(I)*DZ
C
        END DO
      END IF
C
      IF(IDTMINS==2)THEN
       DO I=1,JLT
        IF(CAND_S(I).LE.NEDGE)THEN
          NSMS(I)=NODNX_SMS(N1(I))+NODNX_SMS(N2(I))+
     .            NODNX_SMS(M1(I))+NODNX_SMS(M2(I))
        ELSE
          NSMS(I)=NODNXFIE(NIN)%P(N1(I))+NODNXFIE(NIN)%P(N2(I))+
     .            NODNX_SMS(M1(I))+NODNX_SMS(M2(I))
        END IF
       ENDDO
       IF(IDTMINS_INT/=0)THEN
         DO I=1,JLT
          IF(NSMS(I).EQ.0)NSMS(I)=-1
         ENDDO
       END IF
      ELSEIF(IDTMINS_INT/=0)THEN
        DO I=1,JLT
         NSMS(I)=-1
        ENDDO
      ENDIF
C
C----Friction model : secnd part IDs---------
      IF(INTFRIC > 0) THEN
         DO I=1,JLT

           IF(CAND_S(I).LE.NEDGE)THEN
             IPARTFRICSI(I) = IPARTFRIC_E(CAND_S(I))
           ELSE
             NN = CAND_S(I) - NEDGE            
             IPARTFRICSI(I)= IPARTFRIC_FIE(NIN)%P(NN)
           ENDIF

C
           IPARTFRICMI(I) = IPARTFRIC_E(CAND_M(I))
         ENDDO
       ENDIF
      RETURN
      END
