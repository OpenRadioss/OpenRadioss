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
Chd|  I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        I25ASSE05_E2S                 source/interfaces/int25/i25ass_e2s.F
Chd|        I25ASSE0_E2S                  source/interfaces/int25/i25ass_e2s.F
Chd|        I25ASSE25_E2S                 source/interfaces/int25/i25ass_e2s.F
Chd|        I25ASSE2_E2S                  source/interfaces/int25/i25ass_e2s.F
Chd|        I25SMS_E2S                    source/interfaces/int25/i25sms_e2s.F
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25FOR3_E2S(
     1                  JLT    ,A      ,V      ,IBC     ,ICODT  ,
     2                  FSAV   ,GAP    ,FRIC   ,MS      ,VISC   ,
     3                  VISCF  ,NOINT  ,ITAB   ,CS_LOC  ,CM_LOC ,
     4                  STIGLO ,STIFN  ,STIF   ,FSKYI   ,ISKY   ,
     5                  FCONT  ,DT2T   ,NRTM   ,MSEGTYP ,HS1    ,
     6                  HS2    ,HM1    ,HM2    ,N1      ,N2     ,
     7                  M1     ,M2     ,IVIS2  ,NELTST  ,ITYPTST,
     8                  NX     ,NY     ,NZ     ,GAPVE   ,INACTI ,
     9                  INDEX  ,CAND_P ,NISKYFIE,NEWFRONT,ISECIN ,
     A                  NSTRF  ,SECFCUM,VISCN  ,NEDGE   ,MS1    ,
     B                  MS2    ,MM1    ,MM2    ,VXS1    ,VYS1   ,
     C                  VZS1   ,VXS2   ,VYS2   ,VZS2    ,VXM1   ,
     D                  VYM1   ,VZM1   ,VXM2   ,VYM2    ,VZM2   ,
     E                  NIN    ,NISUB   ,LISUB ,ADDSUBE ,ADDSUBM,
     F                  LISUBE ,LISUBM ,INFLG_SUBE,INFLG_SUBM,FSAVSUB,
     G                  MSKYI_SMS,ISKYI_SMS,NSMS ,JTASK     ,ISENSINT,
     H                  FSAVPARIT,NFT   ,H3D_DATA,INDX1     ,INDX2   ,
     I                  ILEV   ,MBINFLG, EDGE_ID, NEDGE_REM ,FRICC   ,
     J                  IFQ    ,CAND_FX, CAND_FY, CAND_FZ   ,IFPEN   ,
     K                  TAGNCONT ,KLOADPINTER,LOADPINTER ,LOADP_HYD_INTER,
     L                  TYPSUB   ,STARTT,NINLOADP  ,DGAPLOADINT,S_LOADPINTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "assert.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "couple_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: EDGE_ID(2,4*MVSIZ),NEDGE_REM
      INTEGER NELTST,ITYPTST,JLT,IBC,IVIS2,INACTI,NEDGE,NISKYFIE,NIN,NRTM,ILEV,
     .        IFQ
      INTEGER ICODT(*), ITAB(*), ISKY(*),
     .        NOINT,NEWFRONT,ISECIN, NSTRF(*), ISKYI_SMS(*), MSEGTYP(*),
     .        NISUB, LISUB(*), ADDSUBE(*), ADDSUBM(*), LISUBE(*), LISUBM(*),
     .        INFLG_SUBE(*), INFLG_SUBM(*), MBINFLG(*), IFPEN(*),TYPSUB(*)
      INTEGER N1(*), N2(*), M1(*), M2(*), NSMS(*),
     .        CS_LOC(4*MVSIZ), CM_LOC(4*MVSIZ), JTASK,
     .        ISENSINT(*),NFT,INDEX(*), INDX1(4*MVSIZ), INDX2(4*MVSIZ),
     .        TAGNCONT(NLOADP_HYD_INTER,NUMNOD)
      INTEGER  , INTENT(IN) :: NINLOADP,S_LOADPINTER
      INTEGER  , INTENT(IN) :: KLOADPINTER(NINTER+1),LOADPINTER(S_LOADPINTER),
     .        LOADP_HYD_INTER(NLOADP_HYD)
      my_real
     .   STIGLO,
     .   A(3,*), MS(*), V(3,*), FSAV(*),FCONT(3,*),
     .   STIFN(*),FSKYI(LSKYI,NFSKYI),FSAVSUB(NTHVKI,*),
     .   MSKYI_SMS(*), GAPVE(*), CAND_P(4,*),
     .   GAP,FRIC,VISC,VISCF,VIS,DT2T,STARTT
      my_real
     .   HS1(*), HS2(*), HM1(*), HM2(*),
     .   NX(*), NY(*), NZ(*), STIF(*),
     .   SECFCUM(7,NUMNOD,NSECT), VISCN(*),
     .   MS1(*),MS2(*),MM1(*),MM2(*),
     .   VXS1(*),VYS1(*),VZS1(*),VXS2(*),VYS2(*),
     .   VZS2(*),VXM1(*),VYM1(*),VZM1(*),VXM2(*),
     .   VYM2(*),VZM2(*),FSAVPARIT(NISUB+1,11,*),
     .   FRICC(*),CAND_FX(4,*),CAND_FY(4,*),CAND_FZ(4,*)
      my_real  , INTENT(IN) :: DGAPLOADINT(S_LOADPINTER)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J , K0,NBINTER,K1S,K, NI, IL, IE, IG, PP, PPL
      INTEGER NISKYL,NISKYL1,ISIGN
      INTEGER JSUB,KSUB,NSUB,JJ,KK,ISS1,ISS2,IMS1,IMS2,ITYPSUB,
     .   TAGIP(MVSIZ)
      my_real
     .    VX(4*MVSIZ),  VY(4*MVSIZ),  VZ(4*MVSIZ),  VN(4*MVSIZ),
     .   FXI(4*MVSIZ), FYI(4*MVSIZ), FZI(4*MVSIZ), FNI(4*MVSIZ),
     .   FX1(4*MVSIZ), FX2(4*MVSIZ), FX3(4*MVSIZ), FX4(4*MVSIZ),
     .   FY1(4*MVSIZ), FY2(4*MVSIZ), FY3(4*MVSIZ), FY4(4*MVSIZ),
     .   FZ1(4*MVSIZ), FZ2(4*MVSIZ), FZ3(4*MVSIZ), FZ4(4*MVSIZ),
     .   FXT(4*MVSIZ), FYT(4*MVSIZ), FZT(4*MVSIZ),
     .   VIS2(4*MVSIZ),PENE(4*MVSIZ),DIST(4*MVSIZ),
     .   VNX, VNY, VNZ, AA, VMAX,S2,
     .   V2, FM2, DT1INV, VISCA,  FAC,  FF,
     .   FX, FY, FZ, F2, MAS2, DTMI0,DTI,
     .   FACM1, ECONTT, ECONVT, A2,MASM,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8,
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15,
     .   FSAV22, FSAV23, FSAV24,DGAPLOAD,
     .   FSAVSUB1(24,NISUB), IMPX, IMPY, IMPZ,FTN ,FN , FT,BETA
      my_real
     .   PREC
      my_real
     .   ST1(4*MVSIZ),ST2(4*MVSIZ),ST3(4*MVSIZ),ST4(4*MVSIZ),STIF0(4*MVSIZ),
     .   KT(4*MVSIZ),C(4*MVSIZ),CF(4*MVSIZ),
     .   K1(4*MVSIZ),K2(4*MVSIZ),K3(4*MVSIZ),K4(4*MVSIZ),
     .   C1(4*MVSIZ),C2(4*MVSIZ),C3(4*MVSIZ),C4(4*MVSIZ),
     .   CX,CY,CFI,AUX,AAA
      DOUBLE PRECISION
     .   FX6(6,4*MVSIZ), FY6(6,4*MVSIZ), FZ6(6,4*MVSIZ)
C
      INTEGER BITGET
      EXTERNAL BITGET
C-----------------------------------------------

#include "sync_dir.inc"
      IF (IRESP == 1) THEN
        PREC = FIVEEM4
      ELSE
        PREC = EM10
      ENDIF
      IF(DT1>ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONVT = ZERO
      DO I=1,JLT
        STIF0(I) = STIF(I)
      ENDDO
C
      DO I=1,JLT
        S2 = SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2)
        DIST(I)=S2
        IF(GAPVE(I)/=ZERO)THEN
          PENE(I) = MAX(ZERO,GAPVE(I) - S2)
        ELSE ! Solids
          PENE(I) = S2
c           print *,pene(i),itab(n1(i)),itab(n2(i)),itab(m1(i)),itab(m2(i))
        END IF
        S2 = ONE/MAX(EM30,S2)
        NX(I) = NX(I)*S2
        NY(I) = NY(I)*S2
        NZ(I) = NZ(I)*S2
C        WRITE(6,"(2I20,X,A,3Z20)") EDGE_ID(1,I),EDGE_ID(2,I),"NXYZ=",NX(I),NY(I),NZ(I)
      ENDDO
C
      DO I=1,JLT
        DEBUG_E2E(EDGE_ID(1,I)==D_EM .AND. EDGE_ID(2,I) ==  D_ES,CAND_P(INDX2(I),INDX1(I)))
        DEBUG_E2E(EDGE_ID(1,I)==D_EM .AND. EDGE_ID(2,I) ==  D_ES,PENE(I))
        IF(CAND_P(INDX2(I),INDX1(I))==ZERO)CAND_P(INDX2(I),INDX1(I))=PENE(I) ! 1st impact
      ENDDO
C
      IF(INACTI/=-1)THEN ! INACTI=5 & INACTI=0 !
        DO I=1,JLT

          IF(CAND_P(INDX2(I),INDX1(I))<ZERO) THEN ! Penetration coming from starter
            IF(STARTT>ZERO) THEN  ! If Tstart Peneinit = Pene_engine
              CAND_P(INDX2(I),INDX1(I))=PENE(I) ! 1st impact
            ELSE
              CAND_P(INDX2(I),INDX1(I))=-CAND_P(INDX2(I),INDX1(I)) ! 1st impact
            ENDIF
          ENDIF

C          Reduce PENE
          IF(PENE(I)/=CAND_P(INDX2(I),INDX1(I))) ! insures force == zero !
     .      CAND_P(INDX2(I),INDX1(I))=MIN(CAND_P(INDX2(I),INDX1(I)),
     .                           ((ONE-FIVEEM2)*CAND_P(INDX2(I),INDX1(I))+FIVEEM2*PENE(I))  )

          PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDX2(I),INDX1(I)))
          IF( PENE(I)==ZERO )  STIF(I) = ZERO
        ENDDO
      ELSE
        DO I=1,JLT
          IF(CAND_P(INDX2(I),INDX1(I)) < ZERO)THEN
C
C            CAND_P < 0 <=> Initial penetration computed into the Starter => Do not reduce PENE
            CAND_P(INDX2(I),INDX1(I)) = -CAND_P(INDX2(I),INDX1(I))
            IF(PENE(I)/=CAND_P(INDX2(I),INDX1(I))) ! insures no modification of CAND_P !
     .      CAND_P(INDX2(I),INDX1(I)) = MIN(CAND_P(INDX2(I),INDX1(I)),
     .                            ((ONE-FIVEEM2)*CAND_P(INDX2(I),INDX1(I))+FIVEEM2*PENE(I))  )
            CAND_P(INDX2(I),INDX1(I)) = -CAND_P(INDX2(I),INDX1(I)) ! back to negative value
            IF( PENE(I)==ZERO )  STIF(I) = ZERO
          ELSE
C
C            New impact computed into the Engine => Reduce PENE
            IF(PENE(I)/=CAND_P(INDX2(I),INDX1(I))) ! insures force == zero !
     .        CAND_P(INDX2(I),INDX1(I))=MIN(CAND_P(INDX2(I),INDX1(I)),
     .                           ((ONE-FIVEEM2)*CAND_P(INDX2(I),INDX1(I))+FIVEEM2*PENE(I))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
            PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDX2(I),INDX1(I)))
            IF( PENE(I)==ZERO )  STIF(I) = ZERO
          END IF
        ENDDO
      ENDIF

      VMAX = ZERO
      DO I=1,JLT
        VX(I) = HS1(I)*VXS1(I) + HS2(I)*VXS2(I)
     .        - HM1(I)*VXM1(I) - HM2(I)*VXM2(I)
        VY(I) = HS1(I)*VYS1(I) + HS2(I)*VYS2(I)
     .        - HM1(I)*VYM1(I) - HM2(I)*VYM2(I)
        VZ(I) = HS1(I)*VZS1(I) + HS2(I)*VZS2(I)
     .        - HM1(I)*VZM1(I) - HM2(I)*VZM2(I)
        VN(I) = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
      ENDDO
C-------------------------------------------

      DO I=1,JLT
        STIF(I)= HALF*STIF(I)
        FNI(I) = -STIF(I) * PENE(I)
        ECONVT = ECONVT+FNI(I)*VN(I)*DT1
      ENDDO
C---------------------------------
C     DAMPING + FRIC
C---------------------------------
      IF(VISC/=ZERO)THEN
        IF(IVIS2==-1)THEN
          IF(KDTINT==0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
            DO I=1,JLT
              FAC = STIF(I) / MAX(EM30,STIF(I))
              MAS2  = MS1(I)*HS1(I)
     .              + MS2(I)*HS2(I)
              MASM  = MM1(I)*HM1(I)
     .              + MM2(I)*HM2(I)
              VIS2(I) = TWO * STIF(I) * MIN(MAS2,MASM)
              VIS = SQRT(VIS2(I))
              FF  = FAC * VISC * VIS
              STIF(I) = STIF0(I) + FF * DT1INV
              STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
              FF = FF * VN(I)
              ECONVT = ECONVT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
            ENDDO

          ELSE
            DO I=1,JLT
              FAC = STIF(I) / MAX(EM30,STIF(I))
              MAS2  = MS1(I)*HS1(I)
     .              + MS2(I)*HS2(I)
              MASM  = MM1(I)*HM1(I)
     .              + MM2(I)*HM2(I)
              VIS2(I) = TWO * STIF(I) * MIN(MAS2,MASM)
              VIS = SQRT(VIS2(I))
              C(I)= FAC * VISC * VIS
              KT(I)= STIF0(I)
              STIF(I) = STIF(I) + C(I) * DT1INV
              FF = C(I) * VN(I)
              ECONVT = ECONVT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
              CF(I)   = FAC*SQRT(VISCF)*VIS
              STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
            ENDDO
          ENDIF
        ELSEIF(IVIS2==1)THEN
C---------------------------------
          IF(KDTINT==0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
            DO I=1,JLT
              FAC = STIF(I) / MAX(EM30,STIF(I))
              MAS2  = MS1(I)*HS1(I)
     .              + MS2(I)*HS2(I)
              MASM  = MM1(I)*HM1(I)
     .              + MM2(I)*HM2(I)
C           WRITE(6,"(2I20,3Z20)") EDGE_ID(1,I),EDGE_ID(2,I),STIF(I),MASM,MAS2
c           DEBUG_E2E(.TRUE.,STIF(I))
c           DEBUG_E2E(.TRUE.,MAS2)
c           DEBUG_E2E(.TRUE.,MASM)

              VIS2(I) = TWO* STIF(I) * MASM * MAS2 /
     .             MAX(EM30,MASM+MAS2)
              VIS = SQRT(VIS2(I))
              FF  = FAC * VISC * VIS
              STIF(I) = STIF0(I) + FF * DT1INV
              STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
              FF = FF * VN(I)
              ECONVT = ECONVT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
            ENDDO

          ELSE
            DO I=1,JLT
              FAC = STIF(I) / MAX(EM30,STIF(I))
              MAS2  = MS1(I)*HS1(I)
     .              + MS2(I)*HS2(I)
              MASM  = MM1(I)*HM1(I)
     .              + MM2(I)*HM2(I)
              VIS2(I) = TWO* STIF(I) * MASM * MAS2 /
     .             MAX(EM30,MASM+MAS2)
              VIS = SQRT(VIS2(I))
              C(I)= FAC * VISC * VIS
              KT(I)= STIF0(I)
              STIF(I) = STIF(I) + C(I) * DT1INV
              FF = C(I) * VN(I)
              ECONVT = ECONVT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
              CF(I)   = FAC*SQRT(VISCF)*VIS
              STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
            ENDDO
          ENDIF

        ELSEIF(IVIS2==2)THEN
C---------------------------------
C         VISC QUAD TYPE
C---------------------------------
          DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            MAS2  = MS1(I)*HS1(I)
     .            + MS2(I)*HS2(I)
            MASM  = MM1(I)*HM1(I)
     .            + MM2(I)*HM2(I)
            VIS2(I) = TWO * STIF(I) * MIN(MAS2,MASM)
            VIS = SQRT(VIS2(I))
            FF  = FAC * VISC * VIS
            STIF(I) = STIF0(I) + TWO * FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
            FF = FF * VN(I)
            ECONVT = ECONVT + FF * VN(I) * DT1
            FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2==3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
          DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            MAS2  = MS1(I)*HS1(I)
     .            + MS2(I)*HS2(I)
            MASM  = MM1(I)*HM1(I)
     .            + MM2(I)*HM2(I)
            VIS2(I) = TWO * STIF(I) * MIN(MAS2,MASM)
            VIS = SQRT(VIS2(I))
            FF  = FAC *  VISC * VIS
            STIF(I) = STIF0(I) + TWO* FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
            FF = FF * VN(I)
            ECONVT = ECONVT + FF * VN(I) * DT1
            FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2==4)THEN
C---------------------------------
C         VISC = 0
C---------------------------------
          DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            MAS2  = MS1(I)*HS1(I)
     .            + MS2(I)*HS2(I)
            MASM  = MM1(I)*HM1(I)
     .            + MM2(I)*HM2(I)
            VIS2(I) = TWO * STIF(I) * MIN(MAS2,MASM)
            VIS = SQRT(VIS2(I))
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
          ENDDO
        ELSEIF(IVIS2==5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
          DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            MAS2  = MS1(I)*HS1(I)
     .            + MS2(I)*HS2(I)
            MASM  = MM1(I)*HM1(I)
     .            + MM2(I)*HM2(I)
            VIS2(I) = TWO* STIF(I) * MASM * MAS2 /
     .           MAX(EM30,MASM+MAS2)
            VIS = 2. * VISC * DT1INV * MASM * MAS2 /
     .           MAX(EM30,MASM+MAS2)
            STIF(I) = MAX(STIF0(I) ,FAC*SQRT(VISCF*VIS2(I))*DT1INV)
            FF = VIS * VN(I)
            ECONVT = ECONVT + FF * VN(I) * DT1 ! this is wrong (but unused)...
            FNI(I)  = MIN(FNI(I),FF)
          ENDDO
        ELSE
        ENDIF
      ELSE
      ENDIF
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION NORMALE
C---------------------------------
      FSAV1 = ZERO
      FSAV2 = ZERO
      FSAV3 = ZERO
      FSAV8 = ZERO
      FSAV9 = ZERO
      FSAV10= ZERO
      FSAV11= ZERO
      IF(ILEV==2)THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          IE=CM_LOC(I)
          IMS2 = BITGET(MBINFLG(IE),1)
          FXI(I)=NX(I)*FNI(I)
          FYI(I)=NY(I)*FNI(I)
          FZI(I)=NZ(I)*FNI(I)
          IMPX=FXI(I)*DT12
          IMPY=FYI(I)*DT12
          IMPZ=FZI(I)*DT12
          IF (IMS2 > 0 ) THEN
            FSAV1 =FSAV1 -IMPX
            FSAV2 =FSAV2 -IMPY
            FSAV3 =FSAV3 -IMPZ
            FSAV11=FSAV11-FNI(I)*DT12
          ELSE
            FSAV1 =FSAV1 +IMPX
            FSAV2 =FSAV2 +IMPY
            FSAV3 =FSAV3 +IMPZ
            FSAV11=FSAV11+FNI(I)*DT12
          END IF
          FSAV8 =FSAV8 +ABS(IMPX)
          FSAV9 =FSAV9 +ABS(IMPY)
          FSAV10=FSAV10+ABS(IMPZ)
          IF(ISENSINT(1)/=0) THEN
            IF (IMS2 >0 ) THEN
              FSAVPARIT(1,1,I) =  -FXI(I)
              FSAVPARIT(1,2,I) =  -FYI(I)
              FSAVPARIT(1,3,I) =  -FZI(I)
            ELSE
              FSAVPARIT(1,1,I) =  FXI(I)
              FSAVPARIT(1,2,I) =  FYI(I)
              FSAVPARIT(1,3,I) =  FZI(I)
            END IF
          ENDIF
        ENDDO
      ELSE
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          FXI(I)=NX(I)*FNI(I)
          FYI(I)=NY(I)*FNI(I)
          FZI(I)=NZ(I)*FNI(I)
          IMPX=FXI(I)*DT12
          IMPY=FYI(I)*DT12
          IMPZ=FZI(I)*DT12
          FSAV1 =FSAV1 -IMPX
          FSAV2 =FSAV2 -IMPY
          FSAV3 =FSAV3 -IMPZ
          FSAV11=FSAV11-FNI(I)*DT12
          FSAV8 =FSAV8 +ABS(IMPX)
          FSAV9 =FSAV9 +ABS(IMPY)
          FSAV10=FSAV10+ABS(IMPZ)
          IF(ISENSINT(1)/=0) THEN
            FSAVPARIT(1,1,I) =  FXI(I)
            FSAVPARIT(1,2,I) =  FYI(I)
            FSAVPARIT(1,3,I) =  FZI(I)
          ENDIF
        ENDDO
      END IF
      IF (IMCONV==1) THEN
#include "lockon.inc"
        FSAV(1)=FSAV(1)+FSAV1
        FSAV(2)=FSAV(2)+FSAV2
        FSAV(3)=FSAV(3)+FSAV3
        FSAV(8)=FSAV(8)+FSAV8
        FSAV(9)=FSAV(9)+FSAV9
        FSAV(10)=FSAV(10)+FSAV10
        FSAV(11)=FSAV(11)+FSAV11
#include "lockoff.inc"
      ENDIF
C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------
      IF(NISUB/=0)THEN
        DO JSUB=1,NISUB
          DO J=1,24
            FSAVSUB1(J,JSUB)=ZERO
          END DO
        ENDDO
        DO I=1,JLT

          IF(PENE(I) == ZERO)CYCLE

          IL = CS_LOC(I)
          IF(IL<=NEDGE)THEN

            IF (MSEGTYP(CM_LOC(I)) < 0) THEN
              IE= - MSEGTYP(CM_LOC(I))
            ELSE
              IE = CM_LOC(I)
            ENDIF
            IF(IE > NRTM) IE=IE-NRTM

            JJ  =ADDSUBE(IL)
            KK  =ADDSUBM(IE)
            DO WHILE(JJ<ADDSUBE(IL+1))
              JSUB=LISUBE(JJ)
              ITYPSUB = TYPSUB(JSUB)

              IF(ITYPSUB == 1 ) THEN  ! Defining specific inter

                ISS1 = BITGET(INFLG_SUBE(JJ),0)
                ISS2 = BITGET(INFLG_SUBE(JJ),1)
                KSUB=LISUBE(KK)
                DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBE(IE+1)))
                  IMS1 = BITGET(INFLG_SUBM(KK),0)
                  IMS2 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB==JSUB)THEN
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                      (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                      KK=KK+1
                      KSUB=LISUBE(KK)
                      CYCLE
                    END IF
                    IMPX=FXI(I)*DT12
                    IMPY=FYI(I)*DT12
                    IMPZ=FZI(I)*DT12

                    IF(IMS2 > 0)THEN
                      FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)-IMPX
                      FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)-IMPY
                      FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)-IMPZ
                      FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)-FNI(I)*DT12
                    ELSE
                      FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                      FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                      FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
                      FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
                    END IF
C
                    IF(ISENSINT(JSUB+1)/=0) THEN
                      IF(IMS2 > 0)THEN
                        FSAVPARIT(JSUB+1,1,I) =  -FXI(I)
                        FSAVPARIT(JSUB+1,2,I) =  -FYI(I)
                        FSAVPARIT(JSUB+1,3,I) =  -FZI(I)
                      ELSE
                        FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                        FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                        FSAVPARIT(JSUB+1,3,I) =  FZI(I)
                      END IF
                    ENDIF
C
                    FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                    FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                    FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                  ENDIF
                  KK=KK+1
                  KSUB=LISUBE(KK)
                ENDDO
                JJ=JJ+1

              ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only secnd surface

                IMPX=FXI(I)*DT12
                IMPY=FYI(I)*DT12
                IMPZ=FZI(I)*DT12


                FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ

                FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12

                IF(ISENSINT(JSUB+1)/=0) THEN
                  FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                  FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                  FSAVPARIT(JSUB+1,3,I) =  FZI(I)
                ENDIF

                JJ=JJ+1

              ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfs

                ISS2 = BITGET(INFLG_SUBE(JJ),0)
                ISS1 = BITGET(INFLG_SUBE(JJ),1)
                KSUB=LISUBE(KK)
                DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBE(IE+1)))
                  IMS2 = BITGET(INFLG_SUBM(KK),0)
                  IMS1 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB==JSUB)THEN
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                      (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                      KK=KK+1
                      KSUB=LISUBE(KK)
                      CYCLE
                    END IF

                    IF(IMS2 > 0)THEN
                      FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)-IMPX
                      FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)-IMPY
                      FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)-IMPZ
                      FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)-FNI(I)*DT12
                    ELSE
                      FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                      FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                      FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
                      FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
                    ENDIF

                    FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                    FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                    FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                    IF(ISENSINT(JSUB+1)/=0) THEN
                      IF(IMS2 > 0)THEN
                        FSAVPARIT(JSUB+1,1,I) =  -FXI(I)
                        FSAVPARIT(JSUB+1,2,I) =  -FYI(I)
                        FSAVPARIT(JSUB+1,3,I) =  -FZI(I)
                      ELSE
                        FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                        FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                        FSAVPARIT(JSUB+1,3,I) =  FZI(I)
                      END IF
                    ENDIF

C
                  ENDIF
                  KK=KK+1
                  KSUB=LISUBE(KK)
                ENDDO
                JJ=JJ+1

              ENDIF

            END DO
          END IF


          IF (MSEGTYP(CM_LOC(I)) < 0) THEN
            IE= - MSEGTYP(CM_LOC(I))
          ELSE
            IE = CM_LOC(I)
          ENDIF
          IF(IE > NRTM) IE=IE-NRTM

          KK  =ADDSUBM(IE)
          DO WHILE(KK<ADDSUBE(IE+1))
            KSUB=LISUBE(KK)
            ITYPSUB = TYPSUB(KSUB)
            IF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : main side

              IMPX=-FXI(I)*DT12
              IMPY=-FYI(I)*DT12
              IMPZ=-FZI(I)*DT12

              FSAVSUB1(1,KSUB)=FSAVSUB1(1,KSUB)+IMPX
              FSAVSUB1(2,KSUB)=FSAVSUB1(2,KSUB)+IMPY
              FSAVSUB1(3,KSUB)=FSAVSUB1(3,KSUB)+IMPZ

              FSAVSUB1(8,KSUB) =FSAVSUB1(8,KSUB) +ABS(IMPX)
              FSAVSUB1(9,KSUB) =FSAVSUB1(9,KSUB) +ABS(IMPY)
              FSAVSUB1(10,KSUB)=FSAVSUB1(10,KSUB)+ABS(IMPZ)

              FSAVSUB1(11,KSUB)=FSAVSUB1(11,KSUB)-FNI(I)*DT12

              IF(ISENSINT(KSUB+1)/=0) THEN
                FSAVPARIT(KSUB+1,1,I) =  -FXI(I)
                FSAVPARIT(KSUB+1,2,I) =  -FYI(I)
                FSAVPARIT(KSUB+1,3,I) =  -FZI(I)
              ENDIF

            ENDIF
            KK=KK+1
          ENDDO

        END DO
        IF(NSPMD > 1) THEN
          DO I=1,JLT

            IF(PENE(I) == ZERO)CYCLE

            IL = CS_LOC(I)
            IF(IL > NEDGE)THEN

              IF (MSEGTYP(CM_LOC(I)) < 0) THEN
                IE= - MSEGTYP(CM_LOC(I))
              ELSE
                IE = CM_LOC(I)
              ENDIF
              IF(IE > NRTM) IE=IE-NRTM

              IL = IL - NEDGE
              JJ  =ADDSUBSFIE(NIN)%P(IL)
              KK  =ADDSUBM(IE)
              DO WHILE(JJ<ADDSUBSFIE(NIN)%P(IL+1))
                JSUB = LISUBSFIE(NIN)%P(JJ)
                ITYPSUB = TYPSUB(JSUB)

                IF(ITYPSUB == 1 ) THEN  ! Defining specific inter
                  ISS1 = BITGET(INFLG_SUBSFIE(NIN)%P(JJ),0)
                  ISS2 = BITGET(INFLG_SUBSFIE(NIN)%P(JJ),1)
                  KSUB=LISUBE(KK)
                  DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBE(IE+1)))
                    IMS1 = BITGET(INFLG_SUBM(KK),0)
                    IMS2 = BITGET(INFLG_SUBM(KK),1)
                    IF(KSUB==JSUB)THEN
                      IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                         (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBE(KK)
                        CYCLE
                      END IF
                      IMPX=FXI(I)*DT12
                      IMPY=FYI(I)*DT12
                      IMPZ=FZI(I)*DT12

                      IF(IMS2 > 0)THEN
                        FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)-IMPX
                        FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)-IMPY
                        FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)-IMPZ
                        FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)-FNI(I)*DT12
                      ELSE
                        FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                        FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                        FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
                        FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
                      END IF
C
                      IF(ISENSINT(JSUB+1)/=0) THEN
                        IF(IMS2 > 0)THEN
                          FSAVPARIT(JSUB+1,1,I) =  -FXI(I)
                          FSAVPARIT(JSUB+1,2,I) =  -FYI(I)
                          FSAVPARIT(JSUB+1,3,I) =  -FZI(I)
                        ELSE
                          FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                          FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                          FSAVPARIT(JSUB+1,3,I) =  FZI(I)
                        END IF
                      ENDIF
C
                      FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                      FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                      FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                    ENDIF
                    KK=KK+1
                    KSUB=LISUBE(KK)
                  ENDDO
                  JJ=JJ+1

                ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only secnd surface

                  IMPX=FXI(I)*DT12
                  IMPY=FYI(I)*DT12
                  IMPZ=FZI(I)*DT12


                  FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                  FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                  FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ

                  FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                  FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                  FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                  FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12

                  IF(ISENSINT(JSUB+1)/=0) THEN
                    FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                    FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                    FSAVPARIT(JSUB+1,3,I) =  FZI(I)
                  ENDIF

                  JJ=JJ+1

                ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2surfs

                  ISS2 = BITGET(INFLG_SUBSFIE(NIN)%P(JJ),0)
                  ISS1 = BITGET(INFLG_SUBSFIE(NIN)%P(JJ),1)
                  KSUB=LISUBE(KK)
                  DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBE(IE+1)))
                    IMS2 = BITGET(INFLG_SUBM(KK),0)
                    IMS1 = BITGET(INFLG_SUBM(KK),1)
                    IF(KSUB==JSUB)THEN
                      IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                       (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBE(KK)
                        CYCLE
                      END IF

                      IMPX=FXI(I)*DT12
                      IMPY=FYI(I)*DT12
                      IMPZ=FZI(I)*DT12

                      IF(IMS2 > 0)THEN
                        FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)-IMPX
                        FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)-IMPY
                        FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)-IMPZ
                        FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)-FNI(I)*DT12
                      ELSE
                        FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                        FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                        FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
                        FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
                      ENDIF

                      FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                      FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                      FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                      IF(ISENSINT(JSUB+1)/=0) THEN
                        IF(IMS2 > 0)THEN
                          FSAVPARIT(JSUB+1,1,I) =  -FXI(I)
                          FSAVPARIT(JSUB+1,2,I) =  -FYI(I)
                          FSAVPARIT(JSUB+1,3,I) =  -FZI(I)
                        ELSE
                          FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                          FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                          FSAVPARIT(JSUB+1,3,I) =  FZI(I)
                        END IF
                      ENDIF
C
                    ENDIF
                    KK=KK+1
                    KSUB=LISUBE(KK)
                  ENDDO
                  JJ=JJ+1

                ENDIF

              END DO
            END IF
          END DO
        ENDIF
      END IF
C---------------------------------
C     FRICTION
C---------------------------------

      FXT(1:JLT)=ZERO
      FYT(1:JLT)=ZERO
      FZT(1:JLT)=ZERO
C
      FSAV4 = ZERO
      FSAV5 = ZERO
      FSAV6 = ZERO
      FSAV12= ZERO
      FSAV13= ZERO
      FSAV14= ZERO
      FSAV15= ZERO
C
      IF (IFQ /= 0) THEN
        DO I=1,JLT


          IF(PENE(I) == ZERO)CYCLE

          FX = STIF0(I)*VX(I)*DT12
          FY = STIF0(I)*VY(I)*DT12
          FZ = STIF0(I)*VZ(I)*DT12

          FX = CAND_FX(INDX2(I),INDX1(I)) + FX
          FY = CAND_FY(INDX2(I),INDX1(I)) + FY
          FZ = CAND_FZ(INDX2(I),INDX1(I)) + FZ

          FTN = FX*NX(I) + FY*NY(I) + FZ*NZ(I)
          FX = FX - FTN*NX(I)
          FY = FY - FTN*NY(I)
          FZ = FZ - FTN*NZ(I)
          FT = FX*FX + FY*FY + FZ*FZ
          FT = MAX(FT,EM30)

          FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
          BETA = MIN(ONE,FRICC(I)*SQRT(FN/FT))
          FXT(I) = FX * BETA
          FYT(I) = FY * BETA
          FZT(I) = FZ * BETA

          CAND_FX(INDX2(I),INDX1(I)) = FXT(I)
          CAND_FY(INDX2(I),INDX1(I)) = FYT(I)
          CAND_FZ(INDX2(I),INDX1(I)) = FZT(I)

          FXI(I)=FXI(I) + FXT(I)
          FYI(I)=FYI(I) + FYT(I)
          FZI(I)=FZI(I) + FZT(I)

          IFPEN(INDX1(I)) = 1

          FSAV4 = FSAV4 + FXT(I)*DT12
          FSAV5 = FSAV5 + FYT(I)*DT12
          FSAV6 = FSAV6 + FZT(I)*DT12

          FSAV12 = FSAV12 + ABS(FXI(I)*DT12)
          FSAV13 = FSAV13 + ABS(FYI(I)*DT12)
          FSAV14 = FSAV14 + ABS(FZI(I)*DT12)
          FSAV15 = FSAV15 + SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
          ECONVT = ECONVT
     .          + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
        ENDDO
      ENDIF

      IF (INCONV==1) THEN
#include "lockon.inc"
        FSAV(4) = FSAV(4) + FSAV4
        FSAV(5) = FSAV(5) + FSAV5
        FSAV(6) = FSAV(6) + FSAV6
        FSAV(12) = FSAV(12) + FSAV12
        FSAV(13) = FSAV(13) + FSAV13
        FSAV(14) = FSAV(14) + FSAV14
        FSAV(15) = FSAV(15) + FSAV15
#include "lockoff.inc"
      ENDIF

C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------
      IF(NISUB/=0)THEN
        DO I=1,JLT

          IF(PENE(I) == ZERO)CYCLE

          IL = CS_LOC(I)
          IF(IL<=NEDGE)THEN

            IF (MSEGTYP(CM_LOC(I)) < 0) THEN
              IE= - MSEGTYP(CM_LOC(I))
            ELSE
              IE = CM_LOC(I)
            ENDIF
            IF(IE > NRTM) IE=IE-NRTM

            JJ  =ADDSUBE(IL)
            KK  =ADDSUBM(IE)
            DO WHILE(JJ<ADDSUBE(IL+1))
              JSUB=LISUBE(JJ)

              ITYPSUB = TYPSUB(JSUB)

              IF(ITYPSUB == 1 ) THEN  ! Defining specific inter

                ISS1 = BITGET(INFLG_SUBE(JJ),0)
                ISS2 = BITGET(INFLG_SUBE(JJ),1)
                KSUB=LISUBE(KK)
                DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBE(IE+1)))
                  IMS1 = BITGET(INFLG_SUBM(KK),0)
                  IMS2 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB==JSUB)THEN
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                       (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                      KK=KK+1
                      KSUB=LISUBE(KK)
                      CYCLE
                    END IF
                    IMPX=FXT(I)*DT12
                    IMPY=FYT(I)*DT12
                    IMPZ=FZT(I)*DT12
C                  main side :
                    FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                    FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                    FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
                    IMPX=FXI(I)*DT12
                    IMPY=FYI(I)*DT12
                    IMPZ=FZI(I)*DT12
                    FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                    FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                    FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                    IF(ISENSINT(JSUB+1)/=0) THEN
                      FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                      FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                      FSAVPARIT(JSUB+1,6,I) =  FZT(I)
                    ENDIF
C
                    FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                            +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
c                   FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
c     .                        +YP(I)*IMPZ-ZP(I)*IMPY
c                   FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
c     .                        +ZP(I)*IMPX-XP(I)*IMPZ
c                   FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
c     .                           +XP(I)*IMPY-YP(I)*IMPX
C
                  ENDIF
                  KK=KK+1
                  KSUB=LISUBE(KK)
                ENDDO
                JJ=JJ+1

              ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : second side

                IMPX=FXT(I)*DT12
                IMPY=FYT(I)*DT12
                IMPZ=FZT(I)*DT12
C               main side :
                FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
                IMPX=FXI(I)*DT12
                IMPY=FYI(I)*DT12
                IMPZ=FZI(I)*DT12
                FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                IF(ISENSINT(JSUB+1)/=0) THEN
                  FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                  FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                  FSAVPARIT(JSUB+1,6,I) =  FZT(I)
                ENDIF
C
                FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                            +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
c     .                              +XP(I)*IMPY-YP(I)*IMPX

                JJ=JJ+1

              ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2surfs

                ISS2 = BITGET(INFLG_SUBE(JJ),0)
                ISS1 = BITGET(INFLG_SUBE(JJ),1)
                KSUB=LISUBE(KK)
                DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBE(IE+1)))
                  IMS2 = BITGET(INFLG_SUBM(KK),0)
                  IMS1 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB==JSUB)THEN
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                       (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                      KK=KK+1
                      KSUB=LISUBE(KK)
                      CYCLE
                    END IF

                    IMPX=FXT(I)*DT12
                    IMPY=FYT(I)*DT12
                    IMPZ=FZT(I)*DT12

                    IF(IMS2 > 0) THEN
                      FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)-IMPX
                      FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)-IMPY
                      FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)-IMPZ
                    ELSE
C                 main side :
                      FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                      FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                      FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
                    ENDIF

C
                    IMPX=FXI(I)*DT12
                    IMPY=FYI(I)*DT12
                    IMPZ=FZI(I)*DT12
                    FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                    FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                    FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                    IF(ISENSINT(JSUB+1)/=0) THEN
                      IF(IMS2 > 0) THEN
                        FSAVPARIT(JSUB+1,4,I) =  -FXT(I)
                        FSAVPARIT(JSUB+1,5,I) =  -FYT(I)
                        FSAVPARIT(JSUB+1,6,I) =  -FZT(I)
                      ELSE
                        FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                        FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                        FSAVPARIT(JSUB+1,6,I) =  FZT(I)
                      ENDIF
                    ENDIF
C
                    FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                              +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
c     .                              +XP(I)*IMPY-YP(I)*IMPX
                  ENDIF
                  KK=KK+1
                  KSUB=LISUBE(KK)
                ENDDO
                JJ=JJ+1

              ENDIF

            END DO
          END IF

          IF (MSEGTYP(CM_LOC(I)) < 0) THEN
            IE= - MSEGTYP(CM_LOC(I))
          ELSE
            IE = CM_LOC(I)
          ENDIF
          IF(IE > NRTM) IE=IE-NRTM

          KK  =ADDSUBM(IE)
          DO WHILE(KK<ADDSUBE(IE+1))
            KSUB=LISUBE(KK)
            ITYPSUB = TYPSUB(KSUB)
            IF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : main side

              IMPX=-FXT(I)*DT12
              IMPY=-FYT(I)*DT12
              IMPZ=-FZT(I)*DT12
C               main side :
              FSAVSUB1(4,KSUB)=FSAVSUB1(4,KSUB)+IMPX
              FSAVSUB1(5,KSUB)=FSAVSUB1(5,KSUB)+IMPY
              FSAVSUB1(6,KSUB)=FSAVSUB1(6,KSUB)+IMPZ
C
              IMPX=FXI(I)*DT12
              IMPY=FYI(I)*DT12
              IMPZ=FZI(I)*DT12
              FSAVSUB1(12,KSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
              FSAVSUB1(13,KSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
              FSAVSUB1(14,KSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
              IF(ISENSINT(KSUB+1)/=0) THEN
                FSAVPARIT(KSUB+1,4,I) =  -FXT(I)
                FSAVPARIT(KSUB+1,5,I) =  -FYT(I)
                FSAVPARIT(KSUB+1,6,I) =  -FZT(I)
              ENDIF
C
              FSAVSUB1(15,KSUB)= FSAVSUB1(15,KSUB)
     .                          +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
c     .                              +XP(I)*IMPY-YP(I)*IMPX
            ENDIF
            KK=KK+1
          ENDDO

        END DO       ! FAIRE LA PARTIE SPMD
        IF(NSPMD > 1) THEN
          DO I=1,JLT

            IF(PENE(I) == ZERO)CYCLE

            IL = CS_LOC(I)
            IF(IL>NEDGE)THEN
              IL = IL - NEDGE
              IF (MSEGTYP(CM_LOC(I)) < 0) THEN
                IE= - MSEGTYP(CM_LOC(I))
              ELSE
                IE = CM_LOC(I)
              ENDIF
              IF(IE > NRTM) IE=IE-NRTM

              JJ  =ADDSUBSFIE(NIN)%P(IL)
              KK  =ADDSUBM(IE)
              DO WHILE(JJ<ADDSUBSFIE(NIN)%P(IL+1))
                JSUB = LISUBSFIE(NIN)%P(JJ)
                ITYPSUB = TYPSUB(JSUB)

                IF(ITYPSUB == 1 ) THEN  ! Defining specific inter

                  ISS1 = BITGET(INFLG_SUBSFIE(NIN)%P(JJ),0)
                  ISS2 = BITGET(INFLG_SUBSFIE(NIN)%P(JJ),1)
                  KSUB=LISUBE(KK)
                  DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBE(IE+1)))
                    IMS1 = BITGET(INFLG_SUBM(KK),0)
                    IMS2 = BITGET(INFLG_SUBM(KK),1)
                    IF(KSUB==JSUB)THEN
                      IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                       (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBE(KK)
                        CYCLE
                      END IF
                      IMPX=FXT(I)*DT12
                      IMPY=FYT(I)*DT12
                      IMPZ=FZT(I)*DT12
C                  main side :
                      FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                      FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                      FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
                      IMPX=FXI(I)*DT12
                      IMPY=FYI(I)*DT12
                      IMPZ=FZI(I)*DT12
                      FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                      FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                      FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                      IF(ISENSINT(JSUB+1)/=0) THEN
                        FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                        FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                        FSAVPARIT(JSUB+1,6,I) =  FZT(I)
                      ENDIF
C
                      FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                                 +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
c                   FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
c     .                          +YP(I)*IMPZ-ZP(I)*IMPY
c                   FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
c     .                          +ZP(I)*IMPX-XP(I)*IMPZ
c                   FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
c     .                             +XP(I)*IMPY-YP(I)*IMPX
C
                    ENDIF
                    KK=KK+1
                    KSUB=LISUBE(KK)
                  ENDDO
                  JJ=JJ+1


                ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

                  IMPX=FXT(I)*DT12
                  IMPY=FYT(I)*DT12
                  IMPZ=FZT(I)*DT12
C               main side :
                  FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                  FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                  FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
                  IMPX=FXI(I)*DT12
                  IMPY=FYI(I)*DT12
                  IMPZ=FZI(I)*DT12
                  FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                  FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                  FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                  IF(ISENSINT(JSUB+1)/=0) THEN
                    FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                    FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                    FSAVPARIT(JSUB+1,6,I) =  FZT(I)
                  ENDIF
C
                  FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                              +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
c     .                              +XP(I)*IMPY-YP(I)*IMPX

                  JJ=JJ+1

                ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfs

                  ISS2 = BITGET(INFLG_SUBSFIE(NIN)%P(JJ),0)
                  ISS1 = BITGET(INFLG_SUBSFIE(NIN)%P(JJ),1)
                  KSUB=LISUBE(KK)
                  DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBE(IE+1)))
                    IMS2 = BITGET(INFLG_SUBM(KK),0)
                    IMS1 = BITGET(INFLG_SUBM(KK),1)
                    IF(KSUB==JSUB)THEN
                      IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                       (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBE(KK)
                        CYCLE
                      END IF

                      IMPX=FXT(I)*DT12
                      IMPY=FYT(I)*DT12
                      IMPZ=FZT(I)*DT12
                      IF(IMS2 > 0 ) THEN
C                 main side :
                        FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)-IMPX
                        FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)-IMPY
                        FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)-IMPZ
                      ELSE
C                 main side :
                        FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                        FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                        FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
                      ENDIF
C
                      IMPX=FXI(I)*DT12
                      IMPY=FYI(I)*DT12
                      IMPZ=FZI(I)*DT12
                      FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                      FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                      FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                      IF(ISENSINT(JSUB+1)/=0) THEN
                        IF(IMS2 > 0 ) THEN
                          FSAVPARIT(JSUB+1,4,I) =  -FXT(I)
                          FSAVPARIT(JSUB+1,5,I) =  -FYT(I)
                          FSAVPARIT(JSUB+1,6,I) =  -FZT(I)
                        ELSE
                          FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                          FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                          FSAVPARIT(JSUB+1,6,I) =  FZT(I)
                        ENDIF
                      ENDIF
C
                      FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                                +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
c     .                              +XP(I)*IMPY-YP(I)*IMPX
C
                    ENDIF
                    KK=KK+1
                    KSUB=LISUBE(KK)
                  ENDDO
                  JJ=JJ+1

                ENDIF
              END DO
            END IF
          END DO       !SPMD
        ENDIF
#include "lockon.inc"
        DO JSUB=1,NISUB
          NSUB=LISUB(JSUB)
          DO J=1,15
            FSAVSUB(J,NSUB)=FSAVSUB(J,NSUB)+FSAVSUB1(J,JSUB)
          END DO
          FSAVSUB(22,NSUB)=FSAVSUB(22,NSUB)+FSAVSUB1(22,JSUB)
          FSAVSUB(23,NSUB)=FSAVSUB(23,NSUB)+FSAVSUB1(23,JSUB)
          FSAVSUB(24,NSUB)=FSAVSUB(24,NSUB)+FSAVSUB1(24,JSUB)
        END DO
#include "lockoff.inc"
      END IF
C---------------------------------
      IF (IMCONV==1) THEN
#include "lockon.inc"
        ECONTV = ECONTV + ECONVT
        ECONT  = ECONT + ECONTT
#include "lockoff.inc"
      ENDIF
C---------------------------------
      DO I=1,JLT

        IF(PENE(I) == ZERO)CYCLE

        FX1(I)=-FXI(I)*HS1(I)
        FY1(I)=-FYI(I)*HS1(I)
        FZ1(I)=-FZI(I)*HS1(I)
C
        FX2(I)=-FXI(I)*HS2(I)
        FY2(I)=-FYI(I)*HS2(I)
        FZ2(I)=-FZI(I)*HS2(I)
C
        FX3(I)=FXI(I)*HM1(I)
        FY3(I)=FYI(I)*HM1(I)
        FZ3(I)=FZI(I)*HM1(I)
C
        FX4(I)=FXI(I)*HM2(I)
        FY4(I)=FYI(I)*HM2(I)
        FZ4(I)=FZI(I)*HM2(I)
C
      ENDDO

      DO I=1,JLT
        STIF(I) = TWO*STIF(I)
      ENDDO
C
C---------------------------------
      IF(KDTINT==1.OR.IDTMINS==2)THEN
        IF(     (VISC/=ZERO)
     .    .AND.(IVIS2==0.OR.IVIS2==1))THEN
          DO I=1,JLT
            CX= C(I)*C(I)
C
            IF(MS1(I)==ZERO)THEN
              K1(I) =ZERO
              C1(I) =ZERO
            ELSE
              K1(I)=KT(I)*ABS(HS1(I))
              C1(I)=C(I)*ABS(HS1(I))
              CX   =FOUR*C1(I)*C1(I)
              CY   =EIGHT*MS1(I)*K1(I)
              AUX   = SQRT(CX+CY)+TWO*C1(I)
              ST1(I)= K1(I)*AUX*AUX/MAX(CY,EM30)
              CFI   = CF(I)*ABS(HS1(I))
              AUX   = TWO*CFI*CFI/MAX(MS1(I),EM20)
              IF(AUX>ST1(I))THEN
                K1(I) =ZERO
                C1(I) =CFI
              ENDIF
            ENDIF
C
            IF(MS2(I)==ZERO)THEN
              K2(I) =ZERO
              C2(I) =ZERO
            ELSE
              K2(I)=KT(I)*ABS(HS2(I))
              C2(I)=C(I)*ABS(HS2(I))
              CX   =FOUR*C2(I)*C2(I)
              CY   =EIGHT*MS2(I)*K2(I)
              AUX   = SQRT(CX+CY)+TWO*C2(I)
              ST2(I)= K2(I)*AUX*AUX/MAX(CY,EM30)
              CFI   = CF(I)*ABS(HS2(I))
              AUX   = TWO*CFI*CFI/MAX(MS2(I),EM20)
              IF(AUX>ST2(I))THEN
                K2(I) =ZERO
                C2(I) =CFI
              ENDIF
            ENDIF
C
            IF(MM1(I)==ZERO)THEN
              K3(I) =ZERO
              C3(I) =ZERO
            ELSE
              K3(I)=KT(I)*ABS(HM1(I))
              C3(I)=C(I)*ABS(HM1(I))
              CX   =FOUR*C3(I)*C3(I)
              CY   =EIGHT*MM1(I)*K3(I)
              AUX   = SQRT(CX+CY)+TWO*C3(I)
              ST3(I)= K3(I)*AUX*AUX/MAX(CY,EM30)
              CFI   = CF(I)*ABS(HM1(I))
              AUX   = TWO*CFI*CFI/MAX(MM1(I),EM20)
              IF(AUX>ST3(I))THEN
                K3(I) =ZERO
                C3(I) =CFI
              ENDIF
            ENDIF
C
            IF(MM2(I)==ZERO)THEN
              K4(I) =ZERO
              C4(I) =ZERO
            ELSE
              K4(I)=KT(I)*ABS(HM2(I))
              C4(I)=C(I)*ABS(HM2(I))
              CX   =FOUR*C4(I)*C4(I)
              CY   =EIGHT*MM2(I)*K4(I)
              AUX   = SQRT(CX+CY)+TWO*C4(I)
              ST4(I)= K4(I)*AUX*AUX/MAX(CY,EM30)
              CFI   = CF(I)*ABS(HM2(I))
              AUX   = TWO*CFI*CFI/MAX(MM2(I),EM20)
              IF(AUX>ST4(I))THEN
                K4(I) =ZERO
                C4(I) =CFI
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO I=1,JLT
            K1(I) =STIF(I)*ABS(HS1(I))
            C1(I) =ZERO
            K2(I) =STIF(I)*ABS(HS2(I))
            C2(I) =ZERO
            K3(I) =STIF(I)*ABS(HM1(I))
            C3(I) =ZERO
            K4(I) =STIF(I)*ABS(HM2(I))
            C4(I) =ZERO
          ENDDO
        ENDIF
      ENDIF

C------------For /LOAD/PRESSURE tag nodes in contact-------------
      TAGIP(1:JLT) = 0
      IF(NINTLOADP > 0) THEN
        DO K = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1)
          PP = LOADPINTER(K)
          PPL = LOADP_HYD_INTER(PP)
          DGAPLOAD = DGAPLOADINT(K)
          DO I=1,JLT
            IF(PENE(I) > ZERO .OR.DIST(I) <= DGAPLOAD) THEN
              TAGIP(I) = 1
              TAGNCONT(PPL,M1(I)) = 1
              TAGNCONT(PPL,M2(I)) = 1
              IF(CS_LOC(I)<=NEDGE) THEN
C  SPMD : do same after reception of forces for remote nodes
                TAGNCONT(PPL,N1(I)) = 1
                TAGNCONT(PPL,N2(I)) = 1
              ENDIF
            ENDIF
          ENDDO
        ENDDO

      ENDIF
C
C=======================================================================
C     FORCES sur noeuds maites et second
C=======================================================================
      IF(IPARIT==0)THEN
        IF(KDTINT==0)THEN
          CALL I25ASSE0_E2S(JLT  ,CS_LOC ,N1   ,N2   ,M1   ,
     2                 M2   ,HS1   ,HS2  ,HM1  ,HM2  ,
     3                 FX1  ,FY1   ,FZ1  ,FX2  ,FY2  ,
     4                 FZ2  ,FX3   ,FY3  ,FZ3  ,FX4  ,
     5                 FY4  ,FZ4   ,A    ,STIFN,STIF ,
     6                 NEDGE,NIN   ,JTASK,PENE )
        ELSE
          CALL I25ASSE05_E2S(JLT   ,CS_LOC ,N1    ,N2   ,M1   ,
     2                  M2    ,HS1   ,HS2   ,HM1  ,HM2  ,
     3                  FX1   ,FY1   ,FZ1   ,FX2  ,FY2  ,
     4                  FZ2   ,FX3   ,FY3   ,FZ3  ,FX4  ,
     5                  FY4   ,FZ4   ,A     ,STIFN,NEDGE,
     6                  K1    ,K2    ,K3    ,K4   ,C1   ,
     7                  C2    ,C3    ,C4    ,VISCN,NIN  ,
     8                  JTASK ,PENE  )
        END IF
      ELSE
        IF(KDTINT==0)THEN
          CALL I25ASSE2_E2S(JLT   ,CS_LOC ,N1    ,N2    ,M1      ,
     2                 M2    ,HS1    ,HS2   ,HM1   ,HM2     ,
     3                 FX1   ,FY1    ,FZ1   ,FX2   ,FY2     ,
     4                 FZ2   ,FX3    ,FY3   ,FZ3   ,FX4     ,
     5                 FY4   ,FZ4    ,FSKYI ,ISKY  ,NISKYFIE,
     6                 STIF  ,NEDGE  ,NIN   ,NOINT ,PENE    ,
     7                 EDGE_ID,TAGIP )
        ELSE
          CALL I25ASSE25_E2S(JLT  ,CS_LOC ,N1    ,N2      ,M1    ,
     2                  M2   ,HS1    ,HS2   ,HM1     ,HM2   ,
     3                  FX1  ,FY1    ,FZ1   ,FX2     ,FY2   ,
     4                  FZ2  ,FX3    ,FY3   ,FZ3     ,FX4   ,
     5                  FY4  ,FZ4    ,ISKY  ,NISKYFIE,NEDGE ,
     6                  K1   ,K2     ,K3    ,K4      ,C1    ,
     7                  C2   ,C3     ,C4    ,NIN     , NOINT,
     8                  PENE ,TAGIP  )
        END IF
      END IF
C
      IF(IDTMINS==2)THEN
        DTI=DT2T
        CALL I25SMS_E2S(JLT   ,CS_LOC ,N1    ,N2     ,M1     ,
     2               M2    ,HS1    ,HS2   ,HM1    ,HM2    ,
     3               STIF  ,NIN    ,NOINT ,MSKYI_SMS ,ISKYI_SMS,
     4               NSMS  ,K1     ,K2    ,K3     ,K4     ,
     5               C1    ,C2     ,C3    ,C4     ,NEDGE , EDGE_ID)
      END IF
C

      IF (NSPMD>1) THEN
#include "mic_lockon.inc"
        DO I = 1,JLT
          ASSERT(I > 0)
          ASSERT(I <= 4*MVSIZ)
          ASSERT(CS_LOC(I) > 0)
          PRINTIF(CS_LOC(I) < 0,I)
          PRINTIF(CS_LOC(I) < 0,CS_LOC(I))
          IF(CS_LOC(I)>NEDGE)THEN
            NI = CS_LOC(I)-NEDGE
            ASSERT(NI > 0)
C tag temporaire de NSVFI a -
            IF(PENE(I) /= 0.OR.TAGIP(I)==1) THEN
              NSVFIE(NIN)%P(NI) = -ABS(NSVFIE(NIN)%P(NI))
            ENDIF
          ENDIF
        ENDDO
#include "mic_lockoff.inc"
      ENDIF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0)THEN
#include "lockon.inc"
c         goto 1234
        DO I=1,JLT
C
          IF(PENE(I) == ZERO)CYCLE
C
          IF(CS_LOC(I)<=NEDGE) THEN
            FCONT(1,N1(I)) =FCONT(1,N1(I)) + FX1(I)
            FCONT(2,N1(I)) =FCONT(2,N1(I)) + FY1(I)
            FCONT(3,N1(I)) =FCONT(3,N1(I)) + FZ1(I)
            FCONT(1,N2(I)) =FCONT(1,N2(I)) + FX2(I)
            FCONT(2,N2(I)) =FCONT(2,N2(I)) + FY2(I)
            FCONT(3,N2(I)) =FCONT(3,N2(I)) + FZ2(I)
          END IF
          FCONT(1,M1(I)) =FCONT(1,M1(I)) + FX3(I)
          FCONT(2,M1(I)) =FCONT(2,M1(I)) + FY3(I)
          FCONT(3,M1(I)) =FCONT(3,M1(I)) + FZ3(I)
          FCONT(1,M2(I)) =FCONT(1,M2(I)) + FX4(I)
          FCONT(2,M2(I)) =FCONT(2,M2(I)) + FY4(I)
          FCONT(3,M2(I)) =FCONT(3,M2(I)) + FZ4(I)
        ENDDO
c 1234    continue
#include "lockoff.inc"
      ENDIF

C
      IF(ISECIN>0)THEN
        K0=NSTRF(25)
        IF(NSTRF(1)+NSTRF(2)/=0)THEN
          DO I=1,NSECT
            NBINTER=NSTRF(K0+14)
            K1S=K0+30
            DO J=1,NBINTER
              IF(NSTRF(K1S)==NOINT)THEN
                IF(ISECUT/=0)THEN
#include "lockon.inc"
                  DO K=1,JLT
C
                    IF(PENE(K) == ZERO)CYCLE
C
                    IF(CS_LOC(K)<=NEDGE) THEN
                      IF(SECFCUM(4,N1(K),I)==1.)THEN
                        SECFCUM(1,N1(K),I)=SECFCUM(1,N1(K),I)-FX1(K)
                        SECFCUM(2,N1(K),I)=SECFCUM(2,N1(K),I)-FY1(K)
                        SECFCUM(3,N1(K),I)=SECFCUM(3,N1(K),I)-FZ1(K)
                      ENDIF
                      IF(SECFCUM(4,N2(K),I)==1.)THEN
                        SECFCUM(1,N2(K),I)=SECFCUM(1,N2(K),I)-FX2(K)
                        SECFCUM(2,N2(K),I)=SECFCUM(2,N2(K),I)-FY2(K)
                        SECFCUM(3,N2(K),I)=SECFCUM(3,N2(K),I)-FZ2(K)
                      ENDIF
                    END IF
                    IF(SECFCUM(4,M1(K),I)==1.)THEN
                      SECFCUM(1,M1(K),I)=SECFCUM(1,M1(K),I)-FX3(K)
                      SECFCUM(2,M1(K),I)=SECFCUM(2,M1(K),I)-FY3(K)
                      SECFCUM(3,M1(K),I)=SECFCUM(3,M1(K),I)-FZ3(K)
                    ENDIF
                    IF(SECFCUM(4,M2(K),I)==1.)THEN
                      SECFCUM(1,M2(K),I)=SECFCUM(1,M2(K),I)-FX4(K)
                      SECFCUM(2,M2(K),I)=SECFCUM(2,M2(K),I)-FY4(K)
                      SECFCUM(3,M2(K),I)=SECFCUM(3,M2(K),I)-FZ4(K)
                    ENDIF
                  ENDDO
#include "lockoff.inc"
                ENDIF
C +fsav(section)
              ENDIF
              K1S=K1S+1
            ENDDO
            K0=NSTRF(K0+24)
          ENDDO
        ENDIF
      ENDIF
C
      RETURN
      END
