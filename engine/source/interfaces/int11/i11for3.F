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
Chd|  I11FOR3                       source/interfaces/int11/i11for3.F
Chd|-- called by -----------
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11FOR3(
     1                JLT      ,FSAV     ,GAP      ,FRIC    ,MS      ,
     2                VISC     ,VISCF    ,NOINT    ,ITAB    ,CS_LOC  ,
     3                CM_LOC   ,STIF     ,DT2T     ,HS1     ,HS2     ,
     4                HM1      ,HM2      ,N1       ,N2      , M1     ,
     5                M2       ,IVIS2    ,NELTST   ,ITYPTST ,NX      ,
     6                NY       ,NZ       ,GAPV     ,PENIS   ,PENIM   ,
     7                INACTI   ,NEWFRONT ,NRTS     ,MS1     ,MS2     , 
     8                MM1      ,MM2     ,VXS1      ,VYS1    ,VZS1    ,
     9                VXS2     ,VYS2    ,VZS2      ,VXM1    ,VYM1    ,
     A                VZM1     ,VXM2    ,VYM2      ,VZM2    ,NIN     ,
     B                DTMINI   ,IFORM   ,CAND_FX   ,CAND_FY ,CAND_FZ ,       
     C                INDEX    ,IFPEN   ,STFS      ,FNI     ,
     E                FX1      ,FY1     ,FZ1       ,FX2     ,FY2      ,
     F                FZ2      ,FX3     ,FY3       ,FZ3     ,FX4      ,
     G                FY4      ,FZ4     ,K1        ,K2      ,K3       ,
     H                K4       ,C1      ,C2        ,C3      ,C4       ,
     I                INTTH    ,DRAD    ,PENRAD    ,ISENSINT,FSAVPARIT,
     J                NISUB    ,NFT     ,ADDSUBS   ,ADDSUBM ,LISUBS   ,
     K                LISUBM   ,LISUB   ,FSAVSUB   ,FRICC   ,VISCFFRIC,
     L                TAGNCONT ,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,
     M                TYPSUB   ,INFLG_SUBS  ,INFLG_SUBM,NINLOADP     ,
     N                DGAPLOADINT,S_LOADPINTER )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "couple_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,JLT,IBC,IVIS2,INACTI,NRTS,NIN,INTTH  
      INTEGER ITAB(*), 
     .        NOINT,NEWFRONT,NISUB,NFT
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ),
     .        CS_LOC(MVSIZ), CM_LOC(MVSIZ),
     .        NSMS(MVSIZ),IFORM,INDEX(*),IFPEN(*), ISENSINT(*),
     .        ADDSUBS(*),ADDSUBM(*),LISUBS(*),LISUBM(*),LISUB(*),
     .        TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        TYPSUB(*),INFLG_SUBS(*), INFLG_SUBM(*)
      INTEGER  , INTENT(IN) :: NINLOADP,S_LOADPINTER 
      INTEGER  , INTENT(IN) :: KLOADPINTER(NINTER+1),LOADPINTER(S_LOADPINTER),
     .        LOADP_HYD_INTER(NLOADP_HYD)
      my_real 
     .    MS(*), FSAV(*),
     .   STFS(*),GAPV(*),
     .   PENIS(2,*), PENIM(2,*),
     .   GAP, FRIC,VISC,VISCF,VIS,DT2T,DTMINI,DRAD
      my_real
     .   HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), STIF(MVSIZ),
     .   MS1(MVSIZ),MS2(MVSIZ),MM1(MVSIZ),MM2(MVSIZ),
     .   VXS1(MVSIZ),VYS1(MVSIZ),VZS1(MVSIZ),VXS2(MVSIZ),VYS2(MVSIZ),
     .   VZS2(MVSIZ),VXM1(MVSIZ),VYM1(MVSIZ),VZM1(MVSIZ),VXM2(MVSIZ),
     .   VYM2(MVSIZ),VZM2(MVSIZ),CAND_FX(*),CAND_FY(*),
     .   CAND_FZ(*),FNI(*),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),PENRAD(MVSIZ),
     .   FSAVPARIT(NISUB+1,11,*),FSAVSUB(NTHVKI,*),FRICC(MVSIZ),
     .   VISCFFRIC(MVSIZ)
      my_real  , INTENT(IN) :: DGAPLOADINT(S_LOADPINTER)
      INTEGER BITGET
      EXTERNAL BITGET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J , K0,NBINTER,K1S,K, NI
      INTEGER NISKYL,NISKYL1,IDTM,IM,IS,JSUB,KSUB,JJ,KK,NSUB,PP,PPL,
     .   ITYPSUB,ISS1,ISS2,IMS1,IMS2
      my_real
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), 

     .   PENE(MVSIZ),MASMIN(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ),
     .   VNX, VNY, VNZ, AA, VMAX,S2,DIST,RDIST,
     .   V2, FM2, DT1INV, VISCA,  FAC,  FF,
     .   FX, FY, FZ, F2, MAS2, DTI,
     .   FACM1, ECONTT, ECONVT, A2,MASM,ECONTDT,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6,
     .   FSAV8, FSAV9, FSAV10, FSAV11, FSAV12,
     .   FSAV13, FSAV14, FSAV15, DTI2, PPLUS,DTMI0
      my_real PREC,BETA,DGAPLOAD,GAPP
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   CX,CY,CFI,AUX,DTM,FT,FN,FTN,FXT(MVSIZ),FYT(MVSIZ),
     .   FZT(MVSIZ)
C-----------------------------------------------
#include "sync_dir.inc"
      IF (IRESP .EQ. 1) THEN
         PREC = FIVEEM4
      ELSE 
         PREC = EM10
      ENDIF
      IF(DT1.GT.ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONVT = ZERO     
      ECONTDT = ZERO     
C
      IF(INTTH/=0.OR.NINLOADP/=0 )THEN
        DO I=1,JLT
C        RADIATION DISTANCE
         DIST = SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2)
         PENRAD(I)=DIST-GAPV(I)
        ENDDO
      ENDIF
C
      DO I=1,JLT
         S2 = SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2)
         PENE(I) = MAX(ZERO,GAPV(I) - S2)
         S2 = ONE/MAX(EM30,S2)
         NX(I) = NX(I)*S2
         NY(I) = NY(I)*S2
         NZ(I) = NZ(I)*S2
      ENDDO
C
      IF(INACTI.EQ.5)THEN
#include "lockon.inc"
        DO I=1,JLT
         IF(CS_LOC(I).LE.NRTS) THEN
           PENIS(2,CS_LOC(I)) = MAX(PENIS(2,CS_LOC(I)),HALF*PENE(I))
         ELSE
           NI = CS_LOC(I)-NRTS
           PENFI(NIN)%P(2,NI) = MAX(PENFI(NIN)%P(2,NI),HALF*PENE(I))
         END IF
         PENIM(2,CM_LOC(I)) = MAX(PENIM(2,CM_LOC(I)),HALF*PENE(I))
        ENDDO
#include "lockoff.inc"
        DO I=1,JLT
         IF(CS_LOC(I).LE.NRTS) THEN
           PENE(I) = PENE(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           IF(PENE(I).EQ.ZERO)STIF(I)=ZERO
           GAPV(I) = GAPV(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
         ELSE
           NI = CS_LOC(I)-NRTS
           PENE(I) = PENE(I) - PENFI(NIN)%P(1,NI) - PENIM(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           IF(PENE(I).EQ.ZERO)STIF(I)=ZERO
           GAPV(I) = GAPV(I) - PENFI(NIN)%P(1,NI) - PENIM(1,CM_LOC(I))
         END IF
        END DO
      ELSE IF(INACTI.EQ.6)THEN
#include "lockon.inc"
        DO I=1,JLT
         PPLUS=HALF*(PENE(I)+FIVEEM2*(GAPV(I)-PENE(I)))
         IF(CS_LOC(I).LE.NRTS) THEN
           PENIS(2,CS_LOC(I)) = MAX(PENIS(2,CS_LOC(I)),PPLUS)
         ELSE
           NI = CS_LOC(I)-NRTS
           PENFI(NIN)%P(2,NI) = MAX(PENFI(NIN)%P(2,NI),PPLUS)
         END IF
         PENIM(2,CM_LOC(I)) = MAX(PENIM(2,CM_LOC(I)),PPLUS)
        ENDDO
#include "lockoff.inc"
        DO I=1,JLT
         IF(CS_LOC(I).LE.NRTS) THEN
           PENE(I) = PENE(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           IF(PENE(I).EQ.ZERO)STIF(I)=ZERO
           GAPV(I) = GAPV(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
         ELSE
           NI = CS_LOC(I)-NRTS
           PENE(I) = PENE(I) - PENFI(NIN)%P(1,NI) - PENIM(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           IF(PENE(I).EQ.ZERO)STIF(I)=ZERO
           GAPV(I) = GAPV(I) - PENFI(NIN)%P(1,NI) - PENIM(1,CM_LOC(I))
         END IF
        END DO
      ELSE
        DO I=1,JLT
          IF( PENE(I).EQ.ZERO )  STIF(I) = ZERO
        ENDDO
      ENDIF

      VMAX = ZERO
      DO I=1,JLT
        GAPV(I) = ZEP9*GAPV(I)
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
       FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
       FACM1 = ONE/FAC
       IF(( (GAPV(I)-PENE(I))/GAPV(I) ).LT.PREC .AND. 
     .                                STIF(I).GT.ZERO ) THEN
         STIF(I) = ZERO
        IF (IMPL_S.EQ.0) THEN 
          NEWFRONT = -1
#include "lockon.inc"
         IF(CS_LOC(I).LE.NRTS)THEN
           STFS(CS_LOC(I)) = -ABS(STFS(CS_LOC(I)))
           WRITE(ISTDO,*)'WARNING INTERFACE NB',NOINT
           WRITE(ISTDO,*)'LINE ',ITAB(N1(I)),
     .               ITAB(N2(I)),'DE-ACTIVATED FROM','INTERFACE'                 
           WRITE(IOUT,*)'WARNING INTERFACE NB',NOINT
           WRITE(IOUT,*)'GAP=',GAPV(I),'PENE=',PENE(I)
           WRITE(IOUT,*)'LINE ',ITAB(N1(I)),
     .               ITAB(N2(I)),'DE-ACTIVATED FROM','INTERFACE'
         ELSE
           NI = CS_LOC(I)-NRTS
           STIFI(NIN)%P(NI) = -ABS(STIFI(NIN)%P(NI))
           WRITE(ISTDO,*)'WARNING INTERFACE NB',NOINT
           WRITE(ISTDO,*)'LINE ',ITAFI(NIN)%P(N1(I)),
     .           ITAFI(NIN)%P(N2(I)),'DE-ACTIVATED FROM','INTERFACE'                 
           WRITE(IOUT,*)'WARNING INTERFACE NB',NOINT
           WRITE(IOUT,*)'GAP=',GAPV(I),'PENE=',PENE(I)
           WRITE(IOUT,*)'LINE ',ITAFI(NIN)%P(N1(I)),
     .           ITAFI(NIN)%P(N2(I)),'DE-ACTIVATED FROM','INTERFACE'
         END IF
#include "lockoff.inc"
        ENDIF  
         PENE(I)= ZERO
       ENDIF
       ECONTT = ECONTT + HALF*STIF(I)*GAPV(I)**2 *( FACM1 - ONE - 
     .        LOG(FACM1) )
       STIF(I) = HALF*STIF(I) * FAC
       FNI(I)= -STIF(I) * PENE(I) 
      ENDDO

      DTI = EP20
C
      DO I=1,JLT
          DIST=GAPV(I)-PENE(I)
          RDIST  = HALF*DIST / MAX(EM30,-VN(I))
          DTI = MIN(RDIST,DTI)
      ENDDO
      
C Mix with global settings
C      IF (IDTMIN(10).EQ.0) THEN 
C        IDTM=2
C      ELSE
C        IDTM=IDTMIN(10)
C      END IF
C      IF (DTMINI.GT.ZERO) THEN 
C        DTM=DTMINI
C      ELSE 
C        DTM=DTMIN1(10)
C      END IF

C Force to DEL
      IF (DTMINI.GT.ZERO) THEN 
        DTM=DTMINI
        IDTM=2
      ELSE 
        DTM=DTMIN1(10)
        IDTM=IDTMIN(10)
      END IF
C
      IF(DTI.LE.DTM)THEN
        DO I=1,JLT
            DIST=GAPV(I)-PENE(I)
            DTI2   = HALF*DIST / MAX(EM30,-VN(I))
            IF(DTI2.LE.DTM)THEN
#include "lockon.inc"
             IF(CS_LOC(I).LE.NRTS)THEN
               WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .            ' **WARNING MINIMUM TIME STEP ',DTI2,
     .            'IN INTERFACE NB',NOINT,'(DTMIN=',DTM,')'
               WRITE(IOUT,*)'SECONDARY NODES NB',ITAB(N1(I)),
     .             ITAB(N2(I))               
               WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I)) 
             ELSE
               WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .            ' **WARNING MINIMUM TIME STEP ',DTI2,
     .            'IN INTERFACE NB',NOINT,'(DTMIN=',DTM,')'
               WRITE(IOUT,*)'SECONDARY NODES NB',ITAFI(NIN)%P(N1(I)),
     .             ITAFI(NIN)%P(N2(I))               
               WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
             END IF
#include "lockoff.inc"
             IF(IDTM.EQ.1)THEN
               TSTOP = TT
             ELSEIF(IDTM.EQ.2)THEN
#include "lockon.inc"
               WRITE(IOUT,*)'REMOVE SECONDARY LINE FROM INTERFACE'                 
               IF(CS_LOC(I).LE.NRTS)THEN
                 STFS(CS_LOC(I)) = -ABS(STFS(CS_LOC(I)))
               ELSE
                 NI = CS_LOC(I)-NRTS
                 STIFI(NIN)%P(NI) = -ABS(STIFI(NIN)%P(NI))
               END IF
#include "lockoff.inc"
               NEWFRONT = -1
               STIF(I) = ZERO
               DTI = DTM
             ELSEIF(IDTM.EQ.5)THEN
               MSTOP = 2
             ENDIF                
            ENDIF               
        ENDDO
      ENDIF
C
      IF(DTI.LT.DT2T)THEN
        DT2T    = DTI
        NELTST  = NOINT
        ITYPTST = 10
      ENDIF
C---------------------------------
C     DAMPING + FRIC
C---------------------------------
      IF(VISC.NE.ZERO)THEN
          DO I=1,JLT
           MAS2  = MS1(I)*HS1(I)
     .           + MS2(I)*HS2(I)
           MASM  = MM1(I)*HM1(I)
     .           + MM2(I)*HM2(I)
           MASMIN(I) = MIN(MAS2,MASM)
           VIS2(I) = TWO * STIF(I) * MIN(MAS2,MASM)
          ENDDO
      ELSE
          DO I=1,JLT
           IF(VISCFFRIC(I).NE.ZERO) THEN
             MAS2  = MS1(I)*HS1(I)
     .           + MS2(I)*HS2(I)
             MASM  = MM1(I)*HM1(I)
     .           + MM2(I)*HM2(I)
             MASMIN(I) = MIN(MAS2,MASM)
             VIS2(I) = TWO * STIF(I) * MIN(MAS2,MASM)
           ENDIF
          ENDDO       
      ENDIF

C---------------------------------
      IF(VISC.NE.ZERO)THEN
        IF(IVIS2.EQ.0.OR.IVIS2.EQ.1)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          DO I=1,JLT
            IF(VN(I).LT.ZERO)
     .      VIS2(I) = VIS2(I)/(MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)))
          ENDDO
C---------------------------------
          VISCA = ZEP4
          IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
           DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF  = FAC * (
     .        VISC * VIS +
     .        VISCA**2 * TWO * MASMIN(I) * MAX(ZERO,-VN(I)) / 
     .                MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I)/MAX((GAPV(I)-PENE(I)),EM10) 
            STIF(I) = STIF(I) + FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            FF = MIN(FF * VN(I),-FNI(I))  
c           FF = MIN(FF * VN(I),ZERO)  
            FNI(I)  = FNI(I) + FF
cc          ECONVT = ECONVT + FF * VN(I) * DT1
           ENDDO

          ELSE
           DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            C(I)= FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO * MASMIN(I) * MAX(ZERO,-VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10)
            KT(I)   = STIF(I)
            STIF(I) = STIF(I) + C(I) * DT1INV
            FF = MIN(C(I) * VN(I),-FNI(I))  
c           FF = MIN(FF * VN(I),ZERO)  
            FNI(I)  = FNI(I) + FF
            CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
            STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
cc          ECONVT = ECONVT + C(I) * VN(I) * DT1
           ENDDO
          ENDIF

        ELSEIF(IVIS2.EQ.2)THEN
C---------------------------------
C         VISC QUAD TYPE V23f(bug aero amerrissage)
C---------------------------------
          DO I=1,JLT
            VIS2(I) = VIS2(I)/( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)))
          ENDDO
C---------------------------------
          VISCA = HALF
          DO I=1,JLT
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF  = FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO * MASMIN(I) * ABS(VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I)-PENE(I)),EM10) 
           STIF(I) = STIF(I) + TWO * FF * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
           FF = MIN(FF * VN(I),-FNI(I))  
           FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2.EQ.3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
          DO I=1,JLT
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF  = FAC * ( VISC * VIS ) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I)-PENE(I)),EM10) 
           STIF(I) = STIF(I) + TWO * FF * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
           FF = MIN(FF * VN(I),-FNI(I))  
           FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2.EQ.4)THEN
C---------------------------------
C         VISC = 0 
C---------------------------------
          DO I=1,JLT
           VIS = SQRT(VIS2(I)) 
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I)-PENE(I)),EM10) 
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
          ENDDO
        ELSEIF(IVIS2.EQ.5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
          DO I=1,JLT
           MAS2  = MS1(I)*HS1(I)
     .           + MS2(I)*HS2(I)
           MASM  = MM1(I)*HM1(I)
     .           + MM2(I)*HM2(I)
           VIS = 2. * VISC * DT1INV * MASM * MAS2 / 
     .          MAX(EM30,MASM+MAS2)
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) -PENE(I)),EM10) 
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I)*VIS2(I))*DT1INV)
           FF = VIS * VN(I)  
           ECONTDT = ECONTDT + MIN(ZERO,FF-FNI(I)) * VN(I) * DT1
           FNI(I)  = MIN(FNI(I),FF)
          ENDDO
        ELSE
        ENDIF
      ELSE
         DO I=1,JLT
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
         ENDDO
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
      DO I=1,JLT
       FXI(I)=NX(I)*FNI(I)
       FYI(I)=NY(I)*FNI(I)
       FZI(I)=NZ(I)*FNI(I)
       FSAV1=FSAV1+FXI(I)*DT12
       FSAV2=FSAV2+FYI(I)*DT12
       FSAV3=FSAV3+FZI(I)*DT12
       FSAV8=FSAV8+ABS(FXI(I)*DT12)
       FSAV9=FSAV9+ABS(FYI(I)*DT12)
       FSAV10=FSAV10+ABS(FZI(I)*DT12)
       FSAV11=FSAV11+ABS(FNI(I))*DT12
      ENDDO
      IF (INCONV.EQ.1) THEN 
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
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,1,I+NFT) =  FXI(I)
          FSAVPARIT(1,2,I+NFT) =  FYI(I)
          FSAVPARIT(1,3,I+NFT) =  FZI(I)
        ENDDO
      ENDIF
C
C     impulsion normale pour les sous-interfaces
C
      IF (NISUB > 0) THEN
C
      DO I=1,JLT
        IM=CM_LOC(I)
        KK  =ADDSUBM(IM)
        IF (CS_LOC(I).LE.NRTS) THEN
C--     SECONDARY line on the proc
           IS=CS_LOC(I)
           JJ  =ADDSUBS(IS)
           DO WHILE(JJ.LT.ADDSUBS(IS+1))
             JSUB=LISUBS(JJ)
             ITYPSUB = TYPSUB(JSUB)

             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter   

               KSUB=LISUBM(KK)   

               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IM+1)))
C
                 IF(KSUB.EQ.JSUB)THEN
C
                   FSAV1=FXI(I)*DT12
                   FSAV2=FYI(I)*DT12
                   FSAV3=FZI(I)*DT12
                   FSAV8=ABS(FXI(I)*DT12)
                   FSAV9=ABS(FYI(I)*DT12)
                   FSAV10=ABS(FZI(I)*DT12)
                   FSAV11=ABS(FNI(I))*DT12
C
                   NSUB=LISUB(JSUB)
                   FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)+FSAV1
                   FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)+FSAV2
                   FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)+FSAV3
                   FSAVSUB(8,NSUB)=FSAVSUB(8,NSUB)+FSAV8
                   FSAVSUB(9,NSUB)=FSAVSUB(9,NSUB)+FSAV9
                   FSAVSUB(10,NSUB)=FSAVSUB(10,NSUB)+FSAV10
                   FSAVSUB(11,NSUB)=FSAVSUB(11,NSUB)+FSAV11
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                     FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                     FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                     FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                   ENDIF
C
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : secondary side
C
                 FSAV1=FXI(I)*DT12
                 FSAV2=FYI(I)*DT12
                 FSAV3=FZI(I)*DT12
                 FSAV8=ABS(FXI(I)*DT12)
                 FSAV9=ABS(FYI(I)*DT12)
                 FSAV10=ABS(FZI(I)*DT12)
                 FSAV11=ABS(FNI(I))*DT12
C
                 NSUB=LISUB(JSUB)
                 FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)+FSAV1
                 FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)+FSAV2
                 FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)+FSAV3
                 FSAVSUB(8,NSUB)=FSAVSUB(8,NSUB)+FSAV8
                 FSAVSUB(9,NSUB)=FSAVSUB(9,NSUB)+FSAV9
                 FSAVSUB(10,NSUB)=FSAVSUB(10,NSUB)+FSAV10
                 FSAVSUB(11,NSUB)=FSAVSUB(11,NSUB)+FSAV11
C
                 IF(ISENSINT(JSUB+1)/=0) THEN
                   FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                   FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                   FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                 ENDIF
C

                JJ=JJ+1
             ELSEIF(ITYPSUB == 3 ) THEN  ! Inter =0 : collecting forces from all inter with 2 surfacec 

                ISS2 = BITGET(INFLG_SUBS(JJ),0)
                ISS1 = BITGET(INFLG_SUBS(JJ),1)
                KSUB=LISUBM(KK)  
                DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IM+1))) 
                   IMS2 = BITGET(INFLG_SUBM(KK),0)
                   IMS1 = BITGET(INFLG_SUBM(KK),1)
                   IF(KSUB.EQ.JSUB)THEN
                     IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                         (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBM(KK) 
                        CYCLE             
                     END IF
C
                     FSAV1=FXI(I)*DT12
                     FSAV2=FYI(I)*DT12
                     FSAV3=FZI(I)*DT12
                     FSAV8=ABS(FXI(I)*DT12)
                     FSAV9=ABS(FYI(I)*DT12)
                     FSAV10=ABS(FZI(I)*DT12)
                     FSAV11=ABS(FNI(I))*DT12
C
                     NSUB=LISUB(JSUB)
                     IF(IMS2 > 0)THEN
                        FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)-FSAV1
                        FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)-FSAV2
                        FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)-FSAV3

                     ELSE
                        FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)+FSAV1
                        FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)+FSAV2
                        FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)+FSAV3
                     ENDIF
                     FSAVSUB(8,NSUB)=FSAVSUB(8,NSUB)+FSAV8
                     FSAVSUB(9,NSUB)=FSAVSUB(9,NSUB)+FSAV9
                     FSAVSUB(10,NSUB)=FSAVSUB(10,NSUB)+FSAV10
                     FSAVSUB(11,NSUB)=FSAVSUB(11,NSUB)+FSAV11
C
                     IF(ISENSINT(JSUB+1)/=0) THEN
                       IF(IMS2 > 0)THEN
                         FSAVPARIT(JSUB+1,1,I+NFT) =  -FXI(I)
                         FSAVPARIT(JSUB+1,2,I+NFT) =  -FYI(I)
                         FSAVPARIT(JSUB+1,3,I+NFT) =  -FZI(I)
                       ELSE
                         FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                         FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                         FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                       ENDIF
                     ENDIF
C
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ENDIF

           ENDDO

           DO WHILE(KK.LT.ADDSUBM(IM+1))
               KSUB=LISUBM(KK)

               ITYPSUB = TYPSUB(KSUB)
               IF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : main side
C
                 FSAV1=FXI(I)*DT12
                 FSAV2=FYI(I)*DT12
                 FSAV3=FZI(I)*DT12
                 FSAV8=ABS(FXI(I)*DT12)
                 FSAV9=ABS(FYI(I)*DT12)
                 FSAV10=ABS(FZI(I)*DT12)
                 FSAV11=ABS(FNI(I))*DT12
C
                 NSUB=LISUB(KSUB)
                 FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)-FSAV1
                 FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)-FSAV2
                 FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)-FSAV3
                 FSAVSUB(8,NSUB)=FSAVSUB(8,NSUB)+FSAV8
                 FSAVSUB(9,NSUB)=FSAVSUB(9,NSUB)+FSAV9
                 FSAVSUB(10,NSUB)=FSAVSUB(10,NSUB)+FSAV10
                 FSAVSUB(11,NSUB)=FSAVSUB(11,NSUB)+FSAV11
C
                 IF(ISENSINT(JSUB+1)/=0) THEN
                   FSAVPARIT(JSUB+1,1,I+NFT) =  -FXI(I)
                   FSAVPARIT(JSUB+1,2,I+NFT) =  -FYI(I)
                   FSAVPARIT(JSUB+1,3,I+NFT) =  -FZI(I)
                 ENDIF
C

               ENDIF
               KK=KK+1
            ENDDO



        ELSE
C--     Remote SECONDARY line
           IS=CS_LOC(I)-NRTS
           JJ  =ADDSUBSFI(NIN)%P(IS)
           DO WHILE(JJ.LT.ADDSUBSFI(NIN)%P(IS+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             ITYPSUB = TYPSUB(JSUB)

             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter  

               KSUB=LISUBM(KK)   
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IM+1)))
C
                 IF(KSUB.EQ.JSUB)THEN
C
                   FSAV1=FXI(I)*DT12
                   FSAV2=FYI(I)*DT12
                   FSAV3=FZI(I)*DT12
                   FSAV8=ABS(FXI(I)*DT12)
                   FSAV9=ABS(FYI(I)*DT12)
                   FSAV10=ABS(FZI(I)*DT12)
                   FSAV11=ABS(FNI(I))*DT12
C
                   NSUB=LISUB(JSUB)
                   FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)+FSAV1
                   FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)+FSAV2
                   FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)+FSAV3
                   FSAVSUB(8,NSUB)=FSAVSUB(8,NSUB)+FSAV8
                   FSAVSUB(9,NSUB)=FSAVSUB(9,NSUB)+FSAV9
                   FSAVSUB(10,NSUB)=FSAVSUB(10,NSUB)+FSAV10
                   FSAVSUB(11,NSUB)=FSAVSUB(11,NSUB)+FSAV11
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                     FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                     FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                     FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                   ENDIF
C
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface
C
                 FSAV1=FXI(I)*DT12
                 FSAV2=FYI(I)*DT12
                 FSAV3=FZI(I)*DT12
                 FSAV8=ABS(FXI(I)*DT12)
                 FSAV9=ABS(FYI(I)*DT12)
                 FSAV10=ABS(FZI(I)*DT12)
                 FSAV11=ABS(FNI(I))*DT12
C
                 NSUB=LISUB(JSUB)
                 FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)+FSAV1
                 FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)+FSAV2
                 FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)+FSAV3
                 FSAVSUB(8,NSUB)=FSAVSUB(8,NSUB)+FSAV8
                 FSAVSUB(9,NSUB)=FSAVSUB(9,NSUB)+FSAV9
                 FSAVSUB(10,NSUB)=FSAVSUB(10,NSUB)+FSAV10
                 FSAVSUB(11,NSUB)=FSAVSUB(11,NSUB)+FSAV11
C
                 IF(ISENSINT(JSUB+1)/=0) THEN
                   FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                   FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                   FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                 ENDIF
C

                JJ=JJ+1

             ELSEIF(ITYPSUB == 3 ) THEN  ! Inter =0 : collecting forces from all inter with 2 surfacec 

                ISS2 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),0)
                ISS1 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),1)
                KSUB=LISUBM(KK)  
                DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IM+1))) 
                   IMS2 = BITGET(INFLG_SUBM(KK),0)
                   IMS1 = BITGET(INFLG_SUBM(KK),1)
                   IF(KSUB.EQ.JSUB)THEN
                     IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                         (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBM(KK) 
                        CYCLE             
                     END IF
C
                     FSAV1=FXI(I)*DT12
                     FSAV2=FYI(I)*DT12
                     FSAV3=FZI(I)*DT12
                     FSAV8=ABS(FXI(I)*DT12)
                     FSAV9=ABS(FYI(I)*DT12)
                     FSAV10=ABS(FZI(I)*DT12)
                     FSAV11=ABS(FNI(I))*DT12
C
                     NSUB=LISUB(JSUB)
                     IF(IMS2 > 0)THEN
                        FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)-FSAV1
                        FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)-FSAV2
                        FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)-FSAV3

                     ELSE
                        FSAVSUB(1,NSUB)=FSAVSUB(1,NSUB)+FSAV1
                        FSAVSUB(2,NSUB)=FSAVSUB(2,NSUB)+FSAV2
                        FSAVSUB(3,NSUB)=FSAVSUB(3,NSUB)+FSAV3
                     ENDIF
                     FSAVSUB(8,NSUB)=FSAVSUB(8,NSUB)+FSAV8
                     FSAVSUB(9,NSUB)=FSAVSUB(9,NSUB)+FSAV9
                     FSAVSUB(10,NSUB)=FSAVSUB(10,NSUB)+FSAV10
                     FSAVSUB(11,NSUB)=FSAVSUB(11,NSUB)+FSAV11
C
                     IF(ISENSINT(JSUB+1)/=0) THEN
                       IF(IMS2 > 0)THEN
                         FSAVPARIT(JSUB+1,1,I+NFT) =  -FXI(I)
                         FSAVPARIT(JSUB+1,2,I+NFT) =  -FYI(I)
                         FSAVPARIT(JSUB+1,3,I+NFT) =  -FZI(I)
                       ELSE
                         FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                         FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                         FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                       ENDIF
                     ENDIF
C
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ENDIF

          ENDDO
        ENDIF

       ENDDO
C
      ENDIF

C------------For /LOAD/PRESSURE tag nodes in contact-------------
      IF(NINLOADP > 0) THEN
         DO K = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1) 
            PP = LOADPINTER(K)
            PPL = LOADP_HYD_INTER(PP)
            DGAPLOAD = DGAPLOADINT(K)
            DO I=1,JLT
               DIST = PENRAD(I) + GAPV(I)
               GAPP= GAPV(I) + DGAPLOAD
               IF(PENE(I) > ZERO .OR.DIST <= GAPP) THEN
                 TAGNCONT(PPL,M1(I)) = 1
                 TAGNCONT(PPL,M2(I)) = 1
                 IF(CS_LOC(I).LE.NRTS) THEN
C  SPMD : do same after reception of forces for remote nodes 
                   TAGNCONT(PPL,N1(I)) = 1
                   TAGNCONT(PPL,N2(I)) = 1
                 ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDIF
C
C---------------------------------
C     FRICTION
C---------------------------------
      IF(IFORM==1)THEN
        FSAV4 = ZERO
        FSAV5 = ZERO
        FSAV6 = ZERO
        FSAV12 = ZERO
        FSAV13 = ZERO
        FSAV14 = ZERO
        FSAV15 = ZERO
        DO I=1,JLT
         IF(FRICC(I)*VISCFFRIC(I).NE.0.)THEN
          VNX = NX(I)*VN(I)
          VNY = NY(I)*VN(I)
          VNZ = NZ(I)*VN(I)
          VX(I) = VX(I) - VNX
          VY(I) = VY(I) - VNY
          VZ(I) = VZ(I) - VNZ
          V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
          VIS2(I) = VISCFFRIC(I) * VIS2(I)
          FM2  = (FRICC(I)*FNI(I))**2
          F2   = VIS2(I) * V2
          A2 = MIN(F2,FM2) / MAX(EM30,F2)
          AA = SQRT(A2 * VIS2(I))
          FXT(I) = AA * VX(I)
          FYT(I) = AA * VY(I)
          FZT(I) = AA * VZ(I)
          FSAV4 = FSAV4 + FXT(I)*DT12
          FSAV5 = FSAV5 + FYT(I)*DT12
          FSAV6 = FSAV6 + FZT(I)*DT12
          FXI(I)=FXI(I) + FXT(I)
          FYI(I)=FYI(I) + FYT(I)
          FZI(I)=FZI(I) + FZT(I)
          FSAV12 = FSAV12 + ABS(FXI(I)*DT12)
          FSAV13 = FSAV13 + ABS(FYI(I)*DT12)
          FSAV14 = FSAV14 + ABS(FZI(I)*DT12)
          FSAV15 = FSAV15 + SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
          ECONVT = ECONVT + AA * V2 * DT1
         ENDIF
        ENDDO
        IF (INCONV.EQ.1) THEN
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
      ELSEIF(IFORM==2)THEN
C---------------------------------
C       INCREMENTAL (STIFFNESS) FORMULATION
C---------------------------------
        FSAV4 = ZERO
        FSAV5 = ZERO
        FSAV6 = ZERO
        FSAV12 = ZERO
        FSAV13 = ZERO
        FSAV14 = ZERO
        FSAV15 = ZERO
        DO I=1,JLT
          FX = STIF(I)*VX(I)*DT12
          FY = STIF(I)*VY(I)*DT12
          FZ = STIF(I)*VZ(I)*DT12
          FX = CAND_FX(INDEX(I)) + FX
          FY = CAND_FY(INDEX(I)) + FY
          FZ = CAND_FZ(INDEX(I)) + FZ
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
          FSAV4 = FSAV4 + FXT(I)*DT12
          FSAV5 = FSAV5 + FYT(I)*DT12
          FSAV6 = FSAV6 + FZT(I)*DT12
          CAND_FX(INDEX(I)) = FXT(I)
          CAND_FY(INDEX(I)) = FYT(I)
          CAND_FZ(INDEX(I)) = FZT(I)
          IFPEN(INDEX(I)) = 1
          FXI(I)=FXI(I) + FXT(I)
          FYI(I)=FYI(I) + FYT(I)
          FZI(I)=FZI(I) + FZT(I)
          FSAV12 = FSAV12 + ABS(FXI(I)*DT12)
          FSAV13 = FSAV13 + ABS(FYI(I)*DT12)
          FSAV14 = FSAV14 + ABS(FZI(I)*DT12)
          FSAV15 = FSAV15 + SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
          ECONVT = ECONVT 
     .           + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I)) 
        ENDDO
        IF (INCONV.EQ.1) THEN
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

      ENDIF
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,4,I+NFT) =  FXT(I)
          FSAVPARIT(1,5,I+NFT) =  FYT(I)
          FSAVPARIT(1,6,I+NFT) =  FZT(I)
        ENDDO
      ENDIF
C
C
C     impulsion tangentielle dans les sous-interfaces
C
      IF (NISUB > 0) THEN
C
      DO I=1,JLT
        IM=CM_LOC(I)
        KK  =ADDSUBM(IM)
        IF (CS_LOC(I).LE.NRTS) THEN
C--     SECONDARY line on the proc
           IS=CS_LOC(I)
           JJ  =ADDSUBS(IS)

           DO WHILE(JJ.LT.ADDSUBS(IS+1))
             JSUB=LISUBS(JJ)
             ITYPSUB = TYPSUB(JSUB)
             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter   

               KSUB=LISUBM(KK)   
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IM+1)))
                 IF(KSUB.EQ.JSUB)THEN
C
                   FSAV4=FXT(I)*DT12
                   FSAV5=FYT(I)*DT12
                   FSAV6=FZT(I)*DT12
                   FSAV12 = ABS(FXI(I)*DT12)
                   FSAV13 = ABS(FYI(I)*DT12)
                   FSAV14 = ABS(FZI(I)*DT12)
                   FSAV15 = SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
C
                   NSUB=LISUB(JSUB)
                   FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)+FSAV4
                   FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)+FSAV5
                   FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)+FSAV6
                   FSAVSUB(12,NSUB)=FSAVSUB(12,NSUB)+FSAV12
                   FSAVSUB(13,NSUB)=FSAVSUB(13,NSUB)+FSAV13
                   FSAVSUB(14,NSUB)=FSAVSUB(14,NSUB)+FSAV14
                   FSAVSUB(15,NSUB)=FSAVSUB(15,NSUB)+FSAV15
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                     FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                     FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                     FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                   ENDIF
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface
C
               FSAV4=FXT(I)*DT12
               FSAV5=FYT(I)*DT12
               FSAV6=FZT(I)*DT12
               FSAV12 = ABS(FXI(I)*DT12)
               FSAV13 = ABS(FYI(I)*DT12)
               FSAV14 = ABS(FZI(I)*DT12)
               FSAV15 = SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
C
               NSUB=LISUB(JSUB)
               FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)+FSAV4
               FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)+FSAV5
               FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)+FSAV6
               FSAVSUB(12,NSUB)=FSAVSUB(12,NSUB)+FSAV12
               FSAVSUB(13,NSUB)=FSAVSUB(13,NSUB)+FSAV13
               FSAVSUB(14,NSUB)=FSAVSUB(14,NSUB)+FSAV14
               FSAVSUB(15,NSUB)=FSAVSUB(15,NSUB)+FSAV15
C
               IF(ISENSINT(JSUB+1)/=0) THEN
                 FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                 FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                 FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
               ENDIF
C
               JJ = JJ + 1
             ELSEIF(ITYPSUB == 3) THEN  

               ISS2 = BITGET(INFLG_SUBS(JJ),0)
               ISS1 = BITGET(INFLG_SUBS(JJ),1)
               KSUB=LISUBM(KK)  
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IM+1))) 
                  IMS2 = BITGET(INFLG_SUBM(KK),0)
                  IMS1 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB.EQ.JSUB)THEN
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                        (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                       KK=KK+1
                       KSUB=LISUBM(KK) 
                       CYCLE             
                    END IF
C
                   FSAV4=FXT(I)*DT12
                   FSAV5=FYT(I)*DT12
                   FSAV6=FZT(I)*DT12
                   FSAV12 = ABS(FXI(I)*DT12)
                   FSAV13 = ABS(FYI(I)*DT12)
                   FSAV14 = ABS(FZI(I)*DT12)
                   FSAV15 = SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
C
                   NSUB=LISUB(JSUB)
                   IF(IMS2 > 0 ) THEN 
                     FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)-FSAV4
                     FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)-FSAV5
                     FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)-FSAV6
                   ELSE
                     FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)+FSAV4
                     FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)+FSAV5
                     FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)+FSAV6
                   ENDIF
                   FSAVSUB(12,NSUB)=FSAVSUB(12,NSUB)+FSAV12
                   FSAVSUB(13,NSUB)=FSAVSUB(13,NSUB)+FSAV13
                   FSAVSUB(14,NSUB)=FSAVSUB(14,NSUB)+FSAV14
                   FSAVSUB(15,NSUB)=FSAVSUB(15,NSUB)+FSAV15
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                     IF(IMS2 > 0 ) THEN 
                        FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                        FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                        FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                     ELSE
                        FSAVPARIT(JSUB+1,4,I+NFT) =  -FXT(I)
                        FSAVPARIT(JSUB+1,5,I+NFT) =  -FYT(I)
                        FSAVPARIT(JSUB+1,6,I+NFT) =  -FZT(I)
                     ENDIF
                   ENDIF
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ENDIF
           ENDDO

           DO WHILE(KK.LT.ADDSUBM(IM+1))
               KSUB=LISUBM(KK)

               ITYPSUB = TYPSUB(KSUB)
               IF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : main side
C
                 FSAV4=-FXT(I)*DT12
                 FSAV5=-FYT(I)*DT12
                 FSAV6=-FZT(I)*DT12
                 FSAV12 = ABS(FXI(I)*DT12)
                 FSAV13 = ABS(FYI(I)*DT12)
                 FSAV14 = ABS(FZI(I)*DT12)
                 FSAV15 = SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
C
                 NSUB=LISUB(JSUB)
                 FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)+FSAV4
                 FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)+FSAV5
                 FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)+FSAV6
                 FSAVSUB(12,NSUB)=FSAVSUB(12,NSUB)+FSAV12
                 FSAVSUB(13,NSUB)=FSAVSUB(13,NSUB)+FSAV13
                 FSAVSUB(14,NSUB)=FSAVSUB(14,NSUB)+FSAV14
                 FSAVSUB(15,NSUB)=FSAVSUB(15,NSUB)+FSAV15
C
                 IF(ISENSINT(JSUB+1)/=0) THEN
                   FSAVPARIT(JSUB+1,4,I+NFT) =  -FXT(I)
                   FSAVPARIT(JSUB+1,5,I+NFT) =  -FYT(I)
                   FSAVPARIT(JSUB+1,6,I+NFT) =  -FZT(I)
                 ENDIF
C
                 JJ = JJ + 1

               ENDIF
               KK=KK+1
            ENDDO
         ELSE
C--     Remote SECONDARY line
           IS=CS_LOC(I)-NRTS
           JJ  =ADDSUBSFI(NIN)%P(IS)
           DO WHILE(JJ.LT.ADDSUBSFI(NIN)%P(IS+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             ITYPSUB = TYPSUB(JSUB)

             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter   

               KSUB=LISUBM(KK)   
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IM+1))) 
                 IF(KSUB.EQ.JSUB)THEN
C
                   FSAV4=FXT(I)*DT12
                   FSAV5=FYT(I)*DT12
                   FSAV6=FZT(I)*DT12
                   FSAV12 = ABS(FXI(I)*DT12)
                   FSAV13 = ABS(FYI(I)*DT12)
                   FSAV14 = ABS(FZI(I)*DT12)
                   FSAV15 = SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
C
                   NSUB=LISUB(JSUB)
                   FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)+FSAV4
                   FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)+FSAV5
                   FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)+FSAV6
                   FSAVSUB(12,NSUB)=FSAVSUB(12,NSUB)+FSAV12
                   FSAVSUB(13,NSUB)=FSAVSUB(13,NSUB)+FSAV13
                   FSAVSUB(14,NSUB)=FSAVSUB(14,NSUB)+FSAV14
                   FSAVSUB(15,NSUB)=FSAVSUB(15,NSUB)+FSAV15
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                     FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                     FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                     FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                   ENDIF
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surf
C
               FSAV4=FXT(I)*DT12
               FSAV5=FYT(I)*DT12
               FSAV6=FZT(I)*DT12
               FSAV12 = ABS(FXI(I)*DT12)
               FSAV13 = ABS(FYI(I)*DT12)
               FSAV14 = ABS(FZI(I)*DT12)
               FSAV15 = SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
C
               NSUB=LISUB(JSUB)
               FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)+FSAV4
               FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)+FSAV5
               FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)+FSAV6
               FSAVSUB(12,NSUB)=FSAVSUB(12,NSUB)+FSAV12
               FSAVSUB(13,NSUB)=FSAVSUB(13,NSUB)+FSAV13
               FSAVSUB(14,NSUB)=FSAVSUB(14,NSUB)+FSAV14
               FSAVSUB(15,NSUB)=FSAVSUB(15,NSUB)+FSAV15
C
               IF(ISENSINT(JSUB+1)/=0) THEN
                 FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                 FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                 FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
               ENDIF
C
               JJ = JJ + 1

             ELSEIF(ITYPSUB == 3 ) THEN  ! Inter =0 : collecting forces from all inter with 2 Surfs

               ISS2 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),0)
               ISS1 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),1)
               KSUB=LISUBM(KK)  
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IM+1))) 
                  IMS2 = BITGET(INFLG_SUBM(KK),0)
                  IMS1 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB.EQ.JSUB)THEN
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                        (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                       KK=KK+1
                       KSUB=LISUBM(KK) 
                       CYCLE             
                    END IF
C
                   FSAV4=FXT(I)*DT12
                   FSAV5=FYT(I)*DT12
                   FSAV6=FZT(I)*DT12
                   FSAV12 = ABS(FXI(I)*DT12)
                   FSAV13 = ABS(FYI(I)*DT12)
                   FSAV14 = ABS(FZI(I)*DT12)
                   FSAV15 = SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
C
                   NSUB=LISUB(JSUB)
                   IF(IMS2 > 0) THEN
                      FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)-FSAV4
                      FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)-FSAV5
                      FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)-FSAV6
                   ELSE
                      FSAVSUB(4,NSUB)=FSAVSUB(4,NSUB)+FSAV4
                      FSAVSUB(5,NSUB)=FSAVSUB(5,NSUB)+FSAV5
                      FSAVSUB(6,NSUB)=FSAVSUB(6,NSUB)+FSAV6
                   ENDIF
                   FSAVSUB(12,NSUB)=FSAVSUB(12,NSUB)+FSAV12
                   FSAVSUB(13,NSUB)=FSAVSUB(13,NSUB)+FSAV13
                   FSAVSUB(14,NSUB)=FSAVSUB(14,NSUB)+FSAV14
                   FSAVSUB(15,NSUB)=FSAVSUB(15,NSUB)+FSAV15
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                     IF(IMS2 > 0) THEN
                       FSAVPARIT(JSUB+1,4,I+NFT) =  -FXT(I)
                       FSAVPARIT(JSUB+1,5,I+NFT) =  -FYT(I)
                       FSAVPARIT(JSUB+1,6,I+NFT) =  -FZT(I)
                     ELSE
                       FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                       FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                       FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                     ENDIF
                   ENDIF
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ENDIF
           ENDDO
         ENDIF  

       ENDDO
C
       ENDIF
C
       IF (INCONV.EQ.1) THEN 
#include "lockon.inc"
        ECONTV = ECONTV + ECONVT  ! Frictional Energy
        ECONT  = ECONT + ECONTT   ! Elatic Energy
        ECONTD = ECONTD + ECONTDT ! Damping Energy
        FSAV(26) = FSAV(26) + ECONTT
        FSAV(27) = FSAV(27) + ECONVT
        FSAV(28) = FSAV(28) + ECONTDT
#include "lockoff.inc"
       ENDIF 
C---------------------------------
      DO I=1,JLT
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
C
      IF (NSPMD.GT.1) THEN
ctmp+1 mic only
#include "mic_lockon.inc"
        DO I = 1,JLT
          IF(CS_LOC(I).GT.NRTS)THEN
            NI = CS_LOC(I)-NRTS
C tag temporaire de NSVFI a -
            NSVFI(NIN)%P(NI) = -ABS(NSVFI(NIN)%P(NI))
          ENDIF
        ENDDO
ctmp+1 mic only
#include "mic_lockoff.inc"
      ENDIF
C
      DO I=1,JLT
           STIF(I) = TWO*STIF(I)
      ENDDO
C
C---------------------------------
      IF(KDTINT.EQ.1.OR.IDTMINS==2.OR.IDTMINS_INT/=0)THEN
       IF(     (VISC.NE.ZERO)
     .   .AND.(IVIS2.EQ.0.OR.IVIS2.EQ.1))THEN
        DO I=1,JLT
         CX= C(I)*C(I)
C
         IF(MS1(I).EQ.ZERO)THEN
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
          IF(AUX.GT.ST1(I))THEN
           K1(I) =ZERO
           C1(I) =CFI
          ENDIF         
         ENDIF         
C
         IF(MS2(I).EQ.ZERO)THEN
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
          IF(AUX.GT.ST2(I))THEN
           K2(I) =ZERO
           C2(I) =CFI
          ENDIF         
         ENDIF         
C
       IF(MM1(I).EQ.ZERO)THEN
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
          IF(AUX.GT.ST3(I))THEN
           K3(I) =ZERO
           C3(I) =CFI
          ENDIF         
         ENDIF         
C
         IF(MM2(I).EQ.ZERO)THEN
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
          IF(AUX.GT.ST4(I))THEN
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
C
C
      IF(IDTM.EQ.1.OR.IDTM.EQ.2)THEN
       DTMI0 = EP20
       DO I=1,JLT
        DTMI(I) = EP20
        MAS2  = TWO * MASMIN(I)
        IF(MAS2.GT.ZERO.AND.STIF(I).GT.ZERO)THEN
          DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/STIF(I)))
        ENDIF
        DTMI0 = MIN(DTMI0,DTMI(I))
       ENDDO
       IF(DTMI0.LE.DTM)THEN
        DO I=1,JLT
         IF(DTMI(I).LE.DTM)THEN
           IF(IDTM.EQ.1)THEN
#include "lockon.inc"
            IF(CS_LOC(I).LE.NRTS) THEN
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT,'(DTMIN=',DTM,')'
               WRITE(IOUT,*)'SECONDARY NODES NB',ITAB(N1(I)),
     .             ITAB(N2(I))               
               WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))
            ELSE
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT,'(DTMIN=',DTM,')'
               WRITE(IOUT,*)'SECONDARY NODES NB',ITAFI(NIN)%P(N1(I)),
     .             ITAFI(NIN)%P(N2(I))               
               WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))
            END IF
#include "lockoff.inc"
             TSTOP = TT
           ELSEIF(IDTM.EQ.2)THEN
#include "lockon.inc"
            IF(CS_LOC(I).LE.NRTS) THEN
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT,'(DTMIN=',DTM,')'
             WRITE(IOUT,*)'SECONDARY NODES NB',ITAB(N1(I)),
     .             ITAB(N2(I))               
             WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
             WRITE(IOUT,*)'DELETE SECONDARY LINE FROM INTERFACE'                 
             STFS(CS_LOC(I)) = -ABS(STFS(CS_LOC(I)))
            ELSE
             NI = CS_LOC(I)-NRTS
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT,'(DTMIN=',DTM,')'
             WRITE(IOUT,*)'SECONDARY NODES NB',ITAFI(NIN)%P(N1(I)),
     .             ITAFI(NIN)%P(N2(I))               
             WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
             WRITE(IOUT,*)'DELETE SECONDARY LINE FROM INTERFACE'                 
             STIFI(NIN)%P(NI) = -ABS(STIFI(NIN)%P(NI))
            END IF
#include "lockoff.inc"
             NEWFRONT = -1
           ELSEIF(IDTM.EQ.5)THEN
#include "lockon.inc"
            IF(CS_LOC(I).LE.NRTS) THEN
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT,'(DTMIN=',DTM,')'
             WRITE(IOUT,*)'SECONDARY NODES NB',ITAB(N1(I)),
     .             ITAB(N2(I))               
             WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
            ELSE
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT,'(DTMIN=',DTM,')'
             WRITE(IOUT,*)'SECONDARY NODES NB',ITAFI(NIN)%P(N1(I)),
     .             ITAFI(NIN)%P(N2(I))               
             WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
            END IF
#include "lockoff.inc"
             MSTOP = 2
           ENDIF                
         ENDIF
        ENDDO
       ENDIF
      ENDIF
C      
      RETURN
      END
C

