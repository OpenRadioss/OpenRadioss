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
Chd|  I25COR3_E2S                   source/interfaces/int25/i25cor3_e2s.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        INT_CHECKSUM                  share/modules/debug_mod.F     
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COR3_E2S(
     1      JLT    ,LEDGE  ,IRECT  ,X      ,V      ,
     2      CAND_S ,CAND_M ,STFM   ,MS     ,EX     ,
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
     E      VTX_BISECTOR,TYPEDGS,IAS ,JAS  ,IBS    ,
     F      JBS    ,IAM    ,STFE   , EDGE_ID, ITAB ,
     G      INTFRIC,IPARTFRIC_E,IPARTFRIC_ES,IPARTFRIC_EM,
     H      IGSTI  ,KMIN   ,KMAX  ,E2S_NOD_NORMAL,NADMSR,
     I      NORMALN1,NORMALN2,NORMALM1,NORMALM2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
#ifdef WITH_ASSERT
      USE DEBUG_MOD
#endif
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "assert.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "i25edge_c.inc"
#include      "scr02_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: ITAB(*)
      INTEGER :: EDGE_ID(2,4*MVSIZ)
      INTEGER :: INTFRIC ,IPARTFRIC_E(*),IPARTFRIC_ES(4,MVSIZ),IPARTFRIC_EM(4,MVSIZ)
      INTEGER LEDGE(NLEDGE,*), IRECT(4,*), CAND_M(*), CAND_S(*), ADMSR(4,*),
     .        LBOUND(*), JLT, NEDGE, NIN, IEDGE, 
     .        N1(4,MVSIZ), N2(4,MVSIZ), 
     .        M1(4,MVSIZ), M2(4,MVSIZ), 
     .        NODNX_SMS(*), NSMS(4,MVSIZ), 
     .        TYPEDGS(MVSIZ),IAS(MVSIZ),JAS(MVSIZ),IBS(MVSIZ),JBS(MVSIZ),IAM(MVSIZ)
      INTEGER  , INTENT(IN) :: IGSTI, NADMSR
C     REAL
      my_real
     .        X(3,*), STFM(*), STFE(*), MS(*), V(3,*),
     .        XXS1(4,MVSIZ), XXS2(4,MVSIZ), XYS1(4,MVSIZ), XYS2(4,MVSIZ),
     .        XZS1(4,MVSIZ), XZS2(4,MVSIZ), XXM1(4,MVSIZ), XXM2(4,MVSIZ),
     .        XYM1(4,MVSIZ), XYM2(4,MVSIZ), XZM1(4,MVSIZ), XZM2(4,MVSIZ),
     .        VXS1(4,MVSIZ), VXS2(4,MVSIZ), VYS1(4,MVSIZ), VYS2(4,MVSIZ),
     .        VZS1(4,MVSIZ), VZS2(4,MVSIZ), VXM1(4,MVSIZ), VXM2(4,MVSIZ),
     .        VYM1(4,MVSIZ), VYM2(4,MVSIZ), VZM1(4,MVSIZ), VZM2(4,MVSIZ),
     .        MS1(4,MVSIZ),  MS2(4,MVSIZ),  MM1(4,MVSIZ),  MM2(4,MVSIZ),
     .        STIF(4,MVSIZ),STFAC,STS,STM,
     .        GAPE(*)     ,GAPVE(4,MVSIZ),
     .        EX(4,MVSIZ), EY(4,MVSIZ), EZ(4,MVSIZ), FX(MVSIZ), FY(MVSIZ), FZ(MVSIZ)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
      my_real  , INTENT(IN) :: KMIN, KMAX
      REAL*4 , INTENT(IN) :: E2S_NOD_NORMAL(3,NADMSR)
      my_real  , INTENT(INOUT) :: NORMALN1(3,MVSIZ),NORMALN2(3,MVSIZ),
     .       NORMALM1(3,4,MVSIZ),NORMALM2(3,4,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NN, J, JRM, K, KRM, I1, J1, I2, J2, EJ,
     .         IE, JE, SOL_EDGE, SH_EDGE, ES, IS(MVSIZ)
C     INTEGER ::  NOD1S(MVSIZ),NOD2S(MVSIZ)
C     INTEGER ::  NOD1M(MVSIZ),NOD2M(MVSIZ)

      my_real
     .   AAA, DX, DY, DZ, DD, NNI, NI2, INVCOS, GAPE_M(MVSIZ), GAPE_S(MVSIZ)
      INTEGER :: IDS(4)
C-----------------------------------------------

      EDGE_ID(1:2,1:4*MVSIZ) = -666
      DO I=1,JLT

        IAM(I)=CAND_M(I)

        STM=STFM(IAM(I))

#ifdef WITH_ASSERT
C definition of an ID for the edge using hash table
C used for debug only 
C using -DWITH_ASSERT will make IDs unique 
C whatever the number of domains
        IDS(1) =  ITAB(IRECT(1,IAM(I)))
        IDS(2) =  ITAB(IRECT(2,IAM(I)))
        IDS(3) =  ITAB(IRECT(3,IAM(I)))
        IDS(4) =  ITAB(IRECT(4,IAM(I)))
        EDGE_ID(1,I) = INT_CHECKSUM(IDS,4,1)
#else
C by default, the ID is local to each domain
        EDGE_ID(1,I) = I
#endif

        DO EJ=1,4
          M1(EJ,I)=IRECT(EJ,IAM(I))
          M2(EJ,I)=IRECT(MOD(EJ,4)+1,IAM(I))

          XXM1(EJ,I) = X(1,M1(EJ,I))
          XYM1(EJ,I) = X(2,M1(EJ,I))
          XZM1(EJ,I) = X(3,M1(EJ,I))
          XXM2(EJ,I) = X(1,M2(EJ,I))
          XYM2(EJ,I) = X(2,M2(EJ,I))
          XZM2(EJ,I) = X(3,M2(EJ,I))
          VXM1(EJ,I) = V(1,M1(EJ,I))
          VYM1(EJ,I) = V(2,M1(EJ,I))
          VZM1(EJ,I) = V(3,M1(EJ,I))
          VXM2(EJ,I) = V(1,M2(EJ,I))
          VYM2(EJ,I) = V(2,M2(EJ,I))
          VZM2(EJ,I) = V(3,M2(EJ,I))
          MM1(EJ,I) = MS(M1(EJ,I))
          MM2(EJ,I) = MS(M2(EJ,I))
C
          IF(CAND_S(I)<=NEDGE) THEN

            ES    =CAND_S(I)
            IAS(I)=ABS(LEDGE(1,ES))
            JAS(I)=LEDGE(2,ES)
            IBS(I)=LEDGE(3,ES)
            JBS(I)=LEDGE(4,ES)
            N1(EJ,I)=LEDGE(5,ES)
            N2(EJ,I)=LEDGE(6,ES)
C           NOD1S(I) = LEDGE(11,ES)
C           NOD2S(I) = LEDGE(12,ES)
            IS(I) = LEDGE(10,ES)
            EDGE_ID(2,I) = LEDGE(8,ES)

C           IF(IRECT(JAS(I),IAS(I))==N1(EJ,I).AND.IRECT(MOD(JAS(I),4)+1,IAS(I))==N2(EJ,I))THEN
C             IS(I)= 1
C           ELSEIF(IRECT(JAS(I),IAS(I))==N2(EJ,I).AND.IRECT(MOD(JAS(I),4)+1,IAS(I))==N1(EJ,I))THEN
C             IS(I)=-1
C           ELSE
C             print *,'i25cor3_e2s - internal problem',ES,N1(EJ,I),N2(EJ,I),
C    .                            IRECT(JAS(I),IAS(I)),IRECT(MOD(JAS(I),4)+1,IAS(I))
C           END IF

            STS=STFE(ES)
            STIF(EJ,I)=STS*STM / MAX(EM20,STS+STM)
            STIF(EJ,I)=MAX(KMIN,MIN(STIF(EJ,I),KMAX))

            XXS1(EJ,I) = X(1,N1(EJ,I))
            XYS1(EJ,I) = X(2,N1(EJ,I))
            XZS1(EJ,I) = X(3,N1(EJ,I))
            XXS2(EJ,I) = X(1,N2(EJ,I))
            XYS2(EJ,I) = X(2,N2(EJ,I))
            XZS2(EJ,I) = X(3,N2(EJ,I))
            VXS1(EJ,I) = V(1,N1(EJ,I))
            VYS1(EJ,I) = V(2,N1(EJ,I))
            VZS1(EJ,I) = V(3,N1(EJ,I))
            VXS2(EJ,I) = V(1,N2(EJ,I))
            VYS2(EJ,I) = V(2,N2(EJ,I))
            VZS2(EJ,I) = V(3,N2(EJ,I))
            MS1(EJ,I) = MS(N1(EJ,I))
            MS2(EJ,I) = MS(N2(EJ,I))
C
            TYPEDGS(I)=LEDGE(7,ES)
C
          ELSE
            NN = CAND_S(I) - NEDGE           
            IS(I) = LEDGE_FIE(NIN)%P(E_IM,NN) 
            N1(EJ,I)=2*(NN-1)+1
            N2(EJ,I)=2*NN


            EDGE_ID(2,I) = LEDGE_FIE(NIN)%P(E_GLOBAL_ID,NN)

c            STS=STFE(CAND_S(I))
c            STIF(I)=STS*STM / MAX(EM20,STS+STM)
c            STIF(I)=ABS(STIFIE(NIN)%P(NN))*STM
c                  / MAX(EM20,ABS(STIFIE(NIN)%P(NN))+STM)
c
c            TYPEDGS(I)=LEDGE(7,CAND_S(I))
c
            STS=STIFIE(NIN)%P(NN)
            STIF(EJ,I)=STS*STM / MAX(EM20,STS+STM)

            STIF(EJ,I)=MAX(KMIN,MIN(STIF(EJ,I),KMAX))

            TYPEDGS(I)=LEDGE_FIE(NIN)%P(E_TYPE,NN)

            IAS(I)=ABS(LEDGE_FIE(NIN)%P(E_LEFT_SEG  ,NN))
            JAS(I)=LEDGE_FIE(NIN)%P(E_LEFT_ID   ,NN)
            IBS(I)=LEDGE_FIE(NIN)%P(E_RIGHT_SEG ,NN)
            JBS(I)=LEDGE_FIE(NIN)%P(E_RIGHT_ID  ,NN)


            XXS1(EJ,I) = XFIE(NIN)%P(1,N1(EJ,I))
            XYS1(EJ,I) = XFIE(NIN)%P(2,N1(EJ,I))
            XZS1(EJ,I) = XFIE(NIN)%P(3,N1(EJ,I))
            XXS2(EJ,I) = XFIE(NIN)%P(1,N2(EJ,I))
            XYS2(EJ,I) = XFIE(NIN)%P(2,N2(EJ,I))
            XZS2(EJ,I) = XFIE(NIN)%P(3,N2(EJ,I))
            VXS1(EJ,I) = VFIE(NIN)%P(1,N1(EJ,I))
            VYS1(EJ,I) = VFIE(NIN)%P(2,N1(EJ,I))
            VZS1(EJ,I) = VFIE(NIN)%P(3,N1(EJ,I))
            VXS2(EJ,I) = VFIE(NIN)%P(1,N2(EJ,I))
            VYS2(EJ,I) = VFIE(NIN)%P(2,N2(EJ,I))
            VZS2(EJ,I) = VFIE(NIN)%P(3,N2(EJ,I))
            MS1(EJ,I) = MSFIE(NIN)%P(N1(EJ,I)) 
            MS2(EJ,I) = MSFIE(NIN)%P(N2(EJ,I))
C
          END IF
        END DO
      END DO

C
C     THIS is provisional (solids => Zero gap even if secnd shell edge)
      DO I=1,JLT
        GAPE_M(I)=ZERO ! Solids 
                       ! If edge is shared by solid and shell : edge is considered as a shell edge
        IF(CAND_S(I)<=NEDGE) THEN
          GAPE_S(I)=GAPE(CAND_S(I))
        ELSE ! TBD
          GAPE_S(I)= GAPFIE(NIN)%P(CAND_S(I) - NEDGE)
        END IF
        GAPVE(1:4,I)=ZERO
      END DO

      SOL_EDGE=IEDGE/10 ! solids
      SH_EDGE =IEDGE-10*SOL_EDGE ! shells

      DO I=1,JLT
        DEBUG_E2E(EDGE_ID(1,I)==D_EM .AND. EDGE_ID(2,I) == D_ES, STFM(IAM(I)))

        IF ( STFM(IAM(I)) > ZERO) THEN 
        DO EJ=1,4
C Comment savoir si EDG_BiSECTOR a ete calcule
          EX(EJ,I)=EDG_BISECTOR(1,EJ,IAM(I))
          EY(EJ,I)=EDG_BISECTOR(2,EJ,IAM(I))
          EZ(EJ,I)=EDG_BISECTOR(3,EJ,IAM(I))
          DEBUG_E2E(EDGE_ID(1,I)==D_EM .AND. EDGE_ID(2,I) == D_ES,EX(EJ,I))
          DEBUG_E2E(EDGE_ID(1,I)==D_EM .AND. EDGE_ID(2,I) == D_ES,EY(EJ,I))
          DEBUG_E2E(EDGE_ID(1,I)==D_EM .AND. EDGE_ID(2,I) == D_ES,EZ(EJ,I))
        END DO
        ELSE
         EX(1:4,I) = ZERO
         EY(1:4,I) = ZERO 
         EZ(1:4,I) = ZERO
         DEBUG_E2E(EDGE_ID(1,I)==D_EM .AND. EDGE_ID(2,I) == D_ES,ZERO)
        END IF
      END DO

C
      DO I=1,JLT
        IF(CAND_S(I)<=NEDGE) THEN
          FX(I) = EDG_BISECTOR(1,JAS(I),IAS(I))
          FY(I) = EDG_BISECTOR(2,JAS(I),IAS(I))
          FZ(I) = EDG_BISECTOR(3,JAS(I),IAS(I))
         ELSE
           FX(I) = EDG_BISECTOR_FIE(NIN)%P(1,1,CAND_S(I)-NEDGE)
           FY(I) = EDG_BISECTOR_FIE(NIN)%P(2,1,CAND_S(I)-NEDGE)
           FZ(I) = EDG_BISECTOR_FIE(NIN)%P(3,1,CAND_S(I)-NEDGE)
        END IF
      END DO

C
      NSMS(1:4,1:MVSIZ) = -666
      IF(IDTMINS==2)THEN
       DO I=1,JLT
        IF(CAND_S(I)<=NEDGE)THEN
          DO EJ=1,4
            NSMS(EJ,I)=NODNX_SMS(N1(EJ,I))+NODNX_SMS(N2(EJ,I))+
     .              NODNX_SMS(M1(EJ,I))+NODNX_SMS(M2(EJ,I))
            DEBUG_E2E(NSMS(EJ,I) < 0,NODNX_SMS(N1(EJ,I)))
            DEBUG_E2E(NSMS(EJ,I) < 0,NODNX_SMS(N2(EJ,I)))

          END DO
        ELSE
          DO EJ=1,4
            NSMS(EJ,I)=NODNXFIE(NIN)%P(N1(EJ,I))+NODNXFIE(NIN)%P(N2(EJ,I))+
     .              NODNX_SMS(M1(EJ,I))+NODNX_SMS(M2(EJ,I))
            DEBUG_E2E(NSMS(EJ,I) < 0,NODNXFIE(NIN)%P(N1(EJ,I)))
            DEBUG_E2E(NSMS(EJ,I) < 0,NODNXFIE(NIN)%P(N2(EJ,I)))
          END DO
        END IF
       ENDDO

       IF(IDTMINS_INT/=0)THEN
         DO I=1,JLT
         DO EJ=1,4
          IF(NSMS(EJ,I)==0)NSMS(EJ,I)=-1
         ENDDO
         ENDDO
       END IF
      ELSEIF(IDTMINS_INT/=0)THEN
        DO I=1,JLT
        DO EJ=1,4
         NSMS(EJ,I)=-1
        ENDDO
        ENDDO
      ENDIF
C
C----Friction model : secnd part IDs---------
      IF(INTFRIC > 0) THEN
         DO I=1,JLT

           IF(CAND_S(I)<=NEDGE)THEN
             IPARTFRIC_ES(1:4,I) = IPARTFRIC_E(CAND_S(I))
           ELSE
             NN = CAND_S(I) - NEDGE            
             IPARTFRIC_ES(1:4,I)= IPARTFRIC_FIE(NIN)%P(NN)
           ENDIF
C
           IPARTFRIC_EM(1:4,I) = IPARTFRIC_E(CAND_M(I))
         ENDDO
       ENDIF
C-------Normal nodes ---------
      IF(SOL_EDGE/=0)THEN
         DO I=1,JLT
          IF(TYPEDGS(I)/=1)CYCLE
          DO EJ=1,4
             NORMALM1(1:3,EJ,I)=E2S_NOD_NORMAL(1:3,ADMSR(EJ,IAM(I)))
             NORMALM2(1:3,EJ,I)=E2S_NOD_NORMAL(1:3,ADMSR(MOD(EJ,4)+1,IAM(I)))
          ENDDO
          IF(CAND_S(I)<=NEDGE)THEN
             IF(IS(I) == 1 ) THEN
                NORMALN1(1:3,I)=E2S_NOD_NORMAL(1:3,ADMSR(JAS(I),IAS(I)))
                NORMALN2(1:3,I)=E2S_NOD_NORMAL(1:3,ADMSR(MOD(JAS(I),4)+1,IAS(I)))
             ELSE
                NORMALN2(1:3,I)=E2S_NOD_NORMAL(1:3,ADMSR(JAS(I),IAS(I)))
                NORMALN1(1:3,I)=E2S_NOD_NORMAL(1:3,ADMSR(MOD(JAS(I),4)+1,IAS(I)))
             ENDIF
          ELSE
            IF(IS(I) == 1 ) THEN
                NORMALN1(1:3,I)=EDG_BISECTOR_FIE(NIN)%P(1:3,2,CAND_S(I) - NEDGE)
                NORMALN2(1:3,I)=EDG_BISECTOR_FIE(NIN)%P(1:3,3,CAND_S(I) - NEDGE)
            ELSE
                NORMALN2(1:3,I)=EDG_BISECTOR_FIE(NIN)%P(1:3,2,CAND_S(I) - NEDGE)
                NORMALN1(1:3,I)=EDG_BISECTOR_FIE(NIN)%P(1:3,3,CAND_S(I) - NEDGE)
            ENDIF
          ENDIF
            
         ENDDO
      ENDIF
      RETURN
      END
