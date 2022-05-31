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
Chd|  I11KEG3                       source/interfaces/int11/i11keg3.F
Chd|-- called by -----------
Chd|        I11KE3                        source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11KEG3(
     1                  JLT    ,A     ,V        ,GAP    ,FRIC   ,
     2                  MS     ,CS_LOC ,CM_LOC  ,STIGLO ,STIF   ,
     3                  HS1    ,HS2     ,HM1    ,HM2    ,NX     ,
     4                  NY     ,NZ     ,GAPV    ,PENIS  ,PENIM  , 
     5                  INACTI ,NRTS   ,MS1     ,MS2    ,MM1    ,
     6                  MM2    ,VXS1   ,VYS1    ,VZS1   ,VXS2   ,
     7                  VYS2   ,VZS2    ,VXM1   ,VYM1   ,VZM1   ,
     8                  VXM2   ,VYM2    ,VZM2   ,K1I11  ,K1I12  ,
     9                  K1J11  ,K1J12   ,K2I11  ,K2I12  ,K2J11  ,
     A                  K2J12  ,N1      ,N2     ,NIN    ,LREM   ,
     B                  OFF    ,SCALK   ,IDESAC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE IMP_INTM
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,INACTI,NRTS,NIN,LREM,IDESAC
      INTEGER CS_LOC(MVSIZ), CM_LOC(MVSIZ),N1(MVSIZ), N2(MVSIZ)
      my_real
     .   STIGLO,A(3,*), MS(*), V(3,*),
     .   GAPV(*),PENIS(2,*), PENIM(2,*),GAP, FRIC,SCALK
      my_real
     .   HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), STIF(MVSIZ),
     .   MS1(MVSIZ),MS2(MVSIZ),MM1(MVSIZ),MM2(MVSIZ), OFF(MVSIZ),
     .   VXS1(MVSIZ),VYS1(MVSIZ),VZS1(MVSIZ),VXS2(MVSIZ),VYS2(MVSIZ),
     .   VZS2(MVSIZ),VXM1(MVSIZ),VYM1(MVSIZ),VZM1(MVSIZ),VXM2(MVSIZ),
     .   VYM2(MVSIZ),VZM2(MVSIZ),K1I11(3,3,MVSIZ),K1J11(3,3,MVSIZ),
     .    K2I11(3,3,MVSIZ),K2J11(3,3,MVSIZ),K1I12(3,3,MVSIZ),
     .    K1J12(3,3,MVSIZ),K2I12(3,3,MVSIZ),K2J12(3,3,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J , K0,K1S,K, NI,ISF,NN,NS,JLTF,NE,NN1,NN2
      my_real
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),PENE(MVSIZ),
     .   VNX, VNY, VNZ, AA, VMAX,S2,DIST,RDIST,
     .   V2, FM2, DT1INV, VISCA,  FAC,  FF,
     .   FX, FY, FZ, F2, MAS2, FACM1, PPLUS
      my_real
     .   PREC,FACT(MVSIZ),KN(4,MVSIZ),Q(3,3,MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ)
      my_real
     .   Q11,Q12,Q13,Q22,Q23,Q33,H00,VTX,VTY,VTZ,VT,
     .   KT1,KT2,KT3,KT4,Q1,Q2,FACF
C-----------------------------------------------
#include "sync_dir.inc"
      IF (IRESP .EQ. 1) THEN
         PREC = FIVEEM4
      ELSE 
         PREC = EM10
      ENDIF
C
      DO I=1,JLT
         S2 = SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2)
         PENE(I) = GAPV(I) - S2
         S2 = ONE/MAX(EM30,S2)
         NX(I) = NX(I)*S2
         NY(I) = NY(I)*S2
         NZ(I) = NZ(I)*S2
      ENDDO
C
      IF(INACTI.EQ.5)THEN
       IF(IMACH.NE.3) THEN
#include "lockon.inc"
        DO I=1,JLT
        PENIS(2,CS_LOC(I)) = MAX(PENIS(2,CS_LOC(I)),HALF*PENE(I))
        PENIM(2,CM_LOC(I)) = MAX(PENIM(2,CM_LOC(I)),HALF*PENE(I))
        ENDDO
#include "lockoff.inc"
        DO I=1,JLT
         PENE(I) = PENE(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
         PENE(I) = MAX(PENE(I),ZERO)
         IF(PENE(I).EQ.ZERO)STIF(I)=ZERO
         GAPV(I) = GAPV(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
        ENDDO
       ELSE
        DO I=1,JLT
         IF(CS_LOC(I).LE.NRTS) THEN
           PENIS(2,CS_LOC(I)) = MAX(PENIS(2,CS_LOC(I)),HALF*PENE(I))
         ELSE
           NI = CS_LOC(I)-NRTS
           PENFI(NIN)%P(2,NI) = MAX(PENFI(NIN)%P(2,NI),HALF*PENE(I))
         END IF
         PENIM(2,CM_LOC(I)) = MAX(PENIM(2,CM_LOC(I)),HALF*PENE(I))
        ENDDO
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
       END IF
      ELSE IF(INACTI.EQ.6)THEN
       IF(IMACH.NE.3) THEN
#include "lockon.inc"
        DO I=1,JLT
        PPLUS=HALF*(PENE(I)+FIVEEM2*(GAPV(I)-PENE(I)))
        PENIS(2,CS_LOC(I)) = MAX(PENIS(2,CS_LOC(I)),PPLUS)
        PENIM(2,CM_LOC(I)) = MAX(PENIM(2,CM_LOC(I)),PPLUS)
        ENDDO
#include "lockoff.inc"
        DO I=1,JLT
         PENE(I) = PENE(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
         PENE(I) = MAX(PENE(I),ZERO)
         IF(PENE(I).EQ.ZERO)STIF(I)=ZERO
         GAPV(I) = GAPV(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
        ENDDO
       ELSE
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
       END IF
      ENDIF
C
      DO I=1,JLT
        GAPV(I) = ZEP9*GAPV(I)
        VX(I) = HS1(I)*VXS1(I) + HS2(I)*VXS2(I)
     .        - HM1(I)*VXM1(I) - HM2(I)*VXM2(I)
        VY(I) = HS1(I)*VYS1(I) + HS2(I)*VYS2(I)
     .        - HM1(I)*VYM1(I) - HM2(I)*VYM2(I)
        VZ(I) = HS1(I)*VZS1(I) + HS2(I)*VZS2(I)
     .        - HM1(I)*VZM1(I) - HM2(I)*VZM2(I)
        VN(I) = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
        H1(I) = HS1(I)*HM1(I)
        H2(I) = HS1(I)*HM2(I)
        H3(I) = HS2(I)*HM1(I)
        H4(I) = HS2(I)*HM2(I)
      ENDDO
C-------------------------------------------
C
      IF(IMP_INT7.GE.2)THEN
       DO I=1,JLT
         STIF(I) = HALF*STIF(I) 
       ENDDO
      ELSEIF(IMP_INT7.EQ.1)THEN
       DO I=1,JLT
        FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
        IF(( (GAPV(I)-PENE(I))/GAPV(I) ).LT.PREC .AND. 
     .                                STIF(I).GT.ZERO ) THEN
         STIF(I) = ZERO
         PENE(I)= ZERO
         IDESAC = 1
        ELSE
         STIF(I) = HALF*STIF(I) * FAC
        ENDIF
       ENDDO
      ELSE
      DO I=1,JLT
       FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
       IF(( (GAPV(I)-PENE(I))/GAPV(I) ).LT.PREC .AND. 
     .                                STIF(I).GT.ZERO ) THEN
         STIF(I) = ZERO
         PENE(I)= ZERO
         IDESAC = 1
       ELSE
        STIF(I) = HALF*STIF(I) * FAC
       ENDIF
      ENDDO
      DO I=1,JLT
          STIF(I) = STIF(I) * GAPV(I) / 
     .            MAX((GAPV(I) - PENE(I)),EM10) 
      ENDDO
C
      END IF !(IMP_INT7.GE.2)
      IF(IDESAC.GT.0) RETURN
C
C---------------------------------
C    ----sans frottement d'abord--- 
C---------------------------------
      DO I=1,JLT
        VTX = VX(I) -VN(I)*NX(I)
        VTY = VY(I) -VN(I)*NY(I)
        VTZ = VZ(I) -VN(I)*NZ(I)
        VT  = VTX*VTX+VTY*VTY+VTZ*VTZ
        IF (VT.GT.EM20) THEN
         S2=ONE/SQRT(VT)
         Q(1,1,I)=VTX*S2
         Q(1,2,I)=VTY*S2
         Q(1,3,I)=VTZ*S2
         Q(3,1,I)=NX(I)
         Q(3,2,I)=NY(I)
         Q(3,3,I)=NZ(I)
         Q(2,1,I)=Q(3,2,I)*Q(1,3,I)-Q(3,3,I)*Q(1,2,I)
         Q(2,2,I)=Q(3,3,I)*Q(1,1,I)-Q(3,1,I)*Q(1,3,I)
         Q(2,3,I)=Q(3,1,I)*Q(1,2,I)-Q(3,2,I)*Q(1,1,I)
         FACT(I)=FRIC
        ELSE
         FACT(I)=ZERO
        ENDIF
      ENDDO
      IF (SCALK.LT.0) THEN
       ISF=1
      ELSE
       ISF=0
      ENDIF
      FACF=ABS(SCALK)
      IF (ISF.EQ.1) THEN
       DO I=1,JLT
        IF (VN(I).GT.ZERO) THEN
         FAC=STIF(I)*FACF
c         write(iout,*)'FAC+,VN(I)=',FAC,VN(I),I
        ELSEIF (VN(I).LT.ZERO) THEN
         FAC=STIF(I)/FACF
c        write(iout,*)'FAC-,VN(I)=',FAC,VN(I),I
        ELSE
         FAC=STIF(I)
        ENDIF
        KN(1,I)=FAC*H1(I)
        KN(2,I)=FAC*H2(I)
        KN(3,I)=FAC*H3(I)
        KN(4,I)=FAC*H4(I)
        FACT(I)=FAC*FACT(I)
       ENDDO
      ELSE
       DO I=1,JLT
        FAC=STIF(I)*FACF
        KN(1,I)=FAC*H1(I)
        KN(2,I)=FAC*H2(I)
        KN(3,I)=FAC*H3(I)
        KN(4,I)=FAC*H4(I)
        FACT(I)=FAC*FACT(I)
       ENDDO
      ENDIF
      DO I=1,JLT
       Q11=NX(I)*NX(I)
       Q12=NX(I)*NY(I)
       Q13=NX(I)*NZ(I)
       Q22=NY(I)*NY(I)
       Q23=NY(I)*NZ(I)
       Q33=NZ(I)*NZ(I)
       K1I11(1,1,I)=KN(1,I)*Q11
       K1I11(1,2,I)=KN(1,I)*Q12
       K1I11(1,3,I)=KN(1,I)*Q13
       K1I11(2,2,I)=KN(1,I)*Q22
       K1I11(2,3,I)=KN(1,I)*Q23
       K1I11(3,3,I)=KN(1,I)*Q33
       K1J11(1,1,I)=KN(2,I)*Q11
       K1J11(1,2,I)=KN(2,I)*Q12
       K1J11(1,3,I)=KN(2,I)*Q13
       K1J11(2,2,I)=KN(2,I)*Q22
       K1J11(2,3,I)=KN(2,I)*Q23
       K1J11(3,3,I)=KN(2,I)*Q33
       K2I11(1,1,I)=KN(3,I)*Q11
       K2I11(1,2,I)=KN(3,I)*Q12
       K2I11(1,3,I)=KN(3,I)*Q13
       K2I11(2,2,I)=KN(3,I)*Q22
       K2I11(2,3,I)=KN(3,I)*Q23
       K2I11(3,3,I)=KN(3,I)*Q33
       K2J11(1,1,I)=KN(4,I)*Q11
       K2J11(1,2,I)=KN(4,I)*Q12
       K2J11(1,3,I)=KN(4,I)*Q13
       K2J11(2,2,I)=KN(4,I)*Q22
       K2J11(2,3,I)=KN(4,I)*Q23
       K2J11(3,3,I)=KN(4,I)*Q33
      ENDDO
C    ----avec frottement --- 
       DO J=1,3 
        DO K=J,3 
         DO I=1,JLT
          IF (FACT(I).GT.ZERO) THEN
           Q1 =Q(1,J,I)*Q(1,K,I)
           Q2 =Q(2,J,I)*Q(2,K,I)
           FAC=FACT(I)*(Q1+Q2)
           KT1=FAC*H1(I)
           K1I11(J,K,I)=K1I11(J,K,I)+KT1
           KT2=FAC*H2(I)
           K1J11(J,K,I)=K1J11(J,K,I)+KT2
           KT3=FAC*H3(I)
           K2I11(J,K,I)=K2I11(J,K,I)+KT3
           KT4=FAC*H4(I)
           K2J11(J,K,I)=K2J11(J,K,I)+KT4
          ENDIF 
         ENDDO
        ENDDO
       ENDDO
C
       DO J=1,3 
        DO K=J,3 
         DO I=1,JLT
          K1I12(J,K,I)=-K1I11(J,K,I)
          K1J12(J,K,I)=-K1J11(J,K,I)
          K2I12(J,K,I)=-K2I11(J,K,I)
          K2J12(J,K,I)=-K2J11(J,K,I)
         ENDDO
        ENDDO
       ENDDO
       DO J=1,3 
        DO K=J+1,3 
         DO I=1,JLT
          K1I12(K,J,I)=-K1I11(J,K,I)
          K1J12(K,J,I)=-K1J11(J,K,I)
          K2I12(K,J,I)=-K2I11(J,K,I)
          K2J12(K,J,I)=-K2J11(J,K,I)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,JLT
        OFF(I)=ONE
       ENDDO
C
       IF (IMACH.EQ.3.AND.NSPMD.GT.1) THEN
       IF ((INTP_D).GT.0) THEN
        DO I=1,JLT
         IF(CS_LOC(I).GT.NRTS) THEN
	  NN=CS_LOC(I)-NRTS
	  NS=IND_INT(NIN)%P(NN)
C---------pour temporairement diag_ss---
          NN1 = NS
	  FFI(1,NN1)=ZERO
	  FFI(2,NN1)=ZERO
	  FFI(3,NN1)=ZERO
	  DFI(1,NN1)=ZERO
	  DFI(2,NN1)=ZERO
	  DFI(3,NN1)=ZERO
          NN2 = NN1 + 1
	  FFI(1,NN2)=ZERO
	  FFI(2,NN2)=ZERO
	  FFI(3,NN2)=ZERO
	  DFI(1,NN2)=ZERO
	  DFI(2,NN2)=ZERO
	  DFI(3,NN2)=ZERO
	 ENDIF
        ENDDO
       ELSE
        JLTF = 0
        DO I=1,JLT
         IF(CS_LOC(I).GT.NRTS) THEN
          JLTF = JLTF + 1
	  NE=SHF_INT(NIN) + JLTF +LREM
	  NN=CS_LOC(I)-NRTS
	  NS=IND_INT(NIN)%P(NN)
	  STIFS(NE)=STIF(I)
	  H_E(1,NE)=HS1(I)
	  H_E(2,NE)=HS2(I)
	  H_E(3,NE)=HM1(I)
	  H_E(4,NE)=HM2(I)
	  N_E(1,NE)=NX(I)
	  N_E(2,NE)=NY(I)
	  N_E(3,NE)=NZ(I)
C---------pour temporairement diag_ss---
          NN1 = NS
	  FFI(1,NN1)=ZERO
	  FFI(2,NN1)=ZERO
	  FFI(3,NN1)=ZERO
	  DFI(1,NN1)=ZERO
	  DFI(2,NN1)=ZERO
	  DFI(3,NN1)=ZERO
          NN2 = NN1 + 1
	  FFI(1,NN2)=ZERO
	  FFI(2,NN2)=ZERO
	  FFI(3,NN2)=ZERO
	  DFI(1,NN2)=ZERO
	  DFI(2,NN2)=ZERO
	  DFI(3,NN2)=ZERO
	 ENDIF
        ENDDO
       ENDIF 
       ENDIF 
C
      RETURN
      END
Chd|====================================================================
Chd|  I11FRF3                       source/interfaces/int11/i11keg3.F
Chd|-- called by -----------
Chd|        I11FORCF3                     source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE I11FRF3(JLT    ,A      ,V      ,FRIC   ,HS1    ,
     1                  HS2     ,HM1    ,HM2    ,NX     ,NY     ,
     2                  NZ      ,MS1    ,MS2    ,MM1    ,MM2    ,
     3                  VXS1    ,VYS1   ,VZS1   ,VXS2   ,VYS2   ,
     4                  VZS2    ,VXM1   ,VYM1   ,VZM1   ,VXM2   ,
     5                  VYM2    ,VZM2   ,N1     ,N2     ,M1     ,
     6                   M2     ,DXS1   ,DYS1   ,DZS1   ,DXS2   ,  
     7                   DYS2   ,DZS2   ,DXM1   ,DYM1   ,DZM1   ,  
     8                   DXM2   ,DYM2   ,DZM2   ,STIF   , NIN   ,
     9                   SCALK  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NIN
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ)
      my_real
     .   A(3,*),  V(3,*),FRIC,SCALK
      my_real
     .   HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), STIF(MVSIZ),
     .   MS1(MVSIZ),MS2(MVSIZ),MM1(MVSIZ),MM2(MVSIZ),
     .   VXS1(MVSIZ),VYS1(MVSIZ),VZS1(MVSIZ),VXS2(MVSIZ),VYS2(MVSIZ),
     .   VZS2(MVSIZ),VXM1(MVSIZ),VYM1(MVSIZ),VZM1(MVSIZ),VXM2(MVSIZ),
     .   VYM2(MVSIZ),VZM2(MVSIZ),DXS1(MVSIZ),DYS1(MVSIZ),DZS1(MVSIZ),
     .   DXS2(MVSIZ),DYS2(MVSIZ),DZS2(MVSIZ),DXM1(MVSIZ),DYM1(MVSIZ),
     .   DZM1(MVSIZ),DXM2(MVSIZ),DYM2(MVSIZ),DZM2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J, K,IG,ISF,NN,NS,NI,NJ
      my_real
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   DX(MVSIZ), DY(MVSIZ), DZ(MVSIZ), DN(MVSIZ), 
     .   DXI(MVSIZ), DYI(MVSIZ), DZI(MVSIZ), 
     .   DNI(MVSIZ),DT(MVSIZ), DTI(MVSIZ), 
     .   S2,FACN(MVSIZ),FACF, FACT(MVSIZ)
      my_real
     .   FX,FY,FZ,FN,FT,FNI,FTI,VTX,VTY,VTZ,VT,
     .   T1(MVSIZ), T2(MVSIZ), T3(MVSIZ),
     .   KT1,KT2,KT3,KT4,Q1,Q2

C-----------------------------------------------
C
      DO I=1,JLT
        VX(I) = HS1(I)*VXS1(I) + HS2(I)*VXS2(I)
     .        - HM1(I)*VXM1(I) - HM2(I)*VXM2(I)
        VY(I) = HS1(I)*VYS1(I) + HS2(I)*VYS2(I)
     .        - HM1(I)*VYM1(I) - HM2(I)*VYM2(I)
        VZ(I) = HS1(I)*VZS1(I) + HS2(I)*VZS2(I)
     .        - HM1(I)*VZM1(I) - HM2(I)*VZM2(I)
        VN(I) = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
        DXI(I) = HS1(I)*DXS1(I) + HS2(I)*DXS2(I)
        DYI(I) = HS1(I)*DYS1(I) + HS2(I)*DYS2(I)
        DZI(I) = HS1(I)*DZS1(I) + HS2(I)*DZS2(I)
        DNI(I) = NX(I)*DXI(I) + NY(I)*DYI(I) + NZ(I)*DZI(I)
C
        DX(I) = DXI(I)- HM1(I)*DXM1(I) - HM2(I)*DXM2(I)
        DY(I) = DYI(I)- HM1(I)*DYM1(I) - HM2(I)*DYM2(I)
        DZ(I) = DZI(I)- HM1(I)*DZM1(I) - HM2(I)*DZM2(I)
        DN(I) = NX(I)*DX(I) + NY(I)*DY(I) + NZ(I)*DZ(I)
       ENDDO
C-------------------------------------------
      DO I=1,JLT
        VTX = VX(I) -VN(I)*NX(I)
        VTY = VY(I) -VN(I)*NY(I)
        VTZ = VZ(I) -VN(I)*NZ(I)
        VT  = VTX*VTX+VTY*VTY+VTZ*VTZ
        IF (VT.GT.EM20) THEN
         S2=ONE/SQRT(VT)
         T1(I)=VTX*S2
         T2(I)=VTY*S2
         T3(I)=VTZ*S2
         FACT(I)=FRIC
        ELSE
         FACT(I)=ZERO
         T1(I)=ONE
         T2(I)=ZERO
         T3(I)=ZERO
        ENDIF
      ENDDO
      DO I=1,JLT
       DT(I) = T1(I)*DX(I) + T2(I)*DY(I) + T3(I)*DZ(I)
       DTI(I) = T1(I)*DXI(I) + T2(I)*DYI(I) + T3(I)*DZI(I)
      ENDDO
      IF (SCALK.LT.0) THEN
       ISF=1
      ELSE
       ISF=0
      ENDIF
      FACF=ABS(SCALK)
      IF (ISF.EQ.1) THEN
       DO I=1,JLT
        IF (VN(I).GT.ZERO) THEN
         FACN(I)=STIF(I)*FACF
        ELSEIF (VN(I).LT.ZERO) THEN
         FACN(I)=STIF(I)/FACF
        ELSE
         FACN(I)=STIF(I)
        ENDIF
        FACT(I)=FACN(I)*FACT(I)
       ENDDO
      ELSE
       DO I=1,JLT
        FACN(I)=STIF(I)*FACF
        FACT(I)=FACN(I)*FACT(I)
       ENDDO
      ENDIF
C--------partie NML-------
      DO I=1,JLT
        FN = -FACN(I)*DNI(I)
        FX=FN*NX(I)
        FY=FN*NY(I)
        FZ=FN*NZ(I)
        IF (FACT(I).NE.ZERO) THEN
         FT = -FACT(I)*DTI(I)
         FX = FX + FT*T1(I)
         FY = FY + FT*T2(I)
         FZ = FZ + FT*T3(I)
        ENDIF
        A(1,M1(I))=A(1,M1(I))+FX*HM1(I)
        A(2,M1(I))=A(2,M1(I))+FY*HM1(I)
        A(3,M1(I))=A(3,M1(I))+FZ*HM1(I)
        A(1,M2(I))=A(1,M2(I))+FX*HM2(I)
        A(2,M2(I))=A(2,M2(I))+FY*HM2(I)
        A(3,M2(I))=A(3,M2(I))+FZ*HM2(I)
      ENDDO
C--------partie NSL-------
      DO I=1,JLT
        FNI = FACN(I)*DN(I)
        FX=FNI*NX(I)
        FY=FNI*NY(I)
        FZ=FNI*NZ(I)
        IF (FACT(I).NE.ZERO) THEN
         FT = FACT(I)*DT(I)
         FX = FX + FT*T1(I)
         FY = FY + FT*T2(I)
         FZ = FZ + FT*T3(I)
        ENDIF
        NI = N1(I)
        FFI(1,NI)=FFI(1,NI)+FX*HS1(I)
        FFI(2,NI)=FFI(2,NI)+FY*HS1(I)
        FFI(3,NI)=FFI(3,NI)+FZ*HS1(I)
        NJ = N2(I)
        FFI(1,NJ)=FFI(1,NJ)+FX*HS2(I)
        FFI(2,NJ)=FFI(2,NJ)+FY*HS2(I)
        FFI(3,NJ)=FFI(3,NJ)+FZ*HS2(I)
      ENDDO
C
       RETURN
      END
Chd|====================================================================
Chd|  I11KFOR3                      source/interfaces/int11/i11keg3.F
Chd|-- called by -----------
Chd|        I11FKU3                       source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11KFOR3(
     1                  JLT    ,A       ,V       ,GAP    ,MS     ,
     2                  CS_LOC ,CM_LOC  ,STIF    ,FRIC   ,
     3                  HS1    ,HS2     ,HM1    ,HM2    ,NX     ,
     4                  NY     ,NZ     ,GAPV    ,PENIS  ,PENIM  , 
     5                  INACTI ,NRTS   ,MS1     ,MS2    ,MM1    ,
     6                  MM2    ,VXS1   ,VYS1    ,VZS1   ,VXS2   ,
     7                  VYS2   ,VZS2   ,VXM1    ,VYM1   ,VZM1   ,
     8                  VXM2   ,VYM2   ,VZM2    ,N1      ,N2    ,
     9                  M1     ,M2     ,NIN     ,DXS1    ,DYS1  ,
     A                  DZS1   ,DXS2   ,DYS2    ,DZS2    ,D     ,
     B                  SCALK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE IMP_INTM
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
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,INACTI,NRTS,NIN,LREM,IDESAC
      INTEGER CS_LOC(MVSIZ), CM_LOC(MVSIZ),N1(MVSIZ), N2(MVSIZ),
     .        M1(MVSIZ), M2(MVSIZ) 
      my_real
     .   MS(*), A(3,*),V(3,*),
     .   GAPV(*),PENIS(2,*), PENIM(2,*),GAP,STIF(*),FRIC
      my_real
     .   HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), 
     .   MS1(MVSIZ),MS2(MVSIZ),MM1(MVSIZ),MM2(MVSIZ), 
     .   VXS1(MVSIZ),VYS1(MVSIZ),VZS1(MVSIZ),VXS2(MVSIZ),VYS2(MVSIZ),
     .   VZS2(MVSIZ),VXM1(MVSIZ),VYM1(MVSIZ),VZM1(MVSIZ),VXM2(MVSIZ),
     .   VYM2(MVSIZ),VZM2(MVSIZ),SCALK,
     .   DXS1(MVSIZ),DYS1(MVSIZ),DZS1(MVSIZ),
     .   DXS2(MVSIZ),DYS2(MVSIZ),DZS2(MVSIZ),D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J , K0,K1S,K, NI,NN,NS,JLTF,NE,NN1,NN2,NM1,NM2
      my_real
     .   DXM1(MVSIZ),DYM1(MVSIZ),DZM1(MVSIZ),DXM2(MVSIZ),
     .   DYM2(MVSIZ),DZM2(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),PENE(MVSIZ),
     .   DX(MVSIZ), DY(MVSIZ), DZ(MVSIZ), DN(MVSIZ),
     .   DXI(MVSIZ),DYI(MVSIZ), DZI(MVSIZ),DNI(MVSIZ), 
     .   FXI(MVSIZ),FYI(MVSIZ), FZI(MVSIZ),FNI(MVSIZ), 
     .   VNX, VNY, VNZ, AA, VMAX,S2,DIST,RDIST,
     .   V2, FM2, VISCA,  FAC,  FF,DXT,T1,T2,T3,FTN,
     .   FX, FY, FZ, F2, MAS2, FACM1, PPLUS,GAP2,PENE2,PREC
C-----------------------------------------------
#include "sync_dir.inc"
      IF (IRESP .EQ. 1) THEN
         PREC = FIVEEM4
      ELSE 
         PREC = EM10
      ENDIF
        DO I=1,JLT
	  NM1 = M1(I)
	  NM2 = M2(I)
	  DXM1(I) = D(1,NM1)
	  DYM1(I) = D(2,NM1)
	  DZM1(I) = D(3,NM1)
	  DXM2(I) = D(1,NM2)
	  DYM2(I) = D(2,NM2)
	  DZM2(I) = D(3,NM2)
        ENDDO
c        DO I=1,JLT
c         IF(CS_LOC(I).GT.NRTS) THEN
c	  DXS1(I) = ZERO
c	  DYS1(I) = ZERO
c	  DZS1(I) = ZERO
c	  DXS2(I) = ZERO
c	  DYS2(I) = ZERO
c	  DZS2(I) = ZERO
c         ELSE
c	  NN1 = N1(I)
c	  NN2 = N2(I)
c	  DXS1(I) = D(1,NN1)
c	  DYS1(I) = D(2,NN1)
c	  DZS1(I) = D(3,NN1)
c	  DXS2(I) = D(1,NN2)
c	  DYS2(I) = D(2,NN2)
c	  DZS2(I) = D(3,NN2)
c	 ENDIF
c        ENDDO
C	
      DO I=1,JLT
       GAP2 = GAPV(I)*GAPV(I)
       PENE2 = GAP2 - NX(I)*NX(I) - NY(I)*NY(I) - NZ(I)*NZ(I)
       PENE2 = MAX(ZERO,PENE2) 
       IF(PENE2.EQ.ZERO) STIF(I) = ZERO
      ENDDO
C
      DO I=1,JLT
         S2 = SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2)
         PENE(I) = GAPV(I) - S2
         S2 = ONE/MAX(EM30,S2)
         NX(I) = NX(I)*S2
         NY(I) = NY(I)*S2
         NZ(I) = NZ(I)*S2
      ENDDO
C
      IF(INACTI.EQ.5)THEN
       IF(IMACH.NE.3) THEN
#include "lockon.inc"
        DO I=1,JLT
        PENIS(2,CS_LOC(I)) = MAX(PENIS(2,CS_LOC(I)),HALF*PENE(I))
        PENIM(2,CM_LOC(I)) = MAX(PENIM(2,CM_LOC(I)),HALF*PENE(I))
        ENDDO
#include "lockoff.inc"
        DO I=1,JLT
         PENE(I) = PENE(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
         PENE(I) = MAX(PENE(I),ZERO)
         GAPV(I) = GAPV(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
        ENDDO
       ELSE
        DO I=1,JLT
         IF(CS_LOC(I).LE.NRTS) THEN
           PENIS(2,CS_LOC(I)) = MAX(PENIS(2,CS_LOC(I)),HALF*PENE(I))
         ELSE
           NI = CS_LOC(I)-NRTS
           PENFI(NIN)%P(2,NI) = MAX(PENFI(NIN)%P(2,NI),HALF*PENE(I))
         END IF
         PENIM(2,CM_LOC(I)) = MAX(PENIM(2,CM_LOC(I)),HALF*PENE(I))
        ENDDO
        DO I=1,JLT
         IF(CS_LOC(I).LE.NRTS) THEN
           PENE(I) = PENE(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           GAPV(I) = GAPV(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
         ELSE
           NI = CS_LOC(I)-NRTS
           PENE(I) = PENE(I) - PENFI(NIN)%P(1,NI) - PENIM(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           GAPV(I) = GAPV(I) - PENFI(NIN)%P(1,NI) - PENIM(1,CM_LOC(I))
         END IF
        END DO
       END IF
      ELSE IF(INACTI.EQ.6)THEN
       IF(IMACH.NE.3) THEN
#include "lockon.inc"
        DO I=1,JLT
        PPLUS=HALF*(PENE(I)+FIVEEM2*(GAPV(I)-PENE(I)))
        PENIS(2,CS_LOC(I)) = MAX(PENIS(2,CS_LOC(I)),PPLUS)
        PENIM(2,CM_LOC(I)) = MAX(PENIM(2,CM_LOC(I)),PPLUS)
        ENDDO
#include "lockoff.inc"
        DO I=1,JLT
         PENE(I) = PENE(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
         PENE(I) = MAX(PENE(I),ZERO)
         GAPV(I) = GAPV(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
        ENDDO
       ELSE
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
        DO I=1,JLT
         IF(CS_LOC(I).LE.NRTS) THEN
           PENE(I) = PENE(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           GAPV(I) = GAPV(I) - PENIS(1,CS_LOC(I)) - PENIM(1,CM_LOC(I))
         ELSE
           NI = CS_LOC(I)-NRTS
           PENE(I) = PENE(I) - PENFI(NIN)%P(1,NI) - PENIM(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           GAPV(I) = GAPV(I) - PENFI(NIN)%P(1,NI) - PENIM(1,CM_LOC(I))
         END IF
        END DO
       END IF
      ENDIF
C
      DO I=1,JLT
        GAPV(I) = ZEP9*GAPV(I)
        VX(I) = HS1(I)*VXS1(I) + HS2(I)*VXS2(I)
     .        - HM1(I)*VXM1(I) - HM2(I)*VXM2(I)
        VY(I) = HS1(I)*VYS1(I) + HS2(I)*VYS2(I)
     .        - HM1(I)*VYM1(I) - HM2(I)*VYM2(I)
        VZ(I) = HS1(I)*VZS1(I) + HS2(I)*VZS2(I)
     .        - HM1(I)*VZM1(I) - HM2(I)*VZM2(I)
        VN(I) = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
        DXI(I) = HS1(I)*DXS1(I) + HS2(I)*DXS2(I)
        DYI(I) = HS1(I)*DYS1(I) + HS2(I)*DYS2(I)
        DZI(I) = HS1(I)*DZS1(I) + HS2(I)*DZS2(I)
        DNI(I) = NX(I)*DXI(I) + NY(I)*DYI(I) + NZ(I)*DZI(I)
C
        DX(I) = DXI(I)- HM1(I)*DXM1(I) - HM2(I)*DXM2(I)
        DY(I) = DYI(I)- HM1(I)*DYM1(I) - HM2(I)*DYM2(I)
        DZ(I) = DZI(I)- HM1(I)*DZM1(I) - HM2(I)*DZM2(I)
        DN(I) = NX(I)*DX(I) + NY(I)*DY(I) + NZ(I)*DZ(I)
      ENDDO
C-------------------------------------------
C
      IF(IMP_INT7.GE.2)THEN
       DO I=1,JLT
         STIF(I) = HALF*STIF(I) 
       ENDDO
      ELSEIF(IMP_INT7.EQ.1)THEN
       DO I=1,JLT
        FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
        IF(( (GAPV(I)-PENE(I))/GAPV(I) ).LT.PREC .AND. 
     .                                STIF(I).GT.ZERO ) THEN
         STIF(I) = ZERO
         PENE(I)= ZERO
         IDESAC = 1
        ELSE
         STIF(I) = HALF*STIF(I) * FAC
        ENDIF
       ENDDO
      ELSE
      DO I=1,JLT
       FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
       IF(( (GAPV(I)-PENE(I))/GAPV(I) ).LT.PREC .AND. 
     .                                STIF(I).GT.ZERO ) THEN
         STIF(I) = ZERO
         PENE(I)= ZERO
         IDESAC = 1
       ELSE
        STIF(I) = HALF*STIF(I) * FAC
       ENDIF
      ENDDO
      DO I=1,JLT
          STIF(I) = STIF(I) * GAPV(I) / 
     .            MAX((GAPV(I) - PENE(I)),EM10) 
      ENDDO
C
      END IF !(IMP_INT7.GE.2)
C      
       FAC =   ABS(SCALK)   
       DO I=1,JLT
        STIF(I)=STIF(I)*FAC
        FNI(I)= -STIF(I) * DN(I) 
        FXI(I)=NX(I)*FNI(I)
        FYI(I)=NY(I)*FNI(I)
        FZI(I)=NZ(I)*FNI(I)
       ENDDO
C---------------------------------
C     FRICTION
C---------------------------------
      IF(FRIC.NE.ZERO)THEN
       DO I=1,JLT
        VNX = NX(I)*DN(I)
        VNY = NY(I)*DN(I)
        VNZ = NZ(I)*DN(I)
        VX(I) = DX(I) - VNX
        VY(I) = DY(I) - VNY
        VZ(I) = DZ(I) - VNZ
        V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
        DXT = SQRT(V2)
        AA = DXT/MAX(EM30,V2)
        T1 = VX(I)*AA
        T2 = VY(I)*AA
        T3 = VZ(I)*AA
        FTN = -FRIC*STIF(I) * DXT
        FX = FTN * T1
        FY = FTN * T2
        FZ = FTN * T3
        FXI(I)=FXI(I) + FX
        FYI(I)=FYI(I) + FY
        FZI(I)=FZI(I) + FZ
       ENDDO
      ENDIF
C      
C--------main part-------
c
       DO I=1,JLT
	NM1 = M1(I)
	NM2 = M2(I)
        A(1,NM1)=A(1,NM1)+FXI(I)*HM1(I)
        A(2,NM1)=A(2,NM1)+FYI(I)*HM1(I)
        A(3,NM1)=A(3,NM1)+FZI(I)*HM1(I)
        A(1,NM2)=A(1,NM2)+FXI(I)*HM2(I)
        A(2,NM2)=A(2,NM2)+FYI(I)*HM2(I)
        A(3,NM2)=A(3,NM2)+FZI(I)*HM2(I)
       ENDDO
C--------secnd part-------
       DO I=1,JLT
         IF(CS_LOC(I).LE.NRTS) THEN
	  NN1 = N1(I)
	  NN2 = N2(I)
          A(1,NN1)=A(1,NN1)-FXI(I)*HS1(I)
          A(2,NN1)=A(2,NN1)-FYI(I)*HS1(I)
          A(3,NN1)=A(3,NN1)-FZI(I)*HS1(I)
          A(1,NN2)=A(1,NN2)-FXI(I)*HS2(I)
          A(2,NN2)=A(2,NN2)-FYI(I)*HS2(I)
          A(3,NN2)=A(3,NN2)-FZI(I)*HS2(I)
	 ELSE
	  NN=CS_LOC(I)-NRTS
	  NS=IND_INT(NIN)%P(NN)
C---------pour temporairement diag_ss---
          NN1 = NS
	  FFI(1,NN1)=FFI(1,NN1)-FXI(I)*HS1(I)
	  FFI(2,NN1)=FFI(2,NN1)-FYI(I)*HS1(I)
	  FFI(3,NN1)=FFI(3,NN1)-FZI(I)*HS1(I)
          NN2 = NN1 + 1
	  FFI(1,NN2)= FFI(1,NN2)-FXI(I)*HS2(I)
	  FFI(2,NN2)= FFI(2,NN2)-FYI(I)*HS2(I)
	  FFI(3,NN2)= FFI(3,NN2)-FZI(I)*HS2(I)
	 ENDIF
       ENDDO
C
      RETURN
      END
