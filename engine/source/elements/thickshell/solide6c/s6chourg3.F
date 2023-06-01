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
CHD|====================================================================
Chd|====================================================================
Chd|  S6CHOUR3                      source/elements/thickshell/solide6c/s6chourg3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CHOUR3(RHO,VOL,SSP,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   F31,F32,F33,F35,F36,F37,
     .   NU ,FHOUR ,OFF,VOL0,EINT,NEL)
C-----------------------------------------------
C   M O D U L E S
C-----------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER , INTENT(IN)  :: NEL
C     REAL
      my_real, DIMENSION(MVSIZ) , INTENT(IN)  :: 
     .   X1, X2, X3, X4, X5, X6, X7, X8, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,  
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,   
     .   VZ1,VZ2,VZ3,VZ4,VZ5,VZ6,VZ7,VZ8,
     .   RHO,VOL,SSP,NU,OFF
      my_real, DIMENSION(NEL) , INTENT(IN)  :: VOL0
      my_real, DIMENSION(NEL) , INTENT(INOUT)  :: EINT
      my_real, DIMENSION(MVSIZ) , INTENT(INOUT)  :: 
     .   F31,F32,F33,F35,F36,F37
      my_real, DIMENSION(NEL,2) , INTENT(INOUT)  :: FHOUR
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER :: I,J
C                                                                     12
      my_real
     .   DETT ,  
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),JAC_19_37(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35 ,
     .   HX,HY,HZ,H1X,H1Y,H1Z,H2X,H2Y,H2Z,H3Y,H3Z,H4X,H4Y,H4Z,
     .   PX1(MVSIZ), PX2(MVSIZ), PX3(MVSIZ), PX4(MVSIZ),  
     .   PY1(MVSIZ), PY2(MVSIZ), PY3(MVSIZ), PY4(MVSIZ),  
     .   PZ1(MVSIZ), PZ2(MVSIZ), PZ3(MVSIZ), PZ4(MVSIZ),  
     .   PX1H1(MVSIZ),PX2H1(MVSIZ),PX3H1(MVSIZ),PX4H1(MVSIZ),   
     .   PX1H2(MVSIZ),PX2H2(MVSIZ),PX3H2(MVSIZ),PX4H2(MVSIZ),   
     .   JAC1(MVSIZ),JAC2(MVSIZ),JAC3(MVSIZ),
     .   JAC4(MVSIZ),JAC5(MVSIZ),JAC6(MVSIZ),
     .   JAC7(MVSIZ),JAC8(MVSIZ),JAC9(MVSIZ),DET(MVSIZ)
      my_real
     .   G11(MVSIZ),G21(MVSIZ),G31(MVSIZ),G41(MVSIZ),
     .   G51(MVSIZ),G61(MVSIZ),G71(MVSIZ),G81(MVSIZ),
     .   G12(MVSIZ),G22(MVSIZ),G32(MVSIZ),G42(MVSIZ),
     .   G52(MVSIZ),G62(MVSIZ),G72(MVSIZ),G82(MVSIZ),
     .   FCL(MVSIZ),NFHZ1(MVSIZ),NFHZ2(MVSIZ),VZ17,VZ28,VZ35,VZ46,
     .   H1VZ,H2VZ,HGZ1(MVSIZ),HGZ2(MVSIZ),CC,
     .   GG ,E_DT(MVSIZ),THK,THK_1(MVSIZ)
C=======================================================================
      DO I=1,NEL
        X17(I)=X7(I)-X1(I)
        X28(I)=X8(I)-X2(I)
        X35(I)=X5(I)-X3(I)
        X46(I)=X6(I)-X4(I)
        Y17(I)=Y7(I)-Y1(I)
        Y28(I)=Y8(I)-Y2(I)
        Y35(I)=Y5(I)-Y3(I)
        Y46(I)=Y6(I)-Y4(I)
        Z17(I)=Z7(I)-Z1(I)
        Z28(I)=Z8(I)-Z2(I)
        Z35(I)=Z5(I)-Z3(I)
        Z46(I)=Z6(I)-Z4(I)
      END DO
C
C JACOBIAN MATRIX
      DO I=1,NEL
C  -------RI.XI-----------
        JAC3(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JAC1(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JAC2(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
C-------
        X_17_46=X17(I)+X46(I)
        X_28_35=X28(I)+X35(I)
        Y_17_46=Y17(I)+Y46(I)
        Y_28_35=Y28(I)+Y35(I)
        Z_17_46=Z17(I)+Z46(I)
        Z_28_35=Z28(I)+Z35(I)
C  -------SI.XI-----------
        JAC6(I)=X_17_46+X_28_35
        JAC4(I)=Y_17_46+Y_28_35
        JAC5(I)=Z_17_46+Z_28_35
C  -------TI.XI-----------
        JAC9(I)=X_17_46-X_28_35
        JAC7(I)=Y_17_46-Y_28_35
        JAC8(I)=Z_17_46-Z_28_35
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_19_37(I)=JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
        DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
        THK =FOURTH*JAC5(I)
        THK_1(I) =ONE/THK
      ENDDO
C JACOBIAN MATRIX INVERSE 
      DO I=1,NEL
        DETT=ONE_OVER_64/DET(I)
        JACI1=DETT*JAC_59_68(I)
        JACI4=DETT*JAC_67_49(I)
        JACI7=DETT*JAC_48_57(I)
        JACI2=DETT*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JACI5=DETT*JAC_19_37(I)
        JACI8=DETT*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JACI3=DETT*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JACI6=DETT*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JACI9=DETT*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
C
        JACI12=JACI1-JACI2
        JACI45=JACI4-JACI5
        JACI78=JACI7-JACI8
C
        PY3(I)= JACI12+JACI3
        PZ3(I)= JACI45+JACI6
        PX3(I)= JACI78+JACI9
        PY4(I)= JACI12-JACI3
        PZ4(I)= JACI45-JACI6
        PX4(I)= JACI78-JACI9

        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8

        PY1(I)=-JACI12-JACI3
        PZ1(I)=-JACI45-JACI6
        PX1(I)=-JACI78-JACI9
        PY2(I)=-JACI12+JACI3
        PZ2(I)=-JACI45+JACI6
        PX2(I)=-JACI78+JACI9
      ENDDO
C   H1
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         H1X=X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I)
         H1Y=Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I)
         H1Z=Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I)
         HX=ONE_OVER_8*H1X
         HY=ONE_OVER_8*H1Y
         HZ=ONE_OVER_8*H1Z
         PX1H1(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H1(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H1(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H1(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
       DO I=1,NEL
         G11(I)= ONE_OVER_8-PX1H1(I)
         G21(I)= ONE_OVER_8-PX2H1(I)
         G31(I)=-ONE_OVER_8-PX3H1(I)
         G41(I)=-ONE_OVER_8-PX4H1(I)
         G51(I)=-ONE_OVER_8+PX3H1(I)
         G61(I)=-ONE_OVER_8+PX4H1(I)
         G71(I)= ONE_OVER_8+PX1H1(I)
         G81(I)= ONE_OVER_8+PX2H1(I)	
       ENDDO
C   h2
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         H2X=X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I)
         H2Y=Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I)
         H2Z=Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I)
         HX=ONE_OVER_8*H2X
         HY=ONE_OVER_8*H2Y
         HZ=ONE_OVER_8*H2Z	 
         PX1H2(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H2(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H2(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H2(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
       DO I=1,NEL
         G12(I)= ONE_OVER_8-PX1H2(I)
         G22(I)=-ONE_OVER_8-PX2H2(I)
         G32(I)=-ONE_OVER_8-PX3H2(I)
         G42(I)= ONE_OVER_8-PX4H2(I)
         G52(I)=-ONE_OVER_8+PX3H2(I)
         G62(I)= ONE_OVER_8+PX4H2(I)
         G72(I)= ONE_OVER_8+PX1H2(I)
         G82(I)=-ONE_OVER_8+PX2H2(I)	
       ENDDO
       DO I=1,NEL
         VZ17=VZ1(I)-VZ7(I)
         VZ28=VZ2(I)-VZ8(I)
         VZ35=VZ3(I)-VZ5(I)
         VZ46=VZ4(I)-VZ6(I)
         H1VZ=VZ1(I)+VZ2(I)-VZ3(I)-VZ4(I)
     .           -VZ5(I)-VZ6(I)+VZ7(I)+VZ8(I)
         HGZ1(I)=ONE_OVER_8*H1VZ-
     .         (PX1H1(I)*VZ17+PX2H1(I)*VZ28+
     .          PX3H1(I)*VZ35+PX4H1(I)*VZ46)
         H2VZ=VZ1(I)-VZ2(I)-VZ3(I)+VZ4(I)
     .         -VZ5(I)+VZ6(I)+VZ7(I)-VZ8(I)
         HGZ2(I)=ONE_OVER_8*H2VZ-
     .         (PX1H2(I)*VZ17+PX2H2(I)*VZ28+
     .          PX3H2(I)*VZ35+PX4H2(I)*VZ46)
       ENDDO
C
       DO I=1,NEL
         GG =RHO(I)*SSP(I)*SSP(I)*(ONE-TWO*NU(I))/(ONE-NU(I))     
         E_DT(I)=GG*OFF(I)*DT1*(ONE +NU(I))
       ENDDO
      DO I=1,NEL
        CC =ZEP1*E_DT(I)*THK_1(I)
        FHOUR(I,1) =  OFF(I)*FHOUR(I,1)  + CC*HGZ1(I)
        FHOUR(I,2) =  OFF(I)*FHOUR(I,2)  + CC*HGZ2(I)
      ENDDO
      DO I=1,NEL
        CC = THK_1(I)*VOL(I)
        FCL(I)=EM02*RHO(I)*OFF(I)*SSP(I)*VOL(I)**TWO_THIRD ! TWO_THIRD=2/3
        NFHZ1(I) = CC*FHOUR(I,1) + FCL(I)*HGZ1(I)
        NFHZ2(I) = CC*FHOUR(I,2) + FCL(I)*HGZ2(I)
      ENDDO
C------------------------------------------------
C
      DO I=1,NEL
        F31(I) =F31(I)-G11(I)*NFHZ1(I)-G12(I)*NFHZ2(I)
        F32(I) =F32(I)-G21(I)*NFHZ1(I)-G22(I)*NFHZ2(I)
        F33(I) =F33(I)-G31(I)*NFHZ1(I)-G32(I)*NFHZ2(I)
     .                -G41(I)*NFHZ1(I)-G42(I)*NFHZ2(I) ! F34
        F35(I) =F35(I)-G51(I)*NFHZ1(I)-G52(I)*NFHZ2(I)
        F36(I) =F36(I)-G61(I)*NFHZ1(I)-G62(I)*NFHZ2(I)
        F37(I) =F37(I)-G71(I)*NFHZ1(I)-G72(I)*NFHZ2(I)
     .                -G81(I)*NFHZ1(I)-G82(I)*NFHZ2(I) ! F38
      ENDDO
      DO I=1,NEL
        EINT(I)= EINT(I)+DT1*(
     .     NFHZ1(I)*HGZ1(I) + NFHZ2(I)*HGZ2(I) ) 
     .   /MAX(EM20,VOL0(I)) 
      ENDDO
C-----------
      RETURN
C-----------
      END SUBROUTINE S6CHOUR3
