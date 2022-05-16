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
Chd|  FVBRIC                        source/airbag/fvbric.F        
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        SPMD_FVB_GATH                 source/mpi/airbags/spmd_fvb_gath.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE FVBRIC(IVOLU, RVOLU, IBUF, X, NN)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IVOLU(*), IBUF(*), NN
      my_real
     .        RVOLU(*), X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, NBX, NBY, NBRIC, NNB, IFV, NNB1, J, K, NV, NALL,
     .        NC, TNC(4), TNV(5,6), KK, L, N1, N2, N3, N4, N5, N6, N7,
     .        N8, I1, I2 
      my_real
     .        VX3, VY3, VZ3, VX1, VY1, VZ1, NORM, SS, VX2, VY2, VZ2,
     .        X0, Y0, Z0, ZLMIN, ZLMAX, XX, YY, ZZ, ZL, LX, LY, DX, DY,
     .        XLC, YLC, ZLC1, ZLC2, XXG, YYG, ZZG, XC, YC, ZC, L1, L2,
     .        L3, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4, 
     .        NX1, NY1, NZ1, NX2, NY2, NZ2, AREA1, AREA2, NX, NY, NZ,
     .        RR, XCF, YCF, ZCF, VX, VY, VZ, XXX(3,NN), RBID
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG
C
      INTEGER FAC(4,6)
      DATA FAC /1,4,3,2,
     .          5,6,7,8,
     .          1,2,6,5,
     .          2,3,7,6,
     .          3,4,8,7,
     .          4,1,5,8/
C
      IFV=IVOLU(45)
      IF (NSPMD == 1) THEN
C traitement necessaire pour rester p/on
         DO I=1,FVSPMD(IFV)%NN_L
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            XXX(1,I1)=X(1,I2)
            XXX(2,I1)=X(2,I2)
            XXX(3,I1)=X(3,I2)
         ENDDO
c         DO I=1,NN
c            II=IBUF(I)
c            XXX(1,I)=X(1,II)
c            XXX(2,I)=X(2,II)
c            XXX(3,I)=X(3,II)
c         ENDDO
      ELSE
         CALL SPMD_FVB_GATH(IFV, X, XXX, RBID, RBID,
     .                      1  )
         IF (ISPMD.NE.FVSPMD(IFV)%PMAIN-1) RETURN
      ENDIF
      VX3=RVOLU(35)
      VY3=RVOLU(36)
      VZ3=RVOLU(37)
      VX1=RVOLU(38)
      VY1=RVOLU(39)
      VZ1=RVOLU(40)
C Repere local
      NORM=SQRT(VX3**2+VY3**2+VZ3**2)
      VX3=VX3/NORM
      VY3=VY3/NORM
      VZ3=VZ3/NORM
      SS=VX3*VX1+VY3*VY1+VZ3*VZ1
      VX1=VX1-SS*VX3
      VY1=VY1-SS*VY3
      VZ1=VZ1-SS*VZ3
      NORM=SQRT(VX1**2+VY1**2+VZ1**2)
      VX1=VX1/NORM
      VY1=VY1/NORM
      VZ1=VZ1/NORM
      VX2=VY3*VZ1-VZ3*VY1
      VY2=VZ3*VX1-VX3*VZ1
      VZ2=VX3*VY1-VY3*VX1
C
      X0=RVOLU(41)
      Y0=RVOLU(42)
      Z0=RVOLU(43)
C
      ZLMIN=EP30
      ZLMAX=-EP30
      DO I=1,NN
         XX=XXX(1,I)
         YY=XXX(2,I)
         ZZ=XXX(3,I)
         ZL=(XX-X0)*VX3+(YY-Y0)*VY3+(ZZ-Z0)*VZ3
         ZLMIN=MIN(ZLMIN,ZL)
         ZLMAX=MAX(ZLMAX,ZL)
      ENDDO
C
      IFV=IVOLU(45)
      NBX=IVOLU(54)
      NBY=IVOLU(55)
C
      LX=RVOLU(44)
      LY=RVOLU(45)
      DX=DEUX*LX/NBX
      DY=DEUX*LY/NBY
      ZLC1=ZLMIN-MAX(LX,LY)
      ZLC2=ZLMAX+MAX(LX,LY)
      NNB=0
      NNB1=(NBX+1)*(NBY+1)
      DO I=1,NBY+1
         YLC=-LY+(I-1)*DY
         DO J=1,NBX+1
            XLC=-LX+(J-1)*DX
            NNB=NNB+1
            FVDATA(IFV)%XB(1,NNB)=XLC
            FVDATA(IFV)%XB(2,NNB)=YLC
            FVDATA(IFV)%XB(3,NNB)=ZLC1
            FVDATA(IFV)%XB(1,NNB1+NNB)=XLC
            FVDATA(IFV)%XB(2,NNB1+NNB)=YLC
            FVDATA(IFV)%XB(3,NNB1+NNB)=ZLC2
         ENDDO
      ENDDO
C
      NNB=(NBX+1)*(NBY+1)*2
      DO I=1,NNB
         XX=FVDATA(IFV)%XB(1,I)
         YY=FVDATA(IFV)%XB(2,I)
         ZZ=FVDATA(IFV)%XB(3,I)
         XXG=X0+XX*VX1+YY*VX2+ZZ*VX3
         YYG=Y0+XX*VY1+YY*VY2+ZZ*VY3
         ZZG=Z0+XX*VZ1+YY*VZ2+ZZ*VZ3
         FVDATA(IFV)%XB(1,I)=XXG
         FVDATA(IFV)%XB(2,I)=YYG
         FVDATA(IFV)%XB(3,I)=ZZG
      ENDDO
C
      NBRIC=0
      DO I=1,NBY
         DO J=1,NBX
            NBRIC=NBRIC+1
            FVDATA(IFV)%BRIC(1,NBRIC)=(I-1)*(NBX+1)+J
            FVDATA(IFV)%BRIC(2,NBRIC)=(I-1)*(NBX+1)+J+1
            FVDATA(IFV)%BRIC(3,NBRIC)=I*(NBX+1)+J+1
            FVDATA(IFV)%BRIC(4,NBRIC)=I*(NBX+1)+J
            FVDATA(IFV)%BRIC(5,NBRIC)=NNB1+(I-1)*(NBX+1)+J
            FVDATA(IFV)%BRIC(6,NBRIC)=NNB1+(I-1)*(NBX+1)+J+1
            FVDATA(IFV)%BRIC(7,NBRIC)=NNB1+I*(NBX+1)+J+1
            FVDATA(IFV)%BRIC(8,NBRIC)=NNB1+I*(NBX+1)+J
         ENDDO
      ENDDO
C
      DO I=1,NBRIC
         DO J=1,6
            FVDATA(IFV)%TBRIC(J,I)=0
            DO K=1,4
               FVDATA(IFV)%SFAC(J,K,I)=ZERO
            ENDDO
         ENDDO
         DO J=1,7
            FVDATA(IFV)%TBRIC(6+J,I)=0
         ENDDO
      ENDDO
C
      ALLOCATE(ITAG(NNB))
      DO I=1,NNB
         ITAG(I)=0
      ENDDO
C
      DO I=1,NBRIC
         DO J=1,8
            ITAG(FVDATA(IFV)%BRIC(J,I))=1
         ENDDO
         NV=0
         DO J=1,NBRIC
            IF (J.EQ.I) CYCLE
            NALL=0
            NC=0
            DO K=1,8
               IF (ITAG(FVDATA(IFV)%BRIC(K,J)).EQ.1) THEN
                  NC=NC+1
                  TNC(NC)=FVDATA(IFV)%BRIC(K,J)
               ENDIF
               NALL=NALL+ITAG(FVDATA(IFV)%BRIC(K,J))
            ENDDO
            IF (NALL.NE.4) CYCLE
            NV=NV+1
            TNV(1,NV)=J
            TNV(2,NV)=TNC(1)
            TNV(3,NV)=TNC(2)
            TNV(4,NV)=TNC(3)
            TNV(5,NV)=TNC(4)
         ENDDO
         DO J=1,8
            ITAG(FVDATA(IFV)%BRIC(J,I))=0
         ENDDO
         DO J=1,6
            DO K=1,4
               KK=FAC(K,J)
               ITAG(FVDATA(IFV)%BRIC(KK,I))=1
            ENDDO
            DO K=1,NV
               NALL=1
               DO L=1,4
                  NALL=NALL*ITAG(TNV(1+L,K))
               ENDDO
               IF (NALL.EQ.1) FVDATA(IFV)%TBRIC(J,I)=TNV(1,K)
            ENDDO
            DO K=1,4
               KK=FAC(K,J)
               ITAG(FVDATA(IFV)%BRIC(KK,I))=0
            ENDDO
         ENDDO
      ENDDO
      DEALLOCATE(ITAG)
C
      DO I=1,NBRIC
         N1=FVDATA(IFV)%BRIC(1,I)
         N2=FVDATA(IFV)%BRIC(2,I)
         N3=FVDATA(IFV)%BRIC(3,I)
         N4=FVDATA(IFV)%BRIC(4,I)
         N5=FVDATA(IFV)%BRIC(5,I)
         N6=FVDATA(IFV)%BRIC(6,I)
         N7=FVDATA(IFV)%BRIC(7,I)
         N8=FVDATA(IFV)%BRIC(8,I)
         XC=UNHUIT*(FVDATA(IFV)%XB(1,N1)+FVDATA(IFV)%XB(1,N2)+
     .              FVDATA(IFV)%XB(1,N3)+FVDATA(IFV)%XB(1,N4)+
     .              FVDATA(IFV)%XB(1,N5)+FVDATA(IFV)%XB(1,N6)+
     .              FVDATA(IFV)%XB(1,N7)+FVDATA(IFV)%XB(1,N8))
         YC=UNHUIT*(FVDATA(IFV)%XB(2,N1)+FVDATA(IFV)%XB(2,N2)+
     .              FVDATA(IFV)%XB(2,N3)+FVDATA(IFV)%XB(2,N4)+
     .              FVDATA(IFV)%XB(2,N5)+FVDATA(IFV)%XB(2,N6)+
     .              FVDATA(IFV)%XB(2,N7)+FVDATA(IFV)%XB(2,N8))
         ZC=UNHUIT*(FVDATA(IFV)%XB(3,N1)+FVDATA(IFV)%XB(3,N2)+
     .              FVDATA(IFV)%XB(3,N3)+FVDATA(IFV)%XB(3,N4)+
     .              FVDATA(IFV)%XB(3,N5)+FVDATA(IFV)%XB(3,N6)+
     .              FVDATA(IFV)%XB(3,N7)+FVDATA(IFV)%XB(3,N8))
C
         IF (I.EQ.1) THEN
            VX1=FVDATA(IFV)%XB(1,N2)-FVDATA(IFV)%XB(1,N1)
            VY1=FVDATA(IFV)%XB(2,N2)-FVDATA(IFV)%XB(2,N1)
            VZ1=FVDATA(IFV)%XB(3,N2)-FVDATA(IFV)%XB(3,N1)
            VX2=FVDATA(IFV)%XB(1,N4)-FVDATA(IFV)%XB(1,N1)     
            VY2=FVDATA(IFV)%XB(2,N4)-FVDATA(IFV)%XB(2,N1)
            VZ2=FVDATA(IFV)%XB(3,N4)-FVDATA(IFV)%XB(3,N1)
            VX3=FVDATA(IFV)%XB(1,N5)-FVDATA(IFV)%XB(1,N1)     
            VY3=FVDATA(IFV)%XB(2,N5)-FVDATA(IFV)%XB(2,N1)
            VZ3=FVDATA(IFV)%XB(3,N5)-FVDATA(IFV)%XB(3,N1)
            L1=SQRT(VX1**2+VY1**2+VZ1**2)
            L2=SQRT(VX2**2+VY2**2+VZ2**2)
            L3=SQRT(VX3**2+VY3**2+VZ3**2)
            FVDATA(IFV)%DLH=MIN(L1,L2,L3)
         ENDIF
C
         DO J=1,6
            N1=FVDATA(IFV)%BRIC(FAC(1,J),I)         
            N2=FVDATA(IFV)%BRIC(FAC(2,J),I)         
            N3=FVDATA(IFV)%BRIC(FAC(3,J),I)         
            N4=FVDATA(IFV)%BRIC(FAC(4,J),I)
            X1=FVDATA(IFV)%XB(1,N1)
            Y1=FVDATA(IFV)%XB(2,N1)
            Z1=FVDATA(IFV)%XB(3,N1)        
            X2=FVDATA(IFV)%XB(1,N2)
            Y2=FVDATA(IFV)%XB(2,N2)
            Z2=FVDATA(IFV)%XB(3,N2)        
            X3=FVDATA(IFV)%XB(1,N3)
            Y3=FVDATA(IFV)%XB(2,N3)
            Z3=FVDATA(IFV)%XB(3,N3)        
            X4=FVDATA(IFV)%XB(1,N4)
            Y4=FVDATA(IFV)%XB(2,N4)
            Z4=FVDATA(IFV)%XB(3,N4)
C
            VX1=X2-X1
            VY1=Y2-Y1
            VZ1=Z2-Z1
            VX2=X3-X1
            VY2=Y3-Y1
            VZ2=Z3-Z1
            VX3=X4-X1
            VY3=Y4-Y1
            VZ3=Z4-Z1
            NX1=VY1*VZ2-VZ1*VY2
            NY1=VZ1*VX2-VX1*VZ2
            NZ1=VX1*VY2-VY1*VX2
            NX2=VY2*VZ3-VZ2*VY3
            NY2=VZ2*VX3-VX2*VZ3
            NZ2=VX2*VY3-VY2*VX3
            AREA1=UNDEMI*SQRT(NX1**2+NY1**2+NZ1**2)
            AREA2=UNDEMI*SQRT(NX2**2+NY2**2+NZ2**2)
            FVDATA(IFV)%SFAC(J,1,I)=AREA1+AREA2
C
            NX=UNDEMI*(NX1+NX2)
            NY=UNDEMI*(NY1+NY2)
            NZ=UNDEMI*(NZ1+NZ2)
            RR=SQRT(NX**2+NY**2+NZ**2)
            XCF=QUART*(X1+X2+X3+X4)
            YCF=QUART*(Y1+Y2+Y3+Y4)
            ZCF=QUART*(Z1+Z2+Z3+Z4)
            VX=XC-XCF
            VY=YC-YCF
            VZ=ZC-ZCF
            SS=VX*NX+VY*NY+VZ*NZ
            IF (SS.LE.ZERO) THEN
               FVDATA(IFV)%SFAC(J,2,I)=NX/RR
               FVDATA(IFV)%SFAC(J,3,I)=NY/RR
               FVDATA(IFV)%SFAC(J,4,I)=NZ/RR
            ELSE
               FVDATA(IFV)%SFAC(J,2,I)=-NX/RR
               FVDATA(IFV)%SFAC(J,3,I)=-NY/RR
               FVDATA(IFV)%SFAC(J,4,I)=-NZ/RR
            ENDIF   
         ENDDO
      ENDDO
C
      RETURN
      END
