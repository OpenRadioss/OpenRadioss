Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CZDEF                         source/elements/shell/coquez/czdef.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZDEF(JFT ,JLT  ,AREA ,AA  ,V13   ,V24 ,
     2                   VHI ,RLXYZ,VDEF ,VHG ,X13   ,
     3                   X24 ,Y13  ,Y24  ,MX13,MX23  ,MX34,
     4                   MY13,MY23 ,MY34 ,Z1  ,DT1   , OFF, 
     5                   OFFG,RLXYZV )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc" 
#include      "parit_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   RLXYZ(MVSIZ,2,4),V13(MVSIZ,3),V24(MVSIZ,3),
     .   X13(*),X24(*),Y13(*),Y24(*),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*)
      my_real 
     .   AREA(*),Z1(*),VDEF(MVSIZ,8),
     .   AA(*),VHG(MVSIZ,6),VHI(MVSIZ,3),DT1,OFF(*),OFFG(*),OFF_L
      my_real
     .  RLXYZV(MVSIZ,2,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   BCX,BCY,BCXY,R13(2),R24(2),DETA1,
     .   RSOM(2),RHI(2),
     .   BXV2,BYV1,BXR1,BYR2
C-----------------------------------------------
      IF (IVECTOR==1) THEN
       DO I=JFT,JLT
        R13(1) =(RLXYZV(I,1,1)-RLXYZV(I,1,3))*AA(I)
        R24(1) =(RLXYZV(I,1,2)-RLXYZV(I,1,4))*AA(I)
        RSOM(1)=(RLXYZV(I,1,4)+RLXYZV(I,1,3)+RLXYZV(I,1,1)
     .         +RLXYZV(I,1,2))*AA(I)
        RHI(1) =(RLXYZV(I,1,1)-RLXYZV(I,1,2)+RLXYZV(I,1,3)
     .         -RLXYZV(I,1,4))*FOURTH
        R13(2) =(RLXYZV(I,2,1)-RLXYZV(I,2,3))*AA(I)
        R24(2) =(RLXYZV(I,2,2)-RLXYZV(I,2,4))*AA(I)
        RSOM(2)=(RLXYZV(I,2,4)+RLXYZV(I,2,3)+RLXYZV(I,2,1)
     .         +RLXYZV(I,2,2))*AA(I)
        RHI(2) =(RLXYZV(I,2,1)-RLXYZV(I,2,2)+RLXYZV(I,2,3)
     .         - RLXYZV(I,2,4))*FOURTH
C---------------
C  MEMBRANE
C---------------
        VDEF(I,1)= Y24(I)*V13(I,1)-Y13(I)*V24(I,1)
        VDEF(I,2)=-X24(I)*V13(I,2)+X13(I)*V24(I,2)
        BXV2= Y24(I)*V13(I,2)-Y13(I)*V24(I,2)
        BYV1=-X24(I)*V13(I,1)+X13(I)*V24(I,1)
        VDEF(I,3)=BXV2+BYV1
C---------
C  FLEXION
C---------
        VDEF(I,6)=Y24(I)*R13(2)-Y13(I)*R24(2)
        VDEF(I,7)=X24(I)*R13(1)-X13(I)*R24(1)
        BXR1= Y13(I)*R24(1)-Y24(I)*R13(1)
        BYR2=-X24(I)*R13(2)+X13(I)*R24(2)
        VDEF(I,8)=BXR1+BYR2
C--------
C  C.T
C--------
        BCXY=AREA(I)*FOURTH
        BCX=V13(I,3)-MY13(I)*R13(1)+MX13(I)*R13(2)
        BCY=V24(I,3)+MY13(I)*R24(1)-MX13(I)*R24(2)
        VDEF(I,4)=Y24(I)*BCX-Y13(I)*BCY+BCXY*RSOM(2)
        VDEF(I,5)=X13(I)*BCY-X24(I)*BCX-BCXY*RSOM(1)
C----------------------------------
C  VITESSE DE DEFORMATION HOURGLASS
C----------------------------------
        VHG(I,1)=VHI(I,1)-MX13(I)*VDEF(I,1)-MY13(I)*BYV1     
        VHG(I,2)=VHI(I,2)-MX13(I)*BXV2     -MY13(I)*VDEF(I,2)
        VHG(I,3)= RHI(2)-MX13(I)*VDEF(I,6)-MY13(I)*BYR2     
        VHG(I,4)=-RHI(1)-MX13(I)*BXR1     -MY13(I)*VDEF(I,7)
        VHG(I,5)=(VHI(I,3)*4.-(MY13(I)*RSOM(1)-MY23(I)*(R13(1)+R24(1))
     1         +MX23(I)*(R13(2)+R24(2))-MX13(I)*RSOM(2))*AREA(I))*FOUR
        VHG(I,6)=(VHI(I,3)*4.-(MY13(I)*RSOM(1)-MY34(I)*(R13(1)-R24(1))
     1         +MX34(I)*(R13(2)-R24(2))-MX13(I)*RSOM(2))*AREA(I))*FOUR
C 
        VHG(I,1)=VHG(I,1)+( Y24(I)*V13(I,3)-Y13(I)*V24(I,3))*Z1(I)
        VHG(I,2)=VHG(I,2)+(-X24(I)*V13(I,3)+X13(I)*V24(I,3))*Z1(I)
       ENDDO
      ELSE
      DO I=JFT,JLT
        R13(1) =(RLXYZ(I,1,1)-RLXYZ(I,1,3))*AA(I)
        R24(1) =(RLXYZ(I,1,2)-RLXYZ(I,1,4))*AA(I)
        RSOM(1)=(RLXYZ(I,1,4)+RLXYZ(I,1,3)+RLXYZ(I,1,1)+RLXYZ(I,1,2))
     .         *AA(I)
        RHI(1) =(RLXYZ(I,1,1)-RLXYZ(I,1,2)+RLXYZ(I,1,3)-RLXYZ(I,1,4))
     .         *FOURTH
        R13(2) =(RLXYZ(I,2,1)-RLXYZ(I,2,3))*AA(I)
        R24(2) =(RLXYZ(I,2,2)-RLXYZ(I,2,4))*AA(I)
        RSOM(2)=(RLXYZ(I,2,4)+RLXYZ(I,2,3)+RLXYZ(I,2,1)+RLXYZ(I,2,2))
     .         *AA(I)
        RHI(2) =(RLXYZ(I,2,1)-RLXYZ(I,2,2)+RLXYZ(I,2,3)-RLXYZ(I,2,4))
     .         *FOURTH
C---------------
C  MEMBRANE
C---------------
        VDEF(I,1)= Y24(I)*V13(I,1)-Y13(I)*V24(I,1)
        VDEF(I,2)=-X24(I)*V13(I,2)+X13(I)*V24(I,2)
        BXV2= Y24(I)*V13(I,2)-Y13(I)*V24(I,2)
        BYV1=-X24(I)*V13(I,1)+X13(I)*V24(I,1)
        VDEF(I,3)=BXV2+BYV1
C---------
C  FLEXION
C---------
        VDEF(I,6)=Y24(I)*R13(2)-Y13(I)*R24(2)
        VDEF(I,7)=X24(I)*R13(1)-X13(I)*R24(1)
        BXR1= Y13(I)*R24(1)-Y24(I)*R13(1)
        BYR2=-X24(I)*R13(2)+X13(I)*R24(2)
        VDEF(I,8)=BXR1+BYR2
C--------
C  C.T
C--------
        BCXY=AREA(I)*FOURTH
        BCX=V13(I,3)-MY13(I)*R13(1)+MX13(I)*R13(2)
        BCY=V24(I,3)+MY13(I)*R24(1)-MX13(I)*R24(2)
        VDEF(I,4)=Y24(I)*BCX-Y13(I)*BCY+BCXY*RSOM(2)
        VDEF(I,5)=X13(I)*BCY-X24(I)*BCX-BCXY*RSOM(1)
C----------------------------------
C  VITESSE DE DEFORMATION HOURGLASS
C----------------------------------
        VHG(I,1)=VHI(I,1)-MX13(I)*VDEF(I,1)-MY13(I)*BYV1     
        VHG(I,2)=VHI(I,2)-MX13(I)*BXV2     -MY13(I)*VDEF(I,2)
        VHG(I,3)= RHI(2)-MX13(I)*VDEF(I,6)-MY13(I)*BYR2     
        VHG(I,4)=-RHI(1)-MX13(I)*BXR1     -MY13(I)*VDEF(I,7)
        VHG(I,5)=(VHI(I,3)*4.-(MY13(I)*RSOM(1)-MY23(I)*(R13(1)+R24(1))
     1         +MX23(I)*(R13(2)+R24(2))-MX13(I)*RSOM(2))*AREA(I))*FOUR
        VHG(I,6)=(VHI(I,3)*4.-(MY13(I)*RSOM(1)-MY34(I)*(R13(1)-R24(1))
     1         +MX34(I)*(R13(2)-R24(2))-MX13(I)*RSOM(2))*AREA(I))*FOUR
C 
        VHG(I,1)=VHG(I,1)+( Y24(I)*V13(I,3)-Y13(I)*V24(I,3))*Z1(I)
        VHG(I,2)=VHG(I,2)+(-X24(I)*V13(I,3)+X13(I)*V24(I,3))*Z1(I)
      ENDDO
      ENDIF
      DO I=JFT,JLT
       DETA1=Z1(I)*FOUR*AA(I)
       VDEF(I,6)=VDEF(I,6)+(X13(I)*V13(I,1)-X24(I)*V24(I,1))*DETA1
       VDEF(I,7)=VDEF(I,7)+(Y13(I)*V13(I,2)-Y24(I)*V24(I,2))*DETA1
       VDEF(I,8)=VDEF(I,8)+(X13(I)*V13(I,2)-X24(I)*V24(I,2)+
     1                      Y13(I)*V13(I,1)-Y24(I)*V24(I,1))*DETA1
      ENDDO
      OFF_L = ZERO
      DO I=JFT,JLT
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<ZERO)THEN
        DO I=JFT,JLT
         IF(OFFG(I)<ZERO)THEN
          VDEF(I,1)=ZERO
          VDEF(I,2)=ZERO
          VDEF(I,3)=ZERO
          VDEF(I,4)=ZERO
          VDEF(I,5)=ZERO
          VDEF(I,6)=ZERO
          VDEF(I,7)=ZERO
          VDEF(I,8)=ZERO
C
          VHG(I,1)=ZERO    
          VHG(I,2)=ZERO
          VHG(I,3)=ZERO     
          VHG(I,4)=ZERO
          VHG(I,5)=ZERO
          VHG(I,6)=ZERO  
         ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  CZDEFRZ                       source/elements/shell/coquez/czdef.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|        CZDERIRZ                      source/elements/shell/coquez/czdef.F
Chd|====================================================================
        SUBROUTINE CZDEFRZ(JFT ,JLT  ,AREA ,AA  ,RLZ   ,
     1                    VDEF ,VHGZK,VHGZE,X13 ,X24   ,
     2                    Y13  ,Y24  ,MX13 ,MX23,MX34  ,
     3                    MY13 ,MY23 ,MY34 ,Z1  ,VHG   ,
     4                    BM0RZ,BMKRZ,BMERZ,V13 ,V24   ,
     5                    VRLZ )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   RLZ(MVSIZ,4),V13(MVSIZ,3),V24(MVSIZ,3),
     .   X13(*),X24(*),Y13(*),Y24(*),Z1(*),VHG(MVSIZ,6),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*)
      my_real 
     .   AREA(*),VDEF(MVSIZ,8),AA(*),VHGZK(MVSIZ,5),VHGZE(MVSIZ,5),VRLZ(*),
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   AA4,BXV2,BYV1,A05,BXRZ,BYRZ,DHX,DHY,C3,HXX,HYY,HXX_K,HYY_K
C-----------------------------------------------
        CALL CZDERIRZ(JFT ,JLT  ,AREA ,X13 ,X24   ,
     2               Y13  ,Y24  ,MX13 ,MX23,MX34  ,
     3               MY13 ,MY23 ,MY34 ,BM0RZ,BMKRZ,
     4               BMERZ )
C
      DO I=JFT,JLT
C---------------
C  MEMBRANE CONSTANT
C---------------
       VDEF(I,1)=VDEF(I,1)+BM0RZ(I,1,1)*RLZ(I,1)+BM0RZ(I,1,2)*RLZ(I,2)
     1                    +BM0RZ(I,1,3)*RLZ(I,3)+BM0RZ(I,1,4)*RLZ(I,4)
       VDEF(I,2)=VDEF(I,2)+BM0RZ(I,2,1)*RLZ(I,1)+BM0RZ(I,2,2)*RLZ(I,2)
     1                    +BM0RZ(I,2,3)*RLZ(I,3)+BM0RZ(I,2,4)*RLZ(I,4)
       VDEF(I,3)=VDEF(I,3)+BM0RZ(I,3,1)*RLZ(I,1)+BM0RZ(I,3,2)*RLZ(I,2)
     1                    +BM0RZ(I,3,3)*RLZ(I,3)+BM0RZ(I,3,4)*RLZ(I,4)
      ENDDO
C--------NxI,x *A------pay attention Bi*A--- RLZ: VRZ/A--       
      DO I=JFT,JLT
        BXV2= Y24(I)*V13(I,2)-Y13(I)*V24(I,2)
        BYV1=-X24(I)*V13(I,1)+X13(I)*V24(I,1)
        VRLZ(I)=(BXV2-BYV1+
     1           BM0RZ(I,4,1)*RLZ(I,1)+BM0RZ(I,4,2)*RLZ(I,2)
     1          +BM0RZ(I,4,3)*RLZ(I,3)+BM0RZ(I,4,4)*RLZ(I,4))*HALF
      ENDDO
C---------------
C  Hourglass : 3: 4:
C---------------
      DO I=JFT,JLT
       DO J=1,4
        VHGZK(I,J)=BMKRZ(I,J,1)*RLZ(I,1)+BMKRZ(I,J,2)*RLZ(I,2)
     1            +BMKRZ(I,J,3)*RLZ(I,3)+BMKRZ(I,J,4)*RLZ(I,4)
        VHGZE(I,J)=BMERZ(I,J,1)*RLZ(I,1)+BMERZ(I,J,2)*RLZ(I,2)
     1            +BMERZ(I,J,3)*RLZ(I,3)+BMERZ(I,J,4)*RLZ(I,4)
       ENDDO
      ENDDO
C-------VHGZK(5,I) takeing into skew(D)--------
      DO I=JFT,JLT
C        DHX =VHG(I,1)-( Y24(I)*V13(I,3)-Y13(I)*V24(I,3))*Z1(I)
C        DHY =VHG(I,2)-(-X24(I)*V13(I,3)+X13(I)*V24(I,3))*Z1(I)
        DHX =VHG(I,1)
        DHY =VHG(I,2)
        C3=FOUR*AA(I)
        HXX=C3*MY34(I)
        HYY=C3*MX34(I)
        HXX_K=C3*MY23(I)
        HYY_K=C3*MX23(I)
        VHGZE(I,5)=VHGZE(I,4)+HYY*DHX+HXX*DHY
        VHGZK(I,5)=VHGZK(I,4)-HYY_K*DHX-HXX_K*DHY
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CZDERIRZ                      source/elements/shell/coquez/czdef.F
Chd|-- called by -----------
Chd|        CZBER3                        source/elements/shell/coquez/czbe3.F
Chd|        CZDEFRZ                       source/elements/shell/coquez/czdef.F
Chd|-- calls ---------------
Chd|        CBPATCH                       source/elements/shell/coqueba/cbadef.F
Chd|====================================================================
        SUBROUTINE CZDERIRZ(JFT ,JLT  ,AREA ,X13 ,X24   ,
     2                     Y13  ,Y24  ,MX13 ,MX23,MX34  ,
     3                     MY13 ,MY23 ,MY34 ,BM0RZ,BMKRZ,
     4                     BMERZ )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   X13(*),X24(*),Y13(*),Y24(*),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*)
      my_real 
     .   AREA(*),
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   AA4,BXV2,BYV1,A05,NXY,NYX,A05K(4),A05E(4),
     .   XIJ(MVSIZ,4),YIJ(MVSIZ,4),XIL(MVSIZ,4),YIL(MVSIZ,4)
C--------(14,23,32,41)/4---------------------------------------
      DO I=JFT,JLT
       XIJ(I,1) = HALF*(MX13(I)-MX34(I))
       XIJ(I,2) = HALF*(X13(I)+MX23(I)-MX13(I))
       XIJ(I,3) = -XIJ(I,2)
       XIJ(I,4) = -XIJ(I,1)
       YIJ(I,1) = HALF*(MY13(I)-MY34(I))
       YIJ(I,2) = HALF*(Y13(I)+MY23(I)-MY13(I))
       YIJ(I,3) = -YIJ(I,2)
       YIJ(I,4) = -YIJ(I,1)
C--------(12,21,34,43)/4---------------------------------------
       XIL(I,1) = HALF*(MX13(I)-MX23(I))
       XIL(I,2) = -XIL(I,1)
       XIL(I,3) = HALF*(MX13(I)-MX34(I)-X13(I))
       XIL(I,4) = -XIL(I,3)
       YIL(I,1) = HALF*(MY13(I)-MY23(I))
       YIL(I,2) = -YIL(I,1)
       YIL(I,3) = HALF*(MY13(I)-MY34(I)-Y13(I))
       YIL(I,4) = -YIL(I,3)
      ENDDO
C      
      DO I=JFT,JLT
C--------NxI,x *A------pay attention Bi*A--- RLZ: VRZ/A--       
C-------- MY34YIJ KSI - MY23YIL ETA       
       BM0RZ(I,1,1) = (-MY34(I)*YIJ(I,1)+MY23(I)*YIL(I,1))
       BM0RZ(I,1,2) = ( MY34(I)*YIJ(I,2)+MY23(I)*YIL(I,2))
       BM0RZ(I,1,3) = ( MY34(I)*YIJ(I,3)-MY23(I)*YIL(I,3))
       BM0RZ(I,1,4) = (-MY34(I)*YIJ(I,4)-MY23(I)*YIL(I,4))
C-------- MY13YIJ KSI - 2MY34YIL      
       BMKRZ(I,1,1) = (-MY13(I)*YIJ(I,1)-TWO*MY34(I)*YIL(I,1))
       BMKRZ(I,1,2) = ( MY13(I)*YIJ(I,2)-TWO*MY34(I)*YIL(I,2))
       BMKRZ(I,1,3) = ( MY13(I)*YIJ(I,3)-TWO*MY34(I)*YIL(I,3))
       BMKRZ(I,1,4) = (-MY13(I)*YIJ(I,4)-TWO*MY34(I)*YIL(I,4))
C-------- -MY13YIL ETA + 2MY23YIJ      
       BMERZ(I,1,1) = ( MY13(I)*YIL(I,1)+TWO*MY23(I)*YIJ(I,1))
       BMERZ(I,1,2) = ( MY13(I)*YIL(I,2)+TWO*MY23(I)*YIJ(I,2))
       BMERZ(I,1,3) = (-MY13(I)*YIL(I,3)+TWO*MY23(I)*YIJ(I,3))
       BMERZ(I,1,4) = (-MY13(I)*YIL(I,4)+TWO*MY23(I)*YIJ(I,4))
C -------NyI,y *A-------------      
C-------- MX34XIJ KSI - MX23XIL ETA       
       BM0RZ(I,2,1) = (-MX34(I)*XIJ(I,1)+MX23(I)*XIL(I,1))
       BM0RZ(I,2,2) = ( MX34(I)*XIJ(I,2)+MX23(I)*XIL(I,2))
       BM0RZ(I,2,3) = ( MX34(I)*XIJ(I,3)-MX23(I)*XIL(I,3))
       BM0RZ(I,2,4) = (-MX34(I)*XIJ(I,4)-MX23(I)*XIL(I,4))
C-------- MX13XIJ KSI - 2MX34XIL      
       BMKRZ(I,2,1) = (-MX13(I)*XIJ(I,1)-TWO*MX34(I)*XIL(I,1))
       BMKRZ(I,2,2) = ( MX13(I)*XIJ(I,2)-TWO*MX34(I)*XIL(I,2))
       BMKRZ(I,2,3) = ( MX13(I)*XIJ(I,3)-TWO*MX34(I)*XIL(I,3))
       BMKRZ(I,2,4) = (-MX13(I)*XIJ(I,4)-TWO*MX34(I)*XIL(I,4))
C-------- -MX13XIL ETA + 2MX23XIJ      
       BMERZ(I,2,1) = ( MX13(I)*XIL(I,1)+TWO*MX23(I)*XIJ(I,1))
       BMERZ(I,2,2) = ( MX13(I)*XIL(I,2)+TWO*MX23(I)*XIJ(I,2))
       BMERZ(I,2,3) = (-MX13(I)*XIL(I,3)+TWO*MX23(I)*XIJ(I,3))
       BMERZ(I,2,4) = (-MX13(I)*XIL(I,4)+TWO*MX23(I)*XIJ(I,4))
C--------NxI,y *A    
C-------- -MX34YIJ KSI + MX23YIL ETA       
       BM0RZ(I,3,1) = ( MX34(I)*YIJ(I,1)-MX23(I)*YIL(I,1))
       BM0RZ(I,3,2) = (-MX34(I)*YIJ(I,2)-MX23(I)*YIL(I,2))
       BM0RZ(I,3,3) = (-MX34(I)*YIJ(I,3)+MX23(I)*YIL(I,3))
       BM0RZ(I,3,4) = ( MX34(I)*YIJ(I,4)+MX23(I)*YIL(I,4))
C-------- -MX13YIJ KSI + 2MX34YIL      
       BMKRZ(I,3,1) = ( MX13(I)*YIJ(I,1)+TWO*MX34(I)*YIL(I,1))
       BMKRZ(I,3,2) = (-MX13(I)*YIJ(I,2)+TWO*MX34(I)*YIL(I,2))
       BMKRZ(I,3,3) = (-MX13(I)*YIJ(I,3)+TWO*MX34(I)*YIL(I,3))
       BMKRZ(I,3,4) = ( MX13(I)*YIJ(I,4)+TWO*MX34(I)*YIL(I,4))
C-------- MX13YIL ETA - 2MX23YIJ      
       BMERZ(I,3,1) = (-MX13(I)*YIL(I,1)-TWO*MX23(I)*YIJ(I,1))
       BMERZ(I,3,2) = (-MX13(I)*YIL(I,2)-TWO*MX23(I)*YIJ(I,2))
       BMERZ(I,3,3) = ( MX13(I)*YIL(I,3)-TWO*MX23(I)*YIJ(I,3))
       BMERZ(I,3,4) = ( MX13(I)*YIL(I,4)-TWO*MX23(I)*YIJ(I,4))
C--------NyI,x *A   
C-------- -MY34XIJ KSI + MY23XIJ ETA       
       BM0RZ(I,4,1) = ( MY34(I)*XIJ(I,1)-MY23(I)*XIL(I,1))
       BM0RZ(I,4,2) = (-MY34(I)*XIJ(I,2)-MY23(I)*XIL(I,2))
       BM0RZ(I,4,3) = (-MY34(I)*XIJ(I,3)+MY23(I)*XIL(I,3))
       BM0RZ(I,4,4) = ( MY34(I)*XIJ(I,4)+MY23(I)*XIL(I,4))
C-------- -MY13XIJ KSI + 2MY34XIL      
       BMKRZ(I,4,1) = ( MY13(I)*XIJ(I,1)+TWO*MY34(I)*XIL(I,1))
       BMKRZ(I,4,2) = (-MY13(I)*XIJ(I,2)+TWO*MY34(I)*XIL(I,2))
       BMKRZ(I,4,3) = (-MY13(I)*XIJ(I,3)+TWO*MY34(I)*XIL(I,3))
       BMKRZ(I,4,4) = ( MY13(I)*XIJ(I,4)+TWO*MY34(I)*XIL(I,4))
C-------- MY13XIL ETA - 2MY23XIJ      
       BMERZ(I,4,1) = (-MY13(I)*XIL(I,1)-TWO*MY23(I)*XIJ(I,1))
       BMERZ(I,4,2) = (-MY13(I)*XIL(I,2)-TWO*MY23(I)*XIJ(I,2))
       BMERZ(I,4,3) = ( MY13(I)*XIL(I,3)-TWO*MY23(I)*XIJ(I,3))
       BMERZ(I,4,4) = ( MY13(I)*XIL(I,4)-TWO*MY23(I)*XIJ(I,4))
      ENDDO
C--------correction to pass the patch test
      CALL CBPATCH(JFT ,JLT  ,BM0RZ,BMKRZ,BMERZ  ,AREA ,
     +             MX13 ,MX23,MX34  ,MY13 ,MY23 ,MY34)
C      
      DO I=JFT,JLT
C--------BM0RZ(I,3,1)->(NxI,y+NyI,x) *A-;BM0RZ(I,4,1)->(-NxI,y+NyI,x -2NI) *A-----
       A05= HALF*AREA(I)
       A05K(1)= -A05
       A05K(2)= A05
       A05K(3)= A05
       A05K(4)= -A05
       A05E(1)= -A05
       A05E(2)= -A05
       A05E(3)= A05
       A05E(4)= A05
      DO J=1,4
       NXY=BM0RZ(I,3,J)
       NYX=BM0RZ(I,4,J)
       BM0RZ(I,3,J)=NXY+NYX
       BM0RZ(I,4,J)=-NXY+NYX-A05
       NXY=BMKRZ(I,3,J)
       NYX=BMKRZ(I,4,J)
       BMKRZ(I,3,J)=NXY+NYX
       BMKRZ(I,4,J)=-NXY+NYX-A05K(J)
       NXY=BMERZ(I,3,J)
       NYX=BMERZ(I,4,J)
       BMERZ(I,3,J)=NXY+NYX
       BMERZ(I,4,J)=-NXY+NYX-A05E(J)
      ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CZDEFT                        source/elements/shell/coquez/czdef.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZDEFT(JFT ,JLT  ,V13   ,V24 ,RLXYZ,
     2                   VDEF ,X13   ,X24 ,Y13  ,Y24  ,
     3                   Z1   ,AA    )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   RLXYZ(MVSIZ,4),V13(MVSIZ,2),V24(MVSIZ,2),
     .   X13(*),X24(*),Y13(*),Y24(*),AA(*)
      my_real 
     .   Z1(*),VDEF(MVSIZ,8)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   BCX,BCY,BCXY,R13(2),R24(2),DETA1,
     .   BXV2,BYV1,BXR1,BYR2
C-----------------------------------------------
      DO I=JFT,JLT
C---------------
C  MEMBRANE
C---------------
        VDEF(I,1)= Y24(I)*V13(I,1)-Y13(I)*V24(I,1)
        VDEF(I,2)=-X24(I)*V13(I,2)+X13(I)*V24(I,2)
        VDEF(I,4)= Y24(I)*V13(I,2)-Y13(I)*V24(I,2)
        VDEF(I,3)=-X24(I)*V13(I,1)+X13(I)*V24(I,1)
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  CZDEFRZT                      source/elements/shell/coquez/czdef.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        CZDERIRZT                     source/elements/shell/coquez/czdef.F
Chd|====================================================================
        SUBROUTINE CZDEFRZT(JFT ,JLT  ,AREA ,AA  ,RLXYZ ,
     1                     VDEF ,X13 ,X24   ,Y13  ,Y24  ,
     2                     MX13 ,MX23,MX34  ,MY13 ,MY23 ,
     3                     MY34 ,Z1  )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   RLXYZ(MVSIZ,4),
     .   X13(*),X24(*),Y13(*),Y24(*),Z1(*),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*)
      my_real 
     .   AREA(*),VDEF(MVSIZ,8),AA(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   AA4,BXV2,BYV1,A05,BXRZ,BYRZ,DHX,DHY,C3,HXX,HYY,HXX_K,HYY_K,
     .   BM0RZ(MVSIZ,4,4)
C-----------------------------------------------
        CALL CZDERIRZT(JFT ,JLT  ,AREA ,X13 ,X24   ,
     2               Y13  ,Y24  ,MX13 ,MX23,MX34  ,
     3               MY13 ,MY23 ,MY34 ,BM0RZ)
C
      DO I=JFT,JLT
       VDEF(I,1)=VDEF(I,1) + AA(I)*
     1           (BM0RZ(I,1,1)*RLXYZ(I,1)+BM0RZ(I,1,2)*RLXYZ(I,2)
     2           +BM0RZ(I,1,3)*RLXYZ(I,3)+BM0RZ(I,1,4)*RLXYZ(I,4))
       VDEF(I,2)=VDEF(I,2) + AA(I)*
     1           (BM0RZ(I,2,1)*RLXYZ(I,1)+BM0RZ(I,2,2)*RLXYZ(I,2)
     2           +BM0RZ(I,2,3)*RLXYZ(I,3)+BM0RZ(I,2,4)*RLXYZ(I,4))
       VDEF(I,3)=VDEF(I,3) + AA(I)*
     1           (BM0RZ(I,3,1)*RLXYZ(I,1)+BM0RZ(I,3,2)*RLXYZ(I,2)
     2           +BM0RZ(I,3,3)*RLXYZ(I,3)+BM0RZ(I,3,4)*RLXYZ(I,4))
       VDEF(I,4)=VDEF(I,4) + AA(I)*
     1           (BM0RZ(I,4,1)*RLXYZ(I,1)+BM0RZ(I,4,2)*RLXYZ(I,2)
     2           +BM0RZ(I,4,3)*RLXYZ(I,3)+BM0RZ(I,4,4)*RLXYZ(I,4))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CZDERIRZT                     source/elements/shell/coquez/czdef.F
Chd|-- called by -----------
Chd|        CZDEFRZT                      source/elements/shell/coquez/czdef.F
Chd|-- calls ---------------
Chd|        CBPATCH                       source/elements/shell/coqueba/cbadef.F
Chd|====================================================================
        SUBROUTINE CZDERIRZT(JFT ,JLT  ,AREA ,X13 ,X24   ,
     2                     Y13  ,Y24  ,MX13 ,MX23,MX34  ,
     3                     MY13 ,MY23 ,MY34 ,BM0RZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   X13(*),X24(*),Y13(*),Y24(*),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*)
      my_real 
     .   AREA(*),
     .   BM0RZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   AA4,BXV2,BYV1,A05,NXY,NYX,A05K(4),A05E(4),
     .   XIJ(MVSIZ,4),YIJ(MVSIZ,4),XIL(MVSIZ,4),YIL(MVSIZ,4),
     .  BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4)
C--------(14,23,32,41)/4---------------------------------------
      DO I=JFT,JLT
       XIJ(I,1) = HALF*(MX13(I)-MX34(I))
       XIJ(I,2) = HALF*(X13(I)+MX23(I)-MX13(I))
       XIJ(I,3) = -XIJ(I,2)
       XIJ(I,4) = -XIJ(I,1)
       YIJ(I,1) = HALF*(MY13(I)-MY34(I))
       YIJ(I,2) = HALF*(Y13(I)+MY23(I)-MY13(I))
       YIJ(I,3) = -YIJ(I,2)
       YIJ(I,4) = -YIJ(I,1)
C--------(12,21,34,43)/4---------------------------------------
       XIL(I,1) = HALF*(MX13(I)-MX23(I))
       XIL(I,2) = -XIL(I,1)
       XIL(I,3) = HALF*(MX13(I)-MX34(I)-X13(I))
       XIL(I,4) = -XIL(I,3)
       YIL(I,1) = HALF*(MY13(I)-MY23(I))
       YIL(I,2) = -YIL(I,1)
       YIL(I,3) = HALF*(MY13(I)-MY34(I)-Y13(I))
       YIL(I,4) = -YIL(I,3)
      ENDDO
C      
      DO I=JFT,JLT
C--------NxI,x *A------pay attention Bi*A--- RLZ: VRZ/A--       
C-------- MY34YIJ KSI - MY23YIL ETA       
       BM0RZ(I,1,1) = (-MY34(I)*YIJ(I,1)+MY23(I)*YIL(I,1))
       BM0RZ(I,1,2) = ( MY34(I)*YIJ(I,2)+MY23(I)*YIL(I,2))
       BM0RZ(I,1,3) = ( MY34(I)*YIJ(I,3)-MY23(I)*YIL(I,3))
       BM0RZ(I,1,4) = (-MY34(I)*YIJ(I,4)-MY23(I)*YIL(I,4))
C-------- MY13YIJ KSI - 2MY34YIL      
       BMKRZ(I,1,1) = (-MY13(I)*YIJ(I,1)-TWO*MY34(I)*YIL(I,1))
       BMKRZ(I,1,2) = ( MY13(I)*YIJ(I,2)-TWO*MY34(I)*YIL(I,2))
       BMKRZ(I,1,3) = ( MY13(I)*YIJ(I,3)-TWO*MY34(I)*YIL(I,3))
       BMKRZ(I,1,4) = (-MY13(I)*YIJ(I,4)-TWO*MY34(I)*YIL(I,4))
C-------- -MY13YIL ETA + 2MY23YIJ      
       BMERZ(I,1,1) = ( MY13(I)*YIL(I,1)+TWO*MY23(I)*YIJ(I,1))
       BMERZ(I,1,2) = ( MY13(I)*YIL(I,2)+TWO*MY23(I)*YIJ(I,2))
       BMERZ(I,1,3) = (-MY13(I)*YIL(I,3)+TWO*MY23(I)*YIJ(I,3))
       BMERZ(I,1,4) = (-MY13(I)*YIL(I,4)+TWO*MY23(I)*YIJ(I,4))
C -------NyI,y *A-------------      
C-------- MX34XIJ KSI - MX23XIL ETA       
       BM0RZ(I,2,1) = (-MX34(I)*XIJ(I,1)+MX23(I)*XIL(I,1))
       BM0RZ(I,2,2) = ( MX34(I)*XIJ(I,2)+MX23(I)*XIL(I,2))
       BM0RZ(I,2,3) = ( MX34(I)*XIJ(I,3)-MX23(I)*XIL(I,3))
       BM0RZ(I,2,4) = (-MX34(I)*XIJ(I,4)-MX23(I)*XIL(I,4))
C-------- MX13XIJ KSI - 2MX34XIL      
       BMKRZ(I,2,1) = (-MX13(I)*XIJ(I,1)-TWO*MX34(I)*XIL(I,1))
       BMKRZ(I,2,2) = ( MX13(I)*XIJ(I,2)-TWO*MX34(I)*XIL(I,2))
       BMKRZ(I,2,3) = ( MX13(I)*XIJ(I,3)-TWO*MX34(I)*XIL(I,3))
       BMKRZ(I,2,4) = (-MX13(I)*XIJ(I,4)-TWO*MX34(I)*XIL(I,4))
C-------- -MX13XIL ETA + 2MX23XIJ      
       BMERZ(I,2,1) = ( MX13(I)*XIL(I,1)+TWO*MX23(I)*XIJ(I,1))
       BMERZ(I,2,2) = ( MX13(I)*XIL(I,2)+TWO*MX23(I)*XIJ(I,2))
       BMERZ(I,2,3) = (-MX13(I)*XIL(I,3)+TWO*MX23(I)*XIJ(I,3))
       BMERZ(I,2,4) = (-MX13(I)*XIL(I,4)+TWO*MX23(I)*XIJ(I,4))
C--------NxI,y *A    
C-------- -MX34YIJ KSI + MX23YIL ETA       
       BM0RZ(I,3,1) = ( MX34(I)*YIJ(I,1)-MX23(I)*YIL(I,1))
       BM0RZ(I,3,2) = (-MX34(I)*YIJ(I,2)-MX23(I)*YIL(I,2))
       BM0RZ(I,3,3) = (-MX34(I)*YIJ(I,3)+MX23(I)*YIL(I,3))
       BM0RZ(I,3,4) = ( MX34(I)*YIJ(I,4)+MX23(I)*YIL(I,4))
C-------- -MX13YIJ KSI + 2MX34YIL      
       BMKRZ(I,3,1) = ( MX13(I)*YIJ(I,1)+TWO*MX34(I)*YIL(I,1))
       BMKRZ(I,3,2) = (-MX13(I)*YIJ(I,2)+TWO*MX34(I)*YIL(I,2))
       BMKRZ(I,3,3) = (-MX13(I)*YIJ(I,3)+TWO*MX34(I)*YIL(I,3))
       BMKRZ(I,3,4) = ( MX13(I)*YIJ(I,4)+TWO*MX34(I)*YIL(I,4))
C-------- MX13YIL ETA - 2MX23YIJ      
       BMERZ(I,3,1) = (-MX13(I)*YIL(I,1)-TWO*MX23(I)*YIJ(I,1))
       BMERZ(I,3,2) = (-MX13(I)*YIL(I,2)-TWO*MX23(I)*YIJ(I,2))
       BMERZ(I,3,3) = ( MX13(I)*YIL(I,3)-TWO*MX23(I)*YIJ(I,3))
       BMERZ(I,3,4) = ( MX13(I)*YIL(I,4)-TWO*MX23(I)*YIJ(I,4))
C--------NyI,x *A   
C-------- -MY34XIJ KSI + MY23XIJ ETA       
       BM0RZ(I,4,1) = ( MY34(I)*XIJ(I,1)-MY23(I)*XIL(I,1))
       BM0RZ(I,4,2) = (-MY34(I)*XIJ(I,2)-MY23(I)*XIL(I,2))
       BM0RZ(I,4,3) = (-MY34(I)*XIJ(I,3)+MY23(I)*XIL(I,3))
       BM0RZ(I,4,4) = ( MY34(I)*XIJ(I,4)+MY23(I)*XIL(I,4))
C-------- -MY13XIJ KSI + 2MY34XIL      
       BMKRZ(I,4,1) = ( MY13(I)*XIJ(I,1)+TWO*MY34(I)*XIL(I,1))
       BMKRZ(I,4,2) = (-MY13(I)*XIJ(I,2)+TWO*MY34(I)*XIL(I,2))
       BMKRZ(I,4,3) = (-MY13(I)*XIJ(I,3)+TWO*MY34(I)*XIL(I,3))
       BMKRZ(I,4,4) = ( MY13(I)*XIJ(I,4)+TWO*MY34(I)*XIL(I,4))
C-------- MY13XIL ETA - 2MY23XIJ      
       BMERZ(I,4,1) = (-MY13(I)*XIL(I,1)-TWO*MY23(I)*XIJ(I,1))
       BMERZ(I,4,2) = (-MY13(I)*XIL(I,2)-TWO*MY23(I)*XIJ(I,2))
       BMERZ(I,4,3) = ( MY13(I)*XIL(I,3)-TWO*MY23(I)*XIJ(I,3))
       BMERZ(I,4,4) = ( MY13(I)*XIL(I,4)-TWO*MY23(I)*XIJ(I,4))
      ENDDO
C--------correction to pass the patch test
      CALL CBPATCH(JFT ,JLT  ,BM0RZ,BMKRZ,BMERZ  ,AREA ,
     +             MX13 ,MX23,MX34  ,MY13 ,MY23 ,MY34)
C      
C      DO I=JFT,JLT
C--------BM0RZ(I,3,1)->(NxI,y) *A-;BM0RZ(I,4,1)->(NyI,x) *A-----
C      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CZDEFTW                       source/elements/shell/coquez/czdef.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZDEFTW(JFT ,JLT  ,AA  ,V13   ,V24 ,
     2                    RLXYZ,X13  ,X24 ,Y13  ,Y24  ,
     3                    Z1  ,WXY)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   RLXYZ(MVSIZ,2,4),V13(MVSIZ,2),V24(MVSIZ,2),
     .   X13(*),X24(*),Y13(*),Y24(*)
      my_real 
     .   Z1(*),AA(*),WXY(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   BCX,BCY,BCXY,R13(2),R24(2),DETA1,
     .   BXV2,BYV1,BXR1,BYR2,KXY,KYX
C-----------------------------------------------
      DO I=JFT,JLT
       DETA1=Z1(I)*FOUR*AA(I)
        R13(1) =(RLXYZ(I,1,1)-RLXYZ(I,1,3))*AA(I)
        R24(1) =(RLXYZ(I,1,2)-RLXYZ(I,1,4))*AA(I)
        R13(2) =(RLXYZ(I,2,1)-RLXYZ(I,2,3))*AA(I)
        R24(2) =(RLXYZ(I,2,2)-RLXYZ(I,2,4))*AA(I)
C---------
C  FLEXION
C---------
       KYX = Y13(I)*R24(1)-Y24(I)*R13(1)
       KXY =-X24(I)*R13(2)+X13(I)*R24(2)
       KXY = KXY + (Y13(I)*V13(I,1)-Y24(I)*V24(I,1))*DETA1
       KYX = KYX + (X13(I)*V13(I,2)-X24(I)*V24(I,2))*DETA1
       WXY(I) = KXY - KYX
      ENDDO
C
      RETURN
      END

