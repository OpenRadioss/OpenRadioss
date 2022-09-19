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
Chd|  CZFINTCE                      source/elements/shell/coquez/czfintce.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZFINTCE(JFT  ,JLT  ,THK  ,C1   ,A_I  ,X13  ,
     2                      X24  ,Y13  ,Y24  ,Z1   ,MX23 ,MX13 ,
     3                      MX34 ,MY13 ,MY23 ,MY34 ,VSTRE,MSTRE,
     4                      VF   ,VM   ,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL LA PARTIE CONSTANTE
C         VF(3,NODE),VM(2,NODE) : FORCES INTERNES GENERALISEES
C        ENTREES  : THK,AREA,X13,X24,Y13,Y24,MX13,MX34,MY13,MY34,Z1,...
C        SORTIES  : VF,VM
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT ,JLT,NEL
        my_real
     .     THK(*)   ,C1(*)     ,A_I(*)     ,Z1(*)     ,
     .     X13(*)   ,X24(*)    ,Y13(*)     ,Y24(*)    ,
     .     MX13(*)  ,MX23(*)   ,MX34(*)    ,MY13(*)   ,MY23(*)  ,
     .     MY34(*)  ,VSTRE(NEL,5),MSTRE(NEL,3) ,VF(MVSIZ,3,4) ,VM(MVSIZ,2,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J
        my_real
     .     S1, C2, Y13S8, Y24S8, S42S, S52S, X13S8, X24S8
C
        DO I=JFT,JLT
          X13S8=X13(I)*MSTRE(I,3)
          X24S8=X24(I)*MSTRE(I,3)
          Y13S8=Y13(I)*MSTRE(I,3)
          Y24S8=Y24(I)*MSTRE(I,3)
C
          S1=(MY34(I)*MX23(I)-MY23(I)*MX34(I))*THK(I)
          S42S=S1*VSTRE(I,5)
          S52S=S1*VSTRE(I,4)
C-------------------------------------------------
C        PARTIE ANTISYM STOCKEE DANS NOEUDS 1,2
C        PARTIE SYM NOEUDS STOCKEE DANS NOEUDS 3,4
C-------------------------------------------------
          VF(I,1,1)=THK(I)*( Y24(I)*VSTRE(I,1)-X24(I)*VSTRE(I,3))
          VF(I,2,1)=THK(I)*(-X24(I)*VSTRE(I,2)+Y24(I)*VSTRE(I,3))
          VF(I,3,1)=THK(I)*(-X24(I)*VSTRE(I,4)+Y24(I)*VSTRE(I,5))
C
          VM(I,1,1)=C1(I)*(X24(I)*MSTRE(I,2)-Y24S8)-MY13(I)*VF(I,3,1)
          VM(I,2,1)=C1(I)*(Y24(I)*MSTRE(I,1)-X24S8)+MX13(I)*VF(I,3,1)
C
          VM(I,1,3)=-S52S
          VM(I,2,3)= S42S
C
          VF(I,1,2)=THK(I)*(-Y13(I)*VSTRE(I,1)+X13(I)*VSTRE(I,3))
          VF(I,2,2)=THK(I)*( X13(I)*VSTRE(I,2)-Y13(I)*VSTRE(I,3))
          VF(I,3,2)=THK(I)*( X13(I)*VSTRE(I,4)-Y13(I)*VSTRE(I,5))
C
          VM(I,1,2)=C1(I)*(-X13(I)*MSTRE(I,2)+Y13S8)+MY13(I)*VF(I,3,2)
          VM(I,2,2)=C1(I)*(-Y13(I)*MSTRE(I,1)+X13S8)-MX13(I)*VF(I,3,2)
C
          VM(I,1,4)=VM(I,1,3)
          VM(I,2,4)=VM(I,2,3)
C
          C2=C1(I)*Z1(I)*4.*A_I(I)
C
          VF(I,1,1)=VF(I,1,1)+C2*(X13(I)*MSTRE(I,1)+Y13S8)
          VF(I,2,1)=VF(I,2,1)+C2*(Y13(I)*MSTRE(I,2)+X13S8)
          VF(I,1,2)=VF(I,1,2)-C2*(X24(I)*MSTRE(I,1)+Y24S8)
          VF(I,2,2)=VF(I,2,2)-C2*(Y24(I)*MSTRE(I,2)+X24S8)
       ENDDO
       RETURN
       END
Chd|====================================================================
Chd|  CZFINTCRZ                     source/elements/shell/coquez/czfintce.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZFINTCRZ(JFT  ,JLT  ,THK  ,VOL  ,AREA ,X13  ,
     2                       X24  ,Y13  ,Y24  ,Z1   ,MX23 ,MX13 ,
     3                       MX34 ,MY13 ,MY23 ,MY34 ,VSTRE,VSRZ ,
     4                       VF   ,VMZ  ,BM0RZ,KRZ  ,VRLZ ,DT1C ,
     5                       EINT ,OFF  ,NEL  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT ,JLT,NEL
        my_real
     .     THK(*)   ,VOL(*)     ,AREA(*)     ,Z1(*)     ,
     .     X13(*)   ,X24(*)    ,Y13(*)     ,Y24(*)    ,
     .     MX13(*)  ,MX23(*)   ,MX34(*)    ,MY13(*)   ,MY23(*) ,
     .     MY34(*)  ,VSTRE(NEL,5),VSRZ(NEL,19) ,VF(MVSIZ,3,4) ,VMZ(MVSIZ,4),
     .     BM0RZ(MVSIZ,4,4),KRZ(*),VRLZ(*),DT1C,EINT(NEL,2),OFF(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J
        my_real
     .     S1, C2, A05,ERZ,OFF05
C
C      --------const sig_rz added here---------
       DO I=JFT,JLT
        ERZ= VRLZ(I)*DT1C
        OFF05=HALF*OFF(I)*VOL(I)
        EINT(I,1) = EINT(I,1)+ VSRZ(I,19)*ERZ*OFF05
        VSRZ(I,19)= VSRZ(I,19)+KRZ(I)*ERZ*OFF(I)
        EINT(I,1) = EINT(I,1)+ VSRZ(I,19)*ERZ*OFF05
       ENDDO
       DO J=1,4
       DO I=JFT,JLT
        VMZ(I,J)=(BM0RZ(I,1,J)*VSTRE(I,1)+BM0RZ(I,2,J)*VSTRE(I,2)
     .           +BM0RZ(I,3,J)*VSTRE(I,3))*THK(I)
       ENDDO
       ENDDO
C        
        DO I=JFT,JLT
         C2=THK(I)*VSRZ(I,19)*HALF
         DO J=1,4
         VMZ(I,J)= VMZ(I,J)+BM0RZ(I,4,J)*C2
         ENDDO
        ENDDO
C-------------------------------------------------
C        PARTIE ANTISYM STOCKEE DANS NOEUDS 1,2
C        PARTIE SYM NOEUDS STOCKEE DANS NOEUDS 3,4
C-------------------------------------------------
        DO I=JFT,JLT
          C2=THK(I)*VSRZ(I,19)*HALF
C
          VF(I,1,1)=VF(I,1,1)+C2*X24(I)
          VF(I,2,1)=VF(I,2,1)+C2*Y24(I)
          VF(I,1,2)=VF(I,1,2)-C2*X13(I)
          VF(I,2,2)=VF(I,2,2)-C2*Y13(I)
        ENDDO
C
       RETURN
       END
