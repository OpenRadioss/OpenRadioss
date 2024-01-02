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
Chd|  SETPROJKS6                    source/elements/thickshell/solide6c/setprojks6.F
Chd|-- called by -----------
Chd|        S6CCUMG3                      source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- calls ---------------
Chd|        TRANPVQ33                     source/elements/thickshell/solide6c/tranpvq33.F
Chd|        TRANQIKQJ                     source/elements/shell/coquez/czsumg3.F
Chd|        TRANRIDRRJ33                  source/elements/thickshell/solide6c/tranridrrj33.F
Chd|====================================================================
      SUBROUTINE SETPROJKS6(
     1                 DR    ,R1     ,R2     ,R3     ,R4    ,
     1                 R5    ,R6     ,  
     2                 K11,K12,K13,K14,K15,K16,K22,K23,
     3                 K24,K25,K26,K33,K34,K35,K36,K44,
     4                 K45,K46,K55,K56,K66,
     5                 VQ ,JFT ,JLT  ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .    DR(3,3,*),VQ(3,3,*),
     .    R1(3,3,*),R2(3,3,*),R3(3,3,*),R4(3,3,*),R5(3,3,*),R6(3,3,*),
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*)  ,K15(3,3,*) ,
     .   K16(3,3,*),K22(3,3,*),K23(3,3,*),K24(3,3,*)  ,K25(3,3,*) ,
     .   K26(3,3,*),K33(3,3,*),K34(3,3,*),K35(3,3,*)  ,K36(3,3,*) ,
     .   K44(3,3,*),K45(3,3,*),K46(3,3,*),K55(3,3,*)  ,K56(3,3,*) ,
     .   K66(3,3,*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP,IS,IAS,IT,IAT,SHI,SHJ
      my_real
     .    KL(3,3,MVSIZ),KQ(3,3,MVSIZ)
         DATA IS/1/,IAS/0/,IT/1/,IAT/0/
      my_real,
     .  DIMENSION(:,:,:), ALLOCATABLE:: P,KE
C-------------------------------------------------------------
        ALLOCATE(P(18,18,MVSIZ))
        ALLOCATE(KE(18,18,MVSIZ))
C-----------P11=Pr11Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R1 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=0
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I,J,EP)= K11(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P22=Pr22Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R2 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K22(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P33=Pr33Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R3  ,DR , R3 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K33(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P44=Pr44Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R4  ,DR , R4 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K44(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P55=Pr55Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R5  ,DR , R5 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K55(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P66=Pr66Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R6  ,DR , R6 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
       if (SHI/=15) print *,'error,SHI=',SHI
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K66(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P12=Pr12Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R2 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K12(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P21=Pr21Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P13=Q1Pr13Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R3 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=SHJ+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K13(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P31=Pr31Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P14=Pr14Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R4 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=SHJ+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K14(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P41=P41Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P15=Pr15Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R5 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=SHJ+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K15(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P51=P51Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P16=Pr16Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=SHJ+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K16(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P61=P61Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P23=Pr23Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R3 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3
       SHJ=3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K23(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P32=Pr32Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P24=Pr24Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R4 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K24(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P42=Pr42Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P25=Pr25Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R5 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K25(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P52=Pr52Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P26=Pr26Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K26(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P62=Pr62Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P34=Pr34Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R3  ,DR , R4 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K34(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P43=Pr43Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P35=Pr35Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R3  ,DR , R5 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K35(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P53=Pr53Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P36=Pr36Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R3  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K36(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P63=Pr63Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P45=Pr45Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R4  ,DR , R5 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3+3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K45(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P54=Pr54Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P46=Pr46Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R4  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K46(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P64=Pr64Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P56=Pr56Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R5  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3+3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K56(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P65=Pr65Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------
        DO I=1,18 
        DO J=I+1,18 
         DO EP=JFT,JLT 
          KE(J,I,EP)= KE(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------
        CALL TRANQIKQJ(JFT   ,JLT    ,P  ,KE,P ,18 ,IS ) 
C-----------after projection----
       SHI=0
       SHJ=0
C-----------K11
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K11(I,J,EP) =KE(I,J,EP)
          K11(J,I,EP) =KE(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K22
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K22(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K22(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K33
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K33(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K33(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K44
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K44(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K44(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K55
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K55(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K55(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K66
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K66(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K66(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K12
       SHI=0
       SHJ=3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K12(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K13
       SHI=0
       SHJ=3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K13(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K14
       SHI=0
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K14(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K15
       SHI=0
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K15(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K16
       SHI=0
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K16(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K23
       SHI=3
       SHJ=3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K23(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K24
       SHI=3
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K24(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K25
       SHI=3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K25(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K26
       SHI=3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K26(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K34
       SHI=3+3
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K34(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K35
       SHI=3+3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K35(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K36
       SHI=3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K36(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K45
       SHI=3+3+3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K45(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K46
       SHI=3+3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K46(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K56
       SHI=3+3+3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K56(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C        
        DEALLOCATE(P)
        DEALLOCATE(KE)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
