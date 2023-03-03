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
Chd|  SMASS3                        source/elements/solid/solide/smass3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|====================================================================
      SUBROUTINE SMASS3(
     1     RHO    ,MS   ,PARTSAV,X    ,V ,
     2     IPART  ,MSS  ,VOLU   ,
     3     MSNF   ,MSSF ,IN     ,
     4     VR     ,INS  ,WMA    ,RHOCP,MCP,  
     5     MCPS   ,MSSA ,RHOF   ,FRAC ,FILL, 
     6     NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8)
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
      INTEGER IPART(*),NDDIM
      my_real
     .   RHO(*),MS(*),X(3,*),V(3,*),PARTSAV(20,*),VOLU(*),
     .   MSS(8,*), MSSF(8,*), MSNF(*),IN(*),VR(3,*),INS(8,*),WMA(*),
     .   RHOCP(*),MCP(*),MCPS(8,*), MSSA(*), RHOF(*), FRAC(*), FILL(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*), NC8(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "ale_param.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IP,I1,I2,I3,I4,I5,I6,I7,I8
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX, MASSP,INER,RCP
      my_real
     .   MASS(MVSIZ),MASSF(MVSIZ)
C-------------------------------
C=======================================================================
       DO I=LFT,LLT
        MASS(I)  =FILL(I)*RHO(I)*VOLU(I)*ONE_OVER_8
        MASSF(I) =FILL(I)*FRAC(I)*RHOF(I)*VOLU(I)*ONE_OVER_8
        I1 = NC1(I)
        I2 = NC2(I)
        I3 = NC3(I)
        I4 = NC4(I)
        I5 = NC5(I)
        I6 = NC6(I)
        I7 = NC7(I)
        I8 = NC8(I)
        MSS(1,I)=MASS(I)
        MSS(2,I)=MASS(I)
        MSS(3,I)=MASS(I)
        MSS(4,I)=MASS(I)
        MSS(5,I)=MASS(I)
        MSS(6,I)=MASS(I)
        MSS(7,I)=MASS(I)
        MSS(8,I)=MASS(I)
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + EIGHT*MASS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + MASS(I)*
     .       (X(1,I1)+X(1,I2)+X(1,I3)+X(1,I4)
     .       +X(1,I5)+X(1,I6)+X(1,I7)+X(1,I8))
        PARTSAV(3,IP)=PARTSAV(3,IP) + MASS(I)*
     .       (X(2,I1)+X(2,I2)+X(2,I3)+X(2,I4)
     .       +X(2,I5)+X(2,I6)+X(2,I7)+X(2,I8))
        PARTSAV(4,IP)=PARTSAV(4,IP) + MASS(I)*
     .       (X(3,I1)+X(3,I2)+X(3,I3)+X(3,I4)
     .       +X(3,I5)+X(3,I6)+X(3,I7)+X(3,I8))
        XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2)
     .       +X(1,I3)*X(1,I3)+X(1,I4)*X(1,I4)
     .       +X(1,I5)*X(1,I5)+X(1,I6)*X(1,I6)
     .       +X(1,I7)*X(1,I7)+X(1,I8)*X(1,I8))
        XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2)
     .       +X(1,I3)*X(2,I3)+X(1,I4)*X(2,I4)
     .       +X(1,I5)*X(2,I5)+X(1,I6)*X(2,I6)
     .       +X(1,I7)*X(2,I7)+X(1,I8)*X(2,I8))
        YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2)
     .       +X(2,I3)*X(2,I3)+X(2,I4)*X(2,I4)
     .       +X(2,I5)*X(2,I5)+X(2,I6)*X(2,I6)
     .       +X(2,I7)*X(2,I7)+X(2,I8)*X(2,I8))
        YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2)
     .       +X(2,I3)*X(3,I3)+X(2,I4)*X(3,I4)
     .       +X(2,I5)*X(3,I5)+X(2,I6)*X(3,I6)
     .       +X(2,I7)*X(3,I7)+X(2,I8)*X(3,I8))
        ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2)
     .       +X(3,I3)*X(3,I3)+X(3,I4)*X(3,I4)
     .       +X(3,I5)*X(3,I5)+X(3,I6)*X(3,I6)
     .       +X(3,I7)*X(3,I7)+X(3,I8)*X(3,I8))
        ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2)
     .       +X(3,I3)*X(1,I3)+X(3,I4)*X(1,I4)
     .       +X(3,I5)*X(1,I5)+X(3,I6)*X(1,I6)
     .       +X(3,I7)*X(1,I7)+X(3,I8)*X(1,I8))
        PARTSAV(5,IP) =PARTSAV(5,IP)  + MASS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + MASS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + MASS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - MASS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - MASS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - MASS(I) * ZX
C
        PARTSAV(11,IP)=PARTSAV(11,IP) + MASS(I)*
     .       (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4)
     .       +V(1,I5)+V(1,I6)+V(1,I7)+V(1,I8))
        PARTSAV(12,IP)=PARTSAV(12,IP) + MASS(I)*
     .       (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4)
     .       +V(2,I5)+V(2,I6)+V(2,I7)+V(2,I8))
        PARTSAV(13,IP)=PARTSAV(13,IP) + MASS(I)*
     .       (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4)
     .       +V(3,I5)+V(3,I6)+V(3,I7)+V(3,I8))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * MASS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .     +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .     +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4)
     .     +V(1,I5)*V(1,I5)+V(2,I5)*V(2,I5)+V(3,I5)*V(3,I5)
     .     +V(1,I6)*V(1,I6)+V(2,I6)*V(2,I6)+V(3,I6)*V(3,I6)
     .     +V(1,I7)*V(1,I7)+V(2,I7)*V(2,I7)+V(3,I7)*V(3,I7)
     .     +V(1,I8)*V(1,I8)+V(2,I8)*V(2,I8)+V(3,I8)*V(3,I8))
       ENDDO
C
       IF(IREST_MSELT /= 0)THEN
        DO I=LFT,LLT
         MSSA(NFT+I)=MASS(I)
        ENDDO
       ENDIF
C       
       IF(JALE == 3 .AND. JLAG == 1)THEN
          DO I=LFT,LLT
           I1 = NC1(I)
           I2 = NC2(I)
           I3 = NC3(I)
           I4 = NC4(I)
           I5 = NC5(I)
           I6 = NC6(I)
           I7 = NC7(I)
           I8 = NC8(I)
           MSSF(1,I)=MASSF(I)
           MSSF(2,I)=MASSF(I)
           MSSF(3,I)=MASSF(I)
           MSSF(4,I)=MASSF(I)
           MSSF(5,I)=MASSF(I)
           MSSF(6,I)=MASSF(I)
           MSSF(7,I)=MASSF(I)
           MSSF(8,I)=MASSF(I)
         ENDDO 
       ELSEIF(JALE+JEUL > 0)THEN
         DO I=LFT,LLT
           I1 = NC1(I)
           I2 = NC2(I)
           I3 = NC3(I)
           I4 = NC4(I)
           I5 = NC5(I)
           I6 = NC6(I)
           I7 = NC7(I)
           I8 = NC8(I)
           MSSF(1,I)=MASS(I)
           MSSF(2,I)=MASS(I)
           MSSF(3,I)=MASS(I)
           MSSF(4,I)=MASS(I)
           MSSF(5,I)=MASS(I)
           MSSF(6,I)=MASS(I)
           MSSF(7,I)=MASS(I)
           MSSF(8,I)=MASS(I)
         ENDDO
       ENDIF
C
       IF(JTHE < 0 ) THEN      
         DO I=LFT,LLT
           RCP=FILL(I)*RHOCP(I)*VOLU(I)*ONE_OVER_8
           MCPS(1,I) =RCP
           MCPS(2,I)= RCP
           MCPS(3,I)= RCP
           MCPS(4,I)= RCP
           MCPS(5,I)= RCP
           MCPS(6,I)= RCP
           MCPS(7,I)= RCP
           MCPS(8,I)= RCP
         ENDDO 
       ENDIF
C
       IF(ISROT == 1)THEN
        IF(IRODDL == 0)THEN
C prov gw
            WRITE(6,'(A)') 'INTERNAL ERROR 1119'
            CALL MY_EXIT(2)
C           STOP 1119
        ENDIF
        DO I=LFT,LLT
         INER=(MASS(I)*VOLU(I)**TWO_THIRD)/SIX
         INS(1,I)=INER
         INS(2,I)=INER
         INS(3,I)=INER
         INS(4,I)=INER
         INS(5,I)=INER
         INS(6,I)=INER
         INS(7,I)=INER
         INS(8,I)=INER
        ENDDO
       ENDIF

C
      IF(JALE > 0 .AND. NWALE == 4)THEN
        DO I=LFT,LLT
          I1 = NC1(I)
          I2 = NC2(I)
          I3 = NC3(I)
          I4 = NC4(I)
          I5 = NC5(I)
          I6 = NC6(I)
          I7 = NC7(I)
          I8 = NC8(I)
          WMA(I1)=WMA(I1)+THREE_HALF
          WMA(I2)=WMA(I2)+THREE_HALF
          WMA(I3)=WMA(I3)+THREE_HALF
          WMA(I4)=WMA(I4)+THREE_HALF
          WMA(I5)=WMA(I5)+THREE_HALF
          WMA(I6)=WMA(I6)+THREE_HALF
          WMA(I7)=WMA(I7)+THREE_HALF
          WMA(I8)=WMA(I8)+THREE_HALF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
