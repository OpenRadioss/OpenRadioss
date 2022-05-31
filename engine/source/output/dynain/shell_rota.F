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
Chd|  SHELL_ROTA                    source/output/dynain/shell_rota.F
Chd|-- called by -----------
Chd|        DYNAIN_C_STRSG                source/output/dynain/dynain_c_strsg.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SHELL_ROTA(I        ,ILAY    ,NEL    ,IORTH  ,ITY      ,
     2                      IGTYP    ,MLW     ,JDIR   ,SIG    ,ELBUF_STR,
     3                      RX       ,RY      ,RZ     ,SX     ,SY       ,  
     4                      SZ       ,E1X     ,E2X    , E3X   ,E1Y      , 
     5                      E2Y      ,E3Y     ,E1Z    , E2Z   ,E3Z      ,
     6                      DIR_A    ,DIR_B    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, ILAY,I , NS1,ITY, IORTH, IGTYP,MLW ,JDIR
      my_real SIG(6)
      my_real
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ), 
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),    
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), 
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ), 
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   DIR_A(*)  ,DIR_B(*)
 
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILAW , II
      my_real 
     .   DIR(2),
     .   V1,V2,V3,VR,VS,AA,BB,SUMA,
     .   R1,S1,S2,R2,RS1,RS2,R12A,R22A,S12B,S22B,
     .   RS3,R3R3,S3S3,T1,T2,T3,S3,S4,S5,
     .   L11,L12,L13,L22,L23,L33,
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .   S11,S12,S21,S13,S31,S22,S23,S32,S33
      my_real, 
     .   DIMENSION(:) , POINTER :: DIR1 
C=======================================================================
C*******************************************
C     Rotate to global system 
C*******************************************

        IF(ILAY > 0 ) THEN
           ILAW   = ELBUF_STR%BUFLY(ILAY)%ILAW
        ELSE
           ILAW = MLW
        ENDIF
C-----------------------------------
C      Rotation orthotrop > Element
C------------------------------------
       IF (IORTH > 0) THEN      

         IF (IGTYP == 16) THEN 
            II = JDIR + I-1                                                 
            R1 = DIR_A(II)
            S1 = DIR_A(II+NEL)
            R2 = DIR_B(II)
            S2 = DIR_B(II+NEL)
                                           
            RS1= R1*S1                                                         
            RS2= R2*S2                                                         
            R12A = R1*R1                                                       
            R22A = R2*R2                                                       
            S12B = S1*S1                                                       
            S22B = S2*S2                                                       

            RS3 = S1*S2-R1*R2                                                  
            R3R3= ONE+S1*R2+R1*S2                                               
            R3R3= HALF*R3R3                                                  
            S3S3= ONE-S1*R2-R1*S2                                               
            S3S3= HALF*S3S3                                                  
            T1 = SIG(1)                                                     
            T2 = SIG(2)                                                     
            T3 = SIG(3)                                                     
            SIG(1) = R12A*T1 + R22A*T2 - RS3*T3                             
            SIG(2) = S12B*T1 + S22B*T2 + RS3*T3                             
            SIG(3) = RS1*T1  + RS2*T2 + (R3R3 - S3S3)*T3                                                               
c
         ELSEIF ((IGTYP == 51 .OR. IGTYP == 52) .AND. ILAW == 58) THEN    
            II = JDIR + I-1                                                        
            R1 = DIR_A(II)
            S1 = DIR_A(II+NEL)
            R2 = DIR_B(II)
            S2 = DIR_B(II+NEL)
c
            RS1= R1*S1                                                         
            RS2= R2*S2                                                         
            R12A = R1*R1                                                       
            R22A = R2*R2                                                       
            S12B = S1*S1                                                       
            S22B = S2*S2                                                       
            RS3 = S1*S2-R1*R2                                                  
            R3R3= ONE+S1*R2+R1*S2                                               
            R3R3= HALF*R3R3                                                  
            S3S3= ONE-S1*R2-R1*S2                                               
            S3S3= HALF*S3S3                                                  
            T1 = SIG(1)                                                     
            T2 = SIG(2)                                                     
            T3 = SIG(3)                                                     
c
            SIG(1) = R12A*T1 + R22A*T2 - RS3*T3                             
            SIG(2) = S12B*T1 + S22B*T2 + RS3*T3                             
            SIG(3) = RS1*T1  + RS2*T2  + (R3R3 - S3S3)*T3                                                                 
         ELSE                                                             
            IF (ILAW /= 1 .and. ILAW /= 2 .and. ILAW /= 19 .and. ILAW /= 27 .and. ILAW /= 32) THEN
                II = JDIR + I-1                                                        
                DIR(1) = DIR_A(II)
                DIR(2) = DIR_A(II+NEL)
                S1 = DIR(1)*DIR(1)*SIG(1)
     .             + DIR(2)*DIR(2)*SIG(2)
     .             -TWO*DIR(1)*DIR(2)*SIG(3)
                S2 = DIR(2)*DIR(2)*SIG(1)
     .             + DIR(1)*DIR(1)*SIG(2)
     .             +TWO*DIR(2)*DIR(1)*SIG(3)
                S3 = DIR(1)*DIR(2)*SIG(1)
     .             - DIR(2)*DIR(1)*SIG(2)
     .             +(DIR(1)*DIR(1)-DIR(2)*DIR(2))*SIG(3)
                S4 = DIR(2)*SIG(5)+DIR(1)*SIG(4)
                S5 = DIR(1)*SIG(5)-DIR(2)*SIG(4)

                SIG(1)=S1
                SIG(2)=S2
                SIG(3)=S3
                SIG(4)=S4
                SIG(5)=S5
            ENDIF
         ENDIF 

       ENDIF
C-----------------------------------
C      Rotation Element > Global
C     TENSOR ROTATION.
C        R passage Global -> Local.
C        R S Transpose(R)
C------------------------------------

        L11    =SIG(1)
        L22    =SIG(2)
        L33    =ZERO
        L12    =SIG(3)
        L23    =SIG(4)
        L13    =SIG(5)

        R11 = E1X(I)
        R12 = E1Y(I)
        R13 = E1Z(I)

        R21 = E2X(I)
        R22 = E2Y(I)
        R23 = E2Z(I)

        R31 = E3X(I)
        R32 = E3Y(I)
        R33 = E3Z(I)

        S11    =L11*R11+L12*R12+L13*R13 
        S12    =L11*R21+L12*R22+L13*R23 
        S13    =L11*R31+L12*R32+L13*R33
        S21    =L12*R11+L22*R12+L23*R13
        S22    =L12*R21+L22*R22+L23*R23
        S23    =L12*R31+L22*R32+L23*R33 
        S31    =L13*R11+L23*R12
        S32    =L13*R21+L23*R22
        S33    =L13*R31+L23*R32

        SIG(1) =R11*S11+R12*S21+R13*S31
        SIG(2) =R21*S12+R22*S22+R23*S32
        SIG(3) =R31*S13+R32*S23+R33*S33
        SIG(4) =R11*S12+R12*S22+R13*S32
        SIG(5) =R21*S13+R22*S23+R23*S33
        SIG(6) =R11*S13+R12*S23+R13*S33
C
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SHELL_LOCAL_FRAME             source/output/dynain/shell_rota.F
Chd|-- called by -----------
Chd|        DYNAIN_C_STRSG                source/output/dynain/dynain_c_strsg.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SHELL_LOCAL_FRAME(JFT    , JLT   ,ITY      ,IHBE     ,IGTYP   ,
     2                             IXC    ,IXTG   ,NFT      ,X        ,OFFG    ,
     3                             RX     ,RY     ,RZ       ,SX       ,SY      ,  
     4                             SZ     ,E1X    ,E2X      ,E3X      ,E1Y     , 
     5                             E2Y    ,E3Y    ,E1Z      ,E2Z      ,E3Z     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, ITY, NFT ,JFT, JLT , IHBE, IGTYP ,        
     .        IXC(NIXC,*), IXTG(NIXTG,*)
      my_real X(3,*), OFFG(*)
      my_real
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), 
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ), 
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ), 
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ), 
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , K, I1,I2,I3,I4,IREL
      my_real
     .   SUMA(MVSIZ)

C--------------------------------------
C     LOCAL ELEMENT FRAME COMPUTING
C--------------------------------------

      IF (ITY == 3) THEN
C------------------------
C      shells 4 nodes
C-------------------------

         IF (IHBE>10.OR.IGTYP==16.OR.ISHFRAM ==0) THEN
           IREL=0
         ELSEIF (ISHFRAM ==1) THEN
           IREL=2
         ELSE
           IREL=1
         END IF

         DO I=JFT,JLT
          I1=IXC(2,I+NFT)
          I2=IXC(3,I+NFT)
          I3=IXC(4,I+NFT)
          I4=IXC(5,I+NFT)

          RX(I)=X(1,I2)+X(1,I3)-X(1,I1)-X(1,I4)
          SX(I)=X(1,I3)+X(1,I4)-X(1,I1)-X(1,I2)
          RY(I)=X(2,I2)+X(2,I3)-X(2,I1)-X(2,I4)
          SY(I)=X(2,I3)+X(2,I4)-X(2,I1)-X(2,I2)
          RZ(I)=X(3,I2)+X(3,I3)-X(3,I1)-X(3,I4)
          SZ(I)=X(3,I3)+X(3,I4)-X(3,I1)-X(3,I2)
         ENDDO 
C----------------------------
C     LOCAL SYSTEM
C----------------------------
         CALL CLSKEW3(JFT,JLT,IREL,
     .     RX, RY, RZ, 
     .     SX, SY, SZ, 
     .     E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,SUMA,OFFG  )

      ELSE

C---------------------
C       shells 3 nodes
C---------------------

         IF (IHBE>=30) THEN
           IREL=0
         ELSE
           IREL=2
         END IF


         DO I=JFT,JLT
          I1=IXTG(2,I+NFT)
          I2=IXTG(3,I+NFT)
          I3=IXTG(4,I+NFT)

          RX(I)=X(1,I2)-X(1,I1)
          RY(I)=X(2,I2)-X(2,I1)
          RZ(I)=X(3,I2)-X(3,I1)
          SX(I)=X(1,I3)-X(1,I1)
          SY(I)=X(2,I3)-X(2,I1)
          SZ(I)=X(3,I3)-X(3,I1)
         ENDDO 
C----------------------------
C     LOCAL SYSTEM
C----------------------------
         CALL CLSKEW3(JFT,JLT,IREL,
     .     RX, RY, RZ, 
     .     SX, SY, SZ, 
     .     E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,SUMA,OFFG  )

      ENDIF
C
C
C-----------------------------------------------
      RETURN
      END
