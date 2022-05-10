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
Chd|  S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_S4                      source/implicit/assem_s4.F    
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|        MORTHLOCK3                    source/elements/solid/solide4/s4ke3.F
Chd|        S4COORK                       source/elements/solid/solide4/s4coork.F
Chd|        S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|        S4DERIT3                      source/elements/solid/solide4/s4deri3.F
Chd|        S4KGEO3                       source/elements/solid/solide4/s4ke3.F
Chd|        S8EOFF                        source/elements/solid/solide8/s8eoff.F
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S4KE3 (PM    ,GEO   ,IXS    ,X      ,GBUF  ,
     1                  ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                  IADK  ,JDIK  ,NEL    ,
     3                  IPM   ,IGEO  ,IKGEO  ,BUFMAT )
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IKGEO
C
      INTEGER NEL  ,IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)
C
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*), 
     .   K11(9,MVSIZ) ,K12(9,MVSIZ)  ,K13(9,MVSIZ)  ,K14(9,MVSIZ)  ,
     .   K22(9,MVSIZ) ,K23(9,MVSIZ)  ,K24(9,MVSIZ)  ,K33(9,MVSIZ)  ,
     .   K34(9,MVSIZ) ,K44(9,MVSIZ)  , OFF(MVSIZ)   ,BUFMAT(*)     ,
     .   K_DIAG(*)   ,K_LT(*)
      TYPE(G_BUFEL_)  :: GBUF     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LCO, NF1, IFLAG, NB3S, I,IS,IAD0,IBID,NBGAMA,IBID1
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),PID,IADBUF,IKORTH
      my_real
     . RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     . SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     . TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) ,
     .  E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) , 
     .  E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) , 
     .  E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) ,
     . VOLN(MVSIZ), DELTAX(MVSIZ), BID(1)
C----------------
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ) 
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ)
C                                                                     12
       my_real
     .  HH(2,MVSIZ),DD(9,MVSIZ),GG(MVSIZ),DM(9,MVSIZ),GM(9,MVSIZ),
     .      DGM(9,MVSIZ),DG(9,MVSIZ),G33(9,MVSIZ),GAMA(MVSIZ,6)
C----HH(1,):lamda,HH(2,):G
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (ISORTH>0) THEN
       IKORTH=1
      ELSE
       IKORTH=0
      ENDIF
C-----------
      NF1=NFT+1
C-----------
      CALL S4COORK(X,IXS(1,NF1),                                    
     .X1, X2, X3, X4,                                               
     .Y1, Y2, Y3, Y4,                                               
     .Z1, Z2, Z3, Z4,                                               
     .GBUF%OFF,OFF,GBUF%SMSTR,                                      
     .NC1,NC2,NC3,NC4,NGL,MXT,NGEO,                                 
     .K11,K12,K13,K14,K22,K23,                                      
     .K24,K33,K34,K44,NEL)                                              
      CALL S4DERIT3(OFF,VOLN,NGL,DELTAX,MXT,                             
     . X1, X2, X3, X4, Y1, Y2, Y3, Y4,                              
     . Z1, Z2, Z3, Z4, PX1, PX2, PX3, PX4,                          
     . PY1, PY2, PY3, PY4, PZ1, PZ2, PZ3, PZ4,                      
     . RX,  RY,  RZ,  SX,  SY,  SZ, TX , TY , TZ,
     . PM,VOLDP)                   
      CALL SREPLOC3(                                                
     .     RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,   
     .     E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )   
      IF (ISORTH == 0) THEN            
        DO I=LFT,LLT                                            
          GAMA(I,1) = UN                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = UN                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SORTHDIR3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  , 
     .       GBUF%GAMA,GAMA )
        CALL MORTHLOCK3(LFT,LLT ,GAMA,E1X,E2X,E3X,
     +                  E1Y,E2Y ,E3Y ,E1Z,E2Z,E3Z)
      ENDIF
c
       IF (MTN.GE.28) THEN
        IADBUF = IPM(7,MXT(1))
       ELSE
        IADBUF = 0
       ENDIF
       CALL MMATS(LFT    ,LLT     ,PM    ,MXT    ,HH    ,
     .            MTN    ,IKORTH  ,IPM   ,IGEO   ,GAMA  ,
     .            BUFMAT(IADBUF)  ,DM    ,DGM    ,GM    ,
     .            JHBE  ,GBUF%SIG ,BID   ,IBID1  ,NEL   )
        IBID = 0
        IBID1 = 1
        CALL MMSTIFS(PM   ,MXT  ,HH    ,VOLN  ,IBID  ,
     .              DD   ,GG   ,DG    ,G33   ,DM     ,
     .              GM   ,DGM  ,IKORTH,GBUF%SIG,IBID1 ,
     .              IBID1,IBID1,NEL   )
        CALL S4CUMG3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   K11 ,K12  ,K13  ,K14  ,K22  ,
     .   K23 ,K24  ,K33  ,K34  ,K44  ,
     .   DD  ,GG   ,DG   ,G33  ,IKORTH)
C----------------------------
C     geometrical stiffness (membrane only)
C----------------------------
      IF (IKGEO.GT.0) THEN
       CALL S4KGEO3(GBUF%SIG,VOLN ,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   K11,K12,K13,K14,K22,K23,
     .   K24,K33,K34,K44,NEL)
      ENDIF 
      
      IF (NEIG.GT.0) CALL S8EOFF(
     1   LFT, LLT, IXS(1,NF1), ETAG, OFF)
      CALL ASSEM_S4(
     1   IXS(1,NF1),NEL   ,IDDL  ,NDOF  ,K_DIAG,
     2   K_LT  ,IADK  ,JDIK  ,K11   ,K12   ,     
     3   K13   ,K14   ,K22   ,K23   ,K24   ,
     4   K33   ,K34   ,K44   ,OFF   )      
C
      RETURN
      END
Chd|====================================================================
Chd|  S4KGEO3                       source/elements/solid/solide4/s4ke3.F
Chd|-- called by -----------
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|        S4KEH3                        source/elements/solid/solide4/s4ke3.F
Chd|====================================================================
      SUBROUTINE S4KGEO3(SIG , VOL,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   K11,K12,K13,K14,K22,K23,
     .   K24,K33,K34,K44,NEL)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL ou REAL*8
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*) ,
     .   K22(3,3,*) ,K23(3,3,*)  ,K24(3,3,*),
     .   K33(3,3,*),K34(3,3,*),
     .   K44(3,3,*),VOL(*),SIG(NEL,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL ou REAL*8
      my_real
     .   FXX(MVSIZ),FYY(MVSIZ),FZZ(MVSIZ),FXY(MVSIZ),FXZ(MVSIZ),
     .   FYZ(MVSIZ)
C-----------------------------------------------
       DO I=LFT,LLT
        FXX(I)=SIG(I,1)*VOL(I)
        FYY(I)=SIG(I,2)*VOL(I)
        FZZ(I)=SIG(I,3)*VOL(I)
        FXY(I)=SIG(I,4)*VOL(I)
        FYZ(I)=SIG(I,5)*VOL(I)
        FXZ(I)=SIG(I,6)*VOL(I)
       ENDDO
C -----------------------------------------------
      CALL S4KEH3(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K11 )
      CALL S4KEH3(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K22 )
      CALL S4KEH3(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K33 )
      CALL S4KEH3(LFT,LLT,
     .   PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K44 )
      CALL S4KEH3(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K12 )
      CALL S4KEH3(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K13 )
      CALL S4KEH3(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K14 )
      CALL S4KEH3(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K23 )
      CALL S4KEH3(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K24 )
      CALL S4KEH3(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K34 )
C
      RETURN
      END
Chd|====================================================================
Chd|  S4KEH3                        source/elements/solid/solide4/s4ke3.F
Chd|-- called by -----------
Chd|        S10KGEO3                      source/elements/solid/solide10/s10ke3.F
Chd|        S20KGEO3                      source/elements/solid/solide20/s20ke3.F
Chd|        S4KGEO3                       source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|        S8ZKEH3                       source/elements/solid/solide8z/s8zke3.F
Chd|====================================================================
      SUBROUTINE S4KEH3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,KIJ )
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
      INTEGER LFT,LLT
C     REAL ou REAL*8
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),  
     .   FXX(*),FYY(*),FZZ(*),FXY(*),FXZ(*),FYZ(*),KIJ(3,3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL ou REAL*8
      my_real
     .   HIJ(MVSIZ) 
C-----------------------------------------------
      CALL S8ZKEH3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C
       DO J = 1,3
        DO I=LFT,LLT
         KIJ(J,J,I)=KIJ(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  MORTHLOCK3                    source/elements/solid/solide4/s4ke3.F
Chd|-- called by -----------
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MORTHLOCK3(LFT,LLT ,GAMA,E1X,E2X,E3X,
     2                      E1Y,E2Y ,E3Y ,E1Z,E2Z,E3Z)
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
      INTEGER LFT,LLT
      my_real  
     .  E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) , 
     .  E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) , 
     .  E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) ,GAMA(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   S11, S12, S13, S21, S22, S31,S32,S1,S2
C-----gama -> orthotropic system [{a}{b}{c}]---
        DO I=LFT,LLT
          S11 = GAMA(I,1)*E1X(I)+GAMA(I,2)*E2X(I)+GAMA(I,3)*E3X(I)
          S21 = GAMA(I,1)*E1Y(I)+GAMA(I,2)*E2Y(I)+GAMA(I,3)*E3Y(I)
          S31 = GAMA(I,1)*E1Z(I)+GAMA(I,2)*E2Z(I)+GAMA(I,3)*E3Z(I)
C          
          S12 = GAMA(I,4)*E1X(I)+GAMA(I,5)*E2X(I)+GAMA(I,6)*E3X(I)
          S22 = GAMA(I,4)*E1Y(I)+GAMA(I,5)*E2Y(I)+GAMA(I,6)*E3Y(I)
          S32 = GAMA(I,4)*E1Z(I)+GAMA(I,5)*E2Z(I)+GAMA(I,6)*E3Z(I)
          GAMA(I,1) = S11                                  
          GAMA(I,2) = S21                                  
          GAMA(I,3) = S31 
          GAMA(I,4) = S12                                  
          GAMA(I,5) = S22                                  
          GAMA(I,6) = S32
       END DO
C
      RETURN
      END
