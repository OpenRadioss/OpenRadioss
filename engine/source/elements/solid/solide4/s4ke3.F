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
Chd|  S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_S4                      source/implicit/assem_s4.F    
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmstifs.F
Chd|        MORTHLOCK3                    source/elements/solid/solide4/morthlock3.F
Chd|        S4COORK                       source/elements/solid/solide4/s4coork.F
Chd|        S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|        S4DERIT3                      source/elements/solid/solide4/s4derit3.F
Chd|        S4KGEO3                       source/elements/solid/solide4/s4kgeo3.F
Chd|        S8EOFF                        source/elements/solid/solide8/s8eoff.F
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S4KE3(
     1   PM,      GEO,     IXS,     X,
     2   GBUF,    ETAG,    IDDL,    NDOF,
     3   K_DIAG,  K_LT,    IADK,    JDIK,
     4   NEL,     IPM,     IGEO,    IKGEO,
     5   BUFMAT,  NFT,     MTN,     ISMSTR,
     6   JHBE,    IREP,    ISORTH,  IFORMDT)
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IFORMDT
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: IREP
      INTEGER, INTENT(IN) :: ISORTH
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
      CALL S4COORK(
     1   X,         IXS(1,NF1),X1,        X2,
     2   X3,        X4,        Y1,        Y2,
     3   Y3,        Y4,        Z1,        Z2,
     4   Z3,        Z4,        GBUF%OFF,  OFF,
     5   GBUF%SMSTR,NC1,       NC2,       NC3,
     6   NC4,       NGL,       MXT,       NGEO,
     7   K11,       K12,       K13,       K14,
     8   K22,       K23,       K24,       K33,
     9   K34,       K44,       NEL,       ISMSTR)
      CALL S4DERIT3(
     1   OFF,     VOLN,    NGL,     DELTAX,
     2   MXT,     X1,      X2,      X3,
     3   X4,      Y1,      Y2,      Y3,
     4   Y4,      Z1,      Z2,      Z3,
     5   Z4,      PX1,     PX2,     PX3,
     6   PX4,     PY1,     PY2,     PY3,
     7   PY4,     PZ1,     PZ2,     PZ3,
     8   PZ4,     RX,      RY,      RZ,
     9   SX,      SY,      SZ,      TX,
     A   TY,      TZ,      PM,      VOLDP,
     B   NEL,     IFORMDT)
      CALL SREPLOC3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      E1X,     E2X,     E3X,
     4   E1Y,     E2Y,     E3Y,     E1Z,
     5   E2Z,     E3Z,     NEL)
      IF (ISORTH == 0) THEN            
        DO I=1,NEL                                            
          GAMA(I,1) = ONE                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = ONE                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SORTHDIR3(
     1   RX,       RY,       RZ,       SX,
     2   SY,       SZ,       TX,       TY,
     3   TZ,       E1X,      E2X,      E3X,
     4   E1Y,      E2Y,      E3Y,      E1Z,
     5   E2Z,      E3Z,      GBUF%GAMA,GAMA,
     6   NEL,      IREP)
        CALL MORTHLOCK3(1,NEL ,GAMA,E1X,E2X,E3X,
     +                  E1Y,E2Y ,E3Y ,E1Z,E2Z,E3Z)
      ENDIF
c
       IF (MTN>=28) THEN
        IADBUF = IPM(7,MXT(1))
       ELSE
        IADBUF = 0
       ENDIF
       CALL MMATS(1    ,NEL     ,PM    ,MXT    ,HH    ,
     .            MTN    ,IKORTH  ,IPM   ,IGEO   ,GAMA  ,
     .            BUFMAT(IADBUF)  ,DM    ,DGM    ,GM    ,
     .            JHBE  ,GBUF%SIG ,BID   ,IBID1  ,NEL   )
        IBID = 0
        IBID1 = 1
        CALL MMSTIFS(
     1   PM,      MXT,     HH,      VOLN,
     2   IBID,    DD,      GG,      DG,
     3   G33,     DM,      GM,      DGM,
     4   IKORTH,  GBUF%SIG,IBID1,   IBID1,
     5   IBID1,   NEL,     JHBE,    MTN)
        CALL S4CUMG3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   K11,     K12,     K13,     K14,
     5   K22,     K23,     K24,     K33,
     6   K34,     K44,     DD,      GG,
     7   DG,      G33,     IKORTH,  NEL)
C----------------------------
C     geometrical stiffness (membrane only)
C----------------------------
      IF (IKGEO>0) THEN
       CALL S4KGEO3(
     1   GBUF%SIG,VOLN,    PX1,     PX2,
     2   PX3,     PX4,     PY1,     PY2,
     3   PY3,     PY4,     PZ1,     PZ2,
     4   PZ3,     PZ4,     K11,     K12,
     5   K13,     K14,     K22,     K23,
     6   K24,     K33,     K34,     K44,
     7   NEL)
      ENDIF 
      
      IF (NEIG>0) CALL S8EOFF(
     1   1, NEL, IXS(1,NF1), ETAG, OFF)
      CALL ASSEM_S4(
     1   IXS(1,NF1),NEL   ,IDDL  ,NDOF  ,K_DIAG,
     2   K_LT  ,IADK  ,JDIK  ,K11   ,K12   ,     
     3   K13   ,K14   ,K22   ,K23   ,K24   ,
     4   K33   ,K34   ,K44   ,OFF   )      
C
      RETURN
      END
