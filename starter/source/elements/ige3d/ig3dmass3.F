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
Chd|  IG3DMASS3                     source/elements/ige3d/ig3dmass3.F
Chd|-- called by -----------
Chd|        IG3DINIT3                     source/elements/ige3d/ig3dinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IG3DMASS3(
     1  RHO    ,MS    ,PARTSAV,X     ,Y      ,
     2  Z      ,VX    ,VY     ,VZ    ,IPART  ,
     3  MSIG3D ,VOLU  ,MSNF   ,MSSF  ,IN     ,
     4  VR     ,INS   ,WMA    ,RHOCP ,MCP    ,
     5  MCPS   ,MSSA  ,RHOF   ,FRAC  ,NCTRL  ,
     6  KXIG3D ,IXIG3D,R      ,DETJAC,PGAUSS ,
     7  I )
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
      INTEGER IPART(*),NDDIM, NCTRL, I, KXIG3D(NIXIG3D,*),IXIG3D(*)
      my_real
     .   RHO(*),MS(*),X(NCTRL,*),
     .   Y(NCTRL,*),Z(NCTRL,*),
     .   VX(NCTRL,MVSIZ),VY(NCTRL,MVSIZ),VZ(NCTRL,MVSIZ),
     .   PARTSAV(20,*),VOLU(*),
     .   MSIG3D(NUMELIG3D,*), MSSF(8,*), MSNF(*),
     .   IN(*),VR(3,*),INS(8,*),WMA(*),
     .   RHOCP(*),MCP(*),MCPS(8,*), MSSA(*),RHOF(*),
     .   FRAC(*),PGAUSS,DETJAC,R(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com07_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "ale_param.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,IP,I1,I2,I3,I4,I5,I6,I7,I8, ITNCTRL, INCTRL
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX,INER,RCP
      my_real
     .   MASS(MVSIZ),MASSF(MVSIZ)

C=======================================================================
C   S o u r c e  L i n e s
C=======================================================================
C
      XX=ZERO
      XY=ZERO
      YY=ZERO
      YZ=ZERO
      ZZ=ZERO
      ZX=ZERO
C
      DO ITNCTRL=1,NCTRL
        MASS(I)= RHO(I)*R(ITNCTRL)*DETJAC*PGAUSS
        MASSF(I)= FRAC(I)*RHOF(I)*R(ITNCTRL)*DETJAC*PGAUSS
        MSIG3D(I+NFT,ITNCTRL)=MSIG3D(I+NFT,ITNCTRL) + MASS(I)
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + MASS(I)
c
        PARTSAV(2,IP)=PARTSAV(2,IP) + MASS(I)*X(ITNCTRL,I)
        PARTSAV(3,IP)=PARTSAV(3,IP) + MASS(I)*Y(ITNCTRL,I)
        PARTSAV(4,IP)=PARTSAV(4,IP) + MASS(I)*Z(ITNCTRL,I)
c
        XX = XX + MASS(I) * X(ITNCTRL,I)*X(ITNCTRL,I)
        XY = XY + MASS(I) * X(ITNCTRL,I)*Y(ITNCTRL,I)
        YY = YY + MASS(I) * Y(ITNCTRL,I)*Y(ITNCTRL,I)
        YZ = YZ + MASS(I) * Y(ITNCTRL,I)*Z(ITNCTRL,I)
        ZZ = ZZ + MASS(I) * Z(ITNCTRL,I)*Z(ITNCTRL,I)
        ZX = ZX + MASS(I) * Z(ITNCTRL,I)*X(ITNCTRL,I)
      ENDDO
C
      PARTSAV(5,IP) =PARTSAV(5,IP)  + (YY+ZZ)
      PARTSAV(6,IP) =PARTSAV(6,IP)  + (ZZ+XX)
      PARTSAV(7,IP) =PARTSAV(7,IP)  + (XX+YY)
      PARTSAV(8,IP) =PARTSAV(8,IP)  - XY
      PARTSAV(9,IP) =PARTSAV(9,IP)  - YZ
      PARTSAV(10,IP)=PARTSAV(10,IP) - ZX
C
      DO ITNCTRL=1,NCTRL
      PARTSAV(11,IP)=PARTSAV(11,IP) + MASS(I)*
     .     VX(ITNCTRL,I)
      PARTSAV(12,IP)=PARTSAV(12,IP) + MASS(I)*
     .     VY(ITNCTRL,I)
      PARTSAV(13,IP)=PARTSAV(13,IP) + MASS(I)*
     .     VZ(ITNCTRL,I)
      PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * MASS(I) *
     .   (VX(ITNCTRL,I)*VX(ITNCTRL,I) +
     .    VY(ITNCTRL,I)*VY(ITNCTRL,I) +
     .    VZ(ITNCTRL,I)*VZ(ITNCTRL,I))
      ENDDO
C
      IF(IREST_MSELT /= 0)THEN
        MSSA(NFT+I)=MASS(I)
      ENDIF
C       
      IF(JALE == 3 .AND. JLAG == 1)THEN
        DO ITNCTRL=1,NCTRL
          MSSF(ITNCTRL,I)=MASSF(I)
        ENDDO
      ELSEIF(JALE+JEUL>0)THEN
        DO ITNCTRL=1,NCTRL
          MSSF(ITNCTRL,I)=MASS(I)
        ENDDO
      ENDIF
C
c       IF(JTHE < 0 ) THEN
c           RCP=RHOCP(I)*VOLU(I)/NCTRL
c           DO ITNCTRL=1,NCTRL
c             MCPS(ITNCTRL,I) =RCP
c           ENDDO
c       ENDIF
c       IF(ISROT==1)THEN
c        IF(IRODDL==0)THEN
C prov gw
c          STOP 1119
c        ENDIF
c         INER=(MASS(I)*VOLU(I)**TWO_THIRD)/SIX
c         DO ITNCTRL=1,NCTRL
c           INS(ITNCTRL,I)=INER
c         ENDDO
c       ENDIF
C
      IF(JALE > 0 .AND. NWALE == 4)THEN
         DO ITNCTRL=1,NCTRL
          INCTRL = IXIG3D(KXIG3D(4,I+NFT)+ITNCTRL-1)
          WMA(INCTRL)=WMA(INCTRL)+THREE_HALF
         ENDDO
      ENDIF
C-----------
      RETURN
      END
