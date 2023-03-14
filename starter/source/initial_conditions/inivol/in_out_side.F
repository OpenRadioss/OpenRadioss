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
Chd|  IN_OUT_SIDE                   source/initial_conditions/inivol/in_out_side.F
Chd|-- called by -----------
Chd|        PHASE_DETECTION               source/initial_conditions/inivol/phase_detection.F
Chd|        RATIO_FILL                    source/initial_conditions/inivol/ratio_fill.F
Chd|-- calls ---------------
Chd|        NFACETTE                      source/initial_conditions/inivol/nfacette.F
Chd|====================================================================
      SUBROUTINE IN_OUT_SIDE(
     .       INOD      ,INOD2SURF ,KNOD2SURF ,NNOD2SURF ,X         ,
     .       XN        ,DIST      ,NSEG      ,SURF_ELTYP,NOD_NORMAL,
     .       SURF_NODES,SWIFTSURF ,IDSURF    ,SEGTOSURF )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INOD,KNOD2SURF(*),NNOD2SURF,
     .  INOD2SURF(NNOD2SURF,*),NSEG,SURF_ELTYP(NSEG),
     .  SURF_NODES(NSEG,4),SWIFTSURF(NSURF),IDSURF,SEGTOSURF(*)
      my_real
     . X(3,*),XN(3),DIST,
     . NOD_NORMAL(3,*),XFAS(3,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,OK,FAC,FAC1,FAC2,NULL_DIST,
     . IPL,IXPL(4),
     . p,r,p1,p2,dd1(4),dd2(4),ddd1(3),ddd2(3),ITYP,SH_ELEM,ISURF
      my_real
     . DIST_PL(3),VX_NOD_INOD,VY_NOD_INOD,VZ_NOD_INOD,XSIGN(3),
     . V1X,V1Y,V1Z,V2X,V2Y,V2Z,V3X,V3Y,V3Z,V12X,V12Y,V12Z,DOT,
     . NX,NY,NZ
      DATA dd1/4,1,2,3/,dd2/2,3,4,1/,ddd1/3,1,2/,ddd2/2,3,1/
C-----------------------------------------------
        OK = 0
        NULL_DIST = 0
        DO 60 K=1,KNOD2SURF(INOD)
          IPL = INOD2SURF(K,INOD)
          ISURF = SEGTOSURF(IPL)
          IF (ISURF /= IDSURF) GOTO 60
          IPL = IPL - SWIFTSURF(ISURF)
          IF (IPL <= 0 .OR. IPL > NSEG .OR. INOD > NUMNOD) GOTO 60
          ITYP = SURF_ELTYP(IPL)
!
          IF(ITYP==3)THEN
            IXPL(1) = SURF_NODES(IPL,1)
            IXPL(2) = SURF_NODES(IPL,2)
            IXPL(3) = SURF_NODES(IPL,3)
            IXPL(4) = SURF_NODES(IPL,4)
          ELSEIF(ITYP==7)THEN
            IXPL(1) = SURF_NODES(IPL,1)
            IXPL(2) = SURF_NODES(IPL,2)
            IXPL(3) = SURF_NODES(IPL,3)
            IXPL(4) = SURF_NODES(IPL,3)
          ENDIF
C---
          p = 0
C---
          IF(ITYP==3)THEN
           DO r=1,4
             IF(INOD == IXPL(r))THEN
               p = r
               EXIT
             ENDIF
           ENDDO
          ELSEIF(ITYP==7)THEN
           DO r=1,3
             IF(INOD == IXPL(r))THEN
               p = r
               EXIT
             ENDIF
           ENDDO
          ENDIF
C
          IF(p == 0)GOTO 60
C
          IF(ITYP==3)THEN
            p1 = dd1(p)
            p2 = dd2(p)
          ELSEIF(ITYP==7)THEN
            p1 = ddd1(p)
            p2 = ddd2(p)
          ENDIF
C
          XFAS(1,1) = X(1,IXPL(1))
          XFAS(2,1) = X(2,IXPL(1))
          XFAS(3,1) = X(3,IXPL(1))
          XFAS(1,2) = X(1,IXPL(2))
          XFAS(2,2) = X(2,IXPL(2))
          XFAS(3,2) = X(3,IXPL(2))
          XFAS(1,3) = X(1,IXPL(3))
          XFAS(2,3) = X(2,IXPL(3))
          XFAS(3,3) = X(3,IXPL(3))
          XFAS(1,4) = X(1,IXPL(4))
          XFAS(2,4) = X(2,IXPL(4))
          XFAS(3,4) = X(3,IXPL(4))
C
          CALL NFACETTE(XFAS,NX,NY,NZ)
C
          DIST_PL(1:3) = ZERO
C vect NOD --> INOD
          VX_NOD_INOD = XN(1) - X(1,INOD)
          VY_NOD_INOD = XN(2) - X(2,INOD)
          VZ_NOD_INOD = XN(3) - X(3,INOD)
C PLANE 1
C V1xV2
          V12X = NX
          V12Y = NY
          V12Z = NZ
C dist of N to plane 1
          DOT = VX_NOD_INOD*V12X+VY_NOD_INOD*V12Y+VZ_NOD_INOD*V12Z
          DIST_PL(1) = DOT
C PLANE 2
C V_NOR x V1
          V1X = NOD_NORMAL(1,INOD)
          V1Y = NOD_NORMAL(2,INOD)
          V1Z = NOD_NORMAL(3,INOD)
          V2X = XFAS(1,p)-XFAS(1,p1)
          V2Y = XFAS(2,p)-XFAS(2,p1)
          V2Z = XFAS(3,p)-XFAS(3,p1)
          V12X = V1Y*V2Z-V1Z*V2Y
          V12Y = V1Z*V2X-V1X*V2Z
          V12Z = V1X*V2Y-V1Y*V2X
C dist of N to plane 2
          DOT = VX_NOD_INOD*V12X+VY_NOD_INOD*V12Y+VZ_NOD_INOD*V12Z
          DIST_PL(2) = DOT
C PLANE 3
C V_NOR x V2 
          V1X = NOD_NORMAL(1,INOD)
          V1Y = NOD_NORMAL(2,INOD)
          V1Z = NOD_NORMAL(3,INOD)
          V2X = XFAS(1,p2)-XFAS(1,p)
          V2Y = XFAS(2,p2)-XFAS(2,p)
          V2Z = XFAS(3,p2)-XFAS(3,p)
          V12X = V1Y*V2Z-V1Z*V2Y
          V12Y = V1Z*V2X-V1X*V2Z
          V12Z = V1X*V2Y-V1Y*V2X
C dist of N to plane 3
          DOT = VX_NOD_INOD*V12X+VY_NOD_INOD*V12Y+VZ_NOD_INOD*V12Z
          DIST_PL(3) = DOT
C---
          DO r=1,3
            XSIGN(r) = SIGN(ONE,DIST_PL(r))
            IF(DIST_PL(r) == ZERO)XSIGN(r) = ONE !ZERO
          ENDDO
          FAC1 = 0
          FAC2 = 0
C
          IF(NULL_DIST == 1)EXIT
C
          DO r=1,3
            IF(XSIGN(r) >= ZERO)THEN
              FAC1 = FAC1 + 1
            ENDIF
          ENDDO
          IF(FAC1 == 3)THEN
            DIST = ONE
            OK = OK + 1
          ENDIF
          IF(OK > 0)EXIT
C---
 60     CONTINUE
C---
c        IF(NULL_DIST == 1) DIST = ZERO
        IF(OK == 0) DIST = -ONE  ! external node
        IF(NULL_DIST == 1) DIST = ZERO
C-----------------------------------------------
      RETURN
      END
