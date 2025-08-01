Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    ratio_fill       ../starter/source/initial_conditions/inivol/ratio_fill.F
!||--- called by ------------------------------------------------------
!||    inisoldist       ../starter/source/initial_conditions/inivol/inisoldist.F
!||--- calls      -----------------------------------------------------
!||    in_out_side      ../starter/source/initial_conditions/inivol/in_out_side.F
!||--- uses       -----------------------------------------------------
!||    inivol_def_mod   ../starter/share/modules1/inivol_mod.F
!||====================================================================
      SUBROUTINE RATIO_FILL(
     .    X1        ,X2        ,X3        ,X4          ,X5        ,X6      ,X7         ,X8       ,
     .    Y1        ,Y2        ,Y3        ,Y4          ,Y5        ,Y6      ,Y7         ,Y8       ,
     .    Z1        ,Z2        ,Z3        ,Z4          ,Z5        ,Z6      ,Z7         ,Z8       ,
     .    IDP       ,X         ,IXS       ,IPART_      ,IFILL     ,NTRACE  ,NTRACE0    ,DIS      ,
     .    NSOLTOSF  ,NNOD2SURF ,INOD2SURF ,KNOD2SURF   ,JMID      ,IPHASE  ,INPHASE    ,KVOL     ,
     .    SURF_TYPE ,IAD_BUFR  ,BUFSF     ,NOD_NORMAL  ,ISOLNOD   ,NBSUBMAT,FILL_RATIO ,ICUMU    ,
     .    NSEG      ,SURF_ELTYP,SURF_NODES,NBCONTY     ,IDC       ,NBIP    ,IDSURF     ,SWIFTSURF,
     .    SEGTOSURF ,IGRSURF   ,IVOLSURF  ,NSURF_INVOL ,IXQ       ,IXTG    ,ITYP       ,NEL      ,
     .    NUMEL_TOT ,NUM_INIVOL,INIVOL    ,I_INIVOL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE INIVOL_DEF_MOD , ONLY : INIVOL_STRUCT_
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
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: I_INIVOL !< inivol identifier
      INTEGER,INTENT(IN) :: NUM_INIVOL !< number of inivol options
      TYPE (INIVOL_STRUCT_), DIMENSION(NUM_INIVOL), INTENT(INOUT) :: INIVOL        !< inivol data structure
      INTEGER, INTENT(IN) :: NEL !< number of element in current group NG
      INTEGER, INTENT(IN) :: NBSUBMAT !< total number of submaterial
      my_real, INTENT(INOUT) :: KVOL(NBSUBMAT,NEL) !< working array for volume fractions
      INTEGER NTRACE,NTRACE0,IFILL,JMID,IDP,NSEG,NBCONTY,IDC,NNOD2SURF,
     .        ISOLNOD,ICUMU,SURF_TYPE,IAD_BUFR,IDSURF,IVOLSURF(NSURF),NUMEL_TOT
      INTEGER IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),IXTG(NIXTG,NUMELTG),IPART_(*),NSOLTOSF(NBCONTY,*),
     .        INOD2SURF(NNOD2SURF,*),KNOD2SURF(*),IPHASE(NBSUBMAT+1,NUMEL_TOT),
     .        INPHASE(NTRACE,NEL),SURF_ELTYP(NSEG),ITYP,
     .        SURF_NODES(NSEG,4),NBIP(NBSUBMAT,*),SWIFTSURF(NSURF),SEGTOSURF(*),NSURF_INVOL
      my_real X1(*),X2(*),X3(*),X4(*),X5(*),X6(*),X7(*),X8(*),
     .        Y1(*),Y2(*),Y3(*),Y4(*),Y5(*),Y6(*),Y7(*),Y8(*),
     .        Z1(*),Z2(*),Z3(*),Z4(*),Z5(*),Z6(*),Z7(*),Z8(*),
     .        X(3,*),DIS(NSURF_INVOL,*),BUFSF(*),NOD_NORMAL(3,*),FILL_RATIO
      TYPE (SURF_), DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,JJ,K,N,N1,N2,N3,K1,K2,K3,OK,OK1,OK2,OK3,INOD,
     .        IE,NSH,IPL,IP,IXPL(4),GETEL,NPHASE,IPH,NIP,IAD0,
     .        IX(8),NPOINT,NTRACE_TOT,JMID_OLD,ISURF
      INTEGER FULL(MVSIZ),JCT(MVSIZ),TRACEP(MVSIZ),TRACEN(MVSIZ)
      INTEGER BUFFILL1(NBSUBMAT),BUFFILL2(NBSUBMAT,MVSIZ),ELEM_NUMNOD,ISUBMAT_TO_SUBSTRACT

      my_real :: KVOL_BAK(NBSUBMAT)
      my_real
     .  XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,DX,DY,DZ,XX(3,8),
     .  XK(NTRACE0),YK(NTRACE0),ZK(NTRACE0),XFAS(3,4),
     .  X0,Y0,Z0,DIST,DIST_OLD,tmpSUM,XN(3),
     .  XK0(NTRACE),YK0(NTRACE),ZK0(NTRACE),
     .  L12(3,3),L23(3,3),L31(3,3),LL(3,3),
     .  COEF,AAA(3),BBB(3),CCC(3),CG(3)
      my_real
     .   XS(NTRACE,MVSIZ),YS(NTRACE,MVSIZ),ZS(NTRACE,MVSIZ),
     .   DISP(NTRACE,MVSIZ),XP1,YP1,ZP1,XP2,YP2,ZP2,AA,BB,CC,
     .   XG,YG,ZG,SKW(9),DGR,TMP(3),X_PRIME,Y_PRIME,Z_PRIME,
     .   VF_TO_SUBSTRACT
      my_real :: SUMVF

      INTEGER :: d1(4),d2(4),d3(4),d4(4)
      DATA d1/1,2,3,4/,d2/2,3,4,1/,d3/3,4,1,2/,d4/4,1,2,3/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ELEM_NUMNOD = -HUGE(ELEM_NUMNOD)
      ! check part to fill with current phase (JMID)
      K = 0
      DO I=1,NEL
        IF (IPART_(I) /= IDP) CYCLE
        K = K + 1
      ENDDO
      IF (K == 0) RETURN

      DO I=1,NEL
        FULL(I)            = 0
        TRACEP(I)          = 0
        TRACEN(I)          = 0
      ENDDO

      DISP(1:NTRACE,1:MVSIZ) = ZERO
      JMID_OLD = 0

      ! search for cut solid elems by containers
      DO I=1,NEL
        IF (IPART_(I) /= IDP) CYCLE
        K = 0
        OK1 = 0
        OK2 = 0
        OK3 = 0
        IF(N2D == 0)THEN
          IF (ISOLNOD == 4) THEN
            IX(1) =IXS(2,I)
            IX(2) =IXS(4,I)
            IX(3) =IXS(7,I)
            IX(4) =IXS(6,I)
            ELEM_NUMNOD = 4
          ELSEIF (ISOLNOD == 8) THEN
            IX(1) =IXS(2,I)
            IX(2) =IXS(3,I)
            IX(3) =IXS(4,I)
            IX(4) =IXS(5,I)
            IX(5) =IXS(6,I)
            IX(6) =IXS(7,I)
            IX(7) =IXS(8,I)
            IX(8) =IXS(9,I)
            ELEM_NUMNOD = 8
          ELSE
            CYCLE !not a solid elem
          ENDIF
        ELSE
          IF(ITYP == 7)THEN
            IX(1) =IXTG(2,I)
            IX(2) =IXTG(3,I)
            IX(3) =IXTG(4,I)
            IX(4) =0
            ELEM_NUMNOD = 3
          ELSEIF(ITYP == 2)THEN
            IX(1) =IXQ(2,I)
            IX(2) =IXQ(3,I)
            IX(3) =IXQ(4,I)
            IX(4) =IXQ(5,I)
            ELEM_NUMNOD = 4
          ELSE
            CYCLE ! not a solid elem
          ENDIF
        ENDIF
        DO J=1,ELEM_NUMNOD
          N = IX(J)
          IF (DIS(IVOLSURF(IDSURF),N) /= ZERO) THEN
            K = K + 1
            IF (DIS(IVOLSURF(IDSURF),N) > ZERO) THEN
              OK1 = OK1 + 1
            ELSEIF (DIS(IVOLSURF(IDSURF),N) < ZERO) THEN
              OK2 = OK2 + 1
            ENDIF
          ENDIF
          IF (DIS(IVOLSURF(IDSURF),N) == ZERO) OK3 = OK3 + 1
        ENDDO

        IF (K > 0) THEN
          IF (OK1 == ELEM_NUMNOD .OR. (OK1+OK3) == ELEM_NUMNOD) THEN
            FULL(I) = 1
          ELSEIF (OK2 == ELEM_NUMNOD .OR. (OK2+OK3) == ELEM_NUMNOD) THEN
            FULL(I) = -1
          ELSEIF (OK1 > 0 .AND. OK2 > 0) THEN
            FULL(I) = 2
          ENDIF
        ENDIF ! IF (K > 0)
      ENDDO  !  DO I=1,NEL

      IE = 0
      DO I=1,NEL
        JCT(I) = 0
        IF(FULL(I) == 2)THEN
          IE = IE + 1
          JCT(IE) = I
        END IF
      END DO
      GETEL = IE

      ! get trace samples coordinates:

      IF (ISOLNOD == 4 .OR. N2D/=0) THEN
        DO I=1,NEL
          NPOINT = 0
          IF (IPART_(I) /= IDP) CYCLE
          XX(1,1)=X1(I)
          XX(2,1)=Y1(I)
          XX(3,1)=Z1(I)
          XX(1,2)=X2(I)
          XX(2,2)=Y2(I)
          XX(3,2)=Z2(I)
          XX(1,3)=X3(I)
          XX(2,3)=Y3(I)
          XX(3,3)=Z3(I)
          XX(1,4)=X4(I)
          XX(2,4)=Y4(I)
          XX(3,4)=Z4(I)
          DO K=1,4
            ! LEVEL - 1 -> 1 SAMPLE POINT
            NPOINT = NPOINT + 1
            CG(1) = THIRD*(XX(1,d1(K))+XX(1,d2(K))+XX(1,d3(K)))
            CG(2) = THIRD*(XX(2,d1(K))+XX(2,d2(K))+XX(2,d3(K)))
            CG(3) = THIRD*(XX(3,d1(K))+XX(3,d2(K))+XX(3,d3(K)))
            ! COEF = 1.0/4.0
            COEF = FOURTH
            AAA(1) = XX(1,d4(K))+COEF*(XX(1,d1(K))-XX(1,d4(K)))
            AAA(2) = XX(2,d4(K))+COEF*(XX(2,d1(K))-XX(2,d4(K)))
            AAA(3) = XX(3,d4(K))+COEF*(XX(3,d1(K))-XX(3,d4(K)))
            BBB(1) = XX(1,d4(K))+COEF*(XX(1,d2(K))-XX(1,d4(K)))
            BBB(2) = XX(2,d4(K))+COEF*(XX(2,d2(K))-XX(2,d4(K)))
            BBB(3) = XX(3,d4(K))+COEF*(XX(3,d2(K))-XX(3,d4(K)))
            CCC(1) = XX(1,d4(K))+COEF*(XX(1,d3(K))-XX(1,d4(K)))
            CCC(2) = XX(2,d4(K))+COEF*(XX(2,d3(K))-XX(2,d4(K)))
            CCC(3) = XX(3,d4(K))+COEF*(XX(3,d3(K))-XX(3,d4(K)))
            XK0(NPOINT) = FOURTH*(AAA(1)+BBB(1)+CCC(1)+XX(1,d4(K)))
            YK0(NPOINT) = FOURTH*(AAA(2)+BBB(2)+CCC(2)+XX(2,d4(K)))
            ZK0(NPOINT) = FOURTH*(AAA(3)+BBB(3)+CCC(3)+XX(3,d4(K)))
            !  LEVEL - 2 -> 4 SAMPLE POINTS ( 4 triangles on level 2 )
            !  COEF = 3.0/8.0
            COEF = THREE*ONE_OVER_8
            AAA(1) = XX(1,d4(K))+COEF*(XX(1,d1(K))-XX(1,d4(K)))
            AAA(2) = XX(2,d4(K))+COEF*(XX(2,d1(K))-XX(2,d4(K)))
            AAA(3) = XX(3,d4(K))+COEF*(XX(3,d1(K))-XX(3,d4(K)))
            BBB(1) = XX(1,d4(K))+COEF*(XX(1,d2(K))-XX(1,d4(K)))
            BBB(2) = XX(2,d4(K))+COEF*(XX(2,d2(K))-XX(2,d4(K)))
            BBB(3) = XX(3,d4(K))+COEF*(XX(3,d2(K))-XX(3,d4(K)))
            CCC(1) = XX(1,d4(K))+COEF*(XX(1,d3(K))-XX(1,d4(K)))
            CCC(2) = XX(2,d4(K))+COEF*(XX(2,d3(K))-XX(2,d4(K)))
            CCC(3) = XX(3,d4(K))+COEF*(XX(3,d3(K))-XX(3,d4(K)))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(AAA(1)+BBB(1)+CCC(1))
            YK0(NPOINT) = THIRD*(AAA(2)+BBB(2)+CCC(2))
            ZK0(NPOINT) = THIRD*(AAA(3)+BBB(3)+CCC(3))
            L12(1,1) = HALF*(AAA(1)+BBB(1))
            L12(2,1) = HALF*(AAA(2)+BBB(2))
            L12(3,1) = HALF*(AAA(3)+BBB(3))
            L23(1,1) = HALF*(BBB(1)+CCC(1))
            L23(2,1) = HALF*(BBB(2)+CCC(2))
            L23(3,1) = HALF*(BBB(3)+CCC(3))
            L31(1,1) = HALF*(CCC(1)+AAA(1))
            L31(2,1) = HALF*(CCC(2)+AAA(2))
            L31(3,1) = HALF*(CCC(3)+AAA(3))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(AAA(1)+L12(1,1)+L31(1,1))
            YK0(NPOINT) = THIRD*(AAA(2)+L12(2,1)+L31(2,1))
            ZK0(NPOINT) = THIRD*(AAA(3)+L12(3,1)+L31(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(BBB(1)+L12(1,1)+L23(1,1))
            YK0(NPOINT) = THIRD*(BBB(2)+L12(2,1)+L23(2,1))
            ZK0(NPOINT) = THIRD*(BBB(3)+L12(3,1)+L23(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CCC(1)+L23(1,1)+L31(1,1))
            YK0(NPOINT) = THIRD*(CCC(2)+L23(2,1)+L31(2,1))
            ZK0(NPOINT) = THIRD*(CCC(3)+L23(3,1)+L31(3,1))
            !  LEVEL - 3 -> 9 SAMPLE POINTS ( 9 triangles on level 3 )
            !            COEF = 5.0/8.0
            COEF = FIVE*ONE_OVER_8
            AAA(1) = XX(1,d4(K))+COEF*(XX(1,d1(K))-XX(1,d4(K)))
            AAA(2) = XX(2,d4(K))+COEF*(XX(2,d1(K))-XX(2,d4(K)))
            AAA(3) = XX(3,d4(K))+COEF*(XX(3,d1(K))-XX(3,d4(K)))
            BBB(1) = XX(1,d4(K))+COEF*(XX(1,d2(K))-XX(1,d4(K)))
            BBB(2) = XX(2,d4(K))+COEF*(XX(2,d2(K))-XX(2,d4(K)))
            BBB(3) = XX(3,d4(K))+COEF*(XX(3,d2(K))-XX(3,d4(K)))
            CCC(1) = XX(1,d4(K))+COEF*(XX(1,d3(K))-XX(1,d4(K)))
            CCC(2) = XX(2,d4(K))+COEF*(XX(2,d3(K))-XX(2,d4(K)))
            CCC(3) = XX(3,d4(K))+COEF*(XX(3,d3(K))-XX(3,d4(K)))
            CG(1) = THIRD*(AAA(1)+BBB(1)+CCC(1))
            CG(2) = THIRD*(AAA(2)+BBB(2)+CCC(2))
            CG(3) = THIRD*(AAA(3)+BBB(3)+CCC(3))
            L12(1,1) = THIRD*(TWO*AAA(1)+BBB(1))
            L12(2,1) = THIRD*(TWO*AAA(2)+BBB(2))
            L12(3,1) = THIRD*(TWO*AAA(3)+BBB(3))
            L12(1,2) = THIRD*(AAA(1)+TWO*BBB(1))
            L12(2,2) = THIRD*(AAA(2)+TWO*BBB(2))
            L12(3,2) = THIRD*(AAA(3)+TWO*BBB(3))
            L23(1,1) = THIRD*(TWO*BBB(1)+CCC(1))
            L23(2,1) = THIRD*(TWO*BBB(2)+CCC(2))
            L23(3,1) = THIRD*(TWO*BBB(3)+CCC(3))
            L23(1,2) = THIRD*(BBB(1)+TWO*CCC(1))
            L23(2,2) = THIRD*(BBB(2)+TWO*CCC(2))
            L23(3,2) = THIRD*(BBB(3)+TWO*CCC(3))
            L31(1,1) = THIRD*(TWO*CCC(1)+AAA(1))
            L31(2,1) = THIRD*(TWO*CCC(2)+AAA(2))
            L31(3,1) = THIRD*(TWO*CCC(3)+AAA(3))
            L31(1,2) = THIRD*(CCC(1)+TWO*AAA(1))
            L31(2,2) = THIRD*(CCC(2)+TWO*AAA(2))
            L31(3,2) = THIRD*(CCC(3)+TWO*AAA(3))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(AAA(1)+L12(1,1)+L31(1,2))
            YK0(NPOINT) = THIRD*(AAA(2)+L12(2,1)+L31(2,2))
            ZK0(NPOINT) = THIRD*(AAA(3)+L12(3,1)+L31(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(BBB(1)+L12(1,2)+L23(1,1))
            YK0(NPOINT) = THIRD*(BBB(2)+L12(2,2)+L23(2,1))
            ZK0(NPOINT) = THIRD*(BBB(3)+L12(3,2)+L23(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CCC(1)+L23(1,2)+L31(1,1))
            YK0(NPOINT) = THIRD*(CCC(2)+L23(2,2)+L31(2,1))
            ZK0(NPOINT) = THIRD*(CCC(3)+L23(3,2)+L31(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CG(1)+L12(1,1)+L31(1,2))
            YK0(NPOINT) = THIRD*(CG(2)+L12(2,1)+L31(2,2))
            ZK0(NPOINT) = THIRD*(CG(3)+L12(3,1)+L31(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CG(1)+L12(1,2)+L23(1,1))
            YK0(NPOINT) = THIRD*(CG(2)+L12(2,2)+L23(2,1))
            ZK0(NPOINT) = THIRD*(CG(3)+L12(3,2)+L23(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CG(1)+L23(1,2)+L31(1,1))
            YK0(NPOINT) = THIRD*(CG(2)+L23(2,2)+L31(2,1))
            ZK0(NPOINT) = THIRD*(CG(3)+L23(3,2)+L31(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CG(1)+L12(1,1)+L12(1,2))
            YK0(NPOINT) = THIRD*(CG(2)+L12(2,1)+L12(2,2))
            ZK0(NPOINT) = THIRD*(CG(3)+L12(3,1)+L12(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CG(1)+L23(1,1)+L23(1,2))
            YK0(NPOINT) = THIRD*(CG(2)+L23(2,1)+L23(2,2))
            ZK0(NPOINT) = THIRD*(CG(3)+L23(3,1)+L23(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CG(1)+L31(1,1)+L31(1,2))
            YK0(NPOINT) = THIRD*(CG(2)+L31(2,1)+L31(2,2))
            ZK0(NPOINT) = THIRD*(CG(3)+L31(3,1)+L31(3,2))
            ! LEVEL - 4 -> 16 SAMPLE POINTS ( 16 triangles on level 4 )
            ! COEF = 7.0/8.0
            COEF = SEVEN*ONE_OVER_8
            AAA(1) = XX(1,d4(K))+COEF*(XX(1,d1(K))-XX(1,d4(K)))
            AAA(2) = XX(2,d4(K))+COEF*(XX(2,d1(K))-XX(2,d4(K)))
            AAA(3) = XX(3,d4(K))+COEF*(XX(3,d1(K))-XX(3,d4(K)))
            BBB(1) = XX(1,d4(K))+COEF*(XX(1,d2(K))-XX(1,d4(K)))
            BBB(2) = XX(2,d4(K))+COEF*(XX(2,d2(K))-XX(2,d4(K)))
            BBB(3) = XX(3,d4(K))+COEF*(XX(3,d2(K))-XX(3,d4(K)))
            CCC(1) = XX(1,d4(K))+COEF*(XX(1,d3(K))-XX(1,d4(K)))
            CCC(2) = XX(2,d4(K))+COEF*(XX(2,d3(K))-XX(2,d4(K)))
            CCC(3) = XX(3,d4(K))+COEF*(XX(3,d3(K))-XX(3,d4(K)))
            CG(1) = THIRD*(AAA(1)+BBB(1)+CCC(1))
            CG(2) = THIRD*(AAA(2)+BBB(2)+CCC(2))
            CG(3) = THIRD*(AAA(3)+BBB(3)+CCC(3))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = CG(1)
            YK0(NPOINT) = CG(2)
            ZK0(NPOINT) = CG(3)

            L12(1,1) = FOURTH*(THREE*AAA(1)+BBB(1))
            L12(2,1) = FOURTH*(THREE*AAA(2)+BBB(2))
            L12(3,1) = FOURTH*(THREE*AAA(3)+BBB(3))
            L12(1,2) = HALF*(AAA(1)+BBB(1))
            L12(2,2) = HALF*(AAA(2)+BBB(2))
            L12(3,2) = HALF*(AAA(3)+BBB(3))
            L12(1,3) = FOURTH*(AAA(1)+THREE*BBB(1))
            L12(2,3) = FOURTH*(AAA(2)+THREE*BBB(2))
            L12(3,3) = FOURTH*(AAA(3)+THREE*BBB(3))
            L23(1,1) = FOURTH*(THREE*BBB(1)+CCC(1))
            L23(2,1) = FOURTH*(THREE*BBB(2)+CCC(2))
            L23(3,1) = FOURTH*(THREE*BBB(3)+CCC(3))
            L23(1,2) = HALF*(BBB(1)+CCC(1))
            L23(2,2) = HALF*(BBB(2)+CCC(2))
            L23(3,2) = HALF*(BBB(3)+CCC(3))
            L23(1,3) = FOURTH*(BBB(1)+THREE*CCC(1))
            L23(2,3) = FOURTH*(BBB(2)+THREE*CCC(2))
            L23(3,3) = FOURTH*(BBB(3)+THREE*CCC(3))
            L31(1,1) = FOURTH*(THREE*CCC(1)+AAA(1))
            L31(2,1) = FOURTH*(THREE*CCC(2)+AAA(2))
            L31(3,1) = FOURTH*(THREE*CCC(3)+AAA(3))
            L31(1,2) = HALF*(CCC(1)+AAA(1))
            L31(2,2) = HALF*(CCC(2)+AAA(2))
            L31(3,2) = HALF*(CCC(3)+AAA(3))
            L31(1,3) = FOURTH*(CCC(1)+THREE*AAA(1))
            L31(2,3) = FOURTH*(CCC(2)+THREE*AAA(2))
            L31(3,3) = FOURTH*(CCC(3)+THREE*AAA(3))
            LL(1,1) = HALF*(L12(1,2)+L31(1,2))
            LL(2,1) = HALF*(L12(2,2)+L31(2,2))
            LL(3,1) = HALF*(L12(3,2)+L31(3,2))
            LL(1,2) = HALF*(L12(1,2)+L23(1,2))
            LL(2,2) = HALF*(L12(2,2)+L23(2,2))
            LL(3,2) = HALF*(L12(3,2)+L23(3,2))
            LL(1,3) = HALF*(L23(1,2)+L12(1,2))
            LL(2,3) = HALF*(L23(2,2)+L12(2,2))
            LL(3,3) = HALF*(L23(3,2)+L12(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(AAA(1)+L12(1,1)+L31(1,3))
            YK0(NPOINT) = THIRD*(AAA(2)+L12(2,1)+L31(2,3))
            ZK0(NPOINT) = THIRD*(AAA(3)+L12(3,1)+L31(3,3))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(BBB(1)+L12(1,3)+L23(1,1))
            YK0(NPOINT) = THIRD*(BBB(2)+L12(2,3)+L23(2,1))
            ZK0(NPOINT) = THIRD*(BBB(3)+L12(3,3)+L23(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(CCC(1)+L23(1,3)+L31(1,1))
            YK0(NPOINT) = THIRD*(CCC(2)+L23(2,3)+L31(2,1))
            ZK0(NPOINT) = THIRD*(CCC(3)+L23(3,3)+L31(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,1)+L12(1,1)+L31(1,3))
            YK0(NPOINT) = THIRD*(LL(2,1)+L12(2,1)+L31(2,3))
            ZK0(NPOINT) = THIRD*(LL(3,1)+L12(3,1)+L31(3,3))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,2)+L12(1,3)+L23(1,1))
            YK0(NPOINT) = THIRD*(LL(2,2)+L12(2,3)+L23(2,1))
            ZK0(NPOINT) = THIRD*(LL(3,2)+L12(3,3)+L23(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,3)+L23(1,3)+L31(1,1))
            YK0(NPOINT) = THIRD*(LL(2,3)+L23(2,3)+L31(2,1))
            ZK0(NPOINT) = THIRD*(LL(3,3)+L23(3,3)+L31(3,1))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,1)+L12(1,1)+L12(1,2))
            YK0(NPOINT) = THIRD*(LL(2,1)+L12(2,1)+L12(2,2))
            ZK0(NPOINT) = THIRD*(LL(3,1)+L12(3,1)+L12(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,2)+L12(1,2)+L12(1,3))
            YK0(NPOINT) = THIRD*(LL(2,2)+L12(2,2)+L12(2,3))
            ZK0(NPOINT) = THIRD*(LL(3,2)+L12(3,2)+L12(3,3))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,2)+L23(1,1)+L23(1,2))
            YK0(NPOINT) = THIRD*(LL(2,2)+L23(2,1)+L23(2,2))
            ZK0(NPOINT) = THIRD*(LL(3,2)+L23(3,1)+L23(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,3)+L23(1,2)+L23(1,3))
            YK0(NPOINT) = THIRD*(LL(2,3)+L23(2,2)+L23(2,3))
            ZK0(NPOINT) = THIRD*(LL(3,3)+L23(3,2)+L23(3,3))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,3)+L31(1,1)+L31(1,2))
            YK0(NPOINT) = THIRD*(LL(2,3)+L31(2,1)+L31(2,2))
            ZK0(NPOINT) = THIRD*(LL(3,3)+L31(3,1)+L31(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,1)+L31(1,2)+L31(1,3))
            YK0(NPOINT) = THIRD*(LL(2,1)+L31(2,2)+L31(2,3))
            ZK0(NPOINT) = THIRD*(LL(3,1)+L31(3,2)+L31(3,3))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,1)+LL(1,2)+L12(1,2))
            YK0(NPOINT) = THIRD*(LL(2,1)+LL(2,2)+L12(2,2))
            ZK0(NPOINT) = THIRD*(LL(3,1)+LL(3,2)+L12(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,2)+LL(1,3)+L23(1,2))
            YK0(NPOINT) = THIRD*(LL(2,2)+LL(2,3)+L23(2,2))
            ZK0(NPOINT) = THIRD*(LL(3,2)+LL(3,3)+L23(3,2))
            NPOINT = NPOINT + 1
            XK0(NPOINT) = THIRD*(LL(1,3)+LL(1,1)+L31(1,2))
            YK0(NPOINT) = THIRD*(LL(2,3)+LL(2,1)+L31(2,2))
            ZK0(NPOINT) = THIRD*(LL(3,3)+LL(3,1)+L31(3,2))
          ENDDO ! DO K=1,4

          DO J=1,NPOINT
            XS(J,I)=XK0(J)
            YS(J,I)=YK0(J)
            ZS(J,I)=ZK0(J)
          ENDDO
        ENDDO ! DO I=1,NEL

      ! NTRACE_TOT = NPOINT
      ELSEIF (ISOLNOD == 8) THEN

        DO I=1,NEL
          XX(1,1)=X1(I)
          XX(2,1)=Y1(I)
          XX(3,1)=Z1(I)
          XX(1,2)=X2(I)
          XX(2,2)=Y2(I)
          XX(3,2)=Z2(I)
          XX(1,3)=X3(I)
          XX(2,3)=Y3(I)
          XX(3,3)=Z3(I)
          XX(1,4)=X4(I)
          XX(2,4)=Y4(I)
          XX(3,4)=Z4(I)
          XX(1,5)=X5(I)
          XX(2,5)=Y5(I)
          XX(3,5)=Z5(I)
          XX(1,6)=X6(I)
          XX(2,6)=Y6(I)
          XX(3,6)=Z6(I)
          XX(1,7)=X7(I)
          XX(2,7)=Y7(I)
          XX(3,7)=Z7(I)
          XX(1,8)=X8(I)
          XX(2,8)=Y8(I)
          XX(3,8)=Z8(I)

          IF (IPART_(I) /= IDP) CYCLE

          XMIN = EP20
          YMIN = EP20
          ZMIN = EP20
          XMAX =-EP20
          YMAX =-EP20
          ZMAX =-EP20

          DO J=1,8
            XMIN=MIN(XMIN,XX(1,J))
            YMIN=MIN(YMIN,XX(2,J))
            ZMIN=MIN(ZMIN,XX(3,J))
            XMAX=MAX(XMAX,XX(1,J))
            YMAX=MAX(YMAX,XX(2,J))
            ZMAX=MAX(ZMAX,XX(3,J))
          END DO

          DX = (XMAX-XMIN)/FLOAT(NTRACE0)
          DY = (YMAX-YMIN)/FLOAT(NTRACE0)
          DZ = (ZMAX-ZMIN)/FLOAT(NTRACE0)

          N1 = NTRACE0
          N2 = NTRACE0
          N3 = NTRACE0

          XK(1) = XMIN + DX*HALF
          YK(1) = YMIN + DY*HALF
          ZK(1) = ZMIN + DZ*HALF

          DO K1=2,N1
            XK(K1) = XK(K1-1) + DX
            YK(K1) = YK(K1-1) + DY
            ZK(K1) = ZK(K1-1) + DZ
          ENDDO

          J=0
          DO K3=1,N3
            DO K2=1,N2
              DO K1=1,N1
                J=J+1
                XS(J,I)=XK(K1)
                YS(J,I)=YK(K2)
                ZS(J,I)=ZK(K3)
             ENDDO ! DO K1=1,N1
            ENDDO ! DO K2=1,N2
          ENDDO ! DO K3=1,N3
        ENDDO ! DO I=1,NEL
      ENDIF  !  IF (ISOLNOD == 4)

      IF (ISOLNOD == 4 .OR. N2D/=0) THEN
        NTRACE_TOT = NPOINT
      ELSEIF (ISOLNOD == 8) THEN
        NTRACE_TOT = NTRACE
      ENDIF

      DO II=1,GETEL
        I=JCT(II)
        IF (IPART_(I) /= IDP) CYCLE

        DO IP=1,NTRACE_TOT
          INOD = 0
          DIST = ZERO
          DIST_OLD = EP20

          IF(SURF_TYPE == 101)THEN
           !--ellipsoid
            IAD0 = IAD_BUFR
            DIST = ZERO
            !get this out from the loop  ! does not depends on IP
            AA = BUFSF(IAD0+1)
            BB = BUFSF(IAD0+2)
            CC = BUFSF(IAD0+3)
            XG = BUFSF(IAD0+4)
            YG = BUFSF(IAD0+5)
            ZG = BUFSF(IAD0+6)
            SKW(1)=BUFSF(IAD0+7)
            SKW(2)=BUFSF(IAD0+8)
            SKW(3)=BUFSF(IAD0+9)
            SKW(4)=BUFSF(IAD0+10)
            SKW(5)=BUFSF(IAD0+11)
            SKW(6)=BUFSF(IAD0+12)
            SKW(7)=BUFSF(IAD0+13)
            SKW(8)=BUFSF(IAD0+14)
            SKW(9)=BUFSF(IAD0+15)
            DGR=BUFSF(IAD0+36)
            ! transition matrix
            X_PRIME = SKW(1)*(XS(IP,I)-XG) + SKW(4)*(YS(IP,I)-YG) + SKW(7)*(ZS(IP,I)-ZG)
            Y_PRIME = SKW(2)*(XS(IP,I)-XG) + SKW(5)*(YS(IP,I)-YG) + SKW(8)*(ZS(IP,I)-ZG)
            Z_PRIME = SKW(3)*(XS(IP,I)-XG) + SKW(6)*(YS(IP,I)-YG) + SKW(9)*(ZS(IP,I)-ZG)
            TMP(1)= ABS(X_PRIME)/AA
            TMP(2)= ABS(Y_PRIME)/BB
            TMP(3)= ABS(Z_PRIME)/CC
            IF(TMP(1)/=ZERO)TMP(1)= EXP(DGR*LOG(TMP(1)))
            IF(TMP(2)/=ZERO)TMP(2)= EXP(DGR*LOG(TMP(2)))
            IF(TMP(3)/=ZERO)TMP(3)= EXP(DGR*LOG(TMP(3)))
            DIST = (TMP(1)+TMP(2)+TMP(3))
            DISP(IP,I) = ONE-DIST

          ELSEIF (SURF_TYPE == 200) THEN
           !--planar surface
            !get this out from the loop ! does not depends on IP
            IAD0 = IAD_BUFR
            DIST = ZERO

            XP1 = BUFSF(IAD0+1)
            YP1 = BUFSF(IAD0+2)
            ZP1 = BUFSF(IAD0+3)
            XP2 = BUFSF(IAD0+4)
            YP2 = BUFSF(IAD0+5)
            ZP2 = BUFSF(IAD0+6)

            AA = XP2 - XP1
            BB = YP2 - YP1
            CC = ZP2 - ZP1

            DIST = AA*(XS(IP,I)-XP1)+BB*(YS(IP,I)-YP1)+CC*(ZS(IP,I)-ZP1)
            tmpSUM = SQRT(AA*AA+BB*BB+CC*CC)
            tmpSUM = ONE/MAX(EM30,tmpSUM)
            DIST = DIST*tmpSUM
            DISP(IP,I) = DIST

        ELSE
           !--surface of eleme,ts
            IF(N2D == 0)THEN
              IF (ISOLNOD == 4) THEN
                IX(1) =IXS(2,I)
                IX(2) =IXS(4,I)
                IX(3) =IXS(7,I)
                IX(4) =IXS(6,I)
                ELEM_NUMNOD = 4
              ELSEIF (ISOLNOD == 8) THEN
                IX(1) =IXS(2,I)
                IX(2) =IXS(3,I)
                IX(3) =IXS(4,I)
                IX(4) =IXS(5,I)
                IX(5) =IXS(6,I)
                IX(6) =IXS(7,I)
                IX(7) =IXS(8,I)
                IX(8) =IXS(9,I)
                ELEM_NUMNOD = 8
              ENDIF
            ELSE
              IF(ITYP == 7)THEN
                IX(1) =IXTG(2,I)
                IX(2) =IXTG(3,I)
                IX(3) =IXTG(4,I)
                IX(4) =0
                ELEM_NUMNOD = 3
              ELSEIF(ITYP == 2)THEN
                IX(1) =IXQ(2,I)
                IX(2) =IXQ(3,I)
                IX(3) =IXQ(4,I)
                IX(4) =IXQ(5,I)
                ELEM_NUMNOD = 4
              ENDIF
            ENDIF

            DO J=1,ELEM_NUMNOD
              N = IX(J)
!             NSOLTOSF(IDC,N) - the shell node of the container IDC, to which
!                               the distance of the eulerian node N to the shell
!                               container is minimum
              NSH = NSOLTOSF(IDC,N)
              IF (NSH <= 0) CYCLE
!             KNOD2SURF(NSH) - the nb of surfaces of the current container IDC,
!                              connected to node NSH
              DO JJ=1,KNOD2SURF(NSH)
!               INOD2SURF(JJ,NSH) - identify the segment of the current surface
!                                   container
                IPL = INOD2SURF(JJ,NSH)
!               SEGTOSURF(IPL) - identify the right surface to whom the segment
!               IPL belongs. One node NSH can connect segments coming from
!               different surface containers.
!               Do not forget that containers overwrite phases (superpose)
                ISURF = SEGTOSURF(IPL)
                IPL = IPL - SWIFTSURF(ISURF)
                IF (IPL <= 0 .OR. IPL > NSEG) CYCLE
!
                IXPL(1) = IGRSURF(ISURF)%NODES(IPL,1)
                IXPL(2) = IGRSURF(ISURF)%NODES(IPL,2)
                IXPL(3) = IGRSURF(ISURF)%NODES(IPL,3)
                IXPL(4) = IGRSURF(ISURF)%NODES(IPL,4)
!
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

                DO K=1,4
                  ! compute min dist from trace sample points to cutting container:
                  X0 = XS(IP,I) - XFAS(1,K)
                  Y0 = YS(IP,I) - XFAS(2,K)
                  Z0 = ZS(IP,I) - XFAS(3,K)
                  DIST = X0*X0 + Y0*Y0 + Z0*Z0
                  DIST = SQRT(DIST)
                  IF (DIST < DIST_OLD .and. DIST > EM10) THEN
                    DIST_OLD = DIST
                    INOD = IXPL(K)
                  ENDIF
                ENDDO ! DO K=1,4
              ENDDO ! DO II=1,KNOD2SURF(NSH)
            ENDDO ! DO J=1,8

            IF (INOD == 0) GOTO 122

            IF (DIST_OLD == EP20) DIST_OLD = ZERO
            DISP(IP,I)  = DIST_OLD

            ! get sig  n of dist of trace sample points
            XN(1) = XS(IP,I)
            XN(2) = YS(IP,I)
            XN(3) = ZS(IP,I)
            DIST = ZERO
            CALL IN_OUT_SIDE(
     .         INOD      ,INOD2SURF ,KNOD2SURF ,NNOD2SURF ,X         ,
     .         XN        ,DIST      ,NSEG      ,SURF_ELTYP,NOD_NORMAL,
     .         SURF_NODES,SWIFTSURF ,IDSURF    ,SEGTOSURF )
            DISP(IP,I) = DIST

        ENDIF

        ! START counting trace samples and filling process
        JMID_OLD = INPHASE(IP,I)
        IF (DISP(IP,I) > ZERO) THEN
          TRACEP(I) = TRACEP(I) + 1
          IF (IFILL == 0) THEN
            IF (JMID_OLD /= JMID) THEN
              INPHASE(IP,I) = JMID ! get new phase
              NBIP(JMID_OLD,I) = NBIP(JMID_OLD,I) - 1
              NBIP(JMID,I) = NBIP(JMID,I) + 1
            ENDIF
          END IF ! IF (IFILL == 0)
        ELSEIF (DISP(IP,I) < ZERO) THEN
          TRACEN(I) = TRACEN(I) + 1
          IF (IFILL == 1) THEN
            IF (JMID_OLD /= JMID) THEN
              INPHASE(IP,I) = JMID ! get new phase
              NBIP(JMID_OLD,I) = NBIP(JMID_OLD,I) - 1
              NBIP(JMID,I) = NBIP(JMID,I) + 1
            ENDIF
          ENDIF ! IF (IFILL == 1)
        ELSEIF (DISP(IP,I) == ZERO .and. JMID /= JMID_OLD) THEN
          ! eliminate phase within trace sample points on container surface
          NBIP(JMID_OLD,I) = NBIP(JMID_OLD,I) - 1
          INPHASE(IP,I) = 0
        ENDIF ! IF (DISP(IP,I) > ZERO)

 122    CONTINUE

        ENDDO  ! IP=1,NTRACE_TOT
        ! check / remove old non-existing phase within element :
        IF (JMID_OLD > 0)THEN
          IF(NBIP(JMID_OLD,I) < 0)  NBIP(JMID_OLD,I) = 0
        ENDIF

        OK = 0
        NPHASE = IPHASE(NBSUBMAT+1,I)
        K = NPHASE
        DO J=1,NPHASE
          IF (JMID /= IPHASE(J,I)) OK = OK + 1
        ENDDO
        IF (OK == K) THEN
          K = K + 1
          IPHASE(K,I) = JMID
          IPHASE(NBSUBMAT+1,I) = K  ! increasing the nb of phases within cut element
        ENDIF

        IF (TRACEP(I) <= 0 .and. TRACEN(I) > 0) THEN
          FULL(I) = -1
        ELSEIF (TRACEP(I) > 0 .and. TRACEN(I) <= 0) THEN
          FULL(I) = 1
        ENDIF

      ENDDO ! DO II=1,GETEL

      ! recompute / reset / overwrite phases inside solid element
      DO I=1,NEL
        IF (IPART_(I) /= IDP) CYCLE
        IF (FULL(I) == 1 .and. IFILL == 0) THEN
          KVOL_BAK(1:NBSUBMAT) = KVOL(1:NBSUBMAT,I)
          DO J=1,NBSUBMAT
            IPHASE(J,I) = 0
            IF(ICUMU == 0)KVOL(J,I) = ZERO
            NBIP(J,I) = 0
          ENDDO
          IPHASE(1,I) = JMID
          IPHASE(NBSUBMAT+1,I) = 1
          NBIP(JMID,I) = NTRACE_TOT
          DO IP=1,NTRACE_TOT
            INPHASE(IP,I) = JMID
          ENDDO
          IF(ICUMU == 0)KVOL(JMID,I)=ZERO
          KVOL(JMID,I)= KVOL(JMID,I)+FILL_RATIO
          ! if added volume ratio makes that sum is > 1, then substract from previous filling
          IF(ICUMU == -1)THEN
            ! if added volume ratio makes that sum is > 1, then substract from previous filling
            SUMVF = SUM(KVOL(1:NBSUBMAT, I))
            IF (SUMVF > ONE)THEN
              IF(IDC > 1)THEN
               ! SUBSTRACT FROM PREVIOUS STEP
               ISUBMAT_TO_SUBSTRACT = INIVOL(I_INIVOL)%CONTAINER(IDC-1)%SUBMAT_ID
               VF_TO_SUBSTRACT = SUMVF-ONE
               VF_TO_SUBSTRACT = MIN(VF_TO_SUBSTRACT, KVOL(ISUBMAT_TO_SUBSTRACT,I))
               KVOL(ISUBMAT_TO_SUBSTRACT,I) = KVOL(ISUBMAT_TO_SUBSTRACT,I) - VF_TO_SUBSTRACT
              ELSE
               ! SUBSTRACT FROM DEFAULT SUBMATERIAL
               ISUBMAT_TO_SUBSTRACT = MAXLOC(KVOL_BAK,1)
               VF_TO_SUBSTRACT = SUMVF-ONE
               VF_TO_SUBSTRACT = MIN(VF_TO_SUBSTRACT, KVOL(ISUBMAT_TO_SUBSTRACT,I))
               KVOL(ISUBMAT_TO_SUBSTRACT,I) = KVOL(ISUBMAT_TO_SUBSTRACT,I) - VF_TO_SUBSTRACT
              ENDIF
            END IF
          ENDIF
        ELSEIF (FULL(I) == -1 .and. IFILL == 1) THEN
          KVOL_BAK(1:NBSUBMAT) = KVOL(1:NBSUBMAT,I)
          DO J=1,NBSUBMAT
            IPHASE(J,I) = 0
            IF(ICUMU == 0)KVOL(J,I) = ZERO
            NBIP(J,I) = 0
          ENDDO
          IPHASE(1,I) = JMID
          IPHASE(NBSUBMAT+1,I) = 1
          NBIP(JMID,I) = NTRACE_TOT
          DO IP=1,NTRACE_TOT
            INPHASE(IP,I) = JMID
          ENDDO
          IF(ICUMU == 0)KVOL(JMID,I)=ZERO
          KVOL(JMID,I)= KVOL(JMID,I)+FILL_RATIO
          IF(ICUMU == -1)THEN
            ! if added volume ratio makes that sum is > 1, then substract from previous filling
            SUMVF = SUM(KVOL(1:NBSUBMAT, I))
            IF (SUMVF > ONE)THEN
              IF(IDC > 1)THEN
               ! SUBSTRACT FROM PREVIOUS STEP
               ISUBMAT_TO_SUBSTRACT = INIVOL(I_INIVOL)%CONTAINER(IDC-1)%SUBMAT_ID
               VF_TO_SUBSTRACT = SUMVF-ONE
               VF_TO_SUBSTRACT = MIN(VF_TO_SUBSTRACT, KVOL(ISUBMAT_TO_SUBSTRACT,I))
               KVOL(ISUBMAT_TO_SUBSTRACT,I) = KVOL(ISUBMAT_TO_SUBSTRACT,I) - VF_TO_SUBSTRACT
              ELSE
               ! SUBSTRACT FROM DEFAULT SUBMATERIAL
               ISUBMAT_TO_SUBSTRACT = MAXLOC(KVOL_BAK,1)
               VF_TO_SUBSTRACT = SUMVF-ONE
               VF_TO_SUBSTRACT = MIN(VF_TO_SUBSTRACT, KVOL(ISUBMAT_TO_SUBSTRACT,I))
               KVOL(ISUBMAT_TO_SUBSTRACT,I) = KVOL(ISUBMAT_TO_SUBSTRACT,I) - VF_TO_SUBSTRACT
              ENDIF
            END IF
          ENDIF
        ELSEIF (FULL(I) == 2) THEN
          DO J=1,NBSUBMAT
            BUFFILL1(J)  = 0
            BUFFILL2(J,I)= 0
          ENDDO

          DO J=1,IPHASE(NBSUBMAT+1,I)
            IPH = IPHASE(J,I)
            IF (IPH /= 0) THEN
              IF (NBIP(IPH,I) == 0) IPHASE(J,I) = 0
            ENDIF
          ENDDO

          K = 0
          OK = 0
          DO J=1,IPHASE(NBSUBMAT+1,I)
            IF (IPHASE(J,I) /= 0) THEN
              IPH = IPHASE(J,I)
              K = K + 1
              BUFFILL1(K)  = IPHASE(J,I)
              BUFFILL2(K,I)= NBIP(IPH,I)
            ENDIF
          ENDDO

          IF (IPHASE(NBSUBMAT+1,I) > 1) THEN
            DO J=1,NBSUBMAT
              IPHASE(J,I) = 0
              NBIP(J,I) = 0
            ENDDO

            DO J=1,K
              IPHASE(J,I) = BUFFILL1(J)
              IPH = IPHASE(J,I)
              NBIP(IPH,I)   = BUFFILL2(J,I)
            ENDDO
            IPHASE(NBSUBMAT+1,I) = K
          ENDIF
        ENDIF ! IF (FULL(I) == 1 .and. IFILL == 0)


      ENDDO ! DO I=1,NEL

      ! COMPUTE VOLUME FRACTION
      DO I=1,NEL
        IF (IPART_(I) /= IDP) CYCLE
        NIP = 0
        IF (IPHASE(NBSUBMAT+1,I) > 1) THEN
          DO J=1,IPHASE(NBSUBMAT+1,I)
            IPH = IPHASE(J,I)
            NIP  = NIP + NBIP(IPH,I)
          ENDDO
          KVOL_BAK(1:NBSUBMAT) = KVOL(1:NBSUBMAT,I)
          DO J=1,IPHASE(NBSUBMAT+1,I)
            IPH  = IPHASE(J,I)
            IF(ICUMU == 0)KVOL(IPH,I)=ZERO
            KVOL(IPH,I)= KVOL(IPH,I)+FILL_RATIO*FLOAT(NBIP(IPH,I))/FLOAT(NIP)
            IF(ICUMU == -1)THEN
              ! if added volume ratio makes that sum is > 1, then substract from previous filling
              SUMVF = SUM(KVOL(1:NBSUBMAT, I))
              IF (SUMVF > ONE )THEN
                IF(IDC > 1)THEN
                  ! SUBSTRACT FROM PREVIOUS STEP
                  ISUBMAT_TO_SUBSTRACT = INIVOL(I_INIVOL)%CONTAINER(IDC-1)%SUBMAT_ID
                  VF_TO_SUBSTRACT = SUMVF-ONE
                  VF_TO_SUBSTRACT = MIN(VF_TO_SUBSTRACT, KVOL(ISUBMAT_TO_SUBSTRACT,I))
                  KVOL(ISUBMAT_TO_SUBSTRACT,I) = KVOL(ISUBMAT_TO_SUBSTRACT,I) - VF_TO_SUBSTRACT
                ELSE
                  ! SUBSTRACT FROM PREVIOUS STEP
                  ISUBMAT_TO_SUBSTRACT = MAXLOC(KVOL_BAK,1)
                  VF_TO_SUBSTRACT = SUMVF-ONE
                  VF_TO_SUBSTRACT = MIN(VF_TO_SUBSTRACT, KVOL(ISUBMAT_TO_SUBSTRACT,I))
                  KVOL(ISUBMAT_TO_SUBSTRACT,I) = KVOL(ISUBMAT_TO_SUBSTRACT,I) - VF_TO_SUBSTRACT
                ENDIF
              END IF
            ENDIF
          ENDDO
        ELSE
        ENDIF
      ENDDO ! DO I=1,NEL
!------------------
      RETURN
      END
