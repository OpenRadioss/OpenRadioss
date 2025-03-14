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
      !||    vinter             ../engine/source/tools/curve/vinter.F
      !||--- called by ------------------------------------------------------
      !||    asso_plas76        ../engine/source/materials/mat/mat076/asso_plas76.F
      !||    asso_qplas76c      ../engine/source/materials/mat/mat076/asso_qplas76c.F
      !||    fail_fld_c         ../engine/source/materials/fail/fld/fail_fld_c.F
      !||    fail_tab_old_c     ../engine/source/materials/fail/tabulated/fail_tab_old_c.F
      !||    fixtemp            ../engine/source/constraints/thermic/fixtemp.F
      !||    mat122_newton      ../engine/source/materials/mat/mat122/mat122_newton.F
      !||    mat122_nice        ../engine/source/materials/mat/mat122/mat122_nice.F
      !||    mat122c_newton     ../engine/source/materials/mat/mat122/mat122c_newton.F
      !||    mat122c_nice       ../engine/source/materials/mat/mat122/mat122c_nice.F
      !||    no_asso_lplas76c   ../engine/source/materials/mat/mat076/no_asso_lplas76c.F
      !||    no_asso_plas76     ../engine/source/materials/mat/mat076/no_asso_plas76.F
      !||    no_asso_qplas76c   ../engine/source/materials/mat/mat076/no_asso_qplas76c.F
      !||    sigeps100          ../engine/source/materials/mat/mat100/sigeps100.F90
      !||    sigeps127          ../engine/source/materials/mat/mat127/sigeps127.F90
      !||    sigeps127c         ../engine/source/materials/mat/mat127/sigeps127c.F90
      !||    sigeps187          ../engine/source/materials/mat/mat187/sigeps187.F
      !||    sigeps36           ../engine/source/materials/mat/mat036/sigeps36.F
      !||    sigeps36c          ../engine/source/materials/mat/mat036/sigeps36c.F
      !||    sigeps36g          ../engine/source/materials/mat/mat036/sigeps36g.F
      !||    sigeps36pi         ../engine/source/materials/mat/mat036/sigeps36pi.F
      !||    sigeps43c          ../engine/source/materials/mat/mat043/sigeps43c.F
      !||    sigeps43g          ../engine/source/materials/mat/mat043/sigeps43g.F
      !||    sigeps44           ../engine/source/materials/mat/mat044/sigeps44.F
      !||    sigeps44c          ../engine/source/materials/mat/mat044/sigeps44c.F
      !||    sigeps44pi         ../engine/source/materials/mat/mat044/sigeps44pi.F
      !||    sigeps53           ../engine/source/materials/mat/mat053/sigeps53.F
      !||    sigeps55c          ../engine/source/materials/mat/mat055/sigeps55c.F
      !||    sigeps56           ../engine/source/materials/mat/mat056/sigeps56.F
      !||    sigeps56c          ../engine/source/materials/mat/mat056/sigeps56c.F
      !||    sigeps56g          ../engine/source/materials/mat/mat056/sigeps56g.F
      !||    sigeps60           ../engine/source/materials/mat/mat060/sigeps60.F
      !||    sigeps60c          ../engine/source/materials/mat/mat060/sigeps60c.F
      !||    sigeps60g          ../engine/source/materials/mat/mat060/sigeps60g.F
      !||    sigeps64c          ../engine/source/materials/mat/mat064/sigeps64c.F
      !||    sigeps66           ../engine/source/materials/mat/mat066/sigeps66.F
      !||    sigeps66c          ../engine/source/materials/mat/mat066/sigeps66c.F
      !||    sigeps76           ../engine/source/materials/mat/mat076/sigeps76.F
      !||    sigeps76c          ../engine/source/materials/mat/mat076/sigeps76c.F
      !||    sigeps78c          ../engine/source/materials/mat/mat078/sigeps78c.F
      !||    sigeps86c          ../engine/source/materials/mat/mat086/sigeps86c.F
      !||    sigeps86g          ../engine/source/materials/mat/mat086/sigeps86g.F
      !||    sigeps93           ../engine/source/materials/mat/mat093/sigeps93.F
      !||    sigeps93c          ../engine/source/materials/mat/mat093/sigeps93c.F
      !||    sms_fixvel         ../engine/source/ams/sms_fixvel.F
      !||====================================================================
      SUBROUTINE VINTER(TF,IAD,IPOS ,ILEN,NEL,X,DYDX,Y)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C
      INTEGER :: NEL
      INTEGER :: ILEN(NEL),IPOS(NEL),IAD(NEL)
      my_real :: X(NEL),DYDX(NEL),Y(NEL)
      my_real :: TF(2,*)
C-----------------------------------------------
      INTEGER :: I,J,J1,J2,ICONT
C=======================================================================
      J = 0
 100  CONTINUE
C 
       J = J+1
       ICONT = 0
       DO I=1,NEL
         J1 = IPOS(I)+IAD(I)+1
         IF(J<=ILEN(I)-1.AND.X(I)>TF(1,J1))THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
         ENDIF
       ENDDO
C
      IF(ICONT==1)GOTO 100
C
      DO I=1,NEL
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1
        DYDX(I)=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))
        Y(I)   = TF(2,J1) + DYDX(I)*(X(I)-TF(1,J1))
      ENDDO
C
      RETURN
      END
      !||====================================================================
      !||    vinter2             ../engine/source/tools/curve/vinter.F
      !||--- called by ------------------------------------------------------
      !||    fail_orthstrain     ../engine/source/materials/fail/orthstrain/fail_orthstrain_s.F
      !||    fail_orthstrain_c   ../engine/source/materials/fail/orthstrain/fail_orthstrain_c.F
      !||    fail_tab2_b         ../engine/source/materials/fail/tabulated/fail_tab2_b.F90
      !||    fail_tab2_c         ../engine/source/materials/fail/tabulated/fail_tab2_c.F
      !||    fail_tab2_ib        ../engine/source/materials/fail/tabulated/fail_tab2_ib.F90
      !||    fail_tab2_s         ../engine/source/materials/fail/tabulated/fail_tab2_s.F
      !||    fail_tab_c          ../engine/source/materials/fail/tabulated/fail_tab_c.F
      !||    m36iter_imp         ../engine/source/materials/mat/mat036/m36iter_imp.F
      !||    mat121_newton       ../engine/source/materials/mat/mat121/mat121_newton.F
      !||    mat121_nice         ../engine/source/materials/mat/mat121/mat121_nice.F
      !||    mat121c_newton      ../engine/source/materials/mat/mat121/mat121c_newton.F
      !||    mat121c_nice        ../engine/source/materials/mat/mat121/mat121c_nice.F
      !||    redef3              ../engine/source/elements/spring/redef3.F90
      !||    redef3_law113       ../engine/source/elements/spring/redef3_law113.F
      !||    rkenonl             ../engine/source/elements/spring/r4mat3.F
      !||    sigeps121           ../engine/source/materials/mat/mat121/sigeps121.F
      !||    sigeps121c          ../engine/source/materials/mat/mat121/sigeps121c.F
      !||    sigeps158c          ../engine/source/materials/mat/mat158/sigeps158c.F
      !||    sigeps28            ../engine/source/materials/mat/mat028/sigeps28.F
      !||    sigeps36            ../engine/source/materials/mat/mat036/sigeps36.F
      !||    sigeps36c           ../engine/source/materials/mat/mat036/sigeps36c.F
      !||    sigeps56            ../engine/source/materials/mat/mat056/sigeps56.F
      !||    sigeps60            ../engine/source/materials/mat/mat060/sigeps60.F
      !||    sigeps68            ../engine/source/materials/mat/mat068/sigeps68.F
      !||    sigeps70            ../engine/source/materials/mat/mat070/sigeps70.F
      !||    sigeps81            ../engine/source/materials/mat/mat081/sigeps81.F90
      !||    sigeps90            ../engine/source/materials/mat/mat090/sigeps90.F
      !||    sigeps96            ../engine/source/materials/mat/mat096/sigeps96.F
      !||    sigeps96c           ../engine/source/materials/mat/mat096/sigeps96c.F
      !||====================================================================
      SUBROUTINE VINTER2(TF,IAD,IPOS ,ILEN,NEL0,X,DYDX,Y)
C IDEM VINTER AVEC POSSIBILITE DE FAIRE DECROITRE X
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C
      INTEGER ILEN(*),IPOS(*),IAD(*),NEL0
      my_real X(*),DYDX(*),Y(*),TF(2,*)
      INTEGER I,J1,J,ICONT,J2

      my_real YDP, TF2J2,TF2J1,TF1J2,TF1J1,
     .  XDP,YYDP, DYDXDP
      LOGICAL COND1, COND2
C
      J = 0
      ICONT = 1
      DO WHILE(ICONT==1)
C 
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         COND1 = J<=ILEN(I)-1
         IF(COND1) COND1 = X(I)>TF(1,J1)
         COND2 = IPOS(I)>=1
         IF(COND2) COND2 = X(I)<TF(1,J1-1)
         IF(COND1)THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
         ELSEIF(COND2)THEN
           IPOS(I)=IPOS(I)-1
           ICONT = 1
         ENDIF
       ENDDO
C
      ENDDO
C
      DO I=1,NEL0
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1

        TF2J2 = TF(2,J2)
        TF2J1 = TF(2,J1)
        TF1J2 = TF(1,J2)
        TF1J1 = TF(1,J1)
        XDP = X(I)

ccc        DYDX(I)=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))

        DYDXDP = (TF2J2-TF2J1)/(TF1J2-TF1J1)
        DYDX(I) = DYDXDP
ccc        Y(I)   = TF(2,J1) + DYDX(I)*(X(I)-TF(1,J1))
        YYDP = TF2J1 + DYDXDP * (XDP - TF1J1)
        Y(I) = YYDP

      ENDDO
C
      RETURN
      END

      !||====================================================================
      !||    vinter2dp       ../engine/source/tools/curve/vinter.F
      !||--- called by ------------------------------------------------------
      !||    m36iter_imp     ../engine/source/materials/mat/mat036/m36iter_imp.F
      !||    redef3          ../engine/source/elements/spring/redef3.F90
      !||    redef3_law113   ../engine/source/elements/spring/redef3_law113.F
      !||    rkenonl         ../engine/source/elements/spring/r4mat3.F
      !||    sigeps28        ../engine/source/materials/mat/mat028/sigeps28.F
      !||    sigeps68        ../engine/source/materials/mat/mat068/sigeps68.F
      !||====================================================================
      SUBROUTINE VINTER2DP(TF,IAD,IPOS ,ILEN,NEL0,X,DYDX,Y)
C IDEM VINTER AVEC POSSIBILITE DE FAIRE DECROITRE X
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C
      INTEGER ILEN(*),IPOS(*),IAD(*),NEL0
      my_real X(*),DYDX(*),Y(*),TF(2,*)
      INTEGER I,J1,J,ICONT,J2

      DOUBLE PRECISION YDP, TF2J2,TF2J1,TF1J2,TF1J1,
     .  XDP,YYDP, DYDXDP
C
      J = 0
      ICONT = 1
      DO WHILE(ICONT==1)
C 
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         IF(J<=ILEN(I)-1.AND.X(I)>TF(1,J1))THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
         ELSEIF(IPOS(I)>=1.AND.X(I)<TF(1,J1-1))THEN
           IPOS(I)=IPOS(I)-1
           ICONT = 1
         ENDIF
       ENDDO
C
      ENDDO
C
      DO I=1,NEL0
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1

        TF2J2 = TF(2,J2)
        TF2J1 = TF(2,J1)
        TF1J2 = TF(1,J2)
        TF1J1 = TF(1,J1)
        XDP = X(I)

ccc        DYDX(I)=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))

        DYDXDP = (TF2J2-TF2J1)/(TF1J2-TF1J1)
        DYDX(I) = DYDXDP
ccc        Y(I)   = TF(2,J1) + DYDX(I)*(X(I)-TF(1,J1))
        YYDP = TF2J1 + DYDXDP * (XDP - TF1J1)
        Y(I) = YYDP

      ENDDO
C
      RETURN
      END

      !||====================================================================
      !||    finter2       ../engine/source/tools/curve/vinter.F
      !||--- called by ------------------------------------------------------
      !||    fixfingeo     ../engine/source/constraints/general/impvel/fixfingeo.F
      !||    m36iter_imp   ../engine/source/materials/mat/mat036/m36iter_imp.F
      !||    sigeps52      ../engine/source/materials/mat/mat052/sigeps52.F
      !||    sigeps53      ../engine/source/materials/mat/mat053/sigeps53.F
      !||    sigeps56      ../engine/source/materials/mat/mat056/sigeps56.F
      !||    sigeps60      ../engine/source/materials/mat/mat060/sigeps60.F
      !||    sigeps65      ../engine/source/materials/mat/mat065/sigeps65.F
      !||    sigeps66      ../engine/source/materials/mat/mat066/sigeps66.F
      !||    sigeps66c     ../engine/source/materials/mat/mat066/sigeps66c.F
      !||    sigeps71      ../engine/source/materials/mat/mat071/sigeps71.F
      !||    sigeps74      ../engine/source/materials/mat/mat074/sigeps74.F
      !||====================================================================
      my_real FUNCTION FINTER2(TF,IAD,IPOS,ILEN,XX,DYDX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER J,J1,J2,ICONT,ILEN,IPOS,IAD
      my_real TF(2,*),DYDX,XX
C
      J = 0
      ICONT = 1
      DO WHILE(ICONT==1)
        J = J+1
        ICONT = 0
        J1 = IPOS+IAD+1
        IF(J<=ILEN-1.AND.XX>TF(1,J1))THEN
          IPOS=IPOS+1
          ICONT = 1
        ELSEIF(IPOS>=1.AND.XX<TF(1,J1-1))THEN
          IPOS=IPOS-1
          ICONT = 1
        ENDIF
      ENDDO
      J1   = IAD+IPOS
      J2   = J1+1
      DYDX = (TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))
      FINTER2 = TF(2,J1) + DYDX*(XX-TF(1,J1))
      RETURN
      END
