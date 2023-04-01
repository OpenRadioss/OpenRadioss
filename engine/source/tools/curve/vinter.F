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
Chd|  VINTER                        source/tools/curve/vinter.F   
Chd|-- called by -----------
Chd|        FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|        FAIL_TAB_OLD_C                source/materials/fail/tabulated/fail_tab_old_c.F
Chd|        FIXTEMP                       source/constraints/thermic/fixtemp.F
Chd|        MAT87C_TABULATED_TOTALSR      source/materials/mat/mat087/mat87c_tabulated_totalsr.F
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|        SIGEPS187                     source/materials/mat/mat187/sigeps187.F
Chd|        SIGEPS36                      source/materials/mat/mat036/sigeps36.F
Chd|        SIGEPS36C                     source/materials/mat/mat036/sigeps36c.F
Chd|        SIGEPS36G                     source/materials/mat/mat036/sigeps36g.F
Chd|        SIGEPS36PI                    source/materials/mat/mat036/sigeps36pi.F
Chd|        SIGEPS43C                     source/materials/mat/mat043/sigeps43c.F
Chd|        SIGEPS43G                     source/materials/mat/mat043/sigeps43g.F
Chd|        SIGEPS44                      source/materials/mat/mat044/sigeps44.F
Chd|        SIGEPS44C                     source/materials/mat/mat044/sigeps44c.F
Chd|        SIGEPS44PI                    source/materials/mat/mat044/sigeps44pi.F
Chd|        SIGEPS53                      source/materials/mat/mat053/sigeps53.F
Chd|        SIGEPS55C                     source/materials/mat/mat055/sigeps55c.F
Chd|        SIGEPS56                      source/materials/mat/mat056/sigeps56.F
Chd|        SIGEPS56C                     source/materials/mat/mat056/sigeps56c.F
Chd|        SIGEPS56G                     source/materials/mat/mat056/sigeps56g.F
Chd|        SIGEPS57C                     source/materials/mat/mat057/sigeps57c.F
Chd|        SIGEPS60                      source/materials/mat/mat060/sigeps60.F
Chd|        SIGEPS60C                     source/materials/mat/mat060/sigeps60c.F
Chd|        SIGEPS60G                     source/materials/mat/mat060/sigeps60g.F
Chd|        SIGEPS64C                     source/materials/mat/mat064/sigeps64c.F
Chd|        SIGEPS66                      source/materials/mat/mat066/sigeps66.F
Chd|        SIGEPS66C                     source/materials/mat/mat066/sigeps66c.F
Chd|        SIGEPS78C                     source/materials/mat/mat078/sigeps78c.F
Chd|        SIGEPS86C                     source/materials/mat/mat086/sigeps86c.F
Chd|        SIGEPS86G                     source/materials/mat/mat086/sigeps86g.F
Chd|        SIGEPS93                      source/materials/mat/mat093/sigeps93.F
Chd|        SIGEPS93C                     source/materials/mat/mat093/sigeps93c.F
Chd|        SMS_FIXVEL                    source/ams/sms_fixvel.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VINTER(TF,IAD,IPOS ,ILEN,NEL0,X,DYDX,Y)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C
      INTEGER ILEN(*),IPOS(*),IAD(*),NEL0
      my_real X(*),DYDX(*),Y(*),TF(2,*)
      INTEGER I,J1,J,ICONT,J2
C=======================================================================
      J = 0
 100  CONTINUE
C 
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         IF(J<=ILEN(I)-1.AND.X(I)>TF(1,J1))THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
         ENDIF
       ENDDO
C
      IF(ICONT==1)GOTO 100
C
      DO I=1,NEL0
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1
        DYDX(I)=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))
        Y(I)   = TF(2,J1) + DYDX(I)*(X(I)-TF(1,J1))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  VINTER2                       source/tools/curve/vinter.F   
Chd|-- called by -----------
Chd|        FAIL_ORTHSTRAIN               source/materials/fail/orthstrain/fail_orthstrain_s.F
Chd|        FAIL_ORTHSTRAIN_C             source/materials/fail/orthstrain/fail_orthstrain_c.F
Chd|        M36ITER_IMP                   source/materials/mat/mat036/m36iter_imp.F
Chd|        MAT121C_NEWTON                source/materials/mat/mat121/mat121c_newton.F
Chd|        MAT121C_NICE                  source/materials/mat/mat121/mat121c_nice.F
Chd|        MAT121_NEWTON                 source/materials/mat/mat121/mat121_newton.F
Chd|        MAT121_NICE                   source/materials/mat/mat121/mat121_nice.F
Chd|        REDEF3                        source/elements/spring/redef3.F
Chd|        REDEF_SEATBELT                source/tools/seatbelts/redef_seatbelt.F
Chd|        RKENONL                       source/elements/spring/r4mat3.F
Chd|        SIGEPS121                     source/materials/mat/mat121/sigeps121.F
Chd|        SIGEPS121C                    source/materials/mat/mat121/sigeps121c.F
Chd|        SIGEPS158C                    source/materials/mat/mat158/sigeps158c.F
Chd|        SIGEPS28                      source/materials/mat/mat028/sigeps28.F
Chd|        SIGEPS36                      source/materials/mat/mat036/sigeps36.F
Chd|        SIGEPS36C                     source/materials/mat/mat036/sigeps36c.F
Chd|        SIGEPS56                      source/materials/mat/mat056/sigeps56.F
Chd|        SIGEPS60                      source/materials/mat/mat060/sigeps60.F
Chd|        SIGEPS68                      source/materials/mat/mat068/sigeps68.F
Chd|        SIGEPS70                      source/materials/mat/mat070/sigeps70.F
Chd|        SIGEPS96                      source/materials/mat/mat096/sigeps96.F
Chd|        SIGEPS96C                     source/materials/mat/mat096/sigeps96c.F
Chd|-- calls ---------------
Chd|====================================================================
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

Chd|====================================================================
Chd|  VINTER2DP                     source/tools/curve/vinter.F   
Chd|-- called by -----------
Chd|        M36ITER_IMP                   source/materials/mat/mat036/m36iter_imp.F
Chd|        REDEF3                        source/elements/spring/redef3.F
Chd|        RKENONL                       source/elements/spring/r4mat3.F
Chd|        SIGEPS28                      source/materials/mat/mat028/sigeps28.F
Chd|        SIGEPS68                      source/materials/mat/mat068/sigeps68.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  FINTER2                       source/tools/curve/vinter.F   
Chd|-- called by -----------
Chd|        FIXFINGEO                     source/constraints/general/impvel/fixfingeo.F
Chd|        M36ITER_IMP                   source/materials/mat/mat036/m36iter_imp.F
Chd|        SIGEPS56                      source/materials/mat/mat056/sigeps56.F
Chd|        SIGEPS60                      source/materials/mat/mat060/sigeps60.F
Chd|-- calls ---------------
Chd|====================================================================
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
