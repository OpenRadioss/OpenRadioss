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
Chd|  LAG_MULT_H                    source/tools/lagmul/lag_mult_h.F
Chd|-- called by -----------
Chd|        LAG_MULT_SOLV                 source/tools/lagmul/lag_mult_solv.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAG_MULT_H(
     1                    NC     ,LENH   ,LHMAX  ,MS     ,IN     ,
     2                    DIAG   ,HH     ,IADLL  ,LLL    ,JLL    ,
     3                    XLL    ,LTSM   ,IADHF  ,JCIHF  ,IADH   ,
     4                    JCIH   ,RBYL   ,NPBYL  ,ICFTAG ,JCFTAG ,
     5                    NCF_S  ,NCF_E  ,NCR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,NCR,NCF_S,NCF_E,LENH,LHMAX
      INTEGER LLL(*),JLL(*),IADLL(*),IADHF(*),JCIHF(*),IADH(*),JCIH(*),
     .        NPBYL(NNPBY,*),ICFTAG(*),JCFTAG(*)
      my_real
     .  MS(*),IN(*),HH(*),DIAG(*),XLL(*),LTSM(6,*),RBYL(NRBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IK,IC,ICF,JCF,IR,IFX,NFIX,NFRE,JC,JF,IH,IHF
      my_real
     .   HIJ,DD
C=======================================================================
C     stockage creux : diagonale + trangle inf en colonnes:
C     DIAG(NC)
C     IADH(NC+1)
C     JCIH(LENH)
C=======================================================================
      IH      = 1
      IADH(1) = 1
C---
C     partie variable - Interfaces/Rwall
C---
      DO IC=1,NCF_S
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I  = LLL(IK)
          J  = JLL(IK)
          IF (J>3) THEN
            LTSM(J,I) = XLL(IK)/IN(I)
          ELSE
            LTSM(J,I) = XLL(IK)/MS(I)
          ENDIF
        ENDDO
        DO JC=IC+1,NC
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF(HIJ/=ZERO)THEN
            IF(IH>LHMAX)THEN
              CALL ANCMSG(MSGID=114,ANMODE=ANINFO,
     .                    I1=LHMAX) 
              CALL ARRET(2)
            ENDIF
            HH(IH)   = HIJ
            JCIH(IH) = JC
            IH       = IH + 1
          ENDIF
        ENDDO
C---
        IADH(IC+1) = IH
        DD = ZERO
        DO IK=IADLL(IC),IADLL(IC+1)-1
          DD = DD + XLL(IK)*LTSM(JLL(IK),LLL(IK))
        ENDDO
        IF(DD<=ZERO) THEN
          CALL ANCMSG(MSGID=115,ANMODE=ANINFO,
     .                    I1=IC) 
        ENDIF
        DIAG(IC) = DD
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = ZERO
        ENDDO
      ENDDO
C----
C---- partie Fixe
C----
      DO IC=NCF_S+1,NCF_E
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I  = LLL(IK)
          J  = JLL(IK)
          IF (J>3) THEN
            LTSM(J,I) = XLL(IK)/IN(I)
          ELSE
            LTSM(J,I) = XLL(IK)/MS(I)
          ENDIF
        ENDDO
C       Fixe/Fixe
        ICF = ICFTAG(IC-NCF_S)
        DO IHF=IADHF(ICF),IADHF(ICF+1)-1
          JCF = JCIHF(IHF)
          JC  = JCFTAG(JCF)
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF(HIJ/=ZERO)THEN
            HH(IH)   = HIJ
            JCIH(IH) = JC
            IH       = IH + 1
          ENDIF
        ENDDO
C       Fixe/Variable
        DO JC=NCF_E+1,NC
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF(HIJ/=ZERO)THEN
            IF(IH>LHMAX)THEN
              CALL ANCMSG(MSGID=114,ANMODE=ANINFO,
     .                    I1=LHMAX) 
              CALL ARRET(2)
            ENDIF
            HH(IH)   = HIJ
            JCIH(IH) = JC
            IH       = IH + 1
          ENDIF
        ENDDO
C---
        IADH(IC+1) = IH
        DD = ZERO
        DO IK=IADLL(IC),IADLL(IC+1)-1
          DD = DD + XLL(IK)*LTSM(JLL(IK),LLL(IK))
        ENDDO
        IF(DD<=ZERO) THEN
          CALL ANCMSG(MSGID=115,ANMODE=ANINFO,
     .                    I1=IC) 
        ENDIF
        DIAG(IC) = DD
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = ZERO
        ENDDO
      ENDDO
C----
C     partie Variable - RB
C----
      DO IC=NCF_E+1,NCR
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I  = LLL(IK)
          J  = JLL(IK)
          IF (J>3) THEN
            LTSM(J,I) = XLL(IK)/IN(I)
          ELSE
            LTSM(J,I) = XLL(IK)/MS(I)
          ENDIF
        ENDDO
        DO JC=IC+1,NC
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF(HIJ/=ZERO)THEN
            IF(IH>LHMAX)THEN
              CALL ANCMSG(MSGID=114,ANMODE=ANINFO,
     .                    I1=LHMAX) 
              CALL ARRET(2)
            ENDIF
            HH(IH)   = HIJ
            JCIH(IH) = JC
            IH       = IH + 1
          ENDIF
        ENDDO
        IADH(IC+1) = IH
        DD = 0.
        DO IK=IADLL(IC),IADLL(IC+1)-1
          DD = DD + XLL(IK)*LTSM(JLL(IK),LLL(IK))
        ENDDO
        IF(DD<=ZERO) THEN
          CALL ANCMSG(MSGID=115,ANMODE=ANINFO,
     .                    I1=IC) 
        ENDIF
        DIAG(IC) = DD
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = ZERO
        ENDDO
      ENDDO
C----
C     partie RB condensee
C----
      IC = NCR
      DO IR = 1,NRBYLAG
        NFIX = NPBYL(4,IR)
        NFRE = NPBYL(5,IR)
        IFX  = NPBYL(7,IR)
        IF (NFIX>0.AND.NFRE>0) THEN
          DO K = 1,3
            IC = IC + 1
            DO IK=IADLL(IC),IADLL(IC+1)-1
              I  = LLL(IK)
              J  = JLL(IK)
              IF (J<=3) THEN
                LTSM(J,I) = XLL(IK)/MS(I)
                CALL ANCMSG(MSGID=116,ANMODE=ANINFO,
     .                    I1=I,I2=IC) 
                CALL ARRET(2)
              ELSEIF (I/=IFX) THEN
                LTSM(J,I) = XLL(IK)/IN(I)
              ELSEIF (XLL(IK)/=ZERO) THEN
                IF(J==4) THEN
                  LTSM(4,I) = XLL(IK)*RBYL(1,IR)
                  LTSM(5,I) = XLL(IK)*RBYL(6,IR)
                  LTSM(6,I) = XLL(IK)*RBYL(5,IR)
                ELSEIF(J==5) THEN
                  LTSM(4,I) = XLL(IK)*RBYL(6,IR)
                  LTSM(5,I) = XLL(IK)*RBYL(2,IR)
                  LTSM(6,I) = XLL(IK)*RBYL(4,IR)
                ELSEIF(J==6) THEN
                  LTSM(4,I) = XLL(IK)*RBYL(5,IR)
                  LTSM(5,I) = XLL(IK)*RBYL(4,IR)
                  LTSM(6,I) = XLL(IK)*RBYL(3,IR)
                ENDIF
              ENDIF
             ENDDO
            DO JC=IC+1,NC
              HIJ = ZERO
              DO IK=IADLL(JC),IADLL(JC+1)-1
                HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
              ENDDO
              IF(HIJ/=ZERO)THEN
                IF(IH>LHMAX)THEN
                  CALL ANCMSG(MSGID=114,ANMODE=ANINFO,
     .                    I1=LHMAX) 
                  CALL ARRET(2)
                ENDIF
                HH(IH)   = HIJ
                JCIH(IH) = JC
                IH       = IH + 1
              ENDIF
            ENDDO
            IADH(IC+1) = IH
            DD = ZERO
            DO IK=IADLL(IC),IADLL(IC+1)-1
              DD = DD + XLL(IK)*LTSM(JLL(IK),LLL(IK))
            ENDDO
            IF(DD<=ZERO) THEN
              CALL ANCMSG(MSGID=115,ANMODE=ANINFO,
     .                    I1=IC) 
            ENDIF
            DIAG(IC) = DD
            DO IK=IADLL(IC),IADLL(IC+1)-1
              I  = LLL(IK)
              J  = JLL(IK)
              IF (J<=3) THEN
                LTSM(J,I) = ZERO
              ELSE
                LTSM(4,I) = ZERO
                LTSM(5,I) = ZERO
                LTSM(6,I) = ZERO
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      LENH = IH - 1
C--------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  LAG_MULT_HP                   source/tools/lagmul/lag_mult_h.F
Chd|-- called by -----------
Chd|        LAG_MULT_SOLVP                source/tools/lagmul/lag_mult_solv.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAG_MULT_HP(
     1                     NC     ,LENH   ,LHMAX  ,MS      ,IN     ,
     2                     DIAG   ,HH     ,IADLL  ,LLL     ,JLL    ,
     3                     XLL    ,LTSM   ,IADHF  ,JCIHF   ,IADH   ,
     4                     JCIH   ,RBYL   ,NPBYL  ,ICFTAG  ,JCFTAG ,
     5                     NCF_S  ,NCF_E  ,NCR    ,INDEXLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,NCR,NCF_S,NCF_E,LENH,LHMAX
      INTEGER LLL(*),JLL(*),IADLL(*),IADHF(*),JCIHF(*),IADH(*),JCIH(*),
     .        NPBYL(NNPBY,*),ICFTAG(*),JCFTAG(*),INDEXLAG(*)
      my_real
     .  MS(*),IN(*),HH(*),DIAG(*),XLL(*),LTSM(6,*),RBYL(NRBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IK,IC,ICF,JCF,IR,IFX,NFIX,NFRE,JC,JF,IH,IHF,II
      my_real
     .   HIJ,DD
C=======================================================================
C     stockage creux : diagonale + trangle inf en colonnes:
C     DIAG(NC)
C     IADH(NC+1)
C     JCIH(LENH)
C=======================================================================
      IH      = 1
      IADH(1) = 1
C---
C     partie variable - Interfaces/Rwall
C---
      DO IC=1,NCF_S
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I  = LLL(IK)
          II = INDEXLAG(I)
          J  = JLL(IK)
          IF (J>3) THEN
            LTSM(J,I) = XLL(IK)/IN(II)
          ELSE
            LTSM(J,I) = XLL(IK)/MS(II)
          ENDIF
        ENDDO
        DO JC=IC+1,NC
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF(HIJ/=ZERO)THEN
            IF(IH>LHMAX)THEN
              CALL ANCMSG(MSGID=114,ANMODE=ANINFO,
     .                    I1=LHMAX) 
              CALL ARRET(2)
            ENDIF
            HH(IH)   = HIJ
            JCIH(IH) = JC
            IH       = IH + 1
          ENDIF
        ENDDO
C---
        IADH(IC+1) = IH
        DD = ZERO
        DO IK=IADLL(IC),IADLL(IC+1)-1
          DD = DD + XLL(IK)*LTSM(JLL(IK),LLL(IK))
        ENDDO
        IF(DD<=ZERO) THEN
          CALL ANCMSG(MSGID=115,ANMODE=ANINFO,
     .                    I1=IC) 
        ENDIF
        DIAG(IC) = DD
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = ZERO
        ENDDO
      ENDDO
C----
C---- partie Fixe
C----
      DO IC=NCF_S+1,NCF_E
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I  = LLL(IK)
          II = INDEXLAG(I)
          J  = JLL(IK)
          IF (J>3) THEN
            LTSM(J,I) = XLL(IK)/IN(II)
          ELSE
            LTSM(J,I) = XLL(IK)/MS(II)
          ENDIF
        ENDDO
C       Fixe/Fixe
        ICF = ICFTAG(IC-NCF_S)
        DO IHF=IADHF(ICF),IADHF(ICF+1)-1
          JCF = JCIHF(IHF)
          JC  = JCFTAG(JCF)
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF(HIJ/=ZERO)THEN
            HH(IH)   = HIJ
            JCIH(IH) = JC
            IH       = IH + 1
          ENDIF
        ENDDO
C       Fixe/Variable
        DO JC=NCF_E+1,NC
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF(HIJ/=ZERO)THEN
            IF(IH>LHMAX)THEN
              CALL ANCMSG(MSGID=114,ANMODE=ANINFO,
     .                    I1=LHMAX) 
              CALL ARRET(2)
            ENDIF
            HH(IH)   = HIJ
            JCIH(IH) = JC
            IH       = IH + 1
          ENDIF
        ENDDO
C---
        IADH(IC+1) = IH
        DD = ZERO
        DO IK=IADLL(IC),IADLL(IC+1)-1
          DD = DD + XLL(IK)*LTSM(JLL(IK),LLL(IK))
        ENDDO
        IF(DD<=ZERO) THEN
          CALL ANCMSG(MSGID=115,ANMODE=ANINFO,
     .                    I1=IC) 
        ENDIF
        DIAG(IC) = DD
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = ZERO
        ENDDO
      ENDDO
C----
C     partie Variable - RB
C----
      DO IC=NCF_E+1,NCR
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I  = LLL(IK)
          II = INDEXLAG(I)
          J  = JLL(IK)
          IF (J>3) THEN
            LTSM(J,I) = XLL(IK)/IN(II)
          ELSE
            LTSM(J,I) = XLL(IK)/MS(II)
          ENDIF
        ENDDO
        DO JC=IC+1,NC
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF(HIJ/=ZERO)THEN
            IF(IH>LHMAX)THEN
              CALL ANCMSG(MSGID=114,ANMODE=ANINFO,
     .                    I1=LHMAX) 
              CALL ARRET(2)
            ENDIF
            HH(IH)   = HIJ
            JCIH(IH) = JC
            IH       = IH + 1
          ENDIF
        ENDDO
        IADH(IC+1) = IH
        DD = 0.
        DO IK=IADLL(IC),IADLL(IC+1)-1
          DD = DD + XLL(IK)*LTSM(JLL(IK),LLL(IK))
        ENDDO
        IF(DD<=ZERO) THEN
          CALL ANCMSG(MSGID=115,ANMODE=ANINFO,
     .                    I1=IC) 
        ENDIF
        DIAG(IC) = DD
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = ZERO
        ENDDO
      ENDDO
C----
C     partie RB condensee
C----
      IC = NCR
      DO IR = 1,NRBYLAG
        NFIX = NPBYL(4,IR)
        NFRE = NPBYL(5,IR)
        IFX  = NPBYL(7,IR)
        IF (NFIX>0.AND.NFRE>0) THEN
          DO K = 1,3
            IC = IC + 1
            DO IK=IADLL(IC),IADLL(IC+1)-1
              I  = LLL(IK)
              II  = INDEXLAG(I)
              J  = JLL(IK)
              IF (J<=3) THEN
                LTSM(J,I) = XLL(IK)/MS(II)
                CALL ANCMSG(MSGID=116,ANMODE=ANINFO,
     .                    I1=I,I2=IC) 
                CALL ARRET(2)
              ELSEIF (I/=IFX) THEN
                LTSM(J,I) = XLL(IK)/IN(II)
              ELSEIF (XLL(IK)/=ZERO) THEN
                IF(J==4) THEN
                  LTSM(4,I) = XLL(IK)*RBYL(1,IR)
                  LTSM(5,I) = XLL(IK)*RBYL(6,IR)
                  LTSM(6,I) = XLL(IK)*RBYL(5,IR)
                ELSEIF(J==5) THEN
                  LTSM(4,I) = XLL(IK)*RBYL(6,IR)
                  LTSM(5,I) = XLL(IK)*RBYL(2,IR)
                  LTSM(6,I) = XLL(IK)*RBYL(4,IR)
                ELSEIF(J==6) THEN
                  LTSM(4,I) = XLL(IK)*RBYL(5,IR)
                  LTSM(5,I) = XLL(IK)*RBYL(4,IR)
                  LTSM(6,I) = XLL(IK)*RBYL(3,IR)
                ENDIF
              ENDIF
             ENDDO
            DO JC=IC+1,NC
              HIJ = ZERO
              DO IK=IADLL(JC),IADLL(JC+1)-1
                HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
              ENDDO
              IF(HIJ/=ZERO)THEN
                IF(IH>LHMAX)THEN
                  CALL ANCMSG(MSGID=114,ANMODE=ANINFO,
     .                    I1=LHMAX) 
                  CALL ARRET(2)
                ENDIF
                HH(IH)   = HIJ
                JCIH(IH) = JC
                IH       = IH + 1
              ENDIF
            ENDDO
            IADH(IC+1) = IH
            DD = ZERO
            DO IK=IADLL(IC),IADLL(IC+1)-1
              DD = DD + XLL(IK)*LTSM(JLL(IK),LLL(IK))
            ENDDO
            IF(DD<=ZERO) THEN
              CALL ANCMSG(MSGID=115,ANMODE=ANINFO,
     .                    I1=IC) 
            ENDIF
            DIAG(IC) = DD
            DO IK=IADLL(IC),IADLL(IC+1)-1
              I  = LLL(IK)
              J  = JLL(IK)
              IF (J<=3) THEN
                LTSM(J,I) = ZERO
              ELSE
                LTSM(4,I) = ZERO
                LTSM(5,I) = ZERO
                LTSM(6,I) = ZERO
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      LENH = IH - 1
C--------------------------------------------
      RETURN
      END
