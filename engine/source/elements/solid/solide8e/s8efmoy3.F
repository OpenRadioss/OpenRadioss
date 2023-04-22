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
Chd|  S8EFMOY3                      source/elements/solid/solide8e/s8efmoy3.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EFMOY3(
     1   SIGOR,   VOL,     QVIS,    PP,
     2   EINT,    RHO,     Q,       DEFP,
     3   EPSD,    EPSDM,   SIGM,    EINTM,
     4   RHOM,    QM,      DEFPM,   VOLG,
     5   STI,     STIN,    ICP,     OFF,
     6   VOL0,    VOL0G,   G_PLA,   G_EPSD,
     7   EINTTH,  EINTTHM, IEXPAN,  NEL,
     8   CONDE,   CONDEN)
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
#include      "nsvis_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,G_PLA,G_EPSD,IEXPAN,NEL
C     REAL
      my_real
     .   SIGOR(NEL,6),
     .   VOL(*),QVIS(*),PP(*),
     .   EINT(*),RHO(*),Q(*),DEFPM(*),DEFP(*),
     .   SIGM(NEL,6),EINTM(*),RHOM(*),QM(*),EPSD(*),EPSDM(*),
     .   VOLG(*),STI(*),STIN(*),OFF(*),VOL0(*),VOL0G(*),
     .   EINTTH(*),EINTTHM(*),CONDE(*),CONDEN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   P,FAC(MVSIZ),FAC2(MVSIZ)
C-----------------------------------------------
C   - post-traitement-valeur moyenne au sens a'=(_/  a dv ) /v
      DO I=1,NEL
         FAC(I) = OFF(I)*VOL(I)/VOLG(I)
         FAC2(I) = VOL(I)/VOLG(I)
         SIGM(I,1) = SIGM(I,1) + FAC(I) * SIGOR(I,1)
         SIGM(I,2) = SIGM(I,2) + FAC(I) * SIGOR(I,2)
         SIGM(I,3) = SIGM(I,3) + FAC(I) * SIGOR(I,3)
         SIGM(I,4) = SIGM(I,4) + FAC(I) * SIGOR(I,4)
         SIGM(I,5) = SIGM(I,5) + FAC(I) * SIGOR(I,5)
         SIGM(I,6) = SIGM(I,6) + FAC(I) * SIGOR(I,6)
         RHOM(I)   = RHOM(I)   + FAC2(I) * RHO(I)
         EINTM(I)  = EINTM(I)  + EINT(I)*VOL0(I)/VOL0G(I)
         QM(I)     = QM(I)     + FAC(I) * Q(I)
         STIN(I)     = STIN(I)     + STI(I)
      ENDDO
C
      IF(NODADT_THERM == 1) THEN
        DO I=1,NEL
          CONDEN(I)= CONDEN(I)+ CONDE(I)
        ENDDO
      ENDIF
C
      IF (IEXPAN > 0) THEN
        DO I=1,NEL
          EINTTHM(I) = EINTTHM(I) + EINTTH(I)*VOL0(I)/VOL0G(I)
        ENDDO
      ENDIF
C
      IF (G_PLA > 0) THEN
        DO I=1,NEL
          DEFPM(I)  = DEFPM(I)  + FAC(I) * DEFP(I)
        ENDDO
      ENDIF
c
      IF (G_EPSD > 0) THEN
       DO I=1,NEL
         EPSDM(I) = EPSDM(I) + FAC(I) * EPSD(I)
       ENDDO
      ENDIF
c
      IF (ICP == 1) THEN
       DO I=1,NEL
        P =ZEP3*(SIGOR(I,1)+SIGOR(I,2)+SIGOR(I,3)
     .               +SVIS(I,1)+SVIS(I,2)+SVIS(I,3))
        PP(I)     = PP(I)     + FAC(I)* (P-QVIS(I))
       ENDDO
      ENDIF
C-----------
      RETURN
      END
