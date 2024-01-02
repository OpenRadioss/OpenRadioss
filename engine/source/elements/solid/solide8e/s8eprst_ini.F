Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  S8EPRST_INI                   source/elements/solid/solide8e/s8eprst_ini.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EPRST_INI(PR ,PS ,PT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   PR(8,8),PS(8,8),PT(8,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IP
      my_real
     .   PG2,KSI(8),ETA(8),ZETA(8),ETAZETA(8),KSIETA(8),KSIZETA(8),PG,MPG
       PARAMETER (PG=.577350269189625D0)
       PARAMETER (MPG=-PG)
      DATA KSI / MPG, PG, MPG, PG, MPG, PG, MPG, PG/
      DATA ETA / MPG,MPG,  PG, PG, MPG, MPG, PG, PG/
      DATA ZETA / MPG,MPG, MPG, MPG, PG, PG, PG, PG/
C-----------------------------------------------
      PG2=PG*PG
      ETAZETA(1) = PG2
      ETAZETA(2) = PG2
      ETAZETA(3) = -PG2
      ETAZETA(4) = -PG2
      ETAZETA(5) = -PG2
      ETAZETA(6) = -PG2
      ETAZETA(7) = PG2
      ETAZETA(8) = PG2
C
      KSIETA(1) = PG2
      KSIETA(2) = -PG2
      KSIETA(3) = -PG2
      KSIETA(4) = PG2
      KSIETA(5) = PG2
      KSIETA(6) = -PG2
      KSIETA(7) = -PG2
      KSIETA(8) = PG2
C
      KSIZETA(1) = PG2
      KSIZETA(2) = -PG2
      KSIZETA(3) = PG2
      KSIZETA(4) = -PG2
      KSIZETA(5) = -PG2
      KSIZETA(6) = PG2
      KSIZETA(7) = -PG2
      KSIZETA(8) = PG2
C------NI,ksi-----ETA(IP),ZETA(IP),KSIETA(IP).....--
      DO IP = 1,8
       PR(1,IP)=-(ONE-ETA(IP)-ZETA(IP)+ETAZETA(IP))
       PR(2,IP)=-PR(1,IP)
       PR(3,IP)= ONE+ETA(IP)-ZETA(IP)-ETAZETA(IP)
       PR(4,IP)=-PR(3,IP)
       PR(5,IP)=-(ONE-ETA(IP)+ZETA(IP)-ETAZETA(IP))
       PR(6,IP)=-PR(5,IP)
       PR(7,IP)= ONE+ETA(IP)+ZETA(IP)+ETAZETA(IP)
       PR(8,IP)=-PR(7,IP)
       PS(1,IP)=-(ONE-KSI(IP)-ZETA(IP)+KSIZETA(IP))
       PS(2,IP)=-(ONE+KSI(IP)-ZETA(IP)-KSIZETA(IP))
       PS(3,IP)=-PS(2,IP)
       PS(4,IP)=-PS(1,IP)
       PS(5,IP)=-(ONE-KSI(IP)+ZETA(IP)-KSIZETA(IP))
       PS(6,IP)=-(ONE+KSI(IP)+ZETA(IP)+KSIZETA(IP))
       PS(7,IP)=-PS(6,IP)
       PS(8,IP)=-PS(5,IP)
       PT(1,IP)=-(ONE-KSI(IP)-ETA(IP)+KSIETA(IP))
       PT(2,IP)=-(ONE+KSI(IP)-ETA(IP)-KSIETA(IP))
       PT(3,IP)=-(ONE+KSI(IP)+ETA(IP)+KSIETA(IP))
       PT(4,IP)=-(ONE-KSI(IP)+ETA(IP)-KSIETA(IP))
       PT(5,IP)=-PT(1,IP)
       PT(6,IP)=-PT(2,IP)
       PT(7,IP)=-PT(3,IP)
       PT(8,IP)=-PT(4,IP)
      END DO !IP = 1,8
      
      RETURN
      END
