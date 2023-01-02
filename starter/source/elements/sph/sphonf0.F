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
Chd|  SPHONF0                       source/elements/sph/sphonf0.F 
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPHONF0(KXSP ,IXSP    ,NOD2SP  ,IPART   ,IPARTSP ,
     .                LPRTSPH , LONFSPH)
C-----------------------------------------------
C   LONFSPH(3,NUMSPH) : 
C     LIST OF PARTICLES SORTED FROM IPRT=1 TO NPART AND INSIDE EACH PART, 
C       PARTICLES ARE SPLITED IN ACTIVE AND INACTIVE PARTICLES.
C     LONFSPH(K) : PARTICLE NUMBER (1<= ... <=NUMSPH)
C-----------------------------------------------
C   LPRTSPH(2,0:NPART) :
C    LPRTSPH(1,IPRT) : LAST ACTIVE PARTICLE RECORD IN LONFSPH
C                           RELATED TO PART IPRT
C    LPRTSPH(2,IPRT) : LAST INACTIVE PARTICLE RECORD IN LONFSPH
C                           RELATED TO PART IPRT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),IPARTSP(*),
     .        IPART(LIPART1,*),LPRTSPH(2,0:NPART),LONFSPH(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPRT,SUIVSPH,N
C-----------------------------------------------
      SUIVSPH=0
      DO IPRT=1,NPART
       DO N=1,NUMSPH
        IF(IPARTSP(N)==IPRT.AND.(KXSP(2,N) > 0 .OR. (KXSP(2,N)/=0.AND.
     .           N >= FIRST_SPHSOL .AND. N < FIRST_SPHSOL+NSPHSOL)))THEN
         SUIVSPH=SUIVSPH+1
         LONFSPH(SUIVSPH)=N
        ENDIF
       ENDDO
       LPRTSPH(1,IPRT)=SUIVSPH
       DO N=1,NUMSPH
        IF(IPARTSP(N)==IPRT.AND.(KXSP(2,N) < 0 .AND. 
     .           (N < FIRST_SPHSOL .OR. N >= FIRST_SPHSOL+NSPHSOL)))THEN
         SUIVSPH=SUIVSPH+1
         LONFSPH(SUIVSPH)=N
        ENDIF
       ENDDO
       LPRTSPH(2,IPRT)=SUIVSPH
      ENDDO
C--------------------------------------------------
      RETURN
      END
