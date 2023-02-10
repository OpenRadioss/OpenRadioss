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
Chd|  GLOBVARS                      source/spmd/globvars.F        
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE GLOBVARS(IGEO,IXR,NSTRF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),IXR(NIXR,*),
     .        NSTRF(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,P,NNOD,NSEGC,NSEGTG,NSEGS,K0

C   Variable Globale NUMNODG nombre global des noeuds
      NUMNODG = NUMNOD

C   Variable globale NUMELTGG,NUMELCG, nombre global de triangles
      NUMELQG = NUMELQ
      NUMELCG = NUMELC
      NUMELTGG = NUMELTG
      NUMELSG = NUMELS
      NUMELPG = NUMELP
      NUMELRG = NUMELR
      NUMELTRG = NUMELT
      NUMELS10G = NUMELS10
      NUMELS16G = NUMELS16
      NUMELS20G = NUMELS20
      NUMSPHG = NUMSPH
      NUMELXG = NUMELX
C   Variable globales NUMNODM (MAX des NUMNOD_L)
      NUMNODM = 0
C Nombre global des RBE2
      NRBE2G = NRBE2
C Nombre global des RBE3
      NRBE3G = NRBE3

      DO P = 1,NSPMD
       NNOD=0
       DO I = 1, NUMNOD
         IF(NLOCAL(I,P)==1)THEN
           NNOD = NNOD + 1
         ENDIF
       ENDDO
       NUMNODM = MAX(NUMNODM,NNOD)
       ENDDO
C
C   calcul nu nombre Elts 1D pour animations.

        NB1DG = NUMELT + NUMELR + NUMELP
        NSKEWAG = NUMELP + NUMELT + NUMSKW

        DO I=1,NUMELR
          IF(IGEO(11,IXR(1,I))==4 .OR.
     .       IGEO(11,IXR(1,I))==26.OR.
     .       IGEO(11,IXR(1,I))==13.OR.
     .       IGEO(11,IXR(1,I))==25.OR.
     .       IGEO(11,IXR(1,I))==45.OR.     
     .         IGEO(11,IXR(1,I))>=29.AND.
     .         IGEO(11,IXR(1,I))<=33 .OR.
     .       IGEO(11,IXR(1,I))==23)THEN

            NSKEWAG=NSKEWAG+1

          ELSEIF(IGEO(11,IXR(1,I))==12)THEN

            NSKEWAG=NSKEWAG+2
            NB1DG =NB1DG+1
          ENDIF

        ENDDO    
C   pour les sections et pour le proc0 on calcule le max
C   des nombres d'elements coques pour toutes les sections
cow
      SECTIOM = 0
      IF (NSECT>0) THEN
        K0 = NSTRF(25)

        DO I=1,NSECT

C  
          NSEGC = NSTRF(K0+9)
          NSEGTG = NSTRF(K0+13)
          NSEGS=NSTRF(K0+7)
          SECTIOM = MAX(SECTIOM,NSEGC,NSEGTG,NSEGS)
          K0 = NSTRF(K0+24)
        ENDDO
      ENDIF

      RETURN
      END
