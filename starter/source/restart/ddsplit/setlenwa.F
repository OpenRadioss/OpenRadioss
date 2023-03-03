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
Chd|  SETLENWA                      source/restart/ddsplit/setlenwa.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SETLENWA(
     1      LENWA0 ,NTHWA0  ,NAIRWA0   ,NUMELS0  ,NUMELQ0,
     2      NUMELC0,NUMELTG0,NUMELT0   ,NUMELP0  ,NUMELR0,
     3      NUMNOD0,NMNT0   ,L_MUL_LAG1,L_MUL_LAG,MAXNX0 ,
     4      LWASPH0,NUMSPH0   ,LWASPIO  , NRCVVOIS0 )
C Attention les dummy peuvent etre soit locaux soit globaux suivant appel
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com09_c.inc"
#include      "scr05_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENWA0,NTHWA0,NAIRWA0,
     .        NUMELS0,NUMELQ0,NUMELC0,NUMELTG0,NUMELT0,NUMELP0,
     .        NUMELR0,NUMNOD0,NMNT0,L_MUL_LAG1,L_MUL_LAG,MAXNX0,
     .        LWASPH0,NUMSPH0,LWASPIO,NRCVVOIS0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFLUWA, NBHOLWA, NAIRWA, NINTWA, NXELWA, NSPHWA,
     .        NASSNMP, NASSMP, NIDELWA, NWASPH, MSPR,
     .        IUN,LENVOISWA
C-----------------------------------------------
      IUN = 1
C---------------------------------------------------------------------
C SPH
      NSPHWA = MAX(2*KVOISPH,4*KVOISPH,
     .             16*NUMSPH0,LWASPIO)   ! 19*NUMSPH0 enleve (tri en auto)
C work array WASPH, prive au SPH.
      LWASPH0=17*NUMSPH0+MIN(IUN,NSPHIO)*3*NUMSPH0
     .                            +MIN(IUN,NSPHSOL)*NUMSPH0
C---------------------------------------------------------------------
C ALE / CFD
      NFLUWA  = MAX(IALE,ITHERM,IEULER) * MAX(1,NMULT) * 
     .          MAX(7 * (NUMELS0+NUMELQ0),3 * NUMNOD0)
      NBHOLWA = MAX(IALE,ITHERM,IEULER) * NMULT * NUMELQ0
      LENVOISWA = 3 * ( NUMNOD0 + NRCVVOIS0 ) 
C AirBag 
      NAIRWA  = MAX(NAIRWA0, 4 * (NUMELC0 + NUMELTG0 + IBAGSURF))   ! en toute rigueur NAIRWA0 et IBAGSURF locaux
C Interface
      NINTWA  = NMNT0+L_MUL_LAG1                                    ! en toute rigueur NMNT local
C
C Assemblage
      IF(IMACH/=3) THEN
C SMP : allocation memoire partagee dans le WA (appel asspar2)
        NASSNMP = 4*NUMNOD0+2
C valeur precedente divisee par 2 car au moins 2 taches
        NASSMP  = 2*NUMNOD0+1
      ELSE
C SPMD : tout en dynamique
        NASSNMP = 0
        NASSMP  = 0
      END IF
C shooting nodes
      NIDELWA = 3*NUMNOD0
C lenwa non parallele
      LWANMP   = MAX(NTHWA0,NASSNMP,L_MUL_LAG)
C---------------------------------------------------------------------
      NXELWA   = 22*MAXNX0                                         ! en toute rigueur MAXNX local
C lenwa parallele
      LWAMP    = MAX(NAIRWA,NINTWA,NFLUWA+NBHOLWA,NASSMP,NXELWA,NSPHWA,
     +               NIDELWA,LENVOISWA)
C
C MSPR utilise dans le starter
      MSPR    = MAX(4*NUMELR0,NPARG*NGROUP)
C
      LENWA0  = MAX(LWAMP,LWANMP,MSPR)
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SETMULTI                      source/restart/ddsplit/setlenwa.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SETMULTI(IPARI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "spmd_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MULTIMP, ICONT, ITY
C-----------------------------------------------
C
C MULTIMAX : parametre servant a allouer les buffer de comm interface engine (SPMD)
C
      MULTIMAX = 0
      ICONT = 0
      IF(IMACH==3) THEN
        DO I = 1, NINTER
          MULTIMP = IPARI(23,I)
	  IF(MULTIMP<=4) THEN
	    MULTIMAX = MAX(MULTIMAX,4*MULTIMP)
	  ELSEIF(MULTIMP<=12)THEN
	    MULTIMAX= MAX(MULTIMAX,3*MULTIMP)
	  ELSEIF(MULTIMP>12)THEN
	    MULTIMAX= MAX(MULTIMAX,2*MULTIMP)
	  END IF
          ITY = IPARI(7,I)
          IF(ITY==7.OR.ITY==10.OR.ITY==11.OR.
     .       ITY==22)ICONT=1
        END DO
        IF(NUMSPH/=0.AND.MULTIMAX==0)MULTIMAX=1
        IF(NUMELX/=0.AND.MULTIMAX==0)MULTIMAX=4
        IF(NINTER>0.AND.ICONT==0)MULTIMAX=100
      END IF
C
      RETURN
      END
