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
C.../SPHCOMI/
      INTEGER LSPHCOMI
      PARAMETER (LSPHCOMI =18)
C
      COMMON /SPHCOMI/NUMSPH  ,    NISP, KVOISPH,  NSPBUF,  NSELSP,
     .                NISPCOND, NSPCOND, NSPHSYM, NSPHSYMR, MAXPJET,
     .                LWASPH  ,  NSPHIO, NISPHIO, LVSPHIO, ISPHBUC,
     .  LVOISPH , NSPHSOL, FIRST_SPHSOL, ITSOL2SPH, SOL2SPH_FLAG
      INTEGER NUMSPH  ,    NISP, KVOISPH,  NSPBUF,  NSELSP,
     .        NISPCOND, NSPCOND,  NSPHSYM, NSPHSYMR, MAXPJET,
     .        LWASPH  ,  NSPHIO, NISPHIO, LVSPHIO, ISPHBUC,
     .  LVOISPH , NSPHSOL, FIRST_SPHSOL, ITSOL2SPH, SOL2SPH_FLAG
C-------------------------------------------------------------------------------
C.../SPHCOMR/
      INTEGER LSPHCOMR
      PARAMETER (LSPHCOMR = 3)
C
      COMMON /SPHCOMR/ SPASORT,SPATRUE,SPAOLD
C     REAL ou REAL*8
      my_real
     .               SPASORT,SPATRUE,SPAOLD
C-------------------------------------------------------------------------------
C.../SPHSRC1/ scracth
      INTEGER LSPHSRC1
      PARAMETER (LSPHSRC1 =3)
      COMMON /SPHSRC1/ISPHRED,NSPHACT,NSP2SORT
      INTEGER ISPHRED,NSPHACT,NSP2SORT
C.../SPHSRC2/ scracth
      INTEGER LSPHSRC2
      PARAMETER (LSPHSRC2 =7)
      COMMON /SPHSRC2/DBUCS,XBMIN,YBMIN,ZBMIN,XBMAX,YBMAX,ZBMAX
      my_real
     .       DBUCS,XBMIN,YBMIN,ZBMIN,XBMAX,YBMAX,ZBMAX
C.../SPHSRC3/ scracth
      INTEGER LSPHSRC3
      PARAMETER (LSPHSRC3 =3)
      COMMON /SPHSRC3/NBOX,NBOY,NBOZ
      INTEGER NBOX,NBOY,NBOZ
C.../SPHSRC4/ scracth
      INTEGER LSPHSRC4
      PARAMETER (LSPHSRC4 =1)
      COMMON /SPHSRC4/KWASPH
      INTEGER KWASPH
