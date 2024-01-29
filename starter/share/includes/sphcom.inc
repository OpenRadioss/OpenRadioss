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
      PARAMETER (LSPHCOMI =25)
C
      COMMON /SPHCOMI/NUMSPH  ,    NISP, KVOISPH,  NSPBUF,  NSELSP,
     .                NISPCOND, NSPCOND,  NSPHSYM, MAXPJET,
     .                LWASPH  , NSPHIO,  NISPHIO, LVSPHIO, ISPHBUC,
     .   NSEG_IO , SIBUFSSG_IO, LVOISPH, NBPARTINLET, NSPHSOL,
     .   FIRST_SPHSOL, FIRST_SPHRES, ISPHRES, FIRSTNOD_SPHSOL,NSPHACT,ITSOL2SPH,
     .   SOL2SPH_FLAG
      INTEGER NUMSPH  ,   NISP, KVOISPH,  NSPBUF,  NSELSP,
     .   NISPCOND,NSPCOND,  NSPHSYM, MAXPJET,
     .   LWASPH  ,NSPHIO , NISPHIO, LVSPHIO, ISPHBUC,
     .   NSEG_IO, SIBUFSSG_IO, LVOISPH, NBPARTINLET, NSPHSOL,
     .   FIRST_SPHSOL, FIRST_SPHRES,ISPHRES, FIRSTNOD_SPHSOL, NSPHACT,ITSOL2SPH,
     .   SOL2SPH_FLAG
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
C.../SCRATCH/
      COMMON /SPHSCR1/ NSPHRES
      INTEGER NSPHRES
