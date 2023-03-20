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
C--------------------------------------------------------------------
C  Attention : Chaque nouvelle variable ajoutee dans ce common
C              doit etre imperativement initialisee dans la routine
C              INITBUF, et non directement dans les routines telles 
C              que FORINT, ALEMAIN, etc.
C--------------------------------------------------------------------
C.../VECT01/
C$PAR INSTANCE TASK /VECT01/
#include      "task_common.inc"
      task_common /VECT01/ LFT, LLT, NFT, MTN, IAD, ITY, NPT, JALE,
     .   ISMSTR, JEUL, JTUR, JTHE, JLAG, JMULT,JHBE,JIVF,NVAUX,JPOR,
     .   JCVT, JSPH,JCLOSE,JPLASOL, IREP,IINT,IHET,IGTYP,
     .   ISORTH,ISORTHG,ISRAT,ISROT,ICSEN,IFAILURE,
     .   JSMS,ISPH2SOL,IPARTSPH,IGRE,IFORMDT
c$dir thread_private(/VECT01/)
!IBM* THREADLOCAL /VECT01/
!$OMP THREADPRIVATE(/VECT01/)
      INTEGER LFT, LLT, NFT, MTN, IAD, ITY, NPT, JALE, ISMSTR, JEUL,
     .   JTUR, JTHE, JLAG, JMULT,JHBE,JIVF,NVAUX,JPOR,
     .   JCVT, JSPH, JCLOSE, JPLASOL, IREP,IINT,IHET,IGTYP,
     .   ISORTH,ISORTHG,ISRAT,ISROT,ICSEN,IFAILURE,
     .   JSMS,ISPH2SOL,IPARTSPH,IGRE,IFORMDT
