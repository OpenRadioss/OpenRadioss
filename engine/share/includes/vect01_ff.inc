!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!--------------------------------------------------------------------
!  Free-form /VECT01/ for #include from .F90 (e.g. solid_q1np/q1np_forc3.F90).
!  Variable order and types must match engine/share/includes/vect01_c.inc
!  (INITBUF / FORINT).  Keep in sync when vect01_c.inc changes.
!  No task_common.inc: not safe to include from free-form; use plain COMMON
!  (as task_common maps to on current platforms; see spe_inc/task_common.inc).
!--------------------------------------------------------------------
!.../VECT01/
      COMMON /VECT01/ LFT, LLT, NFT, MTN, IAD, ITY, NPT, JALE, &
     &   ISMSTR, JEUL, JTUR, JTHE, JLAG, JMULT,JHBE,JIVF,NVAUX,JPOR, &
     &   JCVT, JSPH,JCLOSE,JPLASOL, IREP,IINT,IHET,IGTYP, &
     &   ISORTH,ISORTHG,ISRAT,ISROT,ICSEN,IFAILURE, &
     &   JSMS,ISPH2SOL,IPARTSPH,IGRE,IFORMDT
!$OMP THREADPRIVATE(/VECT01/)
      INTEGER LFT, LLT, NFT, MTN, IAD, ITY, NPT, JALE, ISMSTR, JEUL, &
     &   JTUR, JTHE, JLAG, JMULT,JHBE,JIVF,NVAUX,JPOR, &
     &   JCVT, JSPH, JCLOSE, JPLASOL, IREP,IINT,IHET,IGTYP, &
     &   ISORTH,ISORTHG,ISRAT,ISROT,ICSEN,IFAILURE, &
     &   JSMS,ISPH2SOL,IPARTSPH,IGRE,IFORMDT
