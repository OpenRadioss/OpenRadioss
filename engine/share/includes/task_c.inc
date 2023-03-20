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
C.../TASK/
      INTEGER INTSEG, PARASIZ, NTHMAX
      PARAMETER(INTSEG=128, PARASIZ=8192, NTHMAX=512)
      COMMON /TASK/ NTHREAD, ILOCK, IPEV, INTWA,NRWDONE,
     .              NRLDONE,NRJDONE,NRIDONE,NRBDONE,NGDONE,
     .              I7KGLO,NACDONE,NSEDONE,NINTDONE,TINTDONE,
     .              NRKDONE,NSGDONE,NSEGDONE,NSRDONE,INTDONE,
     .              NCRIDONE,NBTA,NSORTI,
     .              INTSYNC,PARALLEL_END,PARALLEL_SECTION,NSEDONE2,
     .              ITASKP1_DEF, IOCLOSE,KMP_SET,KMPSTSIZ,STACKSIZ
      INTEGER NTHREAD, ILOCK, IPEV, INTWA ,NRWDONE,
     .        NRKDONE,NRLDONE,NRJDONE,NRIDONE,NRBDONE,NGDONE,
     .        I7KGLO,NACDONE,NSEDONE,NINTDONE,TINTDONE,
     .        NSGDONE,NSEGDONE,NSRDONE,INTDONE,
     .        NCRIDONE,NBTA,NSORTI,
     .        INTSYNC,PARALLEL_END,PARALLEL_SECTION,NSEDONE2,
     .        ITASKP1_DEF, IOCLOSE,KMP_SET,KMPSTSIZ,STACKSIZ
      COMMON/I7CRITR/ XMSRG(7),XSLVG(7),
     .                VMSRG(7),VSLVG(7)
C     REAL ou REAL*8
      my_real
     .   XMSRG, XSLVG, VMSRG, VSLVG
C..../SPMD/
      COMMON /SPMD/ ISPMD, IT_SPMD(PARASIZ), FR_SPMD(PARASIZ,6),
     .              NEW_BOUND,REAL,NMSNMAX,NEW_BOUND_E,S_FRLINK,
     .              NEW_BOUND_A,NEW_BOUND_V,INTSIZE,IFLSIZE,IEXICODT,
     .              IDDW,IPREAD,L_SPMD(PARASIZ),NSPMD_PER_NODE,IDDWSTAT,
     .              COMM_CONT
      INTEGER ISPMD, IT_SPMD, FR_SPMD,
     .        NEW_BOUND,REAL,NMSNMAX,NEW_BOUND_E,S_FRLINK,
     .        NEW_BOUND_A,NEW_BOUND_V,INTSIZE,IFLSIZE,IEXICODT,
     .        IDDW,IPREAD,L_SPMD,NSPMD_PER_NODE,IDDWSTAT,
     .        COMM_CONT
