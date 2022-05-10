Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
C.../SCR03/
C...recopie par defaut en spmd
      INTEGER LSCR03

      PARAMETER (LSCR03 = 22)
      COMMON /SCR03/ NRLINK, NALELK, NCRST, IOUTV, INVERS,
     .               IUSERW, NUSERW, CODVERS, NOTUSED, ICODRUN,
     .               IMINVER, ISRCVER, PCODVER, PMINVER, PSRCVER,
     .               PINVERS, PIRUN, PCODRUN,IISROT,NXREF, NALELINK,
     .               ST_INVERS,PDEL
      INTEGER NRLINK, NALELK, NCRST, IOUTV, INVERS,
     .               IUSERW, NUSERW, CODVERS, NOTUSED, ICODRUN,
     .               IMINVER, ISRCVER, PCODVER, PMINVER, PSRCVER,
     .               PINVERS, PIRUN, PCODRUN,IISROT,NXREF, NALELINK,
     .               ST_INVERS,PDEL
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Clm110 +++
C     CODE OF THIS RUN (1:ENGINE)
C     ICODRUN
C     MINOR VERSION CODE OF THIS RUN
C     IMINVER
C     SOURCE VERSION CODE OF THIS RUN
C     ISRCVER
C     PREVIOUS RUN NUMBER (READ FROM RESTART)
C     PIRUN  
C     CODE OF PREVIOUS RUN  (READ FROM RESTART)
C     PCODRUN
C     CODVERS FOR PREVIOUS RUN (READ FROM RESTART)
C     PCODVER
C     MINOR VERSION CODE OF PREVIOUS RUN (READ FROM RESTART)
C     PMINVER
C     SOURCE VERSION CODE OF PREVIOUS RUN (READ FROM RESTART)
C     PSRCVER
C     INPUT VERSION OF PREVIOUS RUN (READ FROM RESTART)
C     PINVERS
Clm110 ---
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Clm110 +++
      INTEGER IRADIOS, IRESFIL
C     RADIOSS FILE R/W CODE
      PARAMETER (IRADIOS=2143942393)
C     RESTART FILE CODE
      PARAMETER (IRESFIL=101)
Clm110 ---
