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
C.../COM06/
C...mis a zero par defaut en spmd
      INTEGER LCOM06
      PARAMETER (LCOM06 = 60)
C
      COMMON /COM06/ DTDES, DTHIS, DTFAC, DTMIN, VOLMIN,
     .               REINT, DTPAT, TPAT,
     .                      UREINT,ECONTV,EHOUR, DTOUTP,
     .               TOUTP, T1S,   DT2S,  EPOR,  USREINT,
     .               R2RFX1,R2RFX2,DAMPA, DAMPB, DAMPW,
     .               DTHIS1(9),DWMAD,T1SH,SHFTBUCK,DTSTAT,
     .               TSTAT,DTABF(10),DTABFWR(10),DTFACX,
     .               ECONTD,ECONT_CUMU,DTDYNAIN, TDYNAIN
	  COMMON /COM06DP/TFEXT 
C     REAL ou REAL*8
      my_real
     .               DTDES, DTHIS, DTFAC, DTMIN, VOLMIN,
     .               REINT, DTPAT, TPAT,
     .                      UREINT,ECONTV,EHOUR, DTOUTP,
     .               TOUTP, T1S,   DT2S,  EPOR,  USREINT,
     .               R2RFX1,R2RFX2,DAMPA, DAMPB, DAMPW,
     .               DTHIS1,DWMAD ,T1SH,  SHFTBUCK,DTSTAT,
     .               TSTAT,DTABF,DTABFWR ,DTFACX,ECONTD  ,
     .               ECONT_CUMU,DTDYNAIN, TDYNAIN
      DOUBLE PRECISION TFEXT
