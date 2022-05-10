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
C.../IMPL2/
Cqz50c1m
      INTEGER LIMPL2
      PARAMETER(LIMPL2 = 132)
      COMMON /IMPL2/ DT_IMP,L_TOL,N_TOL,D_TOL,KZ_TOL,SCAL_DTP,
     .               SCAL_DTN,DT_MIN,DT_MAX,P_MACH,SK_INT,ALEN,
     .               ALEN0,HHT_A,DAMPA_IMP,DAMPB_IMP,DT1_IMP,
     .               NEWM_A,NEWM_B,SCAL_DTQ,LS_TOL,DTIMPF(101),
     .               N_TOLE,N_TOLF,N_TOLU,DLA_RIKS,SCAL_RIKS,
     .               R_N2,RF_MAX,RF_MIN,DT0_IMP,TOL_DIV,PRSTIFMAT_TOL
C     REAL ou REAL*8
      my_real
     .   DT_IMP,L_TOL,N_TOL,D_TOL,KZ_TOL,SCAL_DTP,SCAL_DTN,DT_MIN,
     .   DT_MAX,P_MACH,SK_INT,ALEN,ALEN0,HHT_A,DAMPA_IMP,DAMPB_IMP,
     .   DT1_IMP,NEWM_A,NEWM_B,SCAL_DTQ ,LS_TOL,DTIMPF,
     .   N_TOLE,N_TOLF,N_TOLU,DLA_RIKS,SCAL_RIKS,R_N2,RF_MAX,RF_MIN,
     .   DT0_IMP,TOL_DIV,PRSTIFMAT_TOL
