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
Chd|====================================================================
Chd|  M18LAW                        source/materials/mat/mat018/m18law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        INTERP                        source/tools/curve/interp.F   
Chd|====================================================================
      SUBROUTINE M18LAW(PM     ,VOLU   ,EINT   ,THETA  ,DELTAX ,
     .                  TF     ,NPF    ,DT2T   ,NELTST ,ITYPTST,
     .                  IPM    ,STI    ,VOLN   ,MAT    ,NGL    ,
     .                  CONDE  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*),IPM(NPROPMI,*),MAT(*),NGL(*)

      INTEGER NELTST,ITYPTST
      my_real
     .   DT2T

      my_real
     .   PM(NPROPM,*), VOLU(*), EINT(*), THETA(*), DELTAX(*), TF(*),
     .   STI(*),VOLN(MVSIZ), CONDE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IFUNC, NPOINT, ITFUN, IKFUN, MX
      my_real A(MVSIZ), B(MVSIZ),
     .   DTX(MVSIZ), SPH(MVSIZ), RHO0, ESPE, DTDE, T0, BID,
     .   TIMESCAL,TSCAL,ESCAL,KSCAL,
     .   SPH_1,A_1,B_1
C-----------------------------------------------
C=======================================================================
      MX = MAT(LFT)
      SPH_1=PM(69,MX)
      A_1  =PM(75,MX)
      B_1  =PM(76,MX)
      DO I=LFT,LLT
        VOLN(I)=VOLU(I)
        SPH(I)=SPH_1
        A(I)  =A_1
        B(I)  =B_1
      ENDDO

      DO I=LFT,LLT
        IFUNC = IPM(12,MX)
        IF(IFUNC.NE.0)THEN
          RHO0=PM( 1,MX)
          NPOINT=(NPF(IFUNC+1)-NPF(IFUNC))/2
          TSCAL = PM(42,MX)
          ESCAL = PM(43,MX)
          ESPE  = ESCAL * EINT(I) / RHO0
          CALL INTERP(TF(NPF(IFUNC)),ESPE,NPOINT,THETA(I),DTDE)
          THETA(I) = TSCAL*THETA(I)
          SPH(I) = RHO0 / MAX(EM20,DTDE)
        ELSE
          THETA(I)=EINT(I)/SPH(I)
        ENDIF
      ENDDO

      ITFUN = IPM(11,MX)
      IF(ITFUN.NE.0)THEN
       T0=PM(79,MX)
       TIMESCAL = PM(41,MX)*TT
       NPOINT=(NPF(ITFUN+1)-NPF(ITFUN))/2
       DO I=LFT,LLT
          CALL INTERP(TF(NPF(ITFUN)),TIMESCAL,NPOINT,THETA(I),BID)
         THETA(I) = T0*THETA(I)
         EINT(I)  = SPH(I)*THETA(I)
       ENDDO
      ENDIF

      DO I=LFT,LLT
        IKFUN = IPM(13,MX)
        IF (IKFUN /= 0) THEN
          TSCAL = THETA(I) / PM(42,MX) 
          KSCAL = PM(44,MX)   
          NPOINT=(NPF(IKFUN+1)-NPF(IKFUN))/2
          CALL INTERP(TF(NPF(IKFUN)),TSCAL,NPOINT,A(I),BID)
          A(I) = KSCAL * A(I)
          B(I) = ZERO
        ENDIF
        DTX(I)=UNDEMI*DELTAX(I)**2*SPH(I)/(A(I)+B(I)*THETA(I))        
      ENDDO

C--------------------------
C     THERMAL TIME STEP
C--------------------------
        IF(IDT_THERM == 1)THEN
          DO I=LFT,LLT
             IF(DTX(I).LT.DT_THERM)  DT_THERM = DTX(I)
             CONDE(I) = QUATRE*VOLU(I)*(A(I)+B(I)*THETA(I))/DELTAX(I)*DELTAX(I)
c             CONDE(I) = CONDE(I)*OFF(I)  
          ENDDO     
        ENDIF  

      DO I=LFT,LLT
        ! dt2, nelts, itypts remplaces par dt2t, neltst, ityptst
        IF(DTX(I).GT.DT2T) CYCLE
        DT2T=DTX(I)
        NELTST =NGL(I)
        ITYPTST=ITY        
      ENDDO

      DO I=LFT,LLT
        STI(I)=ZERO
      END DO

      RETURN
      END
