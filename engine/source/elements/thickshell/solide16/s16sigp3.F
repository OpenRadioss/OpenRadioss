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
Chd|  S16SIGP3                      source/elements/thickshell/solide16/s16sigp3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|-- calls ---------------
Chd|        S8ZSIGP3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S16SIGP3(LFT,LLT,NEL,NPTR,NLAY,NPTT,ICP,MTN,
     .                    NPE,NIPMAX, PX, PY, PZ, VX, VY, VZ, 
     .                    W_GAUSS,DEFP,PM,MXT,SIG,DT1,
     .                    ELBUF_STR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "scr01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT, NPTR,NLAY,NPTT,ICR,ICS,ICT,ICP,NEL,
     .        MTN,NPE,NIPMAX,MXT(*)
      my_real
     .  VX(MVSIZ,*),VY(MVSIZ,*),VZ(MVSIZ,*),
     .  PX(MVSIZ,NPE,*),PY(MVSIZ,NPE,*),PZ(MVSIZ,NPE,*),
     .  DT1 ,W_GAUSS(9,9),DEFP(*),PM(NPROPM,*),SIG(NEL,6)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IL,IS,IT,IR,IP,WI,N, MX
C     REAL ou REAL*8
      my_real
     .  DVM(MVSIZ), DV(MVSIZ,NIPMAX),FAC(MVSIZ),DT3,DVP,F,E0(MVSIZ)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      IS = 1
      DO I=LFT,LLT
        DVM(I)=ZERO
      ENDDO

      DO IT=1,NPTT
       DO IR=1,NPTR
        DO IL=1,NLAY
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
          IP = IR + ( (IL-1) + (IT-1)*NLAY )*NPTR
          WI = W_GAUSS(IR,NPTR)*W_GAUSS(IL,NLAY)*W_GAUSS(IT,NPTT)
         DO I=LFT,LLT
           DV(I,IP)=ZERO
         ENDDO
         DO N=1,NPE
          DO I=LFT,LLT
            DV(I,IP)=DV(I,IP)+PX(I,N,IP)*VX(I,N)+PY(I,N,IP)*VY(I,N)
     .              +PZ(I,N,IP)*VZ(I,N)
            DVM(I)=DVM(I)+DV(I,IP)*WI
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DT3=THIRD*DT1
      IF (ICP == 1) THEN
        DO I=LFT,LLT
          FAC(I)=ONE
        ENDDO
      ELSEIF (ICP == 2) THEN
        MX = MXT(LFT)
        DO I=LFT,LLT
          E0(I)  = PM(20,MX)
        ENDDO
        CALL S8ZSIGP3(LFT  ,LLT       ,SIG  ,E0  ,DEFP,
     2                FAC  ,GBUF%G_PLA,NEL  )
      ENDIF
C
      DO IT=1,NPTT
       DO IR=1,NPTR
        DO IL=1,NLAY
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
          IP = IR + ( (IL-1) + (IT-1)*NLAY )*NPTR
          DO I=LFT,LLT
            F = LBUF%OFF(I)*FAC(I)
            DVP  = DT3*F*(DVM(I)-DV(I,IP))
            IF (DVP > ONE) THEN
              DVP =ZERO
              LBUF%OFF(I)=ZERO
            ENDIF
            LBUF%VOL(I)  = LBUF%VOL(I) *(ONE- DVP)
            LBUF%EINT(I) = LBUF%EINT(I)*(ONE- DVP)
          ENDDO 
        ENDDO
       ENDDO
      ENDDO
C-----------
      RETURN
      END SUBROUTINE S16SIGP3
