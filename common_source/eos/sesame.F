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
C|============================================================
C|  SESAME                                       /dfs/sesame.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|         EOSMAIN                             /matera/mmain.F
C|-- appelle ---------------
C|         MINTP_RE                         /mate26/mintp_re.F
C|         MINTP_RT                         /mate26/mintp_rt.F
C|         MINTP1_RT                          /dfs/mintp1_rt.F
C|============================================================
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  SESAME                        eos/sesame.F                  
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|        MINTP1_RT                     eos/mintp1_rt.F               
Chd|        MINTP_RE                      eos/mintp_re.F                
Chd|        MINTP_RT                      eos/mintp_rt.F                
Chd|====================================================================
      SUBROUTINE SESAME(IFLAG,NEL, PM   ,OFF  ,EINT ,RHO  ,RHO0  , 
     2                  ESPE ,DVOL ,MAT  ,PNEW ,DPDM ,DPDE  ,THETA ,
     3                  BUFMAT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL), IFLAG, NEL
      my_real PM(NPROPM,NUMMAT), BUFMAT(SBUFMAT), 
     .        OFF(NEL) , EINT(NEL), RHO(NEL) , RHO0(NEL),
     .        ESPE(NEL), DVOL(NEL), PNEW(NEL), 
     .        DPDM(NEL), DPDE(NEL), THETA(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, NR, NT, IDR, IDT, IDP, IDE
      my_real ESPEM, PRES, DTDE, DPDR, DPDT
      my_real PC(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF(IFLAG == 0) THEN
C-----------------------------------------
C     COMPUTE BULK MODULUS FOR SOUND SPEED
C-----------------------------------------
       DO I=1,NEL
        MX  = MAT(I)
        NR  = NINT(PM(33,MX))
        NT  = NINT(PM(34,MX))
        IDR = NINT(PM(35,MX))
        IDT = IDR + NR
        IDP = IDT + NT
        IDE = IDP + NR * NT
        ESPEM=ESPE(I)/RHO0(I)
C
C     Init for forcheck
        DTDE = ZERO
        CALL MINTP_RE(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDE),
     .                RHO(I),THETA(I),ESPEM,DTDE)
C
        CALL MINTP1_RT(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDP),
     .                RHO(I),THETA(I),PRES,DPDR,DPDT)
C
        DPDM(I)=RHO0(I)*DPDR
        DPDE(I)=DPDT*DTDE/RHO0(I)
       ENDDO
C
      ELSEIF(IFLAG == 1) THEN
C----------------------------------------
C     UPDATE PRESSURE AND INTERNAL ENERGY
C----------------------------------------
       DO I=1,NEL
        MX      = MAT(I)
        PC(I)   = PM(37,MX)
        NR  = NINT(PM(33,MX))
        NT  = NINT(PM(34,MX))
        IDR = NINT(PM(35,MX))
        IDT = IDR + NR
        IDP = IDT + NT
        IDE = IDP + NR * NT
        ESPEM=ESPE(I)/RHO0(I)
C
        CALL MINTP_RE(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDE),
     .                RHO(I),THETA(I),ESPEM,DTDE)
C
        CALL MINTP_RT(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDP),
     .                RHO(I),THETA(I),PNEW(I),DPDR)

        PNEW(I)= MAX(PNEW(I),PC(I))*OFF(I)
        EINT(I)= EINT(I) - HALF*DVOL(I)*PNEW(I)
       ENDDO
C
      ELSEIF(IFLAG == 2) THEN
         DO I=1, NEL
            MX  = MAT(I)
            NR  = NINT(PM(33,MX))
            NT  = NINT(PM(34,MX))
            PC(I)   = PM(37,MX)
            IDR = NINT(PM(35,MX))
            IDT = IDR + NR
            IDP = IDT + NT
            IDE = IDP + NR * NT
            ESPEM=ESPE(I)/RHO0(I)
C     
            CALL MINTP_RE(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDE),
     .           RHO(I),THETA(I),ESPEM,DTDE)
C     
            CALL MINTP1_RT(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDP),
     .           RHO(I),THETA(I),PRES,DPDR,DPDT)
C     
            DPDM(I)=RHO0(I)*DPDR
            DPDE(I)=DPDT*DTDE/RHO0(I)
            PNEW(I) = MAX(PRES,PC(I))*OFF(I)
         ENDDO
      ENDIF
      RETURN
      END
