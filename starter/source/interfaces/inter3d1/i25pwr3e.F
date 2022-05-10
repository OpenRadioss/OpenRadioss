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
Chd|  I25PWR3E                      source/interfaces/inter3d1/i25pwr3e.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25PWR3E(ITAB ,INACTI,CAND_M,CAND_S ,ISTOK,
     1                   LLT   ,PENE  ,IWPENE ,CAND_P ,
     2                   N1    ,N2    ,M1     ,M2    ,
     3                   NOINT ,NTY   ,IRECT ,ID     ,TITR   ,
     4                   CAND_M_G ,CAND_S_G,CAND_P_G)

      USE MESSAGE_MOD
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),CAND_M(*),CAND_S(*), IRECT(4,*), 
     .        N1(*), N2(*), M1(*), M2(*),
     .        CAND_M_G(*),CAND_S_G(*)
      INTEGER LLT,IWPENE,INACTI,NOINT,NTY,NSN,ISTOK
C     REAL ou REAL*8
      my_real
     .   PENE(*), CAND_P(*), CAND_P_G(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L
      INTEGER IX1, IX2, IX3, IX4, NSVG
C     REAL ou REAL*8
C-----------------------------------------------
       DO I=1,LLT

         IF(PENE(I)/=ZERO)THEN
C          True initial penetration
           IWPENE=IWPENE+1
           IF(IPRI.GE.5)
     .         CALL ANCMSG(MSGID=1631,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ITAB(N1(I)),
     .                         I2=ITAB(N2(I)),
     .                         I3=ITAB(M1(I)),
     .                         I4=ITAB(M2(I)),
     .                         R1=PENE(I),
     .                         PRMOD=MSG_CUMU)
           IF(INACTI.EQ.0)THEN
C            Ignore initial penetrations
C          ELSEIF(INACTI.EQ.1) THEN
C            DESACTIVATION DES NOEUDS
C            WRITE(IOUT,'(A)')'NODE STIFFNESS IS SET TO ZERO'
C            STFN(J) = ZERO
           ELSE IF(INACTI.EQ.5) THEN
C
C            Reduction of PENE
             ISTOK=ISTOK+1
             CAND_M_G(ISTOK)= CAND_M(I)
             CAND_S_G(ISTOK)= CAND_S(I)
             CAND_P_G(ISTOK)= -PENE(I)
           ELSE IF(INACTI.EQ.-1) THEN
             ISTOK=ISTOK+1
C            CAND_P < 0 <=> Initial penetration into the Starter & Initial forces
             CAND_M_G(ISTOK)= CAND_M(I)
             CAND_S_G(ISTOK)= CAND_S(I)
             CAND_P_G(ISTOK)= -PENE(I)
           ENDIF
         ELSE
         END IF

      END DO
C
      RETURN
      END
