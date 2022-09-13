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
Chd|  I21PWR3                       source/interfaces/inter3d1/i21pwr3.F
Chd|-- called by -----------
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I21PWR3(ITAB,INACTI,CAND_E,CAND_N,STFN,
     1                  X     ,I_STOK,NSV   ,IWPENE,PENI,
     2                  NOINT,NTY   ,GAP_S ,MSR ,IRTLM  ,
     3                  IRECT,XM0   ,GAPMIN,GAPMAX,DEPTH,
     4                  NSN  ,ITAG  ,DRAD  ,ID,TITR)
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
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),CAND_E(*),CAND_N(*), IRECT(4,*), IRTLM(2,*),
     .        ITAG(*)
      INTEGER I_STOK,NSV(*),MSR(*),IWPENE,INACTI,NOINT,NTY,NSN
C     REAL
      my_real
     .   STFN(*),X(3,*),PENI(*),GAP_S(*),XM0(3,*),
     .   GAPMIN,GAPMAX,DEPTH,DRAD
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L,JWARN
      INTEGER IX1, IX2, IX3, IX4, NSVG
C     REAL
C-----------------------------------------------
       JWARN = 0
        DO 100 I=1,I_STOK
          J=CAND_N(I)
          L=CAND_E(I)
          IF(IRTLM(1,J)==CAND_E(I))THEN

            IX1=MSR(IRECT(1,L))
            IX2=MSR(IRECT(2,L))
            IX3=MSR(IRECT(3,L))
            IX4=MSR(IRECT(4,L))
            NSVG=NSV(J)
            IF(PENI(J)>ZERO)THEN
              IF(IPRI>=1)THEN
                WRITE(IOUT,FMT=FMW_5I)
     1           ITAB(NSVG),
     2           ITAB(IX1),ITAB(IX2),
     3           ITAB(IX3),ITAB(IX4)
              END IF
c              IF(PENI(J)>ZERO)THEN
                WRITE(IOUT,1000)PENI(J)
c              ENDIF
              IF(INACTI/=6)THEN
                IF(INACTI==1) THEN
C                 DESACTIVATION DES NOEUDS
                  WRITE(IOUT,'(A)')'NODE STIFFNESS IS SET TO ZERO'
                  STFN(J) = ZERO
                ELSE IF(INACTI==2) THEN
C                 DESACTIVATION DES ELEMENTS
                  WRITE(IOUT,'(A)')
     .              'INACTI=2 IS NOT AVAILABLE FOR INTERFACE TYPE21'
                ELSE IF(INACTI==3) THEN
C                 CHANGE LES COORDONNEES DES NOEUDS SECND
                  WRITE(IOUT,'(A)')
     .              'INACTI=3 IS NOT AVAILABLE FOR INTERFACE TYPE21'
                ELSE IF(INACTI==4) THEN
C                 CHANGE LES COORDONNEES DES NOEUDS MAIN
                  WRITE(IOUT,'(A)')
     .              'INACTI=4 IS NOT AVAILABLE FOOR INTERFACE TYPE21'
                ELSE IF(INACTI==5) THEN
C                 REDUCTION DU GAP 
                  JWARN = 1
                ENDIF
              ELSE
C               INACTI==6
C               REDUCTION DU GAP 
                JWARN = 1
C
                PENI(J)=PENI(J)
     .           +ZEP05*(MIN(MAX(GAP_S(J),GAPMIN),GAPMAX)+DEPTH-PENI(J))
              END IF
              IWPENE=IWPENE+1
            ENDIF
          ENDIF
 100    CONTINUE
      IF (JWARN /= 0) WRITE(IOUT,'(A)')'REDUCE INITIAL GAP'
C
      IF(IWPENE/=0) THEN
        CALL ANCMSG(MSGID=499,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,I2=IWPENE)
      ENDIF
C
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13)
      RETURN
      END
