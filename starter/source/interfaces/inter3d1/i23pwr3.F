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
Chd|  I23PWR3                       source/interfaces/inter3d1/i23pwr3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I23PWR3(ITAB,INACTI,CAND_E,CAND_N,STFN,
     1                  X     ,I_STOK,NSV   ,IWPENE,PENE,
     2                  NOINT,NTY   ,GAP_S ,MSR ,
     3                  IRECT,GAPMIN,GAPMAX ,FPENMAX,
     4                  NSN  ,ITAG  ,CAND_EN,CAND_NN,
     5                  CAND_P,STF  ,IFPEN ,IFPENN,GAPV)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),CAND_E(*),CAND_N(*), IRECT(4,*), 
     .        ITAG(*),CAND_NN(*),CAND_EN(*), IFPEN(*),
     .        IFPENN(*)
      INTEGER I_STOK,NSV(*),MSR(*),IWPENE,INACTI,NOINT,NTY,NSN,JWARN
C     REAL ou REAL*8
      my_real
     .   STFN(*),X(3,*),PENE(*),CAND_P(*),GAP_S(*),
     .   GAPMIN,GAPMAX,STF(*),GAPV(*),FPENMAX
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L
      INTEGER IX1, IX2, IX3, IX4, NSVG
C     REAL ou REAL*8
      my_real
     .   PENMAX
C-----------------------------------------------
       JWARN = 0
        DO 100 I=1,I_STOK
          J=CAND_N(I)
          L=CAND_E(I)

          IX1=IRECT(1,L)
          IX2=IRECT(2,L)
          IX3=IRECT(3,L)
          IX4=IRECT(4,L)
          NSVG=NSV(J)
          IF(PENE(I).GT.ZERO)THEN
            IF(IPRI.GE.1)THEN
              WRITE(IOUT,FMT=FMW_5I)
     1         ITAB(NSVG),
     2         ITAB(IX1),ITAB(IX2),
     3         ITAB(IX3),ITAB(IX4)
            END IF
            WRITE(IOUT,1000)PENE(I)
C
            PENMAX=FPENMAX*GAPV(I)
            IF(INACTI.EQ.1) THEN
C             DESACTIVATION DES NOEUDS
              WRITE(IOUT,'(A)')'NODE STIFFNESS IS SET TO ZERO'
              STFN(J) = ZERO
            ELSE IF(INACTI.EQ.2) THEN
C             DESACTIVATION DES ELEMENTS
              WRITE(IOUT,'(A)')'ELEMENT STIFFNESS IS SET TO ZERO'
              STF(CAND_E(I)) = ZERO
            ELSE IF(INACTI.EQ.3) THEN
C             CHANGE LES COORDONNEES DES NOEUDS SLAVE
              WRITE(IOUT,'(A)')
     .          'INACTI=3 IS NOT AVAILABLE FOR INTERFACE TYPE23'
            ELSE IF(INACTI.EQ.4) THEN
C             CHANGE LES COORDONNEES DES NOEUDS MASTER
              WRITE(IOUT,'(A)')
     .          'INACTI=4 IS NOT AVAILABLE FOOR INTERFACE TYPE23'
            ELSEIF(FPENMAX /= ZERO .AND. PENE(I) > PENMAX) THEN
C               DESACTIVATION DES NOEUDS
                WRITE(IOUT,'(A,1PG20.13,A)')
     .                ' MAX INITIAL PENETRATION ',PENMAX,' IS REACHED'
                WRITE(IOUT,'(A)')'NODE STIFFNESS IS SET TO ZERO'
                STFN(J) = ZERO
            ELSE IF(INACTI.EQ.5) THEN
C             REDUCTION DU GAP 
              JWARN = 1
              PENE(I)=PENE(I)+EM08*PENE(I)
            ELSE
C             INACTI.EQ.6
C             REDUCTION DU GAP 
              JWARN = 1
              PENE(I)=PENE(I)+ZEP05*(GAPV(I)-PENE(I))
            END IF
            CAND_P(IWPENE+1)  = PENE(I)
            CAND_NN(IWPENE+1) = CAND_N(I)
            CAND_EN(IWPENE+1) = CAND_E(I)
            IFPENN(IWPENE+1)  = IFPEN(I)
            IWPENE=IWPENE+1
          ENDIF
 100    CONTINUE
        IF (JWARN /= 0) WRITE(IOUT,'(A)')'REDUCE INITIAL GAP'
C
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13)
      RETURN
      END
