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
Chd|  I25PWR3                       source/interfaces/inter3d1/i25pwr3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25PWR3(ITAB  ,INACTI,CAND_E,CAND_N ,STFN     ,
     1                   X     ,I_STOK,NSV   ,IWPENE ,PENE_OLD,
     2                   NOINT ,NTY   ,MSR   ,IRTLM  ,IRECT    ,
     4                   NSN   ,ID    ,TITR   ,MSEGLO)
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
      INTEGER ITAB(*),CAND_E(*),CAND_N(*), IRECT(4,*), IRTLM(4,*),
     .        MSEGLO(*)
      INTEGER I_STOK,NSV(*),MSR(*),IWPENE,INACTI,NOINT,NTY,NSN
C     REAL ou REAL*8
      my_real
     .   STFN(*),X(3,*),PENE_OLD(5,*)
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
       DO 100 I=1,I_STOK
         J=CAND_N(I)
         L=CAND_E(I)


         IF(IRTLM(1,J)==MSEGLO(CAND_E(I)))THEN

           IX1=IRECT(1,L)
           IX2=IRECT(2,L)
           IX3=IRECT(3,L)
           IX4=IRECT(4,L)
           NSVG=NSV(J)

C
           IF(PENE_OLD(5,J)/=ZERO)THEN
C            True initial penetration
             IWPENE=IWPENE+1

             IF(IPRI.GE.5 )
     .           CALL ANCMSG(MSGID=1164,
     .                           MSGTYPE=MSGWARNING,
     .                           ANMODE=ANINFO_BLIND_1,
     .                           I1=ITAB(NSVG),
     .                           I2=ITAB(IX1),
     .                           I3=ITAB(IX2),
     .                           I4=ITAB(IX3),
     .                           I5=ITAB(IX4),
     .                           R1=PENE_OLD(5,J),
     .                           PRMOD=MSG_CUMU)
             IF(INACTI.EQ.0)THEN
C              Ignore initial penetrations
               IRTLM(1,J)   = 0
               IRTLM(2,J)   = 0
               IRTLM(3,J)   = 0
               PENE_OLD(5,J)= ZERO
             ELSEIF(INACTI.EQ.1) THEN
C              DESACTIVATION DES NOEUDS
               WRITE(IOUT,'(A)')'NODE STIFFNESS IS SET TO ZERO'
               STFN(J) = ZERO
C            ELSE IF(INACTI.EQ.2) THEN
C              DESACTIVATION DES ELEMENTS
C              WRITE(IOUT,'(A)')
C    .           'INACTI=2 IS NOT AVAILABLE FOR INTERFACE TYPE25'
C            ELSE IF(INACTI.EQ.3) THEN
C              CHANGE LES COORDONNEES DES NOEUDS SECND
C              WRITE(IOUT,'(A)')
C    .           'INACTI=3 IS NOT AVAILABLE FOR INTERFACE TYPE25'
C            ELSE IF(INACTI.EQ.4) THEN
C              CHANGE LES COORDONNEES DES NOEUDS MAIN
C              WRITE(IOUT,'(A)')
C    .           'INACTI=4 IS NOT AVAILABLE FOOR INTERFACE TYPE25'
             ELSE IF(INACTI.EQ.5) THEN
C              REDUCTION DU GAP
             ELSE IF(INACTI.EQ.-1) THEN
C              Initial penetrations <=> Initial forces
               PENE_OLD(5,J)= ZERO
             ENDIF
           ELSE
C            Reset (Same tracking will be done again in Engine)
             IRTLM(1,J)=0
             IRTLM(2,J)=0
             IRTLM(3,J)=0
           END IF
         END IF
 100  CONTINUE
C
      RETURN
      END
Chd|====================================================================
Chd|  I25CAND                       source/interfaces/inter3d1/i25pwr3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25CAND(CAND_E,CAND_N,NSN  ,IRTLM ,II_STOK ,
     *                   NRTM  ,MSEGTYP)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
      INTEGER CAND_E(*),CAND_N(*),NSN,IRTLM(4,*),II_STOK,
     *                   NRTM,MSEGTYP(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER E, I,ISH
     .        
C     REAL ou REAL*8
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
            II_STOK = 0
            DO I=1,NSN
             E = IRTLM(1,I)
             IF (E > 0) THEN
              II_STOK =II_STOK + 1
              CAND_N(II_STOK) = I
              CAND_E(II_STOK) = E

              ISH = MSEGTYP(E)
C
Cf i25pen3.F <=> (ABS(ISH) /= 0 .AND. ABS(ISH) <= NRTM) .OR. ISH > NRTM
              IF (ISH /= 0)THEN
C
C                coating shells and their opposite segment ::
                 IF(ISH > NRTM)ISH=ISH-NRTM
C
                 II_STOK =II_STOK + 1
                 CAND_N(II_STOK) = I
                 CAND_E(II_STOK) = ABS(ISH)
              ENDIF

             END IF
            END DO          
C      
      RETURN
      END
