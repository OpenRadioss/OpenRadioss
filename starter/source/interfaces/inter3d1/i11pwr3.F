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
Chd|  I11PWR3                       source/interfaces/inter3d1/i11pwr3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11PWR3(ITAB  ,INACTI,CAND_M,CAND_S,
     2                   STFS  ,STFM  ,X     ,NSV   ,IWPENE,
     3                   N1    ,N2    ,M1    ,M2    ,NX    ,
     4                   NY    ,NZ    ,GAPV  ,GAP_S ,GAP_M ,
     5                   PENIS ,PENIM ,IGAP  ,PRINT_WARNING)
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
      INTEGER ITAB(*),CAND_M(*),CAND_S(*),INACTI,IGAP  ,
     4        N1(*) ,N2(*) ,M1(*) ,M2(*)        
      INTEGER NSV(*),IWPENE
      LOGICAL PRINT_WARNING
C     REAL
      my_real
     .   STFS(*),STFM(*),X(3,*),GAP_S(*) ,GAP_M(*),PENIS(2,*) , 
     .   PENIM(2,*),NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),GAPV(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IS, IM,JWARN
C     REAL
      my_real
     .     PENE(MVSIZ), 
     .     PENEOLD, S2, D, PPLUS
C-----------------------------------------------
C
      JWARN = 0
      DO I=1,LLT
         S2 = SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2)
         GAPV(I) = SQRT(GAPV(I))
         PENE(I) = GAPV(I) - S2
         S2 = 1./MAX(EM30,S2)
         NX(I) = NX(I)*S2
         NY(I) = NY(I)*S2
         NZ(I) = NZ(I)*S2
      ENDDO
C
        DO 100 I=LFT,LLT
          IF(IPRI.GE.1)THEN
            WRITE(IOUT,FMT=FMW_4I)
     2       ITAB(N1(I)),ITAB(N2(I)),ITAB(M1(I)),ITAB(M2(I))
          ENDIF
          IF(PENE(I).GT.ZERO)THEN
           IF(IPRI>=5)THEN
            WRITE(IOUT,1000)PENE(I)
            WRITE(IOUT,FMT=FMW_I_3F)ITAB(N1(I)),
     .                                  X(1,N1(I))+PENE(I)*NX(I),
     .                                  X(2,N1(I))+PENE(I)*NY(I),
     .                                  X(3,N1(I))+PENE(I)*NZ(I)
            WRITE(IOUT,FMT=FMW_I_3F)ITAB(N2(I)),
     .                                  X(1,N2(I))+PENE(I)*NX(I),
     .                                  X(2,N2(I))+PENE(I)*NY(I),
     .                                  X(3,N2(I))+PENE(I)*NZ(I)
           ENDIF
C
            IF(INACTI.NE.6)THEN
              PENE(I) = PENE(I) + EM8*PENE(I)
              IF(INACTI.EQ.1) THEN
C               DESACTIVATION DES NOEUDS
                WRITE(IOUT,'(A)')'SECONDARY STIFFNESS IS SET TO ZERO'
                STFS(CAND_S(I)) = ZERO
              ELSE IF(INACTI.EQ.2) THEN
C               DESACTIVATION DES ELEMENTS
                WRITE(IOUT,'(A)')'MAIN STIFFNESS IS SET TO ZERO'
                STFM(CAND_M(I)) = ZERO
              ELSE IF(INACTI.EQ.5) THEN
C               REDUCTION DU GAP 
                IF(PENE(I).GE.GAPV(I)*ZEP995)THEN
                  WRITE(IOUT,'(A)')' *** PENETRATION > GAP - 0.5% !! '
                  WRITE(IOUT,'(A)')'SECONDARY STIFFNESS IS SET TO ZERO'
                  PENE(I)=GAPV(I)
                  STFS(CAND_S(I)) =ZERO
                ELSE
                  JWARN = 1
                  IS=CAND_S(I)
                  IM=CAND_M(I)
                   PENIS(2,IS)=MAX(PENIS(2,IS),HALF*PENE(I))
                   PENIM(2,IM)=MAX(PENIM(2,IM),HALF*PENE(I))
                  PENIS(1,IS)=PENIS(2,IS)
                  PENIM(1,IM)=PENIM(2,IM)
                ENDIF
              ENDIF
C
            ELSE
C             INACTI.EQ.6
              IF(PENE(I).GE.GAPV(I)*ZEP995)THEN
                WRITE(IOUT,'(A)')' *** PENETRATION > GAP - 0.5% !! '
                WRITE(IOUT,'(A)')'SECONDARY STIFFNESS IS SET TO ZERO'
                PENE(I)=GAPV(I)
                STFS(CAND_S(I)) = ZERO
              ELSE
                JWARN = 1
                IS=CAND_S(I)
                IM=CAND_M(I)
                PPLUS=HALF*(PENE(I)+ZEP05*(GAPV(I)-PENE(I)))
                PENIS(2,IS)=MAX(PENIS(2,IS),PPLUS)
                PENIM(2,IM)=MAX(PENIM(2,IM),PPLUS)
                PENIS(1,IS)=PENIS(2,IS)
                PENIM(1,IM)=PENIM(2,IM)
              ENDIF
            END IF
C
            IWPENE=IWPENE+1
          ENDIF
 100    CONTINUE
        IF (JWARN==1 .AND. PRINT_WARNING ) THEN
           WRITE(IOUT,'(A)')'REDUCE INITIAL GAP'
           PRINT_WARNING = .FALSE.
        ENDIF
C
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,
     . ' POSSIBLE NEW COORDINATES OF SECONDARY NODES')
      RETURN
      END
