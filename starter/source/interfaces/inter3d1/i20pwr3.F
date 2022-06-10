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
Chd|  I20PWR3AE                     source/interfaces/inter3d1/i20pwr3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20PWR3AE(ITAB  ,INACTI,CAND_M,CAND_S,
     2                   STFS  ,STFM  ,XANEW ,NSV   ,IWPENE,
     3                   N1    ,N2    ,M1    ,M2    ,NX    ,
     4                   NY    ,NZ    ,GAPV  ,GAP_S ,GAP_M ,
     5                   IGAP  ,X     ,FPENMAX)
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
C     REAL
      my_real
     .   FPENMAX
      my_real
     .   STFS(*),STFM(*),XANEW(3,*),X(3,*),GAP_S(*) ,GAP_M(*),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),GAPV(*)
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
      INTEGER I, IS, IM
C     REAL
      my_real
     .     PENE(MVSIZ), 
     .     PENEOLD, S2, D, PPLUS,PS2, PENMAX
C-----------------------------------------------
C

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
            WRITE(IOUT,1000)PENE(I)
            WRITE(IOUT,FMT=FMW_I_3F)ITAB(N1(I)),
     .                                  XANEW(1,N1(I))+PENE(I)*NX(I),
     .                                  XANEW(2,N1(I))+PENE(I)*NY(I),
     .                                  XANEW(3,N1(I))+PENE(I)*NZ(I)
            WRITE(IOUT,FMT=FMW_I_3F)ITAB(N2(I)),
     .                                  XANEW(1,N2(I))+PENE(I)*NX(I),
     .                                  XANEW(2,N2(I))+PENE(I)*NY(I),
     .                                  XANEW(3,N2(I))+PENE(I)*NZ(I)
            PENE(I) = PENE(I) + EM8*PENE(I)
            PENMAX = FPENMAX*GAPV(I)
            IF (INACTI == 1 .OR. PENE(I) > PENMAX) THEN
C               DESACTIVATION DES NOEUDS
                IF (PENE(I) > PENMAX) 
     .          WRITE(IOUT,'(A,1PG20.13,A)')
     .                ' MAX INITIAL PENETRATION ',PENMAX,' IS REACHED'
                WRITE(IOUT,'(A)')' SECONDARY STIFFNESS IS SET TO ZERO'
                STFS(CAND_S(I)) = ZERO
            ELSE IF(INACTI.EQ.2) THEN
C               DESACTIVATION DES ELEMENTS
                WRITE(IOUT,'(A)')'MAIN STIFFNESS IS SET TO ZERO'
                STFM(CAND_M(I)) = ZERO
            ELSE IF(INACTI.EQ.6) THEN
C             INACTI.EQ.6
C               CHANGE LES COORDONNEES DES NOEUDS SECONDARY
                WRITE(IOUT,'(A)')'NODE COORD IS CHANGED AS PROPOSED'
                PS2 = HALF*PENE(I)
                PENEOLD = SQRT( (XANEW(1,N1(I))-X(1,N1(I)))**2 +
     .                          (XANEW(2,N1(I))-X(2,N1(I)))**2 +
     .                          (XANEW(3,N1(I))-X(3,N1(I)))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,N1(I)) = X(1,N1(I))+PS2*NX(I)
                  XANEW(2,N1(I)) = X(2,N1(I))+PS2*NY(I)
                  XANEW(3,N1(I)) = X(3,N1(I))+PS2*NZ(I)
                ENDIF
                PENEOLD = SQRT( (XANEW(1,N2(I))-X(1,N2(I)))**2 +
     .                          (XANEW(2,N2(I))-X(2,N2(I)))**2 +
     .                          (XANEW(3,N2(I))-X(3,N2(I)))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,N2(I)) = X(1,N2(I))+PS2*NX(I)
                  XANEW(2,N2(I)) = X(2,N2(I))+PS2*NY(I)
                  XANEW(3,N2(I)) = X(3,N2(I))+PS2*NZ(I)
                ENDIF
C               CHANGE LES COORDONNEES DES NOEUDS MAIN
                WRITE(IOUT,'(A)')'SEG. COORD IS CHANGED AS PROPOSED'
                PENEOLD = SQRT( (XANEW(1,M1(I))-X(1,M1(I)))**2 +
     .                          (XANEW(2,M1(I))-X(2,M1(I)))**2 +
     .                          (XANEW(3,M1(I))-X(3,M1(I)))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,M1(I)) = X(1,M1(I))-PS2*NX(I)
                  XANEW(2,M1(I)) = X(2,M1(I))-PS2*NY(I)
                  XANEW(3,M1(I)) = X(3,M1(I))-PS2*NZ(I)
                ENDIF
C
                PENEOLD = SQRT( (XANEW(1,M2(I))-X(1,M2(I)))**2 +
     .                          (XANEW(2,M2(I))-X(2,M2(I)))**2 +
     .                          (XANEW(3,M2(I))-X(3,M2(I)))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,M2(I)) = X(1,M2(I))-PS2*NX(I)
                  XANEW(2,M2(I)) = X(2,M2(I))-PS2*NY(I)
                  XANEW(3,M2(I)) = X(3,M2(I))-PS2*NZ(I)
                ENDIF

            END IF
            IWPENE=IWPENE+1
          ENDIF
 100    CONTINUE
C
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,
     . ' POSSIBLE NEW COORDINATES OF SECONDARY NODES')
      RETURN
      END
Chd|====================================================================
Chd|  I20PWR3A                      source/interfaces/inter3d1/i20pwr3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20PWR3A(ITAB,INACTI,CAND_E,CAND_N,STFN,
     1                  STF ,XANEW   ,NSV   ,IWPENE,IWRN ,
     2                  CAND_EN,CAND_NN,TAG,NOINT,GAPV ,NTY,
     3                  ITIED , FPENMAX,ID,TITR,
     4                  IX1,IX2,IX3,IX4,X1,
     5                  X2 ,X3 ,X4 ,Y1 ,Y2,
     6                  Y3 ,Y4 ,Z1 ,Z2 ,Z3,
     7                  Z4 ,XI ,YI ,ZI ,N1,
     8                  N2 ,N3 ,PENE,NSVG)
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
      INTEGER ITAB(*),CAND_E(*),CAND_N(*),CAND_EN(*),CAND_NN(*)
      INTEGER NSV(*),TAG(*),IWPENE,INACTI,NOINT,NTY,ITIED,IWRN
C     REAL
      my_real
     .   FPENMAX
      my_real
     .   STF(*),STFN(*),XANEW(3,*),GAPV(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: N1,N2,N3,PENE
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XI,YI,ZI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IS,IM
C     REAL
      my_real ::PENEOLD,PPLUS,PS2,PENMAX
      my_real
     .  DN
C-----------------------------------------------
!      EQUIVALENCE (IX1,IV(1,1)) ,(IX2,IV(1,2)) ,(IX3,IV(1,3))
!      EQUIVALENCE (IX4,IV(1,4)) ,(NSVG,IV(1,5))
C
!      EQUIVALENCE (XI,BV(1,13)),(YI,BV(1,14)),(ZI,BV(1,15))
!      EQUIVALENCE (X1,BV(1,1)) ,(Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (X2,BV(1,4)) ,(Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (X3,BV(1,7)) ,(Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (X4,BV(1,10)),(Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (N1,BV(1,19)) ,(N2,BV(1,20)) ,(N3,BV(1,21))
!      EQUIVALENCE (PENE,BV(1,22))
C=======================================================================
        DO 100 I=LFT,LLT
          IF(IPRI.GE.1.AND.PENE(I).GT.ZERO)THEN
            WRITE(IOUT,FMT=FMW_5I)
     1       ITAB(NSVG(I)),
     2       ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))
          ELSEIF(IPRI.GE.6)THEN
            WRITE(IOUT,FMT=FMW_5I)
     1       ITAB(NSVG(I)),
     2       ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))
          ENDIF
          PENMAX = FPENMAX*GAPV(I)
C
          IF(PENE(I).GT.ZERO)THEN
            TAG(NSVG(I))=TAG(NSVG(I))+1
            DN=N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I)
            IF(DN.LE.EM30) THEN
               WRITE(IOUT,1100)PENE(I),ITAB(NSVG(I))
               IF(NTY.NE.10.OR.ITIED.EQ.0)THEN
                 IF (INACTI.NE.1.AND.INACTI.NE.2.AND.
     .               PENE(I).LE.PENMAX) THEN

                   IF (INACTI.EQ.0) THEN
                     CALL ANCMSG(MSGID=612,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO_BLIND_1,
     .                           I1=ID,
     .                           C1=TITR,
     .                           I2=INACTI,
     .                           I3=ITAB(NSVG(I)))
                  ELSE
                     CALL ANCMSG(MSGID=611,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO_BLIND_1,
     .                           I1=ID,
     .                           C1=TITR,
     .                           I2=INACTI,
     .                           I3=ITAB(NSVG(I)))
                  ENDIF
                 ENDIF
               END IF
            ELSEIF (PENE(I) .LE. PENMAX) THEN
              WRITE(IOUT,1000)PENE(I)
              PENE(I) = PENE(I) + EM8*PENE(I)
              WRITE(IOUT,FMT=FMW_I_3F)ITAB(NSVG(I)),
     .                                  XI(I)+PENE(I)*N1(I),
     .                                  YI(I)+PENE(I)*N2(I),
     .                                  ZI(I)+PENE(I)*N3(I)
            ENDIF

            IF (INACTI == 1 .OR. PENE(I) > PENMAX) THEN
C               DESACTIVATION DES NOEUDS
                IF (PENE(I) > PENMAX) 
     .          WRITE(IOUT,'(A,I8,A,1PG20.13,A)')' NODE ',ITAB(NSVG(I)),
     .                ' : MAX INITIAL PENETRATION ',PENMAX,' IS REACHED'
                WRITE(IOUT,'(A)')' SECONDARY STIFFNESS IS SET TO ZERO'
                STFN(CAND_N(I)) = ZERO
            ELSE IF(INACTI.EQ.2) THEN
C               DESACTIVATION DES ELEMENTS
                WRITE(IOUT,'(A)')'ELEMENT STIFFNESS IS SET TO ZERO'
                STF(CAND_E(I)) = ZERO
            ELSE IF(INACTI.EQ.3) THEN
C               CHANGE LES COORDONNEES DES NOEUDS SECONDARY
                WRITE(IOUT,'(A)')'NODE COORD IS CHANGED AS PROPOSED'
                PENEOLD = SQRT( (XANEW(1,NSV(CAND_N(I)))-XI(I))**2 +
     .                          (XANEW(2,NSV(CAND_N(I)))-YI(I))**2 +
     .                          (XANEW(3,NSV(CAND_N(I)))-ZI(I))**2 )
                IF(PENE(I).GT.PENEOLD) THEN
                  XANEW(1,NSV(CAND_N(I))) = XI(I)+PENE(I)*N1(I)
                  XANEW(2,NSV(CAND_N(I))) = YI(I)+PENE(I)*N2(I)
                  XANEW(3,NSV(CAND_N(I))) = ZI(I)+PENE(I)*N3(I)
                ENDIF
            ELSE IF(INACTI.EQ.4) THEN
C               CHANGE LES COORDONNEES DES NOEUDS MAIN
                WRITE(IOUT,'(A)')'SEG. COORD IS CHANGED AS PROPOSED'
                PENEOLD = SQRT( (XANEW(1,IX1(I))-X1(I))**2 +
     .                          (XANEW(2,IX1(I))-Y1(I))**2 +
     .                          (XANEW(3,IX1(I))-Z1(I))**2 )
                IF(PENE(I).GT.PENEOLD) THEN
                  XANEW(1,IX1(I)) = X1(I)-PENE(I)*N1(I)
                  XANEW(2,IX1(I)) = Y1(I)-PENE(I)*N2(I)
                  XANEW(3,IX1(I)) = Z1(I)-PENE(I)*N3(I)
                ENDIF
C
                PENEOLD = SQRT( (XANEW(1,IX2(I))-X2(I))**2 +
     .                          (XANEW(2,IX2(I))-Y2(I))**2 +
     .                          (XANEW(3,IX2(I))-Z2(I))**2 )
                IF(PENE(I).GT.PENEOLD) THEN
                  XANEW(1,IX2(I)) = X2(I)-PENE(I)*N1(I)
                  XANEW(2,IX2(I)) = Y2(I)-PENE(I)*N2(I)
                  XANEW(3,IX2(I)) = Z2(I)-PENE(I)*N3(I)
                ENDIF
C
                PENEOLD = SQRT( (XANEW(1,IX3(I))-X3(I))**2 +
     .                          (XANEW(2,IX3(I))-Y3(I))**2 +
     .                          (XANEW(3,IX3(I))-Z3(I))**2 )
                IF(PENE(I).GT.PENEOLD) THEN
                  XANEW(1,IX3(I)) = X3(I)-PENE(I)*N1(I)
                  XANEW(2,IX3(I)) = Y3(I)-PENE(I)*N2(I)
                  XANEW(3,IX3(I)) = Z3(I)-PENE(I)*N3(I)
                ENDIF
C
                PENEOLD = SQRT( (XANEW(1,IX4(I))-X4(I))**2 +
     .                          (XANEW(2,IX4(I))-Y4(I))**2 +
     .                          (XANEW(3,IX4(I))-Z4(I))**2 )
                IF(PENE(I).GT.PENEOLD) THEN
                  XANEW(1,IX4(I)) = X4(I)-PENE(I)*N1(I)
                  XANEW(2,IX4(I)) = Y4(I)-PENE(I)*N2(I)
                  XANEW(3,IX4(I)) = Z4(I)-PENE(I)*N3(I)
                ENDIF
            ELSE IF(INACTI == 6) THEN
C               INACTI == 6
C               CHANGE LES COORDONNEES DES NOEUDS SECONDARY
                WRITE(IOUT,'(A)')'NODE COORD IS CHANGED AS PROPOSED'
                PS2 = HALF*PENE(I)
                PENEOLD = SQRT( (XANEW(1,NSV(CAND_N(I)))-XI(I))**2 +
     .                          (XANEW(2,NSV(CAND_N(I)))-YI(I))**2 +
     .                          (XANEW(3,NSV(CAND_N(I)))-ZI(I))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,NSV(CAND_N(I))) = XI(I)+PS2*N1(I)
                  XANEW(2,NSV(CAND_N(I))) = YI(I)+PS2*N2(I)
                  XANEW(3,NSV(CAND_N(I))) = ZI(I)+PS2*N3(I)
                ENDIF
C               CHANGE LES COORDONNEES DES NOEUDS MAIN
                WRITE(IOUT,'(A)')'SEG. COORD IS CHANGED AS PROPOSED'
                PENEOLD = SQRT( (XANEW(1,IX1(I))-X1(I))**2 +
     .                          (XANEW(2,IX1(I))-Y1(I))**2 +
     .                          (XANEW(3,IX1(I))-Z1(I))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,IX1(I)) = X1(I)-PS2*N1(I)
                  XANEW(2,IX1(I)) = Y1(I)-PS2*N2(I)
                  XANEW(3,IX1(I)) = Z1(I)-PS2*N3(I)
                ENDIF
C
                PENEOLD = SQRT( (XANEW(1,IX2(I))-X2(I))**2 +
     .                          (XANEW(2,IX2(I))-Y2(I))**2 +
     .                          (XANEW(3,IX2(I))-Z2(I))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,IX2(I)) = X2(I)-PS2*N1(I)
                  XANEW(2,IX2(I)) = Y2(I)-PS2*N2(I)
                  XANEW(3,IX2(I)) = Z2(I)-PS2*N3(I)
                ENDIF
C
                PENEOLD = SQRT( (XANEW(1,IX3(I))-X3(I))**2 +
     .                          (XANEW(2,IX3(I))-Y3(I))**2 +
     .                          (XANEW(3,IX3(I))-Z3(I))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,IX3(I)) = X3(I)-PS2*N1(I)
                  XANEW(2,IX3(I)) = Y3(I)-PS2*N2(I)
                  XANEW(3,IX3(I)) = Z3(I)-PS2*N3(I)
                ENDIF
C
                PENEOLD = SQRT( (XANEW(1,IX4(I))-X4(I))**2 +
     .                          (XANEW(2,IX4(I))-Y4(I))**2 +
     .                          (XANEW(3,IX4(I))-Z4(I))**2 )
                IF(PS2.GT.PENEOLD) THEN
                  XANEW(1,IX4(I)) = X4(I)-PS2*N1(I)
                  XANEW(2,IX4(I)) = Y4(I)-PS2*N2(I)
                  XANEW(3,IX4(I)) = Z4(I)-PS2*N3(I)
                ENDIF
            END IF

            IWPENE=IWPENE+1
          ENDIF
 100    CONTINUE

        IF(IWPENE /= 0 .and.INACTI == 3 .or.INACTI == 4) IWRN = 1
C
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,
     . ' POSSIBLE NEW COORDINATES OF SECONDARY NODE')
 1100 FORMAT(2X,'** INITIAL PENETRATION =',E14.7,
     . ' IMPOSSIBLE TO CALCULATE NEW COORDINATES OF SECONDARY NODE',I8)
      RETURN
      END
Chd|====================================================================
Chd|  I20PWR3                       source/interfaces/inter3d1/i20pwr3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20PWR3(ITAB,INACTI,CAND_E,CAND_N,STFN ,
     1                   STF ,X     ,NSV   ,IWPENE,IWRN ,
     2                   CAND_EN,CAND_NN,TAG,NOINT,GAPV ,
     3                   NTY ,ITIED ,PENIS ,PENIM ,GAP_S,
     4                   IGAP,ID ,TITR,IX1,IX2,
     5                   IX3 ,IX4,N1  ,N2 ,N3 ,
     6                   PENE,NSVG)
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
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),CAND_E(*),CAND_N(*),CAND_EN(*),CAND_NN(*)
      INTEGER NSV(*),TAG(*),IWPENE,INACTI,NOINT,NTY,ITIED,IWRN,
     .        IGAP
C     REAL
      my_real
     .   STF(*),STFN(*),X(3,*),GAPV(*),PENIS(2,*) ,PENIM(2,*),GAP_S(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: N1,N2,N3,PENE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IS,IM,JWARN
C     REAL
      my_real
     .   PENEOLD,PPLUS,AAA
      my_real
     .  DN
C-----------------------------------------------
!      EQUIVALENCE (IX1,IV(1,1)) ,(IX2,IV(1,2)) ,(IX3,IV(1,3))
!      EQUIVALENCE (IX4,IV(1,4)) ,(NSVG,IV(1,5))
C
!      EQUIVALENCE (XI,BV(1,13)),(YI,BV(1,14)),(ZI,BV(1,15))
!      EQUIVALENCE (X1,BV(1,1)) ,(Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (X2,BV(1,4)) ,(Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (X3,BV(1,7)) ,(Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (X4,BV(1,10)),(Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (N1,BV(1,19)) ,(N2,BV(1,20)) ,(N3,BV(1,21))
!      EQUIVALENCE (PENE,BV(1,22))
C=======================================================================
      JWARN = 0
        DO 100 I=LFT,LLT
          IS=CAND_N(I)      
          IM=CAND_E(I)     
          IF(STFN(IS)==ZERO) CYCLE
C
          IF(IPRI.GE.1.AND.PENE(I).GT.ZERO)THEN
            WRITE(IOUT,FMT=FMW_5I)
     1       ITAB(NSVG(I)),
     2       ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))
          ELSEIF(IPRI.GE.6)THEN
            WRITE(IOUT,FMT=FMW_5I)
     1       ITAB(NSVG(I)),
     2       ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))
          ENDIF
          IF(PENE(I).GT.ZERO)THEN
            TAG(NSVG(I))=TAG(NSVG(I))+1
            DN=N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I)
            IF(DN.LE.EM30) THEN
               WRITE(IOUT,1100)PENE(I),ITAB(NSVG(I))
               IF(NTY.NE.10.OR.ITIED.EQ.0)THEN
                 IF(INACTI.NE.1.AND.INACTI.NE.2) THEN

                   IF (INACTI.EQ.0) THEN
                     CALL ANCMSG(MSGID=612,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO_BLIND_1,
     .                           I1=ID,
     .                           C1=TITR,
     .                           I2=INACTI,
     .                           I3=ITAB(NSVG(I)))
                  ELSE
                     CALL ANCMSG(MSGID=611,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO_BLIND_1,
     .                           I1=ID,
     .                           C1=TITR,
     .                           I2=INACTI,
     .                           I3=ITAB(NSVG(I)))
                  ENDIF
                 ENDIF
               END IF

            ELSE
              PENE(I) = PENE(I) + EM8*PENE(I)
            ENDIF

            IF(INACTI == 5.or.INACTI == 6) THEN
C             INACTI == 6
              IF(PENE(I) >= GAPV(I)*ZEP995)THEN
                WRITE(IOUT,'(A)')' *** PENETRATION > GAP - 0.5% !! '
                WRITE(IOUT,'(A)')'SECONDARY STIFFNESS IS SET TO ZERO'
                PENE(I)=GAPV(I)
                STFN(IS) = ZERO
              ELSE
                JWARN = 1
                PPLUS=(PENE(I)+ZEP05*(GAPV(I)-PENE(I)))
                IF(IGAP > 0) THEN
C---
                  IF (PPLUS < GAP_S(IS)) THEN
                    PENIS(2,IS)=MAX(PENIS(2,IS),PPLUS)
                  ELSE
                    PENIS(2,IS)=MAX(PENIS(2,IS),GAP_S(IS))
                    PENIM(2,IM)=MAX(PENIM(2,IM),PPLUS-GAP_S(IS))
                  ENDIF
                ELSE
                  PENIM(2,IM)=MAX(PENIM(2,IM),PPLUS)
                END IF
C---
c                AAA = GAP_S(IS)/GAPV(I)
c                PENIS(2,IS)=MAX(PENIS(2,IS),AAA*PPLUS)
c                PENIM(2,IM)=MAX(PENIM(2,IM),(ONE-AAA)*PPLUS)
C---
                PENIS(1,IS)=PENIS(2,IS)
                PENIM(1,IM)=PENIM(2,IM)
              ENDIF

c              CAND_P(IWPENE+1) = PENE(I)
              CAND_NN(IWPENE+1) = CAND_N(I)
              CAND_EN(IWPENE+1) = CAND_E(I)
            END IF

            IWPENE=IWPENE+1
          ENDIF
 100    CONTINUE

        IF(IWPENE /= 0.and.INACTI == 3.or.INACTI == 4) IWRN = 1
C
 1100 FORMAT(2X,'** INITIAL PENETRATION =',E14.7,
     . ' IMPOSSIBLE TO CALCULATE NEW COORDINATES OF SECONDARY NODE',I8)
      RETURN
      END
Chd|====================================================================
Chd|  I20PWR3E                      source/interfaces/inter3d1/i20pwr3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20PWR3E(ITAB  ,INACTI,CAND_M,CAND_S,
     2                   STFS  ,STFM  ,X     ,NSV   ,IWPENE,
     3                   N1    ,N2    ,M1    ,M2    ,NX    ,
     4                   NY    ,NZ    ,GAPV  ,GAP_S ,GAP_M ,
     5                   PENIS ,PENIM ,IGAP  )
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
          IF(STFS(CAND_S(I))==ZERO) CYCLE
          IF(IPRI.GE.1)THEN
            WRITE(IOUT,FMT=FMW_4I)
     2       ITAB(N1(I)),ITAB(N2(I)),ITAB(M1(I)),ITAB(M2(I))
          ENDIF
          IF(PENE(I).GT.ZERO)THEN
            WRITE(IOUT,1000)PENE(I)
            WRITE(IOUT,FMT=FMW_I_3F)ITAB(N1(I)),
     .                                  X(1,N1(I))+PENE(I)*NX(I),
     .                                  X(2,N1(I))+PENE(I)*NY(I),
     .                                  X(3,N1(I))+PENE(I)*NZ(I)
            WRITE(IOUT,FMT=FMW_I_3F)ITAB(N2(I)),
     .                                  X(1,N2(I))+PENE(I)*NX(I),
     .                                  X(2,N2(I))+PENE(I)*NY(I),
     .                                  X(3,N2(I))+PENE(I)*NZ(I)
            PENE(I) = PENE(I) + EM8*PENE(I)
            IF(INACTI == 5.or.INACTI == 6) THEN
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
            IWPENE=IWPENE+1
          ENDIF
 100    CONTINUE
        IF (JWARN /= 0) WRITE(IOUT,'(A)')'REDUCE INITIAL GAP'
C
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,
     . ' POSSIBLE NEW COORDINATES OF SECONDARY NODES')
      RETURN
      END
