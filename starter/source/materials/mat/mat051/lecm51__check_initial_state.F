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
Chd|  LECM51__CHECK_INITIAL_STATE   source/materials/mat/mat051/lecm51__check_initial_state.F
Chd|-- called by -----------
Chd|        HM_READ_MAT51                 source/materials/mat/mat051/hm_read_mat51.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECM51__CHECK_INITIAL_STATE(   
     .   AV,  R0,     C0,   C1,   C2, C3, C4,   C5,
     .   E0,  PM,   RHO0, RHOR, IEXP, JTHE,PEXT,IFLG,
     .   A0,  A1,     A2,  PLA,
     .   ID,  TITR,
     .   SSP1,SSP2, SSP3, SSP4,
     .   LC1 ,LC2 ,  LC3,  LC4,
     .   TCARP,
     .   P0a          )
      USE MESSAGE_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   Check initial state of the different material
C
C   1 INIVOL FRAC in [0 1]
C   2 AV1+AV2+AV3+AV4 = 1  (tol 0.1%)
C   3 Negative or null density
C   5 Unbalanced initial Pressure
C   4 Global Density not consistent with material 
C                                       densities
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real   
     . AV(4),R0(4), C0(4), C1(4),C2(4), C3(4),
     . A0(4), A1(4),A2(4),
     . C4(4),C5(4), E0(4), PM(4), PEXT,  RHO0, RHOR,
     . SSP(4),LC(4),
     . SSP1,SSP2,SSP3,SSP4,
     . LC1,LC2,LC3,LC4,
     . TCARP,
     . P0a(4)
      INTEGER IEXP, JTHE, IFLG,PLA(4)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER i,j, TEST1,TEST2,TEST3,TEST4,TEST5,
     .        TEST6,TEST7,TEST8,TEST9, IMAX,IPLA(4)
     
      my_real SUM, P0b, err
      
      character*8 chain
      character*47 chain3
      character*63 chain1
C===============================================|
      IPLA(1)=PLA(1)
      IPLA(2)=PLA(2)
      IPLA(3)=PLA(3)
      IPLA(4)=PLA(4) 

      IMAX=IEXP+3
      !IMAX number of phases to be checked 3 or 4
      
      SSP(1:4) = (/SSP1,SSP2,SSP3,SSP4/)
      LC (1:4) = (/ LC1, LC2, LC3, LC4/)      
      
      !0: warning or error not detected
      !1: test failed, message in listing file.
      TEST1=0    !AV in [0,1]
      TEST2=0    !Sum(AV(i))=1
      TEST3=0    !Rho not defined
      TEST4=0    !Unbalanced Pressure  t=0              
      TEST5=0    !Global density = mean(rho_i)
      TEST6=0    !C1,C4,C5 >=0,  C1>0 phase 4
      TEST8=0    !Perfect Gas cavitation pressure
      TEST8=0    !
      TEST9=0    !

      IF(IFLG.EQ.6)THEN
        SUM=0
        DO I=1,IMAX
          SUM=SUM+AV(I)*R0(I)
        END DO
        IF(SUM.NE.ZERO)THEN
          RHO0=SUM
          RHOR=RHO0
        ELSE
          RHO0=UN !will be computed in engine, not need for sound speed. This formulation has SSP=EM20
          RHOR=UN
        ENDIF        
        RETURN
      ENDIF
      !TEST1 (quel que soit iflg)
      ! Initial Volume fraction checked : must be between 0 and 1
      ! Otherwise : error
      DO I=1,IMAX
        IF ((AV(I).LT.ZERO).OR.(AV(I).GT.UN)) THEN
         chain='SUBMAT-0'
         write(chain(8:8),'(i1.1)')I
         chain1='INITIAL VOLUMETRIC FRACTION MUST BE BETWEEN 0 AND 1 FOR '//chain
         CALL ANCMSG(MSGID=99,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2=chain1)
         TEST1=1
         !exit
        END IF
      END DO


      !TEST2 (quel que soit iflg)
      ! Initial Volume fraction : Sum must be 1
      ! Otherwise : warning
      ! if TEST1/=0 it is useless to do this test)
      SUM=ZERO
      IF (TEST1.EQ.0) THEN
        DO I=1,IMAX
          SUM=SUM+AV(I)  ! AV(I) in [0,1], SUM=0 <=> AV(I)=0 , i=1..4
        END DO
        IF(SUM.EQ.ZERO)THEN
         AV(1)=1 
         CALL ANCMSG(MSGID=98,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .   C2=
     .   'INITIAL VOLUMETRIC FRACTIONS ARE NULL , SUBMAT-1 SET TO 100%')
        ELSEIF (abs(1-SUM).GT.EM03) THEN
         CALL ANCMSG(MSGID=99,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .   C2=
     .   'SUM OF INITIAL VOLUMETRIC FRACTION IS NOT EQUAL TO 1')
         TEST2=1
        END IF
      END IF


      !TEST3(quel que soit iflg)
      !Negative Density or Missing density material
      ! No negative density
      ! If AV>0 THEN R0 must be >0
      DO I=1,IMAX
        IF (   ((R0(I).LT.ZERO).OR.((AV(I).GT.ZERO).AND.(R0(I).LE.ZERO)))  ) THEN
         chain='SUBMAT-0'
         write(chain(8:8),'(i1.1)')I
         !print *,"chain(1:8) =", chain(1:8)
         chain3='NULL OR NEGATIVE INITIAL DENSITIES FOR '//chain
         CALL ANCMSG(MSGID=99,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2=chain3)
         TEST3=1
         !exit
        END IF
      END DO
   
      
      !TEST4 (quel que soit iflg)
      !Unbalanced Pressure, material are checked two by two (Cnp couples)
      !IF (TEST1+TEST2+TEST3.EQ.0) THEN !If no error
        SUM=ZERO
        IF(IMAX==4)P0a(4) = C0(4)
        DO I=1,IMAX-1
        if (R0(I).EQ.ZERO)CYCLE !si phase vide eventuelle, autorisee
        P0a(I)=C0(I)+C4(I)*E0(I)
          DO J=I,IMAX
          IF (R0(J).EQ.ZERO)cycle !si phase vide eventuelle, autorisee
          !On compare Pi(t=0) avec Pj(t=0)
            P0b=C0(J)+C4(J)*E0(J)
            Err=abs(P0a(I)-P0b)
          IF (Err.LE.EM20) Err=ZERO  !eviter epsilon machine si P0a ou P0b est nul
          IF (Err.NE.ZERO) THEN  ! JWL under pressure may detonate.
            IF (Err/max(abs(P0a(I)),abs(P0b))>EM06) THEN
                IF (J.EQ.4) THEN
                  TEST4=1
                  CALL ANCMSG(MSGID=98,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID,
     .                        C1=TITR,
     .   C2=
     .            'PRESSURES ARE UNBALANCED WITH JWL MATERIAL')
                  exit
              ELSE
                  TEST4=1
                  CALL ANCMSG(MSGID=98,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID,
     .                        C1=TITR,
     .   C2=
     .            'INITIAL PRESSURES ARE UNBALANCED')
                EXIT
            END IF
            END IF
            END IF
          END DO !next J
          IF(TEST4.EQ.1)EXIT
        END DO !next I
      !END IF
      
      
      !TEST5 (quel que soit iflg)
      !Check the consistency between global density and materials densities.
      !IF(TEST1+TEST2+TEST3.EQ.0)THEN !If no error
        SUM=0
        DO I=1,IMAX
          SUM=SUM+AV(I)*R0(I)
        END DO
        TEST5=1
        RHO0=SUM
        RHOR=RHO0
      !END IF


      !TEST6 (quel que soit iflg)
      ! Check if C1>=0, C4>=0, C5>=0
      ! Otherwise : error
      DO I=1,3
        IF ((C1(I).LT.ZERO)) THEN
         chain='SUBMAT-0'
         write(chain(8:8),'(i1.1)')I
         chain1=
     . 'POLYNOMIAL COEFFICIENT C1 MUST BE POSITIVE OR NULL FOR '//chain
         CALL ANCMSG(MSGID=99,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2=chain1)
         TEST6=1
         !exit
        END IF

        IF ((C2(I).LT.ZERO)) THEN
         chain='SUBMAT-0'
         write(chain(8:8),'(i1.1)')I
         chain1=
     . 'POLYNOMIAL COEFFICIENT C2 IS NEGATIVE FOR '//chain
         CALL ANCMSG(MSGID=98,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               C2=chain1)
         TEST6=1
         !exit
        END IF
        
        IF ((C3(I).LT.ZERO)) THEN
         chain='SUBMAT-0'
         write(chain(8:8),'(i1.1)')I
         chain1=
     . 'POLYNOMIAL COEFFICIENT C3 IS NEGATIVE FOR '//chain
         CALL ANCMSG(MSGID=98,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               C2=chain1)
         TEST6=1
         !exit
        END IF        

        IF ((C4(I).LT.ZERO)) THEN
         chain='SUBMAT-0'
         write(chain(8:8),'(i1.1)')I
         chain1=
     . 'POLYNOMIAL COEFFICIENT C4 MUST BE POSITIVE OR NULL FOR '//chain
         CALL ANCMSG(MSGID=99,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               C2=chain1)
         TEST6=1
         !exit
        END IF        

        IF ((C5(I).LT.ZERO)) THEN
         chain='SUBMAT-0'
         write(chain(8:8),'(i1.1)')I
         chain1=
     . 'POLYNOMIAL COEFFICIENT C5 MUST BE POSITIVE OR NULL FOR '//chain
         CALL ANCMSG(MSGID=99,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               C2=chain1)
         TEST6=1
         !exit
        END IF                        
      END DO !next I
      
          
      !check pressure cut-off pressure for perfect gas
      IF(IFLG.LE.1)THEN
        DO I=1,3
        IF(IPLA(I)>0)CYCLE
        P0a(I) = PEXT+C0(I)+C4(I)*E0(I)
          IF(((PEXT+C0(I)).EQ.C1(I)).AND.(C2(I).EQ.ZERO).AND.
     .    (C3(I).EQ.ZERO).AND.(C4(I).EQ.C5(I)).AND.
     .    (C4(I).GT.ZERO).AND.(P0a(I).GT.ZERO))THEN
           !we have a perfect gas
           IF(PM(I).NE.ZERO .AND. PEXT.NE.ZERO)THEN
             IF(abs(PM(I)+PEXT).GT.EM06)THEN
               !PM(I)=-PEXT 
               chain='SUBMAT-0'
               write(chain(8:8),'(i1.1)')I           
               chain1=
     .   chain//' IS A PERFECT GAS:MINIMUM PRESSURE SHOULD BE -PEXT       '
               CALL ANCMSG(MSGID=98,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ID,
     .                     C1=TITR,
     .                     C2=chain1)
             END IF
           ENDIF
         END IF
        END DO !next I
      END IF !(IFLG.LE.1)
      
      !check Drucker Prager Yield Surface
      IF(IFLG.LE.1)THEN
        DO I=1,3
          chain='SUBMAT-0'
          write(chain(8:8),'(i1.1)')I           
          !
          IF(A1(I) < ZERO .AND. A2(I) == ZERO)THEN                               
            chain1 = chain//': INVERTED YIELD SURFACE. CHECK A1 SIGN.                '
            CALL ANCMSG(MSGID=829,                                               
     .                  MSGTYPE=MSGWARNING,                                    
     .                  ANMODE=ANINFO,                                         
     .                  I1=51,                                                 
     .                  I2=ID,                                                 
     .                  C1='WARNING',                                          
     .                  C2=TITR,                                               
     .                  C3=chain)                                              
          ENDIF                                                                  
          IF(A2(I) < ZERO)THEN                                                   
            chain1 = chain//': UNTIPYCAL YIELD SURFACE. CHECK A2 SIGN.               '        
            CALL ANCMSG(MSGID=829,                                               
     .                  MSGTYPE=MSGWARNING,                                    
     .                  ANMODE=ANINFO,                                         
     .                  I1=51,                                                 
     .                  I2=ID,                                                 
     .                  C1='WARNING',                                          
     .                  C2=TITR,                                               
     .                  C3=chain)                                              
          ENDIF                                                                  
          !
        END DO !next I
      END IF !(IFLG.LE.1)      
      
      RETURN
      END
C======================================================================|








