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
Chd|====================================================================
Chd|  M1ISMSTR11                    source/materials/mat/mat001/m1ismstr11.F
Chd|-- called by -----------
Chd|        M1LAWTOT                      source/materials/mat/mat001/m1lawtot.F
Chd|-- calls ---------------
Chd|        PRINC_U1                      source/materials/mat/mat001/princ_u1.F
Chd|====================================================================
              SUBROUTINE M1ISMSTR11(NEL ,
     1                              ES1 , ES2 ,ES3 ,ES4  ,ES5   ,
     2                              ES6 ,EPSTH, C1 ,G2   ,SIG11 )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL     
      my_real
     .   ES1(*), ES2(*), ES3(*), 
     .   ES4(*), ES5(*), ES6(*),
     .   SIG11(MVSIZ,6), EPSTH(*),C1,G2
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,K,IFAST(MVSIZ),NPRIN,INDEX(MVSIZ),NFAST
      my_real
     .    SV(3),EV(MVSIZ,3),DIRPRV(MVSIZ,3,3),EKK,DAV,P3,
     .    ESPIN1(MVSIZ), ESPIN2(MVSIZ), ESPIN3(MVSIZ), 
     .    ESPIN4(MVSIZ), ESPIN5(MVSIZ), ESPIN6(MVSIZ),
     .    E1, E2, E3, E4, E5, E6,ESPIN(MVSIZ,6), 
     .    E1Q,E2Q,E3Q,E4Q,E5Q,E6Q,
     .    E1T,E2T,E3T,E4T,E5T,E6T,B2(6),EMIN,EMAX
C----------------------------------------------------------------
          NPRIN = 0
          DO I=1,NEL
            IFAST(I) =1
            EMIN=MIN(ES1(I),ES2(I),ES3(I),ES4(I),ES5(I),ES6(I))
            EMAX=MAX(ES1(I),ES2(I),ES3(I),ES4(I),ES5(I),ES6(I))
            IF (EMIN<-EM01.OR.EMAX>EM01) THEN
             NPRIN = NPRIN + 1
             INDEX(NPRIN) = I
             IFAST(I) =0
             ESPIN1(NPRIN)=ES1(I)
             ESPIN2(NPRIN)=ES2(I)
             ESPIN3(NPRIN)=ES3(I)
             ESPIN4(NPRIN)=ES4(I)
             ESPIN6(NPRIN)=ES6(I)
             ESPIN5(NPRIN)=ES5(I)
            END IF
          ENDDO
          DO I=1,NEL
           IF (IFAST(I)==0) CYCLE
           E1=ES1(I)
           E2=ES2(I)
           E3=ES3(I)
           E4=ES4(I)
           E6=ES6(I)
           E5=ES5(I)
           E1Q = E1*E1
           E2Q = E2*E2
           E3Q = E3*E3
           E4Q = E4*E4
           E6Q = E6*E6
           E5Q = E5*E5
           B2(1)=E1Q+E4Q+E6Q
           B2(2)=E4Q+E2Q+E5Q
           B2(3)=E6Q+E5Q+E3Q
           B2(4)=E1*E4+E2*E4+E5*E6
           B2(6)=E1*E6+E4*E5+E3*E6
           B2(5)=E4*E6+E2*E5+E3*E5
           ES1(I)=HALF*(E1-FOURTH*B2(1))
           ES2(I)=HALF*(E2-FOURTH*B2(2))
           ES3(I)=HALF*(E3-FOURTH*B2(3))
           ES4(I)=HALF*(E4-FOURTH*B2(4))
           ES6(I)=HALF*(E6-FOURTH*B2(6))
           ES5(I)=HALF*(E5-FOURTH*B2(5))
          ENDDO
C
          DO I=1,NEL
           IF (IFAST(I)==0) CYCLE
            EKK=ES1(I)+ES2(I)+ES3(I)-EPSTH(I)
            DAV=-THIRD*(ES1(I)+ES2(I)+ES3(I))
            P3 = C1*EKK
            SIG11(I,1)=P3+G2*(ES1(I)+DAV)
            SIG11(I,2)=P3+G2*(ES2(I)+DAV)
            SIG11(I,3)=P3+G2*(ES3(I)+DAV)
            SIG11(I,4)=G2*ES4(I)
            SIG11(I,5)=G2*ES5(I)
            SIG11(I,6)=G2*ES6(I)
          ENDDO
C
       IF (NPRIN>0) THEN
        CALL PRINC_U1(
     1                  NPRIN    , ESPIN1 , ESPIN2 ,ESPIN3 ,ESPIN4 ,
     2                  ESPIN5   ,ESPIN6  ,EV   , DIRPRV  )
#include "vectorize.inc"
        DO K=1,NPRIN
          I = INDEX(K)
          EKK=EV(K,1)+EV(K,2)+EV(K,3)
          DAV=-THIRD*EKK
          P3 = C1*(EKK-EPSTH(I))
          SV(1)=P3 +G2*(EV(K,1)+DAV)
          SV(2)=P3 +G2*(EV(K,2)+DAV)
          SV(3)=P3 +G2*(EV(K,3)+DAV)
           SIG11(I,1) = DIRPRV(K,1,1)*DIRPRV(K,1,1)*SV(1)
     .               + DIRPRV(K,1,2)*DIRPRV(K,1,2)*SV(2)
     .               + DIRPRV(K,1,3)*DIRPRV(K,1,3)*SV(3)     
           SIG11(I,2) = DIRPRV(K,2,2)*DIRPRV(K,2,2)*SV(2)
     .               + DIRPRV(K,2,3)*DIRPRV(K,2,3)*SV(3)
     .               + DIRPRV(K,2,1)*DIRPRV(K,2,1)*SV(1)     
           SIG11(I,3) = DIRPRV(K,3,3)*DIRPRV(K,3,3)*SV(3)
     .               + DIRPRV(K,3,1)*DIRPRV(K,3,1)*SV(1)
     .               + DIRPRV(K,3,2)*DIRPRV(K,3,2)*SV(2)     
           SIG11(I,4) = DIRPRV(K,1,1)*DIRPRV(K,2,1)*SV(1)
     .               + DIRPRV(K,1,2)*DIRPRV(K,2,2)*SV(2)
     .               + DIRPRV(K,1,3)*DIRPRV(K,2,3)*SV(3)     
           SIG11(I,5) = DIRPRV(K,2,2)*DIRPRV(K,3,2)*SV(2)
     .               + DIRPRV(K,2,3)*DIRPRV(K,3,3)*SV(3)
     .               + DIRPRV(K,2,1)*DIRPRV(K,3,1)*SV(1)     
           SIG11(I,6) = DIRPRV(K,3,3)*DIRPRV(K,1,3)*SV(3)
     .               + DIRPRV(K,3,1)*DIRPRV(K,1,1)*SV(1)
     .               + DIRPRV(K,3,2)*DIRPRV(K,1,2)*SV(2)             
        ENDDO
       END IF
C      
      RETURN
      END
