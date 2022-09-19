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
Chd|  DPARRWS                       source/output/anim/dparrws.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE DPARRWS(NESBW,NSTRF,IXC   ,
     2                  IXTG ,X ,NODCUT,RWBUF,NPRW,
     3                  IXS   )
C-----------------------------------------------                        
C   I m p l i c i t   T y p e s                                         
C-----------------------------------------------                        
#include      "implicit_f.inc"
C-----------------------------------------------                        
C   C o m m o n   B l o c k s                                           
C-----------------------------------------------                        
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
      INTEGER NESBW,NSTRF(*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        NODCUT,NPRW(*), IXS(NIXS,*)
      my_real
     .  X(3,*),RWBUF(NRWLP,*)
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
      INTEGER J, JJ, LEN, I, K, L, KK, K0, K1, K5, K9, N,
     .   N0, N1, N2, N3, N4, N10, NSEG, NSEGC, NSEGTG, ITYP,
     .   UNPACK(15,4), II(8), N5, N6, N7, N8, NSEGS, K3,OW
      my_real
     .   XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, D13, XXC, YYC, ZZC
      INTEGER POWER2(8),IPACK
      DATA POWER2/1,2,4,8,16,32,64,128/
C-----------------------------------------------                        
      DATA UNPACK/1,2,1,3,1,2,1,4,1,2,1,3,1,2,1,
     .            0,0,2,0,3,3,2,0,4,4,2,4,3,3,2,
     .            0,0,0,0,0,0,3,0,0,0,4,0,4,4,3,
     .            0,0,0,0,0,0,0,0,0,0,0,0,0,0,4/
      IF (NSECT>0) THEN
        K0 = NSTRF(25)
        DO I=1,NSECT
          JJ = 0
          N0 = NUMNOD + NODCUT + I - 1
          K5=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     1         + 2*NSTRF(K0+7) +NSTRF(K0+8)*2
          NSEGC = NSTRF(K0+9)

          DO J=1,NSEGC
            KK = K5+2*(J-1)
            N  = NSTRF(KK)
            IF(NSTRF(KK+1)/=0) THEN
              N1 = UNPACK(NSTRF(KK+1),1)
              N2 = UNPACK(NSTRF(KK+1),2)
              IF(N2==0)THEN
                N2 = N1
                N3 = N1
              ELSE
                N3 = UNPACK(NSTRF(KK+1),3)
                IF(N3==0)N3 = N2
              ENDIF
              II(1) = N0
              II(2) = IXC(1+N1,N)-1
              II(3) = IXC(1+N2,N)-1
              II(4) = IXC(1+N3,N)-1
              CALL WRITE_I_C(II,4)
            ENDIF
          ENDDO
          JJ = 0
          K9=K5+2*NSTRF(K0+9) +2*NSTRF(K0+10)
     1         +2*NSTRF(K0+11)+2*NSTRF(K0+12)
          NSEGTG = NSTRF(K0+13)
          DO J=1,NSEGTG
            KK = K9+2*(J-1)
            N  = NSTRF(KK)
            IF(NSTRF(KK+1)/=0) THEN
              N1 = UNPACK(NSTRF(1+KK),1)
              N2 = UNPACK(NSTRF(1+KK),2)
              IF(N2==0)THEN
                N2 = N1
                N3 = N1
              ELSE
                N3 = UNPACK(NSTRF(1+KK),3)
                IF(N3==0)N3 = N2
              ENDIF
              II(1) = N0
              II(2) = IXTG(1+N1,N)-1
              II(3) = IXTG(1+N2,N)-1
              II(4) = IXTG(1+N3,N)-1
              CALL WRITE_I_C(II,4)
            ENDIF
          ENDDO
          JJ = 0
          K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
          NSEGS=NSTRF(K0+7)
          IF(NSEGS/=0)THEN
              II(1) = N0
              II(2) = N0
              II(3) = N0
              II(4) = N0
              CALL WRITE_I_C(II,4)
          END IF
C                                                                       
          DO J=1,NSEGS
            KK=K3+2*(J-1)
            IPACK=NSTRF(KK+1)
            IF(IPACK/=0)THEN
              N =NSTRF(KK)
              II(1)=IXS(2,N)-1
              II(2)=IXS(3,N)-1
              II(3)=IXS(4,N)-1
              II(4)=IXS(5,N)-1
              II(5)=IXS(6,N)-1
              II(6)=IXS(7,N)-1
              II(7)=IXS(8,N)-1
              II(8)=IXS(9,N)-1
              IF(     II(2)==II(1).AND.II(4)==II(3)
     .           .AND.II(8)==II(5).AND.II(7)==II(6))THEN
C tetra4, tetra10                                                       
                N1=MOD(IPACK/POWER2(1),2)
                N2=MOD(IPACK/POWER2(3),2)
                N3=MOD(IPACK/POWER2(5),2)
                N4=MOD(IPACK/POWER2(6),2)
                IF(N1/=0.AND.N2/=0.AND.N3/=0)THEN
                 CALL WRITE_I_C(II(1),1)
                 CALL WRITE_I_C(II(3),1)
                 CALL WRITE_I_C(II(5),1)
                 CALL WRITE_I_C(II(5),1)
                END IF
                IF(N1/=0.AND.N2/=0.AND.N4/=0)THEN
                 CALL WRITE_I_C(II(1),1)
                 CALL WRITE_I_C(II(3),1)
                 CALL WRITE_I_C(II(6),1)
                 CALL WRITE_I_C(II(6),1)
                END IF
                IF(N2/=0.AND.N3/=0.AND.N4/=0)THEN
                 CALL WRITE_I_C(II(3),1)
                 CALL WRITE_I_C(II(5),1)
                 CALL WRITE_I_C(II(6),1)
                 CALL WRITE_I_C(II(6),1)
                END IF
                IF(N3/=0.AND.N1/=0.AND.N4/=0)THEN
                 CALL WRITE_I_C(II(5),1)
                 CALL WRITE_I_C(II(1),1)
                 CALL WRITE_I_C(II(6),1)
                 CALL WRITE_I_C(II(6),1)
                END IF
              ELSE
C brick, shell16, brick20                                               
                N1=MOD(IPACK/POWER2(1),2)
                N2=MOD(IPACK/POWER2(2),2)
                N3=MOD(IPACK/POWER2(3),2)
                N4=MOD(IPACK/POWER2(4),2)
                N5=MOD(IPACK/POWER2(5),2)
                N6=MOD(IPACK/POWER2(6),2)
                N7=MOD(IPACK/POWER2(7),2)
                N8=MOD(IPACK/POWER2(8),2)
                IF(N1/=0.AND.N2/=0.AND.N3/=0.AND.N4/=0)THEN
                 CALL WRITE_I_C(II(1),1)
                 CALL WRITE_I_C(II(2),1)
                 CALL WRITE_I_C(II(3),1)
                 CALL WRITE_I_C(II(4),1)
                END IF
                IF(N5/=0.AND.N6/=0.AND.N7/=0.AND.N8/=0)THEN
                 CALL WRITE_I_C(II(5),1)
                 CALL WRITE_I_C(II(6),1)
                 CALL WRITE_I_C(II(7),1)
                 CALL WRITE_I_C(II(8),1)
                END IF
                IF(N1/=0.AND.N5/=0.AND.N6/=0.AND.N2/=0)THEN
                 CALL WRITE_I_C(II(1),1)
                 CALL WRITE_I_C(II(5),1)
                 CALL WRITE_I_C(II(6),1)
                 CALL WRITE_I_C(II(2),1)
                END IF
                IF(N4/=0.AND.N8/=0.AND.N7/=0.AND.N3/=0)THEN
                 CALL WRITE_I_C(II(4),1)
                 CALL WRITE_I_C(II(8),1)
                 CALL WRITE_I_C(II(7),1)
                 CALL WRITE_I_C(II(3),1)
                END IF
                IF(N1/=0.AND.N4/=0.AND.N8/=0.AND.N5/=0)THEN
                 CALL WRITE_I_C(II(1),1)
                 CALL WRITE_I_C(II(4),1)
                 CALL WRITE_I_C(II(8),1)
                 CALL WRITE_I_C(II(5),1)
                END IF
                IF(N2/=0.AND.N3/=0.AND.N7/=0.AND.N6/=0)THEN
                 CALL WRITE_I_C(II(2),1)
                 CALL WRITE_I_C(II(3),1)
                 CALL WRITE_I_C(II(7),1)
                 CALL WRITE_I_C(II(6),1)
                END IF
              END IF
            END IF
          END DO
C                                                                       
          K0=NSTRF(K0+24)
        ENDDO
      ENDIF
C                                                                       
      N0 = NUMNOD + NODCUT + NSECT
      N1 = NUMNOD + NODCUT + NSECT + NRWALL
C                                                                       
      DO N=1,NRWALL
        II(1) = N0
        II(2) = N0
        II(3) = N0
        II(4) = N0
        CALL WRITE_I_C(II,4)
        N0 = N0 + 1
        K=1
        N2=N +NRWALL
        N3=N2+NRWALL
        N4=N3+NRWALL
        ITYP= NPRW(N4)
C                                                                 
        IF(IABS(ITYP)==1.OR.ITYP==4)THEN
             II(1) = N1
             II(2) = N1 + 3
             II(3) = N1 + 2
             II(4) = N1 + 1
             CALL WRITE_I_C(II,4)
             N1 = N1 + 4
        ELSEIF(ITYP==2)THEN
           N10 = N1
           DO J = 1,23
             II(1) = N1
             II(2) = N1 + 2
             II(3) = N1 + 3
             II(4) = N1 + 1
             CALL WRITE_I_C(II,4)
             N1 = N1 + 2
           ENDDO
           II(1) = N1
           II(2) = N10
           II(3) = N10 + 1
           II(4) = N1 + 1
           CALL WRITE_I_C(II,4)
           N1 = N1 + 2
        ELSEIF(ITYP==3)THEN
          DO I = 1,6
           DO J = 1,6
            DO L = 1,6
             II(1) = N1
             II(2) = N1 + 1
             II(3) = N1 + 8
             II(4) = N1 + 7
             CALL WRITE_I_C(II,4)
             N1 = N1 + 1
            ENDDO
            N1 = N1 + 1
           ENDDO
           N1 = N1 + 7
          ENDDO
         ENDIF
         K=K+NPRW(N)
         IF(NPRW(N4)==-1)K=K+NINT(RWBUF(8,N))
      ENDDO
C                                                                       
      RETURN
      END
