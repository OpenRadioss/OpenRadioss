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
Chd|  SMS_RLINK10                   source/ams/sms_rlink.F        
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        SMS_RLINK1                    source/ams/sms_rlink.F        
Chd|        SMS_RLINK2                    source/ams/sms_rlink.F        
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|====================================================================
      SUBROUTINE SMS_RLINK10(
     1   MS    ,A     ,NLINK ,LLINK ,SKEW ,
     2   FR_RL ,WEIGHT,FRL6  ,IDOWN ,TAG_LNK_SMS,
     3   ITAB  ,FRL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLINK(*),LLINK(*),FR_RL(NSPMD+2,*),WEIGHT(*), IDOWN,
     .        TAG_LNK_SMS(*), ITAB(*)
      my_real
     .   MS(*), A(3,*), SKEW(LSKEW,*)
      DOUBLE PRECISION FRL6(4,6,NRLINK)
      my_real
     .       FRL(4,NRLINK)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, K1, N, ISK, I, IC, KIND(NRLINK)
C-----------------------------------------------
C
C Pre calcul index
C
          K = 1
          DO I = 1, NRLINK
            KIND(I) = K
            K = K + NLINK(4*I-3)
          ENDDO
C
C boucle parallele sur les threads SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRLINK
            FRL(1,N)=ZERO
            FRL(2,N)=ZERO
            FRL(3,N)=ZERO
            FRL(4,N)=ZERO
            IF(TAG_LNK_SMS(N) < 0)CYCLE

            K1=4*N-3
            IC=NLINK(K1+1)
            IF(IC==0) CYCLE

            K = KIND(N)
            ISK=NLINK(K1+3)
            IF(ISK==1)THEN
              CALL SMS_RLINK1(
     1          MS      ,A       ,NLINK(K1) ,NLINK(K1+1),LLINK(K),
     2          WEIGHT,FRL6(1,1,N),1   ,IDOWN      ,FR_RL(NSPMD+2,N),
     3          FRL(1,N),TAG_LNK_SMS(N),ITAB)
            ELSE
              CALL SMS_RLINK2(
     1          MS      ,A          ,NLINK(K1),NLINK(K1+1),LLINK(K),
     2          SKEW(1,ISK),WEIGHT  ,FRL6(1,1,N),1        ,IDOWN   ,
     3          FR_RL(NSPMD+2,N),FRL(1,N),TAG_LNK_SMS(N),ITAB)
            ENDIF
          END DO
!$OMP END DO

          IF(NSPMD > 1) THEN
!$OMP SINGLE

C routine appelee rlink par rlink a optimiser si besoin
            DO N=1,NRLINK
              IF(TAG_LNK_SMS(N)==0)CYCLE
C routine de calcul Parith/ON de A rigid link
              IF(FR_RL(ISPMD+1,N)/=0)
     +          CALL SPMD_EXCH_FR6(FR_RL(1,N),FRL6(1,1,N),4*6)
            END DO

!$OMP END SINGLE
          END IF

 100      CONTINUE
C boucle parallele sur les threads SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRLINK
            IF(TAG_LNK_SMS(N) < 0)CYCLE

            K1=4*N-3
            IC=NLINK(K1+1)
            IF(IC==0) CYCLE

            K = KIND(N)
            ISK=NLINK(K1+3)
            IF(ISK==1)THEN
              CALL SMS_RLINK1(
     1          MS      ,A       ,NLINK(K1) ,NLINK(K1+1),LLINK(K),
     2          WEIGHT,FRL6(1,1,N),2  ,IDOWN      ,FR_RL(NSPMD+2,N),
     3          FRL(1,N),TAG_LNK_SMS(N),ITAB)
            ELSE
              CALL SMS_RLINK2(
     1          MS      ,A          ,NLINK(K1),NLINK(K1+1),LLINK(K),
     2          SKEW(1,ISK),WEIGHT  ,FRL6(1,1,N),2        ,IDOWN   ,
     3          FR_RL(NSPMD+2,N),FRL(1,N),TAG_LNK_SMS(N),ITAB)
            ENDIF
          END DO
!$OMP END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RLINK11                   source/ams/sms_rlink.F        
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        SMS_RLINK1                    source/ams/sms_rlink.F        
Chd|        SMS_RLINK2                    source/ams/sms_rlink.F        
Chd|        SMS_RLINK3                    source/ams/sms_rlink.F        
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|====================================================================
      SUBROUTINE SMS_RLINK11(
     1   MS    ,A     ,NNLINK,LLLINK,SKEW  ,
     2   FR_LL ,WEIGHT,FRL6  ,X     ,XFRAME,
     3   V     ,IDOWN ,TAG_LNK_SMS,ITAB,FRL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNLINK(10,*), LLLINK(*), FR_LL(NSPMD+2,*),
     .        WEIGHT(*), IDOWN, TAG_LNK_SMS(*),ITAB(*)
      my_real
     .   MS(*), A(3,*), SKEW(LSKEW,*),
     .   XFRAME(NXFRAME,*), X(3,*), V(3,*)
      DOUBLE PRECISION FRL6(4,6,NLINK)
      my_real
     .       FRL(4,NLINK)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, K1, N, ISK, I, IC, IPOL, KIND(NLINK)
C-----------------------------------------------
C
C Pre calcul index
C
          K = 1
          DO I = 1, NLINK
            KIND(I) = K
            K = K + NNLINK(1,I)
          ENDDO
C boucle parallele sur les threads SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NLINK
            FRL(1,N)=ZERO
            FRL(2,N)=ZERO
            FRL(3,N)=ZERO
            FRL(4,N)=ZERO
            IF(TAG_LNK_SMS(NRLINK+N) < 0)CYCLE

            IC=NNLINK(3,N)
            IF(IC==0)CYCLE

            ISK=NNLINK(5,N)
            IPOL=NNLINK(6,N)
            K = KIND(N)
            IPOL=NNLINK(6,N)
            IF(IPOL==1)THEN
               CALL SMS_RLINK3(
     1      MS       ,X          ,A            ,V        ,NNLINK(1,N),
     2      NNLINK(3,N),LLLINK(K),XFRAME(1,ISK),WEIGHT   ,FRL6(1,1,N),
     3      1  ,IDOWN  ,FR_LL(NSPMD+2,N),FRL(1,N),TAG_LNK_SMS(NRLINK+N),
     4      ITAB)
            ELSEIF(ISK==1)THEN
               CALL SMS_RLINK1(
     1      MS       ,A        ,NNLINK(1,N),NNLINK(3,N),LLLINK(K),
     2      WEIGHT   ,FRL6(1,1,N),1   ,IDOWN      ,FR_LL(NSPMD+2,N),
     3      FRL(1,N) ,TAG_LNK_SMS(NRLINK+N),ITAB)
            ELSE
               CALL SMS_RLINK2(
     1      MS       ,A          ,NNLINK(1,N),NNLINK(3,N),LLLINK(K),
     2      SKEW(1,ISK), WEIGHT   ,FRL6(1,1,N),1          ,IDOWN    ,
     3      FR_LL(NSPMD+2,N),FRL(1,N),TAG_LNK_SMS(NRLINK+N),ITAB)
            ENDIF
          END DO
!$OMP END DO
          IF(NSPMD > 1) THEN
!$OMP SINGLE

C routine appelee rlink par rlink a optimiser si besoin

            DO N=1,NLINK
              IF(TAG_LNK_SMS(NRLINK+N) < 0)CYCLE
C routine de calcul Parith/ON de A rigid link
              IF(FR_LL(ISPMD+1,N)/=0)
     +          CALL SPMD_EXCH_FR6(FR_LL(1,N),FRL6(1,1,N),4*6)
            END DO

!$OMP END SINGLE
          END IF

 100      CONTINUE
C boucle parallele sur les threads SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NLINK
            IF(TAG_LNK_SMS(NRLINK+N) < 0)CYCLE

            IC=NNLINK(3,N)
            IF(IC==0)CYCLE

            ISK=NNLINK(5,N)
            IPOL=NNLINK(6,N)
            K = KIND(N)
            IPOL=NNLINK(6,N)
            IF(IPOL==1)THEN
               CALL SMS_RLINK3(
     1      MS       ,X          ,A            ,V        ,NNLINK(1,N),
     2      NNLINK(3,N),LLLINK(K),XFRAME(1,ISK),WEIGHT   ,FRL6(1,1,N),
     3      2  ,IDOWN  ,FR_LL(NSPMD+2,N),FRL(1,N),TAG_LNK_SMS(NRLINK+N),
     4      ITAB)
            ELSEIF(ISK==1)THEN
               CALL SMS_RLINK1(
     1      MS       ,A        ,NNLINK(1,N),NNLINK(3,N),LLLINK(K),
     2      WEIGHT   ,FRL6(1,1,N),2  ,IDOWN      ,FR_LL(NSPMD+2,N),
     3      FRL(1,N) ,TAG_LNK_SMS(NRLINK+N),ITAB)
            ELSE
               CALL SMS_RLINK2(
     1      MS       ,A          ,NNLINK(1,N),NNLINK(3,N),LLLINK(K),
     2      SKEW(1,ISK),WEIGHT   ,FRL6(1,1,N),2          ,IDOWN    ,
     3      FR_LL(NSPMD+2,N),FRL(1,N),TAG_LNK_SMS(NRLINK+N),ITAB)
            ENDIF
          END DO
!$OMP END DO
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RLINK1                    source/ams/sms_rlink.F        
Chd|-- called by -----------
Chd|        SMS_RLINK10                   source/ams/sms_rlink.F        
Chd|        SMS_RLINK11                   source/ams/sms_rlink.F        
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SMS_RLINK1(MS    ,A    ,NSN  ,IC      ,NOD,
     .                      WEIGHT,FRL6 ,IFLAG,IDOWN   ,PMAIN ,
     .                      FRL   ,TAG_LNK,ITAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, IC, IFLAG, IDOWN, NCNOD, PMAIN, TAG_LNK
      INTEGER NOD(*),WEIGHT(*), ITAB(*)
C     REAL
      my_real
     .   MS(*), A(3,*), FRL(4)
      DOUBLE PRECISION FRL6(4,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IJ, N, K
C     REAL
      my_real
     .   MASS, AX, AY, AZ, XNSN,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN)
C-----------------------------------------------
      SELECT CASE(IDOWN)

      CASE(0)
C
C Remontee
      IF(IFLAG == 1)THEN

C Init Parith/ON
       DO K = 1, 6
         FRL6(1,K) = ZERO
         FRL6(2,K) = ZERO
         FRL6(3,K) = ZERO
         FRL6(4,K) = ZERO
       END DO
C
       DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N)==1) THEN
          F2(I)=A(1,N)
          F3(I)=A(2,N)
          F4(I)=A(3,N)
        ELSE
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
       CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1), 4)

       RETURN
C
      ELSEIF(IFLAG == 2)THEN
C
       AX = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +      FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
       AY = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +      FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
       AZ = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +      FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
       DO I=1,NSN
         N = NOD(I)
C
C transmet force dans la dion au nd
         IF(IC==1.OR.IC==3.OR.IC==5.OR.IC==7)THEN
           A(3,N) =AZ
         ENDIF
         IF(IC==2.OR.IC==3.OR.IC==6.OR.IC==7)THEN
           A(2,N) =AY
         ENDIF
         IF(IC==4.OR.IC==5.OR.IC==6.OR.IC==7)THEN
           A(1,N) =AX
         ENDIF
       END DO
C
      ENDIF

      CASE(1)
C
C Redescente
      IF(IFLAG == 1)THEN
C
C Init Parith/ON
       DO K = 1, 6
         FRL6(1,K) = ZERO
         FRL6(2,K) = ZERO
         FRL6(3,K) = ZERO
         FRL6(4,K) = ZERO
       END DO
C
       DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N)==1) THEN
          F1(I)=MS(N)
          F2(I)=MS(N)*A(1,N)
          F3(I)=MS(N)*A(2,N)
          F4(I)=MS(N)*A(3,N)
        ELSE
          F1(I)=ZERO
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
       CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1), 4)

       RETURN
C
      ELSEIF(IFLAG == 2)THEN
C
       MASS = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +      FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
       AX = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +      FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
       AY = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +      FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
       AZ = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +      FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
       AX=AX/MAX(EM30,MASS)
       AY=AY/MAX(EM30,MASS)
       AZ=AZ/MAX(EM30,MASS)
       DO I=1,NSN
         N = NOD(I)
         IF(IC==1.OR.IC==3.OR.IC==5.OR.IC==7)THEN
           A(3,N) =AZ
         ENDIF
         IF(IC==2.OR.IC==3.OR.IC==6.OR.IC==7)THEN
           A(2,N) =AY
         ENDIF
         IF(IC==4.OR.IC==5.OR.IC==6.OR.IC==7)THEN
           A(1,N) =AX
         ENDIF
       END DO
C
      ENDIF
C
      END SELECT
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RLINK2                    source/ams/sms_rlink.F        
Chd|-- called by -----------
Chd|        SMS_RLINK10                   source/ams/sms_rlink.F        
Chd|        SMS_RLINK11                   source/ams/sms_rlink.F        
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SMS_RLINK2(MS   ,A    ,NSN   ,IC   ,NOD  ,
     2                      SKEW,WEIGHT,FRL6  ,IFLAG,IDOWN,
     3                      PMAIN,FRL,TAG_LNK,ITAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, IC, IFLAG, IDOWN, PMAIN, TAG_LNK
      INTEGER NOD(*),WEIGHT(*), ITAB(*), ISK
C     REAL
      my_real
     .   MS(*), A(3,*),SKEW(*), FRL(4)
      DOUBLE PRECISION FRL6(4,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IC1, ICC, IC2, IC3, I, N, K
C     REAL
      my_real
     .   MASS, AX, AY, AZ, DAX, DAY, DAZ, AAX, AAY,
     .   AAZ, XNSN,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN)
C-----------------------------------------------
      SELECT CASE(IDOWN)

      CASE(0)
C
C Remontee
      IF(IFLAG == 1)THEN

C Init Parith/ON
       DO K = 1, 6
         FRL6(1,K) = ZERO
         FRL6(2,K) = ZERO
         FRL6(3,K) = ZERO
         FRL6(4,K) = ZERO
       END DO

C
c      AX  =ZERO
c      AY  =ZERO
c      AZ  =ZERO
C
       DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N)==1) THEN
          F2(I)=A(1,N)
          F3(I)=A(2,N)
          F4(I)=A(3,N)
        ELSE
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
       CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1), 4)
C
      ELSEIF(IFLAG == 2)THEN
       IC1=IC/4
       ICC=IC-4*IC1
       IC2=ICC/2
       IC3=ICC-2*IC2
C
       AX = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +      FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
       AY = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +      FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
       AZ = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +      FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
       DO I=1,NSN
         N = NOD(I)
C
C transmet force dans la dion au nd
         DAX  =A(1,N)-AX
         DAY  =A(2,N)-AY
         DAZ  =A(3,N)-AZ
         AAX  =IC1*(SKEW(1)*DAX+SKEW(2)*DAY+SKEW(3)*DAZ)
         AAY  =IC2*(SKEW(4)*DAX+SKEW(5)*DAY+SKEW(6)*DAZ)
         AAZ  =IC3*(SKEW(7)*DAX+SKEW(8)*DAY+SKEW(9)*DAZ)
         A(1,N) =A(1,N)-AAX*SKEW(1)-AAY*SKEW(4)-AAZ*SKEW(7)
         A(2,N) =A(2,N)-AAX*SKEW(2)-AAY*SKEW(5)-AAZ*SKEW(8)
         A(3,N) =A(3,N)-AAX*SKEW(3)-AAY*SKEW(6)-AAZ*SKEW(9)
       END DO
      END IF
C

      CASE(1)
C
C Redescente
      IF(IFLAG == 1)THEN

C Init Parith/ON
       DO K = 1, 6
         FRL6(1,K) = ZERO
         FRL6(2,K) = ZERO
         FRL6(3,K) = ZERO
         FRL6(4,K) = ZERO
       END DO

C
c      MASS  =ZERO
c      AX  =ZERO
c      AY  =ZERO
c      AZ  =ZERO
C
       DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N)==1) THEN
          F1(I)=MS(N)
          F2(I)=MS(N)*A(1,N)
          F3(I)=MS(N)*A(2,N)
          F4(I)=MS(N)*A(3,N)
        ELSE
          F1(I)=ZERO
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
       CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 4)
       CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1), 4)

C
      ELSEIF(IFLAG == 2)THEN
C
       IC1=IC/4
       ICC=IC-4*IC1
       IC2=ICC/2
       IC3=ICC-2*IC2
C
       MASS = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +      FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
       AX = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +      FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
       AY = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +      FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
       AZ = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +      FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
       AX=AX/MAX(EM30,MASS)
       AY=AY/MAX(EM30,MASS)
       AZ=AZ/MAX(EM30,MASS)
C
       DO I=1,NSN
        N = NOD(I)
        DAX  =A(1,N)-AX
        DAY  =A(2,N)-AY
        DAZ  =A(3,N)-AZ
        AAX  =IC1*(SKEW(1)*DAX+SKEW(2)*DAY+SKEW(3)*DAZ)
        AAY  =IC2*(SKEW(4)*DAX+SKEW(5)*DAY+SKEW(6)*DAZ)
        AAZ  =IC3*(SKEW(7)*DAX+SKEW(8)*DAY+SKEW(9)*DAZ)
        A(1,N) =A(1,N)-AAX*SKEW(1)-AAY*SKEW(4)-AAZ*SKEW(7)
        A(2,N) =A(2,N)-AAX*SKEW(2)-AAY*SKEW(5)-AAZ*SKEW(8)
        A(3,N) =A(3,N)-AAX*SKEW(3)-AAY*SKEW(6)-AAZ*SKEW(9)
       END DO
      END IF

      END SELECT
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RLINK3                    source/ams/sms_rlink.F        
Chd|-- called by -----------
Chd|        SMS_RLINK11                   source/ams/sms_rlink.F        
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SMS_RLINK3(MS   ,X   ,A     ,V     ,NSN  ,
     2                      IC   ,NOD ,XFRAME,WEIGHT,FRL6 ,
     3                      IFLAG,IDOWN,PMAIN,FRL,TAG_LNK,
     4                      ITAB )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, IC, IFLAG, IDOWN, PMAIN, TAG_LNK
      INTEGER NOD(*),WEIGHT(*), ITAB(*)
C     REAL
      my_real
     .   MS(*), X(3,*), A(3,*), V(3,*),
     .   XFRAME(*), FRL(4)
      DOUBLE PRECISION FRL6(5,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IC1, ICC, IC2, IC3, I, N, K
C     REAL
      my_real
     .   MASS, AX, AY, AZ, DAX, DAY, DAZ, AAX, AAY,
     .   AAZ, RX, RY, RZ, SX, SY, SZ,
     .   TX, TY, TZ, OX, OY, OZ, AA, R2, ATR2,RX0, RY0, RZ0, R,
     .   MR2, ATMR2,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN)
C-----------------------------------------------
      SELECT CASE(IDOWN)

      CASE(0)
C
C Remontee
      SX = XFRAME(1)
      SY = XFRAME(2)
      SZ = XFRAME(3)
      AA = ONE / SQRT(SX*SX + SY*SY + SZ*SZ)
      SX = SX * AA
      SY = SY * AA
      SZ = SZ * AA
      OX = XFRAME(10)
      OY = XFRAME(11)
      OZ = XFRAME(12)

      IF(IFLAG == 1)THEN

C Init Parith/ON
       DO K = 1, 6
         FRL6(1,K) = ZERO
         FRL6(2,K) = ZERO
         FRL6(3,K) = ZERO
         FRL6(4,K) = ZERO
       END DO

c       R2 = ZERO
c       ATR2 = ZERO
C-----------------------------------------------------------------------
C     repere polaire axe S = X(frame) , rayon R , angle T
C-----------------------------------------------------------------------
       DO I=1,NSN
        N = NOD(I)
        RX0 = X(1,N) + HALF * DT2 * V(1,N) - OX
        RY0 = X(2,N) + HALF * DT2 * V(2,N) - OY
        RZ0 = X(3,N) + HALF * DT2 * V(3,N) - OZ
        TX = RY0 * SZ - RZ0 * SY
        TY = RZ0 * SX - RX0 * SZ
        TZ = RX0 * SY - RY0 * SX
        AA = ONE / SQRT(TX*TX + TY*TY + TZ*TZ)
        TX = TX * AA
        TY = TY * AA
        TZ = TZ * AA
        RX = SY * TZ - SZ * TY
        RY = SZ * TX - SX * TZ
        RZ = SX * TY - SY * TX
        R = RX * RX0 + RY * RY0 + RZ * RZ0
        R = MAX(R,EM20)
C-----------------------------------------------------------------------
C       changement de repere global => polaire
C-----------------------------------------------------------------------
        AX = A(1,N)
        AY = A(2,N)
        AZ = A(3,N)
        A(1,N) = RX * AX + RY * AY + RZ * AZ
        A(2,N) = SX * AX + SY * AY + SZ * AZ
        A(3,N) = (TX * AX + TY * AY + TZ * AZ) / R
        IF(WEIGHT(N)==1) THEN
          F1(I) = R*R
          F2(I) = R*R*A(3,N)
c          R2 = R2 + R*R
c          ATR2 = ATR2 + R*R*A(3,N)
        ELSE
          F1(I) = ZERO
          F2(I) = ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
        CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1),5)
        CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1),5)

C-----------------------------------------------------------------------
C       masse quantite de mouvement
C-----------------------------------------------------------------------
        AX  =ZERO
        AY  =ZERO
        MASS=ZERO
        DO I=1,NSN
          N = NOD(I)
          IF(WEIGHT(N)==1) THEN
            F3(I)=A(1,N)
            F4(I)=A(2,N)
          ELSE
            F3(I)=ZERO
            F4(I)=ZERO
          ENDIF
        ENDDO
C
C Traitement Parith/ON avant echange
C
        CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1),5)
        CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1),5)
C
      ELSEIF(IFLAG == 2)THEN
C
         IC1=IC/4
         ICC=IC-4*IC1
         IC2=ICC/2
         IC3=ICC-2*IC2
C
         R2   = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +          FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
         ATR2 = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +          FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
         AX   = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +          FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
         AY   = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +          FRL6(4,4)+FRL6(4,5)+FRL6(4,6)

         AZ= ATR2/R2
         DO I=1,NSN
           N = NOD(I)
C
C transmet force dans la dion au nd 1
           A(1,N) =A(1,N)-IC1*(A(1,N)-AX)
           A(2,N) =A(2,N)-IC2*(A(2,N)-AY)
           A(3,N) =A(3,N)-IC3*(A(3,N)-AZ)
         END DO
C-----------------------------------------------------------------------
C     repere polaire axe S , rayon R , angle T
C-----------------------------------------------------------------------
         DO I=1,NSN
          N = NOD(I)
          RX0 = X(1,N) + HALF * DT2 * V(1,N) - OX
          RY0 = X(2,N) + HALF * DT2 * V(2,N) - OY
          RZ0 = X(3,N) + HALF * DT2 * V(3,N) - OZ
          TX = RY0 * SZ - RZ0 * SY
          TY = RZ0 * SX - RX0 * SZ
          TZ = RX0 * SY - RY0 * SX
          AA = ONE / SQRT(TX*TX + TY*TY + TZ*TZ)
          TX = TX * AA
          TY = TY * AA
          TZ = TZ * AA
          RX = SY * TZ - SZ * TY
          RY = SZ * TX - SX * TZ
          RZ = SX * TY - SY * TX
          R = RX * RX0 + RY * RY0 + RZ * RZ0
          R = MAX(R,EM20)
C-----------------------------------------------------------------------
C       changement de repere polaire => global
C-----------------------------------------------------------------------
          AX = RX * A(1,N) + SX * A(2,N) + TX * A(3,N) * R
          AY = RY * A(1,N) + SY * A(2,N) + TY * A(3,N) * R
          AZ = RZ * A(1,N) + SZ * A(2,N) + TZ * A(3,N) * R
          A(1,N) = AX
          A(2,N) = AY
          A(3,N) = AZ
         ENDDO

      END IF
      CASE(1)
C
C Redescente

       SX = XFRAME(1)
       SY = XFRAME(2)
       SZ = XFRAME(3)
       AA = ONE / SQRT(SX*SX + SY*SY + SZ*SZ)
       SX = SX * AA
       SY = SY * AA
       SZ = SZ * AA
       OX = XFRAME(10)
       OY = XFRAME(11)
       OZ = XFRAME(12)
C-----------------------------------------------------------------------
C     repere polaire axe S = X(frame) , rayon R , angle T
C-----------------------------------------------------------------------
      IF(IFLAG == 1)THEN

C Init Parith/ON
       DO K = 1, 6
         FRL6(1,K) = ZERO
         FRL6(2,K) = ZERO
         FRL6(3,K) = ZERO
         FRL6(4,K) = ZERO
         FRL6(5,K) = ZERO
       END DO

c       R2 = ZERO
c       ATR2 = ZERO
C-----------------------------------------------------------------------
C     repere polaire axe S = X(frame) , rayon R , angle T
C-----------------------------------------------------------------------
       DO I=1,NSN
        N = NOD(I)
        RX0 = X(1,N) + HALF * DT2 * V(1,N) - OX
        RY0 = X(2,N) + HALF * DT2 * V(2,N) - OY
        RZ0 = X(3,N) + HALF * DT2 * V(3,N) - OZ
        TX = RY0 * SZ - RZ0 * SY
        TY = RZ0 * SX - RX0 * SZ
        TZ = RX0 * SY - RY0 * SX
        AA = ONE / SQRT(TX*TX + TY*TY + TZ*TZ)
        TX = TX * AA
        TY = TY * AA
        TZ = TZ * AA
        RX = SY * TZ - SZ * TY
        RY = SZ * TX - SX * TZ
        RZ = SX * TY - SY * TX
        R = RX * RX0 + RY * RY0 + RZ * RZ0
        R = MAX(R,EM20)
C-----------------------------------------------------------------------
C       changement de repere global => polaire
C-----------------------------------------------------------------------
        AX = A(1,N)
        AY = A(2,N)
        AZ = A(3,N)
        A(1,N) = RX * AX + RY * AY + RZ * AZ
        A(2,N) = SX * AX + SY * AY + SZ * AZ
        A(3,N) = (TX * AX + TY * AY + TZ * AZ) / R
        IF(WEIGHT(N)==1) THEN
          F1(I) = R*R*MS(N)
          F2(I) = R*R*MS(N)*A(3,N)
c          MR2 = MR2 + R*R*M
c          ATMR2 = ATMR2 + R*R*MS(N)*A(3,N)
        ELSE
          F1(I) = ZERO
          F2(I) = ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
        CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1),5)
        CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1),5)

C-----------------------------------------------------------------------
C       masse quantite de mouvement
C-----------------------------------------------------------------------
        AX  =ZERO
        AY  =ZERO
        MASS=ZERO
        DO I=1,NSN
          N = NOD(I)
          IF(WEIGHT(N)==1) THEN
            F3(I)=A(1,N)
            F4(I)=A(2,N)
            F5(I)=MS(N)
          ELSE
            F3(I)=ZERO
            F4(I)=ZERO
            F5(I)=ZERO
          ENDIF
        ENDDO
C
C Traitement Parith/ON avant echange
C
        CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1),5)
        CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1),5)
        CALL SUM_6_FLOAT(1  ,NSN  ,F5, FRL6(5,1),5)

C
C      ELSEIF(IFLAG == 2)THEN
C
        IC1=IC/4
        ICC=IC-4*IC1
        IC2=ICC/2
        IC3=ICC-2*IC2
C
         MR2  = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +          FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
         ATMR2= FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +          FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
         AX   = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +          FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
         AY   = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +          FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
         MASS = FRL6(5,1)+FRL6(5,2)+FRL6(5,3)+
     +          FRL6(5,4)+FRL6(5,5)+FRL6(5,6)

         AX= AX/MASS
         AY= AY/MASS
         AZ= ATMR2/MR2
C-----------------------------------------------------------------------
C       link
C-----------------------------------------------------------------------
        DO I=1,NSN
         N = NOD(I)
         A(1,N) =A(1,N)-IC1*(A(1,N)-AX)
         A(2,N) =A(2,N)-IC2*(A(2,N)-AY)
         A(3,N) =A(3,N)-IC3*(A(3,N)-AZ)
        ENDDO
C-----------------------------------------------------------------------
C     repere polaire axe S , rayon R , angle T
C-----------------------------------------------------------------------
       DO I=1,NSN
        N = NOD(I)
        RX0 = X(1,N) + HALF * DT2 * V(1,N) - OX
        RY0 = X(2,N) + HALF * DT2 * V(2,N) - OY
        RZ0 = X(3,N) + HALF * DT2 * V(3,N) - OZ
        TX = RY0 * SZ - RZ0 * SY
        TY = RZ0 * SX - RX0 * SZ
        TZ = RX0 * SY - RY0 * SX
        AA = ONE / SQRT(TX*TX + TY*TY + TZ*TZ)
        TX = TX * AA
        TY = TY * AA
        TZ = TZ * AA
        RX = SY * TZ - SZ * TY
        RY = SZ * TX - SX * TZ
        RZ = SX * TY - SY * TX
        R = RX * RX0 + RY * RY0 + RZ * RZ0
        R = MAX(R,EM20)
C-----------------------------------------------------------------------
C       changement de repere polaire => global
C-----------------------------------------------------------------------
        AX = RX * A(1,N) + SX * A(2,N) + TX * A(3,N) * R
        AY = RY * A(1,N) + SY * A(2,N) + TY * A(3,N) * R
        AZ = RZ * A(1,N) + SZ * A(2,N) + TZ * A(3,N) * R
        A(1,N) = AX
        A(2,N) = AY
        A(3,N) = AZ
       END DO
      END IF

      END SELECT
C
      RETURN
      END
