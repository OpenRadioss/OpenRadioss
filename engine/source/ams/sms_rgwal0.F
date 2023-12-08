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
Chd|  SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_RGWALC_BCS_0              source/ams/sms_rgwalc.F       
Chd|        SMS_RGWALC_BCS_1              source/ams/sms_rgwalc.F       
Chd|        SMS_RGWALC_BILAN              source/ams/sms_rgwalc.F       
Chd|        SMS_RGWALC_FRIC               source/ams/sms_rgwalc.F       
Chd|        SMS_RGWALC_IMPACT             source/ams/sms_rgwalc.F       
Chd|        SMS_RGWALL_BCS_0              source/ams/sms_rgwall.F       
Chd|        SMS_RGWALL_BCS_1              source/ams/sms_rgwall.F       
Chd|        SMS_RGWALL_BILAN              source/ams/sms_rgwall.F       
Chd|        SMS_RGWALL_FRIC               source/ams/sms_rgwall.F       
Chd|        SMS_RGWALL_IMPACT             source/ams/sms_rgwall.F       
Chd|        SMS_RGWALP_BCS_0              source/ams/sms_rgwalp.F       
Chd|        SMS_RGWALP_BCS_1              source/ams/sms_rgwalp.F       
Chd|        SMS_RGWALP_BILAN              source/ams/sms_rgwalp.F       
Chd|        SMS_RGWALP_FRIC               source/ams/sms_rgwalp.F       
Chd|        SMS_RGWALP_IMPACT             source/ams/sms_rgwalp.F       
Chd|        SMS_RGWALS_BCS_0              source/ams/sms_rgwals.F       
Chd|        SMS_RGWALS_BCS_1              source/ams/sms_rgwals.F       
Chd|        SMS_RGWALS_BILAN              source/ams/sms_rgwals.F       
Chd|        SMS_RGWALS_FRIC               source/ams/sms_rgwals.F       
Chd|        SMS_RGWALS_IMPACT             source/ams/sms_rgwals.F       
Chd|        SMS_RGWALT                    source/ams/sms_rgwal0.F       
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|====================================================================
      SUBROUTINE SMS_RGWAL_0(IFLAG  ,X      ,V      ,RWBUF   ,LPRW    ,   
     2                       NPRW   ,MS     ,FSAV   ,FR_WALL ,FOPT    ,
     3                       RWSAV  ,WEIGHT ,IRWL_WORK, NRWL_SMS,FRWL6,
     4                       A      ,RES    ,R      ,FREA    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LPRW(*), NPRW(*), FR_WALL(NSPMD+2,*), WEIGHT(*),
     .        IRWL_WORK(*), NRWL_SMS(*), IFLAG
      my_real
     .   X(3,*), V(3,*),RWBUF(NRWLP,*),RWSAV(*),MS(*),
     .   FSAV(NTHVKI,*), FOPT(6,*), 
     .   A(3,*), RES(3,*), R(3,*), FREA(3,*)
      DOUBLE PRECISION
     .        FRWL6(7,6,NRWALL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, ITYP, ISL, IFQ, N2, N3, N4, N5, N6, N7, N8, L, M,
     .        PMAIN, NSMS, 
     .        KIND(NRWALL), ISLIND(NRWALL)
C-----------------------------------------------
          ISL = 1
          K=1
C
C Pre calcul index
C
          DO N = 1, NRWALL
            KIND(N)  = K
            ISLIND(N) = ISL
C
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL
C
            K=K+NPRW(N)
            IFQ = NINT(RWBUF(15,N))
            IF (IFQ>0) ISL=ISL+NPRW(N)*3
            IF(NPRW(N4)==-1)K=K+NINT(RWBUF(8,N))
          END DO
C

          SELECT CASE (IFLAG)

          CASE(0)
          IFRICW=0
C
          CALL MY_BARRIER
C
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRWALL
            K = KIND(N)
            ISL = ISLIND(N)
C
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL
            N7=N6+NRWALL
            N8=N7+NRWALL
C
            ITYP= NPRW(N4)
C
            NSMS= NPRW(N7)
            IF(ITYP==1.AND.NSMS/=0)THEN
              CALL SMS_RGWALL_IMPACT(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          RWSAV(ISL),NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ELSEIF(ITYP==2.AND.NSMS/=0)THEN
              CALL SMS_RGWALC_IMPACT(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ELSEIF(ITYP==3.AND.NSMS/=0)THEN
              CALL SMS_RGWALS_IMPACT(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ELSEIF(ITYP==4.AND.NSMS/=0)THEN
              CALL SMS_RGWALP_IMPACT(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ENDIF
          END DO
!$OMP END DO
!$OMP SINGLE
          IF(NSPMD > 1) CALL SPMD_ALLGLOB_ISUM9(IFRICW,1)
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
C
C         set A=-V/dt (A is an acceleration)
          CASE(1)
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRWALL
            K = KIND(N)
            ISL = ISLIND(N)
C
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL
            N7=N6+NRWALL
            N8=N7+NRWALL
C
            ITYP= NPRW(N4)
C
            NSMS= NPRW(N7)
            IF(ITYP==1.AND.NSMS/=0)THEN
              CALL SMS_RGWALL_BCS_0(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          RWSAV(ISL),NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ELSEIF(ITYP==2.AND.NSMS/=0)THEN
              CALL SMS_RGWALC_BCS_0(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K)   )
            ELSEIF(ITYP==3.AND.NSMS/=0)THEN
              CALL SMS_RGWALS_BCS_0(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ELSEIF(ITYP==4.AND.NSMS/=0)THEN
              CALL SMS_RGWALP_BCS_0(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ENDIF
          END DO
!$OMP END DO
C
C         set RES=0
          CASE(2)
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRWALL
            K = KIND(N)
            ISL = ISLIND(N)
C
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL
            N7=N6+NRWALL
            N8=N7+NRWALL
C
            ITYP= NPRW(N4)
C
            NSMS= NPRW(N7)
            IF(ITYP==1.AND.NSMS/=0)THEN
              CALL SMS_RGWALL_BCS_1(
     +          X       ,RES         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          RWSAV(ISL),NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ELSEIF(ITYP==2.AND.NSMS/=0)THEN
              CALL SMS_RGWALC_BCS_1(
     +          X       ,RES         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ELSEIF(ITYP==3.AND.NSMS/=0)THEN
              CALL SMS_RGWALS_BCS_1(
     +          X       ,RES         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  , NRWL_SMS(K))
            ELSEIF(ITYP==4.AND.NSMS/=0)THEN
              CALL SMS_RGWALP_BCS_1(
     +          X       ,RES         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K))
            ENDIF
          END DO
!$OMP END DO
C
C         friction force
          CASE(3)
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRWALL
            K = KIND(N)
            ISL = ISLIND(N)
C
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL
            N7=N6+NRWALL
            N8=N7+NRWALL
C
            ITYP= NPRW(N4)
C
            NSMS= NPRW(N7)
            IF(ITYP==1.AND.NSMS/=0)THEN
              CALL SMS_RGWALL_FRIC(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          RWSAV(ISL),NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K), 
     +          FSAV(1,N),FOPT(1,N),RES         ,R     ,FREA       )
            ELSEIF(ITYP==2.AND.NSMS/=0)THEN
              CALL SMS_RGWALC_FRIC(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K),FSAV(1,N), 
     +          FOPT(1,N),RES      ,R       ,FREA       )
            ELSEIF(ITYP==3.AND.NSMS/=0)THEN
              CALL SMS_RGWALS_FRIC(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K),FSAV(1,N), 
     +          FOPT(1,N),RES      ,R       ,FREA       )
            ELSEIF(ITYP==4.AND.NSMS/=0)THEN
              CALL SMS_RGWALP_FRIC(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K),FSAV(1,N), 
     +          FOPT(1,N),RES      ,R       ,FREA       )
            ENDIF
          END DO
!$OMP END DO
C
C         save reaction force
          CASE(4)
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRWALL
            K = KIND(N)
            ISL = ISLIND(N)
C
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL
            N7=N6+NRWALL
            N8=N7+NRWALL
C
            ITYP= NPRW(N4)
C
            DO M = 1, 6
            DO L = 1, 7
              FRWL6(L,M,N) = ZERO
            END DO
            END DO
C
            NSMS= NPRW(N7)
            IF(ITYP==1.AND.NSMS/=0)THEN
              CALL SMS_RGWALL_BILAN(
     +          X       ,FREA        ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT  ,
     +          RWSAV(ISL),NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K), 
     +          FSAV(1,N),FOPT(1,N),FRWL6(1,1,N),A     )
            ELSEIF(ITYP==2.AND.NSMS/=0)THEN
              CALL SMS_RGWALC_BILAN(
     +          X       ,FREA      ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K),FSAV(1,N), 
     +          FOPT(1,N),FRWL6(1,1,N),A    )
            ELSEIF(ITYP==3.AND.NSMS/=0)THEN
              CALL SMS_RGWALS_BILAN(
     +          X       ,FREA        ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K),FSAV(1,N), 
     +          FOPT(1,N),FRWL6(1,1,N),A    )
            ELSEIF(ITYP==4.AND.NSMS/=0)THEN
              CALL SMS_RGWALP_BILAN(
     +          X       ,FREA        ,V           ,RWBUF(1,N),LPRW(K), 
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N8),IRWL_WORK(K),NSMS  ,NRWL_SMS(K),FSAV(1,N), 
     +          FOPT(1,N),FRWL6(1,1,N),A    )
            ENDIF
          END DO
!$OMP END DO

!$OMP SINGLE
C
C Traitements Speciaux : Communications SPMD si moving present
C + Sauvegarde Force et Impultion main
C
          IF(IMCONV == 1) THEN
            DO N=1,NRWALL
              N2=N +NRWALL
              N3=N2+NRWALL
              N4=N3+NRWALL
              N5=N4+NRWALL
              N6=N5+NRWALL
              IF(NPRW(N3) /= 0) THEN
                IF(NSPMD > 1) THEN
C   si proc concerne par le rgwall
                  IF(FR_WALL(ISPMD+1,N)/=0) THEN
                    CALL SPMD_EXCH_FR6(FR_WALL(1,N),FRWL6(1,1,N),7*6)
                  ENDIF
                  PMAIN = FR_WALL(NSPMD+2,N)
                ELSE
                  PMAIN = 1
                ENDIF
              ELSE
                PMAIN = 1
              END IF
C
              CALL SMS_RGWALT(
     1          NPRW(N3),RWBUF(1,N),FRWL6(1,1,N),PMAIN,FSAV(1,N),
     2          FOPT(1,N))
            END DO
          END IF

!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif

          END SELECT

      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALT                    source/ams/sms_rgwal0.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALT(MSR ,RWL,FRWL6,PMAIN,FSAV,
     2                  FOPT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSR, PMAIN
C     REAL
      my_real
     .   RWL(*), FSAV(*),FOPT(6)
      DOUBLE PRECISION
     .   FRWL6(7,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   FXN, FYN, FZN, FXT, FYT, FZT, XMT, DT15, DT25
C-----------------------------------------------
      FXN = FRWL6(1,1)+FRWL6(1,2)+FRWL6(1,3)+
     .      FRWL6(1,4)+FRWL6(1,5)+FRWL6(1,6)
      FYN = FRWL6(2,1)+FRWL6(2,2)+FRWL6(2,3)+
     .      FRWL6(2,4)+FRWL6(2,5)+FRWL6(2,6)
      FZN = FRWL6(3,1)+FRWL6(3,2)+FRWL6(3,3)+
     .      FRWL6(3,4)+FRWL6(3,5)+FRWL6(3,6)
      XMT = FRWL6(4,1)+FRWL6(4,2)+FRWL6(4,3)+
     .      FRWL6(4,4)+FRWL6(4,5)+FRWL6(4,6)
      FXT = FRWL6(5,1)+FRWL6(5,2)+FRWL6(5,3)+
     .      FRWL6(5,4)+FRWL6(5,5)+FRWL6(5,6)
      FYT = FRWL6(6,1)+FRWL6(6,2)+FRWL6(6,3)+
     .      FRWL6(6,4)+FRWL6(6,5)+FRWL6(6,6)
      FZT = FRWL6(7,1)+FRWL6(7,2)+FRWL6(7,3)+
     .      FRWL6(7,4)+FRWL6(7,5)+FRWL6(7,6)
C F et XMT stockoques dans RWL et appliques debut cycle suivant
      RWL(17)=FXN+FXT
      RWL(18)=FYN+FYT
      RWL(19)=FZN+FZT
      RWL(20)=XMT
C test pour ne cummuler qu'une fois en multiprocessors dans le cas moving
      IF(ISPMD+1 == PMAIN.OR. MSR == 0) THEN
        DT15=HALF*DT1
        DT25=HALF*DT2
        FSAV(1)=FSAV(1)+RWL(21)+DT15*FXN
        FSAV(2)=FSAV(2)+RWL(22)+DT15*FYN
        FSAV(3)=FSAV(3)+RWL(23)+DT15*FZN
        FSAV(4)=FSAV(4)+RWL(24)+DT15*FXT
        FSAV(5)=FSAV(5)+RWL(25)+DT15*FYT
        FSAV(6)=FSAV(6)+RWL(26)+DT15*FZT
        RWL(21)=DT25*FXN
        RWL(22)=DT25*FYN
        RWL(23)=DT25*FZN
        RWL(24)=DT25*FXT
        RWL(25)=DT25*FYT
        RWL(26)=DT25*FZT
        FOPT(1)=FOPT(1)+RWL(17)
        FOPT(2)=FOPT(2)+RWL(18)
        FOPT(3)=FOPT(3)+RWL(19)
      END IF
C
      RETURN
      END
