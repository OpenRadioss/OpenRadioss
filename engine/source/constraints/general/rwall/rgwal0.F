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
Chd|  RGWAL0                        source/constraints/general/rwall/rgwal0.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        RGWALC                        source/constraints/general/rwall/rgwalc.F
Chd|        RGWALL                        source/constraints/general/rwall/rgwall.F
Chd|        RGWALP                        source/constraints/general/rwall/rgwalp.F
Chd|        RGWALS                        source/constraints/general/rwall/rgwals.F
Chd|        RGWALT                        source/constraints/general/rwall/rgwal0.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|====================================================================
      SUBROUTINE RGWAL0(X      ,A      ,V      ,RWBUF   ,LPRW  ,
     2                  NPRW   ,MS     ,FSAV   ,FR_WALL ,FOPT  ,
     3                  RWSAV  ,WEIGHT ,FRWL6 ,NODNX_SMS,WEIGHT_MD,
     4                  DIMFB  , FBSAV6,STABSEN,TABSENSOR )
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
#include      "param_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LPRW(*), NPRW(*), FR_WALL(NSPMD+2,*), WEIGHT(*),
     .        IBID, NODNX_SMS(*),WEIGHT_MD(*),
     .        DIMFB,STABSEN,TABSENSOR(*)
      my_real X(3,NUMNOD), A(3,NUMNOD), V(3,NUMNOD),RWBUF(NRWLP,*),RWSAV(*),MS(*),
     .        FSAV(NTHVKI,*), FOPT(6,*)
      DOUBLE PRECISION FRWL6(7,6,NRWALL)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB),RBID(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N,N2,N3,N4,N5,N6, ITYP, ISL, IFQ, ILAGM, IMP,
     .        PMAIN,IPARSENS,ISECT
C-----------------------------------------------
          RBID = ZERO
C Init global result to 0

!$OMP DO
          DO N = 1, NRWALL
            DO K = 1, 6
              FRWL6(1,K,N) = ZERO
              FRWL6(2,K,N) = ZERO
              FRWL6(3,K,N) = ZERO
              FRWL6(4,K,N) = ZERO
              FRWL6(5,K,N) = ZERO
              FRWL6(6,K,N) = ZERO
              FRWL6(7,K,N) = ZERO
            END DO
          END DO
!$OMP END DO

          ISL = 1
          K=1
          IMP=0

          DO N=1,NRWALL
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL
C
            ITYP= NPRW(N4)
            ILAGM= 0
            IF (NPRW(N6) == 1) ILAGM=1
            IF(ITYP == 1.AND.ILAGM == 0)THEN
              CALL RGWALL(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K),
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N5),RWSAV(ISL),FRWL6(1,1,N),IMP       ,IBID   ,
     +          IBID    ,IBID      ,IBID        ,NODNX_SMS ,WEIGHT_MD)
            ELSEIF(ITYP == 2)THEN
              CALL RGWALC(
     +          X       ,A           ,V       ,RWBUF(1,N) ,LPRW(K),
     +          NPRW(N) ,NPRW(N2)    ,NPRW(N3),MS         ,WEIGHT ,
     +          NPRW(N5),FRWL6(1,1,N),IMP     ,IBID       ,IBID   ,
     +          IBID    ,IBID        ,NODNX_SMS , WEIGHT_MD)
C
            ELSEIF(ITYP == 3)THEN
              CALL RGWALS(
     +          X       ,A           ,V       ,RWBUF(1,N),LPRW(K),
     +          NPRW(N) ,NPRW(N2)    ,NPRW(N3),MS        ,WEIGHT ,
     +          NPRW(N5),FRWL6(1,1,N),IMP     ,IBID      ,IBID   ,
     +          IBID    ,IBID        ,NODNX_SMS ,WEIGHT_MD)
            ELSEIF(ITYP == 4)THEN
              CALL RGWALP(
     +          X       ,A           ,V       ,RWBUF(1,N),LPRW(K),
     +          NPRW(N) ,NPRW(N2)    ,NPRW(N3),MS        ,WEIGHT ,
     +          NPRW(N5),FRWL6(1,1,N),IMP     ,IBID      ,IBID   ,
     +          IBID    ,IBID        ,NODNX_SMS ,WEIGHT_MD)
            ENDIF
            K=K+NPRW(N)
            IFQ = NINT(RWBUF(15,N))
            IF (SMINVER < 9.OR.IFQ > 0) THEN
              ISL=ISL+NPRW(N)*3
            ENDIF
            IF(NPRW(N4) == -1)THEN
              K=K+NINT(RWBUF(8,N))
            ENDIF
          END DO

C Explicit barrier required before communication

          CALL MY_BARRIER

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
                  IF(FR_WALL(ISPMD+1,N) /= 0) THEN
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
              IPARSENS=0
              ISECT=0
              IF(STABSEN/=0) THEN
                ISECT=TABSENSOR(N+NSECT+NINTSUB+NINTER+1)-TABSENSOR(N+NSECT+NINTSUB+NINTER)
              ENDIF
              IF(ISECT/=0) THEN
                IPARSENS=1
                CALL RGWALT(
     1            NPRW(N3),RWBUF(1,N),FRWL6(1,1,N),PMAIN,FSAV(1,N),
     2            FOPT(1,N),FBSAV6(1,1,ISECT) , IPARSENS)
              ELSE
                CALL RGWALT(
     1            NPRW(N3),RWBUF(1,N),FRWL6(1,1,N),PMAIN,FSAV(1,N),
     2            FOPT(1,N),RBID     , IPARSENS)
              ENDIF
            END DO
          END IF

!$OMP END SINGLE

      RETURN
      END
Chd|====================================================================
Chd|  RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        FV_RWL                        source/constraints/general/rwall/srw_imp.F
Chd|        GETDYNA_A                     source/implicit/imp_dyna.F    
Chd|        RGWALC                        source/constraints/general/rwall/rgwalc.F
Chd|        RGWALL                        source/constraints/general/rwall/rgwall.F
Chd|        RGWALP                        source/constraints/general/rwall/rgwalp.F
Chd|        RGWALS                        source/constraints/general/rwall/rgwals.F
Chd|        RGWALT                        source/constraints/general/rwall/rgwal0.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        ZEROR                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE RGWAL0_IMP(X      ,D      ,V      ,RWBUF   ,LPRW   ,
     1                  NPRW   ,MS     ,FSAV   ,FR_WALL ,FOPT   ,
     2                  RWSAV  ,WEIGHT ,FSAVD   ,NT_RW  ,
     3                  IDDL   ,IKC    ,ICOMV  ,NDOF  ,FRWL6 ,WEIGHT_MD,
     4                  DIMFB  , FBSAV6,STABSEN,TABSENSOR)
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
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LPRW(*), NPRW(*), FR_WALL(NSPMD+2,*), WEIGHT(*),
     .        NT_RW,IDDL(*),IKC(*),NDOF(*),ICOMV,WEIGHT_MD(*),
     .        DIMFB,STABSEN,TABSENSOR(*)
      my_real X(3,NUMNOD), D(3,NUMNOD), V(3,NUMNOD),RWBUF(NRWLP,*),RWSAV(*),MS(*),
     .        FSAV(NTHVKI,*), FOPT(6,*),FSAVD(NTHVKI,*)
      DOUBLE PRECISION
     .        FRWL6(7,6,NRWALL)
      DOUBLE PRECISION
     .        FBSAV6(12,6,DIMFB),RBID(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, N2, N3, N4, N5, N6, ITYP, ISL, IFQ, ILAGM,
     .        NDS,IMP, PMAIN, IBID,IPARSENS,ISECT
      my_real A(3,NUMNOD),BID,DTI
C-----------------------------------------------
          RBID = ZERO
C Init global result to 0

C for the moment RGWAL0 is called in monoprocessor, so no need of // do loop
c!$OMP DO
          DO N = 1, NRWALL
            DO K = 1, 6
              FRWL6(1,K,N) = ZERO
              FRWL6(2,K,N) = ZERO
              FRWL6(3,K,N) = ZERO
              FRWL6(4,K,N) = ZERO
              FRWL6(5,K,N) = ZERO
              FRWL6(6,K,N) = ZERO
              FRWL6(7,K,N) = ZERO
            END DO
          END DO
c!$OMP END DO

          NDS=0
          IMP=1
                IF (IDYNA > 0) THEN
                  CALL GETDYNA_A(1  ,NUMNOD   ,A )
                ELSE
            CALL ZEROR(A,NUMNOD)
                END IF
          IF (ICOMV == 1) THEN
           DTI = ONE/DT2
           DO N=1,NUMNOD
            V(1,N)=D(1,N)*DTI
            V(2,N)=D(2,N)*DTI
            V(3,N)=D(3,N)*DTI
           ENDDO
          ENDIF
          ISL = 1
          K=1
          DO N=1,NRWALL
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL

            ITYP= NPRW(N4)
            ILAGM= 0
            IF (CODVERS >= 44) THEN
              IF (NPRW(N6) == 1) ILAGM=1
            ENDIF
            IF(ITYP == 1.AND.ILAGM == 0)THEN
              CALL RGWALL(
     +          X       ,A         ,V           ,RWBUF(1,N),LPRW(K),
     +          NPRW(N) ,NPRW(N2)  ,NPRW(N3)    ,MS        ,WEIGHT ,
     +          NPRW(N5),RWSAV(ISL),FRWL6(1,1,N),IMP       ,NT_RW  ,
     +          IDDL    ,IKC       ,NDOF        ,IBID      ,WEIGHT_MD)
            ELSEIF(ITYP == 2)THEN
              CALL RGWALC(
     +          X       ,A           ,V       ,RWBUF(1,N),LPRW(K),
     +          NPRW(N) ,NPRW(N2)    ,NPRW(N3),MS        ,WEIGHT ,
     +          NPRW(N5),FRWL6(1,1,N),IMP     ,NT_RW     ,IDDL   ,
     +          IKC     ,NDOF        ,IBID    ,WEIGHT_MD)
            ELSEIF(ITYP == 3)THEN
              CALL RGWALS(
     +          X       ,A           ,V       ,RWBUF(1,N),LPRW(K),
     +          NPRW(N) ,NPRW(N2)    ,NPRW(N3),MS        ,WEIGHT ,
     +          NPRW(N5),FRWL6(1,1,N),IMP     ,NT_RW     ,IDDL   ,
     +          IKC     ,NDOF        ,IBID    ,WEIGHT_MD)
            ELSEIF(ITYP == 4)THEN
              CALL RGWALP(
     +          X       ,A           ,V       ,RWBUF(1,N),LPRW(K),
     +          NPRW(N) ,NPRW(N2)    ,NPRW(N3),MS        ,WEIGHT ,
     +          NPRW(N5),FRWL6(1,1,N),IMP     ,NT_RW     ,IDDL   ,
     +          IKC     ,NDOF        ,IBID    ,WEIGHT_MD)
            ENDIF

            K=K+NPRW(N)
            IFQ = NINT(RWBUF(15,N))
            IF (SMINVER < 9.OR.IFQ > 0) ISL=ISL+NPRW(N)*3
            IF(NPRW(N4) == -1)K=K+NINT(RWBUF(8,N))
          END DO

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
                  IF(FR_WALL(ISPMD+1,N) /= 0) THEN
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
              IPARSENS=0
              ISECT=0
              IF(STABSEN/=0) ISECT=TABSENSOR(N+NSECT+NINTSUB+NINTER+1)-
     .                             TABSENSOR(N+NSECT+NINTSUB+NINTER)
              IF(ISECT/=0) THEN
                IPARSENS=1
                CALL RGWALT(
     1            NPRW(N3),RWBUF(1,N),FRWL6(1,1,N),PMAIN,FSAV(1,N),
     2            FOPT(1,N),FBSAV6(1,1,ISECT) , IPARSENS)
              ELSE
                CALL RGWALT(
     1            NPRW(N3),RWBUF(1,N),FRWL6(1,1,N),PMAIN,FSAV(1,N),
     2            FOPT(1,N),RBID     , IPARSENS)
              ENDIF
            END DO
          END IF

          IF (NT_RW > 0) THEN
           CALL FV_RWL(IDDL   ,IKC   ,NDOF  ,D    ,V    ,A     )
          ENDIF

      RETURN
      END

Chd|====================================================================
Chd|  RGWALF                        source/constraints/general/rwall/rgwal0.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RGWALF(A      ,RWBUF   ,NPRW   ,MS    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPRW(*)
      my_real A(3,NUMNOD),RWBUF(NRWLP,*),MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,N2,N3,N4,N5,N6, MSR, ITYP, ILAGM
      my_real DM
C-----------------------------------------------
C
C RWL(17) = Fx
C RWL(18) = Fy
C RWL(19) = Fz
C RWL(20) = Somme (Xslv)
C
      DO N=1,NRWALL
        N2=N +NRWALL
        N3=N2+NRWALL
        N4=N3+NRWALL
        N5=N4+NRWALL
        N6=N5+NRWALL
        ITYP= NPRW(N4)
        ILAGM= 0
        IF (NPRW(N6) == 1) ILAGM=1
        IF(ITYP >= 1.AND.ITYP <= 4.AND.ILAGM == 0)THEN
          MSR = NPRW(N3)
          IF(MSR /= 0)THEN
            DM = MS(MSR)+ RWBUF(20,N)
            IF(DM /= ZERO) THEN
              DM = MS(MSR) / DM
              A(1,MSR) = (A(1,MSR) + RWBUF(17,N))*DM
              A(2,MSR) = (A(2,MSR) + RWBUF(18,N))*DM
              A(3,MSR) = (A(3,MSR) + RWBUF(19,N))*DM
            ENDIF
          ENDIF
        ENDIF
      END DO

C
      RETURN
      END

Chd|====================================================================
Chd|  RGWALT                        source/constraints/general/rwall/rgwal0.F
Chd|-- called by -----------
Chd|        RGWAL0                        source/constraints/general/rwall/rgwal0.F
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RGWALT(MSR ,RWL,FRWL6,PMAIN,FSAV,
     2                  FOPT,FBSAV6,IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSR, PMAIN, IPARSENS, I
      my_real RWL(*), FSAV(*),FOPT(6),DIVDT12
      DOUBLE PRECISION FRWL6(7,6)
      DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real FXN, FYN, FZN, FXT, FYT, FZT, XMT
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
C
      IF(DT12 /= ZERO)THEN
        DIVDT12 = ONE / DT12
      ELSE
        DIVDT12 = ZERO
      ENDIF

      IF (IPARSENS  /= 0)THEN
        DO I=1,6
          FBSAV6(1,I) = FRWL6(1,I)*DIVDT12
          FBSAV6(2,I) = FRWL6(2,I)*DIVDT12
          FBSAV6(3,I) = FRWL6(3,I)*DIVDT12
          FBSAV6(4,I) = FRWL6(5,I)*DIVDT12
          FBSAV6(5,I) = FRWL6(6,I)*DIVDT12
          FBSAV6(6,I) = FRWL6(7,I)*DIVDT12
        ENDDO
      ENDIF
C
      IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
C   changement formulation F et XMT stockoques dans RWL et appliques debut cycle suivant
         RWL(17)=(FXN+FXT)*DIVDT12
         RWL(18)=(FYN+FYT)*DIVDT12
         RWL(19)=(FZN+FZT)*DIVDT12
         RWL(20)=XMT
C test  pour ne cummuler qu'une fois en multiprocessors dans le cas moving
        IF(ISPMD+1 == PMAIN.OR. MSR == 0) THEN
          FSAV(1)=FSAV(1)+FXN
          FSAV(2)=FSAV(2)+FYN
          FSAV(3)=FSAV(3)+FZN
          FSAV(4)=FSAV(4)+FXT
          FSAV(5)=FSAV(5)+FYT
          FSAV(6)=FSAV(6)+FZT
          FOPT(1)=FOPT(1)+RWL(17)
          FOPT(2)=FOPT(2)+RWL(18)
          FOPT(3)=FOPT(3)+RWL(19)
        END IF
      ELSE
        RWL(17)=RWL(17)+(FXN+FXT)*DIVDT12
        RWL(18)=RWL(18)+(FYN+FYT)*DIVDT12
        RWL(19)=RWL(19)+(FZN+FZT)*DIVDT12
        RWL(20)=RWL(20)+XMT
C test pour ne cummuler qu'une fois en multiprocessors dans le cas moving
        IF(ISPMD+1 == PMAIN.OR. MSR == 0) THEN
         FSAV(1)=FSAV(1)+FXN
         FSAV(2)=FSAV(2)+FYN
         FSAV(3)=FSAV(3)+FZN
         FSAV(4)=FSAV(4)+FXT
         FSAV(5)=FSAV(5)+FYT
         FSAV(6)=FSAV(6)+FZT
         FOPT(1)=FOPT(1)+(FXN+FXT)*DIVDT12
         FOPT(2)=FOPT(2)+(FYN+FYT)*DIVDT12
         FOPT(3)=FOPT(3)+(FZN+FZT)*DIVDT12
        END IF
      END IF
C
      RETURN
      END
