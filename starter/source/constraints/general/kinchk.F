Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  KINCHK                        source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE KINCHK(IKINE,RWL,ITAB,NPRW,LPRW,KINET,
     1                  NPBY, LPBY,IRBE2,LRBE2,IRBE3,LRBE3,
     2                  NOM_OPT,PTR_NOPT_RWALL,PTR_NOPT_RBE2,
     3                  PTR_NOPT_RBE3,ITAGCYC)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_RWALL,PTR_NOPT_RBE2,
     .        PTR_NOPT_RBE3
      INTEGER IKINE(*),NPRW(*), LPRW(*),ITAB(*),KINET(*),
     .        NPBY(NNPBY,*), LPBY(*),IRBE2(NRBE2L,*),LRBE2(*),
     .        IRBE3(NRBE3L,*),LRBE3(*),
     .        INOPT_RWALL,INOPT_RBE2,INOPT_RBE3,ITAGCYC(*)
      my_real
     .   RWL(NRWLP,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, KK, IKK, IKK2, NK, JWARN, ITYP, NE, NSL,
     .        IK(10), RBWARN, RB2WARN, KRB, NSLRB, NKIN(NUMNOD),
     .        NKINDOUBLE(NUMNOD),MARQUEUR(8192),
     .        MARQUEURDOUBLE(13),J1,FLAG_IKCOND,MARQ2,MARQM2,
     .        NS,NM,M,IAD,NUN,JJ,IKRW(NUMNOD),NIKRW
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------------------------
      MARQ2  = 0
      MARQM2 = 0
C=======================================================================
C  CORRECTIONS AUTOMATIQUES INTERFACE 1,2,9 OU RBODY AVEC RWALL
C=======================================================================
      K=0
      KK=0
      RBWARN = 0
      RB2WARN = 0
      DO N=1,NRWALL

        JWARN = 0
        NSL=NPRW(N)
        ITYP=NPRW(N+3*NRWALL)
        J=0
        DO L=1,NSL
          I=LPRW(K+L)
          IKK=IABS(IKINE(I))
          IF(IRB(IKK)/=0.OR.ITF(IKK)/=0)THEN
            RBWARN = RBWARN + 1
            JWARN = JWARN+1
            IF(IRB(IKK)/=0 .AND. MARQ2 == 0) MARQ2 = 1
            IF(ITF(IKK)/=0 .AND. MARQM2 == 0) MARQM2 = 1
            IF(IWL(IKINE(I))==1)IKINE(I) = IKINE(I) - 4

         ELSE
            IF (IRB2(IKK)/=0) RB2WARN = RB2WARN + 1
            J=J+1
            LPRW(KK+J)=LPRW(K+L)
          ENDIF
        ENDDO
        K  = K+NSL
        NSL=J
        KK = KK+NSL
        NPRW(N)=NSL
        IF(ITYP<0)THEN
          NE=NINT(RWL(8,N))
          DO I=1,NE
            LPRW(KK+I)=LPRW(K+I)
          ENDDO
          K  = K+NE
          KK = KK+NE
        ENDIF
        IF (JWARN>0) THEN
           ID=NOM_OPT(1,PTR_NOPT_RWALL+N)
           CALL FRETITL2(TITR,
     .                   NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_RWALL+N),LTITR)
           CALL ANCMSG(MSGID=446,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=JWARN)
        ENDIF
      ENDDO
C
c      CALL ANCHECK(71)
C----------------------------------------------------------
C - no-autorised hierarchy :Check des noeuds mains des RBE3 sont SECONDARY de RBE2,or INT2
C----------------------------------------------------------
      NUN=0
      DO I=1,NRBE3
        IAD = IRBE3(1,I)
        NM = IRBE3(5,I)
        DO J =1,NM
        M = LRBE3(IAD+J)
        ID=NOM_OPT(1,PTR_NOPT_RBE3+I)
        CALL FRETITL2(TITR,
     .                NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_RBE3+I),LTITR)
          IF (IKRBE2(IKINE(M))==1) THEN
           CALL ANCMSG(MSGID=805,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITAB(M))
           RETURN
          ENDIF
          IF (ITF(IKINE(M))==1) THEN
           CALL ANCMSG(MSGID=1035,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITAB(M))
           RETURN
          ENDIF
        ENDDO
      ENDDO
C----------------------------------------------------------
C - no-autorised hierarchy :Check des noeuds mains des RBE2 SECONDARY de INT2
C----------------------------------------------------------
      DO I=1,NRBE2
               M = IRBE2(3,I)
         ID=NOM_OPT(1,PTR_NOPT_RBE2+I)
           CALL FRETITL2(TITR,
     .                   NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_RBE2+I),LTITR)
          IF (ITF(IKINE(M))==1) THEN
           CALL ANCMSG(MSGID=1036,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITAB(M))
           RETURN
          ENDIF
      ENDDO
C----------------------------------------------------------
C - no-autorised hierarchy :Check des noeuds mains des RBODY sont SECONDARY de RBE2
C----------------------------------------------------------
      DO I=1,NRBODY
        M=NPBY(1,I)
         IF (IKRBE2(IKINE(M))==1) THEN
C           CALL ANCWARN(806,ANINFO_BLIND_1)
C           NUN=NUN+1
           ID=NOM_OPT(1,I)
           CALL FRETITL2(TITR,
     .                   NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
           CALL ANCMSG(MSGID=806,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITAB(M))
           RETURN
         ENDIF
      ENDDO
C----------------------------------------------------------
C - no-autorised hierarchy :Check des noeuds mains des RBODY sont SECONDARY de RBE3
C----------------------------------------------------------
      DO I=1,NRBODY
        M=NPBY(1,I)
         IF (IKRBE3(IKINE(M))==1) THEN
           ID=NOM_OPT(1,I)
           CALL FRETITL2(TITR,
     .                   NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
           CALL ANCMSG(MSGID=810,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITAB(M))
           RETURN
         ENDIF
      ENDDO
C----------------------------------------------------------
C - Check des noeuds mains des RBE2 sont mains de RBODY
C----------------------------------------------------------
      DO I=1,NRBE2
               M = IRBE2(3,I)
               DO J =1,NRBODY
          IF(NPBY(1,J)==M)THEN
           ID=NOM_OPT(1,PTR_NOPT_RBE2+I)
           CALL FRETITL2(TITR,
     .                   NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_RBE2+I),LTITR)
           CALL ANCMSG(MSGID=807,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITAB(M))
           RETURN
          ENDIF
         ENDDO
      ENDDO
C----------------------------------------------------------
C - Check des noeuds mains des Rbody SECONDARY d'autres Rbodies
C----------------------------------------------------------
       KRB = 0
       DO N=1,NRBYKIN
         NSLRB = NPBY(2,N)
         KRB= KRB+NSLRB
       ENDDO
       DO N=1,NRBYLAG
         NSLRB = NPBY(2,N)
         KRB= KRB+3*NSLRB ! possible zeros in LPBY
       ENDDO
       DO N=1,NRBYKIN
         ID=NOM_OPT(1,N)
         CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
          DO I=1,KRB
          IF(NPBY(1,N)==LPBY(I))THEN
            CALL ANCMSG(MSGID=555,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ITAB(NPBY(1,N)))
          ENDIF
        ENDDO
       ENDDO
C----------------------------------------------------------
C - Check incompatibility /BCS/CYCLIC
C----------------------------------------------------------
      IF (NBCSCYC>0) THEN
       DO I=1,NUMNOD
        IF (ITAGCYC(I)==0 .OR. IKINE(I)==0) CYCLE
C-------  RBODY      
        IF (IRB(IKINE(I))/=0 ) THEN
          K=0
          DO N=1,NRBYKIN
            NSL=NPBY(2,N)
C------ not w/ Sensor            
            IF(NPBY(7,N)/=0)THEN
             ID=NOM_OPT(1,N)
             DO J=1,NSL
               IF (LPBY(J+K)==I) THEN
                 CALL ANCMSG(MSGID=1754,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                       I1=ITAGCYC(I),I2=ITAB(I),I3=ID)
               END IF
             ENDDO
            ENDIF
            K=K+NSL
          ENDDO
        END IF !(IRB(IKINE(I))/=0 ) THEN
C-------  RBE2      
        IF (IKRBE2(IKINE(I))/=0 ) THEN
          DO N=1,NRBE2
            K  = IRBE2(1,N)
            NSL= IRBE2(5,N)
            ID = IRBE2(2,N)
            DO J=1,NSL
              IF (LRBE2(J+K)==I) THEN
                 CALL ANCMSG(MSGID=1755,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                       I1=ITAGCYC(I),I2=ITAB(I),I3=ID)
              END IF
            ENDDO
          ENDDO
        END IF 
C-------  RBE3      
        IF (IKRBE3(IKINE(I))/=0 ) THEN
          DO N=1,NRBE3
            NSL= IRBE3(3,N)
            ID = IRBE3(2,N)
            IF (NSL==I) THEN
                 CALL ANCMSG(MSGID=1756,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                       I1=ITAGCYC(I),I2=ITAB(I),I3=ID)
            END IF
          ENDDO
        END IF 
C-------  RLINK      
        IF (IRLK(IKINE(I))/=0 ) THEN
            CALL ANCMSG(MSGID=1757,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ITAGCYC(I),I2=ITAB(I))
        END IF 
       ENDDO
      END IF !(NBCSCYC>0) THEN
C=======================================================================
      IF(IPRI>=6)THEN
        WRITE(IOUT,*)' NODES WITH KINEMATIC CONDITIONS:'
        WRITE(IOUT,*)' --------------------------------'
        K = 0
        DO I=1,NUMNOD
          IF(IKINE(I)/=0)THEN
            K = K + 1
            IK(K) = ITAB(I)
            IF(K==10)THEN
              WRITE(IOUT,FMT=FMW_10I)(IK(J),J=1,K)
              K = 0
            ENDIF
          ENDIF
        ENDDO
        IF(K/=10) WRITE(IOUT,FMT=FMW_10I)(IK(J),J=1,K)
      ENDIF
C
C---------------------------------------------------------------
C LISTE CONDITIONS INCOMPATIBLES PAR TYPE
C LISTE DES NOEUDS CONCERNES
C
C NKIN(I) >= 2     => POSSIBLE CONDITIONS INCOMPATIBLES SUR LE NOEUD I
C FLAG_IKCOND = 1  => CONDITIONS INCOMPATIBLES DANS LE MODELE
C MARQUEURDOUBLE(I) = 1 => CONDITIONS INCOMPATIBLES ENTRE CONDITIONS
C                         DE MEME TYPE SUR LE NOEUD I
C MARQUEUR(I) != 0 => CONDITIONS INCOMPATIBLES ENTRE CONDITIONS
C                         DE TYPES DIFFERENTS SUR LE NOEUD I
C MARQUEUR(I) = 2 => CAS RBODY + RWALL
C MARQUEUR(I) =-2 => CAS INTERFACES TYPE 1,2,9  + RWALL
C MARQUEUR(I) = 3 => CAS INTERFACES TYPE 1,2,9 + RBODY + RWALL
C---------------------------------------------------------------
      DO I=1,NUMNOD
        NKIN(I) = 0
        NKINDOUBLE(I) = 0
      ENDDO
      DO I=1,NUMNOD
        NKIN(I) = IBC(IKINE(I))+ITF(IKINE(I))+IWL(IKINE(I))+
     .       IRB(IKINE(I))+IRB2(IKINE(I))+
     .       IVF(IKINE(I))+IRV(IKINE(I))+IJO(IKINE(I))+
     .       IRBM(IKINE(I))+ILMULT(IKINE(I))+IRLK(IKINE(I))+
     .       IKRBE2(IKINE(I))+IKRBE3(IKINE(I))+
     .       IBC(IKINE(I+3*NUMNOD))+ITF(IKINE(I+3*NUMNOD))+
     .       IWL(IKINE(I+3*NUMNOD))+IRB(IKINE(I+3*NUMNOD))+
     .       IRB2(IKINE(I+3*NUMNOD))+IVF(IKINE(I+3*NUMNOD))+
     .       IRV(IKINE(I+3*NUMNOD))+IJO(IKINE(I+3*NUMNOD))+
     .       IRBM(IKINE(I+3*NUMNOD))+ILMULT(IKINE(I+3*NUMNOD))+
     .       IRLK(IKINE(I+3*NUMNOD))+IKRBE2(IKINE(I+3*NUMNOD))+
     .       IKRBE3(IKINE(I+3*NUMNOD))
      ENDDO
C---------------------------------------------------------------
      FLAG_IKCOND = 0
      DO I=1,8192
        MARQUEUR(I) = 0
      ENDDO
      DO I=1,13
        MARQUEURDOUBLE(I) = 0
      ENDDO
      DO I=1,NUMNOD
        IF(NKIN(I)>=2)THEN
          IF (IBC(IKINE(I))== 1
     .        .AND. IBC(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(1) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (ITF(IKINE(I))== 1
     .        .AND. ITF(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(2) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IWL(IKINE(I))== 1
     .        .AND. IWL(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(3) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IRB(IKINE(I))== 1
     .        .AND. IRB(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(4) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IRB2(IKINE(I))== 1
     .        .AND. IRB2(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(5) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IVF(IKINE(I))== 1
     .        .AND. IVF(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(6) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IRV(IKINE(I))== 1
     .        .AND. IRV(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(7) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IJO(IKINE(I))== 1
     .        .AND. IJO(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(8) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IRBM(IKINE(I))== 1
     .        .AND. IRBM(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(9) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (ILMULT(IKINE(I))== 1
     .        .AND. ILMULT(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(10) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IRLK(IKINE(I))== 1
     .        .AND. IRLK(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(11) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IKRBE2(IKINE(I))== 1
     .        .AND. IKRBE2(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(12) = 1
            FLAG_IKCOND = 1
          ENDIF
          IF (IKRBE3(IKINE(I))== 1
     .        .AND. IKRBE3(IKINE(I+3*NUMNOD))== 1) THEN
            MARQUEURDOUBLE(13) = 1
            FLAG_IKCOND = 1
          ENDIF
        ENDIF
      ENDDO
C---------------------------------------------------------------
      NIKRW = 0
      DO I = 1,NUMNOD
        IF ( IKINE(I) /= 0 .AND. IKINE(I)/=1 .AND. IKINE(I)/=2
     .     .AND. IKINE(I)/=4 .AND. IKINE(I)/=8 .AND. IKINE(I)/=16
     .     .AND. IKINE(I)/=32 .AND. IKINE(I)/=64
     .     .AND. IKINE(I)/=128 .AND. IKINE(I)/=256
     .     .AND. IKINE(I)/=512 .AND. IKINE(I)/=1024
     .     .AND. IKINE(I)/=2048 .AND. IKINE(I)/=4096
     .     .AND. IKINE(I+4*NUMNOD) /= 0          ) THEN
          IF(IWL(IKINE(I))== 1 .AND. IRB(IKINE(I))== 1
     .     .AND. ITF(IKINE(I))== 1 )THEN
            IF (IKINE(I)>14) THEN
              MARQUEUR(IKINE(I)) = 3
              FLAG_IKCOND = 1
            ELSE
              MARQUEUR(IKINE(I)) = 0
            ENDIF
          ELSEIF(IWL(IKINE(I))== 1 .AND. IRB(IKINE(I))== 1)THEN
            IF (IKINE(I)>12) THEN
              MARQUEUR(IKINE(I)) = 2
              FLAG_IKCOND = 1
            ELSE
              MARQUEUR(IKINE(I)) = 0
            ENDIF
          ELSEIF(IWL(IKINE(I))== 1 .AND. ITF(IKINE(I))== 1)THEN
            IF (IKINE(I)>6) THEN
              MARQUEUR(IKINE(I)) = -2
              FLAG_IKCOND = 1
            ELSE
              MARQUEUR(IKINE(I)) = 0
            ENDIF
          ELSE
            MARQUEUR(IKINE(I)) = 1
            FLAG_IKCOND = 1
              NIKRW = NIKRW + 1
              IKRW(NIKRW) = ITAB(I)
          ENDIF
        ENDIF
      ENDDO
C---------------------------------------------------------------
      IF (IPRI>=6 .AND. FLAG_IKCOND==1) THEN
      WRITE(IOUT,*)'                                 '
      WRITE(IOUT,*)
     .       'LIST OF POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS :'
      WRITE(IOUT,*)'--------------------------------------------------'
      WRITE(IOUT,*)'                                 '

      DO I=1,13
        IF ( MARQUEURDOUBLE(I) == 1 )THEN
          IF ( I == 1) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL BOUNDARY CONDITIONS :'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IBC(IKINE(J))== 1
     .          .AND. IBC(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
              WRITE(IOUT,*)'                                           '
          ENDIF
          IF ( I == 2) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL INTERFACES TYPE 1 2 12 OR 9'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (ITF(IKINE(J))== 1
     .          .AND. ITF(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 3) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RIGID WALLS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IWL(IKINE(J))== 1
     .          .AND. IWL(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 4 .OR. I == 5) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RIGID BODIES'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IRB(IKINE(J))== 1
     .          .AND. IRB(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 6) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL IMPOSED ACCELERATIONS, IMPOSED DEP
     .LACEMENTS, IMPOSED VELOCITIES'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IVF(IKINE(J))== 1
     .          .AND. IVF(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 7) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RIVETS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IRV(IKINE(J))== 1
     .          .AND. IRV(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 8) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL CYLINDRICAL JOINTS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IJO(IKINE(J))== 1
     .          .AND. IJO(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 9) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL IMPOSED BODY VELOCITIES'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (ILMULT(IKINE(J))== 1
     .          .AND. ILMULT(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 10) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL LAGRANGE MULTIPLIERS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IBC(IKINE(J))== 1
     .          .AND. IBC(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 11) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RIGID LINKS :'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IRLK(IKINE(J))== 1
     .          .AND. IRLK(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
              WRITE(IOUT,*)'                                           '
          ENDIF
          IF ( I == 12) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RBE2 :'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IKRBE2(IKINE(J))== 1
     .          .AND. IKRBE2(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
              WRITE(IOUT,*)'                                           '
          ENDIF
          IF ( I == 13) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RBE3 :'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'NODES :'
              K = 0
              DO J = 1,NUMNOD
                IF (IKRBE3(IKINE(J))== 1
     .          .AND. IKRBE3(IKINE(J+3*NUMNOD))== 1) THEN
                  K = K + 1
                  IK(K) = ITAB(J)
                  IF(K==10)THEN
                    WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                    K = 0
                  ENDIF
                ENDIF
              ENDDO
              IF(K/=0)THEN
                WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
              ENDIF
              WRITE(IOUT,*)'                                           '
          ENDIF
        ENDIF
      ENDDO
C                                      '
      DO I=1,8192
        IF ( MARQUEUR(I) /= 0 )THEN
          WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
          DO J=1,ABS(MARQUEUR(I))
C cas RBODY & RWALL, INTER 1_2_9 & RWALL, RBODY & INTER 1_2_9 & RWALL
            IF (J==1 .AND. ABS(MARQUEUR(I)) == 2 ) IWL(I)=IWL(I)-1
            IF (J==1 .AND. ABS(MARQUEUR(I)) == 3 ) THEN
              IWL(I)=IWL(I)-1
              ITF(I) = ITF(I) - 1
            ENDIF
            IF (J==2 .AND. MARQUEUR(I) == 2 ) THEN
              WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
              IWL(I) = IWL(I) + 1
              IRB(I) = IRB(I) - 1
            ENDIF
            IF (J==2 .AND. MARQUEUR(I) == -2 ) THEN
              WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
              IWL(I) = IWL(I) + 1
              ITF(I) = ITF(I) - 1
            ENDIF
            IF (J==2 .AND. MARQUEUR(I) == 3 ) THEN
              WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
              IWL(I) = IWL(I) + 1
              IRB(I) = IRB(I) - 1
            ENDIF
            IF (J==3 .AND. MARQUEUR(I) == 3 ) THEN
              WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
              IWL(I) = IWL(I) - 1
              ITF(I) = ITF(I) +1
            ENDIF
C
            IF (IBC(I)== 1) THEN
              WRITE(IOUT,*)'    BOUNDARY CONDITION'
            ENDIF
            IF (ITF(I)== 1) THEN
              WRITE(IOUT,*)'    INTERFACE TYPE 1 2 12 OR 9'
            ENDIF
            IF (IWL(I)== 1) THEN
              WRITE(IOUT,*)'    RIGID WALL'
            ENDIF
            IF (IRB(I)== 1) THEN
              WRITE(IOUT,*)'    RIGID BODY'
            ENDIF
            IF (IRB2(I)== 1) THEN
              WRITE(IOUT,*)'    RIGID BODY'
            ENDIF
            IF (IVF(I)== 1) THEN
              WRITE(IOUT,*)'    IMPOSED ACCELERATION, IMPOSED DISPLACEMENT
     ., IMPOSED VELOCITY'
            ENDIF
            IF (IRV(I)== 1) THEN
              WRITE(IOUT,*)'    RIVET'
            ENDIF
            IF (IJO(I)== 1) THEN
              WRITE(IOUT,*)'    CYLINDRICAL JOINT'
            ENDIF
            IF (IRBM(I)== 1) THEN
              WRITE(IOUT,*)'    IMPOSED BODY VELOCITY'
            ENDIF
            IF (ILMULT(I)== 1) THEN
              WRITE(IOUT,*)'    LAGRANGE MULTIPLIERS'
            ENDIF
            IF (IRLK(I)== 1) THEN
              WRITE(IOUT,*)'    RIGID LINK'
            ENDIF
            IF (IKRBE2(I)== 1) THEN
              WRITE(IOUT,*)'    RBE2'
            ENDIF
            IF (IKRBE3(I)== 1) THEN
              WRITE(IOUT,*)'    RBE3'
            ENDIF
            WRITE(IOUT,*)
     .         '                                            '
            WRITE(IOUT,*)'NODES :'
            K = 0
            DO J1 = 1,NUMNOD
              IF (IKINE(J1) == I) THEN
                 K = K + 1
                 IK(K) = ITAB(J1)
                 IF(K==10)THEN
                   WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
                   K = 0
                 ENDIF
              ENDIF
            ENDDO
            IF(K/=0)THEN
              WRITE(IOUT,FMT=FMW_10I)(IK(L),L=1,K)
            ENDIF
            WRITE(IOUT,*)'                                 '
            IF (J==2 .AND.MARQUEUR(I) == 2 )IRB(I) = IRB(I) + 1
            IF (J==2 .AND.MARQUEUR(I) == -2 )ITF(I) = ITF(I) + 1
            IF (J==3 .AND.MARQUEUR(I) == 3 ) THEN
              IRB(I) = IRB(I) + 1
              IWL(I) = IWL(I) + 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      ENDIF
C---------------------------------------------------------------
      IF(KWARN>0)THEN
         CALL ANCMSG(MSGID=312,ANMODE=ANINFO,MSGTYPE=MSGWARNING,
     .               I1=KWARN)
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
c         WRITE(IOUT,FMT=FMW_10I)(IKRW(L),L=1,KWARN)
c
      ENDIF
C---------------------------------------------------------------
C RESUME CONDITIONS INCOMPATIBLES PAR TYPE
C---------------------------------------------------------------
      WRITE(IOUT,*)'SUMMARY OF POSSIBLE INCOMPATIBLE KINEMATIC CONDITION
     .S :'
      WRITE(IOUT,*)'--------------------------------------------------
     .---'
      WRITE(IOUT,*)'                                 '
      IF (FLAG_IKCOND==0) THEN
        WRITE(IOUT,*)'NO TRUE INCOMPATIBLE KINEMATIC CONDITION'
        IF (MARQ2 == 1) THEN
          WRITE(IOUT,*)' - AFTER SECONDARY NODES OF RIGID BODIES WERE SUPPRE
     .SSED FROM RIGID WALL(S)'
        ENDIF
        IF (MARQM2 == 1) THEN
          WRITE(IOUT,*)' - AFTER SECONDARY NODES OF INTERFACES TYPE 1,2,12 O
     .R 9 WERE SUPPRESSED'
          WRITE(IOUT,*)'   FROM RIGID WALL(S)'
        ENDIF
      ENDIF
C---------------------------------------------------------------
      DO I=1,13
        IF ( MARQUEURDOUBLE(I) == 1 )THEN
          IF ( I == 1) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL BOUNDARY CONDITIONS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 2) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL INTERFACES TYPE 1 2 12 OR 9'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 3) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RIGID WALLS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 4 .OR. I == 5) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RIGID BODIES'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 6) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL IMPOSED ACCELERATIONS, IMPOSED DEP
     .LACEMENTS, IMPOSED VELOCITIES'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 7) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RIVETS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 8) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL CYLINDRICAL JOINTS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 9) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL IMPOSED BODY VELOCITIES'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 10) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL LAGRANGE MULTIPLIERS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 11) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RIGID LINKS'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 12) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RBE2'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
          IF ( I == 13) THEN
            WRITE(IOUT,*)'- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS B
     .ETWEEN'
            WRITE(IOUT,*)'    SEVERAL RBE3'
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDIF
        ENDIF
      ENDDO
C                                      '
      DO I=1,8192
        IF ( MARQUEUR(I) /= 0 )THEN
          WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
          DO J=1,ABS(MARQUEUR(I))
            IF (J==1 .AND. ABS(MARQUEUR(I)) == 2 ) IWL(I)=IWL(I)-1
            IF (J==1 .AND. ABS(MARQUEUR(I)) == 3 ) THEN
              IWL(I )= IWL(I) - 1
              ITF(I) = ITF(I) - 1
            ENDIF
            IF (J==2 .AND. MARQUEUR(I) == 2 ) THEN
              WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
              IWL(I) = IWL(I) + 1
              IRB(I) = IRB(I) - 1
            ENDIF
            IF (J==2 .AND. MARQUEUR(I) == -2 ) THEN
              WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
              IWL(I) = IWL(I) + 1
              ITF(I) = ITF(I) - 1
            ENDIF
            IF (J==2 .AND. MARQUEUR(I) == 3 ) THEN
              WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
              IWL(I) = IWL(I) + 1
              IRB(I) = IRB(I) - 1
            ENDIF
            IF (J==3 .AND. MARQUEUR(I) == 3 ) THEN
              WRITE(IOUT,*)
     .       '- POSSIBLE INCOMPATIBLE KINEMATIC CONDITIONS BETWEEN:'
              IWL(I) = IWL(I) - 1
              ITF(I) = ITF(I) + 1
            ENDIF
C
            IF (IBC(I)== 1) THEN
              WRITE(IOUT,*)'    BOUNDARY CONDITION'
            ENDIF
            IF (ITF(I)== 1) THEN
              WRITE(IOUT,*)'    INTERFACE TYPE 1 2 12 OR 9'
            ENDIF
            IF (IWL(I)== 1) THEN
              WRITE(IOUT,*)'    RIGID WALL'
            ENDIF
            IF (IRB(I)== 1) THEN
              WRITE(IOUT,*)'    RIGID BODY'
            ENDIF
            IF (IRB2(I)== 1) THEN
              WRITE(IOUT,*)'    RIGID BODY'
            ENDIF
            IF (IVF(I)== 1) THEN
              WRITE(IOUT,*)'    IMPOSED ACCELERATION, IMPOSED DISPLACEMENT
     ., IMPOSED VELOCITY'
            ENDIF
            IF (IRV(I)== 1) THEN
              WRITE(IOUT,*)'    RIVET'
            ENDIF
            IF (IJO(I)== 1) THEN
              WRITE(IOUT,*)'    CYLINDRICAL JOINT'
            ENDIF
            IF (IRBM(I)== 1) THEN
              WRITE(IOUT,*)'    IMPOSED BODY VELOCITY'
            ENDIF
            IF (ILMULT(I)== 1) THEN
              WRITE(IOUT,*)'    LAGRANGE MULTIPLIERS'
            ENDIF
            IF (IRLK(I)== 1) THEN
              WRITE(IOUT,*)'    RIGID LINK'
            ENDIF
            IF (IKRBE2(I)== 1) THEN
              WRITE(IOUT,*)'    RBE2'
            ENDIF
            IF (IKRBE3(I)== 1) THEN
              WRITE(IOUT,*)'    RBE3'
            ENDIF
            IF (J==2 .AND.MARQUEUR(I) == 2 )IRB(I) = IRB(I) + 1
            IF (J==2 .AND.MARQUEUR(I) == -2 )ITF(I) = ITF(I) + 1
            IF (J==3 .AND.MARQUEUR(I) == 3 ) THEN
              IRB(I) = IRB(I) + 1
              IWL(I) = IWL(I) + 1
            ENDIF
            IF(KWARN>0.AND.IPRI>=3)THEN
               WRITE(IOUT,*)'NODES :'
               WRITE(IOUT,FMT=FMW_10I)(IKRW(L),L=1,NIKRW)
            ENDIF
            WRITE(IOUT,*)'                                            '
            WRITE(IOUT,*)'                                            '
          ENDDO
        ENDIF
      ENDDO
C---------------------------------------------------------------
      DO I=1,NUMNOD
C reset flag impact / wall
ccc        IF(IWL(IKINE(I))==1)IKINE(I) = IKINE(I) - 4
        KINET(I)=IKINE(I)
      ENDDO
C
Cls
Cls
Cls  WARNING : RETURN HERE !!!
Cls
      RETURN
Cls
Cls
Cls
      JWARN = 0
      DO 100 I=1,NUMNOD
        NK = IBC(IKINE(I))+ITF(IKINE(I))+IWL(IKINE(I))+
     .       IRB(IKINE(I))+IRB2(IKINE(I))+
     .       IVF(IKINE(I))+IRV(IKINE(I))+IJO(IKINE(I))
        IF(NK>=2)THEN
          JWARN = JWARN+1
          WRITE(IOUT,*)
     .     '    -',NK,' KINEMATIC CONDITIONS ON NODE',ITAB(I),':'
          IF(IBC(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - BOUNDARY CONDITION'
          IF(ITF(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - INTERFACE TYPE 1 2 12 OR 9'
          IF(IWL(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - RIGID WALL'
          IF(IRB(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - RIGID BODY'
          IF(IRB2(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - RIGID BODY'
          IF(IVF(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - FIXED VELOCITY'
          IF(IRV(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - RIVET'
          IF(IJO(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - CYLINDRICAL JOINT'
          IF(IRBM(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - IMPOSED BODY VELOCITY'
          IF(ILMULT(IKINE(I))==1) WRITE(IOUT,*)
     .     '             - LAGRANGE MULTIPLIERS'
        ENDIF
 100  CONTINUE
C
      IWARN = IWARN + JWARN
      IF(JWARN/=0)THEN
        WRITE(ISTDO,'(A,I8,A)') ' ** WARNING',JWARN,
     .    ' NODES WITH INCOMPATIBLE KINEMATIC CONDITIONS'
        WRITE(IOUT,FMT=FMW_A_I_A) ' ** WARNING',JWARN,
     .    ' NODES WITH INCOMPATIBLE KINEMATIC CONDITIONS'
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  KINREM                        source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE KINREM(IKINE,IKINEW,RWL,ITAB,NPRW,LPRW,
     1                   NPBY, LPBY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKINE(*), IKINEW(*), NPRW(*), LPRW(*),ITAB(*),
     .        NPBY(NNPBY,*), LPBY(*)
      my_real
     .   RWL(NRWLP,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, KK, IKK, ITYP, NE, NSL
C-----------------------------------------------------------------
C  Recopie dans tableau de travail
      DO N=1,NUMNOD
        IKINEW(N)=IKINE(N)
      END DO
C=======================================================================
C  CORRECTIONS AUTOMATIQUES INTERFACE 1,2,9 OU RBODY AVEC RWALL
C=======================================================================
      K=0
      KK=0

      DO N=1,NRWALL

        NSL=NPRW(N)
        ITYP=NPRW(N+3*NRWALL)
        J=0
        DO L=1,NSL
          I=LPRW(K+L)
          IKK=IABS(IKINEW(I))
          IF(IRB(IKK)/=0.OR.ITF(IKK)/=0)THEN
            IF(IWL(IKINEW(I))==1)IKINEW(I) = IKINEW(I) - 4
          ELSE
            J=J+1
            LPRW(KK+J)=LPRW(K+L)
          ENDIF
        ENDDO
        K  = K+NSL
        NSL=J
        KK = KK+NSL
        NPRW(N)=NSL
        IF(ITYP<0)THEN
          NE=NINT(RWL(8,N))
          DO I=1,NE
            LPRW(KK+I)=LPRW(K+I)
          ENDDO
          K  = K+NE
          KK = KK+NE
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  INIVCHK                       source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        RBE2_IMPD                     source/constraints/general/kinchk.F
Chd|        RBE3_IMPD                     source/constraints/general/kinchk.F
Chd|        RBY_V0                        source/constraints/general/kinchk.F
Chd|====================================================================
      SUBROUTINE INIVCHK(IKINE,RWL,ITAB,NPRW,LPRW,KINET,
     1                   NPBY, LPBY,IRBE2,LRBE2,IRBE3,LRBE3,
     2                   FRBE3,X   ,SKEW ,V   ,VR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "lagmult.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKINE(*),NPRW(*), LPRW(*),ITAB(*),KINET(*),
     .        NPBY(NNPBY,*), LPBY(*),IRBE2(NRBE2L,*),LRBE2(*),
     .        IRBE3(NRBE3L,*),LRBE3(*)
      my_real
     .   RWL(NRWLP,*),V(3,*),VR(3,*),X(3,*),FRBE3(*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, KK, IKK, IKK2, NK, JWARN, ITYP, NE, NSL,
     .        IK(10), RBWARN, RB2WARN, KRB, NSLRB,ICDG,ISEN,M
C-----------------------------------------------------------------
C=======================================================================
C  CORRECTIONS AUTOMATIQUES INITIAL VELOCITIES
C=======================================================================
C should be done in hierarchy order: RBODY,RBE3,RBE2,int2...
C-----------------------------------------------------------------
      K=1
      DO I=1,NRBYKIN
        M  =NPBY(1,I)
        NSL=NPBY(2,I)
        ICDG=NPBY(3,I)
        ISEN=NPBY(4,I)
        IF (ISEN==0) THEN
          CALL RBY_V0(X  ,LPBY(K)  ,M   ,NSL  ,V     ,VR  )
         ENDIF
         K = K + NSL
      ENDDO
C-----------------------------------------------------------------
      DO I=NRBYKIN+1,NRBYLAG
        M  =NPBY(1,I)
        NSL=NPBY(2,I)
        ICDG=NPBY(3,I)
        ISEN=NPBY(4,I)
        IF (ISEN==0) THEN
          CALL RBY_V0(X  ,LPBY(K)  ,M   ,NSL  ,V     ,VR  )
         ENDIF
         K = K + 3*NSL
      ENDDO
C-----------------------------------------------------------------
      IF(NRBE3>0)THEN
       CALL RBE3_IMPD(IRBE3 ,LRBE3 ,X    ,V     ,VR    ,
     1                FRBE3  ,SKEW  )
      ENDIF
C-----------------------------------------------------------------
      IF(NRBE2>0)THEN
       CALL RBE2_IMPD(IRBE2 ,LRBE2 ,X    ,V     ,VR    ,
     1                SKEW  )
      ENDIF
C-----------------------------------------------------------------

      RETURN
      END
Chd|====================================================================
Chd|  RBY_V0                        source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        INIVCHK                       source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBY_V0(X  ,NOD  ,M   ,NSN  ,D     ,DR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), M,NSN
C     REAL
      my_real
     .   X(3,*), D(3,*),DR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C     REAL
      my_real
     .   XS,YS,ZS
C-----------------------------------------------
       DO I=1,NSN
        N = NOD(I)
        XS=X(1,N)-X(1,M)
        YS=X(2,N)-X(2,M)
        ZS=X(3,N)-X(3,M)
        D(1,N)=D(1,M)+DR(2,M)*ZS-DR(3,M)*YS
        D(2,N)=D(2,M)-DR(1,M)*ZS+DR(3,M)*XS
        D(3,N)=D(3,M)+DR(1,M)*YS-DR(2,M)*XS
        DR(1,N)= DR(1,M)
        DR(2,N)= DR(2,M)
        DR(3,N)= DR(3,M)
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_IMPD                     source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        INIVCHK                       source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/kinchk.F
Chd|        RBE3CL                        source/constraints/general/kinchk.F
Chd|====================================================================
      SUBROUTINE RBE3_IMPD(IRBE3 ,LRBE3 ,X    ,D     ,DR    ,
     1                     FRBE3 ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*)
C     REAL
      my_real
     .   X(3,*), D(3,*), DR(3,*),  FRBE3(*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,IROTG,
     .        JT(3,NRBE3),JR(3,NRBE3),NM,NN,K
C     REAL
      my_real
     .        VS(3),VRS(3)
      my_real,
     .         DIMENSION(:,:,:),ALLOCATABLE :: FDSTNB ,MDSTNB

C======================================================================|
      IADS = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ALLOCATE(FDSTNB(3,6,MAX_M))
      IF (IROTG>0) ALLOCATE(MDSTNB(3,6,MAX_M))
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        IF (NS==0) CYCLE
        NML = IRBE3(5,N)
          IROT =MIN(IRBE3(6,N),IRODDL)
        CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X    ,
     .              FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .              MDSTNB  )
        DO J = 1,3
           VS(J) = ZERO
           VRS(J) = ZERO
          ENDDO
        DO I=1,NML
         M = LRBE3(IAD+I)
         DO J = 1,3
          DO K = 1,3
             VS(J) = VS(J)+FDSTNB(K,J,I)*D(K,M)
             VRS(J) = VRS(J)+FDSTNB(K,J+3,I)*D(K,M)
            ENDDO
           ENDDO
        ENDDO
        IF (IROT>0) THEN
         DO I=1,NML
          M = LRBE3(IAD+I)
          DO J = 1,3
           DO K = 1,3
              VS(J) = VS(J)+MDSTNB(K,J,I)*DR(K,M)
              VRS(J) = VRS(J)+MDSTNB(K,J+3,I)*DR(K,M)
             ENDDO
            ENDDO
         ENDDO
        ENDIF
        DO J = 1,3
           D(J,NS) = VS(J) *JT(J,N)
          ENDDO
        IF ((JR(1,N)+JR(2,N)+JR(3,N))>0) THEN
         DO J = 1,3
            DR(J,NS) = VRS(J) *JR(J,N)
           ENDDO
        ENDIF
      ENDDO
C
      DEALLOCATE(FDSTNB)
      IF (IROTG>0) DEALLOCATE(MDSTNB)
C---
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE3                       source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        RBE3_IMPD                     source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),MAX_M , IROTG,JT(3,*)  ,JR(3,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N,NML,IC,ICT,ICR,IROT
C======================================================================|
      MAX_M=0
      IROTG=0
      DO N=1,NRBE3
          NML = IRBE3(5,N)
          IROT =IRBE3(6,N)
        MAX_M=MAX(MAX_M,NML)
        IROTG=MAX(IROTG,IROT)
          IC=IRBE3(4,N)
        ICT=IC/512
        ICR=(IC-512*(ICT))/64
          DO J =1,3
           JT(J,N)=0
           JR(J,N)=0
          ENDDO
        SELECT CASE (ICT)
          CASE(1)
           JT(3,N)=1
          CASE(2)
           JT(2,N)=1
          CASE(3)
           JT(2,N)=1
           JT(3,N)=1
          CASE(4)
           JT(1,N)=1
          CASE(5)
           JT(1,N)=1
           JT(3,N)=1
          CASE(6)
           JT(1,N)=1
           JT(2,N)=1
          CASE(7)
           JT(1,N)=1
           JT(2,N)=1
           JT(3,N)=1
       END SELECT
       SELECT CASE (ICR)
          CASE(1)
           JR(3,N)=1
          CASE(2)
           JR(2,N)=1
          CASE(3)
           JR(2,N)=1
           JR(3,N)=1
          CASE(4)
           JR(1,N)=1
          CASE(5)
           JR(1,N)=1
           JR(3,N)=1
          CASE(6)
           JR(1,N)=1
           JR(2,N)=1
          CASE(7)
           JR(1,N)=1
           JR(2,N)=1
           JR(3,N)=1
       END SELECT
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE3CL                        source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        RBE3_IMPD                     source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        INVERT                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3UF                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3UM                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        ZERO1                         source/constraints/general/rbe3/hm_read_rbe3.F
Chd|====================================================================
      SUBROUTINE RBE3CL(INRBE3  ,ILRBE3  ,NS     ,XYZ    ,FRBE3   ,
     .                  SKEW    ,NG      ,IROT   ,FDSTNB ,MDSTNB  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INRBE3(*),ILRBE3(*),NG, NS,IROT
C     REAL
      my_real
     .   XYZ(3,*), FRBE3(6,*), SKEW(LSKEW,*),FDSTNB(3,6,*), MDSTNB(3,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K,N, M ,NML, IAD,JJ,KG,NSNGLR,IELSUB,IERR,ng1
C     REAL
      my_real
     *        TW(3,NG), RW(3,NG),
     *        FUFXLC(3,NG), FUFYLC(3,NG), FUFZLC(3,NG),
     *        FUMXLC(3,NG), FUMYLC(3,NG), FUMZLC(3,NG),
     *        MXLC(3,NG), MYLC(3,NG), MZLC(3,NG),
     *        FUFX(3,NG), FUFY(3,NG), FUFZ(3,NG),
     *        MUFX(3,NG), MUFY(3,NG), MUFZ(3,NG),
     *        FUMX(3,NG), FUMY(3,NG), FUMZ(3,NG),
     *        MX(3,NG), MY(3,NG), MZ(3,NG),
     *        MUMX(3,NG), MUMY(3,NG), MUMZ(3,NG),
     *        EL(3,3,NG)
      my_real
     *                 DENFX, DENFY, DENFZ, DENMX, DENMY, DENMZ,
     *                 REFPT(3), CGMX(3), CGMY(3), CGMZ(3), AVEREF,
     *                 TFUFX(3), TFUFY(3), TFUFZ(3),
     *                 TMUFX(3), TMUFY(3), TMUFZ(3),
     *                 TFUMX(3), TFUMY(3), TFUMZ(3),
     *                 TMUMX(3), TMUMY(3), TMUMZ(3),
     *                 A(6,6), C(6,6), T(3,3)
C
C     INITIALIZATION
C
      CALL ZERO1(FDSTNB,3*NG*6)
      IF (IROT>0) CALL ZERO1(MDSTNB,3*NG*6)
      CALL ZERO1(A,36)
      CALL ZERO1(C,36)
      CALL ZERO1(CGMX,3)
      CALL ZERO1(CGMY,3)
      CALL ZERO1(CGMZ,3)
      IERR = 0
C
      REFPT(1) = XYZ(1,NS)
      REFPT(2) = XYZ(2,NS)
      REFPT(3) = XYZ(3,NS)
      DO K = 1, NG
            DO I = 1, 3
               TW(I,K) = FRBE3(I,K)
               RW(I,K) = FRBE3(I+3,K)
            ENDDO
      ENDDO
C
C     ERROR OUT IF RBE3 ELEMENT HAS TWO INDEPENDENT NODES WITH
C     NO ROTATIONAL WEIGHTS SET (THIS MEANS THE ELEMENT CANNOT
C     SUPPORT A MOMENT ALONG ITS AXIS)
C
      IF (NG == 2.AND.IROT==0) THEN
            IERR = 322
            GOTO 999
      ENDIF
C
C     CALCULATE DIRECTION COSINES OF LOCAL COORDINATE SYSTEMS, IF ANY
C
        DO K = 1, NG
            IELSUB = ILRBE3(K)
            IF (IELSUB > 0) THEN
               DO I = 1, 3
                     EL(I,1,K) = SKEW(I,IELSUB)
                     EL(I,2,K) = SKEW(I+3,IELSUB)
                     EL(I,3,K) = SKEW(I+6,IELSUB)
               ENDDO
            ENDIF
        ENDDO
C
C     DENOMINATORS FOR DISTRIBUTING FORCES (DENFX, DENFY AND DENFZ)
C
      DENFX = ZERO
      DENFY = ZERO
      DENFZ = ZERO
      AVEREF = ZERO
C
      DO 70 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           IF GRID POINT HAS A LOCAL COORDINATE SYSTEM
C
            DO 60 I = 1, 3
               DENFX = DENFX + TW(I,K)*EL(I,1,K)**2
               DENFY = DENFY + TW(I,K)*EL(I,2,K)**2
               DENFZ = DENFZ + TW(I,K)*EL(I,3,K)**2
 60         CONTINUE
         ELSE
            DENFX = DENFX + TW(1,K)
            DENFY = DENFY + TW(2,K)
            DENFZ = DENFZ + TW(3,K)
         END IF
C
            AVEREF = AVEREF + SQRT( (XYZ(1,KG) - REFPT(1))**2 +
     *                              (XYZ(2,KG) - REFPT(2))**2 +
     *                              (XYZ(3,KG) - REFPT(3))**2 )
 70   CONTINUE
C
      IF (ABS(DENFX) <= EM20) THEN
         IERR = 326
      ENDIF
C
      IF (ABS(DENFY) <= EM20) THEN
         IERR = 327
      ENDIF
C
      IF (ABS(DENFZ) <= EM20) THEN
         IERR = 328
      ENDIF
      IF (IERR /= 0) GOTO 999
         AVEREF = AVEREF/NG
         IF (AVEREF == ZERO) AVEREF = ONE
C
C     CALCULATE 3 CENTERS OF GRAVITY (CGMX, CGMY AND CGMZ) AND
C     DENOMINATORS FOR DISTRIBUTING MOMENTS (DENMX, DENMY AND DENMZ)
C
      DO 40 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           IF THERE IS A LOCAL COORDINATE SYSTEM AT THE GRID POINT
C
            DO 10 I = 1, 3
               CGMX(2) = CGMX(2) + TW(I,K)*EL(I,3,K)**2*XYZ(2,KG)
               CGMX(3) = CGMX(3) + TW(I,K)*EL(I,2,K)**2*XYZ(3,KG)
 10         CONTINUE
C
            DO 20 I = 1, 3
               CGMY(3) = CGMY(3) + TW(I,K)*EL(I,1,K)**2*XYZ(3,KG)
               CGMY(1) = CGMY(1) + TW(I,K)*EL(I,3,K)**2*XYZ(1,KG)
 20         CONTINUE
C
            DO 30 I = 1, 3
               CGMZ(1) = CGMZ(1) + TW(I,K)*EL(I,2,K)**2*XYZ(1,KG)
               CGMZ(2) = CGMZ(2) + TW(I,K)*EL(I,1,K)**2*XYZ(2,KG)
 30         CONTINUE
C
         ELSE
            CGMX(2) = CGMX(2) + TW(3,K)*XYZ(2,KG)
            CGMX(3) = CGMX(3) + TW(2,K)*XYZ(3,KG)
C
            CGMY(3) = CGMY(3) + TW(1,K)*XYZ(3,KG)
            CGMY(1) = CGMY(1) + TW(3,K)*XYZ(1,KG)
C
            CGMZ(1) = CGMZ(1) + TW(2,K)*XYZ(1,KG)
            CGMZ(2) = CGMZ(2) + TW(1,K)*XYZ(2,KG)
         END IF
 40   CONTINUE
         CGMX(2) = CGMX(2)/DENFZ
         CGMX(3) = CGMX(3)/DENFY
C
         CGMY(3) = CGMY(3)/DENFX
         CGMY(1) = CGMY(1)/DENFZ
C
         CGMZ(1) = CGMZ(1)/DENFY
         CGMZ(2) = CGMZ(2)/DENFX
C
      DENMX = ZERO
      DENMY = ZERO
      DENMZ = ZERO
C
      DO 90 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
C
C        NOTE: AS IMPLEMENTED IN NASTRAN 70.7, WE SCALE THE ROTATIONAL
C              WEIGHTS WITH THE SQUARE OF THE AVERAGE DISTANCE OF THE
C              INDEPENDENT GRID POINTS FROM THE REFERENCE POINT TO
C              RENDER THE RBE3 CALCULATIONS UNIT INDEPENDENT
C
         IF (IELSUB > 0) THEN
C
C           IF GRID POINT HAS A LOCAL COORDINATE SYSTEM
C
            DO 80 I = 1, 3
               DENMX = DENMX + RW(I,K)*EL(I,1,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,3,K)*(XYZ(2,KG) - CGMX(2)) -
     *                           EL(I,2,K)*(XYZ(3,KG) - CGMX(3))
     *                         ) **2
               DENMY = DENMY + RW(I,K)*EL(I,2,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,1,K)*(XYZ(3,KG) - CGMY(3)) -
     *                           EL(I,3,K)*(XYZ(1,KG) - CGMY(1))
     *                         ) **2
               DENMZ = DENMZ + RW(I,K)*EL(I,3,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,2,K)*(XYZ(1,KG) - CGMZ(1)) -
     *                           EL(I,1,K)*(XYZ(2,KG) - CGMZ(2))
     *                         ) **2
 80         CONTINUE
         ELSE
            DENMX = DENMX + RW(1,K)*AVEREF**2 +
     *                      TW(2,K)*(XYZ(3,KG) - CGMX(3))**2 +
     *                      TW(3,K)*(XYZ(2,KG) - CGMX(2))**2
            DENMY = DENMY + RW(2,K)*AVEREF**2 +
     *                      TW(1,K)*(XYZ(3,KG) - CGMY(3))**2 +
     *                      TW(3,K)*(XYZ(1,KG) - CGMY(1))**2
            DENMZ = DENMZ + RW(3,K)*AVEREF**2 +
     *                      TW(2,K)*(XYZ(1,KG) - CGMZ(1))**2 +
     *                      TW(1,K)*(XYZ(2,KG) - CGMZ(2))**2
         END IF
 90   CONTINUE
C
C     PERFORM SOME CHECKS ON WEIGHTS, TO MAKE SURE THAT THE RBE3
C     ELEMENT HAS NO UNCONSTRAINED DEGREES OF FREEDOM
C
C
      IF (ABS(DENMX) <= EM20) THEN
         IERR = 329
      ENDIF
C
      IF (ABS(DENMY) <= EM20) THEN
         IERR = 330
      ENDIF
C
      IF (ABS(DENMZ) <= EM20) THEN
         IERR = 331
      ENDIF
C
      IF (IERR /= 0) GOTO 999
C
C     CALCULATE 3 FORCE DISTRIBUTIONS THAT CREATE NET X, Y AND Z FORCES
C     OF 1 (BESIDES OTHER NONZERO FORCES/MOMENTS IN ALL THE DIRECTIONS)
C
      CALL RBE3UF(INRBE3,ILRBE3,EL,TW,XYZ,REFPT,
     *            FUFXLC,FUFYLC,FUFZLC,FUFX,FUFY,FUFZ,MUFX,MUFY,MUFZ,
     *            TFUFX,TFUFY,TFUFZ,TMUFX,TMUFY,TMUFZ,
     *            DENFX,DENFY,DENFZ,NG)
C
C     CALCULATE 3 MOMENT/FORCE DISTRIBUTIONS THAT CREATE NET X, Y AND Z
C     MOMENTS OF 1 (BESIDES OTHER NONZERO FORCES/MOMENTS IN ALL THE
C     DIRECTIONS) AT CGMX, CGMY AND CGMZ RESPECTIVELY
C
      CALL RBE3UM(INRBE3,ILRBE3,EL,TW,RW,XYZ,REFPT,CGMX,CGMY,CGMZ,
     *            FUMXLC,FUMYLC,FUMZLC,MXLC,MYLC,MZLC,
     *            FUMX,FUMY,FUMZ,MX,MY,MZ,MUMX,MUMY,MUMZ,
     *            TFUMX,TFUMY,TFUMZ,TMUMX,TMUMY,TMUMZ,
     *            AVEREF,DENMX,DENMY,DENMZ,NG,IROT )
C
C     DETERMINE COMBINATORY COEFFICIENTS FOR THESE 6 DISTRIBUTIONS
C     (6 COEFFICIENTS FOR EACH OF 6 CASES)
C
C     CASE 1 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT X-FORCE AT REFERENCE POINT
C     CASE 2 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Y-FORCE AT REFERENCE POINT
C     CASE 3 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Z-FORCE AT REFERENCE POINT
C     CASE 4 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT X-MOMENT AT REFERENCE POINT
C     CASE 5 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Y-MOMENT AT REFERENCE POINT
C     CASE 6 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Z-MOMENT AT REFERENCE POINT
C
C     IN ORDER TO DETERMINE THESE COEFFICIENTS, FIRST SET UP A 6X6
C     MATRIX.  THE 6 COLUMNS OF THE INVERSE OF THIS MATRIX ARE THE
C     DESIRED 6 SETS OF COEFFICIENTS.
C
      DO 120 I = 1, 3
         K = I + 3
         A(I,1) = TFUFX(I)
         A(K,1) = TMUFX(I)
         A(I,2) = TFUFY(I)
         A(K,2) = TMUFY(I)
         A(I,3) = TFUFZ(I)
         A(K,3) = TMUFZ(I)
         A(I,4) = TFUMX(I)
         A(K,4) = TMUMX(I)
         A(I,5) = TFUMY(I)
         A(K,5) = TMUMY(I)
         A(I,6) = TFUMZ(I)
         A(K,6) = TMUMZ(I)
 120  CONTINUE
C
C     INVERT THE 6X6 MATRIX
C
      NSNGLR  = 0
      CALL INVERT(A,C,6,NSNGLR)
      IF (NSNGLR /= 0) THEN
        IERR = 332
         GOTO 999
      ENDIF
C
      DO I = 1, 3
       DO J = 1, 6
         DO K = 1, NG
               FDSTNB(I,J,K) = C(1,J)*FUFX(I,K) + C(2,J)*FUFY(I,K) +
     *                         C(3,J)*FUFZ(I,K) + C(4,J)*FUMX(I,K) +
     *                         C(5,J)*FUMY(I,K) + C(6,J)*FUMZ(I,K)
         ENDDO
       ENDDO
      ENDDO
      IF (IROT>0) THEN
       DO I = 1, 3
        DO J = 1, 6
         DO K = 1, NG
               MDSTNB(I,J,K) = C(4,J)*MX(I,K) + C(5,J)*MY(I,K) +
     *                         C(6,J)*MZ(I,K)
         ENDDO
        ENDDO
       ENDDO
      END IF
C
 999  CONTINUE
      IF (IERR>0) THEN
             WRITE(ISTDO,'(A,I10)')
     .        ' ** ERROR IN RBE3 CALCULATION ID=',IERR
             WRITE(IOUT,'(A,I10)')
     .        ' ** ERROR IN RBE3 CALCULATION ID=',IERR
           CALL ARRET(2)
      ENDIF
C
C     DIAGNOSTIC INFORMATION
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_IMPD                     source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        INIVCHK                       source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/kinchk.F
Chd|        RBE2D0                        source/constraints/general/kinchk.F
Chd|        RBE2DL                        source/constraints/general/kinchk.F
Chd|====================================================================
      SUBROUTINE RBE2_IMPD(IRBE2 ,LRBE2 ,X    ,D     ,DR    ,SKEW )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*)
C     REAL
      my_real
     .   X(3,*), D(3,*), DR(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,ISK,
     .        JT(3,NRBE2),JR(3,NRBE2),NM,NN,K,NSL
C     REAL
C======================================================================|
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      DO N=NRBE2,1,-1
        IAD = IRBE2(1,N)
        M  = IRBE2(3,N)
        NSL = IRBE2(5,N)
        ISK = IRBE2(7,N)
          IF (ISK>1) THEN
            CALL RBE2DL(NSL   ,LRBE2(IAD+1),X     ,D     ,DR    ,
     1                  JT(1,N),JR(1,N),M     ,SKEW(1,ISK))
        ELSE
            CALL RBE2D0(NSL   ,LRBE2(IAD+1),X     ,D     ,DR    ,
     1                  JT(1,N),JR(1,N),M     )
        END IF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE2                       source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        RBE2_IMPD                     source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE2(IRBE2 ,JT  ,JR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),JT(3,*)  ,JR(3,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N,NML,IC,ICT,ICR,IROT
C======================================================================|
      DO N=1,NRBE2
          IC=IRBE2(4,N)
        ICT=IC/512
        ICR=(IC-512*(ICT))/64
          IF (IRODDL==0) ICR =0
          DO J =1,3
           JT(J,N)=0
           JR(J,N)=0
          ENDDO
        SELECT CASE (ICT)
          CASE(1)
           JT(3,N)=1
          CASE(2)
           JT(2,N)=1
          CASE(3)
           JT(2,N)=1
           JT(3,N)=1
          CASE(4)
           JT(1,N)=1
          CASE(5)
           JT(1,N)=1
           JT(3,N)=1
          CASE(6)
           JT(1,N)=1
           JT(2,N)=1
          CASE(7)
           JT(1,N)=1
           JT(2,N)=1
           JT(3,N)=1
       END SELECT
       SELECT CASE (ICR)
          CASE(1)
           JR(3,N)=1
          CASE(2)
           JR(2,N)=1
          CASE(3)
           JR(2,N)=1
           JR(3,N)=1
          CASE(4)
           JR(1,N)=1
          CASE(5)
           JR(1,N)=1
           JR(3,N)=1
          CASE(6)
           JR(1,N)=1
           JR(2,N)=1
          CASE(7)
           JR(1,N)=1
           JR(2,N)=1
           JR(3,N)=1
       END SELECT
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2D0                        source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        RBE2_IMPD                     source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2D0(NSL   ,ISL   ,X     ,V     ,VR    ,
     1                  JT    ,JR    ,M     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),JT(3),JR(3),M
C     REAL
      my_real
     .   X(3,*), V(3,*), VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS
C     REAL
      my_real
     .     XS, YS, ZS,VRM(3)
C======================================================================|
        DO J = 1,3
           IF (JT(J)/=0) THEN
          DO I=1,NSL
           NS = ISL(I)
             V(J,NS)= V(J,M)
            ENDDO
         ENDIF
        ENDDO
       IF ((JR(1)+JR(2)+JR(3))>0) THEN
        DO J = 1,3
           IF (JR(J)/=0) THEN
          DO I=1,NSL
           NS = ISL(I)
             VR(J,NS)= VR(J,M)
            ENDDO
         ENDIF
           VRM(J)= VR(J,M)*JR(J)
        ENDDO
        DO I=1,NSL
         NS = ISL(I)
         XS=X(1,NS)-X(1,M)
         YS=X(2,NS)-X(2,M)
         ZS=X(3,NS)-X(3,M)
         V(1,NS)=V(1,NS)+VRM(2)*ZS-VRM(3)*YS
         V(2,NS)=V(2,NS)-VRM(1)*ZS+VRM(3)*XS
         V(3,NS)=V(3,NS)+VRM(1)*YS-VRM(2)*XS
        ENDDO
       END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2DL                        source/constraints/general/kinchk.F
Chd|-- called by -----------
Chd|        RBE2_IMPD                     source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2DL(NSN   ,ISL   ,X     ,V     ,VR    ,
     1                  JT    ,JR    ,M     ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,ISL(*),JT(3),JR(3),M
C     REAL
      my_real
     .   X(3,*), V(3,*), VR(3,*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NS
C     REAL
      my_real
     .   XS, YS, ZS,RX, RY,RZ,LRX, LRY,LRZ,RVX,RVY,RVZ,
     .   DVX,DVY,DVZ,VVX,VVY,VVZ,LXS(NSN), LYS(NSN), LZS(NSN)
C======================================================================|
      DO I=1,NSN
        NS = ISL(I)
        DVX  =V(1,NS)-V(1,M)
        DVY  =V(2,NS)-V(2,M)
        DVZ  =V(3,NS)-V(3,M)
        VVX  =JT(1)*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =JT(2)*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =JT(3)*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        V(1,NS) =V(1,NS)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        V(2,NS) =V(2,NS)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        V(3,NS) =V(3,NS)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
      ENDDO
      IF ((JR(1)+JR(2)+JR(3))>0) THEN
       DO I=1,NSN
        NS = ISL(I)
        XS=X(1,NS)-X(1,M)
        YS=X(2,NS)-X(2,M)
        ZS=X(3,NS)-X(3,M)
        LXS(I)=SKEW(1)*XS+SKEW(2)*YS+SKEW(3)*ZS
        LYS(I)=SKEW(4)*XS+SKEW(5)*YS+SKEW(6)*ZS
        LZS(I)=SKEW(7)*XS+SKEW(8)*YS+SKEW(9)*ZS
       ENDDO
       DO I=1,NSN
        NS = ISL(I)
        DVX  =VR(1,NS)-VR(1,M)
        DVY  =VR(2,NS)-VR(2,M)
        DVZ  =VR(3,NS)-VR(3,M)
        VVX  =JR(1)*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =JR(2)*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =JR(3)*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        VR(1,NS) =VR(1,NS)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        VR(2,NS) =VR(2,NS)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        VR(3,NS) =VR(3,NS)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
        RX=VR(1,M)
        RY=VR(2,M)
        RZ=VR(3,M)
        LRX =JR(1)*(SKEW(1)*RX+SKEW(2)*RY+SKEW(3)*RZ)
        LRY =JR(2)*(SKEW(4)*RX+SKEW(5)*RY+SKEW(6)*RZ)
        LRZ =JR(3)*(SKEW(7)*RX+SKEW(8)*RY+SKEW(9)*RZ)
        RVX=LRY*LZS(I)-LRZ*LYS(I)
        RVY=LRX*LZS(I)+LRZ*LXS(I)
        RVZ=LRX*LYS(I)-LRY*LXS(I)
        V(1,NS) =V(1,NS)+RVX*SKEW(1)+RVY*SKEW(4)+RVZ*SKEW(7)
        V(2,NS) =V(2,NS)+RVX*SKEW(2)+RVY*SKEW(5)+RVZ*SKEW(8)
        V(3,NS) =V(3,NS)+RVX*SKEW(3)+RVY*SKEW(6)+RVZ*SKEW(9)
       ENDDO
      END IF
C---
      RETURN
      END
