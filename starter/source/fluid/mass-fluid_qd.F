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
Chd|  MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F
Chd|-- called by -----------
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INTANL_QD                     source/fluid/intanl_qd.F
Chd|        INTANL_TG                     source/fluid/intanl_tg.F
Chd|        INTGQD                        source/fluid/inte_qd.F
Chd|        INTGTG                        source/fluid/inte_tg.F
Chd|        INTHQD                        source/fluid/inte_qd.F
Chd|        INTHTG                        source/fluid/inte_tg.F
Chd|        INVERT                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MASS_FLUID_QD(NNO, NEL, IFLOW, IBUF, ELEM, X,
     .                         NORMAL, AF, MFLE, CBEM, RHO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NNO, NEL, IFLOW(*), IBUF(*), ELEM(5,*)
        my_real X(3,*), AF(*), NORMAL(3,*), MFLE(NEL,*), CBEM(NEL,*), RHO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IFORM, KFORM, ILVOUT, NELMAX
        INTEGER IN, JN, KN, N1, N2, N3, N4, N5, NN1, NN2, NN3, NN4, NNJ, IEL, JEL, ERR
        my_real X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4, XQ, YQ, ZQ, R2,
     .          XP, YP, ZP, X13, Y13, Z13, X24, Y24, Z24,
     .          NRX, NRY, NRZ,  AREA, D2, RVAL, RVLH, RVLG
        my_real MASSX, MASSY, MASSZ, WI(4,2), SUM
        my_real, DIMENSION(:,:), ALLOCATABLE :: BBEM, EBEM
        my_real :: ALPHA, BETA
        INTEGER :: AERR
        IFORM  = IFLOW(13)
        ILVOUT = IFLOW(17)
        KFORM  = IFLOW(23)
        NELMAX = IFLOW(28)
C-------------------------------------------------------------------
C 1- Compute Bij Cij and Ai
C-------------------------------------------------------------------
        IF (ILVOUT>=1) THEN
          WRITE(ISTDO,'(A)') ' .. FLUID MASS MATRIX:
     . ASSEMBLY OF INTEGRAL OPERATORS'
        ENDIF
C Collocation BEM
        WI(1,1)=FOURTH
        WI(2,1)=FOURTH
        WI(3,1)=FOURTH
        WI(4,1)=FOURTH
        WI(1,2)=THIRD
        WI(2,2)=THIRD
        WI(3,2)=ONE_OVER_6
        WI(4,2)=ONE_OVER_6
        ALLOCATE(BBEM(NEL,NEL), STAT = AERR)
        IF (AERR /= 0) THEN
          CALL ANCMSG(MSGID = 1710, ANMODE=ANINFO, MSGTYPE = MSGERROR)
        ENDIF
        DO IN=1,NEL
          DO JN=1,NEL
            BBEM(IN,JN)=ZERO
            CBEM(IN,JN)=ZERO
          ENDDO
        ENDDO

        IF(IFORM == 1) THEN
C Gauss integration
          DO IEL=1,NEL
C           IF (ILVOUT>=2) CALL PROGBAR_C(IEL,NEL)
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            N4=ELEM(4,IEL)
            N5=ELEM(5,IEL)
            NN1=IBUF(N1)
            NN2=IBUF(N2)
            NN3=IBUF(N3)
            NN4=IBUF(N4)
            X1=X(1,NN1)
            X2=X(1,NN2)
            X3=X(1,NN3)
            X4=X(1,NN4)
            Y1=X(2,NN1)
            Y2=X(2,NN2)
            Y3=X(2,NN3)
            Y4=X(2,NN4)
            Z1=X(3,NN1)
            Z2=X(3,NN2)
            Z3=X(3,NN3)
            Z4=X(3,NN4)
C  Control point P
            XP=WI(1,N5)*X1+WI(2,N5)*X2+WI(3,N5)*X3+WI(4,N5)*X4
            YP=WI(1,N5)*Y1+WI(2,N5)*Y2+WI(3,N5)*Y3+WI(4,N5)*Y4
            ZP=WI(1,N5)*Z1+WI(2,N5)*Z2+WI(3,N5)*Z3+WI(4,N5)*Z4
            D2=MIN((XP-X1)**2+(YP-Y1)**2+(ZP-Z1)**2,
     .             (XP-X2)**2+(YP-Y2)**2+(ZP-Z2)**2,
     .             (XP-X3)**2+(YP-Y3)**2+(ZP-Z3)**2,
     .             (XP-X4)**2+(YP-Y4)**2+(ZP-Z4)**2)
            NRX=NORMAL(1,IEL)
            NRY=NORMAL(2,IEL)
            NRZ=NORMAL(3,IEL)
C
            DO JEL=1,NEL
              IF(IEL == JEL) THEN
                BBEM(IEL,JEL)=TWO*SQRT(PI*AF(JEL))
                CBEM(IEL,JEL)=TWO*PI
              ELSE
                N1=ELEM(1,JEL)
                N2=ELEM(2,JEL)
                N3=ELEM(3,JEL)
                N4=ELEM(4,JEL)
                JN=ELEM(5,JEL)
                NN1=IBUF(N1)
                NN2=IBUF(N2)
                NN3=IBUF(N3)
                NN4=IBUF(N4)
                X1=X(1,NN1)
                X2=X(1,NN2)
                X3=X(1,NN3)
                X4=X(1,NN4)
                Y1=X(2,NN1)
                Y2=X(2,NN2)
                Y3=X(2,NN3)
                Y4=X(2,NN4)
                Z1=X(3,NN1)
                Z2=X(3,NN2)
                Z3=X(3,NN3)
                Z4=X(3,NN4)
                AREA=AF(JEL)
C  Position de la source Q
                XQ=WI(1,JN)*X1+WI(2,JN)*X2+WI(3,JN)*X3+WI(4,JN)*X4
                YQ=WI(1,JN)*Y1+WI(2,JN)*Y2+WI(3,JN)*Y3+WI(4,JN)*Y4
                ZQ=WI(1,JN)*Z1+WI(2,JN)*Z2+WI(3,JN)*Z3+WI(4,JN)*Z4

                IF(JN == 1) THEN
                  CALL INTHQD(X1 , Y1,  Z1,  X2,  Y2,  Z2,
     .                        X3,  Y3,  Z3,  X4,  Y4,  Z4,
     .                        XP,  YP,  ZP,  XQ,  YQ,  ZQ,
     .                        NRX, NRY, NRZ, D2, AREA, RVAL)
                  CBEM(IEL,JEL)=RVAL
                  CALL INTGQD(X1 , Y1,  Z1,  X2,  Y2,  Z2,
     .                        X3,  Y3,  Z3,  X4,  Y4,  Z4,
     .                        XP,  YP,  ZP,  XQ,  YQ,  ZQ,
     .                        D2, AREA, RVAL)
                  BBEM(IEL,JEL)=RVAL
                ELSEIF(JN == 2) THEN
                  CALL INTHTG(X1 , Y1,  Z1,  X2,  Y2,  Z2,
     .                        X3,  Y3,  Z3,  XP,  YP,  ZP,
     .                        NRX,NRY, NRZ,  D2,  AREA,
     .                        XQ,  YQ,  ZQ, RVAL )
                  CBEM(IEL,JEL)=RVAL
                  CALL INTGTG(X1,  Y1,  Z1,  X2,  Y2,  Z2,
     .                        X3,  Y3,  Z3,  XP,  YP,  ZP,
     .                        D2,  AREA,
     .                        XQ,  YQ,  ZQ, RVAL )
                  BBEM(IEL,JEL)=RVAL
                ENDIF
              ENDIF
            ENDDO
          ENDDO

        ELSEIF(IFORM == 2) THEN
C Integration analytique
          DO IEL=1,NEL
C           IF (ILVOUT>=2) CALL PROGBAR_C(IEL,NEL)
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            N4=ELEM(4,IEL)
            IN=ELEM(5,IEL)
            NN1=IBUF(N1)
            NN2=IBUF(N2)
            NN3=IBUF(N3)
            NN4=IBUF(N4)
            X1=X(1,NN1)
            X2=X(1,NN2)
            X3=X(1,NN3)
            X4=X(1,NN4)
            Y1=X(2,NN1)
            Y2=X(2,NN2)
            Y3=X(2,NN3)
            Y4=X(2,NN4)
            Z1=X(3,NN1)
            Z2=X(3,NN2)
            Z3=X(3,NN3)
            Z4=X(3,NN4)
C  Control point P
            XP=WI(1,IN)*X1+WI(2,IN)*X2+WI(3,IN)*X3+WI(4,IN)*X4
            YP=WI(1,IN)*Y1+WI(2,IN)*Y2+WI(3,IN)*Y3+WI(4,IN)*Y4
            ZP=WI(1,IN)*Z1+WI(2,IN)*Z2+WI(3,IN)*Z3+WI(4,IN)*Z4
            DO JEL=1,NEL
              N1=ELEM(1,JEL)
              N2=ELEM(2,JEL)
              N3=ELEM(3,JEL)
              N4=ELEM(4,JEL)
              JN=ELEM(5,JEL)
              NN1=IBUF(N1)
              NN2=IBUF(N2)
              NN3=IBUF(N3)
              NN4=IBUF(N4)
              X1=X(1,NN1)
              X2=X(1,NN2)
              X3=X(1,NN3)
              X4=X(1,NN4)
              Y1=X(2,NN1)
              Y2=X(2,NN2)
              Y3=X(2,NN3)
              Y4=X(2,NN4)
              Z1=X(3,NN1)
              Z2=X(3,NN2)
              Z3=X(3,NN3)
              Z4=X(3,NN4)
              NRX=NORMAL(1,JEL)
              NRY=NORMAL(2,JEL)
              NRZ=NORMAL(3,JEL)
              AREA=AF(JEL)
              IF(JN == 1) THEN
                CALL INTANL_QD(X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .                         X4, Y4, Z4, XP, YP, ZP, NRX,NRY,NRZ,
     .                         AREA, RVLH, RVLG, IEL, JEL )

              ELSEIF(JN == 2) THEN
                CALL INTANL_TG(X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .                         XP, YP, ZP, NRX,NRY,NRZ,
     .                         AREA, RVLH, RVLG, IEL, JEL )
              ENDIF
C Matrice C=H
              IF(IEL == JEL) THEN
                CBEM(IEL,JEL)=TWO*PI
              ELSE
                CBEM(IEL,JEL)=RVLH
              ENDIF
C Matrice B=G
              BBEM(IEL,JEL)=RVLG
            ENDDO
          ENDDO
        ENDIF

        IF(ILVOUT>=3) THEN
          WRITE (*,'(//,A)') ' BBEM MATRIX'
          IF(NEL < 11) THEN
            DO IN = 1,NEL
              WRITE (*,'(10E13.5)')  (BBEM(IN,JN),JN=1,NEL)
            ENDDO
          ELSE
            DO IN = 1,10
              WRITE (*,'(10E13.5)')  (BBEM(IN,JN),JN=1,10)
            ENDDO
            WRITE (*,'(//,A)') ' BBEM MATRIX B1I'
            WRITE (*,'(10E13.5)')  (BBEM(1,JN),JN=1,NEL)
          ENDIF

          WRITE (*,'(//,A)') ' CBEM MATRIX'
          IF(NEL < 11) THEN
            DO IN = 1,NEL
              WRITE (*,'(10E13.5)')  (CBEM(IN,JN),JN=1,NEL)
            ENDDO
          ELSE
            DO IN = 1,10
              WRITE (*,'(10E13.5)')  (CBEM(IN,JN),JN=1,10)
            ENDDO
            WRITE (*,'(//,A)') ' CBEM MATRIX C1I'
            WRITE (*,'(10E13.5)')  (CBEM(1,JN),JN=1,NEL)
          ENDIF
        ENDIF
C------------------------------------------------------------------------
C 2- Compute matrix MFLE = CBEMt * BBEM + BBEMt * CBEM (Case DAA KFORM=1)
C------------------------------------------------------------------------
        IF(KFORM == 1 .AND. NEL >= NELMAX) THEN
c         MFLE  = CBEMt*BBEM ! DGEMM C := alpha*op( A )*op( B ) +beta*C
C                              C = MFLE ; beta = 0
C                              op = T A = CBEM
C                              op = N , B = BBEM
c         MFLE = MFLE + BBEMt*CBEM
C                              C = MFLE ; beta = 1
C                              op = N A = BBEMt
C                              op = T , B = CBEM

C         CMAT(1:NEL,1:NEL) = TRANSPOSE(BBEM(1:NEL,1:NEL))
C         MFLE(1:NEL,1:NEL) = MATMUL(CMAT(1:NEL,1:NEL),CBEM(1:NEL,1:NEL))
C         CMAT(1:NEL,1:NEL) = TRANSPOSE(CBEM(1:NEL,1:NEL))
C         MFLE(1:NEL,1:NEL) = MFLE(1:NEL,1:NEL) + MATMUL(CMAT(1:NEL,1:NEL),BBEM(1:NEL,1:NEL))
C         CMAT(1:NEL,1:NEL) = CBEM(1:NEL,1:NEL)
#if defined(mkl)
C #warning "MKL used: DAA will be fast"

          ALPHA = 1.0D0
          BETA = 0.0D0
          CALL DGEMM('T','N',NEL,NEL,NEL,ALPHA,CBEM,NEL,BBEM,NEL,BETA,MFLE,NEL)
          ALPHA = 1.0D0
          BETA = 1.0D0
          CALL DGEMM('N','T',NEL,NEL,NEL,ALPHA,BBEM,NEL,CBEM,NEL,BETA,MFLE,NEL)
#else
C #warning "MKL not used: DAA will be slow"
          DO JN=1,NEL
            DO IN=1,NEL
              SUM=ZERO
              DO KN=1,NEL
                SUM=SUM+BBEM(KN,IN)*CBEM(KN,JN)+CBEM(KN,IN)*BBEM(KN,JN)
              ENDDO
              MFLE(IN,JN)=SUM
            ENDDO
          ENDDO
#endif
          RETURN
        ENDIF
C---------------------------------------------------
C 2- Compute fluid mass matrix MFLE
C---------------------------------------------------
        IF (ILVOUT>=1) WRITE(ISTDO,'(A)') ' .. FLUID MASS MATRIX'
        ALLOCATE(EBEM(NEL,NEL), STAT = AERR)
        IF (AERR /= 0) THEN
          CALL ANCMSG(MSGID = 1710, ANMODE=ANINFO, MSGTYPE = MSGERROR)
        ENDIF
        CALL INVERT(CBEM,EBEM,NEL,ERR)

        IF(ILVOUT>=3) THEN
          WRITE (*,'(//,A)') ' CBEM-1 MATRIX'
          IF(NEL < 11) THEN
            DO IN = 1,NEL
              WRITE (*,'(10E13.5)')  (EBEM(IN,JN),JN=1,NEL)
            END DO
          ELSE
            DO IN = 1,10
              WRITE (*,'(10E13.5)')  (EBEM(IN,JN),JN=1,10)
            END DO
            WRITE (*,'(//,A)') ' CBEM-1 MATRIX C1I'
            WRITE (*,'(10E13.5)')  (EBEM(1,JN),JN=1,NEL)
          ENDIF
        ENDIF

        DO IN=1,NEL
          DO JN=1,NEL
            MFLE(IN,JN)=ZERO
            CBEM(IN,JN)=ZERO
          ENDDO
        ENDDO
        CBEM(1:NEL, 1:NEL) = MATMUL(BBEM(1:NEL, 1:NEL), EBEM(1:NEL, 1:NEL))
c$$$      DO IN=1,NEL
c$$$         DO JN=1,NEL
c$$$            DO KN=1,NEL
c$$$               CBEM(IN,JN)=CBEM(IN,JN)+BBEM(IN,KN)*EBEM(KN,JN)
c$$$            ENDDO
c$$$         ENDDO
c$$$      ENDDO

        IF(ILVOUT>=3) THEN
          WRITE (*,'(//,A)') ' EBEM MATRIX = BBEM*CBEM-1'
          IF(NEL < 11) THEN
            DO IN = 1,NEL
              WRITE (*,'(10E13.5)')  (CBEM(IN,JN),JN=1,NEL)
            ENDDO
          ELSE
            DO IN = 1,10
              WRITE (*,'(10E13.5)')  (CBEM(IN,JN),JN=1,10)
            ENDDO
            WRITE (*,'(//,A)') ' EBEM MATRIX E1I'
            WRITE (*,'(10E13.5)')  (CBEM(1,JN),JN=1,NEL)
          ENDIF
        ENDIF

        DO IN=1,NEL
          DO JN=1,NEL
            MFLE(IN,JN)=HALF*RHO*AF(IN)*(CBEM(IN,JN)+CBEM(JN,IN))
            BBEM(IN,JN)=MFLE(IN,JN)
          ENDDO
        ENDDO
C
        IF(ILVOUT>=3) THEN
          WRITE (*,'(//,A)') ' FLUID MASS MATRIX'
          IF(NEL < 11) THEN
            DO IN = 1,NEL
              WRITE (*,'(10E13.5)')  (MFLE(IN,JN),JN=1,NEL)
            END DO
          ELSE
            DO IN = 1,10
              WRITE (*,'(10E13.5)')  (MFLE(IN,JN),JN=1,10)
            END DO
            WRITE (*,'(//,A)') ' FLUID MASS MATRIX M1I'
            WRITE (*,'(10E13.5)')  (MFLE(1,JN),JN=1,NEL)
          ENDIF
        ENDIF
C---------------------------------
C     COMPUTE RIGD BODY FLUID MASS
C---------------------------------
        MASSX=ZERO
        MASSY=ZERO
        MASSZ=ZERO
        DO IN=1,NEL
          DO JN=1,NEL
            MASSX=MASSX+NORMAL(1,IN)*MFLE(IN,JN)*NORMAL(1,JN)
            MASSY=MASSY+NORMAL(2,IN)*MFLE(IN,JN)*NORMAL(2,JN)
            MASSZ=MASSZ+NORMAL(3,IN)*MFLE(IN,JN)*NORMAL(3,JN)
          ENDDO
        ENDDO
        WRITE (IOUT,'(/7X,A,E13.5)')  'DAA : RIGID BODY FLUID MASS XX', MASSX
        WRITE (IOUT,'( 7X,A,E13.5)')  'DAA : RIGID BODY FLUID MASS YY', MASSY
        WRITE (IOUT,'( 7X,A,E13.5)')  'DAA : RIGID BODY FLUID MASS ZZ', MASSZ

C---------------------------------------------------
C 3- Compute inverse of fluid mass matrix
C---------------------------------------------------
        IF(KFORM==1) THEN
          DO IN=1,NEL
            DO JN=1,NEL
              EBEM(IN,JN)=ZERO
            ENDDO
          ENDDO
          CALL INVERT(BBEM,EBEM,NEL,ERR)

C
          IF(ILVOUT>=3) THEN
            WRITE (*,'(//,A)') ' INVERSE FLUID MASS MATRIX'
            IF(NEL < 11) THEN
              DO IN = 1,NEL
                WRITE (*,'(10E13.5)')  (EBEM(IN,JN),JN=1,NEL)
              END DO
            ELSE
              DO IN = 1,10
                WRITE (*,'(10E13.5)')  (EBEM(IN,JN),JN=1,10)
              END DO
              WRITE (*,'(//,A)') ' INVERSE FLUID MASS MATRIX MII'
              WRITE (*,'(10E13.5)')  (EBEM(JN,JN),JN=1,NEL)
            ENDIF
          ENDIF

          DO IN=1,NEL
            DO JN=1,NEL
              MFLE(IN,JN)=EBEM(IN,JN)
            ENDDO
          ENDDO
        ENDIF
        IF (ALLOCATED(BBEM)) DEALLOCATE(BBEM)
        IF (ALLOCATED(EBEM))DEALLOCATE(EBEM)

        RETURN
      END
