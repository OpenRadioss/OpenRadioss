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
Chd|  MASS_FLUID_TG                 source/fluid/mass-fluid_tg.F  
Chd|-- called by -----------
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INTANL_TG                     source/fluid/intanl_tg.F      
Chd|        INTGTG                        source/fluid/inte_tg.F        
Chd|        INTHTG                        source/fluid/inte_tg.F        
Chd|        INVERT                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MASS_FLUID_TG(IFORM, ILVOUT, NNO, NEL, IBUF, ELEM, X, 
     .                         NORMAL, AF, MFLE, RHO)
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
      INTEGER IFORM, ILVOUT, NNO, NEL, IBUF(*),  ELEM(3,*)
      my_real X(3,*), AF(*), NORMAL(3,*), MFLE(NEL,*), RHO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IN, JN, KN, N1, N2, N3, NN1, NN2, NN3, NNJ, IEL, JEL, ERR
      my_real X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, XQ, YQ, ZQ, R2,
     .        XP, YP, ZP, X12, Y12, Z12, X13, Y13, Z13,	  
     .        NRX, NRY, NRZ, D2, AREA, RVAL, RVLH, RVLG
      my_real MASSX, MASSY, MASSZ, NORM
      my_real, DIMENSION(:,:), ALLOCATABLE :: BBEM, CBEM, EBEM 
      INTEGER :: AERR
C-------------------------------------------------------------------
C 1- Compute Bij Cij
C-------------------------------------------------------------------
      IF (ILVOUT>=1) WRITE(ISTDO,'(A)') ' .. FLUID MASS MATRIX : ASSEMBLY OF INTEGRAL OPERATORS'

      ALLOCATE(BBEM(NEL,NEL), STAT = AERR)
      IF (AERR /= 0) THEN
         CALL ANCMSG(MSGID = 1710, ANMODE=ANINFO, MSGTYPE = MSGERROR) 
      ENDIF
      ALLOCATE(CBEM(NEL,NEL), STAT = AERR)
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
C Collocation BEM - Gauss integration
        DO IEL=1,NEL
C           IF (ILVOUT>=2) CALL PROGBAR_C(IEL,NEL)
           N1=ELEM(1,IEL)
           N2=ELEM(2,IEL)
           N3=ELEM(3,IEL)
           NN1=IBUF(N1)
           NN2=IBUF(N2)
           NN3=IBUF(N3)
           X1=X(1,NN1)
           X2=X(1,NN2)
           X3=X(1,NN3)
           Y1=X(2,NN1)
           Y2=X(2,NN2)
           Y3=X(2,NN3)
           Z1=X(3,NN1)
           Z2=X(3,NN2)
           Z3=X(3,NN3)
C Control point P
           XP=THIRD*(X1+X2+X3)
           YP=THIRD*(Y1+Y2+Y3)    
           ZP=THIRD*(Z1+Z2+Z3)
           D2=MIN((XP-X1)**2+(YP-Y1)**2+(ZP-Z1)**2,
     .            (XP-X2)**2+(YP-Y2)**2+(ZP-Z2)**2,
     .            (XP-X3)**2+(YP-Y3)**2+(ZP-Z3)**2)

           NRX=NORMAL(1,IEL)
           NRY=NORMAL(2,IEL)
           NRZ=NORMAL(3,IEL)
           DO JEL=1,NEL
              IF(IEL == JEL) THEN
                BBEM(IEL,JEL)=TWO*SQRT(PI*AF(JEL))
                CBEM(IEL,JEL)=TWO*PI
              ELSE
                N1=ELEM(1,JEL)
                N2=ELEM(2,JEL)
                N3=ELEM(3,JEL)
                NN1=IBUF(N1)
                NN2=IBUF(N2)
                NN3=IBUF(N3)
                X1=X(1,NN1)
                X2=X(1,NN2)
                X3=X(1,NN3)
                Y1=X(2,NN1)
                Y2=X(2,NN2)
                Y3=X(2,NN3)
                Z1=X(3,NN1)
                Z2=X(3,NN2)
                Z3=X(3,NN3)
                AREA=AF(JEL)
C Point source Q
                XQ=THIRD*(X1+X2+X3)
                YQ=THIRD*(Y1+Y2+Y3)    
                ZQ=THIRD*(Z1+Z2+Z3)
                CALL INTHTG(X1 , Y1,  Z1,  X2,  Y2,  Z2,  
     .                      X3,  Y3,  Z3,  XP,  YP,  ZP,                    
     .                      NRX,NRY, NRZ,  D2,  AREA,
     .                      XQ,  YQ,  ZQ, RVAL )
                CBEM(IEL,JEL)=RVAL
                CALL INTGTG(X1 , Y1,  Z1,  X2,  Y2,  Z2,  
     .                      X3,  Y3,  Z3,  XP,  YP,  ZP,                    
     .                      D2,  AREA,
     .                      XQ,  YQ,  ZQ, RVAL )
                BBEM(IEL,JEL)=RVAL
              ENDIF
           ENDDO
        ENDDO 

      ELSEIF(IFORM == 2) THEN
C Analytical integration
        DO IEL=1,NEL
C           IF (ILVOUT>=2) CALL PROGBAR_C(IEL,NEL)
           N1=ELEM(1,IEL)
           N2=ELEM(2,IEL)
           N3=ELEM(3,IEL)
           NN1=IBUF(N1)
           NN2=IBUF(N2)
           NN3=IBUF(N3)
           X1=X(1,NN1)
           X2=X(1,NN2)
           X3=X(1,NN3)
           Y1=X(2,NN1)
           Y2=X(2,NN2)
           Y3=X(2,NN3)
           Z1=X(3,NN1)
           Z2=X(3,NN2)
           Z3=X(3,NN3)
           XP=THIRD*(X1+X2+X3)
           YP=THIRD*(Y1+Y2+Y3)    
           ZP=THIRD*(Z1+Z2+Z3)
           DO JEL=1,NEL
              N1=ELEM(1,JEL)
              N2=ELEM(2,JEL)
              N3=ELEM(3,JEL)
              NN1=IBUF(N1)
              NN2=IBUF(N2)
              NN3=IBUF(N3)
              X1=X(1,NN1)
              X2=X(1,NN2)
              X3=X(1,NN3)
              Y1=X(2,NN1)
              Y2=X(2,NN2)
              Y3=X(2,NN3)
              Z1=X(3,NN1)
              Z2=X(3,NN2)
              Z3=X(3,NN3)
              NRX=NORMAL(1,JEL)
              NRY=NORMAL(2,JEL)
              NRZ=NORMAL(3,JEL)
              AREA=AF(JEL)
C Matrice C=H
              CALL INTANL_TG(X1,   Y1,   Z1,   X2,  Y2,  Z2,
     .                       X3,   Y3,   Z3,   XP,  YP,  ZP,
     .                       NRX,  NRY,  NRZ,
     .                       AREA, RVLH, RVLG, IEL, JEL )
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
          WRITE (*,'(//,A)') ' BBEM MATRIX B1J'
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
          WRITE (*,'(//,A)') ' CBEM MATRIX C1J'
          WRITE (*,'(10E13.5)')  (CBEM(1,JN),JN=1,NEL)
        ENDIF
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

C-----------------------------------------
C  5 Fluid Mass matrix
C-----------------------------------------
      DO IN=1,NEL
         DO JN=1,NEL
            MFLE(IN,JN)=ZERO
            CBEM(IN,JN)=ZERO
         ENDDO
      ENDDO
      DO IN=1,NEL
         DO JN=1,NEL
            DO KN=1,NEL
               CBEM(IN,JN)=CBEM(IN,JN)+BBEM(IN,KN)*EBEM(KN,JN)
            ENDDO
         ENDDO
      ENDDO
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
C-----------------------------------------
C  Compute rigid body fluid mass
C-----------------------------------------
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
          WRITE (*,'(//,A)') ' INVERSE FLUID MASS MATRIX M1I'
          WRITE (*,'(10E13.5)')  (EBEM(1,JN),JN=1,NEL)
        ENDIF
      ENDIF


      DO IN=1,NEL
         DO JN=1,NEL
            MFLE(IN,JN)=EBEM(IN,JN)
         ENDDO
      ENDDO

      IF (ALLOCATED(BBEM)) DEALLOCATE(BBEM)
      IF (ALLOCATED(EBEM)) DEALLOCATE(EBEM)	  
      IF (ALLOCATED(CBEM)) DEALLOCATE(CBEM)
	  
      RETURN
      END

      
