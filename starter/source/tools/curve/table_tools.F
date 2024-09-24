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
      !||====================================================================
      !||    table_zero   ../starter/source/tools/curve/table_tools.F
      !||--- called by ------------------------------------------------------
      !||    lectur       ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||    table_mod    ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE TABLE_ZERO(TABLE)
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N
C--------------------------------------
      DO N=1,NTABLE
        TABLE(N)%NOTABLE  = 0
        TABLE(N)%NDIM     = 0
        NULLIFY(TABLE(N)%X)
        NULLIFY(TABLE(N)%Y)
      END DO
      RETURN
      END SUBROUTINE TABLE_ZERO
      !||====================================================================
      !||    table_wresti   ../starter/source/tools/curve/table_tools.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit        ../starter/source/restart/ddsplit/ddsplit.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    table_mod      ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE TABLE_WRESTI(TABLE, LENI)
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENI
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K
C--------------------------------------
      LENI=0
      DO N=1,NTABLE
        LEN    =1
        CALL WRITE_I_C(TABLE(N)%NOTABLE,LEN)
        LENI = LENI + LEN
        LEN    =1
        CALL WRITE_I_C(TABLE(N)%NDIM,LEN)
        LENI = LENI + LEN
        DO K=1,TABLE(N)%NDIM
          LEN    =1
          CALL WRITE_I_C( SIZE(TABLE(N)%X(K)%VALUES) , LEN)
          LENI = LENI + LEN
        END DO
        LEN    =1
        CALL WRITE_I_C(SIZE(TABLE(N)%Y%VALUES),LEN)
        LENI = LENI + LEN
      END DO
      RETURN
      END SUBROUTINE TABLE_WRESTI
      !||====================================================================
      !||    table_wrestr   ../starter/source/tools/curve/table_tools.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit        ../starter/source/restart/ddsplit/ddsplit.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    table_mod      ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE TABLE_WRESTR(TABLE, LENR)
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENR
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K
C--------------------------------------
      LENR=0
      DO N=1,NTABLE
        DO K=1,TABLE(N)%NDIM
          LEN    =SIZE( TABLE(N)%X(K)%VALUES)
          CALL WRITE_DB(TABLE(N)%X(K)%VALUES,LEN)
          LENR=LENR+LEN
        END DO
        LEN    =SIZE( TABLE(N)%Y%VALUES)
        CALL WRITE_DB(TABLE(N)%Y%VALUES,LEN)
        LENR=LENR+LEN
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_WRESTR
      !||====================================================================
      !||    table_interp   ../starter/source/tools/curve/table_tools.F
      !||--- called by ------------------------------------------------------
      !||    get_u_table    ../starter/source/user_interface/utable.F
      !||--- calls      -----------------------------------------------------
      !||    arret          ../starter/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    table_mod      ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE TABLE_INTERP(TABLE,XX,YY)
C-----------------------------------------------
      USE TABLE_MOD
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
      TYPE(TTABLE) TABLE
      my_real, 
     .       DIMENSION(:) :: XX
      my_real
     .       YY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(TTABLE_XY), POINTER :: TY
      INTEGER NDIM, K, NXK(4), I, IPOS(4), IB(2,2,2,2), 
     .        IP,IN,IM,IL,P,N,M,L,N1,N12,N123
      my_real 
     .       DX1,DX2,R(4),UNR(4)
C-----------------------------------------------
      NDIM=TABLE%NDIM
      IF( SIZE(XX) < NDIM )THEN
        WRITE(IOUT,*) ' ** INTERNAL ERROR - TABLE INTERPOLATION'
        WRITE(ISTDO,*)' ** INTERNAL ERROR - TABLE INTERPOLATION'
        CALL ARRET(2)
      END IF
C-----
      DO K=1,NDIM

        NXK(K)=SIZE(TABLE%X(K)%VALUES)
        DO I=2,NXK(K)
         DX2 = TABLE%X(K)%VALUES(I) - XX(K)
         IF(DX2>=ZERO.OR.I==NXK(K))THEN
           IPOS(K)=I-1
           R(K)   =(TABLE%X(K)%VALUES(I)-XX(K))/
     .             (TABLE%X(K)%VALUES(I)-TABLE%X(K)%VALUES(I-1))
           EXIT
         ENDIF
        END DO

      END DO
C-----
      TY=>TABLE%Y
      SELECT CASE(NDIM)

       CASE(4)

        N1  =NXK(1)
        N12 =NXK(1)*NXK(2)
        N123=N12   *NXK(3)
        DO P=0,1
          IP=N123*(IPOS(4)-1+P)
          DO N=0,1
            IN=N12*(IPOS(3)-1+N)
            DO M=0,1
              IM=N1*(IPOS(2)-1+M)
              DO L=0,1
                IL=IPOS(1)+L
                IB(L+1,M+1,N+1,P+1)=IP+IN+IM+IL
              END DO
            END DO
          END DO
        END DO
C
        DO K=1,4
          UNR(K)=ONE-R(K)
        END DO
C
        YY=  R(4)*(   R(3)*(  R(2)*(   R(1)*TY%VALUES(IB(1,1,1,1))
     .                             +UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .                     +UNR(2)*(   R(1)*TY%VALUES(IB(1,2,1,1))
     .                            +UNR(1)*TY%VALUES(IB(2,2,1,1))))
     .             +UNR(3)*(  R(2)*(   R(1)*TY%VALUES(IB(1,1,2,1))
     .                             +UNR(1)*TY%VALUES(IB(2,1,2,1)))
     .                     +UNR(2)*(   R(1)*TY%VALUES(IB(1,2,2,1))
     .                            +UNR(1)*TY%VALUES(IB(2,2,2,1)))))
     .    +UNR(4)*(   R(3)*(  R(2)*(   R(1)*TY%VALUES(IB(1,1,1,2))
     .                             +UNR(1)*TY%VALUES(IB(2,1,1,2)))
     .                     +UNR(2)*(   R(1)*TY%VALUES(IB(1,2,1,2))
     .                            +UNR(1)*TY%VALUES(IB(2,2,1,2))))
     .             +UNR(3)*(  R(2)*(   R(1)*TY%VALUES(IB(1,1,2,2))
     .                             +UNR(1)*TY%VALUES(IB(2,1,2,2)))
     .                     +UNR(2)*(   R(1)*TY%VALUES(IB(1,2,2,2))
     .                            +UNR(1)*TY%VALUES(IB(2,2,2,2)))))
C-----
       CASE(3)

        N1  =NXK(1)
        N12 =NXK(1)*NXK(2)
        DO N=0,1
          IN=N12*(IPOS(3)-1+N)
          DO M=0,1
            IM=N1*(IPOS(2)-1+M)
            DO L=0,1
              IL=IPOS(1)+L
              IB(L+1,M+1,N+1,1)=IN+IM+IL
            END DO
          END DO
        END DO
C
        DO K=1,3
          UNR(K)=ONE-R(K)
        END DO
C
        YY=R(3)  *(   R(2)*(   R(1)*TY%VALUES(IB(1,1,1,1))
     .                      +UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .             +UNR(2)*(   R(1)*TY%VALUES(IB(1,2,1,1))
     .                      +UNR(1)*TY%VALUES(IB(2,2,1,1))))
     .    +UNR(3)*(   R(2)*(   R(1)*TY%VALUES(IB(1,1,2,1))
     .                      +UNR(1)*TY%VALUES(IB(2,1,2,1)))
     .             +UNR(2)*(   R(1)*TY%VALUES(IB(1,2,2,1))
     .                      +UNR(1)*TY%VALUES(IB(2,2,2,1))))
C-----
       CASE(2)

        N1  =NXK(1)
        DO M=0,1
          IM=N1*(IPOS(2)-1+M)
          DO L=0,1
            IL=IPOS(1)+L
            IB(L+1,M+1,1,1)=IM+IL
          END DO
        END DO
C
        DO K=1,2
          UNR(K)=ONE-R(K)
        END DO
C
        YY=(   R(2)*(   R(1)*TY%VALUES(IB(1,1,1,1))
     .              +UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .      +UNR(2)*(   R(1)*TY%VALUES(IB(1,2,1,1))
     .             +UNR(1)*TY%VALUES(IB(2,2,1,1))))

C-----
       CASE(1)

        DO K=1,2
          UNR(K)=ONE-R(K)
        END DO
C
        YY=R(1)*TY%VALUES(IPOS(1))
     .    +UNR(1)*TY%VALUES(IPOS(1)+1)

C-----
      END SELECT

      RETURN
      END SUBROUTINE TABLE_INTERP
      !||====================================================================
      !||    table_vinterp   ../starter/source/tools/curve/table_tools.F
      !||--- called by ------------------------------------------------------
      !||    get_u_vtable    ../starter/source/user_interface/utable.F
      !||--- calls      -----------------------------------------------------
      !||    arret           ../starter/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    table_mod       ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE TABLE_VINTERP(TABLE,DIMX,NEL,IPOS,XX,YY,DYDX1)
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TTABLE) :: TABLE
      INTEGER ,INTENT(IN) :: NEL
      INTEGER ,VALUE ,INTENT(IN) :: DIMX
      INTEGER ,DIMENSION(DIMX,TABLE%NDIM)  :: IPOS
      my_real ,DIMENSION(DIMX,TABLE%NDIM)  :: XX
      my_real ,DIMENSION(NEL) :: YY, DYDX1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL, DIMENSION(NEL) :: NEED_TO_COMPUTE
        INTEGER NDIM, K, NXK(4), I, IB(NEL,2,2,2,2), 
     .        IP,IN,IM,IL,P,N,M,L,N1,N12,N123
        my_real :: DX2,R(NEL,4),UNR(NEL,4),DX2_0(NEL)
        INTEGER :: J
        INTEGER :: NINDX_1,M_INDX1,NINDX_2,M_INDX2
        INTEGER, DIMENSION(NEL) :: INDX_1,INDX_2
C-----------------------------------------------
      R(1:NEL,1:4) = zero
      NDIM=TABLE%NDIM
      IF (SIZE(XX,2) < TABLE%NDIM) THEN
        WRITE(IOUT,*) ' ** INTERNAL ERROR - TABLE INTERPOLATION'
        WRITE(ISTDO,*)' ** INTERNAL ERROR - TABLE INTERPOLATION'
        CALL ARRET(2)
      END IF
C-----
        DO K=1,NDIM
            NXK(K)=SIZE(TABLE%X(K)%VALUES)
        ENDDO

        DO K=1,NDIM
            IPOS(1:NEL,K)=MAX(IPOS(1:NEL,K),1)
            NINDX_1 = 0
            M_INDX1 = 0
            NINDX_2 = 0
            M_INDX2 = NXK(K) + 1
#include "vectorize.inc"
            DO I=1,NEL
                M = IPOS(I,K) 
                DX2_0(I) = TABLE%X(K)%VALUES(M) - XX(I,K)
                IF(DX2_0(I) >= ZERO)THEN
                    NINDX_1 = NINDX_1 + 1
                    INDX_1(NINDX_1) = I
                    M_INDX1 = MAX(M_INDX1,M)
                ELSE
                    NINDX_2 = NINDX_2 + 1
                    INDX_2(NINDX_2) = I
                    M_INDX2 = MIN(M_INDX2,M)
                ENDIF
            ENDDO

            NEED_TO_COMPUTE(1:NINDX_1) = .TRUE.
            DO N=M_INDX1,1,-1
#include "vectorize.inc"
                DO J=1,NINDX_1
                    IF(NEED_TO_COMPUTE(J)) THEN
                        I = INDX_1(J)
                        M = IPOS(I,K)
                        DX2 = TABLE%X(K)%VALUES(N) - XX(I,K)
                        IF(DX2<ZERO.OR.N <=1)THEN 
                            IPOS(I,K)=MAX(N,1) !N 
                            NEED_TO_COMPUTE(J) = .FALSE.
                        ENDIF
                    ENDIF
                ENDDO
            ENDDO
            NEED_TO_COMPUTE(1:NINDX_2) = .TRUE.
            DO N=M_INDX2,NXK(K)
#include "vectorize.inc"
                DO J=1,NINDX_2
                    IF(NEED_TO_COMPUTE(J)) THEN
                        I = INDX_2(J)
                        M = IPOS(I,K) 
                        DX2 = TABLE%X(K)%VALUES(N) - XX(I,K)
                        IF(DX2>=ZERO.OR.N==NXK(K))THEN
                            IPOS(I,K)=N-1
                            NEED_TO_COMPUTE(J) = .FALSE.
                        ENDIF
                    ENDIF
                ENDDO
            ENDDO
       ENDDO

        DO K=1,NDIM
#include "vectorize.inc"
            DO I=1,NEL
                N = IPOS(I,K)
                R(I,K)   =(TABLE%X(K)%VALUES(N+1)-XX(I,K))/
     .                    (TABLE%X(K)%VALUES(N+1)-TABLE%X(K)%VALUES(N))
            END DO
        END DO
C-----
      SELECT CASE(NDIM)

       CASE(4)
C
       N1  =NXK(1)
       N12 =NXK(1)*NXK(2)
       N123=N12   *NXK(3)
       DO I=1,NEL
         DO P=0,1
           IP=N123*(IPOS(I,4)-1+P)
           DO N=0,1
             IN=N12*(IPOS(I,3)-1+N)
             DO M=0,1
               IM=N1*(IPOS(I,2)-1+M)
               DO L=0,1
                 IL=IPOS(I,1)+L
                 IB(I,L+1,M+1,N+1,P+1)=IP+IN+IM+IL
               END DO
             END DO
           END DO
         END DO
       END DO
C
       UNR(1:NEL,1:4)=ONE-R(1:NEL,1:4)
#include "vectorize.inc"
       DO I=1,NEL
C
         YY(I)=
     .     R(I,4)*(R(I,3)*(R(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,1,1,1))
     .                         +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,1,1,1)))
     .                  +UNR(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,2,1,1))
     .                        +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,2,1,1))))
     .          +UNR(I,3)*(R(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,1,2,1))
     .                         +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,1,2,1)))
     .                  +UNR(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,2,2,1))
     .                       +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,2,2,1)))))
     .  +UNR(I,4)*(R(I,3)*(R(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,1,1,1))
     .                         +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,1,1,1)))
     .                  +UNR(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,2,1,1))
     .                        +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,2,1,1))))
     .          +UNR(I,3)*(R(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,1,2,1))
     .                         +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,1,2,1)))
     .                  +UNR(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,2,2,1))
     .                       +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,2,2,1)))))
C
         DYDX1(I)=
     .     (R(I,4)*(R(I,3)*(R(I,2)*( TABLE%Y%VALUES(IB(I,2,1,1,1))
     .                             -TABLE%Y%VALUES(IB(I,1,1,1,1)))
     .                  +UNR(I,2)*( TABLE%Y%VALUES(IB(I,2,2,1,1))
     .                             -TABLE%Y%VALUES(IB(I,1,2,1,1))))
     .          +UNR(I,3)*(R(I,2)*( TABLE%Y%VALUES(IB(I,2,1,2,1))
     .                             -TABLE%Y%VALUES(IB(I,1,1,2,1)))
     .                  +UNR(I,2)*( TABLE%Y%VALUES(IB(I,2,2,2,1))
     .                             -TABLE%Y%VALUES(IB(I,1,2,2,1)))))
     .  +UNR(I,4)*(R(I,3)*(R(I,2)*( TABLE%Y%VALUES(IB(I,2,1,1,1))
     .                             -TABLE%Y%VALUES(IB(I,1,1,1,1)))
     .                  +UNR(I,2)*( TABLE%Y%VALUES(IB(I,2,2,1,1))
     .                             -TABLE%Y%VALUES(IB(I,1,2,1,1))))
     .          +UNR(I,3)*(R(I,2)*( TABLE%Y%VALUES(IB(I,2,1,2,1))
     .                             -TABLE%Y%VALUES(IB(I,1,1,2,1)))
     .                  +UNR(I,2)*( TABLE%Y%VALUES(IB(I,2,2,2,1))
     .                             -TABLE%Y%VALUES(IB(I,1,2,2,1))))))/
     .     (TABLE%X(1)%VALUES(IPOS(I,1)+1)-TABLE%X(1)%VALUES(IPOS(I,1)))

       END DO
C-----
       CASE(3)
C
       N1  =NXK(1)
       N12 =NXK(1)*NXK(2)
       DO I=1,NEL
        DO N=0,1
          IN=N12*(IPOS(I,3)-1+N)
          DO M=0,1
            IM=N1*(IPOS(I,2)-1+M)
            DO L=0,1
              IL=IPOS(I,1)+L
              IB(I,L+1,M+1,N+1,1)=IN+IM+IL
            END DO
          END DO
        END DO
       END DO
C
       UNR(1:NEL,1:3)=ONE-R(1:NEL,1:3)
#include "vectorize.inc"
      DO I=1,NEL
C
        YY(I)=
     .        (R(I,3)*(R(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,1,1,1))
     .                     +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,1,1,1)))
     .              +UNR(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,2,1,1))
     .                    +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,2,1,1))))
     .      +UNR(I,3)*(R(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,1,2,1))
     .                     +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,1,2,1)))
     .              +UNR(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,2,2,1))
     .                   +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,2,2,1)))))
C
        DYDX1(I)=
     .        (R(I,3)*(R(I,2)*( TABLE%Y%VALUES(IB(I,2,1,1,1))
     .                         -TABLE%Y%VALUES(IB(I,1,1,1,1)))
     .              +UNR(I,2)*( TABLE%Y%VALUES(IB(I,2,2,1,1))
     .                         -TABLE%Y%VALUES(IB(I,1,2,1,1))))
     .      +UNR(I,3)*(R(I,2)*( TABLE%Y%VALUES(IB(I,2,1,2,1))
     .                         -TABLE%Y%VALUES(IB(I,1,1,2,1)))
     .              +UNR(I,2)*( TABLE%Y%VALUES(IB(I,2,2,2,1))
     .                         -TABLE%Y%VALUES(IB(I,1,2,2,1)))))/
     .     (TABLE%X(1)%VALUES(IPOS(I,1)+1)-TABLE%X(1)%VALUES(IPOS(I,1)))
C
       END DO
C-----
       CASE(2)
C
       N1  =NXK(1)
       DO I=1,NEL
         DO M=0,1
           IM=N1*(IPOS(I,2)-1+M)
           DO L=0,1
             IL=IPOS(I,1)+L
             IB(I,L+1,M+1,1,1)=IM+IL
           END DO
         END DO
       END DO
C
       UNR(1:NEL,1:2)=ONE-R(1:NEL,1:2)
#include "vectorize.inc"
       DO I=1,NEL
C
        YY(I)=
     .        (R(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,1,1,1))
     .             +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,1,1,1)))
     .      +UNR(I,2)*(R(I,1)*TABLE%Y%VALUES(IB(I,1,2,1,1))
     .                  +UNR(I,1)*TABLE%Y%VALUES(IB(I,2,2,1,1))))
        DYDX1(I)=
     .        (R(I,2)*( TABLE%Y%VALUES(IB(I,2,1,1,1))
     .                 -TABLE%Y%VALUES(IB(I,1,1,1,1)))
     .      +UNR(I,2)*( TABLE%Y%VALUES(IB(I,2,2,1,1))
     .                 -TABLE%Y%VALUES(IB(I,1,2,1,1))))/
     .     (TABLE%X(1)%VALUES(IPOS(I,1)+1)-TABLE%X(1)%VALUES(IPOS(I,1)))
       END DO
C-----
       CASE(1)

       UNR(1:NEL,1:1)=ONE-R(1:NEL,1:1)
#include "vectorize.inc"
       DO I=1,NEL
C
        YY(I)=  R(I,1)*TABLE%Y%VALUES(IPOS(I,1))
     .       +UNR(I,1)*TABLE%Y%VALUES(IPOS(I,1)+1)
        DYDX1(I)=(TABLE%Y%VALUES(IPOS(I,1)+1)-TABLE%Y%VALUES(IPOS(I,1)))/
     .     (TABLE%X(1)%VALUES(IPOS(I,1)+1)-TABLE%X(1)%VALUES(IPOS(I,1)))
       END DO
C-----
      END SELECT
C-----
      RETURN
      END SUBROUTINE TABLE_VINTERP
