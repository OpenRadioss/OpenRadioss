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
Chd|  TABLE_ZERO                    source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
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
Chd|====================================================================
Chd|  TABLE_WRESTI                  source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
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
Chd|====================================================================
Chd|  TABLE_WRESTR                  source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
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
Chd|====================================================================
Chd|  TABLE_WRESTI_MAT              source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        WRITE_MATPARAM                source/materials/mat/write_matparam.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE TABLE_WRESTI_MAT(TABLE, NUMTABL, LENI)
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
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMTABL,LENI
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K
C--------------------------------------
      LENI=0
      DO N=1,NUMTABL
        LEN = 1
        CALL WRITE_I_C(TABLE(N)%NOTABLE,LEN)
        LENI = LENI + LEN
        IF (TABLE(N)%NOTABLE > 0) THEN
          LEN  = 1
          CALL WRITE_I_C(TABLE(N)%NDIM,LEN)
          LENI = LENI + LEN
          DO K=1,TABLE(N)%NDIM
            LEN = 1
            CALL WRITE_I_C( SIZE(TABLE(N)%X(K)%VALUES) , LEN)
            LENI = LENI + LEN
          END DO
          LEN  = 1
          CALL WRITE_I_C(SIZE(TABLE(N)%Y%VALUES),LEN)
          LENI = LENI + LEN
        END IF
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_WRESTI_MAT
Chd|====================================================================
Chd|  TABLE_WRESTR_MAT              source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        WRITE_MATPARAM                source/materials/mat/write_matparam.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE TABLE_WRESTR_MAT(TABLE, NUMTABL, LENR)
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMTABL,LENR
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K
C--------------------------------------
      LENR=0
      DO N=1,NUMTABL
        IF (TABLE(N)%NOTABLE > 0) THEN
          DO K=1,TABLE(N)%NDIM
            LEN    =SIZE( TABLE(N)%X(K)%VALUES)
            CALL WRITE_DB(TABLE(N)%X(K)%VALUES,LEN)
            LENR=LENR+LEN
          END DO
          LEN = SIZE( TABLE(N)%Y%VALUES)
          CALL WRITE_DB(TABLE(N)%Y%VALUES,LEN)
          LENR=LENR+LEN
        END IF
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_WRESTR_MAT
Chd|====================================================================
Chd|  TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        GET_U_TABLE                   source/user_interface/utable.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
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
Chd|====================================================================
Chd|  TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        GET_U_VTABLE                  source/user_interface/utable.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE TABLE_VINTERP(TABLE,NEL0,IPOS,XX,YY,DYDX1)
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
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TTABLE) TABLE
      INTEGER NEL0
      INTEGER, DIMENSION(:,:) :: IPOS
      my_real, 
     .       DIMENSION(:,:) :: XX
      my_real
     .       YY(*), DYDX1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(TTABLE_XY), POINTER :: TY
      INTEGER NDIM, K, NXK(4), I, IB(2,2,2,2,NEL0), 
     .        IP,IN,IM,IL,P,N,M,L,N1,N12,N123
      my_real 
     .       DX1,DX2,R(4,NEL0),UNR(4,NEL0)
C-----------------------------------------------
      NDIM=TABLE%NDIM
      IF( SIZE(XX,2) < TABLE%NDIM )THEN
        WRITE(IOUT,*) ' ** INTERNAL ERROR - TABLE INTERPOLATION'
        WRITE(ISTDO,*)' ** INTERNAL ERROR - TABLE INTERPOLATION'
        CALL ARRET(2)
      END IF
C-----
C initialize IPOS=1 at NCYCLE=0
      IF(NCYCLE==0)THEN
        DO K=1,NDIM
          DO I=1,NEL0
            IPOS(I,K)=1
          END DO
        END DO
      END IF
C-----
      DO K=1,NDIM

        NXK(K)=SIZE(TABLE%X(K)%VALUES)
        DO I=1,NEL0

          M = IPOS(I,K)
          DX2 = TABLE%X(K)%VALUES(M) - XX(I,K)
          IF(DX2>=ZERO)THEN
           DO N=M-1,1,-1
            DX2 = TABLE%X(K)%VALUES(N) - XX(I,K)
            IF(DX2<ZERO.OR.N==1)THEN
              IPOS(I,K)=N
              EXIT
            ENDIF
           END DO
          ELSE
           DO N=M+1,NXK(K)
            DX2 = TABLE%X(K)%VALUES(N) - XX(I,K)
            IF(DX2>=ZERO.OR.N==NXK(K))THEN
              IPOS(I,K)=N-1
              EXIT
            ENDIF
           END DO
          END IF

          N = IPOS(I,K)
          R(K,I)   =(TABLE%X(K)%VALUES(N+1)-XX(I,K))/
     .              (TABLE%X(K)%VALUES(N+1)-TABLE%X(K)%VALUES(N))

        END DO

      END DO
C-----
      TY=>TABLE%Y
      SELECT CASE(NDIM)

       CASE(4)
C
       N1  =NXK(1)
       N12 =NXK(1)*NXK(2)
       N123=N12   *NXK(3)
       DO I=1,NEL0
         DO P=0,1
           IP=N123*(IPOS(I,4)-1+P)
           DO N=0,1
             IN=N12*(IPOS(I,3)-1+N)
             DO M=0,1
               IM=N1*(IPOS(I,2)-1+M)
               DO L=0,1
                 IL=IPOS(I,1)+L
                 IB(L+1,M+1,N+1,P+1,I)=IP+IN+IM+IL
               END DO
             END DO
           END DO
         END DO
       END DO
C
       DO I=1,NEL0
         DO K=1,4
           UNR(K,I)=ONE-R(K,I)
         END DO
C
         YY(I)=
     .     R(4,I)*(R(3,I)*(R(2,I)*(R(1,I)*TY%VALUES(IB(1,1,1,1,I))
     .                         +UNR(1,I)*TY%VALUES(IB(2,1,1,1,I)))
     .                  +UNR(2,I)*(R(1,I)*TY%VALUES(IB(1,2,1,1,I))
     .                        +UNR(1,I)*TY%VALUES(IB(2,2,1,1,I))))
     .          +UNR(3,I)*(R(2,I)*(R(1,I)*TY%VALUES(IB(1,1,2,1,I))
     .                         +UNR(1,I)*TY%VALUES(IB(2,1,2,1,I)))
     .                  +UNR(2,I)*(R(1,I)*TY%VALUES(IB(1,2,2,1,I))
     .                       +UNR(1,I)*TY%VALUES(IB(2,2,2,1,I)))))
     .  +UNR(4,I)*(R(3,I)*(R(2,I)*(R(1,I)*TY%VALUES(IB(1,1,1,1,I))
     .                         +UNR(1,I)*TY%VALUES(IB(2,1,1,1,I)))
     .                  +UNR(2,I)*(R(1,I)*TY%VALUES(IB(1,2,1,1,I))
     .                        +UNR(1,I)*TY%VALUES(IB(2,2,1,1,I))))
     .          +UNR(3,I)*(R(2,I)*(R(1,I)*TY%VALUES(IB(1,1,2,1,I))
     .                         +UNR(1,I)*TY%VALUES(IB(2,1,2,1,I)))
     .                  +UNR(2,I)*(R(1,I)*TY%VALUES(IB(1,2,2,1,I))
     .                       +UNR(1,I)*TY%VALUES(IB(2,2,2,1,I)))))
C
         DYDX1(I)=
     .     R(4,I)*(R(3,I)*(R(2,I)*( TY%VALUES(IB(1,1,1,1,I))
     .                             -TY%VALUES(IB(2,1,1,1,I)))
     .                  +UNR(2,I)*( TY%VALUES(IB(1,2,1,1,I))
     .                             -TY%VALUES(IB(2,2,1,1,I))))
     .          +UNR(3,I)*(R(2,I)*( TY%VALUES(IB(1,1,2,1,I))
     .                             -TY%VALUES(IB(2,1,2,1,I)))
     .                  +UNR(2,I)*( TY%VALUES(IB(1,2,2,1,I))
     .                             -TY%VALUES(IB(2,2,2,1,I)))))
     .  +UNR(4,I)*(R(3,I)*(R(2,I)*( TY%VALUES(IB(1,1,1,1,I))
     .                             -TY%VALUES(IB(2,1,1,1,I)))
     .                  +UNR(2,I)*( TY%VALUES(IB(1,2,1,1,I))
     .                             -TY%VALUES(IB(2,2,1,1,I))))
     .          +UNR(3,I)*(R(2,I)*( TY%VALUES(IB(1,1,2,1,I))
     .                             -TY%VALUES(IB(2,1,2,1,I)))
     .                  +UNR(2,I)*( TY%VALUES(IB(1,2,2,1,I))
     .                             -TY%VALUES(IB(2,2,2,1,I)))))
       END DO
C-----
       CASE(3)
C
       N1  =NXK(1)
       N12 =NXK(1)*NXK(2)
       DO I=1,NEL0
        DO N=0,1
          IN=N12*(IPOS(I,3)-1+N)
          DO M=0,1
            IM=N1*(IPOS(I,2)-1+M)
            DO L=0,1
              IL=IPOS(I,1)+L
              IB(L+1,M+1,N+1,1,I)=IN+IM+IL
            END DO
          END DO
        END DO
       END DO
C
      DO I=1,NEL0
        DO K=1,3
          UNR(K,I)=ONE-R(K,I)
        END DO
C
        YY(I)=
     .        (R(3,I)*(R(2,I)*(R(1,I)*TY%VALUES(IB(1,1,1,1,I))
     .                     +UNR(1,I)*TY%VALUES(IB(2,1,1,1,I)))
     .              +UNR(2,I)*(R(1,I)*TY%VALUES(IB(1,2,1,1,I))
     .                    +UNR(1,I)*TY%VALUES(IB(2,2,1,1,I))))
     .      +UNR(3,I)*(R(2,I)*(R(1,I)*TY%VALUES(IB(1,1,2,1,I))
     .                     +UNR(1,I)*TY%VALUES(IB(2,1,2,1,I)))
     .              +UNR(2,I)*(R(1,I)*TY%VALUES(IB(1,2,2,1,I))
     .                   +UNR(1,I)*TY%VALUES(IB(2,2,2,1,I)))))
C
        DYDX1(I)=
     .        (R(3,I)*(R(2,I)*( TY%VALUES(IB(1,1,1,1,I))
     .                         -TY%VALUES(IB(2,1,1,1,I)))
     .              +UNR(2,I)*( TY%VALUES(IB(1,2,1,1,I))
     .                         -TY%VALUES(IB(2,2,1,1,I))))
     .      +UNR(3,I)*(R(2,I)*( TY%VALUES(IB(1,1,2,1,I))
     .                         -TY%VALUES(IB(2,1,2,1,I)))
     .              +UNR(2,I)*( TY%VALUES(IB(1,2,2,1,I))
     .                         -TY%VALUES(IB(2,2,2,1,I)))))
C
       END DO
C-----
       CASE(2)
C
       N1  =NXK(1)
       DO I=1,NEL0
         DO M=0,1
           IM=N1*(IPOS(I,2)-1+M)
           DO L=0,1
             IL=IPOS(I,1)+L
             IB(L+1,M+1,1,1,I)=IM+IL
           END DO
         END DO
       END DO
C
       DO I=1,NEL0
        DO K=1,2
          UNR(K,I)=ONE-R(K,I)
        END DO
C
        YY(I)=
     .        (R(2,I)*(R(1,I)*TY%VALUES(IB(1,1,1,1,I))
     .             +UNR(1,I)*TY%VALUES(IB(2,1,1,1,I)))
     .      +UNR(2,I)*(R(1,I)*TY%VALUES(IB(1,2,1,1,I))
     .                  +UNR(1,I)*TY%VALUES(IB(2,2,1,1,I))))
        DYDX1(I)=
     .        (R(2,I)*( TY%VALUES(IB(1,1,1,1,I))
     .                 -TY%VALUES(IB(2,1,1,1,I)))
     .      +UNR(2,I)*( TY%VALUES(IB(1,2,1,1,I))
     .                 -TY%VALUES(IB(2,2,1,1,I))))
       END DO
C-----
       CASE(1)

       DO I=1,NEL0
        DO K=1,2
          UNR(K,I)=ONE-R(K,I)
        END DO
C
        YY(I)=  R(1,I)*TY%VALUES(IPOS(I,1))
     .       +UNR(1,I)*TY%VALUES(IPOS(I,1)+1)
        DYDX1(I)=TY%VALUES(IPOS(I,1))-TY%VALUES(IPOS(I,1)+1)
       END DO
C-----
      END SELECT

      RETURN
      END SUBROUTINE TABLE_VINTERP
