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
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        TABLE_MOD                     share/modules/table_mod.F     
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
Chd|====================================================================
Chd|  TABLE_WRESTI                  source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        TABLE_MOD                     share/modules/table_mod.F     
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
Chd|====================================================================
Chd|  TABLE_WRESTR                  source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
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
      RETURN
      END SUBROUTINE TABLE_WRESTR
Chd|====================================================================
Chd|  TABLE_RRESTI                  source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_RRESTI(TABLE)
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
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
      INTEGER LEN, N, K, NXK, NY, STAT, NDIM
C--------------------------------------
      DO N=1,NTABLE
        LEN    =1
        CALL READ_I_C(TABLE(N)%NOTABLE,LEN)
        LEN    =1
        CALL READ_I_C(NDIM,LEN)
        TABLE(N)%NDIM=NDIM

        ALLOCATE(TABLE(N)%X(NDIM),STAT=stat)
        IF(STAT/=0) GOTO 1000

        DO K=1,TABLE(N)%NDIM
          LEN    =1
          CALL READ_I_C(NXK,LEN)
          ALLOCATE(TABLE(N)%X(K)%VALUES(NXK),STAT=stat)
          IF(STAT/=0) GOTO 1000
        END DO
        LEN    =1
        CALL READ_I_C(NY,LEN)

        ALLOCATE(TABLE(N)%Y,STAT=stat)
        IF(STAT/=0) GOTO 1000

        ALLOCATE(TABLE(N)%Y%VALUES(NY),STAT=stat)
        IF(STAT/=0) GOTO 1000
      END DO
      RETURN
C--------------------------------------
 1000 CONTINUE
      CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
      CALL ARRET(2)
C--------------------------------------
      END SUBROUTINE TABLE_RRESTI
Chd|====================================================================
Chd|  TABLE_RRESTR                  source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_RRESTR(TABLE)
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
      INTEGER LEN, N, K, STAT, i
C--------------------------------------
      DO N=1,NTABLE
        DO K=1,TABLE(N)%NDIM
          LEN = SIZE( TABLE(N)%X(K)%VALUES )
          CALL READ_DB(TABLE(N)%X(K)%VALUES,LEN)
        END DO
        LEN = SIZE( TABLE(N)%Y%VALUES )
        CALL READ_DB(TABLE(N)%Y%VALUES,LEN)
      END DO
      RETURN
      END SUBROUTINE TABLE_RRESTR
Chd|====================================================================
Chd|  TABLE_RRESTI_MAT              source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        READ_MATPARAM                 source/output/restart/read_matparam.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_RRESTI_MAT(TABLE, NUMTABL)
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMTABL
      TYPE(TTABLE) ,DIMENSION(NUMTABL) :: TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K, NXK, NY, STAT, NDIM
C--------------------------------------
      DO N=1,NUMTABL
        LEN = 1
        CALL READ_I_C(TABLE(N)%NOTABLE,LEN)
        LEN = 1
        IF (TABLE(N)%NOTABLE > 0) THEN
          CALL READ_I_C(NDIM,LEN)
          TABLE(N)%NDIM=NDIM

          ALLOCATE(TABLE(N)%X(NDIM),STAT=stat)
          IF(STAT/=0) GOTO 1000

          DO K=1,TABLE(N)%NDIM
            LEN = 1
            CALL READ_I_C(NXK,LEN)
            ALLOCATE(TABLE(N)%X(K)%VALUES(NXK),STAT=stat)
            IF(STAT/=0) GOTO 1000
          END DO
          LEN = 1
          CALL READ_I_C(NY,LEN)

          ALLOCATE(TABLE(N)%Y,STAT=stat)
          IF(STAT/=0) GOTO 1000

          ALLOCATE(TABLE(N)%Y%VALUES(NY),STAT=stat)
          IF(STAT/=0) GOTO 1000
        END IF
      END DO
      RETURN
C--------------------------------------
 1000 CONTINUE
      CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
      CALL ARRET(2)
C--------------------------------------
      END SUBROUTINE TABLE_RRESTI_MAT
Chd|====================================================================
Chd|  TABLE_RRESTR_MAT              source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        READ_MATPARAM                 source/output/restart/read_matparam.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_RRESTR_MAT(TABLE, NUMTABL)
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMTABL
      TYPE(TTABLE) ,DIMENSION(NUMTABL) :: TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K, STAT, i
C--------------------------------------
      DO N=1,NUMTABL
        IF (TABLE(N)%NOTABLE > 0) THEN
          DO K=1,TABLE(N)%NDIM
            LEN = SIZE( TABLE(N)%X(K)%VALUES )
            CALL READ_DB(TABLE(N)%X(K)%VALUES,LEN)
          END DO
          LEN = SIZE( TABLE(N)%Y%VALUES )
          CALL READ_DB(TABLE(N)%Y%VALUES,LEN)
        END IF
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_RRESTR_MAT
Chd|====================================================================
Chd|  TABLE_WRESTI_MAT              source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        WRITE_MATPARAM                source/output/restart/write_matparam.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_WRESTI_MAT(TABLE, NUMTABL)
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMTABL
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K
C--------------------------------------
      DO N=1,NUMTABL
        LEN = 1
        CALL WRITE_I_C(TABLE(N)%NOTABLE,LEN)
        IF (TABLE(N)%NOTABLE > 0) THEN
          LEN  = 1
          CALL WRITE_I_C(TABLE(N)%NDIM,LEN)
          DO K=1,TABLE(N)%NDIM
            LEN = 1
            CALL WRITE_I_C( SIZE(TABLE(N)%X(K)%VALUES) , LEN)
          END DO
          LEN  = 1
          CALL WRITE_I_C(SIZE(TABLE(N)%Y%VALUES),LEN)
        END IF
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_WRESTI_MAT
Chd|====================================================================
Chd|  TABLE_WRESTR_MAT              source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        WRITE_MATPARAM                source/output/restart/write_matparam.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_WRESTR_MAT(TABLE, NUMTABL)
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMTABL
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K
C--------------------------------------
      DO N=1,NUMTABL
        IF (TABLE(N)%NOTABLE > 0) THEN
          DO K=1,TABLE(N)%NDIM
            LEN    =SIZE( TABLE(N)%X(K)%VALUES)
            CALL WRITE_DB(TABLE(N)%X(K)%VALUES,LEN)
          END DO
          LEN = SIZE( TABLE(N)%Y%VALUES)
          CALL WRITE_DB(TABLE(N)%Y%VALUES,LEN)
        END IF
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_WRESTR_MAT
Chd|====================================================================
Chd|  TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TAB_XFEM                 source/materials/fail/tabulated/fail_tab_xfem.F
Chd|        GET_TABLE_VALUE               source/user_interface/utable.F
Chd|        GET_U_TABLE                   source/user_interface/utable.F
Chd|        R3DEF3                        source/elements/spring/r3def3.F
Chd|        SIGEPS52                      source/materials/mat/mat052/sigeps52.F
Chd|        SIGEPS52C                     source/materials/mat/mat052/sigeps52c.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_INTERP(TABLE,XX,YY)
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TTABLE) :: TABLE
      my_real ,DIMENSION(:) :: XX
      my_real :: YY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(TTABLE_XY), POINTER :: TY
      INTEGER NDIM, I,K,IP,IN,IM,IL,P,N,M,L,N1,N12,N123
      INTEGER NXK(4),IPOS(4)
      INTEGER IB(2,2,2,2)
      my_real 
     .       DX1,DX2,R(4),UNR(4)
C-----------------------------------------------
      NDIM=TABLE%NDIM
      IF( SIZE(XX) < NDIM )THEN
        CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .              C1='TABLE INTERPOLATION')
        CALL ARRET(2)
      END IF
C-----
      IPOS(1:NDIM)= 1
      R(1:NDIM) = ONE
c
      DO K=1,NDIM
        NXK(K) = SIZE(TABLE%X(K)%VALUES)
        DO I=2,NXK(K)
          DX2 = TABLE%X(K)%VALUES(I) - XX(K)
          IF (DX2>=ZERO .OR. I==NXK(K)) THEN
            IPOS(K)=I-1
            R(K)   =(TABLE%X(K)%VALUES(I)-XX(K))/
     .              (TABLE%X(K)%VALUES(I)-TABLE%X(K)%VALUES(I-1))
            EXIT
          ENDIF
        END DO

        UNR(K) = ONE - R(K)

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
        YY = R(4) * (R(3)*(R(2) * (R(1)*TY%VALUES(IB(1,1,1,1)) + UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .                 + UNR(2) * (R(1)*TY%VALUES(IB(1,2,1,1)) + UNR(1)*TY%VALUES(IB(2,2,1,1))))
     .            +UNR(3)*(R(2) * (R(1)*TY%VALUES(IB(1,1,2,1)) + UNR(1)*TY%VALUES(IB(2,1,2,1)))
     .                 + UNR(2) * (R(1)*TY%VALUES(IB(1,2,2,1)) + UNR(1)*TY%VALUES(IB(2,2,2,1)))))
     .     +UNR(4) *(R(3)*(R(2) * (R(1)*TY%VALUES(IB(1,1,1,2)) + UNR(1)*TY%VALUES(IB(2,1,1,2)))
     .                  +UNR(2) * (R(1)*TY%VALUES(IB(1,2,1,2)) + UNR(1)*TY%VALUES(IB(2,2,1,2))))
     .            +UNR(3)*(R(2) * (R(1)*TY%VALUES(IB(1,1,2,2)) + UNR(1)*TY%VALUES(IB(2,1,2,2)))
     .                  +UNR(2) * (R(1)*TY%VALUES(IB(1,2,2,2)) + UNR(1)*TY%VALUES(IB(2,2,2,2)))))
C-----
       CASE(3)

        N1  = NXK(1)
        N12 = NXK(1)*NXK(2)
        DO N=0,1
          IN = N12*(IPOS(3)-1+N)
          DO M=0,1
            IM = N1*(IPOS(2)-1+M)
            DO L=0,1
              IL = IPOS(1)+L
              IB(L+1,M+1,N+1,1) = IN+IM+IL
            END DO
          END DO
        END DO
c        
        IF (R(2) == ONE) THEN   ! case when second variable has only one value
          YY = R(3)   * (R(1)*TY%VALUES(IB(1,1,1,1)) + UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .       + UNR(3) * (R(1)*TY%VALUES(IB(1,1,2,1)) + UNR(1)*TY%VALUES(IB(2,1,2,1)))
        ELSE
C
          YY = R(3) *(R(2) * (R(1)*TY%VALUES(IB(1,1,1,1)) + UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .             +UNR(2) * (R(1)*TY%VALUES(IB(1,2,1,1)) + UNR(1)*TY%VALUES(IB(2,2,1,1))))
     .     + UNR(3) *(R(2) * (R(1)*TY%VALUES(IB(1,1,2,1)) + UNR(1)*TY%VALUES(IB(2,1,2,1)))
     .             +UNR(2) * (R(1)*TY%VALUES(IB(1,2,2,1)) + UNR(1)*TY%VALUES(IB(2,2,2,1))))
        END IF
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
        YY = (R(2)*(R(1)*TY%VALUES(IB(1,1,1,1)) + UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .     +UNR(2)*(R(1)*TY%VALUES(IB(1,2,1,1)) + UNR(1)*TY%VALUES(IB(2,2,1,1))))

C-----
       CASE(1)

C
        YY = R(1) * TY%VALUES(IPOS(1)) + UNR(1) * TY%VALUES(IPOS(1)+1)

C-----
      END SELECT

      RETURN
      END SUBROUTINE TABLE_INTERP
Chd|====================================================================
Chd|  TABLE_INTERP_DYDX             source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        GET_TABLE_VALUE_DYDX          source/user_interface/utable.F
Chd|        PRESS_SEG3                    source/loads/general/load_pcyl/press_seg3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_INTERP_DYDX(TABLE,XX,XXDIM,YY,DYDX)
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER XXDIM
      TYPE(TTABLE) TABLE
      my_real, INTENT(IN),DIMENSION(XXDIM) :: XX
      my_real, INTENT(OUT) :: YY,DYDX
C-----------------------------------------------
C   L o c a l   V a r  i a b l e s
C-----------------------------------------------
      TYPE(TTABLE_XY), POINTER :: TY
      INTEGER NDIM, I,K,IP,IN,IM,IL,P,N,M,L,N1,N12,N123
      INTEGER NXK(4),IPOS(4)
      INTEGER IB(2,2,2,2)
      my_real 
     .       DX1,DX2,R(4),UNR(4)
C-----------------------------------------------
      NDIM=TABLE%NDIM
      IF( XXDIM < NDIM )THEN
        CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .              C1='TABLE INTERPOLATION')
        CALL ARRET(2)
      END IF
C-----
      IPOS(1:NDIM)= 1
      R(1:NDIM) = ONE
c
      DO K=1,NDIM
        NXK(K) = SIZE(TABLE%X(K)%VALUES)
        DO I=2,NXK(K)
          DX2 = TABLE%X(K)%VALUES(I) - XX(K)
          IF (DX2>=ZERO .OR. I==NXK(K)) THEN
            IPOS(K)=I-1
            R(K)   =(TABLE%X(K)%VALUES(I)-XX(K))/
     .              (TABLE%X(K)%VALUES(I)-TABLE%X(K)%VALUES(I-1))
            EXIT
          ENDIF
        END DO

        UNR(K) = ONE - R(K)

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
        YY = R(4) * (R(3)*(R(2) * (R(1)*TY%VALUES(IB(1,1,1,1)) + UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .                 + UNR(2) * (R(1)*TY%VALUES(IB(1,2,1,1)) + UNR(1)*TY%VALUES(IB(2,2,1,1))))
     .            +UNR(3)*(R(2) * (R(1)*TY%VALUES(IB(1,1,2,1)) + UNR(1)*TY%VALUES(IB(2,1,2,1)))
     .                 + UNR(2) * (R(1)*TY%VALUES(IB(1,2,2,1)) + UNR(1)*TY%VALUES(IB(2,2,2,1)))))
     .     +UNR(4) *(R(3)*(R(2) * (R(1)*TY%VALUES(IB(1,1,1,2)) + UNR(1)*TY%VALUES(IB(2,1,1,2)))
     .                  +UNR(2) * (R(1)*TY%VALUES(IB(1,2,1,2)) + UNR(1)*TY%VALUES(IB(2,2,1,2))))
     .            +UNR(3)*(R(2) * (R(1)*TY%VALUES(IB(1,1,2,2)) + UNR(1)*TY%VALUES(IB(2,1,2,2)))
     .                  +UNR(2) * (R(1)*TY%VALUES(IB(1,2,2,2)) + UNR(1)*TY%VALUES(IB(2,2,2,2)))))
C-----
         DYDX =
     .     (R(4) * (R(3) * (R(2) * ( TY%VALUES(IB(2,1,1,1)) - TY%VALUES(IB(1,1,1,1)))
     .                   +UNR(2) * ( TY%VALUES(IB(2,2,1,1)) - TY%VALUES(IB(1,2,1,1))))
     .          + UNR(3) * (R(2) * ( TY%VALUES(IB(2,1,2,1)) - TY%VALUES(IB(1,1,2,1)))
     .                  + UNR(2) * ( TY%VALUES(IB(2,2,2,1)) - TY%VALUES(IB(1,2,2,1)))))
     .  + UNR(4) * (R(3) * (R(2) * ( TY%VALUES(IB(2,1,1,1)) - TY%VALUES(IB(1,1,1,1)))
     .                  + UNR(2) * ( TY%VALUES(IB(2,2,1,1)) - TY%VALUES(IB(1,2,1,1))))
     .          + UNR(3) * (R(2) * ( TY%VALUES(IB(2,1,2,1)) - TY%VALUES(IB(1,1,2,1)))
     .                  + UNR(2) * ( TY%VALUES(IB(2,2,2,1)) - TY%VALUES(IB(1,2,2,1))))))/
     .     (TABLE%X(1)%VALUES(IPOS(1)+1)-TABLE%X(1)%VALUES(IPOS(1)))


C-----
       CASE(3)

        N1  = NXK(1)
        N12 = NXK(1)*NXK(2)
        DO N=0,1
          IN = N12*(IPOS(3)-1+N)
          DO M=0,1
            IM = N1*(IPOS(2)-1+M)
            DO L=0,1
              IL = IPOS(1)+L
              IB(L+1,M+1,N+1,1) = IN+IM+IL
            END DO
          END DO
        END DO
c        
        IF (R(2) == ONE) THEN   ! case when second variable has only one value
          YY = R(3)   * (R(1)*TY%VALUES(IB(1,1,1,1)) + UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .       + UNR(3) * (R(1)*TY%VALUES(IB(1,1,2,1)) + UNR(1)*TY%VALUES(IB(2,1,2,1)))
        ELSE
C
          YY = R(3) *(R(2) * (R(1)*TY%VALUES(IB(1,1,1,1)) + UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .             +UNR(2) * (R(1)*TY%VALUES(IB(1,2,1,1)) + UNR(1)*TY%VALUES(IB(2,2,1,1))))
     .     + UNR(3) *(R(2) * (R(1)*TY%VALUES(IB(1,1,2,1)) + UNR(1)*TY%VALUES(IB(2,1,2,1)))
     .             +UNR(2) * (R(1)*TY%VALUES(IB(1,2,2,1)) + UNR(1)*TY%VALUES(IB(2,2,2,1))))
        END IF
        
         DYDX =
     .        (R(3) * (R(2) * ( TY%VALUES(IB(2,1,1,1)) - TY%VALUES(IB(1,1,1,1)))
     .             + UNR(2) * ( TY%VALUES(IB(2,2,1,1)) - TY%VALUES(IB(1,2,1,1))))
     .     + UNR(3) * (R(2) * ( TY%VALUES(IB(2,1,2,1)) - TY%VALUES(IB(1,1,2,1)))
     .              +UNR(2) * ( TY%VALUES(IB(2,2,2,1)) - TY%VALUES(IB(1,2,2,1)))))/
     .     (TABLE%X(1)%VALUES(IPOS(1)+1)-TABLE%X(1)%VALUES(IPOS(1)))
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
        YY = (R(2)*(R(1)*TY%VALUES(IB(1,1,1,1)) + UNR(1)*TY%VALUES(IB(2,1,1,1)))
     .     +UNR(2)*(R(1)*TY%VALUES(IB(1,2,1,1)) + UNR(1)*TY%VALUES(IB(2,2,1,1))))
     
        DYDX =
     .        (R(2) * ( TY%VALUES(IB(2,1,1,1)) - TY%VALUES(IB(1,1,1,1)))
     .     + UNR(2) * ( TY%VALUES(IB(2,2,1,1)) - TY%VALUES(IB(1,2,1,1))))/
     .     (TABLE%X(1)%VALUES(IPOS(1)+1)-TABLE%X(1)%VALUES(IPOS(1)))

C-----
       CASE(1)

C
        YY = R(1) * TY%VALUES(IPOS(1)) + UNR(1) * TY%VALUES(IPOS(1)+1)
        
        DYDX = (TY%VALUES(IPOS(1)+1)-TY%VALUES(IPOS(1)))/
     .     (TABLE%X(1)%VALUES(IPOS(1)+1)-TABLE%X(1)%VALUES(IPOS(1)))

C-----
      END SELECT

      RETURN
      END SUBROUTINE TABLE_INTERP_DYDX
Chd|====================================================================
Chd|  TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        ASSO_PLAS76                   source/materials/mat/mat076/asso_plas76.F
Chd|        ASSO_QPLAS76C                 source/materials/mat/mat076/asso_qplas76c.F
Chd|        CONDAMAGE                     source/materials/mat/mat190/condamage.F
Chd|        CONVERSION                    source/materials/mat/mat190/conversion.F
Chd|        FAIL_GENE1_C                  source/materials/fail/gene1/fail_gene1_c.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_INIEVO_C                 source/materials/fail/inievo/fail_inievo_c.F
Chd|        FAIL_INIEVO_S                 source/materials/fail/inievo/fail_inievo_s.F
Chd|        FAIL_TAB2_C                   source/materials/fail/tabulated/fail_tab2_c.F
Chd|        FAIL_TAB2_S                   source/materials/fail/tabulated/fail_tab2_s.F
Chd|        FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|        GET_U_VTABLE                  source/user_interface/utable.F
Chd|        GET_VTABLE_VALUE              source/user_interface/utable.F
Chd|        LAW119_MEMBRANE               source/materials/mat/mat119/law119_membrane.F
Chd|        NO_ASSO_LPLAS76C              source/materials/mat/mat076/no_asso_lplas76c.F
Chd|        NO_ASSO_PLAS76                source/materials/mat/mat076/no_asso_plas76.F
Chd|        NO_ASSO_QPLAS76C              source/materials/mat/mat076/no_asso_qplas76c.F
Chd|        SIGEPS109                     source/materials/mat/mat109/sigeps109.F
Chd|        SIGEPS109C                    source/materials/mat/mat109/sigeps109c.F
Chd|        SIGEPS110C_LITE_NEWTON        source/materials/mat/mat110/sigeps110c_lite_newton.F
Chd|        SIGEPS110C_LITE_NICE          source/materials/mat/mat110/sigeps110c_lite_nice.F
Chd|        SIGEPS110C_NEWTON             source/materials/mat/mat110/sigeps110c_newton.F
Chd|        SIGEPS110C_NICE               source/materials/mat/mat110/sigeps110c_nice.F
Chd|        SIGEPS120_CONNECT_TAB_DP      source/materials/mat/mat120/sigeps120_connect_tab_dp.F
Chd|        SIGEPS120_CONNECT_TAB_VM      source/materials/mat/mat120/sigeps120_connect_tab_vm.F
Chd|        SIGEPS120_TAB_DP              source/materials/mat/mat120/sigeps120_tab_dp.F
Chd|        SIGEPS120_TAB_VM              source/materials/mat/mat120/sigeps120_tab_vm.F
Chd|        SIGEPS73C                     source/materials/mat/mat073/sigeps73c.F
Chd|        SIGEPS74                      source/materials/mat/mat074/sigeps74.F
Chd|        SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|        SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_VINTERP(TABLE,NEL0,IPOS,XX,YY,DYDX1)
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
      R(1:4,1:NEL0) = zero
      NDIM=TABLE%NDIM
      IF( SIZE(XX,2) < TABLE%NDIM )THEN
        CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .              C1='TABLE INTERPOLATION')
        CALL ARRET(2)
      END IF
C-----
      DO K=1,NDIM
        NXK(K)=SIZE(TABLE%X(K)%VALUES)
        DO I=1,NEL0
          IPOS(I,K)=MAX(IPOS(I,K),1)
          M = IPOS(I,K) 
          DX2 = TABLE%X(K)%VALUES(M) - XX(I,K)
          IF(DX2 >= ZERO)THEN
           DO N=M-1,1,-1
            DX2 = TABLE%X(K)%VALUES(N) - XX(I,K)
            IF(DX2<ZERO.OR.N <=1)THEN 
              IPOS(I,K)=MAX(N,1) !N 
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
     .     (R(4,I)*(R(3,I)*(R(2,I)*( TY%VALUES(IB(2,1,1,1,I))
     .                             -TY%VALUES(IB(1,1,1,1,I)))
     .                  +UNR(2,I)*( TY%VALUES(IB(2,2,1,1,I))
     .                             -TY%VALUES(IB(1,2,1,1,I))))
     .          +UNR(3,I)*(R(2,I)*( TY%VALUES(IB(2,1,2,1,I))
     .                             -TY%VALUES(IB(1,1,2,1,I)))
     .                  +UNR(2,I)*( TY%VALUES(IB(2,2,2,1,I))
     .                             -TY%VALUES(IB(1,2,2,1,I)))))
     .  +UNR(4,I)*(R(3,I)*(R(2,I)*( TY%VALUES(IB(2,1,1,1,I))
     .                             -TY%VALUES(IB(1,1,1,1,I)))
     .                  +UNR(2,I)*( TY%VALUES(IB(2,2,1,1,I))
     .                             -TY%VALUES(IB(1,2,1,1,I))))
     .          +UNR(3,I)*(R(2,I)*( TY%VALUES(IB(2,1,2,1,I))
     .                             -TY%VALUES(IB(1,1,2,1,I)))
     .                  +UNR(2,I)*( TY%VALUES(IB(2,2,2,1,I))
     .                             -TY%VALUES(IB(1,2,2,1,I))))))/
     .     (TABLE%X(1)%VALUES(IPOS(I,1)+1)-TABLE%X(1)%VALUES(IPOS(I,1)))

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
c        DYDX1(I)=
c     .        (R(3,I)*(R(2,I)*( TY%VALUES(IB(1,1,1,1,I))
c     .                         -TY%VALUES(IB(2,1,1,1,I)))
c     .              +UNR(2,I)*( TY%VALUES(IB(1,2,1,1,I))
c     .                         -TY%VALUES(IB(2,2,1,1,I))))
c     .      +UNR(3,I)*(R(2,I)*( TY%VALUES(IB(1,1,2,1,I))
c     .                         -TY%VALUES(IB(2,1,2,1,I)))
c     .              +UNR(2,I)*( TY%VALUES(IB(1,2,2,1,I))
c     .                         -TY%VALUES(IB(2,2,2,1,I)))))/
C
        DYDX1(I)=
     .        (R(3,I)*(R(2,I)*( TY%VALUES(IB(2,1,1,1,I))
     .                         -TY%VALUES(IB(1,1,1,1,I)))
     .              +UNR(2,I)*( TY%VALUES(IB(2,2,1,1,I))
     .                         -TY%VALUES(IB(1,2,1,1,I))))
     .      +UNR(3,I)*(R(2,I)*( TY%VALUES(IB(2,1,2,1,I))
     .                         -TY%VALUES(IB(1,1,2,1,I)))
     .              +UNR(2,I)*( TY%VALUES(IB(2,2,2,1,I))
     .                         -TY%VALUES(IB(1,2,2,1,I)))))/
     .     (TABLE%X(1)%VALUES(IPOS(I,1)+1)-TABLE%X(1)%VALUES(IPOS(I,1)))
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
     .        (R(2,I)*( TY%VALUES(IB(2,1,1,1,I))
     .                 -TY%VALUES(IB(1,1,1,1,I)))
     .      +UNR(2,I)*( TY%VALUES(IB(2,2,1,1,I))
     .                 -TY%VALUES(IB(1,2,1,1,I))))/
     .     (TABLE%X(1)%VALUES(IPOS(I,1)+1)-TABLE%X(1)%VALUES(IPOS(I,1)))
       END DO
C-----
       CASE(1)

       DO I=1,NEL0
        DO K=1,1
          UNR(K,I)=ONE-R(K,I)
        END DO
C
        YY(I)=  R(1,I)*TY%VALUES(IPOS(I,1))
     .       +UNR(1,I)*TY%VALUES(IPOS(I,1)+1)
        DYDX1(I)=(TY%VALUES(IPOS(I,1)+1)-TY%VALUES(IPOS(I,1)))/
     .     (TABLE%X(1)%VALUES(IPOS(I,1)+1)-TABLE%X(1)%VALUES(IPOS(I,1)))
       END DO
C-----
      END SELECT
      RETURN
      END SUBROUTINE TABLE_VINTERP
Chd|====================================================================
Chd|  TABLE_INTERP_LAW76            source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        SIGEPS52                      source/materials/mat/mat052/sigeps52.F
Chd|        SIGEPS52C                     source/materials/mat/mat052/sigeps52c.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE_INTERP_LAW76(TABLE,IPOS2,XX,R2,DYDX,YY)
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TTABLE) TABLE
      my_real, 
     .       DIMENSION(:) :: XX
      my_real
     .       YY, R2,DYDX
      INTEGER IPOS2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(TTABLE_XY), POINTER :: TY
      INTEGER NDIM, K, NXK, I, IPOS, IB(2,2,2,2), 
     .        IP,IN,IM,IL,P,N,M,L,N1,N12,N123
      my_real 
     .       DX1,DX,R(2),UNR(2)
C-----------------------------------------------
      NDIM=TABLE%NDIM
      IF( SIZE(XX) < NDIM )THEN
        CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .              C1='TABLE INTERPOLATION')
        CALL ARRET(2)
      END IF
C-----
        R(2)= R2
        k=1
        NXK=SIZE(TABLE%X(1)%VALUES)
        DO I=2,NXK
         DX = TABLE%X(1)%VALUES(I) - XX(1)
         IF(DX >= ZERO.OR.I == NXK)THEN
           IPOS=I-1
           R(1)   =(TABLE%X(1)%VALUES(I)-XX(1))/
     .             (TABLE%X(1)%VALUES(I)-TABLE%X(1)%VALUES(I-1))
           EXIT
         ENDIF
        END DO
C-----
      TY=>TABLE%Y
      SELECT CASE(NDIM)
C-----
       CASE(2)

        N1  =NXK
        DO M=0,1
          IM=N1*(IPOS2-1+M)
          DO L=0,1
            IL=IPOS+L
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
        DYDX=
     .        (R(2)*( TY%VALUES(IB(2,1,1,1))
     .                 -TY%VALUES(IB(1,1,1,1)))
     .      +UNR(2)*( TY%VALUES(IB(2,2,1,1))
     .                 -TY%VALUES(IB(1,2,1,1))))
     .      /(TABLE%X(1)%VALUES(IPOS+1)-TABLE%X(1)%VALUES(IPOS))

C-----
       CASE(1)

        DO K=1,2
          UNR(K)=ONE-R(K)
        END DO
C
        YY=R(1)*TY%VALUES(IPOS)
     .    +UNR(1)*TY%VALUES(IPOS+1)
        DYDX=(TY%VALUES(IPOS+1)-TY%VALUES(IPOS))
     .   /(TABLE%X(1)%VALUES(IPOS+1)-TABLE%X(1)%VALUES(IPOS))

C-----
      END SELECT
      RETURN
      END SUBROUTINE TABLE_INTERP_LAW76

