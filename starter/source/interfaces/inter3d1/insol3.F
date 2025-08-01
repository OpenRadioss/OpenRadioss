Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    insol3      ../starter/source/interfaces/inter3d1/insol3.F
!||--- called by ------------------------------------------------------
!||    i1chk3      ../starter/source/interfaces/inter3d1/i1chk3.F
!||    i20sti3     ../starter/source/interfaces/inter3d1/i20sti3.F
!||    i21els3     ../starter/source/interfaces/inter3d1/i21els3.F
!||    i23gap3     ../starter/source/interfaces/inter3d1/i23gap3.F
!||    i2_surfi    ../starter/source/interfaces/inter3d1/i2_surfi.F90
!||    i2buc1      ../starter/source/interfaces/inter3d1/i2buc1.F
!||    i2chk3      ../starter/source/interfaces/inter3d1/i2chk3.F
!||    i2cor3      ../starter/source/interfaces/inter3d1/i2cor3.F
!||    i3sti3      ../starter/source/interfaces/inter3d1/i3sti3.F
!||    i9sti3      ../starter/source/interfaces/int09/i9sti3.F
!||    r2r_count   ../starter/source/coupling/rad2rad/r2r_count.F
!||--- calls      -----------------------------------------------------
!||    norma1      ../starter/source/interfaces/inter3d1/norma1.F
!||====================================================================
      SUBROUTINE INSOL3(X   ,IRECT,IXS      ,NINT    ,NEL  ,I   ,
     .                 AREA ,NOINT,KNOD2ELS ,NOD2ELS ,IR   ,IXS10,
     .                 IXS16,IXS20)
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
      INTEGER NINT, NEL, I, NOINT,IR
      my_real
     .   AREA
      INTEGER IRECT(4,*), IXS(NIXS,*), KNOD2ELS(*), NOD2ELS(*),
     .        IXS10(6,*), IXS16(8,*), IXS20(12,*)
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IY(4), N, JJ, II, K, NN, KK, IC, IAD,
     .        NUSER, NUSERM
      my_real
     .   N1, N2, N3, DDS
      my_real ::  XX1(4), XX2(4),XX3(4),XS1,YS1,ZS1,XC,YC,ZC
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      NEL=0
      IC=0
      IF(NUMELS==0) RETURN
       IF(IRECT(1,I)>NUMNOD) RETURN
       NUSERM = -1
       IF (NINT<0) NUSER = NOINT
       DO 230 IAD=KNOD2ELS(IRECT(1,I))+1,KNOD2ELS(IRECT(1,I)+1)
        N = NOD2ELS(IAD)
        IF(N <= NUMELS8)THEN
          DO 210 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 210
            ENDDO
            GOTO 230
  210     CONTINUE
        ELSEIF(N <= NUMELS8+NUMELS10)THEN
          DO 220 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 220
            ENDDO
            DO K=1,6
              IF(IXS10(K,N-NUMELS8)==II) GOTO 220
            ENDDO
            GOTO 230
  220     CONTINUE
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20)THEN
          DO 222 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 222
            ENDDO
            DO K=1,12
              IF(IXS20(K,N-NUMELS8-NUMELS10)==II) GOTO 222
            ENDDO
            GOTO 230
  222     CONTINUE
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20+NUMELS16)THEN
          DO 224 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 224
            ENDDO
            DO K=1,8
              IF(IXS16(K,N-NUMELS8-NUMELS10-NUMELS20)==II) GOTO 224
            ENDDO
            GOTO 230
  224     CONTINUE
        ELSE
          GOTO 230
        END IF
        IC=IC+1
        NUSER = IXS(11,N)
        IF (NUSER>NUSERM) THEN
          NEL = N
          NUSERM = NUSER
        ENDIF
  230  CONTINUE
       IF (NUSERM==-1) RETURN
C-----------------------------------------------
C     VERIFICATION DE L'ORIENTATION DES SEGMENTS
C-----------------------------------------------
       XS1=ZERO
       YS1=ZERO
       ZS1=ZERO
       DO 100 JJ=1,4
       NN=IRECT(JJ,I)
       IY(JJ)=NN
       XX1(JJ)=X(1,NN)
       XX2(JJ)=X(2,NN)
       XX3(JJ)=X(3,NN)
       XS1=XS1+FOURTH*X(1,NN)
       YS1=YS1+FOURTH*X(2,NN)
  100  ZS1=ZS1+FOURTH*X(3,NN)
C
       CALL NORMA1(N1,N2,N3,AREA,XX1,XX2,XX3)
       XC=ZERO
       YC=ZERO
       ZC=ZERO
       DO 110 K=1,8
         KK=IXS(K+1,NEL)
         XC=XC+X(1,KK)
         YC=YC+X(2,KK)
         ZC=ZC+X(3,KK)
 110   CONTINUE
       XC=XC*ONE_OVER_8
       YC=YC*ONE_OVER_8
       ZC=ZC*ONE_OVER_8
       IF(IR/=0) RETURN
       IF(IC>=2)RETURN
       DDS=N1*(XC-XS1)+N2*(YC-YS1)+N3*(ZC-ZS1)
       IF(DDS<ZERO) RETURN
       IF(IY(3)==IY(4)) THEN
        IRECT(1,I)=IY(2)
        IRECT(2,I)=IY(1)
       ELSE
        DO 120 KK=1,4
  120   IRECT(KK,I)=IY(4-KK+1)
       ENDIF
       RETURN
C
      END
!||====================================================================
!||    insol3d       ../starter/source/interfaces/inter3d1/insol3.F
!||--- called by ------------------------------------------------------
!||    i24gapm       ../starter/source/interfaces/inter3d1/i24sti3.F
!||    i25gapm       ../starter/source/interfaces/inter3d1/i25sti3.F
!||    i7sti3        ../starter/source/interfaces/inter3d1/i7sti3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../starter/source/output/message/message.F
!||    norma1d       ../starter/source/interfaces/inter3d1/norma1.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE INSOL3D(X   ,IRECT ,IXS     ,NINT   ,NEL,I ,
     .                  AREA ,NOINT ,KNOD2ELS,NOD2ELS,IR ,
     .                  IXS10,IXS16,IXS20    ,TAGELEMS,INDEXE,
     .                  NINV ,IELEM_M ,ELEM_LINKED_TO_SEGMENT,PRINT_ERROR,
     .                  NIN25,NTY     , FLAG_ELEM_INTER25 )
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT, NEL, I, NOINT,IR,J,NINV
      my_real
     .   AREA
      INTEGER IRECT(4,*), IXS(NIXS,*), KNOD2ELS(*), NOD2ELS(*), 
     .        IXS10(6,*), IXS16(8,*), IXS20(12,*)
      INTEGER  , INTENT(INOUT) :: TAGELEMS(NUMELS),INDEXE(NUMELS)
      my_real
     .   X(3,*)
      INTEGER  , INTENT(INOUT) :: IELEM_M(2) ! ID of 1 or 2 solid elements attached to main segment
      INTEGER, DIMENSION(NUMELS), INTENT(INOUT) :: ELEM_LINKED_TO_SEGMENT !< working array, dim=numels
      LOGICAL, INTENT(INOUT) :: PRINT_ERROR !< flag : true if several elements seem to be duplicated
      INTEGER, INTENT(IN) :: NIN25, NTY
      INTEGER, INTENT(IN) :: FLAG_ELEM_INTER25(NINTER25,NUMELS)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IY(4), N, JJ, II, K, NN, KK, IC, IAD,
     .        NUSER, NUSERM, NINDEXE, IELS
      INTEGER :: IJK
      INTEGER :: ELEM_ID
C     REAL
      my_real
     .   N1, N2, N3, DDS
      my_real :: XX1(4),XX2(4),XX3(4),XS1,YS1,ZS1,XC,YC,ZC
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      NEL=0
      IC=0
      PRINT_ERROR = .FALSE.
      IF(NUMELS==0) RETURN
      IF(IRECT(1,I)>NUMNOD) RETURN

       NINDEXE = 0
       NUSERM = -1
       DO 230 IAD=KNOD2ELS(IRECT(1,I))+1,KNOD2ELS(IRECT(1,I)+1)
        N = NOD2ELS(IAD)
        IF(N <= NUMELS8)THEN
          DO 210 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 210
            ENDDO
            GOTO 230
  210     CONTINUE
          NUSER = IXS(11,N)
          IF(TAGELEMS(N)==0) THEN
             IC=IC+1
             TAGELEMS(N) = 1
             NINDEXE = NINDEXE + 1
             INDEXE(NINDEXE) = N
             ELEM_LINKED_TO_SEGMENT(IC) = N
             IF (NUSER>NUSERM) THEN
                NEL = N
                NUSERM = NUSER
             ENDIF
          ENDIF
        ELSEIF(N <= NUMELS8+NUMELS10)THEN
          DO 220 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 220
            ENDDO
            DO K=1,6
              IF(IXS10(K,N-NUMELS8)==II) GOTO 220
            ENDDO
            GOTO 230
  220     CONTINUE
          NUSER = IXS(11,N)
          IF(TAGELEMS(N)==0) THEN
             IC=IC+1
             TAGELEMS(N) = 1
             NINDEXE = NINDEXE + 1
             INDEXE(NINDEXE) = N
             ELEM_LINKED_TO_SEGMENT(IC) = N
             IF (NUSER>NUSERM) THEN
                NEL = N
                NUSERM = NUSER
             ENDIF
          ENDIF
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20)THEN
          DO 222 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 222
            ENDDO
            DO K=1,12
              IF(IXS20(K,N-NUMELS8-NUMELS10)==II) GOTO 222
            ENDDO
            GOTO 230
  222     CONTINUE
          NUSER = IXS(11,N)
          IF(TAGELEMS(N)==0) THEN
             IC=IC+1
             TAGELEMS(N) = 1
             NINDEXE = NINDEXE + 1
             INDEXE(NINDEXE) = N
             ELEM_LINKED_TO_SEGMENT(IC) = N
             IF (NUSER>NUSERM) THEN
                NEL = N
                NUSERM = NUSER
             ENDIF
          ENDIF
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20+NUMELS16)THEN
          DO 224 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 224
            ENDDO
            DO K=1,8
              IF(IXS16(K,N-NUMELS8-NUMELS10-NUMELS20)==II) GOTO 224
            ENDDO
            GOTO 230
  224     CONTINUE
          NUSER = IXS(11,N)
          IF(TAGELEMS(N)==0) THEN
             IC=IC+1
             TAGELEMS(N) = 1
             NINDEXE = NINDEXE + 1
             INDEXE(NINDEXE) = N
             ELEM_LINKED_TO_SEGMENT(IC) = N
             IF (NUSER>NUSERM) THEN
                NEL = N
                NUSERM = NUSER
             ENDIF
          ENDIF
        ELSE
          GOTO 230
        END IF
  230  CONTINUE
       DO JJ= 1,NINDEXE
          N = INDEXE(JJ)
          TAGELEMS(N) = 0
          INDEXE(JJ) = 0
       ENDDO

       IF (NUSERM==-1) RETURN
       IF(IC==1) THEN 
          IELEM_M(1) =   NEL
          IELEM_M(2) =   0
       ELSEIF(IC==2) THEN 
          IELEM_M(1:2) = ELEM_LINKED_TO_SEGMENT(1:2)
          IF(IELEM_M(1)/= NEL) THEN 
            IELS = IELEM_M(1)
            IELEM_M(1) = IELEM_M(2)
            IELEM_M(2) = IELS
          ENDIF
          IF(NTY==25) THEN
            IF(FLAG_ELEM_INTER25(NIN25,IELEM_M(1)) ==1.AND.FLAG_ELEM_INTER25(NIN25,IELEM_M(2)) ==0) THEN
              IC = 1  
            ELSEIF(FLAG_ELEM_INTER25(NIN25,IELEM_M(1)) ==0.AND.FLAG_ELEM_INTER25(NIN25,IELEM_M(2)) ==1) THEN
              IC = 1
              NEL = IELEM_M(2)
              IELEM_M(1) = IELEM_M(2)
              IELEM_M(2) = 0
            ENDIF
          ENDIF
          
       ELSE
          IELEM_M(1:2) = ELEM_LINKED_TO_SEGMENT(1:2)
          PRINT_ERROR = .TRUE.
          DO IJK=1,IC
            ELEM_ID = IXS(11,ELEM_LINKED_TO_SEGMENT(IJK))
            CALL ANCMSG(MSGID=3062,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ELEM_ID ,
     .                PRMOD=MSG_CUMU)
          ENDDO
       ENDIF
C-----------------------------------------------
C     SEGMENTS ORIENTATION CHECKING 
C-----------------------------------------------
       XS1=ZERO
       YS1=ZERO
       ZS1=ZERO
       DO 100 JJ=1,4
       NN=IRECT(JJ,I)
       IY(JJ)=NN
       XX1(JJ)=X(1,NN)
       XX2(JJ)=X(2,NN)
       XX3(JJ)=X(3,NN)
       XS1=XS1+FOURTH*X(1,NN)
       YS1=YS1+FOURTH*X(2,NN)
  100  ZS1=ZS1+FOURTH*X(3,NN)       
C
       CALL NORMA1D(N1,N2,N3,AREA,XX1,XX2,XX3)

       XC=0.
       YC=0.
       ZC=0.
       DO 110 K=1,8
         KK=IXS(K+1,NEL)
         XC=XC+X(1,KK)
         YC=YC+X(2,KK)
         ZC=ZC+X(3,KK)
 110   CONTINUE
       XC=XC*ONE_OVER_8
       YC=YC*ONE_OVER_8
       ZC=ZC*ONE_OVER_8

       IF(IR/=0) RETURN
       IF(IC>=2)RETURN
       DDS=N1*(XC-XS1)+N2*(YC-YS1)+N3*(ZC-ZS1)

       IF(DDS<ZERO) RETURN
       IF(IY(3)==IY(4)) THEN
        IRECT(1,I)=IY(2)
        IRECT(2,I)=IY(1)
       ELSE
        DO 120 KK=1,4
  120   IRECT(KK,I)=IY(4-KK+1)
       ENDIF
c       IF(NINT>0) WRITE (IOUT,1300) I,NOINT
c       IF(NINT<0) WRITE (IOUT,1400) I,NOINT
       NINV = NINV + 1
       IF(IPRI>=10.AND.NINT>0) 
     .         CALL ANCMSG(MSGID=3022,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=I,
     .                         I2=NOINT,
     .                         PRMOD=MSG_CUMU)
       IF(IPRI>=10.AND.NINT< 0) 
     .         CALL ANCMSG(MSGID=3024,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=I,
     .                         I2=NOINT,
     .                         PRMOD=MSG_CUMU)

       RETURN
      END
!||====================================================================
!||    i12sol3                ../starter/source/interfaces/inter3d1/insol3.F
!||--- called by ------------------------------------------------------
!||    i12chk3                ../starter/source/interfaces/inter3d1/i12chk3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    iface                  ../starter/source/ale/ale3d/iface.F
!||    insolt10               ../starter/source/interfaces/inter3d1/insolt10.F
!||    norma1                 ../starter/source/interfaces/inter3d1/norma1.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE I12SOL3(X,IRECT,IXS,NINT,NEL,I,
     .                  AREA,NOINT,IADD,INVC,NF,ITAB,
     .                  KNOD2ELS,NOD2ELS,NTY,ID,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
      INTEGER NINT, NEL, I, NOINT,IR, ITAB(*), KNOD2ELS(*), NOD2ELS(*)
      my_real
     .   AREA
      INTEGER IRECT(4,*), IXS(NIXS,*), IADD(*), INVC(*),NF,NTY
      my_real
     .   X(3,*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IY(4), N, JJ, II, K, NN, KK, IC, IAD,
     .        NUSER, NUSERM,IP(8),CON(8),IBID,MSEGTYP(NUMELS10)
      my_real
     .   N1, N2, N3, DDS
      my_real :: XX1(4), XX2(4),XX3(4),XS1,YS1,ZS1,XC,YC,ZC
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IFACE
      DATA CON/1,2,3,4,5,6,7,8/
C
      IBID = 0
      NEL=0
      IC=0
      IF(NUMELS==0) RETURN
       IF(IRECT(1,I)>NUMNOD) RETURN
       NUSERM = -1
       DO 230 IAD=IADD(IRECT(1,I)),IADD(IRECT(1,I)+1)-1
       DO K=1,8
          IP(K)=0
       ENDDO
        N = INVC(IAD)
        IF(N>NUMELC+NUMELTG.AND.N<=NUMELC+NUMELTG+NUMELS)THEN
          N=N-NUMELC-NUMELTG
          DO 220 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) THEN
                IP(K)=1
                GOTO 220
              ENDIF
            ENDDO
            GOTO 230
  220     CONTINUE
          IC=IC+1
          NF=IFACE(IP,CON)
          NUSER = IXS(11,N)
          IF (NUSER>NUSERM) THEN
            NEL = N
            NUSERM = NUSER
          ENDIF
        ENDIF
  230  CONTINUE
       IF (NUSERM==-1) RETURN
       IF(NEL>NUMELS8.AND.NEL<=NUMELS8+NUMELS10) THEN
         DO K=1,NEL-NUMELS8
          MSEGTYP(K)=10
         ENDDO
     0   CALL INSOLT10(
     1     IXS(1,NEL),IXS(1,NUMELS+1),
     2     IRECT(1,I),NOINT,NEL-NUMELS8,ITAB,
     3     KNOD2ELS,NOD2ELS,NTY,IBID,MSEGTYP,
     4     ID,TITR)
       END IF !(NEL>NUMELS8.AND.NEL<=NUMELS8+NUMELS10) THEN
C
C 2 Elements connects  1 facette !
C
       IF(IC>=2)THEN
         IF(NINT>0) CALL ANCMSG(MSGID=1245,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND,
     .                  I1=I,
     .                  PRMOD=MSG_CUMU)
         IF(NINT<0) CALL ANCMSG(MSGID=1246,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND,
     .                  I1=I,
     .                  PRMOD=MSG_CUMU)
       ENDIF
       
C-----------------------------------------------
C     VERIFICATION DE L'ORIENTATION DES SEGMENTS
C-----------------------------------------------
       XS1=ZERO
       YS1=ZERO
       ZS1=ZERO

       DO 100 JJ=1,4
       NN=IRECT(JJ,I)
       IY(JJ)=NN
       XX1(JJ)=X(1,NN)
       XX2(JJ)=X(2,NN)
       XX3(JJ)=X(3,NN)
       XS1=XS1+FOURTH*X(1,NN)
       YS1=YS1+FOURTH*X(2,NN)
  100  ZS1=ZS1+FOURTH*X(3,NN)       
C
       CALL NORMA1(N1,N2,N3,AREA,XX1,XX2,XX3)
       XC=ZERO
       YC=ZERO
       ZC=ZERO
       DO 110 K=1,8
         KK=IXS(K+1,NEL)
         XC=XC+X(1,KK)
         YC=YC+X(2,KK)
         ZC=ZC+X(3,KK)
 110   CONTINUE
       XC=XC*ONE_OVER_8
       YC=YC*ONE_OVER_8
       ZC=ZC*ONE_OVER_8
       DDS=N1*(XC-XS1)+N2*(YC-YS1)+N3*(ZC-ZS1)
       IF(DDS<0) RETURN
       IF(IY(3)==IY(4)) THEN
        IRECT(1,I)=IY(2)
        IRECT(2,I)=IY(1)
       ELSE
        DO 120 KK=1,4
  120   IRECT(KK,I)=IY(4-KK+1)
       ENDIF

       RETURN

      END
