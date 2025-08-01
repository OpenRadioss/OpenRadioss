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
!||    lecsubmod               ../starter/source/model/submodel/lecsubmod.F
!||--- called by ------------------------------------------------------
!||    lectur                  ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                  ../starter/source/output/message/message.F
!||    euler_vrot              ../starter/source/model/submodel/euler_vrot.F
!||    hm_get_intv             ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count         ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key      ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start         ../starter/source/devtools/hm_reader/hm_option_start.F
!||    usrtos                  ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod      ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod             ../starter/share/message_module/message_mod.F
!||    submodel_mod            ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE LECSUBMOD(ISUBMOD,X,UNITAB,ITABM1,RTRANS,ITAB,LSUBMODEL,IS_DYNA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARLINE, NCHARKEY, NCHARFIELD
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
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  ITABM1(*),ISUBMOD(*),ITAB(*)
      my_real
     .   X(3,*),RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      INTEGER  IS_DYNA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IDU,ISU,NTRANS,UID,
     .           ITRANSSUB,IDSUBOK(NSUBMOD),ISUBOK,IGU,I0,
     .           I1,N0,N1,IFLAGUNIT,IDNOD,NTAG,CTAG,INUM,SIDTRANS,
     .           IDSUB,ITY,K,
     .           CUR_SUBMOD,SUB_LEVEL,NUMNUSR,NUMNUSR2
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNODSUB ! NUMNOD
      INTEGER :: WORK(70000)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX,INDEX1,TAGNODSUB_TMP,IDNODSUB
      CHARACTER(LEN=NCHARKEY) ::  KEY
      CHARACTER(LEN=NCHARLINE) ::CART,MESS
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER(LEN=NCHARFIELD) ::VERS_IN,STRING
      MY_REAL
     .        BID
      my_real
     .   TX,TY,TZ,ANGLE,FAC_L,X0(3),X1(3),ROT(9),S,XP,YP,ZP,
     .   XCOLD(3), XCNEW(3), SX, SY, SZ
      DATA MESS/'SUBMODEL DEFINITION                   '/
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  USRTOS
      EXTERNAL USRTOS
C=======================================================================
      ALLOCATE(TAGNODSUB(NUMNOD))
      ISU = 0
      IS_AVAILABLE = .FALSE.
      UID = 0
      TAGNODSUB = 0
      NUMNUSR = 0

      IF(IS_DYNA /= 0)THEN
        CALL CPP_NODES_COUNT(NUMNUSR,NUMNUSR2)
        ALLOCATE( INDEX(2*NUMNUSR))
        DO I=1,2*NUMNUSR
         INDEX(I)=I
        ENDDO
        ALLOCATE( INDEX1(2*NUMNOD) )
        DO I=1,2*NUMNOD
         INDEX1(I)=I
        ENDDO
        ALLOCATE( TAGNODSUB_TMP(NUMNUSR) )
        DO I=1,NUMNUSR
         TAGNODSUB_TMP(I)=I
        ENDDO
        ALLOCATE( IDNODSUB(NUMNUSR) )
        DO I=1,NUMNUSR
         IDNODSUB(I)=I
        ENDDO 
      ENDIF
C--------------------------------------------------
C      TAG SUBMODEL NODES DYNA
C-------------------------------------------------- 
      IF(IS_DYNA /= 0)THEN
        CALL CPP_NODE_SUB_TAG_DYNA(TAGNODSUB_TMP,IDNODSUB)

C  Considering that  nodes with the same coordinated duplicated over multiple include files have the SAME USER ID
C we have  IDNODSUB(1:NUMNUSR) => 1:NUMNOD 

        CALL MY_ORDERS( 0, WORK, IDNODSUB, INDEX, NUMNUSR , 1)
        CALL MY_ORDERS( 0, WORK, ITAB, INDEX1, NUMNOD , 1)

        I = 1
        J = 1 
        DO WHILE(J <= NUMNUSR .AND. I <= NUMNOD)
C         TAGNODSUB(1:NUMNOD) TAGNODSUB_TMP(1:NUMNUSR) 
          IF(ITAB(INDEX1(I)) == IDNODSUB(INDEX(J))) THEN
             TAGNODSUB(INDEX1(I)) = TAGNODSUB_TMP(INDEX(J))
             I = I + 1
             J = J + 1
          ELSE IF(ITAB(INDEX1(I)) < IDNODSUB(INDEX(J))) THEN
             I = I + 1
          ELSE
             J = J + 1
          ENDIF
        ENDDO
C--------------------------------------------------
C      TAG SUBMODEL NODES RADIOSS
C-------------------------------------------------- 
      ELSE
        CALL CPP_NODE_SUB_TAG(TAGNODSUB)
      ENDIF
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
c      CALL UDOUBLE(ISUBMOD,LSUBMOD,NSUBMOD,MESS,0,BID)
C-------------------------
c dim LSUBMODEL(I)%IDTRANS()     
C-------------------------
      CALL HM_OPTION_COUNT('TRANSFORM',NTRANS)
      CALL HM_OPTION_START('TRANSFORM') 
      DO I=1,NTRANS
        TITR = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR)

        CALL HM_GET_INTV('SUBMODEL',ITRANSSUB,IS_AVAILABLE,LSUBMODEL)

        IF (ITRANSSUB /= 0) THEN
          ISUBOK = 0
          DO J=1,NSUBMOD
            IF (LSUBMODEL(J)%NOSUBMOD == ITRANSSUB) THEN
              LSUBMODEL(J)%NBTRANS = LSUBMODEL(J)%NBTRANS + 1
              EXIT
            ENDIF 
          ENDDO
        ENDIF
      ENDDO
      DO I=1,NSUBMOD
        SIDTRANS = LSUBMODEL(I)%NBTRANS
        ALLOCATE(LSUBMODEL(I)%IDTRANS(SIDTRANS)) 
        LSUBMODEL(I)%IDTRANS = 0
      ENDDO   
C----
C-------------------------
c build LSUBMODEL(I)%IDTRANS() 
C------------------------- 
      IDSUBOK = 0
      CALL HM_OPTION_START('TRANSFORM') 
      DO I=1,NTRANS
        TITR = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR)
C----
        CALL HM_GET_INTV('SUBMODEL',ITRANSSUB,IS_AVAILABLE,LSUBMODEL)
        IF (ITRANSSUB /= 0) THEN
          ISUBOK = 0
          DO J=1,NSUBMOD
            IF (LSUBMODEL(J)%NOSUBMOD == ITRANSSUB) THEN
              IDSUBOK(J) = IDSUBOK(J)+1
              LSUBMODEL(J)%IDTRANS(IDSUBOK(J)) = I
              ISUBOK = 1 
              EXIT
            ENDIF 
          ENDDO
        ENDIF
        IF (ITRANSSUB /= 0 .AND. ISUBOK == 0) THEN
          CALL ANCMSG(MSGID=915,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ITRANSSUB)
        ENDIF
C----
      ENDDO
C-------------------------
c MAKE TRANSFORMATION ON SUBMODEL NODES
C------------------------- 
      DO I=1,NSUBMOD
       CUR_SUBMOD = I
       SUB_LEVEL = LSUBMODEL(I)%LEVEL
       DO WHILE (SUB_LEVEL /= 0)
        IF (LSUBMODEL(CUR_SUBMOD)%NBTRANS /= 0) THEN 
         DO J = 1,LSUBMODEL(CUR_SUBMOD)%NBTRANS
          ITY = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),2)
          IF(ITY == 1)THEN
            TX=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),15)
            TY=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),16)
            TZ=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),17)
            DO K=1,NUMNOD
              IF(TAGNODSUB(K) == I) THEN
                  X(1,K)=X(1,K)+TX
                  X(2,K)=X(2,K)+TY
                  X(3,K)=X(3,K)+TZ
              ENDIF
            ENDDO
          ELSEIF(ITY == 2)THEN
            DO K=1,9
              ROT(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+2)
            ENDDO
            DO K=1,3
              X0(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+11)
            ENDDO

            DO K=1,NUMNOD
              IF(TAGNODSUB(K) == I)  CALL EULER_VROT(X0,X(1,K),ROT)
            ENDDO
          ELSEIF(ITY == 3)THEN 
            TX = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),15)
            TY = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),16)
            TZ = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),17)
            DO K=1,9
              ROT(K) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+2)
            ENDDO
            DO K=1,NUMNOD
              IF(TAGNODSUB(K) == I) THEN
                XP = ROT(1)*X(1,K) + ROT(4)*X(2,K) + ROT(7)*X(3,K) + TX
                YP = ROT(2)*X(1,K) + ROT(5)*X(2,K) + ROT(8)*X(3,K) + TY
                ZP = ROT(3)*X(1,K) + ROT(6)*X(2,K) + ROT(9)*X(3,K) + TZ
                X(1,K) = XP
                X(2,K) = YP
                X(3,K) = ZP
              ENDIF
            ENDDO
          ELSEIF(ITY == 4)THEN
            DO K=1,9
              ROT(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+2)
            ENDDO
            DO K=1,3
              XCOLD(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+11)
            ENDDO
            DO K=1,3
              XCNEW(K) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+14)
            ENDDO
            DO K=1,NUMNOD
              IF(TAGNODSUB(K) == I)  THEN
                XP = X(1,K) - XCOLD(1)
                YP = X(2,K) - XCOLD(2)
                ZP = X(3,K) - XCOLD(3)
                X(1,K) = XCNEW(1) + ROT(1)*XP + ROT(4)*YP + ROT(7)*ZP                        
                X(2,K) = XCNEW(2) + ROT(2)*XP + ROT(5)*YP + ROT(8)*ZP                         
                X(3,K) = XCNEW(3) + ROT(3)*XP + ROT(6)*YP + ROT(9)*ZP      
              END IF                   
            ENDDO
          ELSEIF(ITY == 5)THEN 
            DO K=1,3
              X0(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+11)
            ENDDO
            DO K=1,3
              X1(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+14)
            ENDDO
            TX = X1(1) - X0(1)
            TY = X1(2) - X0(2)
            TZ = X1(3) - X0(3)
            S = ONE/MAX(SQRT(TX*TX + TY*TY + TZ*TZ),EM20)
            TX = TX*S
            TY = TY*S
            TZ = TZ*S
            DO K=1,NUMNOD
              IF(TAGNODSUB(K) == I) THEN
                SX = X(1,K) - X0(1)
                SY = X(2,K) - X0(2)
                SZ = X(3,K) - X0(3)
                S = SX*TX + SY*TY + SZ*TZ
                X(1,K) = X(1,K) - TWO*TX*S
                X(2,K) = X(2,K) - TWO*TY*S
                X(3,K) = X(3,K) - TWO*TZ*S
              ENDIF
            ENDDO
          ELSEIF(ITY == 6)THEN 
            SX = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),20)
            SY = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),21)
            SZ = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),22)
            DO K=1,3
              X0(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+11)
            ENDDO
            DO K=1,NUMNOD
              IF(TAGNODSUB(K) == I) THEN
                X(1,K) = X0(1) + X(1,K) * SX
                X(2,K) = X0(2) + X(2,K) * SY
                X(3,K) = X0(3) + X(3,K) * SZ
              ENDIF
            ENDDO
          ENDIF
         ENDDO
        ENDIF
        SUB_LEVEL = SUB_LEVEL - 1
        CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER
       ENDDO
      ENDDO
C-------------------------
      IF(IS_DYNA /= 0)THEN
        IF (ALLOCATED(INDEX)) DEALLOCATE(INDEX)
        IF (ALLOCATED(INDEX1)) DEALLOCATE(INDEX1)
        IF (ALLOCATED(TAGNODSUB_TMP)) DEALLOCATE(TAGNODSUB_TMP)
        IF (ALLOCATED(IDNODSUB)) DEALLOCATE(IDNODSUB)
      ENDIF
      DEALLOCATE(TAGNODSUB)
C-------------------------
c MAKE TRANSFORMATION ON SUBMODEL NODES
C-------------------------   
c      IF (IPRI > 5) THEN 
c        IF (ITRANSSUB > 0) WRITE (IOUT,100)                                           
c        WRITE (IOUT,1000)                                            
c        DO K=1,NUMNOD                                                
c          WRITE(IOUT,1500) ITAB(K),X(1,K),X(2,K),X(3,K)                                
c        ENDDO
c      ENDIF 
C-------------------------
c 100  FORMAT(//
c     .'  NODAL TRANSFORMATIONS       '/,
c     .'  ---------------------- ')
c 1000 FORMAT(/10X,'NEW NODE COORDINATES',14X,'X',24X,'Y',24X,'Z')
c 1500 FORMAT( 17X,I10,3(5X,E20.13))
      RETURN
      END
