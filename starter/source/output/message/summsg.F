Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SUMMSG                        source/output/message/summsg.F
Chd|-- called by -----------
Chd|        ANPRINT                       source/output/analyse/analyse_arret.F
Chd|        ARRET                         source/system/arret.F         
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SUMMSG()
      USE MESSAGE_MOD
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  IEND,CPT,MODE,I,J,NBERR,NBWARN,K,CPTWARN,CPTERR
      INTEGER  ITAB(5,IWARN+IERR), WORK(70000), INDEX_WARN(5*(IWARN+IERR)),
     .         INDEX_ERR(5*(IWARN+IERR)),ITRI_WARN(5,IWARN+IERR+1),
     .         ITRI_ERR(5,IWARN+IERR+1),IOPTIONTYPE(50,IWARN+IERR), 
     .         INDEXOPT(50*(IWARN+IERR)),INDEXOPT_1(50*(IWARN+IERR)),
     .         W_CPT_TYP(IWARN+IERR),E_CPT_TYP(IWARN+IERR),
     .         W_OPTION_INDEX(IWARN+IERR+2),E_OPTION_INDEX(IWARN+IERR+2)
      CHARACTER*20 TITLE(IWARN+IERR)
      CHARACTER*30 W_OPTION_TYPE(IWARN+IERR),
     .             E_OPTION_TYPE(IWARN+IERR)
      CHARACTER*200 TMPIN
      CHARACTER*60 OPTIONTYPE(IWARN+IERR)
      CHARACTER*ncharline MYFMT,TMP_CHAR
c      CHARACTER*59 SUM_WARN(IWARN*10),SUM_ERR(IERR)
      CHARACTER*59 , DIMENSION(:), ALLOCATABLE :: SUM_WARN,SUM_ERR
C-----------------------------------------------
        REWIND(RES_CHECK)
c///////////////////////////////////
c WARNING(s)
c///////////////////////////////////
        CPT = 0
        W_CPT_TYP(1:IWARN+IERR) = 0
        INDEXOPT = 0
        INDEXOPT_1 = 0
        INDEX_WARN = 0
        W_OPTION_INDEX(1:IWARN+IERR+2) = 0
        ITRI_WARN = 0


        IEND = 0
        DO WHILE(CPT < IWARN)
          READ(RES_CHECK,'(A)',END=110) TMPIN
          IF(TMPIN(1:9)== 'W_OPTION=')THEN
            CPT = CPT + 1
            INDEXOPT_1(CPT) = CPT
            INDEX_WARN(CPT) = CPT
            READ(TMPIN(10:110),'(A)') OPTIONTYPE(CPT)
            DO I=1,50
              IOPTIONTYPE(I,CPT) = ICHAR(OPTIONTYPE(CPT)(I:I)) 
            ENDDO
c
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(13:23),'(I10)') ITAB(2,CPT)
            ITRI_WARN(2,CPT) = ITAB(2,CPT)
c
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(11:21),'(I10)') ITAB(3,CPT)
c
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(8:108),'(A)') TITLE(CPT)
c
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(8:18),'(I10)') ITAB(4,CPT)
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(10:20),'(I10)') ITAB(5,CPT)
          ENDIF
        ENDDO
110     IEND = 1
C
        IF (CPT /= IWARN) THEN
          DO WHILE(CPT < IWARN)
            CPT = CPT + 1
            INDEXOPT_1(CPT) = CPT
            INDEX_WARN(CPT) = CPT
            OPTIONTYPE(CPT) = 'NO CATEGORY'
            DO I=1,50
              IOPTIONTYPE(I,CPT) = ICHAR(OPTIONTYPE(CPT)(I:I)) 
              ITRI_WARN(2,CPT) = 0
              ITAB(4,CPT) = 9998
              ITAB(5,CPT) = 1
            ENDDO
          ENDDO
        ENDIF
c
        MODE = 0
        CALL MY_ORDERS( MODE, WORK, IOPTIONTYPE, INDEXOPT_1, CPT , 50)

        DO I=1,CPT
            INDEXOPT(I) = INDEXOPT_1(I)
        ENDDO
        J = 1
        DO I=2,CPT
          IF(  OPTIONTYPE(INDEXOPT(I))(1:50) == 
     .         OPTIONTYPE(INDEXOPT(I-1))(1:50) ) THEN
                    INDEXOPT(I) = INDEXOPT(I-1)
          ENDIF
        ENDDO
c
        DO I=1,CPT
            ITAB(1,I) = INDEXOPT(I)
            ITRI_WARN(1,I) = ITAB(1,I)
            ITRI_WARN(2,I) = 1
            ITRI_WARN(3,I) = ITAB(4,INDEXOPT_1(I))
            ITRI_WARN(4,I) = 1
            ITRI_WARN(5,I) = 1
        ENDDO
C
        MODE=0
        CALL MY_ORDERS( MODE, WORK, ITRI_WARN, INDEX_WARN, CPT , 5)


c
        REWIND(RES_CHECK)
c
      J = 1
      IF(IWARN /=0) THEN
	    W_CPT_TYP(J) = W_CPT_TYP(J) + 1
	    W_OPTION_TYPE(J) = OPTIONTYPE(INDEXOPT(INDEX_WARN(1)))
            W_OPTION_INDEX(J) = 1
      ENDIF
      DO I=2,CPT
	  IF( ITAB(1,INDEX_WARN(I)) /= ITAB(1,INDEX_WARN(I-1))  )  THEN
            J = J+1
            W_OPTION_INDEX(J) = I
          ENDIF
	  W_CPT_TYP(J) = W_CPT_TYP(J) + 1
	  W_OPTION_TYPE(J) = OPTIONTYPE(INDEXOPT(INDEX_WARN(I)))
      ENDDO
       W_OPTION_INDEX(J+1) = CPT+1
c
      DO I=2,CPT
	IF(ITRI_WARN(3,INDEX_WARN(I)) == ITRI_WARN(3,INDEX_WARN(I-1)) ) THEN
	  ITRI_WARN(4,INDEX_WARN(I)) = ITRI_WARN(4,INDEX_WARN(I-1)) + 1
        ENDIF
      ENDDO
      CPTWARN = CPT
c
      NBWARN = 0
      IF(IWARN /=0)  INDEX_WARN(W_OPTION_INDEX(J+1)) = CPT + 1
c
c///////////////////////////////////
c ERROR(s)
c///////////////////////////////////
        CPT = 0
        E_CPT_TYP(1:IWARN+IERR) = 0
        INDEXOPT = 0
        INDEXOPT_1 = 0
        INDEX_ERR = 0
        E_OPTION_INDEX(1:IWARN+IERR+2) = 0
        ITRI_ERR = 0


        IEND = 0
        DO WHILE(CPT < IERR)
          READ(RES_CHECK,'(A)',END=120) TMPIN
          IF(TMPIN(1:9)== 'E_OPTION=')THEN
            CPT = CPT + 1
            INDEXOPT_1(CPT) = CPT
            INDEX_ERR(CPT) = CPT
            READ(TMPIN(10:110),'(A)') OPTIONTYPE(CPT)
            DO I=1,50
              IOPTIONTYPE(I,CPT) = ICHAR(OPTIONTYPE(CPT)(I:I)) 
            ENDDO
c
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(13:23),'(I10)') ITAB(2,CPT)
            ITRI_ERR(2,CPT) = ITAB(2,CPT)
c
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(11:21),'(I10)') ITAB(3,CPT)
c
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(8:108),'(A)') TITLE(CPT)
c
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(8:18),'(I10)') ITAB(4,CPT)
            READ(RES_CHECK,'(A)',END=110) TMPIN
            READ(TMPIN(10:20),'(I10)') ITAB(5,CPT)
          ENDIF
        ENDDO
120     IEND = 1
c
        IF (CPT /= IERR) THEN
          DO WHILE(CPT < IERR)
            CPT = CPT + 1
            INDEXOPT_1(CPT) = CPT
            INDEX_ERR(CPT) = CPT
            OPTIONTYPE(CPT) = 'NO CATEGORY'
            DO I=1,50
              IOPTIONTYPE(I,CPT) = ICHAR(OPTIONTYPE(CPT)(I:I)) 
              ITRI_ERR(2,CPT) = 0
              ITAB(4,CPT) = 9998
              ITAB(5,CPT) = 1
            ENDDO
          ENDDO
        ENDIF
c
        MODE = 0
        CALL MY_ORDERS( MODE, WORK, IOPTIONTYPE, INDEXOPT_1, CPT , 50)

        DO I=1,CPT
            INDEXOPT(I) = INDEXOPT_1(I)
        ENDDO
        J = 1
        DO I=2,CPT
          IF(  OPTIONTYPE(INDEXOPT(I))(1:50) == 
     .         OPTIONTYPE(INDEXOPT(I-1))(1:50) ) THEN
                    INDEXOPT(I) = INDEXOPT(I-1)
          ENDIF
        ENDDO
c
        DO I=1,CPT
            ITAB(1,I) = INDEXOPT(I)
            ITRI_ERR(1,I) = ITAB(1,I)
            ITRI_ERR(2,I) = 1
            ITRI_ERR(3,I) = ITAB(4,INDEXOPT_1(I))
            ITRI_ERR(4,I) = 1
            ITRI_ERR(5,I) = 1
        ENDDO
C
        MODE=0
        CALL MY_ORDERS( MODE, WORK, ITRI_ERR, INDEX_ERR, CPT , 5)


c
        REWIND(RES_CHECK)
c
      J = 1
      IF(IERR /=0) THEN
	    E_CPT_TYP(J) = E_CPT_TYP(J) + 1
	    E_OPTION_TYPE(J) = OPTIONTYPE(INDEXOPT(INDEX_ERR(1)))
            E_OPTION_INDEX(J) = 1
      ENDIF
      DO I=2,CPT
	  IF( ITAB(1,INDEX_ERR(I)) /= ITAB(1,INDEX_ERR(I-1))  )  THEN
            J = J+1
            E_OPTION_INDEX(J) = I
          ENDIF
	  E_CPT_TYP(J) = E_CPT_TYP(J) + 1
	  E_OPTION_TYPE(J) = OPTIONTYPE(INDEXOPT(INDEX_ERR(I)))
      ENDDO
       E_OPTION_INDEX(J+1) = CPT+1
c
      DO I=2,CPT
	IF(ITRI_ERR(3,INDEX_ERR(I)) == ITRI_ERR(3,INDEX_ERR(I-1)) ) THEN
	  ITRI_ERR(4,INDEX_ERR(I)) = ITRI_ERR(4,INDEX_ERR(I-1)) + 1
        ENDIF
      ENDDO
      CPTERR = CPT
c
      NBERR = 0
      IF(IERR /=0) INDEX_ERR(E_OPTION_INDEX(J+1)) = CPT + 1
c
c///////////////////////////////////
c OUTPUT
c///////////////////////////////////
      ALLOCATE(SUM_WARN(CPTWARN*10),SUM_ERR(CPTERR*10))
      IF( IERR + IWARN /= 0 ) THEN
c
            IF(CPTWARN /= 0) THEN 
              NBWARN = NBWARN + 1
              WRITE(TMP_CHAR ,'(A)') '  '
              DO I=1,58
                SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
              ENDDO 
            ENDIF
c
            DO J=1,IERR+IWARN 
              IF(W_CPT_TYP(J) /= 0) THEN 
                NBWARN = NBWARN + 1
                WRITE(TMP_CHAR ,'(2X,A,A,5X,I6,1X,A)') '---',
     .          	 W_OPTION_TYPE(J),W_CPT_TYP(J),'WARNING(S) '
                DO I=1,58
                  SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
                ENDDO 
c
                NBWARN = NBWARN + 1
                WRITE(TMP_CHAR ,'(7X,A)') '|'
                DO I=1,58
                  SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
                ENDDO 
c
                NBWARN = NBWARN + 1
                WRITE(TMP_CHAR ,'(7X,A)') '|'
                DO I=1,58
                  SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
                ENDDO 
c
                DO K=W_OPTION_INDEX(J),W_OPTION_INDEX(J+1)-1
                  IF(ITRI_WARN(3,INDEX_WARN(K+1)) /= ITRI_WARN(3,INDEX_WARN(K)) )THEN
                   NBWARN = NBWARN + 1
                   IF(ITRI_WARN(3,INDEX_WARN(K)) /= 9998) THEN
                     WRITE(TMP_CHAR ,'(7X,A,I6,X,A,I6)') '|---',
     .                   ITRI_WARN(4,INDEX_WARN(K)),'WARNING ID :       ',ITRI_WARN(3,INDEX_WARN(K))
                   ELSE
                     WRITE(TMP_CHAR ,'(7X,A,I6,A)') '|---',
     .                   ITRI_WARN(4,INDEX_WARN(K)),'WARNING ID :       NO ID '
                   ENDIF
                   DO I=1,58
                     SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
                   ENDDO 
                   IF (ALLOCATED(MESSAGES(1,ITRI_WARN(3,INDEX_WARN(K)))%MESSAGE)) THEN
                     NBWARN = NBWARN + 1
                     WRITE(TMP_CHAR ,'(7X,A,A)') '| ',
     .                   MESSAGES(1,ITRI_WARN(3,INDEX_WARN(K)))%MESSAGE(1)(1:50)
                     DO I=1,58
                       SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
                     ENDDO 
                   ENDIF
c
                   NBWARN = NBWARN + 1
                   WRITE(TMP_CHAR ,'(7X,A)') '|'
                   DO I=1,58
                     SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
                   ENDDO 
                  ENDIF
c
                ENDDO
c
                NBWARN = NBWARN + 1
                WRITE(TMP_CHAR ,'(A)') '  '
                DO I=1,58
                  SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
                ENDDO 
                NBWARN = NBWARN + 1
                WRITE(TMP_CHAR ,'(A)') '  '
                DO I=1,58
                  SUM_WARN(NBWARN)(I:I) = TMP_CHAR(I:I)
                ENDDO 
C
              ENDIF
            ENDDO
C
c
            IF(CPTERR /= 0)THEN
              NBERR = NBERR + 1
              WRITE(TMP_CHAR ,'(A)') '  '
              DO I=1,58
                SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
              ENDDO 
            ENDIF
            DO J=1,IERR+IWARN  
              IF(E_CPT_TYP(J) /= 0) THEN 
                NBERR = NBERR + 1
                WRITE(TMP_CHAR ,'(2X,A,A,5X,I6,1X,A)') '---',
     .          	 E_OPTION_TYPE(J),E_CPT_TYP(J),'  ERROR(S) '
                DO I=1,58
                  SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
                ENDDO 
c
                NBERR = NBERR+ 1
                WRITE(TMP_CHAR ,'(7X,A)') '|'
                DO I=1,58
                  SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
                ENDDO 
c
                NBERR = NBERR+ 1
                WRITE(TMP_CHAR ,'(7X,A)') '|'
                DO I=1,58
                  SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
                ENDDO 
c
                DO K=E_OPTION_INDEX(J),E_OPTION_INDEX(J+1)-1
                  IF(ITRI_ERR(3,INDEX_ERR(K+1)) /= ITRI_ERR(3,INDEX_ERR(K)) )THEN
                   NBERR = NBERR + 1
C
                   IF(ITRI_ERR(3,INDEX_ERR(K)) /= 9998) THEN
                     WRITE(TMP_CHAR ,'(7X,A,I6,X,A,I6)') '|---',
     .                   ITRI_ERR(4,INDEX_ERR(K)),'  ERROR ID :       ',ITRI_ERR(3,INDEX_ERR(K))
                   ELSE
                     WRITE(TMP_CHAR ,'(7X,A,I6,A)') '|---',
     .                   ITRI_ERR(4,INDEX_ERR(K)),'  ERROR ID :       NO ID '
                   ENDIF
C
                    DO I=1,58
                      SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
                    ENDDO
c
                   IF (ALLOCATED(MESSAGES(1,ITRI_ERR(3,INDEX_ERR(K)))%MESSAGE)) THEN
                     NBERR = NBERR + 1
                     WRITE(TMP_CHAR ,'(7X,A,A)') '| ',
     .                   MESSAGES(1,ITRI_ERR(3,INDEX_ERR(K)))%MESSAGE(1)(1:50)
                     DO I=1,58
                       SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
                     ENDDO 
                   ENDIF
c
                   NBERR = NBERR + 1
                   WRITE(TMP_CHAR ,'(7X,A)') '|'
                   DO I=1,58
                     SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
                   ENDDO 
                  ENDIF 
                ENDDO
c
                NBERR = NBERR + 1
                WRITE(TMP_CHAR ,'(A)') '  '
                DO I=1,58
                  SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
                ENDDO 
                NBERR = NBERR + 1
                WRITE(TMP_CHAR ,'(A)') '  '
                DO I=1,58
                  SUM_ERR(NBERR)(I:I) = TMP_CHAR(I:I)
                ENDDO 
C
              ENDIF
            ENDDO
C
            WRITE(IOUT,'(/)')
            MYFMT='(A)'
            WRITE(IOUT,MYFMT)'----------------------------------------------
     .----------------------------------------------------------------------
     .----'
            WRITE(IOUT,MYFMT)'|            ERROR(S) SUMMARY                 
     .              |             WARNING(S) SUMMARY                        
     .   |'
            WRITE(IOUT,MYFMT)'----------------------------------------------
     .----------------------------------------------------------------------
     .----'
            DO I=1,MAX(NBERR,NBWARN)
              TMP_CHAR = ''
              TMP_CHAR(1:1) = '|'
              IF (I <= NBERR) THEN
                DO J=2,59
                  TMP_CHAR(J:J) = SUM_ERR(I)(J-1:J-1)
                ENDDO
              ELSE
                DO J=2,59
                  TMP_CHAR(J:J) = ' '
                ENDDO
              ENDIF
              TMP_CHAR(61:61) = '|'
              IF (I <= NBWARN) THEN
                DO J=62,119
                  TMP_CHAR(J:J) = SUM_WARN(I)(J-61:J-61)
                ENDDO
              ELSE
                DO J=62,119
                  TMP_CHAR(J:J) = ' '
                ENDDO
              ENDIF
              TMP_CHAR(120:120) = '|'
              WRITE(IOUT,MYFMT)TMP_CHAR(1:120)
            ENDDO
            MYFMT='(A)'
            WRITE(IOUT,MYFMT)'----------------------------------------------
     .----------------------------------------------------------------------
     .----'
            WRITE(IOUT,'(/)')
            WRITE(IOUT,'(/)')
      ENDIF
      DEALLOCATE(SUM_WARN,SUM_ERR)




          
      RETURN
      END SUBROUTINE
