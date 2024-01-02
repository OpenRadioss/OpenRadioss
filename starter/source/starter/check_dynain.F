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
Chd|  CHECK_DYNAIN                  source/starter/check_dynain.F 
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHECK_DYNAIN(IPART,IPARTC,IPARTTG,IXC,IXTG,DYNAIN_CHECK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr15_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*), IXC(NIXC,*), IXTG(NIXTG,*),
     .        IPARTC(*), IPARTTG(*)
      INTEGER , INTENT(INOUT) :: DYNAIN_CHECK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N ,NELC , NELTG , IO_ERR1 , IP , NPRT , 
     .   FLG_CHK , IS_CHECK , JWARN, NELMIN , NELMAX , IPRT ,
     .   NELTGG , NELCG , IPRTALL , IERR2 ,IS_READ,
     .   DYNPART(NPART), IPART_DYNAIN(NPART)
C        
      INTEGER WORK(70000)
      INTEGER ,  DIMENSION(:),ALLOCATABLE :: NELIDC ,NELIDTG,
     .        CLEFC ,CLEFTG ,INDXC ,INDXTG ,IDWARN ,NELIDCG ,NELIDTGG
      my_real
     .   T0,DT0
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80 ,CARTE*ncharline
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
C-------------------------------------------------------------------------------
C     CHECK FOR DYNAIN FILE OUTPUT : 3node shell and 4node shell have same ID
C-------------------------------------------------------------------------------  

C-----------------------------------------------
C     READING ENGINE FILE
C-----------------------------------------------   
      FILNAM=ROOTNAM(1:ROOTLEN)//'_0001.rad'
      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))    
      LEN_TMP_NAME = INFILE_NAME_LEN+LEN_TRIM(FILNAM)
      OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
C
      IF (IO_ERR1/=0) THEN
         FILNAM=ROOTNAM(1:ROOTLEN)//'D01'
         TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))    
         LEN_TMP_NAME = INFILE_NAME_LEN+LEN_TRIM(FILNAM)
         OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
      ENDIF      
C
      NELC = 0
      NELTG = 0 
      NPRT = 0
      IPRTALL = 0
      IS_READ = 0
      DYNPART(1:NPART) = 0
      IPART_DYNAIN(1:NPART) = 0
      IF (IO_ERR1==0) THEN
C
10     READ(71,'(A)',END=20) KEYA


        IF(KEYA(1:1)=='#')GOTO 10
        IF(KEYA(1:1)=='$')GOTO 10

C
C--     Check dynain file is requested --
        DYNAIN_CHECK = 1

        IF(KEYA(1:14)=='/DYNAIN/DT/ALL') THEN
          IF(NUMELC/=0)THEN
            ALLOCATE(NELIDC(NUMELC),STAT=IERR2)
            DO I=1,NUMELC
               NELIDC(I) = IXC(NIXC,I)
            ENDDO
            NELC = NUMELC 
          ENDIF
          IF(NUMELTG/=0)THEN
            ALLOCATE(NELIDTG(NUMELTG),STAT=IERR2)
            DO I=1,NUMELTG
               NELIDTG(NELTG) = IXTG(NIXTG,I)
            ENDDO
            NELTG = NUMELTG 
          ENDIF
          NPRT = NPART
          IPRTALL = 1

            DO J=1,NPART
              IPART_DYNAIN(J) = 1
            END DO

         IS_READ = 1

        ELSEIF(KEYA(1:10)=='/DYNAIN/DT') THEN
          READ(71,*,END=20) T0,DT0

          READ(71,'(A)',END=20) CARTE
          J=1
          NPRT = 0
C
C--     Counting and storing parts id --
         IF(CARTE(1:1)/='#'.OR.CARTE(1:1)/='$') THEN

           DO WHILE(CARTE(1:1) /= '/'.OR.LEN_TRIM(CARTE)/=0) 
            DO WHILE (J<=LEN_TRIM(CARTE))
               IF(CARTE(J:J)/=' ') THEN
                 K=J
                 DO WHILE(CARTE(K:K)/=' '.AND.CARTE(K:K)/=CHAR(13).AND.K<=LEN_TRIM(CARTE))
                   K=K+1
                 ENDDO
                 NPRT = NPRT + 1
                 READ(CARTE(J:K-1),'(I10)') IPRT
                 DYNPART(NPRT) = IPRT
                 J = K
               ENDIF
               J = J +1
             ENDDO
             READ(71,*,END=20) CARTE
           ENDDO
         ENDIF
         IS_READ = 1
       ENDIF

       GOTO 10
C  
 20    CONTINUE
       CLOSE(71)

       IF(IS_READ > 0 ) THEN
          IF(NPRT == 0)THEN
             CALL ANCMSG(MSGID=1909,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1)    
   
          ELSEIF(IPRTALL ==0) THEN
C
C--     parts id to local part --

               DO I=1,NPRT
                IP=0
                IPRT = DYNPART(I)
                  DO J=1,NPART
                     IF(IPART(4,J)==IPRT)IP=J
                  END DO
                  IF(IP==0)THEN
                   CALL ANCMSG(MSGID=1908,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=IPRT)
                END IF
                IPART_DYNAIN(IP)=1
             ENDDO
             IF(NUMELC/=0) ALLOCATE(NELIDC(NUMELC),STAT=IERR2)
             IF(NUMELTG/=0) ALLOCATE(NELIDTG(NUMELTG),STAT=IERR2)

          ENDIF
C
C--     Counting concerned elements  --

          NELC = 0 
          DO I=1,NUMELC
              IP = IPARTC(I)
              IF(IPART_DYNAIN(IP)==1) THEN
                NELC = NELC + 1
                NELIDC(NELC) = IXC(NIXC,I)
              ENDIF
          ENDDO
          NELTG = 0 
          DO I=1,NUMELTG
             IP = IPARTTG(I)
              IF(IPART_DYNAIN(IP)==1) THEN
                 NELTG = NELTG + 1
                 NELIDTG(NELTG) = IXTG(NIXTG,I)
              ENDIF
          ENDDO

       ENDIF
C
C
      ENDIF

      FLG_CHK = 0

      IF(NELC/=0.AND.NELTG/=0) FLG_CHK = 1

      IF(FLG_CHK == 1 ) THEN ! IF check is needed
        IS_CHECK = 0

        ALLOCATE(CLEFC(NELC),STAT=IERR2)
        ALLOCATE(INDXC(2*NELC),STAT=IERR2)

        DO N=1,NELC
           INDXC(N)=N
           CLEFC(N)= NELIDC(N)
        END DO
        CALL MY_ORDERS(0,WORK,CLEFC,INDXC,NELC,1)     

        ALLOCATE(CLEFTG(NELTG),STAT=IERR2)
        ALLOCATE(INDXTG(2*NELTG),STAT=IERR2)

        DO N=1,NELTG
           INDXTG(N)=N
           CLEFTG(N)= NELIDTG(N)
        END DO
        CALL MY_ORDERS(0,WORK,CLEFTG,INDXTG,NELTG,1)       

        IF(NELIDTG(INDXTG(1))>=NELIDC(INDXC(1)).AND.NELIDTG(INDXTG(1))<=NELIDC(INDXC(NELC)))THEN
           IS_CHECK = 1
        ENDIF

        IF(NELIDTG(INDXTG(NELTG))>=NELIDC(INDXC(1)).AND.NELIDTG(INDXTG(NELTG))<=NELIDC(INDXC(NELC)))THEN
           IS_CHECK = 1
        ENDIF

        IF(NELIDC(INDXC(1))>=NELIDTG(INDXTG(1)).AND.NELIDC(INDXC(1))<=NELIDTG(INDXTG(NELTG)))THEN
           IS_CHECK = 1
        ENDIF

        IF(NELIDC(INDXC(NELC))>=NELIDTG(INDXTG(1)).AND.NELIDC(INDXC(NELC))<=NELIDTG(INDXTG(NELTG)))THEN
           IS_CHECK = 1
        ENDIF
       
        IF(IS_CHECK == 1) THEN
           NELMIN = MAX(NELIDC(INDXC(1)),NELIDTG(INDXTG(1)))
           NELMAX = MIN(NELIDC(INDXC(NELC)),NELIDTG(INDXTG(NELTG)))

           ALLOCATE(IDWARN(MIN(NELC,NELTG)),STAT=IERR2)
           JWARN = 0
           DO I=1,NELC
              IF(NELIDC(INDXC(I))>=NELMIN.AND.NELIDC(INDXC(I))<=NELMAX) THEN
                 DO J=1,NELTG
                    IF(NELIDTG(INDXTG(J))>=NELMIN.AND.NELIDTG(INDXTG(J))<=NELMAX) THEN
                       IF(NELIDC(INDXC(I))==NELIDTG(INDXTG(J))) THEN
                          JWARN = JWARN + 1
                          IDWARN(JWARN) = NELIDC(INDXC(I)) 
                       ENDIF
                    ENDIF
                 ENDDO
              ENDIF
           ENDDO
           IF(JWARN/=0)THEN
             IF(IPRI>=6)THEN

              WRITE(IOUT,'(A,A)')
     . ' ** ERROR : DYNAIN FILE CAN NOT BE WRITTEN',
     . ' THESE 4 NODE SHELLS AND 3 NODE SHELLS HAVE SAME USER ID'
              WRITE(IOUT,*) IDWARN(1:JWARN)

              CALL ANCMSG(MSGID=1910,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=JWARN)

             ELSE

              CALL ANCMSG(MSGID=1910,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=JWARN)

            ENDIF
           ENDIF
           DEALLOCATE(IDWARN)
        ENDIF

        DEALLOCATE(CLEFC,CLEFTG,INDXC,INDXTG)

      ENDIF

      IF(IS_READ > 0 ) THEN
         IF(NUMELC/=0) DEALLOCATE(NELIDC,STAT=IERR2)
         IF(NUMELTG/=0) DEALLOCATE(NELIDTG,STAT=IERR2)
      ENDIF

      RETURN
      END
