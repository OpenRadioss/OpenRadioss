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
Chd|  READ_DYNAIN                   source/output/dynain/read_dynain.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SPMD_GATHERV_INT              source/mpi/generic/spmd_gatherv_int.F
Chd|        SPMD_GATHER_INT               source/mpi/generic/spmd_gather_int.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|====================================================================
      SUBROUTINE READ_DYNAIN(IPART,DYNAIN_DATA,IPARTC,IPARTTG,IXC,IXTG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STATE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*), IXC(NIXC,*), IXTG(NIXTG,*),IPARTC(*), IPARTTG(*)
      TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDPRT,K_STAT,J,IP
      INTEGER N ,NELC , NELTG , NELCG , NELTGG , 
     .   FLG_CHK , IS_CHECK , JWARN, NELMIN , NELMAX,
     .   MY_SIZEC ,MY_SIZETG ,IERR ,
     .   SIZEC_P0(NSPMD), SIZETG_P0(NSPMD) ,ADRC(NSPMD) ,
     .   ADRTG(NSPMD)
C        
      INTEGER WORK(70000)
      INTEGER ,  DIMENSION(:),ALLOCATABLE :: NELIDC ,NELIDTG,
     .        CLEFC ,CLEFTG ,INDXC ,INDXTG ,IDWARN ,NELIDCG ,
     .        NELIDTGG 
      my_real
     .   T0,DT0
C-----------------------------------------------
      MY_ALLOCATE( DYNAIN_DATA%IPART_DYNAIN,NPART)
      DYNAIN_DATA%IPART_DYNAIN(1:NPART) = 0 
      IF (DYNAIN_DATA%NDYNAINPRT /= 0) THEN
     	  DO I=1,DYNAIN_DATA%NDYNAINPRT
     	    READ(IIN,'(I10)') IDPRT
            IP=0
     	    DO J=1,NPART
     	      IF(IPART(4,J)==IDPRT)IP=J
     	    END DO
     	    IF(IP==0)THEN
     	     CALL ANCMSG(MSGID=290,ANMODE=ANINFO,I1=IDPRT)
             CALL ARRET(2)
          END IF
          DYNAIN_DATA%IPART_DYNAIN(IP)=1
        END DO
      ELSEIF(DYNAIN_DATA%NDYNAINALL /= 0) THEN
     	  DO J=1,NPART
     	    DYNAIN_DATA%IPART_DYNAIN(J) = 1
     	  END DO
      ENDIF

C-------------------------------------------------------------------------------
C     CHECK FOR DYNAIN FILE OUTPUT : 3node shell and 4node shell have same ID
C-------------------------------------------------------------------------------  

      IF(DYNAIN_DATA%DYNAIN_CHECK == 0.AND.(DYNAIN_DATA%NDYNAINPRT /=0 .OR.DYNAIN_DATA%NDYNAINALL /= 0) ) THEN
   
        NELC = 0
        NELTG = 0 
        NELCG = 0
        NELTGG = 0 

        IF(NUMELC/=0) ALLOCATE(NELIDC(NUMELC),STAT=IERR)
        IF(NUMELTG/=0) ALLOCATE(NELIDTG(NUMELTG),STAT=IERR)

        IF(DYNAIN_DATA%NDYNAINALL /= 0) THEN

          IF(NUMELC/=0)THEN
            DO I=1,NUMELC
               NELIDC(I) = IXC(NIXC,I)
            ENDDO
            NELC = NUMELC 
          ENDIF
          IF(NUMELTG/=0)THEN
            DO I=1,NUMELTG
               NELIDTG(NELTG) = IXTG(NIXTG,I)
            ENDDO
            NELTG = NUMELTG 
          ENDIF

        ELSE
          NELC = 0 
          DO I=1,NUMELC
              IP = IPARTC(I)
              IF(DYNAIN_DATA%IPART_DYNAIN(IP)==1) THEN
                 NELC = NELC + 1
                 NELIDC(NELC) = IXC(NIXC,I)
              ENDIF
           ENDDO
           NELTG = 0 
           DO I=1,NUMELTG
              IP = IPARTTG(I)
              IF(DYNAIN_DATA%IPART_DYNAIN(IP)==1) THEN
                 NELTG = NELTG + 1
                 NELIDTG(NELTG) = IXTG(NIXTG,I)
              ENDIF
           ENDDO

        ENDIF


        IF (NSPMD > 1) THEN

      
          SIZEC_P0(1:NSPMD) = 0
          ADRC(1:NSPMD) = 0

      !   send the local size of index to PROC0
          MY_SIZEC = NELC

          CALL SPMD_GATHER_INT(MY_SIZEC,SIZEC_P0,0,1,NSPMD)

          SIZETG_P0(1:NSPMD) = 0
          ADRTG(1:NSPMD) = 0

          MY_SIZETG = NELTG

          CALL SPMD_GATHER_INT(MY_SIZETG,SIZETG_P0,0,1,NSPMD)

          NELCG = 0
          IF(ISPMD==0) THEN
            ADRC(1) = 0
            DO I=1,NSPMD-1
               ADRC(I+1) = ADRC(I) + SIZEC_P0(I)
               NELCG = NELCG + SIZEC_P0(I)
            ENDDO
            NELCG = NELCG + SIZEC_P0(NSPMD)
          ENDIF

          NELTGG = 0
          IF(ISPMD==0) THEN
            ADRTG(1) = 0
            DO I=1,NSPMD-1
               ADRTG(I+1) = ADRTG(I) + SIZETG_P0(I)
               NELTGG = NELTGG + SIZETG_P0(I)
            ENDDO
            NELTGG = NELTGG + SIZETG_P0(NSPMD)
          ENDIF

           ALLOCATE(NELIDCG(NELCG),STAT=IERR)
           ALLOCATE(NELIDTGG(NELTGG),STAT=IERR)

          !   send the local NUMELC to PROC0

          CALL SPMD_GATHERV_INT(NELIDC,NELIDCG,0,MY_SIZEC,NELCG,
     .                          SIZEC_P0,ADRC)
          !   send the local NUMELTG to PROC0
          CALL SPMD_GATHERV_INT(NELIDTG,NELIDTGG,0,MY_SIZETG,NELTGG,
     .                          SIZETG_P0,ADRTG)

        ELSE
          NELCG = NELC
          NELTGG = NELTG
          IF(NELCG/=0) THEN
            ALLOCATE(NELIDCG(NELCG),STAT=IERR)
            NELIDCG(1:NELCG) = NELIDC(1:NELC)
          ENDIF
          IF(NELTGG/=0) THEN     
            ALLOCATE(NELIDTGG(NELTGG),STAT=IERR)     
            NELIDTGG(1:NELTGG) = NELIDTG(1:NELTG)  
          ENDIF  

        ENDIF


        IF(ISPMD == 0) THEN

          FLG_CHK = 0

          IF(NELCG/=0.AND.NELTGG/=0) FLG_CHK = 1
      
          IF(FLG_CHK > 0 ) THEN ! IF checK is needed

              IS_CHECK = 0
   
              ALLOCATE(CLEFC(NELCG),STAT=IERR)
              ALLOCATE(INDXC(2*NELCG),STAT=IERR)

              DO N=1,NELCG
                INDXC(N)=N
                CLEFC(N)= NELIDCG(N)
              END DO
              CALL MY_ORDERS(0,WORK,CLEFC,INDXC,NELCG,1)     

              ALLOCATE(CLEFTG(NELTGG),STAT=IERR)
              ALLOCATE(INDXTG(2*NELTGG),STAT=IERR)

              DO N=1,NELTGG
                INDXTG(N)=N
                CLEFTG(N)= NELIDTGG(N)
              END DO

              CALL MY_ORDERS(0,WORK,CLEFTG,INDXTG,NELTGG,1)         

              IF(NELIDTGG(INDXTG(1))>=NELIDCG(INDXC(1)).AND.NELIDTGG(INDXTG(1))<=NELIDCG(INDXC(NELCG)))THEN
                 IS_CHECK = 1
              ENDIF

              IF(NELIDTGG(INDXTG(NELTGG))>=NELIDCG(INDXC(1)).AND.NELIDTGG(INDXTG(NELTGG))<=NELIDCG(INDXC(NELCG)))THEN
                 IS_CHECK = 1
              ENDIF

              IF(NELIDCG(INDXC(1))>=NELIDTGG(INDXTG(1)).AND.NELIDCG(INDXC(1))<=NELIDTGG(INDXTG(NELTGG)))THEN
                 IS_CHECK = 1
              ENDIF

              IF(NELIDCG(INDXC(NELCG))>=NELIDTGG(INDXTG(1)).AND.NELIDCG(INDXC(NELCG))<=NELIDTGG(INDXTG(NELTGG)))THEN
                 IS_CHECK = 1
              ENDIF
       
              IF(IS_CHECK == 1) THEN
                 NELMIN = MAX(NELIDCG(INDXC(1)),NELIDTGG(INDXTG(1)))
                 NELMAX = MIN(NELIDCG(INDXC(NELCG)),NELIDTGG(INDXTG(NELTGG)))

                 ALLOCATE(IDWARN(MIN(NELCG,NELTGG)),STAT=IERR)

                 JWARN = 0
                 DO I=1,NELCG
                   IF(NELIDCG(INDXC(I))>=NELMIN.AND.NELIDCG(INDXC(I))<=NELMAX) THEN
                     DO J=1,NELTGG
                       IF(NELIDTGG(INDXTG(J))>=NELMIN.AND.NELIDTGG(INDXTG(J))<=NELMAX) THEN
                         IF(NELIDCG(INDXC(I))==NELIDTGG(INDXTG(J))) THEN
                           JWARN = JWARN + 1
                           IDWARN(JWARN) = NELIDCG(INDXC(I)) 
                         ENDIF
                       ENDIF
                     ENDDO
                   ENDIF
                 ENDDO

                 IF(JWARN/=0)THEN
                   WRITE(IOUT,'(A,A)')
     . ' ** ERROR : DYNAIN FILE CAN NOT BE WRITTEN',
     . ' THESE 4 NODE SHELLS AND 3 NODE SHELLS HAVE SAME USER ID'
                  WRITE(IOUT,*) IDWARN(1:JWARN)

                  WRITE(ISTDO,'(A,A,I10,A)')
     . ' ** ERROR : DYNAIN FILE CAN NOT BE WRITTEN',
     . ' 4 NODE SHELLS AND 3 NODE SHELLS MUST TO HAVE DIFFERENT USER ID',
     .  JWARN,'ERROR(S)'
                  CALL ARRET(0)
                 ENDIF

                DEALLOCATE(IDWARN)

              ENDIF
C
              DEALLOCATE(CLEFC,CLEFTG,INDXC,INDXTG)
C
          ENDIF
        ENDIF

        IF(NUMELC/=0) DEALLOCATE(NELIDC,STAT=IERR)
        IF(NUMELTG/=0) DEALLOCATE(NELIDTG,STAT=IERR)
        DEALLOCATE(NELIDCG,NELIDTGG)

      ENDIF


      RETURN
      END
