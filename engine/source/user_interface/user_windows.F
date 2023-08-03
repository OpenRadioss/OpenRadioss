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
Chd|  USER_WINDOWS_ROUTINE          source/user_interface/user_windows.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_USERWI            source/user_interface/dyn_userlib.c
Chd|        SPMD_EXCH_USERWI              source/mpi/user_interface/spmd_exch_userwi.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
       SUBROUTINE USER_WINDOWS_ROUTINE( ISPMD        ,NSPMD         ,USERL_AVAIL   ,
     1                                  USER_WINDOWS ,RAD_INPUTNAME ,LEN_RAD_INPUTNAME,
     2                                  NUMNOD       ,NCYCLE        ,ITAB             ,
     3                                  TT           ,DT1           ,TFEXT            ,
     4                                  D            ,X             ,V                ,
     5                                  VR           ,MS            ,IN               ,
     6                                  STIFN        ,STIFR         ,A                ,
     7                                  AR           ,DT2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE USER_WINDOWS_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)                         :: ISPMD,NSPMD
      INTEGER, INTENT(IN)                         :: USERL_AVAIL
      TYPE(USER_WINDOWS_),INTENT(INOUT)           :: USER_WINDOWS
      INTEGER, INTENT(IN)                         :: LEN_RAD_INPUTNAME
      CHARACTER(LEN=LEN_RAD_INPUTNAME),INTENT(IN) :: RAD_INPUTNAME
      INTEGER, INTENT(IN)                         :: NUMNOD,NCYCLE
      INTEGER, DIMENSION(NUMNOD),INTENT(IN)       :: ITAB
      my_real, INTENT(IN)                         :: TT,DT1,TFEXT
      my_real, DIMENSION(3*NUMNOD), INTENT(IN)    :: D,X,V,VR
      my_real, DIMENSION(NUMNOD), INTENT(IN)      :: MS,IN,STIFN,STIFR
      my_real, DIMENSION(3*NUMNOD), INTENT(INOUT) :: A,AR
      my_real, INTENT(INOUT)                      :: DT2
C-----------------------------------------------
C   Local Variables
C-----------------------------------------------
      INTEGER I,ND,SIZE_OPT,NUVARI
      CHARACTER(LEN=256)                 :: OPTION
      INTEGER, DIMENSION(:) ,ALLOCATABLE :: USER_ITAB
      my_real, DIMENSION(:) ,ALLOCATABLE :: USER_D,USER_X,USER_V,USER_VR
      my_real, DIMENSION(:) ,ALLOCATABLE :: USER_MS,USER_IN,USER_STIFN,USER_STIFR
      my_real, DIMENSION(:) ,ALLOCATABLE :: USER_A,USER_AR
      my_real DTU
C-----------------------------------------------
      IF(ISPMD == 0) THEN
         IF (USERL_AVAIL == 1) THEN
            DTU = EP30
            NUVARI = USER_WINDOWS%NUVARI - 100
           
            ALLOCATE(USER_ITAB(USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_D( 3*USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_X( 3*USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_V( 3*USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_VR(3*USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_MS(USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_IN(USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_STIFN(USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_STIFR(USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_A(3*USER_WINDOWS%N_USERNODS))
            ALLOCATE(USER_AR(3*USER_WINDOWS%N_USERNODS))

            ! -----------------------------------------------------
            !  User Windows will only recieve the nodes it defined 
            ! -----------------------------------------------------
            DO I=1,USER_WINDOWS%N_USERNODS
               ND = USER_WINDOWS%USERNODS(I)
               ! recopy ITAB
               USER_ITAB(I) = ITAB(ND)
               ! recopy D
               USER_D(3*(I-1)+1) = D(3*(ND-1)+1)
               USER_D(3*(I-1)+2) = D(3*(ND-1)+2)
               USER_D(3*(I-1)+3) = D(3*(ND-1)+3)
               ! recopy X
               USER_X(3*(I-1)+1) = X(3*(ND-1)+1)
               USER_X(3*(I-1)+2) = X(3*(ND-1)+2)
               USER_X(3*(I-1)+3) = X(3*(ND-1)+3)
               ! recopy V
               USER_V(3*(I-1)+1) = V(3*(ND-1)+1)
               USER_V(3*(I-1)+2) = V(3*(ND-1)+2)
               USER_V(3*(I-1)+3) = V(3*(ND-1)+3)
               ! Recopy VR
               USER_VR(3*(I-1)+1) = VR(3*(ND-1)+1)
               USER_VR(3*(I-1)+2) = VR(3*(ND-1)+2)
               USER_VR(3*(I-1)+3) = VR(3*(ND-1)+3)
               ! Recopy MS,IN,STIFN,STIFR
               USER_MS(I) = MS(ND)
               USER_IN(I) = IN(ND)
               USER_STIFN(I) = STIFN(ND)
               USER_STIFR(I) = STIFR(ND)
               ! Recopy A, just in case
               USER_A(3*(I-1)+1) = A(3*(ND-1)+1)
               USER_A(3*(I-1)+2) = A(3*(ND-1)+2)
               USER_A(3*(I-1)+3) = A(3*(ND-1)+3)
               ! Recopy AR, just in case
               USER_AR(3*(I-1)+1) = AR(3*(ND-1)+1)
               USER_AR(3*(I-1)+2) = AR(3*(ND-1)+2)
               USER_AR(3*(I-1)+3) = AR(3*(ND-1)+3)
            ENDDO
      
            CALL ENG_USERLIB_USERWI(
     1                               RAD_INPUTNAME      ,LEN_RAD_INPUTNAME   ,
     2                               USER_WINDOWS%NUVAR ,NUVARI              ,USER_WINDOWS%N_USERNODS   ,
     3                               NCYCLE             ,USER_WINDOWS%S_WA   ,USER_WINDOWS%IUSER        ,USER_ITAB    ,TT  ,
     4                               DT1                ,DTU     ,USER_WINDOWS%USREINT      ,TFEXT      ,USER_WINDOWS%USER ,
     5                               USER_D             ,USER_X  ,USER_V                    ,USER_VR    ,USER_MS           ,
     6                               USER_IN            ,USER_STIFN,USER_STIFR              ,USER_A     ,USER_AR           ,
     7                               USER_WINDOWS%WA )
     
            DO I=1,USER_WINDOWS%N_USERNODS
               ND = USER_WINDOWS%USERNODS(I)
               ! Recopy back A
               A(3*(ND-1)+1) = USER_A(3*(I-1)+1)
               A(3*(ND-1)+2) = USER_A(3*(I-1)+2)
               A(3*(ND-1)+3) = USER_A(3*(I-1)+3)
               ! Recopy back AR
               AR(3*(ND-1)+1) = USER_AR(3*(I-1)+1)
               AR(3*(ND-1)+2) = USER_AR(3*(I-1)+2)
               AR(3*(ND-1)+3) = USER_AR(3*(I-1)+3)
            ENDDO

            DEALLOCATE(USER_ITAB)
            DEALLOCATE(USER_D)
            DEALLOCATE(USER_X)
            DEALLOCATE(USER_V)
            DEALLOCATE(USER_VR)
            DEALLOCATE(USER_MS)
            DEALLOCATE(USER_IN)
            DEALLOCATE(USER_STIFN)
            DEALLOCATE(USER_STIFR)
            DEALLOCATE(USER_A)
            DEALLOCATE(USER_AR)


            DT2 = MIN(DT2,DTU)
         ELSE
           ! ----------------
           ! ERROR to be printed & exit
           OPTION='USER WINDOWS'
           SIZE_OPT=LEN_TRIM(OPTION)
           CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE_OPT),ANMODE=ANINFO)
           CALL ARRET(2)
           ! ----------------
         ENDIF
      ENDIF

      IF(NSPMD> 1)THEN
          CALL SPMD_EXCH_USERWI(A,AR,USER_WINDOWS)
      ENDIF
C-----------------------------------------------
      END
C-----------------------------------------------

