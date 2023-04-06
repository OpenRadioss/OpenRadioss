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
Chd|  ST_QAPRINT_TIME_HISTORIES     source/output/qaprint/st_qaprint_time_histories.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        ST_QAPRINT_THGROU             source/output/qaprint/st_qaprint_time_histories.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_TIME_HISTORIES(TH , ITHVAR , IPART  , SUBSETS, 
     .                                     IPARTTH, NTHGRPMX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE GROUPDEF_MOD
        USE OUTPUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(TH_)  :: TH
        INTEGER, INTENT(IN) :: ITHVAR(SITHVAR)  , NTHGRPMX
        INTEGER, INTENT(IN), TARGET :: IPART(LIPART1,NPART+NTHPART),IPARTTH(18*(NPART+NTHPART))
        TYPE(SUBSET_), DIMENSION(NSUBS), INTENT(IN) :: SUBSETS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER, DIMENSION(:,:), POINTER :: IPARTTHI
        LOGICAL :: OK_QA
C-----------------------------------------------
C       /TH
C-----------------------------------------------
        OK_QA = MYQAKEY('/TH')
        IF (OK_QA) THEN
C
          !/TH
          IPARTTHI => IPART(8:9,1:NPART+NTHPART)
          CALL ST_QAPRINT_THGROU(NTHGRP     ,TH%ITHGRP    ,TH%ITHBUF    ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,10   ,TH%SITHGRP    ,TH%SITHBUF)
     
          !/ATH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(1:2*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(1) ,TH%ITHGRPA   ,TH%ITHBUFA   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,1    ,TH%SITHGRPA,TH%SITHBUFA )
     
          !/BTH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(2*(NPART+NTHPART)+1:4*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(2) ,TH%ITHGRPB   ,TH%ITHBUFB   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,2    ,TH%SITHGRPB,TH%SITHBUFB )
      
          !/CTH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(4*(NPART+NTHPART)+1:6*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(3) ,TH%ITHGRPC   ,TH%ITHBUFC   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,3    ,TH%SITHGRPC,TH%SITHBUFC )
      
          !/DTH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(6*(NPART+NTHPART)+1:8*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(4) ,TH%ITHGRPD  , TH%ITHBUFD   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,4    ,TH%SITHGRPD,TH%SITHBUFD )

          !/ETH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(8*(NPART+NTHPART)+1:10*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(5) ,TH%ITHGRPE   ,TH%ITHBUFE   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,5    ,TH%SITHGRPE,TH%SITHBUFE )
 
          !/FTH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(10*(NPART+NTHPART)+1:12*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(6) ,TH%ITHGRPF   ,TH%ITHBUFF   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,6    ,TH%SITHGRPF,TH%SITHBUFF )
     
          !/GTH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(12*(NPART+NTHPART)+1:14*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(7) ,TH%ITHGRPG   ,TH%ITHBUFG   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,7    ,TH%SITHGRPG,TH%SITHBUFG )
     
          !/HTH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(14*(NPART+NTHPART)+1:16*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(8) ,TH%ITHGRPH   ,TH%ITHBUFH   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,8    ,TH%SITHGRPH,TH%SITHBUFH  )
     
          !/ITH
          IPARTTHI(1:2,1:NPART+NTHPART) => IPARTTH(16*(NPART+NTHPART)+1:18*(NPART+NTHPART))
          CALL ST_QAPRINT_THGROU(NTHGRP1(9) ,TH%ITHGRPI   ,TH%ITHBUFI   ,ITHVAR   ,IPART    ,
     .                           IPARTTHI   ,NTHGRPMX  ,SUBSETS   ,9    ,TH%SITHGRPI,TH%SITHBUFI )
C        
        ENDIF
C-----------------------------------------------
        RETURN
      END
!    QA_PRINT for the THGROU


Chd|====================================================================
Chd|  ST_QAPRINT_THGROU             source/output/qaprint/st_qaprint_time_histories.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_TIME_HISTORIES     source/output/qaprint/st_qaprint_time_histories.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_THGROU(NTHGROUP ,ITHGRP   ,ITHBUF   ,ITHVAR   ,IPART    ,
     .                             IPARTTH  ,NTHGRPMX ,SUBSETS  ,ISUBVAR  ,SITHGRP,SITHBUF) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: SITHGRP,SITHBUF
        INTEGER, INTENT(IN) :: ITHGRP(NITHGR,*),ITHBUF(SITHBUF),ITHVAR(SITHVAR),
     .                         IPART(LIPART1,NPART+NTHPART),NTHGRPMX,ISUBVAR ,
     .                         IPARTTH(2,NPART+NTHPART),NTHGROUP
        TYPE(SUBSET_), DIMENSION(NSUBS), INTENT(IN) :: SUBSETS
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, ID, II, TEMP_INT, MY_TH, NVAR, IAD, K, NNE
        INTEGER, DIMENSION(NTHGROUP) :: IDX, IDS
        CHARACTER *nchartitle TITR
        CHARACTER (LEN=255) :: VARNAME
C
C
          IF(NTHGROUP > 0)THEN
            ! Sort with user IDs
            DO I = 1, NTHGROUP
              IDS(I)    = ITHGRP(1,I)
              IDX(I)    = I
            ENDDO    
            CALL QUICKSORT_I2(IDS, IDX, 1, NTHGROUP)    
          ENDIF
          
          !-------------------------------------!
          ! All /TH expect                      !
          ! /TH/SUBS and /TH/PART and /THPART   !
          !-------------------------------------!          
          DO II = 1, NTHGROUP
C
            ! Sorted ID users
            MY_TH = IDX(II)
            ID    = ITHGRP(1,MY_TH)
C
            ! Printing only if ID is stored          
            IF (ID /= 0) THEN
C          
              ! Time history title
              TITR(1:nchartitle)=''
              CALL FRETITL2(TITR, ITHGRP(NITHGR-LTITR+1,MY_TH), LTITR)
              IF (LEN_TRIM(TITR) /= 0) THEN
                CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ID,0.0_8)
              ELSE
                CALL QAPRINT('A_TH_FAKE_NAME', ID,0.0_8)
              ENDIF
C     
              ! Time history group table
              DO I = 1, NITHGR-LTITR
                WRITE(VARNAME,'(A,I0,A,I0)') 'ITHGRP_',I,'_',MY_TH
                TEMP_INT = ITHGRP(I,MY_TH)
                IF ((TEMP_INT /= 0).OR.(I == 2)) THEN 
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
                ENDIF
              ENDDO
C
              ! Time history buffer
              DO I = ITHGRP(5,MY_TH), ITHGRP(8,MY_TH)-1
                WRITE(VARNAME,'(A,I0,A,I0)') 'ITHBUF_',I
                TEMP_INT = ITHBUF(I)
                IF (TEMP_INT /= 0) THEN 
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
                ENDIF
              ENDDO
C
              NNE=ITHGRP(4,MY_TH)
              K=0
              DO I=1,NNE
                ! Title of the object of the time history
                TITR(1:nchartitle)=''
                CALL FRETITL2(TITR, ITHBUF(ITHGRP(8,MY_TH)+K), 39)
                IF (LEN_TRIM(TITR) /= 0) THEN
                  CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),0,0.0_8)
                ELSE
                  CALL QAPRINT('A_TH_OBJECT_FAKE_NAME',0,0.0_8)
                END IF
                K=K+40  
              ENDDO
C
              ! Time history saved variable 
              DO I = 0, ITHGRP(6,MY_TH)-1
                DO K = 1,10
                  WRITE(VARNAME,'(A,I0,A,I0)') 'ITHVAR_',(ITHGRP(9,MY_TH)+I-1)*10+K
                  TEMP_INT = ITHVAR((ITHGRP(9,MY_TH)+I-1)*10+K)         
                  IF (TEMP_INT /= ICHAR(' ')) THEN 
                    CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
                  ENDIF
                ENDDO
              ENDDO
            ENDIF 
          ENDDO  !next /TH    (expect /TH/SUBS, /TH/PART, /THPART)
C
          !--------------------!
          ! Loop over /PART    !
          !     and /THPART    !
          !--------------------!          
          DO II = 1, NPART+NTHPART
C
            ! ID of the part
            ID   = IPART(4,II)
            ! Number of variable
            NVAR = IPARTTH(1,II)
            ! IAD in the buffer table
            IAD  = IPARTTH(2,II)
C
            ! Printing only is the number of variables is higher than 0
            IF (NVAR > 0) THEN
C
              ! ID of the part
              WRITE(VARNAME,'(A,I0,A,I0)') 'PART_ID_',ID
              TEMP_INT = ID
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C          
              ! Title of the part
              TITR(1:nchartitle)=''
              CALL FRETITL2(TITR,IPART(LIPART1-LTITR+1,II),40)
              IF (LEN_TRIM(TITR) /= 0) THEN
                CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ID,0.0_8)
              ELSE
                CALL QAPRINT('A_PART_FAKE_NAME', ID,0.0_8)
              END IF
C
              ! Number of variables
              WRITE(VARNAME,'(A,I0,A,I0)') 'IPARTTH_',1,'_',II
              TEMP_INT = NVAR
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
              ! IAD in the buffer table
              WRITE(VARNAME,'(A,I0,A,I0)') 'IPARTTH_',2,'_',II
              TEMP_INT = IAD
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8) 
C      
              ! Printing the corresponding buffer    
              DO I = IAD,NVAR+IAD-1
                WRITE(VARNAME,'(A,I0,A,I0)') 'ITHBUF_',I
                TEMP_INT = ITHBUF(I)
                IF (TEMP_INT /= 0) THEN 
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
                ENDIF
              ENDDO
C
            ENDIF
          ENDDO !next /TH/PART or /THPART

          !--------------------!        
          ! /TH/SUBS           !
          !--------------------!                                                   
          DO II = 1, NSUBS                                                     
C        
            ! ID of the subset                                                 
            ID   = SUBSETS(II)%ID                                              
            ! Number of variables                                              
            NVAR = SUBSETS(II)%NVARTH(ISUBVAR)                                 
            ! IAD in the buffer table                                          
            IAD  = SUBSETS(II)%THIAD                                           
C                                                                              
            ! Printing only is the number of variables is higher than 0        
            IF (NVAR>0) THEN                                                   
C
              ! ID of the part                                                 
              WRITE(VARNAME,'(A,I0,A,I0)') 'SUBSET_ID_',ID                     
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)        
C                                                                              
              ! Title of the subset                                            
              TITR = SUBSETS(II)%TITLE                                         
              IF (LEN_TRIM(TITR) /= 0) THEN                                    
                CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ID,0.0_8)                  
              ELSE                                                             
                CALL QAPRINT('A_SUBSET_FAKE_NAME', ID,0.0_8)                   
              ENDIF                                                            
C                                                                              
              ! Number of variable                                             
              WRITE(VARNAME,'(A,I0,A,I0)') 'SUBSET_NVARTH_',II                 
              TEMP_INT = NVAR                                                  
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)        
C                                                                              
              ! IAD in the buffer table                                        
              WRITE(VARNAME,'(A,I0,A,I0)') 'SUBSET_THIAD_',II                  
              TEMP_INT = IAD                                                   
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)        
C                                                                              
              ! Printing the corresponding buffer table                        
              DO I = IAD,NVAR+IAD-1                                            
                WRITE(VARNAME,'(A,I0,A,I0)') 'ITHBUF_',I                       
                TEMP_INT = ITHBUF(I)                                           
                IF (TEMP_INT /= 0) THEN                                        
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)    
                ENDIF                                                          
              ENDDO                                                            
C                                                                              
            ENDIF                                                              
          ENDDO !next /TH/SUBS                                                                

C
      END SUBROUTINE
