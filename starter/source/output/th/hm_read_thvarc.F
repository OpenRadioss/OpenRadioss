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
!||    hm_thvarc              ../starter/source/output/th/hm_read_thvarc.F
!||--- called by ------------------------------------------------------
!||    hm_read_thgrki         ../starter/source/output/th/hm_read_thgrki.F
!||    hm_read_thgrki_rbody   ../starter/source/output/th/hm_read_thgrki_rbody.F
!||    hm_read_thgrne         ../starter/source/output/th/hm_read_thgrne.F
!||    hm_read_thgrns         ../starter/source/output/th/hm_read_thgrns.F
!||    hm_read_thgrpa         ../starter/source/output/th/hm_read_thgrpa.F
!||    hm_read_thgrpa_sub     ../starter/source/output/th/hm_read_thgrpa.F
!||    hm_read_thgrsens       ../starter/source/output/th/hm_read_thgrsens.F
!||    hm_read_thgrsurf       ../starter/source/output/th/hm_read_thgrsurf.F
!||    hm_thgrki_vent         ../starter/source/output/th/hm_thgrki_vent.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_get_string_index    ../starter/source/devtools/hm_reader/hm_get_string_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||    th_mod                 ../starter/share/modules1/th_mod.F90
!||====================================================================
      INTEGER FUNCTION HM_THVARC(VARE,NV,IVAR,VARG,NVG,IVARG,NV0,ID,TITR,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE TH_MOD , ONLY : TH_HAS_NODA_PEXT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "r2r_c.inc"
C-----------------------------------------------
      INTEGER NV,NVG,IVAR(*),IVARG(18,*),NV0
      CHARACTER*10,TARGET :: VARE(NV),VARG(NVG)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL IS_AVAILABLE
      INTEGER,DIMENSION(:),ALLOCATABLE :: TAG
      CHARACTER VAR*10
      INTEGER JREC,NVAR,TRU,N,L,M,I,J,K,OK
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ! Tag must start from 0
      ALLOCATE(TAG(0:NV))
C
      ! Number of variables indicated by the user
      CALL HM_GET_INTV('Number_Of_Variables',NVAR,IS_AVAILABLE,LSUBMODEL) 
c
      ! Initialization of the tag table
      DO I=1,NV
        TAG(I)=0
      ENDDO
c
      ! Loop over variables
      DO K=1,NVAR
        CALL HM_GET_STRING_INDEX('VAR',VAR,K,10,IS_AVAILABLE)
        VAR(LEN_TRIM(VAR)+1:10)=' '
        IF (VAR(1:4) == 'QVIS')VAR='BULK'                       
        IF (VAR(1:6) == 'ALL_42') THEN                
          DO I=1,NV0                                 
            TAG(I)=1                                 
          ENDDO 
        ELSEIF (VAR(1:3) == 'ALL') THEN                       
          CALL ANCMSG(MSGID=551,                     
     .                MSGTYPE=MSGERROR,              
     .                ANMODE=ANINFO,                 
     .                I1=ID,                         
     .                C1=TITR,                       
     .                C2=VAR)                                                                 
        ELSEIF (VAR(1:5) == '     ') THEN        
          CYCLE
        ELSE   
          OK = 0                                      
          DO I=1,NVG                                                                  
            IF(VAR == VARG(I))THEN                   
                DO J=1,18                            
                  TAG(IVARG(J,I))=1                  
                ENDDO                                
                OK=1 
                EXIT                                
            ENDIF                                    
          ENDDO                                      
c                                                     
          IF(OK==0)THEN                                       
            DO I=1,NV                                                             
              IF(VAR == VARE(I))THEN
                IF(VAR(1:4) == 'PEXT ')THEN
                  TH_HAS_NODA_PEXT = 1
                ENDIF
                TAG(I)=1                             
                OK=1                                 
C---------Multidomaines : pour les subsets, on stocke des infos en plus dans le th pour merge---------                      
                    IF ((NSUBDOM>0).AND.(SUB_FLAG==1002)) THEN                                        
                    IF (I>=9) THEN                                                                     
                      TAG(3) = 1                                                                         
                      TAG(4) = 1                                                                         
                      TAG(5) = 1                                                                         
                      TAG(6) = 1                                                                         
                      TAG(9) = 1                                                                         
                      TAG(10) = 1                                                                        
                      TAG(11) = 1                                                                        
                    ENDIF                                                                                
                    IF (I==23) THEN                                                                    
                      DO J=12,20                                                                         
                        TAG(J)=1                                                                         
                      ENDDO                                                                              
                    ENDIF                                                                                
                  ENDIF                                                                                  
C---------  ----------------------------------------------------------------------------------------------  
                EXIT                  
              ENDIF                             
            ENDDO      
          ENDIF                         
                                              
          IF(OK==0)THEN                     
             CALL ANCMSG(MSGID=260,           
     .                   MSGTYPE=MSGERROR,    
     .                   ANMODE=ANINFO,       
     .                   I1=ID,               
     .                   C1=TITR,             
     .                   C2=VAR)              
          ENDIF                               
       ENDIF
      ENDDO 
      
      NVAR=0
      DO I=1,NV
        IF(TAG(I) /= 0)THEN
          NVAR=NVAR+1
          IVAR(NVAR)=I
        ENDIF
      ENDDO
         
      
      HM_THVARC=NVAR
       
      DEALLOCATE(TAG)
      RETURN
      END
