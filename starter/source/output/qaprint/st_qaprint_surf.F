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
Chd|  ST_QAPRINT_SURF               source/output/qaprint/st_qaprint_surf.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        QA_PRINT_SURF                 source/output/qaprint/st_qaprint_surf.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_SURF(IGRSURF,IGRSLIN, BUFSF, SBUFSF)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SURF_)   , INTENT(IN), TARGET, DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , INTENT(IN), TARGET, DIMENSION(NSLIN)   :: IGRSLIN
      INTEGER,INTENT(IN) :: SBUFSF    
      my_real :: BUFSF(SBUFSF)  
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      TYPE (SURF_)  , POINTER :: PTR_IGRSURF    
      INTEGER KK ,NN, IAD_PREV
      CHARACTER :: GROUP_NAME*7
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      OK_QA = MYQAKEY('/SURF')  
      IAD_PREV=1
      IF (OK_QA) THEN  
        DO KK = 1, NSURF
          PTR_IGRSURF => IGRSURF(KK)
          IF(KK>1)IAD_PREV=IGRSURF(KK-1)%IAD_BUFR
          GROUP_NAME(1:7) = 'IGRSURF'
          CALL QA_PRINT_SURF(PTR_IGRSURF, GROUP_NAME, BUFSF,SBUFSF, IAD_PREV, 4)
        ENDDO 
      ENDIF  

      OK_QA = MYQAKEY('/LINE') 
      IAD_PREV=1 
      IF (OK_QA) THEN  
        DO KK = 1, NSLIN
          PTR_IGRSURF => IGRSLIN(KK)
          IF(KK>1)IAD_PREV=IGRSURF(KK-1)%IAD_BUFR
          GROUP_NAME(1:7) = 'IGRSLIN'
          CALL QA_PRINT_SURF(PTR_IGRSURF, GROUP_NAME, BUFSF,SBUFSF, IAD_PREV, 2)
        ENDDO 
      ENDIF       
                                                                                 
C-----------------------------------------------
      RETURN
      END


Chd|====================================================================
Chd|  QA_PRINT_SURF                 source/output/qaprint/st_qaprint_surf.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_SURF               source/output/qaprint/st_qaprint_surf.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE QA_PRINT_SURF(PTR_IGRSURF, GROUP_NAME, BUFSF, SBUFSF, IAD_PREV, NNOD)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SURF_),INTENT(IN)  :: PTR_IGRSURF  
      CHARACTER,INTENT(IN) :: GROUP_NAME*7   
      INTEGER, INTENT(IN) :: SBUFSF, IAD_PREV,NNOD
      my_real :: BUFSF(SBUFSF)          
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER (LEN=255) :: VARNAME
      INTEGER KK,ID,LEN_,NEL,NEL_IGE,NN,TMP, IAD_CUR
      DOUBLE PRECISION :: RTMP
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

            ID = PTR_IGRSURF%ID
            LEN_=LEN_TRIM(PTR_IGRSURF%TITLE)
            WRITE(VARNAME,'(A,I0,A,A)') GROUP_NAME//'(',ID,')%TITLE=',PTR_IGRSURF%TITLE(1:LEN_)             
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ID,0.0_8) 
            
            TMP=PTR_IGRSURF%NSEG
            IF(TMP/=0)THEN
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%NSEG='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8)
            ENDIF 
            
            TMP=PTR_IGRSURF%NSEG_IGE
            IF(TMP/=0)THEN
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%NSEG_IGE='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%IAD_IGE
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%IAD_IGE='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%SET_GROUP
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%SET_GROUP='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%TYPE
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%TYPE='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%SET_GROUP
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%SET_GROUP='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
                        
            TMP=PTR_IGRSURF%ID_MADYMO
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%ID_MADYMO='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%IAD_BUFR
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%IAD_BUFR='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%NB_MADYMO
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%NB_MADYMO='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%TYPE_MADYMO
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%TYPE_MADYMO='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%LEVEL
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%LEVEL='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%TH_SURF
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%TH_SURF='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%ISH4N3N
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%ISH4N3N='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%NSEG_R2R_ALL
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%NSEG_R2R_ALL='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            
            TMP=PTR_IGRSURF%NSEG_R2R_SHARE
            IF(TMP/=0)THEN              
              WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%NSEG_R2R_SHARE='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
            ENDIF
            

            NEL=PTR_IGRSURF%NSEG
            NEL_IGE=PTR_IGRSURF%NSEG_IGE
            
            IF (ALLOCATED(PTR_IGRSURF%REVERSED)) THEN
               DO KK=1,MIN(3,NEL)
                 TMP=PTR_IGRSURF%REVERSED(KK)
                 IF(TMP/=0)THEN        
                  WRITE(VARNAME,'(A,I0,A,I0,A)') GROUP_NAME//'(',ID,')%REVERSED(',KK,')='              
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
                 ENDIF
               ENDDO
            ENDIF

            IF (ALLOCATED(PTR_IGRSURF%ELTYP)) THEN
               DO KK=1,MIN(3,NEL)
                 TMP=PTR_IGRSURF%ELTYP(KK)
                 IF(TMP/=0)THEN
                  WRITE(VARNAME,'(A,I0,A,I0,A)') GROUP_NAME//'(',ID,')%ELTYP(',KK,')='              
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8)
                 ENDIF 
               ENDDO
            ENDIF

            IF (ALLOCATED(PTR_IGRSURF%ELEM) .AND. ALLOCATED(PTR_IGRSURF%NODES)) THEN
               DO KK=1,MIN(3,NEL)
                  TMP=PTR_IGRSURF%ELEM(KK)
                  IF(TMP/=0)THEN
                    WRITE(VARNAME,'(A,I0,A,I0,A)') GROUP_NAME//'(',ID,')%ELEM(',KK,')='             
                    CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
                  ENDIF
                  WRITE(VARNAME,'(A,I0,A,I0,A,I0,A,I0,A,I0)') GROUP_NAME//'(',ID,')%ELEM(1:4)=',PTR_IGRSURF%NODES(KK,1),
     .                 ',',PTR_IGRSURF%NODES(KK,2),',',PTR_IGRSURF%NODES(KK,3),',',PTR_IGRSURF%NODES(KK,4)              
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,1.D0) 
               ENDDO               
            ENDIF

            IF (ALLOCATED(PTR_IGRSURF%PROC)) THEN
               DO KK=1,MIN(3,NEL)
                  WRITE(VARNAME,'(A,I0,A,I0,A)') GROUP_NAME//'(',ID,')%PROC(',KK,')='              
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRSURF%PROC(KK),0.0_8) 
               ENDDO
            ENDIF
            
            IF (ALLOCATED(PTR_IGRSURF%ELTYP_IGE)) THEN
               DO KK=1,MIN(3,NEL_IGE)
                  WRITE(VARNAME,'(A,I0,A,I0,A)') GROUP_NAME//'(',ID,')%ELTYP_IGE(',KK,')='              
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRSURF%ELTYP_IGE(KK),0.0_8) 
               ENDDO
            ENDIF

            IF (ALLOCATED(PTR_IGRSURF%ELEM_IGE)) THEN
               DO KK=1,MIN(3,NEL_IGE)
                  WRITE(VARNAME,'(A,I0,A,I0,A)') GROUP_NAME//'(',ID,')%ELEM_IGE(',KK,')='             
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRSURF%ELEM_IGE(KK),0.0_8) 
               ENDDO
            ENDIF

            IF (ALLOCATED(PTR_IGRSURF%ELEM_IGE) .AND. ALLOCATED(PTR_IGRSURF%NODES_IGE)) THEN
               DO KK=1,MIN(3,NEL_IGE)
                 TMP=PTR_IGRSURF%ELEM_IGE(KK)
                 IF(TMP/=0)THEN
                  WRITE(VARNAME,'(A,I0,A,I0,A)') GROUP_NAME//'(',ID,')%ELEM_IGE(',KK,')='              
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TMP,0.0_8) 
                  IF(NNOD==2)THEN
                    WRITE(VARNAME,'(A,I0,A,I0,A,I0)') GROUP_NAME//'(',ID,')%ELEM(1:4)=',PTR_IGRSURF%NODES_IGE(KK,1),
     .                 ',',PTR_IGRSURF%NODES_IGE(KK,2)
                  ELSEIF(NNOD==4)THEN
                    WRITE(VARNAME,'(A,I0,A,I0,A,I0,A,I0,A,I0)') GROUP_NAME//'(',ID,')%ELEM(1:4)=',PTR_IGRSURF%NODES_IGE(KK,1),
     .                 ',',PTR_IGRSURF%NODES_IGE(KK,2),',',PTR_IGRSURF%NODES_IGE(KK,3),',',PTR_IGRSURF%NODES_IGE(KK,4) 
                  ENDIF             
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),1,0.0_8)
                 ENDIF 
               ENDDO
            ENDIF
            
            IAD_CUR=PTR_IGRSURF%IAD_BUFR
            DO KK=MAX(1,IAD_PREV),IAD_CUR
              RTMP = BUFSF(KK)
              IF(RTMP /= ZERO)THEN
                WRITE(VARNAME,'(A,I0,A,I0,A)')  GROUP_NAME//'(',ID,')--->BUFSF(',KK,')='
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,RTMP)
              ENDIF
            ENDDO

                  
      END

