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
Chd|  ST_QAPRINT_GROUPS             source/output/qaprint/st_qaprint_groups.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        QA_PRINT_GROUPS               source/output/qaprint/st_qaprint_groups.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_GROUPS(IGRNOD ,IGRPART ,IGRBRIC  ,IGRSH4N   ,IGRSH3N   ,
     .                       IGRQUAD,IGRBEAM ,IGRTRUSS ,IGRSPRING )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRNOD)  :: IGRNOD 
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRPART) :: IGRPART       
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRBRIC) :: IGRBRIC 
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSHEL) :: IGRSH4N 
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSH3N) :: IGRSH3N 
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRQUAD) :: IGRQUAD 
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRBEAM) :: IGRBEAM 
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRTRUS) :: IGRTRUSS                                     
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSPRI) :: IGRSPRING       
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      TYPE (GROUP_)  , POINTER :: PTR_IGRELEM    
      INTEGER KK 
      CHARACTER :: GROUP_NAME*7
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      OK_QA = MYQAKEY('GROUPS')  
                                                         
      IF (OK_QA) THEN  
       
        DO KK = 1, NGRBRIC
          PTR_IGRELEM => IGRBRIC(KK)
          GROUP_NAME(1:7) = 'IGRBRIC'
          CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
        ENDDO 

        DO KK = 1, NGRPART
          PTR_IGRELEM => IGRPART(KK)
          GROUP_NAME(1:7) = 'IGRPART'
          CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
        ENDDO 
        
        DO KK = 1, NGRQUAD
          PTR_IGRELEM => IGRQUAD(KK)
          GROUP_NAME(1:7) = 'IGRQUAD'
          CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
        ENDDO 

        IF( N2D == 0)THEN
          DO KK = 1, NGRSH3N
            PTR_IGRELEM => IGRSH3N(KK)
            GROUP_NAME(1:7) = 'IGRSH3N'
            CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
          ENDDO 
        ELSEIF( N2D /= 0)THEN
          DO KK = 1, NGRSH3N
            PTR_IGRELEM => IGRSH3N(KK)
            GROUP_NAME(1:7) = 'IGRTRIA'
            CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
          ENDDO 
        ENDIF

        DO KK = 1, NGRSHEL
          PTR_IGRELEM => IGRSH4N(KK)
          GROUP_NAME(1:7) = 'IGRSH4N'
          CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
        ENDDO 

        DO KK = 1, NGRSPRI
          PTR_IGRELEM => IGRSPRING(KK)
          GROUP_NAME(1:7) = 'IGRSPRI'
          CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
        ENDDO 

        DO KK = 1, NGRTRUS
          PTR_IGRELEM => IGRTRUSS(KK)
          GROUP_NAME(1:7) = 'IGRTRUS'
          CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
        ENDDO 

        DO KK = 1, NGRBEAM
          PTR_IGRELEM => IGRBEAM(KK)
          GROUP_NAME(1:7) = 'IGRBEAM'
          CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
        ENDDO 
        
        DO KK = 1, NGRNOD
          PTR_IGRELEM => IGRNOD(KK)
          GROUP_NAME(1:7) = 'IGRNOD '
          CALL QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
        ENDDO 
                                
      ENDIF  
                                                                                 
C-----------------------------------------------
      RETURN
      END


Chd|====================================================================
Chd|  QA_PRINT_GROUPS               source/output/qaprint/st_qaprint_groups.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_GROUPS             source/output/qaprint/st_qaprint_groups.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE QA_PRINT_GROUPS(PTR_IGRELEM, GROUP_NAME)
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
      TYPE (GROUP_),INTENT(IN)  :: PTR_IGRELEM  
      CHARACTER,INTENT(IN) :: GROUP_NAME*7 
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER (LEN=255) :: VARNAME
      INTEGER KK,ID,LEN_
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

            ID = PTR_IGRELEM%ID
            LEN_=LEN_TRIM(PTR_IGRELEM%TITLE)
            WRITE(VARNAME,'(A,I0,A,A)') GROUP_NAME//'(',ID,')%TITLE     =',PTR_IGRELEM%TITLE(1:LEN_)             
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ID,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%NENTITY   ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%NENTITY,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%SET_GROUP   ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%SET_GROUP,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%GRTYPE    ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%GRTYPE,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%SORTED    ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%SORTED,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%GRPGRP    ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%GRPGRP,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%LEVEL     ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%LEVEL,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%R2R_ALL   ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%R2R_ALL,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%R2R_SHARE ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%R2R_SHARE,0.0_8) 
            WRITE(VARNAME,'(A,I0,A)') GROUP_NAME//'(',ID,')%GRTYPE    ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%GRTYPE,0.0_8) 
            !output only first & last elem
            DO KK=1,MIN(1,PTR_IGRELEM%NENTITY)
              WRITE(VARNAME,'(A,A,I10,A,I10,A)') GROUP_NAME,'(',ID,')%ELEM(',KK,') =' 
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%ENTITY(KK),0.0_8) 
            ENDDO
            IF(PTR_IGRELEM%NENTITY > 1)THEN
              WRITE(VARNAME,'(A,A,I10,A,I10,A)') GROUP_NAME,'(',ID,')%ELEM(',PTR_IGRELEM%NENTITY,') =' 
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PTR_IGRELEM%ENTITY(PTR_IGRELEM%NENTITY),0.0_8)               
            ENDIF
                  
      END
