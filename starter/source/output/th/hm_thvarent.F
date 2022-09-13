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
Chd|  HM_THVARVENT                  source/output/th/hm_thvarent.F
Chd|-- called by -----------
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_THVARVENT(VARE,NV,IVAR,VARG,NVG,IVARG,NV0,ID,TITR,
     .                     VARVENT,NBVENT_MAX, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "r2r_c.inc"
#include      "scr03_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t 
C-----------------------------------------------
      INTEGER NV,NVG,IVAR(*),IVARG(18,*),NV0,VARVENT(*),NBVENT_MAX,KK,IDSMAX
      CHARACTER*10 VARE(NV),VARG(NVG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s 
C-----------------------------------------------
      CHARACTER VAR*10
        INTEGER JREC,TRU,N,L,M,I,J,K,OK,TAG(0:250000)
      INTEGER ID,NMONV,IMONV,NLABEL
      CHARACTER*nchartitle,
     .   TITR
      INTEGER NVAR
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NVAR=0
      TRU=1
      
      DO I=1,NV
        TAG(I)=0
      ENDDO

      CALL HM_GET_INTV('Number_Of_Variables',NLABEL,IS_AVAILABLE,LSUBMODEL) 

      DO KK=1,NLABEL
        CALL HM_GET_STRING_INDEX('VAR',VAR,KK,10,IS_AVAILABLE)
            VAR(LEN_TRIM(VAR)+1:10)=' '
            IF (VAR(1:3) == 'DEF') THEN                             
              TAG(101:MIN(150,100+NBVENT_MAX*5))=1 
              VARVENT(1:NBVENT_MAX*5) = 1 
            ELSEIF(VAR(1:5) == '     ')THEN
              CYCLE
            ELSE   
              OK=0
              I=0
              DO WHILE(OK==0.AND.I<NVG)
C GENERIC VARIABLES SAVED
                I=I+1
                IF(VAR==VARG(I))THEN
                    DO J=1,18
                     IF(IVARG(J,I) /= 0) THEN
                      TAG(IVARG(J,I))=1
                      VARVENT(IVARG(J,I)-100) = 1
                     ENDIF
                    ENDDO
                    OK=1
                ENDIF
              ENDDO
              I=0
              DO WHILE(OK==0.AND.I<NV)
C SELECTED VARIABLES SAVED
                I=I+1
                IF(VAR==VARE(I))THEN
                    IF(I > 100) THEN 
                      VARVENT(I-100)=1
                      TAG(I)=1
                    ELSE
                      TAG(I)=0
                    ENDIF
                    OK=1
                ENDIF
              ENDDO
            ENDIF
      ENDDO 

      CALL HM_GET_INTV('idsmax',NMONV,IS_AVAILABLE,LSUBMODEL) 
      DO IMONV=1,NMONV
        CALL HM_GET_INT_ARRAY_INDEX('ids',N,IMONV,IS_AVAILABLE,LSUBMODEL) 
        IF(N/=0)THEN           
          DO I=1,NV              
            IF(TAG(I)/=0)THEN  
              NVAR=NVAR+1        
              IVAR(NVAR)=I       
            ENDIF                
          ENDDO                  
          RETURN                 
        ENDIF 
      ENDDO



      RETURN
      END
