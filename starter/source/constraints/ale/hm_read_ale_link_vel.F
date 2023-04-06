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
Chd|  HM_READ_ALE_LINK              source/constraints/ale/hm_read_ale_link_vel.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_ALE_LINK(ICODE, ISKEW, ITAB, ITABM1, IKINE,
     .     IGRNOD, IBCSLAG, LAG_NCF, LAG_NKF, LAG_NHF,
     .     IKINE1LAG, LINALE, LSUBMODEL, UNITAB)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is reading /ALE/LINK/VEL options in user input file
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: ICODE(NUMNOD), ISKEW(*), ITAB(NUMNOD), ITABM1(*), IKINE(*),IBCSLAG(5,*)
      INTEGER,INTENT(IN) :: LAG_NCF,LAG_NKF,LAG_NHF,IKINE1LAG(*)
      INTEGER,INTENT(INOUT) :: LINALE(*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER GRID_DOF, BID, IFORM,  NODE_ID1, NODE_ID2,POS,IAD0,NNOD
      INTEGER I,JJ(12), IC, NC, N, NUSR, IS, IC1, IC2, IC3, IC4, 
     .        NOSYS, J,J10(10),IGR,IGRS,ISU,IBCALE,J3(3),K,
     .        IC0, IC01, IC02, IC03, IC04, ID ,ILAGM, NBCSLAG,
     .        FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,IKINE1(3*NUMNOD),IGRNS
      CHARACTER KEY*ncharkey,STRING*ncharfield,KEY2*ncharkey
      CHARACTER TITR*nchartitle,OPT*8, CHAR_XYZ*ncharfield,MESS*40
      my_real TSTART,TSTOP
      LOGICAL :: IS_AVAILABLE
      INTEGER :: WX, WY, WZ
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,MY_OR,CHECK_NEW,NGR2USR
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      POS=0
      IGRNS=0
      TSTART=ZERO
      TSTOP=ZERO
      
      WRITE(IOUT,1000)
      CALL HM_OPTION_START('/ALE/LINK/VEL')

      DO I = 1, NALELK
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = N, OPTION_TITR = TITR)
         CALL HM_GET_INTV('node_ID1', NODE_ID1, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('node_ID2', NODE_ID2, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('grnod_ID', IGR, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Wx', WX, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Wy', WY, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Wz', WZ, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Iform', IFORM, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('Tstart', TSTART, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('Tstop', TSTOP, IS_AVAILABLE, LSUBMODEL, UNITAB)
         IF (TSTOP == ZERO) TSTOP = EP30
         INGR2USR => IGRNOD(1:NGRNOD)%ID
         IGRS=NGR2USR(IGR,INGR2USR,NGRNOD)
         IF(IGRS==0)THEN
            WRITE(IOUT,*)  '  -- NODE GROUP',IGR,' NOT FOUND'
            WRITE(ISTDO,*) '  -- NODE GROUP',IGR,' NOT FOUND'              
         ENDIF               
         
         J3(1) = WX
         J3(2) = WY
         J3(3) = WZ
         IC  = J3(1) * 4 + J3(2) * 2 + J3(3)            
         CHAR_XYZ = '          '           
         K=LFIELD
         IF(J3(3)==1 )THEN
            CHAR_XYZ(K:K)='Z'
            K=K-1
         ENDIF
         IF(J3(2)==1 )THEN
            CHAR_XYZ(K:K)='Y'
            K=K-1
         ENDIF
         IF(J3(1)==1 )THEN
            CHAR_XYZ(K:K)='X'
         ENDIF
!     
         IGRNS = IGRNOD(IGRS)%SORTED
!     
         IF(IFORM==0.AND.IGRNS /= 1)THEN
            CALL ANCMSG(MSGID=271,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=N,I2=IFORM) 
         ELSE
            WRITE(IOUT,1100) N,TRIM(TITR),NODE_ID1,NODE_ID2,IGR,CHAR_XYZ,IFORM,TSTART,TSTOP
         ENDIF

         LINALE(POS+1)=N
         LINALE(POS+2)=NODE_ID1   
         LINALE(POS+3)=NODE_ID2
         LINALE(POS+4)=-1                    
         LINALE(POS+5)=IC
         LINALE(POS+6)=IFORM 
         LINALE(POS+7)=IGRS

         POS = POS+1+6 
         MESS=''
         MESS(1:10)='ALE  LINKS'
         BID=USR2SYS(NODE_ID1,ITABM1,MESS,N)
         BID=USR2SYS(NODE_ID2,ITABM1,MESS,N)
        ENDDO
C-----------------------------------------------
1000  FORMAT(/
     . '       ALE LINKS DEFINITIONS '/
     . '      ---------------------- '/)
C-----------------------------------------------     
1100  FORMAT( /5X,'ALE LINK ID ',I10,': ',A,
     .       /10X,'MAIN NODE 1 . . . . . . . . . . . . . ',I10
     .       /10X,'MAIN NODE 2 . . . . . . . . . . . . . ',I10
     .       /10X,'GROUP IDENTIFIER FOR SECONDARY NODES. . . . ',I10    
     .       /10X,'GRID VELOCITY DIRECTIONS TO LINK. . . . ',A
     .       /10X,'FORMULATION . . . . . . . . . . . . . . ',I10
     .       /10X,'START TIME. . . . . . . . . . . . . . . ',1PG20.13
     .       /10X,'STOP TIME . . . . . . . . . . . . . . . ',1PG20.13/)
C-----------------------------------------------
      RETURN
      END
