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
Chd|  FRONTPLUS_RM                  source/spmd/node/frontplus.F  
Chd|-- called by -----------
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRONTPLUS_RM(FRONT,INDEX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FRONT,INDEX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER F_INT,F_KIN,F_ACC
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
      F_INT = FRONT / 100
      F_KIN = ( FRONT - F_INT*100 ) / 10
      F_ACC = FRONT - F_INT*100 - F_KIN*10
 
      IF(INDEX==1) THEN
        IF(F_ACC==0) FRONT = FRONT + 1
      ELSE IF(INDEX==10) THEN
        IF(F_KIN==0) FRONT = FRONT + 10
      ELSE IF(INDEX==100) THEN
        IF(F_INT==0) FRONT = FRONT + 100
      ENDIF
  
      RETURN
      END
Chd|====================================================================
Chd|  IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|-- called by -----------
Chd|        CHECK_SKEW                    source/spmd/domain_decomposition/check_skew.F
Chd|        C_DOMS10                      source/spmd/domdec2.F         
Chd|        DD_FR                         source/spmd/domain_decomposition/domdec1.F
Chd|        DD_FRX                        source/spmd/domain_decomposition/domdec1.F
Chd|        DD_FR_2                       source/spmd/domain_decomposition/domdec1.F
Chd|        DOMAIN_DECOMPOSITION_PCYL     source/loads/general/load_pcyl/domain_decomposition_pcyl.F
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RIVET                 source/elements/reader/hm_read_rivet.F
Chd|        I24SETNODES                   source/interfaces/inter3d1/i24setnodes.F
Chd|        IGRSURF_SPLIT                 source/spmd/igrsurf_split.F   
Chd|        INIEND                        source/interfaces/inter3d1/iniend.F
Chd|        INIEND2D                      source/interfaces/inter3d1/iniend.F
Chd|        INIRBE3                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        INI_SEATBELT                  source/tools/seatbelts/ini_seatbelt.F
Chd|        LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        PORNOD                        source/ale/pornod.F           
Chd|        R2R_DOMDEC                    source/coupling/rad2rad/r2r_domdec.F
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|        READ_SENSOR_DISP              source/tools/sensor/read_sensor_disp.F
Chd|        SET_IBUFSSG_IO                source/starter/lectur.F       
Chd|        SPLIT_JOINT                   source/constraints/general/cyl_joint/split_joint.F
Chd|        SPMDSET                       source/constraints/general/rbody/spmdset.F
Chd|        UELT_SPMD_ADDITIONAL_NODE     source/user_interface/uaccess.F
Chd|        USERWIS_FRONT                 source/user_interface/user_windows_tools.F
Chd|-- calls ---------------
Chd|        REALLOC_IFRONT                source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|====================================================================
      SUBROUTINE IFRONTPLUS(N,P)
C sort insert in chained-list IFRONT node N on SPMD domain P
C IFRONT%IENTRY : entry in IFRONT for node N
C IFRONT%P(1,N) : SPMD domain for node N
C IFRONT%P(2,N) : next index in IFRONT for node N
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,P,P_FOUND
      LOGICAL SEARCH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IAD,CURR,NEXT
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------       
     
c no SPMD domain already attributed for this node
      IF(IFRONT%IENTRY(N)==-1)THEN    
        ! create new entry in IFRONT
	! plug IENTRY(N) to this new entry
        IFRONT%IENTRY(N) = N
        IFRONT%P(1,N) =  P
	IFRONT%P(2,N) =  0	
        RETURN
      ENDIF
      
c one or more SPMD domain(s) already attributed for this node	

      ! set current index to IENTRY(N)
      CURR = IFRONT%IENTRY(N)

      ! if domain to insert is equal than current domain -> quit
      IF(IFRONT%P(1,CURR)==P) RETURN
	
      ! test if domain to insert is lower than current domain 
      ! test assure that P is different from IFRONT%P(1,CURR)
      IF(IFRONT%P(1,CURR)>P)THEN
	! insert P at beginning of the list
        IF(IFRONT_END+1>SIFRONT)THEN
	  ! realloc if needed
	  CALL REALLOC_IFRONT() 
	ENDIF
	! create a new entry
	! plug IENTRY(N) to this new entry
        IFRONT_END = IFRONT_END+1	
	IFRONT%IENTRY(N) = IFRONT_END
	! set new entry to P
        IFRONT%P(1,IFRONT_END) =  P
	! following is CURR index
	IFRONT%P(2,IFRONT_END) =  CURR  
	RETURN
      ENDIF
      
      NEXT = IFRONT%P(2,CURR)     
      ! insertion must be done between CURR and NEXT
      ! when CURR < P < NEXT
      SEARCH = .TRUE.
      ! move forward in the list while IFRONT%P(1,CURR) < P
      ! and while P not found 
      DO WHILE((SEARCH .EQV. .TRUE.).AND.(IFRONT%P(1,CURR)/=P))
        IF(NEXT==0)THEN
	  ! insert at end of list
	  IF(IFRONT_END+1>SIFRONT)THEN
	    CALL REALLOC_IFRONT() 
	  ENDIF      
	  IFRONT_END = IFRONT_END+1		     
	  IFRONT%P(2,CURR) =  IFRONT_END
          IFRONT%P(1,IFRONT_END) =  P
	  IFRONT%P(2,IFRONT_END) =  0
	  SEARCH = .FALSE.
	  RETURN
	ELSEIF(IFRONT%P(1,NEXT)>P)THEN
	  !insert between current and next
	  IF(IFRONT_END+1>SIFRONT)THEN
	    CALL REALLOC_IFRONT() 
	  ENDIF     
	  IFRONT_END = IFRONT_END+1		     
	  IFRONT%P(2,CURR) =  IFRONT_END
          IFRONT%P(1,IFRONT_END) =  P
	  IFRONT%P(2,IFRONT_END) =  NEXT
	  SEARCH = .FALSE.
	  RETURN	   
	ELSE
	  !move forward in list
	   CURR = NEXT
	   NEXT = IFRONT%P(2,NEXT)   
	ENDIF      
      ENDDO	      	
  
      RETURN
      END
Chd|====================================================================
Chd|  IDDCONNECTPLUS                source/spmd/node/frontplus.F  
Chd|-- called by -----------
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|-- calls ---------------
Chd|        REALLOC_IDDCONNECT            source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|====================================================================
      SUBROUTINE IDDCONNECTPLUS(N,P,NUMEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,P,NUMEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IAD,CURR,NEXT
      LOGICAL SEARCH
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------       
     
      !    New entry for this node
      IF(IDDCONNECT%IENTRYDOM(1,N)==-1)THEN    
        ! create new entry in IFRONT
	! plug IENTRY(N) to this new entry
        IDDCONNECT%IENTRYDOM(1,N) = N
        IDDCONNECT%IENTRYDOM(2,N) = IDDCONNECT%IENTRYDOM(2,N) + 1
        IDDCONNECT%PDOM(1,N) =  P
	IDDCONNECT%PDOM(2,N) =  0	
        RETURN
      ENDIF
      !    One or more entries for this node 

      !    Set current index to IENTRY(N)
      CURR = IDDCONNECT%IENTRYDOM(1,N)

      !    If node N to insert is equal than current node --> quit
      IF(IDDCONNECT%PDOM(1,CURR)==P) RETURN

      !    Test if node to insert is lower than current node 
      !    Test assure that P is different from IFRONT%P(1,CURR)
      IF(IDDCONNECT%PDOM(1,CURR)>P)THEN
	!    Insert P at beginning of the list
        IF(IDDCONNECT_END+1>SIDDCONNECT)THEN
	  !    Realloc if needed
	  CALL REALLOC_IDDCONNECT(NUMEL) 
	ENDIF
	!    create a new entry
	!    plug IENTRY(N) to this new entry
        IDDCONNECT_END = IDDCONNECT_END+1	
	IDDCONNECT%IENTRYDOM(1,N) = IDDCONNECT_END
	!    set new entry to P
        IDDCONNECT%PDOM(1,IDDCONNECT_END) =  P
	!    following is CURR index
	IDDCONNECT%PDOM(2,IDDCONNECT_END) =  CURR
        IDDCONNECT%IENTRYDOM(2,N) = IDDCONNECT%IENTRYDOM(2,N) + 1
	RETURN
      ENDIF
      
      NEXT = IDDCONNECT%PDOM(2,CURR)     
      !    insertion must be done between CURR and NEXT
      !    when CURR < P < NEXT
      SEARCH = .TRUE.
      !    move forward in the list while IFRONT%P(1,CURR) < P
      !    and while P not found 
      DO WHILE((SEARCH .EQV. .TRUE.).AND.(IDDCONNECT%PDOM(1,CURR)/=P))
        IF(NEXT==0)THEN
	  ! insert at end of list
	  IF(IDDCONNECT_END+1>SIDDCONNECT)THEN
	    CALL REALLOC_IDDCONNECT(NUMEL) 
	  ENDIF      
	  IDDCONNECT_END = IDDCONNECT_END+1		     
	  IDDCONNECT%PDOM(2,CURR) =  IDDCONNECT_END
          IDDCONNECT%PDOM(1,IDDCONNECT_END) =  P
	  IDDCONNECT%PDOM(2,IDDCONNECT_END) =  0
	  SEARCH = .FALSE.
          IDDCONNECT%IENTRYDOM(2,N) = IDDCONNECT%IENTRYDOM(2,N) + 1
	  RETURN
	ELSEIF(IDDCONNECT%PDOM(1,NEXT)>P)THEN
	  !   insert between current and next
	  IF(IDDCONNECT_END+1>SIDDCONNECT)THEN
	    CALL REALLOC_IDDCONNECT(NUMEL) 
	  ENDIF     
	  IDDCONNECT_END = IDDCONNECT_END+1		     
	  IDDCONNECT%PDOM(2,CURR) =  IDDCONNECT_END
          IDDCONNECT%PDOM(1,IDDCONNECT_END) =  P
	  IDDCONNECT%PDOM(2,IDDCONNECT_END) =  NEXT
	  SEARCH = .FALSE.
          IDDCONNECT%IENTRYDOM(2,N) = IDDCONNECT%IENTRYDOM(2,N) + 1
	  RETURN	   
	ELSE
	   !   move forward in list
	   CURR = NEXT
	   NEXT = IDDCONNECT%PDOM(2,NEXT)  
	ENDIF      
      ENDDO	      	
  
      RETURN
      END      
