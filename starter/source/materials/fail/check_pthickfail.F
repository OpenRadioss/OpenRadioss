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
Chd|  CHECK_PTHICKFAIL              source/materials/fail/check_pthickfail.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CHECK_PTHICKFAIL(ELBUF_STR,MAT_PARAM,IPARG    ,GEO      ,
     .                            IPM      ,STACK    ,IGEO     ,
     .                            NUMMAT   ,NUMGEO   ,NGROUP   ,NPARG    ,
     .                            NPROPMI  ,NPROPGI  ,NPROPG   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD    
      USE STACK_MOD
      USE MESSAGE_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPARG(NPARG,NGROUP),NPROPG,
     .                       IPM(NPROPMI,NUMMAT),IGEO(NPROPGI,NUMGEO),
     .                       NUMMAT,NUMGEO,NGROUP,NPARG,NPROPMI,NPROPGI
      my_real, INTENT(IN) :: GEO(NPROPG,NUMGEO)
      TYPE (ELBUF_STRUCT_) ,DIMENSION(NGROUP) :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NG,ITY,IGTYP,PID,NLAY,IFL,NFAIL,NPTTOT,IL,
     .           IMAT,ISUBSTACK,IPTHKLY,PROP_ID,MAT_ID
      my_real :: P_THICKG,P_THICKFAIL,PTHKLY
c
      !===============================================================================
      ! CHECK PTHICKFAIL VALUE FOR SHELL PROPERTIES
      !===============================================================================
      ! Loop over groups
      DO NG = 1,NGROUP
c
        ! Group parameters
        IGTYP     = IPARG(38,NG) ! -> Property type 
        PID       = IPARG(62,NG) ! -> Property internal ID 
        ISUBSTACK = IPARG(71,NG) ! -> Substack number
c
        ! Check group property
        IF ((IGTYP ==  1).OR.(IGTYP ==  9).OR.(IGTYP == 10).OR.(IGTYP == 11).OR.
     .      (IGTYP == 16).OR.(IGTYP == 17).OR.(IGTYP == 51).OR.(IGTYP == 52)) THEN 
c 
          ! Property parameters
          P_THICKG = GEO(42,PID) ! -> Global Pthickfail parameter of the property
          PROP_ID  = IGEO(1,PID) ! -> Property user ID
          NLAY     = ELBUF_STR(NG)%NLAY ! -> Number of layers
          IPTHKLY  = 1 + 4*NLAY ! -> Address of PTHKFAIL for PLYs
          NPTTOT   = 0
          DO IL = 1,NLAY
            NPTTOT = NPTTOT + ELBUF_STR(NG)%BUFLY(IL)%NPTT
          ENDDO
c
          ! Loop over layers 
          DO IL = 1,NLAY
c              
            ! Layer parameters                                         
            NFAIL = ELBUF_STR(NG)%BUFLY(IL)%NFAIL ! -> Number of failure criteria
            IMAT  = ELBUF_STR(NG)%BUFLY(IL)%IMAT  ! -> Material internal ID 
            IF ((IGTYP == 51).OR.(IGTYP == 52)) THEN 
              PTHKLY = STACK%GEO(IPTHKLY + IL,ISUBSTACK) ! -> Pthickfail parameter for ply
            ENDIF
            MAT_ID = IPM(1,IMAT) ! -> Material user ID
c
            ! Loop over failure criteria
            DO IFL = 1,NFAIL
c
              ! Recover criterion PTHKFAIL value
              P_THICKFAIL = MAT_PARAM(IMAT)%FAIL(IFL)%PTHK
c
              ! Check conflicts between failure criteria and property value
              !   -> For 1 layer properties, check between global and failure criterion value
              IF (NLAY == 1) THEN  
                IF (((P_THICKFAIL > ZERO).AND.(P_THICKG < ZERO)).OR.
     .              ((P_THICKFAIL < ZERO).AND.(P_THICKG > ZERO))) THEN 
                  CALL ANCMSG(MSGID=3029,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=PROP_ID, 
     .                        R1=P_THICKG,
     .                        I2=MAT_ID,    
     .                        R2=P_THICKFAIL,
     .                        PRMOD=MSG_CUMU)
                ENDIF
              !   -> For multiple layer properties with 1 intg. point, check for failure criteria value
              ELSEIF (NLAY == NPTTOT) THEN
                IF (P_THICKFAIL /= ZERO) THEN 
                  CALL ANCMSG(MSGID=3030,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=PROP_ID, 
     .                        I2=IGTYP,
     .                        PRMOD=MSG_CUMU)                  
                ENDIF
              !   -> For multiple layer properties with several intg. points, check between ply and failure criterion value
              ELSE
                IF (((P_THICKFAIL > ZERO).AND.(PTHKLY < ZERO)).OR.
     .              ((P_THICKFAIL < ZERO).AND.(PTHKLY > ZERO))) THEN 
                  CALL ANCMSG(MSGID=3031,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=PROP_ID, 
     .                        I2=IL,
     .                        R1=PTHKLY,
     .                        I3=MAT_ID,    
     .                        R2=P_THICKFAIL,
     .                        PRMOD=MSG_CUMU)
                ENDIF                
              ENDIF
            ENDDO 
          ENDDO
        ENDIF
      ENDDO 
c
      ! Printing out warning messages
      CALL ANCMSG(MSGID=3029,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=3030,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=3031,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
      !===============================================================================
c
      END
