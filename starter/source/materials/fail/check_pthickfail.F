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
      SUBROUTINE CHECK_PTHICKFAIL(ELBUF_STR,IPARG    ,GEO      ,BUFMAT   ,
     .                            SBUFMAT  ,IPM      ,STACK    ,IGEO     ,
     .                            NUMMAT   ,NUMGEO   ,NGROUP   ,NPARG    ,
     .                            NPROPMI  ,NPROPGI  ,NPROPG   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD    
      USE STACK_MOD
      USE MESSAGE_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPARG(NPARG,NGROUP),SBUFMAT,NPROPG,
     .                       IPM(NPROPMI,NUMMAT),IGEO(NPROPGI,NUMGEO),
     .                       NUMMAT,NUMGEO,NGROUP,NPARG,NPROPMI,NPROPGI
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP) :: ELBUF_STR
      my_real, INTENT(IN) :: GEO(NPROPG,NUMGEO),BUFMAT(SBUFMAT)
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NG,ITY,IGTYP,PID,NLAY,IRUPT,IFL,NFAIL,NPTTOT,IL,
     .           IMAT,IADBUFR,IP,NPARAMF,ISUBSTACK,IPTHKLY,PROP_ID,MAT_ID
      my_real :: P_THICKG,P_THICKFAIL,PTHKLY
      my_real, DIMENSION(:), ALLOCATABLE :: UPARAMF
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
              IP      =  (IFL - 1)*15
              IRUPT   =  ELBUF_STR(NG)%BUFLY(IL)%FAIL(1,1,1)%FLOC(IFL)%ILAWF ! -> Failure criterion type
              NPARAMF =  IPM(112 + IP,IMAT) ! -> Number of failure criterion parameter
              IADBUFR =  IPM(114 + IP,IMAT) ! -> Adress of 1st failure criterion parameter
              ALLOCATE(UPARAMF(NPARAMF))
              UPARAMF(1:NPARAMF) = BUFMAT(IADBUFR:IADBUFR+NPARAMF) ! -> Failure criterion parameters
              SELECT CASE (IRUPT)
                ! HASHIN
                CASE(14)
                  P_THICKFAIL = UPARAMF(17)
                ! TAB1 
                CASE(23)
                  P_THICKFAIL = UPARAMF(3)
                ! ORTHSTRAIN
                CASE(24)
                  P_THICKFAIL = UPARAMF(2)
                ! BIQUAD 
                CASE(30)
                  P_THICKFAIL = UPARAMF(7)
                ! HC_DSSE 
                CASE(32)
                  P_THICKFAIL = UPARAMF(6)
                ! ORTHBIQUAD
                CASE(38)
                  P_THICKFAIL = UPARAMF(1)
                ! GENE1
                CASE(39)
                  P_THICKFAIL = UPARAMF(22)
                ! TAB2 
                CASE(41)
                  P_THICKFAIL = UPARAMF(4)
                ! INIEVO
                CASE(42)
                  P_THICKFAIL = UPARAMF(5)
                CASE DEFAULT 
                  P_THICKFAIL = ZERO
              END SELECT
              IF (ALLOCATED(UPARAMF)) DEALLOCATE(UPARAMF)
c
              ! Check conflicts between failure criteria and property value
              !   -> For 1 layer properties, check between global and failure criterion value
              IF (NLAY == 1) THEN  
                IF (((P_THICKFAIL > ZERO).AND.(P_THICKG < ZERO)).OR.
     .              ((P_THICKFAIL < ZERO).AND.(P_THICKG > ZERO))) THEN 
                  CALL ANCMSG(MSGID=3027,
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
                  CALL ANCMSG(MSGID=3028,
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
                  CALL ANCMSG(MSGID=3029,
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
      CALL ANCMSG(MSGID=3027,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=3028,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=3029,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
      !===============================================================================
c
      END