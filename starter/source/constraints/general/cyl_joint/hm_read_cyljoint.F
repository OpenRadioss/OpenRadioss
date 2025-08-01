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
!||    hm_read_cyljoint       ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    anodset                ../starter/source/output/analyse/analyse_node.c
!||    fretitl                ../starter/source/starter/freform.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||    hm_sz_r2r              ../starter/source/coupling/rad2rad/routines_r2r.F
!||    ifrontplus             ../starter/source/spmd/node/frontplus.F
!||    kinset                 ../starter/source/constraints/general/kinset.F
!||    nodgrnr5               ../starter/source/starter/freform.F
!||    udouble                ../starter/source/system/sysfus.F
!||    usr2sys                ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    joint_mod              ../starter/share/modules1/joint_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    r2r_mod                ../starter/share/modules1/r2r_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_CYLJOINT(LJOINT ,ITABM1,IKINE  ,ITAB   ,IGRNOD,
     .                            NOM_OPT,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD     
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
      USE JOINT_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LJOINT(*), ITABM1(*), IKINE(*), ITAB(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)   :: IGRNOD
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K, I, NS, J, LJ, NU, IDIR,ID,N1,N2,IGU,IKINE1(3*NUMNOD),UID,NY,SUB_ID,IGS
      my_real :: BID
      CHARACTER MESS*40
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NODGRNR5
      EXTERNAL USR2SYS,NODGRNR5
C
      DATA MESS/'CYLINDRICAL JOINTS DEFINITION           '/
C=======================================================================
C
      IS_AVAILABLE = .FALSE.
C
      K=1
      NY = 0
C
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C      
      CALL HM_OPTION_START('/CYL_JOINT')
C
      DO I=1,NJOINT
        NY=NY+1
C----------Multidomaines --> on ignore les cyljoint non tags----------
        IF(NSUBDOM>0)THEN
                  IF(TAGCYL(NY)==0)CALL HM_SZ_R2R(TAGCYL,NY,LSUBMODEL)
        END IF
C----------------------------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR)
C
        NOM_OPT(1,I)=ID 
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)       
C
        CALL HM_GET_INTV('independentnode',N1,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('dependentnodes',N2,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('dependentnodeset',IGU,IS_AVAILABLE,LSUBMODEL)
C                                           
        NS=2+NODGRNR5(IGU,IGS,LJOINT(K+3),IGRNOD,ITABM1,MESS)  
C
        LJOINT(K+1)=USR2SYS(N1,ITABM1,MESS,ID)                            
        LJOINT(K+2)=USR2SYS(N2,ITABM1,MESS,ID)                  
        CALL ANODSET(LJOINT(K+1), CHECK_USED)                          
        CALL ANODSET(LJOINT(K+2), CHECK_USED)                         
        LJOINT(K)=NS                                                   
        WRITE (IOUT,1000) ID,NS                                        
        WRITE (IOUT,1100) (ITAB(LJOINT(K+J)),J=1,NS)                   

        !   allocation of cyl_joint%secondary_node : ns = secondary node + main node
        ALLOCATE( CYL_JOIN(I)%SECONDARY_NODE(NS) )
        DO LJ=1,NS
            NU = ITAB(LJOINT(K+LJ))
            CYL_JOIN(I)%SECONDARY_NODE(LJ) = LJOINT(K+LJ)
                  IF(JOINT_SMS) CALL IFRONTPLUS(LJOINT(K+LJ),1)
            !   ----------------------
            DO IDIR=1,6
                CALL KINSET(64,NU,IKINE(LJOINT(K+LJ)),IDIR,0
     .                        ,IKINE1(LJOINT(K+LJ)))
            ENDDO
            !   ----------------------
        ENDDO
        ! save the number of secondary nodes 
        CYL_JOIN(I)%NB_SECONDARY_NODE = NS
        ! save the main nodes 
        ALLOCATE( CYL_JOIN(I)%MAIN_NODE(2) )
        CYL_JOIN(I)%MAIN_NODE(1:2) = LJOINT(K+1:K+2)
        K=K+NS+1

      ENDDO
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(NOM_OPT,LNOPT1,NJOINT,MESS,0,BID)
C

 1000 FORMAT(/' CYLINDRICAL JOINT ',I10,       /
     +          ,' -------------------'/
     + '        ',I8,' NODES:')

 1100 FORMAT(7X,10I10)
      RETURN
      END

