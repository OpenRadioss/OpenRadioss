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
      !||    ini_ifront   ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    lectur       ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod    ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE INI_IFRONT()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------      
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"      
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------      

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------          
      INTEGER I

      DO I=1,NUMNOD
        IFRONT%IENTRY(I) = -1
      ENDDO
      
      DO I=1, SIFRONT
        IFRONT%P(1,I) = -1
        IFRONT%P(2,I) = -1      
      END DO
      
      IFRONT_END = NUMNOD            
C
      RETURN
      END      
      !||====================================================================
      !||    realloc_ifront   ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    ifrontplus       ../starter/source/spmd/node/frontplus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg           ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod        ../starter/share/modules1/front_mod.F
      !||    message_mod      ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE REALLOC_IFRONT()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(my_front) ::  IFRONT_SAVE
      INTEGER I, STAT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(IFRONT_SAVE%P(2,SIFRONT),STAT=stat)  
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IFRONT_SAVE')
           
c save IFRONT in IFRONT_SAVE
      DO I=1,SIFRONT
        IFRONT_SAVE%P(1,I) = IFRONT%P(1,I)
        IFRONT_SAVE%P(2,I) = IFRONT%P(2,I)      
      ENDDO  

c dealloc and realloc with bigger size (SIFRONT+NUMNOD)   
      DEALLOCATE(IFRONT%P)
      ALLOCATE(IFRONT%P(2,SIFRONT+NUMNOD),STAT=stat) 
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IFRONT REALLOC')        
      
      DO I=1,SIFRONT
        IFRONT%P(1,I) = IFRONT_SAVE%P(1,I)
        IFRONT%P(2,I) = IFRONT_SAVE%P(2,I)      
      ENDDO
      DO I=SIFRONT+1,SIFRONT+NUMNOD
        IFRONT%P(1,I) = -1
        IFRONT%P(2,I) = -1      
      ENDDO  
      
      DEALLOCATE(IFRONT_SAVE%P)   
      
c set new size of SIFRONT      
      SIFRONT = SIFRONT+NUMNOD
      
      RETURN
      END     
      !||====================================================================
      !||    plist_ifront                ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    c_irbe2                     ../starter/source/restart/ddsplit/c_irbe2.F
      !||    domain_decomposition_pcyl   ../starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
      !||    get_size_numnod_local       ../starter/source/spmd/get_size_tag.F
      !||    igrsurf_split               ../starter/source/spmd/igrsurf_split.F
      !||    prepare_split_i21           ../starter/source/restart/ddsplit/inter_tools.F
      !||    print_joint                 ../starter/source/constraints/general/cyl_joint/split_joint.F
      !||    r2r_domdec                  ../starter/source/coupling/rad2rad/r2r_domdec.F
      !||    split_cand_i20              ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i24              ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i25              ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i7               ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cfd_solide            ../starter/source/spmd/split_cfd_solide.F
      !||    split_joint                 ../starter/source/constraints/general/cyl_joint/split_joint.F
      !||    split_rwall                 ../starter/source/constraints/general/rwall/split_rwall.F90
      !||    spmd_userwi_rest            ../starter/source/user_interface/user_windows_tools.F
      !||    w_fi                        ../starter/source/restart/ddsplit/w_fi.F
      !||    w_front                     ../starter/source/restart/ddsplit/w_front.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod                   ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE PLIST_IFRONT(TAB,N,CPT)
C returns in "TAB" list of SPMD domains on which node N is sticked
C CPT is the number of SPMD domains on which node N is sticked
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,CPT,TAB(NSPMD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IAD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      TAB(1:NSPMD) = -1
      CPT=0
      IAD=IFRONT%IENTRY(N)
      IF(IAD==-1) RETURN
c if no proc set for this node     
c nothing to do as init has been done to -1 

c only one proc
      IF(IFRONT%P(2,IAD)==0)THEN 
        CPT = CPT+1     
        TAB(CPT)=IFRONT%P(1,IAD)
      ELSE     
c list of procs for node N       
        DO WHILE(IAD/=0)      
          CPT=CPT+1
          TAB(CPT)=IFRONT%P(1,IAD)
          IAD=IFRONT%P(2,IAD)
        ENDDO
      ENDIF
      
      RETURN
      END    
      !||====================================================================
      !||    c_ifront    ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    c_front     ../starter/source/restart/ddsplit/c_front.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod   ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE C_IFRONT(N,CPT)
c returns in CPT the number of procs on which node N is sticked      
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
      INTEGER N,CPT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IAD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CPT=0
      IAD=IFRONT%IENTRY(N)
c no proc set for this node     
c nothing to do as init has been done to -1 
      IF(IAD==-1)THEN
        CPT = 0
        RETURN 
      ENDIF
      
      IF(IFRONT%P(2,IAD)==0)THEN
c only one proc 
        CPT = CPT+1     
      ELSE     
c list of procs for node N       
        DO WHILE(IAD/=0)      
          CPT=CPT+1
          IAD=IFRONT%P(2,IAD)
        ENDDO
      ENDIF
      
      RETURN
      END                  
      !||====================================================================
      !||    nlocal                   ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    c_crkedge                ../starter/source/restart/ddsplit/c_crkedge.F
      !||    c_dampvrel               ../starter/source/restart/ddsplit/c_dampvrel.F
      !||    c_doms10                 ../starter/source/spmd/domdec2.F
      !||    c_front                  ../starter/source/restart/ddsplit/c_front.F
      !||    c_fvbag                  ../starter/source/airbag/c_fvbag.F
      !||    c_fxbody2                ../starter/source/restart/ddsplit/c_fxbody.F
      !||    c_ibcscyc                ../starter/source/restart/ddsplit/c_ibcscyc.F
      !||    c_ibft                   ../starter/source/restart/ddsplit/c_ibft.F
      !||    c_ibfv                   ../starter/source/restart/ddsplit/c_ibfv.F
      !||    c_ibvel                  ../starter/source/restart/ddsplit/c_ibvel.F
      !||    c_icfield                ../starter/source/restart/ddsplit/c_icfield.F
      !||    c_icnds10                ../starter/source/restart/ddsplit/c_icnds10.F
      !||    c_iloadp                 ../starter/source/restart/ddsplit/c_iloadp.F
      !||    c_irbe2                  ../starter/source/restart/ddsplit/c_irbe2.F
      !||    c_irbe3                  ../starter/source/restart/ddsplit/c_irbe3.F
      !||    c_joint_sms              ../starter/source/constraints/general/cyl_joint/write_count_joint_sms.F
      !||    c_llink                  ../starter/source/restart/ddsplit/c_llink.F
      !||    c_mad                    ../starter/source/restart/ddsplit/c_mad.F
      !||    c_poro                   ../starter/source/restart/ddsplit/c_poro.F
      !||    c_rbyk                   ../starter/source/restart/ddsplit/c_rbyk.F
      !||    c_rbymk                  ../starter/source/restart/ddsplit/c_rbymk.F
      !||    c_rwall                  ../starter/source/restart/ddsplit/c_rwall.F
      !||    c_seatbelts              ../starter/source/restart/ddsplit/c_seatbelts.F
      !||    c_sectio                 ../starter/source/restart/ddsplit/c_sectio.F
      !||    c_vois                   ../starter/source/restart/ddsplit/c_vois.F
      !||    domdec1                  ../starter/source/spmd/domain_decomposition/domdec1.F
      !||    domdec2                  ../starter/source/spmd/domdec2.F
      !||    f_nodloc2                ../starter/source/restart/ddsplit/f_nodloc2.F
      !||    fillcne                  ../starter/source/spmd/domdec2.F
      !||    flowdec                  ../starter/source/fluid/flowdec.F
      !||    globvars                 ../starter/source/spmd/globvars.F
      !||    hm_read_rbe2             ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rivet            ../starter/source/elements/reader/hm_read_rivet.F
      !||    i24setnodes              ../starter/source/interfaces/inter3d1/i24setnodes.F
      !||    ini_seatbelt             ../starter/source/tools/seatbelts/ini_seatbelt.F
      !||    iniend                   ../starter/source/interfaces/inter3d1/iniend.F
      !||    iniend2d                 ../starter/source/interfaces/inter3d1/iniend.F
      !||    inirbe3                  ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    ipari_l_ini              ../starter/source/restart/ddsplit/ipari_l_ini.F
      !||    lectur                   ../starter/source/starter/lectur.F
      !||    pre_cndpon               ../starter/source/elements/solid/solide10/dim_s10edg.F
      !||    prepare_split_cand_i21   ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i11        ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i17        ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i20        ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i21        ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i24        ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i25        ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i8         ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i9         ../starter/source/restart/ddsplit/inter_tools.F
      !||    print_joint              ../starter/source/constraints/general/cyl_joint/split_joint.F
      !||    r2r_clean_inter          ../starter/source/coupling/rad2rad/r2r_clean_inter.F
      !||    r2r_domdec               ../starter/source/coupling/rad2rad/r2r_domdec.F
      !||    r2r_split                ../starter/source/coupling/rad2rad/r2r_split.F
      !||    split_cand_i11           ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i20           ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i20_edge      ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i25           ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i7            ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_joint              ../starter/source/constraints/general/cyl_joint/split_joint.F
      !||    split_remnode_i24        ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_xsav               ../starter/source/restart/ddsplit/inter_tools.F
      !||    spmd_userwi_rest         ../starter/source/user_interface/user_windows_tools.F
      !||    spmdset                  ../starter/source/constraints/general/rbody/spmdset.F
      !||    thpinit                  ../starter/source/output/th/thpinit.F
      !||    w_dampvrel               ../starter/source/restart/ddsplit/w_dampvrel.F
      !||    w_fbft                   ../starter/source/restart/ddsplit/w_fbft.F
      !||    w_fi                     ../starter/source/restart/ddsplit/w_fi.F
      !||    w_fixvel                 ../starter/source/restart/ddsplit/w_fixvel.F
      !||    w_frbe3                  ../starter/source/restart/ddsplit/w_frbe3.F
      !||    w_front                  ../starter/source/restart/ddsplit/w_front.F
      !||    w_ibcscyc                ../starter/source/restart/ddsplit/w_ibcscyc.F
      !||    w_ibft                   ../starter/source/restart/ddsplit/w_ibft.F
      !||    w_ibfv                   ../starter/source/restart/ddsplit/w_ibfv.F
      !||    w_ibvel                  ../starter/source/restart/ddsplit/w_ibvel.F
      !||    w_icfield                ../starter/source/restart/ddsplit/w_icfield.F
      !||    w_icnds10                ../starter/source/restart/ddsplit/w_icnds10.F
      !||    w_irbe2                  ../starter/source/restart/ddsplit/w_irbe2.F
      !||    w_irbe3                  ../starter/source/restart/ddsplit/w_irbe3.F
      !||    w_irivet                 ../starter/source/restart/ddsplit/w_irivet.F
      !||    w_iskn                   ../starter/source/restart/ddsplit/w_iskn.F
      !||    w_joint_sms              ../starter/source/constraints/general/cyl_joint/write_count_joint_sms.F
      !||    w_llink                  ../starter/source/restart/ddsplit/w_llink.F
      !||    w_mad                    ../starter/source/restart/ddsplit/w_mad.F
      !||    w_main_proc_weight       ../starter/source/restart/ddsplit/w_master_proc_weight.F
      !||    w_pon                    ../starter/source/restart/ddsplit/w_pon.F
      !||    w_poro                   ../starter/source/restart/ddsplit/w_poro.F
      !||    w_rbyk                   ../starter/source/restart/ddsplit/w_rbyk.F
      !||    w_rbymk                  ../starter/source/restart/ddsplit/w_rbymk.F
      !||    w_rwall                  ../starter/source/restart/ddsplit/w_rwall.F
      !||    w_rwar                   ../starter/source/restart/ddsplit/w_rwar.F
      !||    w_seatbelts              ../starter/source/restart/ddsplit/w_seatbelts.F
      !||    w_secbuf                 ../starter/source/restart/ddsplit/w_secbuf.F
      !||    w_sectio                 ../starter/source/restart/ddsplit/w_sectio.F
      !||    w_th                     ../starter/source/restart/ddsplit/w_th.F
      !||    wrweight_rm              ../starter/source/restart/ddsplit/wrweight_rm.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod                ../starter/share/modules1/front_mod.F
      !||====================================================================
      INTEGER FUNCTION NLOCAL(N,P)  
C returns 1 if node N is sticked on SPMD domain P, else returns 0
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
C-----------------------------------------------      
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------    
      INTEGER N,P,IAD
      LOGICAL PSEARCH
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      PSEARCH = .TRUE.
      IAD = IFRONT%IENTRY(N) 
      NLOCAL = 0

      ! no SPMD domain attributed for this node 
      IF(IAD==-1)THEN
        NLOCAL = 0
        RETURN
      ENDIF
      
      !test if first proc is tested one (most frequent case)
      IF(IFRONT%P(1,IAD)==P)THEN
        NLOCAL = 1
        RETURN
      ENDIF
      
      IAD = IFRONT%P(2,IAD)
      IF (IAD==0)RETURN
      
      DO WHILE(PSEARCH)
        IF(IFRONT%P(1,IAD)==P) THEN
          NLOCAL = 1
          PSEARCH = .FALSE.
        ENDIF
        IF(IFRONT%P(1,IAD)>P) PSEARCH = .FALSE.
        IF(IFRONT%P(2,IAD)==0) PSEARCH = .FALSE.
        IAD = IFRONT%P(2,IAD)
      ENDDO       
           
      RETURN 
      END      

      !||====================================================================
      !||    set_front8      ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    lectur          ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod       ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE SET_FRONT8(IPARI,INTERCEP,INTBUF_TAB,T8,NBT8,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------      
      USE FRONT_MOD
      USE INTBUFDEF_MOD  
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(INT8_STRUCT_) :: T8(NSPMD,NBT8)
      INTEGER :: NBT8,ITAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NI,K,I,PROC,P,Q,NB
      INTEGER N1,N2,N3,N4
      INTEGER ITY,NMN,NRTM,NM_SHARED
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAG,INDEX_IN_COMM
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_IN_FRONT
      INTEGER :: S_FRONT8(NSPMD,NSPMD),IDX(NSPMD)
      INTEGER :: LOCAL_ID,II,JJ,KK,NSN
C--------------------------------------------------------------


      NBT8 = 1
      DO NI=1,NINTER
        !get generic values
        ITY  = IPARI(7,NI)
        NMN  = IPARI(6,NI)
        NRTM = IPARI(4,NI)
        NSN  = IPARI(5,NI)

        LOCAL_ID = 0
        IF(ITY == 8) THEN
          ALLOCATE(INDEX_IN_FRONT(NMN))
          INDEX_IN_FRONT(1:NMN) = 0
          ALLOCATE(TAG(NSPMD,NMN))
          ALLOCATE(INDEX_IN_COMM(NSPMD,NMN))
          TAG(1:NSPMD,1:NMN) = 0
          DO K=1,NRTM
             N1 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
             N2 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
             N3 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
             N4 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
             PROC = INTERCEP(1,NI)%P(K)          
             TAG(PROC,N1) = 1
             TAG(PROC,N2) = 1
             TAG(PROC,N3) = 1
             TAG(PROC,N4) = 1
          ENDDO

          ! Compute the number of main nodes shared between 
          ! each possible couple of proc (i,j) => S_FRONT8(i,j)
          S_FRONT8 = 0
          DO P = 1,NSPMD
             DO Q = P+1,NSPMD
               DO K = 1,NMN
                 IF(TAG(P,K) == 1 .AND. TAG(Q,K) == 1) THEN
                 !The main is shared between procs P and Q
                   LOCAL_ID = LOCAL_ID + 1
                   S_FRONT8(P,Q) = S_FRONT8(P,Q) + 1
                   S_FRONT8(Q,P) = S_FRONT8(Q,P) + 1
                   ! the kth main node will have to be communucated
                   IF( INDEX_IN_FRONT(k) == 0) THEN 
                     INDEX_IN_FRONT(k) = LOCAL_ID
                   ENDIF
                 ENDIF
               ENDDO
             ENDDO 
          ENDDO
          IDX(1:NSPMD) = 0
          INDEX_IN_COMM(1:NSPMD,1:NMN) = 0
          !INDEX_IN_COMM give an index to the communication structures
          ! of each main node (or 0 if the main node is not shared)

          DO K = 1,NMN
            Q = 0
            DO P = 1,NSPMD
             Q = Q + TAG(P,K)  
            ENDDO
            IF(Q > 1) THEN
              DO P = 1,NSPMD
                 IF(TAG(P,K) /= 0) THEN
                 IDX(P) = IDX(P) + 1
                 INDEX_IN_COMM(P,K)=IDX(P)                       
                 ENDIF
              ENDDO
            ENDIF
          ENDDO
          ! symmetric allocation of arrays of size *nb main nodes in common* 
          ! between proc P and proc Q
          DO P = 1,NSPMD
             DO Q = P+1,NSPMD
              NM_SHARED = S_FRONT8(P,Q)
              T8(P,NBT8)%BUFFER(Q)%NBMAIN = 0! NM_SHARED
              ALLOCATE(T8(P,NBT8)%BUFFER(Q)%MAIN_ID(NM_SHARED))  
              ALLOCATE(T8(P,NBT8)%BUFFER(Q)%MAIN_UID(NM_SHARED)) 
              ALLOCATE(T8(P,NBT8)%BUFFER(Q)%NBSECND(NM_SHARED))     
              T8(P,NBT8)%BUFFER(Q)%NBSECND(1:NM_SHARED) = 0
              T8(Q,NBT8)%BUFFER(P)%NBMAIN = 0 !NM_SHARED
              ALLOCATE(T8(Q,NBT8)%BUFFER(P)%MAIN_ID(NM_SHARED))     
              ALLOCATE(T8(Q,NBT8)%BUFFER(P)%MAIN_UID(NM_SHARED))     
              ALLOCATE(T8(Q,NBT8)%BUFFER(P)%NBSECND(NM_SHARED))     
              T8(Q,NBT8)%BUFFER(P)%NBSECND(1:NM_SHARED) = 0
             ENDDO 
          ENDDO


         !Compute the total number of main nodes to exchange per
         ! proc P
          DO P = 1,NSPMD
             K = IDX(P)
             T8(P,NBT8)%S_COMM = K
             ALLOCATE(T8(P,NBT8)%SPMD_COMM_PATTERN(K))
             DO Q = 1,K
               T8(P,NBT8)%SPMD_COMM_PATTERN(Q)%NUMLOC = 0
               T8(P,NBT8)%SPMD_COMM_PATTERN(Q)%NBCOM = 0
               ALLOCATE(T8(P,NBT8)%SPMD_COMM_PATTERN(Q)%PROCLIST(NSPMD))
               T8(P,NBT8)%SPMD_COMM_PATTERN(Q)%PROCLIST(1:NSPMD) = 0
               ALLOCATE(T8(P,NBT8)%SPMD_COMM_PATTERN(Q)%BUF_INDEX(NSPMD))
               T8(P,NBT8)%SPMD_COMM_PATTERN(Q)%BUF_INDEX(1:NSPMD) = 0
             ENDDO
          ENDDO



          ! Fill the part of the structure that depends only
          ! on main nodes

          ! To optimize the communication pattern in the engine, 
          ! The data dependencies are build in a symmetric fashion:
          ! If procs P and Q share main K main nodes, then
          ! T8(Q,NBT8)%BUFFER(P)%MAIN_UID(1:K) = 
          ! T8(P,NBT8)%BUFFER(Q)%MAIN_UID(1:K)  
          IDX(1:NSPMD) = 1 
          S_FRONT8(1:NSPMD,1:NSPMD) = 0
          DO P = 1,NSPMD
            DO K = 1,NMN
              !If the node is has to be communicated by P
              IF(INDEX_IN_COMM(P,K) > 0) THEN 
                DO Q = P+1,NSPMD
                  IF(INDEX_IN_COMM(Q,K)/=0) THEN
!                   Put the main node in the boundary of P with Q
                    LOCAL_ID = INDEX_IN_COMM(P,K)
                    NB = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NBCOM +1  
                    T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%PROCLIST(NB) = Q 
                    II = S_FRONT8(P,Q) + 1  
                    T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%BUF_INDEX(NB) = II 
                    JJ = T8(P,NBT8)%BUFFER(Q)%NBMAIN+1
                    T8(P,NBT8)%BUFFER(Q)%MAIN_ID(II) = K
                    T8(P,NBT8)%BUFFER(Q)%MAIN_UID(II) =
     .                 ITAB(INTBUF_TAB(NI)%MSR(K)) 

                    S_FRONT8(P,Q) = II
                    T8(P,NBT8)%BUFFER(Q)%NBMAIN = JJ
                    T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NBCOM=NB 
                    T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NUMLOC = K 

                    ! SYMMETRIC : put the node in the boundary of Q with P 
                    LOCAL_ID = INDEX_IN_COMM(Q,K)
                    NB = T8(Q,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NBCOM +1  
                    T8(Q,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%PROCLIST(NB) = P 
                    II = S_FRONT8(Q,P) + 1  
                    T8(Q,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%BUF_INDEX(NB) = II 
                    JJ = T8(Q,NBT8)%BUFFER(P)%NBMAIN+1
                    T8(Q,NBT8)%BUFFER(P)%MAIN_ID(II) = K
                    T8(Q,NBT8)%BUFFER(P)%MAIN_UID(II) = 
     .                 ITAB(INTBUF_TAB(NI)%MSR(K)) 
                    S_FRONT8(Q,P) = II
                    T8(Q,NBT8)%BUFFER(P)%NBMAIN = JJ
                    T8(Q,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NBCOM=NB 
                    T8(Q,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NUMLOC = K 
                  ENDIF 
                ENDDO
              ENDIF
            ENDDO
          ENDDO ! NSPMD


          DO P  =1,NSPMD
          ! Count the number of actual secnds that have a main 
          ! shared between multiples procs
            DO I = 1,NSN
              IF(INDEX_IN_COMM(P,INTBUF_TAB(NI)%ILOCS(I)) > 0) THEN  
                 LOCAL_ID = INDEX_IN_COMM(P,INTBUF_TAB(NI)%ILOCS(I))  
                 NB = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NBCOM 
                 DO K =1,NB
                  II = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%BUF_INDEX(K)  
                  Q = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%PROCLIST(K)  
                  T8(P,NBT8)%BUFFER(Q)%NBSECND(II) = 
     .            T8(P,NBT8)%BUFFER(Q)%NBSECND(II) + 1
                  T8(P,NBT8)%BUFFER(Q)%NBSECND_TOT = 
     .            T8(P,NBT8)%BUFFER(Q)%NBSECND_TOT + 1
                 ENDDO
              ENDIF
            ENDDO !NSN
           !DO Q = 1,NSPMD
           ! IF(Q/=P) THEN
           !   ! Total number of secnd node that have main nodes on processor
           !   ! P shared with processor Q
           !   II = T8(P,NBT8)%BUFFER(Q)%NBSECND_TOT 
           !   ALLOCATE(T8(P,NBT8)%BUFFER(Q)%SECND_ID(II))
           !   ALLOCATE(T8(P,NBT8)%BUFFER(Q)%SECND_UID(II))
           !   T8(P,NBT8)%BUFFER(Q)%SECND_ID(1:II) = 0
           !   T8(P,NBT8)%BUFFER(Q)%SECND_UID(1:II) = 0
           !   NB = T8(P,NBT8)%BUFFER(Q)%NBMAIN
           !   IF(NB > 0) THEN
           !   ! This array will keep pointers to secnd_id per main
           !     ALLOCATE(T8(P,NBT8)%BUFFER(Q)%BUFI(NB))
           !     T8(P,NBT8)%BUFFER(Q)%BUFI(1) = 1
           !     DO I = 2,NB                             
           !       T8(P,NBT8)%BUFFER(Q)%BUFI(I) = 
     .     !       T8(P,NBT8)%BUFFER(Q)%BUFI(I-1) + 
     .     !       T8(P,NBT8)%BUFFER(Q)%NBSECND(I-1) 
           !     ENDDO
           !   ENDIF
           ! ENDIF
           !ENDDO !Q = 1,NSPMD
           !DO I = 1,NSN
           !  IF(INTBUF_TAB(NI)%ILOCS(I) > 0 ) THEN
           !  ! If this secnd has a main shared by multiple proc
           !   IF(INDEX_IN_COMM(P,INTBUF_TAB(NI)%ILOCS(I)) > 0) THEN  
           !     LOCAL_ID = INDEX_IN_COMM(P,INTBUF_TAB(NI)%ILOCS(I))  
           !     NB = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NBCOM 
           !     DO K =1,NB
           !      II = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%BUF_INDEX(K)  
           !      Q = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%PROCLIST(K)  
           !      JJ = T8(P,NBT8)%BUFFER(Q)%BUFI(II)  
           !      T8(P,NBT8)%BUFFER(Q)%SECND_ID(JJ) = I 
           !      T8(P,NBT8)%BUFFER(Q)%SECND_UID(JJ)= ITAB(INTBUF_TAB(NI)%NSV(I)) 
           !      T8(P,NBT8)%BUFFER(Q)%BUFI(II) = JJ + 1 
           !     ENDDO
           !   ENDIF
           !  ENDIF
           !ENDDO !I=1,NSN
          ENDDO

!         USEFUL DEBUG PRINT
!         DO P = 1,NSPMD
!           WRITE(6,*) '============== Proc',P,'===',T8(P,NBT8)%S_COMM
!           DO LOCAL_ID=1,T8(P,NBT8)%S_COMM
!             NB = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NBCOM 
!             WRITE(6,*) 'NLOC=',T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%NUMLOC,'NB=',NB
!             DO K =1,NB
!                 II = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%BUF_INDEX(K)  
!                 Q = T8(P,NBT8)%SPMD_COMM_PATTERN(LOCAL_ID)%PROCLIST(K)  
!                 WRITE(6,*) 'Q=',Q,T8(P,NBT8)%BUFFER(Q)%MAIN_UID(II) 
!             ENDDO
!           ENDDO
!         ENDDO
!         DO P = 1,NSPMD
!         DO Q = 1,NSPMD
!            IF(P /= Q) THEN
!              DO II = 1,T8(P,NBT8)%BUFFER(Q)%NBMAIN
!                 WRITE(6,*) "EXCH",P,Q,II,T8(P,NBT8)%BUFFER(Q)%MAIN_UID(II)
!              ENDDO
!            ENDIF
!         ENDDO
!         ENDDO
!          DO P = 1, NSPMD
!             DO K = 1,T8(P,NBT8)%S_COMM 
!             WRITE(6,*) P,ITAB(
!    .         INTBUF_TAB(NI)%MSR(T8(P,NBT8)%SPMD_COMM_PATTERN(K)%NUMLOC)) 
!             ENDDO
!          ENDDO



          DEALLOCATE(TAG)
          DEALLOCATE(INDEX_IN_COMM)
          DEALLOCATE(INDEX_IN_FRONT)
          NBT8 = NBT8 + 1
        ENDIF !ITY == 8
      ENDDO



      END

      !||====================================================================
      !||    set_intercep    ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    lectur          ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    intersurfl      ../starter/source/spmd/node/ddtools.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod       ../starter/share/modules1/front_mod.F
      !||    message_mod     ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE SET_INTERCEP(IPARI,INTERCEP,FLAG,INTBUF_TAB,ITAB,CEP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------      
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),FLAG,ITAB(*),CEP(*)
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  INTERSURFL
      EXTERNAL INTERSURFL             
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITY,NRTM,N,N1,N2,N3,N4,NSN,
     .        NRTS,N1L,N2L,N3L,N4L,NLINM,NLINS
      INTEGER NI,K,I,PROC,IE
C--------------------------------------------------------------
      DO NI=1,NINTER
      
        !get generic values
        ITY    = IPARI(7,NI)
        
        IF ((FLAG==0.AND.(ITY==24.OR.(ITY==25.AND.IPARI(100,NI) == 0))).OR.
     .      (FLAG==1.AND.(ITY==7.OR.ITY==10.OR.
     .      ITY==22.OR.ITY==23)) )THEN
     
          NRTM   = IPARI(4,NI)

C Allocate CEP INTERFACE
          IF (.NOT.(ASSOCIATED(INTERCEP(1,NI)%P)))THEN
            ALLOCATE(INTERCEP(1,NI)%P(NRTM))
            IF(ITY==25) ALLOCATE(INTERCEP(2,NI)%P(NRTM))
          ENDIF   
          INTERCEP(1,NI)%P(1:NRTM)=0
          IF(ITY==25) THEN
            IF (.NOT.(ASSOCIATED(INTERCEP(2,NI)%P)))THEN
                  ALLOCATE(INTERCEP(2,NI)%P(NRTM))
            ENDIF
            INTERCEP(2,NI)%P(1:NRTM) = 0
          ENDIF
              
          DO K=1,NRTM
             N1=INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
             N2=INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
             N3=INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
             N4=INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
             IF(N1>NUMNOD.OR.N2>NUMNOD.OR.
     .          N3>NUMNOD.OR.N4>NUMNOD) THEN
               INTERCEP(1,NI)%P(K) = 1
             ELSE
               !find first SPMD domain on which the 4 nodes of the surface are
               PROC = INTERSURFL(N1,N2,N3,N4)
               INTERCEP(1,NI)%P(K) = PROC       
             ENDIF   
          ENDDO
        ELSEIF (FLAG==0.AND.ITY==25.AND.IPARI(100,NI) > 0)THEN ! in case of solid erosion : split interface according to element proc
            NRTM   = IPARI(4,NI)
C Allocate CEP INTERFACE
            IF (.NOT.(ASSOCIATED(INTERCEP(1,NI)%P)))THEN
               ALLOCATE(INTERCEP(1,NI)%P(NRTM))
            ENDIF   
            INTERCEP(1,NI)%P(1:NRTM)=0
            DO K=1,NRTM
               IE = INTBUF_TAB(NI)%IELEM_M(2*(K-1)+1)
               IF(IE > 0) THEN
                  INTERCEP(1,NI)%P(K) = CEP(IE) + 1
               ENDIF
            ENDDO
        ELSEIF(ITY==24.AND.FLAG==1.AND.IPARI(86,NI) > 0) THEN          

C Allocate CEP INTERFACE
             NRTS   = IPARI(3,NI)

             IF (.NOT.(ASSOCIATED(INTERCEP(3,NI)%P)))THEN
                ALLOCATE(INTERCEP(3,NI)%P(NRTS))
             ENDIF        
             INTERCEP(3,NI)%P(1:NRTS)=0
              
             DO K=1,NRTS
               ! SECND SEGMENT AND ELEMENT HAVE TO ON THE SAME MPI DOMAIN (only solids/ if other CEP(OFF+IE))
                IE = INTBUF_TAB(NI)%IELNRTS(K)
                IF(IE > 0) THEN
                  PROC = CEP(IE)
                  INTERCEP(3,NI)%P(K) = PROC + 1  
                ENDIF
             ENDDO

        !ENDIF INTER TYPE 7, 10, 22, 23, 24, 25
        ELSEIF (ITY==8) THEN
          NRTM   = IPARI(4,NI)

C Allocate CEP INTERFACE
          IF (.NOT.(ASSOCIATED(INTERCEP(1,NI)%P)))THEN
            ALLOCATE(INTERCEP(1,NI)%P(NRTM))
          ENDIF   
          INTERCEP(1,NI)%P(1:NRTM)=0
              
          DO K=1,NRTM
             N1=INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
             N2=INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
             N3=INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
             N4=INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
             N1=INTBUF_TAB(NI)%MSR(N1)
             N2=INTBUF_TAB(NI)%MSR(N2)
             N3=INTBUF_TAB(NI)%MSR(N3)
             N4=INTBUF_TAB(NI)%MSR(N4)

             !find first SPMD domain on which the 4 nodes of the surface are
             PROC = INTERSURFL(N1,N2,N3,N4)
             INTERCEP(1,NI)%P(K) = PROC      
          ENDDO

        ELSEIF (ITY==11) THEN

          NRTS   = IPARI(3,NI)
          NRTM   = IPARI(4,NI)  

C Allocate CEP INTERFACE
          IF (.NOT.(ASSOCIATED(INTERCEP(1,NI)%P)))THEN
            ALLOCATE(INTERCEP(1,NI)%P(NRTM))        
          ENDIF  
          IF (.NOT.(ASSOCIATED(INTERCEP(2,NI)%P)))THEN
            ALLOCATE(INTERCEP(2,NI)%P(NRTS))        
          ENDIF            
          INTERCEP(1,NI)%P(1:NRTM)=0
          INTERCEP(2,NI)%P(1:NRTS)=0      

          DO K=1,NRTM 
            N1 = INTBUF_TAB(NI)%IRECTM(2*(K-1)+1)
            N2 = INTBUF_TAB(NI)%IRECTM(2*(K-1)+2)          
            !find first SPMD domain on which the 2 nodes of the surface are
            !use same generic routine with N1=N2 and N3=N4
            PROC = INTERSURFL(N1,N1,N2,N2)                        
            INTERCEP(1,NI)%P(K) = PROC    
          ENDDO
                  
          DO K=1,NRTS
            N1 = INTBUF_TAB(NI)%IRECTS(2*(K-1)+1)
            N2 = INTBUF_TAB(NI)%IRECTS(2*(K-1)+2)  
            !find first SPMD domain on which the 2 nodes of the surface are
            !use same generic routine with N1=N2 and N3=N4
            PROC = INTERSURFL(N1,N1,N2,N2)                        
            INTERCEP(2,NI)%P(K) = PROC
          ENDDO 
          
        !ENDIF INTER TYPE 11      
        ELSEIF (ITY==20) THEN
        
          NRTM   = IPARI(4,NI)
          NLINS  = IPARI(51,NI)   
          NLINM  = IPARI(52,NI) 
     
C Allocate CEP INTERFACE
          IF (.NOT.(ASSOCIATED(INTERCEP(1,NI)%P)))THEN
            ALLOCATE(INTERCEP(1,NI)%P(NRTM))        
          ENDIF  
          IF (.NOT.(ASSOCIATED(INTERCEP(2,NI)%P)))THEN
            ALLOCATE(INTERCEP(2,NI)%P(NLINM))       
          ENDIF  
          IF (.NOT.(ASSOCIATED(INTERCEP(3,NI)%P)))THEN
            ALLOCATE(INTERCEP(3,NI)%P(NLINS))       
          ENDIF            
          INTERCEP(1,NI)%P(1:NRTM) =0
          INTERCEP(2,NI)%P(1:NLINM)=0
          INTERCEP(3,NI)%P(1:NLINS)=0     
                
          DO K=1,NRTM
            N1L = INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
            N2L = INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
            N3L = INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
            N4L = INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
            N1 = INTBUF_TAB(NI)%NLG(N1L)
            N2 = INTBUF_TAB(NI)%NLG(N2L)
            N3 = INTBUF_TAB(NI)%NLG(N3L)
            N4 = INTBUF_TAB(NI)%NLG(N4L)        
           !find first SPMD domain on which the 4 nodes of the surface are      
            PROC = INTERSURFL(N1,N2,N3,N4)                        
            INTERCEP(1,NI)%P(K) = PROC
          ENDDO

          DO K=1,NLINM
            N1L = INTBUF_TAB(NI)%IXLINM(2*(K-1)+1)
            N2L = INTBUF_TAB(NI)%IXLINM(2*(K-1)+2)
            N1 = INTBUF_TAB(NI)%NLG(N1L)
            N2 = INTBUF_TAB(NI)%NLG(N2L)
            !find first SPMD domain on which the 2 nodes of the surface are
            !use same generic routine with N1=N2 and N3=N4          
            PROC = INTERSURFL(N1,N1,N2,N2)                        
            INTERCEP(2,NI)%P(K) = PROC        
          ENDDO 

          DO K=1,NLINS
            N1L = INTBUF_TAB(NI)%IXLINS(2*(K-1)+1)
            N2L = INTBUF_TAB(NI)%IXLINS(2*(K-1)+2)
            N1 = INTBUF_TAB(NI)%NLG(N1L)
            N2 = INTBUF_TAB(NI)%NLG(N2L)
            !find first SPMD domain on which the 2 nodes of the surface are
            !use same generic routine with N1=N2 and N3=N4          
            PROC = INTERSURFL(N1,N1,N2,N2)                        
            INTERCEP(3,NI)%P(K) = PROC        
          ENDDO                 
                    
        !ENDIF INTER TYPE 11
        ELSEIF (ITY==21) THEN   
        
          NRTS   = IPARI(3,NI)
          
C Allocate CEP INTERFACE
          IF (.NOT.(ASSOCIATED(INTERCEP(1,NI)%P)))THEN
            ALLOCATE(INTERCEP(1,NI)%P(NRTS))        
          ENDIF                   
          INTERCEP(1,NI)%P(1:NRTS) =0   
                  
          DO K=1,NRTS
            N1 = INTBUF_TAB(NI)%IRECTS(4*(K-1)+1)
            N2 = INTBUF_TAB(NI)%IRECTS(4*(K-1)+2)
            N3 = INTBUF_TAB(NI)%IRECTS(4*(K-1)+3)
            N4 = INTBUF_TAB(NI)%IRECTS(4*(K-1)+4)
            !find first SPMD domain on which the 4 nodes of the surface are
            PROC = INTERSURFL(N1,N2,N3,N4)
            INTERCEP(1,NI)%P(K) = PROC            
          ENDDO           
                        
        ENDIF !ENDIF INTER TYPE 21
        

      ENDDO !ENDDO 1,NINTER

      END
      
      !||====================================================================
      !||    fill_intercep   ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    lectur          ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    intersurfl      ../starter/source/spmd/node/ddtools.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod       ../starter/share/modules1/front_mod.F
      !||    message_mod     ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE FILL_INTERCEP(IPARI,INTBUF_TAB,INTERCEP)
C   new routine called right after domdec1 to be used by interface sorting
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------      
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  INTERSURFL
      EXTERNAL INTERSURFL             
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITY,NRTM,N,N1,N2,N3,N4,NSN,
     .        NRTS,N1L,N2L,N3L,N4L,NLINM,NLINS
      INTEGER NI,K,I,PROC
C--------------------------------------------------------------
      DO NI=1,NINTER
      
        !get generic values
        ITY    = IPARI(7,NI)
        
        IF (ITY==7)THEN
     
          NRTM   = IPARI(4,NI)
C Allocate CEP INTERFACE
          IF (.NOT.(ASSOCIATED(INTERCEP(1,NI)%P)))THEN
            ALLOCATE(INTERCEP(1,NI)%P(NRTM))
          ENDIF   
          INTERCEP(1,NI)%P(1:NRTM)=0

          DO K=1,NRTM
             N1=INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
             N2=INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
             N3=INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
             N4=INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
             !find first SPMD domain on which the 4 nodes of the surface are
             IF(N1>NUMNOD.OR.N2>NUMNOD.OR.
     .          N3>NUMNOD.OR.N4>NUMNOD) THEN
               INTERCEP(1,NI)%P(K) = 1
             ELSE
               PROC = INTERSURFL(N1,N2,N3,N4)
               INTERCEP(1,NI)%P(K) = PROC
             ENDIF           
          ENDDO
        
        ENDIF
        
      ENDDO !ENDDO 1,NINTER

      END

      !||====================================================================
      !||    intersurfl      ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    fill_intercep   ../starter/source/spmd/node/ddtools.F
      !||    i24setnodes     ../starter/source/interfaces/inter3d1/i24setnodes.F
      !||    set_intercep    ../starter/source/spmd/node/ddtools.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod       ../starter/share/modules1/front_mod.F
      !||====================================================================
      INTEGER FUNCTION INTERSURFL(N1,N2,N3,N4)   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
C-----------------------------------------------      
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------      
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------    
      INTEGER IAD1,IAD2,IAD3,IAD4,
     .        P1,P2,P3,P4,PMAX
      INTEGER TAB(4),NN 
      LOGICAL SEARCH
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------      
      INTERSURFL = -1
      SEARCH = .TRUE.
      
      IAD1 = IFRONT%IENTRY(N1)
      IAD2 = IFRONT%IENTRY(N2)
      IAD3 = IFRONT%IENTRY(N3)
      IAD4 = IFRONT%IENTRY(N4)
                    
      DO WHILE(SEARCH)
        P1 = IFRONT%P(1,IAD1)
        P2 = IFRONT%P(1,IAD2)
        P3 = IFRONT%P(1,IAD3)
        P4 = IFRONT%P(1,IAD4)    
        IF(P1==P2.AND.P2==P3.AND.P3==P4)THEN
          INTERSURFL = P1
          SEARCH = .FALSE.
        ELSE
          PMAX = MAX(P1,P2,P3,P4)                  
          IF(P1<PMAX) IAD1 = IFRONT%P(2,IAD1)
          IF(P2<PMAX) IAD2 = IFRONT%P(2,IAD2)
          IF(P3<PMAX) IAD3 = IFRONT%P(2,IAD3)
          IF(P4<PMAX) IAD4 = IFRONT%P(2,IAD4)
        ENDIF
      ENDDO
!     IF(INTERSURFL > 4) THEN
!     WRITE(6,*) __FILE__,__LINE__,IAD1,IAD2,IAD3,IAD4
!     ENDIF
        
      RETURN 
      END
      !||====================================================================
      !||    ini_iddconnect   ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    dometis          ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod        ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE INI_IDDCONNECT(NELEM)
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
      INTEGER NELEM 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------          
      INTEGER I

      DO I=1,NELEM
        IDDCONNECT%IENTRYDOM(1,I) = -1
        IDDCONNECT%IENTRYDOM(2,I) =  0
      ENDDO

      DO I=1, SIDDCONNECT
        IDDCONNECT%PDOM(1,I) = -1
        IDDCONNECT%PDOM(2,I) = -1       
      END DO

      IDDCONNECT_END = NELEM            
C
      RETURN
      END
      !||====================================================================
      !||    realloc_iddconnect   ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    iddconnectplus       ../starter/source/spmd/node/frontplus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg               ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod            ../starter/share/modules1/front_mod.F
      !||    message_mod          ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE REALLOC_IDDCONNECT(NELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER,DIMENSION(:,:),ALLOCATABLE ::  IDDCONNECT_SAVE
      INTEGER I, STAT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      STAT = 0
      ALLOCATE(IDDCONNECT_SAVE(2,SIDDCONNECT+NELEM),STAT=stat)  
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IDDCONNECT_SAVE')
           
!     save IDDCONNECT in IFRONT_SAVEDOM
      DO I=1,SIDDCONNECT
        IDDCONNECT_SAVE(1,I) = IDDCONNECT%PDOM(1,I)
        IDDCONNECT_SAVE(2,I) = IDDCONNECT%PDOM(2,I)        
      ENDDO
      DO I=SIDDCONNECT+1,SIDDCONNECT+NELEM
        IDDCONNECT_SAVE(1,I) = -1
        IDDCONNECT_SAVE(2,I) = -1       
      ENDDO    

      CALL MOVE_ALLOC(IDDCONNECT_SAVE, IDDCONNECT%PDOM)
      SIDDCONNECT = SIDDCONNECT+NELEM
      
      RETURN
      END
      !||====================================================================
      !||    c_iddconnect   ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    dometis        ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    plist_bfs      ../starter/source/spmd/node/ddtools.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod      ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE C_IDDCONNECT(N,CPT)
C returns in CPT the number of connected nodes 
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
      INTEGER N,CPT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IAD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CPT=0
      IAD=IDDCONNECT%IENTRYDOM(1,N)
!     if no connected node     
!     nothing to do as init has been done to -1 
      IF(IAD==-1)THEN
        CPT = 0
        RETURN 
      ENDIF
      
      IF(IDDCONNECT%PDOM(2,IAD)==0)THEN
!     only one connected node
        CPT = CPT+1     
      ELSE     
!       list of connected nodes for node N        
        DO WHILE(IAD/=0)      
          CPT=CPT+1
          IAD=IDDCONNECT%PDOM(2,IAD)
        ENDDO
      ENDIF
      
      RETURN
      END
      !||====================================================================
      !||    plist_iddconnect   ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    dometis            ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod          ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE PLIST_IDDCONNECT(ADJNCY,XADJ,N)
C returns in "ADJNCY" nodes connected to node N
C CPT is the number of nodes
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
      INTEGER TAILLE
      INTEGER N,ADJNCY(*),XADJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER CPT,IAD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CPT=XADJ(N)-1
      IAD=IDDCONNECT%IENTRYDOM(1,N)

!     if no connected node     
!     nothing to do as init has been done to -1 

!     only one connected node
      IF(IDDCONNECT%PDOM(2,IAD)==0)THEN 
        CPT = CPT+1     
        ADJNCY(CPT)=IDDCONNECT%PDOM(1,IAD)
      ELSE     
!       list of connected nodes for node N       
        DO WHILE(IAD/=0)      
          CPT=CPT+1
          ADJNCY(CPT)=IDDCONNECT%PDOM(1,IAD)
          IAD=IDDCONNECT%PDOM(2,IAD)
        ENDDO
      ENDIF
      
      RETURN
      END        
      !||====================================================================
      !||    plist_bfs      ../starter/source/spmd/node/ddtools.F
      !||--- called by ------------------------------------------------------
      !||    dometis        ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||--- calls      -----------------------------------------------------
      !||    c_iddconnect   ../starter/source/spmd/node/ddtools.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod      ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE PLIST_BFS(NELEM,NCONNX,COLORS,ROOTS)
C-----------------------------------------------
C            MODULES
C-----------------------------------------------
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEM,  NCONNX,
     .        COLORS(NELEM), ROOTS(NELEM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVISIT, N, I
      INTEGER, DIMENSION(:), ALLOCATABLE ::  FILE_V
      INTEGER, DIMENSION(:), ALLOCATABLE ::  XADJ
      INTEGER :: FILE_NEXT, ROOT, CURRENT, LEN
      INTEGER :: CPT,IAD
C-----------------------------------------------

      ALLOCATE(FILE_V(NELEM))
      ALLOCATE(XADJ(NELEM+1))
      XADJ(1:NELEM+1)=0
      XADJ(1) = 1
      DO I=1,NELEM
        CALL C_IDDCONNECT(I,LEN)
        XADJ(I+1) = XADJ(I) + LEN 
      ENDDO

      DO N = 1, NELEM
        COLORS(N)=0
      END DO
      NVISIT=0
      ROOT=1 ! first element of the graph == first vertex available
      NCONNX=0
      
      DO WHILE (NVISIT < NELEM) ! loop until all vertices are visited
        NCONNX = NCONNX+1
        DO WHILE ((ROOT <= NELEM) .AND. (COLORS(ROOT) /= 0))
          ROOT = ROOT + 1
        END DO
        ROOTS(NCONNX)=ROOT ! record roots for fatest treatments
        FILE_V(1)=ROOT
        FILE_NEXT=2 ! new file initialized with root
        COLORS(ROOT)=NCONNX ! root marked
        NVISIT=NVISIT+1
        DO WHILE (FILE_NEXT > 1) ! test file not nill
          CURRENT = FILE_V(FILE_NEXT-1)
          FILE_NEXT = FILE_NEXT-1

          CPT=XADJ(CURRENT)-1
          IAD=IDDCONNECT%IENTRYDOM(1,CURRENT)

          DO N = XADJ(CURRENT), XADJ(CURRENT+1)-1
C           I = ADJNCY(N)
            CPT = CPT+1     
            I=IDDCONNECT%PDOM(1,IAD)
            IAD=IDDCONNECT%PDOM(2,IAD)
C

            IF(COLORS(I) == 0) THEN ! vertex not treated before
              FILE_V(FILE_NEXT)=I
              FILE_NEXT = FILE_NEXT+1
              COLORS(I) = NCONNX
              NVISIT=NVISIT+1
            END IF
          END DO            
        END DO
      END DO

      DEALLOCATE(FILE_V)
      RETURN
      END

