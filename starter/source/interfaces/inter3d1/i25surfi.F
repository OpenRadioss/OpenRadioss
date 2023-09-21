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
Chd|  I25SURFI                      source/interfaces/inter3d1/i25surfi.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        IN24COQ_SOL3                  source/interfaces/inter3d1/i24surfi.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SH2SURF25                     source/interfaces/inter3d1/i25surfi.F
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PARAMETERS_MOD                ../common_source/modules/interfaces/parameters_mod.F
Chd|====================================================================
      SUBROUTINE I25SURFI(IALLO   ,IPARI   ,IGRNOD  ,IGRSURF ,
     1                    IRECT   ,FRIGAP  ,
     2                    NSV     ,MSR     ,ITAB    ,X       ,
     3                    NBINFLG ,MBINFLG ,MSEGTYP ,ISEADD  ,
     4                    ISEDGE  ,ITAG    ,INTPLY  ,IXC     ,
     5                    IXTG    ,KNOD2ELC,KNOD2ELTG,NOD2ELC,
     6                    NOD2ELTG,KNOD2ELS,NOD2ELS  ,IXS    ,
     7                    IXS10   ,IXS16   ,IXS20    ,IRTSE  ,
     8                    IS2SE   ,IS2PT   ,IS2ID    ,PARAMETERS)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE PARAMETERS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IALLO,NBINFLG(*)
      INTEGER IPARI(NPARI),
     .        IRECT(4,*), NSV(*),MSEGTYP(*),
     .        MSR(*),ITAB(*),MBINFLG(*),
     .        ISEADD(*) ,ISEDGE(*),ITAG(*),INTPLY,
     .        IXC(NIXC,*),IXTG(NIXTG,*),KNOD2ELC(*),KNOD2ELTG(*),
     .        NOD2ELC(*),NOD2ELTG(*),KNOD2ELS(*),NOD2ELS(*),
     .        IRTSE(5,*) ,IS2SE(*),IS2PT(*)   ,IS2ID(*)
      INTEGER IXS(NIXS,*),IXS10(6,*), IXS16(8,*), IXS20(12,*)
      INTEGER MODE, WORK(70000), NRTMP, I1, I2
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: IRECTMP
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: IRECTMP_SAV
      my_real
     .   X(3,*),FRIGAP(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (PARAMETERS_) ,INTENT(INOUT)   :: PARAMETERS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,LL,NL,ISU1,ISU2,NOD1,NRTM,NRTS,NSN,NMN,
     .        NLINSA,NLINMA,ILEV,NLN,ISYM,
     .        NLINS,NLINM,LINE1,LINE2,STAT,IL,IG,N,II,IJ,IMIN
      INTEGER TAG(NUMNOD),TAGS(NUMNOD),
     .        NSU1,NLS1,NLS2,NRTM_SH,ETYP,NRTM0,
     .        IMBIN,IM,IDEB,ISN
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGM
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER BITSET
      EXTERNAL BITSET
C
      CHARACTER MESS*40
      DATA MESS/'INTERFACE INPUT                         '/
      NSN   = 0
      NMN   = 0
      NRTM  = 0
      NRTS  = 0
      NOD1  = IPARI(26)
      NLN   = 0
      ILEV  = IPARI(20)
      ISU1  = IPARI(45)
      ISU2  = IPARI(46)
      NSU1 = 0
      NLS1 = 0
      NLS2 = 0
      IF(IALLO == 2 .AND. ILEV == 2 ) THEN
       IMBIN=1
      ELSE
       IMBIN=0
      END IF
C=======================================================================
c     SURFACES
C=======================================================================
c-----------------------------------------------------------------
c     surface S1
c-----------------------------------------------------------------
c-----------------------------------------------------------------
c     surface S2
c-----------------------------------------------------------------
      SELECT CASE (ILEV)
C-----attention: ISU2=ISU1 /=0            
        CASE(1)
          NRTM = IGRSURF(ISU1)%NSEG
        CASE(2)
          NRTM = IGRSURF(ISU1)%NSEG
          NRTS = IGRSURF(ISU2)%NSEG
          NRTM = NRTM + NRTS
        CASE(3)
          NRTM = IGRSURF(ISU2)%NSEG
      END SELECT
c-----------------------------------------------------------------
      ALLOCATE(INDEX(2*NRTM),IRECTMP(6,NRTM),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='IRECTSAV')
      IRECTMP(1:6,1:NRTM)=0
C
c---------------------------------------
c     copie des surfaces (IALLO == 2)
c---------------------------------------
      L = 0
      IF(ISU1 /= 0)THEN
        DO J=1,IGRSURF(ISU1)%NSEG
          L = L+1
          DO K=1,4
            IRECTMP(K,L) = IGRSURF(ISU1)%NODES(J,K)
          ENDDO
          IRECTMP(5,L) = IGRSURF(ISU1)%ELTYP(J) ! MSEGTYP
          CALL IN24COQ_SOL3(IRECTMP(1,L) ,IXC ,IXTG ,IRECTMP(5,L) ,X  ,
     .                 KNOD2ELC ,KNOD2ELTG ,NOD2ELC ,NOD2ELTG,
     .                 KNOD2ELS,NOD2ELS,IXS ,IXS10 ,IXS16 ,IXS20 )
          IF(IMBIN /= 0)IRECTMP(6,L) = BITSET(IRECTMP(6,L),0) ! MBINFLG

        ENDDO
      ENDIF
      NSU1 = L
      IF(ISU2 /= 0 .AND.ILEV /=1)THEN
        DO J=1,IGRSURF(ISU2)%NSEG
          L = L+1
          DO K=1,4
            IRECTMP(K,L) = IGRSURF(ISU2)%NODES(J,K)

!!            IAD = IAD+1
          ENDDO
          IRECTMP(5,L) = IGRSURF(ISU2)%ELTYP(J) ! MSEGTYP
          CALL IN24COQ_SOL3(IRECTMP(1,L) ,IXC ,IXTG ,IRECTMP(5,L) ,X  ,
     .                 KNOD2ELC ,KNOD2ELTG ,NOD2ELC ,NOD2ELTG,
     .                 KNOD2ELS,NOD2ELS,IXS ,IXS10 ,IXS16 ,IXS20 )
          IF(IMBIN /= 0) IRECTMP(6,L) = BITSET(IRECTMP(6,L),1) ! MBINFLG

        ENDDO
      ENDIF
C=======================================================================
c     Filtre
      NRTMP=NRTM
C
      ALLOCATE(IRECTMP_SAV(6,NRTM),STAT=stat)

        
      IRECTMP_SAV(1:6,1:NRTM) =  IRECTMP(1:6,1:NRTM)
      DO I=1,NRTM
        IF(IRECTMP(5,I)==0.OR.IRECTMP(5,I)==1) THEN
          DO J=1,3
            IMIN = IRECTMP(J,I)
            IJ = J
            DO K=J+1,4
              IF(IRECTMP(K,I)<IMIN) THEN 
                 IMIN = IRECTMP(K,I)
                 IJ = K
              ENDIF
            ENDDO
            IF(IJ /=J ) THEN
              IRECTMP(IJ,I) = IRECTMP(J,I)
              IRECTMP(J,I) = IMIN
            ENDIF
          ENDDO
        ENDIF
      ENDDO     
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, IRECTMP, INDEX, NRTM , 6)

      ALLOCATE(TAGM(NUMNOD)) ! tag table to avoid duplicate segments for solids (/SURF/PART/ALL/)
      TAGM(1:NUMNOD) = 0
      IF(IALLO==1)THEN

        NRTM_SH=0

        IDEB=1
        I1=INDEX(IDEB)
        DO WHILE(IRECTMP(1,I1)==0)
          IDEB=IDEB+1
          I1=INDEX(IDEB)
        END DO

        NRTM   =1
        IF(IRECTMP(5,I1)/=0 .AND. IRECTMP(5,I1)/=1) NRTM_SH=NRTM_SH+1

        DO J=IDEB,NRTMP-1
          I1=INDEX(J)
          I2=INDEX(J+1)
  
          IF(IRECTMP(1,I2)/=IRECTMP(1,I1).OR.
     .       IRECTMP(2,I2)/=IRECTMP(2,I1).OR.
     .       IRECTMP(3,I2)/=IRECTMP(3,I1).OR.
     .       IRECTMP(4,I2)/=IRECTMP(4,I1).OR.
     .       IRECTMP(5,I2)/=IRECTMP(5,I1))THEN ! Teste MSEGTYP pq ya un risque si renumerotation entre les 2 phases
                                                 ! A regler
            NRTM=NRTM+1
            IF(IRECTMP(5,I2)/=0 .AND. IRECTMP(5,I2)/=1) NRTM_SH=NRTM_SH+1

          END IF
        END DO
      ELSE ! IF(IALLO==1)THEN

        IDEB=1
        I1=INDEX(IDEB)
        DO WHILE(IRECTMP(1,I1)==0)
          IDEB=IDEB+1
          I1=INDEX(IDEB)
        END DO

        NRTM=1 ! recompute NRTM same as with IALLO=1 <=> Ipari(4)
        IRECT(1,NRTM)=IRECTMP_SAV(1,I1)
        IRECT(2,NRTM)=IRECTMP_SAV(2,I1)
        IRECT(3,NRTM)=IRECTMP_SAV(3,I1)
        IRECT(4,NRTM)=IRECTMP_SAV(4,I1)
        MSEGTYP(NRTM)=IRECTMP_SAV(5,I1)
        IF(IMBIN/=0) MBINFLG(NRTM)=IRECTMP_SAV(6,I1)

        DO J=IDEB,NRTMP-1
          I1=INDEX(J)
          I2=INDEX(J+1)
          IF(IRECTMP(1,I2)/=IRECTMP(1,I1).OR.
     .       IRECTMP(2,I2)/=IRECTMP(2,I1).OR.
     .       IRECTMP(3,I2)/=IRECTMP(3,I1).OR.
     .       IRECTMP(4,I2)/=IRECTMP(4,I1).OR.
     .       IRECTMP(5,I2)/=IRECTMP(5,I1))THEN ! Teste MSEGTYP pq ya un risque si renumerotation entre les 2 phases
                                                 ! A regler
            NRTM=NRTM+1
            IRECT(1,NRTM)=IRECTMP_SAV(1,I2)
            IRECT(2,NRTM)=IRECTMP_SAV(2,I2)
            IRECT(3,NRTM)=IRECTMP_SAV(3,I2)
            IRECT(4,NRTM)=IRECTMP_SAV(4,I2)
            MSEGTYP(NRTM)=IRECTMP_SAV(5,I2)
            IF(IMBIN/=0) MBINFLG(NRTM)=IRECTMP_SAV(6,I2)

          ELSEIF(IRECTMP(6,I1)/=IRECTMP(6,I2))THEN ! le segment appartient aux 2 surfaces
            IF(IMBIN/=0) MBINFLG(NRTM)=1+2
          END IF
        END DO
      END IF


      DEALLOCATE(INDEX,IRECTMP)
      IF(ALLOCATED(IRECTMP_SAV)) DEALLOCATE(IRECTMP_SAV)
C=======================================================================
c     NOEUDS
C=======================================================================
c-----------------------------------------------------------------
c     tag noeuds surfaces S1 S2; 1,2 on S1,S2, 3 on both (ILEV=1)
c-----------------------------------------------------------------
      DO I=1,NUMNOD
        TAG(I)=0 ! initialisation
        TAGS(I)=0 ! initialisation
      ENDDO
      IF(ISU2 /= 0)THEN
        DO J=1,IGRSURF(ISU2)%NSEG
          DO K=1,4
            TAG(IGRSURF(ISU2)%NODES(J,K)) = 2
          ENDDO
        ENDDO
      ENDIF
      IF(ISU1 /= 0)THEN
        DO J=1,IGRSURF(ISU1)%NSEG
          DO K=1,4
            I=IGRSURF(ISU1)%NODES(J,K)
            IF(TAG(I) == 0)THEN
              TAG(I) = 1
            ELSEIF(TAG(I) == 2)THEN
              TAG(I) = 3
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C for inteply activation needed for Plyxfem + Type24      
      IF(IALLO == 1) THEN
        IF(ISU2 /= 0)THEN
           DO J=1,IGRSURF(ISU2)%NSEG
              DO K=1,4



!!                 IAD = IAD+1
              ENDDO
            ENDDO
         ENDIF
         IF(ISU1 /= 0)THEN
           DO J=1,IGRSURF(ISU1)%NSEG
              DO K=1,4
                I=IGRSURF(ISU1)%NODES(J,K)


!!                IAD = IAD+1
              ENDDO
            ENDDO
         ENDIF 
      ENDIF
c-----------------------------------------------------------------
c     noeuds de la surface S2 : build TAGS,set NSV,MSR if IALLO= 2
c-----------------------------------------------------------------
      IF(ISU2 /= 0)THEN
        DO J=1,IGRSURF(ISU2)%NSEG
          DO K=1,4
            I=IGRSURF(ISU2)%NODES(J,K)
            IF(TAG(I) == 2 )THEN
              NMN = NMN + 1
              IF(IALLO == 2)MSR(NMN) = I
            ENDIF
c     taged nodes on S2 -> negative value
            IF(TAG(I) == 2 .OR. TAG(I) == 3)THEN
              TAG(I) = - TAG(I)
	      IF ( ILEV == 2.AND.TAGS(I) == 0 ) THEN
                NSN = NSN + 1
                TAGS(I) = NSN
                IF(IALLO == 2) THEN
                 NSV(NSN) = I
                 IF(ILEV == 2)NBINFLG(NSN) = BITSET(NBINFLG(NSN),1)
                END IF
	      END IF !( ILEV == 2 ) THEN
            ENDIF
          ENDDO
        ENDDO
      ENDIF
c-----------------------------------------------------------------
c     noeuds de la surface S1: build TAGS,set NSV,MSR if IALLO= 2
c-----------------------------------------------------------------
      IF(ISU1 /= 0)THEN
        DO J=1,IGRSURF(ISU1)%NSEG
          DO K=1,4
            I=IGRSURF(ISU1)%NODES(J,K)
            IF(TAGS(I) == 0 .AND. ILEV /= 3 ) THEN
              NSN = NSN + 1
              TAGS(I) = NSN
              IF(IALLO == 2) THEN
               NSV(NSN) = I
               IF(ILEV == 2)NBINFLG(NSN) = BITSET(NBINFLG(NSN),0)
              END IF
            ELSEIF(ILEV==2)THEN
              IF(IALLO == 2) THEN
                ISN=TAGS(I)
                NBINFLG(ISN) = BITSET(NBINFLG(ISN),0)
              END IF
            ENDIF
c     taged nodes on S1 -> negative value, ->+3 for nodes on both
            IF(TAG(I) == 1 .or. TAG(I) == -3)THEN
              TAG(I) = - TAG(I)
              NMN = NMN + 1
              IF(IALLO == 2)MSR(NMN) = I
            ENDIF
          ENDDO
        ENDDO
      ENDIF
c-----------------------------------------------------------------
c     noeuds du groupe de noeud NOD1: build TAGS,set NSV if IALLO= 2
c-----------------------------------------------------------------
      IF(NOD1 /= 0)THEN
        DO J=1,IGRNOD(NOD1)%NENTITY
          I = IGRNOD(NOD1)%ENTITY(J)
          IF(TAGS(I) == 0)THEN
            NSN = NSN+1
            TAGS(I) = NSN
            IF(IALLO == 2) THEN
             NSV(NSN) = I
             IF(ILEV == 2)NBINFLG(NSN) = BITSET(NBINFLG(NSN),2)
            END IF
          ENDIF
        ENDDO
      ENDIF

      IF(IALLO == 2 .and. IPRI >= 5) THEN
        WRITE(IOUT,'(/,A,I10,/)')' NODES USED FOR SECONDARY SIDE, INTERFACE ID=',IPARI(15)
        WRITE(IOUT,FMT=FMW_10I)(ITAB(NSV(I)),I=1,NSN)
      ENDIF
C=======================================================================
C=======================================================================
c-----------------------------------------------------------------
c     nombre de noeuds dans l'interface(SECONDARY+MAIN)
c-----------------------------------------------------------------
      IF(IALLO == 2) THEN 
       NLN   = IPARI(35)
       IPARI(51) = NLS1
       IPARI(52) = NLS2
C------initialization of doubler M_seg pour shells  add ISU1>0
       NRTM_SH= IPARI(42)
       NRTM0  = IPARI(4) - NRTM_SH
       CALL SH2SURF25(NRTM0,IRECT,IMBIN,MBINFLG,MSEGTYP,IPARI(4))

       IF(IPRI>=5) THEN
         WRITE(IOUT,'(/,A,I10,/)')' SEGMENTS USED FOR MAIN SURFACE, INTERFACE ID=',IPARI(15)
         DO I=1,NRTM
           WRITE(IOUT,FMT=FMW_4I)(ITAB(IRECT(K,I)),K=1,4)
         ENDDO
       ENDIF
C
      ELSE
C----------due the fact that NRTM is modified w/ shell seg
        IPARI(3)  = 0
        IPARI(4)  = NRTM
        IPARI(5)  = NSN
        IPARI(6)  = NMN
        IPARI(35) = NLN
        IPARI(42) = NRTM_SH
        IF(NRTM>NRTM_SH.AND.IPARI(17)/= 0) THEN ! Solid erosion
          IPARI(100) = 1
          PARAMETERS%INT25_EROSION_SOLID = 1
        ENDIF
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SH2SURF25                     source/interfaces/inter3d1/i25surfi.F
Chd|-- called by -----------
Chd|        I25SURFI                      source/interfaces/inter3d1/i25surfi.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SH2SURF25(NRTM0,IRECT,IMBIN,MBINFLG,MSEGTYP,NRTM )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM0,IMBIN,NRTM
      INTEGER IRECT(4,*),MBINFLG(*),MSEGTYP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NR, IT, J, ETYP(NRTM0),ITYPE
C=======================================================================
C------add asymmetric shell segs at end, change MSEGTYP
       DO I=1,NRTM0
        ETYP(I) = MSEGTYP(I)
       END DO
C       
       NR=NRTM0
       DO I=1,NRTM0
        MSEGTYP(I)=0
        IF(ETYP(I) ==3 .OR. ETYP(I) ==7 ) THEN
         NR =NR +1
         IRECT(1,NR)=IRECT(2,I)
         IRECT(2,NR)=IRECT(1,I)
         IRECT(3,NR)=IRECT(4,I)
         IRECT(4,NR)=IRECT(3,I)
         MSEGTYP(I)=NR
         MSEGTYP(NR)=-I
         IF(IMBIN/=0) MBINFLG(NR)=MBINFLG(I)
C------coating shell don't be doubled--         
        ELSEIF(ETYP(I) < 0 ) THEN
C------coating shell oppposite to solid externl segment => reverse ordering --         
         IT= IRECT(1,I)
         IRECT(1,I)=IRECT(2,I)
         IRECT(2,I)=IT
         IT= IRECT(3,I)
         IRECT(3,I)=IRECT(4,I)
         IRECT(4,I)=IT
C
         NR =NR +1
         IRECT(1,NR)=IRECT(2,I)
         IRECT(2,NR)=IRECT(1,I)
         IRECT(3,NR)=IRECT(4,I)
         IRECT(4,NR)=IRECT(3,I)
C--------coating shell <=> MSEGTYP(I) > NRTM     
         MSEGTYP(I) =  NR+NRTM
         MSEGTYP(NR)=-(I+NRTM)
         IF(IMBIN/=0) MBINFLG(NR)=MBINFLG(I)
        ELSEIF(ETYP(I) ==4 .OR. ETYP(I) ==8) THEN 
         NR =NR +1
         IRECT(1,NR)=IRECT(2,I)
         IRECT(2,NR)=IRECT(1,I)
         IRECT(3,NR)=IRECT(4,I)
         IRECT(4,NR)=IRECT(3,I)
C--------coating shell <=> MSEGTYP(I) > NRTM     
         MSEGTYP(I) =  NR+NRTM
         MSEGTYP(NR)=-(I+NRTM)
         IF(IMBIN/=0) MBINFLG(NR)=MBINFLG(I)
        END IF
       END DO
C------------------------------------------------------------
      RETURN
      END





