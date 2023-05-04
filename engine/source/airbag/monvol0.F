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
Chd|  MONVOL0                       source/airbag/monvol0.F       
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        AIRBAGA                       source/airbag/airbag1.F       
Chd|        AIRBAGA1                      source/airbag/airbaga1.F      
Chd|        AIRBAGB                       source/airbag/airbag2.F       
Chd|        AIRBAGB1                      source/airbag/airbagb1.F      
Chd|        SPMD_MV_CA                    source/mpi/airbags/spmd_mv_ca.F
Chd|        VOLOUT                        source/airbag/volout.F        
Chd|        VOLPFV                        source/airbag/volpfv.F        
Chd|        VOLPRE                        source/airbag/volpres.F       
Chd|        VOLPREP                       source/airbag/volpresp.F      
Chd|        VOLPVGA                       source/airbag/volpvg.F        
Chd|        VOLPVGB                       source/airbag/volpvg.F        
Chd|        VOLP_LFLUID                   source/airbag/volp_lfluid.F   
Chd|        VOLUM0                        source/airbag/volum0.F        
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MONVOL_STRUCT_MOD             share/modules/monvol_struct_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE MONVOL0(MONVOL   ,VOLMON  ,X       ,A      ,
     2                   NPC      ,TF      ,V       ,N      ,
     3                   FSAV     ,NSENSOR ,SENSOR_TAB,IGRSURF,
     4                   FR_MV    ,IADMV   ,
     5                   FSKY     ,ICONTACT,PORO    ,IPARG  ,
     6                   ELBUF_TAB,GEO     ,IGEO    ,
     7                   PM       ,IPM     ,IPART   ,IPARTC ,
     8                   IPARTTG  ,IGROUPC ,IGROUPTG,FEXT   ,
     9                   FLAG     ,H3D_DATA ,T_MONVOL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*),MONVOL(*),FR_MV(NSPMD+2,NVOLU),
     .   IADMV(4,*),ICONTACT(*),IPARG(NPARG,*),
     .   IGEO(NPROPGI,*),
     .   IPART(*),IPARTC(*), IPARTTG(*),IPM(NPROPMI,*),
     .   IGROUPC(*), IGROUPTG(*)
      INTEGER FLAG ! 1 => Monvol, 2 => FVMBAG type 6 8
C     REAL
      my_real
     .   X(3,*), A(3,*), TF(*), V(3,*), N(3,*),PORO(*),
     .   VOLMON(*),FSAV(NTHVKI,*),FSKY(*),
     .   GEO(NPROPG,*),PM(NPROPM,*),FEXT(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C     N(3,*)  NORMALE STOCKEE DANS WA
C     PORO(*) STOCKE DANS WA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .     I,ITYP,NCA,IAD,NN,IS,
     .     K1,K2,K6,KK1,KK2,
     .     PMAIN,KIBJET,KRBJET,KIBHOL,KRBHOL,
     .     NJET,IADJET,RADJET,NVENT,IADHOL,RADHOL
      my_real
     .     VOL
C---------------------------------------------------------
C
      K1 = 1
      KK1= 1
      DO I=1,NVOLU
         ITYP=MONVOL(K1+1)
         IF(ITYP==4.OR.ITYP==5.OR.ITYP==7.OR.ITYP==9)THEN
            VOLMON(KK1+31-1)=VOLMON(KK1+12-1)
         ENDIF
         K1 = K1 + NIMV
         KK1 = KK1 + NRVOLU
      ENDDO


      K1 = 1
      K6 = 1
      KK1 = 1 
      K2  = 1 + NIMV*NVOLU
      KK2 = 1 + NRVOLU * NVOLU
      KIBJET = K2  + LICBAG
      KIBHOL = KIBJET + LIBAGJET
      KRBJET = KK2 + LRCBAG
      KRBHOL = KRBJET + LRBAGJET
C
      DO I=1,NVOLU
        ITYP = MONVOL(K1+1)
        NCA  = MONVOL(K1+2) 
        IS   = MONVOL(K1+3)
        NN = IGRSURF(IS)%NSEG
C
        NJET  = MONVOL(K1 + 7)
        IADJET= KIBJET+MONVOL(K1+ 8)
        RADJET= KRBJET+MONVOL(K1+ 9)
        NVENT = MONVOL(K1 +10)
        IADHOL= KIBHOL+MONVOL(K1+11)
        RADHOL= KRBHOL+MONVOL(K1+12)
        IF(FR_MV(ISPMD+1,I)/=0 .OR. FR_MV(NSPMD+2,I)==ISPMD+1) THEN
          PMAIN = FR_MV(NSPMD+2,I)
        ELSE
C saut partie airbag si proc non concerne
          GO TO 100
        ENDIF
        IF(ITYP == 6 .OR. ITYP == 8) GO TO 100
C

        IF(FLAG == 1 .AND. (ITYP == 6 .OR. ITYP == 8)) GOTO 100 
        IF(FLAG == 2 .AND. .NOT.(ITYP == 6 .OR. ITYP == 8)) GOTO 100 

C        IF (IMACH/=3.OR.IPARIT==0) THEN
          CALL VOLUM0(
     1         MONVOL(K1),VOLMON(KK1),VOL       ,X,IGRSURF(IS)%NODES,
     2         N         ,NN         ,IGRSURF(IS)%ELTYP ,IGRSURF(IS)%ELEM,
     3         ICONTACT  ,PORO       ,FR_MV(1,I)  , T_MONVOL(I))
        IF(ITYP==1)THEN
C=======================================================================
C         VOLUME OUTPUT
C=======================================================================
          CALL VOLOUT(VOLMON(KK1),VOL,FSAV(1,I),PMAIN)
        ELSEIF(ITYP==2)THEN
C=======================================================================
C         VOLUME P=F(V)
C=======================================================================
          CALL VOLPFV(MONVOL(K1),VOLMON(KK1),VOL,FSAV(1,I),NPC,
     2                TF        ,PMAIN    )
        ELSEIF(ITYP==3)THEN
C=======================================================================
C         GAS PARFAIT
C=======================================================================
          CALL VOLPVGA(MONVOL(K1),VOLMON(KK1),VOL,FSAV(1,I),NVENT ,
     2            MONVOL(IADHOL) ,VOLMON(RADHOL),PMAIN )
        ELSEIF(ITYP==4.OR.ITYP==5)THEN
C=======================================================================
C         AIRBAGS
C=======================================================================
           CALL AIRBAGA(MONVOL(K1),NJET ,MONVOL(IADJET),NVENT ,MONVOL(IADHOL),
     2 MONVOL(K2),VOLMON(KK1),VOLMON(RADJET),VOLMON(RADHOL),VOLMON(KK2),
     3 VOLMON  ,NPC     ,TF      ,NSENSOR ,SENSOR_TAB ,
     4 VOL     ,PMAIN )
        ELSEIF(ITYP==7.OR.ITYP==9)THEN
C=======================================================================
C         AIRBAGS1 (NOUVEAUX INJECTEURS)
C=======================================================================
           CALL AIRBAGA1(MONVOL(K1),NJET ,MONVOL(IADJET),NVENT ,MONVOL(IADHOL),
     2 MONVOL(K2),VOLMON(KK1),VOLMON(RADJET),VOLMON(RADHOL),VOLMON(KK2),
     3 VOLMON  ,NPC     ,TF      ,NSENSOR ,SENSOR_TAB ,
     4 VOL     ,PMAIN ,GEO     ,IGEO    ,PM)
        ELSEIF(ITYP==10)THEN
C=======================================================================
C         LINEAIRE FLUIDE
C=======================================================================
          CALL VOLP_LFLUID(MONVOL(K1),VOLMON(KK1),VOL,FSAV(1,I),NPC,
     2                    TF        ,PMAIN )
        ENDIF
C=======================================================================
        IF(ITYP/=1)THEN
         IF (IPARIT==0) THEN
            CALL VOLPRE(MONVOL(K1),VOLMON(KK1),NJET ,MONVOL(IADJET),VOLMON(RADJET),
     2                 NSENSOR ,SENSOR_TAB ,X       ,V       ,A       ,
     3                 N       ,NPC     ,TF      ,NN  ,IGRSURF(IS)%NODES,
     4                 FEXT    ,H3D_DATA,IGRSURF(IS)%ELTYP,IGRSURF(IS)%ELEM)
         ELSE
            CALL VOLPREP(MONVOL(K1),VOLMON(KK1),NJET ,MONVOL(IADJET),VOLMON(RADJET),
     2                 NSENSOR ,SENSOR_TAB  ,X       ,V       ,A       ,
     3                 N       ,NPC     ,TF      ,NN  ,IGRSURF(IS)%NODES,
     4                 IADMV(1,K6),FSKY ,FSKY    ,FEXT, H3D_DATA ,
     5                 IGRSURF(IS)%ELTYP,IGRSURF(IS)%ELEM)
         ENDIF
        ENDIF
 100    CONTINUE
c        CALL SPMD_BARRIER()
        K1 = K1 + NIMV
        KK1= KK1 + NRVOLU
        K2 = K2  + NICBAG*NCA
        KK2= KK2 + NRCBAG*NCA
        K6 = K6 + NN
      ENDDO
C
C=======================================================================
      K1 = 1 
      K2 = 1 + NIMV * NVOLU
      KK1 = 1 
      KK2 = 1 + NRVOLU * NVOLU
      DO I=1,NVOLU
        ITYP = MONVOL(K1+1)
        IS   = MONVOL(K1+3)
        NCA  = MONVOL(K1+2) 
        NN  = IGRSURF(IS)%NSEG
        NJET = MONVOL(K1+7)
        IADJET=KIBJET+MONVOL(K1+ 8)
        RADJET=KRBJET+MONVOL(K1+ 9)
        NVENT =MONVOL(K1 +10)
        IADHOL=KIBHOL+MONVOL(K1+11)
        RADHOL=KRBHOL+MONVOL(K1+12)
        IF(FR_MV(ISPMD+1,I)==0.AND.NCA==0) THEN
          GO TO 200
        ENDIF
        IF(FLAG == 1 .AND.      (ITYP == 6 .OR. ITYP == 8)) GOTO 200 
        IF(FLAG == 2 .AND. .NOT.(ITYP == 6 .OR. ITYP == 8)) GOTO 200 

C=======================================================================
C       PERFECT GAS
C=======================================================================
        IF(ITYP==3)THEN
           CALL VOLPVGB(
     1           MONVOL(K1)    ,VOLMON(KK1)   ,VOL  ,FSAV(1,I) ,NVENT  ,
     2           MONVOL(IADHOL),VOLMON(RADHOL),N    ,NN        ,IGRSURF,
     3           IPARG         ,ELBUF_TAB,FR_MV(1,I), IGROUPC, IGROUPTG)
C=======================================================================
C       AIRBAGS
C=======================================================================
        ELSEIF(ITYP==4.OR.ITYP==5.OR.ITYP==7.OR.ITYP==9)THEN
         IF(NCA>0.AND.NSPMD>1) THEN
            CALL SPMD_MV_CA(
     1        FR_MV     ,I    ,NCA,VOLMON(KK1),VOLMON,
     2        MONVOL(K2),NJET ,MONVOL ,VOLMON(KRBJET),1,ITYP,100 )
            IF(FR_MV(ISPMD+1,I)==0) GOTO 150
         END IF
C=======================================================================
C        AIRBAGS /AIRBAG1 /COMMU1
C=======================================================================
         IF (ITYP==7.OR.ITYP==9) THEN
             CALL AIRBAGB1(MONVOL(K1),MONVOL(K2),NJET ,MONVOL(IADJET),NVENT,
     2 MONVOL(IADHOL),VOLMON(KK1),VOLMON ,VOLMON(KK2),VOLMON(RADJET),
     3 VOLMON(RADHOL),FSAV(1,I)  ,N       ,NN      ,
     4 IGRSURF       ,PORO       ,MONVOL  ,VOLMON(KRBJET),
     5 FR_MV(1,I)    ,IPARG      ,IPART   ,IPARTC  ,IPARTTG,
     6 IPM           ,PM         ,ELBUF_TAB,IGROUPC ,IGROUPTG,
     7 IGEO          ,GEO        )
         ELSE
C=======================================================================
C        AIRBAGS /AIRBAG /COMMU
C=======================================================================
             CALL AIRBAGB(MONVOL(K1),MONVOL(K2),NJET ,MONVOL(IADJET),NVENT,
     2 MONVOL(IADHOL),VOLMON(KK1),VOLMON ,VOLMON(KK2),VOLMON(RADJET),
     3 VOLMON(RADHOL),FSAV(1,I)  ,N       ,NN      ,
     4 IGRSURF       ,PORO       ,MONVOL  ,VOLMON(KRBJET),
     5 FR_MV(1,I))
         END IF
C
 150     CONTINUE
         IF(NCA>0.AND.NSPMD>1) THEN
            CALL SPMD_MV_CA(
     1        FR_MV     ,I    ,NCA,VOLMON(KK1),VOLMON,
     2        MONVOL(K2),NJET ,MONVOL ,VOLMON(KRBJET),2,ITYP,100)
         END IF
        ENDIF
 200    CONTINUE
        K1 = K1 + NIMV
        K2 = K2 + NICBAG * NCA
        KK1 = KK1 + NRVOLU
        KK2 = KK2 + NRCBAG * NCA
      ENDDO
      RETURN
      END
