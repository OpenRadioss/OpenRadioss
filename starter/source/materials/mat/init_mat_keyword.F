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
      !||    init_mat_keyword      ../starter/source/materials/mat/init_mat_keyword.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat           ../starter/source/materials/mat/hm_read_mat.F90
      !||    hm_read_mat00         ../starter/source/materials/mat/mat000/hm_read_mat00.F
      !||    hm_read_mat01         ../starter/source/materials/mat/mat001/hm_read_mat01.F
      !||    hm_read_mat02         ../starter/source/materials/mat/mat002/hm_read_mat02.F
      !||    hm_read_mat03         ../starter/source/materials/mat/mat003/hm_read_mat03.F
      !||    hm_read_mat04         ../starter/source/materials/mat/mat004/hm_read_mat04.F
      !||    hm_read_mat05         ../starter/source/materials/mat/mat005/hm_read_mat05.F
      !||    hm_read_mat06         ../starter/source/materials/mat/mat006/hm_read_mat06.F
      !||    hm_read_mat06_keps    ../starter/source/materials/mat/mat006/hm_read_mat06_keps.F
      !||    hm_read_mat10         ../starter/source/materials/mat/mat010/hm_read_mat10.F
      !||    hm_read_mat100        ../starter/source/materials/mat/mat100/hm_read_mat100.F
      !||    hm_read_mat101        ../starter/source/materials/mat/mat101/hm_read_mat101.F
      !||    hm_read_mat102        ../starter/source/materials/mat/mat102/hm_read_mat102.F
      !||    hm_read_mat103        ../starter/source/materials/mat/mat103/hm_read_mat103.F
      !||    hm_read_mat104        ../starter/source/materials/mat/mat104/hm_read_mat104.F
      !||    hm_read_mat106        ../starter/source/materials/mat/mat106/hm_read_mat106.F
      !||    hm_read_mat107        ../starter/source/materials/mat/mat107/hm_read_mat107.F
      !||    hm_read_mat108        ../starter/source/materials/mat/mat108/hm_read_mat108.F
      !||    hm_read_mat109        ../starter/source/materials/mat/mat109/hm_read_mat109.F
      !||    hm_read_mat11         ../starter/source/materials/mat/mat011/hm_read_mat11.F
      !||    hm_read_mat110        ../starter/source/materials/mat/mat110/hm_read_mat110.F
      !||    hm_read_mat111        ../starter/source/materials/mat/mat111/hm_read_mat111.F
      !||    hm_read_mat112        ../starter/source/materials/mat/mat112/hm_read_mat112.F
      !||    hm_read_mat113        ../starter/source/materials/mat/mat113/hm_read_mat113.F
      !||    hm_read_mat114        ../starter/source/materials/mat/mat114/hm_read_mat114.F
      !||    hm_read_mat115        ../starter/source/materials/mat/mat115/hm_read_mat115.F
      !||    hm_read_mat116        ../starter/source/materials/mat/mat116/hm_read_mat116.F
      !||    hm_read_mat117        ../starter/source/materials/mat/mat117/hm_read_mat117.F
      !||    hm_read_mat119        ../starter/source/materials/mat/mat119/hm_read_mat119.F
      !||    hm_read_mat11_k_eps   ../starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
      !||    hm_read_mat12         ../starter/source/materials/mat/mat012/hm_read_mat12.F
      !||    hm_read_mat120        ../starter/source/materials/mat/mat120/hm_read_mat120.F
      !||    hm_read_mat121        ../starter/source/materials/mat/mat121/hm_read_mat121.F
      !||    hm_read_mat122        ../starter/source/materials/mat/mat122/hm_read_mat122.F
      !||    hm_read_mat124        ../starter/source/materials/mat/mat124/hm_read_mat124.F
      !||    hm_read_mat125        ../starter/source/materials/mat/mat125/hm_read_mat125.F90
      !||    hm_read_mat126        ../starter/source/materials/mat/mat126/hm_read_mat126.F90
      !||    hm_read_mat127        ../starter/source/materials/mat/mat127/hm_read_mat127.F90
      !||    hm_read_mat128        ../starter/source/materials/mat/mat128/hm_read_mat128.F90
      !||    hm_read_mat13         ../starter/source/materials/mat/mat013/hm_read_mat13.F
      !||    hm_read_mat14         ../starter/source/materials/mat/mat014/hm_read_mat14.F
      !||    hm_read_mat15         ../starter/source/materials/mat/mat015/hm_read_mat15.F
      !||    hm_read_mat151        ../starter/source/materials/mat/mat151/hm_read_mat151.F
      !||    hm_read_mat158        ../starter/source/materials/mat/mat158/hm_read_mat158.F
      !||    hm_read_mat16         ../starter/source/materials/mat/mat016/hm_read_mat16.F
      !||    hm_read_mat163        ../starter/source/materials/mat/mat163/hm_read_mat163.F90
      !||    hm_read_mat169_arup   ../starter/source/materials/mat/mat169/hm_read_mat169.F90
      !||    hm_read_mat18         ../starter/source/materials/mat/mat018/hm_read_mat18.F
      !||    hm_read_mat19         ../starter/source/materials/mat/mat019/hm_read_mat19.F
      !||    hm_read_mat190        ../starter/source/materials/mat/mat190/hm_read_mat190.F
      !||    hm_read_mat20         ../starter/source/materials/mat/mat020/hm_read_mat20.F
      !||    hm_read_mat21         ../starter/source/materials/mat/mat021/hm_read_mat21.F
      !||    hm_read_mat22         ../starter/source/materials/mat/mat022/hm_read_mat22.F
      !||    hm_read_mat23         ../starter/source/materials/mat/mat023/hm_read_mat23.F
      !||    hm_read_mat24         ../starter/source/materials/mat/mat024/hm_read_mat24.F
      !||    hm_read_mat25         ../starter/source/materials/mat/mat025/hm_read_mat25.F
      !||    hm_read_mat26         ../starter/source/materials/mat/mat026/hm_read_mat26.F
      !||    hm_read_mat27         ../starter/source/materials/mat/mat027/hm_read_mat27.F
      !||    hm_read_mat28         ../starter/source/materials/mat/mat028/hm_read_mat28.F
      !||    hm_read_mat29_31      ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
      !||    hm_read_mat32         ../starter/source/materials/mat/mat032/hm_read_mat32.F
      !||    hm_read_mat33         ../starter/source/materials/mat/mat033/hm_read_mat33.F
      !||    hm_read_mat34         ../starter/source/materials/mat/mat034/hm_read_mat34.F
      !||    hm_read_mat35         ../starter/source/materials/mat/mat035/hm_read_mat35.F
      !||    hm_read_mat36         ../starter/source/materials/mat/mat036/hm_read_mat36.F
      !||    hm_read_mat37         ../starter/source/materials/mat/mat037/hm_read_mat37.F
      !||    hm_read_mat38         ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat40         ../starter/source/materials/mat/mat040/hm_read_mat40.F
      !||    hm_read_mat41         ../starter/source/materials/mat/mat041/hm_read_mat41.F
      !||    hm_read_mat42         ../starter/source/materials/mat/mat042/hm_read_mat42.F
      !||    hm_read_mat43         ../starter/source/materials/mat/mat043/hm_read_mat43.F
      !||    hm_read_mat44         ../starter/source/materials/mat/mat044/hm_read_mat44.F
      !||    hm_read_mat46         ../starter/source/materials/mat/mat046/hm_read_mat46.F
      !||    hm_read_mat48         ../starter/source/materials/mat/mat048/hm_read_mat48.F
      !||    hm_read_mat49         ../starter/source/materials/mat/mat049/hm_read_mat49.F
      !||    hm_read_mat50         ../starter/source/materials/mat/mat050/hm_read_mat50.F90
      !||    hm_read_mat51         ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    hm_read_mat52         ../starter/source/materials/mat/mat052/hm_read_mat52.F
      !||    hm_read_mat53         ../starter/source/materials/mat/mat053/hm_read_mat53.F
      !||    hm_read_mat54         ../starter/source/materials/mat/mat054/hm_read_mat54.F
      !||    hm_read_mat57         ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat58         ../starter/source/materials/mat/mat058/hm_read_mat58.F
      !||    hm_read_mat59         ../starter/source/materials/mat/mat059/hm_read_mat59.F
      !||    hm_read_mat60         ../starter/source/materials/mat/mat060/hm_read_mat60.F
      !||    hm_read_mat62         ../starter/source/materials/mat/mat062/hm_read_mat62.F
      !||    hm_read_mat63         ../starter/source/materials/mat/mat063/hm_read_mat63.F
      !||    hm_read_mat64         ../starter/source/materials/mat/mat064/hm_read_mat64.F
      !||    hm_read_mat65         ../starter/source/materials/mat/mat065/hm_read_mat65.F
      !||    hm_read_mat66         ../starter/source/materials/mat/mat066/hm_read_mat66.F
      !||    hm_read_mat68         ../starter/source/materials/mat/mat068/hm_read_mat68.F
      !||    hm_read_mat69         ../starter/source/materials/mat/mat069/hm_read_mat69.F
      !||    hm_read_mat70         ../starter/source/materials/mat/mat070/hm_read_mat70.F
      !||    hm_read_mat71         ../starter/source/materials/mat/mat071/hm_read_mat71.F
      !||    hm_read_mat72         ../starter/source/materials/mat/mat072/hm_read_mat72.F
      !||    hm_read_mat73         ../starter/source/materials/mat/mat073/hm_read_mat73.F
      !||    hm_read_mat74         ../starter/source/materials/mat/mat074/hm_read_mat74.F
      !||    hm_read_mat75         ../starter/source/materials/mat/mat075/hm_read_mat75.F
      !||    hm_read_mat76         ../starter/source/materials/mat/mat076/hm_read_mat76.F
      !||    hm_read_mat77         ../starter/source/materials/mat/mat077/hm_read_mat77.F
      !||    hm_read_mat78         ../starter/source/materials/mat/mat078/hm_read_mat78.F
      !||    hm_read_mat79         ../starter/source/materials/mat/mat079/hm_read_mat79.F
      !||    hm_read_mat80         ../starter/source/materials/mat/mat080/hm_read_mat80.F
      !||    hm_read_mat81         ../starter/source/materials/mat/mat081/hm_read_mat81.F90
      !||    hm_read_mat82         ../starter/source/materials/mat/mat082/hm_read_mat82.F
      !||    hm_read_mat83         ../starter/source/materials/mat/mat083/hm_read_mat83.F
      !||    hm_read_mat84         ../starter/source/materials/mat/mat084/hm_read_mat84.F
      !||    hm_read_mat87         ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||    hm_read_mat88         ../starter/source/materials/mat/mat088/hm_read_mat88.F
      !||    hm_read_mat90         ../starter/source/materials/mat/mat090/hm_read_mat90.F
      !||    hm_read_mat92         ../starter/source/materials/mat/mat092/hm_read_mat92.F
      !||    hm_read_mat93         ../starter/source/materials/mat/mat093/hm_read_mat93.F
      !||    hm_read_mat94         ../starter/source/materials/mat/mat094/hm_read_mat94.F
      !||    hm_read_mat95         ../starter/source/materials/mat/mat095/hm_read_mat95.F
      !||    hm_read_mat97         ../starter/source/materials/mat/mat097/hm_read_mat97.F
      !||    hm_read_mat_99        ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
      !||    hm_read_visc          ../starter/source/materials/visc/hm_read_visc.F
      !||    sgrtails              ../starter/source/elements/solid/solide/sgrtails.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE INIT_MAT_KEYWORD(MATPARAM,KEYWORD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=*) ::  KEYWORD
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT
C=======================================================================
      SELECT CASE (KEYWORD)
c---
        CASE ("COMPRESSIBLE")      
          MATPARAM%COMPRESSIBILITY = 1
c
        CASE ("INCOMPRESSIBLE")      
          MATPARAM%COMPRESSIBILITY = 2
c
        CASE ("ELASTO_PLASTIC")      
          MATPARAM%COMPRESSIBILITY = 3
c
        CASE ("INCREMENTAL")      
          MATPARAM%STRAIN_FORMULATION = 1
c
        CASE ("TOTAL")      
          MATPARAM%STRAIN_FORMULATION = 2
c
        CASE ("LARGE_STRAIN")      
          MATPARAM%SMSTR = 2
c
        CASE ("SMALL_STRAIN")      
          MATPARAM%SMSTR = 1
c          
        CASE ("HYDROSTATIC")      
          MATPARAM%IPRES = 1
c          
        CASE ("HYDRO_EOS")      
          MATPARAM%IPRES = 2
c          
        CASE ("HOOK")      
          MATPARAM%IPRES = 3
c
        CASE ("ISOTROPIC")      
          MATPARAM%ORTHOTROPY = 1
c
        CASE ("ORTHOTROPIC")      
          MATPARAM%ORTHOTROPY = 2
c
        CASE ("ANISOTROPIC")      
          MATPARAM%ORTHOTROPY = 3
c
        CASE ("SOLID_ISOTROPIC")   ! Compatibility with /PROP/TYPE 6/14/20/21/22
          MATPARAM%PROP_SOLID = 1          
c
        CASE ("SOLID_ORTHOTROPIC") ! Compatibility with /PROP/TYPE 6/21/22
          MATPARAM%PROP_SOLID = 2  
c
        CASE ("SOLID_COMPOSITE")   ! Compatibility with /PROP/TYPE 22
          MATPARAM%PROP_SOLID = 3
c
        CASE ("SOLID_COHESIVE")    ! Compatibility with /PROP/TYPE 43
          MATPARAM%PROP_SOLID = 4
c
        CASE ("SOLID_POROUS")      ! Compatibility with /PROP/TYPE 14/15
          MATPARAM%PROP_SOLID = 5
c 
        CASE ("SOLID_ALL")         ! Compatibility with /PROP/TYPE 6/14/20/21/22/43
          MATPARAM%PROP_SOLID = 6     
c
        CASE ("SHELL_ISOTROPIC")   ! Compatibility with /PROP/TYPE 1/9/10/11/17/19/51/52
          MATPARAM%PROP_SHELL = 1          
c
        CASE ("SHELL_ORTHOTROPIC") ! Compatibility with /PROP/TYPE 9/10/11/17/19/51/52
          MATPARAM%PROP_SHELL = 2  
c
        CASE ("SHELL_COMPOSITE")   ! Compatibility with /PROP/TYPE 10/11/17/19/51/52
          MATPARAM%PROP_SHELL = 3
c
        CASE ("SHELL_ANISOTROPIC") ! Compatibility with /PROP/TYPE 10/11/16/17/19/51/52
          MATPARAM%PROP_SHELL = 4
c
        CASE ("SHELL_ALL")         ! Compatibility with /PROP/TYPE 1/9/10/11/16/17/19/51/52
          MATPARAM%PROP_SHELL = 5          
c
        CASE ("BEAM_CLASSIC")      ! Compatibility with /PROP/TYPE 3
          MATPARAM%PROP_BEAM = 1          
c
        CASE ("BEAM_INTEGRATED")   ! Compatibility with /PROP/TYPE 18
          MATPARAM%PROP_BEAM = 2  
c
        CASE ("BEAM_ALL")          ! Compatibility with /PROP/TYPE 3/18
          MATPARAM%PROP_BEAM = 3
c
        CASE ("SPRING_PREDIT")     ! Compatibility with /PROP/TYPE 36
          MATPARAM%PROP_SPRING = 1          
c
        CASE ("SPRING_MATERIAL")   ! Compatibility with /PROP/TYPE 23
          MATPARAM%PROP_SPRING = 2  
c
        CASE ("SPRING_ALL")        ! Compatibility with /PROP/TYPE 23/36
          MATPARAM%PROP_SPRING = 3   
c
        CASE ("TRUSS")             ! Compatibility with /PROP/TYPE 2
          MATPARAM%PROP_TRUSS = 1 
c
        CASE ("SPH")               ! Compatibility with /PROP/TYPE 34
          MATPARAM%PROP_SPH = 1         
c          
        CASE ("EOS")      
          MATPARAM%COMPATIBILITY_EOS = 1 ! Compatibility with option /EOS
c       
        CASE ("VISC")      
          MATPARAM%COMPATIBILITY_VISC = 1 ! Compatibility with option /VISC
        CASE DEFAULT      
          ! write error : keyword not recognized"
c---
      END SELECT
c-----------
      RETURN
      END 
