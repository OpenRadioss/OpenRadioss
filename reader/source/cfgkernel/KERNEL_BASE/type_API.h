/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#ifndef TYPE_API_H
#define TYPE_API_H

#include "Structure_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/**@name MCDS Types */
/*@{*/

  /** Gets the name of an MCDS type.<br>
      Returns the error code.<br>

      @param object_type Type of object (input)
      @param type_str_p  Pointer on the corresponding string (output)

      @return 0: No error<br>
      @return 1: NULL pointer on the string<br>
  */

  /** Gets the first MCDS object type.<br>
      Returns the error code.<br>

      @param object_type_p Pointer on the result (output)

      @return 0: No error<br>
      @return 1: NULL pointer on the result<br>
  */
  int MCDS_get_first_type(obj_type_e *object_type_p);

  /** Gets the last MCDS object type.<br>
      Returns the error code.<br>

      @param object_type_p Pointer on the result (output)

      @return 0: No error<br>
      @return 1: NULL pointer on the result<br>
  */
  int MCDS_get_last_type(obj_type_e *object_type_p);

  /** Gets the next MCDS object type.<br>
      Returns the error code.<br>

      @param obj_type        Initial type (input)
      @param next_obj_type_p Pointer on the resulting type (output)

      @return 0: No error<br>
      @return 1: NULL pointer on the result<br>
      @return 2: The initial type is not an MCDS type
      @return 3: The resulting type is not an MCDS type
  */
  int MCDS_get_next_type(obj_type_e obj_type,obj_type_e *next_obj_type_p);

  /** Gets the previous MCDS object type.<br>
      Returns the error code.<br>

      @param obj_type        Pointer on the initial type (input)
      @param prev_obj_type_p Pointer on the resulting type (output)

      @return 0: No error<br>
      @return 1: NULL pointer on the result<br>
      @return 2: The initial type is not an MCDS type
      @return 3: The resulting type is not an MCDS type
  */
  int MCDS_get_previous_type(obj_type_e obj_type,obj_type_e *prev_obj_type_p);
/*@}*/

#ifdef __cplusplus
}
#endif

#endif /* TYPE_API_H */




