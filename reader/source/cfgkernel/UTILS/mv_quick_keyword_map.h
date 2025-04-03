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
#ifndef MV_QUICK_KEYWORD_MAP_H
#define MV_QUICK_KEYWORD_MAP_H


#include <UTILS/mv_string.h>
#include <UTILS/mv_iostream.h>
#include <UTILS/mv_stl_various.h>



/** @name Class for managing keywords */
//@{

/// Class for managing keywords
class MvQuickKeywordMap_t {

  friend ostream &operator<<(ostream &os,const MvQuickKeywordMap_t &qkm);

 public: /** @name Constructor and destructor */
  //@{
  /// Constructor
  MvQuickKeywordMap_t(int unknown_ikw=0,const string &unknown_skw=string("UNKNOWN"));
  //@}

 public: /** @name Before building */
  //@{
  /// Adds a keyword
  inline void addKeyword(int ikeyword,const string &skeyword) { myMap[skeyword]=ikeyword; }
  /// Builds the array
  void build();
  //@}

 public: /** @name After building */
  //@{
  /// Converts a ikeyword into a skeyword
  const string &operator[](int ikeyword) const;
  /// Converts a skeyword into a ikeyword
  int operator[](const string &skeyword) const;
  //@}

 private: // Misc
  inline bool isBuilt() const { return !(myArray.empty() && !(myMap.empty())); }
  ostream &display(ostream &os) const;

 private: // Data
  map<string,int> myMap;
  vector<string>  myArray;
  int             myOffset;
  int             myUnknownIKeyword;
  string          myUnknownSKeyword;

};

/// Output into an output stream
ostream &operator<<(ostream &os,const MvQuickKeywordMap_t &qkm);

//@}


#endif //MV_QUICK_KEYWORD_MAP_H




