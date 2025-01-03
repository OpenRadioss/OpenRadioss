//Copyright>    OpenRadioss
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.

#include <unordered_map>
#include <cstdint>
#include <stdexcept>

//       Using C++14, we have no std::shared_mutex, so no internal locking is shown here.
//       The container should be fully populated before concurrent reads to ensure safety.

extern "C" {

void* cpp_create_umap() noexcept {
    // Create a new unordered_map on the heap and return it as a void pointer.
    auto* umap = new std::unordered_map<int,int>();
    return static_cast<void*>(umap);
}

void cpp_free_umap(void* umap_ptr) noexcept {
    // Delete the map, freeing the allocated memory.
    // Caller must ensure umap_ptr is a pointer returned by create_umap.
    auto umap = static_cast<std::unordered_map<int,int>*>(umap_ptr);
    delete umap;
}

void cpp_add_entry_umap(void* umap_ptr, int key, int value) noexcept {
    // Insert or update the value at the given key.
    // This is not thread-safe if called concurrently with reads/writes.
    auto umap = static_cast<std::unordered_map<int,int>*>(umap_ptr);
    (*umap)[key] = value;
}

int cpp_get_value_umap(void* umap_ptr, int key, int default_value) noexcept {
    // Retrieve a value from the map. If the key is not found, return default_value.
    // Safe for concurrent calls *only if* no other thread modifies the map.
    auto umap = static_cast<std::unordered_map<int,int>*>(umap_ptr);
    auto it = umap->find(key);
    return (it != umap->end()) ? it->second : default_value;
}

void cpp_reserve_umap(void* umap_ptr, std::size_t n) noexcept {
    // Reserve space for at least n elements.
    // This can improve performance of subsequent insertions by avoiding rehashing.
    // Must be called before concurrent reads, and never during them.
    auto umap = static_cast<std::unordered_map<int,int>*>(umap_ptr);
    umap->reserve(n);
}

} // extern "C"
