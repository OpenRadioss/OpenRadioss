//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 2026 Siemens
//Copyright>
//Copyright>        This program is free software: you can redistribute it and/or modify
//Copyright>        it under the terms of the GNU Affero General Public License as published by
//Copyright>        the Free Software Foundation, either version 3 of the License, or
//Copyright>        (at your option) any later version.
//Copyright>
//Copyright>        This program is distributed in the hope that it will be useful,
//Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>        GNU Affero General Public License for more details.
//Copyright>
//Copyright>        You should have received a copy of the GNU Affero General Public License
//Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>        Commercial Alternative: Simcenter Radioss Software
//Copyright>
//Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
//Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
//Copyright>        commercial version may interest you: 
//Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.

#include <unordered_map>
#include <string>
#include <utility>
#include <vector>
#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <mutex>

// ------------------------------------------------------------------------------------
// Internal storage: maps allocation site message -> cumulated bytes allocated
// ------------------------------------------------------------------------------------
static std::unordered_map<std::string, uint64_t>& get_alloc_map()
{
    static std::unordered_map<std::string, uint64_t> alloc_map;
    return alloc_map;
}

// ------------------------------------------------------------------------------------
// Internal storage: maps allocation site message -> peak (high-water mark) bytes
// ------------------------------------------------------------------------------------
static std::unordered_map<std::string, uint64_t>& get_peak_map()
{
    static std::unordered_map<std::string, uint64_t> peak_map;
    return peak_map;
}

// ------------------------------------------------------------------------------------
// Internal storage: maps live pointer address -> (allocation site name, bytes)
// One name can have many live pointers (same site called from multiple callers).
// ------------------------------------------------------------------------------------
static std::unordered_map<uintptr_t, std::pair<std::string, uint64_t>>& get_addr_map()
{
    static std::unordered_map<uintptr_t, std::pair<std::string, uint64_t>> addr_map;
    return addr_map;
}

// ------------------------------------------------------------------------------------
// Internal storage: global mutex protecting all allocation-report maps
// ------------------------------------------------------------------------------------
static std::mutex& get_report_mutex()
{
    static std::mutex report_mutex;
    return report_mutex;
}

// ------------------------------------------------------------------------------------
// Helper (mutex must already be held): remove one live allocation entry by address
// and subtract its bytes from the per-site current map.
// ------------------------------------------------------------------------------------
static void erase_addr_entry_unlocked(const uintptr_t iaddr)
{
    auto& addr_map = get_addr_map();
    auto it = addr_map.find(iaddr);
    if (it == addr_map.end()) return;

    const std::string& key = it->second.first;
    uint64_t nb            = it->second.second;

    auto& alloc_map = get_alloc_map();
    auto sit = alloc_map.find(key);
    if (sit != alloc_map.end()) {
        if (sit->second > nb)
            sit->second -= nb;
        else
            alloc_map.erase(sit);
    }

    addr_map.erase(it);
}

// ------------------------------------------------------------------------------------
// Helper: trim trailing spaces from a Fortran string
// ------------------------------------------------------------------------------------
static std::string trim(const char* str, int len)
{
    // Find the last non-space character
    int end = len - 1;
    while (end >= 0 && (str[end] == ' ' || str[end] == '\0')) {
        --end;
    }
    if (end < 0) return std::string();
    return std::string(str, end + 1);
}

// ------------------------------------------------------------------------------------
// C interface: record an allocation (legacy — no address tracking)
//   msg     : Fortran character string (not null-terminated)
//   msg_len : length of the msg string
//   nbytes  : number of bytes allocated (passed as int64)
// ------------------------------------------------------------------------------------
extern "C" void cpp_record_alloc(const char* msg, const int* msg_len, const int64_t* nbytes)
{
#ifdef MEM_REPORT
    std::lock_guard<std::mutex> lock(get_report_mutex());
    std::string key = trim(msg, *msg_len);
    if (key.empty()) return;
    uint64_t& current = get_alloc_map()[key];
    current += static_cast<uint64_t>(*nbytes);
    uint64_t& peak = get_peak_map()[key];
    if (current > peak) peak = current;
#else
    (void)msg;
    (void)msg_len;
    (void)nbytes;
#endif
}

// ------------------------------------------------------------------------------------
// C interface: record an allocation with pointer address
//   addr    : base address of the allocated array (used as unique key per live alloc)
//   msg     : Fortran character string (not null-terminated)
//   msg_len : length of the msg string
//   nbytes  : number of bytes allocated (passed as int64)
//
// A single site name can have many live pointers simultaneously (same site called
// from multiple callers or in a loop), so we track each allocation individually
// via its address.
// ------------------------------------------------------------------------------------
extern "C" void cpp_record_alloc_addr(const void* addr, const char* msg, const int* msg_len, const int64_t* nbytes)
{
#ifdef MEM_REPORT
    std::lock_guard<std::mutex> lock(get_report_mutex());
    std::string key = trim(msg, *msg_len);
    if (key.empty()) return;
    uintptr_t iaddr = reinterpret_cast<uintptr_t>(addr);
    uint64_t nb = static_cast<uint64_t>(*nbytes);

    get_addr_map()[iaddr] = {key, nb};

    uint64_t& current = get_alloc_map()[key];
    current += nb;
    uint64_t& peak = get_peak_map()[key];
    if (current > peak) peak = current;
#else
    (void)addr;
    (void)msg;
    (void)msg_len;
    (void)nbytes;
#endif
}

// ------------------------------------------------------------------------------------
// C interface: record a deallocation by pointer address
//   addr    : base address of the array being freed
//
// Looks up the address in the per-allocation map to find the site name and byte
// count, then decrements the per-site counter and removes the address entry.
// Safe to call for addresses not tracked by cpp_record_alloc_addr (no-op).
// ------------------------------------------------------------------------------------
extern "C" void cpp_record_dealloc_addr(const void* addr)
{
#ifdef MEM_REPORT
    std::lock_guard<std::mutex> lock(get_report_mutex());
    const uintptr_t iaddr = reinterpret_cast<uintptr_t>(addr);
    erase_addr_entry_unlocked(iaddr);
#else
    (void)addr;
#endif
}

// ------------------------------------------------------------------------------------
// C interface: check whether an address is currently tracked as a live allocation
// ------------------------------------------------------------------------------------
extern "C" int cpp_is_alloc_tracked_addr(const void* addr)
{
#ifdef MEM_REPORT
    std::lock_guard<std::mutex> lock(get_report_mutex());
    const uintptr_t iaddr = reinterpret_cast<uintptr_t>(addr);
    return (get_addr_map().find(iaddr) != get_addr_map().end()) ? 1 : 0;
#else
    (void)addr;
    return 0;
#endif
}

// ------------------------------------------------------------------------------------
// C interface: transfer tracking from source address to destination address.
// Used by Fortran move_alloc wrappers when a tracked allocation changes owner.
// ------------------------------------------------------------------------------------
extern "C" void cpp_record_move_alloc_addr(const void* src_addr, const void* dst_addr)
{
#ifdef MEM_REPORT
    std::lock_guard<std::mutex> lock(get_report_mutex());
    const uintptr_t src = reinterpret_cast<uintptr_t>(src_addr);
    const uintptr_t dst = reinterpret_cast<uintptr_t>(dst_addr);
    if (src == dst) return;

    auto& addr_map = get_addr_map();
    auto sit = addr_map.find(src);
    if (sit == addr_map.end()) return;

    // Destination may have held a previous tracked allocation. Remove it first.
    erase_addr_entry_unlocked(dst);

    addr_map[dst] = sit->second;
    addr_map.erase(sit);
#else
    (void)src_addr;
    (void)dst_addr;
#endif
}

// ------------------------------------------------------------------------------------
// C interface: record a deallocation (legacy — name+bytes, no address map)
// ------------------------------------------------------------------------------------
extern "C" void cpp_record_dealloc(const char* msg, const int* msg_len, const int64_t* nbytes)
{
#ifdef MEM_REPORT
    std::lock_guard<std::mutex> lock(get_report_mutex());
    std::string key = trim(msg, *msg_len);
    if (key.empty()) return;
    auto& alloc_map = get_alloc_map();
    auto it = alloc_map.find(key);
    if (it != alloc_map.end()) {
        uint64_t to_subtract = static_cast<uint64_t>(*nbytes);
        if (it->second > to_subtract)
            it->second -= to_subtract;
        else
            alloc_map.erase(it);
    }
#else
    (void)msg;
    (void)msg_len;
    (void)nbytes;
#endif
}

// ------------------------------------------------------------------------------------
// C interface: print a report of the top 100 allocation sites by cumulated size
// ------------------------------------------------------------------------------------
extern "C" void cpp_print_alloc_report()
{
#ifdef MEM_REPORT
    std::lock_guard<std::mutex> lock(get_report_mutex());
    auto& alloc_map = get_alloc_map();
    auto& peak_map = get_peak_map();

    if (alloc_map.empty() && peak_map.empty()) {
        std::printf("\n === ALLOCATION REPORT: no allocations recorded ===\n\n");
        return;
    }

    // Merge keys from both maps (peak_map may have entries removed from alloc_map)
    struct entry_t {
        std::string key;
        uint64_t current;
        uint64_t peak;
    };
    std::vector<entry_t> entries;
    entries.reserve(peak_map.size());
    for (auto& kv : peak_map) {
        uint64_t cur = 0;
        auto it = alloc_map.find(kv.first);
        if (it != alloc_map.end()) cur = it->second;
        entries.push_back({kv.first, cur, kv.second});
    }

    // Sort descending by peak bytes
    std::sort(entries.begin(), entries.end(),
              [](const entry_t& a, const entry_t& b) {
                  return a.peak > b.peak;
              });

    // Print header
    std::printf("\n");
    std::printf(" ====================================================================================================\n");
    std::printf("               MEMORY ALLOCATION REPORT - TOP 100 ALLOCATION SITES (>= 1 MB peak)\n");
    std::printf(" ====================================================================================================\n");
    std::printf(" %4s  %-55s %12s %12s\n", "Rank", "Allocation site (msg)", "Current (MB)", "Peak (MB)");
    std::printf(" %-4s  %-55s %12s %12s\n", "----",
                "-------------------------------------------------------",
                "------------", "------------");

    // Print up to 100 entries, skip those with peak < 1 MB
    static const uint64_t ONE_MB = 1024UL * 1024UL;
    size_t rank = 0;
    uint64_t total_current = 0;
    uint64_t total_peak = 0;
    for (size_t i = 0; i < entries.size() && rank < 100; ++i) {
        uint64_t peak_mb = entries[i].peak / ONE_MB;
        if (peak_mb < 1) break;  // sorted descending, so all remaining are < 1 MB
        ++rank;
        std::printf(" %4zu  %-55s %12lu %12lu\n", rank, entries[i].key.c_str(),
                    static_cast<unsigned long>(entries[i].current / ONE_MB),
                    static_cast<unsigned long>(peak_mb));
        total_current += entries[i].current;
        total_peak += entries[i].peak;
    }

    // Print totals
    uint64_t grand_current = 0;
    uint64_t grand_peak = 0;
    for (auto& e : entries) {
        grand_current += e.current;
        grand_peak += e.peak;
    }

    std::printf(" %-4s  %-55s %12s %12s\n", "----",
                "-------------------------------------------------------",
                "------------", "------------");
    std::printf(" %4s  %-55s %12lu %12lu\n", "", "Displayed total (MB)",
                static_cast<unsigned long>(total_current / ONE_MB),
                static_cast<unsigned long>(total_peak / ONE_MB));
    std::printf(" %4s  %-55s %12lu %12lu\n", "", "Grand total (MB)",
                static_cast<unsigned long>(grand_current / ONE_MB),
                static_cast<unsigned long>(grand_peak / ONE_MB));
    std::printf(" ====================================================================================================\n");
    std::printf("\n");
#endif
}
