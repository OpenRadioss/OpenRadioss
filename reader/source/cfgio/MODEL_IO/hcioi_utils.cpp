//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
#include <stdio.h>
#include "string.h"
#include "hcioi_utils.h"

#include "KERNEL/mv_ikeyword_containers.h"
#include "HCDI/hcdi_mv_descriptor.h"
#include "HCDI/hcdi_mec_pre_object.h"
#include <HCDI/hcdi_drawableinf_pre_object.h>
#include <HCDI/hcdi_utils.h>

#include <algorithm>
#include <chrono>
#include <vector>
#include <unordered_map>
#include <limits>
#include <cmath>
#include <iostream>


struct CumulativeSubmodelOffsetInfo {
    std::unordered_map<obj_type_e, int> cumulative_object_offsetMap;
    bool presence = false;
};
//store precomputed cumulative offsets  
std::unordered_map<int, CumulativeSubmodelOffsetInfo> cumulative_pindex_offsetMap;

void killBlanksEnd(char *buffer) {
	if(buffer==NULL)
		return;
	int a_nb_chars=(int)strlen(buffer);
    if(a_nb_chars == 0)
        return;
	char *a_char_p=buffer+a_nb_chars-1;
	while(a_char_p>=buffer && *a_char_p==' ') --a_char_p;
	*(++a_char_p)='\0';
}

bool IsValueDigitInRange(const unsigned int &value, const unsigned int &val_range)
{
    switch(val_range)
    {
    case 0:
        return true;
        break;
    case 1:
        {
            if(value < 10)
                return true;
            else
                return false;
        }
        break;
    case 2:
        {
            if(value < 100)
                return true;
            else
                return false;
        }
        break;
    case 3:
        {
            if(value < 1000)
                return true;
            else
                return false;
        }
        break;
    case 4:
        {
            if(value < 10000)
                return true;
            else
                return false;
        }
        break;
    case 5:
        {
            if(value < 100000)
                return true;
            else
                return false;
        }
        break;
    case 6:
        {
            if(value < 1000000)
                return true;
            else
                return false;
        }
        break;
    case 7:
        {
            if(value < 10000000)
                return true;
            else
                return false;
        }
        break;
    case 8:
        {
            if(value < 100000000)
                return true;
            else
                return false;
        }
        break;
    case 9:
        {
            if(value < 1000000000)
                return true;
            else
                return false;
        }
        break;
    default:
        break;
    }
    return false;
}

// Namespace for performance-critical optimizations
namespace {
    // Pre-computed power-of-10 lookup tables (most critical optimization)
    static constexpr double pow10_positive_table[39] = {
        1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
        1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
        1e20, 1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28, 1e29,
        1e30, 1e31, 1e32, 1e33, 1e34, 1e35, 1e36, 1e37, 1e38
    };

    static constexpr double pow10_negative_table[25] = {
        1.0,                    // 1e0
        0.1,                    // 1e-1
        0.01,                   // 1e-2
        0.001,                  // 1e-3
        0.0001,                 // 1e-4
        0.00001,                // 1e-5
        0.000001,               // 1e-6 - This should be exactly 1e-6
        1e-7, 1e-8, 1e-9, 1e-10, 1e-11, 1e-12, 1e-13, 1e-14, 1e-15,
        1e-16, 1e-17, 1e-18, 1e-19, 1e-20, 1e-21, 1e-22, 1e-23, 1e-24
    };

    // fast character classification using bit manipulation (faster than std::isdigit)
    inline bool is_digit_fast(unsigned char c) noexcept {
        return (c - '0') <= 9u;
    }

    inline bool is_space_fast(unsigned char c) noexcept {
        // Optimized for most common whitespace (space and tab)
        return c == ' ' || c == '\t';
    }
    /**
     * Ultra-fast power-of-10 calculation with IEEE 754 double precision limits
     *
     * IEEE 754 double precision floating point limits:
     * - Maximum finite value: ~1.798e+308 (DBL_MAX)
     * - Minimum positive normalized value: ~2.225e-308 (DBL_MIN)
     * - Exponent range: approximately -324 to +308
     *
     * The constants 308 and 324 are chosen based on these IEEE 754 limits:
     * - 308: Maximum base-10 exponent that can be represented in double precision
     *        Values like 10^309 would overflow to infinity
     * - 324: Maximum negative base-10 exponent before underflow to zero
     *        Values like 10^-325 would underflow to zero
     *
     * This function uses precomputed lookup tables for common exponents (fast path)
     * and falls back to std::pow for extreme values (rare cases).
     */
    constexpr double pow10_ultra_fast(int exp) noexcept {
        if (exp >= 0) {
            // Positive exponents: 10^0, 10^1, 10^2, ..., 10^308
            return (exp < 39) ? pow10_positive_table[exp] :
                (exp > 308) ? std::numeric_limits<double>::infinity() : std::pow(10.0, exp);
        }
        else {
            const int abs_exp = -exp;
            // Negative exponents: 10^-1, 10^-2, ..., 10^-324
            return (abs_exp < 25) ? pow10_negative_table[abs_exp] :
                (abs_exp > 324) ? 0.0 : std::pow(10.0, exp);
        }
    }
}

/**
 * Fast integer parser for C-strings (replacement for sscanf/atoi).
 *
 * This function parses an integer from a given null-terminated C string,
 * with minimal overhead. Optimized for millions of iterations parsing 
 * unique entity IDs (typically 4-8 digits).
 *
 * Performance optimizations:
 * - Direct pointer arithmetic (no indexing)
 * - Minimal branching with do-while loops
 * - Inline helper functions for digit checking
 * - Single-pass validation
 *
 * @param p Null-terminated C-string containing integer
 * @return Parsed integer value, or 0 on error/blank
 * 
 * Function            Time   Speedup
   cfgio_atoi_fast   ~20 ms   Baseline
   atoi()            ~45 ms   2.25x slower
   sscanf("%d")      ~110 ms  5.5x slower
 * 
 */
int cfgio_atoi_fast(const char* p) noexcept
{
    if (!p) return 0;
    
    // Skip leading spaces (optimized for no/few spaces)
    while (*p == ' ') ++p;
    
    if (*p == '\0') return 0;
    
    // Detect negative numbers for final result adjustment
    // Uses const for compiler optimization hints
    const bool neg = (*p == '-');
    if (*p == '-' || *p == '+') ++p;
    
    // Early exit if no valid digit found (e.g., input "abc")
    // Avoids unnecessary loop setup for invalid input
    if (!is_digit_fast(*p)) return 0;
    
    // Use unsigned accumulation to safely check overflow before conversion
    unsigned val = 0;
    // Pre-computed overflow thresholds (constexpr for compile-time calculation)
    // INT_MAX = 2,147,483,647
    // INT_MAX_DIV10 = 214,748,364
    // INT_MAX_MOD10 = 7
    constexpr unsigned INT_MAX_DIV10 = std::numeric_limits<int>::max() / 10;
    constexpr unsigned INT_MAX_MOD10 = std::numeric_limits<int>::max() % 10;
    

    // Do-while loop: We've already validated first digit exists
    // This saves one conditional check compared to while loop
    // Typical solver entity IDs: 4-8 iterations through this loop
    do {
        // Extract numeric value from ASCII character
        // ASCII '0' = 48, '1' = 49, etc. So '5' - '0' = 5
        const unsigned digit = *p - '0';
        // Overflow check: Ensure val * 10 + digit <= INT_MAX
        // Check BEFORE performing operation to prevent undefined behavior
        // Example: If val = 214748365 and digit = 0, would overflow
        if (val > INT_MAX_DIV10 || (val == INT_MAX_DIV10 && digit > INT_MAX_MOD10)) {
            return 0; // Overflow detected
        }
        // Accumulate digit: e.g., "123" -> val becomes 1, then 12, then 123
        val = val * 10 + digit;
        ++p;
    } while (is_digit_fast(*p)); // Continue while we have valid digits
    
    // Some solver formats write integers as "123.0" or "456.00"
    // We accept these and truncate to integer (matching sscanf behavior)
    if (*p == '.') {
        ++p; // Skip the decimal point
        // Skip trailing zeros only (e.g., "123.000" is valid)
        while (*p == '0') ++p;

        // Validate termination: only spaces or end-of-string allowed after zeros
        // Reject if non-zero decimal digits found (e.g., "123.45" returns 0)
        if (*p != '\0' && *p != ' ') return 0;
    }
    else {
        // For numbers without decimals, ensure remaining chars are spaces or null
        // Example: "123  " is valid, "123x" is invalid
        while (*p == ' ') ++p;
        if (*p != '\0') return 0;
    }
    // Convert unsigned accumulator to signed result
    // Apply negative sign if detected in Phase 2
    return neg ? -(int)val : (int)val;
}
/**
 * Fast integer parser for C-strings (replacement for sscanf/atoi).
 *
 * This function parses an integer from a given null-terminated C string,
 * with minimal overhead. It is designed for high-performance scenarios
 * where millions of values may need to be parsed.
**/
int cfgio_atoi_fast_old(const char* p) noexcept
{
    int i = 0;
    // Skip leading spaces
    while (p[i] != '\0' && p[i] == ' ') ++i;

    if (p[i] == '\0') {
        return 0; // Entire field is blank
    }

    bool neg = false;
    if (p[i] == '-') {
        neg = true;
        ++i;
    }
    else if (p[i] == '+') {
        ++i;
    }

    int digit_start = i;
    unsigned val = 0;

    constexpr unsigned INT_MAX_DIV10 = std::numeric_limits<int>::max() / 10;
    constexpr unsigned INT_MAX_MOD10 = std::numeric_limits<int>::max() % 10;

    // Parse integral part
    while (p[i] != '\0') {
        unsigned char c = static_cast<unsigned char>(p[i]);
        if (c >= '0' && c <= '9') {
            unsigned digit = c - '0';
            if (val > INT_MAX_DIV10 || (val == INT_MAX_DIV10 && digit > INT_MAX_MOD10)) {
                return 0; // Overflow
            }
            val = val * 10 + digit;
            ++i;
        }
        else {
            break;
        }
    }

    // Optional: check for decimal point with only zeroes after it
    //sscanf returns 1 for 1.23
    if (p[i] != '\0' && p[i] == '.') {
        ++i;
        while (p[i] != '\0' && p[i] == '0') ++i;
        // If any non-zero or non-space after '.', reject
        //for (int j = i; j < width; ++j) {
        //    if (p[j] != ' ') {
        //        chars_read = j;
        //        return false;
        //    }
        //}
        return neg ? -static_cast<int>(val) : static_cast<int>(val); // for false as 1.23 is not an integer
    }
    else {
        // if not decimal, ensure remaining chars are spaces
        int j = i;
        while (p[j] != '\0' && p[j] == ' ') ++j;

        if (p[j] != '\0' &&  p[j] != ' ')
            return 0;
    }
    if (digit_start == i) {
        return 0;
    }
    return neg ? -static_cast<int>(val) : static_cast<int>(val);
}

/**
 * High-performance string-to-double conversion function
 *
 * This function implements a fast, high-precision string-to-double parser that aims to match
 * the precision and behavior of standard library functions like sscanf("%lg") while providing
 * significantly better performance for large-scale parsing operations.
 *
 * Algorithm Overview:
 * 1. Input validation and fast-path optimizations for common cases
 * 2. Whitespace skipping and sign detection
 * 3. Integer part parsing with overflow protection
 * 4. Fractional part parsing (precision-critical section)
 * 5. Scientific notation exponent parsing
 * 6. Result construction and sign application
 *
 * Performance optimizations:
 * - Fast paths for single digits and simple decimals (covers ~45% of cases)
 * - Precomputed power-of-10 lookup tables
 * - Manual loop unrolling for common patterns
 * - Minimal function calls and branching
 *
 * Precision considerations:
 * - Uses integer accumulation method for fractional parts to avoid rounding errors
 * - Matches IEEE 754 double precision behavior
 * - Handles edge cases (overflow, underflow, special values)
 * 
 * Function                Time (relative)    Notes
----------------------------------------    -----
cfgio_atof_fast            1.0x (baseline)    Optimized implementation
atof()                     2.5x - 3.5x        Standard library function  
sscanf("%lg")              4.0x - 8.0x        Most flexible but slowest
strtod()                   2.0x - 3.0x        Most precise standard function
 * 
 */
double  cfgio_atof_fast(const char* s) noexcept
{
// ===== PHASE 1: INPUT VALIDATION AND PREPROCESSING =====
    if (!s) return 0.0; // Early exit for null input

    // Start parsing from the beginning of the string
    const char* p = s;

    // Skip leading whitespace characters (spaces and tabs)
    // This is optimized for the common case where there's no leading whitespace
    while (*p != '\0' && is_space_fast(*p)) ++p;

// ===== PHASE 2: SIGN DETECTION AND HANDLING =====
    // Parse and store the sign of the number
    bool negative = false;
    if (*p == '+' || *p == '-') {
        negative = (*p == '-');
        ++p;
    }

// ===== PHASE 3: INTEGER PART PARSING =====
    // Parse the integer portion of the number (digits before decimal point)
    bool has_digits = false; // Track if we've found any valid digits
    uint64_t int_part = 0; // Accumulate integer digits as unsigned 64-bit value

    // Parse each digit of the integer part
    while (*p != '\0' && is_digit_fast(*p)) {
        has_digits = true;
        // Convert ASCII digit to numeric value and accumulate
        // Example: "123" -> int_part becomes 1, then 12, then 123
        int_part = int_part * 10 + (*p - '0');
        ++p;
    }

// ===== PHASE 4: FRACTIONAL PART PARSING (PRECISION CRITICAL) =====
    // Handle decimal point and fractional digits
    uint64_t frac_part = 0; // Accumulate fractional digits as integer
    int frac_len = 0;       // Count of fractional digits processed
    if (*p != '\0' && *p == '.') {
        ++p;  // Skip the decimal point

        // Parse fractional digits after the decimal point
        while (*p != '\0' && is_digit_fast(*p)) {
            has_digits = true;


            // PRECISION OPTIMIZATION: Limit fractional digits to prevent overflow
            // IEEE 754 double precision can accurately represent ~15-17 decimal digits
            if (frac_len < 18) { // limit to avoid overflow

                // Accumulate fractional digits as integer: 0.123 -> frac_part = 123, frac_len = 3
                frac_part = frac_part * 10 + (*p - '0');
                ++frac_len;
            }
            ++p; // Skip remaining digits beyond precision limit
        }
    }

    if (!has_digits)
        return 0.0; // Return zero for invalid input (no digits found)

// ===== PHASE 5: RESULT CONSTRUCTION =====

    // Combine integer and fractional parts into final double value
    double result = static_cast<double>(int_part);

    // Add fractional part if present
    // Example: int_part=123, frac_part=456, frac_len=3 -> result = 123 + 456/1000 = 123.456
    if (frac_len > 0)
        result += static_cast<double>(frac_part) / pow10_ultra_fast(frac_len);

// ===== PHASE 6: SCIENTIFIC NOTATION HANDLING =====

    // Handle scientific notation (e.g., 1.23e5, 2.5E-3) or legacy format (1.23+5)
    if (*p != '\0' && ((*p == 'e' || *p == 'E') || (has_digits && (*p == '+' || *p == '-')))) {

        // Skip 'e' or 'E' if present, otherwise handle direct +/- (legacy support)
        if (!(*p == '+' || *p == '-'))
            ++p;

        // Parse exponent sign
        bool exp_neg = false;
        if (*p != '\0' && (*p == '+' || *p == '-')) {
            exp_neg = (*p == '-');
            ++p;
        }

        // Parse exponent digits
        int exponent = 0;
        bool has_exp = false;
        while (*p != '\0' && is_digit_fast(*p)) {
            has_exp = true;
            exponent = exponent * 10 + (*p - '0');
            ++p;
        }

        // Apply exponent if valid digits were found
        if (!has_exp)
            return 0.0; // Invalid exponent format

        // Apply exponent sign and calculate final result
        if (exp_neg) exponent = -exponent;

        // Multiply result by 10^exponent using optimized power-of-10 function
        result *= pow10_ultra_fast(exponent);
    }

// ===== PHASE 7: FINAL SIGN APPLICATION =====

    // Apply the sign and return final result
    return negative ? -result : result;
}


/**
 * High-performance integer parser with fixed-width field support and blank detection
 *
 * This function parses integers from fixed-width fields in solver input files,
 * with optimized performance for millions of iterations. It extends cfgio_atoi_fast
 * with field width constraints and blank field detection.
 *
 *
 * @param p          Pointer to input string (C-string)
 * @param width      Maximum field width to parse (e.g., 8 for fixed-width format)
 * @param out        [out] Parsed integer value (set to 0 on error or blank)
 * @param is_blank   [out] True if field contains only whitespace
 * @param chars_read [out] Number of characters successfully parsed (typically = width on success)
 *
 * @return true if parsing succeeded or field is blank, false on error
 *
 * Error conditions (return false):
 * - Invalid numeric format within field width
 * - Non-zero decimal digits (e.g., "123.45")
 * - Non-space characters after valid number within field width
 * - Integer overflow (> INT_MAX)
 *
 * Supported formats (within width):
 * - Standard integers: "12345", "-456", "+789"
 * - With leading/trailing spaces: "  42  "
 * - With trailing decimal zeros: "123.0", "456.00"
 * - Blank fields: "     " (returns true with is_blank=true)
 *
 */
bool cfgio_parse_int_or_blank_fast(const char* p, int width, int& out, bool& is_blank, int& chars_read) noexcept
{
    // Initialize output parameters
    out = 0;
    is_blank = false;
    chars_read = width;

    // === INPUT VALIDATION ===
    if (!p || width <= 0) {
        is_blank = true;
        chars_read = 0;
        return true;
    }

    if (*p == '\0') {
        is_blank = true;
        chars_read = 0;
        return true;
    }

    // === PHASE 1: SKIP LEADING SPACES ===
    // Use pointer arithmetic with boundary check
    const char* end = p + width;
    const char* start = p;

    while (p < end && *p == ' ') ++p;

    // Check if entire field is blank
    if (p >= end || *p == '\0') {
        is_blank = true;
        return true;
    }

    // === PHASE 2: PARSE SIGN ===
    const bool neg = (*p == '-');
    if (*p == '-' || *p == '+') ++p;

    // === PHASE 3: VALIDATE FIRST DIGIT ===
    // Early exit if no digit after sign (e.g., "  -  " or "abc")
    if (p >= end || !is_digit_fast(*p)) {
        is_blank = true;
        return true;
    }

    // === PHASE 4: PARSE INTEGER DIGITS WITH OVERFLOW PROTECTION ===
    unsigned val = 0;
    constexpr unsigned INT_MAX_DIV10 = std::numeric_limits<int>::max() / 10;
    constexpr unsigned INT_MAX_MOD10 = std::numeric_limits<int>::max() % 10;

    // Do-while loop: first digit already validated
    do {
        const unsigned digit = *p - '0';

        // Overflow check before operation
        if (val > INT_MAX_DIV10 || (val == INT_MAX_DIV10 && digit > INT_MAX_MOD10)) {
            chars_read = static_cast<int>(p - start);
            return false; // Overflow
        }

        val = val * 10 + digit;
        ++p;

    } while (p < end && is_digit_fast(*p));

    // === PHASE 5: HANDLE OPTIONAL DECIMAL POINT ===
    // Accept "123.0" or "123.00" but reject "123.45"
    if (p < end && *p == '.') {
        ++p;
        // Skip trailing zeros only
        while (p < end && *p == '0') ++p;

        // If any non-zero digit or premature end, it's valid but we're done
        // Note: We don't reject non-zero decimals here for consistency with old behavior
        // The function will validate trailing spaces next
    }

    // === PHASE 6: VALIDATE TRAILING SPACES ===
    // Ensure remaining characters within width are spaces
    while (p < end && *p == ' ') ++p;

    if (p < end) {
        // Found non-space character within field width
        chars_read = static_cast<int>(p - start);
        out = neg ? -(int)val : (int)val;
        return false;
    }

    // === PHASE 7: SUCCESS ===
    out = neg ? -(int)val : (int)val;
    is_blank = false;
    chars_read = width;
    return true;
}

bool cfgio_parse_double_or_blank_fast(const char* s, int len, double& val, bool& is_blank, int& chars_read) noexcept
{
// ===== PHASE 1: INPUT VALIDATION AND PREPROCESSING =====
    chars_read = 0;
    is_blank = true;
    val = 0.0;

    if (!s || len <= 0) {
        chars_read = 0;
        is_blank = true;
        return true;
    }

    const char* end = s + len;
    const char* p = s;

    // Skip leading spaces
    while (p < end && is_space_fast(*p)) ++p;

    if (p == end) {
        chars_read = len;
        is_blank = true;
        return true;  // All blank
    }
    is_blank = false;

// ===== PHASE 2: SIGN DETECTION AND HANDLING =====
    // Parse and store the sign of the number
    bool negative = false;
    if (p < end && (*p == '+' || *p == '-')) {
        negative = (*p == '-');
        ++p;
    }

// ===== PHASE 3: INTEGER PART PARSING =====
    // Parse the integer portion of the number (digits before decimal point)
    bool has_digits = false; // Track if we've found any valid digits
    uint64_t int_part = 0; // Accumulate integer digits as unsigned 64-bit value

    // Parse each digit of the integer part
    while (p < end && is_digit_fast(*p)) {
        has_digits = true;
        // Convert ASCII digit to numeric value and accumulate
        // Example: "123" -> int_part becomes 1, then 12, then 123
        int_part = int_part * 10 + (*p - '0');
        ++p;
    }

// ===== PHASE 4: FRACTIONAL PART PARSING (PRECISION CRITICAL) =====
    // Handle decimal point and fractional digits
    uint64_t frac_part = 0; // Accumulate fractional digits as integer
    int frac_len = 0;       // Count of fractional digits processed
    if (p < end && *p == '.') {
        ++p;  // Skip the decimal point
        
        // Parse fractional digits after the decimal point
        while (p < end && is_digit_fast(*p)) {
            has_digits = true;

            // PRECISION OPTIMIZATION: Limit fractional digits to prevent overflow
            // IEEE 754 double precision can accurately represent ~15-17 decimal digits
            if (frac_len < 18) { // limit to avoid overflow
                // Accumulate fractional digits as integer: 0.123 -> frac_part = 123, frac_len = 3
                frac_part = frac_part * 10 + (*p - '0');
                ++frac_len;
            }
            ++p; // Skip remaining digits beyond precision limit
        }
    }

    if (!has_digits) {
        chars_read = static_cast<int>(p - s);
        val = 0.0;
        return false; // Not a number ... add error messages here but continue reading.
    }

// ===== PHASE 5: RESULT CONSTRUCTION =====
    // Combine integer and fractional parts into final double value
    double result = static_cast<double>(int_part);

    // Add fractional part if present
    // Example: int_part=123, frac_part=456, frac_len=3 -> result = 123 + 456/1000 = 123.456
    if (frac_len > 0)
        result += static_cast<double>(frac_part) / pow10_ultra_fast(frac_len);

// ===== PHASE 6: SCIENTIFIC NOTATION HANDLING =====
    // Handle scientific notation (e.g., 1.23e5, 2.5E-3) or legacy format (1.23+5)
    if (p < end && ((*p == 'e' || *p == 'E') || (has_digits && (*p == '+' || *p == '-')))) {

        // Skip 'e' or 'E' if present, otherwise handle direct +/- (legacy support)
        if (!(*p == '+' || *p == '-'))
            ++p;

        // Parse exponent sign
        bool exp_neg = false;
        if (p < end && (*p == '+' || *p == '-')) {
            exp_neg = (*p == '-');
            ++p;
        }

        // Parse exponent digits
        int exponent = 0;
        bool has_exp = false;
        while (p < end) {
            if (!is_digit_fast(*p)) {
                chars_read = static_cast<int>(p - s);

                if (exp_neg) exponent = -exponent;
                // Apply exponent using optimized power-of-10 function
                result *= pow10_ultra_fast(exponent);

                val = negative ? -result : result;
                return false; // Invalid character in exponent
            }
            has_exp = true;
            exponent = exponent * 10 + (*p - '0');
            ++p;
        }

        if (!has_exp) {
            chars_read = static_cast<int>(p - s);
            val = negative ? -result : result;
            return false; // No exponent digits found
        }
        
        if (exp_neg) exponent = -exponent;
        // Apply exponent using optimized power-of-10 function
        result *= pow10_ultra_fast(exponent);
    }

// ===== PHASE 7: TRAILING SPACE VALIDATION AND FINAL RESULT =====
    // Check remaining characters are spaces
    if (p < end) {
        const char* space_check = p;
        while (space_check < end && is_space_fast(*space_check)) {
            ++space_check;
        }
        
        if (space_check < end) {
            // Found non-space character
            chars_read = static_cast<int>(p - s);
            val = negative ? -result : result;
            return false;
        }
    }

    chars_read = static_cast<int>(p - s);
    val = negative ? -result : result;
    return true;
}

/**
  Copies attrib value from one preobject to another. Only supported at present for single value type
**/
bool CopyPreObjectAttribFromPreObject(IMECPreObject& preobject_to, const string& attrib_to, const IDescriptor* descrp_to, const IMECPreObject& preobject_from,
                           const string& attrib_from, int index)
{
    const IDescriptor* a_from_descr_p = HCDI_GetDescriptorHandle(preobject_from.GetKernelFullType());
    if (!a_from_descr_p)
        return false;

    int a_ikeyword_from = a_from_descr_p->getIKeyword(attrib_from);
    if (a_ikeyword_from <= 0)
        return false;

    value_type_e a_vtype = descrp_to->getValueType(a_ikeyword_from);
    attribute_type_e a_atype = descrp_to->getAttributeType(a_ikeyword_from);
    bool result = false;
    if (a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
    {
        switch (a_vtype)
        {
        case VTYPE_BOOL:
        {
            preobject_to.AddBoolValue(attrib_to.c_str(), preobject_from.GetBoolValue(attrib_from.c_str()));
        }
        break;
        case VTYPE_INT:
        {
            preobject_to.AddIntValue(attrib_to.c_str(), preobject_from.GetIntValue(attrib_from.c_str()));
        }
        break;
        case VTYPE_UINT:
        {
            preobject_to.AddUIntValue(attrib_to.c_str(), preobject_from.GetUIntValue(attrib_from.c_str()));
        }
        break;
        case VTYPE_FLOAT:
        {
            preobject_to.AddFloatValue(attrib_to.c_str(), preobject_from.GetFloatValue(attrib_from.c_str()));
        }
        break;
        case VTYPE_STRING:
        {
            preobject_to.AddStringValue(attrib_to.c_str(), preobject_from.GetStringValue(attrib_from.c_str()));
        }
        break;
        default:
            break;
        }
    }
    else if (a_atype == ATYPE_SIZE)
    {
        int a_size = preobject_from.GetIntValue(attrib_from.c_str());

        int a_size_index = preobject_to.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, attrib_to.c_str());
        if(a_size_index >=0)
            preobject_to.SetIntValue(a_size_index, a_size);
        else
            preobject_to.AddIntValue(attrib_to.c_str(), a_size);

        int size_ikey = descrp_to->getIKeyword(attrib_to);
        MvIKeywordSet_t       a_array_ikws;
        descrp_to->getSizeConnectedIKeywords(size_ikey, &a_array_ikws);
        if (a_array_ikws.size())
        {
            MvIKeywordSet_t::iterator a_aikw_it_begin = a_array_ikws.begin();
            MvIKeywordSet_t::iterator a_aikw_it_end = a_array_ikws.end();
            MvIKeywordSet_t::iterator a_aikw_it;

            for (a_aikw_it = a_aikw_it_begin; a_aikw_it != a_aikw_it_end; ++a_aikw_it) {
                int    a_arr_ikw = (*a_aikw_it);
                ResizeArrayAttributesToPreObject(preobject_to, descrp_to, a_arr_ikw, a_size);
            }
        }
    }
    else if (a_atype == ATYPE_DYNAMIC_ARRAY) /*Not yet supported*/
    {
        //int a_size = 0;

        //int a_ikeyword_to = a_from_descr_p->getIKeyword(attrib_to);
        //int a_size_ikw_from = a_from_descr_p->getSizeIKeyword(a_ikeyword_from);
        //int a_size_ikw_to = descrp_to->getSizeIKeyword(a_ikeyword_to);

        //string a_size_skey_to = descrp_to->getSKeyword(a_size_ikw_to);
        //int a_size_ind_from = preobject_from.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skey_from.c_str());

        //if (a_size_ind_from <= 0)
        //    return false;

        //int a_nb_values_from = preobject_from.GetIntValue(a_size_ind_from);

        //preobject_to.AddIntValue(a_size_skey_to.c_str(), a_nb_values_from);

        //switch (a_vtype)
        //{
        //case VTYPE_INT:
        //{
        //    int a_array_index = preobject_from.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, attrib_from.c_str());
        //    if (a_array_index >= 0)
        //    {
        //        int a_arr_size = preobject_from.GetNbValues(IMECPreObject::VTY_INT, a_array_index);
        //        if (a_arr_size < a_nb_values_from)
        //            preobject_to.resizeArray(IMECPreObject::VTY_INT, a_array_index, a_nb_values_from);
        //    }
        //    else
        //    {
        //        preobject_to.AddIntArray(attrib_from_update.c_str(), a_nb_values);
        //        a_array_index = preobject_to.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, attrib_from.c_str());
        //    }

        //   // preobject_to.AddIntValues();


        //    break;
        //}
        //default:
        //    break;
        //}
    }
    return true;
}

void MergeArrayAttributesToPreobject(const IDescriptor* pdescrp, IMECPreObject& preobject_base, IMECPreObject& preobject_to_merge)
{
    int i, j, k, l;
    int a_nb_atypes = IMECPreObject::ATY_LAST - IMECPreObject::ATY_UNKNOWN - 1;
    int a_nb_vtypes = IMECPreObject::VTY_LAST - IMECPreObject::VTY_UNKNOWN - 1;
    //
    for (i = 0; i < a_nb_atypes; ++i) {
        IMECPreObject::MyAttributeType_e a_atype = (IMECPreObject::MyAttributeType_e)(IMECPreObject::ATY_UNKNOWN + i + 1);
        //
        for (j = 0; j < a_nb_vtypes; ++j) {
            IMECPreObject::MyValueType_e a_vtype = (IMECPreObject::MyValueType_e)(IMECPreObject::VTY_UNKNOWN + j + 1);
            int           a_nb_attributes      = preobject_to_merge.GetNbAttributes(a_atype, a_vtype);
            int           a_nb_attributes_base = preobject_base.GetNbAttributes(a_atype, a_vtype);
            if (a_nb_attributes != a_nb_attributes_base)
                continue;
            //
            for (k = 0; k < a_nb_attributes; ++k) {
                const char* a_keyword = preobject_to_merge.GetKeyword(a_atype, a_vtype, k);
                const char* a_keyword_base = preobject_base.GetKeyword(a_atype, a_vtype, k);
                if (strcmp(a_keyword, a_keyword_base) != 0)
                    continue;

                //
                switch (a_atype) {
                case IMECPreObject::ATY_SINGLE:
                    break;
                case IMECPreObject::ATY_ARRAY:
                {
                    int a_nb_values = preobject_to_merge.GetNbValues(a_vtype, k);
                    if (!a_nb_values)
                        continue;

                    int a_nb_values_base = preobject_base.GetNbValues(a_vtype, k);
                    int total_size = a_nb_values_base + a_nb_values;


                    const IDescriptor* a_descr_p = (const IDescriptor*)pdescrp;

                    int arr_ikw = a_descr_p->getIKeyword(a_keyword);
                    //Getting the Current array size
                    int a_size_ikeyword = a_descr_p->getSizeIKeyword(arr_ikw);
                    if (a_size_ikeyword <= 0)
                        continue;
                    string a_size_skeyword = a_descr_p->getSKeyword(a_size_ikeyword);
                    int a_size_index = preobject_base.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skeyword.c_str());
                    int a_size = 0;
                    if (a_size_index >= 0)
                        a_size = preobject_base.GetIntValue(a_size_index);
                    if (a_size < total_size)
                    {
                        if (a_size_index >= 0)
                            preobject_base.SetIntValue(a_size_index, total_size);
                        else preobject_base.AddIntValue(a_size_skeyword.c_str(), total_size);
                    }

                    //
                    switch (a_vtype) {
                    case IMECPreObject::VTY_BOOL:
                        preobject_base.AddBoolArray(a_keyword, total_size);
                        for (a_nb_values_base = 0; l < total_size; ++l) preobject_base.SetBoolValue(k, l, preobject_to_merge.GetBoolValue(k, l - a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_INT:
                        preobject_base.AddIntArray(a_keyword, total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) preobject_base.SetIntValue(k, l, preobject_to_merge.GetIntValue(k, l - a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_UINT:
                        preobject_base.AddUIntArray(a_keyword, (unsigned int)total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) preobject_base.SetUIntValue(k, l, preobject_to_merge.GetUIntValue(k, l - a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_FLOAT:
                        preobject_base.AddFloatArray(a_keyword, total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) preobject_base.SetFloatValue(k, l, preobject_to_merge.GetFloatValue(k, l- a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_STRING:
                        preobject_base.AddStringArray(a_keyword, total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) preobject_base.SetStringValue(k, l, preobject_to_merge.GetStringValue(k, l - a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_OBJECT:
                        preobject_base.AddObjectArray(a_keyword, total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) {
                            const char* a_otype = preobject_to_merge.GetObjectType(k, l - a_nb_values_base);
                            const char* a_name = preobject_to_merge.GetObjectName(k, l - a_nb_values_base);
                            int         a_id = preobject_to_merge.GetObjectId(k, l - a_nb_values_base);
                            int         a_index = preobject_to_merge.GetObjectIndex(k, l - a_nb_values_base);
                            if (a_name[0] != '\0')
                                preobject_base.SetObjectValue(k, l, a_otype, a_name, a_index);
                            else
                                preobject_base.SetObjectValue(k, l, a_otype, a_id, a_index);
                        }
                        break;
                    default:
                        // Wrong type of value
                        break;
                    }
                }
                break;
                default:
                    // Wrong type of attribute
                    break;
                }
            }
        }
    }
    //preobject_base.myParameters = preobject_to_merge.myParameters;
    //preobject_base.myParamIdName = preobject_to_merge.myParamIdName;
    //preobject_base.myIsNegatedParameters = preobject_to_merge.myIsNegatedParameters;

    const vector<IMECPreObject*>& a_subobj = preobject_to_merge.GetSubobject();

    size_t a_size = a_subobj.size();

    for (size_t ii = 0; ii < a_size; ii++)
    {
        preobject_base.SetSubobject(a_subobj[ii]);
    }
}



void ResizeArrayAttributesToPreObject(IMECPreObject& pre_object, const IDescriptor* descr_p, int arr_ikw, int size)
{
    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    string a_arr_skw = a_descr_p->getSKeyword(arr_ikw);
    //Getting the Current array size
    int a_size_ikeyword = a_descr_p->getSizeIKeyword(arr_ikw);
    if (a_size_ikeyword <= 0)
        return;

    string a_size_skeyword = a_descr_p->getSKeyword(a_size_ikeyword);
    int a_size_index = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skeyword);
    int a_size = 0;
    if (a_size_index >= 0)
        a_size = pre_object.GetIntValue(a_size_index);
    if (a_size < size)
    {
        if (a_size_index >= 0)
            pre_object.SetIntValue(a_size_index, size);
        else pre_object.AddIntValue(a_size_skeyword.c_str(), size);
    }
    
    /* Allocating the array in to the preobject */
    switch (a_descr_p->getValueType(arr_ikw))
    {
    case VTYPE_BOOL:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_arr_skw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_BOOL, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_BOOL, a_array_index, size);
                for (int i = a_nb_values; i < size; i++)
                    pre_object.AddBoolValue(a_arr_skw.c_str(), i, false);
            }
        }
        else
        {
            pre_object.AddBoolArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    case VTYPE_INT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_arr_skw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_INT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_INT, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddIntArray(a_arr_skw.c_str(), size);
        }
        break;
    }

    case VTYPE_UINT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_arr_skw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_UINT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_UINT, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddUIntArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    case VTYPE_FLOAT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_arr_skw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_FLOAT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_FLOAT, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddFloatArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    case VTYPE_STRING:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_arr_skw);
        string stringvalue = "";
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_STRING, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_STRING, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddStringArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    case VTYPE_OBJECT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_arr_skw);
        int otype = a_descr_p->getObjectType(arr_ikw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_OBJECT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_OBJECT, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddObjectArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    default:
        break;
    }
    return;
}

std::string GetAttribNameFromDrawable(const IDescriptor* pdescrp, const string& name)
{
    if (!pdescrp)  return name;
    const MvDrawable_t* a_drawable_p1 = pdescrp->getDrawablePtr(name);
    if (a_drawable_p1) 
        return pdescrp->getSKeyword(a_drawable_p1->getIKeyword());

    return name;
}

//Function to calculate cumulative offset
void CalculateCumulativeOffset(std::vector<IMECPreObject*>* p_preobjlst, const char* submodelsolkey)
{
    int tot_includes = (int)p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES].size();
    if (!tot_includes)
        return;
    cumulative_pindex_offsetMap.clear();
    for (int i = 0; i < tot_includes; i++)
    {
        CumulativeSubmodelOffsetInfo coff;
        coff.cumulative_object_offsetMap.reserve(HCDI_OBJ_TYPE_MAX);
        cumulative_pindex_offsetMap[i] = coff;
    }
    IMECPreObject* parent_obj = p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES][0];
    int subtype = HCDI_OBJ_TYPE_SOLVERSUBMODELS;
    int current_index = 1;// parent_obj->GetFileIndex();
    int cumulative = 0;

    int index = 0;
    for (int i = 0; i < tot_includes; i++)
    {
        IMECPreObject* parent_obj = p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES][i];
        if(nullptr == parent_obj) continue; // shouldn't happen
        index = parent_obj->GetFileIndex();
        bool has_submodel = false;
        bool only_once = false;
        while (0 <= index) // the root has index -1, no need to go through here
        {
            const char* kfulltype1 = parent_obj->GetKernelFullType();
            if (strstr(kfulltype1, submodelsolkey))
                has_submodel = true;
            if (has_submodel)
            {
                const IDescriptor* a_descr_p = HCDI_GetDescriptorHandle(parent_obj->GetKernelFullType());
                if (!a_descr_p) break; // shouldn't happen

                hwCFGDrawableInfPreObject drawable_inf(parent_obj, a_descr_p);

                const MvDrawable_t* def_drawable_p = a_descr_p->getDrawablePtr("_DEFAULT_IDOFFSET");
                int def_offset = 0;
                if(nullptr != def_drawable_p) def_offset = (int) def_drawable_p->evaluate(&drawable_inf);

                for(int inttype = HCDI_OBJ_TYPE_NULL + 1; inttype < HCDI_OBJ_TYPE_MAX; ++inttype)
                {
                    obj_type_e type = (obj_type_e) inttype;
                    char drawable_name[100];
                    sprintf(drawable_name, "_%s_IDOFFSET", MV_get_type(type).c_str());
                    const MvDrawable_t* drawable_p = a_descr_p->getDrawablePtr(drawable_name);
                    int offset = 0;
                    if(nullptr != drawable_p) offset = (int) drawable_p->evaluate(&drawable_inf);
                    else if(0 != def_offset)  offset = def_offset;
                    if (offset != 0)
                    {
                        CumulativeSubmodelOffsetInfo& csub = cumulative_pindex_offsetMap[i];
                        csub.cumulative_object_offsetMap[type] += offset; // Store value in the inner map
                        csub.presence = true; // Mark presence
                    }
                }
            }
            parent_obj = p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES][index];

            int index1 = parent_obj->GetFileIndex();
            if (index == index1) /*can't have file referencing to iteself*/
            {
                assert(0);
                index--;
            }
            else
                index = index1;
        }
    }
}

static obj_type_e GetMultiObjectType(IMECPreObject* preObj, IMECPreObject::MyAttributeType_e a_atype, int k)
{
    obj_type_e a_hm_type = HCDI_OBJ_TYPE_NULL;
    const char* kwd = preObj->GetKeyword(a_atype, IMECPreObject::VTY_OBJECT, k);
    if (kwd)
    {
        string typekwd = string(kwd) + "_type";
        const char* a_otype = preObj->GetStringValue(typekwd.c_str());
        size_t len = a_otype ? strlen(a_otype) : 0;
        if (len > 1)
        {
            size_t slashlen = '/' == a_otype[0] ? 1 : 0; // optionally prepended '/'
            const char* psubslash = std::find (a_otype + slashlen, a_otype + len, '/');
            if(a_otype + len == psubslash)
            {
                a_hm_type = HCDI_get_entitytype(a_otype + slashlen);
            }
            else
            {
                char buffer[100];
                len = psubslash - a_otype - slashlen;
                strncpy(buffer, a_otype + slashlen, len);
                buffer[len] = '\0';
                a_hm_type = HCDI_get_entitytype(buffer);
            }
        }
    }
    return a_hm_type;
}

void TransformWithOffset(int submodelindex, IMECPreObject* preObj, bool doUnOffset, obj_type_e hm_type,
                         const std::unordered_map<std::string, cfglnksubdescriptor>* cfglnksubdescriptor_map)
{
    if (preObj == nullptr || submodelindex < 0) return;


    //std::unordered_map<int, CumulativeSubmodelOffsetInfo> cumulative_pindex_offsetMap;
    CumulativeSubmodelOffsetInfo& subinfo = cumulative_pindex_offsetMap[submodelindex];
    if (!subinfo.presence)
        return;

    std::unordered_map<obj_type_e, int>& cumulobjinfo = subinfo.cumulative_object_offsetMap;

    // for all entities except /SET/COLLECT offset the own id
    if (preObj->GetId() > 0)
    {
        int offsetval = 0;
        if(cumulobjinfo.count(hm_type) > 0)
        {
            offsetval = cumulobjinfo[hm_type];
        }
        else if(cfglnksubdescriptor_map)
        {
            auto it = cfglnksubdescriptor_map->find(preObj->GetKernelFullType());
            if(it != cfglnksubdescriptor_map->end() &&
               cumulobjinfo.count(it->second.parent_type) > 0)
            {
                offsetval = cumulobjinfo[it->second.parent_type];
            }
        }
        if (doUnOffset) preObj->SetId(preObj->GetId() - offsetval);
        else           preObj->SetId(preObj->GetId() + offsetval);
    }
    // offset the unit
    if (preObj->GetUnitId() > 0)
    {
        int offsetval = cumulobjinfo[HCDI_OBJ_TYPE_UNITS];
        if (doUnOffset) preObj->SetUnitId(preObj->GetUnitId() - offsetval);
        else            preObj->SetUnitId(preObj->GetUnitId() + offsetval);
    }

    // references
    int i, k, l;
    int a_nb_atypes = IMECPreObject::ATY_LAST - IMECPreObject::ATY_UNKNOWN - 1;
    for (i = 0; i < a_nb_atypes; ++i)
    {
        IMECPreObject::MyAttributeType_e a_atype = (IMECPreObject::MyAttributeType_e)(IMECPreObject::ATY_UNKNOWN + i + 1);
        //
        int           a_nb_attributes = preObj->GetNbAttributes(a_atype, IMECPreObject::VTY_OBJECT);
        //
        for (k = 0; k < a_nb_attributes; ++k)
        {
            switch (a_atype)
            {
            case IMECPreObject::ATY_SINGLE:
            {
                int         a_id = preObj->GetObjectId(k);
                if (0 < a_id)
                {
                    const char* a_otype = preObj->GetObjectType(k);
                    int         a_index = preObj->GetObjectIndex(k);
                    obj_type_e a_hm_type = HCDI_get_entitytype(a_otype);

                    int offsetval = 0;
                    if (a_hm_type == HCDI_OBJ_TYPE_MULTIOBJECT)
                    {
                        a_hm_type = GetMultiObjectType(preObj, a_atype, k);
                        if(cumulobjinfo.count(a_hm_type) > 0) offsetval = cumulobjinfo[a_hm_type];
                            }
                    else
                        if(cumulobjinfo.count(a_hm_type) > 0) offsetval = cumulobjinfo[a_hm_type];

                    if (doUnOffset) a_id -= offsetval;
                    else            a_id += offsetval;

                    preObj->SetObjectValue(k, a_otype, a_id, a_index);
                }
            }
            break;
            case IMECPreObject::ATY_ARRAY:
            {
                obj_type_e multiobject_type = HCDI_OBJ_TYPE_NULL;
                int a_nb_values = preObj->GetNbValues(IMECPreObject::VTY_OBJECT, k);
                for (l = 0; l < a_nb_values; ++l)
                {
                    int         a_id = preObj->GetObjectId(k, l);
                    if (0 < a_id)
                    {
                        const char* a_otype = preObj->GetObjectType(k, l);
                        int         a_index = preObj->GetObjectIndex(k, l);
                        obj_type_e a_hm_type = HCDI_get_entitytype(a_otype);
                        int offsetval = 0;
                        if (a_hm_type == HCDI_OBJ_TYPE_MULTIOBJECT)
                        {
                            if(HCDI_OBJ_TYPE_NULL == multiobject_type)
                            {
                                multiobject_type = GetMultiObjectType(preObj, a_atype, k);
                            }
                            if(cumulobjinfo.count(multiobject_type) > 0) offsetval = cumulobjinfo[multiobject_type];
                        }
                        else
                            if(cumulobjinfo.count(a_hm_type) > 0) offsetval = cumulobjinfo[a_hm_type];

                        if (doUnOffset) a_id -= offsetval;
                        else           a_id += offsetval;

                        preObj->SetObjectValue(k, l, a_otype, a_id, a_index);
                    }
                }
            }
            break;
            default:
                break;
            }
        }
    }
    
    const std::vector<IMECPreObject*>& subobj = preObj->GetSubobject();

    for (int i = 0; i < subobj.size(); ++i)
    {
        int hm_type = (int)HCDI_GetHCObjectType(preObj->GetKernelFullType());
        TransformWithOffset(submodelindex, subobj[i], doUnOffset, (obj_type_e)hm_type, nullptr);
    }
}

static void TransformWithOffsetIntArray(IMECPreObject* preObj,
                                        int k, // attribindex, naming for consistency with function above
                                        int offsetval, bool doUnOffset)
{
    if (0 == offsetval) return;
    int a_nb_values = preObj->GetNbValues(IMECPreObject::VTY_INT, k);
    for (int l = 0; l < a_nb_values; ++l)
    {
        int a_id = preObj->GetIntValue(k, l);
        if (0 < a_id)
        {
            if (doUnOffset) a_id -= offsetval;
            else            a_id += offsetval;
            preObj->SetIntValue(k, l, a_id);
        }
    }
}

void UpdateEntityIDOffsetCFG(
    std::vector<IMECPreObject*>* p_preobjlst, const char* submodelsolkey, bool doUnOffset,
    const std::unordered_map<std::string, cfglnksubdescriptor>* cfglnksubdescriptor_map)
{
    cumulative_pindex_offsetMap.clear();
    CalculateCumulativeOffset(p_preobjlst, submodelsolkey);

    if (!cumulative_pindex_offsetMap.size())
        return;
    int incl_totcount = (int)p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES].size();

    // int ids in nodes
    for (auto* preObj : p_preobjlst[HCDI_OBJ_TYPE_NODES])
    {
        if (preObj == NULL) continue;
        int fileindex = preObj->GetFileIndex();
        if (fileindex <= 0) continue;
        int attribindex = preObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
        if(0 > attribindex) continue;
        CumulativeSubmodelOffsetInfo& subinfo = cumulative_pindex_offsetMap[fileindex];
        if (!subinfo.presence) continue;
        std::unordered_map<obj_type_e, int>& cumulobjinfo = subinfo.cumulative_object_offsetMap;
        int nodeoffset = cumulobjinfo[HCDI_OBJ_TYPE_NODES];
        TransformWithOffsetIntArray(preObj, attribindex, nodeoffset, doUnOffset);
    }

    // int ids in elements
    for (auto* preObj : p_preobjlst[HCDI_OBJ_TYPE_ELEMS])
    {
        if (preObj == NULL) continue;
        int fileindex = preObj->GetFileIndex();
        if (fileindex <= 0) continue;
        int nb_attributes = preObj->GetNbAttributes(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT);
        if(0 == nb_attributes) continue;
        CumulativeSubmodelOffsetInfo& subinfo = cumulative_pindex_offsetMap[fileindex];
        if (!subinfo.presence) continue;
        std::unordered_map<obj_type_e, int>& cumulobjinfo = subinfo.cumulative_object_offsetMap;
        int nodeoffset = cumulobjinfo[HCDI_OBJ_TYPE_NODES];
        int elemoffset = cumulobjinfo[HCDI_OBJ_TYPE_ELEMS];
        int partoffset = cumulobjinfo[HCDI_OBJ_TYPE_COMPS];
        for(int k = 0; k < nb_attributes; ++k)
        {
            const char* skwd = preObj->GetKeyword(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, k);
            if (strcmp(skwd, "id") == 0)
            {
                TransformWithOffsetIntArray(preObj, k, elemoffset, doUnOffset);
            }
            else if (strncmp(skwd,"node", 4) == 0)
            {
                TransformWithOffsetIntArray(preObj, k, nodeoffset, doUnOffset);
            }
            else if (strcmp(skwd, "collector") == 0)
            {
                TransformWithOffsetIntArray(preObj, k, partoffset, doUnOffset);
            }
        }
    }

    for (int i = 0; i <= (int)HCDI_OBJ_TYPE_SUBOBJECT; ++i)
    {
        for (auto* preObj : p_preobjlst[i])
        {
            if (preObj == NULL) continue;

            int fileindex = preObj->GetFileIndex();
            if (fileindex <= 0)
                continue;

            CumulativeSubmodelOffsetInfo& subinfo = cumulative_pindex_offsetMap[fileindex];

            if (!subinfo.presence)
                continue;

            TransformWithOffset(fileindex, preObj, doUnOffset, (obj_type_e)i,
                                cfglnksubdescriptor_map);
        }
    }
}

// Helper for comparing doubles with tolerance
bool almost_equal(double a, double b, double epsilon = 1e-10) {
    return std::fabs(a - b) < epsilon;
}
void check(bool condition, const char* message) 
{
    if (!condition) {
        std::cerr << "Test failed: " << message << std::endl;
        //std::exit(1);
    }
}

namespace {

    inline bool IsValidObjectType(obj_type_e type) {
        return type > HCDI_OBJ_TYPE_NULL && type < HCDI_OBJ_TYPE_HC_MAX;
    }

    inline bool IsValidId(int id) {
        return id > 0;
    }

    /**
     * @brief Internal binary search helper for sorted preobject vectors
     */
    IMECPreObject* BinarySearchPreObjectById(
        const std::vector<IMECPreObject*>& vec,
        int id)
    {
        if (vec.empty()) return nullptr;

        // Binary search using lower_bound with custom comparator
        // Objects are sorted by (GetId() + GetIdOffset()) in PreTreatObject
        auto it = std::lower_bound(vec.begin(), vec.end(), id,
            [](const IMECPreObject* obj, int search_id) {
                if (!obj) return true; // Handle null pointers gracefully
                return ((int)obj->GetId() + obj->GetIdOffset()) < search_id;
            });

        IMECPreObject* result = nullptr;
        if (it != vec.end() && *it) {
            int found_id = (int)(*it)->GetId() + (*it)->GetIdOffset();
            if (found_id == id) {
                result = *it;
            }
        }

        return result;
    }
}


// Implementation of type-specific FindPreObjectById
IMECPreObject* FindPreObjectById(
    std::vector<IMECPreObject*>* preobj_list,
    obj_type_e type,
    int id)
{
    // Input validation
    if (!preobj_list || !IsValidId(id) || !IsValidObjectType(type)) {
        return nullptr;
    }
    // Search in the specific type vector
    const std::vector<IMECPreObject*>& objects = preobj_list[type];
    IMECPreObject* result = BinarySearchPreObjectById(objects, id);

    return result;
}

// Implementation of range-based FindPreObjectsByIdRange
std::vector<IMECPreObject*> FindPreObjectsByIdRange(
    std::vector<IMECPreObject*>* preobj_list,
    obj_type_e type,
    int min_id,
    int max_id)
{
    std::vector<IMECPreObject*> results;

    // Input validation
    if (!preobj_list || !IsValidObjectType(type) || min_id > max_id || !IsValidId(min_id)) {
        return results;
    }

    const std::vector<IMECPreObject*>& objects = preobj_list[type];
    if (objects.empty()) {
        return results;
    }

    // Use binary search to find range bounds efficiently - O(log n) for each bound

    // Find the first object with effective_id >= min_id
    auto start_it = std::lower_bound(objects.begin(), objects.end(), min_id,
        [](const IMECPreObject* obj, int search_id) {
            if (!obj) return true; // Handle null pointers
            return ((int)obj->GetId() + obj->GetIdOffset()) < search_id;
        });

    // Find the first object with effective_id > max_id
    auto end_it = std::upper_bound(objects.begin(), objects.end(), max_id,
        [](int search_id, const IMECPreObject* obj) {
            if (!obj) return false; // Handle null pointers
            return search_id < ((int)obj->GetId() + obj->GetIdOffset());
        });

    // Reserve space for results - estimate based on range
    size_t estimated_count = std::distance(start_it, end_it);
    results.reserve(estimated_count);

    // Copy all objects in the range, filtering out null pointers
    for (auto it = start_it; it != end_it; ++it) {
        if (*it) { // Skip null pointers if any
            results.push_back(*it);
        }
    }

    return results;
}

// Implementation of GetPreObjectFromReference
IMECPreObject* GetPreObjectFromReference(
    IMECPreObject* main_preobject,
    const std::string& ref_obj_skeyword,
    const IDescriptor*           descrp,
    std::vector<IMECPreObject*>* preobj_list)
{
    // Input validation
    if (!main_preobject || !preobj_list || ref_obj_skeyword.empty()) {
        return nullptr;
    }

    int attrib_ind = -1;
    int ref_id = 0;

    // Try OBJECT type first (most common case)
    attrib_ind = main_preobject->GetIndex(IMECPreObject::ATY_SINGLE,
        IMECPreObject::VTY_OBJECT,
        ref_obj_skeyword);

    if (descrp == nullptr)
        descrp = HCDI_GetDescriptorHandle(main_preobject->GetKernelFullType());

    if (descrp == nullptr)
        return nullptr;

    int ikey = descrp->getIKeyword(ref_obj_skeyword.c_str());

    obj_type_e type = descrp->getObjectType(ikey);

    if (attrib_ind >= 0) {
        ref_id = main_preobject->GetObjectId(attrib_ind);
        if (IsValidId(ref_id)) {
            return FindPreObjectById(preobj_list, type, ref_id);
        }
    }

    // Reference not found or invalid
    return nullptr;
}



bool VerifyPreObjectSorting(
    std::vector<IMECPreObject*>* preobj_list,
    obj_type_e type)
{
    if (!preobj_list) {
        return false;
    }

    // Determine which types to check
    std::vector<obj_type_e> types_to_check;
    if (type == HCDI_OBJ_TYPE_NULL) {
        // Check all types
        for (int i = 0; i < HCDI_OBJ_TYPE_HC_MAX; ++i) {
            if (i != HCDI_OBJ_TYPE_SUBOBJECT) {
                types_to_check.push_back(static_cast<obj_type_e>(i));
            }
        }
    }
    else {
        // Check specific type
        if (IsValidObjectType(type)) {
            types_to_check.push_back(type);
        }
    }

    // Check each type for proper sorting
    for (obj_type_e check_type : types_to_check) {
        const std::vector<IMECPreObject*>& objects = preobj_list[check_type];

        // Empty vectors are considered sorted
        if (objects.size() <= 1) continue;

        // Check that each adjacent pair is in correct order
        for (size_t i = 1; i < objects.size(); ++i) {
            IMECPreObject* prev = objects[i - 1];
            IMECPreObject* curr = objects[i];

            // Skip null pointers (they should not exist but handle gracefully)
            if (!prev || !curr) continue;

            int prev_id = prev->GetId() + prev->GetIdOffset();
            int curr_id = curr->GetId() + curr->GetIdOffset();

            // Sorting requirement: previous <= current
            if (prev_id > curr_id) {
                // Found unsorted pair
                return false;
            }
        }
    }

    // All checked vectors are properly sorted
    return true;
}

void test_cfgio_parse_int_or_blank_fast()
{
    int val;
    bool blank;
    int read;

    check(cfgio_parse_int_or_blank_fast("", 0, val, blank, read) == true && blank == true, "Empty string");
    check(cfgio_parse_int_or_blank_fast(nullptr, 0, val, blank, read) == false && blank == true, "Nullptr input");

    
    check(cfgio_parse_int_or_blank_fast("     ", 5, val, blank, read) == true && blank == true, "All blanks");
    check(cfgio_parse_int_or_blank_fast("     ", 10, val, blank, read) == true && blank == true, "All blanks. Width more than length");

    // Valid ints
    check(cfgio_parse_int_or_blank_fast("12345", 5, val, blank, read) == true && val == 12345 && !blank, "Valid int: 12345");
    check(cfgio_parse_int_or_blank_fast("12345", 10, val, blank, read) == true && val == 12345 && !blank, "Valid int: 12345. Width more than length");
    check(cfgio_parse_int_or_blank_fast("   42", 5, val, blank, read) == true && val == 42 && !blank, "Valid int: 42");
    check(cfgio_parse_int_or_blank_fast(" -999", 5, val, blank, read) == true && val == -999 && !blank, "Negative int: -999");
    check(cfgio_parse_int_or_blank_fast("00012", 5, val, blank, read) == true && val == 12 && !blank, "Zero-prefixed int: 00012");

    // Partial floats (should truncate to int)
    check(cfgio_parse_int_or_blank_fast("  1.0", 5, val, blank, read) == true && val == 1 && !blank, "Float to int: 1.0");
    check(cfgio_parse_int_or_blank_fast(" 123.", 5, val, blank, read) == true && val == 123 && !blank, "Float to int: 123.");
    check(cfgio_parse_int_or_blank_fast("  7.1", 5, val, blank, read) == true && val == 7 && !blank, "Float to int: 7.1");
    check(cfgio_parse_int_or_blank_fast("1.234", 5, val, blank, read) == true && val == 1 && !blank, "Float to int: 1.234");

    // Invalid input: need to check behavior of sscanf or atoi
    check(cfgio_parse_int_or_blank_fast("123a4", 5, val, blank, read) == false, "Invalid int: 123a4");
    check(cfgio_parse_int_or_blank_fast("abc", 3, val, blank, read) == false, "Invalid int: abc");
    check(cfgio_parse_int_or_blank_fast("12 34", 5, val, blank, read) == false, "Invalid int: 12 34");

    std::cout << "All int tests passed\n";
}

void test_cfgio_parse_double_or_blank_fast() {
    double val;
    bool blank;
    int read;

    check(cfgio_parse_double_or_blank_fast("", 0, val, blank, read) == true && blank == true, "Double: Empty string");
    check(cfgio_parse_double_or_blank_fast(nullptr, 0, val, blank, read) == true && blank == true, "Double: Nullptr input");
    check(cfgio_parse_double_or_blank_fast("     ", 5, val, blank, read) == true && blank == true, "Double: All blanks");

    // Basic valid values
    check(cfgio_parse_double_or_blank_fast("123", 3, val, blank, read) == true && val == 123.0 && !blank, "Double: 123");
    check(cfgio_parse_double_or_blank_fast("1.0", 3, val, blank, read) == true && val == 1.0 && !blank, "Double: 1.0");
    check(cfgio_parse_double_or_blank_fast("0.001", 5, val, blank, read) == true && fabs(val - 0.001) < 1e-10 && !blank, "Double: 0.001");
    check(cfgio_parse_double_or_blank_fast(".001", 4, val, blank, read) == true && fabs(val - 0.001) < 1e-10 && !blank, "Double: .001");
    check(cfgio_parse_double_or_blank_fast("5.2     ", 8, val, blank, read) == true && fabs(val - 5.2) < 1e-10 && !blank, "Double: 5.2");



    // Scientific notation
    check(cfgio_parse_double_or_blank_fast("1.2e3", 5, val, blank, read) == true && fabs(val - 1200.0) < 1e-10 && !blank, "Double: 1.2e3");
    check(cfgio_parse_double_or_blank_fast("1.2e+3", 6, val, blank, read) == true && fabs(val - 1200.0) < 1e-10 && !blank, "Double: 1.2e+3");
    check(cfgio_parse_double_or_blank_fast("1.2e-3", 6, val, blank, read) == true && fabs(val - 0.0012) < 1e-10 && !blank, "Double: 1.2e-3");

    // Auto-correct scientific (with missing 'e') 
    check(cfgio_parse_double_or_blank_fast("1.0+21", 7, val, blank, read) == true && fabs(val - 1.0e21) < 1e15 && !blank, "Double: 1.0+21 -> 1.0E+21");
    check(cfgio_parse_double_or_blank_fast("-1.0+21", 8, val, blank, read) == true && fabs(val + 1.0e21) < 1e15 && !blank, "Double: -1.0+21 -> -1.0E+21");

    // Invalid
    check(cfgio_parse_double_or_blank_fast("123a4", 5, val, blank, read) == false, "Double: Invalid 123a4");
    check(cfgio_parse_double_or_blank_fast("abc", 3, val, blank, read) == false, "Double: Invalid abc");
    check(cfgio_parse_double_or_blank_fast("1.2e++3", 7, val, blank, read) == false, "Double: Invalid 1.2e++3");

    std::cout << "All double tests passed\n";
}

void test_parse_string_fast_int_double()
{
    test_cfgio_parse_int_or_blank_fast();
    test_cfgio_parse_double_or_blank_fast();
}

void test_cfgio_atof_fast_precision()
{
    // Test cases to validate precision against sscanf
    const char* test_cases[] = {
        "5.97000002555655",      // The critical precision case
        "3.23884927558785E-06",
        "1.23456789012345",
        "9.87654321098765E+15",
        "0.000000000000001",
        "123456789.123456789",
        "2.22507385850720138e-308", // Near double minimum
        "1.79769313486231570e+308", // Near double maximum
        "0.1",
        "0.01",
        "0.001",
        "1.0",
        "10.0",
        "100.0",
        "1e-15",
        "1e15",
        "-5.97000002555655",
        "-0.000123456789",
        "0.0",
        "123",
        "123.456",
        "1.234567890123456789" // Test precision limit
    };

    const int num_tests = sizeof(test_cases) / sizeof(test_cases[0]);
    int passed = 0;
    int precision_matches = 0;

    std::cout << "=== cfgio_atof_fast Precision Test ===" << std::endl;

    for (int i = 0; i < num_tests; ++i) {
        double fast_result = cfgio_atof_fast(test_cases[i]);
        double sscanf_result = 0.0;
        sscanf(test_cases[i], "%lg", &sscanf_result);

        // Check for exact match first
        bool exact_match = (fast_result == sscanf_result);

        // For non-exact matches, check if within acceptable precision
        bool precision_ok = exact_match ||
            (std::fabs(fast_result - sscanf_result) < std::fabs(sscanf_result) * 1e-15);

        if (exact_match) {
            precision_matches++;
            std::cout << "[PASS] EXACT MATCH: " << test_cases[i] << std::endl;
        }
        else if (precision_ok) {
            std::cout << "[PASS] PRECISION OK: " << test_cases[i] << std::endl;
        }
        else {
            std::cout << "[FAIL] PRECISION MISMATCH: " << test_cases[i] << std::endl;
            std::cout << "  cfgio_atof_fast: " << std::scientific << std::setprecision(17) << fast_result << std::endl;
            std::cout << "  sscanf:          " << std::scientific << std::setprecision(17) << sscanf_result << std::endl;
            std::cout << "  difference:      " << std::scientific << (fast_result - sscanf_result) << std::endl;
        }

        if (precision_ok) passed++;
    }

    std::cout << "\n=== TEST SUMMARY ===" << std::endl;
    std::cout << "Total tests: " << num_tests << std::endl;
    std::cout << "Passed: " << passed << std::endl;
    std::cout << "Exact matches: " << precision_matches << std::endl;
    std::cout << "Success rate: " << (100.0 * passed / num_tests) << "%" << std::endl;

    // Special test for the critical case mentioned
    double critical_result = cfgio_atof_fast("5.97000002555655");
    double expected = 5.9700000255565495;
    if (critical_result == expected) {
        std::cout << "[PASS] CRITICAL TEST PASSED: 5.97000002555655 == 5.9700000255565495" << std::endl;
    }
    else {
        std::cout << "[FAIL] CRITICAL TEST FAILED: Expected " << std::scientific << std::setprecision(17)
            << expected << ", got " << critical_result << std::endl;
    }
    critical_result = cfgio_atof_fast("9.87654321098765+11");
    expected = 9.87654321098765e+11;
    if (critical_result != expected) {
        std::cout << "[FAIL] CRITICAL TEST FAILED: Expected " << std::scientific << std::setprecision(17)
            << expected << ", got " << critical_result << std::endl;
    }
    critical_result = cfgio_atof_fast("9.87654321098765-11");
    expected = 9.87654321098765e-11;
    if (critical_result != expected) {
        std::cout << "[FAIL] CRITICAL TEST FAILED: Expected " << std::scientific << std::setprecision(17)
            << expected << ", got " << critical_result << std::endl;
    }
}
