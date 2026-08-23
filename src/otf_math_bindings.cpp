#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <Rcpp.h>

#include <cstdint>
#include <cstring>
#include <exception>
#include <string>
#include <vector>

#include "microtex.h"
#include "otf/otf_math_reader.h"
#include "unimath/font_src.h"

// The OTF -> CLM synthesis itself lives in the layout engine
// (MicroTeX/lib/otf/otf_math_reader.{h,cpp}) and knows nothing about R.
// Everything here is the R boundary: turn std::vector<u8> into a raw
// vector, and turn C++ exceptions into R conditions.

// [[Rcpp::export]]
SEXP ot_math_table_bytes(std::string path, int index = 0) {
    std::vector<std::uint8_t> buf;
    try {
        buf = microtex::otfMathTableBytes(path, index);
    } catch (const std::exception& e) {
        Rcpp::stop(e.what());
    }
    if (buf.empty()) return R_NilValue;
    Rcpp::RawVector out(buf.size());
    std::memcpy(&out[0], buf.data(), buf.size());
    return out;
}

// [[Rcpp::export]]
Rcpp::RawVector otf_to_clm_bytes(std::string path, int index = 0) {
    std::vector<std::uint8_t> bytes;
    try {
        bytes = microtex::otfToClmBytes(path, index);
    } catch (const std::exception& e) {
        Rcpp::stop(e.what());
    }
    Rcpp::RawVector out(bytes.size());
    if (!bytes.empty()) std::memcpy(&out[0], bytes.data(), bytes.size());
    return out;
}

// [[Rcpp::export]]
std::string microtex_add_font_from_otf(std::string otf_path, int index = 0) {
    std::vector<std::uint8_t> clm;
    try {
        clm = microtex::otfToClmBytes(otf_path, index);
    } catch (const std::exception& e) {
        Rcpp::stop(e.what());
    }
    microtex::FontSrcData src(clm.size(), clm.data(), otf_path);
    try {
        auto meta = microtex::MicroTeX::addFont(src);
        if (!meta.isValid()) return std::string();
        return meta.family;
    } catch (const std::exception& e) {
        Rcpp::warning(std::string("addFont failed: ") + e.what());
        return std::string();
    }
}
