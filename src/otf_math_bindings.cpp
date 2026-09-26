#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <Rcpp.h>

#include <exception>
#include <string>

#include "microtex.h"
#include "unimath/font_src.h"

// Reading a font lives in the layout engine
// (MicroTeX/lib/otf/otf_math_reader.{h,cpp}) and knows nothing about R.
// This is the R boundary: a font that cannot be read is an R error.

// [[Rcpp::export]]
std::string microtex_add_font_from_otf(std::string otf_path, int index = 0) {
    microtex::FontSrcOtf src(otf_path, index);
    try {
        auto meta = microtex::MicroTeX::addFont(src);
        if (!meta.isValid()) return std::string();
        return meta.family;
    } catch (const std::exception& e) {
        Rcpp::stop(std::string("Failed to read font '") + otf_path + "': " + e.what());
    }
}
