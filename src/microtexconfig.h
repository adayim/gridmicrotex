#pragma once

// Generated manually for R package build (normally from microtexconfig.h.in via CMake)

// Suppress warnings in vendored MicroTeX code that we cannot modify.
// This header is force-included via -include microtexconfig.h in Makevars.
#if defined(__clang__)
#pragma clang diagnostic ignored "-Wmismatched-tags"
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
#pragma clang diagnostic ignored "-Wmissing-braces"
#elif defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wreorder"
#endif

// Disable auto font find - we load fonts explicitly
// #define HAVE_AUTO_FONT_FIND

#define MICROTEX_VERSION_MAJOR 1
#define MICROTEX_VERSION_MINOR 0
#define MICROTEX_VERSION_PATCH 0
