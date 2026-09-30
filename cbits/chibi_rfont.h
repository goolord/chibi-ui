/* chibi_rfont: the glue between RFont and chibi-ui.
 *
 * RFont rasterizes glyphs through its embedded stb_truetype and hands each
 * glyph's 8-bit coverage bitmap to a renderer callback. This shim provides
 * that callback, packing the bitmaps into one CPU-side coverage atlas that
 * the OpenGL renderer uploads as an R8 texture, and exposes the per-glyph
 * metrics chibi-ui's draw list needs. Single font at a time. */

#ifndef CHIBI_RFONT_H
#define CHIBI_RFONT_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* One rasterized glyph, in device pixels at the size it was added at. */
typedef struct ChibiGlyph {
    int32_t ax, ay, ax2, ay2; /* atlas texel rect (x, y, x2, y2) */
    float w, h;               /* bitmap size; zero for blank glyphs */
    float x1, y1;             /* draw offsets: left bearing, and baseline-to-top (y1 <= 0) */
    float advance;            /* pen advance */
} ChibiGlyph;

/* Copy the TTF data (RFont keeps and frees it), allocate the atlas, and load
 * the font at the given row height. Returns NULL on failure; frees any font
 * from a previous call. */
void* chibi_rfont_init(const uint8_t* data, uint32_t dataLen, uint32_t maxHeight,
                       uint32_t atlasW, uint32_t atlasH);

/* Free the font, its data, and the atlas. */
void chibi_rfont_free(void* font);

/* Rasterize (or fetch from RFont's cache) and fill out. A cache miss with a
 * full glyph table zeroes the glyph. */
void chibi_rfont_glyph(void* font, uint32_t codepoint, uint32_t size, ChibiGlyph* out);

/* Font metrics in font units: height (ascent - descent), descent (<= 0), and
 * the space advance. */
void chibi_rfont_metrics(void* font, float* fheight, float* descent, float* spaceAdv);

/* The atlas coverage bytes (atlasW x atlasH), owned by the shim. */
uint8_t* chibi_rfont_atlas_pixels(void* font);
uint32_t chibi_rfont_atlas_width(void* font);
uint32_t chibi_rfont_atlas_height(void* font);

/* What changed in the atlas since the last call, clearing it: 0 nothing;
 * 1 rows [*y0, *y1) gained glyphs, and no texel outside them changed; 2 the
 * atlas is new (the first call on a font), so upload all of it. */
int chibi_rfont_take_dirty(void* font, int32_t* y0, int32_t* y1);

#ifdef __cplusplus
}
#endif

#endif /* CHIBI_RFONT_H */
