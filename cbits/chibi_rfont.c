#define RFONT_IMPLEMENTATION
#include "RFont.h"

#include "chibi_rfont.h"

#include <stdlib.h>
#include <string.h>

/* Each font owns its atlas. RFont's renderer hooks pass the per-font
 * wrapper through the opaque RFont_texture handle (a size_t), so several
 * fonts can coexist: the handles chibi_rfont hands out stay valid until
 * chibi_rfont_free is called on them, and nothing one font does touches
 * another. */

typedef struct ChibiFont {
    RFont_font* font;
    uint8_t* pixels; /* coverage atlas */
    uint32_t width, height;
    int dirty;
} ChibiFont;

static RFont_renderer chibi_renderer;
static int chibi_renderer_ready = 0;

/* RFont hands the create_atlas result back to bitmap_to_atlas and
 * free_atlas, so it doubles as the per-font wrapper pointer. */
static size_t chibi_create_atlas(void* ctx, u32 w, u32 h) {
    ChibiFont* f;
    (void)ctx;
    f = (ChibiFont*)calloc(1, sizeof(ChibiFont));
    if (f == NULL) return 0;
    f->pixels = (uint8_t*)calloc((size_t)w * (size_t)h, 1);
    if (f->pixels == NULL) {
        free(f);
        return 0;
    }
    f->width = w;
    f->height = h;
    return (size_t)f;
}

static void chibi_free_atlas(void* ctx, RFont_texture atlas) {
    ChibiFont* f = (ChibiFont*)atlas;
    (void)ctx;
    if (f != NULL) {
        free(f->pixels);
        free(f);
    }
}

/* Pack one glyph's coverage rows at the packer position RFont keeps in
 * font->atlasX/atlasY: wrap to a new row of maxH when the glyph would
 * overflow the atlas width, copy the rows, then advance x. Every glyph
 * leaves one texel of padding after it (and every row below it) so linear
 * atlas filtering cannot bleed a neighbour's edge into this glyph's
 * outermost samples. The glyph's atlas rect comes back as
 * [atlasX - w - 1, atlasX - 1) afterwards; RFont derives it from the
 * amount x advances here, so the pad must stay 1. */
static void chibi_bitmap_to_atlas(void* ctx, RFont_texture atlas, u32 aw, u32 ah, u32 maxH,
                                  u8* bitmap, float w, float h, float* x, float* y) {
    ChibiFont* f = (ChibiFont*)atlas;
    int iw = (int)w;
    int ih = (int)h;
    (void)ctx;
    (void)ah;
    if (*x + w + 1.0f > (float)aw) {
        *x = 0.0f;
        *y += (float)maxH + 1.0f;
    }
    if (bitmap != NULL && iw > 0 && ih > 0 && (int)*x + iw <= (int)aw &&
        (int)*y + ih <= (int)f->height) {
        int row;
        for (row = 0; row < ih; row++) {
            uint8_t* dst = f->pixels + ((size_t)(int)(*y) + (size_t)row) * (size_t)aw + (size_t)(int)(*x);
            const uint8_t* src = bitmap + (size_t)row * (size_t)iw;
            RFONT_MEMCPY(dst, src, (size_t)iw);
        }
        f->dirty = 1;
    }
    *x += w + 1.0f;
}

static void chibi_renderer_setup(void) {
    if (chibi_renderer_ready) return;
    chibi_renderer.ctx = NULL;
    chibi_renderer.proc.size = NULL;
    chibi_renderer.proc.initPtr = NULL;
    chibi_renderer.proc.create_atlas = chibi_create_atlas;
    chibi_renderer.proc.free_atlas = chibi_free_atlas;
    chibi_renderer.proc.bitmap_to_atlas = chibi_bitmap_to_atlas;
    chibi_renderer.proc.render = NULL;
    chibi_renderer.proc.set_framebuffer = NULL;
    chibi_renderer.proc.set_color = NULL;
    chibi_renderer.proc.set_surface = NULL;
    chibi_renderer.proc.freePtr = NULL;
    chibi_renderer_ready = 1;
}

void* chibi_rfont_init(const uint8_t* data, uint32_t dataLen, uint32_t maxHeight,
                       uint32_t atlasW, uint32_t atlasH) {
    uint8_t* copy;
    RFont_font* font;

    if (data == NULL || dataLen == 0) return NULL;

    copy = (uint8_t*)RFONT_MALLOC((size_t)dataLen);
    if (copy == NULL) return NULL;
    RFONT_MEMCPY(copy, data, (size_t)dataLen);

    chibi_renderer_setup();
    font = RFont_font_init_data(&chibi_renderer, copy, maxHeight, atlasW, atlasH);
    if (font == NULL || font->atlas == 0) {
        if (font != NULL) RFont_font_free(&chibi_renderer, font);
        RFONT_FREE(copy);
        return NULL;
    }
    /* The wrapper came back through create_atlas; give it its font. */
    {
        ChibiFont* cf = (ChibiFont*)(size_t)font->atlas;
        cf->font = font;
        return (void*)cf;
    }
}

void chibi_rfont_free(void* handle) {
    ChibiFont* f = (ChibiFont*)handle;
    if (f != NULL && f->font != NULL) RFont_font_free(&chibi_renderer, f->font);
    /* RFont_font_free ran free_atlas, which freed the wrapper. */
}

void chibi_rfont_glyph(void* handle, uint32_t codepoint, uint32_t size, ChibiGlyph* out) {
    ChibiFont* f = (ChibiFont*)handle;
    RFont_glyph g;
    RFONT_MEMSET(&g, 0, sizeof(g));
    if (f != NULL && f->font != NULL)
        g = RFont_font_add_codepoint(&chibi_renderer, f->font, codepoint, (size_t)size);
    out->ax = g.x;
    out->ay = g.y;
    out->ax2 = g.x2;
    out->ay2 = g.y2;
    out->w = g.w;
    out->h = g.h;
    out->x1 = g.x1;
    out->y1 = g.y1;
    out->advance = g.advance;
}

void chibi_rfont_metrics(void* handle, float* fheight, float* descent, float* spaceAdv) {
    ChibiFont* f = (ChibiFont*)handle;
    if (f != NULL && f->font != NULL) {
        *fheight = f->font->fheight;
        *descent = f->font->descent;
        *spaceAdv = f->font->space_adv;
    } else {
        *fheight = 0;
        *descent = 0;
        *spaceAdv = 0;
    }
}

uint8_t* chibi_rfont_atlas_pixels(void* handle) {
    ChibiFont* f = (ChibiFont*)handle;
    return f != NULL ? f->pixels : NULL;
}

uint32_t chibi_rfont_atlas_width(void* handle) {
    ChibiFont* f = (ChibiFont*)handle;
    return f != NULL ? f->width : 0;
}

uint32_t chibi_rfont_atlas_height(void* handle) {
    ChibiFont* f = (ChibiFont*)handle;
    return f != NULL ? f->height : 0;
}

int chibi_rfont_take_dirty(void* handle) {
    ChibiFont* f = (ChibiFont*)handle;
    int was;
    if (f == NULL) return 0;
    was = f->dirty;
    f->dirty = 0;
    return was;
}
