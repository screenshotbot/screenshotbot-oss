#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#if __has_include("MagickWand/MagickWand.h")
# include <MagickWand/MagickWand.h>
#else
# include <wand/MagickWand.h>
#endif

typedef struct _pixel {
        size_t x;
        size_t y;
} pixel;

#if MagickLibVersion >= 0x700
#define IF7(v1,v2) v1
#else
#define IF7(v1,v2) v2
#endif


#define MAX_QUANTUM ((1 << MAGICKCORE_QUANTUM_DEPTH) - 1)

extern int screenshotbot_verify_magick(CompositeOperator srcCompositeOp,
                                       IF7(AlphaChannelOption,AlphaChannelType) onAlphaChannel) {
        size_t depth;
        GetMagickQuantumDepth(&depth);
        if (MAGICKCORE_QUANTUM_DEPTH != depth) {
                return -1;
        }

        if (srcCompositeOp != SrcCompositeOp) {
                return -2;
        }

        if (onAlphaChannel != SetAlphaChannel) {
                return -3;
        }

        const char* features = GetMagickFeatures();
        char* feat = strstr(features, "HDRI");


#if MAGICKCORE_HDRI_ENABLE
        int hdriMatches = (feat != NULL);
#else
        int hdriMatches = (feat == NULL);
#endif
        if (!hdriMatches) {
                printf("HDRI setting does not match, original: %d. Features `%s`",
                       MAGICKCORE_HDRI_ENABLE,
                       features);
                return -4;
        }

        return 1;
}

struct mask {
        size_t x;
        size_t y;
        size_t width;
        size_t height;
};

/*
 * Set a pixel color. This is only really useful for tests.
 */
extern MagickBooleanType
screenshotbot_set_pixel(
        MagickWand *wand,
        const pixel *ppixel, // pointer just for FLI simplicity
        const char* color) {
        PixelIterator *iterator = NewPixelIterator(wand);
        MagickBooleanType ret = MagickFalse;
        size_t height = MagickGetImageHeight(wand);
        pixel pixel = *ppixel;

        if (pixel.y >= height) {
                printf("Invalid height: %zu\n", pixel.y);
                goto cleanup;
        }

        size_t width;
        PixelWand** row;
        for (int y = 0; y <= pixel.y; y++) {
                row = PixelGetNextIteratorRow(iterator, &width);
        }

        if (pixel.x >= width) {
                printf("Invalid width: %zu out of %zu\n", pixel.x, width);
                goto cleanup;
        }

        ret = PixelSetColor(row[pixel.x], color);
        if (!ret) {
                printf("PixelSetColor failed\n");
                goto cleanup;
        }

        ret = PixelSyncIterator(iterator);
cleanup:
        DestroyPixelIterator(iterator);
        return ret;
}

static int _mask_cmp(const void* p1, const void* p2) {
        const struct mask* m1 = (struct mask*) p1;
        const struct mask* m2 = (struct mask*) p2;

        return m1->x - m2->x;
}

static int in_range(int x, int start, int width) {
        return start <= x && x < (start + width);
}

/*
 * Returns a sorted list of masks that that intersect in the y
 * positions. The number that is returned is number of elements in output.
 */
static int filter_masks(struct mask* masks, struct mask* output, int numMasks, int y) {
        int ret = 0;
        for (int i = 0; i < numMasks; i++) {
                struct mask mask = masks[i];
                if (in_range(y, mask.y, mask.height)) {
                        output[ret++] = mask;
                }
        }

        qsort(output, ret, sizeof(struct mask), &_mask_cmp);
        return ret;
}

inline int _max(int a, int b) {
        return (a > b ? a : b);
}

inline int _min(int a, int b) {
        return (a < b ? a : b);
}

extern size_t
screenshotbot_find_non_transparent_pixels_with_masks
(MagickWand* wand, struct mask* masks, size_t numMasks, pixel* output, size_t max) {
        max--;
        PixelIterator* iterator = NewPixelIterator(wand);
        struct mask *tmp = malloc(numMasks * sizeof(struct mask));

        //printf("The top mask is: %zu, %zu\n", masks[0].y, masks[0].height);

        size_t ret = 0;
        size_t height = MagickGetImageHeight(wand);
        for (int y = 0; y < height; ++y) {
                int numLocalMasks = filter_masks(masks, tmp, numMasks, y);
                //printf("For %d, got %d local masks out of %zu\n", y, numLocalMasks, numMasks);
                int nextMask = 0;

                size_t width = 0;
                PixelWand** row = PixelGetNextIteratorRow(iterator, &width);

                for (int x = 0; x < width; x++) {

                        if (nextMask < numLocalMasks) {
                                if (x >= tmp[nextMask].x) {
                                        x = _max(x, tmp[nextMask].x + tmp[nextMask].width) - 1;
                                        nextMask++;
                                        //printf("Set x to %d\n", x);
                                        continue;
                                }
                        }

                        Quantum px = PixelGetAlphaQuantum(row[x]);

                        /*
                         * Currently the yellow that's drawn as part
                         * of the mask is ~204.  However, we also need
                         * to deal with a situation that the yellow is
                         * overlaid on the red pixel. So even though
                         * this function is named "get_non_alpha",
                         * it's actually "get_non_alpha" where green
                         * is 0.
                         */
                        if (px == MAX_QUANTUM && PixelGetGreenQuantum(row[x]) == 0) {
                                if (output) {
                                        output[ret].x = x;
                                        output[ret].y = y;
                                }
                                ret++;

                                if (ret >= max) {
                                        goto cleanup;
                                }
                        }
                }
        }

cleanup:
        free(tmp);
        DestroyPixelIterator(iterator);
        return ret;
}

extern size_t screenshotbot_find_non_transparent_pixels(MagickWand* wand, pixel* output, size_t max) {
        return screenshotbot_find_non_transparent_pixels_with_masks(
                wand,
                NULL,
                0,
                output,
                max);
}

static void draw_red(PixelWand* wand) {
        PixelSetAlphaQuantum(wand, QuantumRange);
        PixelSetRedQuantum(wand, QuantumRange);
        PixelSetGreenQuantum(wand, 0);
        PixelSetBlueQuantum(wand, 0);
}

static void draw_transparent(PixelWand* wand) {
        PixelSetAlphaQuantum(wand, 0);
        PixelSetRedQuantum(wand, 0);
        PixelSetGreenQuantum(wand, 0);
        PixelSetBlueQuantum(wand, 0);
}


static bool fill_row_with_red(int start, PixelWand** drow, size_t dwidth) {
        for (int i = start; i < dwidth; i++) {
                draw_red(drow[i]);
        }
        return start < dwidth;
}

static bool inplace_compare_rows_v2(PixelWand** drow, PixelWand** srow,
                                    size_t dwidth,
                                    size_t swidth,
                                    int dist) {
        int x;
        bool changed = false;
        int sq_dist = 4 * dist * dist;

#define CQ(name,i) (name(drow[i]) == name(srow[i]))
#define DIST(name,i) (((long)name(drow[i])) - ((long)name(srow[i])))

        for (x = 0; x < _min(dwidth, swidth); x++) {
                bool same;

                if (sq_dist == 0) {
                        same = CQ(PixelGetAlphaQuantum, x) &&
                                CQ(PixelGetRedQuantum, x) &&
                                CQ(PixelGetBlueQuantum, x) &&
                                CQ(PixelGetGreenQuantum, x);
                } else {
                        long a = DIST(PixelGetAlphaQuantum, x);
                        long r = DIST(PixelGetRedQuantum, x);
                        long g = DIST(PixelGetGreenQuantum, x);
                        long b = DIST(PixelGetBlueQuantum, x);
                        same = ((a*a + r*r + g*g + b*b) <= sq_dist);
                }
                if (same) {
                        draw_transparent(drow[x]);
                } else {
                        draw_red(drow[x]);
                        changed = true;
                }
        }

        if (fill_row_with_red(x, drow, dwidth)) {
                changed = true;
        }

        return changed;

}

/*
 * Compares dest to src, while modifying dest.  returns 0 if no
 * changes were detected. -1 on error, and 1 on success.
 *
 * dist: the distance between two pixels to be considered different.
 */
extern int screenshotbot_inplace_compare_v2(MagickWand* dest,
                                            MagickWand* src,
                                            int dist) {
        PixelIterator* diter = NULL;
        PixelIterator* siter = NULL;
        int changed = 0;


        diter = NewPixelIterator(dest);
        if (!diter) {
                changed = -1;
                goto cleanup;
        }

        siter = NewPixelIterator(src);
        if (!siter) {
                changed = -1;
                goto cleanup;
        }

        int processedRows = 0;
        //fprintf(stderr, "iterating through the inplace rows\n");
        while (true) {
                processedRows++;
                //fprintf(stderr, "Processing row: %d\n", processedRows);
                size_t dwidth = 0;
                PixelWand** drow = PixelGetNextIteratorRow(diter, &dwidth);

                size_t swidth = 0;
                PixelWand** srow = PixelGetNextIteratorRow(siter, &swidth);

                if (!drow) {
                        //fprintf(stderr, "Reached end at %d\n", processedRows);
                        break;
                } else if (srow) {
                        if (inplace_compare_rows_v2(drow, srow, dwidth, swidth, dist)) {
                                changed = 1;
                        }
                } else {
                        if (fill_row_with_red(0, drow, dwidth)) {
                                changed = 1;
                        }
                }

                // We always have to sync, since we're writing
                // transparent pixels too.
                if (!PixelSyncIterator(diter)) {
                        //fprintf(stderr, "Failed to sync iterator\n");
                        changed = -1;
                        goto cleanup;
                }
        }


        MagickSetImageColorspace(dest, RGBColorspace);

        if (MagickGetImageHeight(dest) < MagickGetImageHeight(src)) {
                changed = 1;
        }
cleanup:
        if (diter) DestroyPixelIterator(diter);
        if (siter) DestroyPixelIterator(siter);

        // If the destination is shorter, then we're not going to
        // process rest of the rows, so we should check that here.
        return changed;
        //return -processedRows;
}

extern int screenshotbot_inplace_compare(MagickWand* dest,
                                         MagickWand* src) {
        return screenshotbot_inplace_compare_v2(dest, src, 0);
}

extern MagickBooleanType
screenshotbot_resize(MagickWand *wand,
                     size_t width,
                     size_t height) {
        return MagickResizeImage(
                wand,
                width,
                height,
                /* this is the only reason we're compiling this */
                LanczosFilter
#if MagickLibVersion < 0x700
                ,1.0
#endif
         );
}
