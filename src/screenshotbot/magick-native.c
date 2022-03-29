#include <stdio.h>
#include <MagickWand/MagickWand.h>

typedef struct _pixel {
        size_t x;
        size_t y;
} pixel;


extern size_t screenshotbot_find_non_transparent_pixels(MagickWand* wand, pixel* output, size_t max) {
        max--;
        PixelIterator* iterator = NewPixelIterator(wand);
        size_t ret = 0;
        size_t height = MagickGetImageHeight(wand);
        for (int y = 0; y < height; ++y) {
                size_t width = 0;
                PixelWand** row = PixelGetNextIteratorRow(iterator, &width);
                for (int x = 0; x < width; x++) {
                        Quantum px = PixelGetAlphaQuantum(row[x]);
                        if (px > 100) {
                                output[ret].x = x;
                                output[ret].y = y;
                                ret++;

                                if (ret >= max) {
                                        goto cleanup;
                                }
                        }
                }
        }

        cleanup:
        DestroyPixelIterator(iterator);
        return ret;
}
