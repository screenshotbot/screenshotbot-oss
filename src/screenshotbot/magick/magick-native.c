#include <stdio.h>
#include <string.h>

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

extern size_t
screenshotbot_find_non_transparent_pixels_with_masks
(MagickWand* wand, struct mask* masks, size_t numMasks, pixel* output, size_t max) {
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

extern size_t screenshotbot_find_non_transparent_pixels(MagickWand* wand, pixel* output, size_t max) {
		return screenshotbot_find_non_transparent_pixels_with_masks(
				wand,
				NULL,
				0,
				output,
				max);
}
