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
