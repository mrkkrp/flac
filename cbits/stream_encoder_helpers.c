#include "stream_encoder_helpers.h"

static unsigned round_to_bytes(unsigned bits)
{
  return (bits + (bits % 8)) / 8;
}

FLAC__bool FLAC__stream_encoder_process_helper
  ( FLAC__StreamEncoder *encoder
  , FLAC__uint64 data_offset
  , FLAC__uint64 data_size
  , const char *ifile_name )
{
  unsigned channels = FLAC__stream_encoder_get_channels(encoder);
  unsigned bits_per_sample = FLAC__stream_encoder_get_bits_per_sample(encoder);
  unsigned msw = round_to_bytes(bits_per_sample); /* mono sample width */
  unsigned block_align = channels * msw;
  FLAC__uint64 samples_to_process = data_size / block_align;
  FLAC__uint64 read_size = 4096;
  void *buffer_raw = malloc(read_size * block_align + 1);
  FLAC__int32 *buffer = malloc(read_size * sizeof(FLAC__int32) * channels);
  FILE *ifile = fopen(ifile_name, "r");
  FLAC__uint64 samples;
  unsigned i;
  FLAC__int32 x;

  if (data_size % block_align)
    {
      free(buffer_raw);
      free(buffer);
      fclose(ifile);
      return false;
    }

  /* move position indicator to beginning of audio data */
  fseek(ifile, data_offset, SEEK_SET);

  while (samples_to_process)
    {
      /* The reading happens by blocks. Every block has read_size samples in
       * it (multi-channel samples, that is). Since we will be using the
       * value returned by fread as indicator of whether something went
       * wrong or not, we need to know beforehand how many samples we need
       * to read. */
      samples = read_size <= samples_to_process ? read_size : samples_to_process;

      /* Read the samples into the “raw” buffer, fail by returning false if
       * number of read samples differs from the expected. */
      if (fread(buffer_raw, block_align, samples, ifile) != samples)
        {
          free(buffer_raw);
          free(buffer);
          fclose(ifile);
          return false;
        }

      /* libFLAC wants samples as FLAC__int32 values, so we need to copy
       * the data into an array of FLAC__int32 values, because what we have
       * read so far probably has different sample width. */

      switch (msw)  /* mono sample width in bytes */
        {
          case 1:

            /* If sample width is equal or less than 8 bit, we deal with one
             * byte per sample and samples are unsigned as per WAVE spec. */
            for (i = 0; i < samples * channels; i++)
              {
                /* Need to center the range at 0 and use signed integer as
                 * per FLAC docs. */
                *(buffer + i) = *((FLAC__uint8 *)buffer_raw + i) - 0x80;
              }
            break;

          case 2:

            /* Here we have signed samples, each having width equal to 16
             * bits. */
            for (i = 0; i < samples * channels; i++)
              {
                /* FIXME Only works on little-endian architectures. */
                *(buffer + i) = *((FLAC__int16 *)buffer_raw + i);
              }
            break;

          case 3:

            /* Singed 24 bit samples. Going with 3 bytes step is not so
             * handy. */
            for (i = 0; i < samples * channels; i++)
              {
                /* FIXME Only works on little-endian architectures. */
                x = *(FLAC__int32 *)((FLAC__byte *)buffer_raw + i * 3);
                if (x & 0x800000) /* do sign extension */
                  x |= 0xff000000; /* negative */
                else
                  x &= 0x00ffffff; /* positive */
                *(buffer + i) = x;
              }
            break;

        default:

          /* Have something else? You are screwed. */
          free(buffer_raw);
          free(buffer);
          fclose(ifile);
          return false;
        }

      /* Finally the easy part: call FLAC encoder function and process the
       * block of data. */
      if (!FLAC__stream_encoder_process_interleaved(encoder, buffer, samples))
        {
          free(buffer_raw);
          free(buffer);
          fclose(ifile);
          return false;
        }
      samples_to_process -= samples;
    }

  free(buffer_raw);
  free(buffer);
  fclose(ifile);
  return true;
}
