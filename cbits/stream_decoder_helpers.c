#include "stream_decoder_helpers.h"

static unsigned round_to_bytes(unsigned bits)
{
  return (bits + (bits % 8)) / 8;
}

static FLAC__StreamDecoderWriteStatus write_callback
  ( const FLAC__StreamDecoder *decoder
  , const FLAC__Frame *frame
  , const FLAC__int32 *const ibuffer[]
  , void *obuffer)
{
  FLAC__uint64 i, ch;
  unsigned channels = frame->header.channels;

  (void)decoder;

  switch (round_to_bytes(frame->header.bits_per_sample))
    {
      case 1:

        /* If sample width is equal or less than 8 bit, we deal with one
         * byte per sample and samples are unsigned as per WAVE spec. */
        for (i = 0; i < frame->header.blocksize; i++)
          {
            for (ch = 0; ch < channels; ch++)
              {
                /* Shift back into unsigned values by adding 0x7f. */
                *((FLAC__uint8 *)obuffer + i * channels + ch)
                  = (FLAC__uint8)ibuffer[ch][i] + 0x80;
              }
          }
        break;

      case 2:

        /* Here we have signed samples, each having width equal to 16
         * bits. */
        for (i = 0; i < frame->header.blocksize; i++)
          {
            for (ch = 0; ch < channels; ch++)
              {
                *((FLAC__int16 *)obuffer + i * channels + ch)
                  = (FLAC__int16)ibuffer[ch][i];
              }
          }
        break;

      case 3:

        /* Singed 24 bit samples. Going with 3 bytes step is not so
         * handy. */
        for (i = 0; i < frame->header.blocksize; i++)
          {
            for (ch = 0; ch < channels; ch++)
              {
                *(FLAC__int32 *)((FLAC__byte *)obuffer + (i * channels + ch) * 3)
                  = (FLAC__int32)ibuffer[ch][i];
              }
          }
        break;
    }

  return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

static void error_callback
  ( const FLAC__StreamDecoder *decoder
  , FLAC__StreamDecoderErrorStatus status
  , void *client_data )
{
  (void)decoder, (void)status, (void)client_data;
  return;
}

FLAC__StreamDecoderInitStatus FLAC__stream_decoder_init_helper
  ( FLAC__StreamDecoder *decoder
  , char *ifile_name
  , void *buffer )
{
  return FLAC__stream_decoder_init_file
    (decoder, ifile_name, write_callback, NULL, error_callback, buffer);
}
