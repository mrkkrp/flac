/*
 * This file is part of ‘flac’ package.
 *
 * Copyright © 2016 Mark Karpov
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the name Mark Karpov nor the names of contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 * NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
                *((FLAC__uint8 *)obuffer + i * channels + ch)
                  = (FLAC__uint8)ibuffer[ch][i];
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
         * handy. Apparently we don't need to mask upper 8 bits because
         * libFLAC doesn't take them into account anyway. Good. */
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
