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

#include "metadata_level2_helpers.h"

/* Stream info */

unsigned FLAC__metadata_get_min_blocksize(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.min_blocksize;
}

unsigned FLAC__metadata_get_max_blocksize(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.max_blocksize;
}

unsigned FLAC__metadata_get_min_framesize(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.min_framesize;
}

unsigned FLAC__metadata_get_max_framesize(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.max_framesize;
}

unsigned FLAC__metadata_get_sample_rate(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.sample_rate;
}

unsigned FLAC__metadata_get_channels(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.channels;
}

unsigned FLAC__metadata_get_bits_per_sample(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.bits_per_sample;
}

FLAC__uint64 FLAC__metadata_get_total_samples(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.total_samples;
}

FLAC__byte *FLAC__metadata_get_md5sum(FLAC__StreamMetadata *block)
{
  return block->data.stream_info.md5sum;
}

/* Application */

FLAC__byte *FLAC__metadata_get_application_id(FLAC__StreamMetadata *block)
{
  return block->data.application.id;
}

FLAC__byte *FLAC__metadata_get_application_data
  (FLAC__StreamMetadata *block, unsigned *length)
{
  *length = block->length - 4;
  return block->data.application.data;
}

void FLAC__metadata_set_application_id
  (FLAC__StreamMetadata *block, FLAC__byte *id)
{
  unsigned i;
  for (i = 0; i < 4; i++)
    *(block->data.application.id + i) = *(id + i);
}

FLAC__bool FLAC__metadata_set_application_data
  (FLAC__StreamMetadata *block, FLAC__byte *data, unsigned length)
{
  return FLAC__metadata_object_application_set_data(block, data, length, true);
}

/* Vorbis comment */

FLAC__byte *FLAC__metadata_get_vorbis_vendor
  (FLAC__StreamMetadata *block, FLAC__uint32 *length)
{
  *length = block->data.vorbis_comment.vendor_string.length;
  return block->data.vorbis_comment.vendor_string.entry;
}

FLAC__bool FLAC__metadata_set_vorbis_vendor
  (FLAC__StreamMetadata *block, FLAC__byte *entry, FLAC__uint32 length)
{
  FLAC__StreamMetadata_VorbisComment_Entry e;
  e.length = length;
  e.entry  = entry;
  return FLAC__metadata_object_vorbiscomment_set_vendor_string(block, e, true);
}

FLAC__byte *FLAC__metadata_get_vorbis_comment
  (FLAC__StreamMetadata *block, const char *name, FLAC__uint32 *length)
{
  int i;
  FLAC__StreamMetadata_VorbisComment_Entry *e;
  i = FLAC__metadata_object_vorbiscomment_find_entry_from(block, 0, name);
  if (i == -1)
    {
      return NULL;
    }
  else
    {
      e = block->data.vorbis_comment.comments + i;
      *length = e->length;
      return e->entry;
    }
}

FLAC__bool FLAC__metadata_set_vorbis_comment
  (FLAC__StreamMetadata *block, FLAC__byte *entry, FLAC__uint32 length)
{
  FLAC__StreamMetadata_VorbisComment_Entry e;
  e.length = length;
  e.entry = entry;
  return FLAC__metadata_object_vorbiscomment_replace_comment(block, e, true, true);
}

FLAC__bool FLAC__metadata_delete_vorbis_comment
  (FLAC__StreamMetadata *block, const char *name)
{
  int i;
  i = FLAC__metadata_object_vorbiscomment_find_entry_from(block, 0, name);
  if (i >= 0)
    {
      return FLAC__metadata_object_vorbiscomment_delete_comment(block, i);
    }
  else
    {
      return true;
    }
}

FLAC__bool FLAC__metadata_is_vorbis_comment_empty(FLAC__StreamMetadata *block)
{
  return (block->data.vorbis_comment.vendor_string.length == 0) &&
    (block->data.vorbis_comment.num_comments == 0);
}
