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

/* Seek table */

unsigned FLAC__metadata_get_seek_points_num(FLAC__StreamMetadata *block)
{
  return block->data.seek_table.num_points;
}

FLAC__StreamMetadata_SeekPoint *FLAC__metadata_get_seek_point
  (FLAC__StreamMetadata *block, unsigned point_num)
{
  return block->data.seek_table.points + point_num;
}

void FLAC__metadata_set_seek_point
  ( FLAC__StreamMetadata *block
  , unsigned point_num
  , FLAC__uint64 sample_number
  , FLAC__uint64 stream_offset
  , unsigned frame_samples )
{
  FLAC__StreamMetadata_SeekPoint sp;
  sp.sample_number = sample_number;
  sp.stream_offset = stream_offset;
  sp.frame_samples = frame_samples;
  FLAC__metadata_object_seektable_set_point(block, point_num, sp);
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

/* CUE sheet */

char *FLAC__metadata_get_cue_sheet_mcn(FLAC__StreamMetadata *block)
{
  return (block->data.cue_sheet.media_catalog_number);
}

FLAC__uint64 FLAC__metadata_get_cue_sheet_lead_in(FLAC__StreamMetadata *block)
{
  return (block->data.cue_sheet.lead_in);
}

FLAC__bool FLAC__metadata_get_cue_sheet_is_cd(FLAC__StreamMetadata *block)
{
  return (block->data.cue_sheet.is_cd);
}

FLAC__byte FLAC__metadata_get_cue_sheet_num_tracks(FLAC__StreamMetadata *block)
{
  return (block->data.cue_sheet.num_tracks);
}

FLAC__uint64 FLAC__metadata_get_cue_sheet_track_offset
  (FLAC__StreamMetadata *block, FLAC__byte n)
{
  return (block->data.cue_sheet.tracks + n)->offset;
}

char *FLAC__metadata_get_cue_sheet_track_isrc
  (FLAC__StreamMetadata *block, FLAC__byte n)
{
  return (block->data.cue_sheet.tracks + n)->isrc;
}

FLAC__bool FLAC__metadata_get_cue_sheet_track_audio
  (FLAC__StreamMetadata *block, FLAC__byte n)
{
  return !(block->data.cue_sheet.tracks + n)->type;
}

FLAC__bool FLAC__metadata_get_cue_sheet_track_preemphasis
  (FLAC__StreamMetadata *block, FLAC__byte n)
{
  return (block->data.cue_sheet.tracks + n)->pre_emphasis;
}

FLAC__byte FLAC__metadata_get_cue_sheet_track_num_indices
  (FLAC__StreamMetadata *block, FLAC__byte n)
{
  return (block->data.cue_sheet.tracks + n)->num_indices;
}

FLAC__uint64 FLAC__metadata_get_cue_sheet_track_index
  (FLAC__StreamMetadata *block, FLAC__byte n, FLAC__byte i)
{
  return ((block->data.cue_sheet.tracks + n)->indices + i)->offset;
}

/* Picture */

FLAC__StreamMetadata_Picture_Type FLAC__metadata_get_picture_type
  (FLAC__StreamMetadata *block)
{
  return block->data.picture.type;
}

char *FLAC__metadata_get_picture_mime_type(FLAC__StreamMetadata *block)
{
  return block->data.picture.mime_type;
}

FLAC__byte *FLAC__metadata_get_picture_description(FLAC__StreamMetadata *block)
{
  return block->data.picture.description;
}

FLAC__uint32 FLAC__metadata_get_picture_width(FLAC__StreamMetadata *block)
{
  return block->data.picture.width;
}

FLAC__uint32 FLAC__metadata_get_picture_height(FLAC__StreamMetadata *block)
{
  return block->data.picture.height;
}

FLAC__uint32 FLAC__metadata_get_picture_depth(FLAC__StreamMetadata *block)
{
  return block->data.picture.depth;
}

FLAC__uint32 FLAC__metadata_get_picture_colors(FLAC__StreamMetadata *block)
{
  return block->data.picture.colors;
}

FLAC__byte *FLAC__metadata_get_picture_data
  (FLAC__StreamMetadata *block, FLAC__uint32 *length)
{
  *length = block->data.picture.data_length;
  return block->data.picture.data;
}

void FLAC__metadata_set_picture_type
  (FLAC__StreamMetadata *block, FLAC__StreamMetadata_Picture_Type picture_type)
{
  block->data.picture.type = picture_type;
}

void FLAC__metadata_set_picture_width
  (FLAC__StreamMetadata *block, FLAC__uint32 width)
{
  block->data.picture.width = width;
}

void FLAC__metadata_set_picture_height
  (FLAC__StreamMetadata *block, FLAC__uint32 height)
{
  block->data.picture.height = height;
}

void FLAC__metadata_set_picture_depth
  (FLAC__StreamMetadata *block, FLAC__uint32 depth)
{
  block->data.picture.depth = depth;
}

void FLAC__metadata_set_picture_colors
  (FLAC__StreamMetadata *block, FLAC__uint32 colors)
{
  block->data.picture.colors = colors;
}
