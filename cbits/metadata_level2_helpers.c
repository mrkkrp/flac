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

FLAC__bool FLAC__metadata_get_cue_sheet_track_has_pregap_index
  (FLAC__StreamMetadata *block, FLAC__byte n)
{
  return ((block->data.cue_sheet.tracks + n)->indices + 0)-> number == 0;
}

FLAC__uint64 FLAC__metadata_get_cue_sheet_track_index
  (FLAC__StreamMetadata *block, FLAC__byte n, FLAC__byte i)
{
  return ((block->data.cue_sheet.tracks + n)->indices + i)->offset;
}

void FLAC__metadata_set_cue_sheet_mcn
  (FLAC__StreamMetadata *block, char *source, unsigned length)
{
  unsigned i;
  char *target = block->data.cue_sheet.media_catalog_number;
  for (i = 0; i < 129; i++)
    {
      if (i < length)
        *(target + i) = *(source + i);
      else
        *(target + i) = 0;
    }
}

void FLAC__metadata_set_cue_sheet_lead_in
  (FLAC__StreamMetadata *block, FLAC__uint64 lead_in)
{
  block->data.cue_sheet.lead_in = lead_in;
}

void FLAC__metadata_set_cue_sheet_is_cd
  (FLAC__StreamMetadata *block, FLAC__bool is_cd)
{
  block->data.cue_sheet.is_cd = is_cd;
}

void FLAC__metadata_set_cue_sheet_track_offset
  (FLAC__StreamMetadata *block, FLAC__byte n, FLAC__uint64 offset)
{
  (block->data.cue_sheet.tracks + n)->offset = offset;
}

void FLAC__metadata_set_cue_sheet_track_number
  (FLAC__StreamMetadata *block, FLAC__byte n, FLAC__byte n_)
{
  (block->data.cue_sheet.tracks + n)->number = n_;
}

void FLAC__metadata_set_cue_sheet_track_isrc
  (FLAC__StreamMetadata *block, FLAC__byte n, char *source, unsigned length)
{
  unsigned i;
  char *target = (block->data.cue_sheet.tracks + n)->isrc;
  for (i = 0; i < 13; i++)
    {
      if (i < length)
        *(target + i) = *(source + i);
      else
        *(target + i) = 0;
    }
}

void FLAC__metadata_set_cue_sheet_track_audio
  (FLAC__StreamMetadata *block, FLAC__byte n, FLAC__bool audio)
{
  (block->data.cue_sheet.tracks + n)->type = !audio;
}

void FLAC__metadata_set_cue_sheet_track_pre_emphasis
  (FLAC__StreamMetadata *block, FLAC__byte n, FLAC__bool pre_emphasis)
{
  (block->data.cue_sheet.tracks + n)->pre_emphasis = pre_emphasis;
}

void FLAC__metadata_set_cue_sheet_track_index
  (FLAC__StreamMetadata *block, FLAC__byte n, FLAC__byte i, FLAC__byte i_, FLAC__uint64 offset)
{
  ((block->data.cue_sheet.tracks + n)->indices + i)->offset = offset;
  ((block->data.cue_sheet.tracks + n)->indices + i)->number = i_;
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
