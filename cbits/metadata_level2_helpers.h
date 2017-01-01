/*
 * This file is part of ‘flac’ package.
 *
 * Copyright © 2016–2017 Mark Karpov
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

#ifndef FLAC__METADATA_LEVEL2_HELPERS_H
#define FLAC__METADATA_LEVEL2_HELPERS_H

#include <FLAC/format.h>
#include <FLAC/metadata.h>

/* Stream info */

unsigned FLAC__metadata_get_min_blocksize(FLAC__StreamMetadata *);
unsigned FLAC__metadata_get_max_blocksize(FLAC__StreamMetadata *);
unsigned FLAC__metadata_get_min_framesize(FLAC__StreamMetadata *);
unsigned FLAC__metadata_get_max_framesize(FLAC__StreamMetadata *);
unsigned FLAC__metadata_get_sample_rate(FLAC__StreamMetadata *);
unsigned FLAC__metadata_get_channels(FLAC__StreamMetadata *);
unsigned FLAC__metadata_get_bits_per_sample(FLAC__StreamMetadata *);
FLAC__uint64 FLAC__metadata_get_total_samples(FLAC__StreamMetadata *);
FLAC__byte *FLAC__metadata_get_md5sum(FLAC__StreamMetadata *);

/* Application */

FLAC__byte *FLAC__metadata_get_application_id(FLAC__StreamMetadata *);
FLAC__byte *FLAC__metadata_get_application_data(FLAC__StreamMetadata *, unsigned *);
void FLAC__metadata_set_application_id(FLAC__StreamMetadata *, FLAC__byte *);
FLAC__bool FLAC__metadata_set_application_data(FLAC__StreamMetadata *, FLAC__byte *, unsigned);

/* Seek table */

unsigned FLAC__metadata_get_seek_points_num(FLAC__StreamMetadata *);
FLAC__StreamMetadata_SeekPoint *FLAC__metadata_get_seek_point(FLAC__StreamMetadata *, unsigned);
void FLAC__metadata_set_seek_point(FLAC__StreamMetadata *, unsigned, FLAC__uint64, FLAC__uint64, unsigned);

/* Vorbis comment */

FLAC__byte *FLAC__metadata_get_vorbis_vendor(FLAC__StreamMetadata *, FLAC__uint32 *);
FLAC__bool FLAC__metadata_set_vorbis_vendor(FLAC__StreamMetadata *, FLAC__byte *, FLAC__uint32);
FLAC__byte *FLAC__metadata_get_vorbis_comment(FLAC__StreamMetadata *, const char*, FLAC__uint32 *);
FLAC__bool FLAC__metadata_set_vorbis_comment(FLAC__StreamMetadata *, FLAC__byte *, FLAC__uint32);
FLAC__bool FLAC__metadata_delete_vorbis_comment(FLAC__StreamMetadata *, const char *);
FLAC__bool FLAC__metadata_is_vorbis_comment_empty(FLAC__StreamMetadata *);

/* CUE sheet */

char *FLAC__metadata_get_cue_sheet_mcn(FLAC__StreamMetadata *);
FLAC__uint64 FLAC__metadata_get_cue_sheet_lead_in(FLAC__StreamMetadata *);
FLAC__bool FLAC__metadata_get_cue_sheet_is_cd(FLAC__StreamMetadata *);
FLAC__byte FLAC__metadata_get_cue_sheet_num_tracks(FLAC__StreamMetadata *);

FLAC__uint64 FLAC__metadata_get_cue_sheet_track_offset(FLAC__StreamMetadata *, FLAC__byte);
char *FLAC__metadata_get_cue_sheet_track_isrc(FLAC__StreamMetadata *, FLAC__byte);
FLAC__bool FLAC__metadata_get_cue_sheet_track_audio(FLAC__StreamMetadata *, FLAC__byte);
FLAC__bool FLAC__metadata_get_cue_sheet_track_preemphasis(FLAC__StreamMetadata *, FLAC__byte);
FLAC__byte FLAC__metadata_get_cue_sheet_track_num_indices(FLAC__StreamMetadata *, FLAC__byte);
FLAC__bool FLAC__metadata_get_cue_sheet_track_has_pregap_index(FLAC__StreamMetadata *, FLAC__byte);
FLAC__uint64 FLAC__metadata_get_cue_sheet_track_index(FLAC__StreamMetadata *, FLAC__byte, FLAC__byte);

void FLAC__metadata_set_cue_sheet_mcn(FLAC__StreamMetadata *, char *, unsigned);
void FLAC__metadata_set_cue_sheet_lead_in(FLAC__StreamMetadata *, FLAC__uint64);
void FLAC__metadata_set_cue_sheet_is_cd(FLAC__StreamMetadata *, FLAC__bool);

void FLAC__metadata_set_cue_sheet_track_offset(FLAC__StreamMetadata *, FLAC__byte, FLAC__uint64);
void FLAC__metadata_set_cue_sheet_track_number(FLAC__StreamMetadata *, FLAC__byte, FLAC__byte);
void FLAC__metadata_set_cue_sheet_track_isrc(FLAC__StreamMetadata *, FLAC__byte, char *, unsigned);
void FLAC__metadata_set_cue_sheet_track_audio(FLAC__StreamMetadata *, FLAC__byte, FLAC__bool);
void FLAC__metadata_set_cue_sheet_track_pre_emphasis(FLAC__StreamMetadata *, FLAC__byte, FLAC__bool);
void FLAC__metadata_set_cue_sheet_track_index(FLAC__StreamMetadata *, FLAC__byte, FLAC__byte, FLAC__byte, FLAC__uint64);

/* Picture */

FLAC__StreamMetadata_Picture_Type FLAC__metadata_get_picture_type(FLAC__StreamMetadata *);
char *FLAC__metadata_get_picture_mime_type(FLAC__StreamMetadata *);
FLAC__byte *FLAC__metadata_get_picture_description(FLAC__StreamMetadata *);
FLAC__uint32 FLAC__metadata_get_picture_width(FLAC__StreamMetadata *);
FLAC__uint32 FLAC__metadata_get_picture_height(FLAC__StreamMetadata *);
FLAC__uint32 FLAC__metadata_get_picture_depth(FLAC__StreamMetadata *);
FLAC__uint32 FLAC__metadata_get_picture_colors(FLAC__StreamMetadata *);
FLAC__byte *FLAC__metadata_get_picture_data(FLAC__StreamMetadata *, FLAC__uint32 *);

void FLAC__metadata_set_picture_type(FLAC__StreamMetadata *, FLAC__StreamMetadata_Picture_Type);
void FLAC__metadata_set_picture_width(FLAC__StreamMetadata *, FLAC__uint32);
void FLAC__metadata_set_picture_height(FLAC__StreamMetadata *, FLAC__uint32);
void FLAC__metadata_set_picture_depth(FLAC__StreamMetadata *, FLAC__uint32);
void FLAC__metadata_set_picture_colors(FLAC__StreamMetadata *, FLAC__uint32);

#endif /* FLAC__METADATA_LEVEL2_HELPERS_H */
