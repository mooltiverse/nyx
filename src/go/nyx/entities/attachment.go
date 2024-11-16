/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package entities

/*
This object models a MIME attachment.

It can be used as a reference to local or remote files, links etc.
*/
type Attachment struct {
	// The attachment file name.
	FileName *string `json:"fileName,omitempty" yaml:"fileName,omitempty"`

	// The attachment (short) description (or label).
	Description *string `json:"description,omitempty" yaml:"description,omitempty"`

	// The attachment path (local file or URL).
	Path *string `json:"path,omitempty" yaml:"path,omitempty"`

	// The attachment MIME type.
	Type *string `json:"type,omitempty" yaml:"type,omitempty"`
}

/*
Standard constructor.

Arguments are as follows:

- fileName the attachment file name.
- description the attachment (short) description (or label).
- attachmentType the attachment MIME type.
- path the attachment path (local file or URL).
*/
func NewAttachmentWith(fileName *string, description *string, path *string, attachmentType *string) *Attachment {
	attachment := Attachment{}

	attachment.FileName = fileName
	attachment.Description = description
	attachment.Path = path
	attachment.Type = attachmentType

	return &attachment
}

/*
Returns the attachment file name.
*/
func (a *Attachment) GetFileName() *string {
	return a.FileName
}

/*
Sets the attachment file name.
*/
func (a *Attachment) SetFileName(fileName *string) {
	a.FileName = fileName
}

/*
Returns the attachment (short) description (or label).
*/
func (a *Attachment) GetDescription() *string {
	return a.Description
}

/*
Sets the attachment (short) description (or label).
*/
func (a *Attachment) SetDescription(description *string) {
	a.Description = description
}

/*
Returns the attachment path (local file or URL).
*/
func (a *Attachment) GetPath() *string {
	return a.Path
}

/*
Sets the attachment path (local file or URL).
*/
func (a *Attachment) SetPath(path *string) {
	a.Path = path
}

/*
Returns the attachment path (local file or URL).
*/
func (a *Attachment) GetType() *string {
	return a.Type
}

/*
Sets the attachment path (local file or URL).
*/
func (a *Attachment) SetType(attachmentType *string) {
	a.Type = attachmentType
}
