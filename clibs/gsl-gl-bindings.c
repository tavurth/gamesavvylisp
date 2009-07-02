//		Copyright (c) William Whitty 2009
//
//	This file is part of GSL. 
//
//     GSL is free software: you can redistribute it and/or modify
//     it under the terms of the GNU Lesser General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
//
//     GSL is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU Lesser General Public License for more details.
//
//     You should have received a copy of the GNU Lesser General Public License
//     along with GSL.  If not, see <http://www.gnu.org/licenses/>.

#include "common.h"

void fatal (char * error) {
	printf("%s\n", error);
	exit(1);
}

void gl_matrix_mode(GLenum mode) {
	glMatrixMode(mode);
}

void gl_load_identity() {
	glLoadIdentity();
}

void gl_clear(int flags) {
	glClear(flags);
}

void gl_clear_color(int r, int g, int b, int a) {
	glClearColor(r, g, b, a);
}

void gl_translate (float x, float y, float z) {
	glTranslatef(x, y, z);
}

void gl_bind_renderbuffer(int id) {
	glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, id);
}

void gl_rotate (float amount, float x, float y, float z) {
	glRotatef(amount, x, y, z);
}

void gl_begin (int flags) {
	glBegin(flags);
}

void gl_end () {
	glEnd();
}

void gl_light (int light, int pname, float param) {
	glLightf(light, pname, param);
}

void gl_light_fv (int light, int pname, float * params) {
	glLightfv(light, pname, params);
}

void gl_material(int face, int pname, float param) {
	glMaterialf(face, pname, param);
}

void gl_material_fv(int face, int pname, float * params) {
	glMaterialfv(face, pname, params);
}

void gl_vertex_3f(float x, float y, float z) {
	glVertex3f(x, y, z);
}

void swap_buffers () {
	SDL_GL_SwapBuffers();
}

void gl_color_4f (float r, float g, float b, float a) {
	glColor4f(r, g, b, a);
}

void gl_enable (int flags) {
	glEnable(flags);
}

void gl_disable (int flags) {
	glDisable(flags);
}

void gl_push_matrix() {
	glPushMatrix();
}

void gl_pop_matrix() {
	glPopMatrix();
}

void gl_push_attrib(int mask) {
	glPushAttrib(mask);
}

void gl_pop_attrib() {
	glPopAttrib();
}

void gl_bind_texture(int target, int tex) {
	glBindTexture(target, tex);
}

void gl_blend_func (int sfactor, int dfactor) {
	glBlendFunc(sfactor, dfactor);
}

void gl_delete_texture(int id) {
	glDeleteTextures(1, (GLuint *) &id);
}

void gl_viewport (int x, int y, int w, int h) {
	glViewport(x, y, w, h);
}

void gl_stencil_op (int sfail, int dfail, int sdpass) {
	glStencilOp(sfail, dfail, sdpass);
}

void gl_stencil_func (int func, int ref, int mask) {
	glStencilFunc(func, ref, mask);
}

void gl_color_mask(short r, short g, short b, short a) {
	glColorMask(r, g, b, a);
}

void glu_perspective(int fov, float aspect, float near_clip, float far_clip) {
	//gluPerspective(fov, aspect, near_clip, far_clip);
	gluPerspective(75, (1024.0 / 512.0), 0.1, 10000);
}
