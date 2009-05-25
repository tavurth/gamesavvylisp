//		Copyright (c) William Whitty 2009
//
//	This file is part of GSL. 
//
//     GSL is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
//
//     GSL is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.
//
//     You should have received a copy of the GNU General Public License
//     along with GSL.  If not, see <http://www.gnu.org/licenses/>.

//		INCLUDES/*{{{*/
#include "common.h"
#include <math.h>/*}}}*/

//		STRUCTS/*{{{*/

typedef struct {
	GLuint id;
        int width, height;
	short bpp;
       	int nUsed, n;		/* position of the Tex in texList 									(deylen - 14/5/2009)*/
}Tex;

struct {
	int motionX, motionY;
	Uint8 state;
}Mouse;

typedef struct {
	Tex ** letters;
	int nLetters;
	float size;
}FFont;

typedef struct {
	Uint32 id;
	int width, height;
	int n;
}Fbo;

typedef struct {
	Uint32 prog, vert, frag;
	int n;
}Shader;
/*}}}*/

//		LISTS/*{{{*/
Tex ** texList;
int nTex;

FFont ** fontList;
int nFonts;

Fbo ** fboList;
int nFbos;

Shader ** shaderList;
int nShaders;

GLuint * imgList;
int nImgs;
/*}}}*/

//		DEFINES/*{{{*/
/* Removing costly calls to sizeof() 														(deylen - 14/5/2009)*/
#define size_tex (sizeof(Tex))
#define size_tex_p (sizeof(Tex *))

#define size_ffont (sizeof(FFont))
#define size_ffont_p (sizeof(FFont *))

#define size_fbo (sizeof(Fbo))
#define size_fbo_p (sizeof(Fbo *))

#define size_shader (sizeof(Shader))
#define size_shader_p (sizeof(Shader *))

#define size_int (sizeof(int))

#define GSL_CATCH_MOUSE		0x00000000001
#define GSL_HIDE_MOUSE		0x00000000002
#define GSL_GET_MOUSE		0x00000000004
#define GSL_DEFAULT_VIDEO	0x00000000008

#define getAngle(x1, y1, x2, y2) (atan(((double) x2 - x1) / (y2 - y1)) + ((y2 >= y1) ? ((x2 > x1) ? 0 : twoPI) : M_PI))
#define PI 3.14159265
#define twoPI (PI * 2)
#define deg(x) ((x / twoPI) * 360)
#define rad(x) ((x / 360.0) * twoPI)
	
#define X 1
#define Y 2

/*}}}*/

//		GLOBALS/*{{{*/

Uint8 * keys;
Uint8 mods;

Uint32 WIDTH, HEIGHT;
Uint32 GSL_TYPING_KEY_REPEAT;
Uint32 GSL_TYPING_KEY_DELAY;
Uint32 GSL_SkipEvents;

Uint8  GSL_DONE_CLEAN;

FILE * GSL_UPDATE_FILE 	= NULL;
char * GSL_UPDATE_LOC  	= NULL;
int    GSL_LAST_VERSION	= 0;
Uint32 GSL_NEXT_UPDATE  = 0;

Uint32 OPTIONS;/*}}}*/

//			UTIL_FUNCTIONS/*{{{*/

void gsl_quit ();

void fatal (char * error) {
	printf("GSL-FATAL -> %s\n", error);
	gsl_quit();
}

void pfatal(char * error)
{
	perror(error);
	exit(1);
}

char * strcpyN(char * text, char * tocopy, int n)
{
	/* Copy n characters from <tocopy> into <text> incrementing both tocopy and text							(deylen - 14/5/2009)*/
	int a;
	char * p = text, *p2 = tocopy;

	for (a=0; a < n; a++)
	*(p++) = *(p2++);

	return text + a;
}

void * c_malloc(size_t t)
{
	void * p;

	if ((p = malloc(t)) == NULL) {
		printf("malloc failed on size %d\n", t);
		fatal("");
	}

	return p;
}

inline char * newStr(int n)
{
	return (char *) c_malloc(sizeof(char) * (n + 2));
}

inline char * newString(char * text){
	char * temp;
	temp = newStr(strlen(text));
	strcpy(temp, text);
	return temp;
}

char * getFileText(char * loc)
{
	FILE * f;

	if ((f = fopen(loc, "rb")) < 0)
		return NULL;
	
	fseek(f, 0, SEEK_END);
	int len = ftell(f);
	fseek(f, 0, SEEK_SET);

	char * data = newStr(len);

	data[fread(data, sizeof(char), len, f)] = '\0';
	fclose(f);

	return data;
}

void cleanAllFonts();
void cleanAllFbos();
void cleanAllShaders();
void cleanAllTextures();/*}}}*/

//			LIST_FUNCTIONS/*{{{*/

inline void ** newList(size_t t, int n)
{
	//function for creating new lists, Object list etc.											(deylen - 14/5/2009)

	void ** temp;

	if ((temp = (void **) malloc(t * (n + 1))) == NULL)
		pfatal("Could not create list, Out of memory");

	return temp;
}

inline void ** resizeList(void ** list, size_t t, int n)
{
	//same as newList, but for resizing;													(deylen - 14/5/2009)

	if ((list = (void **) realloc(list, t * (n + 1))) == NULL)
		pfatal("Could not resize list, Out of memory");
	return list;
}

void ** listAdd(void ** list, size_t t, void * p, int * len)
{
	//adds the pointer p to the list, then extends the list if needs be;									(deylen - 14/5/2009)

	void ** templist;

	if (!*len){
		templist = newList(t, ++(*len));
		templist[0] = p;
	}
	
	else {
		list[*len] = p;
		templist = resizeList(list, t, ++(*len));
	}

	return templist;
}
/*}}}*/

//			DRAW_FUNCTIONS				/*{{{*/

void drawImage(GLuint tex, int x, int y, int z, int w, int h)
{
	glPushMatrix();
	glTranslatef(x,y,z);
	glScalef(w, h, 0);
	glBindTexture(GL_TEXTURE_2D, tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0.01,0.01);	glVertex3f(0, 0, 0);
		glTexCoord2d(0.99,0.01);	glVertex3f(1, 0, 0);
		glTexCoord2d(0.99,0.99);	glVertex3f(1, 1, 0);
		glTexCoord2d(0.01,0.99);	glVertex3f(0, 1, 0);
	glEnd();
	glPopMatrix();
	glBindTexture(GL_TEXTURE_2D, 0);
}

void drawImageRepeat(GLuint tex, int x, int y, int w, int h, int sizex, int sizey)
{
	glPushMatrix();
	float repeatX = w * 1.0f / sizex, repeatY = h * 1.0f / sizey;

	glTranslatef(x,y,0);
	glScalef(w, h, 0);
	glBindTexture(GL_TEXTURE_2D, tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0,0);		glVertex3f(0, 0, 0);
		glTexCoord2d(repeatX,0);	glVertex3f(1, 0, 0);
		glTexCoord2d(repeatX,repeatY);	glVertex3f(1, 1, 0);
		glTexCoord2d(0,repeatY);	glVertex3f(0, 1, 0);
	glEnd();
	glPopMatrix();
}

void gsl_draw_tex (int id, int x, int y, int z, int w, int h) {
	drawImage(id, x, y, z, w, h);
}/*}}}*/

//			GSL_FUNCTIONS				/*{{{*/

void gsl_init_video (int width, int height, int bpp, Uint32 flags) {
	
	SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);

	if (!SDL_SetVideoMode(width, height, bpp, flags))
		fatal("Could not initialise SDL video mode");
	
	WIDTH = width; HEIGHT = height;
	if (OPTIONS & GSL_CATCH_MOUSE)
		SDL_WarpMouse(WIDTH / 2, HEIGHT / 2);

	if (OPTIONS & GSL_DEFAULT_VIDEO) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(80, (width * 1.0 / height), 0.1, 10000);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();

		glEnable(GL_BLEND);
		glBlendFunc(GL_ONE, GL_ZERO);
		return;
	}
}

void gsl_quit () {
	cleanAllShaders();
	cleanAllFbos();
	cleanAllFonts();
	cleanAllTextures();

	SDL_ShowCursor(1);
	SDL_Quit();
	exit(0);
}

void gsl_set_update_file (char * loc)
{
	GSL_UPDATE_LOC = newString(loc);
}

int gsl_should_load_updates () 
{
	Uint32 time = SDL_GetTicks();
	if (time < GSL_NEXT_UPDATE)
		return 0;
	GSL_NEXT_UPDATE = time + 1500;
	//Return if it is not time to do the next update;

	if ((GSL_UPDATE_FILE = fopen(GSL_UPDATE_LOC, "r")) != NULL) {
		fclose(GSL_UPDATE_FILE);
		GSL_UPDATE_FILE = NULL;
		return 1;
	}
	//If the file can be opened, we can assume it has lisp code inside as we delete it after each sucessful update.
	return 0;
}

void gsl_init (int flags, int options) {
	if (SDL_Init(flags) < 0)
		fatal("Could not initialise SDL");
	keys = SDL_GetKeyState(NULL);
	OPTIONS = options;

	if (OPTIONS & GSL_GET_MOUSE){
		OPTIONS |= GSL_HIDE_MOUSE;
		OPTIONS |= GSL_CATCH_MOUSE;
	}

	nTex = nFonts = nFbos = nShaders = nImgs = 0;

	GSL_DONE_CLEAN		= 0;
	GSL_TYPING_KEY_REPEAT   = 50;
	GSL_TYPING_KEY_DELAY    = 500;

	SDL_EventState(SDL_MOUSEMOTION, SDL_IGNORE);

	if (OPTIONS & GSL_HIDE_MOUSE)
		SDL_ShowCursor(0);
}
/*}}}*/

//			EVENT_FUNCTIONS				/*{{{*/

void gsl_pump_events () {
	SDL_PumpEvents();
	mods = SDL_GetModState();
	//Get the state of the keyboard modifiers;												(deylen - 14/5/2009)

	if (OPTIONS & GSL_CATCH_MOUSE) {
		Mouse.state = SDL_GetMouseState(&Mouse.motionX, &Mouse.motionY);

		Mouse.motionX -= (WIDTH / 2);
		Mouse.motionY -= (HEIGHT / 2);
		//Calculate the motion from how much we have moved since last frame (mouse was centered (WIDTH / 2, HEIGHT / 2) last frame);	(deylen - 14/5/2009)
		
		SDL_WarpMouse(WIDTH / 2, HEIGHT / 2);
		//Center the mouse for next frame calculations;											(deylen - 14/5/2009)
	}
}

int gsl_mouse_motion (short type) {
	return (type == X) ? Mouse.motionX : -Mouse.motionY;
}

int gsl_get_key (int key) {
	return keys[key];
}

char * gsl_get_charkey () {
	SDL_Event event;
	SDL_PollEvent(&event);

	static Uint32 lastKey;
	static Uint32 timeTillRepeat;
	Uint32 time = SDL_GetTicks();

	if (time < GSL_SkipEvents)
		return "";		//If we have chosen to skip some events return

	GSL_SkipEvents = 0;

	if (event.type == SDL_KEYDOWN) {
		if (event.key.keysym.sym != lastKey) {
			lastKey = event.key.keysym.sym;
			timeTillRepeat = time + GSL_TYPING_KEY_DELAY;
			return SDL_GetKeyName(event.key.keysym.sym);
		}
		//If the key is not the same as before, return the new key;	(deylen - 15/05/2009)
	}

	else if (event.type == SDL_KEYUP) {
		if (!keys[lastKey])
			lastKey = 0;
		//If we lift a key and the last key is no longer pressed make sure we notice it (deylen - 15/05/2009)
	}
	
	if (keys[lastKey]) {			//If a new key has not been pressed check if the lastKey is still held down
		if (time < timeTillRepeat)
			return "";
		timeTillRepeat = time + GSL_TYPING_KEY_REPEAT;
		return SDL_GetKeyName(lastKey);
	}

	return "";
}

void gsl_skip_events (int time) {
	GSL_SkipEvents = SDL_GetTicks() + time;
}

int gsl_get_mods () {
	return SDL_GetModState();
}/*}}}*/

//			USEFUL_FUNCTIONS			/*{{{*/

void gsl_draw_rect_shadow(int x, int y, int x1, int y1, int x2, int y2)
{
	double angle;
	int range = 500000;
	//This one might take some explaining, if you are confused ask and i'll try to draw a diagram :D					(deylen - 14/05/2009)

	glBegin(GL_POLYGON);
		//LEFT SIDE
		if (x <= x1){
			//TOP_LEFT
			if (y >= y2){
				angle = getAngle(x, y, x1,y1);
				glVertex2f(x1,y1);
				glVertex2f(x1 + sin(angle) * range, y + cos(angle) * range);
				
				angle = getAngle(x, y, x2, y2);
				glVertex2f(x2 + sin(angle) * range, y2 + cos(angle) * range);
				glVertex2f(x2, y2);
			}
			//BOTTOM_LEFT
			else if (y <= y1){
				angle = getAngle(x, y, x1,y2);
				glVertex2f(x1,y2);
				glVertex2f(x1 + sin(angle) * range, y2 + cos(angle) * range);
				
				angle = getAngle(x, y, x2, y1);
				glVertex2f(x2 + sin(angle) * range, y1 + cos(angle) * range);
				glVertex2f(x2, y1);
			}
			//LEFT
			else {
				angle = getAngle(x, y, x1,y1);
				glVertex2f(x1,y1);
				glVertex2f(x1 + sin(angle) * range, y1 + cos(angle) * range);
				
				angle = getAngle(x, y, x1,y2);
				glVertex2f(x1 + sin(angle) * range, y2 + cos(angle) * range);
				glVertex2f(x1,y2);
			}
		}
		//RIGHT SIDE
		else if (x >= x2) {
			//TOP_RIGHT
			if (y >= y2){
				angle = getAngle(x, y, x1,y2);
				glVertex2f(x1,y2);
				glVertex2f(x1 + sin(angle) * range, y2 + cos(angle) * range);
				
				angle = getAngle(x, y, x2, y1);
				glVertex2f(x2 + sin(angle) * range, y1 + cos(angle) * range);
				glVertex2f(x2, y1);
			}
			//BOTTOM_RIGHT
			else if (y <= y1){
				angle = getAngle(x, y, x2, y2);
				glVertex2f(x2, y2);
				glVertex2f(x2 + sin(angle) * range, y2 + cos(angle) * range);
				
				angle = getAngle(x, y, x1,y1);
				glVertex2f(x1 + sin(angle) * range, y1 + cos(angle) * range);
				glVertex2f(x1, y1);
			}
			//RIGHT
			else {
				angle = getAngle(x, y, x2, y2);
				glVertex2f(x2, y2);
				glVertex2f(x2 + sin(angle) * range, y2 + cos(angle) * range);
				
				angle = getAngle(x, y, x2, y1);
				glVertex2f(x2 + sin(angle) * range, y1 + cos(angle) * range);
				glVertex2f(x2, y1);
			}
		}
		//TOP AND BOTTOM
		else {
			//TOP
			if (y >= y2){
				angle = getAngle(x, y, x2, y2);
				glVertex2f(x2, y2);
				glVertex2f(x2 + sin(angle) * range, y2 + cos(angle) * range);
				
				angle = getAngle(x, y, x1,y2);
				glVertex2f(x1 + sin(angle) * range, y2 + cos(angle) * range);
				glVertex2f(x1,y2);
			}
			//BOTTOM
			else if (y <= y1){
				angle = getAngle(x, y, x2, y1);
				glVertex2f(x2, y1);
				glVertex2f(x2 + sin(angle) * range, y1 + cos(angle) * range);
				
				angle = getAngle(x, y, x1,y1);
				glVertex2f(x1 + sin(angle) * range, y1 + cos(angle) * range);
				glVertex2f(x1,y1);
			}
		}
	glEnd();
}/*}}}*/

//			TEXTURE_FUNCTIONS			/*{{{*/

#define WIDTH 1
#define HEIGHT 2
#define NUSED 3
#define POSITION 4

char * loadTGA(int * x, int *y, short * bpp, FILE * f);
GLuint loadGLTex(char * loc, int *x, int *y, short *bpp);

void cleanTexF(Tex * t)
{
	/*Force the cleaning of a tex 														(deylen - 14/5/2009)*/

//	#ifdef _DEBUG
//		printf("Freeing: %s\n", t->u->loc);
//	#endif
	glDeleteTextures(1, &t->id);
	free(t);
}

void delTex(Tex * t)
{
	if (--t->nUsed > 0)
		return;

	/* Delete a Tex from texList (deylen - 14/5/2009)*/
	int a = t->n;

	cleanTexF(t);

	if (nTex-- < 2)
		return;

	while (a++ < nTex){
		texList[a-1] = texList[a];
		texList[a]->n--;
	}
	
	if ((texList = (Tex **) realloc(texList, size_tex * nTex)) == NULL)
		pfatal("Failed to reallocate memory");
}

void cleanAllTextures()
{
	/* Force cleaning of all textures 													(deylen - 14/5/2009)*/
	int a = 0;
	if (nTex) {
		for (; a < nTex; a++)
			if (texList[a])			//this may have been cleaned earlier if we are quitting
				cleanTexF(texList[a]);
		free(texList);
		nTex = 0;
	}
	#ifdef _DEBUG
		printf("|\t--=>\tTexture clean\t----===>%5d freed\t|\n", a);
	#endif
}

void gsl_delete_tex (Tex * t) {
	delTex(t);
}

int * gsl_load_tex(char * loc)
{
	/*Creates and returns a new Texture												(deylen - 14/5/2009)*/
	int * temp = (int *) c_malloc(size_int * 333);
	short bpp = 0;
	temp[0] = loadGLTex(loc, &temp[1], &temp[2], &bpp); 

	return temp;
}

GLuint makeGLTex(char * data, int x, int y, int bpp)
{
	/* Make an OpenGL texture from data <data> and size (x y) 										(deylen - 14/5/2009)*/
	GLuint tex;

	glGenTextures(1, &tex);
	glBindTexture(GL_TEXTURE_2D, tex);
	glTexEnvf(GL_TEXTURE_2D, GL_TEXTURE_ENV, GL_BLEND);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);

	if (bpp == 4)
		gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, x, y, GL_RGBA, GL_UNSIGNED_BYTE, data);
	else
		gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB, x, y, GL_RGB, GL_UNSIGNED_BYTE, data);

	return tex;
}

GLuint loadGLTex(char * loc, int *x, int *y, short * bpp)
{
	/* Load the texture data from file 													(deylen - 14/5/2009)*/
	char * data;
	FILE * f;

	if ((f = fopen(loc, "rb")) == NULL){
		printf("Could not open file %s", loc);
		pfatal("");
	}
	
	if (strstr(loc, ".tga"))
		data = loadTGA(x, y, bpp, f);
	else
		pfatal("Sorry only TGA image format is supported at the moment");

	fclose(f);

	return makeGLTex(data, *x, *y, *bpp);
}

char * BGR_RGB(char * data, int len)
{
	/*flipping R & B pixels															(deylen - 14/5/2009)*/
	
	int a;
	char * p, c;

	for (a=0; a < len; a+=4){
		p = &data[a];

		c = *p;		//store the color in a temporary var										(deylen - 14/5/2009)
	*p = *(p+2);
	*(p+2) = c;	//restore the color from temp var										(deylen - 14/5/2009)
	}

	return data;
}

char * loadTGA(int * x, int *y, short * bpp, FILE * f)
{
	int size;
	char * data;
	unsigned short w, h;
	
	fseek(f, 12, SEEK_SET);
	fread(&w, 2, 1, f);
	fread(&h, 2, 1, f);
	fread(bpp, 1, 1, f);
	fseek(f, 18, SEEK_SET);
	*x = w; *y = h; *bpp /= 8;

	size = (*x) * (*y) * (*bpp);

	data = newStr(size);
	if (fread(data, sizeof(unsigned char), size, f) < size)
		pfatal("Could not read all of data from file. Make sure you have saved the Image without RLE compression\n");

	data = BGR_RGB(data, size);
	
	return data;
}/*}}}*/

//			FONT_TEXT_FUNCTIONS			/*{{{*/

Tex * newTexChar(char * data, int w, int h, int bpp)
{
	Tex * temp = (Tex *) c_malloc(size_tex);

	temp->id     =  makeGLTex(data, w, h, bpp);
	temp->width  =  w;
	temp->height =  h;
	temp->n      =  nTex;
	temp->nUsed  =  1;

	return temp;
}

Tex * newLetter(char * data, int x, int y, int size, int bpp, int w)
{
	/* Create a single letter														(deylen - 14/5/2009)*/

	int a;
	char * letterDat = newStr(size * (size * bpp));
	char * p, * p2 = letterDat;
	Tex * letter;

	p = data + (x * (size * bpp)) + (y * (size * (w * bpp)));
	/* Setting the start point of our texture 												(deylen - 14/05/2009)*/

	/* - - - - - - - - - - *
	 * - - - - _ _ - - - - *
	 * - - - |     | - - - *
	 * - - - |  A  | - - - *
	 * - - - | _ _ | - - - *
	 * - - - - - - - - - - *
	 * - - - - - - - - - - *
	 *
	 *   A is the area of the whole texture that we will extract for our letter 								(deylen - 14/05/2009)*/

	for (a=0; a < size; a++){
		p2 = strcpyN(p2, p, size * bpp);
		p += w * bpp;
	}/* copying the data into letterDat by way of pointer p2 										(deylen - 14/05/2009)*/
	/* add <w * bpp> to move to the next row of the texture 										(deylen - 14/05/2009)*/

	letter = newTexChar(letterDat, size, size, bpp);
	free(letterDat);

	return letter;
}

FFont * newFFont(char * loc)
{
	FFont * temp = (FFont *) c_malloc(sizeof(FFont));

	int w, h, size, x, y;
	short bpp = 0;
	char * data;
	FILE * f;
	Tex * letter;
	temp->nLetters = 0;

	if ((f = fopen(loc, "rb")) == NULL) {
		printf("Could not open %s.\n", loc);
		fatal("");
	}

	data = loadTGA(&w, &h, &bpp, f);
	size = w / 16;
	fclose(f);
	/* Loading the data from our font image													(deylen - 14/05/2009)*/

	for (y=0; y < 16; y++)
		for (x=0; x < 16; x++){
			letter = newLetter(data, x, 15 - y, size, bpp, w);
			temp->letters = (Tex **) listAdd((void **) temp->letters, size_tex_p, letter, &temp->nLetters);
		}
	/* Running through the image and creating each letter											(deylen - 14/05/2009)*/

	temp->size = size / 16.0;
	free(data);

	return temp;
}

int gsl_new_font (char * loc) {
	FFont * temp = newFFont(loc);

	fontList = (FFont **) listAdd((void **) fontList, size_ffont_p, temp, &nFonts);

	return nFonts-1;	/* -1 because listAdd automatically increments the list 							(deylen - 14/05/2009)*/
}

void cleanFFont(FFont * f)
{
	int a;
	for (a=0; a < f->nLetters; a++)
		cleanTexF(f->letters[a]);
	free(f->letters);
	free(f);

	#ifdef _DEBUG
		printf("|\t--=>\tLetters clean\t----===>%5d freed\t|\n", a);
	#endif
}

void cleanAllFonts() {
	int a = 0;
	if (nFonts) {
		for (; a < nFonts; a++)
			if (fontList[a])			//this may have been cleaned earlier if we are quitting
				cleanFFont(fontList[a]);
		free(fontList);
		nFonts = 0;
	}
	#ifdef _DEBUG
		printf("|\t--=>\tFFont clean\t----===>%5d freed\t|\n", a);
	#endif
}

void gsl_draw_char (int font, int character, int x, int y, int z, int w, int h)
{
	gsl_draw_tex(fontList[font]->letters[character]->id, x, y, z, w, h);
}/*}}}*/

//		FBO_FUNCTIONS/*{{{*/
#define GSL_FBO_COLOR_BUFFER 0
#define GSL_FBO_DEPTH_BUFFER 1

Uint32 gsl_new_framebuffer(int width, int height)
{
	if (!GL_ARB_framebuffer_object && !GL_EXT_framebuffer_object) {
		printf("Your graphics card has no framebuffer support!\n");
		return 0;
	}
	Fbo * f = (Fbo *) c_malloc (size_fbo);
	glGenFramebuffersEXT(1, (GLuint *) &f->id);
	f->width = width; f->height = height;

	f->n = nFbos;
	fboList = (Fbo **) listAdd((void **) fboList, size_fbo_p, f, &nFbos);

	return f->n; 
}

void gsl_use_framebuffer (int id) {
	//If the user calls gsl_use_framebuffer with an integer in clisp it then calls this function
	//passing -1. we then unbind the framebuffer.
	//else we bind the framebuffer id sent.
	(id >= 0) ? glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboList[id]->id) : glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
}

void gslFramebufferGetError()
{
	GLuint result;
	if ((result = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT)) != GL_FRAMEBUFFER_COMPLETE_EXT) {
		switch (result) {
			case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
			       printf("OpenGL Framebuffer error -> GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT\n");
			       break;
			case GL_FRAMEBUFFER_UNSUPPORTED:
			       printf("OpenGL Framebuffer error -> GL_FRAMEBUFFER_UNSUPPORTED\n");
			       break;
			default:
			       printf("OpenGL Framebuffer error unknown:\t%x\n", result);
			       break;
		}
		fatal("Could not create color attachment for framebuffer");
	}
}

Uint32 fboAddColorBuffer(Fbo * f, int pos)
{
	if (pos > 9) {
		printf("Could not locate position of color buffer\n");
		return 0;
	}
	GLuint colorBuffer = GL_COLOR_ATTACHMENT0_EXT + pos;	//Calculate the Id of the color buffer
	GLuint img;

	glGenTextures(1, &img);
	glBindTexture(GL_TEXTURE_2D, img);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, f->width, f->height, 0, GL_RGBA, GL_UNSIGNED_INT, NULL);
	//Set the texture options and create the texture

	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, f->id);
	glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, colorBuffer, GL_TEXTURE_2D, img, 0);
	//Bind the texture to the framebuffer

	gslFramebufferGetError();
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
	//Unbind the framebuffer

	return img;
}

Uint32 fboAddDepthBuffer(Fbo * f)
{
	GLuint depth;
	glGenRenderbuffersEXT(1, (GLuint *) &depth);

	glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, (GLuint) depth);
	glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, f->width, f->height); 
	//Creating our depth buffer;
	
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, (GLuint) f->id);
	glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, (GLuint) depth);

	gslFramebufferGetError();
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

	return depth;
}

Uint32 gsl_fbo_add(int id, int type, Uint32 pos)
{
	if (id > nFbos) {
		printf("Invalid fbo\n");
		return 0;
	}

	Fbo * f = fboList[id];
	switch (type) {
		case GSL_FBO_COLOR_BUFFER:
			return fboAddColorBuffer(f, pos);

		case GSL_FBO_DEPTH_BUFFER:
			return fboAddDepthBuffer(f);

		default:
			printf("Unknown buffer type\n");
			return 0;
	}
}

void gsl_fbo_del(GLuint id)
{
	glDeleteFramebuffers(1, (GLuint *) &fboList[id]->id);
	free(fboList[id]);
}

void cleanAllFbos() {
	int a = 0;
	return;
	if (nFbos) {
		for (; a < nFbos; a++)
			if (fboList[a])			//this may have been cleaned earlier if we are quitting
				gsl_fbo_del(a);
		free(fboList);
		nFbos = 0;
	}
	#ifdef _DEBUG
		printf("|\t--=>\tFbo clean\t----===>%5d freed\t|\n", a);
	#endif
}

/*}}}*/

//		SHADER_FUNCTIONS/*{{{*/

GLuint loadShader(char * loc, GLenum type, GLuint * program)
{
	GLint status, shader = glCreateShader(type);
	char * data = getFileText(loc);

	glShaderSource(shader, 1, (const GLchar **) &data, NULL);
	glCompileShader(shader);

	if (!*program)
		*program = glCreateProgram();

	glAttachShader(*program, shader);
	glLinkProgram(*program);
	glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
	if (status == GL_FALSE) {
		printf("\nCould not compile %s\n", loc);
		return 0;
	}

	return shader;
}

Uint32 gsl_new_shader(char * vert, char * frag)
{
	Shader * temp = (Shader *) c_malloc(size_shader);
	temp->prog = 0;

	if (vert)
		temp->vert = loadShader(vert, GL_VERTEX_SHADER, (GLuint *) &temp->prog);
	if (frag)
		temp->frag = loadShader(frag, GL_FRAGMENT_SHADER, (GLuint *) &temp->prog);
	
	temp->n = nShaders;
	shaderList = (Shader **) listAdd((void **) shaderList, size_shader_p, temp, &nShaders);

	return temp->n;
}

void gsl_shader_source(int gsl_shader, int type, char * loc)
{
	#define GSL_SHADER_VERT 0
	#define GSL_SHADER_FRAG 1
	
	//Change the source code of an already created gsl-shader

	char * data;
	GLuint shader;
       
	if ((data = getFileText(loc)) == NULL) {
		printf("Could not load file %s\n", loc);
		return;
	}

	switch (type) {
		case GSL_SHADER_VERT:
			shader = shaderList[gsl_shader]->frag;
			break;

		case GSL_SHADER_FRAG:
			shader = shaderList[gsl_shader]->vert;
			break;

		default:
			printf("Invalid shader type\n");
			return;
	}
	glShaderSource(shader, 1, (const GLchar **) &data, NULL);
	glCompileShader(shader);

	GLint status;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
	if (status == GL_FALSE)
		printf("Could not compile %s\n", loc);
}

void gsl_use_shader(int shader) {
	glUseProgram(shaderList[shader]->prog);
}

void gsl_delete_shader(int shader) {
	glDeleteProgram(shaderList[shader]->prog);
	glDeleteShader(shaderList[shader]->vert);
	glDeleteShader(shaderList[shader]->frag);

	free(shaderList[shader]);
	shaderList[shader] = NULL;
}

void cleanAllShaders() {
	int a = 0;
	if (nShaders) {
		for (; a < nShaders; a++)
			if (shaderList[a])			//this may have been cleaned earlier if we are quitting
				gsl_delete_shader(a);
		free(shaderList);
		nShaders = 0;
	}
	#ifdef _DEBUG
		printf("|\t--=>\tShader clean\t----===>%5d freed\t|\n", a);
	#endif
}
/*}}}*/
