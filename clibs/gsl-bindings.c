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

//		INCLUDES/*{{{*/
#include "common.h"
#include <math.h>/*}}}*/

//		STRUCTS/*{{{*/

typedef struct {
	GLuint id;
        int width, height;
	short bpp;
       	int nUsed, n;		/* position of the Tex in texList 			(deylen - 14/5/2009)*/
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
	int id;
	int width, height;
	int n;
}Fbo;

typedef struct {
	int prog, vert, frag;
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
/*}}}*/

//		DEFINES/*{{{*/
/* Removing costly calls to sizeof() 						(deylen - 14/5/2009)*/
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

#define PI 3.14159265
#define twoPI (PI * 2.0)
#define PI_div2 (PI / 2.0)
#define deg(x) ((x / twoPI) * 360)
#define rad(x) ((x / 360.0) * twoPI)
#define getDist(x1, y1, x2, y2) (sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)))
#define checkDivisors(a, b) ((!a || !b) ? 0 : (a/b))

#define X 1
#define Y 2

double getAngle (int x1, int y1, int x2, int y2);
/*}}}*/

//		GLOBALS/*{{{*/

Uint8 * keys;
Uint8 mods;

int WIDTH, HEIGHT;		//width and height of the screen
int GSL_TYPING_KEY_REPEAT;
int GSL_TYPING_KEY_DELAY;
int GSL_SkipEvents;

FILE * GSL_UPDATE_FILE 	= NULL;
char * GSL_UPDATE_LOC  	= NULL;	//This is the file updates are loaded from
Uint32 GSL_NEXT_UPDATE  = 0;

GLuint WRAP_S, WRAP_T, MAG_FILTER, MIN_FILTER;

void (* GSL_MOUSE_EVENT_FUNC)(int, int) = NULL;
void (* GSL_MOUSE_MOVE_FUNC) (int, int) = NULL;
void (* GSL_KEY_EVENT_FUNC)  (int, int) = NULL;
enum TYPE   { GSL_BUTTON_PRESS = 0, GSL_BUTTON_RELEASE, LAST_TYPE };

int OPTIONS;

void gsl_set_mouse_event_func (void * func) {
	GSL_MOUSE_EVENT_FUNC = func;
}

void gsl_set_mouse_move_func (void * func) {
	GSL_MOUSE_MOVE_FUNC = func;
}

void gsl_set_key_event_func (void * func) {
	GSL_KEY_EVENT_FUNC = func;
}

/*}}}*/

//			UTIL_FUNCTIONS				/*{{{*/

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
	/* Copy n characters from <tocopy> into <text> incrementing both tocopy and text			(deylen - 14/5/2009)*/
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

//			LIST_FUNCTIONS				/*{{{*/

inline void ** newList(size_t t, int n)
{
	//function for creating new lists, Object list etc.					(deylen - 14/5/2009)

	void ** temp;

	if ((temp = (void **) malloc(t * (n + 1))) == NULL)
		pfatal("Could not create list, Out of memory");

	return temp;
}

inline void ** resizeList(void ** list, size_t t, int n)
{
	//same as newList, but for resizing;					(deylen - 14/5/2009)

	if ((list = (void **) realloc(list, t * (n + 1))) == NULL)
		pfatal("Could not resize list, Out of memory");
	return list;
}

void ** listAdd(void ** list, size_t t, void * p, int * len)
{
	//adds the pointer p to the list, then extends the list if needs be;			(deylen - 14/5/2009)

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

void drawImage(GLuint tex, float x, float y, float z, float w, float h)
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

void drawImageRepeat(GLuint tex, float x, float y, float z, float w, float h, float sizex, float sizey)
{
	glPushMatrix();
	float repeatX = 0.99, repeatY = 0.99;
	if (sizex)
		repeatX = w * 1.0f / sizex;
	if (sizey)
		repeatY = h * 1.0f / sizey;

	glTranslatef(x,y,z);
	glScalef(w, h, 0);
	glBindTexture(GL_TEXTURE_2D, tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0.01,0.01);	glVertex3f(0, 0, 0);
		glTexCoord2d(repeatX,0.01);	glVertex3f(1, 0, 0);
		glTexCoord2d(repeatX,repeatY);	glVertex3f(1, 1, 0);
		glTexCoord2d(0.01,repeatY);	glVertex3f(0, 1, 0);
	glEnd();
	glPopMatrix();
	glBindTexture(GL_TEXTURE_2D, 0);
}

void gsl_draw_tex (int id, float x, float y, float z, float w, float h, float repeatX, float repeatY) {
	if (repeatX || repeatY)
		drawImageRepeat(id, x, y, z, w, h, repeatX, repeatY);
	else
		drawImage(id, x, y, z, w, h);
}

void gsl_draw_cube(int x, int y, int z, int sx, int sy, int sz, float anglex, float angley, float anglez, int centered,
		int left_tex, int right_tex, int front_tex, int back_tex, int top_tex, int bot_tex)
{
	glPushMatrix();

	glTranslatef(x, y, z);
	glRotatef(anglex, 1, 0, 0);
	glRotatef(angley, 0, 1, 0);
	glRotatef(anglez, 0, 0, 1);
	if (centered)
		glTranslatef(-sx/2.0, -sy/2.0, -sz/2.0);

	//Front
	glNormal3f(0, 0, -1);
	glBindTexture(GL_TEXTURE_2D, front_tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0.001, 0.001);	glVertex3f(0,  0,  0);
		glTexCoord2d(0.999, 0.001);	glVertex3f(sx, 0,  0);
		glTexCoord2d(0.999, 0.999);	glVertex3f(sx, sy, 0);
		glTexCoord2d(0.001, 0.999);	glVertex3f(0,  sy, 0);
	glEnd();

	//Right
	glNormal3f(1, 0, 0);
	glBindTexture(GL_TEXTURE_2D, front_tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0.001, 0.001);	glVertex3f(sx,  0,  0);
		glTexCoord2d(0.999, 0.001);	glVertex3f(sx,  0,  sz);
		glTexCoord2d(0.999, 0.999);	glVertex3f(sx,  sy, sz);
		glTexCoord2d(0.001, 0.999);	glVertex3f(sx,  sy, 0);
	glEnd();

	//Back
	glNormal3f(0, 0, 1);
	glBindTexture(GL_TEXTURE_2D, front_tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0.001, 0.001);	glVertex3f(sx,  0,  sz);
		glTexCoord2d(0.999, 0.001);	glVertex3f(0,   0,  sz);
		glTexCoord2d(0.999, 0.999);	glVertex3f(0,   sy, sz);
		glTexCoord2d(0.001, 0.999);	glVertex3f(sx,  sy, sz);
	glEnd();

	//Left
	glNormal3f(-1, 0, 0);
	glBindTexture(GL_TEXTURE_2D, front_tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0.001, 0.001);	glVertex3f(0,  0,  sz);
		glTexCoord2d(0.999, 0.001);	glVertex3f(0,  0,  0);
		glTexCoord2d(0.999, 0.999);	glVertex3f(0,  sy, 0);
		glTexCoord2d(0.001, 0.999);	glVertex3f(0,  sy, sz);
	glEnd();

	//Top
	glNormal3f(0, 1, 0);
	glBindTexture(GL_TEXTURE_2D, front_tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0.001, 0.001);	glVertex3f(0,  sy, 0);
		glTexCoord2d(0.999, 0.001);	glVertex3f(sx, sy, 0);
		glTexCoord2d(0.999, 0.999);	glVertex3f(sx, sy, sz);
		glTexCoord2d(0.001, 0.999);	glVertex3f(0,  sy, sz);
	glEnd();

	//Bottom
	glNormal3f(0, -1, 0);
	glBindTexture(GL_TEXTURE_2D, front_tex);
	glBegin(GL_QUADS);
		glTexCoord2d(0.001, 0.001);	glVertex3f(0,  0, 0);
		glTexCoord2d(0.999, 0.001);	glVertex3f(sx, 0, 0);
		glTexCoord2d(0.999, 0.999);	glVertex3f(sx, 0, sz);
		glTexCoord2d(0.001, 0.999);	glVertex3f(0,  0, sz);
	glEnd();

	glPopMatrix();
}

/*}}}*/

//			GSL_FUNCTIONS				/*{{{*/

void gsl_init_video (int width, int height, int bpp, int flags, int options, int fov, float near_clip, int far_clip) {
	
	//Setting up the stencil buffer for windows systems
	SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);
	OPTIONS |= options;

	if (!SDL_SetVideoMode(width, height, bpp, flags))
		fatal("Could not initialise SDL video mode");
	
	WIDTH = width; HEIGHT = height;
	if (OPTIONS & GSL_CATCH_MOUSE)
		SDL_WarpMouse(WIDTH / 2, HEIGHT / 2);

	if (OPTIONS & GSL_DEFAULT_VIDEO) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(fov, (width * 1.0 / height), near_clip, far_clip);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();

		glEnable(GL_BLEND);
		glBlendFunc(GL_ONE, GL_ZERO);
		return;
	}
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

	//Initialise lists
	nTex = nFonts = nFbos = nShaders = 0;

	//Set up our default texture settings
	WRAP_S     = GL_REPEAT;
	WRAP_T     = GL_REPEAT;
       	MAG_FILTER = GL_LINEAR;
       	MIN_FILTER = GL_LINEAR;

	GSL_TYPING_KEY_REPEAT   = 50;
	GSL_TYPING_KEY_DELAY    = 500;

	if (OPTIONS & GSL_HIDE_MOUSE)
		SDL_ShowCursor(0);
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
	//Sets the file where updates should be loaded from
	GSL_UPDATE_LOC = newString(loc);
}

int gsl_should_load_updates () 
{
	int time = SDL_GetTicks();
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
/*}}}*/

//			EVENT_FUNCTIONS				/*{{{*/

void gsl_pump_events () {
	//This is your standard way of processing events. If you need key by key events, or key names
	//take a look at gsl_get_charkey. 				        (deylen - 02/6/2009)

	Uint8 appState;
	SDL_Event e;

	while (SDL_PollEvent(&e)) {
		if (e.type == SDL_MOUSEBUTTONDOWN || e.type == SDL_MOUSEBUTTONUP) {
			if (GSL_MOUSE_EVENT_FUNC)
				GSL_MOUSE_EVENT_FUNC(e.button.button, e.button.type);
		}

		else if (e.type == SDL_KEYDOWN || e.type == SDL_KEYUP) {
			if (GSL_KEY_EVENT_FUNC)
				GSL_KEY_EVENT_FUNC(e.key.keysym.sym, e.key.type);
		}

		else if (e.type == SDL_MOUSEMOTION) {
			if (GSL_MOUSE_MOVE_FUNC)
				GSL_MOUSE_MOVE_FUNC(e.motion.xrel, -e.motion.yrel);
		}

		else if (e.type == SDL_QUIT) 
			gsl_quit();
	} //	Call the correct function when an event takes place

	while (1) {
		SDL_PumpEvents();
		//Get the state of the keyboard modifiers;				(deylen - 14/5/2009)
		mods = SDL_GetModState();

		//GSL specific keys							(deylen - 02/6/2009)
		if (keys[SDLK_ESCAPE] && (mods & KMOD_CTRL))
			gsl_quit();

		//Only continue if the main window is focused
		appState = SDL_GetAppState();
		if (appState & SDL_APPACTIVE && (appState & SDL_APPINPUTFOCUS || appState & SDL_APPMOUSEFOCUS))
			break;

		SDL_Delay(50);
	} //	Focus control

	if (OPTIONS & GSL_CATCH_MOUSE) {
		Mouse.state = SDL_GetMouseState(&Mouse.motionX, &Mouse.motionY);

		Mouse.motionX -= (WIDTH / 2);
		Mouse.motionY -= (HEIGHT / 2);
		//Calculate the motion from how much we have moved since last frame
		//(mouse was centered (WIDTH / 2, HEIGHT / 2) last frame);	(deylen - 14/5/2009)
		
		SDL_WarpMouse(WIDTH / 2, HEIGHT / 2);
		//Center the mouse for next frame calculations;			(deylen - 14/5/2009)
		SDL_PollEvent(&e);
		//Ignore the mouse motion event caused by the call to SDL_WarpMouse
	}
}

int gsl_mouse_motion (short type) {
	return (type == X) ? Mouse.motionX : -Mouse.motionY;
}

int gsl_get_key (int key) {
	//Get the status of <key>
	return keys[key];
}

void gsl_skip_events (int time) {
	//Skip all events for <time> ms 
	GSL_SkipEvents = SDL_GetTicks() + time;
}

int gsl_get_mods () {
	//Return the current state of all keyboard modifiers
	return SDL_GetModState();
}/*}}}*/

//			USEFUL_FUNCTIONS			/*{{{*/

void gsl_draw_rect_shadow(int lightPosX, int lightPosY, int x1, int y1, int x2, int y2)
{
	//Draw the shadows of a rect <x1 y1 x2 y2> from the light pos <lightPosX lightPosY> (2d only)
	
	//Cleaned up this code and added some more comments, if you are still confused send me a mail :)	(deylen - 02/06/2009)
	double angle;
	int range = 500000;
	//This one might take some explaining, if you are confused ask and i'll try to draw a diagram :D	(deylen - 14/05/2009)

	int targetX = 0, targetY = 0, targetX2 = 0, targetY2 = 0;
	//If the light is to the left of the rect
	if (lightPosX <= x1) {
		//If the light is to the top left of the rect
		if (lightPosY >= y2) { 
			//Bottom left of rect
			targetX = x1;
			targetY = y1;

			//Top left of rect
			targetX2 = x2;
			targetY2 = y2;
		}
		
		//Bottom left of rect
		else if (lightPosY <= y1) {
			targetX = x1;
			targetY = y2;

			targetX2 = x2;
			targetY2 = y1;
		}

		//Left of rect
		else {
			targetX = x1;
			targetY = y2;

			targetX2 = x1;
			targetY2 = y1;
		}
	}

	//If the light position is to the right of the rect.
	else if (lightPosX >= x2) {
		//If the light is at the top right of the rect.
		if (lightPosY >= y2) {
			targetX = x1;
			targetY = y2;

			targetX2 = x2;
			targetY2 = y1;
		}
		
		//If the light is at the bottom right of the rect.
		else if (lightPosY <= y1) {
			targetX = x1;
			targetY = y1;

			targetX2 = x2;
			targetY2 = y2;
		}

		//Else the light is directly to the right of the rect.
		else {
			targetX = x2;
			targetY = y2;

			targetX2 = x2;
			targetY2 = y1;
		}
	}

	//If the light position is somewhere in the middle of the rect.
	
	else {
		//If the light position is above the rect.
		if (lightPosY >= y2) {
			targetX = x1;
			targetY = y2;

			targetX2 = x2;
			targetY2 = y2;
		}

		//Else if the light is below the rect.
		else if (lightPosY <= y1) {
			targetX = x1;
			targetY = y1;

			targetX2 = x2;
			targetY2 = y1;
		}

		//Else the light is ontop of the rect, draw no shadows
		else 
			return;
	}

	//Drawing the shadows.
	glBegin(GL_POLYGON);
		angle = getAngle(lightPosX, lightPosY, targetX, targetY);
		glVertex2f(targetX, targetY);
		glVertex2f(targetX + sin(angle) * range, targetY + cos(angle) * range);
		
		angle = getAngle(lightPosX, lightPosY, targetX2, targetY2);
		glVertex2f(targetX2 + sin(angle) * range, targetY2 + cos(angle) * range);
		glVertex2f(targetX2, targetY2);
	glEnd();
}

//Old version: Needs updating
//#define getAngle(x1, y1, x2, y2) (atan((double) (checkDivisors((x2 - x1), (y2 - y1))) + ((y2 >= y1) ? ((x2 > x1) ? 0 : twoPI) : PI)))

//New version:
//Returns the angle from point <x1, y1> to point <x2,y2>
//Note to self and to others:
//	If you are looking for an angle bug it will probably be in your own code and not in this getAngle function.
//	(It has been tested quite a few times :D)
//	If however you find one, or a way to improve performance while keeping stability,
//	please e-mail me @ deylen.t@gmail.com so I can update the master copy of the source.
//
//	The same goes for anything else as well really but especially speed critical systems such as below :)

double getAngle (int x1, int y1, int x2, int y2) {
	double goodEnough = 0.1;	//How many decimal places do you need
	double xDiff = x2 - x1;
	double yDiff = y2 - y1;

	double angle = atan(checkDivisors(xDiff, yDiff));

	if (yDiff >= goodEnough) {	//The second point is above the first
		if (xDiff > 0)	//The second point is to the right of the first
			return angle;
		else	 		//The second point is to the top left of the first
			return twoPI + angle;
	}

	else if (yDiff <= -goodEnough)
		return angle + PI;

	else {	//If the yDifference of the two points is very small
		if (xDiff > 0)
			return PI_div2;		//Return PI/2 if point2->x < point1->x
		else
			return twoPI - PI_div2;	//Return 2PI - PI/2 if point2->x > point1->x
	}
}


float gsl_get_angle (int x, int y, int x2, int y2) {
	return getAngle(x, y, x2, y2);
}

float gsl_get_dist (int x, int y, int x2 ,int y2) {
	return getDist(x, y, x2, y2);
}

float gsl_to_degrees (float angle) {
	return deg(angle);
}

float gsl_to_radians (float angle) {
	return rad(angle);
}

int gsl_x_in_rect (int x, int y, int x2, int y2, int w, int h) {
	return (x >= x2 && x <= x2+w && y >= y2 && y <= y2+h) ? 1 : 0;
}
/*}}}*/

//			TEXTURE_FUNCTIONS			/*{{{*/

#define WIDTH 1
#define HEIGHT 2
#define NUSED 3
#define POSITION 4

char * loadTGA(char * loc, int * x, int *y, short * bpp);
GLuint loadGLTex(char * loc, int *x, int *y, short *bpp);

void cleanTexF(Tex * t)
{
	/*Force the cleaning of a tex 						(deylen - 14/5/2009)*/

//	#ifdef _DEBUG
//		printf("Freeing: %s\n", t->u->loc);
//	#endif
	glDeleteTextures(1, &t->id);
	free(t);
}

void delTex(Tex * t)
{
	//This function deletes a texture from the list, then shifts all the other 
	//textures down into the space that has been created. 			(deylen - 02/06/2009)
	
	if (--t->nUsed > 0)
		return;

	/* Delete a Tex from texList 						(deylen - 14/5/2009)*/
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
	/* Force cleaning of all textures 					(deylen - 14/5/2009)*/
	int a = 0;
	if (nTex) {
		for (; a < nTex; a++)
			if (texList[a])			//this may have been cleaned earlier
				cleanTexF(texList[a]);
		free(texList);
		nTex = 0;
	}
	#ifdef _DEBUG
		printf("|\t--=>\tTexture clean\t----===>%5d freed\t|\n", a);
	#endif
}

void gsl_delete_tex (Tex * t) {
	//To be called from lisp
	delTex(t);
}

int * gsl_load_tex(char * loc, int min_filter, int mag_filter, int wrap_s, int wrap_t)
{
	/*Creates and returns a new Texture				(deylen - 14/5/2009)*/
	MIN_FILTER = min_filter;
	MAG_FILTER = mag_filter;
	WRAP_S     = wrap_s;
	WRAP_T     = wrap_t;

	int * temp = (int *) c_malloc(size_int * 333);
	short bpp = 0;
	temp[0] = loadGLTex(loc, &temp[1], &temp[2], &bpp); 

	return temp;
}

GLuint makeGLTex(char * data, int x, int y, int bpp)
{
	/* Make an OpenGL texture from data <data> and size (x y) 				(deylen - 14/5/2009)*/
	GLuint tex;

	glGenTextures(1, &tex);
	glBindTexture(GL_TEXTURE_2D, tex);
	glTexEnvf(GL_TEXTURE_2D, GL_TEXTURE_ENV, GL_BLEND);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, WRAP_S);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, WRAP_T);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, MAG_FILTER);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, MIN_FILTER);

	GLuint result = 0;
	switch (bpp) {
		case 4:
			result = gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, x, y, GL_BGRA, GL_UNSIGNED_BYTE, data);
			break;
		case 3:
			fatal("Please use only textures with alpha channels for the moment");
			result = gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, x, y, GL_BGRA, GL_UNSIGNED_BYTE, data);
			break;

		default:
			fatal("Unknown image format");
	}

	if (result)
		switch (result) {
			case GL_INVALID_VALUE:
				fatal("OpenGL Error -> GL_INVALID_VALUE");
			case GL_INVALID_ENUM:
				fatal("OpenGL Error -> GL_INVALID_ENUM");
			case GL_INVALID_OPERATION:
				fatal("OpenGL Error -> GL_INVALID_OPERATION");

			default:
				printf("OpenGL Error -> Unknown %d", result);
				fatal("");
		}

	return tex;
}

GLuint loadGLTex(char * loc, int *x, int *y, short * bpp)
{
	/* Load the texture data from file 					(deylen - 14/5/2009)*/
	char * data;

	if (!strstr(loc, ".tga"))
		fatal("Sorry only TGA image format is supported at the moment");

	data = loadTGA(loc, x, y, bpp);
	return makeGLTex(data, *x, *y, *bpp);
}

char * loadTGA(char * loc, int * x, int *y, short * bpp)
{
	//Loads the TGA data from file 		(deylen - 02/06/2009)

	int size;
	char * data;
	unsigned short w, h;
	FILE * f;
	
	if ((f = fopen(loc, "rb")) == NULL){
		printf("Could not open file %s", loc);
		pfatal("");
	}
	
	fseek(f,   12, SEEK_SET);
	fread(&w,  2, 1, f);
	fread(&h,  2, 1, f);
	fread(bpp, 1, 1, f);
	fseek(f,   18, SEEK_SET);
	*x = w; *y = h; *bpp /= 8;

	size = (*x) * (*y) * (*bpp);

	data = newStr(size);
	if (fread(data, sizeof(unsigned char), size, f) < size) {
		free(data);
		pfatal("Could not read all of data from file. Make sure you have saved the Image without RLE compression\n");
	}

	fclose(f);
	
	return data;
}
/*}}}*/

//			FONT_TEXT_FUNCTIONS			/*{{{*/

Tex * newTexChar(char * data, int w, int h, int bpp)
{
	//Creates a new character texture
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
	/* Create a single letter						(deylen - 14/5/2009)*/

	int a;
	char * letterDat = newStr(size * (size * bpp));
	char * p, * p2 = letterDat;
	Tex * letter;

	p = data + (x * (size * bpp)) + (y * (size * (w * bpp)));
	/* Setting the start point of our texture 				(deylen - 14/05/2009)*/

	/* - - - - - - - - - - *
	 * - - - - _ _ - - - - *
	 * - - - |     | - - - *
	 * - @ - |  A  | - B - *
	 * - - - | _ _ | - - - *
	 * - - - - - - - - - - *
	 * - - - - - - - - - - *
	 *
	 *   A is the area of the whole texture that we will extract for our letter 				(deylen - 14/05/2009)*/

	for (a=0; a < size; a++){
		p2 = strcpyN(p2, p, size * bpp);
		p += w * bpp;
	}/* copying the data into letterDat by way of pointer p2						(deylen - 14/05/2009)*/
	/* add <w * bpp> to move to the next row of the texture						(deylen - 14/05/2009)*/

	letter = newTexChar(letterDat, size, size, bpp);
	free(letterDat);

	return letter;
}

FFont * newFFont(char * loc)
{
	//Creates a new font
	
	FFont * temp = (FFont *) c_malloc(sizeof(FFont));

	int w, h, size, x, y;
	short bpp = 0;
	char * data;
	Tex * letter;
	temp->nLetters = 0;

	data = loadTGA(loc, &w, &h, &bpp);
	size = w / 16;
	/* Loading the data from our font image						(deylen - 14/05/2009)*/

	for (y=0; y < 16; y++)
		for (x=0; x < 16; x++){
			letter = newLetter(data, x, 15 - y, size, bpp, w);
			temp->letters = (Tex **) listAdd((void **) temp->letters, size_tex_p, letter, &temp->nLetters);
		}
	/* Running through the image and creating each letter					(deylen - 14/05/2009)*/

	temp->size = size / 16.0;
	free(data);

	return temp;
}

int gsl_new_font (char * loc) 
{
	//To be called from lisp

	FFont * temp = newFFont(loc);

	fontList = (FFont **) listAdd((void **) fontList, size_ffont_p, temp, &nFonts);

	return nFonts-1;	/* -1 because listAdd automatically increments the list 			(deylen - 14/05/2009)*/
}

void cleanFFont(FFont * f)
{
	//Forces cleaning of a font
	
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
	//Force cleaning of all fonts

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
	//Draws a single character to the screen lisp callable
	gsl_draw_tex(fontList[font]->letters[character]->id, x, y, z, w, h, 0.0, 0.0);
}/*}}}*/

//		FBO_FUNCTIONS				/*{{{*/
#define GSL_FBO_COLOR_BUFFER 0
#define GSL_FBO_DEPTH_BUFFER 1

int gsl_new_framebuffer(int width, int height)
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
	//If the user calls gsl_use_framebuffer with an integer in clozure it then calls this function
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

int gsl_fbo_add_color(int id, int pos)
{
	if (pos > GL_MAX_COLOR_ATTACHMENTS_EXT) {
		printf("Your graphics card does not support this color attachment\n");
		return 0;
	}
	Fbo * f = fboList[id];
	GLuint colorBuffer = GL_COLOR_ATTACHMENT0_EXT + pos;	//Calculate the Id of the color buffer
	GLuint img;

	glGenTextures(1, &img);
	glBindTexture(GL_TEXTURE_2D, img);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, f->width, f->height, 0, GL_RGBA, GL_UNSIGNED_INT, NULL);
	//Set the texture options and create the texture

	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, f->id);
	glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, colorBuffer, GL_TEXTURE_2D, img, 0);
	//Bind the texture to the framebuffer

	gslFramebufferGetError();
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
	//Unbind the framebuffer

	return img;
}

int gsl_fbo_add_depth(int id)
{
	Fbo * f = fboList[id];
	GLuint depth;
	glGenRenderbuffersEXT(1, (GLuint *) &depth);

	glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, (GLuint) depth);
	glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24, f->width, f->height); 
	//Creating our depth buffer;
	
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, (GLuint) f->id);
	glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, (GLuint) depth);

	gslFramebufferGetError();
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

	return depth;
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

//		SHADER_FUNCTIONS				/*{{{*/

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
		printf("\nCould not compile %s, does your graphics card support GLSL shaders?\n", loc);
		return 0;
	}

	return shader;
}

int gsl_new_shader_var (int program, char * name)
{
	return glGetUniformLocation(shaderList[program]->prog, name);
}

void gsl_set_shader_var_f (int program, int var, float value) 
{
	glUseProgram(shaderList[program]->prog);
	glUniform1f(var, value);
	glUseProgram(0);
}

void addShadersToProgram (Shader * temp, char * vert, char * frag) {
	if (strlen(vert)) {
		if (temp->vert) {	//Making sure we clean the shader if it already exists
			glDetachShader(temp->prog, temp->vert);
		       	glDeleteShader(temp->vert);
		}
		temp->vert = loadShader(vert, GL_VERTEX_SHADER, (GLuint *) &temp->prog);
	}
	if (strlen(frag)) {
		if (temp->frag) {
			glDetachShader(temp->prog, temp->frag);
		       	glDeleteShader(temp->frag);
		}
		temp->frag = loadShader(frag, GL_FRAGMENT_SHADER, (GLuint *) &temp->prog);
	}
}

int gsl_new_shader(char * vert, char * frag)
{
	Shader * temp = (Shader *) c_malloc(size_shader);
	temp->prog = temp->vert = temp->frag = 0;

	addShadersToProgram(temp, vert, frag);
	addShadersToProgram(temp, "", "b_w.frag2");

	temp->n = nShaders;
	shaderList = (Shader **) listAdd((void **) shaderList, size_shader_p, temp, &nShaders);

	return temp->n;
}

void gsl_shader_source (int shader, char * vert, char * frag) {
	addShadersToProgram(shaderList[shader], vert, frag);
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
