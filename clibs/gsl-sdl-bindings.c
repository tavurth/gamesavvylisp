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

void sdl_init (int flags) {
	SDL_Init(flags);
}

void sdl_quit () {
	SDL_ShowCursor(1);
	SDL_Quit();
}

void sdl_init_video (int width, int height, int bpp, int flags) {
	if (!SDL_SetVideoMode(width, height, bpp, flags))
		fatal("Could not initialise SDL video mode");
}

void sdl_delay (int delay) {
	SDL_Delay(delay);
}

int sdl_get_ticks () {
	return SDL_GetTicks();
}

void sdl_pump_events () {
	SDL_PumpEvents();
}

char * sdl_key_name (int key) {
	return SDL_GetKeyName(key);
}
