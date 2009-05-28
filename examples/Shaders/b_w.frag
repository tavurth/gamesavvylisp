///		Copyright (c) William Whitty 2009
///
///	This file is part of GSL. 
///
///	GSL is free software: you can redistribute it and/or modify
///     it under the terms of the GNU General Public License as published by
///     the Free Software Foundation, either version 3 of the License, or
///     (at your option) any later version.
///
///     GSL is distributed in the hope that it will be useful,
///     but WITHOUT ANY WARRANTY; without even the implied warranty of
///     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///     GNU General Public License for more details.
///
///     You should have received a copy of the GNU General Public License
///     along with GSL.  If not, see <http://www.gnu.org/licenses/>.

uniform sampler2D tex;

void main()
{
	vec4 color = texture2D(tex, gl_TexCoord[0]);
	float maxC = max(max(color.r, color.g), color.b);
	color.rgba = 0;
	if (maxC > 0.75) {
		color.rgb = 1;
	}

	gl_FragColor = color;
}
