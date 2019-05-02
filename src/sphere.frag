#version 410 core

in vec3 ourColor;

out vec4 color;

uniform vec3 ranColor;

void main() {
  // color = vec4(0.3, 0.3, 0.3, 1.0); 
  color = vec4(ranColor + ourColor, 1.0);
  // color = vec4(col, 1.0);
}
