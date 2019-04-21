#version 410 core

layout (location = 0) in vec2 vertexPosition_modelspace;
layout (location = 1) in vec3 vertexColor;

out vec3 ourColor;

uniform mat4 transform;

void main(){

  gl_Position = transform * vec4(vertexPosition_modelspace.xy, 0.0, 1.0);
  ourColor = vertexColor;
}
