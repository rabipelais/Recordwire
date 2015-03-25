#version 150
in vec3 pos;
in vec3 normal;
in vec3 colour;

uniform mat4 cam;
uniform mat4 proj;

out vec3 pointNormal;
out vec3 diffuse;
out vec4 eye;

void main() {
  gl_Position = proj * vec4(pos,1);
  eye = -(cam * vec4(pos,1));
  pointNormal = normal;
  diffuse = colour;
}
