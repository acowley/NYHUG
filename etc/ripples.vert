#version 150
in vec4 vertexPos;
uniform mat4 proj;
out vec2 texCoord;
out vec4 vcolor;

void main() {
  vcolor = vec4(0.0f,0.0f,vertexPos.y * 10.0f, 0.0f);
  texCoord = vertexPos.xz * 0.5f + 0.5f;
  gl_Position = proj * vertexPos;
}
