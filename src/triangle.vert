attribute vec3 aVertexPosition;

uniform float time;

void main(void) {
  gl_Position = vec4(aVertexPosition, 1.0);
  gl_Position.x += sin(time);
}
