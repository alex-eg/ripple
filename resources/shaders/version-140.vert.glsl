#version 140

uniform Transformation {
    mat4 projection;
    mat4 modelview;
};

in vec4 vertex;

void main (void) {
    gl_Position = projection * modelview * vertex;
}