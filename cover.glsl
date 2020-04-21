float r(vec2 n) {
  return fract(sin(dot(n, vec2(0.184, 0.13))) * 37.5453
               * (sin(34.1 * (n.x + 0.2 * n.y))* 0.02 + 0.98));
}

vec3 r3(vec2 n){
    vec3 v = vec3(r(n));
    v.x = r(v.xy);
    v.y = r(v.xy);
    v.z = r(v.xy);
    return v;
}

mat4 rotationMatrix(vec3 axis, float angle) {
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat4(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,  0.0,
                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,  0.0,
                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c,           0.0,
                0.0,                                0.0,                                0.0,                                1.0);
}

vec3 rotate(vec3 v, vec3 axis, float angle) {
	mat4 m = rotationMatrix(axis, angle);
	return (m * vec4(v, 1.0)).xyz;
}

void main() {
    vec2 r = vec2(cos(344.3 * 0.05), sin(344.3 * 0.01));
    vec2 uv = (gl_FragCoord.xy / iResolution.xx - 0.5) + r;

    vec3 v = r3(uv) / 2.0;
    v = rotate(v, vec3(-0.03, -0.04, -0.05), -3.6);

    gl_FragColor = vec4(1.0 - v, 1.0);
}
