1. Run `opam update && opam upgrade`.
2. Install dependencies: `opam install yaml ounit2 qcheck bisect_ppx raylib`.
3. Build the project: `dune build`.

## Running the raytracer

Scenes are described as YAML files. A scene file must contain a `camera` section and an `objects` list. Example (`data/scene.yaml`):

```yaml
camera:
  image_width: 400
  aspect_ratio: 1.7778   # 16/9
  focal_length: 1.0
  center: [0.0, 0.0, 0.35]
  samples_per_pixel: 50
  max_depth: 10
  viewport_height: 2.0

objects:
  - type: sphere
    center: [0.0, 0.0, -1.0]
    radius: 0.5
    material:
      type: lambertian
      albedo: [0.1, 0.2, 0.5]

  - type: triangle
    v0: [-0.5, -0.5, -2.0]
    v1: [0.5, -0.5, -2.0]
    v2: [0.0, 0.5, -2.0]
    material:
      type: metal
      albedo: [0.8, 0.8, 0.8]
      fuzz: 0.1

  - type: triangular_mesh
    file_path: data/bunny.obj
    material:
      type: lambertian
      albedo: [0.7, 0.3, 0.3]
```

Supported object types: `sphere`, `triangle`, `triangular_mesh`.
Supported material types: `lambertian`, `metal` (metal requires a `fuzz` field).

### Single PPM Image

```
dune exec bin/main.exe <input_scene.yaml> <output_image.ppm>
```

### Interactive UI

```
dune exec bin/main.exe -- -ui <input_scene.yaml>
```

WASD translates the camera (A/D = left/right, W/S = up/down), Q/E move it in depth (Z), arrow keys rotate the camera (left/right = yaw, up/down = pitch), R resets position and orientation.
