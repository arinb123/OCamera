## Installation instructions

1. Run `opam update && opam upgrade`.
2. Install dependencies: `opam install yaml ounit2 qcheck bisect_ppx raylib`.

## Running the raytracer

Scenes are described as YAML files. A scene file must contain a `camera` section and an `objects` list. See [data/spheres.yaml](data/spheres.yaml) for an example scene file. 

Supported object types: `sphere`, `triangle`, `triangular_mesh`.
Supported material types: `lambertian`, `metal` (metal requires a `fuzz` field).

**Rendering the image and saving to the disk:**

```
dune exec bin/main.exe <input_scene.yaml> <output_image.ppm>
```

**Rendering the user interface**

```
dune exec bin/main.exe -- -ui <input_scene.yaml>
```

WASD translates the camera (A/D = left/right, W/S = up/down), Q/E move it in depth (Z), arrow keys rotate the camera (left/right = yaw, up/down = pitch), R resets position and orientation.
