1. Run opam upgrade, opam update. 
2. Install yojson. 
3. Put spheres as JSON objects in shapes.json, following the template. The radius and center coordinates (in world coordinates) must be specified.
4. Then, run `dune exec bin/main.exe <input_scene.json> <output_image.ppm>` where `<input_scene.json>` is replaced with the path to the scene json and `<output_image.ppm>` is replaced with the path where the render should be written. 