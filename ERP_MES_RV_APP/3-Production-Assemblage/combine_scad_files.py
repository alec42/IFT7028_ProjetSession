def combine_scad_files(file_paths, output_file_path):
    with open(output_file_path, 'w') as output_file:
        for file_path in file_paths:
            try:
                with open(file_path, 'r') as file:
                    output_file.write(file.read())
                output_file.write('\n\n')
            except FileNotFoundError:
                print(f"File not found: {file_path}. Skipping...")



