import os
import glob

def beam(width, height):
    return f"cube([{width}, {height}, 1]);"

def translate_beam(x, y, beam_string):
    return f"translate([{x}, {y}, 0]) {{\n {beam_string} \n}}"

def concatenate_beams(panel_width, panel_height, gap, floor_beam_width, floor_beam_height, regular_beam_width, regular_beam_height, nb_regular_beams):
    origins_matrix = []
    beam_origins = []
    file_count = 1
    beam_string = ""
    offset_x = 0
    offset_y = 0

    # floor beams
    for i in range(2):
        beam_origins.append((offset_x, offset_y))
        beam_string += translate_beam(offset_x, offset_y, beam(floor_beam_width, floor_beam_height))
        offset_x += floor_beam_width + gap
        if offset_x + floor_beam_width > panel_width:
            origins_matrix.append(beam_origins)
            beam_origins = []
            with open(os.path.join(os.path.dirname(os.path.abspath(__file__)),f'Production_Temp/combined_beams_{file_count}.scad'), 'w') as f:
                f.write(beam_string)
            beam_string = ""
            offset_x = 0
            offset_y = 0
            file_count += 1

    # regular beams
    for i in range(nb_regular_beams):
        beam_origins.append((offset_x, offset_y))
        beam_string += translate_beam(offset_x, offset_y, beam(regular_beam_width, regular_beam_height))
        offset_x += regular_beam_width + gap
        if offset_x + regular_beam_width > panel_width:
            origins_matrix.append(beam_origins)
            beam_origins = []
            with open(os.path.join(os.path.dirname(os.path.abspath(__file__)),f'Production_Temp/combined_beams_{file_count}.scad'), 'w') as f:
                f.write(beam_string)
            beam_string = ""
            offset_x = 0
            offset_y = 0
            file_count += 1

    # write the remaining beam if any
    if beam_string:
        origins_matrix.append(beam_origins)
        with open(os.path.join(os.path.dirname(os.path.abspath(__file__)),f'Production_Temp/combined_beams_{file_count}.scad'), 'w') as f:
            f.write(beam_string)

    return origins_matrix

#matrix = concatenate_beams(500, 1000, 30, 60, 500, 80, 300, 5)
#print(matrix)



# def concatenate_scad_files(file_pattern, output_file_base, gap, max_length):
#     file_paths = sorted(glob.glob(file_pattern))
#     output_file_index = 1
#     current_width = 0
#     combined_content = ""
#
#     for i, file_path in enumerate(file_paths):
#         with open(file_path, "r") as file:
#             file_content = file.read()
#
#         # calculate the new width if we include this file
#         new_width = current_width + 60 + (gap if current_width > 0 else 0)
#
#         # if the new width would exceed max_length, write the current content to a file and start a new one
#         if new_width > max_length and current_width > 0:
#             output_file_path = "{}{}.scad".format(output_file_base, output_file_index)
#             with open(output_file_path, "w") as file:
#                 file.write(combined_content)
#             output_file_index += 1
#             current_width = 0
#             combined_content = ""
#
#         # append the new file content to the current combined content only if it doesn't exceed max_length
#         if new_width <= max_length:
#             translate_x = current_width + (gap if current_width > 0 else 0)
#             file_content = "translate([{}, 0, 0]) {{\n{}\n}}".format(translate_x, file_content)
#             combined_content += file_content + "\n\n"
#             current_width = new_width
#         else: # we need to add the current beam to the next file
#             translate_x = 0 # since this is the first item for the next file, translate_x is set to 0
#             file_content = "translate([{}, 0, 0]) {{\n{}\n}}".format(translate_x, file_content)
#             combined_content = file_content + "\n\n"
#             current_width = 60 # since we have added the current beam to the next file, current_width should be set to the width of a beam
#
#     # write the final combined content to a file
#     if combined_content:
#         output_file_path = "{}{}.scad".format(output_file_base, output_file_index)
#         with open(output_file_path, "w") as file:
#             file.write(combined_content)

# # Usage:
# file_pattern = 'Test/beam_*.scad'
# output_file_base = 'Test/beams_combined_'
# gap = 30
# max_length = 240  # Adjust to the maximum length
# concatenate_scad_files(file_pattern, output_file_base, gap, max_length)


