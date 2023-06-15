import json
import os
import re

import openscad_export
import beam_grouping
import format_dxf_for_laser_cutting
import Production.FindTabPositions as tabs

def export_all_dxfs(order_id):
    project_root = os.path.dirname(os.path.abspath(__file__))

    directory_path = os.path.join(project_root, r"Production_Temp/")

    # Create the directory if it doesn't exist
    if not os.path.exists(directory_path):
        os.makedirs(directory_path)

    directory_temp_assembly = os.path.join(project_root, r"Production_Temp/")

    directory_path = os.path.join(project_root, r"Production_Drawings/")

    # Create the directory if it doesn't exist
    if not os.path.exists(directory_path):
        os.makedirs(directory_path)

    directory_temp_assembly = os.path.join(project_root, r"Production_Drawings/")


    input_path = os.path.join(project_root, "OpenSCAD_Parts/")
    output_path = os.path.join(project_root, "Production_Temp/")
    final_path = os.path.join(project_root, "Production_Drawings/")

    # Specify the directory path
    directory = input_path

    # json_file_path = None
    # json_data = None
    # Iterate over the files in the directory
    for filename in os.listdir(directory):
        # if filename.endswith("commande.json"):
        if filename.endswith(".json"):
            json_file_path = os.path.join(directory, filename)
            break  # Stop iterating once the first matching file is found

    # Read the JSON file
    # if json_file_path is not None:
    with open(json_file_path) as file:
        json_data = json.load(file)
    print(json_data)
    # if json_data is not None:
    # Extract width and length from overallDims
    overall_dims = json_data['overallDims']
    truss_length = overall_dims['width']['value']
    base_truss_length = overall_dims['lenght']['value']
    print(truss_length)
    print(base_truss_length)
    # Create beams .scad
    model_params = json_data["FichiersFabrication"]
    # Count the number of keys starting with "fixed_trusses"
    num_fixed_trusses = sum(key.startswith("fixed_trusses") for key in model_params.keys())
    print(num_fixed_trusses)
    tabs_matrix = beam_grouping.concatenate_beams(500, 1000, 30, 3.5, base_truss_length, 3.5, truss_length, num_fixed_trusses)



    truss_tabs = list(tabs.FindBeamTabPositions(truss_length,3.5))
    beam_tabs = list(tabs.FindBeamTabPositions(base_truss_length,3.5))
    for i in range(len(truss_tabs)):
        point = list(truss_tabs[i])
        truss_tabs[i] = point
    for i in range(len(beam_tabs)):
        point = list(beam_tabs[i])
        beam_tabs[i] = point



    #Export all parts in OpenSCAD_PARTS
    files = os.listdir(input_path)

    # Iterate through each file name
    count = 1
    for file_name in files:
        # Print the file name
        print(file_name)
        if file_name.endswith(".2d.scad"):
            if "truss" not in file_name:
                openscad_export.export_to_dxf(os.path.join(input_path,file_name), os.path.join(output_path,file_name.rsplit(".2d.scad", 1)[0]+".dxf"))

                openscad_export.export_to_dxf(os.path.join(input_path, file_name), os.path.join(final_path, file_name.rsplit(".2d.scad", 1)[0] + ".dxf"))
                format_dxf_for_laser_cutting.format_dxf_for_laser_cutting(os.path.join(output_path,file_name.rsplit(".2d.scad", 1)[0]+".dxf"),os.path.join(final_path, file_name.rsplit(".2d.scad", 1)[0] + ".dxf"),str(order_id) + "/"+str(count) , [])
                count += 1
                
    #Create real files with tabs here.
    print(tabs_matrix)

    if len(tabs_matrix) == 2:
        origins_1 = tabs_matrix[0]
        origins_2 = tabs_matrix[1]

        for point in origins_1[:2]:
            point[0] += beam_tabs[0]
            point[1] += beam_tabs[1]

        for point in origins_1[2:]:
            point[0] += truss_tabs[0]
            point[1] += truss_tabs[1]

        for point in origins_2:
            point[0] += truss_tabs[0]
            point[1] += truss_tabs[1]

        # openscad_export.export_to_dxf(os.path.join(project_root, "OpenSCAD_Parts/combined_beams_1.scad"),
        openscad_export.export_to_dxf(os.path.join(project_root, "Test/combined_beams_1.scad"),
                                      os.path.join(project_root, "Production_Temp/combined_beams_1.dxf"))
        # openscad_export.export_to_dxf(os.path.join(project_root, "OpenSCAD_Parts/combined_beams_2.scad"),
        openscad_export.export_to_dxf(os.path.join(project_root, "Test/combined_beams_2.scad"),
                                      os.path.join(project_root, "Production_Temp/combined_beams_2.dxf"))
        format_dxf_for_laser_cutting.format_dxf_for_laser_cutting(
            os.path.join(project_root, "Production_Temp/combined_beams_1.dxf"),
            os.path.join(project_root, "Production_Drawings/combined_beams_1.dxf"), str(order_id) + "/" + str(count), origins_1,2)
        format_dxf_for_laser_cutting.format_dxf_for_laser_cutting(
            os.path.join(project_root, "Production_Temp/combined_beams_2.dxf"),
            os.path.join(project_root, "Production_Drawings/combined_beams_2.dxf" ), str(order_id) + "/"+ str(count+1), origins_2,2)
    else:
        origins_1 = tabs_matrix[0]
        tabs_1 = []
        for i in range(len(origins_1)):
            point = list(origins_1[i])
            origins_1[i]=point

        for point in origins_1[:2]:
            for beam_tab in beam_tabs:
                tabs_1.append([point[0] + beam_tab[0],point[1] + beam_tab[1]])

        for point in origins_1[2:]:
            for truss_tab in truss_tabs:
                tabs_1.append([point[0] + truss_tab[0],point[1] + truss_tab[1]])

        # openscad_export.export_to_dxf(os.path.join(project_root, "OpenSCAD_Parts/combined_beams_1.scad"),
        openscad_export.export_to_dxf(os.path.join(project_root, "Test/combined_beams_1.scad"),
                                     os.path.join(project_root, "Production_Temp/combined_beams_1.dxf"))
        format_dxf_for_laser_cutting.format_dxf_for_laser_cutting(
            os.path.join(project_root, "Production_Temp/combined_beams_1.dxf"),
            os.path.join(project_root, "Production_Drawings/combined_beams_1.dxf" ),str(order_id) + "/" + str(count), tabs_1,2)

# export_all_dxfs(123456789)
