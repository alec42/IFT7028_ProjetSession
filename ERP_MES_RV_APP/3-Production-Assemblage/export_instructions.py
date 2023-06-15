import reportlab
from svglib.svglib import svg2rlg
# from PIL import Image
import openscad_export
import os
from reportlab.lib.pagesizes import letter
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, PageBreak, Image
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.enums import TA_CENTER
import json

from combine_scad_files import combine_scad_files

def create_fichiers_fabrication_dict(folder_path):
    fichiers_fabrication = {}

    for filename in os.listdir(folder_path):
        if filename.endswith(".scad"):
            file_parts = filename.split(".")
            truss_name = file_parts[0]
            file_extension = file_parts[-1].split("_")[0]  # Extract the file extension (2d or 3d)

            if truss_name not in fichiers_fabrication:
                fichiers_fabrication[truss_name] = [[], []]  # Create a list with 2 empty sub-lists

            file_path = os.path.join(folder_path, filename)

            if file_extension == "2d":
                fichiers_fabrication[truss_name][0].append(file_path)  # Append to the 2D sub-list
            elif file_extension == "3d":
                fichiers_fabrication[truss_name][1].append(file_path)  # Append to the 3D sub-list

    return fichiers_fabrication


def create_teardrop_assembly_manual(order_id):
    # Create a document with the specified file path
    project_root = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(project_root, r"Instructions/Manuel_Assemblage.pdf")
    directory_path = os.path.join(project_root, r"Instructions/")

    # Create the directory if it doesn't exist
    if not os.path.exists(directory_path):
        os.makedirs(directory_path)
    
    directory_temp_assembly = os.path.join(project_root, r"Assembly_Temp/")

    # Create the directory if it doesn't exist
    if not os.path.exists(directory_temp_assembly):
        os.makedirs(directory_temp_assembly)


    doc = SimpleDocTemplate(file_path, pagesize=letter)

    # Create a list to store the contents of the document
    content = []

    # Define the styles to be used for headings and paragraphs
    styles = getSampleStyleSheet()
    heading_style = styles['Heading1']
    paragraph_style = styles['BodyText']
    centered_style = ParagraphStyle(name='Centered', parent=paragraph_style, alignment=TA_CENTER)
    centered_heading = ParagraphStyle(name='Centered', parent=heading_style,alignment=TA_CENTER)
    # Load the assembly image and and the steps images
    project_root = os.path.dirname(os.path.abspath(__file__))

    a_path = os.path.join(project_root, "OpenSCAD_Parts/")


    # Specify the directory path
    directory = a_path

    # json_file_path = None
    # data = None
    # Iterate over the files in the directory
    for filename in os.listdir(directory):
        # if filename.endswith("commande.json"):
        if filename.endswith(".json"):
            json_file_path = os.path.join(directory, filename)
            break  # Stop iterating once the first matching file is found

    # Read the JSON file
    # if json_file_path is not None:
    with open(json_file_path) as file:
        data = json.load(file)
        print(data)
    print(data)
    # if data is not None:
    fichiers_fabrication = data["FichiersFabrication"]

    # Create .scad assemblies for instructions
    # Create assembly .scad
    assembly_parts = []
    assembly_output_path = os.path.join(project_root,r"Assembly_Temp/assembly.scad")
    for key, value in fichiers_fabrication.items():
        #assembly_parts.append(os.path.join(project_root + "\OpenSCAD_Parts/",value[1]))
        print(value)
    for filename in os.listdir(os.path.join(project_root, "OpenSCAD_Parts/")):
        if ".3d.scad" in filename:
            assembly_parts.append(os.path.join(project_root + "/OpenSCAD_Parts/", filename))
    combine_scad_files(assembly_parts,assembly_output_path)
    
    # Iterate over the files in the directory
    for filename in os.listdir(os.path.join(project_root, "OpenSCAD_Parts/")):
        if ".2d.scad" in filename:
            if "left_outside_panel" in filename:
                left_outside_panel_name_2d = filename
            elif "left_inside_panel" in filename:
                left_inside_panel_name_2d = filename
            elif "floor_panel" in filename:
                floor_panel_2d = filename
        elif ".3d.scad" in filename:
            if "left_outside_panel" in filename:
                left_outside_panel_name_3d = filename
            elif "left_inside_panel" in filename:
                left_inside_panel_name_3d = filename
            elif "left_base_board_truss" in filename:
                left_base_truss_3d = filename
            elif "floor_panel" in filename:
                floor_panel_3d = filename
    # Create step 3 assembly (right side)
    step_3_parts = [os.path.join(project_root + "/OpenSCAD_Parts", left_outside_panel_name_3d),os.path.join(project_root + "/OpenSCAD_Parts/", left_inside_panel_name_3d),os.path.join(project_root + "/OpenSCAD_Parts/", left_base_truss_3d)]
    combine_scad_files(step_3_parts, os.path.join(project_root,r"Assembly_Temp/step_3.scad"))
    # Create step 4 assembly ()
    step_4_parts = step_3_parts
    step_4_parts.append(os.path.join(project_root + "/OpenSCAD_Parts/", floor_panel_3d))
    combine_scad_files(step_4_parts, os.path.join(project_root,r"Assembly_Temp/step_4.scad"))

    # Assembly
    openscad_export.generate_isometric_view(r"Assembly_Temp/assembly.scad", "Assembly_Temp/assembly.png")
    # Beams panel
    # openscad_export.export_to_bmp("OpenSCAD_Parts/combined_beams_1.scad", 
    openscad_export.export_to_bmp("Test/combined_beams_1.scad",
                                  "Assembly_Temp/combined_beams_1.svg")
    # Left walls combined panels
    openscad_export.export_to_bmp("OpenSCAD_Parts/" + left_outside_panel_name_2d,
                                  "Assembly_Temp/left_outside_panel.svg")
    openscad_export.export_to_bmp("OpenSCAD_Parts/" + left_inside_panel_name_2d,
                                  "Assembly_Temp/left_inside_panel.svg")
    openscad_export.concatenate_bmp_images("Assembly_Temp/left_outside_panel.bmp","Assembly_Temp/left_inside_panel.bmp","Assembly_Temp/left_combined_panel.bmp")
    # Floor panel
    openscad_export.export_to_bmp("OpenSCAD_Parts/" + floor_panel_2d,
                                  "Assembly_Temp/floor_panel.svg")
    # Step 3
    openscad_export.generate_isometric_view(r"Assembly_Temp/step_3.scad","Assembly_Temp/step_3.png")

    # Step 4
    openscad_export.generate_isometric_view(r"Assembly_Temp/step_4.scad", "Assembly_Temp/step_4.png")

    # Path to images created before. Lenght is always the same s
    images = ["Assembly_Temp/assembly.png","Assembly_Temp/left_combined_panel.bmp","Assembly_Temp/combined_beams_1.bmp","Assembly_Temp/step_3.png","Assembly_Temp/step_4.png","Assembly_Temp/combined_beams_1.bmp","Assembly_Temp/assembly.png"]
    desired_height = 180 # Do not modify
    pdf_images = []
    for img in images:
        if os.path.exists((os.path.join(project_root, img))):
            image = reportlab.platypus.Image(os.path.join(project_root, img))
            aspect_ratio = image.imageWidth / image.imageHeight
            desired_width = desired_height * aspect_ratio
            image._restrictSize(desired_width, desired_height)
            pdf_images.append(image)
        else:
            print(os.path.join(project_root, img))


    content.append(Spacer(1, 12))

    # Add a title to the manual
    title = Paragraph('<u>Manuel d\'assemblage micro-roulotte #'+str(order_id)+'</u>', centered_heading)
    content.append(title)


    # Assembly image
    content.append(Spacer(1, 12))
    content.append(pdf_images[0])
    content.append(Spacer(1, 15))

    # Add an introduction section
    intro_title = Paragraph('Introduction', heading_style)
    content.append(intro_title)
    content.append(Spacer(1, 6))
    intro_text = ("""
     Ce manuel fournit des instructions étape par étape pour l'assemblage de votre micro-roulotte. Veuillez lire 
     attentivement l'intégralité du manuel avant de commencer le processus d'assemblage. Si vous avez des questions, 
     contactez notre équipe d'assistance.
    """)
    intro_paragraph = Paragraph(intro_text, paragraph_style)
    content.append(intro_paragraph)
    content.append(Spacer(1, 12))

    #assembly_title = Paragraph('Étapes d\'assemblage', heading_style)
    #content.append(assembly_title)
    #content.append(Spacer(1, 6))

    # Add step 1
    step1_title = Paragraph('Étape 1: Coller les deux couches formant les murs', heading_style)
    content.append(step1_title)

    # Step 1 image
    content.append(Spacer(1, 12))
    content.append(pdf_images[1])
    content.append(Spacer(1, 15))

    content.append(Spacer(1, 6))
    step1_text = """
    1. Prendre le mur intérieur gauche et répartir uniformément la colle sur son coté externe.<br/>
    2. Coller le mur en prenant soin de bien l'aligner sur le coté interne du mur extérieur gauche.<br/>
    3. Attendre que la colle sèche.<br/>
    """
    step1_paragraph = Paragraph(step1_text, paragraph_style)
    content.append(step1_paragraph)
    content.append(Spacer(1, 12))
    content.append(PageBreak())

    # Add step 2
    step2_title = Paragraph('Étape 2: Fixer la poutre de plancher aux mur intérieur', heading_style)
    content.append(step2_title)

    # Step 2 image
    content.append(Spacer(1, 12))
    content.append(pdf_images[2])
    content.append(Spacer(1, 15))

    content.append(Spacer(1, 6))
    step2_text = """
    1. Coller la poutre de plancher au mur intérieur gauche tel qu'affiché sur le schéma.
    """
    step2_paragraph = Paragraph(step2_text, paragraph_style)
    content.append(step2_paragraph)
    content.append(Spacer(1, 12))

    # Add step 3
    step3_title = Paragraph('Étape 3: Répéter pour l\'autre coté', heading_style)
    content.append(step3_title)

    # Step 3 image
    content.append(Spacer(1, 12))
    content.append(pdf_images[3])
    content.append(Spacer(1, 15))

    content.append(Spacer(1, 6))
    step3_text = """
    1. Répéter les étapes 1 et 2 pour le coté droit de la roulotte.
    """
    step3_paragraph = Paragraph(step3_text, paragraph_style)
    content.append(step3_paragraph)
    content.append(Spacer(1, 12))
    content.append(PageBreak())

    # Add step 4
    step4_title = Paragraph('Étape 4: Fixer les murs au plancher', heading_style)
    content.append(step4_title)

    # Step 4 image
    content.append(Spacer(1, 12))
    content.append(pdf_images[4])
    content.append(Spacer(1, 15))

    content.append(Spacer(1, 6))
    step4_text = """
    Note: Il est recommandé d'effectuer les prochaines étapes à 2 afin de tenir le tout en place. <br/>
    1. Coller le coté gauche de la roulotte au plancher.<br/>
    2. Coller le coté droit de la roulotte au plancher.<br/>
    3. Réaliser l\'étape 5 avant que la colle ne sèche.<br/>
    """
    step4_paragraph = Paragraph(step4_text, paragraph_style)
    content.append(step4_paragraph)
    content.append(Spacer(1, 12))

    # Add step 5
    step5_title = Paragraph('Étape 5: Ajouter les poutres transversales', heading_style)
    content.append(step5_title)

    # Step 5 image
    content.append(Spacer(1, 12))
    content.append(pdf_images[5])
    content.append(Spacer(1, 15))

    content.append(Spacer(1, 6))
    step5_text = """
    1. En s'assurant de conserver les murs en contact avec le plancher, coller les poutres une à une.
    """
    step5_paragraph = Paragraph(step5_text, paragraph_style)
    content.append(step5_paragraph)
    content.append(Spacer(1, 12))
    content.append(PageBreak())

    # Add step 6
    step6_title = Paragraph('Étape 6: Ajouter le dessus de la roulotte', heading_style)
    content.append(step6_title)

    # Step 6 image
    content.append(Spacer(1, 12))
    content.append(pdf_images[6])
    content.append(Spacer(1, 15))

    content.append(Spacer(1, 6))
    step6_text = """
    1. Placer le dessus de la roulotte.
    """
    step6_paragraph = Paragraph(step6_text, paragraph_style)
    content.append(step6_paragraph)
    content.append(Spacer(1, 12))

    # Build the document with the content
    doc.build(content)


# Usage example:
# create_teardrop_assembly_manual(123456789)
