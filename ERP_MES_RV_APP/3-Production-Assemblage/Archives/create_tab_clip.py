import os

import svgwrite



import os
import svgwrite

import os
import svgwrite

def create_svg_with_tab(svg_path, x_coord, y_coord, tab_width, tab_height):
    # Create a new SVG document with the "full" profile
    dwg = svgwrite.Drawing(profile='full')

    project_root = os.path.dirname(os.path.abspath(__file__))
    path = os.path.join(project_root, svg_path)

    # Load the original SVG to retrieve its dimensions
    original_svg = svgwrite.Drawing(filename=path)
    original_width = original_svg['width']
    original_height = original_svg['height']

    # Set the dimensions of the canvas based on the original SVG
    canvas_width = original_width
    canvas_height = original_height

    # Add a white rectangle covering the entire canvas
    white_rect = dwg.rect(insert=(0, 0), size=(canvas_width, canvas_height), fill="white")
    dwg.add(white_rect)

    # Define the clipping path
    clip_path = dwg.defs.add(dwg.clipPath(id='my_clip_path'))
    clip_path.add(dwg.rect(insert=(x_coord, y_coord), size=(tab_width, tab_height)))

    # Create a group and apply the clipping path to it
    group = dwg.g(clip_path="url(#my_clip_path)")

    # Include the original SVG using the <image> entity
    image = svgwrite.image.Image(href=path, size=(original_width, original_height))
    group.add(image)

    # Add the group to the SVG document
    dwg.add(group)

    # Save the modified SVG document
    output_svg_path = "../Test/output_with_tab.svg"
    dwg.saveas(output_svg_path)






create_svg_with_tab("../Test/Mediamodifier-Design.svg", 20, 20, 500, 500)
