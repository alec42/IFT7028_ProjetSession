import os
import svgwrite

def create_svg_with_tab(svg_path, x_coord, y_coord, tab_width, tab_height):
    # Create a new SVG document with the "full" profile
    dwg = svgwrite.Drawing(profile='full')

    project_root = os.path.dirname(os.path.abspath(__file__))
    path = os.path.join(project_root, svg_path)
    print(path)
    # Load existing SVG from a file
    dwg.add(svgwrite.image.Image(href=path))

    # Add a white rectangle as the tab
    tab_rect = dwg.rect(insert=(x_coord, y_coord), size=(tab_width, tab_height), fill="white")
    dwg.add(tab_rect)

    # Save the modified SVG document
    output_svg_path = "../Test/output_with_tab.svg"
    dwg.saveas(output_svg_path)

create_svg_with_tab("../Test/Mediamodifier-Design.svg", 20, 20, 500, 500)