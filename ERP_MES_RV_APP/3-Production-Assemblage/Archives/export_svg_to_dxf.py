import os

import ezdxf
import svgpathtools
from ezdxf.addons import Importer




def export_svg_to_dxf(svg_path, dxf_path):
    # Load the SVG file using ezdxf's Importer
    project_root = os.path.dirname(os.path.abspath(__file__))
    path = os.path.join(project_root, svg_path)
    output_dxf_path = os.path.join(project_root, dxf_path)
    # Create a new DXF document
    doc = ezdxf.new(dxfversion='R2010')

    # Load the SVG file using ezdxf's Importer
    importer = Importer(doc, svg_path)
    importer.import_svg()

    # Save the DXF document
    doc.saveas(output_dxf_path )




# Specify the input SVG file path and output DXF file path
input_svg_path = "../Test/output_with_tab.svg"
output_dxf_path = "../Test/output_with_tab.dxf"

# Call the export function
export_svg_to_dxf(input_svg_path, output_dxf_path)